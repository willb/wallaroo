# cmd_relationships: Create relationships between entities
#
#  Copyright (c) 2011 Red Hat, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module Wallaroo
  module Shell
    module VerifyOps
      def verify_input
        bad_target = verify_target
        bad_names = verify_names
        if (not bad_target.empty?) || (not bad_names.empty?)
          bad_target.each {|n| puts "#{options[0][:opt_name].split('-')[0].downcase} '#{n}' does not exist"}
          bad_names.each {|n| puts "#{options[1][:opt_name].split('-')[0].downcase} '#{n}' does not exist"}
          exit!(1, "Invalid input")
        end
      end
    end

    module GroupMembershipOps
      include VerifyOps

      def act
        verify_input
        @names.each do |n|
          obj = store.getNode(n)
          obj.modifyMemberships(command, [@target], {})
        end
        return 0
      end
    end

    module NodeMembershipOps
      def sub_group_for_node
        false
      end

      def target_type
        @t_type ||= (self.class.opname.split("-") - prepositions)[1]
      end

      def name_type
        @n_type ||= (self.class.opname.split("-") - prepositions)[2].gsub(/[s]$/, "")
      end

      def verify_names
        store.send("checkGroupValidity", @names)
      end

      def options
        [{:opt_name=>"#{target_type.upcase}-NAME", :desc=>"the name of the #{target_type.downcase} to act upon"},
         {:opt_name=>"GROUP-NAME", :desc=>"the names of groups to #{command.downcase}"}]
      end
    end

    module HashArgType
      def arg_type
        Hash
      end
    end

    module ParamOps
      include HashArgType
      def options
        [{:opt_name=>"#{target_type.upcase}-NAME", :desc=>"the name of the #{target_type.downcase} to act upon"},
         {:opt_name=>"#{name_type.upcase}-NAME[=VALUE]", :desc=>"the names[=values] of #{name_type.downcase}s to #{command.downcase}"}]
      end
    end

    module RelationshipOps
      include VerifyOps

      def supports_options
        false
      end

      def sub_group_for_node
        true
      end


      def options
        [{:opt_name=>"#{target_type.upcase}-NAME", :desc=>"the name of the #{target_type.downcase} to act upon"},
         {:opt_name=>"#{name_type.upcase}-NAME", :desc=>"the names of #{name_type.downcase}s to #{command.downcase}"}]
      end

      def arg_type
        Array
      end

      def opargs
        options.collect {|x| "#{x[:opt_name].to_s.upcase}" }.join(" ") + " [...]"
      end

      def prepositions
        ["to", "from", "on"]
      end

      def command
        @cmd ||= (self.class.opname.split("-") - prepositions)[0].upcase
      end

      def target_type
        @t_type ||= (self.class.opname.split("-") - prepositions)[2]
      end

      def name_type
        @n_type ||= (self.class.opname.split("-") - prepositions)[1].gsub(/[s]$/, "")
      end

      def self.included(receiver)
        if receiver.respond_to?(:register_callback)
          receiver.register_callback :after_option_parsing, :parse_args
        end
      end

      def parse_args(*args)
        opts = 0
        self.options.each do |opt|
          input = args.shift
          if input == nil and command != "REPLACE"
            exit!(1, "you must specify a #{opt[:opt_name]}")
          elsif opts < (self.options.length-1)
            @target = input
          else
            if arg_type == Hash
              @names = {}
              args.unshift(input) if input
              args.each do |a|
                tmp = a.split("=", 2)
                @names[tmp[0]] = 0 if tmp.length == 1
                @names[tmp[0]] = tmp[1] if tmp.length == 2
              end
            else
              @names = []
              @names << input if input and not input.empty?
              args.each do |a|
                @names << a if not a.empty?
              end
              @names.uniq!
            end
          end
          opts += 1
        end
      end

      def init_option_parser
        if options.size > 0
          d = options.collect {|o| "#{o[:opt_name]} is #{o[:desc]}\n" }
        end
        OptionParser.new do |opts|
          opts.banner = "Usage:  wallaby #{self.class.opname} #{opargs} " + ("[OPTIONS]" if supports_options).to_s + "\n#{self.class.description}\n#{d}"

          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end

          if supports_options
      opts.on("-p", "--priority PRI", "Add #{name_type.upcase}-NAME(s) at given priority") do |p|
              @priority = p.to_i
            end
          end
        end
      end

      def full_target_type
        Wallaroo::Client.constants.grep(/^#{target_type.capitalize}/).select {|x| Wallaroo::Client.const_get(x).ancestors.include?(::Wallaroo::Client::Proxying) }[0]
      end

      def verify_target
        store.send("check#{full_target_type}Validity", [@target])
      end

      def verify_names
        t = Wallaroo::Client.constants.grep(/^#{name_type.slice(0,5).capitalize}/).select {|x| Wallaroo::Client.const_get(x).ancestors.include?(::Wallaroo::Client::Proxying) }[0]
        n = @names
        n = @names.keys if @names.class == Hash
        if t == nil
          store.send("check#{full_target_type}Validity", n)
        else
          store.send("check#{t}Validity", n)
        end
      end

      def act
        verify_input
        cname = Wallaroo::Client.constants.grep(/^#{target_type.capitalize}/).select {|x| Wallaroo::Client.const_get(x).ancestors.include?(::Wallaroo::Client::Proxying) }[0]
        obj = store.cm.make_proxy_object(cname.downcase, @target)
        obj.refresh
        
        if cname == "Node" and sub_group_for_node
          obj = obj.identity_group
          cname = "Group"
        end
        cmethod = find_method(name_type.slice(0,5).capitalize, cname).select {|m| m if m.index("modify") != nil}[0]
        if (@priority == nil) || (command != "ADD")
          obj.send(cmethod, command, @names, {})
        else
          get = get_from_set(cmethod.to_sym)
          cur = obj.send(get)
          cnt = 0
          @names.select {|x| cur.include?(x)}.each {|y| cnt += 1 if cur.index(y) < @priority}
          cur = cur - @names
          if command == "ADD"
            cur.insert(@priority - cnt, *@names).compact!
          end
          obj.send(cmethod, "REPLACE", cur, {})
        end
        return 0
      end
      
      private
      def find_method(sn, type="Store")
        ::Wallaroo::Client.const_get(type).instance_methods(false).map {|m| m.to_s}.grep(/#{sn}/)
      end
      
      def get_from_set(set_m)
        set_m.to_s =~ /^set(.+)/
        if $1 == nil
          set_m.to_s =~ /^modify(.+)/
        end
        if $1 == nil
          raise RuntimeError.new("Invalid set accessor #{set_m.inspect}")
        end
        getter = $1.gsub(/([A-Z][a-z]*)/, '\1_').chop
        getter.downcase.to_sym
      end
    end

    class AddParamConflict < Command
      include RelationshipOps

      def self.opname
        "add-conflicts-to-param"
      end
    
      def self.description
        "Add conflicts to a parameter in the store."
      end
    end

    class RemoveParamConflict < Command
      include RelationshipOps

      def self.opname
        "remove-conflicts-from-param"
      end
    
      def self.description
        "Remove conflicts from a parameter in the store."
      end
    end

    class ReplaceParamConflict < Command
      include RelationshipOps

      def self.opname
        "replace-conflicts-on-param"
      end
    
      def self.description
        "Replace the conflicts on a parameter in the store with a new set of conflicts."
      end
    end

    class AddParamDepend < Command
      include RelationshipOps

      def self.opname
        "add-dependencies-to-param"
      end
    
      def self.description
        "Add dependencies to a parameter in the store."
      end
    end

    class RemoveParamDepend < Command
      include RelationshipOps

      def self.opname
        "remove-dependencies-from-param"
      end
    
      def self.description
        "Remove dependencies from a parameter in the store."
      end
    end

    class ReplaceParamDepend < Command
      include RelationshipOps

      def self.opname
        "replace-dependencies-on-param"
      end
    
      def self.description
        "Replace the dependencies on a parameter in the store with a new set of dependencies."
      end
    end

    class AddFeatureConflict < Command
      include RelationshipOps

      def self.opname
        "add-conflicts-to-feature"
      end
    
      def self.description
        "Add conflicts to a feature in the store."
      end
    end

    class RemoveFeatureConflict < Command
      include RelationshipOps

      def self.opname
        "remove-conflicts-from-feature"
      end
    
      def self.description
        "Remove conflicts from a feature in the store."
      end
    end

    class ReplaceFeatureConflict < Command
      include RelationshipOps

      def self.opname
        "replace-conflicts-on-feature"
      end
    
      def self.description
        "Replace the conflicts on a feature in the store with a new set of conflicts."
      end
    end

    class AddFeatureDepend < Command
      include RelationshipOps

      def self.opname
        "add-dependencies-to-feature"
      end
    
      def self.description
        "Add dependencies to a feature in the store."
      end
    end

    class RemoveFeatureDepend < Command
      include RelationshipOps

      def self.opname
        "remove-dependencies-from-feature"
      end
    
      def self.description
        "Remove dependencies from a feature in the store."
      end
    end

    class ReplaceFeatureDepend < Command
      include RelationshipOps

      def self.opname
        "replace-dependencies-on-feature"
      end
    
      def self.description
        "Replace the dependencies on a feature in the store with a new set of dependencies."
      end
    end

    class AddFeatureInclude < Command
      include RelationshipOps

      def self.opname
        "add-includes-to-feature"
      end
    
      def self.description
        "Add includes to a feature in the store."
      end

      def supports_options
        true
      end
    end

    class RemoveFeatureInclude < Command
      include RelationshipOps

      def self.opname
        "remove-includes-from-feature"
      end
    
      def self.description
        "Remove includes from a feature in the store."
      end
    end

    class ReplaceFeatureInclude < Command
      include RelationshipOps

      def self.opname
        "replace-includes-on-feature"
      end
    
      def self.description
        "Replace the includes on a feature in the store with a new set of includes."
      end
    end

    class AddNodeFeature < Command
      include RelationshipOps

      def self.opname
        "add-features-to-node"
      end
    
      def self.description
        "Add features to a node in the store."
      end

      def supports_options
        true
      end
    end

    class RemoveNodeFeature < Command
      include RelationshipOps

      def self.opname
        "remove-features-from-node"
      end
    
      def self.description
        "Remove features from a node in the store."
      end
    end

    class ReplaceNodeFeature < Command
      include RelationshipOps

      def self.opname
        "replace-features-on-node"
      end
    
      def self.description
        "Replace the features on a node in the store with a new set of features."
      end
    end

    class AddGroupFeature < Command
      include RelationshipOps

      def self.opname
        "add-features-to-group"
      end
    
      def self.description
        "Add features to a group in the store."
      end

      def supports_options
        true
      end
    end

    class RemoveGroupFeature < Command
      include RelationshipOps

      def self.opname
        "remove-features-from-group"
      end
    
      def self.description
        "Remove features from a group in the store."
      end
    end

    class ReplaceGroupFeature < Command
      include RelationshipOps

      def self.opname
        "replace-features-on-group"
      end
    
      def self.description
        "Replace the features on a group in the store with a new set of features."
      end
    end

    class AddNodeParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "add-params-to-node"
      end
    
      def self.description
        "Add parameters to a node in the store."
      end
    end

    class RemoveNodeParam < Command
      include RelationshipOps
      include HashArgType

      def self.opname
        "remove-params-from-node"
      end
    
      def self.description
        "Remove parameters from a node in the store."
      end
    end

    class ReplaceNodeParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "replace-params-on-node"
      end
    
      def self.description
        "Replace the parameters on a node in the store with a new set of parameters."
      end
    end

    class AddGroupParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "add-params-to-group"
      end
    
      def self.description
        "Add parameters to a group in the store."
      end
    end

    class RemoveGroupParam < Command
      include RelationshipOps
      include HashArgType

      def self.opname
        "remove-params-from-group"
      end
    
      def self.description
        "Remove parameters from a group in the store."
      end
    end

    class ReplaceGroupParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "replace-params-on-group"
      end
    
      def self.description
        "Replace the parameters on a group in the store with a new set of parameters."
      end
    end

    class AddFeatureParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "add-params-to-feature"
      end
    
      def self.description
        "Add parameters to a feature in the store."
      end
    end

    class RemoveFeatureParam < Command
      include RelationshipOps
      include HashArgType

      def self.opname
        "remove-params-from-feature"
      end
    
      def self.description
        "Remove parameters from a feature in the store."
      end
    end

    class ReplaceFeatureParam < Command
      include RelationshipOps
      include ParamOps

      def self.opname
        "replace-params-on-feature"
      end
    
      def self.description
        "Replace the parameters on a feature in the store with a new set of parameters."
      end
    end

    class AddSubsysParam < Command
      include RelationshipOps

      def self.opname
        "add-params-to-subsystem"
      end
    
      def self.description
        "Add parameters to a subsystem in the store."
      end
    end

    class RemoveSubsysParam < Command
      include RelationshipOps

      def self.opname
        "remove-params-from-subsystem"
      end
    
      def self.description
        "Remove parameters from a subsystem in the store."
      end
    end

    class ReplaceSubsysParam < Command
      include RelationshipOps

      def self.opname
        "replace-params-on-subsystem"
      end
    
      def self.description
        "Replace the parameters on a subsystem in the store with a new set of parameters."
      end
    end

    class AddNodeMembership < Command
      include RelationshipOps
      include NodeMembershipOps

      def self.opname
        "add-node-memberships"
      end
    
      def self.description
        "Add group memberships to a node in the store."
      end

      def supports_options
        true
      end
    end

    class RemoveNodeMembership < Command
      include RelationshipOps
      include NodeMembershipOps

      def self.opname
        "remove-node-memberships"
      end
    
      def self.description
        "Remove group memberships from a node in the store."
      end
    end

    class ReplaceNodeMembership < Command
      include RelationshipOps
      include NodeMembershipOps

      def self.opname
        "replace-node-memberships"
      end
    
      def self.description
        "Replace group memberships on a node in the store with a new set of group memberships."
      end
    end

    class AddNodeGroup < Command
      include RelationshipOps
      include GroupMembershipOps

      def self.opname
        "add-nodes-to-group"
      end
    
      def self.description
        "Add nodes to a group in the store."
      end
    end

    class RemoveNodeGroup < Command
      include RelationshipOps
      include GroupMembershipOps

      def self.opname
        "remove-nodes-from-group"
      end
    
      def self.description
        "Remove nodes from a group in the store."
      end
    end
  end
end
