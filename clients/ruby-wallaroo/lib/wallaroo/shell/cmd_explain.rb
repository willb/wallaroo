# explain:  where does this node's configuration come from?
#
# Copyright (c) 2009--2010 Red Hat, Inc.
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
    class Explain < Command
      def self.opname
        "explain"
      end
      
      def self.description
        "Outputs an annotated display of a node's current configuration."
      end
      
      def init_option_parser
        OptionParser.new do |opts|
          
          opname = "explain"
          
          opts.banner = "Usage:  wallaby #{opname} nodename\nOutputs an annotated display of this node's configuration."
            
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
      
      def check_args(*args)
        if args.length == 0
          puts "fatal:  you must specify at least one node to explain"
        end
        
        @nodes = args.dup
      end
      
      register_callback :after_option_parsing, :check_args
      
      def act
        result = 0
        
        fail("not implemented for wallaroo")
        
        invalid_nodes = store.checkNodeValidity(@nodes)
        if invalid_nodes.size > 0
          invalid_nodes.each do |n|
            puts "warning:  node #{n} does not exist"
            @nodes.delete(n)
            result = 1
          end
        end
        
        @nodes.each do |node|
          node = store.getNode(node)
          
          exp = explain_one_node(node)
          config = node.getConfig
          
          puts "### Explaining the configuration for #{node.name}"
          node.memberships.reverse_each do |m|
            puts "###  -- which is a member of group #{m}"
          end
          
          config.keys.sort.each do |param|
            value = config[param]
            puts "# #{param} #{exp[param] || "has no explanation"}"
            puts "#{param} = #{value}"
          end
          
        end
        
        result
      end
      
      private
      def explain_one_node(node)
        explanation = {"WALLABY_CONFIG_VERSION"=>"is set automatically"}
        
        memberships = [store.getDefaultGroup] + node.memberships.reverse.map {|gn| store.getGroupByName(gn)} + [node.identity_group]
        
        memberships.each do |group|
          group.features.each do |f|
            explain_one_feature(f, ", which is installed on #{group.display_name}", explanation)
          end
          
          group.params.keys.each do |param|
            explanation[param] = "is set explicitly in #{group.display_name}"
          end
        end
        
        explanation
      end
      
      def explain_one_feature(f, suffix, explanation)
        f = store.getFeature(f)
        
        f.included_features.reverse_each do |inc|
          explain_one_feature(inc, ", which is included by #{f.name}#{suffix}", explanation)
        end
        
        f.param_meta.each do |param, meta_map|
          explanation[param] = "#{meta_map['uses_default'] ? "is set to use its default value in" : "is explicitly set in"} #{f.name}#{suffix}"
        end
      end
    end
  end
end