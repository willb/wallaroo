# config-proxies.rb:  support code for wallaby dump and restore
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

require 'set'
require 'yaml'
require 'cgi'
require 'fileutils'

module Wallaroo
  module SerializedConfigs 
  end
end

module Mrg 
  module Grid
    SerializedConfigs = ::Wallaroo::SerializedConfigs
  end
end

module Wallaroo
  module SerializedConfigs
    module DBHelpers
      def create_relationships
        log.info("Creating relationships between store entities")
        @callbacks.each {|cb| cb.call}
        @callbacks = []
      end
      
      def dictify(ls)
        Hash[*ls.map {|obj| [obj.name, obj]}.flatten]
      end
    end

    class MsgSink
      def method_missing(sym, *args)
        nil
      end
    end
    
    module DefaultStruct
      module Cm
        def saved_fields
          @saved_fields ||= {}
        end
        
        def field(name, kind=String)
          saved_fields[name] = kind
          attr_accessor name
        end
      end
      
      module Im
        def initialize(kwargs=nil)
          kwargs ||= {}
          sf = self.class.saved_fields

          sf.keys.each do |key|
            if kwargs[key]
              self.send("#{key}=".to_sym, kwargs[key])
            else
              what = sf[key]
              default = what.is_a?(Class) ? what.new : what
              self.send("#{key}=".to_sym, default)
            end
          end
        end
      end
      
      def self.included(receiver)
        receiver.extend         Cm
        receiver.send :include, Im
      end
    end
    
    # sort of bogus, not included in DefaultStruct in production.  Perhaps useful for testing
    module DSComparators
      def eql?(other)
        return false unless self.class == other.class
        self == other
      end
      
      def <=>(other)
        return nil unless self.class == other.class
        
        self.class.saved_fields.keys.each do |msg|
          mine = self.send(msg)
          yours = self.send(msg)
          $submine = mine
          $subyours = yours
          
          if mine.respond_to? :<=>
            result = mine <=> yours
            return result unless result == 0
          else
            result = (mine == yours || nil)
            return result if result.nil?
          end
          
          0
        end
      end
      
      def ==(other)
        self.class.saved_fields.each do |msg,default|
          mine = self.send(msg)
          yours = self.send(msg)
          
          if default == Set
            mine = (mine.to_a.sort_by {|s| s.name} rescue Set[*mine.to_a])
            yours = (yours.to_a.sort_by {|s| s.name} rescue Set[*yours.to_a])
          end
          
          unless mine == yours
            return false
          end
        end
        
        true
      end
      
      def hash
        result = {'PROXY_CLASSNAME'=>self.class.name}
        
        self.class.saved_fields.each do |msg,default|
          result[msg] = default == Set ? default[*self.send(msg)] : self.send(msg)
        end

        result.hash
      end
    end
    
    class Configuration 
      def initialize
        raise "Configuration proxy not implemented"
      end
    end

    class Store
      include DefaultStruct
      field :nodes, Set
      field :groups, Set
      field :params, Set
      field :features, Set
      field :subsystems, Set
    end

    class Feature
      include DefaultStruct
      field :name, String
      field :params, Hash
      field :included, Array
      field :conflicts, Set
      field :depends, Array
      field :annotation, ""
    end
    
    class Group
      include DefaultStruct
      field :name, String
      field :is_identity_group, false
      field :features, Array
      field :params, Hash
      field :annotation, ""
    end
    
    class Parameter
      include DefaultStruct
      field :name, String
      field :kind, String
      field :default_val, String
      field :description, String
      field :must_change, false
      field :level, 0
      field :needs_restart, false
      field :conflicts, Set
      field :depends, Set
      field :annotation, ""
    end
    
    class Node
      include DefaultStruct
      field :name, String
      field :idgroup, String
      field :membership, Array
      field :provisioned, true
      field :last_updated_version, 0
      field :annotation, ""
    end        
    
    class Subsystem
      include DefaultStruct
      field :name, String
      field :params, Set
      field :annotation, ""
    end
    
    module ConfigTextParsing
      def parse(ymltxt)
        begin
          
          yrepr = YAML::parse(ymltxt).transform
          
          raise RuntimeError.new("serialized object not of the correct type") if not (yrepr.is_a?(::Mrg::Grid::SerializedConfigs::Store) or yrepr.is_a?(::Wallaroo::SerializedConfigs::Store))
          
          @nodes = dictify(yrepr.nodes)
          @groups = dictify(yrepr.groups)
          @params = dictify(yrepr.params)
          @features = dictify(yrepr.features)
          @subsystems = dictify(yrepr.subsystems)
          
          @callbacks = []
        rescue Exception=>ex
          raise RuntimeError.new("Invalid snapshot file; #{ex.message}")
        end
      end
    end            

    class ConfigLoader
      include DBHelpers
      include ConfigTextParsing

      module InternalHelpers
        def listify(ls)
          ls
        end
       
        def setify(s)
          s.uniq
        end
      end
    
      module QmfHelpers
        def listify(ls)
          ls
        end
        
        def setify(ls)
          ls
        end
      end
    
      def ConfigLoader.log
        @log ||= MsgSink.new
      end
      
      def ConfigLoader.log=(lg)
        @log = lg
      end
      
      def log
        self.class.log
      end
      
      def initialize(store, ymltxt)
        @store = store
       
        # operating over QMF via the config-client lib
        log.debug "ConfigLoader IS operating over QMF"
        class << self
          include QmfHelpers
        end
        
        yrepr = nil

        parse(ymltxt)
        
      end
      
      def load
        create_entities
        create_relationships
      end
      
      private
      def create_entities
        create_nodes
        create_groups
        create_params
        create_features
        create_subsystems
      end
      
      def create_nodes
        @nodes.each do |name, old_node|
          log.info("Creating node '#{name}'")
          options = {}
          # options["seek_versioned_config"] = old_node.last_updated_version if old_node.last_updated_version && old_node.last_updated_version > 0
          node = @store.addNodeWithOptions(name, options)
          node.makeUnprovisioned unless (old_node.provisioned)
          node.setAnnotation(old_node.annotation) rescue nil
          
          memberships = old_node.membership
          if memberships.size > 0
            flmemberships = listify(memberships)
            @callbacks << lambda do
              node = @store.getNode(name)
              log.info("Setting memberships for node #{name}")
              log.debug("Node #{name} has memberships #{flmemberships.inspect}")
              node.modifyMemberships("ADD", flmemberships, {})
            end
          end
        end
      end
      
      def create_groups
        @groups.each do |name, old_group|
          group = nil
          if name.index("+++") == 0
            # this is an identity or default group; don't create it
            log.info("Finding special group '#{name}'")
            group = @store.getGroup({"NAME"=>name})
          else
            log.info("Creating group '#{name}'")
            group = @store.addExplicitGroup(name)
          end
          
          if old_group.features.size > 0
            flfeatures = listify(old_group.features)
            @callbacks << lambda do
              group=@store.getGroup({"NAME"=>name})
              log.info("Setting features for group #{name}")
              log.debug("Group #{name} has features #{flfeatures.inspect}")
              group.modifyFeatures("ADD", flfeatures, {})
            end
          end
          
          if old_group.params.size > 0
            @callbacks << lambda do
              group=@store.getGroup({"NAME"=>name})
              log.info("Setting params for group #{name}")
              log.debug("Group #{name} has params #{old_group.params.inspect}")
              group.modifyParams("ADD", old_group.params, {})
            end
          end
          
          group.setAnnotation(old_group.annotation)  rescue nil
        end
      end
      
      def create_params
        @params.each do |name, old_param|
          param = @store.cm.make_proxy_object(:parameter, name)
          log.info "Creating parameter '#{name}'"
          
          param.setKind(old_param.kind)
          param.setDefault(old_param.default_val) unless old_param.must_change
          param.setDescription(old_param.description)
          param.setMustChange(old_param.must_change)
          param.setVisibilityLevel(old_param.level)
          param.setRequiresRestart(old_param.needs_restart)
          param.setAnnotation(old_param.annotation) rescue nil
          param.create!

          {:conflicts=>:modifyConflicts,:depends=>:modifyDepends}.each do |get,set|
            if old_param.send(get).size > 0
              @callbacks << lambda do
                param = @store.getParam(name)
                log.info "Setting #{get} for parameter #{name}"
                log.debug "#{get.to_s.capitalize} for parameter #{name} are #{old_param.send(get).inspect}"
                params = setify(old_param.send(get))
                param.send(set, "ADD", params, {"skip_validation"=>"true"})
              end
            end
          end
        end
      end
      
      def create_features
        @features.each do |name, old_feature|
          log.info "Creating feature '#{name}'"
          feature = @store.addFeature(name)
          feature.setAnnotation(old_feature.annotation) rescue nil
          [[:params, :modifyParams, :skk, "parameters"],[:included, :modifyIncludedFeatures, :listify, "included features"],[:conflicts, :modifyConflicts, :setify, "conflicting features"],[:depends, :modifyDepends, :listify, "feature dependencies"]].each do |get,set,xform,desc|
            if old_feature.send(get).size > 0
              @callbacks << lambda do
                feature = @store.getFeature(name)
                log.info "Setting #{desc} for #{name}"
                log.debug "#{desc.capitalize} for #{name} are #{old_feature.send(get).inspect}"
                feature.send(set, "ADD", self.send(xform, old_feature.send(get)), {"skip_validation"=>"true"})
              end
            end
          end
        end
      end
      
      def create_subsystems
        @subsystems.each do |name, old_ss|
          log.info "Creating subsystem '#{name}'"
          subsys = @store.addSubsys(name)
          subsys.setAnnotation(old_ss.annotation)  rescue nil
          if old_ss.params.size > 0
            @callbacks << lambda do
              subsys = @store.getSubsys(name)
              log.info "Setting parameters for subsystem #{name}"
              log.debug "Subsystem #{name} has parameters #{old_ss.params.inspect}"
              subsys.modifyParams("ADD", setify(old_ss.params), {})
            end
          end
        end
      end
      
      def skk(x)
        x
      end
    end
    
    class ConfigSerializer
      module QmfConfigSerializer
        # this is a no-op if we're using ConfigClients
        def get_object(o)
          o
        end
        
        # XXX:  as of recently, this (like the analogous method in
        # InStoreConfigSerializer) duplicates effort with methods
        # in the Store client class
        def get_instances(klass)
          @console.objects(:class=>klass.to_s, :timeout=>45).map do |obj|
            ::Wallaroo::Client.const_get(klass).new(obj, @console)
          end
        end
      end
      
      def initialize(store, over_qmf=false, console=nil)
        @store = store
        @console = console if over_qmf
        @struct = Store.new
        
        if over_qmf
          class << self
            include QmfConfigSerializer
          end
        else
          class << self
            include InStoreConfigSerializer
          end
        end
      end
      
      def serialize
        @struct.nodes = serialize_nodes.sort_by {|o| o.name}
        @struct.groups = serialize_groups.sort_by {|o| o.name}
        @struct.params = serialize_params.sort_by {|o| o.name}
        @struct.features = serialize_features.sort_by {|o| o.name}
        @struct.subsystems = serialize_subsystems.sort_by {|o| o.name}
        @struct
      end
      
      private
      def serialize_nodes
        get_instances(:Node).map do |n|
          node = get_object(n)
          out = Node.new
          out.name = node.name
          out.provisioned = node.provisioned
          out.membership = fl_normalize(node.memberships)
          out.last_updated_version = node.last_updated_version
          out.annotation = node.annotation
          out
        end
      end
      
      def serialize_groups
        get_instances(:Group).map do |g|
          group = get_object(g)
          out = Group.new
          out.name = group.name
          out.is_identity_group = group.is_identity_group
          out.features = fl_normalize(group.features)
          out.params = group.params
          out.annotation = group.annotation
          out
        end
      end
      
      def serialize_params
        get_instances(:Parameter).map do |p|
          param = get_object(p)
          out = Parameter.new
          out.name = param.name
          out.kind = param.kind
          out.default_val = param.default.to_s
          out.description = param.description
          out.must_change = param.must_change
          out.level = param.visibility_level
          out.needs_restart = param.requires_restart
          out.conflicts = fs_normalize(param.conflicts)
          out.depends = fs_normalize(param.depends)
          out.annotation = param.annotation
          out
        end
      end
      
      def serialize_features
        get_instances(:Feature).map do |f|
          feature = get_object(f)
          out = Feature.new
          out.name = feature.name
          params = feature.params
          out.annotation = feature.annotation
          
          # Ensure that params that should get the default values are serialized
          default_params = feature.param_meta.select {|k,v| v["uses_default"] == true || v["uses_default"] == 1}.map {|pair| pair[0]}
          default_params.each {|dp_key| params[dp_key] = 0}
          
          out.params = params
          out.included = fl_normalize(feature.included_features)
          out.conflicts = fs_normalize(feature.conflicts)
          out.depends = fl_normalize(feature.depends)
          out
        end
      end
      
      def serialize_subsystems
        get_instances(:Subsystem).map do |s|
          subsys = get_object(s)
          out = Subsystem.new
          out.name = subsys.name
          out.annotation = subsys.annotation
          out.params = fs_normalize(subsys.params)
          out
        end
      end
      
      def fs_normalize(fs)
        fs.sort
      end
      
      def fl_normalize(fl)
        fl
      end
    end
  end
end
