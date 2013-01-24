# Copyright (c) 2012 Red Hat, Inc.
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

require 'wallaroo/client'

module Wallaroo
  module Client
    class Store
      include ::Wallaroo::Client::Util
      
      attr_reader :cm
      
      def initialize(cm)
        @cm = cm
      end
      
      [:Feature, :Group, :Node, :Parameter, :Subsystem].each do |klass|
        mname = "check#{klass}Validity".to_sym
        # XXX: probably best to use REST API list-entity support
        define_method mname.to_sym do |eset|
          eset.collect([]) do |entity, acc| 
            acc << entity unless cm.make_proxy_object(klass, entity).exists?
          end
        end
      end
        
      [[:getDefaultGroup, "+++DEFAULT"], [:getSkeletonGroup, "+++SKEL"]].each do |msg, grp|
        define_method msg do
          cm.make_proxy_object(:group, grp)
        end
      end
      
      [:Feature, :Group, :Node, :Parameter, :Subsystem].each do |klass|
        define_method "#{klass.to_s.downcase}s" do
          not_implemented
        end
      end
      
      def set_user_privs(user, role, options=nil)
        options||={}
        not_implemented
      end

      def del_user(user, options=nil)
        options||={}
        not_implemented
      end

      def users(options=nil)
        options||={}
        not_implemented
      end

      def getGroup(query)
        name = query["NAME"] || query["name"]
        fatal("invalid query type #{query.inspect}") unless name
        
        result = cm.make_proxy_object(:group, name)        
        result.refresh
        
        result
      end
        
      def getGroupByName(name)
        getGroup("name"=>name)
      end
        
      def addExplicitGroup(name)
        result = cm.make_proxy_object(:group, name)
        fatal("group #{name} already exists") if result.exists?
        result.create!
        result
      end

      def getExplicitGroup(name)
        getGroup({"NAME"=>name})
      end

      def removeGroup(uid)
        not_implemented
      end

      def getFeature(name)
        result = cm.make_proxy_object(:feature, name)
        result.refresh
      end

      def addFeature(name)
        result = cm.make_proxy_object(:feature, name)
        fatal("feature #{name} already exists") if result.exists?
        result.create!
      end

      def removeFeature(name)
        not_implemented
      end

      def addNode(name, options=nil)
        options ||= {}
        result = cm.make_proxy_object(:node, name)
        if result.exists?
          result.makeProvisioned
          result.refresh
        else
          result.create!
        end
      end

      alias addNodeWithOptions addNode

      def getNode(name)
        result = cm.make_proxy_object(:node, name)
        if result.exists?
          result.refresh
        else
          result.provisioned = false
          result.create!
        end
      end

      def removeNode(name)
        not_implemented
      end

      def affectedEntities(options=nil)
        options ||= {}
        not_implemented
      end

      def affectedNodes(options=nil)
        options ||= {}
        not_implemented
      end

      def addParam(name)
        result = cm.make_proxy_object(:parameter, name)
        fatal("parameter #{name} already exists") if result.exists?
        result.create!
        result
      end

      def getParam(name)
        result = cm.make_proxy_object(:parameter, name)
        result.refresh
      end

      def getMustChangeParams
        not_implemented
      end

      def removeParam(name)
        not_implemented
      end

      def addSubsys(name)
        result = cm.make_proxy_object(:subsystem, name)
        fatal("subsystem #{name} already exists") if result.exists?
        result.create!
        result
      end

      def getSubsys(name)
        result = cm.make_proxy_object(:subsystem, name)
        result.refresh
      end

      def removeSubsys(name)
        not_implemented
      end
        
      def activateConfig
        not_implemented
      end
        
      alias activateConfiguration activateConfig

      def makeSnapshotWithOptions(name, options=nil)
        options ||= {}
        not_implemented
      end
        
      alias makeSnapshot makeSnapshotWithOptions
        
      def loadSnapshot(name)
        not_implemented
      end

      def removeSnapshot(name)
        not_implemented
      end

      def storeinit(kwargs=nil)
        nil
      end
      
    end
  end
end
      