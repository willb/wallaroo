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

require 'time'

module Wallaroo
  module Client
    
    class Node
      include ::Wallaroo::Client::Proxying      
      
      [:name, :memberships, :identity_group].each do |what|
        # XXX: distinguish sensibly between readonly and read-write attributes
        declare_attribute what
      end
      
      declare_attribute :provisioned
      
      def identity_group
        result = cm.make_proxy_object(:group, attr_vals[:identity_group])
        result.refresh
        result
      end
      
      def last_updated_version
        attr_vals["last_updated_version"]
      end
      
      def checkin
        ts = timestamp
        metapath = "/meta/node/#{URI.encode(self.name)}"
        meta = cm.fetch_json_resource(metapath, "", {})
        meta["last-checkin"] = ts
        cm.put_json_resource(metapath, meta, true)
        ts
      end
      
      def last_checkin
        meta = cm.fetch_json_resource("/meta/node/#{URI.encode(self.name)}", "", {})
        meta["last-checkin"] || 0
      end
      
      def modifyMemberships(command, groups, options=nil)
        options ||= {}
        case command.upcase
        when "ADD" then
          self.memberships << groups
          self.memberships.uniq!
          update!
        when "REMOVE" then
          self.memberships = self.memberships - groups
          update!
        when "REPLACE" then
          self.memberships = groups
          update!
        else 
          # XXX: use a real client library exception here
          raise "modifyMemberships command #{command} not implemented"
        end
      end
      
      def getConfig(options=nil)
        options ||= {}
        if options["version"]
          return cm.fetch_json_resource("/config/node/#{URI.encode(self.name)}", "commit=#{options["version"]}", {}).to_a
        end
        
        cm.fetch_json_resource("/config/node/#{URI.encode(self.name)}")
      end
      
      def whatChanged(old_version, new_version)
        # XXX: this identifies must_change and subsystem properties as of the current connection meta information -- not as of new_version
        store = Store.new(cm)
        old_config = cm.fetch_json_resource("/config/node/#{URI.encode(self.name)}", "commit=#{old_version}").to_a
        new_config = cm.fetch_json_resource("/config/node/#{URI.encode(self.name)}", "commit=#{new_version}").to_a

        params = ((old_config - new_config) + (new_config - old_config)).map {|x,y| x}.uniq - ["WALLABY_CONFIG_VERSION"]
        mc_params = params.select {|p| store.getParam(p).refresh.must_change }
        
        subsystems = cm.list_objects(:subsystem).map {|kln| cm.make_proxy_object("Subsystem", kln).refresh}

        restart, reconfig = subsystems.inject([[],[]]) do |(rs, rc), sub|
          if sub.params & mc_params != []
            rs << sub.name
          elsif sub.params & params != []
            rc << sub.name
          end
          [rs, rc]
        end
        
        [params, restart, reconfig]
      end

      def explain
        # TODO
        not_implemented
      end

      def makeProvisioned
        self.provisioned = true
        update!
      end

      def makeUnprovisioned
        self.provisioned = false
        update!
      end
      
      private
      def timestamp(tm=nil)
        tm ||= Time.now.utc
        (tm.tv_sec * 1000000) + tm.tv_usec
      end
    end
  end
end