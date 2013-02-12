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
        not_implemented unless options.empty?
        
        cm.fetch_json_resource("/config/node/#{URI.encode(self.name)}")
      end
      
      def whatChanged(old_version, new_version)
        # TODO
        not_implemented
      end

      def explain
        # TODO
        not_implemented
      end

      def makeProvisioned
        self.provisioned = true
        update!
      end
    end
  end
end