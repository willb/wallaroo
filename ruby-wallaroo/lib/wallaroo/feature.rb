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
    class Feature
      include ::Wallaroo::Client::Proxying      
      include ::Wallaroo::Client::ArcUtils
      
      [:name, :includes, :conflicts, :depends, :parameters].each do |what|
        # XXX: distinguish sensibly between readonly and read-write attributes
        declare_attribute what
      end
      
      alias included_features includes
      alias included_features= includes=

      [:name].each do |what|
        define_method "set#{Util.camelcase(what.to_s)}" do |val|
          self.attr_vals[what] = val
          self.update!
          self.refresh
        end
      end

      [[:included_features, "includes", true], 
       [:depends, "depends on", false], 
       [:conflicts, "conflicts with", false]].each do |what, explain, order_preserving|
        define_method "modify#{Util.camelcase(what.to_s)}" do |command, fset, *options|
          options = options[0] || {}
          modify_arcs(command,fset,options,what,"#{what}=",:explain=>explain, :preserve_order=>order_preserving)
          update!
        end
      end

      def modifyParams(command, params, options=nil)
        options ||= {}
        case command.upcase
        when "ADD" then
          self.parameters = self.parameters.merge(params)
        when "REMOVE" then
          self.parameters = self.parameters.reject {|k,v| params.include?(k) }
        when "REPLACE" then
          self.parameters = params
        else 
          fail(Errors.make(Errors::BAD_COMMAND, errwhat), "Invalid command #{command}")
        end
        update!
      end
    end
  end
end
      