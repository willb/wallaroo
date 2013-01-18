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
    class Parameter
      include ::Wallaroo::Client::Proxying      
      include ::Wallaroo::Client::ArcUtils
      
      [:name, :depends, :conflicts, :kind, :description, :default_val, :must_change, :visibility_level, :requires_restart].each do |what|
        # XXX: distinguish sensibly between readonly and read-write attributes
        declare_attribute what
      end
      
      [:name, :kind, :description, :default_val, :must_change, :visibility_level, :requires_restart].each do |what|
        define_method "set#{Util.camelcase(what.to_s)}" do |val|
          self.attr_vals[what] = val
          self.update!
          self.refresh
        end
      end
      
      [[:depends, "depends on"], [:conflicts, "conflicts with"]].each do |what, explain|
        define_method "modify#{Util.camelcase(what.to_s)}" do |command, pset, *options|
          options = options[0] || {}
          modify_arcs(command,pset,options,what,"#{what}=",:explain=>explain)
          update!
        end
      end
    end
  end
end
      