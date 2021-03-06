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
    class Subsystem
      include ::Wallaroo::Client::Proxying      
      include ::Wallaroo::Client::ArcUtils
      
      [:name, :parameters].each do |what|
        # XXX: distinguish sensibly between readonly and read-write attributes
        declare_attribute what
      end

      alias params parameters

      def modifyParams(command, pset, options=nil)
        options ||= {}
        modify_arcs(command,pset,options,:parameters,:parameters=,:explain=>"is interested in")
        update!
      end
    end
  end
end
      