# group:  wallaby shell group crud functionality
#
# Copyright (c) 2011 Red Hat, Inc.
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

require 'wallaroo/shell/entity_ops'

module Wallaroo
  module Shell
    module GroupOps
      def api_messages
        @api_messages ||= {:name=>:setName, :annotation=>:setAnnotation}.freeze
      end

      def api_accessors
        @api_accessors ||= [:name, :membership, :features, :params, :annotation]
      end

      def accessor_options
        @accessor_options ||= {:annotation=>String}
      end
      
    end

    class AddGroup < Command
      include EntityOps
      include GroupOps
      
      def self.opname
        "add-group"
      end

      def self.description
        "Adds a group to the store."
      end

      def storeop
        :addExplicitGroup
      end

      register_callback :after_option_parsing, :post_arg_callback
    end

    class ModifyGroup < Command
      include EntityOps
      include GroupOps
      
      def self.opname
        "modify-group"
      end
      
      def self.description
        "Alters metadata for a group in the store."
      end
      
      def storeop
        :getGroupByName
      end
      
      def supports_options
        true
      end
      
      def multiple_targets
        false
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
   
    class RemoveGroup < Command
      include EntityOps
      include GroupOps
      
      def self.opname
        "remove-group"
      end
      
      def self.description
        "Deletes a group from the store."
      end

      def storeop
        :removeGroup
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class ShowGroup < Command
      include EntityOps
      include GroupOps
      documented_only_if_default_name
      
      def self.opname
        "show-group"
      end
      
      def self.description
        "Displays the properties of a group."
      end
      
      def storeop
        :getGroupByName
      end
      
      def entity_callback(group)
        puts "#{group.name}"
        api_accessors.each do |k|
          puts "  #{k}:  #{group.send(k).inspect}"
        end
      end

      register_callback :after_option_parsing, :post_arg_callback

      Wallaroo::Shell.register_command(self, opname + "s")
    end

    class ListGroup < Command
      include EntityOps
      include GroupOps
      
      def self.opname
        "list-groups"
      end

      def self.opargs
        ""
      end

      def self.description
        "Lists all the group names in the store."
      end

      def act
        store.objects_of_type("Group").each do |group|
          if not group.is_identity_group
            if group.name =~ /^[+]{3}/
              puts "#{group.display_name} #{group.name}"
            else
              puts "#{group.name}"
            end
          end
        end
        0
      end
    end
  end
end