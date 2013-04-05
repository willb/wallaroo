# param:  wallaby shell param crud functionality
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

require 'wallaroo/shell/entity_ops'

module Wallaroo
  module Shell
    module ParamOps
      def api_messages
        @api_messages ||= {:kind=>:setKind, :default_val=>:setDefault, :description=>:setDescription, :annotation=>:setAnnotation, :must_change=>:setMustChange, :level=>:setVisibilityLevel, :needsRestart=>:setRequiresRestart}.freeze
      end

      def api_accessors
        @api_accessors ||= [:kind, :default, :description, :must_change, :requires_restart, :visibility_level, :depends, :conflicts, :annotation]
      end

      def accessor_options
        @accessor_options ||= {:kind=>String, :default_val=>String, :description=>String, :must_change=>{"yes"=>true, "no"=>false, "YES"=>true, "NO"=>false}, :level=>Integer, :needsRestart=>{"yes"=>true, "no"=>false, "YES"=>true, "NO"=>false}, :annotation=>String}
      end

      def noun
        "parameter"
      end

    end          

    class AddParam < Command
      include EntityOps
      include ParamOps
      
      def self.opname
        "add-param"
      end

      def self.description
        "Adds a parameter to the store."
      end

      def storeop
        :addParam
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class ModifyParam < Command
      include EntityOps
      include ParamOps
      
      def self.opname
        "modify-param"
      end

      def self.description
        "Alters metadata for a parameter in the store."
      end

      def storeop
        :getParam
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class RemoveParam < Command
      include EntityOps
      include ParamOps
      
      def self.opname
        "remove-param"
      end
      
      def self.description
        "Deletes a parameter from the store."
      end
      
      def supports_options
        false
      end
      
      def storeop
        :removeParam
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class ShowParam < Command
      include EntityOps
      include ParamOps
      documented_only_if_default_name

      def self.opname
        "show-param"
      end
      
      def self.description
        "Displays metadata about a parameter in the store."
      end
      
      def supports_options
        false
      end
      
      def storeop
        :getParam
      end
      
      def show_banner
        false
      end

      def entity_callback(param)
        puts "#{param.name}"
        api_accessors.each do |k|
          puts "  #{k}:  #{param.send(k).inspect}"
        end
      end

      register_callback :after_option_parsing, :post_arg_callback

      Wallaroo::Shell.register_command(self, opname + "s")
    end

    class ListParam < Command
      def self.opname
        "list-params"
      end

      def self.opargs
        ""
      end

      def self.description
        "Lists all the parameter names in the store."
      end

      def supports_options
        false
      end

      def act
        store.objects_of_type("Parameter").each do |param|
          puts "#{param.name}"
        end
        0
      end
    end
  end
end
