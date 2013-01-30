# node:  wallaby shell node crud functionality
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
    module NodeOps
      def api_messages
        @api_messages ||= {:annotation=>:setAnnotation}.freeze
      end

      def api_accessors
        @api_accessors ||= [:name, :provisioned, :last_checkin, :last_updated_version, :memberships, :annotation]
      end

      def accessor_options
        @accessor_options ||= {:annotation=>String}
      end

      def supports_options
        true
      end

      def noun
        "node"
      end
    end

    class AddNode < Command
      include EntityOps
      include NodeOps
      
      def self.opname
        "add-node"
      end

      def self.description
        "Adds a node to the store."
      end

      def storeop
        :addNode
      end

      register_callback :after_option_parsing, :post_arg_callback
    end

    class ModifyNode < Command
      include EntityOps
      include NodeOps
      
      def self.opname
        "modify-node"
      end

      def self.description
        "Alters metadata for a node in the store."
      end

      def storeop
        :getNode
      end
      
      def supports_options
        true
      end

      def multiple_targets
        false
      end
      
      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class RemoveNode < Command
      include EntityOps
      include NodeOps
      
      def self.opname
        "remove-node"
      end
      
      def self.description
        "Deletes a node from the store."
      end
      
      def storeop
        :removeNode
      end

      register_callback :after_option_parsing, :post_arg_callback
    end
    
    class ShowNode < Command
      include EntityOps
      include NodeOps
      documented_only_if_default_name

      def self.opname
        "show-node"
      end
      
      def self.description
        "Displays the properties of a node."
      end
      
      def storeop
        :getNode
      end
      
      def show_banner
        false
      end

      def entity_callback(node)
        puts "#{node.name}"
        api_accessors.each do |k|
          puts "  #{k}:  #{node.send(k).inspect}"
        end
        id_group = node.identity_group
        [:features, :params].each do |k|
          puts "  #{k}:  #{id_group.send(k).inspect}"
        end
      end

      register_callback :after_option_parsing, :post_arg_callback

      Wallaroo::Shell.register_command(self, opname + "s")
    end

    class ShowNodeConfig < Command
      include EntityOps
      include NodeOps
      documented_if_environment_has :WALLABY_TECH_PREVIEW

      def self.opname
        "show-node-config"
      end
      
      def self.description
        "Displays the configuration of a node."
      end
      
      def storeop
        :getNode
      end
      
      def show_banner
        false
      end

      def multiple_targets
        false
      end

      def supports_options
        false
      end

      def init_option_parser
        @options = {}
        OptionParser.new do |opts|
          opts.banner = "Usage:  wallaby #{self.class.opname} NODE [OPTIONS]"

          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end

          opts.on("--version VERSION", Integer, "shows this node's configuration at VERSION") do |ver|
            @options[:latest] = ver
          end
        end
      end

      def entity_callback(node)
        ver = (@options.has_key?(:latest) ? {"version"=>@options[:latest]} : {})
        # XXX: should this be in a friendlier format for human readers?  or a particular machine-readable format?
        puts "  config:  #{node.getConfig(ver).inspect}"
      end

      register_callback :after_option_parsing, :post_arg_callback

    end

    class ListNode < Command
      include EntityOps
      include NodeOps

      def self.opname
        "list-nodes"
      end

      def self.opargs
        ""
      end

      def self.description
        "Lists all the node names in the store."
      end

      def act
        store.console.objects(:class=>"Node").each do |node|
          puts "#{node.name}"
        end
        0
      end
    end
  end
end