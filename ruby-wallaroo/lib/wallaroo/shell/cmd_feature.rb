# feature:  wallaby shell feature crud functionality
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

require 'mrg/grid/config/shell/entity_ops'

module Mrg
  module Grid
    module Config
      module Shell
        module FeatureOps
          def api_messages
            @api_messages ||= {:name=>:setName, :annotation=>:setAnnotation}.freeze
          end

          def api_accessors
            @api_accessors ||= [:name, :params, :depends, :conflicts, :included_features, :annotation]
          end

          def accessor_options
            @accessor_options ||= {:annotation=>String}
          end
          
        end

        class AddFeature < Command
          include EntityOps
          include FeatureOps
          
          def self.opname
            "add-feature"
          end

          def self.description
            "Adds a feature to the store."
          end

          def storeop
            :addFeature
          end

          register_callback :after_option_parsing, :post_arg_callback
        end
        
        class ModifyFeature < Command
          include EntityOps
          include FeatureOps
          
          def self.opname
            "modify-feature"
          end

          def self.description
            "Alters metadata for a feature in the store."
          end

          def storeop
            :getFeature
          end
          
          def supports_options
            true
          end

          def multiple_targets
            false
          end

          register_callback :after_option_parsing, :post_arg_callback
        end
        
        class RemoveFeature < Command
          include EntityOps
          include FeatureOps
          
          def self.opname
            "remove-feature"
          end
          
          def self.description
            "Deletes a feature from the store."
          end
          
          def storeop
            :removeFeature
          end

          register_callback :after_option_parsing, :post_arg_callback
        end
        
        class ShowFeature < Command
          include EntityOps
          include FeatureOps
          documented_only_if_default_name

          def self.opname
            "show-feature"
          end
          
          def self.description
            "Displays the properties of a feature."
          end
          
          def storeop
            :getFeature
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

          Mrg::Grid::Config::Shell.register_command(self, opname + "s")
        end

        class ListFeatures < Command
          include EntityOps
          include FeatureOps
          
          def self.opname
            "list-features"
          end
          
          def self.description
            "Lists all the feature names in the store."
          end
          
          def act
            store.console.objects(:class=>"Feature").each do |feature|
              puts "#{feature.name}"
            end
            0
          end
        end
      end
    end
  end
end
