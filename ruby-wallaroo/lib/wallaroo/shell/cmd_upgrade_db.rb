# cmd_upgrade_db.rb:  
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

require 'mrg/grid/config-patches'

module Mrg
  module Grid
    module Config
      module Shell
        class Upgrade_db < ::Mrg::Grid::Config::Shell::Command
          # opname returns the operation name; for "wallaby foo", it
          # would return "foo".
          def self.opname
            "upgrade-db"
          end
        
          # description returns a short description of this command, suitable 
          # for use in the output of "wallaby help commands".
          def self.description
            "Upgrade the wallaby database."
          end
        
          def supports_options
            true
          end

          def init_option_parser
            # Edit this method to generate a method that parses your command-line options.
            @force = false
            OptionParser.new do |opts|
              opts.banner = "Usage:  wallaby #{self.class.opname}\n#{self.class.description}"
        
              opts.on("-h", "--help", "displays this message") do
                puts @oparser
                exit
              end

              opts.on("-d", "--directory VALUE", "directory containing patch files") do |dir|
                @patch_dir = dir
              end

              opts.on("-f", "--force", "force upgrade") do
                @force = true
              end

              opts.on("-v", "--verbose", "verbose logging") do
                Mrg::Grid::PatchConfigs::PatchLoader.log = LoadSupport::SimpleLog.new(:info, :debug)
              end
            end
          end
        
          def init_patch_dir(*args)
            @patch_dir = "/var/lib/wallaby/patches"
          end

          register_callback :before_option_parsing, :init_patch_dir

          def init_log(*args)
            Mrg::Grid::PatchConfigs::PatchLoader.log = LoadSupport::SimpleLog.new(:info)
          end

          register_callback :before_option_parsing, :init_log

          def act
            patcher = Mrg::Grid::PatchConfigs::PatchLoader.new(store, @force)

            files = Dir["#{@patch_dir}/db*.wpatch"]
            files.sort! {|x, y| Mrg::Grid::PatchConfigs::DBVersion.new(x) <=> Mrg::Grid::PatchConfigs::DBVersion.new(y) }
            files.each do |file|
              if not File.directory?(file)
                fhdl = open("#{file}")
                patcher.load_yaml(fhdl.read)
                begin
                  patcher.load
                rescue Exception=>ex
                  patcher.revert_db
                  raise ex
                ensure
                  fhdl.close
                end
              end
            end
            puts "Database upgrade completed successfully"
            return 0
          end
        end
      end
    end
  end
end
