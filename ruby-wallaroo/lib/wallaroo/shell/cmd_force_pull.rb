# cmd_force_restart.rb:  Force all daemons to restart
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

module Mrg
  module Grid
    module Config
      module Shell
        class ForcePull < ::Mrg::Grid::Config::Shell::Command
          # opname returns the operation name; for "wallaby foo", it
          # would return "foo".
          def self.opname
            "force-pull"
          end
        
          # description returns a short description of this command, suitable 
          # for use in the output of "wallaby help commands".
          def self.description
            "Force a configuration pull"
          end
        
          def init_option_parser
            # Edit this method to generate a method that parses your command-line options.
            OptionParser.new do |opts|
              @nodes = ""
              @groups = ""
              @all_nodes = false
              opts.banner = "Usage:  wallaby #{self.class.opname}\n#{self.class.description}"
        
              opts.on("-h", "--help", "displays this message") do
                puts @oparser
                exit
              end

              opts.on("-n", "--nodes NLIST", "the nodes to force to pull configuration") do |list|
                @nodes = list
              end

              opts.on("-g", "--groups GLIST", "the groups of nodes to force to pull configuration") do |list|
                @groups = list
              end

              opts.on("-a", "--all", "force all nodes to pull configuration") do
                @all_nodes = true
              end
            end
          end
        
          def act
            nlist = @nodes.split(',')
            glist = @groups.split(',')
            val = Time.now.to_f
            if @all_nodes == true
              store.getDefaultGroup.modifyParams('add', {'WALLABY_FORCE_CONFIG_PULL'=>val}, {})
            else
              invalids = store.checkNodeValidity(nlist)
              if invalids != []
                exit!(1, "Invalid node name(s): #{invalids.inspect}")
              end

              invalids = store.checkGroupValidity(glist)
              if invalids != []
                exit!(1, "Invalid group name(s): #{invalids.inspect}")
              end

              nlist.each do |n|
                gobj = store.getNode(n).identity_group
                gobj.modifyParams('add', {'WALLABY_FORCE_CONFIG_PULL'=>val}, {})
              end

              glist.each do |g|
                gobj = store.getGroup({"Name"=>g})
                gobj.modifyParams('add', {'WALLABY_FORCE_CONFIG_PULL'=>val}, {})
              end
            end

            store.activateConfiguration()
            return 0
          end
        end
      end
    end
  end
end
