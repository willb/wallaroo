# dump:  dump a snapshot to a file
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

module Wallaroo
  module Shell
    class Dump < Command
      def self.opname
        "dump"
      end
      
      def self.description
        "Dumps a wallaby snapshot to a file."
      end
      
      def init_option_parser
        OptionParser.new do |opts|
          
          opname = "dump"
          
          opts.banner = "Usage:  wallaby #{opname} SNAPFILE\nDumps a wallaby snapshot to SNAPFILE."
            
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
      
      def check_args(*args)
        if args.size > 1
          puts "wallaby dump: You must specify only one output file (or \"--\")."
          puts op
          exit
        end

        @outfile = (args[0] || "--")
      end
      
      register_callback :after_option_parsing, :check_args
      
      def act
        s = Mrg::Grid::SerializedConfigs::ConfigSerializer.new(store, true, store.console)

        serialized = s.serialize

        if @outfile != "--" then
          File.open(@outfile, "w") do |of|
            of.write(serialized.to_yaml)
          end
        else
          puts serialized.to_yaml
        end
        
        0
      end
    end
  end
end