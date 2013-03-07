# console:  interactive wallaby environment
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

require 'irb'

module Wallaby
  def self.store
    @wallaby_store
  end

  def self.store=(ws)
    @wallaby_store = ws
  end
  
  def store
    ::Wallaby::store
  end
end

module Wallaroo
  module Shell
    class Shebang < Command
      documented_if_environment_has :WALLABY_TECH_PREVIEW
      
      def self.opname
        "shebang"
      end
      
      def self.description
        "Provides an interpreter for wallaby scripts."
      end
      
      def init_option_parser
        @loadfiles = []
        OptionParser.new do |opts|
          
          opname = "shebang"
          
          opts.banner = "Usage:  wallaby #{opname} [-h] SCRIPT [SCRIPT_OPTIONS...]\nWallaby script interpreter.  SCRIPT_OPTIONS are passed directly to SCRIPT."
          
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
                
      def partition_args(*args)
        nonscript_args = []
        @script_args = []
        while (args[0] == "-h" || args[0] == "--help")
          nonscript_args << args.shift
        end
        
        @script_args.replace(args)
        args.replace(nonscript_args.uniq)
        args
      end
      
      def set_evalfile(*ignored)
        args = @script_args
        unless args.size > 0
          exit!(1, "You must provide a script file name.")
        end
        
        unless File.exists?(args[0])
          exit!(1, "You must provide a valid script file name; #{args[0]} does not exist.")
        end
        
        @evalfile = args[0]
        ARGV.replace(args)
      end
      
      register_callback :preprocess_options, :partition_args          
      register_callback :after_option_parsing, :set_evalfile
      
      def act
        ::Wallaby::store = store
        begin
          load @evalfile
          0
        rescue Exception=>e
          puts "#{@evalfile} failed with #{e}"
          puts e.backtrace.join("\n")
          1
        end
      end          
    end
  end
end