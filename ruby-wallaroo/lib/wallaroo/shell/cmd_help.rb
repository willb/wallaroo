# skel:  template wallaby-shell class
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
    class Help < Command
      
      def self.opname
        # Edit this method to provide the wallaby operation name.
        "help"
      end
      
      def self.description
        "Provides brief documentation for wallaby shell commands."
      end
      
      def init_option_parser
        OptionParser.new do |opts|
          opts.banner = "Usage:  wallaby #{self.class.opname} [commands|COMMAND_NAME]\n#{self.class.description}"
          
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
      
      def initializer_callback
        @store = nil
      end
      
      register_callback :initializer, :initializer_callback
      
      def after_option_parsing_callback(*args)
        @args = args.dup
        if args.length == 0
          puts "Use \"wallaby help COMMAND\" to get help on COMMAND."
          puts "Available commands include:"
          describe_commands
          exit!(0)
        end
      end

      def describe_commands
        CommandS.keys.sort.each do |cmd|
          command = CommandS[cmd]
          if command.is_documented?(cmd)
            puts "#{cmd}:  #{CommandS[cmd].description rescue ""}"
          end
        end
      end

      register_callback :after_option_parsing, :after_option_parsing_callback
      
      def act
        command = @args[0].downcase
        
        cklass = CommandS[command]
        
        if cklass
          puts cklass.new(nil, nil, nil).oparser
          return 0
        else
          puts "#{command} is not a recognized command." unless command == "commands"
          puts "Use \"wallaby help COMMAND\" to get help on COMMAND."
          puts "Available commands include:"

          describe_commands

          return command == "commands" ? 0 : 1
        end
      end
      
    end
    
  end
end
