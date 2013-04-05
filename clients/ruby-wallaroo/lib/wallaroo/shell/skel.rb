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

    # This is an example wallaby shell command.  You can copy this file to a new file
    # named cmd_xxx.rb (where "xxx" is any unique suffix) and place it in your Wallaby
    # shell commands directory.  
    class Skel < Command
      
      def self.opname
        # Edit this method to provide the wallaby operation name.
        "skel"
      end
      
      def self.description
        # Edit this method to provide a short description of this operation.
        "Does nothing, successfully."
      end
      
      def init_option_parser
        # Edit this method to parse command-line options.
        OptionParser.new do |opts|
          opts.banner = "Usage:  wallaby #{self.class.opname}\n#{self.class.description}."
          
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
      
      def initializer_callback
        # You can define multiple callback methods for each of several events.
        # This method will be called at the end of the initializer.
      end
      
      # This call registers "initializer_callback" to be called at the end of the
      # initializer.  You can define multiple callback methods; they will be called
      # in the order that they are registered.
      register_callback :initializer, :initializer_callback
      
      def before_option_parsing_callback(*args)
        # This call will be invoked before command-line argument processing.  args
        # will contain every argument passed to this command (before parsing).
      end

      register_callback :before_option_parsing, :before_option_parsing_callback
      
      def after_option_parsing_callback(*args)
        # This call will be invoked after command-line argument processing.  args
        # will contain every argument passed to this command (minus any processed 
        # command-line options).  It may include, for example, input filenames.
      end

      register_callback :after_option_parsing, :after_option_parsing_callback
      
      def act
        # This method is responsible for actually performing the work of the command.  
        # It may read the @kwargs instance variable, which should be a hash, and must
        # return an integer, corresponding to the exit code of the command.
        0
      end
      
    end
    
  end
end