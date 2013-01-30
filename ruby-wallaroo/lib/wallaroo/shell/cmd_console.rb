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
    class Console < Command
      def self.opname
        "console"
      end
      
      def self.description
        "Provides an interactive wallaby environment."
      end
      
      def init_option_parser
        @loadfiles = []
        OptionParser.new do |opts|
          
          opname = "console"
          
          opts.banner = "Usage:  wallaby #{opname} [SCRIPT...]\ninteractive wallaby environment."
          
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end

          opts.on("-r", "--require-file FILE", "require FILE before doing anything else") do |file|
            @loadfiles << file
          end

        end
      end
      
      def set_evalfiles(*args)
        @evalfiles = args.dup
      end
      
      register_callback :after_option_parsing, :set_evalfiles
      
      def act
        ARGV.clear
        ::Wallaby::store = store
        @loadfiles.each {|f| require f }
        if @evalfiles.size > 0
          begin
            @evalfiles.each {|f| load f }
            0
          rescue Exception=>e
            puts "script failed; #{e}"
            puts e.backtrace.join("\n")
            1
          end
        else
          ::IRB.start
        end
      end
      
    end
  end
end