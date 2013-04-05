# apropos:  wallaby parameter apropos functionality
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
    class Apropos < Command
      def self.opname
        "apropos"
      end

      def self.description
        "Provides a list of parameters that contain KEYWORD in their descriptions."
      end

      def init_option_parser
        @use_regex = false
        @insens = nil

        OptionParser.new do |opts|
          
          opts.banner = "Usage:  wallaby #{self.class.opname} KEYWORD\nProvides a list of parameters that contain KEYWORD in their descriptions."
            
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end

          opts.on("--regex", "interpret KEYWORD as a regular expression") do
            @use_regex = true
          end

          opts.on("-i", "--case-insensitive", "return case-insensitive matches for KEYWORD") do
            @insens = Regexp::IGNORECASE
          end
        end
      end

      def check_args(*args)
        if args.size != 1
          puts "error:  you must specify a keyword"
          puts oparser
          exit!(1)
        end

        @keyword = args[0]
      end
      
      register_callback :after_option_parsing, :check_args

      def act
        if @use_regex
          @matches = Proc.new do |name|
            @regexp ||= Regexp.new(@keyword, @insens)
            name =~ @regexp
          end
        else
          @matches = Proc.new do |name|
            @matchkwd ||= @insens ? @keyword.downcase : @keyword
            name = name.downcase if @insens
            name.include?(@matchkwd)
          end
        end

        params = store.parameters.select {|p| @matches.call(p.description) }.sort_by {|prm| prm.name}

        params.each do |prm|
          puts "#{prm.name}:  #{prm.description}"
        end

        params.size > 0 ? 0 : 1
      end
      
    end
  end
end