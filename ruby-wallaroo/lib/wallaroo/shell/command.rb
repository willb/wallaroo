# command:  base class for wallaby-shell commands
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
    class Command
      def self.opname
        "command"
      end
      
      def self.description
        "Does nothing, successfully."
      end
      
      def self.is_documented?(name=nil)
        if self.respond_to?(:documented_if)
          self.documented_if(:name=>name)
        else
          true
        end
      end

      def self.documented_only_if_default_name
        class << self
          def documented_if(options)
            name = (options[:name] || nil)
            opname == name
          end
        end
      end

      # 1. If val is unspecified or nil, this command is documented
      # if var is set (at all) in the environment.
      # 2. If val is a string, this command is documented if var is
      # set in the environment and its value is identical ignoring 
      # case to val.
      # 3. If val is an array, this command is documented if var is
      # set in the environment and its value is identical ignoring case 
      # to an element of val.
      # 4. If val is a proc, this command is documented if calling val
      # with ENV[var] as its argument returns something other than 
      # false or nil.
      def self.documented_if_environment_has(var, val=nil)
        var = var.to_s
        ec = (class << self; self; end)
        ec.send(:define_method, :documented_if) do |options|
          if val == nil
            !!(ENV[var])
          elsif val.is_a?(String)
            ENV[var] && ENV[var].downcase == val.downcase
          elsif val.is_a?(Array)
            ENV[var] && val.map {|v| v.downcase}.include?(ENV[var].downcase)
          elsif val.respond_to(:call)
            val.call(ENV[var])
          else
            false
          end
        end
      end

      attr_reader :oparser
      
      def init_option_parser
        OptionParser.new do |opts|
          
          opname = @op
          
          opts.banner = "Usage:  wallaby #{opname}\n#{self.class.description}"
            
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
      
      def initialize(storeclient, name, op=nil)

        @op = op || self.class.opname
        @store = storeclient
        @name = name
        @kwargs = {}

        @oparser = init_option_parser

        run_callbacks(:initializer)
      end
      
      def main(args)
        args = fold_callbacks(:preprocess_options, *args)
        run_callbacks(:before_option_parsing, *args)
        
        begin
          oparser.parse!(args)
        rescue OptionParser::InvalidOption => io
          puts io
          puts oparser
          return 1
        rescue OptionParser::InvalidArgument => ia
          puts ia
          puts oparser
          return 1
        rescue OptionParser::AmbiguousOption => ao
          puts ao
          puts oparser
          return 1
        end

        run_callbacks(:after_option_parsing, *args)
        
        @wallaby_command_args ||= args
        
        begin
          act
        rescue ShellCommandFailure => scf
          puts "fatal:  #{scf.message}" if scf.message
          scf.status
        rescue SystemExit=>ex
          ex.status
        rescue Exception=>ex
          puts "fatal:  #{ex}"
          puts ex.backtrace.join("\n") if ENV['WALLABY_SHELL_DEBUG']
          1
        end
      end
      
      def act
        0
      end
      
      class << Command
        def callbacks
          init_callbacks
          @callbacks
        end

        # Registers a callback to be called 
        def register_callback(c_when, method)
          callbacks[c_when] << method
        end

        private
        def inherited(klass)
          COMMAND_LIST << klass
        end

        def init_callbacks
          @callbacks ||= Hash.new {|h,k| h[k] = []}
        end
      end
      
      private
      def run_callbacks(c_when, *args)
        self.class.callbacks[c_when].each do |callback|
          self.send(callback, *args)
        end
      end

      def fold_callbacks(c_when, *args)
        self.class.callbacks[c_when].inject(args) do |acc, callback|
          self.send(callback, *acc)
        end
      end

      
      def store
        if @store.is_a? Proc
          @store = @store.call
        end
        @store
      end
      
      def exit!(status, message=nil)
        scf = ShellCommandFailure.new
        scf.status = status
        scf.message = message
        raise scf
      end
    end
  end
end