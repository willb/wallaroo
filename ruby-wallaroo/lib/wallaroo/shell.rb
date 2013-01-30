# shell:  wallaby shell commands
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

require 'qmf'
require 'optparse'
require 'timeout'
require 'pathname'

require 'mrg/grid/config-client'
require 'mrg/grid/config-proxies'

require 'mrg/grid/config/shell/command'
require 'mrg/grid/config/version'

Version = ::Mrg::Grid::Config::Version.as_string

module Mrg
  module Grid
    module Config
      module Shell
        COMMANDS={}
        COMMAND_LIST=[]

        VALID_MECHANISMS = %w{ANONYMOUS PLAIN GSSAPI DIGEST-MD5 CRAM-MD5 OTP}

        Args = Struct.new(:cmd, :for_wt, :for_cmd)

        BASE_COMMAND_DIR = File.join(File.expand_path(File.dirname(__FILE__)), "shell")

        class ShellCommandFailure < RuntimeError
          attr_accessor :status, :message
        end

        def self.USER_COMMAND_DIR
          result = ENV['WALLABY_COMMAND_DIR']
          return nil unless result

          begin
            result = Pathname.new(File.expand_path(result))
            return result.realpath.to_s
          rescue Exception=>ex
            puts ex.inspect if ENV['WALLABY_SHELL_DEBUG']
            return nil
          end
        end

        def self.register_command(klass, name=nil)
          # XXX:  validate registered commands
          name ||= klass.opname
          COMMANDS[name] = klass
        end

        def self.preprocess_args(args)
          result = Args.new
          pivot = args.size

          args.each_with_index do |arg,idx|
            if (result.cmd = Mrg::Grid::Config::Shell::COMMANDS[arg]; result.cmd)
              pivot = idx
              break
            end
          end

          result.for_wt = args.slice(0,pivot)
          result.for_cmd = args.slice(pivot + 1, args.size)

          result
        end
        
        def self.install_commands(extra=nil)
          extra ||= self.USER_COMMAND_DIR
          commands = []
          commands = commands + Dir["#{Mrg::Grid::Config::Shell::BASE_COMMAND_DIR}/cmd_*.rb"]
          commands = commands + Dir["#{extra}/cmd_*.rb"] if extra
          
          commands.each do |command|
            require File.join(File.dirname(command), File.basename(command, File.extname(command)))
          end
          
          COMMAND_LIST.each do |cmd_klass|
            register_command(cmd_klass)
          end
        end

        def self.main(args)
          host = ENV['WALLABY_BROKER_HOST'] || "localhost"
          port = (ENV['WALLABY_BROKER_PORT'] || 5672).to_i
          username = ENV['WALLABY_BROKER_USER']
          password = ENV['WALLABY_BROKER_PASSWORD']
          explicit_mechanism = ENV['WALLABY_BROKER_MECHANISM']
          if explicit_mechanism && !(VALID_MECHANISMS.include?(explicit_mechanism))
            puts "warning:  rejecting bogus WALLABY_BROKER_MECHANISM of '#{explicit_mechanism}'"
            puts "warning:  valid mechanisms include #{VALID_MECHANISMS.sort.join(", ")}"
            explicit_mechanism = nil
          end

          debug = :warn

          @op = OptionParser.new do |opts|
            opts.banner = "Usage:  wallaby [options] command [command-args]\nUse \"wallaby help commands\" for a list of commands"

            opts.on("-h", "--help", "shows this message") do
              puts @op
              exit
            end

            opts.on("-H", "--host HOSTNAME", "qpid broker host (default localhost)") do |h|
              host = h
            end

            opts.on("-p", "--port NUM", Integer, "qpid broker port (default 5672)") do |num|
              port = num.to_i
            end

            opts.on("-U", "--user NAME", "qpid username") do |name|
              username = name
            end

            opts.on("-P", "--password PASS", "qpid password") do |pass|
              password = pass
            end

            opts.on("-M", "--auth-mechanism PASS", VALID_MECHANISMS, "authentication mechanism (#{VALID_MECHANISMS.join(", ")})") do |mechanism|
              explicit_mechanism = mechanism
            end
          end

          args = preprocess_args(args) unless args.is_a?(Args)

          begin
            @op.parse!(args.for_wt)
          rescue OptionParser::InvalidOption
            puts @op
            exit(1)
          rescue OptionParser::InvalidArgument => ia
            puts ia
            puts @op
            exit(1)
          rescue OptionParser::AmbiguousOption => ia
            puts ia
            puts @op
            exit(1)
          rescue OptionParser::MissingArgument => ia
            puts "#{ia} requires an argument"
            puts @op
            exit(1)
          end

          unless args.cmd
            puts "fatal:  you must specify a command; use \"wallaby help commands\" for a list."
            puts @op
            exit(1)
          end

          store_client = Proc.new do
            console = Qmf::Console.new

            settings = Qmf::ConnectionSettings.new
            settings.username = username if username
            settings.password = password if password
            settings.host = host
            settings.port = port

            implicit_mechanism = (username || password) ? "PLAIN" : "ANONYMOUS"
            settings.mechanism = explicit_mechanism || implicit_mechanism

            begin
              Timeout.timeout(15) do
                connection = Qmf::Connection.new(settings)

                broker = console.add_connection(connection)

                broker.wait_for_stable
              end
            rescue Timeout::Error
              puts "fatal:  timed out connecting to broker on #{host}:#{port}"
              exit!(1)
            end

            stores = console.objects(:class=>"Store")

            if stores.size > 1
              puts "fatal:  there is more than one Wallaby agent running on the specified broker (#{host}:#{port}); only one may run at a time"
              puts "  Agents found:"
              stores.each do |s|
                host, pid = s.host_and_pid
                puts "    pid #{pid} on #{host}"
              end
              exit(1)
            end

            store, = stores

            unless store
              puts "fatal:  cannot find a wallaby agent on the specified broker (#{host}:#{port}); is one running?"
              puts "use -h for help"
              exit!(1)
            end

            Mrg::Grid::ConfigClient::Store.new(store, console)
          end

          begin
            exit!(args.cmd.new(store_client, "").main(args.for_cmd) || 0)
          rescue ShellCommandFailure => scf
            puts "fatal:  #{scf.message}" if scf.message
            exit!(scf.status)
          rescue SystemExit => ex
            exit!(ex.status)
          rescue Exception => ex
            puts "fatal:  #{ex.inspect}"
            exit!(127)
          end
        end
      end
    end
  end
end

Mrg::Grid::Config::Shell.install_commands
