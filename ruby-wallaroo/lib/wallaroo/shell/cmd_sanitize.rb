# sanitize:  obscures sensitive data from wallaby dump output (for use in bug reporting)
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

module Mrg
  module Grid
    module Config
      module Shell
        class Sanitize < Command
          def self.opname
            # Edit this method to provide the wallaby operation name.
            "sanitize"
          end
          
          def self.description
            # Edit this method to provide a short description of this operation.
            "Obfuscates potentially-sensitive entity names in a wallaby database dump."
          end
          
          def init_option_parser
            # Edit this method to parse command-line options.
            OptionParser.new do |opts|
              opts.banner = "Usage:  wallaby #{self.class.opname} INFILE OUTFILE\n#{self.class.description}."
              
              opts.on("-h", "--help", "displays this message") do
                puts @oparser
                exit
              end
              
              %w{node feature group subsystem parameter}.each do |entity|
                opts.on("--#{entity}s", "obfuscate #{entity} names") do
                  instance_variable_set "@ob_#{entity}s", true
                end
              end
              
              opts.on("-r REGEX", "--replace REGEX", "obfuscate parameter values matching REGEX") do |exp|
                @exps << exp
              end
              
              opts.on("-k KEY", "--key KEY", "use KEY for padding when generating obfuscated strings", "(default is randomly-generated key)") do |key|
                @key = key
              end
              
              opts.on("-v", "--verbose", "output detailed information about progress") do 
                @verbose = true
              end
            end
          end
          
          def initializer_callback
            # You can define multiple callback methods for each of several events.
            # This method will be called at the end of the initializer.
            
            @store = Proc.new { nil }
            @exps = []
            @key = rand(2 << 1024).to_s(16)
          end
          
          # This call registers "initializer_callback" to be called at the end of the
          # initializer.  You can define multiple callback methods; they will be called
          # in the order that they are registered.
          register_callback :initializer, :initializer_callback
          
          def after_option_parsing_callback(*args)
            # This call will be invoked after command-line argument processing.  args
            # will contain every argument passed to this command (minus any processed 
            # command-line options).  It may include, for example, input filenames.
            @args = args.dup
            @input_filename, @output_filename = @args
            if @exps.size > 0
              @value_matcher = Regexp.union(*@exps.map {|e| Regexp.new(e)})
            end
          end

          register_callback :after_option_parsing, :after_option_parsing_callback
          
          def act
            # This method is responsible for actually performing the work of the command.  
            # It may read the @kwargs instance variable, which should be a hash, and must
            # return an integer, corresponding to the exit code of the command.
            puts "using key:  \'#{@key}\'" if @verbose
            
            load_proxies
            
            %w{node feature group subsystem parameter}.each do |entity|
              message = "ob_#{entity}s"
              self.send(message) if instance_variable_get("@#{message}")
            end
            
            ob_values
            
            dump_proxies
            0
          end
          
          private
          
          def obfuscate(name, suffix="")
            Digest::MD5.hexdigest(@key + name) + suffix
          end
          
          def load_proxies(filename=nil)
            filename ||= @input_filename
            puts "loading from #{filename.inspect}" if @verbose
            begin
              @proxy_store = YAML::parse(File.open(filename, "r") {|f| f.read}).transform
              raise "Invalid snapshot file format for #{filename}" unless @proxy_store.is_a?(::Mrg::Grid::SerializedConfigs::Store)
            rescue Exception=>ex
              raise ::Mrg::Grid::Config::Shell::ShellCommandFailure.new((ex.message rescue ex.inspect))
            end
          end
          
          def dump_proxies(filename=nil)
            filename ||= @output_filename
            puts "storing to #{filename.inspect}" if @verbose
            begin
              File.open(filename, "w") {|of| of.write(@proxy_store.to_yaml)}
            rescue Exception=>ex
              raise ::Mrg::Grid::Config::Shell::ShellCommandFailure.new((ex.message rescue ex.inspect))
            end
          end
          
          def ob_nodes
            translation = {}
            @proxy_store.nodes.each do |node|
              puts "obfuscating node name for #{node.name}" if @verbose
              n_hash = Digest::MD5.hexdigest(node.name)
              new_node_name = obfuscate(node.name, ".sanitized.local")
              translation["+++#{n_hash}"] = new_node_name
              node.name = new_node_name
            end
            
            @proxy_store.groups.each do |group|
              new_node_name = translation[group.name]
              if new_node_name
                puts "renaming identity group #{group.name}" if @verbose
                group.name = "+++#{Digest::MD5.hexdigest(new_node_name)}"
              end
            end
          end

          def ob_features
            feature_obfuscate = lambda {|f| obfuscate(f, "-feature")}
            
            @proxy_store.features.each do |feature|
              puts "obfuscating feature name for #{feature.name}" if @verbose
              
              feature.name = obfuscate(feature.name, "-feature")
              
              feature.included.map! &feature_obfuscate
              feature.conflicts.map! &feature_obfuscate
              feature.depends.map! &feature_obfuscate
            end
            
            @proxy_store.groups.each do |group|
              group.features.map! &feature_obfuscate
            end
          end
          
          def ob_groups
            @proxy_store.groups.each do |group|
              unless group.name =~ /^\+\+\+/
                puts "obfuscating group name for #{group.name}" if @verbose
                group.name = obfuscate(group.name, "-group")
              end
            end
          end
          
          def ob_parameters
            param_obfuscate = lambda {|p| p.name = obfuscate(p.name, "-param")}
            
            key_obfuscate = lambda do |entity|
              entity.params = entity.params.inject({}) do |acc,(k,v)|
                acc[obfuscate(k,"-param")] = v
                acc
              end
            end
            
            set_obfuscate = lambda {|p| p = obfuscate(p, "-param")}
            
            @proxy_store.params.each &param_obfuscate
            @proxy_store.features.each &key_obfuscate
            @proxy_store.groups.each &key_obfuscate
            @proxy_store.subsystems.each do |s|
              s.params.map! &set_obfuscate
            end
            
          end
          
          def ob_values
            puts "obfuscating values that match #{@value_matcher.inspect}" if @verbose
            return unless @value_matcher
            
            value_obfuscate = lambda do |entity|
              entity.params = entity.params.inject({}) do |acc, (k,v)|
                if v =~ @value_matcher
                  puts "changing value for #{k}=#{v} in config for #{entity.name}" if @verbose
                  acc[k] = obfuscate(v, "-value")
                else
                  acc[k] = v
                end
                
                acc
              end
            end
            
            @proxy_store.groups.each &value_obfuscate
            @proxy_store.features.each &value_obfuscate
          end
          
        end
      end
    end
  end
end
