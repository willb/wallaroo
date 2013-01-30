# feature_import:  translates a file of PARAM=VALUE pairs to a new
# feature definition, creating parameters in the store as necessary.
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
    class FeatureImport < Command
      def self.opname
        "feature-import"
      end
      
      def self.description
        "Imports a wallaby feature from a Condor configuration file."
      end
      
      def parse_config_file(fn)
        results = {:name=>"", :includes=>[], :depends=>[], :params=>{}}
        lines = open(fn) {|f| f.readlines }
        lines.each do |line|
          case line.strip
          when /^#includes\s+(.*)$/ then results[:includes] << $1.strip
          when /^#dependson\s+(.*)$/ then results[:depends] << $1.strip
          when /^#default\s+(.*)$/ then results[:params][$1.strip] = 0
          when /^#name\s+(.*)$/ then results[:name] = $1.strip
          when /^([^=]*)=(.*)$/ then results[:params][$1.strip] = ($2.strip.size > 0 ? $2.strip : 0)
          else puts "warning:  unrecognized config file line #{line}" unless line =~ /^#.*/
          end
        end

        results
      end

      def init_option_parser
        OptionParser.new do |opts|
          
          opname = "feature-import"
          
          opts.banner = "Usage:  wallaby #{opname} [options] FILE\nImports a wallaby feature from a specially-formatted configuration file."
            
          opts.on("-h", "--help", "displays this message") do
            puts opts
            exit
          end
          
          opts.on("-n", "--name NAME", "name for given feature (overrides one specified in the file)") do |nm|
            @feature_name = nm
          end
        end
      end

      def parse_file(*args)
        unless args.size == 1 && args[0]
          puts "fatal:  you must specify exactly one file name (use --help for more information)"
          exit(1)
        end
        
        begin
          @feature = parse_config_file(args[0])
        rescue SystemCallError => sce
          puts "fatal: #{sce.message}"
          exit(1)
        end            
        
        if @feature_name
          # command-line name takes precedence
          @feature[:name] = @feature_name
        end

        unless @feature[:name] && @feature[:name].size > 0
          puts "fatal: No feature name supplied.  You must provide one, either\nwith a '--name' parameter on the command-line, or with\na '#name' directive in the feature file."
          puts oparser
          exit(1)
        end
      end
      
      register_callback :after_option_parsing, :parse_file
      
      def act(kwargs=nil)
        puts "Creating feature #{@feature[:name]}..."

        params = @feature[:params].keys
        invalid_params = store.checkParameterValidity(params)

        unless invalid_params == []
          puts "Creating necessary parameters..."
          invalid_params.each {|param| puts "Creating parameter #{param}" ; store.addParam(param)}
        end

        f = nil
        
        begin
          f = store.addFeature(@feature[:name])
        rescue
          nil
        end
        
        unless f
          puts "fatal:  can't create feature #{@feature[:name]}"
          return 1
        end

        unless @feature[:includes] == []
          puts "Setting included features to #{@feature[:includes].inspect}"
          f.modifyIncludedFeatures("ADD", @feature[:includes], {})
        end

        unless @feature[:depends] == []
          puts "Setting feature dependencies to #{@feature[:depends].inspect}"
          f.modifyDepends("ADD", @feature[:depends], {})
        end

        unless @feature[:params] == {}
          puts "Setting feature parameters to #{@feature[:params].inspect}"
          f.modifyParams("ADD", @feature[:params], {})
        end
        
        0
      end
    end
  end
end