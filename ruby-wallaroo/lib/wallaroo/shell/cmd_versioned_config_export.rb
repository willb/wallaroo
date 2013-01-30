# cmd_versioned_config_export.rb:  exports versioned configurations to plain text files
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

require 'mrg/grid/config'

USE_PREPARED_STATEMENTS = ENV['WALLABY_USE_PREPARED_STATEMENTS'] && (ENV['WALLABY_USE_PREPARED_STATEMENTS'] == "1" || ENV['WALLABY_USE_PREPARED_STATEMENTS'] =~ /^ty/i)

module Mrg
  module Grid
    module Config
      module Shell
        class VersionedConfigExport < ::Mrg::Grid::Config::Shell::Command
          documented_if_environment_has :WALLABY_TECH_PREVIEW

          # opname returns the operation name; for "wallaby foo", it
          # would return "foo".
          def self.opname
            "vc-export"
          end
        
          # description returns a short description of this command, suitable 
          # for use in the output of "wallaby help commands".
          def self.description
            "exports versioned configurations from DBFILE to plain text files"
          end
        
          def init_option_parser
            # Edit this method to generate a method that parses your command-line options.
            OptionParser.new do |opts|
              opts.banner = "Usage:  wallaby #{self.class.opname} DBFILE\n#{self.class.description}"
        
              opts.on("-h", "--help", "displays this message") do
                puts @oparser
                exit
              end

              opts.on("--verbose", "outputs information about command progress") do
                @verbose = true
              end

              opts.on("-o DIR", "--output-dir DIR", "set output directory to DIR") do |dir|
                @output_dir = dir
              end

              opts.on("--earliest NUM", Integer, "output only the earliest NUM configurations") do |num|
                old_lp = @list_preproc

                @list_preproc = Proc.new do |ls| 
                  ls = old_lp.call(ls) if old_lp
                  ls.sort_by {|cv| cv.version}.slice(0,num)
                end
              end

              opts.on("--latest NUM", Integer, "output only the most recent NUM configurations") do |num|
                old_lp = @list_preproc

                @list_preproc = Proc.new do |ls|
                  ls = old_lp.call(ls) if old_lp 
                  ls.sort_by {|cv| cv.version}.slice(-num,ls.size)
                end
              end

              opts.on("--since DATE", "output only configurations since the given date") do |date|
                since = parsetime(date)
                old_lp = @list_preproc

                @list_preproc = Proc.new do |ls|
                  ls = old_lp.call(ls) if old_lp 
                  ls.select {|cv| cv.version >= since}
                end
              end
            end
          end
        
          def initializer_callback
            # You can define multiple callback methods for each of several events.
            # This method will be called at the end of the initializer.
            
            @store = Proc.new { nil }
            @output_dir = "snapshots"
          end
          
          # This call registers "initializer_callback" to be called at the end of the
          # initializer.  You can define multiple callback methods; they will be called
          # in the order that they are registered.
          register_callback :initializer, :initializer_callback

          def after_option_parsing_callback(*args)
            # This call will be invoked after command-line argument processing.  args
            # will contain every argument passed to this command (minus any processed 
            # command-line options).  It may include, for example, input filenames.
            exit!(1, "You must specify exactly one database file; use --help for help") unless args.length == 1
            @infile, = args.dup
          end

          register_callback :after_option_parsing, :after_option_parsing_callback

          def act
            # This method is responsible for actually performing the work of
            # the command. It may read the @kwargs instance variable, which
            # should be a hash, and must return an integer, corresponding to
            # the exit code of the command.
        
            # It may access the wallaby store with the "store" method; it will
            # only connect to the wallaby store after the first time "store" is
            # invoked. See the Wallaby client API for more information on
            # methods supported by store and other Wallaby API entities.
        
            # You may exit the command from a callee of act by using the exit!
            # method, which takes a status code and an optional explanation of
            # why you are exiting. For example:
        
            Rhubarb::Persistence::open(@infile,:snapshot,USE_PREPARED_STATEMENTS)
            Mrg::Grid::Config::SNAP_DB_TABLES.each {|tab| tab.db = Rhubarb::Persistence::dbs[:snapshot] }

            versions = ::Mrg::Grid::Config::ConfigVersion.find_all
            versions = @list_preproc.call(versions) if @list_preproc

            versions.each do |cv|
              
              this_snapshot_dir = "#{@output_dir}/#{cv.version}"
              this_time = ts2time(cv.version)
              
              ["/nodes", "/groups", ""].each do |dir|
                current_dir = "#{this_snapshot_dir}#{dir}"
                FileUtils.mkdir_p(current_dir)
              end

              ::Mrg::Grid::Config::VersionedNodeConfig.find_by(:version=>cv).each do |vnc|
                gname = group_name(vnc.node.name)
                subdir, name = gname ? ["groups", gname] : ["nodes", vnc.node.name]
                
                path = "#{this_snapshot_dir}/#{subdir}/#{name}"
                puts "writing config to #{path} ..." if @verbose

                open(path, "w") do |f|
                  vnc.config.to_hash.each do |k,v|
                    f.write("#{k}=#{v}\n")
                  end

                  if vnc.config.respond_to?(:groups)
                    f.write("\n# The following parameter did not appear in the stored configuration\n# It is here solely to list the group memberships of this node.\n")
                    f.write("WALLABY_GROUPS = #{vnc.config.groups.join(", ")}")
                  end
                end

                File.utime(this_time, this_time, path)
              end

              ["/nodes", "/groups", ""].each do |dir|
                current_dir = "#{this_snapshot_dir}#{dir}"
                File.utime(this_time, this_time, current_dir)
              end
            end

            return 0
          end

          private
          def ts2time(ts)
            Time.at(ts/1000000, ts%1000000)
          end

          def parsetime(str)
            default = Time.parse("1/1/1900", nil)
            tm = Time.parse(str, default)
            fail("Can't parse time '#{str}'; must be a valid time and after 1/1/1900") if default == tm
            Rhubarb::Util::timestamp(tm)
          end

          def group_name(nm)
            return nm if nm == "+++DEFAULT"
            nm.slice(0,3) == "+++" && nm.slice(3, nm.length)
          end
        end
      end
    end
  end
end
