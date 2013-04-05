# snapshot:  wallaby shell list-, make-, and load-snapshot functionality
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
    module SnapshotBase
      def check_name(*args)
        if args.size != 1 && @op != :listSnapshots
          exit!(1, "you must specify exactly one snapshot name")
        end
        
        @name = args[0]
      end

      def act(kwargs=nil)
        store.send(storeop, @name)
      end
      
      def init_option_parser
        OptionParser.new do |opts|
          opts.banner = "Usage:  wallaby #{self.class.opname}#{self.class.opargs}\n#{self.class.description}"
          if @op == :makeSnapshot
            opts.on("-a", "--annotation COMMENT", "provides an annotation for the given snapshot") do |comment|
              @comment = comment
            end
          end
          
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
        end
      end
    end
    
    class MakeSnapshot < Command
      include SnapshotBase

      def self.opname
        "make-snapshot"
      end
      
      def self.opargs
        " SNAPNAME"
      end
      
      def self.description
        "Makes a snapshot with a given name."
      end

      register_callback :after_option_parsing, :check_name

      def act(kwargs=nil)
        @comment ||= ""
        store.makeSnapshotWithOptions(@name, "annotation"=>@comment)
        0
      end

      private
      def storeop
        :makeSnapshot
      end
    end

    class RemoveSnapshot < Command
      include SnapshotBase

      def self.opname
        "remove-snapshot"
      end
      
      def self.opargs
        " SNAPNAME"
      end
      
      def self.description
        "Removes a snapshot with a given name."
      end

      register_callback :after_option_parsing, :check_name

      private
      def storeop
        :removeSnapshot
      end
    end
    
    class LoadSnapshot < Command
      include SnapshotBase

      def self.opname
        "load-snapshot"
      end
      
      def self.opargs
        " SNAPNAME"
      end
      
      def self.description
        "Loads the snapshot with a given name."
      end

      register_callback :after_option_parsing, :check_name

      private
      def storeop
        :loadSnapshot
      end
    end
    
    class ListSnapshots < Command
      include SnapshotBase
      
      def self.opname
        "list-snapshots"
      end
      
      def self.opargs
        ""
      end
      
      def self.description
        "Lists snapshots in the store."
      end

      def act
        store.objects_of_type("Tag").each do |snap|
          puts "#{snap.name}"
        end
        0
      end
    end
  end
end
