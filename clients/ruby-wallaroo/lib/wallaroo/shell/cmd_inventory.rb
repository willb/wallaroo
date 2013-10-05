# inventory:  wallaby node inventory command
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

require 'ostruct'

module Wallaroo
  module Shell
    class Inventory < Command
      SORTKEYS = %w{name checkin}
      NODEKINDS = %w{provisioned unprovisioned}
      
      def format_time(t)
        return "never" if t == 0
        Time.at(t/1000000,t%1000000).to_s
      end
      
      def self.opname
        "inventory"
      end
      
      def self.description
        "Lists a (non-strict) subset of wallaby-managed nodes."
      end
      
      def init_option_parser
        
        OptionParser.new do |opts|
          @sortby = 'name'
          @constraint = nil
          
          opname = "inventory"
          
          opts.banner = "Usage:  wallaby #{opname} [options]\nInventory of nodes that are managed by wallaby."
            
          opts.on("-h", "--help", "displays this message") do
            puts @oparser
            exit
          end
          
          opts.on("-s", "--sort KEY", SORTKEYS, "sort by key", "   (#{SORTKEYS.join(", ")})") do |sort|
            @sortby = sort.downcase
          end

          opts.on("-o", "--only KIND", NODEKINDS, "show only KIND nodes", "   (#{NODEKINDS.join(", ")})") do |nkind|
            @constraint = "provisioned == #{nkind == 'provisioned'}"
          end

          opts.on("-l", "--long", "don't truncate long node names") do
            @long = true
          end

          opts.on("-J", "--json", "output node list as a JSON object") do
            @json = true
          end

          opts.on("-p", "--plain", "omit column headings") do
            @plain = true
          end
          
          opts.on("-c", "--constraint EXPR", "show only nodes for which EXPR is true") do |expr|
            @constraint = expr
          end
        end
      end

      def act
        nodes = store.objects_of_type("Node")

        [::Fixnum, ::Bignum].each do |kls|
          kls.class_eval do
            def minute_ago
              tm = Time.now.utc - (60 * self)
              (tm.tv_sec * 1000000) + tm.tv_usec
            end
            
            def hour_ago
              (60 * self).minutes_ago
            end
            
            def day_ago
              (24 * self).hours_ago
            end
            
            def week_ago
              (7 * self).days_ago
            end
            
            def year_ago
              ta = Time.now.utc.to_a
              ta[5] = ta[5] - self
              tm = Time.utc(*ta)
              (tm.tv_sec * 1000000) + tm.tv_usec
            end
            
            alias :years_ago :year_ago
            alias :minutes_ago :minute_ago
            alias :hours_ago :hour_ago
            alias :days_ago :day_ago
            alias :weeks_ago :week_ago
            
            # NB:  the following two only make sense if used 
            # in conjunction with the X_ago methods, which
            # return timestamps.  Consider the following example:
            #   last_checkin.was_more_than 4.hours_ago
            # 4.hours_ago returns a timestamp, and the last checkin
            # is also a timestamp.  If the integer representing the
            # last checkin timestamp is less than the result of 
            # 4.hours_ago, then the last checkin was more than 
            # four hours ago.  This is perilously close to Rails-
            # level silliness, and I am not proud of it.
            
            def was_more_than(num)
              return self < num
            end
            
            def was_less_than(num)
              return self > num
            end
            
            def is_never
              self == 0
            end
          end
        end

        nodes = nodes.select {|node| node.send(:freeze!); @constraint == nil || safe_instance_eval(node,@constraint)}
        node_structs = nodes.map do |node|
          n = OpenStruct.new
          n.name = node.name
          n.provisioned = node.provisioned
          n.checkin = node.last_checkin
          n.formatted_checkin = format_time(node.last_checkin)
          n
        end
        
        if @json
          class << self
            include JsonNodePrinter
          end              
        else
          class << self
            include TabularNodePrinter
          end
        end

        return dump_nodes(node_structs)
      end
      
      private
      def safe_instance_eval(obj, str)
        Thread.start {
          $SAFE=4
          obj.instance_eval str
        }.value
      end
      
    end
  end
  
  module TabularNodePrinter
    def dump_nodes(node_structs)
      if node_structs.size == 0
        puts "No matching nodes configured."
        return 1
      end
      
      namelength,checkinlength = @long ? node_structs.inject(["node name".size, "last checkin".size]) {|acc, node| [[node.name.size, acc[0]].max, [node.formatted_checkin.size, acc[1]].max]} : [42, 28]
      
      format_str = "%1.1s %#{namelength}.#{namelength}s %#{checkinlength}.#{checkinlength}s\n"
      
      unless @plain
        printf(format_str, "P", "Node name", "Last checkin")
        printf(format_str, "-", "---------", "------------")
      end
      
      node_structs.sort_by {|node| node.send(@sortby)}.each do |node|
        printf(format_str, node.provisioned ? "+" : "-", node.name, node.formatted_checkin)
      end
      return 0
    end
  end

  module JsonNodePrinter
    def dump_nodes(node_structs)
      result = {"nodes"=>node_structs.map {|ns| hashify_one_node(ns)}}
      puts result.inspect.gsub('"=>', '" : ')
      return 0
    end
    
    def hashify_one_node(ns)
      result = {}
      %w{name provisioned checkin}.each {|msg| result[msg] = ns.send(msg)}
      result
    end
  end
end
