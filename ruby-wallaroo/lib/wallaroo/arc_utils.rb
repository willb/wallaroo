# port of Wallaby's arc utils module for Wallaroo
#
# Copyright (c) 2009-2013 Red Hat, Inc.
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

require 'set'

module Wallaroo
  module Client
    module ArcUtils
      # * command is the "modify" command passed in from the tool
      # * dests is the set of keys for the input set
      # * options are provided by the tool
      # * getmsg is the message to get the current set of arcs
      # * setmsg is the message to set the current set of arcs
      # * keyword arguments include
      # ** =:explain= is a string describing the relationship modeled by the arc (for error messages)
      # ** =:keymsg= is the message to get the key value from self
      # ** =:preserve_order= is true if these arcs represent a list
      # ** =:xc= is a message to get the transitive closure of this relation from self
      def modify_arcs(command,dests,options,getmsg,setmsg,kwargs=nil)
        # NB:  this must work for lists and sets; note the ADD/UNION case
        dests = dests.keys if dests.is_a? Hash
        
        kwargs ||= {}
          
        explain = kwargs[:explain] || "have an arc to"
        what = kwargs[:what] || self.class.name.split("::").pop.downcase
        errwhat = Errors.const_get(what.upcase) || Errors::UNKNOWN
        keymsg = kwargs[:name] || :name
        preserve_order = kwargs[:preserve_order]
        command = command.upcase
          
        case command
        when "ADD" then 
          old_dests = preserve_order ? self.send(getmsg) : Set[*self.send(getmsg)]
          new_dests = preserve_order ? dests : Set[*dests]
          fail(Errors.make(Errors::CIRCULAR_RELATIONSHIP, Errors::INVALID_RELATIONSHIP, errwhat), "#{what} #{name} cannot #{explain} itself") if new_dests.include? self.send(keymsg)
          self.send(setmsg, (old_dests + new_dests).to_a.uniq) # the uniq is important so this can work either as a list or set
        when "REPLACE" then 
          new_dests = preserve_order ? dests : Set[*dests]
          fail(Errors.make(Errors::CIRCULAR_RELATIONSHIP, Errors::INVALID_RELATIONSHIP, errwhat), "#{what} #{name} cannot #{explain} itself") if new_dests.include? self.send(keymsg)
          self.send(setmsg, new_dests.to_a)
        when "REMOVE" then
          old_dests = self.send(getmsg)
          removed_dests = dests
          new_dests = old_dests - removed_dests
          self.send(setmsg, new_dests)
        when "INTERSECT", "DIFF", "UNION" then
          fail(Errors.make(Errors::INTERNAL_ERROR, Errors::NOT_IMPLEMENTED, errwhat), "#{command} not implemented for order-preserving relations") if preserve_order
          old_dests = Set[*self.send(getmsg)]
          supplied_dests = Set[*dests]
          new_dests = ArcUtils.send("#{command.downcase}_collections", old_dests, supplied_dests).to_a
          fail(Errors.make(Errors::CIRCULAR_RELATIONSHIP, Errors::INVALID_RELATIONSHIP, errwhat), "#{what} #{name} cannot #{explain} itself") if new_dests.include? self.send(keymsg)
          self.send(setmsg, new_dests)
        else fail(Errors.make(Errors::BAD_COMMAND, errwhat), "Invalid command #{command}")
        end
      end
        
      def self.intersect_collections(first, second)
        first & second
      end

      def self.union_collections(first, second)
        first | second
      end
        
      def self.diff_collections(first, second)
        (first | second) - (first & second)
      end
        
    end
  end
end