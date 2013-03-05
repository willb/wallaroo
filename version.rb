# Wallaroo version information

module Wallaroo
   module Version
     def self.init
       # XXX:  do I even need to mention that this method 
       # should be refactored in the very near future?
       items = Hash[*open("./wallaroo/src/version.hrl", "r") {|f| f.read}.grep(/.*?(major|minor|patch|build),\s*(.*?)\}.*/).map {|s| s.gsub(/.*?(major|minor|patch|build),\s*(.*?)\}.*/, "\\1=\\2").chomp}.map {|s| res = s.split("="); [res[0].upcase.to_sym, res[1]]}.flatten]  
       [:MAJOR, :MINOR, :PATCH].each {|k| const_set(k, items[k].to_i) }
       build = items[:BUILD].gsub(/\A"|"\Z/, '')
       build = nil unless build.size > 0
       const_set(:BUILD, build)
     end
     
     def self.as_string
       BUILD ? "#{MAJOR}.#{MINOR}.#{PATCH}.#{BUILD}" : "#{MAJOR}.#{MINOR}.#{PATCH}"
     end
   end
end