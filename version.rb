# Wallaroo version information

module Wallaroo
   module Version
	   MAJOR=1
	   MINOR=9
	   PATCH=0
	   BUILD=0
     
     def self.as_string
       BUILD ? "#{MAJOR}.#{MINOR}.#{PATCH}.#{BUILD}" : "#{MAJOR}.#{MINOR}.#{PATCH}"
     end
   end
end