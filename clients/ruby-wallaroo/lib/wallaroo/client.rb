# Copyright (c) 2012 Red Hat, Inc.
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

require 'json'
require 'net/http'
require 'uri'
require 'cgi'

module Wallaroo
  module Client
    module Util
      def self.camelcase(str)
        str.capitalize.gsub(/_([a-z])/) {|match| match[1].chr.upcase}
      end
      
      def self.sha_for(cm)
        return cm.how.what if cm.how.how == :commit
        where = nil

        if cm.how == :none
          where = cm.make_proxy_object(:tag, "empty")
        else
          where = cm.make_proxy_object(cm.how.how, cm.how.what)
        end
        
        where.refresh
        where.commit
      end
      
      private
      def fatal(message, code=nil)
        raise "#{message}, #{code}"
      end
      
      # for backwards-compatibility with ported Wallaby code
      def fail(code, message)
        fatal(message, code)
      end
      
      def current_caller
        caller[1] =~ /`([^']*)'/ and $1
      end
          
      def not_implemented
        fatal "#{self.class.name}##{current_caller} is not implemented"
      end
    end
    
    class ConnectionMeta
      DEFAULTS = {:host=>"localhost", :port=>8000, :scheme=>"http", :username=>"", :pw=>""}

      attr_reader :host, :port, :scheme, :username, :pw, :how
      def initialize(options=nil)
        options ||= {}
        options = DEFAULTS.merge(options)
        %w{host port scheme username pw}.each do |attribute|
          self.instance_variable_set("@#{attribute}", options[attribute.to_sym])
        end
        @how = Proxying.mk_how(options)
      end
      
      def make_proxy_object(kind, name)
        klazz = ::Wallaroo::Client.const_get(kind.to_s.capitalize)
        result = klazz.new("/#{klazz.respond_to?(:plural_name) ? klazz.plural_name : (kind.to_s.downcase + "s")}/#{name}", self)
        result
      end
      
      def list_objects(kind)
        klazz = ::Wallaroo::Client.const_get(kind.to_s.capitalize)
        fetch_json_resource("/#{klazz.respond_to?(:plural_name) ? klazz.plural_name : (kind.to_s.downcase + "s")}")
      end
      
      def fetch_json_resource(path, custom_q=nil, default_val=nil)
        custom_q ||= self.how.to_q
        url = URI::HTTP.new(self.scheme, nil, self.host, self.port, nil, path, nil, custom_q, nil)
        http = Net::HTTP.new(url.host, url.port)
        request = Net::HTTP::Get.new(url.request_url)
        request.basic_auth(self.username, self.pw) unless (self.username == "" && self.pw == "")
        response = http.request(request)
        if default_val
          response.code =~ /^2/ ? JSON.parse(response.body) : default_val
        else
          JSON.parse(response.body)
        end
      end
      
      def put_json_resource(path, object, skip_q=false)
        url = URI::HTTP.new(self.scheme, nil, self.host, self.port, nil, path, nil, skip_q ? nil : self.how.to_q, nil)
        http = Net::HTTP.new(url.host, url.port)
        request = Net::HTTP::Put.new(url.request_uri)
        request.body = object.to_json
        request.basic_auth(self.username, self.pw) unless (self.username == "" && self.pw == "")
        request.content_type = "application/json"
          
        response = http.request(request)
          
        unless response.code =~ /^2/
          fatal response.body, response.code
        end
      end
      
      def delete_resource(kind, name, skip_q=false)
        klazz = ::Wallaroo::Client.const_get(kind.to_s.capitalize)
        path = "/#{klazz.respond_to?(:plural_name) ? klazz.plural_name : (kind.to_s.downcase + "s")}/#{name}"
        url = URI::HTTP.new(self.scheme, nil, self.host, self.port, nil, path, nil, skip_q ? nil : self.how.to_q, nil)
        http = Net::HTTP.new(url.host, url.port)
        request = Net::HTTP::Delete.new(url.request_uri)
        request.basic_auth(self.username, self.pw) unless (self.username == "" && self.pw == "")
        
        response = http.request(request)
        
        unless response.code =~ /^2/
          fatal response.body, response.code
        end
        
        match = response.header["location"].match(/.*?(commit)=([0-9a-f]+)/)
        self.how.update!(match[2]) if match
        match[2]
      end
      
      def visit!(what, name)
        @how = Proxying.mk_how({what=>name})
      end
    end
    
    module Proxying
      class How
        attr_accessor :how, :what
        
        def initialize(how, what=nil)
          @how = how
          @what = what
        end
        
        def to_q
          how == :none ? nil : "#{how.to_s}=#{CGI.escape(what)}"
        end
        
        def update!(sha)
          return if how == :branch          
          self.how = :commit
          self.what = sha
        end
      end
      
      def self.mk_how(options)
        [:branch, :tag, :commit, :none].map do |kind| 
          what = options[kind] 
          what ? How.new(kind, what) : nil
        end.find(Proc.new {How.new(:tag, "current")}) {|v| v != nil }
      end
      
      module CM
        def declare_attribute(name, readonly=nil)
          ensure_accessors
          attributes << name.to_s
          getter_name = "#{name}".to_sym
          setter_name = "#{name}=".to_sym
          
          self.class_eval do
            define_method getter_name do
              attr_vals[name]
            end
            
            unless readonly
              define_method setter_name do |new_val|
                attr_vals[name] = new_val
              end
            end
          end
        end
        
        def plural_name
          (name.split("::").pop.downcase + "s").sub(/hs$/, "hes")
        end
        
        def ensure_accessors
          unless self.respond_to? :attributes
            class << self
              attr_accessor :attributes
            end
            self.attributes ||= []
          end
        end
      end
      
      module IM
        def initialize(path, cm)
          pathparts = path.split("/")
          @path = pathparts.map {|elt| CGI.escape(elt)}.join("/")
          @cm = cm
          @orig_name = pathparts[-1]
          @attr_vals = {:name=>@orig_name}
        end
        
        def inspect
          "<#{self.class.name.to_s}: #{self.name rescue self.object_id}>"
        end

        def exists?
          http = Net::HTTP.new(url.host, url.port)
          request = Net::HTTP::Get.new(url.request_uri)
          request.basic_auth(@cm.username, @cm.pw) unless (@cm.username == "" && @cm.pw == "")
          
          response = http.request(request)

          return response.code.to_i < 400
        end
      
        def refresh
          @url = nil
          http = Net::HTTP.new(url.host, url.port)
          request = Net::HTTP::Get.new(url.request_uri)
          request.basic_auth(@cm.username, @cm.pw) unless (@cm.username == "" && @cm.pw == "")
          
          response = http.request(request)
          
          unless response.code == "200"
            # XXX: improve error handling to be on par with QMF client
            fatal response.body, response.code
          end
          
          hash = JSON.parse(response.body)
          
          self.class.attributes.each do |name|
            attr_vals[name.to_sym] = hash[name]
          end
          
          self
        end
      
        def update!
          http = Net::HTTP.new(url.host, url.port)
          request = Net::HTTP::Put.new(url.request_uri)
          request.body = attr_vals.to_json
          request.content_type = "application/json"
          request.basic_auth(@cm.username, @cm.pw) unless (@cm.username == "" && @cm.pw == "")
          
          response = http.request(request)
          
          unless response.code =~ /^2/
            fatal response.body, response.code
          end

          update_commit(response.header["location"])
          self.refresh
        end
        
        def create!
          http = Net::HTTP.new(url.host, url.port)
          request = Net::HTTP::Put.new(url.request_uri)
          request.body = attr_vals.to_json
          request.content_type = "application/json"
          request.basic_auth(@cm.username, @cm.pw) unless (@cm.username == "" && @cm.pw == "")
          
          response = http.request(request)
          
          unless response.code =~ /^2/
            fatal response.body, response.code
          end

          puts "response.header['location'] is #{response.header["location"]}" if $WALLAROO_CLIENT_DEBUG

          update_commit(response.header["location"])
          self.refresh
        end
        
        def attr_vals
          @attr_vals
        end

        private
        def url
          @url ||= URI::HTTP.new(cm.scheme, nil, cm.host, cm.port, nil, path, nil, skip_q ? nil : cm.how.to_q, nil)
          puts "just made a URL:  #{@url}"  if $WALLAROO_CLIENT_DEBUG
          @url
        end
        
        def skip_q
          false
        end
        
        def update_commit(location)
          puts "location is '#{location}'"  if $WALLAROO_CLIENT_DEBUG
          
          match = location.match(/.*?(commit)=([0-9a-f]+)/)
          
          puts "match is #{match}"  if $WALLAROO_CLIENT_DEBUG
          
          if match
            cm.how.update!(match[2])
          end
        end
      end
      
      def self.included(receiver)
        receiver.extend CM
        receiver.send :include, IM

        receiver.send :include, ::Wallaroo::Client::Util
        receiver.send :attr_accessor, :cm, :path
      end
    end   
  end
end
      