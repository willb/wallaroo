# Copyright (c) 2013 Red Hat, Inc.
# Author:  William Benton (willb@redhat.com)

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

import re
import json
import requests
import urlparse
import string

class Proxying(type):
    def pn(cls): 
       return re.sub("hs$", "hes", ("%ss" % cls.__name__).lower())
    
    def mkpa(cls):
        if 'proxattrs' not in dir(cls):
            cls.proxattrs = set([])
        return cls.proxattrs
    
    plural_name = property(pn)
    proxied_attributes = property(mkpa)

def proxied_attr(klass, name):
    klass.proxied_attributes.add(name)

def proxied_attr_get(name):
    def aget(self):
        return self.attr_vals.has_key(name) and self.attr_vals[name] or None
    return aget

def proxied_attr_set(name):
    def aset(self, val):
        self.attr_vals[name] = val
    return aset

def proxied_attr_getset(name):
    return [proxied_attr_get(name), proxied_attr_set(name)]

def proxied_attr_update(name):
    def pu(self, val):
        self.attr_vals[name] = val
        self.update()
    pu

class Proxy(object):
    __metaclass__ = Proxying
    
    def __init__(self, path, cm):
        self.path = path
        self.cm = cm
        self.__url = None
        self.attr_vals = dict([[k, None] for k in self.__class__.proxied_attributes])
        self.attr_vals["name"] = string.split(path, "/")[-1]
    
    def mkurl(self):
        if self.__url is None:
            self.__url = urlparse.urlunparse((self.cm.scheme, "%s:%d" % (self.cm.host, self.cm.port), self.path, None, None, None))
        return self.__url
    
    def mkparams(self):
        return self.cm.how.to_q()
    
    url = property(mkurl)
    query = property(mkparams) 
    
    def refresh(self):
        self.__url = None
        response = requests.get(self.url, params=(self.skip_q and {} or self.query))
        
        if response.status_code != 200:
            raise RuntimeError("Error %d:  %s" % (response.status_code, response.text))
        
        self.attr_vals = response.json()
    
    def update(self):
        payload = json.dumps(dict([(k,v) for (k,v) in self.attr_vals.iteritems() if v]))
        headers = {'content-type' : 'application/json'}
        response = requests.put(self.url, params=(self.skip_q and {} or self.query), data=payload, headers=headers)
        
        if response.status_code < 200 or response.status_code > 399:
            raise RuntimeError("Error %d:  %s" % (response.status_code, response.text))
        
        self.update_commit(response.headers['location'])
        self.refresh()
    
    def create(self):
        self.update()
    
    def exists(self):
        response = requests.get(self.url, params=self.query)
        return response.status_code < 400
    
    def update_commit(self, location):
        m = re.match(".*?(commit)=([0-9a-f]+)", location)
        if m is not None:
            self.cm.how.update(m.groups()[1])
    
    def skip_q(self):
        False

