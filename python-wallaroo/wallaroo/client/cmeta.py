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

from .how import How
import urlparse
import requests
import json

def mk_how(options):
    PREFERRED_ORDER = ["branch", "tag", "commit", "none"]
    hows = [How(k, options[k]) for k in PREFERRED_ORDER if options.has_key(k)]
    return len(hows) > 0 and hows[0] or How("tag", "current")
    
def mk_url(cm, path):
    return urlparse.urlunparse((cm.scheme, "%s:%d" % (cm.host, cm.port), path, None, None, None))

class ConnectionMeta(object):
    def __init__(self, host="localhost", port=8000, scheme="http", username=None, pw=None, **kwargs):
        self.host = host
        self.port = port
        self.scheme = scheme
        self.username = username
        self.how = mk_how(kwargs)
        self.client = __import__("wallaroo").client
    
    def make_proxy_object(self, kind, name):
        klazz = type(kind) is str and getattr(self.client, kind) or kind
        return klazz("/%s/%s" % (klazz.plural_name, name), self)
    
    def list_objects(self, kind):
        klazz = getattr(wallaroo.client, kind.lower())
        self.fetch_json_resource("/%s" % klazz.plural_name)
    
    def fetch_json_resource(self, path, query=None):
        q = query and query or self.how.to_q()
        return requests.get(mk_url(self, path), params=q).json()
