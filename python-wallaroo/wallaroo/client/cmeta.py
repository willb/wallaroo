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
import urllib
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
    
    def make_proxy_object(self, kind, name, refresh=False):
        klazz = type(kind) is str and getattr(self.client, kind) or kind
        retval = klazz("/%s/%s" % (klazz.plural_name, urllib.quote_plus(name)), self)
        if refresh:
            retval.refresh()
        return retval
    
    def list_objects(self, kind):
        klazz = getattr(self.client, kind.lower())
        return self.fetch_json_resource("/%s" % klazz.plural_name)
    
    def fetch_json_resource(self, path, query=None, default=None):
        q = query and query or self.how.to_q()
        if default is not None:
            return requests.get(mk_url(self, path), params=q).json()
        else:
            result = requests.get(mk_url(self, path), params=q)
            result.status_code != 404 and result.json or default
    
    def put_json_resource(self, path, dct, skip_q=False):
        q = not skip_q and self.how.to_q() or None
        payload = json.dumps(dict([(k,v) for (k,v) in dct.iteritems()]))
        headers = {'content-type' : 'application/json'}
        response = requests.put(mk_url(self, path), params=q, data=payload, headers=headers)
        
        if response.status_code < 200 or response.status_code > 399:
            raise RuntimeError("Error %d:  %s" % (response.status_code, response.text))
