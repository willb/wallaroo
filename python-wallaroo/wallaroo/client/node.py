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

from .proxy import Proxy, proxied_attr
from .proxy import proxied_attr_get as pag, proxied_attr_set as pas, proxied_attr_getset as pags

from .arc_utils import arcmethod

import errors
from errors import not_implemented, fail

class node(Proxy):
    name = property(pag("name"))
    memberships = property(*pags("memberships"))
    identity_group = property(lambda self : self.cm.make_proxy_object("group", self.attr_vals["identity_group"], refresh=True))
    provisioned = property(*pags("provisioned"))
    last_updated_version = property(pag("last_updated_version"))
    
    modifyMemberships = arcmethod(pag("memberships"), pas("memberships"), heterogeneous=True, preserve_order=True)
    
    def getConfig(self, **options):
        if len(options) > 0:
            not_implemented()
        return self.cm.fetch_json_resource("/config/node/%s" % self.name)
    
    def makeProvisioned(self):
        self.provisioned = True
        self.update()
    
    def explain(self):
        not_implemented()
    
    def whatChanged(old, new):
        not_implemented()

proxied_attr(node, "name")
proxied_attr(node, "memberships")
proxied_attr(node, "identity_group")
proxied_attr(node, "provisioned")