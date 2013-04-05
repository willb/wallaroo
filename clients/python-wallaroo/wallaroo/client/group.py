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

from proxy import Proxy, proxied_attr
from proxy import proxied_attr_get as pag, proxied_attr_set as pas, proxied_attr_getset as pags

from arc_utils import arcmethod

import errors
from errors import not_implemented, fail

import urllib

class group(Proxy):
    name = property(pag("name"))
    features = property(*pags("features"))
    parameters = property(*pags("parameters"))
    
    # alias for backwards-compatibility
    params = property(pag("parameters")) 
    
    modifyFeatures = arcmethod(*pags("features"), heterogeneous=True, preserve_order=True)
    
    def getConfig(self, **options):
        if len(options) > 0:
            not_implemented()
        return self.cm.fetch_json_resource("/config/group/%s" % urllib.quote_plus(self.name))
    
    def explain(self):
        not_implemented()
    
    def modifyParams(self, command, params, **options):
        command = command.upper()
        if command == "ADD":
            for k, v in params.iteritems():
                self.parameters[k] = v
        elif command == "REMOVE":
            for k in [k for k in params if k in self.parameters]:
                del self.parameters[k]
        elif command == "REPLACE":
            self.parameters = params
        else:
            fail(errors.make(errors.BAD_COMMAND, errors.GROUP), "Invalid command %s" % command)
        self.update()
    
    def members(self):
        all_nodes = [self.cm.make_proxy_object("node", node, True) for node in self.cm.list_objects("node")]
        return [node.name for node in all_nodes if self.name in node.memberships]
    
    membership = property(members)

proxied_attr(group, "name")
proxied_attr(group, "features")
proxied_attr(group, "parameters")
