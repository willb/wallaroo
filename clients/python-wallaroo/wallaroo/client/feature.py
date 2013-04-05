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

from arc_utils import arcmethod, uniq

import errors
from errors import not_implemented, fail

class feature(Proxy):
    name = property(*pags("name"))
    
    included_features = property(*pags("includes"))
    includes = property(*pags("includes")) #convenience alias
    
    depends = property(*pags("depends"))
    conflicts = property(*pags("conflicts"))
    parameters = property(*pags("parameters"))
    
    modifyIncludes = arcmethod(*pags("includes"), explain="includes", preserve_order=True)
    modifyDepends = arcmethod(*pags("depends"), explain="depends on", preserve_order=False)
    modifyConflicts = arcmethod(*pags("conflicts"), explain="conflicts with", preserve_order=False)
    
    def setName(self, nn):
        self.name = nn
        self.update()
    
    def modifyParams(command, params, **options):
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
            fail(errors.make(errors.BAD_COMMAND, errors.FEATURE), "Invalid command %s" % command)
        self.update()

proxied_attr(feature, "name")
proxied_attr(feature, "includes")
proxied_attr(feature, "conflicts")
proxied_attr(feature, "depends")
proxied_attr(feature, "parameters")