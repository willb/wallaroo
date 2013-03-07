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

from .proxy import Proxy, proxied_attr, proxied_attr_update
from .proxy import proxied_attr_get as pag, proxied_attr_set as pas, proxied_attr_getset as pags

from .arc_utils import arcmethod, uniq

import errors
from errors import not_implemented, fail

from util import camelcase

class parameter(Proxy):
    name = property(*pags("name"))
    depends = property(*pags("depends"))
    conflicts = property(*pags("conflicts"))
    kind = property(*pags("kind"))
    description = property(*pags("description"))
    default_val = property(*pags("default_val"))
    must_change = property(*pags("must_change"))
    requires_restart = property(*pags("requires_restart"))
    
    modifyDepends = arcmethod(*pags("depends"), explain="depends on", preserve_order=False)
    modifyConflicts = arcmethod(*pags("conflicts"), explain="conflicts with", preserve_order=False)

for attr in ["name", "conflicts", "depends", "kind", "description", "default_val", "must_change", "visibility_level", "requires_restart"]:
    proxied_attr(parameter, attr)
    setattr(parameter, "set%s" % "".join(x.capitalize() for x in attr.split("_")), proxied_attr_update(attr))