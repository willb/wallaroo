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

from proxy import Proxy, proxied_attr, proxied_attr_update
from proxy import proxied_attr_get as pag, proxied_attr_set as pas, proxied_attr_getset as pags

from arc_utils import arcmethod, uniq

import errors
from errors import not_implemented, fail

from util import camelcase

class subsystem(Proxy):
    name = property(*pags("name"))
    parameters = property(*pags("parameters"))
    
    setName = proxied_attr_update("name")
    
    modifyParams = arcmethod(*pags("parameters"), explain="depends on", preserve_order=False, heterogeneous=True)

for attr in ["name", "parameters"]:
    proxied_attr(subsystem, attr)
