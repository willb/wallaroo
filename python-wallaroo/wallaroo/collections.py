# Mixin support for entity-collection operations in Wallaby.
# Copyright (c) 2011 Red Hat, Inc.
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

import wallaby
from wallaby.mixin import patch_to, calculated_attribute

def qmf_collection_of(klass):
    def bogus_decorator(ignored):
        def get_collection(self):
            return [klass(obj, self.console) for obj in self.console.getObjects(_class=klass.__name__)]
        return get_collection
    return bogus_decorator

class StorePatches(object):
    @calculated_attribute(wallaby.Store, "nodes")
    @patch_to(wallaby.Store)
    @qmf_collection_of(wallaby.Node)
    def node_collection():
        pass
    
    @calculated_attribute(wallaby.Store, "features")
    @patch_to(wallaby.Store)
    @qmf_collection_of(wallaby.Feature)
    def feature_collection():
        pass
    
    @calculated_attribute(wallaby.Store, "groups")
    @patch_to(wallaby.Store)
    @qmf_collection_of(wallaby.Group)
    def group_collection():
        pass
    
    @calculated_attribute(wallaby.Store, "parameters")
    @patch_to(wallaby.Store)
    @qmf_collection_of(wallaby.Parameter)
    def parameter_collection():
        pass
    
    @calculated_attribute(wallaby.Store, "subsystems")
    @patch_to(wallaby.Store)
    @qmf_collection_of(wallaby.Subsystem)
    def subsystem_collection():
        pass




