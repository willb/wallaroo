# Mixin support for node-tagging operations in Wallaby.
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
from wallaby_mixin import patch_to, calculated_attribute

PARTITION_GROUP = "===TAGS_BELOW==="
TAG_SENTINEL_PARAM = "WALLABY_TAGS"
TAG_SENTINEL_PARAM_ATTR = "__tag_sentinel_param"
store = None

def uniq(ls):
  def gen_check_and_add(d):
    def ca(name):
      result = name in d
      d[name] = True
      return result
    return ca
  c_and_a = gen_check_and_add({})
  return [x for x in ls if not c_and_a(x)]

class StorePatches(object):
    @patch_to(wallaby.Store)
    def getPartitionGroup(self):
        valid = len(self.checkGroupValidity([PARTITION_GROUP])) == 0
        if valid:
            return self.getGroup({"name": PARTITION_GROUP})
        else:
            return self.addExplicitGroup(PARTITION_GROUP)
    
    @patch_to(wallaby.Store)
    def addTag(self, tag_name):
        """
        Adds a tag group with the given name to the store.  If a group with the given name already exists, ensure it is marked as a tag group.
        """
        tag = None
        if self.checkGroupValidity([tag_name]) == [tag_name]:
            tag = self.addExplicitGroup(tag_name)
        else:
            tag = self.getGroupByName(tag_name)
        if not hasattr(self, TAG_SENTINEL_PARAM_ATTR):
            if self.checkParameterValidity([TAG_SENTINEL_PARAM]) == [TAG_SENTINEL_PARAM]:
                self.addParam(TAG_SENTINEL_PARAM)
            setattr(self, TAG_SENTINEL_PARAM_ATTR, True)
        tag.modifyParams("ADD", {TAG_SENTINEL_PARAM : ">= %s" % tag_name})
        return tag
    
    @patch_to(wallaby.Store)
    def isTag(self, tag):
        if hasattr(tag, "params"):
            return tag.params.has_key(TAG_SENTINEL_PARAM)
        if type(tag) == str and self.checkGroupValidity([tag]) == []:
            return self.getGroupByName(tag).params.has_key(TAG_SENTINEL_PARAM)
        return False

class NodePatches(object):
    @calculated_attribute(wallaby.Node, "tags")
    @patch_to(wallaby.Node)
    def getTags(self):
        memberships = self.memberships
        if not PARTITION_GROUP in memberships:
            return []
        else:
            partition = memberships.index(PARTITION_GROUP)
            return memberships[partition+1:]
    
    @patch_to(wallaby.Node)
    def modifyTags(self, op, tags, **options):
        global store
        memberships = self.memberships
        current_tags = self.getTags()
        tag_set = set(current_tags + [PARTITION_GROUP])
        new_tags = []
        
        if op == "ADD":
            new_tags = current_tags + tags
            pass
        elif op == "REPLACE":
            new_tags = tags
            pass
        elif op == "REMOVE":
            new_tags = [tag for tag in current_tags if tag not in tags]
        else:
            raise NotImplementedError("modifyTags:  operation " + op + " not understood")
        
        just_memberships = [grp for grp in memberships if grp not in tag_set]
        new_memberships = uniq(just_memberships + [PARTITION_GROUP] + new_tags)
        
        if "ensure_partition_group" in options and options["ensure_partition_group"] is not False:
            if store is None:
                raise RuntimeError("You must call tagging.setup(store) before using the ensure_partition_group option")
            store.getPartitionGroup()
        
        if "create_missing_tags" in options and options["create_missing_tags"] is not False:
            if store is None:
                raise RuntimeError("You must call tagging.setup(store) before using the create_missing_tags option")
            for missing_tag in store.checkGroupValidity(new_tags):
                store.addTag(missing_tag)
        
        return self.modifyMemberships("REPLACE", new_memberships, {})


def setup(the_store):
    global store
    store = the_store
    store.getPartitionGroup()



