# Wallaroo client infrastructure

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

from cmeta import ConnectionMeta
from node import node
from feature import feature
from group import group
from parameter import parameter
from subsystem import subsystem
from heads import tag, branch
from util import pluralize, sha_for
from singleton import v as store_singleton, set_v as set_store_singleton

import errors
from errors import not_implemented, fail

from constants import PARTITION_GROUP, LABEL_SENTINEL_PARAM, LABEL_SENTINEL_PARAM_ATTR

import os

class store(object):
    def __init__(self, cm):
        super(store, self).__init__()
        self.cm = cm
    
    def fatal(msg):
        raise RuntimeError(msg)
    
    def set_user_privs(self, user, role, **options):
        not_implemented()
    
    def del_user(self, user, **options):
        not_implemented()
    
    def users(self, **options):
        not_implemented()
    
    def getGroup(self, **query):
        name = (query.has_key("name") and query["name"]) or (query.has_key("NAME") and query["NAME"]) or None
        if name is None:
            fail(errors.make(errors.BAD_ARGUMENT), "invalid query type %r" % query)
        result = self.cm.make_proxy_object(group, name)
        result.refresh()
        
        return result
    
    def getGroupByName(self, name):
        return self.getGroup(name=name)
    
    def getExplicitGroup(self, name):
        return self.getGroupByName(name)
    
    def addExplicitGroup(self, name):
        result = self.cm.make_proxy_object(group, name)
        if result.exists():
            fatal("group %s already exists" % name)
        result.create()
        return result
    
    def removeGroup(self):
        not_implemented()
    
    def getFeature(self, name):
        result = self.cm.make_proxy_object(feature, name)
        result.refresh()
        return result
    
    def addFeature(self, name):
        result = self.cm.make_proxy_object(feature, name)
        if result.exists():
            fatal("feature %s already exists" % name)
        result.create()
        return result
    
    def removeFeature(self):
        not_implemented()
    
    def addNode(self, name, **options):
        result = self.cm.make_proxy_object(node, name)
        if result.exists():
            result.makeProvisioned()
            result.refresh()
        else:
            result.create()
        return result
    
    def getNode(self, name):
        result = self.cm.make_proxy_object(node, name)
        if result.exists():
            result.refresh()
        else:
            result.provisioned = False
            result.create()
        return result
    
    def removeNode(self, name):
        not_implemented()
    
    def addParam(self, name):
        result = self.cm.make_proxy_object(parameter, name)
        if result.exists():
            fatal("parameter %s already exists" % name)
        result.create()
        return result
    
    def getParam(self, name):
        result = self.cm.make_proxy_object(parameter, name)
        result.refresh()
        return result
    
    def getMustChangeParams(self):
        not_implemented()
    
    def removeParam(self, name):
        not_implemented()
    
    def addSubsys(self, name):
        result = self.cm.make_proxy_object(subsystem, name)
        if result.exists():
            fatal("subsystem %s already exists" % name)
        result.create()
        return result
    
    def getSubsys(self, name):
        result = self.cm.make_proxy_object(subsystem, name)
        result.refresh()
        return result
        
    def removeSubsys(self, name):
        not_implemented()
    
    def activateConfig(self):
        tago = self.cm.make_proxy_object(tag, "current")
        tago.meta = {"validated" : True}
        tago.commit = sha_for(self.cm)
        tago.exists() and tago.update() or tago.create()
    
    def makeSnapshotWithOptions(self, name, **options):
        tago = self.cm_make_proxy_object(tag, name)
        tago.commit = sha_for(cm)
        if options.has_key["annotation"]:
            tago.annotation = options["annotation"]
        if options.has_key["meta"]:
            tago.meta = options["meta"]
        tago.exists() and tago.update() or tago.create()
        return tago
    
    # methods for label support below
    def getPartitionGroup(self):
        valid = len(self.checkGroupValidity([PARTITION_GROUP])) == 0
        if valid:
            return self.getGroup({"name": PARTITION_GROUP})
        else:
            return self.addExplicitGroup(PARTITION_GROUP)
    
    def addLabel(self, label_name):
        """
        Adds a label group with the given name to the store.  If a group with the given name already exists, ensure it is marked as a label group.
        """
        label = None
        if self.checkGroupValidity([label_name]) == [label_name]:
            label = self.addExplicitGroup(label_name)
        else:
            label = self.getGroupByName(label_name)
        if not hasattr(self, LABEL_SENTINEL_PARAM_ATTR):
            if self.checkParameterValidity([LABEL_SENTINEL_PARAM]) == [LABEL_SENTINEL_PARAM]:
                self.addParam(LABEL_SENTINEL_PARAM)
            setattr(self, LABEL_SENTINEL_PARAM_ATTR, True)
        label.modifyParams("ADD", {LABEL_SENTINEL_PARAM : ">= %s" % label_name})
        return label
    
    def isLabel(self, label):
        if hasattr(label, "params"):
            return label.params.has_key(LABEL_SENTINEL_PARAM)
        if type(label) == str and self.checkGroupValidity([label]) == []:
            return self.getGroupByName(label).params.has_key(LABEL_SENTINEL_PARAM)
        return False

store.addNodeWithOptions = store.addNode
store.activateConfiguration = store.activateConfig

def mkcv(klass):
    def cv(self, eset):
        valid_ents = set(self.cm.list_objects(klass))
        return [ent for ent in eset if ent not in valid_ents]
    return cv

def mkle(klass):
    def le(self):
        return self.cm.list_objects(klass)
    return le

for klass in ["Feature", "Group", "Node", "Parameter", "Subsystem", "Branch", "Tag"]:
    setattr(store, "check%sValidity" % klass, mkcv(klass))
    setattr(store, pluralize(klass), property(mkle(klass)))

def mkgsg(grp):
    def get_special_group(self):
        return self.cm.make_proxy_object("group", grp)
    return get_special_group

for msg, grp in [("getDefaultGroup", "+++DEFAULT"), ("getSkeletonGroup", "+++SKEL")]:
    setattr(store, msg, mkgsg(grp))

def setup_labeling(the_store):
    set_store_singleton(the_store)
    the_store.getPartitionGroup()
    return the_store

def connect(**options):
    defaults = {
        "host":os.getenv("WALLAROO_HOST") or "localhost", 
        "port":os.getenv("WALLAROO_PORT") or 8000, 
        "scheme" : os.getenv("WALLAROO_SCHEME") or "http", 
        "username": os.getenv("WALLAROO_USER") or None, 
        "pw": os.getenv("WALLAROO_PASS") or None
    }
    
    for k in options:
        defaults[k] = options[k]
    
    return setup_labeling(store(ConnectionMeta(**defaults)))