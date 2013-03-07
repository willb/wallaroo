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

from .arc_utils import arcmethod, uniq
from .singleton import v as store_singleton

import errors
from errors import not_implemented, fail

from constants import PARTITION_GROUP, LABEL_SENTINEL_PARAM, LABEL_SENTINEL_PARAM_ATTR

from datetime import datetime
import calendar

def ts():
    now = datetime.utcnow()
    return (calendar.timegm(now.utctimetuple()) * 1000000) + now.microsecond

class node(Proxy):
    name = property(pag("name"))
    memberships = property(*pags("memberships"))
    identity_group = property(lambda self : self.cm.make_proxy_object("group", self.attr_vals["identity_group"], refresh=True))
    provisioned = property(*pags("provisioned"))
    last_updated_version = property(pag("last_updated_version"))
    
    modifyMemberships = arcmethod(pag("memberships"), pas("memberships"), heterogeneous=True, preserve_order=True)
    
    def getConfig(self, **options):
        if options.has_key("version"):
            return self.cm.fetch_json_resource("/config/node/%s" % self.name, {"commit":options["version"]}, {})
        return self.cm.fetch_json_resource("/config/node/%s" % self.name)
    
    def makeProvisioned(self):
        self.provisioned = True
        self.update()
    
    def explain(self):
        not_implemented()
    
    def checkin(self):
        metapath = "/meta/node/%s" % self.name
        # now = datetime.utcnow().isoformat()
        now = ts()
        meta = self.cm.fetch_json_resource(metapath, False, default={})
        meta["last-checkin"] = now
        self.cm.put_json_resource(metapath, meta, False)
        return now
    
    def last_checkin(self):
        metapath = "/meta/node/%s" % self.name
        meta = self.cm.fetch_json_resource(metapath, False, default={})
        return meta.has_key("last-checkin") and meta["last-checkin"] or 0
    
    def whatChanged(self, old, new):
        oc = self.cm.fetch_json_resource("/config/node/%s" % self.name, {"commit":old}, {})
        nc = self.cm.fetch_json_resource("/config/node/%s" % self.name, {"commit":new}, {})
        
        ock = set(oc)
        nck = set(nc)
        
        params = set([p for p in (ock | nck) if p not in ock or p not in nck or oc[p] != nc[p]]) - set(["WALLABY_CONFIG_VERSION"])
        mc_params = set([p for p in params if store_singleton().getParam(p).must_change])
        
        subsystems = [store_singleton().getSubsys(sub) for sub in self.cm.list_objects("subsystem")]
        
        restart, reconfig = [], []
        
        for ss in subsystems:
            ss.refresh
            ssp = set(ss.parameters)
            if ssp.intersection(mc_params):
                restart.append(ss.name)
            elif ssp.intersection(params):
                reconfig.append(ss.name)
        
        return [list(params), restart, reconfig]
        
    
    # labeling support below
    def getLabels(self):
        memberships = self.memberships
        if not PARTITION_GROUP in memberships:
            return []
        else:
            partition = memberships.index(PARTITION_GROUP)
            return memberships[partition+1:]
    
    labels=property(getLabels)
    
    def modifyLabels(self, op, labels, **options):
        thestore = store_singleton()
        memberships = self.memberships
        current_labels = self.getLabels()
        label_set = set(current_labels + [PARTITION_GROUP])
        new_labels = []
        
        if op == "ADD":
            new_labels = current_labels + labels
            pass
        elif op == "REPLACE":
            new_labels = labels
            pass
        elif op == "REMOVE":
            new_labels = [label for label in current_labels if label not in labels]
        else:
            raise NotImplementedError("modifyLabels:  operation " + op + " not understood")
        
        just_memberships = [grp for grp in memberships if grp not in label_set]
        new_memberships = uniq(just_memberships + [PARTITION_GROUP] + new_labels)
        
        if "ensure_partition_group" in options and options["ensure_partition_group"] is not False:
            if thestore is None:
                raise RuntimeError("store singleton must be initialized before using the ensure_partition_group option")
            thestore.getPartitionGroup()
        
        if "create_missing_labels" in options and options["create_missing_labels"] is not False:
            if thestore is None:
                raise RuntimeError("store singleton must be initialized before using the create_missing_labels option")
            for missing_label in thestore.checkGroupValidity(new_labels):
                thestore.addLabel(missing_label)
        
        return self.modifyMemberships("REPLACE", new_memberships, {})

proxied_attr(node, "name")
proxied_attr(node, "memberships")
proxied_attr(node, "identity_group")
proxied_attr(node, "provisioned")