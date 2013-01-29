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

import errors
from errors import fail
import itertools

def default_get(d, key, default=None):
    return d.has_key(key) and d[key] or default

def default_getattr(o, key, default=None):
    return hasattr(o,key) and getattr(o,key) or default

def uniq(ls):
    def gen_check_and_add(seen):
      def ca(elt):
        result = elt in seen
        seen.add(elt)
        return result
      return ca
    c_and_a = gen_check_and_add(set([]))
    return [x for x in ls if not c_and_a(x)]

def combine(old, new):
    [x for y in old, new for x in y]

def intersect_collections(f,s):
    return f & s

def union_collections(f, s):
    return f | s

def diff_collections(f, s):
    return (f | s) - (f & s)

def arcmethod(getfun, setfun, **kwargs):
    explain = default_get(kwargs, "explain", "have an arc to")
    preserve_order = default_get(kwargs, "preserve_order", False)
    heterogeneous = default_get(kwargs, "heterogeneous", False)
    
    def modify_arcs(self, command, dests, options):
        keyfun = self.name
        command = command.upper()
        what = default_get(kwargs, "what", self.__class__.__name__.lower())
        errwhat = default_getattr(errors, what.upper(), errors.UNKNOWN)
        
        if command == "ADD":
            old_dests = preserve_order and getfun() or set(getfun())
            new_dests = preserve_order and dests or set(dests)
            if keyfun() in new_dests and not heterogeneous:
                fail(errors.make(errors.CIRCULAR_RELATIONSHIP, errors.INVALID_RELATIONSHIP, errwhat), "%s %s cannot %s itself" % (what, name, explain))
            
            setfun(uniq(itertools.chain(old, new)))
            self.update()
        elif command == "REPLACE":
            new_dests = preserve_order and dests or set(dests)
            if keyfun() in new_dests and not heterogeneous:
                fail(errors.make(errors.CIRCULAR_RELATIONSHIP, errors.INVALID_RELATIONSHIP, errwhat), "%s %s cannot %s itself" % (what, name, explain))
            
            setfun(new_dests)
            self.update()
        elif command == "REMOVE":
            setfun([dest for new_dests in getfun() if not dest in set(dests)])
            self.update()
        elif command in ["INTERSECT", "DIFF", "UNION"]:
            if preserve_order:
                fail(errors.make(errors.INTERNAL_ERROR, errors.NOT_IMPLEMENTED, errwhat), "%s not implemented for order-preserving relations" % command)
            
            op = globals()["%s_collections" % command.lower()]
            old_dests = getfun()
            supplied_dests = set(dests)
            new_dests = op(old_dests, supplied_dests)
            
            if keyfun() in new_dests and not heterogeneous:
                fail(errors.make(errors.CIRCULAR_RELATIONSHIP, errors.INVALID_RELATIONSHIP, errwhat), "%s %s cannot %s itself" % (what, name, explain))
            
            setfun(new_dests)
            self.update()
        else:
            fail(errors.make(errors.BAD_COMMAND, errwhat), "Invalid command %s" % command)
    return modify_arcs