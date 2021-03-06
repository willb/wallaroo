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

import re

def camelcase(value):
    def ccgen():
        yield type(value).lower
        while True:
            yield type(value).capitalize
    gen = ccgen()
    return "".join(gen.next()(x) if x else '_' for x in value.split("_"))

def pluralize(val):
    return re.sub("hs$", "hes", ("%ss" % val).lower())
    
def sha_for(cm):
    if cm.how.how == "commit":
        return cm.how.what
    where = None
    
    if cm.how == "none":
        where = cm.make_proxy_object("tag", "empty")
    else:
        where = cm.make_proxy_object(cm.how.how, cm.how.what)
    
    where.refresh()
    return where.commit()