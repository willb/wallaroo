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

class How(object):
    def __init__(self, how, what=None):
        self.how = how
        self.what = what
    
    def to_q(self):
        "Note that requests allows passing query parameters as hashes"
        return self.how == "none" and None or {self.how : self.what}
    
    def update(self, sha):
        if self.how == "branch":
            return
        self.how = "commit"
        self.what = sha