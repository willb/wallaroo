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

import inspect

FAILURE = 1
  
# Kinds of failure
INTERNAL_ERROR = FAILURE << 1
NOT_IMPLEMENTED = FAILURE << 2
BAD_ARGUMENT = FAILURE << 3
  
# bits 4 -- 6 reserved for future high-level failures
UNAUTHORIZED = FAILURE << 7
NAME_ALREADY_IN_USE = FAILURE << 8
INVALID_NAME = FAILURE << 9
NONEXISTENT_ENTITY = FAILURE << 10
BAD_COMMAND = FAILURE << 11
BAD_QUERY = FAILURE << 12
CIRCULAR_RELATIONSHIP = FAILURE << 13
INVALID_RELATIONSHIP = FAILURE << 14
ARGUMENT_TOO_LONG = FAILURE << 15
  
# Entities implicated in failures
UNKNOWN = FAILURE << 22
SNAPSHOT = FAILURE << 23
NODE = FAILURE << 24
GROUP = FAILURE << 25
FEATURE = FAILURE << 26
PARAMETER = FAILURE << 27
SUBSYSTEM = FAILURE << 28

# Convenience methods
def make(*error_kinds):
    return reduce(lambda x, y: x | y, error_kinds, FAILURE)

def fail(how, msg="no further information"):
    raise RuntimeError("failed with error %d (%s)" % (how, msg))

def not_implemented(msg=""):
    whence = inspect.stack()[1][3]
    if len(msg) > 0:
        msg = " (%s)" % msg
    raise RuntimeError("%s not implemented%s" % (whence, msg))
