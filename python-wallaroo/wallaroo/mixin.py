# Decorators to specify mixins for the Wallaby client library.
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

def patch_to(klass):
    def do_mixin(target):
        setattr(klass, target.__name__, target)
        return target
    return do_mixin

def calculated_attribute(klass, attr_name):
    """
# Think of this as a way to add an attribute/property to another class.
# Example usage:
class Foo(object):
    pass

class Blah(object):
    # NB:  you cannot call this with a reference to the class you're declaring
    @calculated_attribute(Foo, "bar")
    def myBlah(self):
        return self.blah
    def setBlah(self, b):
        self.blah = b
    """
    def do_calculated_attribute(target):
        def default_fallback(self, name):
            raise AttributeError("%r has no attribute named %r") % (self,name)
        fallback = None
        
        if "__getattr__" in dir(klass):
            fallback = klass.__getattr__
        else:
            fallback = default_fallback
        
        def calc_attr(self, name):
            if name == attr_name:
                return target(self)
            else:
                return fallback(self, name)
        
        klass.__getattr__ = calc_attr
        
        return target
    
    return do_calculated_attribute
