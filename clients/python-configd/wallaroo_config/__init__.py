# configd-specific Wallaroo client
# is intended to work with Python 2.4
# other applications should use wallaroo.client

try:
    import json
except ImportError:
    import simplejson as json

import urllib2
import urllib
import urlparse

from datetime import datetime
import calendar

import os
import socket

def ts():
    now = datetime.utcnow()
    return (calendar.timegm(now.utctimetuple()) * 1000000) + now.microsecond

def mk_how(options):
    PREFERRED_ORDER = ["branch", "tag", "commit", "none"]
    hows = [How(k, options[k]) for k in PREFERRED_ORDER if options.has_key(k)]
    return len(hows) > 0 and hows[0] or How("tag", "current")
    
def mk_url(cm, path):
    return urlparse.urlunparse((cm.scheme, "%s:%d" % (cm.host, cm.port), path, None, None, None))

class http_result(object):
    def __init__(self, handler):
        self.handler = handler
    
    status_code = property(lambda self: self.handler.getcode())
    headers = property(lambda self: self.handler.info.headers())

    def geturl(self):
        return self.handler.geturl()
    
    def json(self):
        return json.load(self.handler)


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

class ConnectionMeta(object):
    def __init__(self, host="localhost", port=8000, scheme="http", username=None, pw=None, **kwargs):
        self.host = host
        self.port = port
        self.scheme = scheme
        self.username = username
        self.how = mk_how(kwargs)
    
    def __qs(self, custom_q=None):
        q = ""
        if custom_q is None:
            q = urllib.urlencode(self.how.to_q())
        else:
            q = urllib.urlencode(custom_q)
        return q
    
    def __path(self, path, q):
        return urlparse.urlunparse((self.scheme, "%s:%d" % (self.host, self.port), path, None, q, None))
    
    def __get(self, path, custom_q=None):
        # XXX: no auth yet
        q = self.__qs(custom_q)
        uri = self.__path(path, q)
        req = urllib2.Request(uri)
        return http_result(urllib2.urlopen(req))
    
    def __put(self, path, dct, custom_q=None):
        q = self.__qs(custom_q)
        uri = self.__path(path, q)
        payload = json.dumps(dict([(k,v) for (k,v) in dct.iteritems()]))
        
        opener = urllib2.build_opener(urllib2.HTTPHandler)
        request = urllib2.Request(uri, data=payload)
        request.add_header('content-type', 'application/json')
        request.get_method = lambda: 'PUT'
        return http_result(opener.open(request))
    
    def list_objects(self, kind_plural):
        return self.fetch_json_resource("/%s" % kind_plural)
    
    def fetch_json_resource(self, path, query=None, default=None):
        q = query and query or self.how.to_q()
        if default is None:
            return self.__get(path, q).json()
        else:
            try:
                return self.__get(path, q).json()
            except urllib2.HTTPError, e:
                return default
    
    def put_json_resource(self, path, dct, skip_q=False):
        q = not skip_q and self.how.to_q() or {}
        response = self.__put(path, dct, q)
        
        if response.status_code < 200 or response.status_code > 399:
            raise RuntimeError("Error %d:  %s" % (response.status_code, response.text))

class client(object):
    def __init__(self, nodename, cm):
        self.cm = cm
        self.nodename = nodename
        self.nodestruct = {}
        self.last_current = None
        self.gs = []
        self.fs = []
        self.refresh()
    
    def __current_sha(self):
        c = self.cm.fetch_json_resource("/tags/current", {}, {"commit":0})
        return c["commit"]
    
    def refresh(self, force=False):
        cs = self.__current_sha()
        if self.last_current != cs or force:
            self.last_current = cs
            self.nodestruct = self.cm.fetch_json_resource("/nodes/%s" % self.nodename, {"tag":"current"}, {})
        return self.last_current
    
    def __val(self, key, default):
        return self.nodestruct.has_key(key) and self.nodestruct[key] or default
    
    def config(self, **options):
        if options.has_key("version"):
            return self.cm.fetch_json_resource("/config/node/%s" % self.nodename, {"commit":options["version"]}, {})
        return self.cm.fetch_json_resource("/config/node/%s" % self.nodename, {"tag":"current"}, {})
    
    def last_updated(self):
        return self.__val("last_updated", 0)
    
    def __subsys(self, ss):
        return self.cm.fetch_json_resource("/subsystems/%s" % ss, {"tag":"current"}, {"parameters":[]})
    
    def all_subsystems(self):
        return self.cm.fetch_json_resource("/subsystems/", {"tag":"current"}, [])
    
    def all_params(self):
        return self.cm.fetch_json_resource("/parameters/", {"tag":"current"}, [])
    
    def __param_must_change(self, param):
        p = self.cm.fetch_json_resource("/parameters/%s" % param, {"tag":"current"}, {"must_change":False})
        return p["must_change"]
    
    def whatChanged(self, oldver, newver):
        oc = self.cm.fetch_json_resource("/config/node/%s" % self.name, {"commit":old}, {})
        nc = self.cm.fetch_json_resource("/config/node/%s" % self.name, {"commit":new}, {})
        
        ock = set(oc)
        nck = set(nc)
        
        params = set([p for p in (ock | nck) if p not in ock or p not in nck or oc[p] != nc[p]]) - set(["WALLABY_CONFIG_VERSION"])
        mc_params = set([p for p in params if self.__param_must_change(p)])
        
        subsystems = [self.__subsys(ss) for sub in self.all_subsystems()]
        
        restart, reconfig = [], []
        
        for ss in subsystems:
            ssp = set(ss["parameters"])
            if ssp.intersection(mc_params):
                restart.append(ss.name)
            elif ssp.intersection(params):
                reconfig.append(ss.name)
        
        return [list(params), restart, reconfig]
        
    
    def checkin(self):
        metapath = "/meta/node/%s" % self.nodename
        # now = datetime.utcnow().isoformat()
        now = ts()
        meta = self.cm.fetch_json_resource(metapath, False, default={})
        meta["last-checkin"] = now
        self.cm.put_json_resource(metapath, meta, False)
        return now
    
    def memberships(self):
        acc = ["+++DEFAULT"]
        idgroup = self.__val("identity_group", None)
        
        if idgroup is not None:
            acc = acc + [idgroup]
        
        return acc + self.__val("memberships", ["+++SKEL"])
    
    def __ffg(self, group):
        g = self.cm.fetch_json_resource("/groups/%s" % group, {"tag":"current"}, {"features":[]})
        return g["features"]
        
    def features(self):
        ms = self.memberships()
        return list(set([]).union(*[set(self.__ffg(g)) for g in ms]))
        
def connect(**options):
    defaults = {
        "host":os.getenv("WALLAROO_HOST") or "localhost", 
        "port":os.getenv("WALLAROO_PORT") or 8000, 
        "scheme" : os.getenv("WALLAROO_SCHEME") or "http", 
        "username": os.getenv("WALLAROO_USER") or None, 
        "pw": os.getenv("WALLAROO_PASS") or None,
        "node": socket.gethostname()
    }
    
    for k in options:
        defaults[k] = options[k]
    
    nodename = defaults["node"]
    del defaults["node"]
    return client(nodename, ConnectionMeta(**defaults))