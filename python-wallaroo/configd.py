#!/usr/bin/python

import shlex
import socket
import subprocess
import time
import wallaby
from qmf.console import Session

session = Session ()
broker = session.addBroker()

obj, = session.getObjects(_class="Store", _package='com.redhat.grid.config')
store = wallaby.Store(obj, session)

cmds = shlex.split("condor_config_val WALLABY_CONFIG_VERSION")
cobj = subprocess.Popen(cmds, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
(out, err) = cobj.communicate()
curr_ver = out.strip()
print "My current version is '%s'" % str(curr_ver)

cmds = shlex.split("condor_config_val LOCAL_CONFIG_FILE")
cobj = subprocess.Popen(cmds, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
(out, err) = cobj.communicate()
conf_file = out.strip()
print "Writing config to '%s'" % conf_file

name = socket.gethostname()
while True:
  print "Checking version in store"
  node = store.getNode(name)
  ver = str(node.last_updated_version)
  if ver != curr_ver:
    print "Writing config version '%s'" % str(ver)
    config = node.getConfig({'version':ver})
    fobj = open(conf_file, 'w')
    fobj.seek(0, 0)
    for key in config.keys():
      fobj.write("%s = %s\n" % (key, config[key]))
    fobj.close()
    curr_ver = ver
  time.sleep(10)

