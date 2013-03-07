#!/usr/bin/python

import os
import shlex
import socket
import subprocess
import time
import wallaroo.client

store = wallaroo.client.connect(**{"tag":"current"})

conf_file = "condor_config.wallaroo"
ver = None
curr_ver = 0

proper = os.getenv("WALLAROO_PROPER") is not None

if proper:
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
  if ver != curr_ver:
    print "Writing config version '%s'" % str(ver)
    config = node.getConfig()
    fobj = open(conf_file, 'w')
    fobj.seek(0, 0)
    for key in config.keys():
      fobj.write("%s = %s\n" % (key, config[key]))
    fobj.close()
    curr_ver = ver
    if proper:
        cobj = subprocess.Popen(["condor_reconfig"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (out, err) = cobj.communicate()
        print out
  time.sleep(45)

