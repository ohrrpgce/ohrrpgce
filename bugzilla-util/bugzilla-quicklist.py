#!/usr/bin/env python

import sys
import wca_mysql

def no_log(s):
  pass

def stderr_log(s):
  sys.stderr.write(s + "\n")

sql_log = no_log

bugzilla = wca_mysql.MySQL("bugzilla.cfg", sql_log)
bugs = bugzilla.table("bugs")
results = bugs.select(what="bug_id, short_desc, bug_status, resolution")

for row in results:
  print "%s\t%s\t%s\t%s" % (row["bug_id"], row["bug_status"], row["resolution"], row["short_desc"])
