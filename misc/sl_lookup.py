#!/usr/bin/env python

"""To use this script, start in the source dir where plotscr.hsd and slices.bi
   are loacted, and run:

      python misc/sl_lookup.py

   The constants from slices.bi will be converted over to plotscr.hsd
   Note that you don't *need* this script. You can just update plotscr.hsd
   by hand if you wish.
"""


import re

########################################################################

class Lookup(object):
  
    def __init__(self, name, code):
        self.name = name
        self.code = code

    def basic_name(self):
        return self.name.upper().replace(" ", "_")

    def hspeak_name(self):
        return self.name.lower().replace("_", " ")

#-----------------------------------------------------------------------

class Lookup_Updater(object):

    def __init__(self):
        self.lookups = []
        self.longest = 0

    def add(self, name, code):
        l = Lookup(name, code)
        self.lookups.append(l)
        if len(name) > self.longest:
            self.longest = len(name)

    def pad(self, n):
        format = "%%-%ds" % (self.longest)
        return format % n

    def basic(self):
        result = ""
        for l in self.lookups:
            n = l.basic_name()
            n = self.pad(n)
            result += "CONST SL_%s = %d\n" % (n, l.code)
        return result

    def hspeak(self):
        result = "define constant, begin\n"
        for l in self.lookups:
            result += "%d, sl:%s\n" % (l.code, l.hspeak_name())
        result += "end\n"
        return result

#-----------------------------------------------------------------------

class Reader(object):
  
    def __init__(self, filename, look):
        print filename
        f = open(filename, "r")
        txt = f.read()
        f.close()
        pattern = r"^\'[ \t]*\<SLICE LOOKUP CODES\>.*^\'[ \t]*\<\/SLICE LOOKUP CODES\>"
        regex = re.compile(pattern, re.M|re.S)
        match = regex.search(txt)
        if not match:
            raise Exception("failed to find comment markers")
        constants = match.group(0).split("\n")
        regex = re.compile("^[ \t]*CONST SL_(.*?)[ \t]*=[ \t]*(-?\d+)[ \t]*$", re.I)
        for line in constants:
            match = regex.match(line)
            if match:
                name = match.group(1)
                code = int(match.group(2))
                print name, code
                look.add(name, code)

#-----------------------------------------------------------------------

class Replacer(object):
  
    def __init__(self, filename, replacement):
        print filename
        f = open(filename, "r")
        txt = f.read()
        f.close()
        pattern = r"^\#[ \t]*\<SLICE LOOKUP CODES\>.*^\#[ \t]*\<\/SLICE LOOKUP CODES\>"
        regex = re.compile(pattern, re.M|re.S)
        if not regex.search(txt):
            raise Exception("failed to find comment markers")
        replacement = "#<SLICE LOOKUP CODES>\n" + replacement + "#</SLICE LOOKUP CODES>"
        txt = regex.sub(replacement, txt)
        f = open(filename, "w")
        f.write(txt)
        f.close()

########################################################################

look = Lookup_Updater()
reader = Reader("slices.bi", look)
repl = Replacer("plotscr.hsd", look.hspeak())
