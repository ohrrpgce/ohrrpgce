#!/usr/bin/env python

"""To use this script, start in the source dir where plotscr.hsd and slices.bi
   are loacted, and run:

      python misc/sl_lookup.py

   The constants from slices.bi will be converted over to slices.bas,
   plotscr.hsd and plotdict.xml. Note that you don't *need* this script.
   You can just update slices.bas, plotscr.hsd and plotdict.xml by hand
   if you wish.
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
        if code in [l.code for l in self.lookups]:
            raise Exception("slice code %d is used more than once" % code)
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

    def docs(self):
        result = ""
        for l in self.lookups:
            result += "lookup slice(sl:%s)\n" % (l.hspeak_name())
        return result

    def names(self):
        result = ""
        for l in self.lookups:
            n = l.basic_name()
            result += '  CASE SL_%s: RETURN "%s"\n' % (n, n.lower())
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

    pattern = r"^\#[ \t]*\<SLICE LOOKUP CODES\>.*?^\#[ \t]*\<\/SLICE LOOKUP CODES\>"
    replace = "#<SLICE LOOKUP CODES>\n%s#</SLICE LOOKUP CODES>"
  
    def __init__(self, filename, replacement):
        print filename
        f = open(filename, "r")
        txt = f.read()
        f.close()
        regex = re.compile(self.pattern, re.M|re.S)
        if not regex.search(txt):
            raise Exception("failed to find comment markers")
        replacement = self.replace % (replacement)
        txt = regex.sub(replacement, txt)
        f = open(filename, "w")
        f.write(txt)
        f.close()

class DocReplacer(Replacer):
    pattern = r"^# This is a list of special slice lookup codes.*?^\<\/example\>"
    replace = "# This is a list of special slice lookup codes\n%s</example>"

class NameReplacer(Replacer):
    pattern = r"^\'[ \t]*\<SLICE LOOKUP NAMES\>.*?^\'[ \t]*\<\/SLICE LOOKUP NAMES\>"
    replace = "'<SLICE LOOKUP NAMES>\n%s'</SLICE LOOKUP NAMES>"

########################################################################

look = Lookup_Updater()
reader = Reader("slices.bi", look)
repl = Replacer("plotscr.hsd", look.hspeak())
repl = DocReplacer("docs/plotdict.xml", look.docs())
repl = NameReplacer("slices.bas", look.names())
