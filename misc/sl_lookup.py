#!/usr/bin/env python
#This is not linked with any other code. It is just run by itself to
#update slices.bi and plotscr.hsd

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

class Replacer(object):
  
    def __init__(self, filename, commentmark, replacement):
        print filename
        f = open(filename, "r")
        txt = f.read()
        f.close()
        pattern = r"^%(commentmark)s[ \t]*\<SLICE LOOKUP CODES\>.*^%(commentmark)s[ \t]*\<\/SLICE LOOKUP CODES\>" % {"commentmark": re.escape(commentmark)}
        regex = re.compile(pattern, re.M|re.S)
        if not regex.search(txt):
            raise Exception("failed to find comment markers")
        replacement = commentmark + "<SLICE LOOKUP CODES>\n" + replacement + commentmark + "</SLICE LOOKUP CODES>"
        txt = regex.sub(replacement, txt)
        f = open(filename, "w")
        f.write(txt)
        f.close()

########################################################################

look = Lookup_Updater()

look.add("TEXTBOX_TEXT"     , -100001)
look.add("TEXTBOX_PORTRAIT" , -100002)
look.add("TEXTBOX_CHOICE0"  , -100003)
look.add("TEXTBOX_CHOICE1"  , -100004)

repl = Replacer("slices.bi", "'", look.basic())
repl = Replacer("plotscr.hsd", "#", look.hspeak())
