#!/usr/bin/env python

"""rpgbatch script to look for script identifiers (currently just script names)
which contain unusual characters."""

import re
from rpgbatch.scanscripts import ScriptScanner

class ScrScanIdentifiers(ScriptScanner):
    def setup(self):
        self.strange_names = []

    def process_script(self, rpg, gameinfo, zipinfo, script):
        if re.search('(^[0-9-]|[^a-z0-9:_-])', script.name):
            self.strange_names.append((script.name, gameinfo.id))

    def print_results(self):
        if len(self.strange_names):
            print "Saw script names containing unusual characters:"
            for name, game in self.strange_names:
                print "\"" + name + "\" in " + game


ScrScanIdentifiers().run()
