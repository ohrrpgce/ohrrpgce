#!/usr/bin/env python3

"""Look for uses of get/setherostat with undocumented stat IDs: to get/set the hero level or gained levels
(and previously more)."""

from nohrio.scripts import kInt
from rpgbatch.scanscripts import ScriptScanner

class ScanHeroStat(ScriptScanner):
    def setup(self):
        # setherostat, getherostat
        self.cmd_logging = {83:'', 64:''}

    def visit_node(self, cmd_or_script, node, script, gameinfo):
        """Called for all script nodes which match a command or script in self.cmd_logging.
        cmd_or_script: a command id or a script name"""
        stat = node.arg(1)
        if stat.kind != kInt or stat.id < 0 or stat.id > 11:
            super(ScanHeroStat, self).visit_node(cmd_or_script, node, script, gameinfo)

    def print_results(self):
        self.print_logged_commands()


ScanHeroStat().run()
