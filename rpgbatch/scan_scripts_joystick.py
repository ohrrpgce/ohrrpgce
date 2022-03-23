#!/usr/bin/env python

"""Find all uses of joystick access commands,
(and previously more)."""

from nohrio.scripts import kInt
from rpgbatch.scanscripts import ScriptScanner

class ScanJoyScripts(ScriptScanner):
    def setup(self):
        self.cmd_logging = {
            # keyval, keyispressed, joystickbutton, joystickaxis
            30:'', 235:'', 242:'', 243:'',
            # keypress, newkeypress, getjoystickname, joystickbuttoncount, joystickaxiscount, joystickhatcount
            678:'', 679:'', 680:'', 681:'',
        }

    def visit_node(self, cmd_or_script, node, script, gameinfo):
        """Called for all script nodes which match a command or script in self.cmd_logging.
        cmd_or_script: a command id or a script name"""
        if node.id == 243:  # joystickaxis
            joynum = node.arg(2, 0)
        elif 679 <= node.id <= 681:  # joystickbuttoncount, joystickaxiscount, joystickhatcount
            joynum = node.arg(0, 0)
        else:
            joynum = node.arg(1, 0)
        logit = False
        if joynum.kind != kInt or joynum.id != 0:  # Not joystick 0
            logit = True
        if node.id in (30, 235, 678, 679):
            arg = node.arg(0)
            if arg.kind != kInt:
                # This is usually used for looping over keys or remappable controls
                pass  #logit = True
            elif arg.id >= 128:   # joy:* scancode
                logit = True
        else:
            logit = True
        if logit:
            super(ScanJoyScripts, self).visit_node(cmd_or_script, node, script, gameinfo)

    def print_results(self):
        self.print_logged_commands()


ScanJoyScripts().run()
