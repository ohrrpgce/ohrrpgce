#!/usr/bin/env python

"""Adaptable example of using ScriptScanner to find uses of script commands, or decompile certain scripts."""

from rpgbatch.scanscripts import ScriptScanner

class ScrScanExample(ScriptScanner):

    def setup(self):
        # cmd_logging: Script commands to be logged and printed.
        # You can also add standard (plotscr.hsd) script names to this, eg. 'minutesofplay':''

        if True:
            # noop, stringfromtextbox, initmouse, readgeneral, writegeneral, readgmap, writegmap, readenemydata, writeenemydata
            self.cmd_logging = {0:'', 240:'', 159:'', 147:'', 148:'', 178:'', 179:'', 230:'', 231:''}

        if False:
            # 159,initmouse,0             # init mouse, return true if a mouse is installed
            # 160,mousepixelx,0           # returns mouse x coordinate on the screen
            # 161,mousepixely,0           # returns mouse y coordinate on the screen
            # 162,mousebutton,1,0         # returns true if the specified button is pressed
            # 163,putmouse,2,160,100      # places the mouse at a point on the screen
            # 164,mouseregion,4,-1,-1,-1,-1 # define the rectangle in which the mouse can move (xmin, xmax, ymin, ymax)
            # 492,mouseclick,1,0            # returns true if the specified button is pressed (button)
            # 601,unhidemousecursor,0       # unhides the OS mouse cursor
            # 602,hidemousecursor,0         # hides the OS mouse cursor
            self.cmd_logging = {}
            for i in range(159,164+1) + [492,601,602]:
                self.cmd_logging[i] = ''

        # A list of script names, either standard or not. Every version of a script matching this name
        # will be decompiled and dumped
        self.decompile_scripts = []

    def process_script(self, rpg, gameinfo, zipinfo, script):
        if script.name in self.decompile_scripts:
            already_decompiled = False
            if script.seen_before:
                for seen_script in self.scripthashes[script.md5]:
                    if seen_script.name == script.name:
                        already_decompiled = True
                        break
            if not already_decompiled:
                print
                print script
                print

    def print_duplicate_scripts(self):
        "Print list of all scripts that were duplicated"
        dups = 0
        almost_dup = 0
        for md5, scripts in self.scripthashes.iteritems():
            if len(scripts) > 1 and scripts[0].name not in self.standardindex:
                dups += 1
                games = []
                duptext = ''
                for script in scripts:
                    if True:  #if script.game not in games:
                        games.append(script.game)
                        duptext += "\n   " + script.name + " in " + script.game
                if len(games) > 1:
                    if len(set(script.gamename for script in scripts)) == 1:
                        almost_dup += 1
                    else:
                        print "Script duplicated", len(games), "times:", duptext
        print
        print "%d sets of duplicate scripts. %d were from same game or different versions of it, not printed" % (dups, almost_dup)

    def print_results(self):
        self.print_source_stats()
        self.print_duplicate_scripts()
        self.print_logged_commands()
        self.print_command_usage()


ScrScanExample().run()
