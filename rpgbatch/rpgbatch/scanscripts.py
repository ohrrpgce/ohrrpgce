#!/usr/bin/env python

"""A ScriptScanner class for iterating over each script of each game, using rpgbatch+nohrio."""

import os
import sys
import numpy as np
import re
from nohrio.ohrrpgce import *
from nohrio.wrappers import OhrData
from nohrio.scripts import *
from rpgbatch import RPGIterator, RPGInfo


def iter_script_tree2(root):
    yield root
    for arg in root.args():
        # Wow! Passing each item back up the call chain has got to be inefficient!
        for ret in iter_script_tree(arg):
            yield ret

# This is about 30% faster than iter_script_tree2... disappointing
def iter_script_tree(root):
    "Iterate over the descendents of a script node"
    node = ScriptNode(root.scriptset(), root.script, root.offset)
    yield node
    if root.argnum == 0:
        return
    lst = [root.offset + 3, root.argnum]
    data = root.scrdata()
    while len(lst):
        node.offset = data[lst[-2]]
        yield node
        if lst[-1] == 1:
            lst = lst[:-2]
        else:
            lst[-2] += 1
            lst[-1] -= 1
        argnum = node.argnum
        if argnum:
            lst.append(node.offset + 3)
            lst.append(argnum)


class ScriptScanner(object):
    """Base class for iterating over each script of each game, using rpgbatch+nohrio,
    and collecting interesting stats on scripts, especially command usage.

    Generally, you extend this class and override setup() (to set self.cmd_logging)
    and/or print_results().
    """

    def setup(self):
        """Optionally override this."""
        pass

    def print_results(self):
        "Optionally override this."
        self.print_logged_commands()
        self.print_source_stats()
        self.print_command_usage()

    def visit_node(self, cmd_or_script, node, script, gameinfo):
        """Optionally override this.
        Called for all script nodes which match a command or script in self.cmd_logging.
        cmd_or_script: a command id or a script name
        """
        self.cmd_logging[cmd_or_script] += "Found in " + script.name + " in " + gameinfo.name + ":\n   " + str(node) + '\n'

    def __init__(self):

        # All commands or scripts in cmd_logging will be logged and printed.
        # The keys are either IDs (e.g. 159 for initmouse) for commands, or "standard
        # script" (scripts in plotscr.hsd) names, eg. 'minutesofplay'.
        # The values should be initialised to '', e.g. {159: ''}
        self.cmd_logging = {}

        self.commands_info = {}
        self.scripthashes = {}

        self.standardscrs = {'names': [], 'versions':[], 'games':[]}
        self.standardindex = {}

        self.parse_args()  # Loads standardscrs from plotscr.hs, if on the commandline

        self.setup()

    def load_plotscr_hs(self, filename):
        stdnames = self.standardscrs['names']
        scriptset = HSScripts(filename)
        for name in scriptset.scriptnames.values():
            if name not in stdnames:
                self.standardindex[name] = len(stdnames)
                stdnames.append(name)
        self.commands_info.update(scriptset.commands_info)
        del scriptset
        print "Read", len(stdnames), "standard scripts from", filename
        self.standardscrs['versions'] = [0 for x in stdnames]
        self.standardscrs['games'] = [[] for x in stdnames]

        # A few special cases for scripts which were removed from plotscr.hsd
        # (all of these were in fact replaced with builtin commands)
        for n in ('setstring', 'appendstring', 'suspendmapmusic', 'resumemapmusic', 'setenemyrewards', 'getenemyrewards'):
            if n not in stdnames:
                self.standardindex[name] = len(stdnames)
                stdnames.append(n)

    def parse_args(self):
        if len(sys.argv) < 2:
            print "Usage:"
            print " " + sys.argv[0] + " [plotscr.hs]* [src:identifier] locations ..."
            print "Optionally pass one or more copies of plotscr.hs (plotscr.hsd compiled)"
            print "as the first argument to identify the standard scripts."
            print "More advanced options are available only by writing a script."
            print "Specify .rpg files, .rpgdir directories, .zip files, or directories containing"
            print "any of these as arguments."
            sys.exit()

        rpgsources = sys.argv[1:]
        while len(rpgsources) and rpgsources[0].endswith('.hs'):
            self.load_plotscr_hs(rpgsources[0])
            rpgsources.pop(0)
        self.rpgbatch_args = rpgsources

    def commandname(self, id):
        if id in self.commands_info:
            return self.commands_info[id]['name']
        return "cmd%d" % id

    def run(self):
        """Scans the games specified on the commandline, then calls self.print_results()"""

        scriptbytes = 0
        scriptuniquebytes = 0
        scriptuniquenum = 0

        # We'll store usage counts for both commands and standard scripts in cmdcounts.
        # The scripts start at 2000.
        table_size = 2000 + len(self.standardscrs['names'])

        self.cmdcounts = np.zeros(shape = (0, table_size), dtype = np.int32)
        self.cmdcounts_in_plotscrhsd = np.zeros((table_size), np.int32)

        self.rpgidx = np.zeros(shape = 0, dtype = RPGInfo)

        rpgs = RPGIterator(self.rpgbatch_args)
        for rpg, gameinfo, zipinfo in rpgs:
            gameinfo.has_source = False
            gameinfo.scripts_backup = False
            if zipinfo:
                gameinfo.has_source = (len(zipinfo.scripts) > 0)
            self.rpgidx = np.append(self.rpgidx, gameinfo)

            # Counts of commands/standard scripts used in this game
            cmdusage = np.zeros((1, table_size), np.int32)

            hspfile = rpg.lump_path('.hsp')
            gameinfo.has_scripts = os.path.isfile(hspfile)
            if gameinfo.has_scripts:
                scriptset = HSScripts(hspfile)
                gameinfo.scripts_backup = scriptset.source

                # Use whichever commands.bin lump has the most in it
                if len(scriptset.commands_info) > len(self.commands_info):
                    self.commands_info = scriptset.commands_info

                # Map script IDs to standardscrs indices
                id_to_standardindex = {}
                for script_id, name in scriptset.scriptnames.iteritems():
                    idx = self.standardindex.get(name)
                    if idx:
                        id_to_standardindex[script_id] = idx

                if hasattr(self, 'process_game_with_scripts'):
                    self.process_game_with_scripts(rpg, gameinfo, zipinfo, scriptset)

                for script_id in scriptset.scriptnames.iterkeys():
                    script = scriptset.script(script_id)
                    if not script:
                        continue
                    #script = copy(script)
                    #script.drop_data()
                    script.game = gameinfo.name  # RPG file and long name
                    script.gamename = gameinfo.longname
                    scriptbytes += script.lump_size
                    is_standard_script = script.name in self.standardindex

                    # The first Script in each list of Scripts in scripthashes has a vector of
                    # command usage counts for that script (hacky)
                    md5 = script.invariate_md5()
                    script.md5 = md5
                    script.seen_before = md5 in self.scripthashes

                    if hasattr(self, 'process_script'):
                        self.process_script(rpg, gameinfo, zipinfo, script)

                    if script.seen_before:
                        # Only need one copy of the script data
                        script.drop_data()
                        self.scripthashes[md5].append(script)
                        # Commands in standard scripts are only counted once per unique copy of the script.
                        # Commands in other scripts are counted for each game they appear in
                        if not is_standard_script:
                            cmdusage[0] += self.scripthashes[md5][0].cmdusage
                    else:
                        script.cmdusage = np.zeros((table_size), np.int32)
                        scriptuniquenum += 1
                        self.scripthashes[md5] = [script]
                        scriptuniquebytes += script.lump_size

                        for node in iter_script_tree(script.root()):
                            kind = node.kind
                            if kind == kCmd:
                                # Count command usage
                                script.cmdusage[node.id] += 1
                                # Ignore occurrences in standard scripts
                                if not is_standard_script and node.id in self.cmd_logging:
                                    self.visit_node(node.id, node, script, gameinfo)
                            elif kind == kScript:
                                # Count standard script usage
                                idx = id_to_standardindex.get(node.id)
                                if idx:
                                    script.cmdusage[2000 + idx] += 1
                                    # Ignore occurrences in standard scripts
                                    if not is_standard_script:
                                        node_script_name = self.standardscrs['names'][idx]
                                        if node_script_name in self.cmd_logging:
                                            self.visit_node(node_script_name, node, script, gameinfo)
                        if is_standard_script:
                            self.cmdcounts_in_plotscrhsd += script.cmdusage
                        else:
                            cmdusage[0] += script.cmdusage

                # When scriptset is closed underlying file is closed, practically equivalent to drop_data
                # on all scripts in scripthashes
                scriptset.close()
                del scriptset

            self.cmdcounts = np.append(self.cmdcounts, cmdusage, axis = 0)

        self.rpgidx = self.rpgidx.view(OhrData)

        print
        rpgs.print_summary()
        print "Scanned %d unique scripts totalling %.2f MB (%.2f MB nonunique)" % (scriptuniquenum, scriptuniquebytes / 2.**20, scriptbytes / 2.**20)

        self.print_results()

    def print_logged_commands(self):
        "Print usage of commands/scripts listed in self.cmd_logging"
        print
        for cmdid, log in self.cmd_logging.iteritems():
            if isinstance(cmdid, int):
                name = self.commandname(cmdid)
            print "--- All uses of %s (aside from standard scripts) ---" % name
            print log

    def print_source_stats(self):
        "Print table of stats about source code availability in scanned games"
        tally = np.zeros((len(self.rpgidx), 2, 2, 3), dtype = np.int)
        for i, r in enumerate(self.rpgidx):
            backup_type = 0
            if r.scripts_backup == 'source.txt':
                backup_type = 1
            elif r.scripts_backup == 'source.lumped':
                backup_type = 2
            tally[i, int(r.has_scripts), int(r.has_source), backup_type] = 1

        tally = tally.sum(axis=0)

        print
        print "%d/%d games had imported scripts:" % (tally[1].sum(), len(self.rpgidx))
        tally = tally[1]
        print "                no backup   |   source.txt   |   source.lumped"
        print "no src in zip      %3d             %3d                %3d" % tuple(tally[0])
        print "with src in zip    %3d             %3d                %3d" % tuple(tally[1])

        total = tally.sum()
        print
        print "in sum %d/%d games had script source available" % (total - tally[0][0].sum(), total)

    def print_command_usage(self):
        "Print table of the usage counts for all script commands and plotscr.hsd scripts"

        for scripts in self.scripthashes.itervalues():
            idx = self.standardindex.get(scripts[0].name)
            if idx:
                self.standardscrs['versions'][idx] += 1

        print
        cmdsums = self.cmdcounts.sum(axis=0)
        cmdgamecounts = (self.cmdcounts > 0).sum(axis=0)
        for i in xrange(len(cmdsums)):
            if cmdsums[i] or self.cmdcounts_in_plotscrhsd[i] or (i in self.commands_info) or i >= 2000:
                if i >= 2000:
                    header = self.standardscrs['names'][i - 2000] + " (%d versions)" % self.standardscrs['versions'][i - 2000]
                else:
                    header =  "%-3s%s" % (str(i) + ':', self.commandname(i))
                mark = ''
                header += ' ' * max(1, 41 - len(header) - len(str(cmdsums[i])))
                print "%s%d uses in %3d games + %2d in plotscr.hsd" % (header, cmdsums[i], cmdgamecounts[i], self.cmdcounts_in_plotscrhsd[i])

    def delete_script_refs(self):
        "Delete weakrefs from ascripts so that they are pickleable"
        for scripts in self.scripthashes.itervalues():
            for script in scripts:
                if hasattr(script, 'scriptset'):
                    if script.scriptset() != None:
                        print "WARNING: undeleted HSScripts object"
                    del script.scriptset
