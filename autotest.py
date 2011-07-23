#!/usr/bin/env python

import optparse
import subprocess
import re
import os
import sys
import glob
import shutil
import textwrap

########################################################################

def get_run_command(cmd, exitcode = None):
    """
    Returns stdout as a string
    """
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    com = proc.communicate()
    result = com[0].split("\n")
    if len(com[1]) > 0:
        raise ExecError(exitcode, "subprocess.Popen().communicate() returned stderr:\n%s" % (com[1]))
    return result

def run_command_exitcode(cmd):
    """
    Returns the exitcode of a command. All errors ignored.
    """
    proc = subprocess.Popen(cmd, shell=True)
    return proc.wait()

def run_command(cmd, exitcode = None):
    proc = subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE)
    com = proc.communicate()
    if len(com[1]) > 0:
        raise ExecError(exitcode, "subprocess.Popen().communicate() returned stderr:\n%s" % (com[1]))

class ExceptWithExitCode(Exception):
    exitcode = 125

    def __init__(self, exitcode, *rest):
        if exitcode != None:
            ExceptWithExitCode.exitcode = exitcode
        super(ExceptWithExitCode, self).__init__(*rest)

class ExecError(ExceptWithExitCode):
    pass

def startfile(filename):
    try:
        # this should work on Windows
        os.startfile(filename)
    except:
        # this should work on Linux
        subprocess.Popen(['xdg-open', filename])

########################################################################

def delete_pattern(filename_pattern):
    for filename in glob.iglob(filename_pattern):
        os.unlink(filename)

def move_pattern(filename_pattern, dest_dir):
    moved = 0
    for filename in glob.iglob(filename_pattern):
        shutil.move(filename, dest_dir)
        moved += 1
    if moved == 0:
        print 'NOTICE: No files moved using move_pattern("%s", "%s")' % (filename_pattern, dest_dir)

########################################################################

class Options(object):
    
    def __init__(self):
        # Arggh, optparse by default throws away all newlines in the
        # help strings -- have to use our own formatter to override this
        class BetterHelpFormatter(optparse.IndentedHelpFormatter):
            def format_description(self, description):
                return "\n".join([textwrap.fill(line, self.width) for line in description.split("\n")])

        parser = optparse.OptionParser(formatter=BetterHelpFormatter(), usage="%prog [options] filename.rpg", description="""This tool runs OHRRPGCE rpg files in autotest mode. It requires either an svn or git working copy of the OHRRPGCE source. The rpg will be run twice, once for the current local working copy, and once for the revision you want to compare with. You will be alerted of any differences. The purpose of this tool is to detect regressions and unintended side-effects of bugfixes. it is not useful for validating new features or bugfixes that legitimately involve a visible change in behavior. It is only useful for rpg files that run deterministically with no user input.

This script can be used with 'git bisect run': it returns 0 on pass, 1 on fail or error while running Game, and 125 for other errors. For example:
  ./autotest.py testgame/autotest.rpg -r abc123
  #...failure
  git bisect start HEAD abc123
  git run autotest.py testgame/autotest.rpg -a
""")
        parser.add_option("-r", dest="rev",
                  help="Revision to test the current working copy against. If you leave this out, it assumes that you want to compare local changes against a clean copy of the currently checked out revision.", default=None)
        parser.add_option("-a", "--again",
                  action="store_true", dest="again", default=False,
                  help="Compare again against the last revision you compared against. This is much faster than testing against a specific revision. ")
        parser.add_option("-g", "--gif",
                  action="store_true", dest="anim", default=False,
                  help="Create an animating gif to display the difference between the old and new screenshots (requires ImageMagick's convert utility to be installed)")
        parser.add_option("-p", "--replay",
                  action="store_true", dest="replay", default=False,
                  help="If a .ohrkey recorded playthrough exists with the same name as the rpg file, it will be played back.")
        (options, args) = parser.parse_args()
        self.rev = options.rev
        self.again = options.again
        self.anim = options.anim
        self.replay = options.replay
        self.rpgs = args
        self.parser = parser

########################################################################

class Context(object):
    
    def __init__(self):
        self.remember_dir = os.getcwd()
        if os.path.isdir(".svn"):
            self.using_svn = True
            lines = get_run_command("svn info")
            for line in lines:
                match = re.match(r"^URL: (.*)$", line)
                if match:
                    self.url = match.group(1)
                match = re.match(r"^Revision: (.*)$", line)
                if match:
                    self.rev = match.group(1)
        elif os.path.isdir(".git"):
            self.using_svn = False
            self.absolute_rev = get_run_command("git rev-parse HEAD")[0]
            # Determine what branch we're on by partially rev-parsing 'HEAD'
            # (Will this work correctly? Maybe should just read .git/HEAD)
            self.rev = get_run_command("git rev-parse --symbolic-full-name HEAD")[0]
            if self.rev == "HEAD":
                # Not on a branch
                self.rev = self.absolute_rev
        else:
            self.quithelp("This is neither an svn nor a git (root) directory. This script should be run from an svn or git working copy of the OHRRPGCE source")

########################################################################

class Platform(object):
    
    def __init__(self):
        self.gamefiles = ["misc/gdbscripts", "misc/gdbcmds1.txt", "misc/gdbcmds2.txt"]
        if sys.platform == "win32":
            self.gamefiles += ["game.exe", "gdbgame.bat"]
            self.game = "./gdbgame.bat"
        else:
            self.gamefiles += ["ohrrpgce-game", "gdbgame.sh"]
            self.game = "./gdbgame.sh"

########################################################################

class AutoTest(object):
    
    def __init__(self):
        self.opt = Options()
        self.context = Context()
        self.plat = Platform()
        self.validate()

    def validate(self):
        if len(self.opt.rpgs) == 0:
            self.quithelp("No rpg files were specified on the command line.")
        if not os.path.isfile(self.plat.game):
            self.quithelp("%s was not found in the current directory. This script should be run from the OHRRPGCE source directory" % (self.plat.game))
        if self.opt.again and self.opt.rev:
            self.quithelp("Can't use -a and -r at the same time.")
        if self.opt.rev is None:
            self.opt.rev = self.context.rev
        # I think David had some snippet for converting from git to svn rev, could add git support later
        if self.context.using_svn and self.opt.rev < 4491:
            self.quithelp("autotesting was not available before revision 4491")

    def againfail(self, rpg):
        if self.opt.again:
            self.quithelp("Can't test game '%s' in -a mode because it has not been tested already" % (os.path.basename(rpg)))

    def quithelp(self, message):
        self.opt.parser.print_help()
        print "\nERROR:", message
        raise SystemExit(125)
    
    def run_tests(self):
        if not os.path.isdir("autotest"):
            os.mkdir("autotest")
            f = open(os.path.join("autotest", "README.txt"), "wt")
            f.write("This folder is used by the autotest.py script. it is safe to delete this folder. autotest.py can re-create the folder next time it is run.")
            f.close()
        for rpg in self.opt.rpgs:
            self.test_rpg(rpg)
    
    def test_rpg(self, rpg):
        rpg = os.path.abspath(rpg)
        (shortname, ext) = os.path.splitext(os.path.basename(rpg))
        workdir = os.path.abspath(os.path.join("autotest", shortname))
        against = os.path.join(workdir, "against")
        if not os.path.isdir(workdir):
            self.againfail(rpg)
            os.mkdir(workdir)
        olddir = os.path.join(workdir, "old")
        if not os.path.isdir(olddir):
            self.againfail(rpg)
            os.mkdir(olddir)
        newdir = os.path.join(workdir, "new")
        if not os.path.isdir(newdir):
            os.mkdir(newdir)
        self.prepare_rev(self.opt.rev, rpg, against)
        if not self.context.using_svn:
            # Copy rpg to workdir because otherwise if it's checked into git, it
            # could change during a bisect.
            if not self.opt.again:
                shutil.copy(rpg, workdir)
            rpg = os.path.join(workdir, os.path.split(rpg)[1])
            if not os.path.isfile(rpg):
                self.againfail(rpg)
        if not self.opt.again:
            os.chdir(against)
            self.run_rpg(rpg, olddir)
            os.chdir(self.context.remember_dir)
        self.prepare_current(newdir)
        self.run_rpg(rpg, newdir)
        self.compare_output(shortname, olddir, newdir)
        
    def prepare_rev(self, rev, rpg, d):
        if not os.path.isdir(d):
            self.againfail(rpg)
            os.mkdir(d)
        if self.context.using_svn:
            self.prepare_rev_svn(rev, rpg, d)
        else:
            self.prepare_rev_git(rev, rpg, d)

    def prepare_rev_svn(self, rev, rpg, d):
        os.chdir(d)
        if not os.path.isdir(".svn"):
            self.againfail(rpg)
            run_command("svn checkout -r %s '%s' ." % (rev, self.context.url))
        else:
            if not self.opt.again:
                run_command("svn update -r %s" % (rev))
        if not self.opt.again:
            run_command("scons game")
        os.chdir(self.context.remember_dir)
    
    def prepare_rev_git(self, rev, rpg, d):
        if not self.opt.again:
            absolute_rev = get_run_command("git rev-parse %s" % rev)[0]
            revfile = open(os.path.join(d, "rev.txt"), "w+")  # Note: may not exist
            oldrev = revfile.read()
            if oldrev != absolute_rev:
                wc_dirty = run_command_exitcode("git diff --quiet") or run_command_exitcode("git diff --cached --quiet")
                if wc_dirty:
                    print "Your working copy or index is dirty; stashing your changes..."
                    run_command("git stash save -q 'Changes to %s saved by autotest.py'" % self.context.rev)
                else:
                    if absolute_rev == self.context.absolute_rev:
                        self.quithelp("There are no local changes and you either didn't specify -r, or specified HEAD! Nothing to do.")
                try:
                    run_command("git checkout -q %s" % rev)
                    run_command("scons game")
                    for f in self.plat.gamefiles:
                        dest = os.path.join(d, os.path.dirname(f))
                        if not os.path.isdir(dest):
                            os.mkdir(dest)
                        shutil.copy(f, dest)
                    revfile.seek(0)
                    revfile.write("%s" % absolute_rev)
                finally:
                    run_command("git checkout -q " + self.context.rev)
                    if wc_dirty:
                        print "Popping stashed changes to the working copy..."
                        run_command("git stash pop --index -q")
            revfile.close()

    def prepare_current(self, d):
        run_command("scons game")
    
    def run_rpg(self, rpg, dump_dir):
        print "======"
        print "running %s in %s and putting checkpoints in %s" % (rpg, os.getcwd(), dump_dir)
        print "------"
        delete_pattern(os.path.join(dump_dir, "checkpoint*.bmp"))
        replay = ''
        if self.opt.replay:
            (prefix, ext) = os.path.splitext(rpg)
            ohrkey = prefix + ".ohrkey"
            if os.path.isfile(ohrkey):
                replay = "-autosnap 1 -replayinput '%s'" % (ohrkey)
        cmd = "%s -z 1 -autotest %s '%s'" % (self.plat.game, replay, rpg)
        run_command(cmd, 1)
        move_pattern("checkpoint*.bmp", dump_dir)
    
    def compare_output(self, rpg, olddir, newdir):
        checked = []
        print "verifying checkpoints in %s..." % (rpg)
        oldfiles = glob.glob(os.path.join(olddir, "checkpoint*.bmp"))
        oldfiles.sort()
        for oldfile in oldfiles:
            short = os.path.basename(oldfile)
            newfile = os.path.join(newdir, short)
            if not os.path.isfile(newfile):
                self.fail(rpg, "checkpoint missing in new file", [oldfile])
            old = open(oldfile, "rb").read()
            new = open(newfile, "rb").read()
            if old <> new:
                self.fail(rpg, "checkpoints are different!", [oldfile, newfile])
            checked.append(newfile)
        newfiles = glob.glob(os.path.join(newdir, "checkpoint*.bmp"))
        newfiles.sort()
        for newfile in newfiles:
            if newfile not in checked:
                self.fail(rpg, "checkpoint missing in old file??", [newfile])
        print "  all checkpoints passed in %s" % (rpg)

    def fail(self, rpg, message, screenshots=[]):
        ExceptWithExitCode.exitcode = 1  # in case some exception occurs
        print "*****************************************"
        print rpg, "-", message
        if len(screenshots) == 2 and self.opt.anim:
            self.show_animating_gif(rpg, screenshots)
        else:
            for ss in screenshots:
                print ss
                startfile(ss)
        raise SystemExit(1)

    def show_animating_gif(self, rpg, screenshots):
        animfile = os.path.join("autotest", rpg, "difference.gif")
        cmd = "convert -delay 20 -loop 0"
        for ss in screenshots:
            print ss
            cmd += ' "%s"' % (ss)
        cmd += ' "%s"' % (animfile)
        run_command(cmd)
        startfile(animfile)

########################################################################

if __name__ == "__main__":
    # This is to get the ability to throw an exception and get a traceback, and
    # to set the exit code as well
    def show_traceback_and_exit(*args):
        sys.__excepthook__(*args)
        sys.exit(ExceptWithExitCode.exitcode)
    sys.excepthook = show_traceback_and_exit

    tester = AutoTest()
    tester.run_tests()

