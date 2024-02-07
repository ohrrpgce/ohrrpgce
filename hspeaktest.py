#!/usr/bin/env python3

"""
This is a tool for automated testing of HSpeak, specifically checking
that HSpeak throws errors or warnings only when expected.

Each line which is to be tested should have a comment starting with 'OK',
'WARN' or 'ERROR'. The rest of the comment is also searched for the following
keywords:
-'known':  a known failure; mark failing tests with this so that it's easy to
           spot newly introduced bugs. (There are a lot of known hspeak bugs!)
-'elsewhere':  (ERROR/WARN): this line causes an error/warning message for a
           line other than this one.
HSpeak is invoked repeatedly, with only one tagged line present at a time.
Lines beginning with ## are not passed to hspeak.

(All OK tests could actually be tested at once by checking for the line number
of any errors, and rerunning with erroring lines removed)
"""
from __future__ import print_function
import sys
import os
import re
import subprocess
import optparse

if hasattr(sys, 'intern'):
    # Python 3
    intern = sys.intern

def command_output_and_exitcode(cmd, args):
    """Runs a program and returns (stdout, exitcode) pair."""
    assert isinstance(args, (list, tuple))
    proc = subprocess.Popen([cmd] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    proc.wait()  # Needed to fetch the returncode
    outtext = proc.stdout.read().decode()
    errtext = proc.stderr.read().decode()
    if errtext:
        # HSpeak never writes anything to stderr
        print('%s\n%s\n%s\n%s' % ('-'*40, outtext, errtext, '-'*40))
        raise Exception("Didn't expect HSpeak to write to stderr. Did it crash?")
    return outtext.strip(), proc.returncode

def read_input_file(inputfile):
    """Read each line of a file and figure out how it is tagged.
    Returns a list of (tag, extra, line) tuples, where extra is any
    extra comment after the tag, eg. for
      noop(),                # ERROR  (known failure)
    tag == 'ERROR', extra == '  (known failure)'
    Lines starting with '##' get the tag COMMENT.
    """
    lines = []
    with open(inputfile, "r") as infile:
        for line in infile:
            # Ignore commented tests
            match = re.match("^[ \t]*#(#)?", line)
            extra = None
            if match:
                if match.group(1):
                    tag = 'COMMENT'
                else:
                    tag = None
            else:
                match = re.search("# *(ERROR|OK|WARN)(.*)$", line)
                if match:
                    tag = intern(match.group(1))
                    extra = match.group(2)
                else:
                    tag = None
            lines.append((tag, extra, line.rstrip()))
    return lines

def run_with_line(lines, keep_lineidx):
    """Recreate the input file with all tagged lines removed except 'keep_lineidx'
    (0-based index)."""
    selected_lines = []
    for idx,(tag,extra,line) in enumerate(lines):
        if tag != "COMMENT":
            if not tag or idx == keep_lineidx:
                selected_lines.append(line)
            if idx == keep_lineidx:
                expected_lineno = len(selected_lines)

    tempfile = os.path.join("build", "testhspeak.hss")
    with open(tempfile, "w") as outfile:
        outfile.write("\n".join(selected_lines))

    if options.eui:
        ret = command_output_and_exitcode('eui', ['hspeak.exw', '-ybk', tempfile])
    else:
        ret = command_output_and_exitcode('./hspeak', ['-ybk', tempfile])
    #os.unlink(tempfile)
    return ret, expected_lineno

def test_line(lines, lineidx):
    """Run a single test. Prints the result. Returns True if passed."""
    lineno = lineidx + 1   # 1-based index
    (stdout, exitcode), expected_lineno = run_with_line(lines, lineidx)
    tag, extra, line = lines[lineidx]
    expected = {None: 0, 'OK': 0, 'ERROR': 1, 'WARN': 2}[tag]
    def describe_code(exitcode):
        return {0: 'OK', 1: 'ERROR', 2: 'WARN'}.get(int(exitcode), 'exitcode %s' % exitcode)

    def print_line():
        print("Testing line %d: %s" % (lineno, line))

    if not options.hide_passes:
        print_line()

    def print_stdout():
        if options.hide_passes:
            print_line()
        if options.show_failures or options.show_all:
            trim_to = 0
            if options.trim_output:
                # Trim everything before the warning/error message
                COLRED = '\x1b[22;31m'
                for errstart in (COLRED + 'ERROR', COLRED + 'WARNING', 'ERROR', 'WARNING'):
                    if errstart in stdout:
                        trim_to = stdout.find(errstart)
                        break
                else:
                    # No message at all; drop everything
                    return
            print('%s\n%s\n%s' % ('-'*40, stdout[trim_to:], '-'*40))

    if expected != exitcode:
        print_stdout()
        print("Test on line %d FAILED: expected %s got %s" %
              (lineno, describe_code(expected), describe_code(exitcode)))
        return False
    elif expected > 0 and 'elsewhere' not in extra and ('n line %d ' % (expected_lineno)) not in stdout:
        # Some errors say 'on line X', others say 'in line X'
        print_stdout()
        print("Test on line %d FAILED: didn't print an error/warning for line %d" % (lineno, expected_lineno))
        return False
    if options.show_all:
        print_stdout()
    return True

def run_all_tests(inputfile):
    """Read a file an test each line which is tagged. Returns true if all passed."""
    successes, ran, known_failures = 0, 0, 0
    lines = read_input_file(inputfile)
    for lineidx, (tag, extra, line) in enumerate(lines):
        #print tag, line
        if tag and tag != 'COMMENT':
            if options.testline == None or int(options.testline) == lineidx + 1:
                ran += 1
                success = test_line(lines, lineidx)
                successes += success
                if extra and 'known' in extra.lower():
                    if success:
                        print("NOTE: line %d marked known-failure, but passed:\n%s" % (lineidx + 1, line))
                    else:
                        known_failures += 1
    print("Ran %d tests, %d failed (%d are known failures)" % (ran, ran - successes, known_failures))
    return ran == successes

if __name__ == '__main__':
    parser = optparse.OptionParser(usage="%prog [options] testfile.hss",
                                   description="Runs HSpeak on a file of testcases, checking whether each line "
                                   "produces an error, warning, or compiles, according to its annotation.")
    parser.add_option("--eui",
                      action="store_true", dest="eui", default=False,
                      help="Run 'eui hspeak.exw' instead of './hspeak' (slower than compiling hspeak!)")
    # This is probably too many options...
    parser.add_option("-a", "--all",
                      action="store_false", dest="hide_passes", default=True,
                      help="List all tests run, including those that passed")
    parser.add_option("-s", "--show",
                      action="store_true", dest="show_failures", default=False,
                      help="Show any warning/error messages for failed tests")
    parser.add_option("-f", "--full",
                      action="store_false", dest="trim_output", default=True,
                      help="Show full output from hspeak for failed tests, not just warning/error messages. Implies --show")
    parser.add_option("-S", "--showall",
                      action="store_true", dest="show_all", default=False,
                      help="Show hspeak's full stdout for all tests (implies --all, --show and --full)")
    parser.add_option("-t", "--testline",
                      dest="testline", default=None,
                      help="Test only a specific testcase line")

    (options, _args) = parser.parse_args()
    if len(_args) != 1:
        parser.print_help()
        sys.exit(1)
    inputfile = _args[0]
    if options.trim_output == False:
        options.show_failures = True
    if options.show_all:
        options.trim_output = False

    if run_all_tests(inputfile):
        sys.exit(0)
    else:
        sys.exit(1)
