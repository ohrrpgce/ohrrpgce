#!/usr/bin/env python

"""
This is a tool for automated testing of HSpeak, specifically checking
that HSpeak throws errors or warnings only when expected.

Each line which is to be tested should have a comment containing only 'OK',
'WARN' or 'ERROR'.
HSpeak is invoked repeatedly, with only one tagged line present at a time.
Lines beginning with ## are not passed to hspeak.

(All OK tests could actually be tested at once by checking for the line number
of any errors, and rerunning with erroring lines removed)
"""
import sys
import os
import re
import subprocess

quiet = True            # Don't print tests that pass
show_all = False        # Always print hspeak's stdout
show_failures = False   # Print stdout on failure

def command_output_and_exitcode(cmd, args):
    """Runsa  program and returns (stdout, exitcode) pair."""
    assert isinstance(args, (list, tuple))
    proc = subprocess.Popen([cmd] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    outtext = proc.stdout.read()
    errtext = proc.stderr.read()
    if errtext:
        # HSpeak never writes anything to stderr
        print('%s\n%s\n%s\n%s' % ('-'*40, outtext, errtext, '-'*40))
        raise Exception("Didn't expect HSpeak to write to stderr. This needs updating?")
    proc.wait()  # Needed to fetch the returncode
    return outtext.strip(), proc.returncode

def read_input_file(inputfile):
    """Read each line of a file and figure out how it is tagged.
    Returns a list of (lineno, tag, line) tuples, where lineno."""
    lines = []
    with open(inputfile, "r") as infile:
        for line in infile:
            # Ignore commented tests
            match = re.match("^[ \t]*#(#)?", line)
            if match:
                if match.group(1):
                    tag = 'COMMENT'
                else:
                    tag = None
            else:
                match = re.search("# *(ERROR|OK|WARN) *$", line)
                if match:
                    tag = intern(match.group(1))
                else:
                    tag = None
            lines.append((tag, line.rstrip()))
    return lines

def run_with_line(lines, lineno):
    """Recreate the input file with all tagged lines removed except 'lineno'.
    lineno counts from zero!"""
    selected_lines = []
    for idx,(tag,line) in enumerate(lines):
        if tag != "COMMENT":
            if not tag or idx == lineno:
                selected_lines.append(line)
            if idx == lineno:
                expected_lineno = len(selected_lines)

    tempfile = os.path.join("build", "testhspeak.hss")
    with open(tempfile, "w") as outfile:
        outfile.write("\n".join(selected_lines))

    ret = command_output_and_exitcode("./hspeak", ['-ybk', tempfile])
    #os.unlink(tempfile)
    return ret, expected_lineno

def test_line(lines, lineno):
    """Run a single test. Prints the result. Returns true if passed."""
    (stdout, exitcode), expected_lineno = run_with_line(lines, lineno)
    tag = lines[lineno][0]
    expected = {None: 0, 'OK': 0, 'ERROR': 1, 'WARN': 2}[tag]
    def describe_code(exitcode):
        return {0: 'OK', 1: 'ERROR', 2: 'WARN'}.get(int(exitcode), 'exitcode %s' % exitcode)

    def print_line():
        print("Testing line %d: %s" % (lineno, lines[lineno][1]))

    if not quiet:
        print_line()

    def print_stdout():
        if quiet:
            print_line()
        if show_failures or show_all:
            print('%s\n%s\n%s' % ('-'*40, stdout, '-'*40))

    if expected != exitcode:
        print_stdout()
        print("Test on line %d FAILED: expected %s got %s" %
              (lineno, describe_code(expected), describe_code(exitcode)))
        return False
    elif expected > 0 and ('n line %d ' % expected_lineno) not in stdout:
        # Some errors say 'on line X', others say 'in line X'
        print_stdout()
        print("Test on line %d FAILED: didn't print an error/warning for line %d" % (lineno, expected_lineno))
        return False
    if show_all:
        print_stdout()
    return True

def run_all_tests(inputfile):
    """Read a file an test each line which is tagged. Returns true if all passed."""
    successes, ran = 0, 0
    lines = read_input_file(inputfile)
    for lineno, (tag, line) in enumerate(lines):
        #print tag, line
        if tag and tag != 'COMMENT':
            ran += 1
            successes += test_line(lines, lineno)
    print
    print("Ran %d tests, %d failed" % (ran, ran - successes))
    return ran == successes

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: ./hspeaktest.py testfile.hss")
        sys.exit(1)
    inputfile = sys.argv[1]

    if run_all_tests(inputfile):
        sys.exit(0)
    else:
        sys.exit(1)
