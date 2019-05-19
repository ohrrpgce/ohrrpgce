#!/bin/env python3
"""
Routines, using Breakpad to:
-generate a Breakpad .sym file from a .pdb file using dump_syms.exe
 (requires Windows or Wine)
-generate a backtrace and other useful info from a minidump and a .sym file
 using minidump_stackwalk.

This module is not specific to the OHRRPGCE, except for the tool paths
"""

import os
from os.path import join as pathjoin
import sys
import platform
import subprocess
import functools

SUPPORT_DIR = pathjoin(os.path.dirname(__file__), '../support/')

HOST_WIN32 = platform.system() == 'Windows'
if platform.system() == 'Linux' and platform.machine() == 'x86_64':
    STACKWALK = SUPPORT_DIR + 'linux_x64/minidump_stackwalk'
else:
    STACKWALK = 'minidump_stackwalk'  # it'll need to be in your $PATH

cxxfilt_missing = False

def demangle_name(name):
    """Demangle a C++ function name using c++filt, also used for overloaded FB
    functions, or return unchanged if not mangled."""
    global cxxfilt_missing
    if cxxfilt_missing:
        return name

    # Somehow a leading underscore gets stripped from the name, which prevents
    # c++filt from recognising the mangling
    if not name.startswith('_'):
        cleaned = '_' + name
    else:
        cleaned = name

    try:
        cleaned = subprocess.check_output(['c++filt', cleaned]).strip().decode('utf8')
    except FileNotFoundError:  # c++filt not found
        cxxfilt_missing = True
        return name

    if not name.startswith('_') and cleaned.startswith('_'):
        cleaned = cleaned[1:]

    # The list of argument types can be really long, if so trim them off
    if '(' in cleaned:
        param_start = cleaned.index('(')
        params = cleaned[param_start+1 : -1]
        if len(params) > 8:
            cleaned = cleaned[:param_start]

    return cleaned

def produce_breakpad_symbols_windows(pdb, breakpad_cache_dir, tag = '', verbose = False):
    """Produce a Breakpad .sym file from a .pdb or non-stripped .exe file, if it
    doesn't already exist, and store it organised in a file hierarchy like
    $breakpad_cache_dir/custom.pdb/A3CE56E9574946E7889C371E95F63D331/custom.sym
    which is how minidump_stackwalk wants its .sym files.

    Also, a second .sym 'indicator' file is created to indicate that the .sym
    already exists: it's either a symlink to the real .sym file on Unix, or an
    text file on with the same on Windows.  If `tag` is empty, this second file
    is created next to the .pdb/.exe file, otherwise it's created in
    `breakpad_cache_dir` with `tag` added to its name.  `tag` is useful for
    producing .sym files for .pdb files checked into git: the tag can be the git
    commit hash of the particular version of the .pdb we're processing.

    pdb:    path to .pdb or non-stripped .exe file.
    breakpad_cache_dir:
            root of tree of Breakpad .sym files
    tag:    a version string, in case the path `pdb is not uniquely identifying

    Note (verbose=True only): you'll see a message "Couldn't locate EXE or DLL
    file." if the original exe/dll isn't next to the .pdb, but this doesn't
    matter, the only effect is that a line like "INFO CODE_ID 5B8FB86D53000
    gfx_directx.dll" will be missing from the .sym file, which is not used for
    anything.
    """

    pdb_dir, pdb_fullname = os.path.split(pdb)
    pdb_basename = os.path.splitext(pdb_fullname)[0]
    if tag:
        os.makedirs(breakpad_cache_dir, exist_ok = True)
        indicator_symfile = pathjoin(breakpad_cache_dir, pdb_basename + tag + '.sym')
    else:
        # Put the indicator file next to `pdb`
        indicator_symfile = os.path.splitext(pdb)[0] + '.sym'
    if os.path.isfile(indicator_symfile) or os.path.islink(indicator_symfile):
        # Already done. If this local file exists, then the one in
        # breakpad_cache_dir should too.
        if verbose:
            print("Already generated", cache_symfile, file=sys.stderr)
        return

    print('Converting to .sym...      ', file=sys.stderr, end='\r')
    cmd = [SUPPORT_DIR + 'dump_syms.exe', pdb]
    if not HOST_WIN32:
        cmd = ['wine'] + cmd
    with open(indicator_symfile, 'w') as fil:
        stderr = sys.stderr if verbose else subprocess.DEVNULL
        subprocess.check_call(cmd, stdout=fil, stderr=stderr)
    # Wipe previous line
    #print(' ' * 79, end='\r')

    # We have to get the pdb hash out of the .sym file
    with open(indicator_symfile, 'r') as fil:
        pdb_hash = fil.readline().split(' ')[3]

    # Move the file to the cache dir
    symdir = pathjoin(breakpad_cache_dir, pdb_fullname, pdb_hash)
    os.makedirs(symdir, exist_ok = True)
    cache_symfile = pathjoin(symdir, pdb_basename + '.sym')
    os.rename(indicator_symfile, cache_symfile)

    if HOST_WIN32:
        # Avoid use of symlinks
        with open(indicator_symfile, 'w') as fil:
            fil.write(cache_symfile)
    else:
        os.symlink(os.path.relpath(cache_symfile, os.path.dirname(indicator_symfile)), indicator_symfile)
    if verbose:
        print("Generated", cache_symfile, file=sys.stderr)

@functools.lru_cache(maxsize = None)
def get_source_around_line(git_dir, gitrev, filename, lineno, context = 1):
    """Return a few lines of source code around the given line"""
    print('Querying git for source...    ', file=sys.stderr, end='\r')
    contents = subprocess.check_output(['git', '-C', git_dir, 'show', gitrev + ':' + filename]).decode('utf8')
    lines = contents.replace('\r', '').split('\n')
    ret = []
    # Add one extra line before, because lineno is typically the line after the actual one
    for lineidx in range(max(0, lineno - context - 1), lineno + context + 1):
        cursor = ' ->' if lineidx == lineno else '   '
        ret.append(cursor + '%4d ' % lineidx + lines[lineidx])
    return ret

def analyse_minidump(minidump, pdb, breakpad_cache_dir, git_dir = None, gitrev = None, verbose = False, stack_detail = False):
    """
    Analyse a minidump file using Breakpad.
    Returns a triple (stacktrace, crash_summary, info) where:
    stacktrace:  a string containing a stacktrace of the signalling thread
    crash_summary:  a one-line string summary of the stacktrace
    info:        a list of (key,value) pairs with some additional interesting info

    minidump:           path to a minidump .dmp file
    pdb:                path to .pdb file
    breakpad_cache_dir: where Breakpad .sym files should be cached
    git_dir:            path to git repo containing the source code
    gitrev:             git hash of the commit
    stack_detail:       if true return all the output from minidump_stackwalk
                        including stack contents, but no crash_summary or info.
    """
    produce_breakpad_symbols_windows(pdb, breakpad_cache_dir)

    print('Reading minidump...          ', file=sys.stderr, end='\r')
    stackwalk_args = [STACKWALK, '-m', minidump, breakpad_cache_dir]
    if stack_detail:
        # Instead of -m for machine-readable, produce human-readable output including stack contents
        stackwalk_args[1] = '-s'
    stderr = sys.stderr if verbose else subprocess.DEVNULL
    output = subprocess.check_output(stackwalk_args, stderr=stderr).decode('latin-1')
    if stack_detail:
        return [output], '', ''  # Can't produce summary

    info = []
    bt = []
    shortbt = []
    crash_thread_prefix = '%DUMMY'

    if verbose:
        print(output, file=sys.stderr)

    for line in output.split('\n'):
        if line.startswith('CPU'):
            _, arch, description, cpus = line.split('|')
            info.append( ('CPU', '%s %s, %s cores' % (arch, description, cpus)) )
        elif line.startswith('Crash'):
            _, exception, addr, crash_thread = line.split('|')
            crash_thread_prefix = crash_thread + '|'
            # This is a string unlike the numeric exception code in crashrpt.xml, so return it
            info.append( ('Exception', exception) )
            # This usually doesn't agree with ExceptionAddress in crashrpt.xml... that is the instruction ptr?
            info.append( ('Minidump crash address', addr) )
            info.append( ('Crashed thread', crash_thread) )
        elif line.startswith(crash_thread_prefix):
            # One stack in the stacktrace for the signalling thread
            thread, framenum, module, function, filename, linenum, offset = line.split('|')
            function = demangle_name(function)
            if int(framenum) == 20:
                bt.append('[Truncated further frames]')
                break
            if linenum:
                bt.append('@ %-20s \t(%s:%s + %s)' % (function, filename, linenum, offset) )
                shortbt.append('%s(%s:%s)' % (function, filename, linenum) )
                if int(framenum) < 10 and git_dir:
                    bt += get_source_around_line(git_dir, gitrev, filename, int(linenum), 1)
            elif function:
                tmp = '(%s)' % (filename,) if filename else ''
                bt.append('@ %s + %s %s' % (function, offset, tmp) )
                shortbt.append('%s%s' % (function, tmp) )
            elif filename:
                # Don't know whether this ever happens
                bt.append('@ [%s + %s (%s)]' % (module, offset, filename) )
                shortbt.append('[%s+%s(%s)]' % (module, offset, filename) )
            else:
                bt.append('@ [%s + %s]' % (module, offset) )
                shortbt.append('[%s+%s]' % (module, offset) )

    # Form the summary: just the top three stack frames
    if any(fr[0] != '[' for fr in shortbt):
        # If we have some frames with proper line info but the top frames are
        # inside libraries with no info, skip over them except the first one called
        deleted_frames = False
        while shortbt[0][0] == '[' and shortbt[1][0] == '[':
            del shortbt[0]
            deleted_frames = True
        if deleted_frames:
            shortbt[0] = '... <- ' + shortbt[0]
    crash_summary = ' <- '.join(shortbt[:3])

    return bt, crash_summary, info
