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

def copy_file_from_git(git_dir, gitrev, path, outpath):
    """Copy a file from a git repo at a specific revision, and write it to `outpath`"""
    print('Copying ' + path + ' from git...    ', file=sys.stderr, end='\r')
    with open(outpath, "wb") as fil:
        subprocess.check_call(['git', '-C', git_dir, 'show', gitrev + ':' + path], stdout=fil)

def produce_breakpad_symbols_windows(pdb, breakpad_cache_dir, from_git_rev = None, git_dir = None, verbose = False):
    """Produce a Breakpad .sym file from a .pdb or non-stripped .exe file, if it
    doesn't already exist, and store it in a "symbol store" hierarchy like
    $breakpad_cache_dir/custom.pdb/A3CE56E9574946E7889C371E95F63D331/custom.sym
    which is how minidump_stackwalk wants its .sym files (symbol stores/servers
    are used by Microsoft debug tools and numerous others including BreakPad).

    Indicator files:
    Also, a second .sym 'indicator' file is created to indicate that the .sym
    already exists: it's either a symlink to the real .sym file on Unix, or an
    text file on with the same on Windows (because symlinks are Vista+ NTFS
    only).  If `from_git_rev` is empty, this indicator file is created next to
    the .pdb/.exe file, otherwise it's created at the root of
    `breakpad_cache_dir` with `from_git_rev` added to its name.
    (Indicator files are an optimisation, ideally we should extract the GUID/age
    from the pdb and search for that in the cache, but it would require yet
    another wine invoke.)
    The indicator file isn't created if the pdb is already in the symbol store.

    The .pdb is also copied into the symbol store, so that it can be found
    by other tools like the Visual Studio debugger.

    Returns True if created a .sym file, False if didn't (probably already
    existed).

    pdb:     path to .pdb or non-stripped .exe file.
             Might be either inside the symbol store, or elsewhere.
    breakpad_cache_dir:
             root of "symbol store" tree of Breakpad .sym files
             (doesn't need to exist yet.)
    from_git_rev:
             use a pdb file checked into a git repo, at this git commit
             ('pdb' is a path inside a git working tree, but we pull the file
             from git instead of using the currently checked-out copy)
    git_dir: only used if from_git_rev given. pdb must be under this directory.

    Note (verbose=True only): you'll see a message "Couldn't locate EXE or DLL
    file." if the original exe/dll isn't next to the .pdb, but this doesn't
    matter, the only effect is that a line like "INFO CODE_ID 5B8FB86D53000
    gfx_directx.dll" will be missing from the .sym file, which is not used for
    anything.
    However, if we built 64-bit binaries then having the .exe/.dll next to would
    be mandatory, because stack-unwinding metadata isn't in the .pdb.
    (Alternatively, we could fork dump_syms and add an arg to add a call to
    PDBSourceLineWriter::SetCodeFile().)

    """

    os.makedirs(breakpad_cache_dir, exist_ok = True)

    pdb_dir, pdb_fullname = os.path.split(pdb)
    pdb_basename = os.path.splitext(pdb_fullname)[0]
    if from_git_rev:
        indicator_symfile = pathjoin(breakpad_cache_dir, pdb_basename + '-' + from_git_rev + '.sym')
    else:
        # Put the indicator file next to `pdb`
        indicator_symfile = os.path.splitext(pdb)[0] + '.sym'

    if os.path.exists(indicator_symfile):  # (false for broken symlinks)
        # Already done. If this local file exists, then the one in
        # breakpad_cache_dir should too.
        if verbose:
            print("Already generated", pdb_basename + '.sym', file=sys.stderr)
        return False
    if os.path.islink(indicator_symfile):
        # Broken symlink
        os.remove(indicator_symfile)

    if from_git_rev:
        # We can't use just `pdb`, which might be the wrong version, get it from git
        pdb_checkedout = pathjoin(breakpad_cache_dir, '_' + pdb_fullname)  # temp filename
        copy_file_from_git(git_dir, from_git_rev, os.path.relpath(pdb, git_dir), pdb_checkedout)
    else:
        pdb_checkedout = pdb

    # We don't know where to put the .sym file yet, because we need to produce
    # it and read it to get the id, so write it at $indicator_symfile for now

    print('Converting to .sym...         ', file=sys.stderr, end='\r')
    cmd = [SUPPORT_DIR + 'dump_syms.exe', pdb_checkedout]
    if not HOST_WIN32:
        cmd = ['wine'] + cmd
    try:
        with open(indicator_symfile, 'w') as fil:
            stderr = sys.stderr if verbose else subprocess.DEVNULL
            subprocess.check_call(cmd, stdout=fil, stderr=stderr)
    except:
        if os.path.isfile(indicator_symfile):
            os.unlink(indicator_symfile)
        raise
    # Wipe previous line
    #print(' ' * 79, end='\r')

    # Get pdb id (GUID+age) out of the .sym file
    with open(indicator_symfile, 'r') as fil:
        pdb_id = fil.readline().split(' ')[3]

    # Move the .pdb and .sym file into the symbol store unless they're already
    # there because `pdb` is there.
    symdir = pathjoin(breakpad_cache_dir, pdb_fullname, pdb_id)
    cache_symfile = pathjoin(symdir, pdb_basename + '.sym')
    #if os.path.realpath(indicator_symfile) != os.path.realpath(cache_symfile):
    if os.path.realpath(pdb_dir) != os.path.realpath(symdir):
        os.makedirs(symdir, exist_ok = True)

        # Move .sym
        os.rename(indicator_symfile, cache_symfile)

        # Copy/move/symlink .pdb
        symdir_pdb_path = pathjoin(symdir, pdb_fullname)
        if from_git_rev:
            os.rename(pdb_checkedout, symdir_pdb_path)
            if verbose:
                print("Moving", pdb_checkedout, "to", symdir_pdb_path, file=sys.stderr)
        else:
            if HOST_WIN32: # Avoid symlink
                shutil.copy2(pdb, symdir_pdb_path)
            else:
                os.symlink(os.path.relpath(pdb, symdir), symdir_pdb_path)

        # Add indicator file (elsewhere)
        if HOST_WIN32:
            # Avoid use of symlinks
            with open(indicator_symfile, 'w') as fil:
                fil.write(cache_symfile)
        else:
            os.symlink(os.path.relpath(cache_symfile, os.path.dirname(indicator_symfile)), indicator_symfile)

    if verbose:
        print("Generated", cache_symfile, file=sys.stderr)
    return True

@functools.lru_cache(maxsize = None)
def get_source_around_line(git_dir, gitrev, filename, lineno, context = 1):
    """Return a few lines of source code around the given line"""
    print('Querying git for source...    ', file=sys.stderr, end='\r')
    contents = subprocess.check_output(['git', '-C', git_dir, 'show', gitrev + ':' + filename]).decode('utf8')
    lines = contents.replace('\r', '').split('\n')
    ret = []
    # Add one extra line before, because lineno is typically the line after the actual one
    for lineidx in range(max(0, lineno - context - 1), min(len(lines), lineno + context + 1)):
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
