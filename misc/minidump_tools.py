#!/bin/env python3
"""
Routines, using Breakpad and other utilities to:
-generate a Breakpad .sym file from a .pdb file using dump_syms.exe
 (requires Windows or Wine)
-populate/generate/download a "symbol store" hierarchy of .pdb, executable (.exe,
 .dll, etc), and .sym files, which can be used by Breakpad or Microsoft debuggers
-generate a backtrace and other useful info from a minidump and a .sym file
 using minidump_stackwalk.

This module is not specific to the OHRRPGCE, except for the tool paths
"""

import os
from os.path import join as pathjoin
import ntpath
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

def download_windows_symbols(pdb_name, pdb_id, breakpad_cache_dir, verbose = False):
    """Download a .pdb file for a Microsoft module (.dll, .drv). Slow!
    Returns the path to the .pdb or None."""
    pdb_path = pathjoin(breakpad_cache_dir, pdb_name, pdb_id, pdb_name)
    if os.path.isfile(pdb_path) or os.path.islink(pdb_path):
        # (We might have created a symlink to the pdb in produce_breakpad_symbols_windows())
        if verbose:
            print("Already downloaded", pdb_id + '/' + pdb_name, file=sys.stderr)
        return pdb_path

    print('Downloading ' + pdb_name + ' (slow!)...       ', file=sys.stderr, end='\r')

    env = {'_NT_SYMBOL_PATH': 'srv*' + breakpad_cache_dir + '*http://msdl.microsoft.com/download/symbols'}
    # Split id into GUID and age
    cmd = [SUPPORT_DIR + 'RetrieveSymbols.exe', pdb_id[:32], pdb_id[32:], pdb_name]
    # Also possible to use MS's symchk instead of RetrieveSymbols.exe
    # cmd = [SUPPORT_DIR + 'symchk.exe', '/id', minidump,
    #        '/s', 'srv*' + breakpad_cache_dir + '*http://msdl.microsoft.com/download/symbols',
    #        '/od', '/ov', '/ob', '/v' if verbose else '/q', '/fm', pdb_name.replace('pdb', 'dll')]
    if not HOST_WIN32:
        env['WINEDLLOVERRIDES'] = 'dbghelp,dbgeng=n'  # Use our copy of dbghelp, not wine's stub one
        env['WINEDEBUG'] = '-all'
        cmd = ['wine'] + cmd
    stderr = sys.stderr if verbose else subprocess.DEVNULL
    try:
        subprocess.check_call(cmd, stdout=stderr, stderr=stderr, env=env)
    except subprocess.CalledProcessError:
        # Couldn't fetch the dll.
        return None
    return pdb_path

def copy_file_from_git(git_dir, gitrev, path, outpath):
    """Copy a file from a git repo at a specific revision, and write it to `outpath`"""
    print('Copying ' + path + ' from git...    ', file=sys.stderr, end='\r')
    with open(outpath, "wb") as fil:
        subprocess.check_call(['git', '-C', git_dir, 'show', gitrev + ':' + path], stdout=fil)

def produce_breakpad_symbols_windows(pdb, breakpad_cache_dir, exe = None, from_git_rev = None, git_dir = None, verbose = False, add_indicator = True):
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
    The indicator file isn't created if the pdb is already in the symbol store,
    or if add_indicator=False.

    The .pdb is also copied into the symbol store, so that it can be found
    by other tools like the Visual Studio debugger.

    Returns True if created a .sym file, False if didn't (probably already
    existed).

    pdb:     path to .pdb or non-stripped .exe file.
             Might be either inside the symbol store, or elsewhere.
    exe:     Matching exe/dll to the pdb file. Don't need to pass this if it's
             next to the .pdb. Will be installed into the symbol store.
             Optional, but good to install for other debuggers.
    breakpad_cache_dir:
             root of "symbol store" tree of Breakpad .sym files
             (doesn't need to exist yet.)
    from_git_rev:
             use a pdb file checked into a git repo, at this git commit
             ('pdb' is a path inside a git working tree, but we pull the file
             from git instead of using the currently checked-out copy)
    git_dir: only used if from_git_rev given. pdb must be under this directory.

    Note (verbose=True only): you'll see a message "Couldn't locate EXE or DLL
    file." if the original exe/dll isn't next to the .pdb and `exe` isn't
    provided, which is a problem for VC++'s debugger but not breakpad.
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
        tempdir = pathjoin(breakpad_cache_dir, 'temp')
        os.makedirs(tempdir, exist_ok = True)

        # We can't use just `pdb`, which might be the wrong version, get it from git
        pdb_checkedout = pathjoin(tempdir, pdb_fullname)  # temp filename
        copy_file_from_git(git_dir, from_git_rev, os.path.relpath(pdb, git_dir), pdb_checkedout)
        if exe:
            _, exe_fullname = os.path.split(exe)
            exe_checkedout = pathjoin(tempdir, exe_fullname)  # temp filename
            copy_file_from_git(git_dir, from_git_rev, os.path.relpath(exe, git_dir), exe_checkedout)
    else:
        pdb_checkedout = pdb
        exe_checkedout = exe

    # We don't know where to put the .sym file yet, because we need to produce
    # it and read it to get the id, so write it at $indicator_symfile for now

    print('Converting to .sym...                  ', file=sys.stderr, end='\r')
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

    # Get pdb id (GUID+age) and module id out of the .sym file
    exe_id = None
    with open(indicator_symfile, 'r') as fil:
        pdb_id = fil.readline().split(' ')[3]
        toks = fil.readline().split(' ')
        # The INFO line is missing if the module wasn't next to the .pdb
        # If it's here, I assume that means it IS next to the pdb
        if toks[:2] == ['INFO', 'CODE_ID']:
            exe_id = toks[2]
            exe_name = toks[3].strip()
            if not exe:
                # Explicit path not given, so pdb_checkedout is in pdb_dir
                exe_checkedout = pathjoin(pdb_dir, exe_name)


    def install_file_in_store(cache_dir, srcfile):
        "Copy/move/symlink srcfile into cache_dir, as appropriate"
        os.makedirs(cache_dir, exist_ok = True)
        _, filename = os.path.split(srcfile)
        cache_file = pathjoin(cache_dir, filename)
        if os.path.exists(cache_file):
            return

        if os.path.islink(cache_file):  #Stale symlink?
            os.remove(cache_file)

        if from_git_rev:
            os.rename(srcfile, cache_file)
            if verbose:
                print("Moving", srcfile, "to", cache_file, file=sys.stderr)
        elif not os.path.lexists(cache_file):
            if HOST_WIN32: # Avoid symlink
                shutil.copy2(srcfile, cache_file)
            else:
                os.symlink(os.path.relpath(srcfile, cache_dir), cache_file)

    # Move the .pdb and .sym file into the symbol store unless they're already
    # there because `pdb` is there.
    symdir = pathjoin(breakpad_cache_dir, pdb_fullname, pdb_id)
    cache_symfile = pathjoin(symdir, pdb_basename + '.sym')
    #if os.path.realpath(indicator_symfile) != os.path.realpath(cache_symfile):
    if os.path.realpath(pdb_dir) != os.path.realpath(symdir):
        # Copy/move/symlink .pdb
        install_file_in_store(symdir, pdb_checkedout)

        # Move .sym
        os.rename(indicator_symfile, cache_symfile)

        # Add indicator file (elsewhere)
        if add_indicator:
            if HOST_WIN32:
                # Avoid use of symlinks
                with open(indicator_symfile, 'w') as fil:
                    fil.write(cache_symfile)
            else:
                os.symlink(os.path.relpath(cache_symfile, os.path.dirname(indicator_symfile)), indicator_symfile)

    if exe_id:
        # Symlink/copy the module to the symbol store
        exedir = pathjoin(breakpad_cache_dir, exe_name, exe_id)
        # Copy/move/symlink exe
        install_file_in_store(exedir, exe_checkedout)

    if verbose:
        print("Generated", cache_symfile, file=sys.stderr)
    return True

@functools.lru_cache(maxsize = None)
def get_source_around_line(git_dir, gitrev, filename, lineno, context = 1):
    """Return a few lines of source code around the given line"""
    print('Querying git for source...    ', file=sys.stderr, end='\r')
    if '\\' in filename and filename == filename.upper():
        # Some source files have mangled filenames like C:\USERS\JAMES\SRC\OHR\REL\FUFLUNS\CUSTOM_UDTS.BI
        # Hard to tell how many directories to trim.
        filename = ntpath.basename(filename).lower()

    try:
        contents_raw = subprocess.check_output(['git', '-C', git_dir, 'show', gitrev + ':' + filename])
    except subprocess.CalledProcessError as ex:
        print(ex, file=sys.stderr)
        return []
    try:
        contents = contents_raw.decode('utf8')
    except UnicodeDecodeError:
        contents = contents_raw.decode('latin1')
    lines = contents.replace('\r', '').split('\n')

    ret = []
    # Add one extra line before, because lineno is typically the line after the actual one
    for lineidx in range(max(1, lineno - context - 1), min(len(lines), lineno + context + 1)):
        cursor = ' ->' if lineidx == lineno else '   '
        ret.append(cursor + '%4d ' % lineidx + lines[lineidx - 1])
    return ret

def analyse_minidump(minidump, breakpad_cache_dir, git_dir = None, gitrev = None, verbose = False, fetch = True, stack_detail = False, ignore_pdbs = []):
    """
    Analyse a minidump file using Breakpad.
    Can automatically download Microsoft .pdb files, but you will first want to
    call produce_breakpad_symbols_windows() with additional application-specific
    .pdb files, to add them to the cache/symbol store.
    Returns a triple (stacktrace, crash_summary, info) where:
    stacktrace:  a string containing a stacktrace of the signalling thread
    crash_summary:  a one-line string summary of the stacktrace
    info:        a list of (key,value) pairs with some additional interesting info

    minidump:           path to a minidump .dmp file
    breakpad_cache_dir: where Breakpad .sym files should be cached
    git_dir:            path to git repo containing the source code
    gitrev:             git hash of the commit
    fetch:              whether to download pdbs
    stack_detail:       if true return all the output from minidump_stackwalk
                        including stack contents, but no crash_summary or info.
    ignore_pdbs:        list of pdb filenames not to try to download, because
                        they aren't Microsoft modules (this simply saves time)
    """
    print('Reading minidump...          ', file=sys.stderr, end='\r')
    stackwalk_args = [STACKWALK, '-m', minidump, breakpad_cache_dir]
    # Capture stderr to find out which pdbs are missing
    proc = subprocess.run(stackwalk_args, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    sw_out = proc.stdout.decode('latin-1')
    sw_err = proc.stderr.decode('latin-1')

    stderr = subprocess.DEVNULL
    if verbose >= 2:
        print(sw_err, file=sys.stderr)
        stderr = sys.stderr

    info, bt, shortbt, missing_pdbs, wanted_modules = _parse_stackwalk_output(sw_out, sw_err, git_dir, gitrev)
    if verbose:
        print("missing_pdbs:", missing_pdbs)
        print("wanted_modules:", wanted_modules)
    missing_pdbs.difference_update(ignore_pdbs)

    # First, try to download any needed pdb files from the Windows symbols server.
    # Downloading is slow, so only download .pdbs for modules actually in the stack trace.
    newsyms = False
    for line in sw_out.split('\n'):
        if line.startswith('Module'):
            _, module, version, pdb_name, pdb_id, memstart, memend, ismain = line.split('|')
            # pdb_id is 33 chars long, the GUID with a one-char age suffix.
            if module in wanted_modules and pdb_name in missing_pdbs and pdb_id:
                if fetch:
                    pdb_path = download_windows_symbols(pdb_name, pdb_id, breakpad_cache_dir, verbose)
                    if pdb_path:
                        if produce_breakpad_symbols_windows(pdb_path, breakpad_cache_dir, verbose=verbose):
                            newsyms = True
                else:
                    bt += ['(Run with --fetch to download missing symbols)']
                    break

    if newsyms:
        # Should get better stacktraces this time
        sw_out = subprocess.check_output(stackwalk_args, stderr=stderr).decode('latin-1')
        info, bt, shortbt, _, _ = _parse_stackwalk_output(sw_out, '', git_dir, gitrev)

    if stack_detail:
        # Replace -m (machine-readable) with -s, for human-readable output with stack contents
        stackwalk_args[1] = '-s'
        sw_out = subprocess.check_output(stackwalk_args, stderr=stderr).decode('latin-1')
        bt = [sw_out]
    elif verbose:
        print(sw_out, file=sys.stderr)

    return bt, _crash_summary(shortbt), info

def _parse_stackwalk_output(sw_out, sw_err, git_dir, gitrev):
    """Parse the stdout and stderr of minidump_stackwalk"""
    info = []
    bt = []
    shortbt = []
    # .pdb files for which the .sym file wasn't loaded.
    missing_pdbs = set()
    # .exe/.dll/etc files which appear to be missing. Not a subset of missing_pdbs.
    wanted_modules = set()

    for line in sw_err.split('\n'):
        if "Couldn't load symbols for: " in line:
            # Printed to stderr. Each line ends in e.g.
            # "Couldn't load symbols for: dsound.pdb|F38F478065E247C68EDA699606F56EED2"
            pdb = line[line.rfind(':') + 2:].split('|')[0]
            pdb = ntpath.basename(pdb)
            if len(pdb):  # Don't know why that happens
                missing_pdbs.add(pdb)

    crash_thread_prefix = '%DUMMY'
    for line in sw_out.split('\n'):
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
            framenum = int(framenum)
            if not function and module and framenum <= 6:
                # The function name may be missing even if the .sym is present,
                # e.g. for a static function, but we will match against
                # missing_pdbs to filter out false-positives.  Only try to
                # download symbols for top frames, because it's horribly slow.
                wanted_modules.add(module)
            function = demangle_name(function)
            if framenum == 20:
                bt.append('[Truncated further frames]')
                break
            if linenum:
                bt.append('@ %-20s \t(%s:%s + %s)' % (function, filename, linenum, offset) )
                shortbt.append('%s(%s:%s)' % (function, filename, linenum) )
                if framenum < 10 and git_dir:
                    bt += get_source_around_line(git_dir, gitrev, filename, int(linenum), 1)
            elif function:
                if filename or module:
                    tmp = '(%s)' % (filename or module,)
                else:
                    tmp = ''
                bt.append('@ %s + %s %s' % (function, offset, tmp) )
                shortbt.append('%s%s' % (function, tmp) )
            elif filename:
                # Don't know whether this ever happens
                bt.append('@ [%s + %s (%s)]' % (module, offset, filename) )
                shortbt.append('[%s+%s(%s)]' % (module, offset, filename) )
            else:
                bt.append('@ [%s + %s]' % (module, offset) )
                shortbt.append('[%s+%s]' % (module, offset) )

    return info, bt, shortbt, missing_pdbs, wanted_modules

def _crash_summary(shortbt):
    "Form the stack summary: just the top few relevant stack frames"
    shortbt = shortbt.copy()
    deleted_frames = ''

    # Trim frames due to calling crashrpt_send_report() (OK, this bit is OHRRPGCE-specific)
    for idx, fr in reversed(list(enumerate(shortbt))):
        if 'crashrpt_send_report' in fr or 'showbug' in fr or 'fatalbug' in fr:
            del shortbt[:idx+1]
            deleted_frames = '...' + fr.split('(')[0]
            break

    if any(fr[0] != '[' for fr in shortbt):
    #if any(':' not in fr for fr in shortbt):
        # If we have some frames with function names but the top frames don't,
        # skip over them except the first one
        while shortbt[0][0] == '[':  # and shortbt[1][0] == '[':
            if not deleted_frames:
                deleted_frames = shortbt[0] + ' ...nosyms'
            del shortbt[0]

    # Remove filenames + line numbers or module names behind function names,
    # or addresses inside modules, for all but top frame to shorten the summary
    for idx, fr in enumerate(shortbt):
        if idx > 0:
            fr = fr.split('(')[0]
            if '+0x' in fr:  # [MODULE+0xADDRESS]
                fr = fr.split('+0x')[0] + ']'
            shortbt[idx] = fr

    # Join with ' <- ' until too long
    ret = deleted_frames
    for fr in shortbt[:7]:
        if ret:
            ret += ' <- '
        ret += fr
        if len(ret) > 100: break
    return ret
