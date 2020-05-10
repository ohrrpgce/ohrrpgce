#!/usr/bin/env python

"""
Various utility functions used by SConscript while building, but could also be
used by other tools.
"""

from __future__ import print_function
import os
import sys
from os.path import join as pathjoin
import subprocess
import platform
import re
import datetime
import fnmatch
import itertools
try:
    from SCons.Util import WhereIs
except ImportError:
    # If this script is imported from outside scons
    def WhereIs(exename):
        for p in os.environ["PATH"].split(os.pathsep):
            for ext in ("", ".exe", ".bat"):
                path = os.path.join(p, exename + ext)
                if os.path.exists(path):
                    return path
#from SCons.Tool import SourceFileScanner

host_win32 = platform.system() == 'Windows'

########################################################################
# Utilities

def get_command_outputs(cmd, args, shell = True, error_on_stderr = False):
    """Runs a shell command and returns stdout and stderr as strings"""
    if shell:
        # Argument must be a single string (additional arguments get passed as extra /bin/sh args)
        if isinstance(args, (list, tuple)):
            args = ' '.join(args)
        cmdargs = '"' + cmd + '" ' + args
    else:
        assert isinstance(args, (list, tuple))
        cmdargs = [cmd] + args
    proc = subprocess.Popen(cmdargs, shell=shell, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    outtext = proc.stdout.read().decode().strip()
    errtext = proc.stderr.read().decode().strip()
    proc.wait()  # To get returncode
    if proc.returncode or (error_on_stderr and errtext):
        exit("subprocess.Popen(%s) failed:\n%s\nstderr:%s" % (cmdargs, outtext, errtext))
    return outtext, errtext

def get_command_output(cmd, args, shell = True, ignore_stderr = False):
    """Runs a shell command and returns stdout as a string.
    Halts program on nonzero return or if ignore_stderr=False and anything printed to stderr."""
    # Annoyingly fbc prints (at least some) error messages to stdout instead of stderr
    return get_command_outputs(cmd, args, shell, not ignore_stderr)[0]

########################################################################
# Scanning for FB include files

include_re = re.compile(r'^\s*#include\s+"(\S+)"', re.M | re.I)

# Add an include file to this list if it should be a dependency even if it doesn't exist in a clean build.
generated_includes = ['ver.txt']

def scrub_includes(includes):
    """Remove those include files from a list which scons should ignore
    because they're standard FB/library includes."""
    ret = []
    for fname in includes:
        if fname in generated_includes or os.path.isfile(fname):
            # scons should expect include files in rootdir, where FB looks for them
            # (verprint() provides #/ver.txt, and without the #/ scons won't run it)
            ret.append ('#' + os.path.sep + fname)
    return ret

def basfile_scan(node, env, path):
    contents = node.get_text_contents()
    included = scrub_includes (include_re.findall (contents))
    #print str(node) + " includes", included
    return env.File(included)

########################################################################
# Scanning for HS include files

hss_include_re = re.compile(r'^\s*include\s*,\s*"?([^"\n]+)"?', re.M | re.I)

def hssfile_scan(node, env, path):
    """Find files included into a .hss."""
    contents = node.get_text_contents()
    included = []
    subdir = os.path.dirname(node.srcnode().path)
    for include in hss_include_re.findall (contents):
        include = include.strip()
        # Search for the included file in the same directory as 'node'
        check_for = os.path.join(subdir, include)
        if os.path.isfile(check_for):
            include = check_for
        included.append(include)
    #print str(node) + " includes", included
    # Turning into File nodes allows plotscr.hsd & scancode.hsi to be found in the root dir
    return env.File(included)

########################################################################
# Querying svn, git

def missing (name, message):
    print("%r executable not found. It may not be in the PATH, or simply not installed.\n%s" % (name, message))

def query_revision (rootdir, revision_regex, date_regex, ignore_error, *command):
    "Get the SVN revision and date (YYYYMMDD format) from the output of a command using regexps"
    # Note: this is reimplemented in linux/ohr_debian.py
    rev = 0
    date = ''
    output = None
    try:
        f = subprocess.Popen (command, stdout = subprocess.PIPE, stderr = subprocess.PIPE, cwd = rootdir)
        output = f.stdout.read().decode()
        errmsg = f.stderr.read().decode()
        if errmsg and not ignore_error:
            print(errmsg)
    except OSError:
        missing (command[0], '')
        output = ''
    date_match = re.search (date_regex, output)
    if date_match:
       date = date_match.expand ('\\1\\2\\3')
    rev_match = re.search (revision_regex, output)
    if rev_match:
        rev = int (rev_match.group(1))
    return date, rev

def query_svn (rootdir, command):
    """Call with either 'svn info' or 'git svn info'
    Returns a (rev,date) pair, or (0, '') if not an svn working copy"""
    return query_revision (rootdir, 'Revision: (\d+)', 'Last Changed Date: (\d+)-(\d+)-(\d+)', True, *command.split())

def query_git (rootdir):
    """Figure out last svn commit revision and date from a git repo
    which is a git-svn mirror of an svn repo.
    Returns a (rev,date) pair, or (0, '') if not a git repo"""
    if os.path.isdir (os.path.join (rootdir, '.git')):
        # git svn info is really slow on Windows
        if not host_win32 and os.path.isdir (os.path.join (rootdir, '.git', 'svn', 'refs', 'remotes')):
            # If git config settings for git-svn haven't been set up yet, or git-svn hasn't been
            # told to initialise yet, this will take a long time before failing
            date, rev = query_svn (rootdir, 'git svn info')
        else:
            # Try to determine SVN revision ourselves, otherwise doing
            # a plain git clone won't have the SVN revision info
            date, rev = query_revision (rootdir, 'git-svn-id.*@(\d+)', 'Date:\s*(\d+)-(\d+)-(\d+)', False,
                                        *'git log --grep git-svn-id --date short -n 1'.split())
    else:
        date, rev = '', 0
    return date, rev

def query_svn_rev_and_date(rootdir):
    """Determine svn revision and date, from svn, git, or svninfo.txt
    NOTE: Actually, we return current date instead of svn last-modified date,
    as the source might be locally modified"""
    date, rev = query_git (rootdir)
    if rev == 0:
        date, rev = query_svn (rootdir, 'svn info')
    if rev == 0:
        print("Falling back to reading svninfo.txt")
        date, rev = query_svn (rootdir, 'cat svninfo.txt')
    if rev == 0:
        print()
        print(""" WARNING!!
Could not determine SVN revision, which will result in RPG files without full
version info and could lead to mistakes when upgrading .rpg files. A file called
svninfo.txt should have been included with the source code if you downloaded a
.zip instead of using svn or git.""")
        print()

    # Discard git/svn date and use current date instead because it doesn't reflect when
    # the source was actually last modified.
    # Unless overridden: https://reproducible-builds.org/specs/source-date-epoch/
    if 'SOURCE_DATE_EPOCH' in os.environ:
        build_date = datetime.datetime.utcfromtimestamp(int(os.environ['SOURCE_DATE_EPOCH']))
    else:
        build_date = datetime.date.today()
    date = build_date.strftime ('%Y%m%d')

    return rev, date

########################################################################

def get_euphoria_version(EUC):
    """Returns an integer like 40103 meaning 4.1.3"""
    # euc does something really weird when you try to capture stderr. Seems to
    # duplicate stdout to stderr.
    # Using stderr=subprocess.STDOUT to merge stderr back into stdout works around it
    # but only on Linux/Mac
    # This works even if you are redirecting:
    #    scons hspeak 2>&1 | tee
    # Which is important because the nightly builds need to do that
    eucver = subprocess.check_output([EUC, "--version"], stderr=subprocess.STDOUT).decode()
    eucver = re.findall(" v([0-9.]+)", eucver)[0]
    print("Euphoria version", eucver)
    x,y,z = eucver.split('.')
    return int(x)*10000 + int(y)*100 + int(z)

########################################################################

class ToolInfo:
    "Info about a compiler, returned by get_cc_info()"
    def __str__(self):
        return self.path
    def describe(self):
        return self.path + " (" + self.fullversion + ")"


def findtool(mod, envvar, toolname, always_expand = False):
    """Look for a callable program.
    Returns None if it's not in PATH only if always_expand!"""
    if os.environ.get(envvar):
        ret = os.environ.get(envvar)
    elif WhereIs(mod['target_prefix'] + toolname):
        ret = mod['target_prefix'] + toolname
    else:
        ret = toolname
    # standalone builds of FB on Windows do not search $PATH for binaries,
    # so we have to do so for it!
    if mod['win32'] or always_expand:
        ret = WhereIs(ret)
    return ret

########################################################################

def get_cc_info(CC):
    "Process the output of gcc -v or clang -v for program name, version, and target. Returns a ToolInfo"
    # Used to call -dumpfullversion, -dumpversion, -dumpmachine instead
    ret = ToolInfo()
    stdout,stderr = get_command_outputs(CC, ["-v"])  # shell=True just to get "command not found" error
    match = re.search("(\S+) version ([0-9.]+)", stderr)
    match2 = re.search("Target: (\S+)", stderr)
    if not match or not match2:
        exit("Couldn't understand output of %s:\n%s\n%s\n" % (CC, stdout, stderr))
    ret.fullversion = match.group(1) + " " + match.group(2)
    ret.name = match.group(1)
    ret.version = int(match.group(2).replace('.', '')) # Convert e.g. 4.9.2 to 492
    ret.target = match2.group(1)
    ret.is_clang = ret.name == 'clang'
    ret.is_gcc = ret.name == 'gcc'
    ret.path = CC
    return ret

########################################################################
# Querying fbc

def get_fb_info(fbc):
    """Returns FBC, a ToolInfo for the FB compiler containing version and default target and arch info."""
    FBC = ToolInfo()
    fbc = os.path.expanduser(fbc)  # expand ~
    if not os.path.isfile (fbc):
        fbc = WhereIs (fbc)
        if not fbc:
            print("FreeBasic compiler is not installed! (Couldn't find fbc)")
            sys.exit(1)
    FBC.path = fbc
    FBC.name = os.path.basename(fbc)

    # Newer versions of fbc (1.0+) print e.g. "FreeBASIC Compiler - Version $VER ($DATECODE), built for linux-x86 (32bit)"
    # older versions printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux"
    # older still printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux (target:linux)"
    fbcinfo = get_command_output(fbc, ["-version"])
    version, date = re.findall("Version ([0-9.]+) ([0-9()-]+)", fbcinfo)[0]
    FBC.fullversion = version + ' ' + date
    # Convert e.g. 1.04.1 into 1041
    FBC.version = (lambda x,y,z: int(x)*1000 + int(y)*10 + int(z))(*version.split('.'))

    fbtarget = re.findall("target:([a-z]*)", fbcinfo)  # Old versions of fbc.
    if len(fbtarget) == 0:
        # New versions of fbc. Format is os-cpufamily, and it is the
        # directory name where libraries are kept in non-standalone builds.
        fbtarget = re.findall(" built for ([a-zA-Z0-9-_]+)", fbcinfo)
        if len(fbtarget) == 0:
            raise Exception("Couldn't determine fbc default target")
    fbtarget = fbtarget[0]
    if fbtarget == 'win64':
        # Special case (including new versions of fbc)
        FBC.default_target, FBC.default_arch = 'win32', 'x86_64'
    elif '-' in fbtarget:
        # New versions of fbc
        FBC.default_target, FBC.default_arch = fbtarget.split('-')
    else:
        # Old versions of fbc, and special case for dos, win32, xbox
        FBC.default_target, FBC.default_arch = fbtarget, 'x86'

    return FBC

########################################################################

def read_codename_and_branchrev(rootdir):
    """Retrieve branch name and svn revision.
    Note: if branch_rev is -1, the current svn revision should be used."""
    f = open(os.path.join(rootdir, 'codename.txt'), 'r')
    lines = []
    for line in f:
        if not line.startswith('#'):
            lines.append(line.rstrip())
    f.close()
    if len(lines) != 2:
        exit('Expected two noncommented lines in codename.txt')
    codename = lines[0]
    branch_rev = int(lines[1])
    return codename, branch_rev


def verprint(mod, builddir, rootdir):
    """
    Generate ver.txt, iver.txt (Innosetup), distver.bat.

    mod:      The SConscript module
    rootdir:  the directory containing this script
    builddir: the directory where object files should be placed
              However, all files created here are currently placed in rootdir
    """
    class AttributeDict:
        def __init__(self, d):
            self.__dict__ = d
    mod = AttributeDict(mod)   # Allow mod.member instead of mod['member']

    def openw (whichdir, filename):
        if not os.path.isdir (whichdir):
            os.mkdir (whichdir)
        return open (os.path.join (whichdir, filename), 'w')

    rev, date = query_svn_rev_and_date(rootdir)

    codename, branch_rev = read_codename_and_branchrev(rootdir)
    if branch_rev <= 0:
        branch_rev = rev

    results = []

    # Backends
    supported_gfx = []
    for gfx in mod.gfx:
        if gfx in mod.gfx_map.keys():
            results.append ('#DEFINE GFX_%s_BACKEND' % gfx.upper())
            supported_gfx.append (gfx)
        else:
            exit("Unrecognised gfx backend " + gfx)
    for m in mod.music:
        if m in mod.music_map.keys():
            results.append ('#DEFINE MUSIC_%s_BACKEND' % m.upper())
            results.append ('#DEFINE MUSIC_BACKEND "%s"' % m)
        else:
            exit("Unrecognised music backend " + m)
    results.append ('#DEFINE SUPPORTED_GFX "%s "' % ' '.join (supported_gfx))
    tmp = ['gfx_choices(%d) = @%s_stuff' % (i, v) for i, v in enumerate (supported_gfx)]
    results.append ("#DEFINE GFX_CHOICES_INIT  " +\
      " :  ".join (['redim gfx_choices(%d)' % (len(supported_gfx) - 1)] + tmp))

    if not mod.gengcc or mod.CC.fullversion == mod.FBCC.fullversion:
        ccversion = mod.CC.fullversion
    else:
        # Using two different C/C++ compilers!
        ccversion = mod.CC.fullversion + ' + ' + mod.FBCC.fullversion

    archinfo = mod.arch
    if mod.arch == '(see target)':
        archinfo = mod.target

    data = {
        'codename': codename, 'date': date, 'arch': archinfo,
        'rev': rev, 'branch_rev': branch_rev,
        'name':   'OHRRPGCE',
        'gfx':    'gfx_' + "+".join(supported_gfx),
        'music':  'music_' + "+".join(mod.music),
        'asan':   'AddrSan ' if mod.asan else '',
        'portable': 'portable ' if mod.portable else '',
        'pdb':    'pdb ' if mod.pdb else '',
        'ccver':  ccversion,
        'fbver':  mod.FBC.fullversion,
        'uname':  platform.uname()[1],
    }

    results.extend ([
        'CONST short_version as string = "%(name)s %(codename)s %(date)s"' % data,
        'CONST version_code as string = "%(name)s Editor version %(codename)s"' % data,
        'CONST version_build as string = "%(date)s.%(rev)s %(gfx)s %(music)s"' % data,
        'CONST version_revision as integer = %(rev)d' % data,
        'CONST version_date as integer = %(date)s' % data,
        'CONST version_branch as string = "%(codename)s"' % data,
        'CONST version_branch_revision as integer = %(branch_rev)s' % data,
        ('CONST long_version as string = "%(name)s %(codename)s %(date)s.%(rev)s %(gfx)s/%(music)s '
         'FreeBASIC %(fbver)s %(ccver)s %(arch)s %(asan)s%(portable)s%(pdb)s Built on %(uname)s"') % data])

    # If there is a build/ver.txt placed there by previous versions of this function
    # then it must be deleted because scons thinks that one is preferred
    # (ver.txt does not go in build/ because FB doesn't look there for includes)
    try:
        os.remove (builddir + 'ver.txt')
    except OSError: pass
    f = openw (rootdir, 'ver.txt')
    f.write ('\n'.join (results))
    f.write ('\n')
    f.close()
    tmpdate = '.'.join([data['date'][:4],data['date'][4:6],data['date'][6:8]])
    f = openw (rootdir, 'iver.txt')
    f.write ('AppVerName=%(name)s %(codename)s %(date)s\n' % data)
    f.write ('VersionInfoVersion=%s.%s\n' % (tmpdate, rev))
    f.close ()
    f = openw(rootdir, 'distver.bat')
    f.write('SET OHRVERCODE=%s\nSET OHRVERDATE=%s\nSET SVNREV=%s'
            % (codename, tmpdate.replace('.', '-'), rev))
    f.close()

########################################################################
# Android

def android_source_actions (env, sourcelist, rootdir, destdir):
    """Returns a pair (source_nodes, actions) for android-source=1 builds.
    The actions symlink & copy a set of C and C++ files to destdir (which is android/tmp/),
    including all C/C++ sources and C-translations of .bas files.
    """
    source_files = []
    source_nodes = []
    for node in sourcelist:
        assert len(node.sources) == 1
        # If it ends with .bas then we can't use the name of the source file,
        # since it doesn't have the game- or edit- prefix if any;
        # use the name of the resulting target instead, which is an .o
        if node.sources[0].name.endswith('.bas'):
            source_files.append (node.abspath[:-2] + '.c')
            # 'node' is for an .o file, but actually we pass -r to fbc, so it
            # produces a .c instead of an .o output. SCons doesn't care that no .o is generated.
            source_nodes += [node]
        else:
            # node.sources[0] itself is a path in build/ (to a nonexistent file)
            source_files.append (node.sources[0].srcnode().abspath)
            source_nodes += node.sources

    # hacky. Copy the right source files to a temp directory because the Android.mk used
    # by the SDL port selects too much.
    # The more correct way to do this would be to use VariantDir to get scons
    # to automatically copy all sources to destdir, but that requires teaching it
    # that -gen gcc generates .c files. (Actually, I think it knows that now)
    actions = ['rm -fr %s/*' % destdir]
    # This actually creates the symlinks before the C/C++ files are generated, but that's OK
    processed_dirs = set()
    for src in source_files:
        relsrc = src.replace(rootdir, '').replace('build' + os.path.sep, '')
        srcdir, _ = os.path.split(relsrc)
        newdir = os.path.join(destdir, srcdir)
        if srcdir not in processed_dirs:
            # Create directory and copy all headers in it
            processed_dirs.add(srcdir)
            actions += ['mkdir -p ' + newdir]
            # Glob doesn't support {,} syntax
            # I couldn't figure out how to use SourceFileScanner to find headers
            for header in env.Glob(pathjoin(srcdir, '*.h')) + env.Glob(pathjoin(srcdir + '*.hpp')):
                actions += ['ln -s %s %s/' % (header, newdir)]
        actions += ['ln -s %s %s' % (src, newdir)]
    # Cause build.sh to re-generate Settings.mk, since extraconfig.cfg may have changed
    actions += ['touch %s/android/AndroidAppSettings.cfg' % rootdir]
    return source_nodes, actions

########################################################################
# Manipulating binaries

# ___fb_ctx is decorated version on Windows
keep_symbols = ['__fb_ctx', '___fb_ctx']

def strip_nonfunction_symbols(binary, target_prefix, builddir, env):
    """Modifies a binary in-place, stripping symbols for global variables
    and undefined symbols (e.g. left behind by --gc-sections)"""
    nm = WhereIs(target_prefix + "nm")
    syms = get_command_output(nm, [binary], False)
    symfilename = os.path.relpath(builddir + binary + '.unwanted_symbols')
    with open(symfilename, 'w') as symfile:
        for line in syms.split('\n'):
            toks = line.strip().split(' ')
            if len(toks) == 3:
                address, symtype, symbol = toks
            else:
                symtype, symbol = toks
            assert len(symtype) == 1
            # Remove the following symbols:
            # U: undefined symbols
            # b/B, d/D, r/R: local/global variables (uninitialised, initalised, readonly)
            #    These are no use to the crash handler, only to gdb.
            # i: DLL junk (Windows only), not needed in a linked binary
            if symtype in 'UbBdDrRi':
                if symbol not in keep_symbols:
                    symfile.write(symbol + '\n')
    objcopy = WhereIs(target_prefix + "objcopy")
    env.Execute(objcopy + ' --strip-symbols ' + symfilename + ' ' + binary)

########################################################################
# Portability checks
    
def check_lib_requirements(binary):
    """Check and print which versions of glibc and gcc dependency libraries (including libstdc++.so)
    that an ELF binary requires.

    Note that libstdc++ version requirements are reported as GCC requirements,
    because each libstdc++ version is tied to a specific GCC version.
    Old versions before ~2010 are lumped together, and GCC versions newer than 6.1
    aren't supported yet.
    """

    libraries = []
    current_lib = None
    req = {'CXXABI': (), 'GLIBC': (), 'GLIBCXX': (), 'GCC': ()}
    for line in get_command_output("objdump", ["-p", binary]).split('\n'):
        match = re.search("required from (.*):", line)
        if match:
            current_lib = match.group(1)
            libraries.append(current_lib)
        match = re.search("(CXXABI|GCC|GLIBC|GLIBCXX)_([0-9.]*)", line)
        if match:
            symbol = match.group(1)
            version = tuple(map(int, match.group(2).split('.')))
            #print symbol, version
            req[symbol] = max(req[symbol], version)

    # Tables giving the required version of GCC corresponding to each GLIBCXX symbol versioning tag
    # From https://gcc.gnu.org/onlinedocs/libstdc++/manual/abi.html (Section 4)
    GLIBCXX_to_gcc = {
        (3,4):    (3,4,0),
        (3,4,1):  (3,4,1),
        (3,4,2):  (3,4,2),
        (3,4,3):  (3,4,3),
        (3,4,4):  (4,0,0),
        (3,4,5):  (4,0,1),
        (3,4,6):  (4,0,2),
        (3,4,7):  (4,0,3),
        (3,4,8):  (4,1,1),
        (3,4,9):  (4,2,0),
        (3,4,10): (4,3,0),
        (3,4,11): (4,4,0),
        (3,4,12): (4,4,1),
        (3,4,13): (4,4,2),
        (3,4,14): (4,5,0),
        (3,4,15): (4,6,0),
        (3,4,16): (4,6,1),
        (3,4,17): (4,7,0),
        (3,4,18): (4,8,0),
        (3,4,19): (4,8,3),
        (3,4,20): (4,9,0),
        (3,4,21): (5,1,0),
        (3,4,22): (6,1,0),
        (3,4,23): (7,1,0),
        (3,4,24): (7,2,0),
        (3,4,25): (8,0,0),
        (3,4,26): (9,0,0),
    }

    # Ditto for CXXABI
    CXXABI_to_gcc = {
        (1,3):   (3,4,0),
        (1,3,1): (4,0,0),
        (1,3,2): (4,3,0),
        (1,3,3): (4,4,0),
        #(1,3,3): (4,4,1),
        #(1,3,3): (4,4,2),
        (1,3,4): (4,5,0),
        (1,3,5): (4,6,0),
        #(1,3,5): (4,6,1),
        (1,3,6): (4,7,0),
        (1,3,7): (4,8,0),
        #(1,3,7): (4,8,3),
        (1,3,8): (4,9,0),
        (1,3,9): (5,1,0),
        (1,3,10): (6,1,0),
        (1,3,11): (7,1,0),
        #(1,3,11): (7,2,0),
        #(1,3,11): (8,0,0),
        #(1,3,11): (9,0,0),
    }

    # From https://gcc.gnu.org/releases.html
    gcc_release_dates = {
        (4,3,0): '2008-03-05',
        (4,4,0): '2009-04-21',
        (4,4,1): '2009-07-22',
        (4,4,2): '2009-10-15',
        (4,5,0): '2010-04-14',
        (4,6,0): '2011-03-25',
        (4,6,1): '2011-06-27',
        (4,7,0): '2012-03-22',
        (4,8,0): '2013-03-22',
        (4,8,3): '2014-05-22',
        (4,9,0): '2014-04-22',
        (5,1,0): '2015-04-22',
        (6,1,0): '2016-04-27',
        (7,1,0): '2017-05-02',
        (7,2,0): '2017-08-14',
        (8,1,0): '2018-05-02',
        (8,2,0): '2018-07-26',
    }

    # From https://sourceware.org/glibc/wiki/Glibc%20Timeline
    glibc_release_dates = {
        (2,12): '2010-05-03',
        (2,12,1): '2010-08-03',
        (2,12,2): '2010-12-13',
        (2,13): '2011-02-01',
        (2,14): '2011-06-01',
        (2,14,1): '2011-10-07',
        (2,15): '2012-03-21',
        (2,16): '2012-06-30',
        (2,17): '2012-12-25',
        (2,18): '2013-08-12',
        (2,19): '2014-02-07',
        (2,20): '2014-09-08',
        (2,21): '2015-02-06',
        (2,22): '2015-08-14',
        (2,23): '2016-02-19',
        (2,24): '2016-08-04',
        (2,25): '2017-02-01',
        (2,26): '2017-08-01',
        (2,27): '2018-02-01',
        (2,28): '2018-08-01',
        (2,29): '2019-01-31',
    }
    #print req

    def verstring(version_tuple):
        unknown = [a for a in version_tuple if 'unknown' in str(a)]
        if unknown: return unknown[0]
        return '.'.join(map(str, version_tuple))

    def lookup_version(version_tuple, table):
        if version_tuple < min(table):
            return "before " + table[min(table)]
        elif version_tuple > max(table):
            return "after " + table[max(table)]
        elif version_tuple in table:
            return table[version_tuple]
        return "unknown"

    gcc_ver_reqs = []
    gcc_req = ''


    if 'libstdc++.so.6' in libraries:
        gcc_ver_reqs.append((3,4,0))  # First version using .so version .6

    if req['GCC']:
        gcc_ver_reqs.append(req['GCC'])
    # fixme: this isn't very good
    if req['CXXABI'] < min(CXXABI_to_gcc.keys()):
        gcc_ver_reqs.append((0, 'unknown ancient version')) #pass
    else: #if req['CXXABI'] in GLIBCXX_to_gcc:
        gcc_ver_reqs.append(CXXABI_to_gcc.get(req['CXXABI'], (999, 'unknown future version')))
    if req['GLIBCXX'] < min(GLIBCXX_to_gcc.keys()):
        gcc_ver_reqs.append((0, 'unknown ancient version')) #pass
    else: #if req['GLIBCXX'] in GLIBCXX_to_gcc:
        gcc_ver_reqs.append(GLIBCXX_to_gcc.get(req['GLIBCXX'], (999, 'unknown future version')))
    if gcc_ver_reqs:
        max_version = max(gcc_ver_reqs)
        gcc_req = verstring(max_version) + ' (released %s)' % lookup_version(max_version, gcc_release_dates)
    if gcc_req:
        gcc_req = 'and libs for gcc ' + gcc_req

    glibc_release = lookup_version(req['GLIBC'], glibc_release_dates)
    print(">>  %s requires glibc %s (released %s) %s" % (
        binary, verstring(req['GLIBC']), glibc_release, gcc_req))

#check_lib_requirements("ohrrpgce-game")
