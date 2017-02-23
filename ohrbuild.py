#!/usr/bin/env python
import os
import platform
import re
import fnmatch
import sys
import itertools

def get_command_output(cmd, args, shell = True):
    """Runs a shell command and returns stdout as a string"""
    import subprocess
    if shell:
        # Argument must be a single string (additional arguments get passed as extra /bin/sh args)
        if isinstance(args, (list, tuple)):
            args = ' '.join(args)
        cmdargs = '"' + cmd + '" ' + args
    else:
        assert isinstance(args, (list, tuple))
        cmdargs = [cmd] + args
    proc = subprocess.Popen(cmdargs, shell=shell, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    errtext = proc.stderr.read()
    outtext = proc.stdout.read()
    # Annoyingly fbc prints (at least some) error messages to stdout instead of stderr
    if len(errtext) > 0 or proc.returncode:
        raise Exception("subprocess.Popen(%s) failed;\n%s\n%s" % (cmdargs, outtext, errtext))
    return outtext.strip()

include_re = re.compile(r'^\s*#include\s+"(\S+)"', re.M | re.I)

# Add an include file to this list if it should be a dependency even if it doesn't exist in a clean build.
generated_includes = ['ver.txt']

def scrub_includes(includes):
    """Remove those include files from a list which scons should ignore
    because they're standard includes."""
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
    return included

def get_fb_info(env, fbc):
    """Find fbc and query its version and default target and arch."""
    fbc_binary = fbc
    if not os.path.isfile (fbc_binary):
        fbc_binary = env.WhereIs (fbc)
    if not fbc_binary:
        raise Exception("FreeBasic compiler is not installed!")
    # Newer versions of fbc (1.0+) print e.g. "FreeBASIC Compiler - Version $VER ($DATECODE), built for linux-x86 (32bit)"
    # older versions printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux"
    # older still printed "FreeBASIC Compiler - Version $VER ($DATECODE) for linux (target:linux)"
    fbcinfo = get_command_output(fbc_binary, ["-version"])
    fbcversion = re.findall("Version ([0-9.]*)", fbcinfo)[0]
    # Convert e.g. 1.04.1 into 1041
    fbcversion = (lambda x,y,z: int(x)*1000 + int(y)*10 + int(z))(*fbcversion.split('.'))

    fbtarget = re.findall("target:([a-z]*)", fbcinfo)  # Old versions of fbc.
    if len(fbtarget) == 0:
        # New versions of fbc. Format is os-cpufamily, and it is the
        # directory name where libraries are kept in non-standalone builds.
        fbtarget = re.findall(" built for ([a-z0-9-]+)", fbcinfo)
        if len(fbtarget) == 0:
            raise Exception("Couldn't determine fbc default target")
    fbtarget = fbtarget[0]
    if '-' in fbtarget:
        # New versions of fbc
        default_target, default_arch = fbtarget.split('-')
    else:
        # Old versions
        default_target, default_arch = fbtarget, 'x86'

    return fbc_binary, fbcversion, default_target, default_arch

def verprint (used_gfx, used_music, fbc, arch, asan, builddir, rootdir):
    """
    Generate ver.txt, iver.txt (Innosetup), distver.bat.

    rootdir:  the directory containing this script
    builddir: the directory where object files should be placed
    However, all files created here are currently placed in rootdir
    """
    def openw (whichdir, filename):
        if not os.path.isdir (whichdir):
            os.mkdir (whichdir)
        return open (os.path.join (whichdir, filename), 'wb')
    import datetime
    results = []
    supported_gfx = []
    f = open (os.path.join (rootdir, 'codename.txt'),'rb')
    lines = []
    for line in f:
        if not line.startswith ('#'):
            lines.append (line.rstrip())
    f.close()
    if len(lines) != 2:
        exit('Expected two noncommented lines in codename.txt')
    codename = lines[0]
    branch_rev = int(lines[1])
    # now automagically determine branch and svn
    def missing (name, message):
        tmp ="%r executable not found. It may not be in the PATH, or simply not installed." % name
        tmp += '\n' + message
        print tmp
    def query_svn (*command):
        # Note: this is reimplemented in linux/ohr_debian.py
        from subprocess import Popen, PIPE
        import re
        # Always use current date instead
        #date_rex = re.compile ('Last Changed Date: ([0-9]+)-([0-9]+)-([0-9]+)')
        rev_rex = re.compile ('Revision: ([0-9]+)')
        date = datetime.date.today().strftime ('%Y%m%d')
        rev = 0
        output = None
        try:
            f = Popen (command, stdout = PIPE, stderr = PIPE, cwd = rootdir)
            output = f.stdout.read()
        except WindowsError:
            missing (command[0], '')
            output = ''
        except OSError:
            missing (command[0], '')
            output = ''
        #if date_rex.search (output):
        #    date = date_rex.search (output).expand ('\\1\\2\\3')
        if rev_rex.search (output):
            rev = int (rev_rex.search (output).expand ('\\1'))
        return date, rev
    def query_fb ():
        from subprocess import Popen, PIPE
        import re
        rex = re.compile ('FreeBASIC Compiler - Version (([0-9a-f.]+) ([0-9()-]+))')
        try:
            f = Popen ([fbc,'-version'], stdout = PIPE)
        except WindowsError:
            missing (fbc,'FB is necessary to compile. Halting compilation.')
            sys.exit (0)
        except OSError:
            missing (fbc,'FB is necessary to compile. Halting compilation.')
            sys.exit (0)

        output = f.stdout.read()
        if rex.search (output):
            return rex.search (output).expand ('\\1')
        return '??.??.? (????-??-??)'
    name = 'OHRRPGCE'
    date, rev = query_svn ('svn','info')
    if rev == 0:
        # On Windows, "git svn info" seems to take longer than a human lifetime
        if platform.system () == 'Windows':
            print "Not attempting to get SVN revision from git; takes forever"
        else:
            # If git config settings for git-svn have been set up but git-svn hasn't been
            # told to initialise yet, this will take a long time before failing...
            # but there's no good reason that should occur
            date, rev = query_svn ('git','svn','info')
    if rev == 0:
        print "Falling back to reading svninfo.txt"
        date, rev = query_svn ('cat','svninfo.txt')
    if rev == 0:
        print
        print " WARNING!!"
        print "Could not determine SVN revision, which will result in RPG files without full version info and could lead to mistakes when upgrading .rpg files. A file called svninfo.txt should have been included with the source code if you downloaded a .zip instead of using svn or git."
        print
    if branch_rev <= 0:
        branch_rev = rev
    fbver = query_fb ()
    for g in used_gfx:
        if g in ('sdl','fb','alleg','directx','sdlpp','console'):
            results.append ('#DEFINE GFX_%s_BACKEND' % g.upper())
            supported_gfx.append (g)
        else:
            exit("Unrecognised gfx backend " + g)
    for m in used_music:
        if m in ('native','sdl','native2','allegro','silence'):
            results.append ('#DEFINE MUSIC_%s_BACKEND' % m.upper())
            results.append ('#DEFINE MUSIC_BACKEND "%s"' % m)
        else:
            exit("Unrecognised music backend " + m)
    results.append ('#DEFINE SUPPORTED_GFX "%s "' % ' '.join (supported_gfx))
    tmp = ['gfx_choices(%d) = @%s_stuff' % (i, v) for i, v in enumerate (supported_gfx)]
    results.append ("#DEFINE GFX_CHOICES_INIT  " +\
      " :  ".join (['redim gfx_choices(%d)' % (len(supported_gfx) - 1)] + tmp))

    gfx_code = 'gfx_' + "+".join (supported_gfx)
    music_code = 'music_' + "+".join (used_music)
    asan = 'AddrSan' if asan else ''
    data = {'name' : name, 'codename': codename, 'date': date, 'arch': arch, 'asan': asan,
            'rev' : rev, 'branch_rev' : branch_rev, 'fbver': fbver, 'music': music_code,
            'gfx' : gfx_code}

    results.extend ([
        'CONST version as string = "%(name)s %(codename)s %(date)s"' % data,
        'CONST version_code as string = "%(name)s Editor version %(codename)s"' % data,
        'CONST version_revision as integer = %(rev)d' % data,
        'CONST version_date as integer = %(date)s' % data,
        'CONST version_branch as string = "%(codename)s"' % data,
        'CONST version_branch_revision as integer = %(branch_rev)s' % data,
        'CONST version_build as string = "%(date)s %(gfx)s %(music)s"' % data,
        ('CONST long_version as string = "%(name)s '
        '%(codename)s %(date)s.%(rev)s %(gfx)s/%(music)s FreeBASIC %(fbver)s %(arch)s %(asan)s"') %  data])

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
    f = openw (rootdir, 'distver.bat')
    f.write('SET OHRVERCODE=%s\nSET OHRVERDATE=%s' % (codename,
                                                      tmpdate.replace ('.','-')))
    f.close()

def android_source_actions (sourcelist, rootdir, destdir):
    # Get a list of C and C++ files to use as sources
    source_files = []
    for node in sourcelist:
        assert len(node.sources) == 1
        # If it ends with .bas then we can't use the name of the source file,
        # since it doesn't have the game- or edit- prefix if any;
        # use the name of the resulting target instead, which is an .o
        if node.sources[0].name.endswith('.bas'):
            source_files.append (node.abspath[:-2] + '.c')
        else:
            # node.sources[0] itself is a path in build/ (to a nonexistent file)
            source_files.append (node.sources[0].srcnode().abspath)
    # hacky. Copy the right source files to a temp directory because the Android.mk used
    # by the SDL port selects too much.
    # The more correct way to do this would be to use VariantDir to get scons
    # to automatically copy all sources to destdir, but that requires teaching it
    # that -gen gcc generates .c files.
    # (This links lib/gif.cpp as gif.cpp, so copy lib/gif.h to gif.h)
    actions = [
        'rm -fr %s/*' % destdir,
        'mkdir -p %s/fb' % destdir,
        # This actually creates the symlinks before the C/C++ files are generated, but that's OK
        'ln -s ' + ' '.join(source_files) + ' ' + destdir,
        'cp %s/*.h %s/' % (rootdir, destdir),
        'cp %s/fb/*.h %s/fb/' % (rootdir, destdir),
        'cp %s/lib/*.h %s/' % (rootdir, destdir),
        'cp %s/android/sdlmain.c %s' % (rootdir, destdir),
        # Cause build.sh to re-generate Settings.mk, since extraconfig.cfg may have changed
        'touch %s/android/AndroidAppSettings.cfg' % (rootdir),
    ]
    return actions
    
def check_lib_requirements(binary):
    """Check and print which versions of glibc and gcc dependency libraries (including libstdc++.so)
    that an ELF binary requires"""

    current_lib = None
    req = {'CXXABI': (), 'GLIBC': (), 'GLIBCXX': (), 'GCC': ()}
    for line in get_command_output("objdump", ["-p", binary]).split('\n'):
        match = re.search("required from (.*):", line)
        if match:
            current_lib = match.group(1)
        match = re.search("(CXXABI|GCC|GLIBC|GLIBCXX)_([0-9.]*)", line)
        if match:
            symbol = match.group(1)
            version = tuple(map(int, match.group(2).split('.')))
            #print symbol, version
            req[symbol] = max(req[symbol], version)

    # Tables giving the required version of GCC
    GLIBCXX_to_gcc = {
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
    }

    CXXABI_to_gcc = {
        (1,3,2): (4,3,0),
        (1,3,3): (4,4,0),
        (1,3,3): (4,4,1),
        (1,3,3): (4,4,2),
        (1,3,4): (4,5,0),
        (1,3,5): (4,6,0),
        (1,3,5): (4,6,1),
        (1,3,6): (4,7,0),
        (1,3,7): (4,8,0),
        (1,3,7): (4,8,3),
        (1,3,8): (4,9,0),
        (1,3,9): (5,1,0),
        (1,3,10): (6,1,0),
    }

    gcc_release_dates = {
        (4,3,0): 'March 5, 2008',
        (4,4,0): 'April 21, 2009',
        (4,4,1): 'July 22, 2009',
        (4,4,2): 'October 15, 2009',
        (4,5,0): 'April 14, 2010',
        (4,6,0): 'March 25, 2011',
        (4,6,1): 'June 27, 2011',
        (4,7,0): 'March 22, 2012',
        (4,8,0): 'March 22, 2013',
        (4,8,3): 'May 22, 2014',
        (4,9,0): 'April 22, 2014',
        (5,1,0): 'April 22, 2015',
        (6,1,0): 'April 27, 2016',
    }

    glibc_release_dates = {
        (2,26): '2017-08-01',
        (2,25): '2017-02-01',
        (2,24): '2016-08-04',
        (2,23): '2016-02-19',
        (2,22): '2015-08-14',
        (2,21): '2015-02-06',
        (2,20): '2014-09-08',
        (2,19): '2014-02-07',
        (2,18): '2013-08-12',
        (2,17): '2012-12-25',
        (2,16): '2012-06-30',
        (2,15): '2012-03-21',
        (2,14,1): '2011-10-07',
        (2,14): '2011-06-01',
        (2,13): '2011-02-01',
        (2,12,2): '2010-12-13',
        (2,12,1): '2010-08-03',
        (2,12): '2010-05-03',
    }
    #print req

    def verstring(version_tuple):
        return '.'.join(map(str, version_tuple))

    if req['GLIBCXX'] > (3,4,22) or req['CXXABI'] > (1,3,10):
        gcc_req = '>6.1.0'
    else:
        max_versions = []
        if req['GCC']:
            max_versions.append(req['GCC'])
        # fixme: this isn't very good
        if req['CXXABI'] < (1,3,2):
            pass
        else: #if req['CXXABI'] in GLIBCXX_to_gcc:
            max_versions.append(CXXABI_to_gcc.get(req['CXXABI'], (9, 'unknown')))
        if req['GLIBCXX'] < (3,4,10):
            pass
        else: #if req['GLIBCXX'] in GLIBCXX_to_gcc:
            max_versions.append(GLIBCXX_to_gcc.get(req['GLIBCXX'], (9, 'unknown')))
        if len(max_versions) == 0:
            gcc_req = None
        else:
            max_version = max(max_versions)
            gcc_req = verstring(max_version) + ' (released %s)' % gcc_release_dates[max_version]
    if gcc_req:
        gcc_req = 'and libs for gcc ' + gcc_req

    if req['GLIBC'] < (2,11):
        glibc_release = '<2010'
    else:
        glibc_release = glibc_release_dates.get(req['GLIBC'], 'unknown')
    print ">>  %s requires glibc %s (released %s) %s" % (
        binary, verstring(req['GLIBC']), glibc_release, gcc_req)

#check_lib_requirements("ohrrpgce-game")
