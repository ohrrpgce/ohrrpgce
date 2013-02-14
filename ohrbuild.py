#!/usr/bin/env python
import os
import platform
import re
import fnmatch
import sys

def get_run_command(cmd):
    """Runs a shell commands and returns stdout as a string"""
    import subprocess
    proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    errtext = proc.stderr.read()
    if len(errtext) > 0:
        raise Exception("subprocess.Popen(%s) returned stderr:\n%s" % (cmd, errtext))
    return proc.stdout.read().strip()

include_re = re.compile(r'^\s*#include\s+"(\S+)"', re.M | re.I)

standard_bi = ['crt', 'SDL', 'libxml', 'fbgfx.bi', 'windows.bi', 'win/mmsystem.bi',
               'file.bi', 'allegro.bi', 'string.bi', 'curses.bi']

def scrub_includes(includes):
    return [v for v in includes if not any([v.startswith(inc) for inc in standard_bi])]

def basfile_scan(node, env, path):
    contents = node.get_text_contents()
    included = scrub_includes (include_re.findall (contents))
    #print str(node) + " includes", included
    return included

def verprint (used_gfx, used_music, svn, git, fbc):
    # generate ver.txt
    # generate iver.txt (Install-info)
    # generate distver.bat (?)
    def openw (filename):
        return open (filename, 'wb')
    import datetime
    results = []
    supported_gfx = []
    f = open ('codename.txt','rb')
    codename = f.readline().rstrip()
    f.close()
    # now automagically determine branch and svn
    def missing (name, message):
        tmp ="%r executable not found. It may not be in the PATH, or simply not installed." % name
        tmp += '\n' + message
        print tmp
    def query_svn (*command):
        from subprocess import Popen, PIPE
        import re
        date_rex = re.compile ('Last Changed Date: ([0-9]+)-([0-9]+)-([0-9]+)')
        rev_rex = re.compile ('Last Changed Rev: ([0-9]+)')
        date = datetime.date.today().strftime ('%Y%m%d')
        rev = 0
        output = None
        try:
            f = Popen (command, stdout = PIPE, stderr = PIPE)
            output = f.stdout.read()
        except WindowsError:
            missing (command[0], 'version output may be wrong as a result.')
            output = ''
        except OSError:
            missing (command[0], 'version output may be wrong as a result.')
            output = ''
        if date_rex.search (output):
            date = date_rex.search (output).expand ('\\1\\2\\3')
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
            missing (fbc,'FBC is necessary to compile. Halting compilation.')
            sys.exit (0)
        except OSError:
            missing (fbc,'FBC is necessary to compile. Halting compilation.')
            sys.exit (0)

        output = f.stdout.read()
        if rex.search (output):
            return rex.search (output).expand ('\\1')
        return '??.??.? (????-??-??)'
    name = 'OHRRPGCE'
    date, rev = query_svn (svn,'info')
    if rev == 0:
        date, rev = query_svn (git,'svn','info')
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
    results.append ("#define GFX_CHOICES_INIT  " +\
      " :  ".join (['redim gfx_choices(%d)' % (len(supported_gfx) - 1)] + tmp))

    gfx_code = 'gfx_' + "+".join (supported_gfx)
    music_code = 'music_' + "+".join (used_music)
    data = {'name' : name, 'codename': codename, 'date': date,
            'rev' : rev, 'fbver': fbver, 'music': music_code,
            'gfx' : gfx_code}

    results.extend ([
        'CONST version as string = "%(name)s %(codename)s %(date)s"' % data,
        'CONST version_code as string = "%(name)s Editor version %(codename)s"' % data,
        'CONST version_revision as integer = %(rev)d' % data,
        'CONST version_branch as string = "%(codename)s"' % data,
        'CONST version_build as string = "%(date)s %(gfx)s %(music)s"' % data,
        ('CONST long_version as string = "%(name)s '
        '%(codename)s %(date)s.%(rev)s %(gfx)s/%(music)s FreeBASIC %(fbver)s"') %  data])
    f = openw ('ver.txt')
    f.write ('\n'.join (results))
    f.write ('\n')
    f.close()
    tmpdate = '.'.join([data['date'][:4],data['date'][4:6],data['date'][6:8]])
    f = openw ('iver.txt')
    f.write ('AppVerName=%(name)s %(codename)s %(date)s\n' % data)
    f.write ('VersionInfoVersion=%s.%s\n' % (tmpdate, rev))
    f.close ()
    f = openw ('distver.bat')
    f.write('@ECHO OFF\n')
    f.write('SET OHRVERCODE=%s\nSET OHRVERDATE=%s' % (codename,
                                                      tmpdate.replace ('.','-')))
    f.close()
    # I am curious why there is not a distver.sh generated in the original
    # verprint. An oversight?

