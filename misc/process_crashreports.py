#!/bin/env python3
"""
This is a utility for analyzing crashrpt crash/bug reports, which are .zip
files containing a Windows minidump, crashrpt xml file, {c,g}_debug.txt, and
possibly more.  The minidump is analyzed using Breakpad (see
minidump_stacktrace.py).

Requirements:
-wine
-minidump_stacktrace (a copy for linux x86_64 is provided)
-7za

Optional:
-c++filt
-git and a copy of the OHRRPGCE git repo (which this file should be part of)

This utility was written to run on Linux, but should run on Windows too with
some simple changes (untested).

Obtain the crash reports from hamsterrepublic.com if you have ssh access,
or just ask for them.

Run with --help for usage info.
"""

import sys
import os
from os.path import join as pathjoin
import ntpath
import platform
import subprocess
import re
import time
import urllib.request
import functools
import xml.etree.ElementTree as ET
import argparse

import minidump_tools


TAIL_LINES = 5   # Number of final lines of c/g_debug.txt to print
ERROR_LINES = 4  # Number of initial, and also number of final errors in c/g_debug.txt to print
                 # (e.g. 3 means print 6 errors in total), unless the error is included in the tail

# Where build symbols .7z files will be downloaded and extracted to, and .sym files are put
# (Set by commandline flag)
SYMS_CACHE_DIR = 'symbols_cache'

# Location of a git repo of the OHRRPGCE source code.
# Define this to print out the git hash for the svn commit that the executables in a report
# were built with, or leave blank if you don't care.
GIT_DIR = pathjoin(os.path.dirname(__file__), '..')

if not os.path.isdir(pathjoin(GIT_DIR, '.git')):
    print("NOTE: Running without a OHRRPGCE git repo, backtraces won't show source code or use gfx_directx.pdb, etc, files in win32/")
    GIT_DIR = ''

SUPPORT_DIR = pathjoin(os.path.dirname(__file__), '../support/')

SYMBOLS_ARCHIVE_URL = 'http://hamsterrepublic.com/ohrrpgce/symbols-archive/'

# Map from the backends to the name used to identify the file in SYMBOLS_ARCHIVE_URL.
# This name is defined in distrib-nightly-win.bat, as the first argument to
# distrib-nightly-win-packnupload.bat (or the wine versions thereof),
# and by "set BUILDNAME=..." in distrib-win.{bat,sh}
BACKENDS_SYMSNAME = {
    'gfx_directx+sdl+fb/music_sdl': 'music_sdl',   # And its variant, music_sdl-debug
    'gfx_directx+sdl+fb/music_native': 'music_native',
    'gfx_directx+sdl+fb/music_native2': 'music_native2',
    'gfx_directx+sdl+fb/music_silence': 'music_silence',
    'gfx_sdl2+directx+fb/music_sdl2': 'sdl2',
}

HOST_WIN32 = platform.system() == 'Windows'


@functools.lru_cache(maxsize = None)
def svn_to_git_rev(rev):
    print('Querying git for svn rev...  ', file=sys.stderr, end='\r')
    gitrev = subprocess.check_output(['git', '-C', GIT_DIR, 'svn', 'find-rev', 'r' + rev]).decode('utf8').strip()
    return gitrev

@functools.lru_cache(maxsize = None)
def file_lastchange_git_rev(git_dir, path, as_of_commit = ''):
    """Return the (abbreviated) hash of the last git commit to modify a file,
    either in HEAD or as of the given commit/branch."""
    print('Querying git log...        ', file=sys.stderr, end='\r')
    return subprocess.check_output(['git', '-C', git_dir, 'log', '--max-count=1', '--format=%h', as_of_commit, path]).decode('utf8').split()[0]


def print_attr(name, value):
    """Print an attribute of a report"""
    print("%22s %s" % (name, value))

def print_matching_line(line, key, regex = None, groupnum = 1):
    """If a line matches a regex, print match for group `groupnum`, or whatever
    follows if no groups, and also return it.

    `key`: name of the printed attribute
    """
    if not regex:
        regex = key
    match = re.search(regex, line, re.I)
    if match:
        if len(match.groups()):
            value = match.group(groupnum)
        else:
            value = line[match.end():].strip()
        print_attr(key, value)
        return value
    return ''

class NoSymbolsError(ValueError): pass

def symbols_filename_from_build(build):
    """Work out what the name of the symbols file (as uploaded to SYMBOLS_ARCHIVE_URL)
    for this build is, or raise NoSymbolsError if there is none.

    build is a string, long_version + build_info in the FB source.
    """
    if not build:  # Should always be present
        raise NoSymbolsError("build string missing from crashrpt.xml!")
        syms_fname = None

    ohrvercode = build.split(' ')[1]  # This is 'wip' for a nightly, or else the version
    backends = build.split(' ')[3]

    if 'vampirecell' not in build:
        raise NoSymbolsError("No symbols: not an official build!")
    elif 'pdb' not in build:
        raise NoSymbolsError("Not built with pdb symbols!")
    elif backends not in BACKENDS_SYMSNAME:
        raise NoSymbolsError("No symbols: unrecognised build backends")
    else:
        buildtag = BACKENDS_SYMSNAME[backends]
        if '-exx' in build:
            buildtag += '-debug'
        date, svnrev = build.split(' ')[2].split('.')
        split_date = '%s-%s-%s' % (date[:4], date[4:6], date[6:])  # e.g 2019-02-09
        return 'ohrrpgce-symbols-win-%s-r%s-%s-%s.7z' % (buildtag, svnrev, split_date, ohrvercode)


def download_and_extract_symbols(syms_fname, args):
    """Downloads a file from SYMBOLS_ARCHIVE_URL to SYMS_CACHE_DIR and extracts it,
    if it hasn't already been.
    Returns directory it was extracted to."""
    if not os.path.isdir(SYMS_CACHE_DIR):
        os.mkdir(SYMS_CACHE_DIR)

    syms_7z = pathjoin(SYMS_CACHE_DIR, syms_fname)
    cachedir = syms_7z.replace('.7z', '')
    print_attr('Symbols', syms_fname)
    if not os.path.isdir(cachedir):
        if not os.path.isfile(syms_7z):
            syms_url = SYMBOLS_ARCHIVE_URL + syms_fname
            print('Downloading...          ', file=sys.stderr, end='\r')
            urllib.request.urlretrieve(syms_url, syms_7z)
        os.mkdir(cachedir)
        exe = '7za'
        if HOST_WIN32:
            exe = pathjoin(SUPPORT_DIR, '7za.exe')
        stdout = sys.stderr if args.verbose else subprocess.DEVNULL
        print('Extracting...           ', file=sys.stderr, end='\r')
        subprocess.check_call([exe, 'x', '-o' + cachedir, syms_7z], stdout=stdout)
    return cachedir

def process_minidump(build, reportdir, is_custom, args):
    """Read a minidump file (producing .sym files as necessary), print info
    from it, and return a (stacktrace, crash_summary) pair."""
    # Try to determine the symbols .7z archive
    try:
        syms_fname = symbols_filename_from_build(build)
        error_note = ""
    except NoSymbolsError as err:
        syms_fname = None
        error_note = str(err)

    breakpad_root = pathjoin(SYMS_CACHE_DIR, 'breakpad')

    if GIT_DIR and build:
        date, svnrev = build.split(' ')[2].split('.')
        gitrev = svn_to_git_rev(svnrev)
        print_attr('Git commit', gitrev[:9])

        # Make sure we've produced the .sym files for any .pdb files checked into
        # git rather than included in the symbols .7z archive.
        # (SDL.pdb, SDL2.pdb are missing, not provided by SDL, would need to
        # rebuild them ourselves)
        for pdb in ('win32/gfx_directx.pdb', 'win32/SDL_mixer.pdb', 'win32/SDL2_mixer.pdb'):
            minidump_tools.produce_breakpad_symbols_windows(pathjoin(GIT_DIR, pdb), breakpad_root, file_lastchange_git_rev(GIT_DIR, pdb, gitrev), GIT_DIR, verbose=args.verbose)
    else:
        gitrev = None

    ignore_pdbs = ['CrashRpt1403.pdb', 'game.pdb', 'custom.pdb']  # SDL.dll doesn't even have a .pdb

    if syms_fname:
        pdb_dir = download_and_extract_symbols(syms_fname, args)

        pdbname = 'custom' if is_custom else 'game'
        pdb = pathjoin(pdb_dir, pdbname + '.pdb')
        minidump_tools.produce_breakpad_symbols_windows(pdb, breakpad_root, verbose=args.verbose)

    minidump = pathjoin(reportdir, 'crashdump.dmp')
    stacktrace, crash_summary, info = minidump_tools.analyse_minidump(minidump, breakpad_root, GIT_DIR, gitrev, args.verbose, args.fetch, args.stack_detail, ignore_pdbs)
    for name, value in info:
        print_attr(name, value)
    if error_note:
        stacktrace += [error_note]
        crash_summary += ' ' + error_note
    return stacktrace, crash_summary

class ReportSummary:
    def __getattr__(self, attr):
        return ''

def process_crashrpt_report(reportdir, uuid, upload_time, args):
    """Print info about an unzipped crashrpt report, and return a row for the
    summary table as a ReportSummary instance."""

    summary = ReportSummary()
    summary.uuid = uuid[:6]
    summary.upload_date = time.strftime('%Y%m%d', upload_time)

    if not os.path.isdir(reportdir):
        summary.crash_summary = 'Could not unzip'
        return summary

    # Print interesting info from the xml file
    xmlfile = pathjoin(reportdir, 'crashrpt.xml')
    try:
        tree = ET.parse(xmlfile)
    except FileNotFoundError:
        summary.crash_summary = 'crashrpt.xml missing'
        return summary
    root = tree.getroot()

    real_uuid = root.find('CrashGUID').text
    if real_uuid != uuid:
        if uuid != '???':
            print('Warning! UUID ' + real_uuid + ' found in report ' + reportdir)
        summary.uuid = real_uuid[:6]

    print('\n\n\n######### Report ' + real_uuid + ' #########')
    print_attr('Upload time', time.strftime('%Y-%m-%d %H:%M:%S UTC', upload_time))
    print_attr('Crash time', root.find('SystemTimeUTC').text.replace('T', ' ').replace('Z', ' UTC'))

    for tagname in ('AppName', 'ExceptionModule', 'ExceptionAddress', 'MemoryUsageKbytes',
                    'OperatingSystem', 'GeoLocation', 'UserEmail', 'ProblemDescription'):
        elmt = root.find(tagname)
        if elmt is not None and elmt.text:
            if tagname == 'UserEmail':
                # Censor emails
                if args.email == False and '@' in elmt.text:
                    user, domain = elmt.text.split('@')
                    elmt.text = user[:5] + '***@' + domain
                summary.email = elmt.text
            if tagname == 'ProblemDescription':
                elmt.text = elmt.text.replace('\n', ' ').replace('\r', '')
                summary.description = elmt.text
            print_attr(tagname, elmt.text)
    build = None
    for x in root.iter('Prop'):
        print_attr(x.attrib['name'].title(), x.attrib['value'])
        if x.attrib['name'] == 'build':
            build = x.attrib['value']

    is_custom = (root.find('AppName').text == 'OHRRPGCE-Custom')
    summary.program = 'Custom' if is_custom else 'Game'

    # List any extra files attached to the report. Log files can be deleted by users, or the can add
    # more, and c/g_debug_archive.txt might not exist
    files = []
    for item in root.find('FileList'):
        name = item.attrib['name']
        if name not in ('crashrpt.xml', 'crashdump.dmp'):
            files.append(name)
    print_attr('Extra files', ' '.join(files))

    error_regex = re.compile(' *[0-9]+\.[0-9]+ +!')

    # Read the debug log, pull out certain data like game name and the last lines
    log_prefix = 'c' if is_custom else 'g'
    logfname = pathjoin(reportdir, log_prefix + '_debug.txt')
    errors = []  # list of (lineidx, line) pairs
    debug_archive_errors = 0
    loglines = []
    try:
        with open(logfname, 'r', encoding='latin-1') as logfile:
            loglines = logfile.readlines()
            for lineidx, line in enumerate(loglines):
                # Custom: Editing game <.rpg> (<getdisplayname()>)
                game = print_matching_line(line, 'Editing game')
                if game:
                    summary.game = ntpath.basename(game)
                # Game: the .rpg file (but ignore '----Loading a game----' line printed by Custom)
                game = print_matching_line(line, 'Playing game', 'Loading (.*rpg.*)----')
                if game:
                    summary.game += ntpath.basename(game) + ' '
                # Game: getdisplayname()
                game = print_matching_line(line, 'Game name', '[ 0-9.:]+ Name: (.+)')
                if game:
                    summary.game += game
                # Game: run via Test Game, show the Custom version info message
                print_matching_line(line, 'Spawned from Custom', 'Received message from Custom: (V.*)')
                #print_matching_line(line, 'settings_dir', 'settings_dir: (.*)')
                # Most of the backends return an info string on initialising, printed in quotes
                print_matching_line(line, 'Backend init info', '(gfx_.*".*)')
                # gfx_directx doesn't, it prints directly to debug log, and there's no
                # one-line summary, so just show this
                print_matching_line(line, 'Backend init info', 'Initialising (gfx_directx)')

                if not summary.username:
                    user = print_matching_line(line, 'Username', r'settings_dir: .*(Documents and Settings|Users)\\([^\\]+)\\', 2)
                    if user:
                        summary.username = user
                    # Fallback determination of username, if the crash happens during Test Game startup
                    user = print_matching_line(line, 'Username', r'message from Custom: W .*(Documents and Settings|Users)\\([^\\]+)\\', 2)
                    if user:
                        summary.username = user

                err = re.match(error_regex, line)
                if err:
                    #errmsg = line[err.end():].strip()  # Strip timestamp
                    errmsg = line.strip()  # Better to include the timestamp
                    errors.append( (lineidx, errmsg) )
                    # Errors are printed later

        # Count errors in debug archive
        with open(pathjoin(reportdir, log_prefix + '_debug_archive.txt'), 'r', encoding='latin-1') as logfile:
            for line in logfile.readlines():
                if re.match(error_regex, line):
                    debug_archive_errors += 1
    except FileNotFoundError:
        pass

    #print(time.strptime(root.find('SystemTimeUTC').text, '%Y-%m-%dT%H:%M:%SZ'))

    # Get stacktrace and other info from the minidump
    if args.no_stacktrace:
        stacktrace, summary.crash_summary = None, 'N/A'
    else:
        stacktrace, summary.crash_summary = process_minidump(build, reportdir, is_custom, args)
    # Print the stacktrace later, at the end

    # Print errors
    if debug_archive_errors:
        print_attr(log_prefix + '_debug_archive errors', debug_archive_errors)
    if errors:
        print_attr(log_prefix + '_debug errors', len(errors))
        print()
        print('----- Errors in %s_debug.txt -----' % log_prefix)

        def print_err(errnum):
            # Skip any errors that will be printed in the tail
            lineidx, errmsg = errors[errnum]
            if lineidx < len(loglines) - TAIL_LINES:
                #print_attr('Err msg %d' % (errnum + 1), errmsg)
                print(errmsg)

        # Print the first ERROR_LINES errors
        for errnum in range(0, min(len(errors), ERROR_LINES)):
            print_err(errnum)
        if ERROR_LINES < len(errors) - ERROR_LINES:
            print('...skipping...')
        # Print the last ERROR_LINES errors (which aren't also one of the first)
        for errnum in range(max(ERROR_LINES, len(errors) - ERROR_LINES), len(errors)):
            print_err(errnum)
        if errors[-1][0] >= len(loglines) - TAIL_LINES:
            print('...more in tail')

    # Print tail of log file
    if loglines:
        print()
        print('----- Tail of %s_debug.txt -----' % log_prefix)
        for idx in range(max(0, len(loglines) - TAIL_LINES), len(loglines)):
            #print_attr('tail %s_debug.txt' % log_prefix, loglines[idx].strip())
            print(loglines[idx].strip())

    # Print stacktrace
    if stacktrace:
        print()
        print('----- Stacktrace -----')
        for line in stacktrace:
            print(line)

    # Return summary
    summary.version = ' '.join(build.split(' ')[1:3])  # ohrvercode, builddate.svnrev
    return summary


def process_crashrpt_reports_directory(reports_dir, args):
    """Process a directory containing crashrpt .zip reports,
    printing information about each one."""

    uuids = set()

    # First get list of report zips, and unzip them to an <uuid>.unzipped directory if not already
    for fname in os.listdir(reports_dir):
        uuid, ext = os.path.splitext(fname)
        if ext == '.zip':
            zipfile = pathjoin(reports_dir, fname)
            reportdir = pathjoin(reports_dir, uuid + '.unzipped')
            if not os.path.isdir(reportdir):
                os.mkdir(reportdir)
                print('Unzipping...          ', file=sys.stderr, end='\r')
                stdout = sys.stderr if args.verbose else subprocess.DEVNULL
                try:
                    subprocess.check_call(['unzip', '-d', reportdir, zipfile], stdout=stdout)
                except Exception as err:
                    # If unzipping failed, process_crashrpt_report will notice that the dir is missing
                    print(err)
            elif args.new:
                # Skip already unzipped reports
                continue
            upload_time = time.gmtime(os.stat(zipfile).st_mtime)
            uuids.add((upload_time, uuid))

    uuids = sorted(uuids)
    if args.last:
        uuids = uuids[-args.last:]

    # Then process each unzipped report directory, sorted by upload date
    report_summaries = []
    for upload_time, uuid in uuids:
        reportdir = pathjoin(reports_dir, uuid + '.unzipped')
        report_summaries.append(process_crashrpt_report(reportdir, uuid, upload_time, args))

    # Then print summaries
    print_summary_table(report_summaries)

def print_summary_table(report_summaries):
    print('\n\n\n######### Summary #########')
    print('%-6s  %-8s  %-6s  %-27s  %s' % ('UUID', 'Uploaded', 'Prog', 'Version', 'Top stack frames'))
    print('%-6s  %-8s  %-6s  %-27s  %s' % ('Who', '', 'User', 'Game', 'Description'))
    for rpt in report_summaries:
        print(f'{rpt.uuid:<6}  {rpt.upload_date:8}  {rpt.program:<6}  {rpt.version:<27.27}  {rpt.crash_summary}\n'
              f'{rpt.email:16.16}  {rpt.username:6.6}  {rpt.game:<27.27}  {rpt.description}\n')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Print summary of one or more CrashRpt reports. See comments at top of file.")
    parser.add_argument("report_dir", help="A directory containing either crashrpt .zip files, or a single unzipped report (must contain crashrpt.xml).")
    parser.add_argument("syms_cache_dir", help="Directory to which to download and"
                        " extract build symbols to. Will be created if it doesn't exist.")
    parser.add_argument("-f", "--fetch", help="Download Microsoft .pdbs (slow)", action="store_true")
    parser.add_argument("-n", "--new", help="Process new reports only (not yet unzipped)", action="store_true")
    parser.add_argument("-l", "--last", help="Process only at most the last N reports", type=int)
    parser.add_argument("-v", "--verbose", help="Verbose output: show stderr output of invoked programs", action="store_true")
    parser.add_argument("-e", "--email", help="Don't mask emails", action="store_true")
    parser.add_argument("--no-stacktrace", help="Don't produce stacktraces. No external tools needed.", action="store_true")
    parser.add_argument("-d", "--stack-detail", help="Show details of stack contents in stacktraces, including function pointers. "
                        "Might help to decipher corrupt stacks or when the crash is in a Windows system dll. "
                        "(Source code will not be intermingled and stacktrace summaries won't be produced.)", action="store_true")
    args = parser.parse_args()

    SYMS_CACHE_DIR = args.syms_cache_dir  # Global

    xmlfile = pathjoin(args.report_dir, 'crashrpt.xml')
    if os.path.isfile(xmlfile):
        upload_time = time.gmtime(os.stat(xmlfile).st_mtime)
        summary = process_crashrpt_report(args.report_dir, "???", upload_time, args)
        print_summary_table([summary])
    else:
        process_crashrpt_reports_directory(args.report_dir, args)
