#!/usr/bin/env python3
"""
Utility to check and print which versions of glibc, stdc++/supc++ and gcc dependency libraries (libgcc_s)
that an ELF binary requires, and when those versions were released.

Python 2.7 or 3.x

Placed in the public domain by Ralph Versteegen.
"""

from __future__ import print_function
import sys
import subprocess
import re


def check_deps(binary):
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
    for line in subprocess.check_output(["objdump", "-p", binary]).decode().split('\n'):
        match = re.search("required from (.*):", line)
        if match:
            current_lib = match.group(1)
            libraries.append(current_lib)
        match = re.search("(CXXABI|GCC|GLIBC|GLIBCXX)_([0-9.]*)", line)
        if match:
            symbol = match.group(1)
            version = tuple(map(int, match.group(2).split('.')))
            #print(symbol, version)
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
        (3,4,25): (8,1,0),
        (3,4,26): (9,1,0),
        (3,4,27): (9,2,0),
        (3,4,28): (9,3,0),
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
        (1,3,12): (9,1,0),
    }

    # From https://gcc.gnu.org/releases.html
    # This list is missing various stable branch releases
    gcc_release_dates = {
        (4,0,0): '2005-04-20',
        (4,1,0): '2006-02-28',
        (4,2,0): '2007-05-13',
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
        (8,3,0): '2019-02-22',
        (8,4,0): '2020-03-04',
        (9,1,0): '2019-05-03',
        (9,2,0): '2019-08-12',
        (9,3,0): '2020-03-12',
        (10,1,0): '2020-05-07',
        (10,2,0): '2020-07-23',
    }

    # From https://sourceware.org/glibc/wiki/Glibc%20Timeline
    glibc_release_dates = {
        (2,4):    '2006-03-06',
        (2,5):    '2006-09-29',
        (2,6):    '2007-05-17',
        (2,6,1):  '2007-07-31',
        (2,7):    '2007-10-19',
        (2,8):    '2008-04-12',
        (2,9):    '2008-11-13',
        (2,10):   '2009-05-09',
        (2,10,1): '2009-05-18',
        (2,10,2): '2009-11-16',
        (2,11):   '2009-11-03',
        (2,11,1): '2009-12-29',
        (2,11,2): '2010-05-19',
        (2,11,3): '2010-11-30',
        (2,12):   '2010-05-03',
        (2,12,1): '2010-08-03',
        (2,12,2): '2010-12-13',
        (2,13):   '2011-02-01',
        (2,14):   '2011-06-01',
        (2,14,1): '2011-10-07',
        (2,15):   '2012-03-21',
        (2,16):   '2012-06-30',
        (2,17):   '2012-12-25',
        (2,18):   '2013-08-12',
        (2,19):   '2014-02-07',
        (2,20):   '2014-09-08',
        (2,21):   '2015-02-06',
        (2,22):   '2015-08-14',
        (2,23):   '2016-02-19',
        (2,24):   '2016-08-04',
        (2,25):   '2017-02-01',
        (2,26):   '2017-08-02',
        (2,27):   '2018-02-01',
        (2,28):   '2018-08-01',
        (2,29):   '2019-02-01',
        (2,30):   '2019-08-01',
        (2,31):   '2020-02-01',
        (2,32):   '2020-08-05',
        (2,33):   '2021-02-01',  # Future
        (2,34):   '2021-08-01',  # Future
        (2,35):   '2022-02-01',  # Future
    }
    #print(req)

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


##############################################################################

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: %s <executable or .so>")
        exit(1)
    check_deps(sys.argv[1])
