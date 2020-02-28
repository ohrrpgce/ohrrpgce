#!/usr/bin/env python
from __future__ import print_function
from dateutil import parser
import sys

bugfix_releases = True
if len(sys.argv) > 1:
    if sys.argv[1] == '--skip-bugfix':
        bugfix_releases = False
    else:
        print("Extracts all release dates from whatsnew.txt, and print number of days between releases.")
        print("Usage: misc/releases.py [--skip-bugfix]")
        sys.exit()

last_release_line = -999
releases = []
for lineno, line in enumerate(open("whatsnew.txt")):
    if not line.startswith(' ') and 'WHAT IS NEW' not in line:
        name = ''
        if bugfix_releases == False and '+' in line:
            continue
        if 'release notes lost' in line:
            releases.append((0, 'lost'))
            continue
        if '[' in line:
            name = line[line.index('['):].strip()
            line = line[:line.index('[')]
        if len(line.strip()) and '?' not in line:
            if bugfix_releases == False and lineno <= last_release_line + 10:
                # The previous release was pretty minor, call it a bugfix release
                del releases[-1]
            last_release_line = lineno
            releases.append((parser.parse(line.strip()), name[1:-1]))

for release, lastrelease in zip(releases, releases[1:] + [None]):
    date, name = release
    if date:
        datestr = date.strftime("%Y-%m-%d")
        if lastrelease and lastrelease[0]:
            print(datestr, name.rjust(18), str((date - lastrelease[0]).days).rjust(4), "days")
        else:
            print(datestr, name)
    else:
        print('Lost...')

if False:
    # This generates some markup for the timeline on the wikipedia article,
    # but it needs hand-editing to fix overlapping labels.
    for release, name in releases:
        if not release: continue
        date = release.strftime("%d/%m/%Y")
        if '+' in name:
            text = "  mark:(line,yellow)  #" + name
        else:
            text = "fontsize:M text:" + name.lower()
        print("  at:%s shift:($dx,$dy) %s" % (date, text))
