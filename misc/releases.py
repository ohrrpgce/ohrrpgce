#!/usr/bin/env python2
"""
Extract all release dates from whatsnew.txt, and print number of days between releases.
"""
from dateutil import parser
releases = []
for line in open("whatsnew.txt"):
    if not line.startswith(' ') and 'WHAT IS NEW' not in line:
        name = ''
        # if '+' in line:
        #     continue   # skip bugfix releases
        if '[' in line:
            name = line[line.index('['):].strip()
            line = line[:line.index('[')]
        if len(line.strip()) and '?' not in line:
            releases.append((parser.parse(line.strip()), name[1:-1]))

for release, lastrelease in zip(releases[:-1], releases[1:]):
    release, name = release
    date = release.strftime("%Y-%m-%d")
    print date, name.rjust(14), str((release - lastrelease[0]).days).rjust(4), "days"

if False:
    # This generates some markup for the timeline on the wikipedia article,
    # but it needs hand-editing to fix overlapping labels.
    for release, name in releases:
        date = release.strftime("%d/%m/%Y")
        if '+' in name:
            text = "  mark:(line,yellow)  #" + name
        else:
            text = "fontsize:M text:" + name.lower()
        print("  at:%s shift:($dx,$dy) %s" % (date, text))
