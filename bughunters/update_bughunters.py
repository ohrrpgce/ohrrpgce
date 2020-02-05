#!/usr/bin/env python3

"""
This script updates the Bughunters League Table on the wiki
https://rpg.hamsterrepublic.com/ohrrpgce/index.php?title=Bughunters_League_Table

It reads a file called bughunters.txt which contains an emacs org-mode-formatted
table with a list of bugs, (and other junk which is ignored), converts the
bughunters table to mediawiki markup and generates the summary tables, then
fetches the existing Bughunters_League_Table page from the wiki and pastes in
the new tables, leaving the rest of the page alone.  The result is then written
to leaguetable_wiki.txt to be manually put on the wiki.

License: Released into the public domain.
"""

import io
from collections import Counter, defaultdict
from bs4 import BeautifulSoup, NavigableString
import bs4
import argparse
from urllib.request import urlopen, urlretrieve
#from urllib.error import HTTPError

parse_lib = "html.parser"


def auto_decode(data, default_encoding = 'utf-8'):
    try:
        data = data.decode(default_encoding)
    except UnicodeDecodeError:
        data = data.decode('latin-1')
    return data

def get_url(url):
    print("Fetching", url)
    response = urlopen(url)
    return response.read()

def get_page(url, encoding = 'utf-8'):
    """Download a URL of an HTML page or fetch it from the cache, and return a BS object"""
    data = get_url(url)
    data = auto_decode(data, encoding)
    # Convert non-breaking spaces to spaces
    #data = data.replace(u'\xa0', ' ')
    data = data.replace("&#160;", ' ')
    return BeautifulSoup(data, parse_lib)

def get_old_page():
    """Get the (mediawiki markup) contents of Bughunters_League_Table article"""
    dom = get_page("https://rpg.hamsterrepublic.com/ohrrpgce/index.php?title=Bughunters_League_Table&action=edit")
    return dom.find(id="wpTextbox1").string

######################################################################

class LeagueTableUpdater:

    def __init__(self, since_rev = 0):
        self.counts = defaultdict(int)
        self.totalcounts = defaultdict(int)

        # Just for print_summary(), we can also count bugfixes since a certain revision
        # (I used this for stating summaries in my devlog)
        self.fixed_since_rev = since_rev
        self.fixed_since = defaultdict(int)

    def read_and_format_bugs_table(self, infile):
        """Scan infile for the org-mode table of bugs, process it (storing data in self),
        and return mediawiki-formatted table"""
        bugstbl = io.StringIO()

        self.points = Counter()
        self.bugs = Counter()
        intable = False
        bugstbl.write('{| class=wikitable\n')
        for line in infile.readlines():
            line = line.strip()
            if line == "START_TABLE":
                intable = True
            elif not line.startswith("| "):
                intable = False
            elif intable:
                # Example line:
                #| sotrain515  |   2 | E       | no          | #2036 Crash when switching collections in slice collection editor  |
                parts = [p.strip() for p in line.split('|')]
                reporters = parts[1]
                pts = parts[2]
                version = parts[3]
                fixed = parts[4]
                if reporters == 'Reporter':
                    # This is the header row of the table
                    bugstbl.write("! Reporter || Points || Version || Fixed? || Bug\n")
                else:
                    if 'part' in fixed:
                        fixed = 'part'
                    # Change "yes (git)" to "yes"
                    if fixed.startswith('yes'):
                        fixed = 'yes'
                    reported_version = None  # Version it was reported in
                    fixed_version = None  # Version it was fixed in (note this doesn't end up in any summary table)
                    # If fixed is prefixed with the version it was fixed in, like "F/", split that out.
                    if len(fixed) > 2 and fixed[1] == '/':
                        fixed_version = fixed[0]
                        fixed = fixed[2:]
                    # If fixed is a list of one or more svn revisions, change to 'yes'
                    try:
                        fixed_revs = [int(x) for x in fixed.split('/')]
                        fixed = 'yes'
                    except:
                        fixed_revs = []
                        pass
                    if version != 'N/A' and fixed not in ('notabug', 'wontfix'):
                        # Tally bugs by version and status into 'self.counts' and 'self.totalcounts'
                        # Merge the following classes:
                        # X X! X? X-  -> X
                        # X+ X+! X+?   -> X+
                        reported_version = version.replace('?','').replace('!','').replace('-','')
                        self.counts[(reported_version,fixed)] += 1
                        self.totalcounts[reported_version] += 1
                        if len(fixed_revs) >= 1 and fixed_revs[0] >= self.fixed_since_rev:
                            self.fixed_since[reported_version] += 1
                    try:
                        for who in reporters.split('/'):
                            if who not in ('Anonymous', 'tmc', 'James'):  # Exclude certain users
                                self.points[who] += float(pts)
                                self.bugs[who] += 1
                    except ValueError:
                        pass
                    if parts[5].startswith('#'):
                        # Bug number
                        bugnum, rest = parts[5].split(' ', 1)
                        parts[5] = '{{bug|%s}} ' % bugnum[1:] + rest
                    elif parts[5].startswith('gh#'):
                        # GitHub Bug number
                        bugnum, rest = parts[5].split(' ', 1)
                        parts[5] = '{{ghbug|%s}} ' % bugnum[3:] + rest
                    del parts[0]
                    del parts[-1]
                    bugstbl.write("|-\n| " + " || ".join(parts) + "\n")

        bugstbl.write("|}")
        return bugstbl.getvalue()

    def print_summary(self):
        """Print out a summary (to stdout) of bugs and bugfixes."""
        print("Reported |                |      ")
        print("Version  |     Status     | Count")
        for (version, status), count in sorted(self.counts.items()):
            print("%8s | %14s | %s" % (version, status, count))

        if self.fixed_since_rev:
            print("Since r%d fixed bugs: %s" % (self.fixed_since_rev, list(self.fixed_since.items())))

    def leaderboard_table(self):
        """Return mediawiki table of people sorted by points"""
        def sortkey(who_points):
            who,pts = who_points
            return -pts, -self.bugs[who], who.lower()  # Lexicographic sort, by three keys
        point_pairs = sorted(self.points.items(), key=sortkey)

        leaguetbl = io.StringIO()
        leaguetbl.write("""{| class=wikitable
|-
! Place || Who || Points || Reported bugs
""")

        table = []
        mult = Counter()
        lastpair = None
        for i, (who, pts) in enumerate(point_pairs):
            pair = pts, self.bugs[who]
            if pair != lastpair:
                placing = i + 1
            mult[placing] += 1
            lastpair = pair
            table.append((placing, who, pts))

        for placing, who, pts in table:
            placestr = str(placing)
            bugcount = self.bugs[who]
            if mult[placing] > 1:
                placestr += "="
            if placing <= 3:
                placestr += " [[File:Golden star.png|25px]]" * (4 - placing)
            if placing <= 10:
                who = "'''%s'''" % who
            leaguetbl.write("|-\n")
            leaguetbl.write("| %s || %s || %g || %d\n" % (placestr, who, pts, bugcount))
        leaguetbl.write("|}")
        return leaguetbl.getvalue()

        #print("Points:", sorted(self.points.items(), key= lambda x: x[1], reverse = True))
        #print("Points:", self.points.most_common())

        #print("Bugs:", self.bugs.most_common())

    def summary_table(self):
        counts = self.counts
        totalcounts = self.totalcounts
        ret = """{| class=wikitable
|-
! Reported in !! Fixed !! Partially fixed !! Unmerged fix !! Not fixed !! Total reported"""

        for version in ("Etheldreme", "Fufluns"):
            code = version[0]

            def row(code):
                return "|                %2d    || %2d              || %2d           || %2d        || %2d" % (
                    counts[code,'yes'], counts[code,'part'], counts[code,'no (unmerged)'], counts[code,'no'], totalcounts[code]
                )

            ret += """
|-
! [[%s]]
%s
|-
! Post-[[%s]] [[nightly builds]]
%s""" % (version, row(code), version, row(code+'+'))
        ret += "\n|}"
        return ret

    def update_article(self):

        def skiptable(it):
            """Skip rest of an org-mode-formatted table"""
            while True:
                line = next(it)
                if line == "|}": break

        def copy_till_table(it, dest, marker):
            line = None
            while line != marker:
                line = next(it)
                dest.append(line)
            skiptable(it)

        with open("bughunters.txt", "r") as infile:
            bugstbl = self.read_and_format_bugs_table(infile)

        self.print_summary()

        oldpage = get_old_page()
        page = []
        it = iter(oldpage.split('\n'))

        copy_till_table(it, page, "<!--SCORETABLE-->")
        page.append(self.leaderboard_table())
        copy_till_table(it, page, "<!--SUMTABLE-->")
        page.append(self.summary_table())
        copy_till_table(it, page, "<!--BUGTABLE-->")
        page.append(bugstbl)
        page += list(it)

        print("Writing leaguetable_wiki.txt")
        ofile = open("leaguetable_wiki.txt", "w")
        ofile.write("\n".join(page))


######################################################################

parser = argparse.ArgumentParser(description="Updates Bughunters League Table; reads bughunters.txt and writes leaguetable_wiki.txt")
parser.add_argument("--since", help="Count bugs fixed since this svn revision (inclusive)", type=int, default=0)
args = parser.parse_args()

LeagueTableUpdater(args.since).update_article()
