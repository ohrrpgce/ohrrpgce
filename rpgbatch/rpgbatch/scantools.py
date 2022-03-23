"""
Wrappers around rpgbatch for counting stuff.
"""


import sys
import os
from collections import defaultdict
from . import rpgbatch


class Tabulator:
    """Class for generic logging and tallying occurrences of things.
    Add a field (a thing that is tallied) with field(), then call add()
    for occurrences, and finish with print()."""

    class Field:
        def __init__(self, name, print_each, context_type):
            self.name = name
            self.print_each = print_each
            self.total = 0
            self.counts = defaultdict(int)
            self.occurrences = defaultdict(list)
            self.context_type = context_type

        def add(self, thing = 1, context = None):
            context = str(context)
            if isinstance(thing, (float, int)):
                self.total += thing
                self.counts[context] += thing
            else:
                self.total += 1
                self.counts[context] += 1
                # Don't hold onto thing, convert to str
                self.occurrences[context].append(str(thing))

    def __init__(self):
        self.fields = {}
        self.context = None

    def field(self, fieldid, name = None, print_each = True, context_type = "games"):
        """Add a loggable thing. 'name' is the description used for printing it,
        'print_each' is whether to print every occurrence."""
        if name is None:
            name = fieldid
        self.fields[fieldid] = self.Field(name, print_each, context_type)

    def set_context(self, context):
        """Set the default context for add()"""
        self.context = str(context)

    def add(self, fieldid, thing = 1, context = 'default'):
        """Log one or more occurrences of a field.
        'thing' is either number of occurrences, or the thing (convertable to string) to log,
        and optionally 'context' is where it occurred."""
        if context is 'default':
            context = self.context
        self.fields[fieldid].add(thing, context)

    def print(self):
        """Print out the occurrences and summaries."""

        def print_summary(field):
            if len(field.counts) == 1 and 'None' in field.counts:
                if len(field.occurrences) == 0:
                    # We recorded neither the contexts nor individual occurrences
                    return False
                context_count = ""
            else:
                context_count = "in %d %s" % (len(field.counts), field.context_type)
            print("%d %s %s" % (field.total, field.name, context_count))
            return True

        # Print occurrences
        print()
        for field in self.fields.values():
            if len(field.counts) and field.print_each and print_summary(field):
                print("-" * 79)
                for context, count in field.counts.items():
                    print("%d in %s" % (count, context))
                    for item in field.occurrences[context]:
                        print("  ", item)
                print()

        # Repeat the summary, including zeroes
        for field in self.fields.values():
            print_summary(field)
        print()


class Scanner:
    """Wrapper around rpgbatch.RPGIterator.
    Subclass this class and define at least one of:

    def process_game(self, stats, rpg, gameinfo, zipinfo):

    def process_zip(self, stats, zipinfo):

    then call run(). Override setup_stats() to add any fields you want to record occurrences of.
    """

    def __init__(self):
        self.stats = Tabulator()
        self.setup_stats(self.stats)

    def setup_stats(self, stats):
        pass

    def run(self):
        if len(sys.argv) < 2:
            sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
        self.rpgbatch_args = sys.argv[1:]

        rpgs = rpgbatch.RPGIterator(self.rpgbatch_args, yield_zips = hasattr(self, "process_zip"))
        for info in rpgs:

            if isinstance(info, rpgbatch.ArchiveInfo):
                # Yielded a single ArchiveInfo after processing a zip
                zipinfo = info
                print("Processing", os.path.basename(zipinfo.path))
                self.stats.set_context(zipinfo)
                self.process_zip(self.stats, zipinfo)
                del zipinfo
            elif hasattr(self, "process_game"):
                rpg, gameinfo, zipinfo = info
                print("Processing RPG ", gameinfo.id, "from", gameinfo.src)
                print(" > ", gameinfo.longname, " --- ", gameinfo.aboutline)
                self.stats.set_context(gameinfo)
                self.process_game(self.stats, rpg, gameinfo, zipinfo)

        print()
        self.stats.print()
        rpgs.print_summary()

