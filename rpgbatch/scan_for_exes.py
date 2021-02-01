#!/usr/bin/env python
#
# Find .exe files in zips
from rpgbatch import scantools

class ExeScanner(scantools.Scanner):

    def setup_stats(self, stats):
        stats.field('exes', context_type = 'zips')
        stats.field('custom.exe', context_type = 'zips')

    def process_zip(self, stats, zipinfo):
        for exe in zipinfo.exes:
            stats.add('exes', exe)
            if 'custom' in exe.lower():
                stats.add('custom.exe')

ExeScanner().run()
