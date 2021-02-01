#!/usr/bin/env python
#
# Example of a script that scan attacks

from __future__ import print_function
import time
from nohrio.ohrrpgce import *
from rpgbatch.rpgbatch import readbit
from rpgbatch import scantools

class AttackScanner(scantools.Scanner):
    def setup_stats(self, stats):
        stats.field('attacks', print_each = False)
        stats.field('num_reset_attacks', '"reset targ stat" attacks')
        stats.field('bitandfail', "attacks with elemental fail conds")
        stats.field('reset_without_inflict', 'attacks with "show without inflicting"')
        stats.field('badattacks', "attacks with bad data")

    def process_game(self, stats, rpg, gameinfo, zipinfo):
        gameinfo.extra_info = time.ctime(gameinfo.mtime)

        try:
            attacks = rpg.data('attack.full')
        except (ValueError, AssertionError) as e:
            print(e)
            stats.add('badattacks')
            return

        def describe_attack(attackid, attack):
            return ('attack %d "%s" \t  description: "%s"'
                    % (attackid, get_str16(attack['name']), get_str8(attack['description'])))

        for attackid, attack in enumerate(attacks):
            stats.add('attacks')
            if readbit(attack['bitsets1'], 57):  # "reset target stat to max before hit"
                #print attack['bitsets1']
                stats.add('num_reset_attacks')

                if readbit(attack['bitsets1'], 51):  # "show damage without inflicting"
                    stats.add('reset_without_inflict', describe_attack(attackid, attack))

                if False:
                    if attack['target_stat'] <= 1:  #targetting HP or MP
                        for i in range(21, 21 + 16):
                            # Check whether any of the old 16 elemental + enemy killer bits are set
                            if readbit(attack['bitsets1'], i):
                                stats.add('bitandfail', describe_attack(attackid, attack))
                                break

AttackScanner().run()
