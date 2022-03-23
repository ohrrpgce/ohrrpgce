#!/usr/bin/env python3
#
# Script to scan for attacks assigned to
# an enemy which cost gold

import sys
from nohrio.ohrrpgce import *
from rpgbatch.rpgbatch import RPGIterator

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
rpgsources = sys.argv[1:]

totalattacks = 0
totalenemies = 0
attacks_costing_gold = 0
enemies_costing_gold = 0
games_w_enemies_costing_gold = 0
games_w_badattacks = 0
games_w_badenemies = 0

rpgs = RPGIterator(rpgsources)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.archinym = rpg.archinym.prefix
    gameinfo.arch_version = rpg.archinym.version

    print("Processing RPG ", gameinfo.id, "from", gameinfo.src)
    print(" > ", gameinfo.longname, " --- ", gameinfo.aboutline)

    try:
        attacks = rpg.data('attack.full')
    except () as e: #(ValueError, AssertionError) as e:
        print("Ex1: ", e)
        games_w_badattacks += 1
        continue

    try:
        enemies = rpg.flexidata('dt1')
    except () as e:#(ValueError, AssertionError) as e:
        print("Ex2:", e)
        games_w_badenemies += 1
        continue

    numelements = rpg.data('gen')['numelements']

    affected = 0  # num affected enemies
    totalattacks += len(attacks)
    totalenemies += len(enemies)

    costing_gold = [attackid for attackid, attack in enumerate(attacks) if attack['cost']['money'] > 0]
    attacks_costing_gold += len(costing_gold)

    def process_enemy_attack(attackid):
        if attackid in costing_gold:
            print("Enemy %d uses gold-costing attack %d %s" % (enemyid, attackid, get_str16(attacks[attackid]['name'])))
            return 1
        return 0

    # Find enemies which use one of those attacks
    for enemyid, enemy in enumerate(enemies):
        for atype in ('regular', 'desperation', 'alone', 'counter_stat'):
            for attackid in enemy['attacks'][atype]:
                affected += process_enemy_attack(attackid)

        for atype, el_offset in (('counter_elem', 0), ('counter_elem2', 8)):
            for attacknum, attackid in enumerate(enemy['attacks'][atype]):
                # Ignore attacks in slots for unused elements
                if el_offset + attacknum < numelements:
                    affected += process_enemy_attack(attackid)

    enemies_costing_gold += affected
    if affected:
        games_w_enemies_costing_gold += 1

rpgs.print_summary()

print("COUNTS:")
print("games with: bad attack data:", games_w_badattacks, " bad enemy data:", games_w_badenemies)
print("num attacks:", totalattacks, "attacks costing gold:", attacks_costing_gold, " num enemies:", totalenemies)
print("enemies_costing_gold", enemies_costing_gold)
print("affected games:", games_w_enemies_costing_gold)
