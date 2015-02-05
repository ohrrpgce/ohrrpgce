'OHRRPGCE GAME - More various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "scripting.bi"
#include "savegame.bi"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmod.bi"
#include "bmodsubs.bi"

'FIXME: this should not be called directly here. needs wrapping in allmodex.bi
'Mike: why? it's already wrapped in gfx_*.bas
#include "gfx.bi"

''''' Global variables

'Script commands in this file need to REDIM plotslices() and timers(), but FB
'doesn't let you REDIM a global array in a module other than where it is defined!

'Using a lower bound of 1 because 0 is considered an invalid handle
'The size of 64 is just so we won't have to reallocate for a little while
REDIM plotslices(1 TO 64) as Slice Ptr
plotslicesp = @plotslices(1)

REDIM timers(15) as PlotTimer

SUB add_rem_swap_lock_hero (byref box as TextBox)
'---ADD/REMOVE/SWAP/LOCK
'---ADD---
DIM i as integer
IF box.hero_addrem > 0 THEN
 i = first_free_slot_in_party()
 IF i > -1 THEN
  addhero box.hero_addrem, i
 END IF
END IF '---end if > 0
'---REMOVE---
IF box.hero_addrem < 0 THEN
 IF herocount(40) > 1 THEN
  i = findhero(-box.hero_addrem, 0, 40, 1)
  IF i > -1 THEN gam.hero(i).id = -1
  IF herocount(3) = 0 THEN forceparty
 END IF
END IF '---end if < 0
'---SWAP-IN---
IF box.hero_swap > 0 THEN
 i = findhero(box.hero_swap, 40, 0, -1)
 IF i > -1 THEN
  FOR o as integer = 0 TO 3
   IF gam.hero(o).id = -1 THEN
    doswap i, o
    EXIT FOR
   END IF
  NEXT o
 END IF
END IF '---end if > 0
'---SWAP-OUT---
IF box.hero_swap < 0 THEN
 i = findhero(-box.hero_swap, 0, 40, 1)
 IF i > -1 THEN
  FOR o as integer = 40 TO 4 STEP -1
   IF gam.hero(o).id = -1 THEN
    doswap i, o
    IF herocount(3) = 0 THEN forceparty
    EXIT FOR
   END IF
  NEXT o
 END IF
END IF '---end if < 0
'---UNLOCK HERO---
IF box.hero_lock > 0 THEN
 DIM heroat as integer = findhero(box.hero_lock, 0, 40, 1)
 IF heroat > -1 THEN setbit hmask(), 0, heroat, 0
END IF '---end if > 0
'---LOCK HERO---
IF box.hero_lock < 0 THEN
 DIM heroat as integer = findhero(-box.hero_lock, 0, 40, 1)
 IF heroat > -1 THEN setbit hmask(), 0, heroat, 1
END IF '---end if > 0

'--indirect effects
party_change_updates
END SUB

SUB embedtext (text as string, byval limit as integer=0)
 text = embed_text_codes(text)
 '--enforce limit (if set)
 IF limit > 0 THEN
  text = LEFT(text, limit)
 END IF
END SUB

FUNCTION embed_text_codes (text_in as string, byval callback as ANY Ptr=0, byval arg0 as ANY ptr=0, byval arg1 as ANY ptr=0, byval arg2 as ANY ptr=0) as string
' The callback is a sub that accepts 2 strings and 3 integers. It should have the following signature
' SUB MyCallback(code as string, result as string, n0 as any ptr, n1 as any ptr, n2 as any ptr)

 DIM text as string = text_in
 DIM start as integer = 1
 DIM insert as string
 DO WHILE start < LEN(text)
  '--seek an embed spot
  DIM embedbegin as integer = INSTR(start, text, "${")
  IF embedbegin = 0 THEN EXIT DO '--failed to find an embed spot
  DIM embedend as integer = INSTR(embedbegin + 4, text, "}")
  IF embedend = 0 THEN EXIT DO '--embed spot has no end
  '--break apart the string
  DIM before as string = MID(text, 1, large(embedbegin - 1, 0))
  DIM after as string = MID(text, embedend + 1)
  '--extract the code
  DIM code as string = MID(text, embedbegin + 2, embedend - 1 - (embedbegin + 1))
  '--set a reasonable default for the insert text if the code is not matched
  insert = "${" & code & "}"
  '--extract the command and arg
  DIM act as string = LEFT(code, 1)
  DIM arg_str as string = MID(code, 2)
  '--convert the arg to a number
  DIM arg as integer = str2int(arg_str)
  '--discourage bad arg values (not perfect)
  IF NOT (arg = 0 AND arg_str <> STRING(LEN(arg_str), "0")) THEN
   IF arg >= 0 THEN '--only permit postive args
    '--by default the embed is unchanged
    insert = "${" & act & arg_str & "}"
    '--evalued possible actions
    SELECT CASE UCASE(act)
     CASE "H": '--Hero name by ID
      '--defaults blank if not found
      insert = ""
      '--first search for a copy of the hero in the party
      DIM where as integer = findhero(arg + 1, 0, 40, 1)
      IF where >= 0 THEN
       insert = gam.hero(where).name
      ELSE
       insert = getheroname(arg)
      END IF
     CASE "P": '--Hero name by Party position
      IF arg < 40 THEN
       '--defaults blank if not found
       insert = ""
       IF gam.hero(arg).id >= 0 THEN
        insert = gam.hero(arg).name
       END IF
      END IF
     CASE "C": '--Hero name by caterpillar position
      '--defaults blank if not found
      insert = ""
      DIM where as integer = rank_to_party_slot(arg)
      IF where >= 0 AND where <= 3 THEN
       insert = gam.hero(where).name
      END IF
     CASE "V": '--global variable by ID
      '--defaults blank if out-of-range
      insert = ""
      IF arg >= 0 AND arg <= maxScriptGlobals THEN
       insert = STR(global(arg))
      END IF
     CASE "S": '--string variable by ID
      insert = ""
      IF bound_arg(arg, 0, UBOUND(plotstr), "string ID", "${S#} text box insert", NO) THEN
       insert = plotstr(arg).s
      END IF
     CASE "B": '--buttonname (platform-specific)
      insert = get_buttonname_code(arg)
    END SELECT
   END IF
  END IF
  IF callback <> 0 THEN
   DIM runner as SUB(code as string, result as string, byval arg0 as any ptr, byval arg1 as any ptr, byval arg2 as any ptr)
   runner = callback
   runner(code, insert, arg0, arg1, arg2)
  END IF
  '--skip past this embed
  text = before & insert & after
  start = LEN(before) + LEN(insert) + 1
 LOOP
 RETURN text
END FUNCTION

' Implementation of "string sprintf". Reads from retval(1...).
' retval(1) is the format string id; retval(2...) are the arguments
' Returns the formatted string
FUNCTION script_sprintf() as string
 DIM ret as string
 DIM formatstring as string = plotstr(retvals(1)).s
 DIM nextarg as integer = 2  'retval() index
 DIM copystart as integer = 1  'Position to copy literally from. 1-based indexing

 WHILE copystart <= LEN(formatstring)
  DIM percentptr as zstring ptr
  percentptr = strchr(STRPTR(formatstring) + copystart - 1, ASC("%"))
  'Position of the start of this format code. 1-based indexing
  DIM percentpos as integer
  percentpos = (percentptr - STRPTR(formatstring)) + 1

  IF percentptr = NULL THEN EXIT WHILE
  ret &= MID(formatstring, copystart, percentpos - copystart)

  ' Check what the next letter is
  IF percentptr[1] = 0 THEN  ' End of string
   scripterr interpreter_context_name() & !"Found lone % at end of format string:\n" & formatstring, serrBadOp
   EXIT WHILE
  ELSEIF percentptr[1] = ASC("%") THEN
   ret &= "%"
   copystart = percentpos + 2
  ELSE
   IF nextarg >= curcmd->argc THEN
    scripterr interpreter_context_name() & "There are only " & curcmd->argc & !" formatting arguments, but format string has more codes than that:\n" & formatstring, serrBadOp
    EXIT WHILE
   ELSE
    IF percentptr[1] = ASC("s") THEN
     ' String
     IF valid_plotstr(retvals(nextarg), serrBadOp) THEN
      ret &= plotstr(retvals(nextarg)).s
     END IF
    ELSEIF percentptr[1] = ASC("d") THEN
     ' Decimal
     ret &= retvals(nextarg)
    ELSEIF percentptr[1] = ASC("x") THEN
     ' Hexidecimal
     ret &= LCASE(HEX(retvals(nextarg)))
    ELSEIF percentptr[1] = ASC("c") THEN
     ' Character
     IF bound_arg(retvals(nextarg), 0, 255, "%c character code", , , serrBadOp) THEN
      ret &= CHR(retvals(nextarg))
     END IF
    END IF
    copystart = percentpos + 2
    nextarg += 1
   END IF

  END IF

 WEND
 ret &= MID(formatstring, copystart)

 IF nextarg <> curcmd->argc THEN
  scripterr interpreter_context_name() & "There were more arguments (" & curcmd->argc & ") than were needed (only " & nextarg & !"):\n" & formatstring, serrBadOp
 END IF

 RETURN ret
END FUNCTION


FUNCTION scriptstat (byval id as integer) as bool
'contains an assortment of scripting commands that
'used to depend on access to the hero stat array stat(), but that is irrelevant now,
'because that is a global gam.hero().stat
'Returns true if command was handled.

SELECT CASE as CONST id
 CASE 64'--get hero stat (hero, stat, type)
  'FIXME: unfortunately this can also access hero level and more
  'which will suck when we want to add more stats
  DIM slot as integer = bound(retvals(0), 0, 40)
  WITH gam.hero(slot)
   IF retvals(2) = 0 THEN  'current stat
    DIM statnum as integer = bound(retvals(1), 0, 13)
    IF statnum = 13 THEN
     'This is just backcompat for a very undocumented bugfeature
     scriptret = .wep_pic
    ELSEIF statnum = 12 THEN
     'This is backcompat for a somewhat documented feature
     scriptret = .lev
    ELSE
     scriptret = .stat.cur.sta(statnum)
    END IF
   ELSEIF retvals(2) = 1 THEN  'maximum stat
    DIM statnum as integer = bound(retvals(1), 0, 13)
    IF statnum = 13 THEN
     'This is just backcompat for a very undocumented bugfeature
     scriptret = .wep_pal
    ELSEIF statnum = 12 THEN
     'This is backcompat for a barely documented feature
     scriptret = .lev_gain
    ELSE
     scriptret = .stat.max.sta(statnum)
    END IF
   ELSEIF retvals(2) = 2 THEN  'base stat
    IF valid_stat(retvals(1)) THEN
     scriptret = .stat.base.sta(retvals(1))
    END IF
   ELSE
    scripterr "get hero stat: stat type not 'current stat', 'maximum stat' or 'base stat'"
   END IF
  END WITH
 CASE 66'--add hero
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxHero) THEN
   DIM slot as integer = first_free_slot_in_party()
   IF slot >= 0 THEN
    'retvals(0) is the real hero id, addhero subtracts the 1 again
    addhero retvals(0) + 1, slot
   END IF
   scriptret = slot
  END IF
 CASE 67'--delete hero
  IF herocount(40) > 1 THEN
   DIM i as integer = findhero(bound(retvals(0), 0, 59) + 1, 0, 40, 1)
   IF i > -1 THEN gam.hero(i).id = -1
   IF herocount(3) = 0 THEN forceparty
   party_change_updates
  END IF
 CASE 68'--swap out hero
  DIM i as integer = findhero(retvals(0) + 1, 0, 40, 1)
  IF i > -1 THEN
   FOR o as integer = 40 TO 4 STEP -1
    IF gam.hero(o).id = -1 THEN
     doswap i, o
     IF herocount(3) = 0 THEN forceparty
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 69'--swap in hero
  DIM i as integer = findhero(retvals(0) + 1, 40, 0, -1)
  IF i > -1 THEN
   FOR o as integer = 0 TO 3
    IF gam.hero(o).id = -1 THEN
     doswap i, o
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 83'--set hero stat (hero, stat, value, type)
  'FIXME: this command can also set hero level (without updating stats)
  ' which sucks for when we want to add more stats.
  DIM slot as integer = bound(retvals(0), 0, 40)
  WITH gam.hero(slot)
   IF retvals(3) = 0 THEN  'current stat
    DIM statnum as integer = bound(retvals(1), 0, 13)
    IF statnum = 13 THEN
     'This is just backcompat for a very undocumented bugfeature
     .wep_pic = retvals(2)
    ELSEIF statnum = 12 THEN
     'This is backcompat for a mostly undocumented feature
     .lev = retvals(2)
    ELSE
     .stat.cur.sta(statnum) = retvals(2)
     IF statnum = statHP THEN
      evalherotags
      tag_updates
     END IF
    END IF
   ELSEIF retvals(3) = 1 THEN  'maximum stat
    DIM statnum as integer = bound(retvals(1), 0, 13)
    IF statnum = 13 THEN
     'This is backcompat for a very undocumented bugfeature
     .wep_pal = retvals(2)
    ELSEIF statnum = 12 THEN
     'This is backcompat for an undocumented feature
     .lev_gain = retvals(2)
    ELSE
     .stat.base.sta(statnum) += retvals(2) - .stat.max.sta(statnum)
     .stat.max.sta(statnum) = retvals(2)
    END IF
   ELSEIF retvals(3) = 2 THEN  'base stat
    IF valid_stat(retvals(1)) THEN
     .stat.base.sta(retvals(1)) = retvals(2)
     recompute_hero_max_stats slot
    END IF
   ELSE
    scripterr "set hero stat: stat type not 'current stat', 'maximum stat' or 'base stat'"
   END IF
  END WITH
 CASE 89'--swap by position
  doswap bound(retvals(0), 0, 40), bound(retvals(1), 0, 40)
 CASE 110'--set hero picture
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   DIM heronum as integer = bound(retvals(0), 0, 40)
   DIM whichsprite as integer = bound(retvals(2), 0, 2)
   SELECT CASE whichsprite
    CASE 0:
     gam.hero(heronum).battle_pic = bound(retvals(1), 0, gen(genMaxHeroPic))
    CASE 1:
     gam.hero(heronum).pic = bound(retvals(1), 0, gen(genMaxNPCPic))
     IF heronum < 4 THEN vishero
    CASE 2:
     gam.hero(heronum).portrait_pic = bound(retvals(1), -1, gen(genMaxPortrait))
   END SELECT
  END IF
 CASE 111'--set hero palette
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   DIM heronum as integer = bound(retvals(0), 0, 40)
   DIM whichsprite as integer = bound(retvals(2), 0, 2)
   SELECT CASE whichsprite
    CASE 0:
     gam.hero(heronum).battle_pal = bound(retvals(1), -1, 32767)
    CASE 1:
     gam.hero(heronum).pal = bound(retvals(1), -1, 32767)
     IF heronum < 4 THEN vishero
    CASE 2:
     gam.hero(heronum).portrait_pal = bound(retvals(1), -1, 32767)
   END SELECT
  END IF
 CASE 112'--get hero picture
  SELECT CASE retvals(1)
   CASE 0:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).battle_pic
   CASE 1:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).pic
   CASE 2:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).portrait_pic
  END SELECT
 CASE 113'--get hero palette
  SELECT CASE retvals(1)
   CASE 0:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).battle_pal
   CASE 1:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).pal
   CASE 2:
    scriptret = gam.hero(bound(retvals(0), 0, 40)).portrait_pal
  END SELECT
 CASE 150'--status screen
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    status_screen retvals(0)
   END IF
  END IF
 CASE 152'--spells menu
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    old_spells_menu retvals(0)
   END IF
  END IF
 CASE 154'--equip menu
  'Can explicitly choose a hero to equip
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    equip retvals(0)
   END IF
  ELSEIF retvals(0) = -1 THEN
   'Or pass -1 to equip the first hero in the party
   equip rank_to_party_slot(0)
  END IF
 CASE 157'--order menu
  hero_swap_menu 0
 CASE 158'--team menu
  hero_swap_menu 1
 CASE 183'--set hero level (who, what, allow forgetting spells)
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 THEN  'we should make the regular level limit customisable anyway
   gam.hero(retvals(0)).lev_gain = retvals(1) - gam.hero(retvals(0)).lev
   gam.hero(retvals(0)).lev = retvals(1)
   gam.hero(retvals(0)).exp_next = exptolevel(retvals(1) + 1)
   gam.hero(retvals(0)).exp_cur = 0  'XP attained towards the next level
   updatestatslevelup retvals(0), retvals(2) 'updates stats and spells
  END IF
 CASE 184'--give experience (who, how much)
  'who = -1 targets battle party
  IF retvals(0) <> -1 THEN
   IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
    giveheroexperience retvals(0), retvals(1)
    updatestatslevelup retvals(0), 0
    evalherotags  'could revive a dead hero, I think
    tag_updates
   END IF
  ELSE
   'This sets the level gain and learnt spells and calls updatestatslevelup for every hero
   distribute_party_experience retvals(1)
  END IF
 CASE 185'--hero levelled (who)
  scriptret = gam.hero(bound(retvals(0), 0, 40)).lev_gain
 CASE 186'--spells learnt
  'NOTE: this is deprecated but will remain for backcompat. New games should use "spells learned" 
  DIM found as integer = 0
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   FOR i as integer = retvals(0) * 96 TO retvals(0) * 96 + 95
    IF readbit(learnmask(), 0, i) THEN
     IF retvals(1) = found THEN
      scriptret = spell(retvals(0), (i \ 24) MOD 4, i MOD 24) - 1
      EXIT FOR
     END IF
     found = found + 1
    END IF
   NEXT
   IF retvals(1) = -1 THEN scriptret = found  'getcount
  END IF
 CASE 269'--totalexperience
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = hero_total_exp(retvals(0))
  END IF
 CASE 270'--experience to level
  scriptret = total_exp_to_level(retvals(0))
 CASE 271'--experiencetonextlevel
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = gam.hero(retvals(0)).exp_next - gam.hero(retvals(0)).exp_cur
  END IF
 CASE 272'--setexperience  (who, what, allowforget)
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 THEN
   setheroexperience retvals(0), retvals(1), retvals(2)
  END IF
 CASE 445'--update level up learning(who, allowforget)
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   learn_spells_for_current_level retvals(0), (retvals(1)<>0)
  END IF
 CASE 449'--reset hero picture
  DIM heronum as integer = retvals(0)
  DIM whichsprite as integer = retvals(1)
  IF really_valid_hero_party(heronum, , serrBound) THEN
   IF bound_arg(whichsprite, 0, 2, "hero picture type") THEN
    DIM her as herodef
    loadherodata her, gam.hero(heronum).id
    SELECT CASE whichsprite
     CASE 0:
      gam.hero(heronum).battle_pic = her.sprite
     CASE 1:
      gam.hero(heronum).pic = her.walk_sprite
      IF heronum < 4 THEN vishero
     CASE 2:
      gam.hero(heronum).portrait_pic = her.portrait
    END SELECT
   END IF
  END IF
 CASE 450'--reset hero palette
  DIM heronum as integer = retvals(0)
  DIM whichsprite as integer = retvals(1)
  IF really_valid_hero_party(heronum, , serrBound) THEN
   IF bound_arg(whichsprite, 0, 2, "hero picture type") THEN
    DIM her as herodef
    loadherodata her, gam.hero(heronum).id
    SELECT CASE whichsprite
     CASE 0:
      gam.hero(heronum).battle_pal = her.sprite_pal
     CASE 1:
      gam.hero(heronum).pal = her.walk_sprite_pal
      IF heronum < 4 THEN vishero
     CASE 2:
      gam.hero(heronum).portrait_pal = her.portrait_pal
    END SELECT
   END IF
  END IF
 CASE 497'--set hero base elemental resist (hero, element, percent)
  IF really_valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, gen(genNumElements) - 1, "element number") THEN
    gam.hero(retvals(0)).elementals(retvals(1)) = 0.01 * retvals(2)
   END IF
  END IF
 CASE 498'--hero base elemental resist as int (hero, element)
  IF really_valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, gen(genNumElements) - 1, "element number") THEN
    scriptret = 100 * gam.hero(retvals(0)).elementals(retvals(1))  'rounds to nearest int
   END IF
  END IF
 CASE 499'--hero total elemental resist as int (hero, element)
  IF really_valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, gen(genNumElements) - 1, "element number") THEN
    REDIM elementals(gen(genNumElements) - 1) as single
    calc_hero_elementals elementals(), retvals(0)
    scriptret = 100 * elementals(retvals(1))  'rounds to nearest int
   END IF
  END IF
 CASE 545 '--get hero stat cap (stat)
  'Replaces a plotscr.hsd script
  IF valid_stat(retvals(0)) THEN
   scriptret = gen(genStatCap + retvals(0))
  END IF
 CASE 546 '--set hero stat cap (stat, value)
  IF valid_stat(retvals(0)) THEN
   IF retvals(1) < 0 THEN
    scripterr "set hero stat cap: invalid negative cap value " & retvals(1)
   ELSE
    gen(genStatCap + retvals(0)) = retvals(1)
    FOR hero_slot as integer = 0 TO UBOUND(gam.hero)
     'This is maybe a bit heavy handed, because it caps all stats to the caps.
     update_hero_max_and_cur_stats hero_slot
    NEXT
   END IF
  END IF
 CASE 556 '--input string with virtual keyboard (string ID, maxlen, onlyplayer=-1)
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).s = cheezy_virtual_keyboard(plotstr(retvals(0)).s, retvals(1), retvals(2))
  END IF
 CASE 557'--get item description(str,itm)
  scriptret = 0
  IF valid_plotstr(retvals(0)) THEN
   IF valid_item(retvals(1)) THEN
    plotstr(retvals(0)).s = readitemdescription(retvals(1))
    scriptret = 1
   END IF
  END IF

 CASE ELSE
  RETURN NO

END SELECT
RETURN YES
END FUNCTION

SUB forceparty ()
'---MAKE SURE YOU HAVE AN ACTIVE PARTY---
DIM fpi as integer = findhero(-1, 0, 40, 1)
IF fpi > -1 THEN
 FOR fpo as integer = 0 TO 3
  IF gam.hero(fpo).id = -1 THEN
   doswap fpi, fpo
   EXIT FOR
  END IF
 NEXT fpo
END IF
END SUB

FUNCTION gethighbyte (byval n as integer) as integer
RETURN n SHL 8
END FUNCTION

'Deprecated; Use get_valid_npc for all new NPC commands
FUNCTION getnpcref (byval seekid as integer, byval offset as integer) as integer
SELECT CASE seekid

 CASE -300 TO -1'--direct reference
  getnpcref = (seekid + 1) * -1
  EXIT FUNCTION

 CASE 0 TO UBOUND(npcs) 'ID
  DIM found as integer = 0
  FOR i as integer = 0 TO 299
   IF npc(i).id - 1 = seekid THEN
    IF found = offset THEN
     getnpcref = i
     EXIT FUNCTION
    END IF
    found = found + 1
   END IF
  NEXT i

END SELECT

'--failure
getnpcref = -1
END FUNCTION

'Replacement for getnpcref.
'Given NPC ref or NPC ID, return npc() index, or throw a scripterr and return -1
'Note this is stricter than getnpcref: invalid npc refs are not alright!
'References to Hidden/Disabled NPCs are alright.
FUNCTION get_valid_npc (byval seekid as integer, byval errlvl as scriptErrEnum = serrBadOp) as integer
 IF seekid < 0 THEN
  DIM npcidx as integer = (seekid + 1) * -1
  IF npcidx > 299 ORELSE npc(npcidx).id = 0 THEN
   scripterr current_command_name() & ": invalid npc reference " & seekid & " (maybe the NPC was deleted?)", errlvl
   RETURN -1
  END IF
  RETURN npcidx
 ELSE
  FOR i as integer = 0 TO 299
   IF npc(i).id - 1 = seekid THEN RETURN i
  NEXT
  scripterr current_command_name() & ": invalid npc reference; no NPCs of ID " & seekid & " exist", errlvl
  RETURN -1
 END IF
END FUNCTION

'Given NPC ref or NPC ID, return an NPC ID, or throw a scripterr and return -1
'References to Hidden/Disabled NPCs are alright.
FUNCTION get_valid_npc_id (byval seekid as integer, byval errlvl as scriptErrEnum = serrBadOp) as integer
 IF seekid >= 0 THEN
  IF seekid > UBOUND(npcs) THEN
   scripterr current_command_name() & ": invalid NPC ID " & seekid, errlvl
   RETURN -1
  END IF
  RETURN seekid
 ELSE
  DIM npcidx as integer = (seekid + 1) * -1
  IF npcidx > UBOUND(npc) THEN
   scripterr current_command_name() & ": invalid NPC reference " & seekid, errlvl
   RETURN -1
  ELSEIF npc(npcidx).id = 0 THEN
   scripterr current_command_name() & ": invalid NPC reference " & seekid & " (maybe the NPC was deleted?)", errlvl
   RETURN -1
  ELSE
   DIM id as integer = ABS(npc(npcidx).id) - 1
   IF id > UBOUND(npcs) THEN
    'Note that an NPC may be marked hidden because it has an invalid ID
    scripterr current_command_name() & ": NPC reference " & seekid & " is for a disabled NPC with invalid ID " & npc(npcidx).id & " (the map must be incompletely loaded)", errlvl
    RETURN -1
   END IF
   RETURN id
  END IF
 END IF
END FUNCTION

SUB greyscalepal
FOR i as integer = bound(retvals(0), 0, 255) TO bound(retvals(1), 0, 255)
 master(i).r = bound((master(i).r + master(i).g + master(i).b) / 3, 0, 255)
 master(i).g = master(i).r
 master(i).b = master(i).r
NEXT i
END SUB

FUNCTION rank_to_party_slot (byval rank as integer) as integer
 'Returns the party slot of the nth hero in the party (not just caterpillar party), or -1
 DIM heronum as integer = -1
 FOR party_slot as integer = 0 TO 3
  IF gam.hero(party_slot).id >= 0 THEN heronum += 1
  IF heronum = rank THEN
   RETURN party_slot
  END IF
 NEXT
 RETURN -1
END FUNCTION

FUNCTION party_slot_to_rank (byval slot as integer) as integer
 'Returns the rank of the hero in a party slot (not just caterpillar party), or -1 if invalid
 IF slot < -1 OR slot > UBOUND(gam.hero) THEN RETURN -1
 DIM heronum as integer = 0
 FOR party_slot as integer = 0 TO slot - 1
  IF gam.hero(party_slot).id >= 0 THEN heronum += 1
 NEXT
 RETURN heronum
END FUNCTION

FUNCTION herobyrank (byval rank as integer) as integer
 'Return the ID of the nth hero in the *caterpillar* party
 DIM party_slot as integer = rank_to_party_slot(rank)
 IF party_slot >= 0 AND party_slot <= 3 THEN RETURN gam.hero(party_slot).id
 RETURN -1
END FUNCTION

SUB interpolatecat
'given the current positions of the caterpillar party, interpolate their inbetween frames
FOR o as integer = 0 TO 10 STEP 5
 FOR i as integer = o + 1 TO o + 4
  catx(i) = catx(i - 1) + ((catx(o + 5) - catx(o)) / 5)
  caty(i) = caty(i - 1) + ((caty(o + 5) - caty(o)) / 5)
  catd(i) = catd(o)
 NEXT i
NEXT o
END SUB

SUB visnpc()
 'Hide/Unhide NPCs based on tag requirements. (No other function should do so)
 'This SUB will be called when a map is incompletely loaded (NPC instances before definitions
 'or vice versa), and that's hard to avoid, because a script could load them with two separate loadmapstate
 'calls. So we must tolerate invalid NPC IDs and anything else. So here we mark all NPCs as hidden which
 'would otherwise cause problems

 'To scripts, hiding an NPC is like deleting it, and unhiding an NPC is like creating it.
 'Therefore, zone exit triggers *do not* happen when hiding an NPC, and zone entry triggers *do*
 'happen when unhiding an NPC (rather than remembering the old zones)
 'However, we run the zone entry triggers elsewhere (update_npcs), otherwise tags toggled by the
 'triggers would immediately affect NPCs not yet processed (it's better if the order doesn't
 'matter), and worse, visnpc might be called several times per tick!

 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id = 0 THEN CONTINUE FOR

  DIM npc_id as integer = ABS(npc(i).id) - 1

  IF npc_id > UBOUND(npcs) THEN
   'Invalid ID number; hide. Probably a partially loaded map.
   npc(i).id = -npc_id - 1
   CONTINUE FOR
  END IF
 
  '--check tags
  IF istag(npcs(npc_id).tag1, 1) ANDALSO istag(npcs(npc_id).tag2, 1) ANDALSO istag(onetime(), npcs(npc_id).usetag, 0) = 0 THEN
   npc(i).id = npc_id + 1
  ELSE
   npc(i).id = -npc_id - 1
  END IF
  
  IF npc(i).id > 0 THEN
   '--NPC exists and is visible
   IF npc(i).sl = 0 THEN
    npc(i).sl = create_walkabout_slices(npc_layer())
    'debug "npc(" & i & ").sl=" & npc(i).sl & " [visnpc]"
    '--set sprite
    set_walkabout_sprite npc(i).sl, npcs(npc_id).picture, npcs(npc_id).palette
   END IF
  ELSE
   '--hidden
   IF npc(i).sl <> 0 THEN
    'debug "delete npc sl " & i & " [visnpc]"
    DeleteSlice @npc(i).sl
   END IF
   v_free npc(i).curzones
  END IF

 NEXT i
END SUB

FUNCTION script_keyval (byval key as integer, byval joynum as integer = 0) as integer
 'Wrapper around keyval for use by scripts: performs scancode mapping for back-compat

 DIM ret as integer = 0

 IF key >= 0 AND key <= 127 THEN
  ret = keyval(key)
 ELSEIF key >= 128 AND key <= 147 THEN
  'This is just partial joystick support! We don't support keyrepeat, instead both bits have
  'same value.
  'For real joystick support, it needs to be handled in allmodex.bas.
  DIM b as integer, xaxis as integer, yaxis as integer '0 >= {xaxis, yaxis} >= 100
  IF readjoy(joynum, b, xaxis, yaxis) THEN
   IF key >= 128 AND key <= 143 THEN
    ret = (b SHR (key - 128)) AND 1
   ELSEIF key = 144 THEN 'x left
    ret = abs(xaxis <= -50) 'true = -1...
   ELSEIF key = 145 THEN 'x right
    ret = abs(xaxis >= 50)
   ELSEIF key = 146 THEN 'y up
    ret = abs(yaxis <= -50)
   ELSEIF key = 147 THEN 'y down
    ret = abs(yaxis >= 50)
   END IF
  ELSE
   ret = 0
  END IF
  'Set key-down and key-press bits
  ret *= 3
 END IF

 IF readbit(gen(), genBits2, 8) = 0 THEN  'If improved scancodes not enabled
  'The new scancodes separate some keys which previously had the same scancode.
  'For backwards compatibility (whether or not you recompile your scripts with
  'a new copy of scancodes.hsi) we make the newly separated scancodes behave
  'as if they were indistinguishable.
  SELECT CASE key
   CASE scHome TO scDelete
    ret OR= keyval(key + scNumpad7 - scHome)
   CASE scNumpad7 TO scNumpad9, scNumpad4 TO scNumpad6, scNumpad1 TO scNumpadPeriod
    ret OR= keyval(key - scNumpad7 + scHome)
   CASE scSlash:       ret OR= keyval(scNumpadSlash)
   CASE scEnter:       ret OR= keyval(scNumpadEnter)
   CASE scNumlock:     ret OR= keyval(scPause)
   CASE scNumpadSlash: ret OR= keyval(scSlash)
   CASE scNumpadEnter: ret OR= keyval(scEnter)
   CASE scPause:       ret OR= keyval(scNumlock)
  END SELECT
 END IF

 RETURN ret
END FUNCTION

SUB onkeyscript (byval scriptnum as integer)
 DIM doit as integer = NO

 'carray is checked just for joystick movement
 FOR i as integer = 0 TO 5
  IF carray(i) THEN doit = YES: EXIT FOR
 NEXT i

 'Checks keyboard and joystick keys
 IF anykeypressed(YES, 0) THEN doit = YES

 'Because anykeypressed doesn't check it, and we don't want to break scripts looking for key:alt (== scUnfilteredAlt)
 IF keyval(scUnfilteredAlt) > 0 THEN doit = YES

 IF gam.mouse_enabled THEN
  IF gam.mouse.buttons THEN doit = YES
 END IF
 
 IF nowscript >= 0 THEN
  IF scriptinsts(nowscript).waiting = waitingOnCmd AND scriptinsts(nowscript).curvalue = 9 THEN
   '--never trigger a onkey script when the previous script
   '--has a "wait for key" command active
   doit = NO
  END IF
 END IF

 IF doit THEN
  trigger_script scriptnum, YES, "on-key", "", scrqBackcompat()
 END IF

END SUB

FUNCTION playtime (byval d as integer, byval h as integer, byval m as integer) as string
 DIM s as string = ""

 SELECT CASE d
  CASE 1
   s = s & d & " " & readglobalstring(154, "day", 10) & " "
  CASE IS > 1
   s = s & d & " " & readglobalstring(155, "days", 10) & " "
 END SELECT

 SELECT CASE h
  CASE 1
   s = s & h & " " & readglobalstring(156, "hour", 10) & " "
  CASE IS > 1
   s = s & h & " " & readglobalstring(157, "hours", 10) & " "
 END SELECT

 SELECT CASE m
  CASE 1
   s = s & m & " " & readglobalstring(158, "minute", 10) & " "
  CASE IS > 1
   s = s & m & " " & readglobalstring(159, "minutes", 10) & " "
 END SELECT

 RETURN s

END FUNCTION

SUB playtimer
 STATIC n as double
 
 IF TIMER >= n + 1 OR n - TIMER > 3600 THEN
  n = INT(TIMER)
  gen(genSeconds) = gen(genSeconds) + 1
  WHILE gen(genSeconds) >= 60
   gen(genSeconds) = gen(genSeconds) - 60
   gen(genMinutes) = gen(genMinutes) + 1
  WEND
  WHILE gen(genMinutes) >= 60
   gen(genMinutes) = gen(genMinutes) - 60
   gen(genHours) = gen(genHours) + 1
   refresh_keepalive_file
  WEND
  WHILE gen(genHours) >= 24
   gen(genHours) = gen(genHours) - 24
   IF gen(genDays) < 32767 THEN gen(genDays) = gen(genDays) + 1
  WEND
 END IF
 IF autosnap > 0 ANDALSO (get_tickcount MOD autosnap) = 0 THEN
  write_checkpoint
 END IF
END SUB

FUNCTION rankincaterpillar (byval heroid as integer) as integer
 'Returns -1 if the hero is not found.
 'Returns the last hero's rank if there are more than one copy of the same hero
 
 DIM result as integer = -1
 DIM o as integer = 0
 FOR i as integer = 0 TO 3
  IF gam.hero(i).id >= 0 THEN
   IF gam.hero(i).id = heroid THEN result = o
   o += 1
  END IF
 NEXT i
 RETURN result
END FUNCTION

FUNCTION scriptmisc (byval id as integer) as bool

'contains a whole mess of scripting commands that do not depend on
'any main-module level local variables or GOSUBs
'Returns true if command was handled.

DIM npcref as integer = ANY

SELECT CASE as CONST id

 CASE 135'--puthero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   cropposition retvals(1), retvals(2), 20
   catx(retvals(0) * 5) = retvals(1)
   caty(retvals(0) * 5) = retvals(2)
  END IF
 CASE 136'--putnpc
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   cropposition retvals(1), retvals(2), 20
   npc(npcref).x = retvals(1)
   npc(npcref).y = retvals(2)
  END IF
 CASE 137'--putcamera
  gen(cameramode) = stopcam
  mapx = retvals(0)
  mapy = retvals(1)
  limitcamera mapx, mapy
 CASE 138'--heropixelx
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catx(retvals(0) * 5)
  END IF
 CASE 139'--heropixely
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = caty(retvals(0) * 5)
  END IF
 CASE 140'--npcpixelx
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).x
  END IF
 CASE 141'--npcpixely
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).y
  END IF
 CASE 142'--camerapixelx
  scriptret = mapx
 CASE 143'--camerapixely
  scriptret = mapy
 CASE 147'--read general
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(gen) THEN
   scriptret = gen(retvals(0))
  END IF
 CASE 148'--write general
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(gen) THEN
   gen(retvals(0)) = retvals(1)
  END IF
 CASE 159'--init mouse
  IF havemouse() THEN scriptret = 1 ELSE scriptret = 0
  hidemousecursor
  gam.mouse = readmouse  'Why do we do this?
  gam.mouse_enabled = YES
 CASE 160'--get mouse x
  scriptret = gam.mouse.x
 CASE 161'--get mouse y
  scriptret = gam.mouse.y
 CASE 162'--mouse button
  IF retvals(0) <= 2 THEN
   IF gam.mouse.buttons AND (2 ^ retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 163'--put mouse
  movemouse bound(retvals(0), 0, 319), bound(retvals(1), 0, 199)
  gam.mouse = readmouse
 CASE 164'--mouse region(xmin, xmax, ymin, ymax)
  IF retvals(0) = -1 AND retvals(1) = -1 AND retvals(2) = -1 AND retvals(3) = -1 THEN
   mouserect -1, -1, -1, -1
  ELSE
   retvals(0) = bound(retvals(0), 0, 319)
   retvals(1) = bound(retvals(1), retvals(0), 319)
   retvals(2) = bound(retvals(2), 0, 199)
   retvals(3) = bound(retvals(3), retvals(2), 199)
   mouserect retvals(0), retvals(1), retvals(2), retvals(3)
  END IF
  gam.mouse = readmouse
 CASE 178'--read gmap
  IF retvals(0) >= 0 AND retvals(0) <= 19 THEN
   scriptret = gmap(retvals(0))
  END IF
 CASE 179'--write gmap
  IF retvals(0) >= 0 AND retvals(0) <= 19 THEN
   gmap(retvals(0)) = retvals(1)
   IF retvals(0) = 2 OR retvals(0) = 3 THEN check_menu_tags  'save and minimap menu options
   IF retvals(0) = 4 THEN gam.showtext_ticks = 0  'cancel map name display
   IF retvals(0) = 5 THEN setoutside -1  'hint: always use the wrapper in plotscr.hsd
   IF retvals(0) = 6 AND gmap(5) = 2 THEN setoutside retvals(1)
   IF retvals(0) = 16 THEN refresh_walkabout_layer_sort()
   lump_reloading.gmap.dirty = YES
  END IF
 CASE 492'--mouse click
  IF retvals(0) <= 2 THEN
   IF gam.mouse.clicks AND (2 ^ retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  END IF

'old scriptmisc

 CASE 0'--noop
  scripterr "encountered clean noop", serrInfo
 CASE 1'--Wait (cycles)
  IF retvals(0) > 0 THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 2'--wait for all
  script_start_waiting(retvals(0))
 CASE 3'--wait for hero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 4'--waitforNPC
  IF retvals(0) >= -300 AND retvals(0) <= UBOUND(npcs) THEN
   script_start_waiting(retvals(0), gam.map.id)
  END IF
 CASE 5'--suspend npcs
  setbit gen(), genSuspendBits, suspendnpcs, 1
 CASE 6'--suspend player
  setbit gen(), genSuspendBits, suspendplayer, 1
  update_virtual_gamepad_display()
 CASE 7'--resume npcs
  setbit gen(), genSuspendBits, suspendnpcs, 0
 CASE 8'--resume player
  setbit gen(), genSuspendBits, suspendplayer, 0
  update_virtual_gamepad_display()
 CASE 9'--wait for key
  script_start_waiting(retvals(0))
 CASE 10'--walk hero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   SELECT CASE retvals(1)
    CASE 0'--north
     catd(retvals(0) * 5) = 0
     herow(retvals(0)).ygo = retvals(2) * 20
    CASE 1'--east
     catd(retvals(0) * 5) = 1
     herow(retvals(0)).xgo = (retvals(2) * 20) * -1
    CASE 2'--south
     catd(retvals(0) * 5) = 2
     herow(retvals(0)).ygo = (retvals(2) * 20) * -1
    CASE 3'--west
     catd(retvals(0) * 5) = 3
     herow(retvals(0)).xgo = retvals(2) * 20
   END SELECT
  END IF
 CASE 12'--check tag
  scriptret = ABS(istag(retvals(0), 0))
 CASE 13'--set tag
  IF retvals(0) > 1 THEN
   IF retvals(0) <= max_tag() THEN
    settag tag(), retvals(0), retvals(1)
   ELSE
    'This is the sort of thing scripterr level 1 was for, except we can't go adding
    'those to existing commands
    debug "Setting onetime tags with the settag command is deprecated"
    settag onetime(), retvals(0) - (max_tag()+1), retvals(1)
   END IF
   tag_updates
  END IF
 CASE 17'--get item
  IF valid_item(retvals(0)) THEN
   IF retvals(1) >= 1 THEN
    getitem retvals(0), retvals(1)
    evalitemtags
    tag_updates
   END IF
  END IF
 CASE 18'--delete item
  IF valid_item(retvals(0)) THEN
   IF retvals(1) >= 1 THEN
    delitem retvals(0), retvals(1)
    evalitemtags
    tag_updates
   END IF
  END IF
 CASE 19'--leader
  scriptret = herobyrank(0)
 CASE 20'--get money
  gold = gold + retvals(0)
 CASE 21'--lose money
  gold = gold - retvals(0)
  IF gold < 0 THEN gold = 0
 CASE 22'--pay money
  IF gold - retvals(0) >= 0 THEN
   gold = gold - retvals(0)
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 25'--set hero frame
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   herow(retvals(0)).wtog = bound(retvals(1), 0, 1) * 2
  END IF
 CASE 27'--suspend overlay
  setbit gen(), genSuspendBits, suspendoverlay, 1
 CASE 28'--play song
  'loadsong game + "." + STR(retvals(0))
  wrappedsong retvals(0)
 CASE 29'--stop song
  stopsong
 CASE 30'--keyval
  'This used to be keyispressed; which undocumentedly reported two bits
  'instead of true/false.
  IF retvals(0) >= 0 AND retvals(0) <= 147 THEN
   'keyval() reports a 3rd bit, but didn't at the time that this command was (re-)documented
   scriptret = script_keyval(retvals(0)) AND 3
  ELSE
   scripterr "invalid scancode keyval(" & retvals(0) & ")", serrBound
  END IF
 CASE 31'--rank in caterpillar
  scriptret = rankincaterpillar(retvals(0))
 CASE 38'--camera follows hero
  gen(cameramode) = herocam
  gen(cameraArg) = bound(retvals(0), 0, 3)
 CASE 40'--pan camera
  gen(cameramode) = pancam
  gen(cameraArg) = small(large(retvals(0), 0), 3)
  gen(cameraArg2) = large(retvals(1), 0) * (20 / large(retvals(2), 1))
  gen(cameraArg3) = large(retvals(2), 1)
 CASE 41'--focus camera
  gen(cameramode) = focuscam
  gen(cameraArg) = (retvals(0) * 20) - 150
  gen(cameraArg2) = (retvals(1) * 20) - 90
  gen(cameraArg3) = ABS(retvals(2))
  gen(cameraArg4) = ABS(retvals(2))
  limitcamera gen(cameraArg), gen(cameraArg2)
 CASE 42'--wait for camera
  script_start_waiting(retvals(0))
 CASE 43'--hero x
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catx(retvals(0) * 5) \ 20
  END IF
 CASE 44'--hero y
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = caty(retvals(0) * 5) \ 20
  END IF
 CASE 47'--suspend obstruction
  setbit gen(), genSuspendBits, suspendobstruction, 1
 CASE 48'--resume obstruction
  setbit gen(), genSuspendBits, suspendobstruction, 0
 CASE 49'--suspend hero walls
  setbit gen(), genSuspendBits, suspendherowalls, 1
 CASE 50'--suspend NPC walls
  setbit gen(), genSuspendBits, suspendnpcwalls, 1
 CASE 51'--resume hero walls
  setbit gen(), genSuspendBits, suspendherowalls, 0
 CASE 53'--set hero direction
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   catd(retvals(0) * 5) = ABS(retvals(1)) MOD 4
  END IF
 CASE 57, 118'--suspend caterpillar
  setbit gen(), genSuspendBits, suspendcaterpillar, 1
 CASE 58, 119'--resume caterpillar
  setbit gen(), genSuspendBits, suspendcaterpillar, 0
  interpolatecat
 CASE 59'--wait for text box
  IF readbit(gen(), genSuspendBits, suspendboxadvance) = 0 THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 60'--equip where
  scriptret = 0
  IF valid_item(retvals(1)) THEN
   IF valid_hero_party(retvals(0)) THEN
    loaditemdata buffer(), retvals(1)
    DIM hero_id as integer = gam.hero(retvals(0)).id
    IF hero_id >= 0 THEN
     IF readbit(buffer(), 66, hero_id) THEN
      scriptret = buffer(49)
     END IF
    END IF
   END IF
  END IF
 CASE 62, 168'--suspend random enemies
  setbit gen(), genSuspendBits, suspendrandomenemies, 1
  '--resume random enemies is not here! it works different!
 CASE 65'--resume overlay
  setbit gen(), genSuspendBits, suspendoverlay, 0
 CASE 70'--room in active party
  scriptret = 4 - herocount(3)
 CASE 71'--lock hero
  DIM hero_slot as integer = findhero(retvals(0) + 1, 0, 40, 1)
  IF hero_slot > -1 THEN setbit hmask(), 0, hero_slot, 1
 CASE 72'--unlock hero
  DIM hero_slot as integer = findhero(retvals(0) + 1, 0, 40, 1)
  IF hero_slot > -1 THEN setbit hmask(), 0, hero_slot, 0
 CASE 74'--set death script
  gen(genGameoverScript) = large(retvals(0), 0)
 CASE 75'--fade screen out
  FOR i as integer = 0 TO 2
   retvals(i) = bound(iif(retvals(i), retvals(i) * 4 + 3, 0), 0, 255)
  NEXT
  fadeout retvals(0), retvals(1), retvals(2)
  IF gam.need_fade_in ANDALSO gam.fade_in_script_overridable THEN
   'For backwards compatibility, if a fade delay has been increased so that a
   'fadescreenout that used to occur after a queued fade now happens before,
   'that queued fade needs to be cancelled so that the screen stays faded until
   'the corresponding fadescreenin.
   gam.need_fade_in = NO
  END IF
 CASE 76'--fade screen in
  fadein
  IF gam.need_fade_in AND gam.fade_in_delay <= 0 THEN
   'Avoid unnecessary pause
   gam.need_fade_in = NO
  END IF
 CASE 81'--set hero speed
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   herow(retvals(0)).speed = bound(retvals(1), 0, 20)
  END IF
 CASE 82'--inventory
  scriptret = countitem(retvals(0))
 CASE 84'--suspend box advance
  setbit gen(), genSuspendBits, suspendboxadvance, 1
 CASE 85'--resume box advance
  setbit gen(), genSuspendBits, suspendboxadvance, 0
 CASE 87'--set hero position
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
  cropposition retvals(1), retvals(2), 1
   FOR i as integer = 0 TO 4
    catx(small(retvals(0) * 5 + i, 15)) = retvals(1) * 20
    caty(small(retvals(0) * 5 + i, 15)) = retvals(2) * 20
   NEXT i
  END IF
 CASE 90'--find hero
  scriptret = findhero(retvals(0) + 1, 0, 40, 1)
 CASE 91'--check equipment
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = eqstuf(retvals(0), bound(retvals(1) - 1, 0, 4)) - 1
  ELSE
   scriptret = 0
  END IF
 CASE 92'--days of play
  scriptret = gen(genDays)
 CASE 93'--hours of play
  scriptret = gen(genHours)
 CASE 94'--minutes of play
  scriptret = gen(genMinutes)
 CASE 95'--resume NPC walls
  setbit gen(), genSuspendBits, suspendnpcwalls, 0
 CASE 96'--set hero Z
  catz(bound(retvals(0), 0, 3) * 5) = retvals(1)
 CASE 102'--hero direction
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catd(retvals(0) * 5)
  END IF
 CASE 103'--reset palette
  loadpalette master(), gam.current_master_palette
  LoadUIColors uilook(), boxlook(), gam.current_master_palette
 CASE 104'--tweak palette
  IF bound_arg(retvals(3), 0, 255, "start pal index") THEN
   IF bound_arg(retvals(4), 0, 255, "end pal index") THEN
    tweakpalette retvals(0), retvals(1), retvals(2), retvals(3), retvals(4)
   END IF
  END IF
 CASE 105'--read color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   IF retvals(1) = 0 THEN scriptret = master(retvals(0)).r / 4
   IF retvals(1) = 1 THEN scriptret = master(retvals(0)).g / 4
   IF retvals(1) = 2 THEN scriptret = master(retvals(0)).b / 4
  END IF
 CASE 106'--write color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   DIM col as integer = bound(retvals(2), 0, 63)
   IF retvals(1) = 0 THEN master(retvals(0)).r = iif(col, col * 4 + 3, 0)
   IF retvals(1) = 1 THEN master(retvals(0)).g = iif(col, col * 4 + 3, 0)
   IF retvals(1) = 2 THEN master(retvals(0)).b = iif(col, col * 4 + 3, 0)
  END IF
 CASE 107'--update palette
  setpal master()
 CASE 108'--seed random
  IF retvals(0) THEN
   mersenne_twister retvals(0)
  ELSE
   mersenne_twister TIMER
  END IF
 CASE 109'--grey scale palette
  greyscalepal
 CASE 114'--read global
  IF retvals(0) >= 0 AND retvals(0) <= maxScriptGlobals THEN
   scriptret = global(retvals(0))
  ELSE
   scripterr "readglobal: Cannot read global " & retvals(0) & ". Out of range", serrBadOp
  END IF
 CASE 115'--write global
  IF retvals(0) >= 0 AND retvals(0) <= maxScriptGlobals THEN
   global(retvals(0)) = retvals(1)
  ELSE
   scripterr "writeglobal: Cannot write global " & retvals(0) & ". Out of range", serrBadOp
  END IF
 CASE 116'--hero is walking
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF herow(retvals(0)).xgo = 0 AND herow(retvals(0)).ygo = 0 THEN
    scriptret = 0
   ELSE
    scriptret = 1
   END IF
   IF readbit(gen(), genSuspendBits, suspendcaterpillar) = 0 THEN
    ' Other heroes trail behind the leader automatically without using .xgo and .ygo.
    ' walkhero partially works when the caterpillar party is enabled too
    ' (well they move, but don't animate), so combine the two
    IF herow(0).xgo <> 0 OR herow(0).ygo <> 0 THEN
     scriptret = 1
    END IF
   END IF
  END IF
 CASE 127'--teach spell
  scriptret = trylearn(bound(retvals(0), 0, 40), retvals(1))
 CASE 128'--forget spell
  scriptret = 0
  retvals(0) = bound(retvals(0), 0, 40)
  FOR i as integer = 0 TO 3
   FOR j as integer = 0 TO 23
    IF spell(retvals(0), i, j) = retvals(1) THEN
     spell(retvals(0), i, j) = 0
     scriptret = 1
    END IF
   NEXT j
  NEXT i
 CASE 129'--read spell
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 THEN
   scriptret = spell(retvals(0), retvals(1), retvals(2))
  ELSE
   scriptret = 0
  END IF
 CASE 130'--write spell
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 AND retvals(3) >= 0 THEN
   spell(retvals(0), retvals(1), retvals(2)) = retvals(3)
  END IF
 CASE 131'--knows spell
  scriptret = 0
  retvals(0) = bound(retvals(0), 0, 40)
  IF retvals(1) > 0 THEN
   FOR i as integer = 0 TO 3
    FOR j as integer = 0 TO 23
     IF spell(retvals(0), i, j) = retvals(1) THEN
      scriptret = 1
      EXIT FOR
     END IF
    NEXT j
   NEXT i
  END IF
 CASE 132'--can learn spell
  scriptret = 0
  DIM partyslot as integer
  DIM heroID as integer
  partyslot = bound(retvals(0), 0, 40)
  heroID = gam.hero(partyslot).id
  IF heroID = -1 THEN
   scripterr "can learn spell: fail on empty party slot " & partyslot, serrBound
  ELSE
   IF retvals(1) > 0 THEN
    DIM her as herodef
    loadherodata her, heroID
    FOR i as integer = 0 TO 3
     FOR j as integer = 0 TO 23
      IF spell(partyslot, i, j) = 0 THEN
       IF her.spell_lists(i,j).attack = retvals(1) AND her.spell_lists(i,j).learned = retvals(2) THEN
        scriptret = 1
        EXIT FOR
       END IF
      END IF
     NEXT j
    NEXT i
   END IF
  END IF
 CASE 133'--hero by slot
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = gam.hero(retvals(0)).id
  ELSE
   scriptret = -1
  END IF
 CASE 134'--hero by rank
  scriptret = herobyrank(retvals(0))
 CASE 145'--pick hero
  scriptret = onwho(readglobalstring(135, "Which Hero?", 20), 1)
 CASE 146'--rename hero by slot
  IF valid_hero_party(retvals(0)) THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    renamehero retvals(0), YES
   END IF
  END IF
 CASE 171'--saveslotused
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF save_slot_used(retvals(0) - 1) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 172'--importglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF retvals(1) = -1 THEN 'importglobals(slot)
    retvals(1) = 0
    retvals(2) = maxScriptGlobals
   END IF
   IF retvals(1) >= 0 AND retvals(1) <= maxScriptGlobals THEN
    IF retvals(2) = -1 THEN 'importglobals(slot,id)
     DIM remval as integer = global(retvals(1))
     loadglobalvars retvals(0) - 1, retvals(1), retvals(1)
     scriptret = global(retvals(1))
     global(retvals(1)) = remval
    ELSE                    'importglobals(slot,first,last)
     IF retvals(2) <= maxScriptGlobals AND retvals(1) <= retvals(2) THEN
      loadglobalvars retvals(0) - 1, retvals(1), retvals(2)
     END IF
    END IF
   END IF
  END IF
 CASE 173'--exportglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 AND retvals(1) >= 0 AND retvals(2) <= maxScriptGlobals AND retvals(1) <= retvals(2) THEN
   saveglobalvars retvals(0) - 1, retvals(1), retvals(2)
  END IF
 CASE 175'--deletesave
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   erase_save_slot retvals(0) - 1
  END IF
 CASE 176'--run script by id
  DIM rsr as integer
  DIM argc as integer = curcmd->argc  'Must store before calling runscript
  rsr = runscript(retvals(0), NO, NO, "indirect", plottrigger) 'possible to get ahold of triggers
  IF rsr = 1 THEN
   '--fill heap with arguments
   FOR i as integer = 1 TO argc - 1  'flexible argument number!
    setScriptArg i - 1, retvals(i)
   NEXT i
   'NOTE: scriptret is not set here when this command is successful. The return value of the called script will be returned.
  ELSE
   scripterr "run script by id failed loading " & retvals(0), serrError
   scriptret = -1
  END IF
 CASE 180'--mapwidth([map])
  'map width did not originally have an argument
  IF curcmd->argc = 0 ORELSE retvals(0) = -1 ORELSE retvals(0) = gam.map.id THEN
   scriptret = mapsizetiles.x
  ELSE
   IF bound_arg(retvals(0), 0, gen(genMaxMap), "map number", , , serrBadOp) THEN
    DIM as TilemapInfo mapsize
    GetTilemapInfo maplumpname(retvals(0), "t"), mapsize
    scriptret = mapsize.wide
   END IF
  END IF
 CASE 181'--mapheight([map])
  'map height did not originally have an argument
  IF curcmd->argc = 0 ORELSE retvals(0) = -1 ORELSE retvals(0) = gam.map.id THEN
   scriptret = mapsizetiles.y
  ELSE
   IF bound_arg(retvals(0), 0, gen(genMaxMap), "map number", , , serrBadOp) THEN
    DIM as TilemapInfo mapsize
    GetTilemapInfo maplumpname(retvals(0), "t"), mapsize
    scriptret = mapsize.high
   END IF
  END IF
 CASE 187'--getmusicvolume
  scriptret = get_music_volume * 255
 CASE 188'--setmusicvolume
  set_music_volume bound(retvals(0), 0, 255) / 255
 CASE 189, 307'--get formation song
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   scriptret = form.music
   IF id = 189 THEN scriptret += 1
  END IF
 CASE 190'--set formation song
  'set formation song never worked, so don't bother with backwards compatibility
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) AND retvals(1) >= -2 AND retvals(1) <= gen(genMaxSong) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   form.music = retvals(1)
   SaveFormation form, retvals(0)
  ELSE
   scriptret = -1
  END IF
 CASE 191'--hero frame
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = herow(retvals(0)).wtog \ 2
  END IF
 CASE 195'--load sound (BACKWARDS COMPATABILITY HACK )
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF retvals(0) >= 0 AND retvals(0) <= 7 THEN
   backcompat_sound_slot_mode = -1
   backcompat_sound_slots(retvals(0)) = retvals(1) + 1
  END IF
 CASE 196'--free sound (BACKWARDS COMPATABILITY HACK)
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF retvals(0) >= 0 AND retvals(0) <= 7 THEN
   backcompat_sound_slots(retvals(0)) = 0
  END IF
 CASE 197'--play sound
  DIM sfxid as integer = backcompat_sound_id(retvals(0))
  IF sfxid >= 0 AND sfxid <= gen(genMaxSFX) THEN
   if retvals(2) then stopsfx sfxid
   playsfx sfxid, retvals(1)
   scriptret = -1
  END IF
 CASE 198'--pause sound
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxSFX) THEN
   pausesfx retvals(0)
   scriptret = -1
  END IF
 CASE 199'--stop sound
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxSFX) THEN
   stopsfx retvals(0)
   scriptret = -1
  END IF
 CASE 200'--system hour (time is always hh:mm:ss)
  scriptret = str2int(MID(TIME, 1, 2))
 CASE 201'--system minute
  scriptret = str2int(MID(TIME, 4, 2))
 CASE 202'--system second
  scriptret = str2int(MID(TIME, 7, 2))
 CASE 203'--current song
  IF gam.music_change_delay > 0 THEN
   'If a music change is queued pretend it has already happened, to avoid confusion and
   'problems (including compatability) in map autorun scripts
   scriptret = gam.delayed_music
  ELSE
   scriptret = presentsong
  END IF
 CASE 204'--get hero name(str,her)
  IF valid_plotstr(retvals(0)) AND valid_hero_party(retvals(1)) THEN
   plotstr(retvals(0)).s = gam.hero(retvals(1)).name
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 205'--set hero name
  IF valid_plotstr(retvals(0)) AND valid_hero_party(retvals(1)) THEN
   gam.hero(retvals(1)).name = plotstr(retvals(0)).s
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 206'--get item name(str,itm)
  scriptret = 0
  IF valid_plotstr(retvals(0)) THEN
   IF valid_item(retvals(1)) THEN
    plotstr(retvals(0)).s = readitemname(retvals(1))
    scriptret = 1
   END IF
  END IF
 CASE 207'--get map name(str,map)
   IF valid_plotstr(retvals(0)) = NO OR retvals(1) < 0 OR retvals(1) > gen(genMaxMap) THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = getmapname(retvals(1))
   scriptret = 1
  END IF
 CASE 208'--get attack name(str,atk)
  'WARNING: backcompat only. new games should prefer read attack name
  IF valid_plotstr(retvals(0)) = NO OR retvals(1) < 0 OR retvals(1) > gen(genMaxAttack) THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = readattackname(retvals(1) + 1)
   scriptret = 1
  END IF
 CASE 209'--get global string(str,glo)
  'This command is basically unusable without a table of constants, it has almost certainly never been used.
  'Maybe someday it will be replaced - we can't add 'setglobalstring' unless the length is encoded in the offset constant.
  IF valid_plotstr(retvals(0)) = NO OR retvals(1) < 0 OR retvals(1) > 309 THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = readglobalstring(retvals(1), "", 255)
   scriptret = 1
  END IF
 CASE 211'--clear string
  IF valid_plotstr(retvals(0)) THEN plotstr(retvals(0)).s = ""
 CASE 212'--append ascii
  IF valid_plotstr(retvals(0)) THEN
   IF retvals(1) >= 0 AND retvals(1) <= 255 THEN
    plotstr(retvals(0)).s = plotstr(retvals(0)).s + CHR(retvals(1))
    scriptret = LEN(plotstr(retvals(0)).s)
   END IF
  END IF
 CASE 213'--append number
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).s = plotstr(retvals(0)).s & retvals(1)
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 214'--copy string
  IF valid_plotstr(retvals(0)) AND valid_plotstr(retvals(1)) THEN
   plotstr(retvals(0)).s = plotstr(retvals(1)).s
  END IF
 CASE 215'--concatenate strings
  IF valid_plotstr(retvals(0)) AND valid_plotstr(retvals(1)) THEN
   plotstr(retvals(0)).s = plotstr(retvals(0)).s + plotstr(retvals(1)).s
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 216'--string length
  IF valid_plotstr(retvals(0)) THEN
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 217'--delete char
  IF valid_plotstr(retvals(0)) THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    DIM beforestr as string = LEFT(plotstr(retvals(0)).s, retvals(1) - 1)
    DIM afterstr as string = MID(plotstr(retvals(0)).s, retvals(1) + 1)
    plotstr(retvals(0)).s = beforestr & afterstr
   END IF
  END IF
 CASE 218'--replace char
  IF valid_plotstr(retvals(0)) AND retvals(2) >= 0 AND retvals(2) <= 255 THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    MID(plotstr(retvals(0)).s, retvals(1), 1) = CHR(retvals(2))
   END IF
  END IF
 CASE 219'--ascii from string
  IF valid_plotstr(retvals(0)) AND retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
   scriptret = plotstr(retvals(0)).s[retvals(1)-1]'you can index strings a la C
  END IF
 CASE 220'--position string
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).X = retvals(1)
   plotstr(retvals(0)).Y = retvals(2)
  END IF
 CASE 221'--set string bit
  IF valid_plotstr(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   if retvals(2) then
    plotstr(retvals(0)).bits = plotstr(retvals(0)).bits or 2 ^ retvals(1)
   else
    plotstr(retvals(0)).bits = plotstr(retvals(0)).bits and not 2 ^ retvals(1)
   end if
  END IF
 CASE 222'--get string bit
  IF valid_plotstr(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   'scriptret = readbit(plotstrBits(), retvals(0), retvals(1))
   scriptret = plotstr(retvals(0)).bits AND 2 ^ retvals(1)
   IF scriptret THEN scriptret = 1
  END IF
 CASE 223'--string color
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).Col = bound(retvals(1), -1, 255)  'Allow -1 for default
   plotstr(retvals(0)).BGCol = bound(retvals(2), 0, 255)
  END IF
 CASE 224'--string X
  IF valid_plotstr(retvals(0)) THEN
   scriptret = plotstr(retvals(0)).X
  END IF
 CASE 225'--string Y
  IF valid_plotstr(retvals(0)) THEN
   scriptret = plotstr(retvals(0)).Y
  END IF
 CASE 226'--system day (date is always mm-dd-yyyy)
  scriptret = str2int(MID(DATE, 4, 2))
 CASE 227'--system month
  scriptret = str2int(MID(DATE, 1, 2))
 CASE 228'--system year
  scriptret = str2int(MID(DATE, 7, 4))
 CASE 229'--string compare
  IF valid_plotstr(retvals(0)) AND valid_plotstr(retvals(1)) THEN
   scriptret = IIF(plotstr(retvals(0)).s = plotstr(retvals(1)).s, 1, 0)
  END IF
 CASE 230'--read enemy data
  'Boy, was this command a bad idea!
  '106 was the largest used offset until very recently, so we'll limit it there to
  'prevent further damage
  'Note: elemental/enemytype bits no longer exist (should still be able to read them
  'from old games, though)
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy ID") AND bound_arg(retvals(1), 0, 106, "data index", , , serrBadOp) THEN
   scriptret = ReadShort(tmpdir & "dt1.tmp", retvals(0) * getbinsize(binDT1) + retvals(1) * 2 + 1)
  END IF
 CASE 231'--write enemy data
  'Boy, was this command a bad idea!
  '106 was the largest used offset until very recently, so we'll limit it there to
  'prevent further damage
  'Note: writing elemental/enemytype bits no longer works
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy ID") AND bound_arg(retvals(1), 0, 106, "data index", , , serrBadOp) THEN
   'Show an error if out of range, but be lenient and continue anyway, capping
   'stats (and other data...) to 32767
   bound_arg(retvals(2), -32768, 32767, "value")
   retvals(2) = bound(retvals(2), -32768, 32767)
   WriteShort(tmpdir & "dt1.tmp", retvals(0) * getbinsize(binDT1) + retvals(1) * 2 + 1, retvals(2))
  END IF
 CASE 232'--trace
  IF valid_plotstr(retvals(0)) THEN
   debug "TRACE: " + plotstr(retvals(0)).s
  END IF
 CASE 233'--get song name
  IF valid_plotstr(retvals(0)) AND retvals(1) >= 0 THEN
   plotstr(retvals(0)).s = getsongname(retvals(1))
  END IF
 CASE 235'--key is pressed
  'Undocumented second argument is joystick number
  IF script_keyval(retvals(0), bound(retvals(1), 0, 7)) THEN scriptret = 1 ELSE scriptret = 0
 CASE 236'--sound is playing
  DIM sfxid as integer = backcompat_sound_id(retvals(0))
  IF sfxid >= 0 AND sfxid <= gen(genMaxSFX) THEN
   scriptret = sfxisplaying(sfxid)
  END IF
 CASE 237'--sound slots (BACKWARDS COMPATABILITY HACK)
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF backcompat_sound_slot_mode THEN
    scriptret = 8
  END IF
 CASE 238'--search string
  IF valid_plotstr(retvals(0)) AND valid_plotstr(retvals(1)) THEN
    WITH plotstr(retvals(0))
     scriptret = instr(bound(retvals(2), 1, LEN(.s)), .s, plotstr(retvals(1)).s)
    END WITH
  ELSE
   scriptret = 0
  END IF
 CASE 239'--trim string
  IF valid_plotstr(retvals(0)) THEN
   IF retvals(1) = -1 THEN
    plotstr(retvals(0)).s = trim(plotstr(retvals(0)).s)
   ELSE
    IF retvals(1) <= LEN(plotstr(retvals(0)).s) AND retvals(2) >= 1 THEN
     retvals(1) = large(retvals(1),1)
     'retvals(2) = bound(retvals(2),1,LEN(plotstr(retvals(0)).s))
     plotstr(retvals(0)).s = MID(plotstr(retvals(0)).s,retvals(1),retvals(2))
    ELSE
     plotstr(retvals(0)).s = ""
    END IF
   END IF
  END IF
 CASE 240'-- string from textbox (string, box, line, ignored)
  IF valid_plotstr(retvals(0)) THEN
   DIM box as TextBox
   retvals(1) = bound(retvals(1),0,gen(genMaxTextbox))
   retvals(2) = bound(retvals(2),0,7)
   LoadTextBox box, retvals(1)
   plotstr(retvals(0)).s = trim(box.text(retvals(2)))
   embedtext plotstr(retvals(0)).s
  END IF
 CASE 241'-- expand string(id)
  IF valid_plotstr(retvals(0)) THEN
   embedtext plotstr(retvals(0)).s
  END IF
 CASE 242'-- joystick button
  retvals(0) = bound(retvals(0)-1,0,15)
  retvals(1) = bound(retvals(1),0,7)
  DIM b as integer
  IF readjoy(retvals(1),b,0,0) THEN
   scriptret = (b SHR retvals(0)) AND 1
  ELSE
   scriptret = 0
  END IF
 CASE 243'-- joystick axis
  retvals(0) = bound(retvals(0),0,1)
  retvals(2) = bound(retvals(2),0,7)
  DIM as integer xaxis, yaxis
  IF readjoy(retvals(2), 0, xaxis, yaxis) THEN
   IF retvals(0) = 0 THEN  'x axis
    'debug "x " & xaxis
    scriptret = int((xaxis / 100) * retvals(1)) 'normally, xaxis * 100
   ELSEIF retvals(0) = 1 THEN  'y axis
    'debug "y " & yaxis
    scriptret = int((yaxis / 100) * retvals(1)) 'normally, yaxis * 100
   END IF
  ELSE
   'debug "joystick failed"
   scriptret = 0
  END IF
 CASE 244'--wait for scancode
  script_start_waiting(retvals(0))
 CASE 249'--party money
  scriptret = gold
 CASE 250'--set money
  IF retvals(0) >= 0 THEN gold = retvals(0)
 CASE 251'--set string from table
  IF bound_arg(retvals(0), 0, UBOUND(plotstr), "string ID", !"$# = \"...\"") THEN
   WITH *scriptinsts(nowscript).scr
    DIM stringp as integer ptr = .ptr + .strtable + retvals(1)
    IF .strtable + retvals(1) >= .size ORELSE .strtable + (stringp[0] + 3) \ 4 >= .size THEN
     scripterr "script corrupt: illegal string offset", serrError
    ELSE
     plotstr(retvals(0)).s = read32bitstring(stringp)
     scriptret = retvals(0)
    END IF
   END WITH
  END IF
 CASE 252'--append string from table
  IF bound_arg(retvals(0), 0, UBOUND(plotstr), "string ID", !"$# + \"...\"") THEN
   WITH *scriptinsts(nowscript).scr
    DIM stringp as integer ptr = .ptr + .strtable + retvals(1)
    IF .strtable + retvals(1) >= .size ORELSE .strtable + (stringp[0] + 3) \ 4 >= .size THEN
     scripterr "script corrupt: illegal string offset", serrError
    ELSE
     plotstr(retvals(0)).s += read32bitstring(stringp)
     scriptret = retvals(0)
    END IF
   END WITH
  END IF
 CASE 256'--suspend map music
  setbit gen(), genSuspendBits, suspendambientmusic, 1
 CASE 257'--resume map music
  setbit gen(), genSuspendBits, suspendambientmusic, 0
 CASE 260'--set timer(id, count, speed, trigger, string, flags)
  'All args except id default to -1 (default)
  IF bound_arg(retvals(0), 0, UBOUND(timers), "timer ID") THEN
    WITH timers(retvals(0))
      IF retvals(1) > -1 THEN .count = retvals(1): .ticks = 0
      IF retvals(2) > -1 THEN
        .speed = retvals(2)
      ELSEIF retvals(2) = -1 AND .speed = 0 THEN
        .speed = 18
      END IF
      IF retvals(3) <> -1 THEN .trigger = retvals(3)
      IF retvals(4) <> -1 THEN
       IF valid_plotstr(retvals(4)) THEN .st = retvals(4) + 1
      END IF
      IF .st > 0 THEN plotstr(.st - 1).s = seconds2str(.count)
      IF retvals(5) <> -1 THEN .flags = retvals(5)
      IF .speed < -1 THEN .speed *= -1: .speed -= 1
    END WITH
  END IF
 CASE 261'--stop timer
  IF bound_arg(retvals(0), 0, UBOUND(timers), "timer ID") THEN
   timers(retvals(0)).speed = 0
  END IF
 CASE 262'--read timer
  IF bound_arg(retvals(0), 0, UBOUND(timers), "timer ID") THEN
   scriptret = timers(retvals(0)).count
  END IF
 CASE 263'--get color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   scriptret = master(retvals(0)).col
  END IF
 CASE 264'--set color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   retvals(1) = retvals(1) OR &HFF000000 'just in case, set the alpha
   master(retvals(0)).col = retvals(1)
  END IF
 CASE 265'--rgb
  scriptret = RGB(bound(retvals(0),0,255), bound(retvals(1),0,255), bound(retvals(2),0,255))
 CASE 266'--extractcolor
  dim c as rgbcolor
  c.col = retvals(0)
  SELECT CASE retvals(1)
   CASE 0
    scriptret = c.r
   CASE 1
    scriptret = c.g
   CASE 2
    scriptret = c.b
  END SELECT
 CASE 268'--load palette
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxMasterPal) THEN
   loadpalette master(), retvals(0)
   LoadUIColors uilook(), boxlook(), retvals(0)
  END IF
 CASE 273'--milliseconds
  scriptret = fmod((TIMER * 1000) + 2147483648.0, 4294967296.0) - 2147483648.0
 CASE 308'--add enemy to formation (formation, enemy id, x, y, slot = -1)
  scriptret = -1
  IF valid_formation(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= gen(genMaxEnemy) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   DIM slot as integer = -1
   FOR i as integer = 0 TO 7
    IF form.slots(i).id = -1 THEN slot = i: EXIT FOR
   NEXT
   IF retvals(4) >= 0 AND retvals(4) <= 7 THEN
    IF form.slots(retvals(4)).id = -1 THEN slot = retvals(4)
   END IF
   IF slot >= 0 THEN
    DIM szindex as integer = ReadShort(tmpdir & "dt1.tmp", retvals(1) * getbinsize(binDT1) + 111) 'picture size
    DIM size as integer
    IF szindex = 0 THEN size = 34
    IF szindex = 1 THEN size = 50
    IF szindex = 2 THEN size = 80
    WITH form.slots(slot)
     .id = retvals(1)
     .pos.x = large( (small(retvals(2), 230) - size \ 2) , 0)  'approximately the 0 - 250 limit of the formation editor
     .pos.y = large( (small(retvals(3), 199) - size) , 0)
    END WITH
   END IF
   SaveFormation form, retvals(0)
   scriptret = slot
  END IF
 CASE 309'--find enemy in formation (formation, enemy id, number)
  IF valid_formation(retvals(0)) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   DIM slot as integer = 0
   scriptret = -1
   FOR i as integer = 0 TO 7
    IF form.slots(i).id >= 0 AND (retvals(1) = form.slots(i).id OR retvals(1) = -1) THEN
     IF retvals(2) = slot THEN scriptret = i: EXIT FOR
     slot += 1
    END IF
   NEXT
   IF retvals(2) = -1 THEN scriptret = slot
  END IF
 CASE 310'--delete enemy from formation (formation, slot)
  IF valid_formation_slot(retvals(0), retvals(1)) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   form.slots(retvals(1)).id = -1
   SaveFormation form, retvals(0)
  END IF
 CASE 311'--formation slot enemy (formation, slot)
  scriptret = -1
  IF valid_formation_slot(retvals(0), retvals(1)) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   scriptret = form.slots(retvals(1)).id
  END IF
 CASE 312, 313'--formation slot x (formation, slot), formation slot y (formation, slot)
  IF valid_formation_slot(retvals(0), retvals(1)) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   DIM enemy_id as integer = form.slots(retvals(1)).id
   scriptret = form.slots(retvals(1)).pos.n(id - 312)
   'now find the position of the bottom center of the enemy sprite
   IF enemy_id >= 0 THEN
    DIM pictype as integer = ReadShort(tmpdir & "dt1.tmp", enemy_id * getbinsize(binDT1) + 111) 'picture size
    DIM picsize as integer
    IF pictype = 0 THEN picsize = 34
    IF pictype = 1 THEN picsize = 50
    IF pictype = 2 THEN picsize = 80
    IF id = 312 THEN scriptret += picsize \ 2 ELSE scriptret += picsize
   END IF
  END IF
 CASE 314'--set formation background (formation, background, animation frames, animation ticks)
  IF valid_formation(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= gen(genNumBackdrops) - 1 THEN 
   DIM form as Formation
   LoadFormation form, retvals(0)
   form.background = retvals(1)
   form.background_frames = bound(retvals(2), 1, 50)
   form.background_ticks = bound(retvals(3), 0, 1000)
   SaveFormation form, retvals(0)
  END IF
 CASE 315'--get formation background (formation)
  IF valid_formation(retvals(0)) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   scriptret = form.background
  END IF
 CASE 316'--last formation
  scriptret = lastformation
 CASE 317'--random formation (formation set)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   scriptret = random_formation(retvals(0))
  END IF
 CASE 318'--formation set frequency (formation set)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   DIM formset as FormationSet
   LoadFormationSet formset, retvals(0)
   scriptret = formset.frequency
  END IF
 CASE 319'--formation probability (formation set, formation)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   DIM formset as FormationSet
   LoadFormationSet formset, retvals(0)
   DIM slot as integer = 0
   scriptret = 0
   FOR i as integer = 0 TO UBOUND(formset.formations)
    IF formset.formations(i) = retvals(1) THEN scriptret += 1
    IF formset.formations(i) >= 0 THEN slot += 1
   NEXT
   'probability in percentage points
   IF slot > 0 THEN scriptret = (scriptret * 100) / slot
  END IF
 CASE 321'--get hero speed (hero)
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = herow(retvals(0)).speed
  END IF
 CASE 322'--load hero sprite
  scriptret = load_sprite_plotslice(0, retvals(0), retvals(1))
 CASE 323'--free sprite
  IF valid_plotslice(retvals(0), 2) THEN
   IF plotslices(retvals(0))->SliceType = slSprite THEN
    DeleteSlice @plotslices(retvals(0))
   ELSE
    scripterr "free sprite: slice " & retvals(0) & " is a " & SliceTypeName(plotslices(retvals(0))), serrBadOp
   END IF
  END IF
 CASE 324 '--put slice  (previously place sprite)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .x = retvals(1)
    .y = retvals(2)
   END WITH
  END IF
 CASE 326 '--set sprite palette
  IF valid_plotslice(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , ,retvals(1)
  END IF
 CASE 327 '--replace hero sprite
  replace_sprite_plotslice retvals(0), 0, retvals(1), retvals(2)
 CASE 328 '--set sprite frame
  IF valid_plotslice(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , , , retvals(1)
  END IF
 CASE 558'--set sprite set number
  IF valid_plotslice(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , retvals(1)
  END IF
 CASE 329'--load walkabout sprite
  scriptret = load_sprite_plotslice(4, retvals(0), retvals(1))
 CASE 330 '--replace walkabout sprite
  replace_sprite_plotslice retvals(0), 4, retvals(1), retvals(2)
 CASE 331'--load weapon sprite
  scriptret = load_sprite_plotslice(5, retvals(0), retvals(1))
 CASE 332 '--replace weapon sprite
  replace_sprite_plotslice retvals(0), 5, retvals(1), retvals(2)
 CASE 333'--load small enemy sprite
  scriptret = load_sprite_plotslice(1, retvals(0), retvals(1))
 CASE 334 '--replace small enemy sprite
  replace_sprite_plotslice retvals(0), 1, retvals(1), retvals(2)
 CASE 335'--load medium enemy sprite
  scriptret = load_sprite_plotslice(2, retvals(0), retvals(1))
 CASE 336 '--replace medium enemy sprite
  replace_sprite_plotslice retvals(0), 2, retvals(1), retvals(2)
 CASE 337'--load large enemy sprite
  scriptret = load_sprite_plotslice(3, retvals(0), retvals(1))
 CASE 338 '--replace large enemy sprite
  replace_sprite_plotslice retvals(0), 3, retvals(1), retvals(2)
 CASE 339'--load attack sprite
  scriptret = load_sprite_plotslice(6, retvals(0), retvals(1))
 CASE 340 '--replace attack sprite
  replace_sprite_plotslice retvals(0), 6, retvals(1), retvals(2)
 CASE 341'--load border sprite
  scriptret = load_sprite_plotslice(7, retvals(0), retvals(1))
 CASE 342 '--replace border sprite
  replace_sprite_plotslice retvals(0), 7, retvals(1), retvals(2)
 CASE 343'--load portrait sprite
  scriptret = load_sprite_plotslice(8, retvals(0), retvals(1))
 CASE 344 '--replace portrait sprite
  replace_sprite_plotslice retvals(0), 8, retvals(1), retvals(2)
 CASE 345 '--clone sprite
  IF valid_plotsprite(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = NewSliceOfType(slSprite, SliceTable.scriptsprite)
   sl->Clone(plotslices(retvals(0)), sl)
   scriptret = create_plotslice_handle(sl)
  END IF
 CASE 346 '--get sprite frame
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->frame
  END IF
 CASE 347 '--sprite frame count
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   WITH *dat
    scriptret = sprite_sizes(.spritetype).frames
   END WITH
  END IF
 CASE 348 '--slice x
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->X
  END IF
 CASE 349 '--slice y
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->Y
  END IF
 CASE 350 '--set slice x
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->X = retvals(1)
  END IF
 CASE 351 '--set slice y
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->Y = retvals(1)
  END IF
 CASE 352 '--slice width
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->Width
  END IF
 CASE 353 '--slice height
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->Height
  END IF
 CASE 354 '--set horiz align
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , , serrBadOp) THEN
    plotslices(retvals(0))->AlignHoriz = retvals(1)
   END IF
  END IF
 CASE 355 '--set vert align
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , , serrBadOp) THEN
    plotslices(retvals(0))->AlignVert = retvals(1)
   END IF
  END IF
 CASE 356 '--set horiz anchor
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , , serrBadOp) THEN
    plotslices(retvals(0))->AnchorHoriz = retvals(1)
   END IF
  END IF
 CASE 357 '--set vert anchor
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , , serrBadOp) THEN
    plotslices(retvals(0))->AnchorVert = retvals(1)
   END IF
  END IF
 CASE 358 '--number from string
  IF valid_plotstr(retvals(0)) THEN
   scriptret = str2int(plotstr(retvals(0)).s, retvals(1))
  END IF
 CASE 359 '--slice is sprite
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slSprite THEN scriptret = 1
  END IF
 CASE 360 '--sprite layer
  scriptret = find_plotslice_handle(SliceTable.ScriptSprite)
 CASE 361 '--free slice
  IF valid_plotslice(retvals(0), 2) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   IF sl->Protect THEN
    scripterr "free slice: cannot free protected " & SliceTypeName(sl) & " slice " & retvals(0), serrBadOp
   ELSE
    DeleteSlice @plotslices(retvals(0))
   END IF
  END IF
 CASE 362 '--first child
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   scriptret = find_plotslice_handle(sl->FirstChild)
  END IF
 CASE 363 '--next sibling
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   scriptret = find_plotslice_handle(sl->NextSibling)
  END IF
 CASE 364 '--create container
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slContainer, SliceTable.scriptsprite)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  scriptret = create_plotslice_handle(sl)
 CASE 365 '--set parent
  IF valid_plotslice(retvals(0)) AND valid_plotslice(retvals(1)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   IF sl->Protect THEN
    scripterr "set parent: cannot reparent protected " & SliceTypeName(sl) & " slice " & retvals(0), serrBadOp
   ELSE
    SetSliceParent sl, plotslices(retvals(1))
   END IF
  END IF
 CASE 366 '--check parentage
  IF valid_plotslice(retvals(0)) AND valid_plotslice(retvals(1)) THEN
   IF verifySliceLineage(plotslices(retvals(0)), plotslices(retvals(1))) THEN
    scriptret = 1
   END IF
  END IF
 CASE 367 '--slice screen x
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = sl->ScreenX + SliceXAnchor(sl)
  END IF
 CASE 368 '--slice screen y
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = sl->ScreenY + SliceYAnchor(sl)
  END IF
 CASE 369 '--slice is container
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slContainer THEN scriptret = 1
  END IF
 CASE 370 '--create rect
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slRectangle, SliceTable.scriptsprite)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  IF bound_arg(retvals(2), -1, 14, "style") THEN
   ChangeRectangleSlice sl, retvals(2)
  END IF
  scriptret = create_plotslice_handle(sl)
 CASE 371 '--slice is rect
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slRectangle THEN scriptret = 1
  END IF
 CASE 372 '--set slice width
  IF valid_resizeable_slice(retvals(0)) THEN
   plotslices(retvals(0))->Width = retvals(1)
  END IF
 CASE 373 '--set slice height
  IF valid_resizeable_slice(retvals(0)) THEN
   plotslices(retvals(0))->Height = retvals(1)
  END IF
 CASE 374 '--get rect style
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->style
  END IF
 CASE 375 '--set rect style
  IF bound_arg(retvals(1), -1, 14, "style") THEN
   change_rect_plotslice retvals(0), retvals(1)
  END IF
 CASE 376 '--get rect fgcol
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->fgcol
  END IF
 CASE 377 '--set rect fgcol
  IF bound_arg(retvals(1), 0, 255, "fgcol") THEN
   change_rect_plotslice retvals(0), , ,retvals(1)
  END IF
 CASE 378 '--get rect bgcol
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->bgcol
  END IF
 CASE 379 '--set rect bgcol
  IF bound_arg(retvals(1), 0, 255, "bgcol") THEN
   change_rect_plotslice retvals(0), ,retvals(1)
  END IF
 CASE 380 '--get rect border
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->border
  END IF
 CASE 381 '--set rect border
  IF bound_arg(retvals(1), -2, 14, "border") THEN
   change_rect_plotslice retvals(0), , , ,retvals(1)
  END IF
 CASE 382 '--get rect trans
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->translucent
  END IF
 CASE 383 '--set rect trans
  IF bound_arg(retvals(1), 0, 2, "transparency") THEN
   change_rect_plotslice retvals(0), , , , ,retvals(1)
  END IF
 CASE 384 '--slice collide point
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = ABS(SliceCollidePoint(sl, retvals(1), retvals(2)))
  END IF
 CASE 385 '--slice collide
  IF valid_plotslice(retvals(0)) THEN
   IF valid_plotslice(retvals(1)) THEN
    RefreshSliceScreenPos plotslices(retvals(0))
    RefreshSliceScreenPos plotslices(retvals(1))
    scriptret = ABS(SliceCollide(plotslices(retvals(0)), plotslices(retvals(1))))
   END IF
  END IF
 CASE 386 '--slice contains
  IF valid_plotslice(retvals(0)) THEN
   IF valid_plotslice(retvals(1)) THEN
    scriptret = ABS(SliceContains(plotslices(retvals(0)), plotslices(retvals(1))))
   END IF
  END IF
 CASE 387 '--clamp slice
  IF valid_plotslice(retvals(0)) THEN
   IF valid_plotslice(retvals(1)) THEN
    SliceClamp plotslices(retvals(1)), plotslices(retvals(0))
   END IF
  END IF
 CASE 388 '--horiz flip sprite
  IF valid_plotsprite(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , , , , retvals(1)
  END IF
 CASE 389 '--vert flip sprite
  IF valid_plotsprite(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , , , , , retvals(1)
  END IF
 CASE 390 '--sprite is horiz flipped
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->flipHoriz THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 391 '--sprite is vert flipped
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->flipVert THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 392 '--set top padding
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->PaddingTop = retvals(1)
  END IF
 CASE 393 '--get top padding
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->PaddingTop
  END IF
 CASE 394 '--set left padding
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->PaddingLeft = retvals(1)
  END IF
 CASE 395 '--get left padding
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->PaddingLeft
  END IF
 CASE 396 '--set bottom padding
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->PaddingBottom = retvals(1)
  END IF
 CASE 397 '--get bottom padding
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->PaddingBottom
  END IF
 CASE 398 '--set right padding
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->PaddingRight = retvals(1)
  END IF
 CASE 399 '--get right padding
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->PaddingRight
  END IF
 CASE 400 '--fill parent
  IF valid_resizeable_slice(retvals(0), YES) THEN
   plotslices(retvals(0))->Fill = (retvals(1) <> 0)
  END IF
 CASE 401 '--is filling parent
  IF valid_plotslice(retvals(0)) THEN
   IF plotslices(retvals(0))->Fill THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 402 '--slice to front
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))->Parent
   SetSliceParent plotslices(retvals(0)), sl
  END IF
 CASE 403 '--slice to back
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   IF sl->Parent = 0 THEN
    scripterr "slice to back: invalid on root slice", serrBadOp
   ELSE
    InsertSliceBefore sl->Parent->FirstChild, sl
   END IF
  END IF
 CASE 404 '--last child
  IF valid_plotslice(retvals(0)) THEN
   scriptret = find_plotslice_handle(LastChild(plotslices(retvals(0))))
  END IF
 CASE 405 '--y sort children
  IF valid_plotslice(retvals(0)) THEN
   YSortChildSlices plotslices(retvals(0))
  END IF
 CASE 406 '--set sort order
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->Sorter = retvals(1)
  END IF
 CASE 407 '--sort children
  IF valid_plotslice(retvals(0)) THEN
   CustomSortChildSlices plotslices(retvals(0)), retvals(1)
  END IF
 CASE 408 '--previous sibling
  IF valid_plotslice(retvals(0)) THEN
   scriptret = find_plotslice_handle(plotslices(retvals(0))->PrevSibling)
  END IF 
 CASE 409 '--get sort order
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->Sorter
  END IF
 CASE 410 '--get slice extra (handle, extra)
  IF valid_plotslice(retvals(0)) THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    scriptret = plotslices(retvals(0))->Extra(retvals(1))
   END IF
  END IF
 CASE 411 '--set slice extra (handle, extra, val)
  IF valid_plotslice(retvals(0)) THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    plotslices(retvals(0))->Extra(retvals(1)) = retvals(2)
   END IF
  END IF
 CASE 412 '--get sprite type
  IF valid_plotslice(retvals(0)) THEN
   IF plotslices(retvals(0))->SliceType = slSprite THEN
    DIM dat as SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
    scriptret = dat->spritetype
   ELSE
    scriptret = -1
   END IF
  END IF
 CASE 413 '--get sprite set number
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
   scriptret = dat->record
  END IF 
 CASE 414 '--get sprite palette
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
   IF dat->paletted = NO THEN
    scripterr "get sprite palette: this sprite is unpaletted", serrWarn
   ELSE
    scriptret = dat->pal
   END IF
  END IF 
 CASE 415 '--suspend timers
  FOR i as integer = 0 TO ubound(timers)
   timers(i).pause = YES
  NEXT i
 CASE 416 '--resume timers
  FOR i as integer = 0 TO ubound(timers)
   timers(i).pause = NO
  NEXT i
 CASE 325, 417 '--set sprite visible, set slice visible
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .Visible = (retvals(1) <> 0)
   END WITH
  END IF
 CASE 418 '--get slice visible
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    scriptret = ABS(.Visible)
   END WITH
  END IF
 CASE 419 '--slice edge x
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge") THEN
    DIM sl as Slice Ptr
    sl = plotslices(retvals(0))
    scriptret = sl->X - SliceXAnchor(sl) + SliceEdgeX(sl, retvals(1))
   END IF
  END IF
 CASE 420 '--slice edge y
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge") THEN
    DIM sl as Slice Ptr
    sl = plotslices(retvals(0))
    scriptret = sl->Y - SliceYAnchor(sl) + SliceEdgeY(sl, retvals(1))
   END IF
  END IF
 CASE 421 '--create text
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slText, SliceTable.scriptsprite)
  scriptret = create_plotslice_handle(sl)
 CASE 422 '--set slice text
  IF valid_plottextslice(retvals(0)) THEN
   IF valid_plotstr(retvals(1)) THEN
    ChangeTextSlice plotslices(retvals(0)), plotstr(retvals(1)).s
   END IF
  END IF
 CASE 423 '--get text color
  IF valid_plottextslice(retvals(0)) THEN
   DIM dat as TextSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->col
  END IF
 CASE 424 '--set text color
  IF valid_plottextslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "color") THEN
    ChangeTextSlice plotslices(retvals(0)), , retvals(1)
   END IF
  END IF
 CASE 425 '--get wrap
  IF valid_plottextslice(retvals(0)) THEN
   DIM dat as TextSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = ABS(dat->wrap)
  END IF
 CASE 426 '--set wrap
  IF valid_plottextslice(retvals(0)) THEN
   ChangeTextSlice plotslices(retvals(0)), , , ,(retvals(1)<>0)
  END IF
 CASE 427 '--slice is text
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slText THEN scriptret = 1
  END IF
 CASE 428 '--get text bg
  IF valid_plottextslice(retvals(0)) THEN
   DIM dat as TextSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->bgcol
  END IF
 CASE 429 '--set text bg
  IF valid_plottextslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "color") THEN
    ChangeTextSlice plotslices(retvals(0)), , , , , retvals(1)
   END IF
  END IF
 CASE 430 '--get outline
  IF valid_plottextslice(retvals(0)) THEN
   DIM dat as TextSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = ABS(dat->outline)
  END IF
 CASE 431 '--set outline
  IF valid_plottextslice(retvals(0)) THEN
   ChangeTextSlice plotslices(retvals(0)), , ,(retvals(1)<>0)
  END IF
 CASE 433'--slice at pixel(parent, x, y, num, descend)
  IF valid_plotslice(retvals(0)) THEN
   RefreshSliceScreenPos plotslices(retvals(0))
   IF retvals(3) <= -1 THEN
    DIM slnum as integer = -1
    FindSliceAtPoint(plotslices(retvals(0)), retvals(1), retvals(2), slnum, retvals(4))
    scriptret = -slnum - 1
   ELSE
    scriptret = find_plotslice_handle(FindSliceAtPoint(plotslices(retvals(0)), retvals(1), retvals(2), retvals(3), retvals(4)))
   END IF
  END IF
 CASE 434'--find colliding slice(parent, handle, num, descend)
  IF valid_plotslice(retvals(0)) AND valid_plotslice(retvals(1)) THEN
   RefreshSliceScreenPos plotslices(retvals(0))
   RefreshSliceScreenPos plotslices(retvals(1))
   IF retvals(2) <= -1 THEN
    DIM slnum as integer = -1
    FindSliceCollision(plotslices(retvals(0)), plotslices(retvals(1)), slnum, retvals(3))
    scriptret = -slnum - 1
   ELSE
    scriptret = find_plotslice_handle(FindSliceCollision(plotslices(retvals(0)), plotslices(retvals(1)), retvals(2), retvals(3)))
   END IF
  END IF
 CASE 435'--parent slice
  IF valid_plotslice(retvals(0)) THEN
   scriptret = find_plotslice_handle(plotslices(retvals(0))->Parent)
  END IF
 CASE 436'--child count
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->NumChildren
  END IF
 CASE 437'--lookup slice
  IF retvals(1) = 0 THEN
   '--search the whole slice tree
   scriptret = find_plotslice_handle(LookupSlice(retvals(0)))
  ELSE
   '--search starting from a certain slice
   IF valid_plotslice(retvals(1)) THEN
    scriptret = find_plotslice_handle(LookupSlice(retvals(0), plotslices(retvals(1))))
   END IF
  END IF
 CASE 439'--slice is valid
  scriptret = 0
  IF retvals(0) >= LBOUND(plotslices) AND retvals(0) <= UBOUND(plotslices) THEN
   IF plotslices(retvals(0)) <> 0 THEN
    scriptret = 1
    IF ENABLE_SLICE_DEBUG THEN
     IF SliceDebugCheck(plotslices(retvals(0))) = NO THEN scriptret = 0
    END IF
   END IF
  END IF
 CASE 440'--item in slot
  IF valid_item_slot(retvals(0)) THEN
   IF inventory(retvals(0)).used = NO THEN
    scriptret = -1
   ELSE
    scriptret = inventory(retvals(0)).id
   END IF
  END IF
 CASE 441'--set item in slot
  IF valid_item_slot(retvals(0)) THEN
   IF retvals(1) = -1 THEN
    WITH inventory(retvals(0))
     .used = NO
     .id = 0
     .num = 0
    END WITH
   ELSEIF valid_item(retvals(1)) THEN
    WITH inventory(retvals(0))
     .id = retvals(1)
     IF .num < 1 OR .used = NO THEN .num = 1
     .used = YES
     DIM stacksize as integer = get_item_stack_size(.id)
     IF .num > stacksize THEN  'overflow the stack
      DIM spare as integer = .num - stacksize
      .num = stacksize
      getitem retvals(1), spare
     END IF
    END WITH
   END IF
   update_inventory_caption retvals(0)
   evalitemtags
   tag_updates
  END IF
 CASE 442'--item count in slot
  IF valid_item_slot(retvals(0)) THEN
   IF inventory(retvals(0)).used = NO THEN
    scriptret = 0
   ELSE
    scriptret = inventory(retvals(0)).num
   END IF
  END IF
 CASE 443'--set item count in slot
  IF valid_item_slot(retvals(0)) THEN
   WITH inventory(retvals(0))
    IF retvals(1) = 0 THEN
     .used = NO
     .id = 0
     .num = 0
    ELSEIF .used = NO THEN
     scripterr "set item count in slot: can't set count for empty slot " & retvals(0), serrBound
    ELSE
     DIM stacksize as integer = get_item_stack_size(.id)
     IF bound_arg(retvals(1), 1, stacksize, "item count") THEN
      .num = retvals(1)
     END IF
    END IF
   END WITH
   update_inventory_caption retvals(0)
   evalitemtags
   tag_updates
  END IF
 CASE 444 '--put sprite, place sprite
  IF valid_plotsprite(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .X = retvals(1)
    .Y = retvals(2)
   END WITH
  END IF
 CASE 446 '--move slice below
  IF valid_plotslice(retvals(0)) ANDALSO valid_plotslice(retvals(1)) THEN
   IF retvals(0) = retvals(1) THEN
    scripterr "moveslicebelow: tried to move a slice below itself", serrBadOp
   ELSE
    IF plotslices(retvals(0))->Protect ANDALSO plotslices(retvals(0))->Parent <> plotslices(retvals(1))->Parent THEN
     scripterr "moveslicebelow: tried to change the parent of a protected slice", serrBadOp
    ELSE
     InsertSliceBefore plotslices(retvals(1)), plotslices(retvals(0))
    END IF
   END IF
  END IF
 CASE 447 '--move slice above
  IF valid_plotslice(retvals(0)) ANDALSO valid_plotslice(retvals(1)) THEN
   IF retvals(0) = retvals(1) THEN
    scripterr "movesliceabove: tried to move a slice above itself", serrBadOp
   ELSE
    IF plotslices(retvals(0))->Protect ANDALSO plotslices(retvals(0))->Parent <> plotslices(retvals(1))->Parent THEN
     scripterr "movesliceabove: tried to change the parent of a protected slice", serrBadOp
    ELSE
     DIM sl as Slice Ptr = plotslices(retvals(1))
     IF sl->NextSibling THEN
      InsertSliceBefore sl->NextSibling, plotslices(retvals(0))
     ELSE
      IF sl->Parent = NULL THEN
       scripterr "movesliceabove: Root shouldn't have siblings", serrBadOp
      ELSE
       'sets as last child
       SetSliceParent plotslices(retvals(0)), sl->Parent
      END IF
     END IF
    END IF
   END IF
  END IF
 CASE 448 '--slice child
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr = plotslices(retvals(0))->FirstChild
   FOR i as integer = 0 TO retvals(1)
    IF sl = NULL THEN EXIT FOR
    IF i = retvals(1) THEN scriptret = find_plotslice_handle(sl)
    sl = sl->NextSibling
   NEXT
  END IF
 CASE 451 '--set slice clipping
  IF valid_plotslice(retvals(0)) THEN
   plotslices(retvals(0))->Clip = (retvals(1) <> 0)
  END IF
 CASE 452 '--get slice clipping
  IF valid_plotslice(retvals(0)) THEN
   scriptret = ABS(plotslices(retvals(0))->Clip <> 0)
  END IF
 CASE 453 '--create grid
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slGrid, SliceTable.scriptsprite)
  scriptret = create_plotslice_handle(sl)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  ChangeGridSlice sl, retvals(2), retvals(3)
 CASE 454 '--slice is grid
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slGrid THEN scriptret = 1
  END IF
 CASE 455 '--set grid columns
  IF valid_plotgridslice(retvals(0)) THEN
   ChangeGridSlice plotslices(retvals(0)), , retvals(1)
  END IF
 CASE 456 '--get grid columns
  IF valid_plotgridslice(retvals(0)) THEN
   DIM dat as GridSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->cols
  END IF
 CASE 457 '--set grid rows
  IF valid_plotgridslice(retvals(0)) THEN
   ChangeGridSlice plotslices(retvals(0)), retvals(1)
  END IF
 CASE 458 '--get grid rows
  IF valid_plotgridslice(retvals(0)) THEN
   DIM dat as GridSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->rows
  END IF
 CASE 459 '--show grid
  IF valid_plotgridslice(retvals(0)) THEN
   DIM dat as GridSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   dat->show = (retvals(1) <> 0)
  END IF
 CASE 460 '--grid is shown
  IF valid_plotgridslice(retvals(0)) THEN
   DIM dat as GridSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = ABS(dat->show <> 0)
  END IF
 CASE 461 '--load slice collection
  DIM sl as Slice Ptr
  IF isfile(workingdir & SLASH & "slicetree_0_" & retvals(0) & ".reld") THEN
   sl = NewSliceOfType(slContainer, SliceTable.scriptsprite)
   SliceLoadFromFile sl, workingdir & SLASH & "slicetree_0_" & retvals(0) & ".reld"
   scriptret = create_plotslice_handle(sl)
  ELSE
   scripterr current_command_name() & ": invalid slice collection id " & retvals(0), serrBadOp
   scriptret = 0
  END IF
 CASE 462 '--set slice edge x
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge") THEN
    DIM sl as Slice Ptr
    sl = plotslices(retvals(0))
    sl->X = retvals(2) + SliceXAnchor(sl) - SliceEdgeX(sl, retvals(1))
   END IF
  END IF
 CASE 463 '--slice edge y
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge") THEN
    DIM sl as Slice Ptr
    sl = plotslices(retvals(0))
    sl->Y = retvals(2) + SliceYAnchor(sl) - SliceEdgeY(sl, retvals(1))
   END IF
  END IF
 CASE 464 '--get slice lookup
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->Lookup
  END IF
 CASE 465 '--set slice lookup
  IF valid_plotslice(retvals(0)) THEN
   IF retvals(1) < 0 THEN
    scripterr current_command_name() & ": negative lookup codes are reserved, they can't be set.", serrBadOp
   ELSEIF plotslices(retvals(0))->Lookup < 0 THEN
    scripterr current_command_name() & ": can't modify the lookup code of a special slice.", serrBadOp
   ELSE
    plotslices(retvals(0))->Lookup = retvals(1)
   END IF
  END IF
 CASE 466 '--trace value internal (string, value, ...)
  DIM result as string
  FOR i as integer = 0 TO curcmd->argc - 1
   IF i MOD 2 = 0 THEN
    IF i <> 0 THEN result &= ", "
    WITH *scriptinsts(nowscript).scr
     DIM stringp as integer ptr = .ptr + .strtable + retvals(i)
     IF .strtable + retvals(i) >= .size ORELSE .strtable + (stringp[0] + 3) \ 4 >= .size THEN
      scripterr "script corrupt: illegal string offset", serrError
     ELSE
      result &= read32bitstring(stringp) & " = "
     END IF
    END WITH
   ELSE
    result &= retvals(i)
   END IF
  NEXT
  debug "TRACE: " & result
 CASE 467 '--map cure
  IF bound_arg(retvals(0), 1, gen(genMaxAttack)+1, "attack ID") THEN
   IF valid_hero_party(retvals(1)) THEN
    IF valid_hero_party(retvals(2), -1) THEN
     scriptret = ABS(outside_battle_cure(retvals(0) - 1, retvals(1), retvals(2), 0))
    END IF
   END IF
  END IF
 CASE 468 '--read attack name
  scriptret = 0
  IF valid_plotstr(retvals(0)) AND bound_arg(retvals(1), 1, gen(genMaxAttack)+1, "attack ID") THEN
   plotstr(retvals(0)).s = readattackname(retvals(1) - 1)
   scriptret = 1
  END IF
 CASE 469'--spells learned
  DIM found as integer = 0
  IF valid_hero_party(retvals(0)) THEN
   FOR i as integer = retvals(0) * 96 TO retvals(0) * 96 + 95
    IF readbit(learnmask(), 0, i) THEN
     IF retvals(1) = found THEN
      scriptret = spell(retvals(0), (i \ 24) MOD 4, i MOD 24)
      EXIT FOR
     END IF
     found = found + 1
    END IF
   NEXT
   IF retvals(1) = -1 THEN scriptret = found  'getcount
  END IF
 CASE 470'--allocate timers
  IF bound_arg(retvals(0), 0, 100000, "number of timers", , , serrBadOp) THEN
   REDIM PRESERVE timers(large(0, retvals(0) - 1))
   IF retvals(0) = 0 THEN
    'Unfortunately, have to have at least one timer. Deactivate/blank it, in case the player
    'wants "allocate timers(0)" to kill all timers.
    REDIM timers(0)
   END IF
  END IF
/'  Disabled until an alternative ("new timer") is decided upon
 CASE 471'--unused timer
  scriptret = -1
  FOR i as integer = 0 TO UBOUND(timers)
   IF timers(i).speed <= 0 THEN
    scriptret = i
    WITH timers(scriptret)
     .speed = 0
     .ticks = 0
     .count = 0
     .st = 0
     .trigger = 0
     .flags = 0
    END WITH
    EXIT FOR
   END IF
  NEXT
  IF scriptret = -1 THEN
   scriptret = UBOUND(timers) + 1
   IF scriptret < 100000 THEN
    REDIM PRESERVE timers(scriptret)
   END IF
  END IF
'/
 CASE 480'--read zone (id, x, y)
  IF valid_zone(retvals(0)) THEN
   IF valid_tile_pos(retvals(1), retvals(2)) THEN
    scriptret = IIF(CheckZoneAtTile(zmap, retvals(0), retvals(1), retvals(2)), 1, 0)
   END IF
  END IF
 CASE 481'--write zone (id, x, y, value)
  IF valid_zone(retvals(0)) THEN
   IF valid_tile_pos(retvals(1), retvals(2)) THEN
    IF retvals(3) THEN
     IF SetZoneTile(zmap, retvals(0), retvals(1), retvals(2)) = 0 THEN
      scriptret = 1
      'Is serrWarn the best for commands which fail? Do we need another?
      scripterr "writezone: the maximum number of zones, 15, already overlap at " & retvals(1) & "," & retvals(2) & "; attempt to add another failed", serrWarn
     END IF
    ELSE
     UnsetZoneTile(zmap, retvals(0), retvals(1), retvals(2))
    END IF
    lump_reloading.zonemap.dirty = YES
   END IF
  END IF
 CASE 482'--zone at spot (x, y, count)
  IF valid_tile_pos(retvals(0), retvals(1)) THEN
   REDIM zoneshere() as integer
   GetZonesAtTile(zmap, zoneshere(), retvals(0), retvals(1))
   IF retvals(2) = -1 THEN  'getcount
    scriptret = UBOUND(zoneshere) + 1
   ELSEIF retvals(2) < -1 THEN
    scripterr "zone at spot: bad 'count' argument " & retvals(2), serrBadOp
   ELSE
    IF retvals(2) <= UBOUND(zoneshere) THEN scriptret = zoneshere(retvals(2))
   END IF
  END IF
 CASE 483'--zone number of tiles (id)
  IF valid_zone(retvals(0)) THEN
   scriptret = GetZoneInfo(zmap, retvals(0))->numtiles
  END IF
/' Unimplemented
 CASE 484'--draw with zone (id, layer)
  IF valid_zone(retvals(0)) THEN
  END IF
 CASE 485'--zone next tile x (id, x, y)
  IF valid_zone(retvals(0)) THEN
  END IF
 CASE 486'--zone next tile y (id, x, y)
  IF valid_zone(retvals(0)) THEN
  END IF
'/
 CASE 487'--get zone name (string, id)
  IF valid_plotstr(retvals(0)) AND valid_zone(retvals(1)) THEN
   plotstr(retvals(0)).s = GetZoneInfo(zmap, retvals(1))->name
  END IF
 CASE 488'--get zone extra (id, extra)
  IF valid_zone(retvals(0)) AND bound_arg(retvals(1), 0, 2, "extra data number", , , serrBadOp) THEN
   scriptret = GetZoneInfo(zmap, retvals(0))->extra(retvals(1))
  END IF
 CASE 489'--set zone extra (id, extra, value)
  IF valid_zone(retvals(0)) AND bound_arg(retvals(1), 0, 2, "extra data number", , , serrBadOp) THEN
   GetZoneInfo(zmap, retvals(0))->extra(retvals(1)) = retvals(2)
   lump_reloading.zonemap.dirty = YES
  END IF
 CASE 493'--load backdrop sprite (record)
  scriptret = load_sprite_plotslice(sprTypeMXS, retvals(0))
 CASE 494 '--replace backdrop sprite (handle, record)
  replace_sprite_plotslice retvals(0), sprTypeMXS, retvals(1)
 CASE 495 '--get sprite trans (handle)
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
   scriptret = IIF(dat->trans, 1, 0)
  END IF 
 CASE 496 '--set sprite trans (handle, bool)
  IF valid_plotsprite(retvals(0)) THEN
   ChangeSpriteSlice plotslices(retvals(0)), , , , , , , retvals(1)
  END IF 
 CASE 500 '--set slice velocity x (handle, pixels per tick, ticks)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .Velocity.X = retvals(1)
    .VelTicks.X = retvals(2)
    .TargTicks = 0
   END WITH
  END IF
 CASE 501 '--set slice velocity y (handle, pixels per tick)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .Velocity.Y = retvals(1)
    .VelTicks.Y = retvals(2)
    .TargTicks = 0
   END WITH
  END IF
 CASE 502 '--get slice velocity x (handle)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    scriptret = .Velocity.X
   END WITH
  END IF
 CASE 503 '--get slice velocity y (handle)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    scriptret = .Velocity.Y
   END WITH
  END IF
 CASE 504 '--set slice velocity (handle, x pixels per tick, y pixels per tick, ticks)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .Velocity.X = retvals(1)
    .Velocity.Y = retvals(2)
    .VelTicks.X = retvals(3)
    .VelTicks.Y = retvals(3)
    .TargTicks = 0
   END WITH
  END IF
 CASE 505 '--stop slice (handle)
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    .Velocity.X = 0
    .Velocity.Y = 0
    .VelTicks.X = 0
    .VelTicks.Y = 0
    .TargTicks = 0
   END WITH
  END IF
 CASE 506 '--move slice to (handle, x, y, ticks)
  IF valid_plotslice(retvals(0)) THEN
   IF retvals(3) < 1 THEN
     scripterr current_command_name() & ": ticks arg " & retvals(3) & " mustn't be < 1", serrBadOp
   ELSE
    SetSliceTarg plotslices(retvals(0)), retvals(1), retvals(2), retvals(3)
   END IF
  END IF
 CASE 507 '--move slice by (handle, rel x, rel y, ticks)
  IF valid_plotslice(retvals(0)) THEN
   IF retvals(3) < 1 THEN
     scripterr current_command_name() & ": ticks arg " & retvals(3) & " mustn't be < 1", serrBadOp
   ELSE
    WITH *plotslices(retvals(0))
     SetSliceTarg plotslices(retvals(0)), .X + retvals(1), .Y + retvals(2), retvals(3)
    END WITH
   END IF
  END IF
 CASE 508'--wait for slice
  IF valid_plotslice(retvals(0)) THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 509'--slice is moving
  IF valid_plotslice(retvals(0)) THEN
   WITH *plotslices(retvals(0))
    IF .Velocity.X <> 0 ORELSE .Velocity.Y <> 0 ORELSE .TargTicks > 0 THEN
     scriptret = 1
    END IF
   END WITH
  END IF
 CASE 510 '--create ellipse
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slEllipse, SliceTable.scriptsprite)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  IF retvals(2) <> -1 ANDALSO bound_arg(retvals(2), 0, 255, "bordercol") THEN
   ChangeEllipseSlice sl, retvals(2)
  END IF
  IF retvals(3) <> -1 ANDALSO bound_arg(retvals(3), 0, 255, "fillcol") THEN
   ChangeEllipseSlice sl, , retvals(3)
  END IF
  scriptret = create_plotslice_handle(sl)
 CASE 511 '--slice is ellipse
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slEllipse THEN scriptret = 1
  END IF
 CASE 512 '--set ellipse border col
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "bordercol") THEN
    ChangeEllipseSlice plotslices(retvals(0)), retvals(1)
   END IF
  END IF
 CASE 513 '--set ellipse fill col
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "fillcol") THEN
    ChangeEllipseSlice plotslices(retvals(0)), , retvals(1)
   END IF
  END IF
 CASE 514 '--get ellipse border col
  IF valid_plotslice(retvals(0)) THEN
   DIM dat as EllipseSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->bordercol
  END IF
 CASE 515 '--get ellipse fill col
  IF valid_plotslice(retvals(0)) THEN
   DIM dat as EllipseSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->fillcol
  END IF
 CASE 516 '--_checkpoint
  IF autotestmode = YES THEN
   write_checkpoint
  ELSE
   debug "_checkpoint ignored"
  END IF
 CASE 519 '--get hero slice
  IF bound_arg(retvals(0), 0, 3, "caterpillar slot") THEN
   scriptret = find_plotslice_handle(herow(retvals(0)).sl)
  END IF
 CASE 520 '--get NPC slice
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = find_plotslice_handle(npc(npcref).sl)
  END IF
 CASE 521 '--get door x
  IF valid_door(retvals(0)) THEN
   scriptret = gam.map.door(retvals(0)).x
  ELSE
   scriptret = -1
  END IF
 CASE 522 '--get door y
  IF valid_door(retvals(0)) THEN
   scriptret = gam.map.door(retvals(0)).y - 1
  ELSE
   scriptret = -1
  END IF
 CASE 523 '--get door destination id
  scriptret = -1
  IF valid_door(retvals(0)) THEN
   DIM linknum as integer = find_doorlink(retvals(0))
   IF linknum >= 0 THEN scriptret = gam.map.doorlinks(linknum).dest
  END IF
 CASE 524 '--get door destination map
  scriptret = -1
  IF valid_door(retvals(0)) THEN
   DIM linknum as integer = find_doorlink(retvals(0))
   IF linknum >= 0 THEN scriptret = gam.map.doorlinks(linknum).dest_map
  END IF
 CASE 525 '--door exists
  IF retvals(0) >= 0 AND retvals(0) <= 99 THEN
   scriptret = readbit(gam.map.door(retvals(0)).bits(), 0, 0)
  END IF
 CASE 526 '--get attack caption
  IF valid_plotstr(retvals(0), 5) AND bound_arg(retvals(1), 1, gen(genMaxAttack)+1, "attack ID", , , serrBadOp) THEN
   plotstr(retvals(0)).s = readattackcaption(retvals(1) - 1)
   scriptret = 1
  END IF
 CASE 527 '--get rect fuzziness (slice)
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->translucent = transFuzzy THEN
    scriptret = dat->fuzzfactor
   ELSEIF dat->translucent = transHollow THEN
    scriptret = 0
   ELSEIF dat->translucent = transOpaque THEN
    scriptret = 100
   END IF
  END IF
 CASE 528 '--set rect fuzziness (slice, percent)
  IF valid_plotrect(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 100, "fuzziness percentage", , , serrBadOp) THEN
    IF retvals(1) = 0 THEN
     'Reset fuzzfactor to default 50% for future "set rect trans (sl, trans:fuzzy)"
     ChangeRectangleSlice plotslices(retvals(0)), , , , , transHollow, 50
    ELSEIF retvals(1) = 100 THEN 
     ChangeRectangleSlice plotslices(retvals(0)), , , , , transOpaque, 50
    ELSE
     ChangeRectangleSlice plotslices(retvals(0)), , , , , transFuzzy, retvals(1)
    END IF
   END IF
  END IF
 CASE 529 '-- textbox line (string, box, line, expand, strip)
  IF valid_plotstr(retvals(0), 5) ANDALSO _
     bound_arg(retvals(1), 0, gen(genMaxTextbox), "textbox", , , serrBadOp) THEN
   IF retvals(2) < 0 THEN
    'There's no upper bound on valid textbox line numbers
    scripterr "textbox line: invalid line number " & retvals(2), serrBadOp
   ELSEIF retvals(2) > 7 THEN
    'On the other hand, this is currently impossible
    plotstr(retvals(0)).s = ""
   ELSE
    DIM box as TextBox
    LoadTextBox box, retvals(1)
    plotstr(retvals(0)).s = box.text(retvals(2))
    IF retvals(4) THEN plotstr(retvals(0)).s = trim(plotstr(retvals(0)).s)
    IF retvals(3) THEN embedtext plotstr(retvals(0)).s
   END IF
  END IF
 CASE 530 '--get slice text (string, slice)
  IF valid_plotstr(retvals(0), serrBadOp) THEN
   IF valid_plottextslice(retvals(1)) THEN
    DIM dat as TextSliceData Ptr
    dat = plotslices(retvals(1))->SliceData
    plotstr(retvals(0)).s = dat->s
   END IF
  END IF
 CASE 531 '--get input text (string)
  IF valid_plotstr(retvals(0)) THEN
   IF gam.getinputtext_enabled = NO THEN
    scripterr "'get input text' needs to be enabled with 'enable input text'", serrBadOp
   ELSE
    plotstr(retvals(0)).s = getinputtext()
   END IF
  END IF
 CASE 532 '--enable input text (enable)
  gam.getinputtext_enabled = retvals(0)
 CASE 533 '--input text enabled
  scriptret = gam.getinputtext_enabled
 CASE 534 '--set hero hand x
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    gam.hero(retvals(0)).hand_pos(retvals(1)).x = retvals(2)
   END IF
  END IF
 CASE 535 '--set hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    gam.hero(retvals(0)).hand_pos(retvals(1)).y = retvals(2)
   END IF
  END IF
 CASE 536 '--get hero hand x
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    scriptret = gam.hero(retvals(0)).hand_pos(retvals(1)).x
   END IF
  END IF
 CASE 537 '--get hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    scriptret = gam.hero(retvals(0)).hand_pos(retvals(1)).y
   END IF
  END IF
 CASE 538 '--get default hero hand x
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    scriptret = GetHeroHandPos(gam.hero(retvals(0)).id, retvals(1), NO)
   END IF
  END IF
 CASE 539 '--get default hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(0), 0, 1, "attack frame") THEN
    scriptret = GetHeroHandPos(gam.hero(retvals(0)).id, retvals(1), YES)
   END IF
  END IF
 CASE 540'--check onetime
  IF bound_arg(retvals(0), 1, 15999, "onetime use tag") THEN
   scriptret = ABS(istag(onetime(), retvals(0), 0))
  END IF
 CASE 541'--set onetime
  IF bound_arg(retvals(0), 1, 15999, "onetime use tag") THEN
   settag onetime(), retvals(0), retvals(1)
   tag_updates
  END IF
 CASE 542 '--microseconds
  scriptret = fmod((TIMER * 1e6) + 2147483648.0, 4294967296.0) - 2147483648.0
 CASE 543 '--enemy elemental resist as int (enemy, element)
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy id", , , serrBadOp) THEN
   IF bound_arg(retvals(1), 0, gen(genNumElements) - 1, "element number") THEN
    DIM enemy as EnemyDef
    loadenemydata enemy, retvals(0), YES
    scriptret = 100 * enemy.elementals(retvals(1))  'rounds to nearest int
   END IF
  END IF
 CASE 544 '--hero Z
  IF really_valid_hero_party(retvals(0), 3) THEN
   scriptret = catz(retvals(0) * 5)
  END IF
 CASE 547 '--item maximum stack size (item id)
  IF valid_item(retvals(0)) THEN
   scriptret = get_item_stack_size(retvals(0))
  END IF
 CASE 548 '--npc Z
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = npc(npcref).z
  END IF
 CASE 549 '--set npc Z
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   npc(npcref).z = retvals(1)
  END IF
 CASE 550 '--door at spot
  scriptret = find_door(retvals(0), retvals(1))
 CASE 551 '--suspend doors
  setbit gen(), genSuspendBits, suspenddoors, 1
 CASE 552 '--resume doors
  setbit gen(), genSuspendBits, suspenddoors, 0
 CASE 553 '--running on desktop
#IFDEF __FB_ANDROID__
 scriptret = 0
#ELSE
 scriptret = 1
#ENDIF
 CASE 554 '--running on mobile
  scriptret = IIF(running_on_mobile(), 1, 0)
 CASE 555 '--running on console
  scriptret = IIF(running_on_console(), 1, 0)
 CASE 565 '--string sprintf (dest string id, format string id, args...)
  IF valid_plotstr(retvals(0), serrBadOp) AND valid_plotstr(retvals(1), serrBadOp) THEN
   plotstr(retvals(0)).s = script_sprintf()
   scriptret = retvals(0)
  END IF
 CASE 566 '--script error (string id)
  IF retvals(0) = -1 THEN
   scripterr "(Triggered with ""scripterror"", no message)", serrBadOp
  ELSEIF valid_plotstr(retvals(0), serrBadOp) THEN
   scripterr !"(Triggered with ""scripterror""):\n" & plotstr(retvals(0)).s, serrBadOp
  END IF
 CASE 567 '--get script name (string id, script id)
  IF valid_plotstr(retvals(0), serrBadOp) THEN
   ' Should be safe to call with any invalid ID number
   DIM scrname as string = scriptname(retvals(1))
   ' Real script names can't start with [, this indicates an invalid ID
   IF scrname[0] = ASC("[") THEN
    scripterr "getscriptname: invalid script ID " & retvals(1), serrBadOp
   ELSE
    plotstr(retvals(0)).s = scrname
   END IF
  END IF
 CASE 568 '--get calling script id (depth)
  IF retvals(0) < 1 THEN
   scripterr "get calling script id: expected a depth of at least 1", serrBadOp
  ELSE
   ' Returns 0 if non-existent
   scriptret = ancestor_script_id(nowscript, retvals(0))
  END IF

'old scriptnpc

 CASE 26'--set NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).frame = bound(retvals(1), 0, 1) * 2
 CASE 39'--camera follows NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   gen(cameramode) = npccam
   gen(cameraArg) = npcref
  END IF
 CASE 45'--NPC x
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).x \ 20
 CASE 46'--NPC y
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).y \ 20
 CASE 52'--walk NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   SELECT CASE retvals(1)
    CASE 0'--north
     npc(npcref).dir = 0
     npc(npcref).ygo = retvals(2) * 20
    CASE 1'--east
     npc(npcref).dir = 1
     npc(npcref).xgo = retvals(2) * -20
    CASE 2'--south
     npc(npcref).dir = 2
     npc(npcref).ygo = retvals(2) * -20
    CASE 3'--west
     npc(npcref).dir = 3
     npc(npcref).xgo = retvals(2) * 20
   END SELECT
  END IF
 CASE 54'--set NPC direction
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).dir = ABS(retvals(1)) MOD 4
 CASE 88'--set NPC position
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   cropposition retvals(1), retvals(2), 1
   npc(npcref).x = retvals(1) * 20
   npc(npcref).y = retvals(2) * 20
  END IF
 CASE 101'--NPC direction
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).dir
 CASE 117, 177'--NPC is walking
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF npc(npcref).xgo = 0 AND npc(npcref).ygo = 0 THEN
    scriptret = 0
   ELSE
    scriptret = 1
   END IF
   IF id = 117 THEN scriptret = scriptret XOR 1 'Backcompat hack
  END IF
 CASE 120'--NPC reference
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npcs) THEN
   DIM found as integer = 0
   FOR i as integer = 0 TO UBOUND(npc)
    IF npc(i).id - 1 = retvals(0) THEN
     IF found = retvals(1) THEN
      scriptret = (i + 1) * -1
      EXIT FOR
     END IF
     found = found + 1
    END IF
   NEXT i
  END IF
 CASE 121'--NPC at spot
  scriptret = 0
  DIM found as integer = 0
  FOR i as integer = 0 TO UBOUND(npc)
   IF npc(i).id > 0 THEN
    IF npc(i).x \ 20 = retvals(0) THEN 
     IF npc(i).y \ 20 = retvals(1) THEN
      IF found = retvals(2) THEN
       scriptret = (i + 1) * -1
       EXIT FOR
      END IF
      found = found + 1
     END IF
    END IF
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 122'--get NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = ABS(npc(npcref).id) - 1
  ELSE
   scriptret = -1
  END IF
 CASE 123'--NPC copy count
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npcs) THEN
   FOR i as integer = 0 TO UBOUND(npc)
    IF npc(i).id - 1 = retvals(0) THEN
     scriptret = scriptret + 1
    END IF
   NEXT i
  END IF
 CASE 124'--change NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 AND retvals(1) >= 0 AND retvals(1) <= UBOUND(npcs) THEN
   npc(npcref).id = retvals(1) + 1
   '--update the walkabout sprite for the changed NPC
   set_walkabout_sprite npc(npcref).sl, npcs(retvals(1)).picture, npcs(retvals(1)).palette
   '--run visnpc to apply any changes to the NPCs tag-visibility
   visnpc
  END IF
 CASE 125'--create NPC
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npcs) THEN
   DIM i as integer
   FOR i = UBOUND(npc) TO 0 STEP -1
    IF npc(i).id = 0 THEN EXIT FOR
   NEXT
   'for backwards compatibility with games that max out the number of NPCs, try to overwrite tag-disabled NPCs
   'FIXME: delete this bit once we raise the NPC limit
   IF i = -1 THEN
    FOR i = UBOUND(npc) TO 0 STEP -1
     IF npc(i).id <= 0 THEN EXIT FOR
    NEXT
    'I don't want to raise an error here, again because it probably happens in routine in games like SoJ
    DIM msgtemp as string = "create NPC: trying to create NPC id " & retvals(0) & " at " & retvals(1)*20 & "," & retvals(2)*20
    IF i = -1 THEN 
     scripterr msgtemp & "; failed: too many NPCs exist", serrBound
    ELSE
     scripterr msgtemp & "; warning: had to overwrite tag-disabled NPC id " & ABS(npc(i).id)-1 & " at " & npc(i).x & "," & npc(i).y & ": too many NPCs exist", serrBound
    END IF
   END IF
   IF i > -1 THEN
    'This deletes the walkabout slice
    CleanNPCInst npc(i)
    DIM npc_id as integer = retvals(0)
    npc(i).id = npc_id + 1
    cropposition retvals(1), retvals(2), 1
    npc(i).x = retvals(1) * 20
    npc(i).y = retvals(2) * 20
    npc(i).dir = ABS(retvals(3)) MOD 4
    npc(i).sl = create_walkabout_slices(npc_layer())
    set_walkabout_sprite npc(i).sl, npcs(npc_id).picture, npcs(npc_id).palette
    set_walkabout_vis npc(i).sl, YES
    'debug "npc(" & i & ").sl=" & npc(i).sl & " [create npc(" & retvals(0) & ")]"
    update_npc_zones i
    scriptret = (i + 1) * -1
   END IF
  END IF
 CASE 126 '--destroy NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   'Don't run zone exit triggers.
   'This deletes the walkabout slice
   CleanNPCInst npc(npcref)
  END IF
 CASE 165'--NPC at pixel
  scriptret = 0
  DIM found as integer = 0
  FOR i as integer = 0 TO UBOUND(npc)
   IF npc(i).id > 0 THEN 
    IF npc(i).x <= retvals(0) AND npc(i).x > (retvals(0) - 20) THEN 
     IF npc(i).y <= retvals(1) AND npc(i).y > (retvals(1) - 20) THEN
      IF found = retvals(2) THEN
       scriptret = (i + 1) * -1
       EXIT FOR
      END IF
      found = found + 1
     END IF
    END IF
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 182'--read NPC
  IF bound_arg(retvals(1), 0, 16, "NPCstat: constant") THEN
   DIM npcid as integer = get_valid_npc_id(retvals(0), 4)
   IF npcid <> -1 THEN
    scriptret = GetNPCD(npcs(npcid), retvals(1))
   END IF
  END IF
 CASE 192'--NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).frame \ 2
 CASE 193'--NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    scriptret = npc(npcref).extra(retvals(1))
   END IF
  END IF
 CASE 194'--set NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    npc(npcref).extra(retvals(1)) = retvals(2)
   END IF
  END IF
 CASE 472'--set NPC ignores walls (npc, value)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   npc(npcref).ignore_walls = (retvals(1) <> 0)
  END IF
 CASE 473'--get NPC ignores walls (npc)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = iif(npc(npcref).ignore_walls, 1, 0)
  END IF
 CASE 474'--set NPC obstructs (npc, value)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   npc(npcref).not_obstruction = (retvals(1) = 0)
  END IF
 CASE 475'--get NPC obstructs (npc)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = iif(npc(npcref).not_obstruction, 0, 1)
  END IF
 CASE 476'--set NPC usable (npc, value)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   npc(npcref).suspend_use = (retvals(1) = 0)
  END IF
 CASE 477'--get NPC usable (npc)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = iif(npc(npcref).suspend_use, 0, 1)
  END IF
 CASE 478'--set NPC moves (npc, value)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   npc(npcref).suspend_ai = (retvals(1) = 0)
  END IF
 CASE 479'--get NPC moves (npc)
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = iif(npc(npcref).suspend_ai, 0, 1)
  END IF
 CASE 559'--get sprite default pal
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->paletted = NO THEN
    'Only paletted sprites have default palettes
    scriptret = -1
   ELSE
    scriptret = getdefaultpal(dat->spritetype, dat->record)
   END IF
  END IF
 CASE 560'--NPC is disabled
  npcref = getnpcref(retvals(0), 0)
  scriptret = 0
  IF npcref >= 0 THEN
   IF npc(npcref).id < 0 THEN
    scriptret = 1
   END IF
  ELSE
   scriptret = 1
  END IF
 CASE 569'--camera follows slice
  IF valid_plotslice(retvals(0)) THEN
   gen(cameramode) = slicecam
   gen(cameraArg) = retvals(0)
  END IF
 CASE 570'--get active battle pause on all menus
  scriptret = IIF(readbit(gen(), genBits, 13) <> 0, 1, 0)
 CASE 571'--set active battle pause on all menus
  setbit gen(), genBits, 13, retvals(0)
 CASE 572'--dissolve sprite
  IF valid_plotsprite(retvals(0)) THEN
   DissolveSpriteSlice plotslices(retvals(0)), retvals(1), retvals(2), retvals(3), retvals(4), retvals(5)
  END IF
 CASE 573'--cancel dissolve
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat THEN
    dat->dissolving = NO
    dat->d_auto = NO
   END IF
  END IF
 CASE 574'--sprite is dissolving
  scriptret = 0
  IF valid_plotsprite(retvals(0)) THEN
   DIM dat as SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat THEN
    IF dat->dissolving THEN scriptret = 1
   END IF
  END IF
 CASE 575'--wait for dissolve
  IF valid_plotsprite(retvals(0)) THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 576'--hide virtual gamepad
  gam.script_hide_virtual_gamepad = YES
  gam.script_show_virtual_gamepad = NO
  update_virtual_gamepad_display()
 CASE 577'--show virtual gamepad
  gam.script_hide_virtual_gamepad = NO
  gam.script_show_virtual_gamepad = YES
  update_virtual_gamepad_display()
 CASE 578'--auto virtual gamepad
  gam.script_hide_virtual_gamepad = NO
  gam.script_show_virtual_gamepad = NO
  update_virtual_gamepad_display()
 CASE 579'--get vert align
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->AlignVert
  END IF
 CASE 580'--get horiz align
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->AlignHoriz
  END IF
 CASE 581'--get vert anchor
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->AnchorVert
  END IF
 CASE 582'--get horiz anchor
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->AnchorHoriz
  END IF
 CASE 583'--set select slice index
  IF valid_plotselectslice(retvals(0)) THEN
   DIM dat as SelectSliceData Ptr
   dat = GetSelectSliceData(plotslices(retvals(0)))
   dat->index = retvals(1) 'An invalid index just means that no child slice is visible.
  END IF
 CASE 584'--get select slice index
  IF valid_plotselectslice(retvals(0)) THEN
   DIM dat as SelectSliceData Ptr
   dat = GetSelectSliceData(plotslices(retvals(0)))
   scriptret = dat->index
  END IF
 CASE 585 '--create select
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slSelect, SliceTable.scriptsprite)
  scriptret = create_plotslice_handle(sl)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
 CASE 586 '--slice is select
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slSelect THEN scriptret = 1
  END IF
 CASE 587 '--slice index among siblings
  IF valid_plotslice(retvals(0)) THEN
   scriptret = SliceIndexAmongSiblings(plotslices(retvals(0)))
  END IF
 CASE ELSE
  RETURN NO

END SELECT
RETURN YES
END FUNCTION

SUB tweakpalette (byval r as integer, byval g as integer, byval b as integer, byval first as integer = 0, byval last as integer = 255)
 FOR i as integer = first TO last
  master(i).r = bound(master(i).r + r * 4, 0, 255)
  master(i).g = bound(master(i).g + g * 4, 0, 255)
  master(i).b = bound(master(i).b + b * 4, 0, 255)
 NEXT i
END SUB

SUB update_vehicle_state ()
STATIC aheadx as integer
STATIC aheady as integer

IF vstate.mounting THEN '--scramble-----------------------
 '--part of the vehicle automount where heros scramble--
 IF npc(vstate.npc).xgo = 0 AND npc(vstate.npc).ygo = 0 THEN
  '--npc must stop before we mount
  IF vehscramble(npc(vstate.npc).x, npc(vstate.npc).y) THEN
   'Finished scramble
   vstate.mounting = NO
   IF vstate.dat.elevation > 0 THEN vstate.rising = YES
  END IF
 END IF
END IF'--scramble mount
IF vstate.rising THEN '--rise----------------------
 DIM risen_count as integer = 0
 FOR i as integer = 0 TO 3
  IF catz(i * 5) < vstate.dat.elevation THEN
   catz(i * 5) = catz(i * 5) + large(1, small(4, (vstate.dat.elevation - catz(i * 5) + 1) \ 2))
  ELSE
   risen_count += 1
  END IF
 NEXT i
 IF risen_count = 4 THEN
  vstate.rising = NO
 END IF
END IF
IF vstate.falling THEN '--fall-------------------
 DIM fallen_count as integer = 0
 FOR i as integer = 0 TO 3
  IF catz(i * 5) > 0 THEN
   catz(i * 5) = catz(i * 5) - large(1, small(4, (vstate.dat.elevation - catz(i * 5) + 1) \ 2))
  ELSE
   fallen_count += 1
  END IF
 NEXT i
 IF fallen_count = 4 THEN
  FOR i as integer = 0 TO 3
   catz(i * 5) = 0
  NEXT i
  vstate.falling = NO
  vstate.init_dismount = YES
 END IF
END IF
IF vstate.init_dismount THEN '--dismount---------------
 vstate.init_dismount = NO
 DIM disx as integer = catx(0) \ 20
 DIM disy as integer = caty(0) \ 20
 IF vstate.dat.dismount_ahead AND vstate.dat.pass_walls_while_dismounting THEN
  '--dismount-ahead is true, dismount-passwalls is true
  aheadxy disx, disy, catd(0), 1
  cropposition disx, disy, 1
 END IF
 IF vehpass(vstate.dat.dismount_to, readblock(pass, disx, disy), -1) THEN
  '--dismount point is landable
  FOR i as integer = 0 TO 15
   catx(i) = catx(0)
   caty(i) = caty(0)
   catd(i) = catd(0)
   catz(i) = 0
  NEXT i
  IF vstate.dat.dismount_ahead = YES THEN
   vstate.ahead = YES
   aheadx = disx * 20
   aheady = disy * 20
  ELSE
   vstate.trigger_cleanup = YES
  END IF
 ELSE
  '--dismount point is unlandable
  IF vstate.dat.elevation > 0 THEN
   vstate.rising = YES '--riseagain
  END IF
 END IF
END IF
IF vstate.trigger_cleanup THEN '--clear
 IF vstate.dat.on_dismount < 0 THEN trigger_script ABS(vstate.dat.on_dismount), YES, "vehicle dismount", "", scrqBackcompat()
 IF vstate.dat.on_dismount > 0 THEN loadsay vstate.dat.on_dismount
 settag vstate.dat.riding_tag, NO
 IF vstate.dat.dismount_ahead = YES AND vstate.dat.pass_walls_while_dismounting = NO THEN
  'FIXME: Why is this here, when dismounting is apparently also handled by vehscramble?
  'Does this have to do with Bug 764 - "Blocked by" vehicle setting does nothing ?
  SELECT CASE catd(0)
   CASE 0
    herow(0).ygo = 20
   CASE 1
    herow(0).xgo = -20
   CASE 2
    herow(0).ygo = -20
   CASE 3
    herow(0).xgo = 20
  END SELECT
 END IF
 herow(0).speed = vstate.old_speed
 npc(vstate.npc).xgo = 0
 npc(vstate.npc).ygo = 0
 npc(vstate.npc).z = 0
 delete_walkabout_shadow npc(vstate.npc).sl
 '--clear vehicle (sets vstate.active=NO, etc)
 reset_vehicle vstate
 FOR i as integer = 0 TO 15   'Why is this duplicated from dismounting?
  catx(i) = catx(0)
  caty(i) = caty(0)
  catd(i) = catd(0)
  catz(i) = 0
 NEXT i
 gam.random_battle_countdown = range(100, 60)
END IF
IF vstate.ahead THEN '--dismounting ahead
 IF vehscramble(aheadx, aheady) THEN
  vstate.ahead = NO
  vstate.trigger_cleanup = YES '--clear (happens next tick, maybe not intentionally)
 END IF
END IF
IF vstate.active = YES AND vehicle_is_animating() = NO THEN
 IF txt.showing = NO AND readbit(gen(), genSuspendBits, suspendplayer) = 0 THEN
  REDIM button(1) as integer
  button(0) = vstate.dat.use_button
  button(1) = vstate.dat.menu_button
  FOR i as integer = 0 TO 1
   IF carray(ccUse + i) > 1 AND herow(0).xgo = 0 AND herow(0).ygo = 0 THEN
    SELECT CASE button(i)
     CASE -2
      '-disabled
     CASE -1
      add_menu 0
      menusound gen(genAcceptSFX)
     CASE 0
      '--dismount
      vehicle_graceful_dismount
     CASE IS > 0
      trigger_script button(i), YES, "vehicle button " & i, "", scrqBackcompat()
    END SELECT
   END IF
  NEXT i
 END IF
END IF'--not animating

IF vstate.active THEN npc(vstate.npc).z = catz(0)

END SUB 'result

SUB vehicle_graceful_dismount ()
 herow(0).xgo = 0
 herow(0).ygo = 0
 IF vstate.dat.elevation > 0 THEN
  vstate.falling = YES
 ELSE
  vstate.init_dismount = YES
 END IF
END SUB

FUNCTION vehpass (byval n as integer, byval tile as integer, byval default as integer) as integer
 '--true means passable
 '--false means impassable

 DIM v as integer = default

 SELECT CASE n
  CASE 1
   v = (tile AND passVehA)
  CASE 2
   v = (tile AND passVehB)
  CASE 3
   v = ((tile AND passVehA) = passVehA) AND ((tile AND passVehB) = passVehB)
  CASE 4
   v = ((tile AND passVehA) = passVehA) OR ((tile AND passVehB) = passVehB)
  CASE 5
   v = NOT ((tile AND passVehA) = passVehA)
  CASE 6
   v = NOT ((tile AND passVehB) = passVehB)
  CASE 7
   v = NOT (((tile AND passVehA) = passVehA) OR ((tile AND passVehB) = passVehB))
  CASE 8
   v = YES
 END SELECT
 
 RETURN v <> 0

END FUNCTION

'Reload party walkabout graphics
SUB vishero ()
 DIM cater_slot as integer = 0
 FOR party_slot as integer = 0 TO 3
  IF gam.hero(party_slot).id >= 0 THEN
   set_walkabout_sprite herow(cater_slot).sl, gam.hero(party_slot).pic, gam.hero(party_slot).pal
   cater_slot += 1
  END IF
 NEXT
END SUB

'Change picture and/or palette of a walkabout slice.
'
'Here is an exhaustive list of the conditions under which a walkabout sprite slice gets updated,
'(picture and/or palette is changed), wiping out any modifications by scripts.
'Note everything else such as extra data and child slices of the walkabout sprite and container
'slices always remain untouched, except for the container position, shadow slice, and walking animation.
'
'-When a specific NPC is enabled or disabled by tag changes
' (disabled NPCs have their container slices deleted, not just hidden)
'-When alternpc, changenpcid, etc, is used, affecting a specific NPC/NPC ID
'-When reset_npc_graphics gets called, which happens when loading map state (sometimes after
' a battle, changing maps, loadmapstate) or when live previewing (changes to NPC data)
'ALL hero sprite slices get reloaded (vishero is called) when:
'-calling reset/setheropicture/palette(outsidebattle) on a walkabout party hero
'-the hero party changes, such as when changing the order of heroes
'Unlike NPCs, hero container slices are hidden rather than disabled when a hero slot is empty.
'Hero container slices are never recreated when changing maps, etc.
'
SUB set_walkabout_sprite (byval cont as Slice Ptr, byval pic as integer=-1, byval pal as integer=-2)
 DIM sprsl as Slice Ptr
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_sprite"
 ELSE
  sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, cont)
  IF sprsl = 0 THEN
   debug "null sprite slice in set_walkabout_sprite"
  ELSE
   ChangeSpriteSlice sprsl, 4, pic, pal
  END IF
 END IF
END SUB

SUB set_walkabout_frame (byval cont as Slice Ptr, byval direction as integer, byval frame as integer)
 DIM sprsl as Slice Ptr
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_frame"
 ELSE
  sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, cont)
  IF sprsl = 0 THEN
   debug "null sprite slice in set_walkabout_frame"
  ELSE
   ChangeSpriteSlice sprsl, , , , direction * 2 + frame
  END IF
 END IF
END SUB

SUB set_walkabout_vis (byval cont as Slice Ptr, byval vis as integer)
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_vis"
 ELSE
  cont->Visible = vis
 END IF
END SUB

SUB wrapaheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer, byval unitsize as integer)
 'alters X and Y ahead by distance in direction, wrapping if neccisary
 'unitsize is 20 for pixels, 1 for tiles
 
 aheadxy x, y, direction, distance
 
 IF gmap(5) = 1 THEN
  wrapxy x, y, mapsizetiles.x * unitsize, mapsizetiles.y * unitsize
 END IF
 
END SUB

SUB cropposition (byref x as integer, byref y as integer, byval unitsize as integer)

 IF gmap(5) = 1 THEN
  wrapxy x, y, mapsizetiles.x * unitsize, mapsizetiles.y * unitsize
 ELSE
  x = bound(x, 0, (mapsizetiles.x - 1) * unitsize)
  y = bound(y, 0, (mapsizetiles.y - 1) * unitsize)
 END IF

END SUB

FUNCTION wrappass (byval x as integer, byval y as integer, byref xgo as integer, byref ygo as integer, byval isveh as integer) as integer
 wrappass = 0
 ' returns true if blocked by terrain
 REDIM pd(3) as integer
 
 DIM tilex as integer = x
 DIM tiley as integer = y
 DIM p as integer = readblock(pass, tilex, tiley)
 
 FOR i as integer = 0 TO 3
  tilex = x
  tiley = y
  wrapaheadxy tilex, tiley, i, 1, 1
  IF tilex < 0 ORELSE tilex >= pass.wide ORELSE tiley < 0 ORELSE tiley >= pass.high THEN
   pd(i) = 15
  ELSE
   pd(i) = readblock(pass, tilex, tiley)
  END IF
 NEXT i
 
 'debug "wrappass x=" & x & " y=" & y & " xgo=" & xgo & " ygo=" & ygo & " p=" & p & " north=" & pd(0) & " east=" & pd(1) & " south=" & pd(2) & " west=" & pd(3)
 
 IF ygo > 0 ANDALSO movdivis(ygo) ANDALSO ((p AND passNorthWall) = passNorthWall ORELSE (pd(0) AND passSouthWall) = passSouthWall ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(0), 0))) THEN ygo = 0: wrappass = 1
 IF ygo < 0 ANDALSO movdivis(ygo) ANDALSO ((p AND passSouthWall) = passSouthWall ORELSE (pd(2) AND passNorthWall) = passNorthWall ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(2), 0))) THEN ygo = 0: wrappass = 1
 IF xgo > 0 ANDALSO movdivis(xgo) ANDALSO ((p AND passWestWall) = passWestWall   ORELSE (pd(3) AND passEastWall) = passEastWall   ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(3), 0))) THEN xgo = 0: wrappass = 1
 IF xgo < 0 ANDALSO movdivis(xgo) ANDALSO ((p AND passEastWall) = passEastWall   ORELSE (pd(1) AND passWestWall) = passWestWall   ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(1), 0))) THEN xgo = 0: wrappass = 1

END FUNCTION

FUNCTION wrapzonecheck (byval zone as integer, byval x as integer, byval y as integer, byval xgo as integer, byval ygo as integer) as integer
 'x, y in pixels
 'Warning: always wraps! But that isn't a problem on non-wrapping maps.

 x -= xgo
 y -= ygo
 wrapxy (x, y, mapsizetiles.x * 20, mapsizetiles.y * 20)
 RETURN CheckZoneAtTile(zmap, zone, x \ 20, y \ 20)
END FUNCTION

FUNCTION wrapcollision (byval xa as integer, byval ya as integer, byval xgoa as integer, byval ygoa as integer, byval xb as integer, byval yb as integer, byval xgob as integer, byval ygob as integer) as integer
 DIM as integer x1, x2, y1, y2
 x1 = (xa - bound(xgoa, -20, 20)) \ 20
 x2 = (xb - bound(xgob, -20, 20)) \ 20
 y1 = (ya - bound(ygoa, -20, 20)) \ 20
 y2 = (yb - bound(ygob, -20, 20)) \ 20

 IF gmap(5) = 1 THEN
  RETURN (x1 - x2) MOD mapsizetiles.x = 0 AND (y1 - y2) MOD mapsizetiles.y = 0
 ELSE
  RETURN (x1 = x2) AND (y1 = y2)
 END IF

END FUNCTION

FUNCTION wraptouch (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval distance as integer) as integer
 'whether 2 walkabouts are within distance pixels horizontally + vertically
 IF gmap(5) = 1 THEN
  IF ABS((x1 - x2) MOD (mapsizetiles.x * 20 - distance)) <= distance AND ABS((y1 - y2) MOD (mapsizetiles.y * 20 - distance)) <= distance THEN RETURN 1
 ELSE
  IF ABS(x1 - x2) <= 20 AND ABS(y1 - y2) <= 20 THEN RETURN 1
 END IF
 RETURN 0
END FUNCTION

SUB wrapxy (byref x as integer, byref y as integer, byval wide as integer, byval high as integer)
 '--wraps the given X and Y values within the bounds of width and height
 x = ((x MOD wide) + wide) MOD wide  'negative modulo is the devil's creation and never helped me once
 y = ((y MOD high) + high) MOD high
END SUB

SUB wrappedsong (byval songnumber as integer)
 IF songnumber <> presentsong THEN
  playsongnum songnumber
  presentsong = songnumber
 ELSE
  'Has this ever worked? Maybe in old DOS versions
  'resumesong
 END IF
 'Cancel any queued music change
 gam.music_change_delay = 0
END SUB

SUB stopsong
 presentsong = -1
 music_stop
 'Cancel any queued music change
 gam.music_change_delay = 0
END SUB

FUNCTION backcompat_sound_id (byval id as integer) as integer
 IF backcompat_sound_slot_mode THEN
  'BACKWARDS COMPATABILITY HACK
  IF id >= 0 AND id <= 7 THEN
   RETURN backcompat_sound_slots(id) - 1
  END IF
 ELSE
  'Normal playsound mode
  RETURN id
 END IF
END FUNCTION

'Returns true if the scramble is finished
FUNCTION vehscramble(byval targx as integer, byval targy as integer) as bool
 DIM scrambled_heroes as integer = 0
 DIM count as integer = herocount()
 DIM scramx as integer
 DIM scramy as integer
 FOR i as integer = 0 TO 3
  IF i < count THEN
   scramx = catx(i * 5)
   scramy = caty(i * 5)
   IF ABS(scramx - targx) < large(herow(i).speed, 4) THEN
    scramx = targx
    herow(i).xgo = 0
    herow(i).ygo = 0
   END IF
   IF ABS(scramy - targy) < large(herow(i).speed, 4) THEN
    scramy = targy
    herow(i).xgo = 0
    herow(i).ygo = 0
   END IF
   IF ABS(targx - scramx) > 0 AND herow(i).xgo = 0 THEN
    herow(i).xgo = 20 * SGN(scramx - targx)
   END IF
   IF ABS(targy - scramy) > 0 AND herow(i).ygo = 0 THEN
    herow(i).ygo = 20 * SGN(scramy - targy)
   END IF
   IF gmap(5) = 1 THEN
    '--this is a wrapping map
    IF ABS(scramx - targx) > mapsizetiles.x * 20 / 2 THEN herow(i).xgo *= -1
    IF ABS(scramy - targy) > mapsizetiles.y * 20 / 2 THEN herow(i).ygo *= -1
   END IF
   IF scramx - targx = 0 AND scramy - targy = 0 THEN scrambled_heroes += 1
   catx(i * 5) = scramx
   caty(i * 5) = scramy
  END IF
 NEXT i
 IF scrambled_heroes = count THEN
  IF vstate.dat.on_mount < 0 THEN trigger_script ABS(vstate.dat.on_mount), YES, "vehicle on-mount", "", scrqBackcompat()
  IF vstate.dat.on_mount > 0 THEN loadsay vstate.dat.on_mount
  herow(0).speed = vstate.dat.speed
  IF herow(0).speed = 3 THEN herow(0).speed = 10
  '--null out hero's movement
  FOR i as integer = 0 TO 3
   herow(i).xgo = 0
   herow(i).ygo = 0
  NEXT i
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

SUB loadsay (byval box_id as integer)
DIM j as integer

DO '--This loop is where we find which box will be displayed right now
 '--load data from the textbox lump
 LoadTextBox txt.box, box_id

 '-- evaluate "instead" conditionals
 IF istag(txt.box.instead_tag, 0) THEN
  '--do something else instead
  IF txt.box.instead < 0 THEN
   trigger_script -txt.box.instead, YES, "textbox instead", "box " & box_id, scrqBackcompat()
   txt.sayer = -1
   EXIT SUB
  ELSE
   IF box_id <> txt.box.instead THEN
    box_id = txt.box.instead
    CONTINUE DO' Skip back to the top of the loop and get another box
   END IF
  END IF
 END IF

 EXIT DO'--We have the box we want to display, proceed
LOOP

'--Store box ID number for later reference
txt.id = box_id

gen(genTextboxBackdrop) = 0
WITH txt.choicestate
 .pt = 0
 .size = 2
 .last = 1
END WITH

FOR j as integer = 0 TO 7
 embedtext txt.box.text(j), 38
NEXT j

'-- set tags indicating the text box has been seen.
IF istag(txt.box.settag_tag, 0) THEN
 settag txt.box.settag1
 settag txt.box.settag2
 'NOTE: We just changed tags, but we do not want tag_updates to update
 '  NPC visibility until after the box adances. We do however update
 '  menu tags right away.
 tag_updates NO
END IF

'--make a sound if the choicebox is enabled
IF txt.box.choice_enabled THEN MenuSound gen(genAcceptSFX)

'-- update backdrop if necessary
IF txt.box.backdrop > 0 THEN
 gen(genTextboxBackdrop) = txt.box.backdrop
END IF

'-- change music if necessary
IF txt.box.music > 0 THEN
 txt.remember_music = presentsong
 wrappedsong txt.box.music - 1
END IF

'--play a sound effect
IF txt.box.sound_effect > 0 THEN
 playsfx txt.box.sound_effect - 1
END IF

'-- evaluate menu conditionals
IF istag(txt.box.menu_tag, 0) THEN
 add_menu txt.box.menu
END IF

txt.showing = YES
txt.fully_shown = NO
txt.show_lines = 0

'--Create a set of slices to display the text box
init_text_box_slices txt

update_virtual_gamepad_display()

END SUB

FUNCTION valid_spriteslice_dat(byval sl as Slice Ptr) as integer
 IF sl = 0 THEN scripterr "null slice ptr in valid_spriteslice_dat", serrBug : RETURN NO
 DIM dat as SpriteSliceData Ptr = sl->SliceData
 IF dat = 0 THEN
  scripterr SliceTypeName(sl) & " handle " & retvals(0) & " has null dat pointer", serrBug
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_plotslice(byval handle as integer, byval errlev as scriptErrEnum = serrBadOp) as integer
 IF handle < LBOUND(plotslices) OR handle > UBOUND(plotslices) THEN
  scripterr current_command_name() & ": invalid slice handle " & handle, errlev
  RETURN NO
 END IF
 IF plotslices(handle) = 0 THEN
  scripterr current_command_name() & ": slice handle " & handle & " has already been deleted", errlev
  RETURN NO
 END IF
 IF ENABLE_SLICE_DEBUG THEN
  IF SliceDebugCheck(plotslices(handle)) = NO THEN
   scripterr current_command_name() & ": slice " & handle & " " & plotslices(handle) & " is not in the slice debug table!", serrBug
   RETURN NO
  END IF
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_plotsprite(byval handle as integer) as integer
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slSprite THEN
   IF valid_spriteslice_dat(plotslices(handle)) THEN
    RETURN YES
   END IF
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a sprite", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotrect(byval handle as integer) as integer
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slRectangle THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a rect", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plottextslice(byval handle as integer) as integer
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slText THEN
   IF plotslices(handle)->SliceData = 0 THEN
    scripterr current_command_name() & ": text slice handle " & handle & " has null data", serrBug
    RETURN NO
   END IF
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not text", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotgridslice(byval handle as integer) as integer
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slGrid THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a grid", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotselectslice(byval handle as integer) as integer
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slSelect THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a select", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_resizeable_slice(byval handle as integer, byval ignore_fill as integer=NO) as integer
 IF valid_plotslice(handle) THEN
  DIM sl as Slice Ptr
  sl = plotslices(handle)
  IF sl->SliceType = slRectangle OR sl->SliceType = slContainer OR sl->SliceType = slGrid OR sl->SliceType = slEllipse THEN
   IF sl->Fill = NO OR ignore_fill THEN
    RETURN YES
   ELSE
    scripterr current_command_name() & ": slice handle " & handle & " cannot be resized while filling parent", serrBadOp
   END IF
  ELSE
   IF sl->SliceType = slText THEN
    DIM dat as TextSliceData ptr
    dat = sl->SliceData
    IF dat = 0 THEN scripterr "sanity check fail, text slice " & handle & " has null data", serrBug : RETURN NO
    IF dat->wrap = YES THEN
     RETURN YES
    ELSE
     scripterr current_command_name() & ": text slice handle " & handle & " cannot be resized unless wrap is enabled", serrBadOp
    END IF
   ELSE
    scripterr current_command_name() & ": slice handle " & handle & " is not resizeable", serrBadOp
   END IF
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION create_plotslice_handle(byval sl as Slice Ptr) as integer
 IF sl = 0 THEN scripterr "create_plotslice_handle null ptr", serrBug : RETURN 0
 IF sl->TableSlot <> 0 THEN
  'this should not happen! Call find_plotslice_handle instead.
  scripterr "Error: " & SliceTypeName(sl) & " " & sl & " references plotslices(" & sl->TableSlot & ") which has " & plotslices(sl->TableSlot), serrBug
  RETURN 0
 END IF
 DIM i as integer
 'First search for an empty slice handle slot (which sucks because it means they get re-used)
 FOR i = LBOUND(plotslices) to UBOUND(plotslices)
  IF plotslices(i) = 0 THEN
   'Store the slice pointer in the handle slot
   plotslices(i) = sl
   'Store the handle slot in the slice
   sl->TableSlot = i
   ' and return the handle number
   RETURN i
  END IF
 NEXT
 'If no room is available, make the array bigger.
 REDIM PRESERVE plotslices(LBOUND(plotslices) TO UBOUND(plotslices) * 1.5 + 32)
 plotslicesp = @plotslices(1)
 'Store the slice pointer in the handle slot
 plotslices(i) = sl
 'Store the handle slot in the slice
 sl->TableSlot = i
 ' and return the handle number
 RETURN i
END FUNCTION

FUNCTION find_plotslice_handle(byval sl as Slice Ptr) as integer
 IF sl = 0 THEN RETURN 0 ' it would be silly to search for a null pointer
 IF sl->TableSlot THEN RETURN sl->TableSlot
 'slice not in table, so create a new handle for it
 RETURN create_plotslice_handle(sl)
END FUNCTION

SUB set_plotslice_handle(byval sl as Slice Ptr, handle as integer)
 'This function is used to restore handles when loading a slice collection from a saved game.
 'This should ONLY be called when starting a game, before any scripts have run!
 IF sl = 0 THEN debugc errPromptBug, "set_plotslice_handle null ptr"
 IF sl->TableSlot <> 0 THEN
  debugc errPromptBug, "set_plotslice_handle shouldn't be called on a slice with existing TableSlot"
 END IF

 IF handle > UBOUND(plotslices) THEN
  REDIM PRESERVE plotslices(LBOUND(plotslices) TO handle * 1.5 + 32)
  plotslicesp = @plotslices(1)
 END IF

 IF plotslices(handle) <> 0 THEN
  debugc errPromptBug, "set_plotslice_handle: non-empty plotslices(" & handle & ")"
 END IF
 
 'Store the slice pointer in the handle slot
 plotslices(handle) = sl
 'Store the handle slot in the slice
 sl->TableSlot = handle
END SUB

'By default, no palette set
FUNCTION load_sprite_plotslice(byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2) as integer
 WITH sprite_sizes(spritetype)
  IF bound_arg(record, 0, gen(.genmax) + .genmax_offset, "sprite record number") THEN
   DIM sl as Slice Ptr
   sl = NewSliceOfType(slSprite, SliceTable.scriptsprite)
   ChangeSpriteSlice sl, spritetype, record, pal
   RETURN create_plotslice_handle(sl)
  END IF
 END WITH
 RETURN 0 'Failure, return zero handle
END FUNCTION

'By default, no palette change
SUB replace_sprite_plotslice(byval handle as integer, byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2)
 WITH sprite_sizes(spritetype)
  IF valid_plotsprite(handle) THEN
   IF bound_arg(record, 0, gen(.genmax) + .genmax_offset, "sprite record number") THEN
    ChangeSpriteSlice plotslices(handle), spritetype, record, pal
   END IF
  END IF
 END WITH
END SUB

SUB change_rect_plotslice(byval handle as integer, byval style as integer=-2, byval bgcol as integer=-99, byval fgcol as integer=-99, byval border as integer=-3, byval translucent as RectTransTypes=transUndef)
 IF valid_plotslice(handle) THEN
  DIM sl as Slice Ptr
  sl = plotslices(handle)
  IF sl->SliceType = slRectangle THEN
   ChangeRectangleSlice sl, style, bgcol, fgcol, border, translucent
  ELSE
   scripterr current_command_name() & ": " & SliceTypeName(sl) & " is not a rect", serrBadOp
  END IF
 END IF
END SUB

SUB write_checkpoint ()
 'This is used for automated testing.
 ' currently just writes a screenshot,
 ' but might also dump slice tree and other stuff too in the future.
 STATIC n as integer = 0
 DIM f as string = absolute_with_orig_path("checkpoint" & right("0000" & n, 5))
 bmp_screenshot f
 n += 1
END SUB
