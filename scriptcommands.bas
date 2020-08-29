'OHRRPGCE GAME
'(C) Copyright 1997-2015 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' All script commands are implemented here, arranged in rather random order,
' including functions for the various handles and references that can be passed to script commands.

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
#include "scriptcommands.bi"
#include "yetmore2.bi"
#include "walkabouts.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmod.bi"
#include "bmodsubs.bi"

''''' Local functions
DECLARE SUB run_game ()
DECLARE FUNCTION check_game_exists () as integer
DECLARE FUNCTION get_optional_arg(byval retval_index as integer, byval default as integer) as integer
DECLARE FUNCTION get_door_on_map(byref thisdoor as Door, byval door_id as integer, byval map_id as integer) as bool
DECLARE FUNCTION allow_gmap_idx(gmap_idx as integer) as bool


''''' Global variables

'Script commands in this file need to REDIM plotslices() and timers(), but FB
'doesn't let you REDIM a global array in a module other than where it is defined!

'Using a lower bound of 1 because 0 is considered an invalid handle
'The size of 64 is just so we won't have to reallocate for a little while
REDIM plotslices(1 TO 64) as Slice Ptr
plotslicesp = @plotslices(1)

'Next slice handle to try assigning (if unused); for linear scans
DIM next_slice_handle as integer = 1   '== LBOUND(plotslices)
'Tracks the number of unassigned handles less than next_slice_handle
DIM num_reusable_slice_handles as integer

REDIM timers(numInitialTimers - 1) as PlotTimer



'==========================================================================================
'                                    Text embed codes
'==========================================================================================


SUB embedtext (text as string, byval limit as integer=0, byval saveslot as integer=-1)
'saveslot is optional. If >= 0 then that save slot will be used for reading things like hero names
 text = embed_text_codes(text, saveslot)
 '--enforce limit (if set)
 IF limit > 0 THEN
  text = LEFT(text, limit)
 END IF
END SUB

SUB embedslicetree (byval sl as Slice Ptr, byval saveslot as integer=-1)
 IF sl->SliceType = slText THEN
  DIM text as string = sl->TextData->s
  text = embed_text_codes(text, saveslot)
  ChangeTextSlice sl, text
 END IF
 DIM ch as Slice Ptr = sl->FirstChild
 DO WHILE ch
  embedslicetree ch, saveslot
  ch = ch->NextSibling
 LOOP
END SUB

FUNCTION embed_text_codes (text_in as string, byval saveslot as integer=-1, byval callback as FnEmbedCode=0, byval arg0 as ANY ptr=0, byval arg1 as ANY ptr=0, byval arg2 as ANY ptr=0) as string
' Expand embed codes like ${H0}.
' The optional callback can be passed to process additional codes.
' It should set its result string if it recognised the code, and otherwise
' leave it alone. arg0, arg1, arg2 are forwarded to it.
'saveslot is optional. If >= 0 then that save slot will be used for reading things like hero names
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
    '--evaluate standard insert actions based on the currently loaded game
    IF saveslot >= 0 THEN
     insert = saveslot_embed_codes(saveslot, act, arg)
    ELSE
     insert = standard_embed_codes(act, arg)
    END IF
    SELECT CASE UCASE(act)
     CASE "B": '--buttonname (platform-specific)
      insert = get_buttonname_code(arg)
    END SELECT
   END IF
  END IF
  IF callback <> NULL THEN
   callback(code, insert, arg0, arg1, arg2)
  END IF
  '--skip past this embed
  text = before & insert & after
  start = LEN(before) + LEN(insert) + 1
 LOOP
 RETURN text
END FUNCTION

FUNCTION standard_embed_codes(act as string, byval arg as integer) as string
 'act --- the code text. It is normally alpha only. For example, the "H" in ${H0}
 'arg --- the code argument. This is an integer. For example, the 0 in ${H0}

 '--by default the embed is unchanged
 DIM insert as string = "${" & act & arg & "}"
 SELECT CASE UCASE(act)
  CASE "H": '--Hero name by ID
   '--first search for a copy of the hero in the party
   DIM where as integer = findhero(arg)
   IF where >= 0 THEN
    insert = gam.hero(where).name
   ELSE
    insert = getheroname(arg, NO)  'Don't default to "Hero #" if blank name
   END IF
  CASE "P": '--Hero name by Party position
   IF arg >= 0 ANDALSO arg <= UBOUND(gam.hero) THEN
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
   IF where >= 0 ANDALSO where <= active_party_slots() - 1 THEN
    insert = gam.hero(where).name
   END IF
  CASE "V": '--global variable by ID
   '--defaults blank if out-of-range
   insert = ""
   IF arg >= 0 ANDALSO arg <= maxScriptGlobals THEN
    insert = STR(global(arg))
   END IF
  CASE "S": '--string variable by ID
   insert = ""
   IF in_bound(arg, 0, UBOUND(plotstr)) THEN
    insert = plotstr(arg).s
   END IF
 END SELECT
 RETURN insert
END FUNCTION

FUNCTION saveslot_embed_codes(byval saveslot as integer, act as string, byval arg as integer) as string
 'saveslot -- the save slot number that we should read values from. 0-maxSaveSlotCount-1
 'act --- the code text. It is normally alpha only. For example, the "H" in ${H0}
 'arg --- the code argument. This is an integer. For example, the 0 in ${H0}

 '--by default the embed is unchanged
 DIM insert as string = "${" & act & arg & "}"

 DIM node as NodePtr = saveslot_quick_root_node(saveslot)
 IF node = 0 THEN RETURN insert

 SELECT CASE UCASE(act)
  CASE "H": '--Hero name by ID
   '--first search for a copy of the hero in the party
   DIM where as integer = saveslot_findhero(node, arg)
   IF where >= 0 THEN
    insert = saveslot_hero_name_by_slot(node, where)
   ELSE
    insert = getheroname(arg, NO)  'Don't default to "Hero #" if blank name
   END IF
  CASE "P": '--Hero name by Party position
   '--defaults blank if not found
   '--Don't have to check validity of arg here. Don't assume UBOUND(gam.hero) is fixed.
   insert = saveslot_hero_name_by_slot(node, arg)
  CASE "C": '--Hero name by caterpillar position
   '--defaults blank if not found
   insert = ""
   DIM where as integer = saveslot_rank_to_party_slot(node, arg)
   IF where >= 0 ANDALSO where <= active_party_slots() - 1 THEN
    insert = saveslot_hero_name_by_slot(node, where)
   END IF
  CASE "V": '--global variable by ID
   '--defaults blank if out-of-range
   insert = ""
   IF arg >= 0 ANDALSO arg <= maxScriptGlobals THEN
    insert = STR(saveslot_global(node, arg))
   END IF
  CASE "S": '--string variable by ID
   '--Don't have to check validity of arg here
   insert = saveslot_plotstr(node, arg)
 END SELECT

 FreeDocument(GetDocument(node))

 RETURN insert
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
    ELSEIF percentptr[1] = ASC("o") THEN
     ' Octal
     ret &= OCT(retvals(nextarg))
    ELSEIF percentptr[1] = ASC("b") THEN
     ' Binary
     DIM temp as string = BIN(retvals(nextarg))
     ' Split into groups of 8 digits
     WHILE LEN(temp)
      DIM numbits as integer = LEN(temp) MOD 8
      IF numbits = 0 THEN numbits = 8 : ret &= " "
      ret &= LEFT(temp, numbits)
      temp = MID(temp, numbits + 1)
     WEND
    ELSEIF percentptr[1] = ASC("c") THEN
     ' Character
     IF bound_arg(retvals(nextarg), 0, 255, "%c character code", , serrBadOp) THEN
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


'==========================================================================================
'                               Battle and caterpillar party
'==========================================================================================


FUNCTION rank_to_party_slot (byval rank as integer) as integer
 'Returns the party slot of the nth hero in the party
 'This is used for converting caterpillar rank into party slot.
 'Returns -1 if there are not that many heroes in the active party
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
 FAIL_IF(gam.hero(slot).id < 0, "empty slot", -1)
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


'==========================================================================================
'                             Keypresses and script triggering
'==========================================================================================


FUNCTION script_keyval (byval key as KBScancode, byval joynum as integer = 0) as KeyBits
 'Wrapper around keyval for use by scripts: performs scancode mapping for back-compat

 IF key < scKEYVAL_FIRST ORELSE key > scKEYVAL_LAST THEN
  scripterr current_command_name() & ": invalid scancode keyval(" & key & ")", serrBound
  RETURN 0
 END IF

 DIM ret as KeyBits = 0

 IF key <= scLAST THEN
  ret = keyval(key)
 ELSE
  'keyval does this anyway, but always only checks joystick 0
  ret = joykeyval(keybd_to_joy_scancode(key), joynum)
 END IF

 IF gam.click_keys THEN
  DIM mask as integer
  SELECT CASE key
   CASE scAny    : mask = -1
   CASE ccUse    : mask = mouseLeft
   CASE ccCancel : mask = mouseRight
   CASE ccMenu   : mask = mouseRight
   'CASE ccRun
  END SELECT

  IF mask THEN
   DIM byref mouse as MouseInfo = readmouse()
   'Check for release, not click, because that's how all builtin
   'use/menu/cancel/textbox advance controls work.
   IF mouse.release AND mask THEN ret OR= 6
   IF mouse.buttons AND mask THEN ret OR= 1
  END IF
 END IF

 IF prefbit(24) = NO THEN  'If improved scancodes not enabled
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

 IF prefbit(47) = NO THEN  '!Map joystick controls to keyboard keys for scripts
  'Device -1 is all joysticks OR'd together
  SELECT CASE key
   CASE scUp:     ret OR= device_carray(ccUp, -1)
   CASE scDown:   ret OR= device_carray(ccDown, -1)
   CASE scLeft:   ret OR= device_carray(ccLeft, -1)
   CASE scRight:  ret OR= device_carray(ccRight, -1)
   CASE scEnter:  ret OR= device_carray(ccUse, -1)
   CASE scEsc:    ret OR= device_carray(ccCancel, -1)
  END SELECT
 END IF

 RETURN ret
END FUNCTION

' Trigger the on-keypress script if appropriate
SUB trigger_onkeypress_script ()
 DIM doit as bool = NO

 'Checks whether keyboard and joystick keys are down, and optionally the mouse
 '(we check keys are down, not whether they're held/released.)
 '(Mouse is checked if "init mouse" has been run at least once, or if one of these settings enabled)
 DIM checkmouse as bool = gam.mouse_enabled
 gam.click_keys = get_gen_bool("/mouse/click_keys")
 IF gam.click_keys ORELSE get_gen_bool("/mouse/move_hero") THEN checkmouse = YES
 IF anykeypressed(YES, checkmouse, 1) THEN doit = YES  'trigger_level=1

 'Because anykeypressed doesn't check it, and we don't want to break scripts looking for key:alt (== scUnfilteredAlt)
 IF keyval(scUnfilteredAlt) > 0 THEN doit = YES

 IF nowscript >= 0 THEN
  IF scriptinsts(nowscript).waiting = waitingOnCmd AND scriptinsts(nowscript).curvalue = 9 THEN
   '--never trigger a onkey script when the previous script
   '--has a "wait for key" command active
   doit = NO
  END IF
 END IF

 IF doit THEN
  DIM trigger as integer = trigger_or_default(gmap(15), gen(genDefOnKeypressScript))
  IF trigger THEN
   trigger_script trigger, 1, YES, "on-key", "", mainFibreGroup
  END IF
 END IF
END SUB


'==========================================================================================
'                                      Wait Commands
'==========================================================================================


' Implementations of 'wait' commands.
SUB process_wait_conditions()
 WITH scriptinsts(nowscript)

   ' Evaluate wait conditions, even if the fibre is paused (unimplemented),
   ' as waiting for unpause first will just lead to bugs eg. due to map changes
   ' (Note however that is the way the old one-script-at-a-time mode works: wait
   ' conditions not considered until its turn to run)

   IF .waiting = waitingOnTick THEN
    .waitarg -= 1
    IF .waitarg <= 0 THEN script_stop_waiting()
    EXIT SUB
   END IF

   SELECT CASE .curvalue
    CASE 15, 35, 61, 76'--use door, use NPC, teleport to map, fade screen in
     script_stop_waiting()

    CASE 16'--fight formation
     script_stop_waiting(IIF(gam.wonbattle, 1, 0))

    CASE 1'--wait number of ticks
     .waitarg -= 1
     IF .waitarg < 1 THEN
      script_stop_waiting()
     END IF

    CASE 2'--wait for all
     DIM unpause as bool = YES
     FOR i as integer = 0 TO 3
      IF herow(i).xgo <> 0 ORELSE herow(i).ygo <> 0 ORELSE hero_is_pathfinding(i) THEN unpause = NO
     NEXT i
     IF readbit(gen(), genSuspendBits, suspendnpcs) = 1 THEN
      FOR i as integer = 0 TO UBOUND(npc)
       IF npc(i).id > 0 ANDALSO (npc(i).xgo <> 0 OR npc(i).ygo <> 0) THEN unpause = NO: EXIT FOR
      NEXT i
     END IF
     FOR i as integer = 0 TO UBOUND(npc)
      'check for script initiated NPC pathing even when npcs are not suspended
      IF npc(i).id > 0 ANDALSO npc(i).pathover.override THEN unpause = NO: EXIT FOR
     NEXT i
     IF gen(genCameraMode) = pancam ORELSE gen(genCameraMode) = focuscam THEN unpause = NO
     IF unpause THEN
      script_stop_waiting()
     END IF

    CASE 3'--wait for hero
     IF .waitarg < 0 OR .waitarg > 3 THEN
      scripterr "waiting for nonexistant hero " & .waitarg, serrBug  'should be bound by waitforhero
      script_stop_waiting()
     ELSE
      IF herow(.waitarg).xgo = 0 ANDALSO herow(.waitarg).ygo = 0 ANDALSO NOT hero_is_pathfinding(.waitarg) THEN
       script_stop_waiting()
      END IF
     END IF

    CASE 4'--wait for NPC
     DIM npcref as NPCIndex = getnpcref(.waitarg, 0)
     IF npcref >= 0 ANDALSO .waitarg2 = gam.map.id THEN
      IF npc(npcref).xgo = 0 ANDALSO npc(npcref).ygo = 0 ANDALSO npc(npcref).pathover.override = NO THEN
       script_stop_waiting()
      END IF
     ELSE
      '--no reference found, why wait for a non-existant npc?
      script_stop_waiting()
     END IF

    CASE 9, 244'--wait for key, wait for scancode
     IF txt.showing ANDALSO use_touch_textboxes() THEN
      'If a touch textbox is currently being displayed, we make a special
      'exception and treat any touch as the key we are waiting for
      IF readmouse().release AND mouseLeft THEN
       script_stop_waiting()
       EXIT SUB
      END IF
     ELSE
      a_script_wants_keys()
     END IF
     IF .waitarg <> scAny THEN
      IF script_keyval(.waitarg) > 1 THEN
       script_stop_waiting()
       EXIT SUB
      END IF

      'Because carray(ccMenu) doesn't include it, and we don't want to break scripts
      'doing waitforkey(menu key) followed by looking for key:alt (== scUnfilteredAlt)
      IF .waitarg = ccMenu AND keyval(scUnfilteredAlt) > 1 THEN script_stop_waiting()
     ELSE
      '.waitarg == scAny
      DIM temp as KBScancode = anykeypressed(YES, gam.click_keys)  'check joystick, maybe mouse
      'Because anykeypressed doesn't check it, and we don't want to break scripts
      'doing waitforkey(any key) followed by looking for key:alt (== scUnfilteredAlt)
      IF keyval(scUnfilteredAlt) > 1 THEN temp = scUnfilteredAlt
      IF temp THEN
       script_stop_waiting(temp)
      END IF
     END IF

    CASE 42'--wait for camera
     IF gen(genCameraMode) <> pancam ANDALSO gen(genCameraMode) <> focuscam THEN script_stop_waiting()

    CASE 59'--wait for text box
     IF txt.showing = NO OR readbit(gen(), genSuspendBits, suspendboxadvance) = 1 THEN
      script_stop_waiting()
     END IF

    CASE 73, 234, 438'--game over, quit from loadmenu, reset game

    CASE 508'--wait for slice
     IF valid_plotslice(.waitarg, serrWarn) THEN
      IF plotslices(.waitarg)->Velocity.X = 0 ANDALSO plotslices(.waitarg)->Velocity.Y = 0 ANDALSO plotslices(.waitarg)->TargTicks = 0 THEN
       script_stop_waiting()
      END IF
     ELSE
      'If the slice ceases to exist, we should stop waiting for it (after throwing our minor warning)
      script_stop_waiting()
     END IF

    CASE 575'--wait for dissolve
     IF valid_plotslice(.waitarg, serrWarn) THEN
      IF NOT SpriteSliceIsDissolving(plotslices(.waitarg), YES) THEN
       script_stop_waiting()
      END IF
     ELSE
      'If the slice ceases to exist, we should stop waiting for it (after throwing our minor warning)
      script_stop_waiting()
     END IF

    CASE ELSE
     scripterr "illegal wait substate " & .curvalue, serrBug
     script_stop_waiting()
   END SELECT

 END WITH
END SUB


'==========================================================================================
'                                      Script Commands
'==========================================================================================

'DIM SHARED used_script_commands(maxScriptCmdID) as bool

' This entry point is called from the script interpreter.
SUB script_functions(byval cmdid as integer)
 'These variables are uninitialised for speed
 DIM menuslot as integer = ANY
 DIM mislot as integer = ANY
 DIM npcref as NPCIndex = ANY
 DIM mi as MenuDefItem ptr = ANY
 DIM i as integer = ANY
 scriptret = 0

 'IF cmdid <= maxScriptCmdID ANDALSO used_script_commands(cmdid) = NO THEN
 ' debug "Used command " & cmdid & " " & commandname(cmdid)
 ' used_script_commands(cmdid) = YES
 'END IF

 SELECT CASE as CONST cmdid

 CASE 11'--show textbox (box)
  'showtextbox(0) does nothing
  gam.want.box = large(0, retvals(0))
  IF immediate_showtextbox ANDALSO gam.want.box > 0 THEN loadsay gam.want.box: gam.want.box = 0
 CASE 15'--use door
  gam.want.door = retvals(0) + 1
  gam.want.door_fadescreen = get_optional_arg(1, 1) <> 0
  script_start_waiting(0)
 CASE 16'--fight formation
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) THEN
   gam.want.battle = retvals(0) + 1
   script_start_waiting(0)
  ELSE
   scriptret = -1
  END IF
 CASE 23'--unequip
  IF valid_hero_party(retvals(0)) THEN
   unequip retvals(0), bound(retvals(1) - 1, 0, 4)
  END IF
 CASE 24'--force equip
  IF valid_hero_party(retvals(0)) THEN
   i = retvals(0)
   IF valid_item(retvals(2)) THEN
    doequip retvals(2), i, bound(retvals(1) - 1, 0, 4)
   END IF
  END IF
 CASE 32'--show backdrop
  gen(genScrBackdrop) = bound(retvals(0) + 1, 0, gen(genNumBackdrops))
 CASE 33'--show map
  gen(genScrBackdrop) = 0
 CASE 34'--dismount vehicle
  forcedismount
 CASE 35'--use NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   gam.want.usenpc = npcref + 1
   script_start_waiting()
  END IF
 CASE 37'--use shop
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxShop) THEN
   shop retvals(0)
  END IF
 CASE 55'--get default weapon
  IF valid_hero_party(retvals(0)) THEN
   scriptret = gam.hero(retvals(0)).def_wep - 1
  ELSE
   scriptret = 0
  END IF
 CASE 56'--set default weapon
  IF valid_hero_party(retvals(0)) THEN
   IF valid_item(retvals(1)) THEN
    '--identify new default weapon
    DIM as integer newdfw = retvals(1)
    '--remember old default weapon
    DIM as integer olddfw = gam.hero(retvals(0)).def_wep - 1
    '--remember currently equipped weapon
    DIM as integer cureqw = gam.hero(retvals(0)).equip(0).id
    '--change default
    gam.hero(retvals(0)).def_wep = newdfw + 1
    IF cureqw <> olddfw THEN
     '--if previously using a weapon, re-equip old weapon
     doequip cureqw, retvals(0), 0
    ELSE
     '--otherwize equip new default weapon
     doequip newdfw, retvals(0), 0
    END IF
   END IF
  END IF
 CASE 61'--teleport to map
  IF valid_map(retvals(0)) THEN
   gam.map.id = retvals(0)
   (herox(0)) = retvals(1) * 20
   (heroy(0)) = retvals(2) * 20
   gam.want.teleport = YES
   script_start_waiting(0)
  END IF
 CASE 63, 169'--resume random enemies
  setbit gen(), genSuspendBits, suspendrandomenemies, 0
  gam.random_battle_countdown = range(100, 60)
 CASE 73'--game over
  gam.quit = YES
  script_start_waiting()
 CASE 77'--show value
  gam.showstring = STR(retvals(0))
 CASE 78'--alter NPC
  IF bound_arg(retvals(1), 0, maxNPCDataField, "NPCstat: constant") THEN
   DIM pool as integer = get_optional_arg(3, -1)
   IF retvals(0) < 0 AND pool <> -1 THEN
    scripterr current_command_name() & ": npc pool argument is only allowed when using an NPC ID, but an NPC reference was used"
   ELSE
    DIM npcid as NPCTypeID = get_valid_npc_id(retvals(0), serrBound, IIF(pool=-1,0,pool))
    IF npcid <> -1 THEN
     IF pool = -1 THEN
      IF retvals(0) < 0 THEN
       'Was specified as a reference, We want whatever NPC pool the NPC actually belongs to
       DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBound, 0)
       pool = npc(npcref).pool
      ELSE
       'A pool wasn't specified, assume the ID was a local ID
       pool = 0
      END IF
     END IF
     DIM write_value as bool = YES
     IF retvals(1) = 0 THEN  'NPCstat:picture
      IF retvals(2) < 0 OR retvals(2) > gen(genMaxNPCPic) THEN
       write_value = NO
      ELSE
       change_npc_def_sprite npcid, retvals(2), pool
      END IF
     END IF
     IF retvals(1) = 1 THEN  'NPCstat:palette
      change_npc_def_pal npcid, retvals(2), pool
     END IF
     'Shouldn't we check validity of retvals(2) for other data?
     IF write_value THEN SetNPCD(npool(pool).npcs(npcid), retvals(1), retvals(2))
     lump_reloading.npcd.dirty = YES
    END IF
   END IF
  END IF
 CASE 79'--show no value
  gam.showstring = ""
 CASE 80'--current map
  scriptret = gam.map.id
 CASE 86'--advance text box
  advance_text_box
 CASE 97'--read map block
  retvals(2) = get_optional_arg(2, 0)
  IF retvals(2) >= 0 AND retvals(2) <= UBOUND(maptiles) THEN
   scriptret = readblock(maptiles(retvals(2)), bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1), 0)
  END IF
 CASE 98'--write map block
  retvals(3) = get_optional_arg(3, 0)
  IF retvals(3) >= 0 AND retvals(3) <= UBOUND(maptiles) AND retvals(2) >= 0 AND retvals(2) <= 255 THEN
   writeblock maptiles(retvals(3)), bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1), retvals(2)
   lump_reloading.maptiles.dirty = YES
  END IF
 CASE 99'--read pass block
  scriptret = readblock(pass, bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1), 0)
 CASE 100'--write pass block
  'pass isn't known to be the same size as mapsizetiles
  writeblock pass, bound(retvals(0), 0, pass.wide - 1), bound(retvals(1), 0, pass.high - 1), bound(retvals(2), 0, 255)
  lump_reloading.passmap.dirty = YES
 CASE 144'--load tileset(tileset, map layer) or load tileset(tileset) or load tileset()
  'Unlike "change tileset", doesn't modify gmap
  IF retvals(0) <= gen(genMaxTile) THEN
   IF get_optional_arg(1, -1) < 0 THEN
    IF retvals(0) < 0 THEN
     'Reload all layers back to tilesets defined in gmap(), try to preserve animation states
     loadmaptilesets tilesets(), gmap(), NO
    ELSE
     'Change default tileset. Scan for layers set to use default.
     FOR i = 0 TO mapLayerMax
      IF gmap(layer_tileset_index(i)) = 0 THEN loadtilesetdata tilesets(), i, retvals(0)
     NEXT
    END IF
   ELSEIF valid_map_layer(retvals(1), serrWarn) AND retvals(0) >= 0 THEN
    'Load different tileset for an individual layer.
    loadtilesetdata tilesets(), retvals(1), retvals(0)
   END IF
   'Important to refresh map slices regardless of how the tileset was changed
   refresh_map_slice_tilesets
  END IF
 CASE 305'--change tileset(tileset, layer) or change tileset(tileset) or change tileset()
  'Unlike "load tileset" this modifies gmap() for persistent (given map state saving) effects
  IF retvals(0) <= gen(genMaxTile) THEN
   IF retvals(1) < 0 THEN
    IF retvals(0) < 0 THEN
     'Reset all tilesets changes made with "load tileset", by reloading from gmap()
     'Does NOT reset changes made by "change tileset".
    ELSE
     'Change default tileset
     gmap(0) = retvals(0)
    END IF
   ELSEIF valid_map_layer(retvals(1), serrWarn) THEN
    'Change tileset for an individual layer (-1 changes it to default tilesets)
    gmap(layer_tileset_index(retvals(1))) = large(0, retvals(0) + 1)
   END IF
   lump_reloading.maptiles.dirty = YES  'Tilesets are treated as part of tilemap data, not gmap
   'load while trying to preserve animation states
   loadmaptilesets tilesets(), gmap(), NO
   refresh_map_slice_tilesets
  END IF
 CASE 151'--show mini map
  stop_fibre_timing
  minimap heropos(0)
  start_fibre_timing
 CASE 153'--items menu
  stop_fibre_timing
  gam.want.box = item_screen()
  IF gam.want.box ANDALSO immediate_showtextbox THEN loadsay gam.want.box: gam.want.box = 0
  start_fibre_timing
 CASE 155, 170'--save menu
  'ID 155 is a backcompat hack
  stop_fibre_timing
  scriptret = picksave() + 1
  IF scriptret = -1 THEN scriptret = 0  'Cancelled/Exit
  IF scriptret > 0 AND (retvals(0) OR cmdid = 155) THEN
   savegame scriptret - 1
  END IF
  start_fibre_timing
 CASE 166'--save in slot
  IF valid_save_slot(retvals(0)) THEN
   savegame retvals(0) - 1
  END IF
 CASE 167'--last save slot
  scriptret = lastsaveslot
 CASE 174'--load from slot(slot, args...)
  IF curcmd->argc = 0 THEN
   scripterr "load from slot: Expected save slot as argument"
  ELSEIF valid_save_slot(retvals(0)) THEN
   IF save_slot_used(retvals(0) - 1) THEN
    gam.want.loadgame = retvals(0)
    ' Save extra args
    REDIM gam.want.script_args(-1 TO curcmd->argc - 2)
    FOR i as integer = 1 TO curcmd->argc - 1
     gam.want.script_args(i - 1) = retvals(i)
    NEXT
    script_start_waiting()
   END IF
  END IF
 CASE 210'--show string
  IF valid_plotstr(retvals(0)) THEN
   gam.showstring = plotstr(retvals(0)).s
  END IF
 CASE 234'--load menu (reallyload, show new game)
  stop_fibre_timing
  ' Originally only had one argument; 'show new game' should default to true
  retvals(1) = get_optional_arg(1, 1) <> 0
  scriptret = pickload(retvals(1)) + 1
  IF retvals(0) THEN
   'Enact whatever the user picked
   IF scriptret = -1 THEN
    'Cancelled/Exit
    gam.quit = YES
    gam.want.dont_quit_to_loadmenu = YES  'don't go straight back to loadmenu!
    script_start_waiting()
    fadeout uilook(uiFadeoutNewGame)
   ELSEIF scriptret > 0 THEN
    gam.want.loadgame = scriptret
    script_start_waiting()
  'ELSEIF scriptret = 0 THEN  'New Game/no saves available (menu skipped)
    'Do nothing, script continues
   END IF
  END IF
  start_fibre_timing
 CASE 245'--save map state
  IF retvals(1) > -1 AND retvals(1) <= 31 THEN
   savemapstate_bitmask retvals(1), retvals(0), "state"
  ELSEIF retvals(1) = 255 THEN
   savemapstate_bitmask gam.map.id, retvals(0), "map"
  END IF
 CASE 246'--load map state
  IF retvals(1) > -1 AND retvals(1) <= 31 THEN
   loadmapstate_bitmask retvals(1), retvals(0), "state", YES  'dontfallback=YES
  ELSEIF retvals(1) = 255 THEN
   loadmapstate_bitmask gam.map.id, retvals(0), "map"
  END IF
 CASE 247'--reset map state
  loadmap_bitmask gam.map.id, retvals(0)
 CASE 248'--delete map state
  deletemapstate gam.map.id, retvals(0), "map"
 CASE 253'--set tile animation offset
  retvals(2) = get_optional_arg(2, 0)
  IF (retvals(0) = 0 OR retvals(0) = 1) AND valid_map_layer(retvals(2), serrBound) THEN
   tilesets(retvals(2))->anim(retvals(0)).cycle = retvals(1) MOD 160
  END IF
 CASE 254'--get tile animation offset
  retvals(1) = get_optional_arg(1, 0)
  IF (retvals(0) = 0 OR retvals(0) = 1) AND valid_map_layer(retvals(1), serrBound) THEN
   scriptret = tilesets(retvals(1))->anim(retvals(0)).cycle
  END IF
 CASE 255'--animation start tile
  retvals(1) = get_optional_arg(1, 0)
  IF (retvals(0) >= 0 AND retvals(0) < 256) AND valid_map_layer(retvals(1), serrBound) THEN
   scriptret = tile_anim_deanimate_tile(retvals(0), tilesets(retvals(1))->tastuf())
  END IF
 CASE 258'--check hero wall
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   DIM as integer tempxgo = 0, tempygo = 0
   IF retvals(1) = 0 THEN tempygo = 20
   IF retvals(1) = 1 THEN tempxgo = -20
   IF retvals(1) = 2 THEN tempygo = -20
   IF retvals(1) = 3 THEN tempxgo = 20
   scriptret = wrappass(herotx(retvals(0)), heroty(retvals(0)), tempxgo, tempygo, 0)
  END IF
 CASE 259'--check NPC wall
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   'Only check walls for NPC who actually exists
   DIM as integer tempxgo = 0, tempygo = 0
   IF retvals(1) = 0 THEN tempygo = 20
   IF retvals(1) = 1 THEN tempxgo = -20
   IF retvals(1) = 2 THEN tempygo = -20
   IF retvals(1) = 3 THEN tempxgo = 20
   scriptret = wrappass(npc(npcref).x \ 20, npc(npcref).y \ 20, tempxgo, tempygo, 0)
  END IF
 CASE 267'--main menu
  scriptret = add_menu(0)
 CASE 274'--open menu
  IF bound_arg(retvals(0), 0, gen(genMaxMenu), "menu ID") THEN
   scriptret = add_menu(retvals(0), (retvals(1) <> 0))
  END IF
 CASE 275'--read menu int
  IF valid_menu_handle(retvals(0), menuslot) THEN
   scriptret = read_menu_int(menus(menuslot), retvals(1))
  END IF
 CASE 276'--write menu int
  IF valid_menu_handle(retvals(0), menuslot) THEN
   write_menu_int(menus(menuslot), retvals(1), retvals(2))
   mstates(menuslot).need_update = YES
  END IF
 CASE 277'--read menu item int
  IF valid_menu_item_handle_ptr(retvals(0), mi) THEN
   scriptret = read_menu_item_int(*mi, retvals(1))
  END IF
 CASE 278'--write menu item int
  IF valid_menu_item_handle_ptr(retvals(0), mi, menuslot) THEN
   write_menu_item_int(*mi, retvals(1), retvals(2))
   mstates(menuslot).need_update = YES
  END IF
 CASE 279'--create menu
  scriptret = add_menu(-1)
  menus(topmenu).allow_gameplay = YES
 CASE 280'--close menu (menu, run close script)
  IF valid_menu_handle(retvals(0), menuslot) THEN
   remove_menu menuslot, get_optional_arg(1, NO) <> 0
  END IF
 CASE 281'--top menu
  IF topmenu >= 0 THEN
   scriptret = menus(topmenu).handle
  END IF
 CASE 282'--bring menu forward
  IF valid_menu_handle(retvals(0), menuslot) THEN
   bring_menu_forward menuslot
  END IF
 CASE 283'--add menu item
  IF valid_menu_handle(retvals(0), menuslot) THEN
   append_menu_item(menus(menuslot), "")
   scriptret = assign_menu_item_handle(*menus(menuslot).last)
   mstates(menuslot).need_update = YES
  END IF
 CASE 284'--delete menu item
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   remove_menu_item menus(menuslot), mislot
   mstates(menuslot).need_update = YES
  END IF
 CASE 285'--get menu item caption
  IF valid_menu_item_handle_ptr(retvals(0), mi, menuslot) THEN
   IF valid_plotstr(retvals(1)) THEN
    plotstr(retvals(1)).s = get_menu_item_caption(*mi, menus(menuslot))
   END IF
  END IF
 CASE 286'--set menu item caption
  IF valid_menu_item_handle_ptr(retvals(0), mi) THEN
   IF valid_plotstr(retvals(1)) THEN
    mi->caption = plotstr(retvals(1)).s
   END IF
  END IF
 CASE 287'--get level mp
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, maxMPLevel, "mp level") THEN
    retvals(2) = get_optional_arg(2, 0)
    IF retvals(2) = 0 THEN
     'Current stat
     scriptret = gam.hero(retvals(0)).levelmp(retvals(1))
    ELSEIF retvals(2) = 1 THEN
     'Maximum stat
     DIM levelmp(maxMPLevel) as integer
     get_max_levelmp(levelmp(), gam.hero(retvals(0)).lev)
     scriptret = levelmp(retvals(1))
    ELSE
     scripterr "getlevelmp: stat type should be currentstat (0) or maximumstat (1), not " & retvals(2), serrBadOp
    END IF
   END IF
  END IF
 CASE 288'--set level mp
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, maxMPLevel, "mp level") THEN
    gam.hero(retvals(0)).levelmp(retvals(1)) = retvals(2)
   END IF
  END IF
 CASE 289'--bottom menu
  IF topmenu >= 0 THEN
   scriptret = menus(0).handle
  END IF
 CASE 290'--previous menu
  IF valid_menu_handle(retvals(0), menuslot) THEN
   menuslot = menuslot - 1
   IF menuslot >= 0 THEN
    scriptret = menus(menuslot).handle
   END IF
  END IF
 CASE 291'--next menu
  IF valid_menu_handle(retvals(0), menuslot) THEN
   menuslot = menuslot + 1
   IF menuslot <= topmenu THEN
    scriptret = menus(menuslot).handle
   END IF
  END IF
 CASE 292'--menu item by slot
  IF valid_menu_handle(retvals(0), menuslot) THEN
   scriptret = menu_item_handle_by_slot(menuslot, retvals(1), retvals(2)<>0)
  END IF
 CASE 293'--previous menu item
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   scriptret = menu_item_handle_by_slot(menuslot, mislot - 1, retvals(1)<>0)
  END IF
 CASE 294'--next menu item
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   scriptret = menu_item_handle_by_slot(menuslot, mislot + 1, retvals(1)<>0)
  END IF
 CASE 295'--selected menu item
  IF retvals(0) = -1 THEN
   IF topmenu >= 0 THEN
    scriptret = menu_item_handle_by_slot(topmenu, mstates(topmenu).pt)
   END IF
  ELSE
   IF valid_menu_handle(retvals(0), menuslot) THEN
    scriptret = menu_item_handle_by_slot(menuslot, mstates(menuslot).pt)
   END IF
  END IF
 CASE 296'--select menu item
  IF valid_menu_item_handle_ptr(retvals(0), mi, menuslot, mislot) THEN
   update_menu_item *mi
   'Note: you can select hidden items!
   'After a tick, the selection should get moved to a valid item.
   IF mi->unselectable THEN
    'scripterr "Can't select unselectable menu item", serrInfo
   ELSE
    mstates(menuslot).pt = mislot
    mstates(menuslot).need_update = YES
    scriptret = 1
   END IF
  END IF
 CASE 297'--parent menu
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   scriptret = menus(menuslot).handle
  END IF
 CASE 298'--get menu ID
  IF valid_menu_handle(retvals(0), menuslot) THEN
   scriptret = menus(menuslot).record
  END IF
 CASE 299'--swap menu items
  DIM as integer menuslot2, mislot2
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   IF valid_menu_item_handle(retvals(1), menuslot2, mislot2) THEN
    swap_menu_items menus(menuslot), mislot, menus(menuslot2), mislot2
    mstates(menuslot).need_update = YES
    mstates(menuslot2).need_update = YES
   END IF
  END IF
 CASE 300'--find menu item caption
  IF valid_plotstr(retvals(1)) THEN
   IF valid_menu_handle(retvals(0), menuslot) THEN
    DIM start_slot as integer
    IF retvals(2) = 0 THEN
     start_slot = 0
    ELSE
     DIM menuslot2 as integer
     IF valid_menu_item_handle(retvals(2), menuslot2, start_slot) = NO THEN
      start_slot = -1
     ELSEIF menuslot2 <> menuslot THEN
      start_slot = -1
      scripterr "find menu item caption: 'search after' menu item doesn't belong to the same menu"
     ELSE
      start_slot += 1
     END IF
    END IF
    IF start_slot >= 0 THEN
     mislot = find_menu_item_slot_by_string(menuslot, plotstr(retvals(1)).s, start_slot, (retvals(3) <> 0))
     IF mislot >= 0 THEN scriptret = menus(menuslot).items[mislot]->handle
    END IF
   END IF
  END IF
 CASE 301'--find menu ID
  IF bound_arg(retvals(0), 0, gen(genMaxMenu), "menu ID") THEN
   menuslot = find_menu_id(retvals(0))
   IF menuslot >= 0 THEN
    scriptret = menus(menuslot).handle
   ELSE
    scriptret = 0
   END IF
  END IF
 CASE 302'--menu is open
  menuslot = find_menu_handle(retvals(0))
  IF menuslot = -1 THEN
   scriptret = 0
  ELSE
   scriptret = 1
  END IF
 CASE 303'--menu item slot
  IF valid_menu_item_handle(retvals(0), menuslot, mislot) THEN
   scriptret = mislot
  END IF
 CASE 304'--outside battle cure
  'WARNING: This exists for backcompat, but "map cure" should be prefered.
  'See bug 719
  IF bound_arg(retvals(0), 0, gen(genMaxAttack), "attack ID") THEN
   IF valid_hero_party(retvals(1)) THEN
    IF valid_hero_party(retvals(2), -1) THEN
     scriptret = ABS(outside_battle_cure(retvals(0), retvals(1), retvals(2), NO))
    END IF
   END IF
  END IF
 CASE 306'--layer tileset
  IF valid_map_layer(retvals(0), serrBound) THEN
   scriptret = tilesets(retvals(0))->num
  END IF
 CASE 320'--current text box
  scriptret = -1
  IF txt.showing = YES THEN scriptret = txt.id
  IF immediate_showtextbox = NO ANDALSO gam.want.box > 0 THEN scriptret = gam.want.box
 CASE 432 '--use menu item
  IF valid_menu_item_handle_ptr(retvals(0), mi, menuslot) THEN
   activate_menu_item(*mi, menuslot)
  END IF
 CASE 438 '--reset game
  gam.want.resetgame = YES
  REDIM gam.want.script_args(-1 TO curcmd->argc - 1)
  FOR i as integer = 0 TO curcmd->argc - 1  'flexible argument number!
   gam.want.script_args(i) = retvals(i)
  NEXT
  script_start_waiting()
 CASE 490'--use item (id)
  scriptret = 0
  IF valid_item(retvals(0)) THEN
   IF use_item_by_id(retvals(0), gam.want.box) THEN
    scriptret = 1
   END IF
   IF immediate_showtextbox ANDALSO gam.want.box > 0 THEN loadsay gam.want.box: gam.want.box = 0
  END IF
 CASE 491'--use item in slot (slot)
  scriptret = 0
  IF valid_item_slot(retvals(0)) THEN
   IF use_item_in_slot(retvals(0), gam.want.box) THEN
    scriptret = 1
   END IF
   evalitemtags
   IF immediate_showtextbox ANDALSO gam.want.box > 0 THEN loadsay gam.want.box: gam.want.box = 0
  END IF
 CASE 517'--menu item by true slot
  IF valid_menu_handle(retvals(0), menuslot) THEN
   mi = dlist_nth(menus(menuslot).itemlist, retvals(1))
   IF mi THEN
    scriptret = mi->handle
   ELSE
    scriptret = 0
   END IF
  END IF
 CASE 518'--menu item true slot
  IF valid_menu_item_handle_ptr(retvals(0), mi, menuslot) THEN
   scriptret = dlist_find(menus(menuslot).itemlist, mi)
   IF scriptret < 0 THEN scripterr "menuitemtrueslot: dlist corruption", serrBug
  END IF
 CASE 619'--menu item at pixel
  FOR menuslot = topmenu TO 0 STEP -1
   mislot = find_menu_item_at_point(mstates(menuslot), retvals(0), retvals(1))
   IF mislot >= mstates(menuslot).first THEN
    scriptret = menu_item_handle_by_slot(menuslot, mislot)
    EXIT FOR
   END IF
  NEXT
 CASE 623, 624'--check wall collision x/y (pixel x, pixel y, width, height, xgo, ygo, friction)
  ' It's fine for X/Y to be over the map edge, whether wrapping or not.
  IF retvals(2) < 0 OR retvals(3) < 0 THEN
   scripterr current_command_name() & ": negative width or height not allowed"
  ELSE
   DIM friction as integer = bound(get_optional_arg(6, 100), 0, 100)
   DIM as XYPair startpos = (retvals(0), retvals(1)), pos
   sliding_wallmap_collision(startpos, pos, XY(retvals(2), retvals(3)), XY(retvals(4), retvals(5)), NO, YES, friction)
   IF cmdid = 623 THEN
    scriptret = pos.x - startpos.x
   ELSE
    scriptret = pos.y - startpos.y
   END IF
  END IF
 CASE 625'--move slice with wallchecking (sl, xgo, ygo, friction)
  IF valid_plotslice(retvals(0)) THEN
   DIM friction as integer = bound(get_optional_arg(3, 100), 0, 100)
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   WITH *sl
    ' This will work regardless of what the slice is parented to.
    DIM as XYPair startpos = .ScreenPos + XY(mapx, mapy), pos
    scriptret = sliding_wallmap_collision(startpos, pos, .Size, XY(retvals(1), retvals(2)), NO, YES, friction)
    .Pos += pos - startpos
   END WITH
  END IF
 CASE 671 '--menu item selectable
  IF valid_menu_item_handle_ptr(retvals(0), mi) THEN
   update_menu_item *mi
   scriptret = IIF(mi->visible ANDALSO NOT mi->unselectable, 1, 0)
  END IF
 CASE 672 '--menu item disabled
  IF valid_menu_item_handle_ptr(retvals(0), mi) THEN
   update_menu_item *mi
   scriptret = IIF(mi->disabled, 1, 0)
  ELSE
   scriptret = 1
  END IF
 CASE 673 '--menu item visible
  IF valid_menu_item_handle_ptr(retvals(0), mi) THEN
   update_menu_item *mi
   scriptret = IIF(mi->visible, 1, 0)
  ELSE
   scriptret = 1
  END IF

 CASE 135'--put hero
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   cropposition retvals(1), retvals(2), 20
   (herox(retvals(0))) = retvals(1)
   (heroy(retvals(0))) = retvals(2)
  END IF
 CASE 136'--put npc
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   cropposition retvals(1), retvals(2), 20
   npc(npcref).x = retvals(1)
   npc(npcref).y = retvals(2)
  END IF
 CASE 137'--put camera
  gen(genCameraMode) = stopcam
  mapx = retvals(0)
  mapy = retvals(1)
  limitcamera mapx, mapy
 CASE 138'--hero pixel x
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = herox(retvals(0))
  END IF
 CASE 139'--hero pixel y
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = heroy(retvals(0))
  END IF
 CASE 140'--npc pixel x
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).x
  END IF
 CASE 141'--npc pixel y
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).y
  END IF
 CASE 142'--camera pixel x
  scriptret = mapx
 CASE 143'--camera pixel y
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
  gam.mouse_enabled = YES
 CASE 160'--mouse pixel x
  scriptret = readmouse().x
 CASE 161'--mouse pixel y
  scriptret = readmouse().y
 CASE 162'--mouse button
  IF retvals(0) <= 31 THEN
   IF readmouse().buttons AND (2 ^ retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 163'--put mouse
  movemouse bound(retvals(0), 0, get_resolution().w - 1), bound(retvals(1), 0, get_resolution().h - 1)
 CASE 164'--mouse region(xmin, xmax, ymin, ymax)
  IF retvals(0) = -1 AND retvals(1) = -1 AND retvals(2) = -1 AND retvals(3) = -1 THEN
   mouserect -1, -1, -1, -1
  ELSE
   retvals(0) = bound(retvals(0), 0, get_resolution().w - 1)
   retvals(1) = bound(retvals(1), retvals(0), get_resolution().w - 1)
   retvals(2) = bound(retvals(2), 0, get_resolution().h - 1)
   retvals(3) = bound(retvals(3), retvals(2), get_resolution().h - 1)
   mouserect retvals(0), retvals(1), retvals(2), retvals(3)
  END IF
 CASE 178'--read gmap
  'Don't support reading most gmap indices
  IF allow_gmap_idx(retvals(0)) THEN
   scriptret = gmap(retvals(0))
  END IF
 CASE 179'--write gmap
  'Don't support changing most gmap indices
  IF allow_gmap_idx(retvals(0)) THEN
   gmap(retvals(0)) = retvals(1)
   IF retvals(0) = 2 OR retvals(0) = 3 THEN update_menu_items  'save and minimap menu options
   IF retvals(0) = 4 THEN gam.showtext_ticks = 0  'cancel map name display
   IF retvals(0) = 16 THEN refresh_walkabout_layer_sort()
   IF retvals(0) = 19 THEN refresh_map_slice() 'map layer visibility
   'If changing gmap(31) were allowed (position of walkabout layer), would also need to call refresh_map_slice
   lump_reloading.gmap.dirty = YES
  END IF
 CASE 492'--mouse click
  IF retvals(0) <= 4 THEN
   IF readmouse().clicks AND (2 ^ retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 646'--mouse release
  IF retvals(0) <= 4 THEN
   IF readmouse().release AND (2 ^ retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 4'--wait for NPC
  IF retvals(0) >= -300 AND retvals(0) <= UBOUND(npool(0).npcs) THEN
   script_start_waiting(retvals(0), gam.map.id)
  END IF
 CASE 5'--suspend npcs
  setbit gen(), genSuspendBits, suspendnpcs, 1
 CASE 6'--suspend player
  setbit gen(), genSuspendBits, suspendplayer, 1
 CASE 7'--resume npcs
  setbit gen(), genSuspendBits, suspendnpcs, 0
 CASE 8'--resume player
  setbit gen(), genSuspendBits, suspendplayer, 0
 CASE 9, 244'--wait for key, wait for scancode
  'waitforkey used to take constants upkey, usekey, etc, which had values 0-5,
  'but if scripts_use_cc_scancodes is true, these constants now have values <= -1,
  'and can be passed to keyval.
  DIM scancode as KBScancode = retvals(0)
  IF cmdid = 9 ANDALSO scripts_use_cc_scancodes = NO THEN
   'Backcompat: constants >= 0 are 'usekey', etc, not key:.../sc... scancodes
   IF scancode >= 0 THEN
    IF scancode = 99 THEN
     scancode = scAny
    ELSEIF scancode > 5 THEN
     'Invalid keycode! Probably used a scancode instead of a *key constant.
     'This acts like anykey.
     scancode = scAny
     'Not much point showing an error, because if you recompile the problem will
     'go away! However, the behaviour will change, so better warn people.
     scripterr "You wrongly passed a scancode (eg. key:space) to the 'wait for key' command. This was equivalent to 'anykey', but 'wait for key' now genuinely supports scancodes. if you reimport your scripts, this will wait for that key instead of for anykey.", serrBadOp
    ELSE
     scancode = ccHIGHEST - scancode  'Map to cc* constant
    END IF
   END IF
  END IF
  IF scancode < scKEYVAL_FIRST ORELSE scancode > scKEYVAL_LAST THEN
   scripterr "Unrecognised scancode " & scancode, serrBadOp
  ELSE
   script_start_waiting(scancode)
  END IF
 CASE 10'--walk hero
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   SELECT CASE retvals(1)
    CASE 0'--north
     (herodir(retvals(0))) = dirNorth
     herow(retvals(0)).ygo = retvals(2) * 20
    CASE 1'--east
     (herodir(retvals(0))) = dirEast
     herow(retvals(0)).xgo = (retvals(2) * 20) * -1
    CASE 2'--south
     (herodir(retvals(0))) = dirSouth
     herow(retvals(0)).ygo = (retvals(2) * 20) * -1
    CASE 3'--west
     (herodir(retvals(0))) = dirWest
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
    scripterr "Setting onetime tags with the settag command is deprecated", serrInfo
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   herow(retvals(0)).wtog = bound(retvals(1), 0, WALKFRAMES - 1) * wtog_ticks()
  END IF
 CASE 27'--suspend overlay
  setbit gen(), genSuspendBits, suspendoverlay, 1
 CASE 28'--play song
  'loadsong game + "." + STR(retvals(0))
  wrappedsong retvals(0)
 CASE 29'--stop song
  stopsong
 CASE 30'--keyval (scancode, joystick num)
  'This used to be keyispressed; which undocumentedly reported two bits
  'instead of true/false.
  a_script_wants_keys()
  DIM joynum as integer = get_optional_arg(1, 0)
  'keyval() reports a 3rd bit, but didn't at the time that this command was (re-)documented
  scriptret = script_keyval(retvals(0), joynum) AND 3
 CASE 31'--rank in caterpillar
  scriptret = rankincaterpillar(retvals(0))
 CASE 38'--camera follows hero
  gen(genCameraMode) = herocam
  gen(genCameraArg1) = bound(retvals(0), 0, active_party_slots - 1)
 CASE 40'--pan camera
  gen(genCameraMode) = pancam
  gen(genCameraArg1) = bound(retvals(0), 0, active_party_slots - 1)
  gen(genCameraArg2) = large(retvals(1), 0) * (20 / large(retvals(2), 1))
  gen(genCameraArg3) = large(retvals(2), 1)
 CASE 41'--focus camera
  gen(genCameraMode) = focuscam
  gen(genCameraArg1) = (retvals(0) * 20) - (get_resolution().w - 20) / 2
  gen(genCameraArg2) = (retvals(1) * 20) - (get_resolution().h - 20) / 2
  gen(genCameraArg3) = ABS(retvals(2))
  gen(genCameraArg4) = ABS(retvals(2))
  limitcamera gen(genCameraArg1), gen(genCameraArg2)
 CASE 42'--wait for camera
  script_start_waiting(retvals(0))
 CASE 43'--hero x
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = herotx(retvals(0))
  END IF
 CASE 44'--hero y
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = heroty(retvals(0))
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   (herodir(retvals(0))) = ABS(retvals(1)) MOD 4
  END IF
 CASE 57, 118'--suspend caterpillar
  setbit gen(), genSuspendBits, suspendcaterpillar, 1
 CASE 58, 119'--resume caterpillar
  setbit gen(), genSuspendBits, suspendcaterpillar, 0
  interpolatecat ()
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
     IF item_read_equipbit(buffer(), hero_id) THEN
      ' It's equippable; return slot+1 for the first equippable slot
      FOR i as integer = 0 to 4
       IF item_is_equippable_in_slot(buffer(), i) THEN scriptret = i + 1
      NEXT i
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
  scriptret = active_party_slots() - active_party_size()
 CASE 71'--lock hero
  DIM hero_slot as integer = findhero(retvals(0), , serrWarn)
  IF hero_slot > -1 THEN gam.hero(hero_slot).locked = YES
 CASE 72'--unlock hero
  DIM hero_slot as integer = findhero(retvals(0), , serrWarn)
  IF hero_slot > -1 THEN gam.hero(hero_slot).locked = NO
 CASE 74'--set death script
  gen(genGameoverScript) = large(retvals(0), 0)
 CASE 75'--fade screen out
  FOR i as integer = 0 TO 2
   'Convert from 0-63 -> 0-255
   retvals(i) = bound(IIF(retvals(i), retvals(i) * 4 + 3, 0), 0, 255)
  NEXT
  stop_fibre_timing
  fadeout retvals(0), retvals(1), retvals(2)
  start_fibre_timing
  IF gam.need_fade_in ANDALSO gam.fade_in_script_overridable THEN
   'For backwards compatibility, if a fade delay has been increased so that a
   'fadescreenout that used to occur after a queued fade now happens before,
   'that queued fade needs to be cancelled so that the screen stays faded until
   'the corresponding fadescreenin.
   gam.need_fade_in = NO
  END IF
 CASE 76'--fade screen in
  IF vpages_are_32bit() ANDALSO masterpal_has_changed(master()) THEN
   'We can't fade between two master palettes (not even out and in) in 32-bit color mode,
   'so bit of a hack to provide good backcompat: an implicit wait, mostly pause the game
   'logic, redraw the scene, and blend between old and new page
   queue_fade_in
   gam.need_fade_page = YES
   script_start_waiting(0)
  ELSE
   stop_fibre_timing
   fadein
   start_fibre_timing
   IF gam.need_fade_in AND gam.fade_in_delay <= 0 THEN
    'Avoid unnecessary pause
    gam.need_fade_in = NO
   END IF
  END IF
 CASE 81'--set hero speed
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   change_hero_speed(retvals(0), bound(retvals(1), 0, 20))
  END IF
 CASE 82'--inventory
  scriptret = countitem(retvals(0))
 CASE 84'--suspend box advance
  setbit gen(), genSuspendBits, suspendboxadvance, 1
 CASE 85'--resume box advance
  setbit gen(), genSuspendBits, suspendboxadvance, 0
 CASE 87'--set hero position
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
  cropposition retvals(1), retvals(2), 1
  resetcaterpillar_for_one_hero retvals(0), retvals(1) * 20, retvals(2) * 20
  END IF
 CASE 90'--find hero
  scriptret = findhero(retvals(0))
 CASE 91'--check equipment
  IF valid_hero_party(retvals(0)) THEN
   scriptret = gam.hero(retvals(0)).equip(bound(retvals(1) - 1, 0, 4)).id
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
  (heroz(bound(retvals(0), 0, 3))) = retvals(1)
 CASE 102'--hero direction
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = herodir(retvals(0))
  END IF
 CASE 103'--reset palette
  gam.current_master_palette = gen(genMasterPal)
  load_master_and_uicol gam.current_master_palette
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
   reseed_prng retvals(0)
  ELSE
   reseed_prng TIMER * 1e9
  END IF
 CASE 109'--greyscale palette
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   'Both scripted and user pathfinding count as walking
   'Note that when holding down an arrow key we will return false at the end of
   'every step, not so when pathfinding or doing a long "walk hero"
   IF herow(retvals(0)).xygo <> 0 ORELSE hero_is_pathfinding(retvals(0)) THEN
    scriptret = 1
   END IF
   IF readbit(gen(), genSuspendBits, suspendcaterpillar) = 0 THEN
    ' Other heroes trail behind the leader automatically without using .xgo and .ygo.
    ' walkhero partially works when the caterpillar party is enabled too
    ' (well they move, but don't animate), so combine the two
    IF herow(0).xygo <> 0 ORELSE hero_is_pathfinding(0) THEN
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
    IF gam.hero(retvals(0)).spells(i, j) = retvals(1) THEN
     gam.hero(retvals(0)).spells(i, j) = 0
     scriptret = 1
    END IF
   NEXT j
  NEXT i
 CASE 129'--read spell
  IF valid_hero_party(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 THEN
   scriptret = gam.hero(retvals(0)).spells(retvals(1), retvals(2))
  ELSE
   scriptret = 0
  END IF
 CASE 130'--write spell
  IF valid_hero_party(retvals(0)) AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 AND retvals(3) >= 0 THEN
   gam.hero(retvals(0)).spells(retvals(1), retvals(2)) = retvals(3)
  END IF
 CASE 131'--knows spell
  scriptret = 0
  retvals(0) = bound(retvals(0), 0, 40)
  IF retvals(1) > 0 THEN
   FOR i as integer = 0 TO 3
    FOR j as integer = 0 TO 23
     IF gam.hero(retvals(0)).spells(i, j) = retvals(1) THEN
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
    DIM her as HeroDef
    loadherodata her, heroID
    FOR i as integer = 0 TO 3
     FOR j as integer = 0 TO 23
      IF gam.hero(partyslot).spells(i, j) = 0 THEN
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
  IF valid_hero_party(retvals(0)) THEN
   scriptret = gam.hero(retvals(0)).id
  ELSE
   scriptret = -1
  END IF
 CASE 134'--hero by rank
  scriptret = herobyrank(retvals(0))
 CASE 145'--pick hero
  DIM stringid as integer = get_optional_arg(0, -1)
  DIM skip_if_alone as bool = get_optional_arg(1, NO) <> 0
  IF stringid = -1 ORELSE valid_plotstr(stringid, serrBadOp) THEN
   DIM prompt as string
   IF stringid = -1 THEN
    prompt = readglobalstring(135, "Which Hero?", 20)
   ELSE
    prompt = plotstr(stringid).s
   END IF
   scriptret = onwho(prompt, skip_if_alone)
  ELSE
   scriptret = -1
  END IF
 CASE 146'--rename hero by slot
  IF valid_hero_party(retvals(0)) THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    scriptret = IIF(renamehero(retvals(0), YES), 1, 0)
   END IF
  END IF
 CASE 171'--save slot used
  IF valid_save_slot(retvals(0)) THEN
   IF save_slot_used(retvals(0) - 1) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 172'--import globals
  'If the save slot isn't used, this zeroes out the globals, and shows no error
  IF valid_save_slot(retvals(0)) THEN
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
 CASE 173'--export globals
  IF valid_save_slot(retvals(0)) THEN
   IF retvals(1) >= 0 AND retvals(2) <= maxScriptGlobals AND retvals(1) <= retvals(2) THEN
    saveglobalvars retvals(0) - 1, retvals(1), retvals(2)
   END IF
  END IF
 CASE 175'--delete save
  IF valid_save_slot(retvals(0)) THEN
   erase_save_slot retvals(0) - 1
  END IF
 CASE 176'--run script by id
  DIM rsr as integer
  DIM argc as integer = curcmd->argc  'Must store before calling runscript
  rsr = runscript(retvals(0), NO, NO, "runscriptbyid")
  IF rsr = 1 THEN
   '--fill heap with arguments
   FOR i as integer = 1 TO argc - 1  'flexible argument number!
    setScriptArg i - 1, retvals(i)
   NEXT i
   'NOTE: scriptret is not set here when this command is successful. The return value of the called script will be returned.
  ELSE
   scripterr "run script by id failed loading " & retvals(0), serrMajor
   scriptret = -1
  END IF
 CASE 180'--map width([map])
  'map width did not originally have an argument
  DIM map_id as integer = get_optional_arg(0, -1)
  IF map_id = -1 ORELSE map_id = gam.map.id THEN
   scriptret = mapsizetiles.x
  ELSE
   IF valid_map(map_id) THEN
    DIM as TilemapInfo mapsize
    GetTilemapInfo maplumpname(map_id, "t"), mapsize
    scriptret = mapsize.wide
   END IF
  END IF
 CASE 181'--map height([map])
  'map height did not originally have an argument
  DIM map_id as integer = get_optional_arg(0, -1)
  IF map_id = -1 ORELSE map_id = gam.map.id THEN
   scriptret = mapsizetiles.y
  ELSE
   IF valid_map(map_id) THEN
    DIM as TilemapInfo mapsize
    GetTilemapInfo maplumpname(map_id, "t"), mapsize
    scriptret = mapsize.high
   END IF
  END IF
 CASE 187'--get music volume
  scriptret = get_music_volume * 255
 CASE 188'--set music volume
  set_music_volume bound(retvals(0), 0, 255) / 255
 CASE 189, 307'--get formation song
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) THEN
   DIM form as Formation
   LoadFormation form, retvals(0)
   scriptret = form.music
   IF cmdid = 189 THEN scriptret += 1
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = wtog_to_frame(herow(retvals(0)).wtog)
  END IF
 CASE 195'--load sound (BACKWARDS COMPATABILITY HACK )
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF retvals(0) >= 0 AND retvals(0) <= 7 THEN
   backcompat_sound_slot_mode = YES
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
   playsfx sfxid, IIF(retvals(1) <> 0, -1, 0)
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
 CASE 200'--system hour (TIME is always hh:mm:ss)
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
  IF valid_plotstr(retvals(0)) ANDALSO valid_hero_party(retvals(1)) THEN
   plotstr(retvals(0)).s = gam.hero(retvals(1)).name
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 205'--set hero name
  IF valid_plotstr(retvals(0)) ANDALSO valid_hero_party(retvals(1)) THEN
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
   IF valid_plotstr(retvals(0)) = NO ORELSE valid_map(retvals(1)) = NO THEN
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
  IF valid_plotstr(retvals(0)) ANDALSO retvals(1) >= 0 ANDALSO retvals(1) <= 255 THEN
   WITH plotstr(retvals(0))
    .s &= CHR(retvals(1))
    scriptret = LEN(.s)
   END WITH
  END IF
 CASE 213'--append number (id, value, minlength, zeropad)
  IF valid_plotstr(retvals(0)) THEN
   DIM byref thestring as string = plotstr(retvals(0)).s
   DIM minlength as integer = get_optional_arg(2, 0)  'Can be negative
   IF ABS(minlength) > 1 THEN
    DIM zeropad as bool = get_optional_arg(3, NO)
    DIM fmt as string = "%"
    IF zeropad THEN fmt &= "0"  ' This has no effect if minlength is negative
    fmt &= minlength & "d"
    thestring &= strprintf(fmt, retvals(1))
   ELSE
    thestring &= retvals(1)
   END IF
   scriptret = LEN(thestring)
  END IF
 CASE 214'--copy string
  IF valid_plotstr(retvals(0)) ANDALSO valid_plotstr(retvals(1)) THEN
   plotstr(retvals(0)).s = plotstr(retvals(1)).s
  END IF
 CASE 215'--concatenate strings
  IF valid_plotstr(retvals(0)) ANDALSO valid_plotstr(retvals(1)) THEN
   plotstr(retvals(0)).s += plotstr(retvals(1)).s
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 216'--string length
  IF valid_plotstr(retvals(0)) THEN
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 217'--delete char
  IF valid_plotstr(retvals(0)) THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    WITH plotstr(retvals(0))
     .s = LEFT(.s, retvals(1) - 1) & MID(.s, retvals(1) + 1)
    END WITH
   END IF
  END IF
 CASE 218'--replace char
  IF valid_plotstr(retvals(0)) ANDALSO retvals(2) >= 0 ANDALSO retvals(2) <= 255 THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    MID(plotstr(retvals(0)).s, retvals(1), 1) = CHR(retvals(2))
   END IF
  END IF
 CASE 219'--ascii from string
  IF valid_plotstr(retvals(0)) ANDALSO retvals(1) >= 1 ANDALSO retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
   scriptret = plotstr(retvals(0)).s[retvals(1)-1]'you can index strings a la C
  END IF
 CASE 220'--position string
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).X = retvals(1)
   plotstr(retvals(0)).Y = retvals(2)
  END IF
 CASE 221'--set string bit
  IF valid_plotstr(retvals(0)) ANDALSO retvals(1) >= 0 ANDALSO retvals(1) <= 15 THEN
   IF retvals(2) THEN
    plotstr(retvals(0)).bits OR= 2 ^ retvals(1)
   ELSE
    plotstr(retvals(0)).bits AND= NOT 2 ^ retvals(1)
   END IF
  END IF
 CASE 222'--get string bit
  IF valid_plotstr(retvals(0)) ANDALSO retvals(1) >= 0 ANDALSO retvals(1) <= 15 THEN
   'scriptret = readbit(plotstrBits(), retvals(0), retvals(1))
   scriptret = IIF(plotstr(retvals(0)).bits AND 2 ^ retvals(1), 1, 0)
  END IF
 CASE 223'--string color
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).col = bound(retvals(1), -1, 255)  'Allow -1 for default
   plotstr(retvals(0)).bgcol = bound(retvals(2), 0, 255)
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
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy ID") AND bound_arg(retvals(1), 0, 106, "data index", , serrBadOp) THEN
   scriptret = ReadShort(tmpdir & "dt1.tmp", retvals(0) * getbinsize(binDT1) + retvals(1) * 2 + 1)
  END IF
 CASE 231'--write enemy data
  'Boy, was this command a bad idea!
  '106 was the largest used offset until very recently, so we'll limit it there to
  'prevent further damage
  'Note: writing elemental/enemytype bits no longer works
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy ID") AND bound_arg(retvals(1), 0, 106, "data index", , serrBadOp) THEN
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
 CASE 235'--key is pressed (scancode, joystick num)
  a_script_wants_keys()
  IF script_keyval(retvals(0), retvals(1)) THEN scriptret = 1 ELSE scriptret = 0
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
  IF valid_plotstr(retvals(0)) ANDALSO valid_plotstr(retvals(1)) THEN
   WITH plotstr(retvals(0))
    scriptret = INSTR(bound(retvals(2), 1, LEN(.s)), .s, plotstr(retvals(1)).s)
   END WITH
  ELSE
   scriptret = 0
  END IF
 CASE 239'--trim string
  IF valid_plotstr(retvals(0)) THEN
   WITH plotstr(retvals(0))
    IF retvals(1) = -1 THEN
     .s = TRIM(.s)
    ELSE
     IF retvals(1) <= LEN(.s) ANDALSO retvals(2) >= 1 THEN
      retvals(1) = large(retvals(1), 1)
      'retvals(2) = bound(retvals(2), 1, LEN(.s))
      .s = MID(.s, retvals(1), retvals(2))
     ELSE
      .s = ""
     END IF
    END IF
   END WITH
  END IF
 CASE 240'-- string from textbox (string, box, line, ignored)   [obsolete]
  IF valid_plotstr(retvals(0)) THEN
   DIM box as TextBox
   retvals(1) = bound(retvals(1), 0, gen(genMaxTextbox))
   LoadTextBox box, retvals(1)
   retvals(2) = bound(retvals(2), 0, UBOUND(box.text))
   plotstr(retvals(0)).s = TRIM(box.text(retvals(2)))
   embedtext plotstr(retvals(0)).s
  END IF
 CASE 241'-- expand string(id, saveslot)
  retvals(1) = get_optional_arg(1, 0)
  IF valid_plotstr(retvals(0)) THEN
   'Retvals(1) can be 0 for the default of using current game state, or a save slot 1-maxSaveSlotCount
   IF retvals(1) = 0 ORELSE valid_save_slot(retvals(1)) THEN
    embedtext plotstr(retvals(0)).s, , retvals(1) - 1
   END IF
   scriptret = retvals(0)
  END IF
 CASE 242'-- joystick button(button, joystick)
  IF bound_arg(retvals(0), 1, 32, "button number 1-32") ANDALSO valid_joystick(retvals(1)) THEN
   DIM key as KeyBits = joykeyval(joyButton1 + retvals(0) - 1, retvals(1))
   scriptret = IIF(key > 0, 1, 0)
  END IF
 CASE 243'-- joystick axis(axis, scale, joystick)
  IF valid_joystick(retvals(2)) THEN
   scriptret = (joystick_axis(retvals(0), retvals(2)) / 1000) * retvals(1)
  END IF
 CASE 249'--party money
  scriptret = gold
 CASE 250'--set money
  IF retvals(0) >= 0 THEN gold = retvals(0)
 CASE 251'--set string from table
  IF bound_arg(retvals(0), 0, UBOUND(plotstr), "string ID", !"$# = \"...\"") THEN
   plotstr(retvals(0)).s = script_string_constant(nowscript, retvals(1))
   scriptret = retvals(0)
  END IF
 CASE 252'--append string from table
  IF bound_arg(retvals(0), 0, UBOUND(plotstr), "string ID", !"$# + \"...\"") THEN
   plotstr(retvals(0)).s += script_string_constant(nowscript, retvals(1))
   scriptret = retvals(0)
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
      IF retvals(3) <> -1 ANDALSO .trigger <> retvals(3) THEN
       'When changing the trigger, any script args are forgotten
       ERASE .script_args
       .trigger = retvals(3)
      END IF
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
   WITH master(retvals(0))
    .col = retvals(1)
    .a = 255  'just in case, set the alpha
   END WITH
  END IF
 CASE 265'--rgb
  scriptret = RGB(bound(retvals(0),0,255), bound(retvals(1),0,255), bound(retvals(2),0,255))
 CASE 266'--extract color
  DIM c as RGBcolor = TYPE(retvals(0))
  SELECT CASE retvals(1)
   CASE 0 : scriptret = c.r
   CASE 1 : scriptret = c.g
   CASE 2 : scriptret = c.b
   CASE 3 : scriptret = c.a  'No use yet
  END SELECT
 CASE 268'--load palette
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxMasterPal) THEN
   gam.current_master_palette = retvals(0)
   load_master_and_uicol gam.current_master_palette
  END IF
 CASE 273'--milliseconds
  ' We shift the zero point so that negative return values don't occur unless the
  ' game has been open 24 days. Do this because on Windows TIMER seems to reset when
  ' you reboot, making it unlikely that people will ever test their game with negative
  ' milliseconds.
  scriptret = fmod(((TIMER - gam.timer_offset) * 1000) + 2147483648.0, 4294967296.0) - 2147483648.0
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
    DIM size as XYPair
    IF szindex = 0 THEN size = XY(34, 34)
    IF szindex = 1 THEN size = XY(50, 50)
    IF szindex = 2 THEN size = XY(80, 80)
    WITH form.slots(slot)
     .id = retvals(1)
     ' Convert to top-left coord
     .pos.x = retvals(2) - size.w \ 2
     .pos.y = retvals(3) - size.h
     ' These are the same limits as used in the formation editor
     ' FIXME: battles are still stuck at 320x200 for the moment, but switch to this later
     ' .pos.x = bound(.pos.x, -size.w\2, get_resolution().w - size.w\2)
     ' .pos.y = bound(.pos.y, -size.h\2, get_resolution().h - size.h\2)
     .pos.x = bound(.pos.x, -size.w\2, 320 - size.w\2)
     .pos.y = bound(.pos.y, -size.h\2, 200 - size.h\2)
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
   scriptret = form.slots(retvals(1)).pos.n(cmdid - 312)
   'now find the position of the bottom center of the enemy sprite
   IF enemy_id >= 0 THEN
    DIM pictype as integer = ReadShort(tmpdir & "dt1.tmp", enemy_id * getbinsize(binDT1) + 111) 'picture size
    DIM picsize as integer
    IF pictype = 0 THEN picsize = 34
    IF pictype = 1 THEN picsize = 50
    IF pictype = 2 THEN picsize = 80
    IF cmdid = 312 THEN scriptret += picsize \ 2 ELSE scriptret += picsize
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = herow(retvals(0)).speed
  END IF
 CASE 322'--load hero sprite
  scriptret = load_sprite_plotslice(0, retvals(0), retvals(1))
 CASE 323'--free sprite
  IF retvals(0) = 0 THEN
   'No warning
  ELSEIF valid_plotslice(retvals(0), serrWarn) THEN
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
   scriptret = SpriteSliceNumFrames(plotslices(retvals(0)))
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
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , serrBadOp) THEN
    plotslices(retvals(0))->AlignHoriz = retvals(1)
   END IF
  END IF
 CASE 355 '--set vert align
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , serrBadOp) THEN
    plotslices(retvals(0))->AlignVert = retvals(1)
   END IF
  END IF
 CASE 356 '--set horiz anchor
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , serrBadOp) THEN
    plotslices(retvals(0))->AnchorHoriz = retvals(1)
   END IF
  END IF
 CASE 357 '--set vert anchor
  IF valid_plotslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 2, "edge:... constant", , serrBadOp) THEN
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
  IF valid_plotslice(retvals(0), serrWarn) THEN
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
  IF valid_resizeable_slice(retvals(0), NO, YES) THEN
   plotslices(retvals(0))->Width = retvals(1)
  END IF
 CASE 373 '--set slice height
  IF valid_resizeable_slice(retvals(0), YES, NO) THEN
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
   IF dat->use_raw_box_border THEN
    scriptret = -99  'border:raw
   ELSE
    scriptret = dat->border
   END IF
  END IF
 CASE 381 '--set rect border
  IF bound_arg(retvals(1), -2, 14, "border") THEN  'border:raw not allowed
   change_rect_plotslice retvals(0), , , ,retvals(1)
  END IF
 CASE 382 '--get rect trans
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->translucent
  END IF
 CASE 383 '--set rect trans
  IF bound_arg(retvals(1), 0, transLAST, "trans:... transparency setting") THEN
   change_rect_plotslice retvals(0), , , , ,retvals(1)
  END IF
 CASE 384 '--slice collide point
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = ABS(SliceCollidePoint(sl, XY(retvals(1), retvals(2))))
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
  IF valid_resizeable_slice(retvals(0), YES, YES) THEN
   'FIXME: need to ensure we don't clash with Cover Children by disabling
   'that as appropriate. See SliceLegalCoverModes.
   'TODO: there's no command to change slice fill mode!
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
   scriptret = find_plotslice_handle(plotslices(retvals(0))->LastChild)
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
  setbit gen(), genSuspendBits, suspendtimers, 1
 CASE 416 '--resume timers
  setbit gen(), genSuspendBits, suspendtimers, 0
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
 CASE 422 '--set slice text (slice, string)
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
 CASE 433'--slice at pixel(parent, x, y, num, descend, visibleonly)
  'visibleonly is recent addition
  retvals(5) = get_optional_arg(5, 0)
  IF valid_plotslice(retvals(0)) THEN
   ' We update retvals(0) and its ancestors, FindSliceAtPoint updates its descendents.
   RefreshSliceScreenPos plotslices(retvals(0))
   IF retvals(3) <= -1 THEN
    DIM slnum as integer = -1
    FindSliceAtPoint(plotslices(retvals(0)), XY(retvals(1), retvals(2)), slnum, retvals(4), retvals(5))
    scriptret = -slnum - 1
   ELSE
    DIM slnum as integer = retvals(3)  ' Avoid modification to retvals
    scriptret = find_plotslice_handle(FindSliceAtPoint(plotslices(retvals(0)), XY(retvals(1), retvals(2)), slnum, retvals(4), retvals(5)))
   END IF
  END IF
 CASE 434'--find colliding slice(parent, handle, num, descend, visibleonly)
  retvals(4) = get_optional_arg(4, 0)
  IF valid_plotslice(retvals(0)) AND valid_plotslice(retvals(1)) THEN
   ' We update retvals(0/1) and their ancestors, FindSliceCollision updates retvals(0)'s descendents.
   RefreshSliceScreenPos plotslices(retvals(0))
   RefreshSliceScreenPos plotslices(retvals(1))
   IF retvals(2) <= -1 THEN
    DIM slnum as integer = -1
    FindSliceCollision(plotslices(retvals(0)), plotslices(retvals(1)), slnum, retvals(3), retvals(4))
    scriptret = -slnum - 1
   ELSE
    DIM slnum as integer = retvals(2)  ' Avoid modification to retvals
    scriptret = find_plotslice_handle(FindSliceCollision(plotslices(retvals(0)), plotslices(retvals(1)), slnum, retvals(3), retvals(4)))
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
   scriptret = find_plotslice_handle(LookupSlice(retvals(0), SliceTable.Root))
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
   DIM as Slice ptr sl0 = plotslices(retvals(0)), sl1 = plotslices(retvals(1))
   IF sl0 = sl1 THEN
    scripterr "moveslicebelow: tried to move a slice below itself"
   ELSEIF sl0->Protect ANDALSO sl0->Parent <> sl1->Parent THEN
    scripterr "moveslicebelow: tried to change the parent of a protected slice"
   ELSEIF sl1->Parent = NULL THEN
    scripterr "moveslicebelow: Root can't have siblings"
   ELSE
    InsertSliceBefore sl1, sl0
   END IF
  END IF
 CASE 447 '--move slice above
  IF valid_plotslice(retvals(0)) ANDALSO valid_plotslice(retvals(1)) THEN
   DIM as Slice ptr sl0 = plotslices(retvals(0)), sl1 = plotslices(retvals(1))
   IF sl0 = sl1 THEN
    scripterr "movesliceabove: tried to move a slice above itself"
   ELSEIF sl0->Protect ANDALSO sl0->Parent <> sl1->Parent THEN
    scripterr "movesliceabove: tried to change the parent of a protected slice"
   ELSEIF sl1->Parent = NULL THEN
    scripterr "movesliceabove: Root can't have siblings"
   ELSE
    InsertSliceAfter sl1, sl0
   END IF
  END IF
 CASE 448 '--slice child
  IF valid_plotslice(retvals(0)) THEN
   scriptret = find_plotslice_handle(SliceChildByIndex(plotslices(retvals(0)), retvals(1)))
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
   SliceLoadFromFile sl, workingdir & SLASH & "slicetree_0_" & retvals(0) & ".reld", , retvals(0)
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
    result &= script_string_constant(nowscript, retvals(i)) & " = "
   ELSE
    result &= retvals(i)
   END IF
  NEXT
  debug "TRACE: " & result
 CASE 467 '--map cure
  IF bound_arg(retvals(0), 1, gen(genMaxAttack)+1, "attack ID") THEN
   IF valid_hero_party(retvals(1)) THEN
    IF valid_hero_party(retvals(2), -1) THEN
     'If the attack can not target dead heroes, then this fails and returns false
     scriptret = ABS(outside_battle_cure(retvals(0) - 1, retvals(1), retvals(2), NO))
    END IF
   END IF
  END IF
 CASE 468 '--read attack name
  scriptret = 0
  IF valid_plotstr(retvals(0)) AND bound_arg(retvals(1), 1, gen(genMaxAttack)+1, "attack ID") THEN
   plotstr(retvals(0)).s = readattackname(retvals(1) - 1)
   scriptret = 1
  END IF
 CASE 470'--allocate timers
  IF bound_arg(retvals(0), 0, 100000, "number of timers", , serrBadOp) THEN
   REDIM PRESERVE timers(large(0, retvals(0) - 1))
   IF retvals(0) = 0 THEN
    'Unfortunately, have to have at least one timer. Deactivate/blank it, in case the user
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
    scriptret = 1
    IF WriteZoneTile(zmap, retvals(0), retvals(1), retvals(2), retvals(3)) = 0 THEN
     scriptret = 0
     scripterr "writezone: the maximum number of zones, 15, already overlap at " & XY(retvals(1), retvals(2)) & "; attempt to add another failed"
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
  IF valid_zone(retvals(0)) AND bound_arg(retvals(1), 0, 2, "extra data number", , serrBadOp) THEN
   scriptret = GetZoneInfo(zmap, retvals(0))->extra(retvals(1))
  END IF
 CASE 489'--set zone extra (id, extra, value)
  IF valid_zone(retvals(0)) AND bound_arg(retvals(1), 0, 2, "extra data number", , serrBadOp) THEN
   GetZoneInfo(zmap, retvals(0))->extra(retvals(1)) = retvals(2)
   lump_reloading.zonemap.dirty = YES
  END IF
 CASE 493'--load backdrop sprite (record)
  scriptret = load_sprite_plotslice(sprTypeBackdrop, retvals(0))
 CASE 494 '--replace backdrop sprite (handle, record)
  replace_sprite_plotslice retvals(0), sprTypeBackdrop, retvals(1)
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
  IF valid_plotellipse(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "bordercol") THEN
    ChangeEllipseSlice plotslices(retvals(0)), retvals(1)
   END IF
  END IF
 CASE 513 '--set ellipse fill col
  IF valid_plotellipse(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 255, "fillcol") THEN
    ChangeEllipseSlice plotslices(retvals(0)), , retvals(1)
   END IF
  END IF
 CASE 514 '--get ellipse border col
  IF valid_plotellipse(retvals(0)) THEN
   DIM dat as EllipseSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->bordercol
  END IF
 CASE 515 '--get ellipse fill col
  IF valid_plotellipse(retvals(0)) THEN
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
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = find_plotslice_handle(herow(retvals(0)).sl)  'May be NULL (empty ranks)
  END IF
 CASE 520 '--get NPC slice
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   scriptret = find_plotslice_handle(npc(npcref).sl)  'May be NULL (tag-disabled NPCs)
  END IF
 CASE 521 '--get door x (doorid, [mapid])
  scriptret = -1
  DIM map_id as integer = get_optional_arg(1, -1)
  DIM thisdoor as Door
  IF get_door_on_map(thisdoor, retvals(0), map_id) THEN
   IF valid_door(thisdoor, retvals(0)) THEN
    scriptret = thisdoor.pos.x
   END IF
  END IF
 CASE 522 '--get door y (doorid, [mapid])
  scriptret = -1
  DIM map_id as integer = get_optional_arg(1, -1)
  DIM thisdoor as Door
  IF get_door_on_map(thisdoor, retvals(0), map_id) THEN
   IF valid_door(thisdoor, retvals(0)) THEN
    scriptret = thisdoor.pos.y
   END IF
  END IF
 CASE 523 '--get door destination id (doorid, [mapid])
  'Documented to return -1 without error for invalid door/doorlink
  '(Only throws error on bad map ID)
  scriptret = -1
  DIM map_id as integer = get_optional_arg(1, -1)
  IF map_id = -1 ORELSE valid_map(map_id) THEN
   DIM dlink as DoorLink
   'Returns NO if the door is unused
   IF find_doorlink(dlink, retvals(0), map_id) THEN
    scriptret = dlink.dest
   END IF
  END IF
 CASE 524 '--get door destination map (doorid, [mapid])
  'Documented to return -1 without error for invalid door/doorlink
  '(Only throws error on bad map ID)
  scriptret = -1
  DIM map_id as integer = get_optional_arg(1, -1)
  IF map_id = -1 ORELSE valid_map(map_id) THEN
   DIM dlink as DoorLink
   'Returns NO if the door is unused
   IF find_doorlink(dlink, retvals(0), map_id) THEN
    scriptret = dlink.dest_map
   END IF
  END IF
 CASE 525 '--door exists (doorid, [mapid])
  DIM map_id as integer = get_optional_arg(1, -1)
  DIM thisdoor as Door
  IF get_door_on_map(thisdoor, retvals(0), map_id) THEN
   scriptret = iif(thisdoor.exists, 1, 0)
  END IF
 CASE 526 '--get attack caption
  IF valid_plotstr(retvals(0), serrBadOp) ANDALSO _
     bound_arg(retvals(1), 1, gen(genMaxAttack)+1, "attack ID", , serrBadOp) THEN
   plotstr(retvals(0)).s = readattackcaption(retvals(1) - 1)
   scriptret = 1
  END IF
 CASE 527, 701 '--527: get rect fuzziness (slice), 701: get rect opacity (slice)
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->translucent = transFuzzy ORELSE dat->translucent = transBlend THEN
    scriptret = dat->fuzzfactor
   ELSEIF dat->translucent = transHollow THEN
    scriptret = 0
   ELSEIF dat->translucent = transOpaque THEN
    scriptret = 100
   END IF
  END IF
 CASE 528, 702 '--528: set rect fuzziness (slice, percent), 702: set rect opacity (slice, percent)
  IF valid_plotrect(retvals(0)) THEN
   'Allow out of bounds percentages, just like "set opacity"
   DIM opacity as integer = bound(retvals(1), 0, 100)
   DIM trans as RectTransTypes
   IF opacity = 0 THEN
    'Reset fuzzfactor to default 50% for future "set rect trans (sl, trans:fuzzy)";
    'the trans setting overrides the opacity
    trans = transHollow
    opacity = 50
   ELSEIF opacity = 100 THEN
    'Ditto
    trans = transOpaque
    opacity = 50
   ELSEIF cmdid = 528 THEN  'set rect fuzziness
    trans = transFuzzy
   ELSE  'set rect opacity
    trans = transBlend
   END IF
   ChangeRectangleSlice plotslices(retvals(0)), , , , , trans, opacity
  END IF
 CASE 529 '-- textbox line (string, box, line, expand, strip)
  IF valid_plotstr(retvals(0), serrBadOp) ANDALSO _
     bound_arg(retvals(1), 0, gen(genMaxTextbox), "textbox", , serrBadOp) THEN
   IF retvals(2) < 0 THEN
    scripterr "textbox line: invalid line number " & retvals(2), serrBadOp
   ELSE
    DIM box as TextBox
    LoadTextBox box, retvals(1)
    WITH plotstr(retvals(0))
     'There's no upper bound on valid textbox line numbers
     IF retvals(2) <= UBOUND(box.text) THEN
      .s = box.text(retvals(2))
     ELSE
      .s = ""
     END IF
     IF retvals(4) THEN .s = TRIM(.s)
     IF retvals(3) THEN embedtext .s
    END WITH
   END IF
  END IF
 CASE 530 '--get slice text (string, slice)
  IF valid_plotstr(retvals(0), serrBadOp) THEN
   IF valid_plottextslice(retvals(1)) THEN
    plotstr(retvals(0)).s = plotslices(retvals(1))->TextData->s
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
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    WITH gam.hero(retvals(0))
     .hand_pos(retvals(1)).x = retvals(2)
     .hand_pos_overridden = YES
    END WITH
   END IF
  END IF
 CASE 535 '--set hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    WITH gam.hero(retvals(0))
     .hand_pos(retvals(1)).y = retvals(2)
     .hand_pos_overridden = YES
    END WITH
   END IF
  END IF
 CASE 536 '--get hero hand x
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    scriptret = gam.hero(retvals(0)).hand_pos(retvals(1)).x
   END IF
  END IF
 CASE 537 '--get hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    scriptret = gam.hero(retvals(0)).hand_pos(retvals(1)).y
   END IF
  END IF
 CASE 538 '--get default hero hand x
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    scriptret = GetHeroHandPos(gam.hero(retvals(0)).id, retvals(1)).x
   END IF
  END IF
 CASE 539 '--get default hero hand y
  IF valid_hero_party(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "attack frame", , serrBadOp) THEN
    scriptret = GetHeroHandPos(gam.hero(retvals(0)).id, retvals(1)).y
   END IF
  END IF
 CASE 540'--check onetime
  IF bound_arg(retvals(0), 1, max_onetime, "onetime use tag") THEN
   scriptret = ABS(istag(onetime(), retvals(0), 0))
  END IF
 CASE 541'--set onetime
  IF bound_arg(retvals(0), 1, max_onetime, "onetime use tag") THEN
   settag onetime(), retvals(0), retvals(1)
   tag_updates
  END IF
 CASE 542 '--microseconds
  ' TIMER, as a double, only has 53 bits of precision, and on Unix the first ~31
  ' bits are used up with the seconds since the Epoch, leaving barely enough
  ' bits for microsecond precision.
  scriptret = fmod((TIMER * 1e6) + 2147483648.0, 4294967296.0) - 2147483648.0
 CASE 543 '--enemy elemental resist as int (enemy, element)
  IF bound_arg(retvals(0), 0, gen(genMaxEnemy), "enemy id", , serrBadOp) THEN
   IF bound_arg(retvals(1), 0, gen(genNumElements) - 1, "element number") THEN
    DIM enemy as EnemyDef
    loadenemydata enemy, retvals(0), YES
    scriptret = 100 * enemy.elementals(retvals(1))  'rounds to nearest int
   END IF
  END IF
 CASE 544 '--hero Z
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   scriptret = heroz(retvals(0))
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
  scriptret = find_door(XY(retvals(0), retvals(1)))
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
 CASE 566 '--script error (string id, [hide frame])
  'What if we want to renumber error levels? I think I'll leave this arg for now
  DIM errlvl as scriptErrEnum = serrBadOp 'get_optional_arg(2, serrBadOp)
  IF retvals(0) = -1 THEN
   scripterr "(Triggered with ""scripterror"", no message)", errlvl
  ELSEIF valid_plotstr(retvals(0), serrBadOp) THEN
   IF get_optional_arg(1, 0) THEN
    'For script commands in plotscr.hsd.
    'TODO: report line number in the parent script instead, or
    'n frames up the stack.
    scripterr plotstr(retvals(0)).s, errlvl
   ELSE
    scripterr !"(Triggered with ""scripterror""):\n" & plotstr(retvals(0)).s, errlvl
   END IF
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
 CASE 595'--running on windows
  #IFDEF __FB_WIN32__
   scriptret = 1
  #ENDIF
 CASE 596'--running on mac
  #IFDEF __FB_DARWIN__
   scriptret = 1
  #ENDIF
 CASE 597'--running on linux
  #IFDEF __GNU_LINUX__
   scriptret = 1
  #ENDIF
 CASE 618'--debug menu
  stop_fibre_timing
  debug_menu
  start_fibre_timing
 CASE 620'--run game (string id)
  run_game
 CASE 627'--check game exists (string id)
  scriptret = check_game_exists
 CASE 674'--set last save slot
  IF retvals(0) = 0 ORELSE valid_save_slot(retvals(0)) THEN
   lastsaveslot = retvals(0)
  END IF

'old scriptnpc

 CASE 26'--set NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).wtog = bound(retvals(1), 0, WALKFRAMES - 1) * wtog_ticks()
 CASE 39'--camera follows NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   gen(genCameraMode) = npccam
   gen(genCameraArg1) = npcref
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
   'Only scripted pathfinding, not 'Chase You (Pathfinding)', counts as walking,
   'so at the end of a step the NPC isn't walking. During a step it is, of course.
   IF npc(npcref).xygo = 0 ANDALSO npc(npcref).pathover.override = NO THEN
    scriptret = 0
   ELSE
    scriptret = 1
   END IF
   IF cmdid = 117 THEN scriptret = scriptret XOR 1 'Backcompat hack
  END IF
 CASE 120'--NPC reference
  scriptret = 0
  DIM pool as integer = get_optional_arg(3, 0)
  IF bound_arg(pool, 0, 1, "pool id") THEN
   IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npool(pool).npcs) THEN
    DIM find_disabled as bool = get_optional_arg(2, 0) <> 0
    DIM found as integer = 0
    FOR i as integer = 0 TO UBOUND(npc)
     DIM id as integer = npc(i).id
     IF find_disabled THEN id = ABS(id)
     IF id - 1 = retvals(0) ANDALSO npc(i).pool = pool THEN
      IF found = retvals(1) THEN
       scriptret = (i + 1) * -1
       EXIT FOR
      END IF
      found = found + 1
     END IF
    NEXT i
   END IF
  END IF
 CASE 121'--NPC at spot
  IF retvals(2) = -1 THEN
   scriptret = count_npcs_at_spot(XY(retvals(0), retvals(1)))
  ELSE
   DIM npcidx as NPCIndex = npc_at_spot(XY(retvals(0), retvals(1)), retvals(2))
   'convert the npc() index number into a script npc reference
   '(also converts -1 failure value into 0 failure value)
   scriptret = (npcidx + 1) * -1
  END IF
 CASE 122'--get NPC ID
  ' Note: this command can be given an ID, effectively checking whether any NPCs with that ID exist
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = ABS(npc(npcref).id) - 1
  ELSE
   scriptret = -1
  END IF
 CASE 123'--NPC copy count
  scriptret = 0
  DIM pool as integer = get_optional_arg(1, 0)
  IF bound_arg(pool, 0, 1, "pool id") THEN
   IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npool(pool).npcs) THEN
    FOR i as integer = 0 TO UBOUND(npc)
     IF npc(i).id - 1 = retvals(0) ANDALSO npc(i).pool = pool THEN
      scriptret = scriptret + 1
     END IF
    NEXT i
   END IF
  END IF
 CASE 124'--change NPC ID
  DIM pool as integer = get_optional_arg(2, -1)
  npcref = getnpcref(retvals(0), 0, IIF(pool=-1, 0, pool))
  IF npcref >= 0 THEN
   IF pool = -1 THEN pool = npc(npcref).pool
   IF bound_arg(pool, 0, 1, "npc pool") THEN
    IF retvals(1) >= 0 AND retvals(1) <= UBOUND(npool(pool).npcs) THEN
     npc(npcref).id = retvals(1) + 1
     npc(npcref).pool = pool
     '--update the walkabout sprite for the changed NPC
     set_walkabout_sprite npc(npcref).sl, npool(pool).npcs(retvals(1)).picture, npool(pool).npcs(retvals(1)).palette
     '--run visnpc to apply any changes to the NPCs tag-visibility
     visnpc
    END IF
   END IF
  END IF
 CASE 125'--create NPC
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= UBOUND(npool(0).npcs) THEN
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
    DIM msgtemp as string = "create NPC: trying to create NPC id " & retvals(0) & " at " & XY(retvals(1), retvals(2)) * 20
    IF i = -1 THEN 
     scripterr msgtemp & "; failed: too many NPCs exist"
    ELSE
     scripterr msgtemp & "; warning: had to overwrite tag-disabled NPC id " & ABS(npc(i).id)-1 & " at " & npc(i).pos & ": too many NPCs exist", serrWarn
    END IF
   END IF
   IF i > -1 THEN
    'This deletes the walkabout slice
    CleanNPCInst npc(i)
    DIM npc_id as integer = retvals(0)
    DIM pool as integer = get_optional_arg(4, 0)
    npc(i).id = npc_id + 1
    npc(i).pool = pool
    cropposition retvals(1), retvals(2), 1
    npc(i).x = retvals(1) * 20
    npc(i).y = retvals(2) * 20
    npc(i).dir = ABS(retvals(3)) MOD 4
    npc(i).sl = create_npc_slices(i)  'Calls set_walkabout_sprite
    'debug "npc(" & i & ").sl=" & npc(i).sl & " [create npc(" & retvals(0) & ")]"
    update_npc_zones i
    scriptret = (i + 1) * -1
   END IF
  END IF
 CASE 126 '--destroy NPC (aka delete NPC)
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
  IF bound_arg(retvals(1), 0, maxNPCDataField, "NPCstat: constant") THEN
   DIM pool as integer = get_optional_arg(2, -1)
   DIM npcid as NPCTypeID = get_valid_npc_id(retvals(0), serrBound, IIF(pool=-1, 0, pool))
   IF retvals(0) < 0 AND pool <> -1 THEN
    scripterr current_command_name() & ": npc pool argument is only allowed when using an NPC ID, but an NPC reference was used "
   ELSE
    IF npcid <> -1 THEN
     IF pool = -1 THEN
      IF retvals(0) < 0 THEN
       'Was specified as a reference, We want whatever NPC pool the NPC actually belongs to
       DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBound, 0)
       pool = npc(npcref).pool
      ELSE
       'A pool wasn't specified, assume the ID was a local ID
       pool = 0
      END IF
     END IF
     scriptret = GetNPCD(npool(pool).npcs(npcid), retvals(1))
     IF retvals(1) = 12 THEN  'NPCstat:script
      scriptret = decodetrigger(scriptret, NO)  'showerr=NO
     END IF
    END IF
   END IF
  END IF
 CASE 192'--NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = wtog_to_frame(npc(npcref).wtog)
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
   IF dat->paletted = NO OR dat->spritetype = sprTypeFrame THEN
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
   gen(genCameraMode) = slicecam
   gen(genCameraArg1) = retvals(0)
  END IF
 CASE 570'--get active battle pause on all menus
  scriptret = IIF(prefbit(13), 1, 0)  '"Pause on all battle menus & targeting"
 CASE 571'--set active battle pause on all menus
  setprefbit 13, retvals(0)
 CASE 572'--dissolve sprite
  IF valid_plotsprite(retvals(0)) THEN
   DissolveSpriteSlice plotslices(retvals(0)), retvals(1), retvals(2), retvals(3), retvals(4), retvals(5)
  END IF
 CASE 573'--cancel dissolve
  IF valid_plotsprite(retvals(0)) THEN
   CancelSpriteSliceDissolve plotslices(retvals(0))
  END IF
 CASE 574'--sprite is dissolving
  scriptret = 0
  IF valid_plotsprite(retvals(0)) THEN
   'Note that unlike "wait for dissolve", this isn't restricted to auto-dissolve
   IF SpriteSliceIsDissolving(plotslices(retvals(0)), NO) THEN scriptret = 1
  END IF
 CASE 575'--wait for dissolve
  IF valid_plotsprite(retvals(0)) THEN
   script_start_waiting(retvals(0))
  END IF
 CASE 576'--hide virtual gamepad
  gam.pad.script_hide_virtual_gamepad = YES
  gam.pad.script_show_virtual_gamepad = NO
  update_virtual_gamepad_display()
 CASE 577'--show virtual gamepad
  gam.pad.script_hide_virtual_gamepad = NO
  gam.pad.script_show_virtual_gamepad = YES
  update_virtual_gamepad_display()
 CASE 578'--auto virtual gamepad
  gam.pad.script_hide_virtual_gamepad = NO
  gam.pad.script_show_virtual_gamepad = NO
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
 CASE 587 '--slice child index
  IF valid_plotslice(retvals(0)) THEN
   scriptret = SliceIndexAmongSiblings(plotslices(retvals(0)))
  END IF
 CASE 588 '--create scroll
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slScroll, SliceTable.scriptsprite)
  scriptret = create_plotslice_handle(sl)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
 CASE 589 '--slice is scroll
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slScroll THEN scriptret = 1
  END IF
 CASE 590'--set scroll bar style
  IF valid_plotscrollslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 14, "box style") THEN
    ChangeScrollSlice plotslices(retvals(0)), retvals(1)
   END IF
  END IF
 CASE 591'--get scroll bar style
  IF valid_plotscrollslice(retvals(0)) THEN
   DIM dat as ScrollSliceData Ptr
   dat = GetScrollSliceData(plotslices(retvals(0)))
   scriptret = dat->style
  END IF
 CASE 592'--set scroll check depth
  IF valid_plotscrollslice(retvals(0)) THEN
   ChangeScrollSlice plotslices(retvals(0)), , retvals(1)
  END IF
 CASE 593'--get scroll check depth
  IF valid_plotscrollslice(retvals(0)) THEN
   DIM dat as ScrollSliceData Ptr
   dat = GetScrollSliceData(plotslices(retvals(0)))
   scriptret = dat->check_depth
  END IF
 CASE 594'--scroll to child (parent, descendent, apply_padding) aka scroll to slice
  IF valid_plotslice(retvals(0)) ANDALSO valid_plotslice(retvals(1)) THEN
   ScrollToChild plotslices(retvals(0)), plotslices(retvals(1)), get_optional_arg(2, NO)
  END IF
 CASE 598'--next npc reference
  'Argument should be 0 or an NPC reference (< 0)
  IF retvals(0) > 0 THEN
   scripterr current_command_name() & ": invalid npc reference " & retvals(0)
   scriptret = 0
  ELSE
   'Default to 0 if no more NPCs
   scriptret = 0
   'OK if this is past end of the array
   DIM first_npcref as integer = (-retvals(0) - 1) + 1
   FOR i as integer = first_npcref TO UBOUND(npc)
    IF npc(i).id > 0 THEN
     scriptret = (i + 1) * -1
     EXIT FOR
    END IF
   NEXT i
  END IF
 CASE 64'--get hero stat (hero, stat, type)
  'TODO: unfortunately this can also access hero level & "hero levelled" which will suck
  'when we want to add more stats. Backcompat bit needed.
  DIM slot as integer = bound(retvals(0), 0, 40)  'Might want to keep this bound() for backcompat?
  DIM statnum as integer = retvals(1)
  IF statnum = 12 ORELSE valid_stat(statnum) THEN
   WITH gam.hero(slot)
    IF retvals(2) = 0 THEN  'current stat
     IF statnum = 12 THEN
      'This is backcompat for a somewhat documented feature (used in a lot of games)
      scriptret = .lev
     ELSE
      scriptret = .stat.cur.sta(statnum)
     END IF
    ELSEIF retvals(2) = 1 THEN  'maximum stat
     IF statnum = 12 THEN
      'This is backcompat for a barely documented feature (used in a lot of games)
      scriptret = .lev_gain
     ELSE
      scriptret = .stat.max.sta(statnum)
     END IF
    ELSEIF retvals(2) = 2 THEN  'base stat
     IF statnum <> 12 THEN
      scriptret = .stat.base.sta(statnum)
     END IF
    ELSE
     scripterr "get hero stat: stat type not 'current stat', 'maximum stat' or 'base stat'"
    END IF
   END WITH
  END IF
 CASE 66'--add hero
  IF bound_arg(retvals(0), 0, gen(genMaxHero), "hero ID") THEN
   DIM slot as integer = first_free_slot_in_party()
   IF slot >= 0 THEN
    addhero retvals(0), slot
   END IF
   scriptret = slot
  END IF
 CASE 67'--delete hero (hero ID)
  IF party_size() > 1 AND retvals(0) >= 0 THEN
   DIM slot as integer = findhero(retvals(0), , serrWarn)
   IF slot > -1 THEN deletehero slot
  END IF
 CASE 68'--swap out hero
  DIM i as integer = findhero(retvals(0), , serrWarn)
  IF i > -1 THEN
   FOR o as integer = 40 TO 4 STEP -1
    IF gam.hero(o).id = -1 THEN
     doswap i, o
     IF active_party_size() = 0 THEN forceparty
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 69'--swap in hero
  DIM i as integer = findhero(retvals(0), -1, serrWarn)
  IF i > -1 THEN
   FOR o as integer = 0 TO 3
    IF gam.hero(o).id = -1 THEN
     doswap i, o
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 83'--set hero stat (hero, stat, value, type)
  'TODO: this command can also set hero level (without updating stats)
  ' which sucks for when we want to add more stats. Need backcompat bit.
  DIM slot as integer = bound(retvals(0), 0, 40)  'Might want to keep this bound() for backcompat?
  DIM statnum as integer = retvals(1)
  IF statnum = 12 ORELSE valid_stat(statnum) THEN
   WITH gam.hero(slot)
    IF retvals(3) = 0 THEN  'current stat
     IF statnum = 12 THEN
      'This is backcompat for a mostly undocumented feature (used in several games)
      .lev = retvals(2)
     ELSE
      .stat.cur.sta(statnum) = retvals(2)
      IF statnum = statHP THEN
       evalherotags
       tag_updates
      END IF
     END IF
    ELSEIF retvals(3) = 1 THEN  'maximum stat
     IF statnum = 12 THEN
      'This is backcompat for an undocumented feature (nonetheless used in several games)
      .lev_gain = retvals(2)
     ELSE
      .stat.base.sta(statnum) += retvals(2) - .stat.max.sta(statnum)
      .stat.max.sta(statnum) = retvals(2)
     END IF
    ELSEIF retvals(3) = 2 THEN  'base stat
     IF statnum <> 12 THEN
      .stat.base.sta(statnum) = retvals(2)
      recompute_hero_max_stats slot
     END IF
    ELSE
     scripterr "set hero stat: stat type not 'current stat', 'maximum stat' or 'base stat'"
    END IF
   END WITH
  END IF
 CASE 89'--swap by position
  doswap bound(retvals(0), 0, 40), bound(retvals(1), 0, 40)
  'FIXME: missing forceparty call! (bug #1111)
 CASE 110'--set hero picture
  IF valid_hero_party(retvals(0)) THEN
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
  IF valid_hero_party(retvals(0)) THEN
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
  stop_fibre_timing
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    status_screen retvals(0)
   END IF
  END IF
  start_fibre_timing
 CASE 152'--spells menu
  stop_fibre_timing
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    old_spells_menu retvals(0)
   END IF
  END IF
  start_fibre_timing
 CASE 154'--equip menu(who [, allow_switch])
  retvals(1) = get_optional_arg(1, 1)
  stop_fibre_timing
  DIM allow_switch as bool
  allow_switch = (retvals(1) <> 0)
  'Can explicitly choose a hero to equip
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF gam.hero(retvals(0)).id >= 0 THEN
    equip_menu retvals(0), allow_switch
   END IF
  ELSEIF retvals(0) = -1 THEN
   'Or pass -1 to equip the first hero in the party
   equip_menu rank_to_party_slot(0), allow_switch
  END IF
  start_fibre_timing
 CASE 157'--order menu
  stop_fibre_timing
  hero_swap_menu 0
  start_fibre_timing
 CASE 158'--team menu
  stop_fibre_timing
  hero_swap_menu 1
  start_fibre_timing
 CASE 183'--set hero level (who, what, allow forgetting spells)
  IF valid_hero_party(retvals(0)) AND retvals(1) >= 0 THEN  'we should make the regular level limit customisable anyway
   gam.hero(retvals(0)).lev_gain = retvals(1) - gam.hero(retvals(0)).lev
   gam.hero(retvals(0)).lev = retvals(1)
   gam.hero(retvals(0)).exp_next = exptolevel(retvals(1) + 1, gam.hero(retvals(0)).exp_mult)
   gam.hero(retvals(0)).exp_cur = 0  'XP attained towards the next level
   updatestatslevelup retvals(0), retvals(2) <> 0 'updates stats and spells
   evalherotags
   tag_updates
  END IF
 CASE 184'--give experience (who, how much)
  'who = -1 targets battle party
  IF retvals(0) <> -1 THEN
   IF valid_hero_party(retvals(0)) THEN
    giveheroexperience retvals(0), retvals(1)
    updatestatslevelup retvals(0), NO
   END IF
  ELSE
   'This sets the level gain and learnt spells and calls updatestatslevelup for every hero
   distribute_party_experience retvals(1)
  END IF
  evalherotags
  tag_updates
 CASE 185'--hero levelled (who)
  scriptret = gam.hero(bound(retvals(0), 0, 40)).lev_gain
 CASE 186 /'spells learnt'/, 469 /'spells learned'/
  'NOTE: 'spells learnt' is deprecated but will remain for backcompat. New games should use "spells learned".
  'spells learned returns the spell ID offset by 1, for consistency with other attack commands and atk:name
  'constants, which are all consistently wrong!
  DIM found as integer = 0
  IF valid_hero_party(retvals(0)) THEN
   WITH gam.hero(retvals(0))
    FOR i as integer = 0 TO 4 * 24 - 1
     IF readbit(.learnmask(), 0, i) THEN
      IF retvals(1) = found THEN
       scriptret = .spells(i \ 24, i MOD 24)  'Attack ID + 1
       IF cmdid = 186 THEN scriptret -= 1
       EXIT FOR
      END IF
      found += 1
     END IF
    NEXT
    IF retvals(1) = -1 THEN scriptret = found  'getcount
   END WITH
  END IF
 CASE 269'--total experience
  IF valid_hero_party(retvals(0)) THEN
   scriptret = hero_total_exp(retvals(0))
  END IF
 CASE 270'--experience to level
  retvals(1) = get_optional_arg(1, -1)
  IF retvals(1) = -1 ORELSE valid_hero_party(retvals(1)) THEN
   IF retvals(1) = -1 THEN
    'Default experience curve
    scriptret = total_exp_to_level(retvals(0))
   ELSEIF gam.hero(retvals(1)).id >= 0 THEN
    scriptret = total_exp_to_level(retvals(0), gam.hero(retvals(1)).exp_mult)
   ELSE
    scripterr interpreter_context_name() + "empty hero slot " & retvals(1)
   END IF
  END IF
 CASE 271'--experience to next level
  IF valid_hero_party(retvals(0)) THEN
   scriptret = gam.hero(retvals(0)).exp_next - gam.hero(retvals(0)).exp_cur
  END IF
 CASE 272'--set experience  (who, what, allowforget)
  IF valid_hero_party(retvals(0)) AND retvals(1) >= 0 THEN
   setheroexperience retvals(0), retvals(1), (retvals(2) <> 0)
  END IF
 CASE 445'--update level up learning(who, allowforget)
  IF valid_hero_party(retvals(0)) THEN
   learn_spells_for_current_level retvals(0), (retvals(1) <> 0)
  END IF
 CASE 449'--reset hero picture
  DIM heronum as integer = retvals(0)
  DIM whichsprite as integer = retvals(1)
  IF really_valid_hero_party(heronum, , serrBound) THEN
   IF bound_arg(whichsprite, 0, 2, "hero picture type") THEN
    DIM her as HeroDef
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
    DIM her as HeroDef
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
  'This command tries to guess the best method for your current platform
  IF valid_plotstr(retvals(0)) THEN
   IF running_on_mobile() THEN
    'Mobile with touchscreen. Player argument ignored for now.
    hide_virtual_gamepad()
    gam.pad.being_shown = NO
    plotstr(retvals(0)).s = touch_virtual_keyboard(plotstr(retvals(0)).s, retvals(1))
    update_virtual_gamepad_display()
   ELSE
    'Desktop (arrow keys) and console (d-pad)
    plotstr(retvals(0)).s = gamepad_virtual_keyboard(plotstr(retvals(0)).s, retvals(1), retvals(2))
   END IF
  END IF
 CASE 557'--get item description(str,itm)
  scriptret = 0
  IF valid_plotstr(retvals(0)) THEN
   IF valid_item(retvals(1)) THEN
    plotstr(retvals(0)).s = readitemdescription(retvals(1))
    scriptret = 1
   END IF
  END IF
 CASE 599 '--input string with mouse keyboard (string ID, maxlen)
  IF valid_plotstr(retvals(0)) THEN
   hide_virtual_gamepad()
   plotstr(retvals(0)).s = touch_virtual_keyboard(plotstr(retvals(0)).s, retvals(1))
   update_virtual_gamepad_display()
  END IF
 CASE 600 '--running on ouya
  'See also "running on console"
  scriptret = IIF(running_on_ouya(), 1, 0)
 CASE 601 '--unhide mouse cursor
  showmousecursor
  mouserect -1, -1, -1, -1
 CASE 602 '--hide mouse cursor
  hidemousecursor
 CASE 603'--pixel focus camera
  gen(genCameraMode) = focuscam
  gen(genCameraArg1) = retvals(0) - get_resolution().w / 2
  gen(genCameraArg2) = retvals(1) - get_resolution().h / 2
  gen(genCameraArg3) = ABS(retvals(2))
  gen(genCameraArg4) = ABS(retvals(2))
  limitcamera gen(genCameraArg1), gen(genCameraArg2)
 CASE 604 '--send email (save slot, subject string id, body string id)
  IF retvals(0) = 0 ORELSE valid_save_slot(retvals(0)) THEN
   DIM as string subject, body
   IF retvals(1) <> -1 ANDALSO valid_plotstr(retvals(1)) THEN  'subject string id
    subject = plotstr(retvals(1)).s
   END IF
   IF retvals(2) <> -1 ANDALSO valid_plotstr(retvals(2)) THEN  'body string id
    body = plotstr(retvals(2)).s
   END IF
   email_save_to_developer retvals(0) - 1, "", subject, body
  END IF
 CASE 605 '--dump slice tree
  IF retvals(0) = 0 THEN
   SliceDebugDumpTree SliceTable.Root
  ELSEIF valid_plotslice(retvals(0)) THEN
   SliceDebugDumpTree plotslices(retvals(0))
  END IF
 CASE 606 '--create panel
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slPanel, SliceTable.scriptsprite)
  scriptret = create_plotslice_handle(sl)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
 CASE 607 '--slice is panel
  IF valid_plotslice(retvals(0)) THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slPanel THEN scriptret = 1
  END IF
 CASE 608'--get panel is vertical
  IF valid_plotpanelslice(retvals(0)) THEN
   DIM dat as PanelSliceData Ptr
   dat = GetPanelSliceData(plotslices(retvals(0)))
   scriptret = IIF(dat->vertical, 1, 0)
  END IF
 CASE 609'--set panel is vertical
  IF valid_plotpanelslice(retvals(0)) THEN
   ChangePanelSlice plotslices(retvals(0)), retvals(1) <> 0
  END IF
 CASE 610'--get panel primary index
  IF valid_plotpanelslice(retvals(0)) THEN
   DIM dat as PanelSliceData Ptr
   dat = GetPanelSliceData(plotslices(retvals(0)))
   scriptret = dat->primary
  END IF
 CASE 611'--set panel primary index
  IF valid_plotpanelslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 1, "panel child index") THEN
    ChangePanelSlice plotslices(retvals(0)), , retvals(1)
   END IF
  END IF
 CASE 612'--get panel percent as int
  IF valid_plotpanelslice(retvals(0)) THEN
   DIM dat as PanelSliceData Ptr
   dat = GetPanelSliceData(plotslices(retvals(0)))
   scriptret = CINT(dat->percent * 100)
  END IF
 CASE 613'--set panel percent
  IF valid_plotpanelslice(retvals(0)) THEN
   IF bound_arg(retvals(1), 0, 100, "percent") THEN
    ChangePanelSlice plotslices(retvals(0)), , , , CDBL(retvals(1))
   END IF
  END IF
 CASE 614'--get panel pixels
  IF valid_plotpanelslice(retvals(0)) THEN
   DIM dat as PanelSliceData Ptr
   dat = GetPanelSliceData(plotslices(retvals(0)))
   scriptret = dat->pixels
  END IF
 CASE 615'--set panel pixels
  IF valid_plotpanelslice(retvals(0)) THEN
   ChangePanelSlice plotslices(retvals(0)), , , retvals(1)
  END IF
 CASE 616'--get panel padding
  IF valid_plotpanelslice(retvals(0)) THEN
   DIM dat as PanelSliceData Ptr
   dat = GetPanelSliceData(plotslices(retvals(0)))
   scriptret = dat->padding
  END IF
 CASE 617'--set panel padding
  IF valid_plotpanelslice(retvals(0)) THEN
   ChangePanelSlice plotslices(retvals(0)), , , , , retvals(1)
  END IF
 CASE 621'--get battle countdown
  scriptret = gam.random_battle_countdown
 CASE 622'--set battle countdown
  gam.random_battle_countdown = large(0, retvals(0))
 CASE 626 '--textbox text (string, box, expand, strip)
  IF valid_plotstr(retvals(0), serrBadOp) ANDALSO _
     bound_arg(retvals(1), 0, gen(genMaxTextbox), "textbox", , serrBadOp) THEN
   DIM box as TextBox
   LoadTextBox box, retvals(1)
   plotstr(retvals(0)).s = textbox_lines_to_string(box)
   IF retvals(3) THEN plotstr(retvals(0)).s = trim(plotstr(retvals(0)).s)
   IF retvals(2) THEN embedtext plotstr(retvals(0)).s
  END IF
 CASE 628'--pathfind npc to
  DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBadOp)
  IF npcref >= 0 THEN
   cancel_npc_movement_override (npc(npcref))
   npc(npcref).pathover.override = NPCOverrideMove.POS
   npc(npcref).pathover.dest_pos = XY(retvals(1), retvals(2))
   npc(npcref).pathover.stop_after_stillticks = retvals(3)
   IF npc(npcref).pathover.stop_after_stillticks THEN
    npc(npcref).stillticks = 0
   END IF
  END IF
 CASE 629'--npc chases npc
  DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBadOp)
  DIM dest_npcref as NPCIndex = get_valid_npc(retvals(1), serrBadOp)
  IF npcref >= 0 ANDALSO dest_npcref <> -1 THEN
   cancel_npc_movement_override (npc(npcref))
   npc(npcref).pathover.override = NPCOverrideMove.NPC
   npc(npcref).pathover.dest_npc = dest_npcref
   npc(npcref).pathover.stop_when_npc_reached = (retvals(2) <> 0)
   npc(npcref).pathover.stop_after_stillticks = retvals(3)
   IF npc(npcref).pathover.stop_after_stillticks THEN
    npc(npcref).stillticks = 0
   END IF
  END IF
 CASE 630'--cancel npc walk
  DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBadOp)
  IF npcref >= 0 THEN
   cancel_npc_movement_override (npc(npcref))
   cancel_npc_walk (npc(npcref))
  END IF
 CASE 631'--player is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendplayer), 1, 0)
 CASE 632'--npcs are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendnpcs), 1, 0)
 CASE 633'--obstruction is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendobstruction), 1, 0)
 CASE 634'--hero walls are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendherowalls), 1, 0)
 CASE 635'--npc walls are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendnpcwalls), 1, 0)
 CASE 636'--caterpillar is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendcaterpillar), 1, 0)
 CASE 637'--doors are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspenddoors), 1, 0)
 CASE 638'--random enemies are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendrandomenemies), 1, 0)
 CASE 639'--box advance is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendboxadvance), 1, 0)
 CASE 640'--overlay is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendoverlay), 1, 0)
 CASE 641'--map music is suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendambientmusic), 1, 0)
 CASE 642'--timers are suspended
  scriptret = IIF(readbit(gen(), genSuspendBits, suspendtimers), 1, 0)
 CASE 643'--get screen width
  scriptret = gen(genResolutionX)
 CASE 644'--get screen height
  scriptret = gen(genResolutionY)
 CASE 645'--set screen resolution
  'FIXME: this is secret and undocumented until the gfx_directx backends supports resolution changing
  IF retvals(0) < MinResolutionX ORELSE retvals(0) > MaxResolutionX ORELSE retvals(1) < MinResolutionY ORELSE retvals(1) > MaxResolutionY THEN
   scripterr interpreter_context_name() + "invalid resolution " & retvals(0) & "," & retvals(1) & " should be in the range of " & MinResolutionX & "," & MinResolutionY & " to " & MaxResolutionX & "," & MaxResolutionY, serrBound
  ELSE
   gen(genResolutionX) = retvals(0)
   gen(genResolutionY) = retvals(1)
   apply_game_window_settings()
  END IF
 CASE 647'--_cancel runfast
  use_speed_control = YES
 CASE 648'--_runfast
  use_speed_control = NO
 CASE 649'--multdiv
  'Return int(float(a)*b/c), clamped to a 32-bit int, and rounded
  '(Break ties towards +inf, since that's what JS does; FB/x86 breaks ties towards even)
  IF retvals(2) = 0 THEN
   scripterr strprintf("division by zero: %d*%d/0", retvals(0), retvals(1)), serrBadOp
  ELSE
   scriptret = INT(bound(CDBL(retvals(0)) * retvals(1) / retvals(2), CDBL(INT_MIN), CDBL(INT_MAX)) + 0.5)
  END IF
 CASE 650 '--set rect raw border
  IF bound_arg(retvals(1), -2, gen(genMaxBoxBorder), "raw border") THEN
   change_rect_plotslice retvals(0), , , , , , , retvals(1)
  END IF
 CASE 651 '--get rect raw border
  IF valid_plotrect(retvals(0)) THEN
   DIM dat as RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->use_raw_box_border THEN
    scriptret = dat->raw_box_border
   ELSEIF dat->border >= 0 THEN
    scriptret = boxlook(dat->border).border - 1  'possibly border:line
   ELSE
    scriptret = dat->border  'border:line or border:none
   END IF
  END IF
 CASE 652 '--clone slice(recurse)
  IF valid_plotslice(retvals(0)) THEN
   DIM as Slice ptr sl, ret
   sl = plotslices(retvals(0))
   IF sl->Parent THEN ret = CloneSliceTree(sl, retvals(1) <> 0, NO)
   IF ret = 0 THEN  'Returned in the following case:
    scripterr "cloneslice: Can't copy a Map layer slice or the Root slice"
   ELSE
    'sl has a parent
    InsertSliceBefore sl, ret
    scriptret = create_plotslice_handle(ret)
   END IF
  END IF
 CASE 653 '--reset formation
  IF valid_formation(retvals(0)) THEN
   DIM form as Formation
   LoadFormation form, game & ".for", retvals(0)
   SaveFormation form, tmpdir & "for.tmp", retvals(0)
  END IF
 CASE 654 '--reset formation slot
  IF valid_formation_slot(retvals(0), retvals(1)) THEN
   DIM orig_form as Formation
   LoadFormation orig_form, game & ".for", retvals(0)
   DIM cur_form as Formation
   LoadFormation cur_form, tmpdir & "for.tmp", retvals(0)
   cur_form.slots(retvals(1)) = orig_form.slots(retvals(1))
   SaveFormation cur_form, tmpdir & "for.tmp", retvals(0)
  END IF
 CASE 655 '--slice is map layer
  IF valid_plotslice(retvals(0)) THEN
   IF plotslices(retvals(0))->SliceType = slMap THEN scriptret = 1
  END IF
 CASE 656 '--npc reference from slice
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice ptr = plotslices(retvals(0))
   scriptret = 0
   IF *sl->Context IS NPCSliceContext THEN scriptret = -1 * (1 + CAST(NPCSliceContext ptr, sl->Context)->npcindex)
  END IF
 CASE 657 '--hero rank from slice
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice ptr = plotslices(retvals(0))
   scriptret = -1
   IF *sl->Context IS HeroSliceContext THEN
    scriptret = party_slot_to_rank(CAST(HeroSliceContext ptr, sl->Context)->slot)
   END IF
  END IF
 CASE 658 '--slice type
  IF valid_plotslice(retvals(0)) THEN
   scriptret = plotslices(retvals(0))->SliceType
  END IF
 CASE 659 '--_asserteq(x, y, stringid, stringoffset)
  IF retvals(0) <> retvals(1) THEN
   IF bound_arg(retvals(2), 0, UBOUND(plotstr), "string ID", "assert expression string") THEN
    plotstr(retvals(2)).s = script_string_constant(nowscript, retvals(3)) & _
                            " [actual values were " & retvals(0) & " == " & retvals(1) & "]"
    scriptret = 1
   END IF
  END IF
 CASE 660 '--save screenshot
  DIM fname as string = screenshot()
  show_overlay_message "Saved screenshot " & trimpath(fname), 1.5
 CASE 661 '--slice is line
  IF valid_plotslice(retvals(0)) THEN
   scriptret = (plotslices(retvals(0))->SliceType = slLine)
  END IF
 CASE 662 '--create line
  IF valid_color(retvals(2)) THEN
   DIM sl as Slice ptr
   sl = NewSliceOfType(slLine, SliceTable.scriptsprite)
   scriptret = create_plotslice_handle(sl)
   sl->Width = retvals(0)
   sl->Height = retvals(1)
   DIM dat as LineSliceData ptr = sl->SliceData
   dat->SetColor(retvals(2))
  END IF
 CASE 663 '--get line color
  IF valid_plotlineslice(retvals(0)) THEN
   DIM dat as LineSliceData ptr = plotslices(retvals(0))->SliceData
   scriptret = dat->col
  END IF
 CASE 664 '--set line color
  IF valid_plotlineslice(retvals(0)) THEN
   IF valid_color(retvals(1)) THEN
    DIM dat as LineSliceData ptr = plotslices(retvals(0))->SliceData
    dat->SetColor(retvals(1))
   END IF
  END IF
 CASE 665 '--force mount vehicle
  npcref = get_valid_npc(retvals(0))
  IF npcref >= 0 THEN
   forcemountvehicle npcref
  END IF
 CASE 666 '--current vehicle id
  IF vstate.active THEN
   scriptret = vstate.id
  ELSE
   scriptret = -1
  END IF
 CASE 667 '--current vehicle npc
  IF vstate.active THEN
   'Return an NPC reference
   scriptret = (vstate.npc + 1) * -1
  ELSE
   'Not riding
   scriptret = 0
  END IF
 CASE 668 '--pathfind hero to
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   cancel_hero_pathfinding(retvals(0))
   path_hero_to_tile(retvals(0), XY(retvals(1), retvals(2)), retvals(3))
  END IF
 CASE 669 '--hero chases npc
  DIM dest_npcref as NPCIndex = get_valid_npc(retvals(1), serrBadOp)
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   cancel_hero_pathfinding(retvals(0))
   path_hero_to_npc(retvals(0), dest_npcref, retvals(2) <> 0, retvals(3))
  END IF
 CASE 670 '--cancel hero walk
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   cancel_hero_pathfinding(retvals(0))
   cancel_hero_walk(retvals(0))
  END IF
 CASE 675 '--speaking npc
  scriptret = -1 - txt.sayer  '0 if no textbox or not triggered by an NPC
 CASE 676 '--keypress (scancode, joynum)
  a_script_wants_keys()
  scriptret = IIF(script_keyval(retvals(0), retvals(1)) > 1, 1, 0)
 CASE 677 '--new keypress (scancode, joynum)
  a_script_wants_keys()
  scriptret = IIF(script_keyval(retvals(0), retvals(1)) AND 4, 1, 0)
 CASE 678 '--get joystick name (stringid, joynum)
  IF valid_plotstr(retvals(0), serrBadOp) ANDALSO valid_joystick(retvals(1)) THEN
   DIM info as JoystickInfo ptr = joystick_info(retvals(1))
   IF info THEN
    plotstr(retvals(0)).s = info->name
   END IF
  END IF
 CASE 679 '--joystick button count (joynum)
  IF valid_joystick(retvals(0)) THEN
   DIM info as JoystickInfo ptr = joystick_info(retvals(0))
   IF info THEN
    scriptret = info->num_buttons
   END IF
  END IF
 CASE 680 '--joystick axis count (joynum)
  IF valid_joystick(retvals(0)) THEN
   DIM info as JoystickInfo ptr = joystick_info(retvals(0))
   IF info THEN
    scriptret = info->num_axes
   END IF
  END IF
 CASE 681 '--joystick hat count (joynum)
  IF valid_joystick(retvals(0)) THEN
   DIM info as JoystickInfo ptr = joystick_info(retvals(0))
   IF info THEN
    scriptret = info->num_hats
   END IF
  END IF
 CASE 682 '--find color(r, g, b, searchstart)
  'r, g, b don't have to be in the range 0-255.
  scriptret = nearcolor(master(), retvals(0), retvals(1), retvals(2), retvals(3))
 CASE 683 '--override tick milliseconds(ms)
  IF bound_arg(retvals(0), 16, 200, "milliseconds (must be in the range 16 to 200)") THEN
   set_speedcontrol retvals(0)
   set_animation_framerate retvals(0)
  END IF
 CASE 684 '--cancel override tick milliseconds
  set_speedcontrol gen(genMillisecPerFrame)
  set_animation_framerate gen(genMillisecPerFrame)
 CASE 685'--suspend textbox controls
  setbit gen(), genSuspendBits, suspendtextboxcontrols, 1
 CASE 686'--resume textbox controls
  setbit gen(), genSuspendBits, suspendtextboxcontrols, 0
 CASE 687'--textbox controls are suspended
  scriptret = readbit(gen(), genSuspendBits, suspendtextboxcontrols)
 CASE 688'--menu item count (menu)
  IF valid_menu_handle(retvals(0), menuslot) THEN
   scriptret = menus(menuslot).numitems
  END IF
 CASE 689'--visible menu item count (menu)
  IF valid_menu_handle(retvals(0), menuslot) THEN
   scriptret = count_visible_menu_items(menus(menuslot))
  END IF
 CASE 690 '-- replace substring (in string ID, replace what ID, with what ID, max replacements, case insensitive)
  IF valid_plotstr(retvals(0), serrBadOp) ANDALSO _
     valid_plotstr(retvals(1), serrBadOp) ANDALSO _
     valid_plotstr(retvals(2), serrBadOp) THEN
   scriptret = replacestr(plotstr(retvals(0)).s, plotstr(retvals(1)).s, plotstr(retvals(2)).s, retvals(3), (retvals(4) <> 0))
  END IF
 CASE 691 '--decode trigger
  scriptret = decodetrigger(retvals(0), NO)  'showerr=NO
 CASE 692 '--get scancode name (string id, scancode, long name)
  'TODO: doesn't support joystick scancodes
  IF valid_plotstr(retvals(0)) ANDALSO bound_arg(retvals(1), ccLOWEST, scJoyLAST, "keyboard scancode") THEN
   plotstr(retvals(0)).s = scancodename(retvals(1), retvals(2) <> 0)
  END IF
 CASE 693 '--get hero slice by slot
  'Empty party slots are OK, returns 0
  IF valid_hero_party(retvals(0)) THEN
   scriptret = find_plotslice_handle(gam.hero(retvals(0)).sl)
  END IF
 CASE 694 '--hero slot from slice
  IF valid_plotslice(retvals(0)) THEN
   DIM sl as Slice ptr = plotslices(retvals(0))
   scriptret = -1
   IF *sl->Context IS HeroSliceContext THEN
    scriptret = CAST(HeroSliceContext ptr, sl->Context)->slot
   END IF
  END IF
 CASE 695 '--get opacity (slice)
  IF valid_plotslice(retvals(0)) THEN
   'Non-blendable slice types allowed
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)), NO)
   IF drawopts ANDALSO drawopts->with_blending THEN
    scriptret = 100 * drawopts->opacity
   ELSE
    scriptret = 100
   END IF
  END IF
 CASE 696 '--set opacity (slice, opacity)
  IF valid_plotslice(retvals(0)) THEN
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)))
   IF drawopts THEN
    'Setting opacity to a value outside 0-100% is not an error
    drawopts->opacity = bound(retvals(1), 0, 100) * 0.01
    IF retvals(1) < 100 THEN drawopts->with_blending = YES
   END IF
  END IF
 CASE 697 '--get blending enabled (slice)
  scriptret = 0
  IF valid_plotslice(retvals(0)) THEN
   'Non-blendable slice types allowed
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)), NO)
   IF drawopts ANDALSO drawopts->with_blending THEN
    scriptret = 1
   END IF
  END IF
 CASE 698 '--set blending enabled (slice, bool)
  IF valid_plotslice(retvals(0)) THEN
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)))
   IF drawopts THEN drawopts->with_blending = retvals(1) <> 0
  END IF
 CASE 699 '--get blend mode (slice)
  IF valid_plotslice(retvals(0)) THEN
   'Non-blendable slice types allowed
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)), NO)
   IF drawopts ANDALSO drawopts->with_blending THEN
    scriptret = drawopts->blend_mode
   ELSE
    scriptret = blendModeNormal
   END IF
  END IF
 CASE 700 '--set blend mode (slice, blendmode)
  IF valid_plotslice(retvals(0)) ANDALSO bound_arg(retvals(1), 0, blendModeLast, "blend mode", , serrBadOp) THEN
   DIM drawopts as DrawOptions ptr = get_slice_drawopts(plotslices(retvals(0)))
   IF drawopts THEN
    drawopts->blend_mode = retvals(1)
    drawopts->with_blending = YES
   END IF
  END IF
 CASE 703 '--set timer args (id, args...)
  IF curcmd->argc = 0 THEN
   scripterr "set_timer_args needs at least one argument: ID", serrBadOp
  ELSEIF bound_arg(retvals(0), 0, UBOUND(timers), "timer ID") THEN
   WITH timers(retvals(0))
    IF .trigger <= 0 THEN
     scripterr "Timer " & retvals(0) & " isn't set to trigger a script. Call ""set timer"" before ""set timer args"".", serrBadOp
    ELSE
     'The number of args passed can be 0, in which case we REDIM -1 TO -1,
     'which is different from an un-DIM'd arraay which is 0 TO -1 and causes
     'the default timer ID arg to be passed.
     REDIM .script_args(-1 TO curcmd->argc - 2)
     FOR i as integer = 1 TO curcmd->argc - 1
      .script_args(i - 1) = retvals(i)
     NEXT
    END IF
   END WITH
  END IF
 CASE 704'-- expand strings in slices(sl, saveslot)
  retvals(1) = get_optional_arg(1, 0)
  IF valid_plotslice(retvals(0)) THEN
   'Retvals(1) can be 0 for the default of using current game state, or a save slot 1-maxSaveSlotCount
   IF retvals(1) = 0 ORELSE valid_save_slot(retvals(1)) THEN
    embedslicetree plotslices(retvals(0)), retvals(1) - 1
   END IF
   scriptret = retvals(0)
  END IF
 CASE 705'-- hero is chasing
  IF valid_hero_caterpillar_rank(retvals(0)) THEN
   IF hero_is_pathfinding(retvals(0)) THEN
    IF gam.hero_pathing(retvals(0)).mode = HeroPathingMode.NPC THEN
     'If the hero is actually chasing an NPC, convert the NPC number into an NPC reference
     scriptret = (gam.hero_pathing(retvals(0)).dest_npc + 1) * -1
    END IF
   END IF
  END IF
 CASE 706'-- npc is chasing
  DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBadOp)
  IF npcref >= 0 THEN
   IF npc(npcref).pathover.override = NPCOverrideMove.NPC THEN
    'If the npc is actualy chasing another NPC, convert the target NPC number into an NPC reference
    scriptret = (npc(npcref).pathover.dest_npc + 1) * -1
   END IF
  END IF
 CASE 707'-- max map id
  scriptret = gen(genMaxMap)
 CASE 708'-- get slice lookup name (string, code, use default)
  IF valid_plotstr(retvals(0)) THEN
   plotstr(retvals(0)).s = SliceLookupCodename(retvals(1), retvals(2))
  END IF
 CASE 709'-- get npc pool
  IF retvals(0) > 0 THEN
   scripterr current_command_name() & ": invalid npc reference " & retvals(0)
   scriptret = 0
  ELSE
   DIM npcref as NPCIndex = get_valid_npc(retvals(0), serrBound, 0)
   IF npcref >= 0 THEN
    scriptret = npc(npcref).pool
   END IF
  END IF

 CASE ELSE
  'We also check the HSP header at load time to check there aren't unsupported commands
  scripterr "Unsupported script command " & cmdid & " " & commandname(cmdid) & ". " _
            "Try downloading the latest version of the OHRRPGCE.", serrError

 END SELECT
END SUB


'==========================================================================================
'                                     Music commands
'==========================================================================================


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


'==========================================================================================
'                                      NPC references
'==========================================================================================


'Implementation of "npc reference".
'Deprecated; Use get_valid_npc for all new NPC commands
FUNCTION getnpcref (byval seekid as NPCScriptref, byval copynum as integer, byval pool as integer=0) as NPCIndex
 SELECT CASE seekid
 CASE -300 TO -1'--direct reference
  RETURN (seekid + 1) * -1

 CASE 0 TO UBOUND(npool(pool).npcs) 'ID
  DIM found as integer = 0
  FOR i as integer = 0 TO UBOUND(npc)
   IF npc(i).id - 1 = seekid ANDALSO npc(i).pool = pool THEN
    IF found = copynum THEN
     RETURN i
    END IF
    found += 1
   END IF
  NEXT i
 END SELECT

 '--failure
 RETURN -1
END FUNCTION

'Replacement for getnpcref.
'Given NPC ref or NPC ID, return npc() index, or throw a scripterr and return -1
'Note this is stricter than getnpcref: invalid npc refs are not alright!
'References to Hidden/Disabled NPCs are alright.
FUNCTION get_valid_npc (byval seekid as NPCScriptref, byval errlvl as scriptErrEnum = serrBadOp, byval pool as integer=0) as NPCIndex
 IF seekid < 0 THEN
  DIM npcidx as NPCIndex = (seekid + 1) * -1
  IF npcidx > UBOUND(npc) ORELSE npc(npcidx).id = 0 THEN
   scripterr current_command_name() & ": invalid npc reference " & seekid & " (maybe the NPC was deleted?)", errlvl
   RETURN -1
  END IF
  RETURN npcidx
 ELSE
  FOR i as integer = 0 TO UBOUND(npc)
   IF npc(i).id - 1 = seekid ANDALSO npc(i).pool = pool THEN RETURN i
  NEXT
  scripterr current_command_name() & ": invalid npc reference; no NPCs of ID " & seekid & " from pool " & pool & " exist", errlvl
  RETURN -1
 END IF
END FUNCTION

'Given NPC ref or NPC ID, return an NPC ID, or throw a scripterr and return -1
'References to Hidden/Disabled NPCs are alright.
FUNCTION get_valid_npc_id (byval seekid as NPCScriptref, byval errlvl as scriptErrEnum = serrBadOp, byval pool as integer=0) as NPCTypeID
 IF seekid >= 0 THEN
  IF seekid > UBOUND(npool(pool).npcs) THEN
   scripterr current_command_name() & ": invalid NPC ID " & seekid, errlvl
   RETURN -1
  END IF
  RETURN seekid
 ELSE
  DIM npcidx as NPCIndex = (seekid + 1) * -1
  IF npcidx > UBOUND(npc) THEN
   scripterr current_command_name() & ": invalid NPC reference " & seekid, errlvl
   RETURN -1
  ELSEIF npc(npcidx).id = 0 THEN
   scripterr current_command_name() & ": invalid NPC reference " & seekid & " (maybe the NPC was deleted?)", errlvl
   RETURN -1
  ELSE
   DIM id as NPCTypeID = ABS(npc(npcidx).id) - 1
   IF id > UBOUND(npool(pool).npcs) THEN
    'Note that an NPC may be marked hidden because it has an invalid ID
    scripterr current_command_name() & ": NPC reference " & seekid & " is for a disabled NPC with invalid ID " & npc(npcidx).id & " (the map must be incompletely loaded)", errlvl
    RETURN -1
   END IF
   RETURN id
  END IF
 END IF
END FUNCTION


'==========================================================================================
'                                      Slice handles
'==========================================================================================


FUNCTION valid_spriteslice_dat(byval sl as Slice Ptr) as bool
 IF sl = 0 THEN scripterr "null slice ptr in valid_spriteslice_dat", serrBug : RETURN NO
 DIM dat as SpriteSliceData Ptr = sl->SliceData
 IF dat = 0 THEN
  scripterr SliceTypeName(sl) & " handle " & retvals(0) & " has null dat pointer", serrBug
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_plotslice(byval handle as integer, byval errlev as scriptErrEnum = serrBadOp) as bool
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

FUNCTION valid_plotsprite(byval handle as integer) as bool
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

FUNCTION valid_plotrect(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slRectangle THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a rect", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plottextslice(byval handle as integer) as bool
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

FUNCTION valid_plotellipse(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slEllipse THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not an ellipse", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotlineslice(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slLine THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a line", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotgridslice(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slGrid THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a grid", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotselectslice(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slSelect THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a select", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotscrollslice(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slScroll THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a scroll", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotpanelslice(byval handle as integer) as bool
 IF valid_plotslice(handle) THEN
  IF plotslices(handle)->SliceType = slPanel THEN
   RETURN YES
  ELSE
   scripterr current_command_name() & ": slice handle " & handle & " is not a panel", serrBadOp
  END IF
 END IF
 RETURN NO
END FUNCTION

SUB unresizable_error(sl as Slice ptr, handle as integer, reason as string, errlvl as scriptErrEnum = serrBadOp)
 scripterr strprintf("%s: %s slice handle %d cannot be resized%s", current_command_name(), SliceTypeName(sl), handle, reason), errlvl
END SUB

FUNCTION valid_resizeable_slice(byval handle as integer, byval horiz_fill_ok as bool=NO, byval vert_fill_ok as bool=NO) as bool
 IF valid_plotslice(handle) THEN
  DIM sl as Slice Ptr
  sl = plotslices(handle)
  IF SlicePossiblyResizable(sl) = NO THEN
   'Text slices are resizable horizontally only if and only if they wrap
   'TODO: they are never resizable vertically, but for backcompat not doing anything about that now...
   IF sl->SliceType = slText THEN
    unresizable_error sl, handle, ", unless wrap is enabled"
   ' Scaling sprite slices aren't available in games yet.
   'ELSEIF sl->SliceType = slSprite THEN
   ' scripterr current_command_name() & ": sprite slice handle " & handle & " cannot be resized unless scaling is enabled", serrBadOp
   ELSE
    unresizable_error sl, handle, ", due to its type"
   END IF
   RETURN NO
  END IF

  'This is only for "set slice width/height"; "fill parent" needs to do its own checks
  IF ((sl->CoverChildren AND coverHoriz) ANDALSO horiz_fill_ok = NO) ORELSE _
     ((sl->CoverChildren AND coverVert)  ANDALSO vert_fill_ok = NO) THEN
   unresizable_error sl, handle, " while Covering Children", serrWarn
   RETURN NO
  END IF

  IF sl->Fill = NO THEN RETURN YES
  SELECT CASE sl->Fillmode
   CASE sliceFillFull
    IF horiz_fill_ok ANDALSO vert_fill_ok THEN RETURN YES
   CASE sliceFillHoriz
    IF horiz_fill_ok THEN RETURN YES
   CASE sliceFillVert
    IF vert_fill_ok THEN RETURN YES
  END SELECT
  'Maybe this should be just an info message?
  unresizable_error sl, handle, " while Filling Parent", serrWarn
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
 'If a lot of slices have been deleted, then loop back to the beginning
 'to find reusable slices. We delay doing this so that handles are less likely
 'to get reused quickly (which we don't actually want).
 'In future, new script interpreter's garbage collection will obsolete this.
 IF num_reusable_slice_handles > 5000 THEN
  next_slice_handle = LBOUND(plotslices)
  num_reusable_slice_handles = 0
 END IF

 DIM i as integer
 FOR i = next_slice_handle to UBOUND(plotslices)
  IF plotslices(i) = 0 THEN
   'Store the slice pointer in the handle slot
   plotslices(i) = sl
   'Store the handle slot in the slice
   sl->TableSlot = i
   next_slice_handle = i + 1
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
 next_slice_handle = i + 1
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
 IF sl = 0 THEN showbug "set_plotslice_handle null ptr"
 IF sl->TableSlot <> 0 THEN
  showbug "set_plotslice_handle shouldn't be called on a slice with existing TableSlot"
 END IF

 IF handle > UBOUND(plotslices) THEN
  REDIM PRESERVE plotslices(LBOUND(plotslices) TO handle * 1.5 + 32)
  plotslicesp = @plotslices(1)
 END IF

 IF plotslices(handle) <> 0 THEN
  showbug "set_plotslice_handle: non-empty plotslices(" & handle & ")"
 END IF
 
 'Store the slice pointer in the handle slot
 plotslices(handle) = sl
 'Store the handle slot in the slice
 sl->TableSlot = handle
END SUB

FUNCTION valid_spriteset(spritetype as SpriteType, record as integer) as bool
 RETURN bound_arg(record, 0, sprite_sizes(spritetype).lastrec, "spriteset number", , serrBadOp)
END FUNCTION

'By default, no palette set
FUNCTION load_sprite_plotslice(byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2) as integer
 IF valid_spriteset(spritetype, record) THEN
  DIM sl as Slice Ptr
  sl = NewSliceOfType(slSprite, SliceTable.scriptsprite)
  ChangeSpriteSlice sl, spritetype, record, pal
  RETURN create_plotslice_handle(sl)
 ELSE
  RETURN 0 'Failure, return zero handle
 END IF
END FUNCTION

'By default, no palette change
SUB replace_sprite_plotslice(byval handle as integer, byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2)
 IF valid_plotsprite(handle) THEN
  IF valid_spriteset(spritetype, record) THEN
   ChangeSpriteSlice plotslices(handle), spritetype, record, pal
  END IF
 END IF
END SUB

SUB change_rect_plotslice(byval handle as integer, byval style as integer=-2, byval bgcol as integer=-99, byval fgcol as integer=-99, byval border as RectBorderTypes=borderUndef, byval translucent as RectTransTypes=transUndef, byval fuzzfactor as integer=0, byval raw_box_border as RectBorderTypes=borderUndef)
 IF valid_plotslice(handle) THEN
  DIM sl as Slice Ptr
  sl = plotslices(handle)
  IF sl->SliceType = slRectangle THEN
   ChangeRectangleSlice sl, style, bgcol, fgcol, border, translucent, fuzzfactor, raw_box_border
  ELSE
   scripterr current_command_name() & ": " & SliceTypeName(sl) & " is not a rect", serrBadOp
  END IF
 END IF
END SUB

'Get a slice's DrawOptions, or NULL if it doesn't have any.
'required: whether to throw an error on wrong type.
FUNCTION get_slice_drawopts(sl as Slice ptr, required as bool = YES) as DrawOptions ptr
 BUG_IF(sl = NULL, "null ptr", NULL)
 IF sl->SliceType = slSprite THEN
  RETURN @sl->SpriteData->drawopts
 ELSEIF sl->SliceType = slMap THEN
  RETURN @sl->MapData->drawopts
 ELSEIF required THEN
  scripterr strprintf("%s: %s slices don't have blending settings. " _
                      "This command can only be used on Sprite or Map layer slices", _
                      current_command_name(), SliceTypeName(sl))
 END IF
 RETURN NULL
END FUNCTION


'==========================================================================================
'                                Menu and menuitem handles
'==========================================================================================


FUNCTION find_menu_id (byval id as integer) as integer
 DIM i as integer
 FOR i = topmenu TO 0 STEP -1
  IF menus(i).record = id THEN
   RETURN i 'return slot
  END IF
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

'NOTE: you should nearly always use valid_menu_handle instead, which throws an error.
FUNCTION find_menu_handle (byval handle as integer) as integer
 DIM i as integer
 FOR i = 0 TO topmenu
  IF menus(i).handle = handle THEN RETURN i 'return slot
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

FUNCTION valid_menu_handle (handle as integer, byref found_in_menuslot as integer) as bool
 found_in_menuslot = find_menu_handle(handle)
 IF found_in_menuslot = -1 THEN
  scripterr current_command_name() + ": Invalid menu handle " & handle
  RETURN NO
 ELSE
  RETURN YES
 END IF
END FUNCTION

'Returns mislot and sets found_in_menuslot, otherwise returns -1 and sets found_in_menuslot=-1
FUNCTION find_menu_item_handle (handle as integer, byref found_in_menuslot as integer) as integer
 FOR menuslot as integer = 0 TO topmenu
  WITH menus(menuslot)
   FOR mislot as integer = 0 TO .numitems - 1
    IF .items[mislot]->handle = handle THEN
     found_in_menuslot = menuslot
     RETURN mislot
    END IF
   NEXT mislot
  END WITH
 NEXT menuslot
 found_in_menuslot = -1
 RETURN -1 ' Not found
END FUNCTION

' If handle is valid, return true and set menuslot and mislot, otherwise show an error and return false
' and set to -1,-1.
FUNCTION valid_menu_item_handle (handle as integer, byref found_in_menuslot as integer, byref found_in_mislot as integer = 0) as bool
 found_in_mislot = find_menu_item_handle(handle, found_in_menuslot)
 IF found_in_mislot = -1 THEN
  scripterr current_command_name() + ": invalid menu item handle " & handle
  RETURN NO
 ELSE
  RETURN YES
 END IF
END FUNCTION

' If handle is valid, return true and fill in the ptr to the MenuDefItem
FUNCTION valid_menu_item_handle_ptr (handle as integer, byref mi as MenuDefItem ptr, byref found_in_menuslot as integer = 0, byref found_in_mislot as integer = 0) as bool
 IF valid_menu_item_handle(handle, found_in_menuslot, found_in_mislot) THEN
  mi = menus(found_in_menuslot).items[found_in_mislot]
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

FUNCTION assign_menu_item_handle (byref mi as MenuDefItem) as integer
 STATIC new_handle as integer = 0
 new_handle = new_handle + 1
 mi.handle = new_handle
 RETURN new_handle
END FUNCTION

FUNCTION assign_menu_handles (byref menu as MenuDef) as integer
 STATIC new_handle as integer = 0
 new_handle = new_handle + 1
 menus(topmenu).handle = new_handle
 FOR i as integer = 0 TO menu.numitems - 1
  assign_menu_item_handle *menu.items[i]
 NEXT i
 RETURN new_handle
END FUNCTION

FUNCTION menu_item_handle_by_slot(byval menuslot as integer, byval mislot as integer, byval visible_only as bool=YES) as integer
 IF menuslot >= 0 AND menuslot <= topmenu THEN
  WITH menus(menuslot)
   IF mislot >= 0 AND mislot < .numitems THEN
    WITH *.items[mislot]
     IF visible_only ANDALSO NOT .visible THEN RETURN 0
     RETURN .handle
    END WITH
   END IF
  END WITH
 END IF
 RETURN 0
END FUNCTION

FUNCTION find_menu_item_slot_by_string(byval menuslot as integer, s as string, byval mislot as integer=0, byval visible_only as bool=YES) as integer
 DIM i as integer
 DIM cap as STRING
 WITH menus(menuslot)
  FOR i = mislot TO .numitems - 1
   WITH *.items[i]
    IF visible_only AND NOT .visible THEN CONTINUE FOR
    cap = get_menu_item_caption(*menus(menuslot).items[i], menus(menuslot))
    IF cap = s THEN
     RETURN i
    END IF
   END WITH
  NEXT i
 END WITH
 RETURN -1 ' not found
END FUNCTION


'==========================================================================================
'                        Other script command arg checking/decoding
'==========================================================================================

'This doesn't check how many joysticks are plugged in, because it's not an error
'to poll a missing joystick
FUNCTION valid_joystick(byval joynum as integer) as bool
  RETURN bound_arg(joynum, 0, 15, "joystick", , serrBadOp)
END FUNCTION

FUNCTION valid_item_slot(byval item_slot as integer) as bool
 RETURN bound_arg(item_slot, 0, last_inv_slot(), "item slot")
END FUNCTION

FUNCTION valid_item(byval itemID as integer) as bool
 RETURN bound_arg(itemID, 0, gen(genMaxItem), "item ID")
END FUNCTION

'TODO: Only use this where a command should be able to act on empty caterpillar hero slots
FUNCTION valid_hero_caterpillar_rank(who as integer) as bool
 RETURN bound_arg(who, 0, 3, "hero caterpillar party rank")
END FUNCTION

'TODO: Only use this where a command should be able to act on empty hero slots
'(for compatibility, that's most of them!)
FUNCTION valid_hero_party(byval who as integer, byval minimum as integer=0) as bool
 RETURN bound_arg(who, minimum, 40, "hero party slot")
END FUNCTION

FUNCTION really_valid_hero_party(byval who as integer, byval maxslot as integer=40, byval errlvl as scriptErrEnum = serrBadOp) as bool
 'Defaults to a non-suppressed error
 IF bound_arg(who, 0, maxslot, "hero party slot", , errlvl) = NO THEN RETURN NO
 IF gam.hero(who).id = -1 THEN
  scripterr current_command_name() + ": Party hero slot " & who & " is empty", errlvl
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_stat(byval statid as integer) as bool
 RETURN bound_arg(statid, 0, statLast, "stat ID", , serrBadOp)
END FUNCTION

FUNCTION valid_plotstr(byval n as integer, byval errlvl as scriptErrEnum = serrBound) as bool
 RETURN bound_arg(n, 0, UBOUND(plotstr), "string ID", , errlvl)
END FUNCTION

FUNCTION valid_formation(byval form as integer) as bool
 RETURN bound_arg(form, 0, gen(genMaxFormation), "formation ID")
END FUNCTION

FUNCTION valid_formation_slot(byval form as integer, byval slot as integer) as bool
 IF bound_arg(form, 0, gen(genMaxFormation), "formation ID") THEN
  RETURN bound_arg(slot, 0, 7, "formation slot")
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_zone(byval id as integer) as bool
 RETURN bound_arg(id, 1, zoneLASTREADABLE, "zone ID", , serrBadOp)
END FUNCTION

FUNCTION valid_door(byval id as integer) as bool
 IF bound_arg(id, 0, UBOUND(gam.map.door), "door", , serrBadOp) = NO THEN RETURN NO
 IF gam.map.door(id).exists = NO THEN
  scripterr current_command_name() & ": invalid door id " & id, serrBadOp
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_door(thisdoor as Door, byval id as integer=-1) as bool
 IF thisdoor.exists = NO THEN
  DIM errtext as string = current_command_name() & ": invalid (non-existent) door object"
  IF id >= 0 THEN errtext &= " id " & id
  scripterr errtext, serrBadOp
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_tile_pos(byval x as integer, byval y as integer) as bool
 IF x < 0 OR y < 0 OR x >= mapsizetiles.x OR y >= mapsizetiles.y THEN
  scripterr current_command_name() + ": invalid map position " & XY(x,y) & " -- map is " & mapsizetiles.wh & " tiles", serrBadOp
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_map(map_id as integer) as bool
 RETURN bound_arg(map_id, 0, gen(genMaxMap), "map number", , serrBadOp)
END FUNCTION

FUNCTION valid_map_layer(layer as integer, errorlevel as scriptErrEnum = serrBadOp) as bool
 IF layer < 0 OR layer > UBOUND(maptiles) THEN
  scripterr current_command_name() + ": invalid map layer " & layer & " -- last map layer is " & UBOUND(maptiles), errorlevel
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_save_slot(slot as integer) as bool
 RETURN bound_arg(slot, 1, maxSaveSlotCount, "save slot", , serrBadOp)
END FUNCTION

'Loads a Door; map_id -1 means "current map".
'Returns true if thisdoor could be loaded EVEN IF the door doesn't exist (marked unused in door.bits())!
'Use valid_door() instead or afterwards to check the door exists.
FUNCTION get_door_on_map(byref thisdoor as Door, byval door_id as integer, byval map_id as integer) as bool
 IF map_id = -1 OR map_id = gam.map.id THEN
  'default to current map
  IF door_id < 0 OR door_id > UBOUND(gam.map.door) THEN RETURN NO
  thisdoor = gam.map.door(door_id)
  RETURN YES
 END IF
 IF valid_map(map_id) THEN
  IF read_one_door(thisdoor, map_id, door_id) THEN
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_color(index as integer) as bool
 RETURN bound_arg(index, -1 * uiColorLast - 1, 255, "color index (0-255) or UI color constant", , serrBadOp)
END FUNCTION


'==========================================================================================
'                             Utility functions for default arguments 
'==========================================================================================

'This function is for when a new argument has been added that will exist in newer compiled
' scripts, but might be missing in old compiled scripts
FUNCTION get_optional_arg(byval retval_index as integer, byval default as integer) as integer
 IF curcmd->argc < retval_index + 1 THEN
  RETURN default
 END IF
 RETURN retvals(retval_index)
END FUNCTION

'==========================================================================================
'                             Misc command implementations
'==========================================================================================


SUB tweakpalette (byval r as integer, byval g as integer, byval b as integer, byval first as integer = 0, byval last as integer = 255)
 FOR i as integer = first TO last
  master(i).r = bound(master(i).r + r * 4, 0, 255)
  master(i).g = bound(master(i).g + g * 4, 0, 255)
  master(i).b = bound(master(i).b + b * 4, 0, 255)
 NEXT i
END SUB

'"greyscale palette" command
SUB greyscalepal ()
 FOR i as integer = bound(retvals(0), 0, 255) TO bound(retvals(1), 0, 255)
  WITH master(i)
   .r = bound(CINT(.r * 0.3 + .g * 0.59 + .b * 0.11), 0, 255)
   .g = .r
   .b = .r
   END WITH
 NEXT i
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

' Implementation of "check game exists"
LOCAL FUNCTION check_game_exists () as integer
 IF valid_plotstr(retvals(0), serrBadOp) = NO THEN RETURN 0
 ' Parse the path
 DIM path as string = plotstr(retvals(0)).s
 ' find_file_portably returns either an error message or a path
 path = find_file_portably(path)
 
 IF is_rpg(path) ORELSE is_rpgdir(path) THEN
  RETURN 1
 END IF
END FUNCTION

' Implementation of "run game".
LOCAL SUB run_game ()
 ' Not being able to load the game should always show an error (use serrMajor for everything)
 IF valid_plotstr(retvals(0), serrMajor) = NO THEN RETURN

 IF running_as_slave THEN
  ' This would require more work to implement
  scripterr "Sorry, you can't use " + current_command_name() + " while Testing Game"
  RETURN
 END IF

 ' Parse the path
 DIM path as string = plotstr(retvals(0)).s
 ' find_file_portably returns either an error message or a path
 path = find_file_portably(path)
 IF isfile(path) = NO ANDALSO isdir(path) = NO THEN
  scripterr interpreter_context_name() + path, serrMajor
  RETURN
 END IF
 IF select_rpg_or_rpgdir(path) = NO THEN
  scripterr interpreter_context_name() + "Not a valid game: " + path, serrMajor
  RETURN
 END IF

 gam.want.rungame = path
 ' TODO: when switching to fibres, should call exit_interpreter() or something like that instead
 script_start_waiting()
END SUB

'Although gmap commands are pretty undocumented anyway, restrict their use to avoid backcompat worries
FUNCTION allow_gmap_idx(gmap_idx as integer) as bool
 SELECT CASE gmap_idx
  CASE 0 TO 19:   RETURN YES
  CASE 32 TO 33:  RETURN YES
  CASE 378:       RETURN YES
 END SELECT
 RETURN NO
END FUNCTION
