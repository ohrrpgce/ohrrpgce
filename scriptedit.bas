'OHRRPGCE CUSTOM - Script manager (importing and browsers)
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"
#include "const.bi"
#include "common.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "cglobals.bi"
#include "ver.txt"
#include "scrconst.bi"

'--Types

TYPE TriggerSet
 size as integer
 trigs as TriggerData ptr
 usedbits as unsigned integer ptr
END TYPE

DIM SHARED script_import_defaultdir as string

'--Local subs and functions
DECLARE FUNCTION compilescripts (fname as string, hsifile as string) as string
DECLARE SUB importscripts (f as string, quickimport as bool)
DECLARE FUNCTION isunique (s as string, set() as string) as bool
DECLARE FUNCTION exportnames () as string
DECLARE SUB addtrigger (scrname as string, byval id as integer, byref triggers as TriggerSet)


'Prints a hamsterspeak constant to already-open filehandle
SUB writeconstant (byval filehandle as integer, byval num as integer, names as string, unique() as string, prefix as string)
 DIM s as string
 DIM n as integer = 2
 DIM suffix as string
 s = TRIM(sanitize_script_identifier(names))
 IF s <> "" THEN
  WHILE NOT isunique(s + suffix, unique())
   suffix = " " & n
   n += 1
  WEND
  s = num & "," & prefix & ":" & s & suffix
  PRINT #filehandle, s
 END IF
END SUB

'Returns name of .hsi file
FUNCTION exportnames () as string
 REDIM u(0) as string
 DIM her as HeroDef
 DIM menu_set as MenuSet
 menu_set.menufile = workingdir & SLASH & "menus.bin"
 menu_set.itemfile = workingdir & SLASH & "menuitem.bin"
 DIM elementnames() as string
 getelementnames elementnames()

 DIM outf as string = trimextension(trimpath(sourcerpg)) + ".hsi"

 clearpage 0
 setvispage 0
 textcolor uilook(uiText), 0
 DIM pl as integer = 0
 printstr "exporting HamsterSpeak Definitions to:", 0, pl * 8, 0: pl = pl + 1
 printstr RIGHT(outf, 40), 0, pl * 8, 0: pl = pl + 1
 setvispage 0, NO

 DIM fh as integer = FREEFILE
 OPENFILE(outf, FOR_OUTPUT, fh)
 PRINT #fh, "# HamsterSpeak constant definitions for " & trimpath(sourcerpg)
 PRINT #fh, ""
 PRINT #fh, "define constant, begin"

 printstr "tag names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 2 TO gen(genMaxTagname)
  writeconstant fh, i, load_tag_name(i), u(), "tag"
 NEXT i

 printstr "song names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxSong)
  writeconstant fh, i, getsongname(i), u(), "song"
 NEXT i

 printstr "sound effect names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxSFX)
  writeconstant fh, i, getsfxname(i), u(), "sfx"
 NEXT i
 setvispage 0

 printstr "hero names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxHero)
  loadherodata her, i
  writeconstant fh, i, her.name, u(), "hero"
 NEXT i

 printstr "item names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxItem)
  writeconstant fh, i, readitemname(i), u(), "item"
 NEXT i
 setvispage 0

 printstr "stat names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO UBOUND(statnames)
  writeconstant fh, i, statnames(i), u(), "stat"
 NEXT i

 printstr "element names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genNumElements) - 1
  writeconstant fh, i, elementnames(i), u(), "element"
 NEXT i

 printstr "slot names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 IF LCASE(readglobalstring(38, "Weapon")) <> "weapon" THEN
  writeconstant fh, 1, "Weapon", u(), "slot"
 END IF
 writeconstant fh, 1, readglobalstring(38, "Weapon"), u(), "slot"
 FOR i as integer = 0 TO 3
  writeconstant fh, i + 2, readglobalstring(25 + i, "Armor" & i+1), u(), "slot"
 NEXT i
 setvispage 0

 printstr "map names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxMap)
  writeconstant fh, i, getmapname(i), u(), "map"
 NEXT i

 printstr "attack names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxAttack)
  writeconstant fh, i + 1, readattackname(i), u(), "atk"
 NEXT i
 setvispage 0

 printstr "shop names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxShop)
  writeconstant fh, i, readshopname(i), u(), "shop"
 NEXT i
 setvispage 0

 printstr "menu names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxMenu)
  writeconstant fh, i, getmenuname(i), u(), "menu"
 NEXT i
 setvispage 0

 printstr "enemy names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 FOR i as integer = 0 TO gen(genMaxEnemy)
  writeconstant fh, i, readenemyname(i), u(), "enemy"
 NEXT i
 setvispage 0

 printstr "slice lookup names", 0, pl * 8, 0: pl = pl + 1
 REDIM u(0) as string
 REDIM slicelookup(0) as string
 load_string_list slicelookup(), workingdir & SLASH & "slicelookup.txt"
 FOR i as integer = 1 TO UBOUND(slicelookup)
  writeconstant fh, i, slicelookup(i), u(), "sli"
 NEXT i
 setvispage 0

 PRINT #fh, "end"
 CLOSE #fh

 printstr "done", 0, pl * 8, 0: pl = pl + 1
 setvispage 0, NO

 RETURN outf
END FUNCTION

SUB addtrigger (scrname as string, byval id as integer, triggers as TriggerSet)
 WITH triggers
  FOR i as integer = 0 TO .size - 1
   IF .trigs[i].name = scrname THEN
    .trigs[i].id = id
    .usedbits[i \ 32] = BITSET(.usedbits[i \ 32], i MOD 32)
    EXIT SUB
   END IF
  NEXT

  'add to the end
  .trigs[.size].name = scrname
  .trigs[.size].id = id
  .usedbits[.size \ 32] = BITSET(.usedbits[.size \ 32], .size MOD 32)

  'expand
  .size += 1
  IF .size MOD 32 = 0 THEN
   DIM allocnum as integer = .size + 32
   .usedbits = REALLOCATE(.usedbits, allocnum \ 8)  'bits/byte
   .trigs = REALLOCATE(.trigs, allocnum * SIZEOF(TriggerData))

   IF .usedbits = 0 OR .trigs = 0 THEN showerror "Could not allocate memory for script importation": EXIT SUB

   FOR i as integer = .size TO allocnum - 1
    DIM dummy as TriggerData ptr = NEW (@.trigs[i]) TriggerData  'placement new, initialise those strings
   NEXT
   .usedbits[.size \ 32] = 0
  END IF
 END WITH
END SUB

' If quickimport is true, doesn't display the names of imported scripts
SUB compile_andor_import_scripts (f as string, quickimport as bool = NO)
 DIM extn as string = LCASE(justextension(f))
 IF extn <> "hs" AND extn <> "hsp" THEN
  DIM hsifile as string = exportnames
  f = compilescripts(f, hsifile)
  IF f <> "" THEN
   importscripts f, quickimport
   safekill f  'reduce clutter
  END IF
 ELSE
  importscripts f, quickimport
 END IF
END SUB

SUB importscripts (f as string, quickimport as bool)
 DIM triggers as TriggerSet
 DIM triggercount as integer
 DIM temp as short
 DIM fptr as integer
 DIM dotbin as integer
 DIM headersize as integer
 DIM recordsize as integer

 'Under the best conditions this check is redundant, but it is still good to check anyway...
 IF NOT isfile(f) THEN
  pop_warning f & " does not exist."
  EXIT SUB
 END IF

 DIM headerbuf(1) as integer
 loadrecord headerbuf(), f, 2
 IF headerbuf(0) = 21320 AND headerbuf(1) = 0 THEN  'Check first 4 bytes are "HS\0\0"
  unlumpfile(f, "hs", tmpdir)
  DIM header as HSHeader
  load_hsp_header tmpdir & "hs", header
  IF header.valid = NO THEN
   pop_warning f & " appears to be corrupt."
   EXIT SUB
  END IF
  IF header.hsp_format > CURRENT_HSP_VERSION THEN
   debug f & " hsp_format=" & header.hsp_format & " from future, hspeak version " & header.hspeak_version
   pop_warning "This compiled .hs script file is in a format not understood by this version of Custom. Please ensure Custom and HSpeak are from the same release of the OHRRPGCE."
   EXIT SUB
  END IF

  writeablecopyfile f, game + ".hsp"
  textcolor uilook(uiMenuItem), 0
  unlumpfile(game + ".hsp", "scripts.bin", tmpdir)
  IF isfile(tmpdir & "scripts.bin") THEN
   dotbin = -1
   fptr = FREEFILE
   OPENFILE(tmpdir + "scripts.bin", FOR_BINARY, fptr)
   'load header
   GET #fptr, , temp
   headersize = temp
   GET #fptr, , temp
   recordsize = temp
   SEEK #fptr, headersize + 1
   
   'the scripts.bin lump does not have a format version field in its header, instead use header size
   IF headersize <> 4 THEN
    pop_warning f + " is in an unrecognised format. Please upgrade to the latest version of CUSTOM."
    EXIT SUB
   END IF
  ELSE
   dotbin = 0
   unlumpfile(game + ".hsp", "scripts.txt", tmpdir)

   IF isfile(tmpdir + "scripts.txt") = 0 THEN
    pop_warning f + " appears to be corrupt. Please try to recompile your scripts."
    EXIT SUB
   END IF

   fptr = FREEFILE
   OPENFILE(tmpdir + "scripts.txt", FOR_INPUT, fptr)
  END IF

  'load in existing trigger table
  WITH triggers
    DIM fh as integer = 0
    .size = 0
    DIM fname as string = workingdir & SLASH & "lookup1.bin"
    IF isfile(fname) THEN
     fh = FREEFILE
     OPENFILE(fname, FOR_BINARY, fh)
     .size = LOF(fh) \ 40
    END IF

    'number of triggers rounded to next multiple of 32 (as triggers get added, allocate space for 32 at a time)
    DIM allocnum as integer = (.size \ 32) * 32 + 32
    .trigs = CALLOCATE(allocnum, SIZEOF(TriggerData))
    .usedbits = CALLOCATE(allocnum \ 8)

    IF .usedbits = 0 OR .trigs = 0 THEN showerror "Could not allocate memory for script importation": EXIT SUB
   
    IF fh THEN
     FOR j as integer = 0 TO .size - 1
      loadrecord buffer(), fh, 20, j
      .trigs[j].id = buffer(0)
      .trigs[j].name = readbinstring(buffer(), 1, 36)
     NEXT
     CLOSE fh
    END IF
  END WITH

  '--save a temporary backup copy of plotscr.lst
  IF isfile(workingdir & SLASH & "plotscr.lst") THEN
   copyfile workingdir & SLASH & "plotscr.lst", tmpdir & "plotscr.lst.tmp"
  END IF

  reset_console

  gen(genNumPlotscripts) = 0
  gen(genMaxRegularScript) = 0
  DIM viscount as integer = 0
  DIM names as string = ""
  DIM num as string
  DIM argc as string
  DIM dummy as string
  DIM id as integer
  DIM trigger as integer
  DIM plotscr_lsth as integer = FREEFILE
  IF OPENFILE(workingdir + SLASH + "plotscr.lst", FOR_BINARY, plotscr_lsth) THEN
   visible_debug "Could not open " + workingdir + SLASH + "plotscr.lst"
   CLOSE fptr
   EXIT SUB
  END IF
  DO
   IF EOF(fptr) THEN EXIT DO
   IF dotbin THEN 
    'read from scripts.bin
    loadrecord buffer(), fptr, recordsize \ 2
    id = buffer(0)
    trigger = buffer(1)
    names = readbinstring(buffer(), 2, 36)
   ELSE
    'read from scripts.txt
    LINE INPUT #fptr, names
    LINE INPUT #fptr, num
    LINE INPUT #fptr, argc
    FOR i as integer = 1 TO str2int(argc)
     LINE INPUT #fptr, dummy
    NEXT i
    id = str2int(num)
    trigger = 0
    names = LEFT(names, 36)
   END IF

   'save to plotscr.lst
   buffer(0) = id
   writebinstring names, buffer(), 1, 36
   storerecord buffer(), plotscr_lsth, 20, gen(genNumPlotscripts)
   gen(genNumPlotscripts) = gen(genNumPlotscripts) + 1
   IF buffer(0) > gen(genMaxRegularScript) AND buffer(0) < 16384 THEN gen(genMaxRegularScript) = buffer(0)

   'process trigger
   IF trigger > 0 THEN
    addtrigger names, id, triggers
    triggercount += 1
   END IF

   'display progress
   IF id < 16384 OR trigger > 0 THEN
    viscount = viscount + 1
    IF quickimport = NO THEN append_message names & ", "
   END IF
  LOOP
  CLOSE plotscr_lsth

  'output the updated trigger table
  WITH triggers
    FOR j as integer = 0 TO .size - 1
     IF BIT(.usedbits[j \ 32], j MOD 32) = 0 THEN .trigs[j].id = 0
     buffer(0) = .trigs[j].id
     writebinstring .trigs[j].name, buffer(), 1, 36
     storerecord buffer(), workingdir + SLASH + "lookup1.bin", 20, j
     .trigs[j].DESTRUCTOR()
    NEXT

    DEALLOCATE(.trigs)
    DEALLOCATE(.usedbits)
  END WITH

  CLOSE #fptr
  IF dotbin THEN safekill tmpdir & "scripts.bin" ELSE safekill tmpdir & "scripts.txt"

  '--reload lookup1.bin and plotscr.lst
  load_script_triggers_and_names

  '--fix the references to any old-style plotscripts that have been converted to new-style scripts.
  append_message " ...autofix broken triggers..."
  autofix_broken_old_scripts

  '--erase the temporary backup copy of plotscr.lst
  safekill tmpdir & "plotscr.lst.tmp"
  
  textcolor uilook(uiText), 0
  show_message "imported " & viscount & " scripts"
  IF quickimport = NO THEN waitforanykey
 ELSE
  pop_warning f + " is not really a compiled .hs file. Did you create it by compiling a" _
              " script file with hspeak.exe, or did you just give your script a name that" _
              " ends in .hs and hoped it would work? Use hspeak.exe to create real .hs files"
 END IF

 'Cause the cache in scriptname() (and also in commandname()) to be dropped
 game_unique_id = STR(randint(INT_MAX))
END SUB

FUNCTION isunique (s as string, set() as string) as bool
 DIM key as string
 key = sanitize_script_identifier(LCASE(s), NO)

 FOR i as integer = 1 TO UBOUND(set)
  IF key = set(i) THEN RETURN NO
 NEXT i

 str_array_append set(), key
 RETURN YES
END FUNCTION

SUB reimport_previous_scripts ()
 DIM fname as string
 'isfile currently broken, returns true for directories
 IF script_import_defaultdir = "" ORELSE isfile(script_import_defaultdir) = NO ORELSE isdir(script_import_defaultdir) THEN
  fname = browse(9, script_import_defaultdir, "", "browse_hs")
 ELSE
  fname = script_import_defaultdir
 END IF
 IF fname <> "" THEN
  compile_andor_import_scripts fname, YES
 END IF
 clearkey scEnter
 clearkey scSpace
END SUB

SUB scriptman ()
 DIM menu(4) as string
 DIM menu_display(4) as string

 menu(0) = "Previous Menu"
 menu(1) = "Compile and/or Import scripts (.hss/.hs)"
 menu(2) = "Export names for scripts (.hsi)"
 menu(3) = "Check where scripts are used..."
 menu(4) = "Find broken script triggers..."

 DIM selectst as SelectTypeState
 DIM state as MenuState
 DIM f as string
 state.pt = 1
 state.size = 24
 state.last = UBOUND(menu)

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "script_management"
  IF keyval(scF5) > 1 THEN
   reimport_previous_scripts
  END IF
  usemenu state
  IF select_by_typing(selectst) THEN
   select_on_word_boundary menu(), selectst, state
  END IF
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0
     EXIT DO
    CASE 1
     DIM fname as string
     fname = browse(9, script_import_defaultdir, "", "browse_hs")
     IF fname <> "" THEN
      'clearkey scEnter
      'clearkey scSpace
      compile_andor_import_scripts fname
     END IF
    CASE 2
     DIM dummy as string = exportnames()
     waitforanykey
    CASE 3
     script_usage_list()
    CASE 4
     script_broken_trigger_list()
   END SELECT
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
  edgeprint "Press F9 to Compile & Import your", 20, 160, uilook(uiText), dpage
  edgeprint "scripts anywhere from any menu.", 20, 170, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION get_hspeak_version(hspeak_path as string) as string
 'Note this will momentarily pop up a console window on Windows, unpleasant.
 'Could get around this by using open_piped_process

 DIM blurb as string, stderr_s as string
 IF run_and_get_output(escape_filename(hspeak_path) & " -k", blurb, stderr_s) <> 0 THEN
  visible_debug !"Error occurred when running hspeak:\n" & stderr_s
  RETURN ""
 END IF

 DIM hsversion as string = MID(blurb, INSTR(blurb, " v") + 2, 3)
 IF LEN(hsversion) <> 3 ORELSE isdigit(hsversion[0]) = NO THEN
  debug !"Couldn't get HSpeak version from blurb:\n'" & blurb & "'"
  RETURN ""
 END IF
 RETURN hsversion
END FUNCTION

'Returns filename of .hs file, or "" on failure
FUNCTION compilescripts(fname as string, hsifile as string) as string
 DIM as string outfile, hspeak, errmsg, hspeak_ver, args
 hspeak = find_helper_app("hspeak")
 IF hspeak = "" THEN
  visible_debug missing_helper_message("hspeak")
  RETURN ""
 END IF
 args = "-y"

 hspeak_ver = get_hspeak_version(hspeak)
 debuginfo "hspeak version '" & hspeak_ver & "'"
 IF hspeak_ver = "" THEN
  'If get_hspeak_version failed (returning ""), then spawn_and_wait usually will too.
  'However if hspeak isn't compiled as a console program then we can run it but not get its output.
  notification "Your copy of HSpeak is faulty or not supported. You should download a copy of HSpeak from http://rpg.hamsterrepublic.com/ohrrpgce/Downloads"
  RETURN ""
 ELSEIF strcmp(STRPTR(hspeak_ver), @RECOMMENDED_HSPEAK_VERSION) < 0 THEN
  IF version_branch = "wip" THEN
   notification "Your copy of HSpeak is out of date. You should download a nightly build of HSpeak from http://rpg.hamsterrepublic.com/ohrrpgce/Downloads"
  ELSE
   notification "Your copy of HSpeak is out of date. You should use the version of HSpeak that was provided with the OHRRPGCE."
  END IF
 END IF

 IF slave_channel <> NULL_CHANNEL THEN
  IF isfile(game & ".hsp") THEN
   'Try to reuse script IDs from existing scripts if any, so that currently running scripts
   'don't start calling the wrong scripts due to ID remapping
   IF strcmp(STRPTR(hspeak_ver), STRPTR("3Pa")) >= 0 THEN
    unlumpfile game & ".hsp", "scripts.bin", tmpdir
    'scripts.bin will be missing in scripts compiled with very old HSpeak versions
    args += " --reuse-ids " & escape_filename(tmpdir & "scripts.bin")
   END IF
  END IF
 END IF

 IF LEN(hsifile) > 0 AND strcmp(STRPTR(hspeak_ver), STRPTR("3S ")) >= 0 THEN
  args += " --include " & escape_filename(hsifile)
 END IF

 outfile = trimextension(fname) + ".hs"
 safekill outfile
 'Wait for keys: we spawn a command prompt/xterm/Terminal.app, which will be closed when HSpeak exits
 errmsg = spawn_and_wait(hspeak, args & " " & escape_filename(simplify_path_further(fname, curdir)))
 IF LEN(errmsg) THEN
  visible_debug errmsg + !"\n\nNo scripts were imported."
  RETURN ""
 END IF
 IF isfile(outfile) = NO THEN
  notification !"Compiling failed.\n\nNo scripts were imported."
  RETURN ""
 END IF
 RETURN outfile
END FUNCTION
