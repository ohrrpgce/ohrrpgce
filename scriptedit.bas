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
#include "custom.bi"

'--Types

TYPE TriggerSet
 size as integer
 trigs as TriggerData ptr
 usedbits as unsigned integer ptr
END TYPE

DIM inside_importscripts as bool

DIM SHARED script_import_default as string   'File, directory, or "" if not initialised

'--Local subs and functions
DECLARE FUNCTION compilescripts (fname as string, hsifile as string, quickimport as bool = NO) as string
DECLARE FUNCTION importscripts (hsfile as string, srcfile as string, quickimport as bool = NO) as bool
DECLARE SUB browse_andor_import_scripts (fname as string = "", quickimport as bool = NO)
DECLARE FUNCTION isunique (s as string, set() as string) as bool
DECLARE FUNCTION exportnames (outdir as string = "") as string
DECLARE SUB export_scripts()
DECLARE SUB addtrigger (scrname as string, byval id as integer, byref triggers as TriggerSet)
DECLARE SUB decompile_scripts()
DECLARE SUB seekscript (byref temp as integer, byval seekdir as integer, byval triggertype as integer)

DECLARE SUB script_list_export (menu() as string, description as string, remove_first_items as integer)
DECLARE SUB visit_scripts(byval visit as FnScriptVisitor)
DECLARE SUB gather_script_usage(list() as string, byval id as integer, byval trigger as integer=0, byref meter as integer, byval meter_times as integer=1, box_instead_cache() as integer, box_after_cache() as integer, box_preview_cache() as string)
DECLARE SUB script_usage_list ()
DECLARE SUB script_broken_trigger_list()
DECLARE SUB autofix_broken_old_scripts()

'==========================================================================================
'                                       Export HSI
'==========================================================================================


FUNCTION isunique (s as string, set() as string) as bool
 DIM key as string
 key = sanitize_script_identifier(LCASE(s), NO)

 FOR i as integer = 1 TO UBOUND(set)
  IF key = set(i) THEN RETURN NO
 NEXT i

 a_append set(), key
 RETURN YES
END FUNCTION

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
'outdir is where to put it (defaults to CURDIR, ie next to the .rpg)
FUNCTION exportnames (outdir as string = "") as string
 REDIM u(0) as string
 DIM her as HeroDef
 DIM elementnames() as string
 getelementnames elementnames()

 DIM outf as string = join_path(outdir, trimextension(trimpath(sourcerpg)) + ".hsi")
 outf = simplify_path_further(outf)

 clearpage 0
 setvispage 0
 textcolor uilook(uiText), 0
 DIM pl as integer = 0
 printstr "Exporting HamsterSpeak Definitions to:", 0, pl * 8, 0: pl = pl + 1
 printstr RIGHT(outf, 40), 0, pl * 8, 0: pl = pl + 1
 setvispage 0, NO

 DIM fh as integer
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
 IF sanitize_script_identifier(LCASE(readglobalstring(38, "Weapon"))) <> "weapon" THEN
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


'==========================================================================================
'                                   Import scripts
'==========================================================================================


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
    .trigs[i].Constructor()  'Zero everything out
   NEXT
   .usedbits[.size \ 32] = 0
  END IF
 END WITH
END SUB

' Returns true on success
' If quickimport is true, doesn't display the names of imported scripts
FUNCTION compile_andor_import_scripts (filename as string, quickimport as bool = NO) as bool
 'We don't have to check whether this is non-zero, as only possible to re-enter via the global F9 menu.
 'Also, this is the single point of entry to compilescripts and importscripts (they're only called
 'from this function)
 inside_importscripts = YES
 DIM ret as bool = NO
 DIM extn as string = LCASE(justextension(filename))
 IF extn <> "hs" AND extn <> "hsp" THEN
  'Put the .hsi next to the .hss
  DIM hssdir as string = trimfilename(filename)
  DIM hsifile as string = exportnames(hssdir)
  DIM hsfile as string = compilescripts(filename, hsifile, quickimport)
  IF hsfile <> "" THEN  'success
   ret = importscripts(hsfile, filename, quickimport)
   safekill hsfile  'reduce clutter
  END IF
 ELSE
  ret = importscripts(filename, filename, quickimport)
 END IF
 inside_importscripts = NO
 RETURN ret
END FUNCTION

' Imports scripts from an .hs file.
' Returns true on success
' srcfile is used as the filename in messages to the user.
' If quickimport is true, doesn't display the names of imported scripts
FUNCTION importscripts (hsfile as string, srcfile as string = "", quickimport as bool = NO) as bool
 DIM triggers as TriggerSet
 DIM triggercount as integer
 DIM fptr as integer
 DIM dotbin as bool
 DIM scripts_bin_recordsize as integer

 'Under the best conditions this check is redundant, but it is still good to check anyway...
 IF NOT isfile(hsfile) THEN
  pop_warning hsfile & " does not exist."
  RETURN NO
 END IF

 DIM headerbuf(1) as integer
 loadrecord headerbuf(), hsfile, 2
 IF headerbuf(0) = 21320 AND headerbuf(1) = 0 THEN  'Check first 4 bytes are "HS\0\0"
  unlumpfile(hsfile, "hs", tmpdir)
  DIM header as HSHeader
  load_hsp_header tmpdir & "hs", header
  IF header.valid = NO THEN
   pop_warning hsfile & " appears to be corrupt."
   RETURN NO
  END IF
  IF header.hsp_format > CURRENT_HSP_VERSION THEN
   debug hsfile & " hsp_format=" & header.hsp_format & " from future, hspeak version " & header.hspeak_version
   pop_warning "This compiled .hs script file is in a format not understood by this version of Custom. Please ensure Custom and HSpeak are from the same release of the OHRRPGCE."
   RETURN NO
  END IF

  'Open either scripts.bin or (if compiled by ancient hspeak) scripts.txt to read list of scripts
  safekill tmpdir & "scripts.bin"
  unlumpfile(hsfile, "scripts.bin", tmpdir)
  IF OPENFILE(tmpdir + "scripts.bin", FOR_BINARY + ACCESS_READ, fptr) = fberrOK THEN
   dotbin = YES
   'load header
   DIM headersize as integer
   DIM temp as short
   GET #fptr, , temp
   headersize = temp
   GET #fptr, , temp
   scripts_bin_recordsize = temp
   SEEK #fptr, headersize + 1
   
   'the scripts.bin lump does not have a format version field in its header, instead use header size
   IF headersize <> 4 THEN
    visible_debug hsfile + " is in an unrecognised format. Please upgrade to the latest version of CUSTOM."
    RETURN NO
   END IF
  ELSE
   dotbin = NO
   safekill tmpdir + "scripts.txt"
   unlumpfile(hsfile, "scripts.txt", tmpdir)
   IF OPENFILE(tmpdir + "scripts.txt", FOR_INPUT, fptr) <> fberrOK THEN
    visible_debug hsfile + " appears to be corrupt. Please try to recompile your scripts."
    RETURN NO
   END IF
  END IF

  'load in existing trigger table
  WITH triggers
    DIM fh as integer = 0
    .size = 0
    DIM fname as string = workingdir & SLASH & "lookup1.bin"
    IF OPENFILE(fname, FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
     .size = LOF(fh) \ 40
    END IF

    'number of triggers rounded to next multiple of 32 (as triggers get added, allocate space for 32 at a time)
    DIM allocnum as integer = (.size \ 32) * 32 + 32
    .trigs = CALLOCATE(allocnum, SIZEOF(TriggerData))
    .usedbits = CALLOCATE(allocnum \ 8)

    IF .usedbits = 0 OR .trigs = 0 THEN showerror "Could not allocate memory for script importation": RETURN NO

    IF fh THEN
     FOR j as integer = 0 TO .size - 1
      loadrecord buffer(), fh, 20, j
      .trigs[j].id = buffer(0)
      .trigs[j].name = readbinstring(buffer(), 1, 36)
     NEXT
     CLOSE fh
    END IF
  END WITH

  console_reset
  textcolor uilook(uiMenuItem), 0

  DIM numscripts as integer = 0
  DIM maxscriptid as integer = 0
  DIM viscount as integer = 0
  DIM scrname as string = ""
  DIM id as integer
  DIM trigger as integer

  'We first write to plotscr.lst.tmp, which we afterwards move to plotscr.lst, with
  'previous file moved to plotscr.lst.old.tmp.
  DIM plotscr_lsth as integer
  IF OPENFILE(workingdir + SLASH + "plotscr.lst.tmp", FOR_BINARY + ACCESS_WRITE + OR_ERROR, plotscr_lsth) THEN
   CLOSE fptr
   RETURN NO
  END IF

  'Loop through each script in either scripts.bin or scripts.txt
  console_show_message "Imported:  "
  DO
   IF EOF(fptr) THEN EXIT DO
   IF dotbin THEN
    'read from scripts.bin
    loadrecord buffer(), fptr, scripts_bin_recordsize \ 2
    id = buffer(0)
    trigger = buffer(1)
    scrname = readbinstring(buffer(), 2, 36)
   ELSE
    'read from scripts.txt
    DIM num as string
    DIM argc as string
    LINE INPUT #fptr, scrname
    LINE INPUT #fptr, num
    LINE INPUT #fptr, argc
    'Skip the default arguments (note: they're actually garbage in old hspeak versions)
    FOR i as integer = 1 TO str2int(argc)
     DIM dummy as string
     LINE INPUT #fptr, dummy
    NEXT i
    id = str2int(num)
    trigger = 0
    scrname = LEFT(scrname, 36)
   END IF

   'save to plotscr.lst.tmp
   buffer(0) = id
   writebinstring scrname, buffer(), 1, 36
   storerecord buffer(), plotscr_lsth, 20, numscripts
   numscripts += 1
   IF buffer(0) > maxscriptid AND buffer(0) < 16384 THEN maxscriptid = buffer(0)

   'process trigger
   IF trigger > 0 THEN
    addtrigger scrname, id, triggers
    triggercount += 1
   END IF

   'display progress
   IF id < 16384 OR trigger > 0 THEN
    'This is a plotscript
    viscount += 1
    IF quickimport = NO THEN console_append_message scrname & ", "
   END IF
  LOOP
  CLOSE plotscr_lsth

  'output the updated trigger table
  DIM lookupfh as integer
  IF OPENFILE(workingdir + SLASH + "lookup1.bin.tmp", FOR_BINARY + ACCESS_WRITE + OR_ERROR, lookupfh) <> fberrOK THEN
   CLOSE fptr
   RETURN NO
  END IF
  WITH triggers
    FOR j as integer = 0 TO .size - 1
     IF BIT(.usedbits[j \ 32], j MOD 32) = 0 THEN .trigs[j].id = 0
     buffer(0) = .trigs[j].id
     writebinstring .trigs[j].name, buffer(), 1, 36
     storerecord buffer(), lookupfh, 20, j
     .trigs[j].DESTRUCTOR()
    NEXT

    DEALLOCATE(.trigs)
    DEALLOCATE(.usedbits)
  END WITH
  CLOSE lookupfh
  CLOSE fptr
  IF dotbin THEN safekill tmpdir & "scripts.bin" ELSE safekill tmpdir & "scripts.txt"

  '--No error has occurred, so commit changes
  writeablecopyfile hsfile, game + ".hsp"
  '--Make a copy of the old plotscr.lst, used by autofix_broken_old_scripts
  renamefile workingdir + SLASH + "plotscr.lst", workingdir + SLASH + "plotscr.lst.old.tmp"
  renamefile workingdir + SLASH + "plotscr.lst.tmp", workingdir + SLASH + "plotscr.lst"
  renamefile workingdir + SLASH + "lookup1.bin.tmp", workingdir + SLASH + "lookup1.bin"
  gen(genNumPlotscripts) = numscripts
  gen(genMaxRegularScript) = maxscriptid

  '--reload lookup1.bin and plotscr.lst
  load_script_triggers_and_names

  '--fix the references to any old-style plotscripts that have been converted to new-style scripts.
  console_show_message ""
  console_show_message "Scanning script triggers..."
  autofix_broken_old_scripts
  safekill tmpdir & "plotscr.lst.old.tmp"

  textcolor uilook(uiText), 0
  console_show_message "Imported " & viscount & " plotscripts."
  IF option_nowait THEN
   PRINT "Imported " & viscount & " plotscripts."
  END IF

  IF quickimport THEN
   ' The console_show_messages above will be gone before the user can see them
   show_overlay_message "Imported " & viscount & " plotscripts from " & trimpath(srcfile), 3.
  ELSE
   waitforanykey
  END IF
 ELSE
  pop_warning hsfile + " is not really a compiled .hs file. Did you create it by compiling a" _
              " script file with hspeak.exe, or did you just give your script a name that" _
              " ends in .hs and hoped it would work? Use hspeak.exe to create real .hs files"
  RETURN NO
 END IF

 'Cause the cache in scriptname() (and also in commandname()) to be dropped
 '(Yuck!)
 game_unique_id = STR(randint(INT_MAX))

 RETURN YES
END FUNCTION

SUB browse_andor_import_scripts (fname as string = "", quickimport as bool = NO)
 IF fname = "" ORELSE real_isfile(fname) = NO THEN
  fname = browse(browseScripts, script_import_default, "", "browse_hs")
 END IF
 IF fname <> "" THEN
  compile_andor_import_scripts fname, quickimport
 END IF
 setkeys  'Clear keys
END SUB

SUB reimport_previous_scripts ()
 browse_andor_import_scripts script_import_default, YES
END SUB

'==========================================================================================


SUB scriptman ()
 DIM menu(5) as string
 DIM menu_display(5) as string

 menu(0) = "Previous Menu"
 menu(1) = "Compile and/or Import scripts (.hss/.hs)"
 menu(2) = "Export names for scripts (.hsi)"
 menu(3) = "Export scripts backup copy (.hss)"
 menu(4) = "Check where scripts are used..."
 menu(5) = "Find broken script triggers..."

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.pt = 1
 state.size = 24
 state.last = UBOUND(menu)

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
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
     browse_andor_import_scripts()
    CASE 2
     exportnames()
     waitforanykey
    CASE 3
     export_scripts()
    CASE 4
     script_usage_list()
    CASE 5
     script_broken_trigger_list()
   END SELECT
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
  wrapprint "Press F9 to Compile & Import your scripts anywhere from any menu.", _
            20, pBottom - 12, uilook(uiText), dpage, rWidth - 40

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB


'==========================================================================================
'                                   Compile scripts
'==========================================================================================


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
FUNCTION compilescripts(fname as string, hsifile as string, quickimport as bool = NO) as string

 clearpage vpage
 wrapprint "Compiling " & simplify_path_further(fname) & !"...\nPlease wait for HSpeak to finish, then close it.", pCentered, pCentered, uilook(uiText), vpage, rWidth - 40
 setvispage vpage, NO

 DIM as string outfile, hspeak, errmsg, hspeak_ver, args
 hspeak = find_helper_app("hspeak")
 IF hspeak = "" THEN
  visible_debug missing_helper_message("hspeak" DOTEXE)
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

 IF isfile(game & ".hsp") THEN
  'Try to reuse script IDs from existing scripts if any, so that 1) during live
  'preview, currently running scripts don't start calling the wrong scripts due
  'to ID remapping; 2) you can store script IDs in saved games and still have them
  'work later.
  IF strcmp(STRPTR(hspeak_ver), STRPTR("3Pa")) >= 0 THEN
   unlumpfile game & ".hsp", "scripts.bin", tmpdir
   IF real_isfile(tmpdir & "scripts.bin") THEN
    'scripts.bin will be missing in scripts compiled with very old HSpeak versions
    args += " --reuse-ids " & escape_filename(tmpdir & "scripts.bin")
   END IF
  END IF
 END IF

 IF LEN(hsifile) > 0 AND strcmp(STRPTR(hspeak_ver), STRPTR("3S ")) >= 0 THEN
  args += " --include " & escape_filename(hsifile)
 END IF

 IF quickimport THEN args += " -j"

 outfile = trimextension(fname) + ".hs"
 safekill outfile
 'Wait for keys: we spawn a command prompt/xterm/Terminal.app, which will be closed when HSpeak exits
 'HSpeak returns 1 on an error, 2 on a warning, so ignore exit code
 errmsg = spawn_and_wait(hspeak, args & " " & escape_filename(simplify_path_further(fname)), NO)
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


'==========================================================================================
'                                     Export scripts
'==========================================================================================


SUB export_scripts()
 DIM hsp as string = game & ".hsp"
 IF NOT isfile(hsp) THEN
  notification "Game has no imported scripts"
  EXIT SUB
 END IF
 DIM header as HSHeader
 unlumpfile(hsp, "hs", tmpdir)
 load_hsp_header tmpdir & "hs", header
 IF header.valid = NO THEN
  pop_warning hsp & " appears to be corrupt."
  EXIT SUB
 END IF

 IF islumpfile(hsp, "source.lumped") THEN
  ' Extract to folder
  ' unlumpfile shows a message on error
  unlumpfile(hsp, "source.lumped", tmpdir)
  DIM lumpedsources as string = tmpdir & SLASH "source.lumped"
  IF NOT isfile(lumpedsources) THEN
   notification "Couldn't extract scripts; corruption?"
   EXIT SUB
  END IF

  DIM dest as string
  dest = inputfilename("Export scripts to which (new) directory?", "", trimfilename(sourcerpg), "", _
                       trimpath(trimextension(sourcerpg)) & " scripts")

  IF LEN(dest) = 0 THEN
   EXIT SUB  'cancelled
  ELSEIF isdir(dest) THEN
   'Overwrite existing files
  ELSEIF isfile(dest) THEN
   notification "Destination directory `" & dest & "' already exists as a file! Pick a different name."
   EXIT SUB
  ELSEIF makedir(dest) <> 0 THEN
   notification "Couldn't create directory " & dest
   EXIT SUB
  END IF

  IF unlump(lumpedsources, dest) THEN
   notification "Extracted scripts to " & dest
  END IF

 ELSEIF islumpfile(hsp, "source.txt") THEN
  ' Extract as a single file
  notification "A backup of the scripts is available, but they are all mushed " _
               "up into a single file. You'll have to clean it up."
  DIM dest as string
  dest = inputfilename("Export scripts to which file?", ".hss", "", "", trimextension(trimpath(sourcerpg)))
  IF LEN(dest) THEN
   unlumpfile(hsp, "source.txt", tmpdir)
   copyfile tmpdir & "source.txt", dest
   safekill tmpdir & "source.txt"
   notification "Extracted scripts as " & dest
  END IF

 ELSE
  IF header.hspeak_version = "" ORELSE strcmp(STRPTR(header.hspeak_version), STRPTR("3I ")) < 0 THEN
   ' They are old enough for hsdecmpl to work
   notification "No backup of the original script source is available, but the scripts are " _
                "old enough to decompile with HSDECMPL, which I will now attempt. " _
                "This may not work; ask for help by email/forums if not."
   decompile_scripts
  ELSE
   IF yesno("A backup of original script source is not available (it was " _
            "purposedfully omitted), and the scripts appear to be too recent for " _
            "the HSDECMPL decompiler to work. Try anyway?", NO, NO) THEN
    decompile_scripts
   END IF
  END IF
 END IF
END SUB

'Try to decompile old scripts using the HSDECMPL tool.
SUB decompile_scripts()
 DIM hsdecmpl as string
 hsdecmpl = find_helper_app("hsdecmpl", YES, "http://hamsterrepublic.com/ohrrpgce/thirdparty/hsdecmpl.zip")
 IF LEN(hsdecmpl) = 0 THEN
  notification "The scripts could be decompiled with HSDECMPL but it's not installed. " _
               "Get it from http://rpg.hamsterrepublic.com/ohrrpgce/HS_Decompiler. " _
               "Ask for help by email/on the forums."
  EXIT SUB
 END IF

 DIM dest as string
 dest = inputfilename("Export scripts to which file?", ".hss", "", "", trimextension(trimpath(sourcerpg)))
 IF dest = "" THEN EXIT SUB

 DIM as string args, spawn_ret, hsi
 hsi = exportnames()

 args = escape_filename(game & ".hsp") & " " & escape_filename(dest & ".hss")
 args &= " " & escape_filename("plotscr.hsd+scancode.hsi+" & hsi)
 args &= " /F:default"
 spawn_ret = spawn_and_wait(hsdecmpl, args)
 IF LEN(spawn_ret) THEN
  notification "Running HSDECMPL failed: " & spawn_ret
 END IF
END SUB


'==========================================================================================
'                              Script browsing & selecting
'==========================================================================================


'Modifies 'trigger' (a script trigger) and returns the display name of the selected script (which might "[none]")
'allow_default: true when 0 means default and -1 means none.
'default: only when allow_default. The ID/trigger of the default script.
FUNCTION scriptbrowse (byref trigger as integer, byval triggertype as integer, scrtype as string, allow_default as bool = NO, default as integer = 0) as string
 DIM localbuf(20) as integer
 REDIM scriptnames(0) as string
 REDIM scriptids(0) as integer
 
 DIM fh as integer
 DIM i as integer

 'First option is [none]
 scriptnames(0) = "[none]"
 scriptids(0) = IIF(allow_default, -1, 0)
 'Then maybe comes "[default] default-script-name"
 IF allow_default THEN
  a_append scriptnames(), scriptname_default(0, default)
  a_append scriptids(), 0
 END IF

 'If trigger is a script that isn't imported, either numbered or a plotscript, then
 'show it as a special option at the top of the menu, equivalent to cancelling
 IF trigger > 0 THEN
  IF a_find(script_names(), trigger) = -1 THEN
   a_append scriptnames(), scriptname(trigger)
   a_append scriptids(), trigger
  END IF
 END IF

 ' 0 to firstscript - 1 are special options (none, current script)
 ' firstscript to numberedlast are oldstyle numbered scripts
 ' numberedlast + 1 to scriptmax are newstyle trigger scripts
 DIM firstscript as integer = UBOUND(scriptids) + 1

 'Look through lists of definescript scripts too
 IF OPENFILE(workingdir + SLASH + "plotscr.lst", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  FOR record as integer = 0 TO gen(genNumPlotscripts) - 1
   loadrecord localbuf(), fh, 20
   'Add any non-autonumbered scripts (definescript was used)
   IF localbuf(0) < 16384 THEN
    a_append scriptnames(), localbuf(0) & " " + readbinstring(localbuf(), 1, 36)
    a_append scriptids(), localbuf(0)
   END IF
  NEXT
  CLOSE #fh
 END IF

 DIM numberedlast as integer = UBOUND(scriptids)

 'And list of plotscripts
 IF OPENFILE(workingdir + SLASH + "lookup1.bin", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  FOR record as integer = 0 TO (LOF(fh) \ 40) - 1
   loadrecord localbuf(), fh, 20
   IF localbuf(0) <> 0 THEN
    a_append scriptids(), 16384 + record
    a_append scriptnames(), readbinstring(localbuf(), 1, 36)
   END IF
  NEXT
  CLOSE #fh
 END IF

 DIM scriptmax as integer = UBOUND(scriptids)

 IF scriptmax < firstscript AND allow_default = NO THEN
  RETURN "[no scripts]"
 END IF

 'insertion sort numbered scripts by id
 FOR i = firstscript + 1 TO numberedlast
  FOR j as integer = i - 1 TO firstscript STEP -1
   IF scriptids(j + 1) < scriptids(j) THEN
    SWAP scriptids(j + 1), scriptids(j)
    SWAP scriptnames(j + 1), scriptnames(j)
   ELSE
    EXIT FOR
   END IF
  NEXT
 NEXT

 'sort trigger scripts by name
 FOR i = numberedlast + 1 TO scriptmax - 1
  FOR j as integer = scriptmax TO i + 1 STEP -1
   IF LCASE(scriptnames(i)) > LCASE(scriptnames(j)) THEN
    SWAP scriptids(i), scriptids(j)
    SWAP scriptnames(i), scriptnames(j)
   END IF
  NEXT
 NEXT

 DIM scriptnames_display(scriptmax) as string

 DIM selectst as SelectTypeState
 DIM state as MenuState
 WITH state
  .pt = 0
  .autosize = YES
  .autosize_ignore_lines = 2
 END WITH
 init_menu_state state, scriptnames()
 'Preselect the current script (which could include [none], [default],
 'or the special missing option)
 FOR i = 0 TO scriptmax
  IF trigger = scriptids(i) THEN state.pt = i: EXIT FOR
 NEXT
 state.top = large(0, small(state.pt - 10, scriptmax - 21))

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN RETURN scriptname(trigger)
  IF keyval(scF1) > 1 THEN show_help "scriptbrowse"
  IF enter_space_click(state) THEN EXIT DO
  usemenu state

  IF select_by_typing(selectst) THEN
   select_instr scriptnames(), selectst, state
  END IF

  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage
  textcolor uilook(uiText), 0
  printstr "Pick " & LCASE(scrtype), 0, 0, dpage
  highlight_menu_typing_selection scriptnames(), scriptnames_display(), selectst, state
  standardmenu scriptnames_display(), state, 8, 10, dpage
  SWAP dpage, vpage
  setvispage vpage
  dowait
 LOOP

 trigger = scriptids(state.pt)
 IF scriptids(state.pt) < 16384 THEN
  'ID is prepended: strip "123 Spifflicate" to "Spifflicate"
  RETURN MID(scriptnames(state.pt), INSTR(scriptnames(state.pt), " ") + 1)
 ELSE
  RETURN scriptnames(state.pt)
 END IF
END FUNCTION

FUNCTION scrintgrabber (byref n as integer, byval min as integer, byval max as integer, byval less as KBScancode=ccLeft, byval more as KBScancode=ccRight, byval scriptside as integer, byval triggertype as integer) as bool
 'Allow scrolling through scripts with the less/more keys, plus obsolete typing in of
 'script IDs. That is really obsolete because the script ID is no longer displayed
 'unless there is no script with that ID AND it's less than the last used ID, so it's
 'impossible to see what you're doing. Should just delete that stuff.
 '
 'Returns true if n was changed.
 'less/more: scancodes
 'scriptside is 1 or -1: on which side of zero are the scripts
 'min or max on side of scripts is ignored

 DIM temp as integer = n
 IF scriptside < 0 THEN
  temp = -n
  SWAP less, more
  min = -min
  max = -max
  SWAP min, max
 END IF

 DIM seekdir as integer = 0
 IF keyval(more) > 1 THEN
  seekdir = 1
 ELSEIF keyval(less) > 1 THEN
  seekdir = -1
 END IF

 DIM scriptscroll as bool = NO
 IF seekdir <> 0 THEN
  scriptscroll = NO
  IF temp = min AND seekdir = -1 THEN
   temp = -1
   scriptscroll = YES
  ELSEIF (temp = 0 AND seekdir = 1) OR temp > 0 THEN
   scriptscroll = YES
  END IF
  IF scriptscroll THEN
   'scroll through scripts
   seekscript temp, seekdir, triggertype
   IF temp = -1 THEN temp = min
  ELSE
   'regular scroll
   temp += seekdir
  END IF
 ELSE
  IF (temp > 0 AND temp < 16384) OR (temp = 0 AND scriptside = 1) THEN
   'if a number is entered, don't seek to the next script, allow "[id]" to display instead
   IF intgrabber(temp, 0, 16383, 0, 0) THEN
    'if temp starts off greater than gen(genMaxRegularScript) then don't disturb it
    temp = small(temp, gen(genMaxRegularScript))
   END IF
  ELSEIF temp < 0 OR (temp = 0 AND scriptside = -1) THEN
   intgrabber(temp, min, 0, 0, 0)
  ELSE
   ' Only treat Backspace this way if not typing in an ID
   IF keyval(scBackspace) > 1 THEN temp = 0
  END IF
 END IF

 IF keyval(scDelete) > 1 THEN temp = 0
 IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN temp = bound(-temp, min, gen(genMaxRegularScript))

 temp = temp * SGN(scriptside)
 scrintgrabber = (temp <> n) ' Returns true if byref n has changed
 n = temp
END FUNCTION

LOCAL SUB seekscript (byref temp as integer, byval seekdir as integer, byval triggertype as integer)
 'Helper function to find the next script ID/trigger assigned to a script that exists.
 'temp = -1 means scroll to last script
 'returns 0 when scrolled past first script, -1 when went past last
 'triggertype not used (yet?)

 DIM buf(19) as integer
 DIM plotids(gen(genMaxRegularScript)) as integer
 DIM recordsloaded as integer = 0
 DIM screxists as bool = NO

 DIM num_triggers as integer
 DIM fh as integer
 IF OPENFILE(workingdir & SLASH & "lookup1.bin", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  num_triggers = LOF(fh) \ 40
 END IF
 IF temp = -1 THEN temp = num_triggers + 16384

 DO
  temp += seekdir
  IF temp > gen(genMaxRegularScript) AND temp < 16384 THEN
   IF seekdir > 0 THEN
    temp = 16384
   ELSE
    temp = gen(genMaxRegularScript)
   END IF
  END IF
  IF temp <= 0 THEN EXIT DO
  IF temp >= num_triggers + 16384 THEN
   temp = -1
   EXIT DO
  END IF
  'check script exists, else keep looking
  IF temp < 16384 THEN
   IF plotids(temp) THEN
    screxists = YES
   ELSE
    ' Find out which script IDs < 16384 are used (do this just once)
    WHILE recordsloaded < gen(genNumPlotscripts)
     loadrecord buf(), workingdir + SLASH + "plotscr.lst", 20, recordsloaded
     recordsloaded += 1
     IF buf(0) = temp THEN screxists = YES: EXIT WHILE
     IF buf(0) <= gen(genMaxRegularScript) THEN plotids(buf(0)) = YES
    WEND
   END IF
  END IF
  IF temp >= 16384 ANDALSO fh THEN
   loadrecord buf(), fh, 20, temp - 16384
   IF buf(0) THEN screxists = YES
  END IF
  IF screxists THEN EXIT DO
 LOOP

 IF fh THEN CLOSE fh
END SUB


'==========================================================================================
'                             Script usage visiter & autofix
'==========================================================================================


'--For each script trigger datum in the game, call visitor (whether or not there
'--is a script set there; however fields which specify either a script or
'--something else, eg. either a script or a textbox, may be skipped)
SUB visit_scripts(byval visitor as FnScriptVisitor)
 DIM as integer i, j, idtmp
 DIM resave as bool

 '--global scripts
 visitor(gen(genNewGameScript), "new game", "")
 visitor(gen(genLoadGameScript), "load game", "")
 visitor(gen(genGameoverScript), "game over", "")
 visitor(gen(genEscMenuScript), "esc menu", "")

 '--Text box scripts
 DIM box as TextBox
 FOR i as integer = 0 TO gen(genMaxTextbox)
  LoadTextBox box, i
  resave = NO
  IF box.instead < 0 THEN
   idtmp = -box.instead
   resave OR= visitor(idtmp, "box " & i & " (instead)", textbox_preview_line(box, vpages(vpage)->w - 80))
   box.instead = -idtmp
  END IF
  IF box.after < 0 THEN
   idtmp = -box.after
   resave OR= visitor(idtmp, "box " & i & " (after)", textbox_preview_line(box, vpages(vpage)->w - 80))
   box.after = -idtmp
  END IF
  IF resave THEN
   SaveTextBox box, i
  END IF
 NEXT i
 
 '--Map scripts and NPC scripts
 DIM gmaptmp(dimbinsize(binMAP)) as integer
 REDIM npctmp(0) as NPCType
 FOR i = 0 TO gen(genMaxMap)
  resave = NO
  loadrecord gmaptmp(), game & ".map", getbinsize(binMAP) \ 2, i
  resave OR= visitor(gmaptmp(7), "map " & i & " autorun", "")
  resave OR= visitor(gmaptmp(12), "map " & i & " after-battle", "")
  resave OR= visitor(gmaptmp(13), "map " & i & " instead-of-battle", "")
  resave OR= visitor(gmaptmp(14), "map " & i & " each-step", "")
  resave OR= visitor(gmaptmp(15), "map " & i & " on-keypress", "")
  IF resave THEN
   storerecord gmaptmp(), game & ".map", getbinsize(binMAP) \ 2, i
  END IF
  'loop through NPC's
  LoadNPCD maplumpname(i, "n"), npctmp()
  resave = NO
  FOR j = 0 TO UBOUND(npctmp)
   resave OR= visitor(npctmp(j).script, "map " & i & " NPC " & j, "")
  NEXT j
  IF resave THEN
   SaveNPCD maplumpname(i, "n"), npctmp()
  END IF
 NEXT i
 
 '--vehicle scripts
 DIM vehicle as VehicleData
 FOR i = 0 TO gen(genMaxVehicle)
  resave = NO
  LoadVehicle game & ".veh", vehicle, i
  IF vehicle.use_button > 0 THEN
   resave OR= visitor(vehicle.use_button, "use button veh " & i, """" & vehicle.name & """")
  END IF
  IF vehicle.menu_button > 0 THEN
   resave OR= visitor(vehicle.menu_button, "menu button veh " & i, """" & vehicle.name & """")
  END IF
  IF vehicle.on_mount < 0 THEN
   idtmp = -(vehicle.on_mount)
   resave OR= visitor(idtmp, "mount vehicle " & i, """" & vehicle.name & """")
   vehicle.on_mount = -idtmp
  END IF
  IF vehicle.on_dismount < 0 THEN
   idtmp = -(vehicle.on_dismount)
   resave OR= visitor(idtmp, "dismount vehicle " & i,  """" & vehicle.name & """")
   vehicle.on_dismount = -idtmp
  END IF
  IF resave THEN
   SaveVehicle game & ".veh", vehicle, i
  END IF
 NEXT i
 
 '--shop scripts
 DIM shoptmp(19) as integer
 DIM shopname as string
 FOR i = 0 TO gen(genMaxShop)
  loadrecord shoptmp(), game & ".sho", 20, i
  shopname = readbadbinstring(shoptmp(), 0, 15)
  IF visitor(shoptmp(19), "show inn " & i, """" & shopname & """") THEN
   storerecord shoptmp(), game & ".sho", 20, i
  END IF
 NEXT i
 
 '--menu scripts
 DIM menu_set as MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 FOR i = 0 TO gen(genMaxMenu)
  resave = NO
  DIM menutmp as MenuDef
  LoadMenuData menu_set, menutmp, i
  FOR j = 0 TO menutmp.numitems - 1
   WITH *menutmp.items[j]
    IF .t = 4 THEN
     resave OR= visitor(.sub_t, "menu " & i & " item " & j, """" & .caption & """")
    END IF
   END WITH
  NEXT j
  resave OR= visitor(menutmp.on_close, "menu " & i & " on-close", """" & menutmp.name & """")
  IF resave THEN
   SaveMenuData menu_set, menutmp, i
  END IF
 NEXT i

END SUB

'For script_usage_list and script_usage_visitor
DIM SHARED plotscript_order() as integer
DIM SHARED script_usage_menu() as IntStrPair

'Internal to script_usage_list
LOCAL FUNCTION script_usage_visitor(byref trig as integer, description as string, caption as string) as bool
 IF trig = 0 THEN RETURN NO
 '--See script_usage_list about rank calculation
 DIM rank as integer = trig
 IF trig >= 16384 THEN rank = 100000 + plotscript_order(trig - 16384)
 a_append script_usage_menu(), rank, "  " & description & " " & caption
 RETURN NO  'trig not modified
END FUNCTION

' Export menu() to a file, except for the first items.
SUB script_list_export (menu() as string, description as string, remove_first_items as integer)
 DIM title as string = trimpath(trimextension(sourcerpg)) + " " + description
 DIM fname as string = inputfilename("Filename to export " & description & " list to?", _
                                     ".txt", "", "", title)
 IF LEN(fname) THEN
  DIM lines() as string
  a_copy menu(), lines()
  FOR i as integer = 1 TO remove_first_items
   a_pop lines(), 0
  NEXT
  a_insert lines(), 0, title
  a_insert lines(), 1, "Exported " & DATE & " " & TIME
  a_insert lines(), 2, ""
  lines_to_file lines(), fname + ".txt", LINE_END
 END IF
END SUB

SUB script_usage_list ()
 DIM buf(20) as integer
 DIM id as integer
 DIM s as string
 DIM fh as integer
 DIM i as integer
 'DIM t as double = TIMER

 'Build script_usage_menu, which is an list of menu items, initially out of order.
 'The integer in each pair is used to sort the menu items into the right order:
 'items for old-style scripts have rank = id
 'all plotscripts are ordered by name and given rank = 100000 + alphabetic rank
 'Start by adding all the script names to script_usage_menu (so that they'll
 'appear first when we do a stable sort), then add script instances.

 REDIM script_usage_menu(1)
 script_usage_menu(0).i = -2
 script_usage_menu(0).s = "Back to Previous Menu"
 script_usage_menu(1).i = -1
 script_usage_menu(1).s = "Export to File..."
 DIM num_fixed_menu_items as integer = 2

 'Loop through old-style non-autonumbered scripts
 IF OPENFILE(workingdir & SLASH & "plotscr.lst", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  FOR i as integer = 0 TO gen(genNumPlotscripts) - 1
   loadrecord buf(), fh, 20, i
   id = buf(0)
   IF id <= 16383 THEN
    s = id & ":" & readbinstring(buf(), 1, 38)
    a_append script_usage_menu(), id, s
   END IF
  NEXT i
  CLOSE #fh
 END IF

 'Loop through new-style plotscripts

 'First, a detour: determine the alphabetic rank of each plotscript
 REDIM plotscripts(0) as string
 IF OPENFILE(workingdir & SLASH & "lookup1.bin", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  WHILE loadrecord(buf(), fh, 20, , NO)  'expectexists=NO
   s = readbinstring(buf(), 1, 38)
   a_append plotscripts(), s
  WEND

  'Have to skip if no plotscripts
  IF UBOUND(plotscripts) > 0 THEN
   'We must skip plotscripts(0)
   REDIM plotscript_order(UBOUND(plotscripts) - 1)
   qsort_strings_indices plotscript_order(), @plotscripts(1), UBOUND(plotscripts), sizeof(string)
   invert_permutation plotscript_order()

   'OK, now that we can calculate ranks, we can add new-style scripts
   SEEK #fh, 1
   i = 0
   WHILE loadrecord(buf(), fh, 20)
    id = buf(0)
    IF id <> 0 THEN
     s = readbinstring(buf(), 1, 38)
     a_append script_usage_menu(), 100000 + plotscript_order(i), s
    END IF
    i += 1
   WEND
  END IF
  CLOSE #fh
 END IF

 'add script instances to script_usage_menu
 visit_scripts @script_usage_visitor

 IF UBOUND(script_usage_menu) = 1 THEN
  a_append script_usage_menu(), 0, "No scripts imported!"
 END IF

 'sort, and build menu() (for standardmenu)
 DIM indices(UBOUND(script_usage_menu)) as integer
 REDIM menu(UBOUND(script_usage_menu)) as string
 sort_integers_indices indices(), @script_usage_menu(0).i, UBOUND(script_usage_menu) + 1, sizeof(IntStrPair)

 DIM currentscript as integer = -1
 DIM j as integer = 0
 FOR i as integer = 0 TO UBOUND(script_usage_menu)
  WITH script_usage_menu(indices(i))
   IF MID(.s, 1, 1) = " " THEN
    'script trigger
    'Do not add triggers which are missing their scripts; those go in the other menu
    IF .i <> currentscript THEN CONTINUE FOR
   END IF
   menu(j) = .s
   j += 1
   currentscript = .i
  END WITH
 NEXT
 REDIM PRESERVE menu(j - 1)
 DIM menu_display(j - 1) as string

 'Free memory
 REDIM plotscript_order(0)
 REDIM script_usage_menu(0)

 'debug "script usage in " & ((TIMER - t) * 1000) & "ms"

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.autosize = YES
 init_menu_state state, menu()
 
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "script_usage_list"
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN script_list_export menu(), "script usage", num_fixed_menu_items
  END IF
  usemenu state
  IF select_by_typing(selectst) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP 
END SUB

'--For script_broken_trigger_list and check_broken_script_trigger
DIM SHARED missing_script_trigger_list() as string

LOCAL FUNCTION check_broken_script_trigger(byref trig as integer, description as string, caption as string) as bool
 IF trig <= 0 THEN RETURN NO ' No script trigger
 '--decode script trigger
 DIM id as integer
 id = decodetrigger(trig)
 '--Check for missing new-style script
 IF id = 0 THEN
  a_append missing_script_trigger_list(), description & " " & scriptname(trig) & " missing. " & caption 
 ELSEIF id < 16384 THEN
  '--now check for missing old-style scripts
  IF a_find(script_names(), id) <> -1 THEN RETURN NO 'Found okay

  a_append missing_script_trigger_list(), description & " ID " & id & " missing. " & caption
 ELSEIF id >= 16384 AND id = trig THEN
  '--The trigger was not decoded, which should not happen since you can't select autonumbered scripts!
  '--Prehaps a lump was copied from a different .rpg file
  visible_debug description & " (" & caption & ") script trigger " & (trig - 16384) & " is invalid"
 END IF
 RETURN NO
END FUNCTION

SUB script_broken_trigger_list()
 REDIM missing_script_trigger_list(1) as string
 missing_script_trigger_list(0) = "Back to Previous Menu"
 missing_script_trigger_list(1) = "Export to File..."
 DIM num_fixed_menu_items as integer = 2

 visit_scripts @check_broken_script_trigger

 IF UBOUND(missing_script_trigger_list) = num_fixed_menu_items - 1 THEN
  a_append missing_script_trigger_list(), "No broken triggers found!"
 END IF

 DIM state as MenuState
 state.autosize = YES
 init_menu_state state, missing_script_trigger_list()

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "script_broken_trigger_list"
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN script_list_export missing_script_trigger_list(), "broken triggers", num_fixed_menu_items
  END IF
  usemenu state

  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage 
  standardmenu missing_script_trigger_list(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP 
 'Free memory
 REDIM missing_script_trigger_list(0)
END SUB

'Internal, called from autofix_broken_old_scripts()
FUNCTION autofix_old_script_visitor(byref id as integer, description as string, caption as string) as bool
 '--returns true if a fix has occured
 IF id = 0 THEN RETURN NO ' not a trigger
 IF id >= 16384 THEN RETURN NO 'New-style script
 IF a_find(script_names(), id) <> -1 THEN RETURN NO 'Found okay

 DIM buf(19) as integer
 DIM fh as integer
  
 DIM found_name as string = ""

 'Read the old copy of plotscr.lst
 IF OPENFILE(workingdir & "plotscr.lst.old.tmp", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  FOR i as integer = 0 TO (LOF(fh) \ 40) - 1
   loadrecord buf(), fh, 20, i
   IF buf(0) = id THEN '--Yay! found it in the old file!
    found_name = readbinstring(buf(), 1, 38)
    EXIT FOR
   END IF
  NEXT i
  CLOSE #fh
 END IF

 IF found_name = "" THEN RETURN NO '--broken but unfixable (no old name)

 IF OPENFILE(workingdir & SLASH & "lookup1.bin", FOR_BINARY + ACCESS_READ, fh) = fberrOK THEN
  FOR i as integer = 0 TO (LOF(fh) \ 40) - 1
   loadrecord buf(), fh, 20, i
   IF found_name = readbinstring(buf(), 1, 38) THEN '--Yay! found it in the new file!
    id = 16384 + i
    CLOSE #fh
    RETURN YES '--fixed it, report a change!
   END IF
  NEXT i
  CLOSE #fh
 END IF

 RETURN NO '--broken but unfixable (no matching new name)
END FUNCTION

'If the user converted any scripts from old-style definescript scripts into
'plotscripts, automatically convert any triggers where those script IDs were used.
'This is called after importing scripts.
SUB autofix_broken_old_scripts()
 '--sanity test
 IF NOT isfile(workingdir & "plotscr.lst.old.tmp") THEN
  debug "can't autofix broken old scripts, can't find: " & workingdir & "plotscr.lst.old.tmp"
  EXIT SUB
 END IF

 visit_scripts @autofix_old_script_visitor
END SUB
