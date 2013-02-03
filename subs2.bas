'OHRRPGCE CUSTOM - More misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'FIXME: A large majority of the code in subs2.bas is textbox editor related.
'       Maybe this file should be renamed textboxedit.bas ?
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

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

#include "scrconst.bi"

'Types

TYPE TriggerSet
 size as integer
 trigs as TriggerData ptr
 usedbits as unsigned integer ptr
END TYPE

'--Local subs and functions
DECLARE FUNCTION compilescripts (fname as string) as string
DECLARE SUB importscripts (f as string)
DECLARE SUB writeconstant (byval filehandle as integer, byval num as integer, names as string, unique() as string, prefix as string)
DECLARE FUNCTION isunique (s as string, set() as string) as integer
DECLARE SUB exportnames ()
DECLARE SUB addtrigger (scrname as string, byval id as integer, byref triggers as TRIGGERSET)

DECLARE FUNCTION textbox_condition_caption(tag as integer, prefix as string = "") as string
DECLARE FUNCTION textbox_condition_short_caption(tag as integer) as string
DECLARE SUB write_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer, num as integer)
DECLARE FUNCTION read_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer) as integer
DECLARE FUNCTION box_conditional_type_by_menu_index(menuindex as integer) as integer
DECLARE SUB update_textbox_editor_main_menu (byref box as TextBox, menu() as string)
DECLARE SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, override_y as integer=-1, suppress_text as integer=NO)
DECLARE SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB update_textbox_appearance_editor_menu (menu() as string, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, holdscreen as integer)
DECLARE SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_create_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_copy_style_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_connection_captions(byref node as TextboxConnectNode, id as integer, tag as integer, box as TextBox, topcation as string, use_tag as integer = YES)
DECLARE SUB textbox_connection_draw_node(byref node as TextboxConnectNode, x as integer, y as integer, selected as integer)
DECLARE SUB textbox_choice_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_conditionals(byref box as TextBox)
DECLARE SUB textbox_update_conditional_menu(byref box as TextBox, menu() as string)

'These are used in the TextBox conditional editor
CONST condEXIT   = -1
CONST condTAG    = 0
CONST condBATTLE = 1
CONST condSHOP   = 2
CONST condHERO   = 3
CONST condMONEY  = 4
CONST condDOOR   = 5
CONST condITEM   = 6
CONST condBOX    = 7
CONST condMENU   = 8
CONST condSETTAG = 9

SUB exportnames ()

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
'Need to call this quite a lot to refresh the screen for FB. Bit of a
'compromise between showing the process and slowing things down, since
'it will refresh the window every time.
setvispage 0

DIM fh as integer = FREEFILE
OPEN outf FOR OUTPUT as #fh
PRINT #fh, "# HamsterSpeak constant definitions for " & trimpath(sourcerpg)
PRINT #fh, ""
PRINT #fh, "define constant, begin"

printstr "tag names", 0, pl * 8, 0: pl = pl + 1
REDIM u(0) as string
FOR i as integer = 2 TO max_tag()
 writeconstant fh, i, load_tag_name(i), u(), "tag"
NEXT i

printstr "song names", 0, pl * 8, 0: pl = pl + 1
REDIM u(0) as string
FOR i as integer = 0 TO gen(genMaxSong)
 writeconstant fh, i, getsongname(i), u(), "song"
NEXT i
setvispage 0

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
writeconstant fh, 1, "Weapon", u(), "slot"
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
setvispage 0
waitforanykey

END SUB

SUB addtrigger (scrname as string, byval id as integer, triggers as TRIGGERSET)
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

SUB compile_andor_import_scripts (f as string)
 IF justextension(f) <> "hs" THEN
  f = compilescripts(f)
  IF f <> "" THEN
   importscripts f
   safekill f  'reduce clutter
  END IF
 ELSE
  importscripts f
 END IF
END SUB

SUB importscripts (f as string)
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
   OPEN tmpdir + "scripts.bin" FOR BINARY as #fptr
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
   OPEN tmpdir + "scripts.txt" FOR INPUT as #fptr
  END IF

  'load in existing trigger table
  WITH triggers
    DIM fh as integer = 0
    .size = 0
    DIM fname as string = workingdir & SLASH & "lookup1.bin"
    IF isfile(fname) THEN
     fh = FREEFILE
     OPEN fname FOR BINARY as #fh
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
  IF OPEN(workingdir + SLASH + "plotscr.lst" FOR BINARY AS #plotscr_lsth) THEN
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
   IF trigger <> 0 THEN
    addtrigger names, id, triggers
    triggercount += 1
   END IF

   'display progress
   IF id < 16384 OR trigger > 0 THEN
    viscount = viscount + 1
    append_message names & ", "
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

  '--reload cache (we only cache this to share code between Game and Custom)
  load_lookup1_bin lookup1_bin_cache()

  '--fix the references to any old-style plotscripts that have been converted to new-style scripts.
  append_message " ...autofix broken triggers..."
  autofix_broken_old_scripts

  '--erase the temporary backup copy of plotscr.lst
  safekill tmpdir & "plotscr.lst.tmp"
  
  textcolor uilook(uiText), 0
  show_message "imported " & viscount & " scripts"
  waitforanykey
 ELSE
  pop_warning f + " is not really a compiled .hs file. Did you create it by compiling a" _
              " script file with hspeak.exe, or did you just give your script a name that" _
              " ends in .hs and hoped it would work? Use hspeak.exe to create real .hs files"
 END IF
END SUB

FUNCTION isunique (s as string, set() as string) as integer
 DIM key as string
 key = sanitize_script_identifier(LCASE(s), NO)

 FOR i as integer = 1 TO UBOUND(set)
  IF key = set(i) THEN RETURN NO
 NEXT i

 str_array_append set(), key
 RETURN YES
END FUNCTION

SUB scriptman ()
STATIC defaultdir as string
DIM menu(5) as string

DIM menumax as integer = 4
menu(0) = "Previous Menu"
menu(1) = "Compile and/or Import scripts (.hss/.hs)"
menu(2) = "Export names for scripts (.hsi)"
menu(3) = "Check where scripts are used..."
menu(4) = "Find broken script triggers..."

DIM f as string
DIM pt as integer = 1
DIM tog as integer
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "script_management"
 usemenu pt, 0, 0, menumax, 24
 IF enter_or_space() THEN
  SELECT CASE pt
   CASE 0
    EXIT DO
   CASE 1
    f = browse(9, defaultdir, "", "",, "browse_hs")
    IF f <> "" THEN
     'clearkey scEnter
     'clearkey scSpace
     compile_andor_import_scripts f
    END IF
   CASE 2
    exportnames
   CASE 3
    script_usage_list()
   CASE 4
    script_broken_trigger_list()
  END SELECT
 END IF

 clearpage dpage
 standardmenu menu(), menumax, 22, pt, 0, 0, 0, dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

FUNCTION get_hspeak_version(hspeak_path as string) as string
 DIM tempf as string = tmpdir & "hspeak_ver.txt"
 'Note this will momentarily pop up a console window on Windows, unpleasant.
 'Could get around this by using open_piped_process
 safe_shell escape_filename(hspeak_path) & " -k > " & escape_filename(tempf)
 DIM fh as integer = FREEFILE
 IF OPEN(tempf FOR INPUT AS fh) THEN
  debug "get_hspeak_version: Couldn't run " & hspeak_path
  RETURN ""
 END IF
 DIM line1 as string
 INPUT #fh, line1
 CLOSE fh
 safekill tempf
 'debug "line1: " & line1
 DIM version as string = MID(line1, INSTR(line1, " v") + 2, 3)
 IF LEN(version) <> 3 ORELSE isdigit(version[0]) = NO THEN
  debug "Couldn't get HSpeak version from head line: " & line1
  RETURN ""
 END IF
 RETURN version
END FUNCTION

'Returns filename of .hs file
FUNCTION compilescripts(fname as string) as string
 DIM as string outfile, hspeak, errmsg, hspeak_ver, args
 hspeak = find_helper_app("hspeak")
 IF hspeak = "" THEN
  notification missing_helper_message("hspeak")
  RETURN ""
 END IF
 args = "-y"

 IF slave_channel <> NULL_CHANNEL THEN
  IF isfile(game & ".hsp") THEN
   'Try to reuse script IDs from existing scripts if any, so that currently running scripts
   'don't start calling the wrong scripts due to ID remapping
   hspeak_ver = get_hspeak_version(hspeak)
   'debug "hspeak version '" & hspeak_ver & "'"
   IF hspeak_ver = "" ORELSE strcmp(STRPTR(hspeak_ver), STRPTR("3Pa")) < 0 THEN
    'If get_hspeak_version failed (returning ""), then spawn_and_wait should as well
    IF LEN(hspeak_ver) THEN
     notification "Your copy of HSpeak is out of date. You should use the latest version."
    END IF
   ELSE
    unlumpfile game & ".hsp", "scripts.bin", tmpdir
    'scripts.bin will be missing in scripts compiled with very old HSpeak versions
    args += " --reuse-ids " & escape_filename(tmpdir & "scripts.bin")
   END IF
  END IF
 END IF
 outfile = trimextension(fname) + ".hs"
 safekill outfile
 'Wait for keys: we spawn a command prompt/xterm/Terminal.app, which will be closed when HSpeak exits
 errmsg = spawn_and_wait(hspeak, args & " " & escape_filename(simplify_path_further(fname, curdir)))
 IF LEN(errmsg) THEN
  notification errmsg
  RETURN ""
 END IF
 IF isfile(outfile) = NO THEN
  notification "Compilation failed."
  RETURN ""
 END IF
 RETURN outfile
END FUNCTION

SUB text_box_editor () 'textage
 STATIC browse_default as string
 DIM menu(10) as string
 DIM h(2) as string
 DIM tagmn as string
 DIM gcsr as integer
 DIM tcur as integer
 DIM box as TextBox
 DIM st as TextboxEditState
 WITH st
  .id = 1
  .search = ""
 END WITH

 '--For the import/export support
 DIM box_text_file as string
 DIM overwrite as integer = NO
 DIM remember_boxcount as integer
 DIM backup_say as string
 DIM import_warn as string = ""

 menu(0) = "Return to Main Menu"
 menu(1) = "Text Box"
 menu(2) = "Edit Text"
 menu(3) = "Edit Conditionals"
 menu(4) = "Edit Choice"
 menu(5) = "Box Appearance"
 menu(6) = "Next:"
 menu(7) = "Text Search:"
 menu(8) = "Connected Boxes..."
 menu(9) = "Export text boxes..."
 menu(10) = "Import text boxes..."
 
 DIM state as MenuState
 state.pt = 0
 state.last = UBOUND(menu)
 state.size = 24
 
 DIM style_clip as integer = 0
 
 DIM remptr as integer
 DIM temptrig as integer
 
 textbox_edit_load box, st, menu()
 setkeys YES
 DO
  setwait 55
  setkeys YES
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_main"
  IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 0 THEN
   SaveTextBox box, st.id
   cropafter st.id, gen(genMaxTextBox), 0, game & ".say", curbinsize(binSAY)
   textbox_edit_load box, st, menu()
  END IF
  usemenu state
  remptr = st.id
  SELECT CASE state.pt
   CASE 7'textsearch
    strgrabber st.search, 36
   CASE 6'quickchainer
    IF scrintgrabber(box.after, 0, gen(genMaxTextbox), scLeft, scRight, -1, plottrigger) THEN
     update_textbox_editor_main_menu box, menu()
    END IF'--modify next
   CASE ELSE '--not using the quick textbox chainer nor the search
    IF keyval(scAlt) > 0 AND keyval(scC) > 1 THEN style_clip = st.id
    IF keyval(scAlt) > 0 AND keyval(scV) > 1 THEN
     IF yesno("Copy box " & style_clip & "'s style to this box") THEN
      textbox_copy_style_from_box style_clip, box, st
      textbox_edit_load box, st, menu()
     END IF
    END IF
    IF intgrabber_with_addset(st.id, 0, gen(genMaxTextBox), 32767, "text box") THEN
     SaveTextBox box, remptr
     IF st.id > gen(genMaxTextBox) THEN
      gen(genMaxTextBox) = st.id
      textbox_create_from_box 0, box, st
     END IF
     textbox_edit_load box, st, menu()
    END IF
    IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND gen(genMaxTextBox) < 32767 THEN
     SaveTextBox box, st.id
     IF yesno("Create a textbox like this one?") THEN
      gen(genMaxTextBox) += 1
      st.id = gen(genMaxTextBox)
      textbox_create_from_box remptr, box, st
     END IF
     textbox_edit_load box, st, menu()
    END IF
  END SELECT
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 2 THEN textbox_line_editor box, st
   IF state.pt = 3 THEN
    textbox_conditionals box
    update_textbox_editor_main_menu box, menu()
   END IF
   IF state.pt = 4 THEN textbox_choice_editor box, st
   IF state.pt = 5 THEN textbox_appearance_editor box, st
   IF state.pt = 6 THEN
    IF box.after > 0 THEN
     SaveTextBox box, st.id
     st.id = box.after
     textbox_edit_load box, st, menu()
    ELSE
     temptrig = ABS(box.after)
     scriptbrowse temptrig, plottrigger, "textbox plotscript"
     box.after = -temptrig
     update_textbox_editor_main_menu box, menu()
    END IF
   END IF
   IF state.pt = 7 AND keyval(scEnter) > 1 THEN
    textbox_seek box, st
    textbox_edit_load box, st, menu()
   END IF
   IF state.pt = 8 THEN
    textbox_connections box, st, menu()
   END IF
   IF state.pt = 9 THEN '--Export textboxes to a .TXT file
    STATIC metadata(3) as integer
    DIM metadatalabels(3) as string
    metadatalabels(0) = "Text"
    metadata(0) = YES '--by default, export text
    metadatalabels(1) = "Conditionals"
    metadatalabels(2) = "Choices"
    metadatalabels(3) = "Appearance"
   
    IF askwhatmetadata(metadata(), metadatalabels()) = YES THEN
     box_text_file = inputfilename("Filename for TextBox Export?", ".txt", "", "input_file_export_textbox")
     IF box_text_file <> "" THEN
      box_text_file = box_text_file & ".txt"
      IF export_textboxes(box_text_file, metadata()) THEN
       notification "Successfully exported " & box_text_file
      ELSE
       notification "Failed to export " & box_text_file
      END IF '--export_textboxes
     END IF '--box_text_file <> ""
    END IF '--metadata
   END IF
   IF state.pt = 10 THEN '-- Import text boxes from a .TXT file
    SaveTextBox box, st.id
    IF yesno("Are you sure? Boxes will be overwritten", NO) THEN
     box_text_file = browse(0, browse_default, "*.txt", tmpdir, 0, "browse_import_textbox")
     clearpage vpage
     backup_say = tmpdir & "backup-textbox-lump.say"
     '--make a backup copy of the .say lump
     copyfile game & ".say", backup_say
     IF NOT isfile(backup_say) THEN
      notification "unable to save a backup copy of the text box data to " & backup_say
     ELSE
      '--Backup was successfuly, okay to proceed
      remember_boxcount = gen(genMaxTextbox)
      import_warn = ""
      IF import_textboxes(box_text_file, import_warn) THEN
       notification "Successfully imported """ & box_text_file & """." & import_warn
      ELSE
       'Failure! Reset, revert, abort, run-away!
       gen(genMaxTextBox) = remember_boxcount
       copyfile backup_say, game & ".say"
       notification "Import failed, restoring backup. " & import_warn
      END IF
     END IF
     LoadTextBox box, st.id
    END IF
   END IF
  END IF
  textcolor uilook(uiMenuItem), 0
  IF state.pt = 1 THEN textcolor uilook(uiSelectedItem + state.tog), 0

  IF st.id = 0 THEN
    menu(1) = "Text Box 0 [template]"
  ELSE
    menu(1) = "Text Box " & st.id
  END IF
  menu(7) = "Text Search:"
  IF state.pt = 7 THEN menu(7) &= st.search
 
  '--Draw box
  clearpage dpage
  textbox_edit_preview box, st, 96

  textcolor uilook(uiText), uilook(uiHighlight)
  printstr "+ to copy", 248, 0, dpage
  printstr "ALT+C copy style", 192, 8, dpage
  IF style_clip > 0 THEN printstr "ALT+V paste style", 184, 16, dpage
  standardmenu menu(), state, 0, 0, dpage, YES  'edged=YES

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 SaveTextBox box, st.id
 WITH st.portrait
  IF .sprite THEN frame_unload @.sprite
  IF .pal    THEN palette16_unload @.pal
 END WITH
 'See wiki for .SAY file format docs
END SUB

SUB textbox_conditionals(byref box as TextBox)
 DIM menu(-1 TO 22) as string
 
 DIM state as MenuState
 state.top = -1
 state.pt = 0
 state.first = -1
 state.last = 22
 state.size = 21

 DIM grey(-1 TO 22) as integer
 'This array tells which rows in the conditional editor are grey
 grey(-1) = NO
 grey(0) = YES
 grey(1) = NO
 grey(2) = YES
 grey(3) = NO
 grey(4) = NO
 grey(5) = YES
 grey(6) = NO
 grey(7) = YES
 grey(8) = NO
 grey(9) = YES
 grey(10) = NO
 grey(11) = YES
 grey(12) = NO
 grey(13) = YES
 grey(14) = NO
 grey(15) = NO
 grey(16) = NO
 grey(17) = YES
 grey(18) = NO
 grey(19) = YES
 grey(20) = NO
 grey(21) = YES 'Menu Tag
 grey(22) = NO

 DIM num as integer
 DIM temptrig as integer
 DIM c as integer

 textbox_update_conditional_menu box, menu()
 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_conditions"
  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE -1 '--Previous menu
     EXIT DO
    CASE 1 '--instead script
     temptrig = large(-box.instead, 0)
     scriptbrowse temptrig, plottrigger, "instead of textbox plotscript"
     box.instead = -temptrig
    CASE 22 '--after script
     temptrig = large(-box.after, 0)
     scriptbrowse temptrig, plottrigger, "after textbox plotscript"
     box.after = -temptrig
   END SELECT
   textbox_update_conditional_menu box, menu()
  END IF
  usemenu state
  IF keyval(scDelete) > 1 THEN ' Pressed the delete key
   write_box_conditional_by_menu_index box, state.pt, 0
   textbox_update_conditional_menu box, menu()
  END IF
  IF state.pt >= 0 THEN
   num = read_box_conditional_by_menu_index(box, state.pt)
   SELECT CASE box_conditional_type_by_menu_index(state.pt)
    CASE condTAG
     tag_grabber num
    CASE condSETTAG
     tag_grabber num, , , NO
    CASE condBATTLE
     intgrabber num, 0, gen(genMaxFormation)
    CASE condSHOP
     xintgrabber num, 0, gen(genMaxShop), -1, -32000
    CASE condHERO
     intgrabber num, -99, 99
    CASE condMONEY
     intgrabber num, -32000, 32000
    CASE condDOOR
     intgrabber num, 0, 199
    CASE condITEM
     xintgrabber num, 0, gen(genMaxItem), 0, -gen(genMaxItem)
    CASE condBOX
     scrintgrabber num, 0, gen(genMaxTextbox), scLeft, scRight, -1, plottrigger
    CASE condMENU
     intgrabber num, 0, gen(genMaxMenu)
   END SELECT
   IF num <> read_box_conditional_by_menu_index(box, state.pt) THEN
    'The value has changed
    write_box_conditional_by_menu_index(box, state.pt, num)
    textbox_update_conditional_menu box, menu()
   END IF
  END IF

  clearpage dpage
  FOR i as integer = state.top TO state.top + state.size
   textcolor uilook(uiMenuItem), 0
   IF grey(i) = YES THEN
    c = uilook(uiSelectedDisabled)
    SELECT CASE read_box_conditional_by_menu_index(box, i)
     CASE -1 ' Check tag 1=OFF always true
      c = uilook(uiHighlight)
     CASE 0, 1 ' Disabled or check tag 1=ON never true
      c = uilook(uiDisabledItem)
    END SELECT
    rectangle 0, (i - state.top) * 9, 312, 8, c, dpage
   ELSE
    IF read_box_conditional_by_menu_index(box, i) = 0 THEN textcolor uilook(uiDisabledItem), 0
   END IF
   IF i = state.pt THEN textcolor uilook(uiSelectedItem + state.tog), 0
   printstr menu(i), 0, (i - state.top) * 9, dpage
  NEXT i
  draw_fullscreen_scrollbar state, , dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_update_conditional_menu(byref box as TextBox, menu() as string)
 menu(-1) = "Go Back"
 menu(0) = textbox_condition_caption(box.instead_tag, "INSTEAD")
 SELECT CASE box.instead
  CASE 0
   menu(1) = " use [text box or script] instead"
  CASE IS < 0
   menu(1) = " run " & scriptname(-box.instead) & " instead"
  CASE IS > 0
   menu(1) = " jump to text box " & box.instead & " instead"
 END SELECT
 menu(2) = textbox_condition_caption(box.settag_tag, "SETTAG")
 menu(3) = tag_set_caption(box.settag1, " set tag")
 menu(4) = tag_set_caption(box.settag2, " set tag")
 menu(5) = textbox_condition_caption(box.money_tag, "MONEY")
 IF box.money < 0 THEN
  menu(6) = " lose " & ABS(box.money) & "$"
 ELSE
  menu(6) = " gain " & ABS(box.money) & "$"
 END IF
 menu(7) = textbox_condition_caption(box.battle_tag, "BATTLE")
 menu(8) = " fight enemy formation " & box.battle
 menu(9) = textbox_condition_caption(box.item_tag, "ITEM")
 SELECT CASE box.item
  CASE 0 :      menu(10) = " do not add/remove items"
  CASE IS > 0 : menu(10) = " add one " & load_item_name(ABS(box.item), 0, 0)
  CASE IS < 0 : menu(10) = " remove one " & load_item_name(ABS(box.item), 0, 0)
 END SELECT
 menu(11) = textbox_condition_caption(box.shop_tag, "SHOP")
 SELECT CASE box.shop
  CASE IS > 0 : menu(12) = " go to shop " & box.shop - 1 & " " & readshopname(box.shop - 1)
  CASE IS < 0 : menu(12) = " go to an Inn that costs " & -box.shop & "$"
  CASE 0 :      menu(12) = " restore Hp and Mp [select shop here]"
 END SELECT
 menu(13) = textbox_condition_caption(box.hero_tag, "HEROES")
 SELECT CASE box.hero_addrem
  CASE 0 :      menu(14) = " do not add/remove heros"
  CASE IS > 0 : menu(14) = " add " & getheroname(ABS(box.hero_addrem) - 1) & " to party"
  CASE IS < 0 : menu(14) = " remove " & getheroname(ABS(box.hero_addrem) - 1) & " from party"
 END SELECT
 SELECT CASE box.hero_swap
  CASE 0 :      menu(15) = " do not swap in/out heros"
  CASE IS > 0 : menu(15) = " swap in " & getheroname(ABS(box.hero_swap) - 1)
  CASE IS < 0 : menu(15) = " swap out " & getheroname(ABS(box.hero_swap) - 1)
 END SELECT
 SELECT CASE box.hero_lock
  CASE 0 :      menu(16) = " do not unlock/lock heros"
  CASE IS > 0 : menu(16) = " unlock " & getheroname(ABS(box.hero_lock) - 1)
  CASE IS < 0 : menu(16) = " lock " & getheroname(ABS(box.hero_lock) - 1)
 END SELECT
 menu(17) = textbox_condition_caption(box.door_tag, "DOOR")
 menu(18) = " instantly use door " & box.door
 menu(19) = textbox_condition_caption(box.menu_tag, "MENU")
 menu(20) = " open menu " & box.menu & " " & getmenuname(box.menu)
 menu(21) = textbox_condition_caption(box.after_tag, "AFTER")
 SELECT CASE box.after
  CASE 0 :      menu(22) = " use [text box or script] next"
  CASE IS < 0 : menu(22) = " run " & scriptname(-box.after) & " next"
  CASE IS > 0 : menu(22) = " jump to text box " & box.after & " next"
 END SELECT
END SUB

SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, override_y as integer=-1, suppress_text as integer=NO)
 DIM ypos as integer
 IF override_y >= 0 THEN
  ypos = override_y
 ELSE
  ypos = 4 + box.vertical_offset * 4
 END IF
 IF box.no_box = NO THEN
  edgeboxstyle 4, ypos, 312, get_text_box_height(box), box.boxstyle, dpage, (box.opaque = NO)
 END IF
 IF suppress_text = NO THEN
  DIM col as integer
  DIM i as integer
  FOR i as integer = 0 TO 7
   col = uilook(uiText)
   IF box.textcolor > 0 THEN col = box.textcolor
   edgeprint box.text(i), 8, 3 + ypos + i * 10, col, dpage
  NEXT i
 END IF
 IF box.portrait_box THEN
  edgeboxstyle 4 + box.portrait_pos.x, ypos  + box.portrait_pos.y, 50, 50, box.boxstyle, dpage, YES
 END IF
 WITH st.portrait
  IF .sprite THEN frame_draw .sprite, .pal, 4 + box.portrait_pos.x, ypos + box.portrait_pos.y,,,dpage
 END WITH
END SUB

SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState, menu() as string)
 LoadTextBox box, st.id
 update_textbox_editor_main_menu box, menu()
 load_text_box_portrait box, st.portrait
END SUB

SUB update_textbox_editor_main_menu (byref box as TextBox, menu() as string)
 IF box.after = 0 THEN
  box.after_tag = 0
 ELSE
  IF box.after_tag = 0 THEN box.after_tag = -1 ' Set "After" text box conditional to "Always"
 END IF
 SELECT CASE box.after_tag
  CASE 0
   menu(6) = "Next: None Selected"
  CASE -1
   IF box.after >= 0 THEN
    menu(6) = "Next: Box " & box.after
   ELSE
    menu(6) = "Next: script " & scriptname(ABS(box.after))
   END IF
  CASE ELSE
   IF box.after >= 0 THEN
    menu(6) = "Next: Box " & box.after & " (conditional)"
   ELSE
    menu(6) = "Next: script " & scriptname(ABS(box.after)) & " (conditional)"
   END IF
 END SELECT
END SUB

FUNCTION textbox_condition_caption(tag as integer, prefix as string = "") as string
 DIM prefix2 as string
 IF LEN(prefix) > 0 THEN prefix2 = prefix & ": "
 IF tag = 0 THEN RETURN prefix2 & "Never do the following"
 IF tag = 1 THEN RETURN prefix2 & "If tag 1 = ON [Never]"
 IF tag = -1 THEN RETURN prefix2 & "Always do the following"
 RETURN prefix2 & "If tag " & ABS(tag) & " = " + onoroff(tag) & " (" & load_tag_name(tag) & ")"
END FUNCTION

FUNCTION textbox_condition_short_caption(tag as integer) as string
 IF tag = 0 THEN RETURN "NEVER"
 IF tag = 1 THEN RETURN "NEVER(1)"
 IF tag = -1 THEN RETURN "ALWAYS"
 RETURN "IF TAG " & ABS(tag) & "=" + UCASE(onoroff(tag))
END FUNCTION

SUB writeconstant (byval filehandle as integer, byval num as integer, names as string, unique() as string, prefix as string)
 'prints a hamsterspeak constant to already-open filehandle
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

SUB write_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer, num as integer)
 WITH box
  SELECT CASE menuindex
   CASE 0:  .instead_tag = num
   CASE 1:  .instead     = num
   CASE 2:  .settag_tag  = num
   CASE 3:  .settag1     = num
   CASE 4:  .settag2     = num
   CASE 5:  .money_tag   = num
   CASE 6:  .money       = num
   CASE 7:  .battle_tag  = num
   CASE 8:  .battle      = num
   CASE 9:  .item_tag    = num
   CASE 10: .item        = num
   CASE 11: .shop_tag    = num
   CASE 12: .shop        = num
   CASE 13: .hero_tag    = num
   CASE 14: .hero_addrem = num
   CASE 15: .hero_swap   = num
   CASE 16: .hero_lock   = num
   CASE 17: .door_tag    = num
   CASE 18: .door        = num
   CASE 19: .menu_tag    = num
   CASE 20: .menu        = num
   CASE 21: .after_tag   = num
   CASE 22: .after       = num
  END SELECT
 END WITH
END SUB

FUNCTION read_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer) as integer
 WITH box
  SELECT CASE menuindex
   CASE 0:  RETURN .instead_tag
   CASE 1:  RETURN .instead
   CASE 2:  RETURN .settag_tag
   CASE 3:  RETURN .settag1
   CASE 4:  RETURN .settag2
   CASE 5:  RETURN .money_tag
   CASE 6:  RETURN .money
   CASE 7:  RETURN .battle_tag
   CASE 8:  RETURN .battle
   CASE 9:  RETURN .item_tag
   CASE 10: RETURN .item
   CASE 11: RETURN .shop_tag
   CASE 12: RETURN .shop
   CASE 13: RETURN .hero_tag
   CASE 14: RETURN .hero_addrem
   CASE 15: RETURN .hero_swap
   CASE 16: RETURN .hero_lock
   CASE 17: RETURN .door_tag
   CASE 18: RETURN .door
   CASE 19: RETURN .menu_tag
   CASE 20: RETURN .menu
   CASE 21: RETURN .after_tag
   CASE 22: RETURN .after
  END SELECT
 END WITH
END FUNCTION

FUNCTION box_conditional_type_by_menu_index(menuindex as integer) as integer
 SELECT CASE menuindex
  CASE -1      : RETURN condEXIT
  CASE 1, 22   : RETURN condBOX
  CASE 3, 4    : RETURN condSETTAG
  CASE 6       : RETURN condMONEY
  CASE 8       : RETURN condBATTLE
  CASE 10      : RETURN condITEM
  CASE 12      : RETURN condSHOP
  CASE 14,15,16: RETURN condHERO
  CASE 18      : RETURN condDOOR
  CASE 20      : RETURN condMENU
  CASE ELSE    : RETURN condTAG
 END SELECT
END FUNCTION

SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, holdscreen as integer)
 DIM speed as integer = 1
 DIM tog as integer = 0
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_position_portrait"
  IF enter_or_space() THEN EXIT DO
  speed = 1
  IF keyval(scLeftShift) OR keyval(scRightShift) THEN speed = 10
  IF keyval(scLeft)  > 0 THEN box.portrait_pos.x -= speed
  IF keyval(scRight) > 0 THEN box.portrait_pos.x += speed
  IF keyval(scUp)    > 0 THEN box.portrait_pos.y -= speed
  IF keyval(scDown)  > 0 THEN box.portrait_pos.y += speed
  textbox_edit_preview box, st
  edgeprint "Arrow keys to move, space to confirm", 0, 190, uilook(uiSelectedItem + tog), dpage
  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
END SUB

SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM menu(17) as string
 DIM state as MenuState
 state.size = 20
 state.last = UBOUND(menu)
 state.need_update = YES
 
 'Show backdrop
 DIM holdscreen as integer
 holdscreen = allocatepage
 IF box.backdrop > 0 THEN
  loadmxs game & ".mxs", box.backdrop - 1, vpages(holdscreen)
 END IF

 DIM i as integer
 DIM col as integer

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_appearance"
  usemenu state
  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0: EXIT DO ' Exit the appearance menu
    CASE 3: box.textcolor = color_browser_256(box.textcolor)
    CASE 6: IF box.music > -1 THEN playsongnum box.music - 1
    CASE 7: box.no_box = (NOT box.no_box)
    CASE 8: box.opaque = (NOT box.opaque)
    CASE 9: box.restore_music = (NOT box.restore_music)
    CASE 16: box.stop_sound_after = (NOT box.stop_sound_after)
    CASE 17: box.backdrop_trans = (NOT box.backdrop_trans)
    CASE 12:
     IF box.portrait_type = 1 THEN
      box.portrait_pal = pal16browse(box.portrait_pal, 8, box.portrait_id)
     END IF
    CASE 13: box.portrait_box = (NOT box.portrait_box)
    CASE 14: textbox_position_portrait box, st, holdscreen
    CASE 15: IF box.sound_effect > 0 THEN playsfx box.sound_effect - 1
   END SELECT
   state.need_update = YES
  END IF
  IF keyval(scLeft) > 1 OR keyval(scRight) > 1 THEN
   SELECT CASE state.pt
    CASE 7: box.no_box = (NOT box.no_box)
    CASE 8: box.opaque = (NOT box.opaque)
    CASE 9: box.restore_music = (NOT box.restore_music)
    CASE 13: box.portrait_box = (NOT box.portrait_box)
    CASE 16: box.stop_sound_after = (NOT box.stop_sound_after)
    CASE 17: box.backdrop_trans = (NOT box.backdrop_trans)
   END SELECT
   state.need_update = YES
  END IF
  SELECT CASE state.pt
   CASE 1: state.need_update = intgrabber(box.vertical_offset, 0, 49)
   CASE 2: state.need_update = intgrabber(box.shrink, -1, 21)
   CASE 3: state.need_update = intgrabber(box.textcolor, 0, 255)
   CASE 4: state.need_update = intgrabber(box.boxstyle, 0, 14)
   CASE 5:
    IF zintgrabber(box.backdrop, -1, gen(genNumBackdrops) - 1) THEN
     state.need_update = YES
     clearpage holdscreen
     IF box.backdrop > 0 THEN
      loadmxs game & ".mxs", box.backdrop - 1, vpages(holdscreen)
     END IF
    END IF
   CASE 6:
    IF zintgrabber(box.music, -1, gen(genMaxSong)) THEN
     state.need_update = YES
     music_stop
    END IF
   CASE 10:
    state.need_update = intgrabber(box.portrait_type, 0, 3)
   CASE 11:
    SELECT CASE box.portrait_type
     CASE 1: state.need_update = intgrabber(box.portrait_id, 0, gen(genMaxPortrait))
     CASE 2: state.need_update = intgrabber(box.portrait_id, 0, 3)
     CASE 3: state.need_update = intgrabber(box.portrait_id, 0, 40)
    END SELECT
   CASE 12:
    IF box.portrait_type = 1 THEN
     state.need_update = intgrabber(box.portrait_pal, -1, gen(genMaxPal))
    END IF
   CASE 15:
    IF zintgrabber(box.sound_effect, -1, gen(genMaxSFX)) THEN
     state.need_update = YES
     resetsfx
    END IF
  END SELECT
  IF state.need_update THEN
   state.need_update = NO
   update_textbox_appearance_editor_menu menu(), box, st
  END IF

  copypage holdscreen, dpage
  textbox_edit_preview box, st
  FOR i as integer = 0 TO UBOUND(menu)
   col = uilook(uimenuItem)
   IF i = state.pt THEN col = uilook(uiSelectedItem + state.tog)
   edgeprint menu(i), 0, i * 10, col, dpage
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 freepage holdscreen
 resetsfx
 music_stop
END SUB

SUB update_textbox_appearance_editor_menu (menu() as string, byref box as TextBox, byref st as TextboxEditState)
 menu(0) = "Go Back"
 menu(1) = "Position:"
 menu(2) = "Shrink:"
 menu(3) = "Textcolor:"
 menu(4) = "Box style:"
 menu(5) = "Backdrop:"
 menu(6) = "Music:"
 menu(7) = "Show Box:"
 menu(8) = "Translucent:"
 menu(9) = "Restore Music:"
 menu(10) = "Portrait type:"
 menu(11) = "Portrait ID:"
 menu(12) = "Portrait Palette:"
 menu(13) = "Portrait Box:"
 menu(14) = "Position Portrait..."
 menu(15) = "Sound Effect:"
 menu(16) = "Stop sound after box:"
 menu(17) = "Transparent backdrop:"
 DIM menutemp as string
 DIM i as integer
 FOR i as integer = 0 TO UBOUND(menu)
  menutemp = ""
  SELECT CASE i
   CASE 1: menutemp = "" & box.vertical_offset
   CASE 2:
    IF box.shrink = -1 THEN
     menutemp = "Auto"
    ELSE
     menutemp = "" & box.shrink
    END IF
   CASE 3: menutemp = "" & box.textcolor
   CASE 4: menutemp = "" & box.boxstyle
   CASE 5: IF box.backdrop THEN menutemp = "" & box.backdrop - 1 ELSE menutemp = "NONE"
   CASE 6: IF box.music THEN menutemp = (box.music - 1) & " " & getsongname(box.music - 1) ELSE menutemp = "NONE"
   CASE 7: menutemp = yesorno(NOT box.no_box)
   CASE 8: menutemp = yesorno(NOT box.opaque)
   CASE 9: menutemp = yesorno(box.restore_music)
   CASE 10:
    SELECT CASE box.portrait_type
     CASE 0: menutemp = "NONE"
     CASE 1: menutemp = "Fixed"
     CASE 2: menutemp = "Hero (by caterpillar order)"
     CASE 3: menutemp = "Hero (by party order)"
    END SELECT
   CASE 11:
    menutemp = STR(box.portrait_id)
    SELECT CASE box.portrait_type
     CASE 0: menutemp = menutemp & " (N/A)"
     CASE 2: IF box.portrait_id = 0 THEN menutemp = menutemp & " (Leader)"
     CASE 3: IF box.portrait_id > 3 THEN menutemp = menutemp & " (Reserve)"
     CASE ELSE: menutemp = "" & box.portrait_id
    END SELECT
   CASE 12:
    menutemp = defaultint(box.portrait_pal)
    SELECT CASE box.portrait_type
     CASE 0: menutemp = menutemp & " (N/A)"
     CASE 1:
     CASE ELSE: menutemp = menutemp & " (N/A, see hero editor)"
    END SELECT
   CASE 13: menutemp = yesorno(box.portrait_box)
   CASE 14:
   CASE 15:
    IF box.sound_effect = 0 THEN
     menutemp = menutemp & "NONE"
    ELSE
     menutemp = menutemp & box.sound_effect - 1 & " " & getsfxname(box.sound_effect - 1)
    END IF
   CASE 16: menutemp = yesorno(box.stop_sound_after)
   CASE 17:
    IF box.backdrop > 0 THEN
     menutemp = yesorno(box.backdrop_trans)
    ELSE
     menutemp = "(N/A)"
    END IF
  END SELECT
  IF LEN(menutemp) THEN menutemp = " " & menutemp
  menu(i) = menu(i) & menutemp
 NEXT i
 load_text_box_portrait box, st.portrait
END SUB

SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
 SaveTextBox box, st.id
 DIM remember_id as integer = st.id
 st.id += 1
 DIM foundstr as integer = NO
 DO
  IF st.id > gen(genMaxTextBox) THEN st.id = 0
  IF st.id = remember_id THEN
   edgeboxstyle 115, 90, 100, 20, 0, vpage
   edgeprint "Not found.", 120, 95, uilook(uiText), vpage
   setvispage vpage
   waitforanykey
   EXIT DO
  END IF
  LoadTextBox box, st.id
  foundstr = NO
  FOR i as integer = 0 TO UBOUND(box.text)
   IF INSTR(UCASE(box.text(i)), UCASE(st.search)) > 0 THEN foundstr = YES
  NEXT i
  IF foundstr THEN EXIT DO
  st.id += 1
 LOOP
END SUB

SUB textbox_create_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 '--this inits a new text box, and copies in values from another box for defaults
 ClearTextBox box
 textbox_copy_style_from_box template_box_id, box, st
END SUB

SUB textbox_copy_style_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 '--copies in styles values from another box for defaults
 DIM boxcopier as TextBox
 LoadTextBox boxcopier, template_box_id
 WITH box
  .no_box          = boxcopier.no_box
  .opaque          = boxcopier.opaque
  .restore_music   = boxcopier.restore_music
  .portrait_box    = boxcopier.portrait_box
  .vertical_offset = boxcopier.vertical_offset
  .shrink          = boxcopier.shrink
  .textcolor       = boxcopier.textcolor
  .boxstyle        = boxcopier.boxstyle
  .portrait_type   = boxcopier.portrait_type
  .portrait_id     = boxcopier.portrait_id
  .portrait_pal    = boxcopier.portrait_pal
  .portrait_pos    = boxcopier.portrait_pos
  .sound_effect    = boxcopier.sound_effect
  .stop_sound_after= boxcopier.stop_sound_after
 END WITH
 SaveTextBox box, st.id
END SUB

SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM state as MenuState
 WITH state
  state.size = 22
  state.last = UBOUND(box.text)
 END WITH
 DIM insertpt as integer = -1
 setkeys YES
 DO
  setwait 55
  setkeys YES
  state.tog = state.tog XOR 1
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_line_editor"
  IF keyval(scEnter) > 1 AND state.pt < state.last THEN state.pt += 1
  IF usemenu(state) THEN insertpt = -1
  IF state.pt <= state.last AND state.pt >= state.first THEN
   stredit box.text(state.pt), insertpt, 38
  END IF

  'Display the box
  clearpage dpage
  textbox_edit_preview box, st, 4, YES
  'Display the lines in the box
  FOR i as integer = state.first TO state.last
   textcolor uilook(uiText), 0
   IF box.textcolor > 0 THEN textcolor box.textcolor, 0
   IF state.pt = i THEN
    textcolor uilook(uiText), uilook(uiHighlight + state.tog)
    printstr " ", 8 + insertpt * 8, 8 + i * 10, dpage
    textcolor uilook(uiSelectedItem + state.tog), 0
   END IF
   printstr box.text(i), 8, 8 + i * 10, dpage
  NEXT i
  textcolor uilook(uiSelectedItem + state.tog), 0
  printstr "-", 0, 8 + state.pt * 10, dpage
  textcolor uilook(uiText), 0
  printstr "Text Box " & st.id, 0, 100, dpage
  printstr "${C0} = Leader's name", 0, 120, dpage
  printstr "${C#} = Hero name at caterpillar slot #", 0, 128, dpage
  printstr "${P#} = Hero name at party slot #", 0, 136, dpage
  printstr "${H#} = Name of hero ID #", 0, 144, dpage
  printstr "${V#} = Global Plotscript Variable ID #", 0, 152, dpage
  printstr "${S#} = Insert String Variable with ID #", 0, 160, dpage
  printstr "CTRL+SPACE: choose an extended character", 0, 176, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_choice_editor (byref box as TextBox, byref st as TextboxEditState)
 'tchoice:
 DIM state as MenuState
 WITH state
  .last = 5
  .size = 24
 END WITH
 DIM menu(5) as string
 menu(0) = "Go Back"
 setkeys YES
 DO
  setwait 55
  setkeys YES
  state.tog = state.tog XOR 1
  IF keyval(scEsc) > 1 THEN EXIT SUB
  IF keyval(scF1) > 1 THEN show_help "textbox_choice_editor"
  usemenu state
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT SUB
   IF state.pt = 1 THEN box.choice_enabled = (NOT box.choice_enabled)
  END IF
  IF state.pt = 1 THEN
   IF keyval(scLeft) > 1 OR keyval(scRight) > 1 THEN box.choice_enabled = (NOT box.choice_enabled)
  END IF
  FOR i as integer = 0 TO 1
   IF state.pt = 2 + (i * 2) THEN strgrabber box.choice(i), 15
   IF state.pt = 3 + (i * 2) THEN tag_grabber box.choice_tag(i), , , NO
  NEXT i
  IF box.choice_enabled THEN menu(1) = "Choice = Enabled" ELSE menu(1) = "Choice = Disabled"
  FOR i as integer = 0 TO 1
   menu(2 + (i * 2)) = "Option " & i & " text:" + box.choice(i)
   menu(3 + (i * 2)) = tag_set_caption(box.choice_tag(i))
  NEXT i
 
  clearpage dpage
  standardmenu menu(), state, 0, 8, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState, menu() as string)
'FIXME: menu() should be moved to become a member of st, then we wouldn't have to pass it around
 SaveTextBox box, st.id
 DIM do_search as integer = YES
 REDIM prev(5) as TextboxConnectNode
 DIM current as TextboxConnectNode
 REDIM nxt(2) as TextboxConnectNode

 DIM column as integer = 1
 DIM col_limit_left as integer = 1
 DIM col_limit_right as integer = 1
 
 DIM state as MenuState
 state.size = 6
 DIM nxt_state as MenuState
 nxt_state.size = 2
 DIM searchbox as TextBox
 
 DIM nxt_add_type as integer
 DIM remember_insert as integer
 DIM remember_insert_tag as integer
 
 DIM y as integer

 setkeys
 DO
  setwait 55
  setkeys
  IF do_search THEN
   column = 1
   col_limit_left = 1
   col_limit_right = 2
   '--Current box
   textbox_connection_captions current, st.id, 0, box, "BOX", NO
   '--Next boxes
   nxt_state.last = -1
   nxt_state.pt = 0
   nxt_state.top = 0
   REDIM nxt(2) as TextboxConnectNode
   IF box.instead_tag <> 0 THEN
    nxt_state.last += 1
    LoadTextBox searchbox, box.instead
    textbox_connection_captions nxt(nxt_state.last), box.instead, box.instead_tag, searchbox, "INSTEAD"
   END IF
   IF box.after_tag <> 0 THEN
    nxt_state.last += 1
    LoadTextBox searchbox, box.after
    textbox_connection_captions nxt(nxt_state.last), box.after, box.after_tag, searchbox, "AFTER"
   END IF
   nxt_state.last += 1
   WITH nxt(nxt_state.last)
    .add = YES
    .lines(0) = " INSERT A"
    .lines(1) = " NEW BOX"
    .style = 1
   END WITH
   '--Previous boxes
   state.last = -1
   state.pt = 0
   state.top = 0
   FOR i as integer = 0 TO gen(genMaxTextBox)
    LoadTextBox searchbox, i
    WITH searchbox
     IF .instead = st.id AND .instead_tag <> 0 THEN
      state.last += 1
      IF UBOUND(prev) < state.last THEN
       REDIM PRESERVE prev(UBOUND(prev) + 10)
      END IF
      textbox_connection_captions prev(state.last), i, .instead_tag, searchbox, "REPLACES"
      col_limit_left = 0
     END IF
     IF .after = st.id AND .after_tag <> 0 THEN
      state.last += 1
      IF UBOUND(prev) < state.last THEN
       REDIM PRESERVE prev(UBOUND(prev) + 10)
      END IF
      textbox_connection_captions prev(state.last), i, .after_tag, searchbox, "BEFORE"
      col_limit_left = 0
     END IF
    END WITH
   NEXT i
   do_search = NO
  END IF
  IF keyval(scEsc) > 1 THEN EXIT SUB
  IF keyval(scF1) > 1 THEN show_help "textbox_connections"
  '--Horizontal column navigation
  IF keyval(scLeft) > 1 THEN column = loopvar(column, col_limit_left, col_limit_right, -1)
  IF keyval(scRight) > 1 THEN column = loopvar(column, col_limit_left, col_limit_right, 1)
  '--Vertical navigation within selected column
  SELECT CASE column
   CASE 0 'Previous
    usemenu state
    IF enter_or_space() THEN
     IF prev(state.pt).id >= 0 THEN
      st.id = prev(state.pt).id
      textbox_edit_load box, st, menu()
      do_search = YES
     END IF
    END IF
   CASE 1 'Current
    IF enter_or_space() THEN EXIT SUB
   CASE 2 'Next
    usemenu nxt_state
    IF enter_or_space() THEN
     IF nxt(nxt_state.pt).add THEN
      'Add a box
      nxt_add_type = twochoice("Add a new box?", "After this box", "Instead of this box", 0, -1)
      IF nxt_add_type >= 0 THEN
       '--Add a box
       gen(genMaxTextbox) += 1
       IF nxt_add_type = 0 THEN
        '--an after box
        remember_insert = box.after
        remember_insert_tag = box.after_tag
        box.after_tag = -1
        box.after = gen(genMaxTextbox)
       ELSEIF nxt_add_type = 1 THEN
        '--an instead box
        remember_insert = box.instead
        remember_insert_tag = box.instead_tag
        box.instead_tag = -1
        box.instead = gen(genMaxTextbox)
       END IF
       SaveTextBox box, st.id
       st.id = gen(genMaxTextBox)
       textbox_create_from_box 0, box, st
       'Having added the new box, now we insert it into the existing chain
       IF nxt_add_type = 0 THEN
        box.after = remember_insert
        box.after_tag = remember_insert_tag
       ELSEIF nxt_add_type = 1 THEN
        box.instead = remember_insert
        box.instead_tag = remember_insert_tag
       END IF
       SaveTextBox box, st.id
       textbox_edit_load box, st, menu()
       do_search = YES
      END IF
     ELSE
      'Navigate to a box
      IF nxt(nxt_state.pt).id >= 0 THEN
       st.id = nxt(nxt_state.pt).id
       textbox_edit_load box, st, menu()
       do_search = YES
      END IF
     END IF
    END IF
  END SELECT

  '--Draw box preview
  clearpage dpage
  textbox_edit_preview box, st, 96
  '--Draw previous
  IF state.last >= 0 THEN
   FOR i as integer = state.top TO small(state.last, state.top + state.size)
    y = (i - state.top) * 25
    textbox_connection_draw_node prev(i), 0, y, (column = 0 AND state.pt = i)
   NEXT i
  ELSE
   edgeprint "No Previous", 0, 0, uilook(uiMenuItem), dpage
  END IF
  '--Draw current
  y = 10 * large(nxt_state.last, 0)
  textbox_connection_draw_node current, 106, y, (column = 1)
  '--Draw next
  IF nxt_state.last >= 0 THEN
   FOR i as integer = nxt_state.top TO small(nxt_state.last, nxt_state.top + nxt_state.size)
    y = (i - nxt_state.top) * 25
    textbox_connection_draw_node nxt(i), 212, y, (column = 2 AND nxt_state.pt = i)
   NEXT i
  ELSE
   edgeprint "No Next", 212, 0, uilook(uiMenuItem), dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_connection_captions(byref node as TextboxConnectNode, id as integer, tag as integer, box as TextBox, topcation as string, use_tag as integer = YES)
 DIM preview_line as string
 IF id >= 0 THEN
  node.lines(0) = topcation & " " & id
  preview_line = textbox_preview_line(box)
  IF use_tag THEN
   node.lines(1) = textbox_condition_short_caption(tag)
   node.lines(2) = LEFT(textbox_preview_line(box), 13)
  ELSE
   node.lines(1) = LEFT(preview_line, 13)
   node.lines(2) = MID(preview_line, 13, 13)
  END IF
 ELSE
  node.lines(0) = topcation & " " & "SCRIPT"
  preview_line = scriptname(ABS(id))
  node.lines(1) = LEFT(preview_line, 13)
  node.lines(2) = MID(preview_line, 13, 13)
 END IF
 node.id = id
END SUB

SUB textbox_connection_draw_node(byref node as TextboxConnectNode, x as integer, y as integer, selected as integer)
 STATIC tog as integer = 0
 edgeboxstyle x, y, 104, 26, node.style, dpage, (NOT selected), YES
 textcolor uilook(uiMenuItem), 0
 IF selected THEN
  textcolor uilook(uiSelectedItem + tog), 0
  tog = tog XOR 1
 END IF
 FOR i as integer = 0 TO 2
  printstr node.lines(i), x + 1, y + i * 8 + 1, dpage
 NEXT i
END SUB
