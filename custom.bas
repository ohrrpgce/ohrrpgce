'OHRRPGCE CUSTOM - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "config.bi"
#include "datetime.bi"  'for date serials
#include "string.bi"  'for date serials
#include "ver.txt"
#include "udts.bi"
#include "const.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "flexmenu.bi"
#include "slices.bi"
#include "cglobals.bi"
#include "uiconst.bi"
#include "scrconst.bi"
#include "sliceedit.bi"
#include "reloadedit.bi"
#include "editedit.bi"
#include "os.bi"
#include "distribmenu.bi"
#include "thingbrowser.bi"
#include "plankmenu.bi"
#include "custom.bi"

#IFDEF USE_RASTERIZER
 #include "matrixMath.bi"
#ENDIF


'''' Local function and type declarations

' Stores information about a previous or ongoing Custom editing session
TYPE SessionInfo
 workingdir as string              'The directory containing this session's files
 partial_rpg as bool               '__danger.tmp exists: was in process of unlumping or deleting lumps
 fresh_danger_tmp as bool          '__danger.tmp exists and isn't stale: may still be running
 info_file_exists as bool          'session_info.txt.tmp exists. If not, everything in this UDT below this point is unknown.
 pid as integer                    'Process ID or 0
 running as bool                   'That process is still running
 sourcerpg as string               'May be blank
 'The following are represented as native FB DateSerials, not Unix mtimes. 0.0 means N/A
 sourcerpg_old_mtime as double     'mtime of the sourcerpg when was opened/last saved by that copy of Custom
 sourcerpg_current_mtime as double 'mtime of the sourcerpg right now, as seen by us
 session_start_time as double      'When the game was last unlumped/saved (or if none, when Custom was launched)
 last_lump_mtime as double         'mtime of the most recently modified lump
END TYPE

DECLARE FUNCTION newRPGfile (templatefile as string, newrpg as string) as bool
DECLARE SUB setup_workingdir ()
DECLARE SUB check_for_crashed_workingdirs ()
DECLARE FUNCTION check_a_crashed_workingdir (sessinfo as SessionInfo) as bool
DECLARE FUNCTION empty_workingdir (workdir as string) as bool
DECLARE FUNCTION handle_dirty_workingdir (sessinfo as SessionInfo) as bool
DECLARE FUNCTION check_ok_to_open (filename as string) as bool
DECLARE FUNCTION get_previous_session_info (workdir as string) as SessionInfo

DECLARE SUB secret_menu ()
DECLARE SUB condition_test_menu ()
DECLARE SUB quad_transforms_menu ()
DECLARE SUB rotozoom_tests ()
DECLARE SUB text_test_menu ()
DECLARE SUB new_graphics_tests ()
DECLARE SUB plankmenu_cursor_move_tests
DECLARE SUB HTTP_demo()
DECLARE SUB CreateProcess_tests()

DECLARE SUB cleanup_and_terminate (show_quit_msg as bool = YES, retval as integer = 0)
DECLARE SUB import_scripts_and_terminate (scriptfile as string)
DECLARE SUB export_translations_and_terminate (translationfile as string)

DECLARE SUB prompt_for_password()
DECLARE SUB prompt_for_save_and_quit()
DECLARE SUB choose_rpg_to_open (rpg_browse_default as string)
DECLARE SUB main_editor_menu()
DECLARE SUB gfx_editor_menu()


'=================================== Globals ==================================

DIM activepalette as integer = -1
'The following are set from commandline options
DIM auto_distrib as string 'Which distribution option to package automatically
DIM option_nowait as bool  'Currently only used when importing scripts from the commandline: don't wait
DIM export_translations_to as string

DIM editing_a_game as bool
DIM last_active_seconds as double

DIM slave_channel as IPCChannel = NULL_CHANNEL
DIM slave_process as ProcessHandle = 0

'Should we delete workingdir when quitting normally?
'False if relumping workingdir failed.
DIM cleanup_workingdir_on_exit as bool = YES

'Affects show/fatalerror: have we started editing (ie. finished upgrades and other startup)?
'If not, we should cleanup working.tmp instead of preserving it
DIM cleanup_workingdir_on_error as bool = YES

'======================== Setup directories & debug log =======================
' This is almost identical to startup code in Game; please don't unnecessarily diverge.

orig_dir = CURDIR
'Note: debug log messages go in CURDIR until log_dir set below

set_app_dir

'temporarily set current directory, will be changed to game directory later if writable
'(This is where new .rpg files go by default)
'(This change in working directory is done only by Custom, not Game)
IF diriswriteable(app_dir) THEN
 'When CUSTOM is installed read-write, work in CUSTOM's folder
 CHDIR app_dir
ELSE
 'If CUSTOM is installed read-only, use your Documents dir as the default
 '(On Mac, this will also happen due to Gatekeeper Path Randomization (aka App Translocation))
 CHDIR get_documents_dir()
END IF

#IFDEF __FB_ANDROID__
 'Prevent log_dir from being changed to the .rpg directory
 '(But why? If it's on external storage, that seems like great place to put it)
 log_dir = orig_dir & SLASH
 overrode_log_dir = YES
#ELSE
 log_dir = CURDIR & SLASH
#ENDIF

'Once log_dir is set, can create debug log.
start_new_debug "Starting OHRRPGCE Custom"
debuginfo DATE & " " & TIME
debuginfo long_version & build_info
debuginfo "exepath: " & EXEPATH & ", exe: " & COMMAND(0)
debuginfo "orig_dir: " & orig_dir
debuginfo "curdir: " & CURDIR
' Load these three strings with info collectable before backend initialisation
read_backend_info()
debuginfo "Runtime info: " & gfxbackendinfo & "  " & musicbackendinfo & "  " & systeminfo

settings_dir = get_settings_dir()
documents_dir = get_documents_dir()  'may depend on app_dir
debuginfo "documents_dir: " & documents_dir
'FIXME: why do we use different temp dirs in game and custom?
'Plus, tmpdir is shared between all running copies of Custom, which could cause problems.
tmpdir = settings_dir & SLASH
IF NOT isdir(tmpdir) THEN fatalerror "Unable to create temp directory " & tmpdir

set_global_config_file
config_prefix = "edit."


'========================== Process commandline flags =========================

'Read the default backend from config first so that --gfx overrides it.
'And if it's missing, default to SDL, for resizable windows
prefer_gfx_backend "sdl"
DIM backend as string = read_config_str("gfx.backend")
IF LEN(backend) THEN prefer_gfx_backend backend

REDIM cmdline_args() as string
' This can modify log_dir and restart the debug log
processcommandline cmdline_args(), @gamecustom_setoption, orig_dir & SLASH & "ohrrpgce_arguments.txt"


'======================= Initialise backends/graphics =========================

load_default_master_palette master()
DefaultUIColors uilook(), boxlook()
getdefaultfont current_font()

setmodex
setpal master()
setfont current_font()
textcolor uilook(uiText), 0

setwindowtitle "O.H.R.RPG.C.E"
showmousecursor
unlock_resolution 320, 200   'Minimum window size

debuginfo musicbackendinfo  'Preliminary info before initialising backend
setupmusic

'Cleanups/recovers any working.tmp for any crashed copies of Custom; requires graphics up and running
check_for_crashed_workingdirs

'This also calls write_session_info
setup_workingdir


'=============================== Select a game ================================

DIM scriptfile as string
DIM rpg_browse_default as string

FOR i as integer = 0 TO UBOUND(cmdline_args)
 DIM arg as string
 arg = simplify_path(absolute_with_orig_path(cmdline_args(i)))
 DIM extn as string = LCASE(justextension(arg))

 IF (extn = "hs" OR extn = "hss" OR extn = "txt") AND isfile(arg) THEN
  scriptfile = arg
  CONTINUE FOR
 ELSEIF extn = "rpg" AND isfile(arg) THEN
  sourcerpg = arg
  game = trimextension(trimpath(sourcerpg))
 ELSEIF isdir(arg) THEN
  IF isfile(arg + SLASH + "archinym.lmp") THEN 'ok, accept it
   sourcerpg = trim_trailing_slashes(arg)
   game = trimextension(trimpath(sourcerpg))
  ELSE
   rpg_browse_default = arg
  END IF
 ELSE
  visible_debug !"File not found/invalid option:\n" & cmdline_args(i)
 END IF
NEXT
IF game = "" THEN
 scriptfile = ""
 choose_rpg_to_open(rpg_browse_default)
END IF

sourcerpg = absolute_path(sourcerpg)

IF check_ok_to_open(sourcerpg) = NO THEN
 cleanup_and_terminate NO
END IF

write_session_info

DIM dir_to_change_into as string = trimfilename(sourcerpg)

end_debug
IF dir_to_change_into <> "" ANDALSO diriswriteable(dir_to_change_into) THEN
 CHDIR dir_to_change_into
 IF overrode_log_dir = NO THEN log_dir = dir_to_change_into & SLASH
END IF
'otherwise, keep current directory as it was (FIXME: ideally would now be the same as in Game)
'Final log_dir set, no more need to remember.
remember_debug_messages = NO

start_new_debug "Loading a game"
debuginfo DATE & " " & TIME
debuginfo "curdir: " & CURDIR
debuginfo "tmpdir: " & tmpdir
debuginfo "settings_dir: " & settings_dir

' Local config file overrides global one
' TODO: only settings actually present in the file should override ones in the global file!
DIM tmpstr as string = trimfilename(sourcerpg) & SLASH & "ohrrpgce_config.ini"
IF isfile(tmpstr) THEN
 global_config_file = tmpstr
END IF
DIM game_id as string = trimpath(trimextension(sourcerpg))  'some unique ID scheme would be nice
config_prefix = "edit.game_" & game_id & "."
flush_gfx_config_settings

'============================= Unlump, Upgrade, Load ==========================

'Start counting edit_time from now
active_seconds = 0.
idle_time_threshold = large(read_config_int("idle_time", 30), 1)

'For getdisplayname
copylump sourcerpg, "archinym.lmp", workingdir, YES

debuginfo "Editing game " & sourcerpg & " (" & getdisplayname(" ") & ")"
setwindowtitle "O.H.R.RPG.C.E - " + trimpath(sourcerpg)

'--set game according to the archinym
copylump sourcerpg, "archinym.lmp", workingdir, YES
copylump sourcerpg, "*.gen", workingdir, YES
DIM archinym as string
archinym = readarchinym(workingdir, sourcerpg)
game = workingdir + SLASH + archinym

copylump sourcerpg, archinym + ".gen", workingdir
xbload game + ".gen", gen(), "general data is missing: RPG file appears to be corrupt"

IF gen(genVersion) > CURRENT_RPG_VERSION THEN
 debug "genVersion = " & gen(genVersion)
 future_rpg_warning
END IF

prompt_for_password

clearpage vpage
textcolor uilook(uiText), 0
printstr "UNLUMPING DATA: please wait.", 0, 0, vpage
setvispage vpage, NO

touchfile workingdir + SLASH + "__danger.tmp"
IF isdir(sourcerpg) THEN
 'work on an unlumped RPG file. Don't take hidden files
 'Convert to lowercase while copying (only needed for ancient unlumped games)
 copyfiles sourcerpg, workingdir, , YES
ELSE
 unlump sourcerpg, workingdir + SLASH
END IF
safekill workingdir + SLASH + "__danger.tmp"

'Perform additional checks for future rpg files or corruption
rpg_sanity_checks

'upgrade obsolete RPG files
upgrade YES

set_music_volume 0.01 * gen(genMusicVolume)
set_global_sfx_volume 0.01 * gen(genSFXVolume)

'Unload any default graphics (from data/defaultgfx) that might have been cached
sprite_empty_cache
palette16_empty_cache

'Load the game's palette, uicolors, font
activepalette = gen(genMasterPal)
loadpalette master(), activepalette
setpal master()
LoadUIColors uilook(), boxlook(), activepalette
clearpage dpage
clearpage vpage
xbload game + ".fnt", current_font(), "Font not loaded"
setfont current_font()

loadglobalstrings
getstatnames statnames()
load_special_tag_caches
load_script_triggers_and_names

IF scriptfile <> "" THEN import_scripts_and_terminate scriptfile

'Set by --export-trans
IF export_translations_to <> "" THEN export_translations_and_terminate export_translations_to

IF auto_distrib <> "" THEN
 auto_export_distribs auto_distrib
 cleanup_workingdir_on_exit = YES
 cleanup_and_terminate NO
END IF

'Reset start of session to after upgrades (to see which lumps are edited)
write_session_info

'From here on, preserve working.tmp if something goes wrong
cleanup_workingdir_on_error = NO

'debuginfo "mem usage " & memory_usage_string()

IF isdir(CURDIR & SLASH & "import") THEN set_browse_default(CURDIR & SLASH & "import")

editing_a_game = YES
main_editor_menu
'Execution ends inside main_editor_menu

'=======================================================================

SUB main_editor_menu()
 REDIM menu(20) as string
 DIM menu_display(UBOUND(menu)) as string

 menu(0) = "Edit Graphics"
 menu(1) = "Edit Maps"
 menu(2) = "Edit Heroes"
 menu(3) = "Edit Enemies"
 menu(4) = "Edit Attacks"
 menu(5) = "Edit Battle Formations"
 menu(6) = "Edit Items"
 menu(7) = "Edit Shops"
 menu(8) = "Edit Text Boxes"
 menu(9) = "Edit Tag Names"
 menu(10) = "Edit Menus"
 menu(11) = "Edit Slice Collections"
 menu(12) = "Edit Vehicles"
 menu(13) = "Import Music"
 menu(14) = "Import Sound Effects"
 menu(15) = "Edit Global Text Strings"
 menu(16) = "Edit General Game Settings"
 menu(17) = "Script Management"
 menu(18) = "Distribute Game"
 #IFDEF __FB_ANDROID__
  menu(19) = "Quit or Save"
  REDIM PRESERVE menu(19)
 #ELSE
  menu(19) = "Test Game"
  menu(20) = "Quit or Save"
 #ENDIF

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.last = UBOUND(menu)
 state.autosize = YES
 state.autosize_ignore_pixels = 24
 DIM menuopts as MenuOptions
 menuopts.scrollbar = YES
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  usemenu state
  IF keyval(ccCancel) > 1 THEN
   prompt_for_save_and_quit
  END IF
  IF keyval(scF1) > 1 THEN
   show_help "main"
  END IF

  IF keyval(scF5) > 1 THEN   'Redundant, but for people with muscle memory
   reimport_previous_scripts
  END IF

  IF select_by_typing(selectst) THEN
   IF RIGHT(selectst.buffer, 4) = "spam" THEN
    select_clear selectst
    secret_menu
   ELSE
    select_on_word_boundary_excluding menu(), selectst, state, "edit"
   END IF
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN gfx_editor_menu
   IF state.pt = 1 THEN map_picker
   IF state.pt = 2 THEN hero_editor_main
   IF state.pt = 3 THEN enemy_editor_main
   IF state.pt = 4 THEN attack_editor_main
   IF state.pt = 5 THEN formation_editor
   IF state.pt = 6 THEN item_editor
   IF state.pt = 7 THEN shop_editor_main
   IF state.pt = 8 THEN textbox_editor_main
   IF state.pt = 9 THEN tags_menu
   IF state.pt = 10 THEN menu_editor
   IF state.pt = 11 THEN slice_editor SL_COLLECT_USERDEFINED
   IF state.pt = 12 THEN vehicle_editor
   IF state.pt = 13 THEN song_editor_main
   IF state.pt = 14 THEN sfx_editor_main
   IF state.pt = 15 THEN global_text_strings_editor
   IF state.pt = 16 THEN general_data_editor
   IF state.pt = 17 THEN scriptman
   IF state.pt = 18 THEN distribute_game
   #IFDEF __FB_ANDROID__
    IF state.pt = 19 THEN prompt_for_save_and_quit
   #ELSE
    IF state.pt = 19 THEN spawn_game_menu(keyval(scShift) > 0, keyval(scCtrl) > 0)
    IF state.pt = 20 THEN prompt_for_save_and_quit
   #ENDIF
   '--always resave .GEN and general.reld after any menu
   '(I don't know whether saving GEN is necessary, but saving general.reld
   'is just in case we forget wherever it should have been saved)
   xbsave game + ".gen", gen(), 1000
   write_general_reld()
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage, menuopts

  textcolor uilook(uiSelectedDisabled), 0
  printstr version_code, 0, pBottom - 16, dpage
  printstr version_build & " In use: " & gfxbackend & "/" & musicbackend, 0, pBottom - 8, dpage
  textcolor uilook(uiText), 0
  printstr "Press F1 for help on any menu!", 0, pBottom, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB gfx_editor_menu()
 DIM menu(15) as string
 DIM menu_display(UBOUND(menu)) as string

 menu(0) = "Return to Main Menu"
 menu(1) = "Edit Tilesets"
 menu(2) = "Import/Export Tilesets"
 menu(3) = "Draw Walkabout Graphics"
 menu(4) = "Draw Hero Battle Graphics"
 menu(5) = "Draw Small Enemy Graphics  34x34"
 menu(6) = "Draw Medium Enemy Graphics 50x50"
 menu(7) = "Draw Big Enemy Graphics    80x80"
 menu(8) = "Draw Attacks"
 menu(9) = "Draw Weapons"
 menu(10) = "Draw Box Edges"
 menu(11) = "Draw Portraits"
 menu(12) = "Import/Export Backdrops"
 menu(13) = "Change User-Interface Colors"
 menu(14) = "Change Box Styles"
 menu(15) = "Edit Font"

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN
   show_help "gfxmain"
  END IF
  usemenu state

  IF select_by_typing(selectst) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN
    EXIT DO
   END IF
   IF state.pt = 1 THEN maptile
   IF state.pt = 2 THEN
    gen(genMaxTile) = gen(genMaxTile) + 1
    'Tilesets still use the old mxs backdrop browser
    importmxs ".til", "tileset", gen(genMaxTile), sprTypeTileset
    gen(genMaxTile) = gen(genMaxTile) - 1
   END IF
   IF state.pt = 3 THEN spriteset_editor sprTypeWalkabout
   IF state.pt = 4 THEN spriteset_editor sprTypeHero
   IF state.pt = 5 THEN spriteset_editor sprTypeSmallEnemy
   IF state.pt = 6 THEN spriteset_editor sprTypeMediumEnemy
   IF state.pt = 7 THEN spriteset_editor sprTypeLargeEnemy
   IF state.pt = 8 THEN spriteset_editor sprTypeAttack
   IF state.pt = 9 THEN spriteset_editor sprTypeWeapon
   IF state.pt = 10 THEN spriteset_editor sprTypeBoxBorder
   IF state.pt = 11 THEN spriteset_editor sprTypePortrait
   IF state.pt = 12 THEN backdrop_browser
   IF state.pt = 13 THEN ui_color_editor(activepalette)
   IF state.pt = 14 THEN ui_boxstyle_editor(activepalette)
   IF state.pt = 15 THEN font_editor current_font()
   '--always resave the .GEN lump after any menu
   xbsave game + ".gen", gen(), 1000
  END IF
 
  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

END SUB

SUB choose_rpg_to_open (rpg_browse_default as string)
 'This sub sets the globals: game and sourcerpg

 DIM state as MenuState
 state.pt = 1
 state.last = 2
 state.size = 20

 DIM root as Slice ptr
 root = NewSliceOfType(slContainer)
 SliceLoadFromFile root, finddatafile("choose_rpg.slice")
 
 DIM chooserpg_menu(2) as string
 chooserpg_menu(0) = "CREATE NEW GAME"
 chooserpg_menu(1) = "LOAD EXISTING GAME"
 chooserpg_menu(2) = "EXIT PROGRAM"
 DIM opts as MenuOptions
 opts.edged = YES

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN cleanup_and_terminate
  IF keyval(scF1) > 1 THEN show_help "choose_rpg"
  IF keyval(scF6) > 1 THEN slice_editor root, SL_COLLECT_EDITOR, "choose_rpg.slice"

  DIM menusl as Slice ptr = LookupSliceSafe(SL_EDITOR_SPLASH_MENU, root)

  usemenu state
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0
     game = inputfilename("Filename of New Game?", ".rpg", rpg_browse_default, "input_file_new_game", , NO)
     IF game <> "" THEN
       IF NOT newRPGfile(finddatafile("ohrrpgce.new"), game & ".rpg") THEN cleanup_and_terminate
       sourcerpg = game & ".rpg"
       game = trimpath(game)
       EXIT DO
     END IF
    CASE 1
     sourcerpg = browse(browseRPG, rpg_browse_default, , "custom_browse_rpg")
     game = trimextension(trimpath(sourcerpg))
     IF game <> "" THEN EXIT DO
    CASE 2
     cleanup_and_terminate
   END SELECT
  END IF
 
  clearpage dpage
  DrawSlice root, dpage
  standardmenu chooserpg_menu(), state, menusl->ScreenX, menusl->ScreenY, dpage, opts
  wrapprint short_version & " " & gfxbackend & "/" & musicbackend, 8, pBottom - 14, uilook(uiMenuItem), dpage
  edgeprint "Press F1 for help on any menu!", 8, pBottom - 4, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 DeleteSlice @root
END SUB

SUB prompt_for_save_and_quit()
 xbsave game & ".gen", gen(), 1000

 DIM quit_menu(3) as string
 quit_menu(0) = "Continue editing"
 quit_menu(1) = "Save changes and continue editing"
 quit_menu(2) = "Save changes and quit"
 quit_menu(3) = "Discard changes and quit"
 setquitflag NO  'Stop firing esc's, if the user asked to quit the program
 
 DIM quitnow as integer
 quitnow = sublist(quit_menu(), "quit_and_save")
 IF getquitflag() THEN '2nd quit request? Right away!
  DIM basename as string = trimextension(sourcerpg)
  DIM lumpfile as string
  DIM i as integer = 0
  DO
   lumpfile = basename & ".rpg_" & i & ".bak"
   i += 1
  LOOP WHILE isfile(lumpfile)
  clearpage 0
  printstr "Saving as " & lumpfile, 0, 0, 0
  printstr "LUMPING DATA: please wait...", 0, 10, 0
  setvispage 0, NO
  write_rpg_or_rpgdir workingdir, lumpfile
  cleanup_and_terminate
  EXIT SUB
 END IF 

 IF (quitnow = 2 OR quitnow = 3) AND slave_channel <> NULL_CHANNEL THEN
  'Prod the channel to see whether it's still up (send ping)
  channel_write_line(slave_channel, "P ")

  IF slave_channel <> NULL_CHANNEL THEN
   IF yesno("You are still running a copy of this game. Quitting will force " & GAMEEXE & " to quit as well. Really quit?") = NO THEN quitnow = 0
  END IF
 END IF
 IF quitnow = 1 OR quitnow = 2 THEN
  save_current_game
 END IF
 IF quitnow = 3 THEN
  DIM quit_confirm(1) as string
  quit_confirm(0) = "I changed my mind! Don't quit!"
  quit_confirm(1) = "I am sure I don't want to save."
  IF sublist(quit_confirm()) <= 0 THEN quitnow = 0
  cleanup_workingdir_on_exit = YES  'This only makes a difference if a previous attempt to save failed
 END IF
 setkeys YES
 IF quitnow > 1 THEN cleanup_and_terminate

END SUB

SUB prompt_for_password()
 '--Is a password set?
 IF checkpassword("") THEN EXIT SUB
 
 '--Input password
 DIM pas as string = ""
 DIM passcomment as string = ""
 DIM tog as integer
 passcomment = "If you've forgotten your password, don't panic! It can be easily removed. " _
               "Contact the OHRRPGCE developers, or learn to compile the source code yourself."
 'Uncomment to display the/a password
 'passcomment = getpassword
 setkeys YES
 DO
  setwait 55
  setkeys YES
  tog = tog XOR 1
  IF keyval(ccCancel) > 0 THEN cleanup_and_terminate
  IF keyval(scAnyEnter) > 1 THEN
   IF checkpassword(pas) THEN
    EXIT SUB
   ELSE
    cleanup_and_terminate
   END IF
  END IF
  strgrabber pas, 17
  clearpage dpage
  wrapprint "This game requires a password to edit. Type it in and press ENTER", 10, 10, uilook(uiText), dpage
  textcolor uilook(uiSelectedItem + tog), 1
  printstr STRING(LEN(pas), "*"), 20, 40, dpage
  wrapprint passcomment, 15, pBottom - 15, uilook(uiText), dpage, rWidth - 30
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB import_scripts_and_terminate (scriptfile as string)
 debuginfo "Importing scripts from " & scriptfile
 DIM success as bool
 success = compile_andor_import_scripts(absolute_with_orig_path(scriptfile), option_nowait)
 IF success THEN
  xbsave game & ".gen", gen(), 1000
  save_current_game
 END IF
 cleanup_workingdir_on_exit = YES  'Cleanup even if saving the .rpg failed: no loss
 IF success = NO AND option_nowait THEN PRINT "Compiling or importing failed"
 cleanup_and_terminate NO, IIF(success, 0, 1)
END SUB

SUB export_translations_and_terminate (translationfile as string)
 debuginfo "Importing scripts from " & translationfile
 DIM success as bool
 success = export_translations(translationfile)
 PRINT "Exporting " & translationfile & IIF(success, " succeeded", " failed")
 cleanup_and_terminate NO, IIF(success, 0, 1)
END SUB

SUB cleanup_and_terminate (show_quit_msg as bool = YES, retval as integer = 0)
 debuginfo "Cleaning up and terminating " & retval
 IF slave_channel <> NULL_CHANNEL THEN
  channel_write_line(slave_channel, "Q ")
  #IFDEF __FB_WIN32__
   'On windows, can't delete workingdir until Game has closed the music. Not too serious though
   basic_textbox "Waiting for " & GAMEEXE & " to clean up...", uilook(uiText), vpage
   setvispage vpage, NO
   IF channel_wait_for_msg(slave_channel, "Q", "", 2000) = 0 THEN
    basic_textbox "Waiting for " & GAMEEXE & " to clean up... giving up.", uilook(uiText), vpage
    setvispage vpage, NO
    sleep 700
   END IF
  #ENDIF
  channel_close(slave_channel)
 END IF
 IF slave_process <> 0 THEN
  basic_textbox "Waiting for " & GAMEEXE & " to quit...", uilook(uiText), vpage
  setvispage vpage, NO
  'Under GNU/Linux this calls pclose which will block until Game has quit.
  cleanup_process @slave_process
 END IF
 closemusic
 'catch sprite leaks
 sprite_empty_cache
 palette16_empty_cache
 cleanup_global_reload_doc
 clear_binsize_cache
 IF show_quit_msg ANDALSO read_config_bool("show_quit_msg", YES) ANDALSO getquitflag() = NO THEN
  clearpage vpage
  ' Don't let Spoonweaver's cat near your power cord!
  pop_warning "Don't forget to keep backup copies of your work! You never know when an unknown bug or a cat-induced hard-drive crash or a little brother might delete your files!", YES
 END IF
 IF cleanup_workingdir_on_exit THEN
  empty_workingdir workingdir
 END IF
 restoremode
 debuginfo "End."
 end_debug
 SYSTEM retval
END SUB


'==========================================================================================
'                                       Global menus
'==========================================================================================


LOCAL FUNCTION volume_controls_callback(menu as MenuDef, state as MenuState, dataptr as any ptr) as bool
 ' This code is duplicated from player_menu_keys :(
 IF keyval(scF1) > 1 THEN show_help("editor_volume")
 DIM BYREF mi as MenuDefItem = *menu.items[state.pt]
 IF mi.t = mtypeSpecial AND (mi.sub_t = spMusicVolume OR mi.sub_t = spVolumeMenu) THEN
  IF keyval(ccLeft) > 1 THEN set_music_volume large(get_music_volume - 1/16, 0.0)
  IF keyval(ccRight) > 1 THEN set_music_volume small(get_music_volume + 1/16, 1.0)
 END IF
 IF mi.t = mtypeSpecial AND mi.sub_t = spSoundVolume THEN
  IF keyval(ccLeft) > 1 THEN set_global_sfx_volume large(get_global_sfx_volume - 1/16, 0.0)
  IF keyval(ccRight) > 1 THEN set_global_sfx_volume small(get_global_sfx_volume + 1/16, 1.0)
 END IF
 RETURN NO
END FUNCTION

' Allow changing the in-editor volume
SUB Custom_volume_menu
 DIM menu as MenuDef
 create_volume_menu menu
 run_MenuDef menu, @volume_controls_callback
END SUB

'Record a combined editor+player gif
SUB start_recording_combined_gif()
 IF slave_channel = NULL_CHANNEL THEN EXIT SUB
 DIM screenfile as string = tmpdir & "screenshare" & randint(100000) & ".bmp"
 channel_write_line(slave_channel, "SCREEN " & screenfile)
 start_recording_gif screenfile
 debuginfo "...recording with secondscreen " & screenfile
END SUB

TYPE CustomGlobalMenu
 items(any) as string
 item_codes(any) as integer
 DECLARE SUB append(code as integer, text as string)
END TYPE

SUB CustomGlobalMenu.append(code as integer, text as string)
 a_append item_codes(), code
 a_append items(), text
END SUB

' Accessible with F8 if we are editing a game
SUB Custom_global_menu
 DIM menu as CustomGlobalMenu
 IF editing_a_game THEN
  IF inside_importscripts = NO THEN
   'Don't reallow importing if we're already in the middle of it
   'TODO: maybe this should also be disallowed from inside scriptbrowse, etc?
   menu.append 0, "Reimport scripts"
  END IF
  #IFNDEF __FB_ANDROID__
   menu.append 1, "Test Game"
  #ENDIF
  'menu.append 10, "Save Game"
 END IF
 menu.append 2, "Volume"
 menu.append 3, "Macro record/replay (Ctrl-F11)"
 menu.append 4, "Zoom 1x"
 menu.append 5, "Zoom 2x"
 menu.append 6, "Zoom 3x"
 menu.append 7, "Zoom 4x"
 menu.append 8, "Switch graphics backend (Ctrl-F7)"
 'menu.append 9, "Music backend settings"
 IF slave_channel <> NULL_CHANNEL THEN
  IF recording_gif() THEN
   menu.append 12, "Stop recording .gif video (Ctrl-F12)"
  ELSE
   menu.append 11, "Record combined editor+player .gif"
  END IF
 END IF
 DIM note as string
 IF num_logged_errors THEN note = ": " & num_logged_errors & " errors" ELSE note = " log"
 menu.append 13, "View c_debug.txt" & note & " (Ctrl-F8)"

 DIM choice as integer
 choice = multichoice("Global Editor Options (F9)", menu.items())
 IF choice > -1 THEN choice = menu.item_codes(choice)

 IF choice = 0 THEN
  reimport_previous_scripts
 ELSEIF choice = 1 THEN
  spawn_game_menu(keyval(scShift) > 0, keyval(scCtrl) > 0)
 ELSEIF choice = 2 THEN
  Custom_volume_menu
 ELSEIF choice = 3 THEN
  macro_controls
 ELSEIF choice = 4 THEN
  set_scale_factor 1, NO
 ELSEIF choice = 5 THEN
  set_scale_factor 2, NO
 ELSEIF choice = 6 THEN
  set_scale_factor 3, NO
 ELSEIF choice = 7 THEN
  set_scale_factor 4, NO
 ELSEIF choice = 8 THEN
  gfx_backend_menu
 ELSEIF choice = 9 THEN
  music_backend_menu
 ELSEIF choice = 10 THEN
  'Warning: data in the current menu may not be saved! So figured it better to avoid this.
  save_current_game
 ELSEIF choice = 11 THEN
  start_recording_combined_gif
 ELSEIF choice = 12 THEN
  stop_recording_video
 ELSEIF choice = 13 THEN
  open_document log_dir & *app_log_filename
 END IF
END SUB

' This is called after every setkeys unless we're already inside global_setkeys_hook
' It should be fine to call any allmodex function in here, but beware we might
' not have loaded a game yet!
SUB global_setkeys_hook
 IF keyval(scF9) > 1 THEN Custom_global_menu
 'The other keys documented in Custom_global_menu are checked in allmodex_controls
END SUB

'==========================================================================================
'                    Creating/cleaning working.tmp and creating games
'==========================================================================================


' Returns true for success
FUNCTION newRPGfile (templatefile as string, newrpg as string) as bool
 IF newrpg = "" THEN RETURN NO
 ' Error already shown if missing
 IF NOT isfile(templatefile) THEN RETURN NO
 textcolor uilook(uiSelectedDisabled), 0
 printstr "Please Wait...", 0, 100, vpage
 printstr "Creating RPG File", 0, 110, vpage
 setvispage vpage, NO
 writeablecopyfile templatefile, newrpg
 printstr "Unlumping", 0, 120, vpage
 setvispage vpage, NO
 unlump newrpg, workingdir + SLASH

 '--create archinym information lump
 DIM fh as integer
 OPENFILE(workingdir + SLASH + "archinym.lmp", FOR_OUTPUT, fh)
 PRINT #fh, "ohrrpgce"
 PRINT #fh, short_version
 CLOSE #fh

 DIM root_node as NodePtr
 root_node = get_general_reld()

 '--Delete general.reld version info. It will then be set by upgrade()
 IF root_node = NULL THEN showerror "Couldn't load general.reld!" : RETURN NO
 DIM vernode as NodePtr
 vernode = GetChildByName(root_node, "editor_version")
 IF vernode THEN FreeNode vernode
 vernode = GetChildByName(root_node, "prev_editor_versions")
 IF vernode THEN FreeNode vernode

 '--Set creation time, wipe edit_time
 SetChildNode(root_node, "edit_time", 0.)
 DIM created_node as NodePtr
 created_node = SetChildNode(root_node, "created", NOW)
 SetChildNode(created_node, "str", format_date(NOW))

 close_general_reld

 printstr "Finalumping", 0, 130, vpage
 setvispage vpage, NO
 '--re-lump files as NEW rpg file
 RETURN write_rpg_or_rpgdir(workingdir, newrpg)
END FUNCTION

' Argument is a timeserial
FUNCTION format_date(timeser as double) as string
 IF timeser = 0 THEN RETURN "0"
 RETURN FORMAT(timeser, "yyyy mmm dd hh:mm:ss")
END FUNCTION

'Returns the last mtime of any file in a directory (excluding *.tmp)
FUNCTION directory_last_mtime(directory as string) as double
 DIM lasttime as double = 0
 DIM filelist() as string
 findfiles directory, ALLFILES, fileTypeFile, NO, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  IF RIGHT(filelist(i), 4) <> ".tmp" THEN
   lasttime = large(lasttime, FILEDATETIME(directory + SLASH + filelist(i)))
  END IF
 NEXT
 RETURN lasttime
END FUNCTION

' Write workingdir/session_info.txt.tmp
' Note: we assume that whenever this is called (and sourcerpg is set) that we are
' loading or saving the game.
SUB write_session_info ()
 DIM text(11) as string
 text(0) = short_version
 text(1) = get_process_path(get_process_id())  'May not match COMMAND(0)
 text(2) = "# Custom pid:"
 text(3) = STR(get_process_id())
 text(4) = "# Editing start (load/save) time:"
 text(5) = format_date(NOW)
 text(6) = STR(NOW)
 text(7) = "# Game path:"
 'sourcerpg may be blank if we're not yet editing a game
 IF LEN(sourcerpg) THEN
  text(8) = absolute_path(sourcerpg)
  text(9) = "# Last modified time of game:"
  DIM modified as double
  IF isfile(sourcerpg) THEN
   modified = FILEDATETIME(sourcerpg)
  ELSE  'rpgdir
   modified = directory_last_mtime(sourcerpg)
  END IF
  text(10) = format_date(modified)
  text(11) = STR(modified)
 END IF
 lines_to_file text(), workingdir + SLASH + "session_info.txt.tmp", LINE_END
END SUB

' Collect data about a previous (or ongoing) editing session from a dirty working.tmp
FUNCTION get_previous_session_info (workdir as string) as SessionInfo
 DIM ret as SessionInfo
 DIM exe as string
 DIM sessionfile as string
 sessionfile = workdir + SLASH + "session_info.txt.tmp"
 ret.workingdir = workdir
 IF isfile(sessionfile) THEN
  ret.info_file_exists = YES
  DIM text() as string
  lines_from_file text(), sessionfile
  IF UBOUND(text) < 3 THEN
   'Invalid file. We've always written at least 8 lines (but many may be blank)
   debug sessionfile & " appears to contain garbage."
  ELSE
   'The metadata file's mtime should be nearly the same, but in future maybe we will want to write it
   'without saving the game.
   'ret.session_start_time = FILEDATETIME(sessionfile)
   IF UBOUND(text) >= 6 THEN
    ret.session_start_time = VAL(text(6))
   END IF
   IF UBOUND(text) >= 8 ANDALSO LEN(text(8)) > 0 THEN
    ret.sourcerpg = text(8)
    IF isfile(ret.sourcerpg) THEN
     ret.sourcerpg_current_mtime = FILEDATETIME(ret.sourcerpg)
    ELSE  'Is an .rpgdir
     ret.sourcerpg_current_mtime = directory_last_mtime(ret.sourcerpg)
    END IF
    IF UBOUND(text) >= 11 THEN ret.sourcerpg_old_mtime = VAL(text(11))
   END IF
   ret.pid = VAL(text(3))
   exe = text(1)
   ' It's possible that this copy of Custom crashed and another copy was run with the same pid,
   ' but it's incredibly unlikely
   DIM pid_current_exe as string = get_process_path(ret.pid)
   debuginfo "pid_current_exe = " & pid_current_exe
   ret.running = (LEN(exe) ANDALSO (pid_current_exe = exe ORELSE exe = "<unknown>"))
   #IFDEF __FB_ANDROID__
    'It's not possible to run two copies of the app at the same time
    ret.running = NO
   #ENDIF
  END IF
 ELSE
  'We don't know anything, except that we could work out session_start_time by looking at working.tmp mtimes.
 END IF

 ' When was a lump last modified?
 ret.last_lump_mtime = directory_last_mtime(workdir)

 ret.partial_rpg = isfile(workdir + SLASH + "__danger.tmp")
 IF ret.partial_rpg THEN
  'Check if the file is stale
  DIM daysago as double = NOW - FILEDATETIME(workdir + SLASH + "__danger.tmp")
  debuginfo "Found __danger.tmp file, " & (daysago * 24 * 60) & " minutes old"
  IF daysago * 24 * 60 < 1 THEN  'Less than 1 minute old
   ret.fresh_danger_tmp = YES
  END IF
 END IF

 debuginfo "prev_session.workingdir = " & ret.workingdir
 debuginfo "prev_session.info_file_exists = " & yesorno(ret.info_file_exists)
 debuginfo "prev_session.pid = " & ret.pid & " (exe = " & exe & ")"
 debuginfo "prev_session.running = " & yesorno(ret.running)
 debuginfo "prev_session.partial_rpg = " & yesorno(ret.partial_rpg)
 debuginfo "prev_session.fresh_danger_tmp = " & yesorno(ret.fresh_danger_tmp)
 debuginfo "prev_session.sourcerpg = " & ret.sourcerpg
 debuginfo "prev_session.sourcerpg_old_mtime = " & format_date(ret.sourcerpg_old_mtime)
 debuginfo "prev_session.sourcerpg_current_mtime = " & format_date(ret.sourcerpg_current_mtime)
 debuginfo "prev_session.session_start_time = " & format_date(ret.session_start_time)
 debuginfo "prev_session.last_lump_mtime = " & format_date(ret.last_lump_mtime)

 RETURN ret
END FUNCTION

' Try to delete everything in the given directory in a race-condition-safe order. Returns true if succeeded.
' (This is overkill now, I guess)
FUNCTION empty_workingdir (workdir as string) as bool
 touchfile workdir + SLASH + "__danger.tmp"
 DIM filelist() as string
 findfiles workdir, ALLFILES, fileTypeFile, NO, filelist()
 ' Delete these metadata files last
 a_shuffle_to_end filelist(), a_findcasei(filelist(), "__danger.tmp")
 a_shuffle_to_end filelist(), a_findcasei(filelist(), "session_info.txt.tmp")
 FOR i as integer = 0 TO UBOUND(filelist)
  DIM fname as string = workdir + SLASH + filelist(i)
  IF NOT safekill(fname) THEN
   'notification "Could not clean up " & workdir & !"\nYou may have to manually delete its contents."
   RETURN NO
  END IF
 NEXT
 killdir workdir, YES  'recursively, just in case
 RETURN YES
END FUNCTION

' Selects an unused workingdir path and creates it
SUB setup_workingdir ()
 ' This can't pick "working.tmp", so old versions of Custom won't see and clobber it.
 DIM idx as integer = 0
 DO
  workingdir = tmpdir & "working" & idx & ".tmp"
  IF NOT isdir(workingdir) THEN EXIT DO
  idx += 1
 LOOP

 debuginfo "Working in " & workingdir
 IF makedir(workingdir) <> 0 THEN
  fatalerror "Couldn't create " & workingdir & !"\nCheck c_debug.txt"
 END IF
 write_session_info
END SUB

' Check whether any other copy of Custom is already editing sourcerpg
FUNCTION check_ok_to_open (filename as string) as bool
 debuginfo "check_ok_to_open..."
 DIM olddirs() as string
 findfiles tmpdir, "working*.tmp", fileTypeDirectory, NO, olddirs()

 FOR idx as integer = 0 TO UBOUND(olddirs)
  DIM sessinfo as SessionInfo = get_previous_session_info(tmpdir & olddirs(idx))

  IF paths_equal(sessinfo.sourcerpg, filename) THEN
   IF NOT sessinfo.running THEN
    ' Apparently this crashed between when we launched, and when the .rpg was selected in the browser.
    ' Return true if we managed to delete it.
    RETURN check_a_crashed_workingdir(sessinfo)
   ELSE
    DIM msg as string
    msg = "Another copy of " CUSTOMEXE " is already editing " & decode_filename(sourcerpg) & _
          !".\nYou can't open the same game twice at once! " _
          "(Make a copy first if you really want to.)"
    'IF is_windows_9x() THEN
     'sessinfo.running is not reliable on Win9x, so provide a bypass ... maybe it's not 100% reliable anyway
     IF twochoice(msg, "OK, quit", "No! I swear it's crashed! Recover it.") = 1 THEN
      RETURN check_a_crashed_workingdir(sessinfo)
     END IF
    'ELSE
    ' notification msg
    'END IF
   END IF
   RETURN NO
  END IF
 NEXT
 RETURN YES
END FUNCTION

SUB check_for_crashed_workingdirs ()
 'This also finds working.tmp, which belongs to old versions
 DIM olddirs() as string
 findfiles tmpdir, "working*.tmp", fileTypeDirectory, NO, olddirs()

 FOR idx as integer = 0 TO UBOUND(olddirs)
  DIM sessinfo as SessionInfo = get_previous_session_info(tmpdir & olddirs(idx))

  IF sessinfo.info_file_exists THEN
   IF sessinfo.running THEN
    ' Not crashed, so ignore
    CONTINUE FOR
   END IF
   debuginfo "Found workingtmp for crashed Custom"
  END IF

  check_a_crashed_workingdir sessinfo
 NEXT
END SUB

'Returns true if we deleted this session successfully
FUNCTION check_a_crashed_workingdir (sessinfo as SessionInfo) as bool
 IF sessinfo.info_file_exists THEN
  IF sessinfo.partial_rpg THEN
   debuginfo "...crashed while unlumping/deleting temp files, silent cleanup"
   ' In either case, safe to delete files.
   RETURN empty_workingdir(sessinfo.workingdir)
  END IF

  IF LEN(sessinfo.sourcerpg) = 0 THEN
   debuginfo "...crashed before opening a game, silent cleanup"
   RETURN empty_workingdir(sessinfo.workingdir)
  END IF
 END IF

 ' Does this look like a game, or should we just delete it?
 IF NOT sessinfo.fresh_danger_tmp THEN
  DIM filelist() as string
  findfiles sessinfo.workingdir, ALLFILES, fileTypeFileOrDir, NO, filelist()

  IF UBOUND(filelist) <= 5 THEN
   'Just some stray files that refused to delete last time,
   'or possibly an old copy of Custom running but no game opened yet no way to handle that
   debuginfo (UBOUND(filelist) + 1) & " files in working.tmp, silent cleanup"
   RETURN empty_workingdir(sessinfo.workingdir)
  END IF
 END IF

 'Auto-handling failed, ask user what to do
 RETURN handle_dirty_workingdir(sessinfo)
END FUNCTION

' When recovering an rpg from working.tmp, pick an unused destination filename.
FUNCTION pick_recovered_rpg_filename(old_sourcerpg as string) as string
 DIM destdir as string
 DIM destfile_basename as string
 IF LEN(old_sourcerpg) THEN
  ' Put next to original file
  destdir = add_trailing_slash(trimfilename(old_sourcerpg))
  IF NOT diriswriteable(destdir) THEN destdir = ""
  destfile_basename = trimpath(trimextension(old_sourcerpg)) & " crash-recovered "
 ELSE
  destfile_basename = "crash-recovered"
 END IF
 IF NOT diriswriteable(destdir) THEN destdir = documents_dir & SLASH

 DIM index as integer = 0
 DO
  DIM destfile as string = destdir & destfile_basename & index & ".rpg"
  IF NOT isfile(destfile) THEN RETURN destfile
  index += 1
 LOOP
END FUNCTION

'Returns true if we can continue, false to cleanup_and_terminate
FUNCTION recover_workingdir (sessinfo as SessionInfo) as bool
 DIM origname as string = trimpath(sessinfo.sourcerpg)  'Might be ""
 'Trim "crash-recovered"
 DIM where as integer
 where = INSTR(origname, " crash-recovered ")
 IF where THEN
  origname = LEFT(origname, where - 1) & ".rpg"
 END IF

 DIM destfile as string
 destfile = pick_recovered_rpg_filename(origname)

 printstr "Saving as " + decode_filename(destfile), 0, 180, vpage
 printstr "LUMPING DATA: please wait...", 0, 190, vpage
 setvispage vpage, NO
 '--re-lump recovered files as RPG file
 IF write_rpg_or_rpgdir(sessinfo.workingdir, destfile) = NO THEN
  RETURN NO
 END IF
 clearpage vpage

 DIM msg as string
 IF LEN(origname) = 0 THEN origname = "gamename.rpg"
 msg = !"The recovered game has been saved as\n" & _
       fgtag(uilook(uiSelectedItem), decode_filename(destfile)) & !"\n" _
       "You can rename it to " & origname & ", but ALWAYS keep the previous copy " _
       !"as a backup because some data in the recovered file might be corrupt!\n" _
       "If you have questions, ask ohrrpgce-crash@HamsterRepublic.com"
 notification msg
 RETURN empty_workingdir(sessinfo.workingdir)
END FUNCTION

'Called when a partial or complete copy of a game exists
'Returns true if cleaned away, false if not cleaned up
FUNCTION handle_dirty_workingdir (sessinfo as SessionInfo) as bool
 clearpage vpage

 IF isfile(sessinfo.workingdir + SLASH + "__danger.tmp") THEN
  ' Don't provide option to recover, as this looks like garbage.
  ' If we've reached this point, then already checked whether it's a modern Custom
  ' ...but once, I saw a dirty working.tmp with __danger.tmp but no other
  ' files. Usually, __danger.tmp wouldn't appear without the session info file.
  ' However, maybe another copy of custom is busy unlumping a big game, so ask before deleting.
  DIM choice as integer
  choice = twochoice("Found a partial temporary copy of a game. " _
                     "It looks like a copy of " + CUSTOMEXE + " is or was in the process of " _
                     "either unlumping a game or deleting its temporary files. " _
                     "It might have crashed, or still be running. What do you want to do?", _
                     "Ignore", _
                     "Erase temporary files", _
                     0, 0)
  IF choice = 0 THEN
   RETURN NO
  ELSE
   RETURN empty_workingdir(sessinfo.workingdir)
  END IF
 END IF

 DIM msg as string
 DIM helpfile as string
 IF sessinfo.info_file_exists THEN
  ' We already checked Custom isn't still running

  msg = CUSTOMEXE " crashed while editing a game, but the temp unsaved modified copy of the game still exists." LINE_END
  msg &= "Original file:" LINE_END
  msg &= decode_filename(sessinfo.sourcerpg) & LINE_END

  IF sessinfo.sourcerpg_current_mtime < sessinfo.session_start_time THEN
   ' It's a bit confusing to tell the user 4 last-mod times, so skip this one.
   msg &= "Last modified " & format_date(sessinfo.sourcerpg_old_mtime) & LINE_END
  END IF

  ' The }'s get replaced with either | or a space.
  msg &=  "}|" LINE_END _
          "}+>Loaded or last saved by Custom " LINE_END _
          "}  at:        " & format_date(sessinfo.session_start_time) & LINE_END _
          "}  Last edit: " & format_date(sessinfo.last_lump_mtime)

  IF sessinfo.sourcerpg_current_mtime > sessinfo.session_start_time THEN
   msg &= LINE_END "|" LINE_END _
          "+-> WARNING: " & decode_filename(trimpath(sessinfo.sourcerpg)) & " modified since it was loaded or saved!" _
          " Modified " & format_date(sessinfo.sourcerpg_current_mtime) ' & LINE_END

   replacestr(msg, LINE_END "}", LINE_END "|")
   helpfile = "recover_unlumped_rpg_outdated"
  ELSE
   replacestr(msg, LINE_END "}", LINE_END " ")
   helpfile = "recover_unlumped_rpg"
  END IF

 ELSE
  msg = !"An unknown game was found unlumped.\n" _
        "It appears that an old version of " + CUSTOMEXE + " is either already running, " _
        "or it has crashed."
 END IF

 DIM cleanup_menu(2) as string
 cleanup_menu(0) = "DO NOTHING (ask again later)"
 cleanup_menu(1) = "RECOVER temp files as a .rpg"
 cleanup_menu(2) = "ERASE temp files"
 DIM choice as integer
 choice = multichoice(msg, cleanup_menu(), 0, 0, helpfile, NO)  'Left justified

 IF choice = 0 THEN RETURN NO
 IF choice = 1 THEN RETURN recover_workingdir(sessinfo)
 IF choice = 2 THEN RETURN empty_workingdir(sessinfo.workingdir)  'erase
END FUNCTION


'==========================================================================================
'                               Secret/testing/debug menus
'==========================================================================================

SUB backend_keyrepeat_bugtest
 notification !"Holding down the key that triggered this box should not advance it\n" _
              "waitforanykey:\n(Press any key, and hold it down)"
 basic_textbox !"Nor this box\nwaitforkeyrelease:\n(Release key to advance)", uilook(uiText), vpage
 setvispage vpage
 waitforkeyrelease
END SUB

SUB secret_menu ()
 DIM menu(...) as string = { _
     "Editor Slice Editor", _
     "Reload Editor", _
     "Editor Editor", _
     "Conditions and More Tests", _
     "Transformed Quads", _
     "plankmenu cursor move tests", _
     "Text tests", _
     "Font tests", _
     "Stat Growth Chart", _
     "Edit Status Screen", _
     "Edit Status Screen Stat Plank", _
     "Edit Item Screen", _
     "Edit Item Screen Item Plank", _
     "Edit Spell Screen", _
     "Edit Spell Screen Spell List Plank", _
     "Edit Spell Screen Spell Plank", _
     "Edit Virtual Keyboard Screen", _
     "New Spriteset/Animation Editor", _
     "New backdrop browser", _
     "RGFX tests", _
     "Backend Keyrepeat Bugtest", _
     "HTTP test", _
     "CreateProcess tests (Windows only)", _
     "Edit Translations", _
     "Rotozoom tests/benchmarks", _
     "Test Game under Valgrind", _
     "Test Game under GDB" _
 }
 DIM st as MenuState
 st.autosize = YES
 st.last = UBOUND(menu)
 #IFDEF __FB_ANDROID__
  st.last -= 2  'Remove the "Test Game" options
 #ENDIF

 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF enter_space_click(st) THEN
   IF st.pt = 0 THEN slice_editor SL_COLLECT_EDITOR, get_data_dir() & SLASH "blank.slice", YES
   IF st.pt = 1 THEN reload_editor
   IF st.pt = 2 THEN editor_editor
   IF st.pt = 3 THEN condition_test_menu
   IF st.pt = 4 THEN quad_transforms_menu
   IF st.pt = 5 THEN plankmenu_cursor_move_tests
   IF st.pt = 6 THEN text_test_menu
   IF st.pt = 7 THEN font_test_menu
   IF st.pt = 8 THEN stat_growth_chart
   IF st.pt = 9 THEN slice_editor SL_COLLECT_STATUSSCREEN
   IF st.pt = 10 THEN slice_editor SL_COLLECT_STATUSSTATPLANK
   IF st.pt = 11 THEN slice_editor SL_COLLECT_ITEMSCREEN
   IF st.pt = 12 THEN slice_editor SL_COLLECT_ITEMPLANK
   IF st.pt = 13 THEN slice_editor SL_COLLECT_SPELLSCREEN
   IF st.pt = 14 THEN slice_editor SL_COLLECT_SPELLLISTPLANK
   IF st.pt = 15 THEN slice_editor SL_COLLECT_SPELLPLANK
   IF st.pt = 16 THEN slice_editor SL_COLLECT_VIRTUALKEYBOARDSCREEN
   IF st.pt = 17 THEN
    DIM options(...) as string = {"Hero", "Small Enemy", "Medium Enemy", "Large Enemy", "Walkabouts", "Weapons", "Attack", "Boxborder", "Portrait", "Backdrop", "Enemy"}
    DIM sprtype as SpriteType = multichoice("Edit what?", options())
    IF sprtype > -1 THEN spriteset_editor sprtype
   END IF
   IF st.pt = 18 THEN backdrop_browser
   IF st.pt = 19 THEN new_graphics_tests
   IF st.pt = 20 THEN backend_keyrepeat_bugtest
   IF st.pt = 21 THEN HTTP_demo
   IF st.pt = 22 THEN CreateProcess_tests
   IF st.pt = 23 THEN translations_menu
   IF st.pt = 24 THEN rotozoom_tests
   IF st.pt = 25 THEN spawn_game_menu NO, YES 'With valgrind
   IF st.pt = 26 THEN spawn_game_menu YES     'With gdb
  END IF
  usemenu st
  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
END SUB

FUNCTION window_size_description(scale as integer) as string
 IF scale >= 11 THEN
  ' Not implemented yet.
  RETURN "maximize"
 ELSE
  RETURN "~" & (10 * scale) & "% screen width"
 END IF
END FUNCTION

SUB resolution_menu ()
 DIM menu(9) as string
 DIM st as MenuState
 st.size = 24
 st.last = UBOUND(menu)
 DIM selectable(UBOUND(menu)) as bool
 flusharray selectable(), , YES

 DIM gen_root as NodePtr = get_general_reld()
 DIM console as NodePtr = GetOrCreateChild(gen_root, "console_options")

 'FIXME: selecting a resolution other than 320x200 causes the distrib menu
 'to not package gfx_directx.dll; remove that when gfx_directx is updated

 DO
  setwait 55
  setkeys
  DIM quit as bool = (keyval(ccCancel) > 1 OR (enter_space_click(st) AND st.pt = 0))
  IF usemenu(st, selectable()) ORELSE quit THEN
   ' Reinforce limits, because we temporarily allow 0 while typing for convenience
   gen(genResolutionX) = large(MinResolutionX, gen(genResolutionX))
   gen(genResolutionY) = large(MinResolutionY, gen(genResolutionY))
  END IF
  IF quit THEN EXIT DO
  IF keyval(scF1) > 1 THEN
    show_help "window_settings"
  END IF
  SELECT CASE st.pt
   CASE 1: st.need_update OR= intgrabber(gen(genFullscreen), 0, 1)
   CASE 2: st.need_update OR= intgrabber(gen(genWindowSize), 1, 10)
   CASE 3: st.need_update OR= intgrabber(gen(genLivePreviewWindowSize), 1, 10)
   CASE 4: st.need_update OR= intgrabber(gen(genRungameFullscreenIndependent), 0, 1)
   CASE 5
    DIM margins as integer = GetChildNodeInt(console, "safe_margin", 0)
    IF (margins = 0 ANDALSO keyval(scBackspace) > 1) ORELSE keyval(scDelete) > 1 THEN
     FreeChildNode console, "safe_margin"
     st.need_update = YES
    ELSE
     IF intgrabber(margins, 0, 10) THEN
      SetChildNode console, "safe_margin", margins
      st.need_update = YES
     END IF
    END IF
   CASE 8: st.need_update OR= intgrabber(gen(genResolutionX), 0, MaxResolutionX)
   CASE 9: st.need_update OR= intgrabber(gen(genResolutionY), 0, MaxResolutionY)
  END SELECT
  IF st.need_update THEN
   xbsave game + ".gen", gen(), 1000   'Instant live previewing update
   st.need_update = NO
  END IF
  menu(0) = "Previous Menu"
  menu(1) = "Default to fullscreen: " & yesorno(gen(genFullscreen))
  menu(2) = "Default window size: " & window_size_description(gen(genWindowSize))
  menu(3) = "Test-Game window size: " & window_size_description(gen(genLivePreviewWindowSize))
  menu(4) = "rungame fullscreen state: "
  IF gen(genRungameFullscreenIndependent) THEN
   menu(4) &= "independent"
  ELSE
   menu(4) &= "shared with this game"
  END IF
  menu(5) = "Console TV safe margin: " & GetChildNodeStr(console, "safe_margin", "Default")  'This is an integer
  selectable(6) = NO
  selectable(7) = NO
  menu(7) = fgtag(uilook(eduiHeading), " Experimental options")
  menu(8) = "Game horizontal resolution: " & gen(genResolutionX) & " pixels"
  menu(9) = "Game vertical resolution: " & gen(genResolutionY) & " pixels"
  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 xbsave game + ".gen", gen(), 1000
 write_general_reld()
END SUB

'This menu is for testing experimental Condition UI stuff
SUB condition_test_menu ()
 DIM as Condition cond1, cond2, cond3, cond4
 DIM as AttackElementCondition atkcond
 DIM float as double
 DIM float_repr as string = "0%"
 DIM atkcond_repr as string = ": Never"
 DIM menu(8) as string
 DIM st as MenuState
 st.last = UBOUND(menu)
 st.size = 22
 DIM tmp as integer

 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "condition_test"
  tmp = 0
  IF st.pt = 0 THEN
   IF enter_space_click(st) THEN EXIT DO
  ELSEIF st.pt = 2 THEN
   tmp = cond_grabber(cond1, YES , NO, st)
  ELSEIF st.pt = 3 THEN
   tmp = cond_grabber(cond2, NO, NO, st)
  ELSEIF st.pt = 5 THEN
   tmp = cond_grabber(cond3, YES, YES, st)
  ELSEIF st.pt = 6 THEN
   tmp = cond_grabber(cond4, NO, YES, st)
  ELSEIF st.pt = 7 THEN
   tmp = percent_cond_grabber(atkcond, atkcond_repr, ": Never", -9.99, 9.99, 5)
  ELSEIF st.pt = 8 THEN
   tmp = percent_grabber(float, float_repr, -9.99, 9.99, 5)
  END IF
  usemenu st

  clearpage vpage
  menu(0) = "Previous menu"
  menu(1) = "Enter goes to tag browser for tag conds:"
  menu(2) = " If " & condition_string(cond1, (st.pt = 2), "Always", 45)
  menu(3) = " If " & condition_string(cond2, (st.pt = 3), "Never", 45)
  menu(4) = "Enter always goes to cond editor:"
  menu(5) = " If " & condition_string(cond3, (st.pt = 5), "Always", 45)
  menu(6) = " If " & condition_string(cond4, (st.pt = 6), "Never", 45)
  menu(7) = "Fail vs damage from <fire>" & atkcond_repr
  menu(8) = "percent_grabber : " & float_repr
  standardmenu menu(), st, 0, 0, vpage
  printstr STR(tmp), 0, 190, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
END SUB

#IFDEF USE_RASTERIZER

SUB quad_transforms_menu ()
 DIM menu(...) as string = {"Arrows: scale X and Y", "<, >: change angle", "[, ]: change sprite"}
 DIM st as MenuState
 st.last = 2
 st.size = 22
 st.need_update = YES

 DIM spritemode as integer = -1  ' Not a SpriteType. A .PT# number or -1 to show master palette

 DIM testframe as Frame ptr
 DIM vertices(3) as Float3

 DIM angle as single
 DIM scale as Float2 = (2.0, 2.0)
 DIM position as Float2 = (150, 50)

 switch_to_32bit_vpages()
 DIM vpage8 as integer = allocatepage( , , 8)

 DIM as double drawtime, pagecopytime

 DIM spriteSurface as Surface ptr

 DIM masterPalette as RGBPalette ptr
 gfx_paletteFromRGB(@master(0), @masterPalette)

 DO
  setwait 55

  if st.need_update then
   if spritemode < -1 then spritemode = sprTypeLastPT
   if spritemode > sprTypeLastPT then spritemode = -1

   frame_unload @testframe

   select case spritemode
    case 0 to sprTypeLastPT
     DIM tempsprite as GraphicPair
     load_sprite_and_pal tempsprite, spritemode, 0, -1
     with tempsprite
      testframe = frame_new(.sprite->w, .sprite->h, , YES)
      frame_draw .sprite, .pal, 0, 0, , , testframe
     end with
     unload_sprite_and_pal tempsprite
    case else
     testframe = frame_new(16, 16)
     FOR i as integer = 0 TO 255
      putpixel testframe, (i MOD 16), (i \ 16), i
     NEXT
   end select

   gfx_surfaceDestroy( @spriteSurface )
   gfx_surfaceCreateFrameView( testframe, @spriteSurface )

   DIM testframesize as Rect
   WITH testframesize
    .top = 0
    .left = 0
    .right = spriteSurface->width - 1
    .bottom = spriteSurface->height - 1
   END WITH
   vec3GenerateCorners @vertices(0), 4, testframesize

   st.need_update = NO
  end if

  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(ccLeft)  THEN scale.x -= 0.1
  IF keyval(ccRight) THEN scale.x += 0.1
  IF keyval(ccUp)    THEN scale.y -= 0.1
  IF keyval(ccDown)  THEN scale.y += 0.1
  IF keyval(scLeftCaret)  THEN angle -= 0.1
  IF keyval(scRightCaret) THEN angle += 0.1
  IF keyval(scLeftBracket) > 1 THEN spritemode -= 1: st.need_update = YES
  IF keyval(scRightBracket) > 1 THEN spritemode += 1: st.need_update = YES

  clearpage vpage8
  standardmenu menu(), st, 0, 0, vpage8
  ' We have to draw onto a temp 8-bit Surface, because frame_draw with scale
  ' isn't supported with Surfaces yet
  frame_draw testframe, , 20, 50, 2, , vpages(vpage8)  'drawn at 2x scale

  'Can only display the previous frame's time to draw, since we don't currently
  'have any functions to print text to surfaces
  printstr "Drawn in " & FIX(drawtime * 1000000) & " usec, pagecopytime = " & FIX(pagecopytime * 1000000) & " usec", 0, 190, vpage
  debug "Drawn in " & FIX(drawtime * 1000000) & " usec, pagecopytime = " & FIX(pagecopytime * 1000000) & " usec"

  pagecopytime = TIMER
  'Copy from vpage8 (8 bit Frame) to the render target surface
  frame_draw vpages(vpage8), NULL, 0, 0, , NO, vpage
  pagecopytime = TIMER - pagecopytime

  DIM starttime as double = TIMER

  DIM matrix as Float3x3
  matrixLocalTransform @matrix, angle, scale, position
  DIM trans_vertices(3) as Float3
  vec3Transform @trans_vertices(0), 4, @vertices(0), 4, matrix

  'may have to reorient the tex coordinates
  DIM pt_vertices(3) as VertexPT
  pt_vertices(0).tex.u = 0
  pt_vertices(0).tex.v = 0
  pt_vertices(1).tex.u = 1
  pt_vertices(1).tex.v = 0
  pt_vertices(2).tex.u = 1
  pt_vertices(2).tex.v = 1
  pt_vertices(3).tex.u = 0
  pt_vertices(3).tex.v = 1
  FOR i as integer = 0 TO 3
   pt_vertices(i).pos.x = trans_vertices(i).x
   pt_vertices(i).pos.y = trans_vertices(i).y
  NEXT

  gfx_renderQuadTexture( @pt_vertices(0), spriteSurface, masterPalette, YES, NULL, vpages(vpage)->surf )
  drawtime = TIMER - starttime

  setvispage vpage
  dowait
 LOOP
 setkeys
 frame_unload @testframe
 freepage vpage8
 switch_to_8bit_vpages()
 gfx_surfaceDestroy(@spriteSurface)
 gfx_paletteDestroy(@masterPalette)
END SUB

#ELSE

SUB quad_transforms_menu ()
 notification "Compile with 'scons raster=1' to enable."
END SUB

#ENDIF

'smooth is 0, 1 or 2
SUB rotozoom_test_with (img as GraphicPair, rotate as double, zoomx as double, zoomy as double, smooth as integer)
 clearpage vpage

 DIM as Surface ptr in_surf, out_surf
 IF smooth > 0 THEN
  in_surf = frame_to_surface32(img.sprite, master())
 ELSE
  IF gfx_surfaceCreateFrameView(img.sprite, @in_surf) THEN EXIT SUB
 END IF

 ' First warm up the CPU, because CPU frequency toggling is the norm
 ' these days and it makes timing meaningless unless at a reliable frequency
 DIM rztime as double = TIMER
 WHILE TIMER - rztime < 150e-3
  out_surf = rotozoomSurface(in_surf, rotate, 1., 1., NO)
  gfx_surfaceDestroy(@out_surf)
 WEND

 ' Repeat 10 times and take the min time
 DIM rzmin as double = 1e99
 FOR repeat as integer = 1 TO 10
  ' Repeat several times until at least 3ms passed
  rztime = TIMER
  DIM cnt as integer = 0
  WHILE TIMER - rztime < 3e-3
   gfx_surfaceDestroy(@out_surf)
   IF smooth = 2 THEN
    out_surf = surface_scale(in_surf, large(1, img.sprite->w * zoomx), large(1, img.sprite->h * zoomy))
   ELSE
    out_surf = rotozoomSurface(in_surf, rotate, zoomx, zoomy, smooth)  'smooth 0/1
   END IF
   BUG_IF(out_surf = NULL, "rotozoom returned NULL")
   cnt += 1
  WEND
  rzmin = small(rzmin, (TIMER - rztime) / cnt)
 NEXT

 DIM spr as Frame ptr = frame_with_surface(out_surf)
 frame_draw spr, img.pal, pCentered, pCentered - 50, , NO, vpage
 setvispage vpage
 notification strprintf("zoom %.2fx%.2f (size %d*%d) rotate %.1f smooth %d: %.1fus", zoomx, zoomy, out_surf->width, out_surf->height, rotate, smooth, (rzmin * 1e6))

 gfx_surfaceDestroy(@in_surf)
 gfx_surfaceDestroy(@out_surf)
 frame_unload @spr
END SUB

SUB rotozoom_tests ()
 switch_to_32bit_vpages

 DIM img as GraphicPair
 load_sprite_and_pal img, sprTypeBackdrop, 1
 rotozoom_test_with img, 45, 1.2, 1.2, 0
 rotozoom_test_with img, 45, 1.2, 1.2, 1
 rotozoom_test_with img, 45, 1.2, 1.2, 2
 unload_sprite_and_pal img

 load_sprite_and_pal img, sprTypeLargeEnemy, 1
 FOR zoom as double = 0.5 TO 6.501 STEP 2.
  rotozoom_test_with img, 130, zoom, zoom, 0
 NEXT
 unload_sprite_and_pal img

 switch_to_8bit_vpages
END SUB


SUB text_test_menu
 DIM text as string = load_help_file("texttest")
 DIM mouse as MouseInfo
 hidemousecursor
 DO
  setwait 55
  setkeys
  mouse = readmouse
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN
   show_help "texttest"
   text = load_help_file("texttest")
  END IF
  IF keyval(scF2) > 1 THEN
   pop_warning !"Extreemmmely lonngggg Extreemmmely lonngggg Extreemmmely lonngggg Extreemmmely lonngggg Extreemmmely lonngggg Extreemmmely lonngggg Extreemmmely lonngggg \n\ntext\nbox\n\nnargh\nnargh\nnargh\nndargh\nnargh\nnagrgh\nnargh\n\nmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm"
  END IF
  IF keyval(scF3) > 1 THEN
   text = load_help_file("texttest_stress_test")
  END IF

  DIM curspos as StringCharPos
  DIM pos2 as StringSize
  find_point_in_text @curspos, mouse.x - 20, mouse.y - 20, text, 280, 0, 0, 0, YES, YES

  text_layout_dimensions @pos2, text, curspos.charnum, , 280, fonts(0), YES, YES

  clearpage vpage
  edgeboxstyle 10, 10, 300, 185, 0, vpage
  wrapprint text, 20, 20, , vpage, 280, , fontPlain
  rectangle vpages(vpage), 20 + pos2.lastw, 20 + pos2.h - pos2.finalfont->h, 8, pos2.finalfont->h, 5
  printstr CHR(3), mouse.x - 2, mouse.y - 2, vpage
  printstr STR(curspos.charnum), 0, 190, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 showmousecursor
END SUB

SUB new_graphics_tests
 DIM ofile as string = tmpdir + SLASH + "backdrops.rgfx"
 convert_mxs_to_rgfx(game + ".mxs", ofile, sprTypeBackdrop)

 notification " .mxs size " & filelen(game + ".mxs") & "B, .rgfx size " & filelen(ofile) & "B"

 'Lets see how long the document takes to load
 DIM doc as DocPtr
 DIM starttime as double = timer
 doc = LoadDocument(ofile, optNoDelay)
 notification "Backdrop .rgfx completely loaded in " & CINT((timer - starttime) * 1000) & "ms"
 FreeDocument doc
 doc = NULL

 DIM fr as Frame ptr
 DIM rgfx_time as double
 FOR i as integer = 0 TO gen(genNumBackdrops) - 1
  starttime = timer
  IF doc = NULL THEN doc = rgfx_open(ofile)
  fr = rgfx_load_spriteset(doc, sprTypeBackdrop, i)
  rgfx_time += timer - starttime
  frame_draw fr, , 0, 0, 1, NO, vpage
  setvispage vpage
  ' waitforanykey
  frame_unload @fr
 NEXT
 starttime = timer
 'Load backdrops without caching
 FOR i as integer = 0 TO gen(genNumBackdrops) - 1
  fr = frame_load_mxs(game + ".mxs", i)
  frame_unload @fr
 NEXT
 notification gen(genNumBackdrops) & " backdrops loaded from .rgfx in " & CINT(rgfx_time * 1000) & "ms; " _
     "loaded from mxs in " & CINT((timer - starttime) * 1000) & "ms"
END SUB

SUB plankmenu_tests_generate_grid(root as Slice ptr, scatter as integer, percent as integer)
 DeleteSliceChildren root
 FOR y as integer = 0 TO 4
  FOR x as integer = 0 TO 6
   IF randint(100) > percent THEN CONTINUE FOR
   DIM sl as Slice ptr = NewSliceOfType(slRectangle, root)
   sl->X = x * 40 + randint(scatter)
   sl->Y = y * 40 + randint(scatter)
   DIM sizerange as integer = small(scatter * 2, 36)
   sl->Width = 20 + randint(sizerange) - sizerange \ 2
   sl->Height = 20 + randint(sizerange) - sizerange \ 2
   'default set_plank_state makes the SELECTABLE rectanges invisible, so put it on top of another one!
   ChangeRectangleSlice sl, , uiDisabledItem * -1 - 1
   sl->Lookup = SL_PLANK_HOLDER
   DIM highlight as Slice ptr = NewSliceOfType(slRectangle, sl)
   ChangeRectangleSlice highlight, , , , borderNone
   highlight->Fill = YES
   highlight->Visible = NO
   highlight->Lookup = SL_PLANK_MENU_SELECTABLE
  NEXT
 NEXT
END SUB

SUB draw_effective_points(ps as PlankState, axis as integer, d as integer)
 REDIM planks(any) as Slice Ptr
 find_all_planks ps, ps.m, planks()

 DIM as PlankViewpoint viewpoint = PlankViewpoint(ps.cur, axis, d)

 FOR i as integer = 0 TO UBOUND(planks)
  DIM pnt as FwdSide
  IF viewpoint.plank_effective_pos(pnt, planks(i)) = NO THEN CONTINUE FOR
  DIM effpos as XYPair
  DIM byref origin as FwdSide = viewpoint.prev_center
  IF axis = 0 THEN
   effpos = XY((origin.fwd + pnt.fwd) * d, origin.side + pnt.side)
  ELSE
   effpos = XY(origin.side + pnt.side, (origin.fwd + pnt.fwd) * d)
  END IF
  rectangle effpos.x - 1, effpos.y - 1, 3, 3, findrgb(255,255,0), vpage
 NEXT i
END SUB

'Draw lines showing positions for slice centers which plank_menu_move_cursor
'would give equal preference
SUB draw_isoline(sl as Slice ptr, ps as PlankState, movex as integer, movey as integer)
 DIM sl_center as XYPair = sl->ScreenPos + sl->Size \ 2
 DIM edgelen as integer
 IF movex THEN edgelen = sl->Height ELSE edgelen = sl->Width
 DIM col as integer = findrgb(255,255,255)

 'DIM parabola_scale as double = 2. / large(8, edgelen) ^ 1.5

 FOR dist as integer = 15 TO 150 STEP 25
  DIM radius as double = dist  '(dist / 10)^0.5

  ' DIM as double semimajor = 1'large(8, ABS(sl->Width * movex) + ABS(sl->Height * movey))
  ' DIM as double semiminor = 1'large(8, ABS(sl->Width * movey) + ABS(sl->Height * movex))

  DIM as integer prev_side_width = large(16, ABS(sl->Width * movey) + ABS(sl->Height * movex))

  DIM as double directedness = 2. ^ 2

  DIM as double semimajor = 0.3333 ''large(16, ABS(sl->Width * movex) + ABS(sl->Height * movey))
  DIM as double semiminor = 1'large(16, ABS(sl->Width * movey) + ABS(sl->Height * movex))
  semimajor *= radius
  semiminor *= radius

  DIM as double angle = ATAN2(movex, movey)

  ' The center of the ellipse is 'radius' pixels in the move direction
  DIM as XYPair el_center = XY(movex * radius, movey * radius)

  el_center += sl_center

  ellipse vpages(vpage), el_center.x, el_center.y, semimajor, col, , semiminor, angle

  /'
  FOR side as integer = -100 TO 100
   DIM pnt as XYPair
   DIM as integer fwd

   fwd = dist - parabola_scale * large(0,(ABS(side) - 0)) ^ 2.5


   IF fwd <= 0 THEN CONTINUE FOR
   IF movex THEN
    pnt.x = fwd * movex
    pnt.y = side
   ELSE
    pnt.x = side
    pnt.y = fwd * movey
   END IF

   pnt += sl_center
   putpixel pnt.x, pnt.y, col, vpage
  NEXT
  '/
 NEXT

 DIM as integer axis, d
 IF movex < 0 THEN axis = 0 : d = -1
 IF movex > 0 THEN axis = 0 : d = 1
 IF movey < 0 THEN axis = 1 : d = -1
 IF movey > 0 THEN axis = 1 : d = 1
 draw_effective_points ps, axis, d
END SUB

SUB plankmenu_cursor_move_tests
 DIM as integer scatter = 14, percent = 70

 DIM root as Slice ptr = NewSliceOfType(slContainer)
 root->Fill = YES
 DIM ps as PlankState
 ps.m = root
 DIM update as bool = YES

 DIM as integer movex, movey

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF6) > 1 THEN slice_editor root, SL_COLLECT_EDITOR
  IF keyval(scPlus) > 1 THEN scatter += 1 : update = YES
  IF keyval(scMinus) > 1 THEN scatter -= 1 : update = YES
  IF keyval(scLeftCaret) > 1 THEN percent -= 1 : update = YES
  IF keyval(scRightCaret) > 1 THEN percent += 1 : update = YES
  IF update THEN
   plankmenu_tests_generate_grid root, scatter, percent
   ps.cur = 0
   update = NO
  END IF

  IF ps.cur THEN set_plank_state ps, ps.cur, plankNORMAL

  IF keyval(scShift) > 0 THEN
   ' movex = 0
   ' movey = 0
   IF keyval(ccLeft) > 0 THEN movex = -1 : movey = 0
   IF keyval(ccRight) > 0 THEN movex = 1 : movey = 0
   IF keyval(ccUp) > 0 THEN movey = -1 : movex = 0
   IF keyval(ccDown) > 0 THEN movey = 1 : movex = 0
  ELSE
   plank_menu_arrows(ps)
  END IF
  IF keyval(scSpace) > 0 THEN movex = 0 : movey = 0
  IF ps.cur THEN set_plank_state ps, ps.cur, plankSEL

  clearpage vpage
  DrawSlice root, vpage
  IF ps.cur ANDALSO (movex OR movey) THEN draw_isoline ps.cur, ps, movex, movey
  wrapprint "Scatter: " & scatter & " (+/-)  Present: " & percent & "% (</>) " & _
            "SHIFT+arrows: isolines (SPACE clears)", pLeft, pBottom, uilook(uiText), vpage
  setvispage vpage
  dowait

  IF ps.cur = 0 THEN ps.cur = top_left_plank(ps)  'Do after first draw
 LOOP
 DeleteSlice @root
END SUB

SUB HTTP_demo()
 DIM url as string = "http://rpg.hamsterrepublic.com/nightly-archive/"
 IF prompt_for_string(url, "URL to fetch?", 100) = NO THEN EXIT SUB
 DIM req as HTTPRequest
 HTTP_request(@req, url, "GET", NULL, 0)
 notification "failed=" & yesorno(req.failed) & " " & req.status & " - " & *req.status_string
 pop_warning *cast(zstring ptr, req.response)
 HTTP_Request_destroy(@req)
END SUB

EXTERN "C"
 'This global affects the behaviour of open_process, which is called by
 'safe_shell, run_and_get_output, and a few other places.
 EXTERN CreateProc_opts as integer
END EXTERN

SUB CreateProcess_tests()
#IFDEF __FB_WIN32__
 DIM menu(3) as string
 menu(0) = "Get HSpeak version (run_and_get_output)"
 menu(1) = "Run madplay and get output (run_and_get_output)"
 menu(2) = "Run madplay without output (safe_shell)"
 menu(3) = "CreateProcess: "
 DIM CPopts(9) as string
 CPopts(0) = "Normal/unmodified"
 CPopts(1) = "CREATE_NO_WINDOW"
 CPopts(2) = "DETACHED_PROCESS (no window)"
 CPopts(3) = "CREATE_NEW_CONSOLE"
 CPopts(4) = "No flags"
 CPopts(5) = "No flags, inactive window"
 CPopts(6) = "No flags, inactive minimised"
 CPopts(7) = "No flags, active minimised"
 CPopts(8) = "CREATE_NO_WINDOW and no I/O"
 CPopts(9) = "DETACHED_PROCESS and no I/O"

 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 DIM madplay as string = find_helper_app("madplay")
 ERROR_IF(madplay = "", missing_helper_message("madplay"))
 DIM hspeak as string = find_helper_app("hspeak")
 ERROR_IF(hspeak = "", missing_helper_message("hspeak"))

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  usemenu state
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0
     notification "hspeak version '" &  get_hspeak_version(hspeak) & "'"
    CASE 1
     DIM as string outp, errp
     DIM errlvl as integer = run_and_get_output(madplay & " -V", outp, errp)
     IF errlvl = 0 THEN
      notification "Test output: " & outp & !"\n-----------------------\n" & errp
     ELSE
      notification "Running madplay failed, " & errlvl & !"\n" & outp & !"\n" & errp
     END IF
    CASE 2
     DIM handle as ProcessHandle
     handle = open_process(madplay, "-q", YES, NO)
     DIM ran as bool = handle <> 0
     VAR errlvl = wait_for_process(@handle, 2500)
     IF ran THEN
      notification iif(errlvl=2, "Success", "Unexpected error " & errlvl)
     ELSE
      notification "Couldn't start"
     END IF
   END SELECT
  END IF
  IF state.pt = 3 THEN intgrabber CreateProc_opts, 0, 8
  menu(3) = "CreateProcess: " & CPopts(CreateProc_opts)

  clearpage vpage
  standardmenu menu(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
#ENDIF
END SUB
