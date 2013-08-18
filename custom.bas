'OHRRPGCE CUSTOM - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
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

'FIXME: add header files for these declarations
DECLARE SUB importbmp (f as string, cap as string, byref count as integer)
DECLARE SUB vehicles ()
DECLARE SUB scriptman ()
DECLARE SUB map_picker ()
DECLARE SUB sprite (byval xw as integer, byval yw as integer, byref sets as integer, byval perset as integer, byval soff as integer, info() as string, byval zoom as integer, byval fileset as integer, byval fullset as integer=NO, byval cursor_start as integer=0, byval cursor_top as integer=0)
DECLARE SUB importsong ()
DECLARE SUB importsfx ()
DECLARE SUB gendata ()
DECLARE SUB item_editor ()
DECLARE SUB formation_editor ()
DECLARE SUB enemydata ()
DECLARE SUB hero_editor ()
DECLARE SUB text_box_editor ()
DECLARE SUB maptile ()
DECLARE SUB compile_andor_import_scripts (f as string)

'Local function declarations
DECLARE FUNCTION newRPGfile (templatefile as string, newrpg as string) as integer
DECLARE FUNCTION makeworkingdir () as integer
DECLARE FUNCTION handle_dirty_workingdir () as integer
DECLARE SUB secret_menu ()
DECLARE SUB condition_test_menu ()
DECLARE SUB quad_transforms_menu ()
DECLARE SUB arbitrary_sprite_editor ()
DECLARE SUB text_test_menu ()
DECLARE SUB font_test_menu ()
DECLARE SUB resolution_menu ()

DECLARE SUB shopdata ()
DECLARE SUB shop_stuff_edit (byval shop_id as integer, byref thing_total as integer)
DECLARE SUB shop_save_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
DECLARE SUB shop_load_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
DECLARE SUB update_shop_stuff_menu (byref stuf as ShopStuffState, stufbuf() as integer, byval thing_total as integer)
DECLARE SUB update_shop_stuff_type(byref stuf as ShopStuffState, stufbuf() as integer, byval reset_name_and_price as integer=NO)
DECLARE SUB shop_menu_update(byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_save (byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_load (byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_add_new (shopst as ShopEditState)

DECLARE SUB cleanupfiles ()
DECLARE SUB cleanup_and_terminate ()
DECLARE SUB import_scripts_and_terminate (scriptfile as string)
DECLARE SUB prompt_for_password()
DECLARE SUB prompt_for_save_and_quit()
DECLARE SUB choose_rpg_to_open ()
DECLARE SUB main_editor_menu()
DECLARE SUB gfx_editor_menu()

'Global variables
REDIM gen(360)
DIM gen_reld_doc as DocPtr
REDIM buffer(16384)
REDIM master(255) as RGBcolor
REDIM uilook(uiColors)
DIM statnames() as string
REDIM herotags(59) as HeroTagsCache
REDIM itemtags(maxMaxItems) as ItemTagsCache
REDIM lookup1_bin_cache(-1 TO -1) as TriggerData
DIM joy(4) as integer
DIM vpage as integer = 0
DIM dpage as integer = 1
DIM activepalette as integer = -1
DIM fadestate as integer

'FIXME: too many directory variables! Clean this nonsense up
DIM game as string
DIM sourcerpg as string
DIM exename as string
DIM homedir as string
DIM workingdir as string
DIM app_dir as string

DIM slave_channel as IPCChannel = NULL_CHANNEL
DIM slave_process as ProcessHandle = 0

EXTERN running_as_slave as integer
DIM running_as_slave as integer = NO  'This is just for the benefit of gfx_sdl

'Local variables (declaring these up here is often necessary due to gosubs)
DIM scriptfile as string
DIM archinym as string
DIM SHARED nocleanup as integer = NO

'--Startup

'Note: On Android exename is "sdl" and exepath is "" (currently unimplemented in FB and meaningless for an app anyway)

#IFDEF __FB_ANDROID__
 log_dir = CURDIR & SLASH
#ENDIF

exename = trimextension(trimpath(COMMAND(0)))

'why do we use different temp dirs in game and custom?
set_homedir

app_dir = exepath  'Note that exepath is a FreeBasic builtin, and not derived from the above exename

#IFDEF __FB_DARWIN__
 'Bundled apps have starting current directory equal to the location of the bundle, but exepath points inside
 IF RIGHT(exepath, 19) = ".app/Contents/MacOS" THEN
  data_dir = parentdir(exepath, 1) + "Resources"
  app_dir = parentdir(exepath, 3)
 END IF
 IF app_dir = "/Applications/" THEN
  app_dir = ENVIRON("HOME") & SLASH & "Documents"
  CHDIR app_dir
 END IF
#ENDIF

'temporarily set current directory, will be changed to game directory later if writable
orig_dir = CURDIR()
IF diriswriteable(app_dir) THEN
 'When CUSTOM is installed read-write, work in CUSTOM's folder
 CHDIR app_dir
ELSE
 'If CUSTOM is installed read-only, use your home dir as the default
 CHDIR homedir
END IF

'Start debug file as soon as the directory is set
start_new_debug
debuginfo long_version & build_info & " " & systeminfo
debuginfo DATE & " " & TIME

'seed the random number generator
mersenne_twister TIMER

'IF NOT isdir(settings_dir) THEN makedir settings_dir
set_settings_dir
tmpdir = settings_dir & SLASH
IF NOT isdir(tmpdir) THEN
 IF makedir(tmpdir) <> 0 THEN fatalerror "Unable to create temp directory " & tmpdir
END IF

processcommandline

load_default_master_palette master()
DefaultUIColors uilook()
REDIM current_font(1023) as integer
getdefaultfont current_font()

setmodex
debuginfo musicbackendinfo  'Preliminary info before initialising backend
setwindowtitle "O.H.R.RPG.C.E"
setpal master()
setfont current_font()
textcolor uilook(uiText), 0

'Cleanups up working.tmp if existing; requires graphics up and running
workingdir = tmpdir & "working.tmp"
IF makeworkingdir() = NO THEN cleanup_and_terminate

FOR i as integer = 1 TO UBOUND(cmdline_args)
 DIM arg as string
 arg = absolute_with_orig_path(cmdline_args(i))
 DIM extn as string = LCASE(justextension(arg))

 IF (extn = "hs" OR extn = "hss" OR extn = "txt") AND isfile(arg) THEN
  scriptfile = arg
  CONTINUE FOR
 ELSEIF (extn = "rpg" AND isfile(arg)) ORELSE isdir(arg) THEN
  sourcerpg = arg
  game = trimextension(trimpath(sourcerpg))
 ELSE
  visible_debug !"File not found/invalid option:\n" & cmdline_args(i)
 END IF
NEXT
IF game = "" THEN
 scriptfile = ""
 choose_rpg_to_open()
END IF

IF NOT is_absolute_path(sourcerpg) THEN sourcerpg = absolute_path(sourcerpg)

DIM dir_to_change_into as string = trimfilename(sourcerpg)

end_debug
IF dir_to_change_into <> "" ANDALSO diriswriteable(dir_to_change_into) THEN
 CHDIR dir_to_change_into
END IF
'otherwise, keep current directory as it was, net effect: it is the same as in Game

start_new_debug
debuginfo long_version & build_info
debuginfo "Runtime info: " & gfxbackendinfo & "  " & musicbackendinfo & "  " & systeminfo

'For getdisplayname
copylump sourcerpg, "archinym.lmp", workingdir, YES

debuginfo "Editing game " & sourcerpg & " (" & getdisplayname(" ") & ") " & DATE & " " & TIME
setwindowtitle "O.H.R.RPG.C.E - " + trimpath(sourcerpg)

'--set game according to the archinym
copylump sourcerpg, "archinym.lmp", workingdir, YES
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
setvispage vpage

touchfile workingdir + SLASH + "__danger.tmp"
IF isdir(sourcerpg) THEN
 'work on an unlumped RPG file. Don't take hidden files
 copyfiles sourcerpg, workingdir
ELSE
 unlump sourcerpg, workingdir + SLASH
END IF
safekill workingdir + SLASH + "__danger.tmp"

'Perform additional checks for future rpg files or corruption
rpg_sanity_checks

'upgrade obsolete RPG files
upgrade

'Load the game's palette, uicolors, font
activepalette = gen(genMasterPal)
loadpalette master(), activepalette
setpal master()
LoadUIColors uilook(), activepalette
clearpage dpage
clearpage vpage
xbload game + ".fnt", current_font(), "Font not loaded"
setfont current_font()

IF scriptfile <> "" THEN import_scripts_and_terminate scriptfile

loadglobalstrings
getstatnames statnames()
load_special_tag_caches
load_lookup1_bin lookup1_bin_cache()

setupmusic

'From here on, preserve working.tmp if something goes wrong
cleanup_on_error = NO

main_editor_menu
'Execution ends inside main_editor_menu
'=======================================================================

SUB main_editor_menu()
 DIM menu(21) as string
 DIM menu_display(UBOUND(menu)) as string
 
 menu(0) = "Edit Graphics"
 menu(1) = "Edit Map Data"
 menu(2) = "Edit Global Text Strings"
 menu(3) = "Edit Heroes"
 menu(4) = "Edit Enemies"
 menu(5) = "Edit Attacks"
 menu(6) = "Edit Items"
 menu(7) = "Edit Shops"
 menu(8) = "Edit Battle Formations"
 menu(9) = "Edit Text Boxes"
 menu(10) = "Edit Menus"
 menu(11) = "Edit Vehicles"
 menu(12) = "Edit Tag Names"
 menu(13) = "Import Music"
 menu(14) = "Import Sound Effects"
 menu(15) = "Edit Font"
 menu(16) = "Edit General Game Data"
 menu(17) = "Script Management"
 menu(18) = "Edit Slice Collections"
 menu(19) = "Distribute Game"
 menu(20) = "Test Game"
 menu(21) = "Quit Editing"
 
 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  usemenu state
  IF keyval(scEsc) > 1 THEN
   prompt_for_save_and_quit
  END IF
  IF keyval(scF1) > 1 THEN
   show_help "main"
  END IF

  IF select_by_typing(selectst) THEN
   IF selectst.buffer = "spam" THEN
    select_clear selectst
    secret_menu
   ELSE
    select_on_word_boundary_excluding menu(), selectst, state, "edit"
   END IF
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN gfx_editor_menu
   IF state.pt = 1 THEN map_picker
   IF state.pt = 2 THEN edit_global_text_strings
   IF state.pt = 3 THEN hero_editor
   IF state.pt = 4 THEN enemydata
   IF state.pt = 5 THEN attackdata
   IF state.pt = 6 THEN item_editor
   IF state.pt = 7 THEN shopdata
   IF state.pt = 8 THEN formation_editor
   IF state.pt = 9 THEN text_box_editor
   IF state.pt = 10 THEN menu_editor
   IF state.pt = 11 THEN vehicles
   IF state.pt = 12 THEN tags_menu
   IF state.pt = 13 THEN importsong
   IF state.pt = 14 THEN importsfx
   IF state.pt = 15 THEN fontedit current_font()
   IF state.pt = 16 THEN gendata
   IF state.pt = 17 THEN scriptman
   IF state.pt = 18 THEN slice_editor
   IF state.pt = 19 THEN distribute_game
   IF state.pt = 20 THEN spawn_game_menu
   IF state.pt = 21 THEN
    prompt_for_save_and_quit
   END IF
   '--always resave the .GEN lump after any menu
   xbsave game + ".gen", gen(), 1000
  END IF
 
  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage

  textcolor uilook(uiSelectedDisabled), 0
  printstr version_code, 0, 176, dpage
  printstr version_build, 0, 184, dpage
  textcolor uilook(uiText), 0
  printstr "Press F1 for help on any menu!", 0, 192, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

END SUB

SUB gfx_editor_menu()

 DIM menu(13) as string
 DIM menu_display(UBOUND(menu)) as string

 menu(0) = "Back to the main menu"
 menu(1) = "Edit Maptiles"
 menu(2) = "Draw Walkabout Graphics"
 menu(3) = "Draw Hero Graphics"
 menu(4) = "Draw Small Enemy Graphics  34x34"
 menu(5) = "Draw Medium Enemy Graphics 50x50"
 menu(6) = "Draw Big Enemy Graphics    80x80"
 menu(7) = "Draw Attacks"
 menu(8) = "Draw Weapons"
 menu(9) = "Draw Box Edges"
 menu(10) = "Draw Portrait Graphics"
 menu(11) = "Import/Export Screens"
 menu(12) = "Import/Export Full Maptile Sets"
 menu(13) = "Change User-Interface Colors"

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 DIM walkabout_frame_captions(7) as string = {"Up A","Up B","Right A","Right B","Down A","Down B","Left A","Left B"}
 DIM hero_frame_captions(7) as string = {"Standing","Stepping","Attack A","Attack B","Cast/Use","Hurt","Weak","Dead"}
 DIM enemy_frame_captions(0) as string = {"Enemy (facing right)"}
 DIM weapon_frame_captions(1) as string = {"Frame 1","Frame 2"}
 DIM attack_frame_captions(2) as string = {"First Frame","Middle Frame","Last Frame"}
 DIM box_border_captions(15) as string = {"Top Left Corner","Top Edge Left","Top Edge","Top Edge Right","Top Right Corner","Left Edge Top","Right Edge Top","Left Edge","Right Edge","Left Edge Bottom","Right Edge Bottom","Bottom Left Corner","Bottom Edge Left","Bottom Edge","Bottom Edge Right","Bottom Right Corner"}
 DIM portrait_captions(0) as string = {"Character Portrait"}

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scEsc) > 1 THEN
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
   IF state.pt = 2 THEN sprite 20, 20, gen(genMaxNPCPic),    8, 5, walkabout_frame_captions(),  4, 4
   IF state.pt = 3 THEN sprite 32, 40, gen(genMaxHeroPic),   8, 16, hero_frame_captions(), 4, 0
   IF state.pt = 4 THEN sprite 34, 34, gen(genMaxEnemy1Pic), 1, 2, enemy_frame_captions(), 4, 1
   IF state.pt = 5 THEN sprite 50, 50, gen(genMaxEnemy2Pic), 1, 4, enemy_frame_captions(), 2, 2
   IF state.pt = 6 THEN sprite 80, 80, gen(genMaxEnemy3Pic), 1, 10, enemy_frame_captions(), 2, 3
   IF state.pt = 7 THEN sprite 50, 50, gen(genMaxAttackPic), 3, 12, attack_frame_captions(), 2, 6
   IF state.pt = 8 THEN sprite 24, 24, gen(genMaxWeaponPic), 2, 2, weapon_frame_captions(), 4, 5
   IF state.pt = 9 THEN sprite 16, 16, gen(genMaxBoxBorder), 16, 7, box_border_captions(), 4, 7
   IF state.pt = 10 THEN sprite 50, 50, gen(genMaxPortrait), 1, 4, portrait_captions(), 2, 8
   IF state.pt = 11 THEN importbmp ".mxs", "screen", gen(genNumBackdrops)
   IF state.pt = 12 THEN
    gen(genMaxTile) = gen(genMaxTile) + 1
    importbmp ".til", "tileset", gen(genMaxTile)
    gen(genMaxTile) = gen(genMaxTile) - 1
    tileset_empty_cache
   END IF
   IF state.pt = 13 THEN ui_color_editor(activepalette)
   '--always resave the .GEN lump after any menu
   xbsave game + ".gen", gen(), 1000
  END IF
 
  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
 
  textcolor uilook(uiSelectedDisabled), 0
  printstr version_code, 0, 176, dpage
  printstr version_build, 0, 184, dpage
  textcolor uilook(uiText), 0
  printstr "Press F1 for help on any menu!", 0, 192, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

END SUB

SUB choose_rpg_to_open ()
 'This sub sets the globals: game and sourcerpg

 DIM state as MenuState
 state.pt = 1
 state.last = 2
 state.size = 20
 
 DIM chooserpg_menu(2) as string
 chooserpg_menu(0) = "CREATE NEW GAME"
 chooserpg_menu(1) = "LOAD EXISTING GAME"
 chooserpg_menu(2) = "EXIT PROGRAM"

 DIM result as string

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN cleanup_and_terminate
  usemenu state
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0
     game = inputfilename("Filename of New Game?", ".rpg", CURDIR, "input_file_new_game", , NO)
     IF game <> "" THEN
       IF NOT newRPGfile(finddatafile("ohrrpgce.new"), game & ".rpg") THEN cleanup_and_terminate
       sourcerpg = game & ".rpg"
       game = trimpath(game)
       EXIT DO
     END IF
    CASE 1
     sourcerpg = browse(7, "", "*.rpg", tmpdir, 0, "custom_browse_rpg")
     game = trimextension(trimpath(sourcerpg))
     IF game <> "" THEN EXIT DO
    CASE 2
     cleanup_and_terminate
   END SELECT
  END IF
 
  clearpage dpage
  standardmenu chooserpg_menu(), state, 0, 0, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
END SUB

SUB prompt_for_save_and_quit()

 xbsave game & ".gen", gen(), 1000

 DIM quit_menu(3) as string
 quit_menu(0) = "Continue editing"
 quit_menu(1) = "Save changes and continue editing"
 quit_menu(2) = "Save changes and quit"
 quit_menu(3) = "Discard changes and quit"
 clearkey(-1) 'stop firing esc's, if the user hit esc+pgup+pgdown
 
 DIM quitnow as integer
 quitnow = sublist(quit_menu(), "quit_and_save")
 IF keyval(-1) THEN '2nd quit request? Right away!
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
  setvispage 0
  dolumpfiles lumpfile
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
 'Uncomment to display the/a password
 'passcomment = getpassword
 setkeys YES
 DO
  setwait 55
  setkeys YES
  tog = tog XOR 1
  IF keyval(scEnter) > 1 THEN
   IF checkpassword(pas) THEN
    EXIT SUB
   ELSE
    cleanup_and_terminate
   END IF
  END IF
  strgrabber pas, 17
  clearpage dpage
  textcolor uilook(uiText), 0
  printstr "This game requires a password to edit", 0, 0, dpage
  printstr " Type it in and press ENTER", 0, 9, dpage
  textcolor uilook(uiSelectedItem + tog), 1
  printstr STRING(LEN(pas), "*"), 0, 20, dpage
  printstr passcomment, 0, 40, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB import_scripts_and_terminate (scriptfile as string)
 debuginfo "Importing scripts from " & scriptfile
 xbload game & ".gen", gen(), "general data is missing, RPG file corruption is likely"
 upgrade 'needed because it has not already happened because we are doing command-line import
 compile_andor_import_scripts absolute_with_orig_path(scriptfile)
 xbsave game & ".gen", gen(), 1000
 save_current_game
 cleanupfiles
 end_debug
 restoremode
 SYSTEM
END SUB

SUB cleanup_and_terminate ()
 IF slave_channel <> NULL_CHANNEL THEN
  channel_write_line(slave_channel, "Q ")
  #IFDEF __FB_WIN32__
   'On windows, can't delete workingdir until Game has closed the music. Not too serious though
   basic_textbox "Waiting for " & GAMEEXE & " to clean up...", uilook(uiText), vpage
   setvispage vpage
   IF channel_wait_for_msg(slave_channel, "Q", "", 2000) = 0 THEN
    basic_textbox "Waiting for " & GAMEEXE & " to clean up... giving up.", uilook(uiText), vpage
    setvispage vpage
    sleep 700
   END IF
  #ENDIF
  channel_close(slave_channel)
 END IF
 IF slave_process <> 0 THEN cleanup_process @slave_process
 closemusic
 'catch sprite leaks
 sprite_empty_cache
 palette16_empty_cache
 cleanup_global_reload_doc
 clear_binsize_cache
 IF keyval(-1) = 0 THEN
  clearpage vpage
  ' Don't let Spoonweaver's cat near your power cord!
  pop_warning "Don't forget to keep backup copies of your work! You never know when an unknown bug or a cat-induced hard-drive crash or a little brother might delete your files!", YES
 END IF
 cleanupfiles
 end_debug
 restoremode
 SYSTEM

END SUB

SUB cleanupfiles ()
 'WARNING: nocleanup is module-shared
 IF nocleanup = NO THEN killdir workingdir
END SUB

SUB shopdata ()
 DIM shopbuf(20) as integer

 DIM sbit(-1 TO 7) as string
 sbit(0) = "Buy"
 sbit(1) = "Sell"
 sbit(2) = "Hire"
 sbit(3) = "Inn"
 sbit(4) = "Equip"
 sbit(5) = "Save"
 sbit(6) = "Map"
 sbit(7) = "Team"

 DIM shopst as ShopEditState
 shopst.havestuf = NO

 shop_load shopst, shopbuf()
 shopst.st.last = 6
 shopst.st.size = 24

 DIM new_shop_id as integer
 
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "shop_main"
  IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 0 THEN cropafter shopst.id, gen(genMaxShop), 0, game + ".sho", 40
  usemenu shopst.st
  IF shopst.st.pt = 1 THEN
   '--only allow adding shops up to 99
   'FIXME: This is because of the limitation on remembering shop stock in the SAV format
   '       when the SAV format has changed, this limit can easily be lifted.
   new_shop_id = shopst.id
   IF intgrabber_with_addset(new_shop_id, 0, gen(genMaxShop), 99, "Shop") THEN
    shop_save shopst, shopbuf()
    shopst.id = new_shop_id
    IF shopst.id > gen(genMaxShop) THEN
     shop_add_new shopst
    END IF
    shop_load shopst, shopbuf()
   END IF
  END IF
  IF shopst.st.pt = 2 THEN
   strgrabber shopst.name, 15
   shopst.st.need_update = YES
  END IF
  IF enter_space_click(shopst.st) THEN
   IF shopst.st.pt = 0 THEN EXIT DO
   IF shopst.st.pt = 3 AND shopst.havestuf THEN
    shop_stuff_edit shopst.id, shopbuf(16)
   END IF
   IF shopst.st.pt = 4 THEN editbitset shopbuf(), 17, 7, sbit(): shopst.st.need_update = YES
   IF shopst.st.pt = 6 THEN
    shopst.menu(6) = "Inn Script: " & scriptbrowse_string(shopbuf(19), plottrigger, "Inn Plotscript")
   END IF
  END IF
  IF shopst.st.pt = 5 THEN
   IF intgrabber(shopbuf(18), 0, 32767) THEN shopst.st.need_update = YES
  END IF
  IF shopst.st.pt = 6 THEN
   IF scrintgrabber(shopbuf(19), 0, 0, scLeft, scRight, 1, plottrigger) THEN shopst.st.need_update = YES
  END IF
  
  IF shopst.st.need_update THEN
   shopst.st.need_update = NO
   shop_menu_update shopst, shopbuf()
  END IF
  
  clearpage dpage
  standardmenu shopst.menu(), shopst.st, shopst.shaded(), 0, 0, dpage 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 shop_save shopst, shopbuf()

END SUB

SUB shop_load (byref shopst as ShopEditState, shopbuf() as integer)
 setpicstuf shopbuf(), 40, -1
 loadset game & ".sho", shopst.id, 0
 shopst.name = readbadbinstring(shopbuf(), 0, 15)
 shopst.st.need_update = YES
END SUB

SUB shop_save (byref shopst as ShopEditState, shopbuf() as integer)
 shopbuf(16) = small(shopbuf(16), 49)
 writebadbinstring shopst.name, shopbuf(), 0, 15
 setpicstuf shopbuf(), 40, -1
 storeset game & ".sho", shopst.id, 0
END SUB

SUB shop_menu_update(byref shopst as ShopEditState, shopbuf() as integer)
 shopst.menu(0) = "Return to Main Menu"
 shopst.menu(1) = CHR(27) & " Shop " & shopst.id & " of " & gen(genMaxShop) & CHR(26)
 shopst.menu(2) = "Name: " & shopst.name
 shopst.menu(3) = "Edit Available Stuff..."
 shopst.menu(4) = "Select Shop Menu Items..."
 shopst.menu(5) = "Inn Price: " & shopbuf(18)
 IF readbit(shopbuf(), 17, 3) = 0 THEN shopst.menu(5) = "Inn Price: N/A"
 shopst.menu(6) = "Inn Script: " & scriptname(shopbuf(19))
 IF readbit(shopbuf(), 17, 0) ORELSE readbit(shopbuf(), 17, 1) ORELSE readbit(shopbuf(), 17, 2) THEN
  shopst.havestuf = YES
  shopst.shaded(3) = NO
 ELSE
  shopst.havestuf = NO
  ' Grey out "Edit available stuff"
  shopst.shaded(3) = YES
 END IF
END SUB

SUB shop_add_new (shopst as ShopEditState)
  DIM menu(2) as string
  DIM shoptocopy as integer = 0
  DIM state as MenuState
  state.last = UBOUND(menu)
  state.size = 24
  state.pt = 1

  state.need_update = YES
  setkeys
  DO
    setwait 55
    setkeys
    IF keyval(scESC) > 1 THEN  'cancel
      shopst.id -= 1
      EXIT DO
    END IF
    IF keyval(scF1) > 1 THEN show_help "shop_new"
    usemenu state
    IF state.pt = 2 THEN
      IF intgrabber(shoptocopy, 0, gen(genMaxShop)) THEN state.need_update = YES
    END IF
    IF state.need_update THEN
      state.need_update = NO
      menu(0) = "Cancel"
      menu(1) = "New Blank Shop"
      menu(2) = "Copy of Shop " & shoptocopy & " " & readshopname(shoptocopy) 'readbadbinstring(shopbuf(), 0, 15, 0)
    END IF
    IF enter_space_click(state) THEN
      DIM shopbuf(19) as integer
      DIM stufbuf(curbinsize(binSTF) \ 2 - 1) as integer
      SELECT CASE state.pt
        CASE 0 ' cancel
          shopst.id -= 1
          EXIT DO
        CASE 1 ' blank
          gen(genMaxShop) += 1
          '--Create a new shop record
          flusharray shopbuf()
          '--Create a new shop stuff record
          flusharray stufbuf()
          'FIXME: load the name and price for first shop item
          stufbuf(19) = -1  'Default in-stock to infinite
        CASE 2 ' copy
          gen(genMaxShop) += 1
          loadrecord shopbuf(), game + ".sho", 20, shoptocopy
          loadrecord stufbuf(), game + ".stf", 50 * getbinsize(binSTF) \ 2, shoptocopy
      END SELECT
      storerecord shopbuf(), game + ".sho", 20, shopst.id
      'Save all 50 shop stock items at once
      storerecord stufbuf(), game + ".stf", 50 * getbinsize(binSTF) \ 2, shopst.id
      EXIT DO
    END IF

    clearpage vpage
    standardmenu menu(), state, 0, 0, vpage
    setvispage vpage
    dowait
  LOOP
END SUB

SUB shop_stuff_edit (byval shop_id as integer, byref thing_total as integer)
 DIM stuf as ShopStuffState

 stuf.max(3) = 1
 stuf.min(5) = -1
 stuf.max(5) = 9999
 FOR i as integer = 6 TO 9
  stuf.min(i) = -max_tag()
  stuf.max(i) = max_tag()
 NEXT i
 stuf.min(10) = -32767
 stuf.max(10) = 32767
 FOR i as integer = 11 TO 17 STEP 2
  stuf.max(i) = gen(genMaxItem)
  stuf.min(i) = -1
  stuf.max(i + 1) = 999
  stuf.min(i + 1) = 1
 NEXT

 stuf.min(20) = -32767
 stuf.max(20) = 32767
 stuf.max(21) = gen(genMaxItem)
 stuf.min(21) = -1
 stuf.max(22) = 999
 stuf.min(22) = 1

 stuf.thing = 0
 stuf.default_thingname = "" 'FIXME: this isn't updated anywhere yet
 stuf.thingname = ""
 
 stuf.st.pt = 0
 stuf.st.last = 2
 stuf.st.size = 24
 
 DIM stufbuf(curbinsize(binSTF) \ 2 - 1) as integer
 shop_load_stf shop_id, stuf, stufbuf()
 
 update_shop_stuff_type stuf, stufbuf()
 update_shop_stuff_menu stuf, stufbuf(), thing_total
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "shop_stuff"
  IF stuf.st.pt = 0 ANDALSO enter_space_click(stuf.st) THEN EXIT DO

  SELECT CASE stuf.st.pt
   CASE 1 'browse shop stuff
    DIM newthing as integer = stuf.thing
    IF intgrabber_with_addset(newthing, 0, thing_total, 49, "Shop Thing") THEN
     shop_save_stf shop_id, stuf, stufbuf()
     stuf.thing = newthing
     IF stuf.thing > thing_total THEN
      thing_total = stuf.thing
      flusharray stufbuf(), dimbinsize(binSTF), 0
      stufbuf(19) = -1 ' When adding new stuff, default in-stock to infinite
      update_shop_stuff_type stuf, stufbuf(), YES  ' load the name and price
      shop_save_stf shop_id, stuf, stufbuf()
     END IF
     shop_load_stf shop_id, stuf, stufbuf()
     update_shop_stuff_type stuf, stufbuf()
     stuf.st.need_update = YES
    END IF
   CASE 2 'name
    IF strgrabber(stuf.thingname, 16) THEN stuf.st.need_update = YES
   CASE 3 TO 4 'type and ID
    IF intgrabber(stufbuf(17 + stuf.st.pt - 3), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN
     stuf.st.need_update = YES
     update_shop_stuff_type stuf, stufbuf(), YES
    END IF
   CASE 6 TO 7 '--condition tags
    IF tag_grabber(stufbuf(17 + stuf.st.pt - 3), , , YES) THEN stuf.st.need_update = YES
   CASE 8 TO 9 '--set tags
    IF tag_grabber(stufbuf(17 + stuf.st.pt - 3), , , NO) THEN stuf.st.need_update = YES
   CASE 11 '--must trade in item 1 type
    IF zintgrabber(stufbuf(25), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
   CASE 13, 15, 17 '--must trade in item 2+ types
    IF zintgrabber(stufbuf(18 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
   CASE 12, 14, 16, 18 '--trade in item amounts
    stufbuf(18 + stuf.st.pt) += 1
    IF intgrabber(stufbuf(18 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    stufbuf(18 + stuf.st.pt) -= 1
   CASE 19, 20 '--sell type, price
    IF intgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    IF (stufbuf(26) < 0 OR stufbuf(26) > 3) AND stufbuf(17) <> 1 THEN stufbuf(26) = 0
   CASE 21 '--trade in for
    IF zintgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
   CASE 22 '--trade in for amount
    stufbuf(7 + stuf.st.pt) += 1
    IF intgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    stufbuf(7 + stuf.st.pt) -= 1
   CASE ELSE
    IF intgrabber(stufbuf(17 + stuf.st.pt - 3), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN
     stuf.st.need_update = YES
    END IF
  END SELECT

  usemenu stuf.st

  IF stuf.st.need_update THEN
   update_shop_stuff_menu stuf, stufbuf(), thing_total
  END IF
   
  clearpage dpage
  standardmenu stuf.menu(), stuf.st, 0, 0, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 shop_save_stf shop_id, stuf, stufbuf()

END SUB

SUB update_shop_stuff_type(byref stuf as ShopStuffState, stufbuf() as integer, byval reset_name_and_price as integer=NO)
 '--Re-load default names and default prices
 SELECT CASE stufbuf(17)
  CASE 0' This is an item
   DIM item_tmp(dimbinsize(binITM)) as integer
   loaditemdata item_tmp(), stufbuf(18)
   stuf.item_value = item_tmp(46)
   IF reset_name_and_price THEN
    stuf.thingname = load_item_name(stufbuf(18),1,1)
    stufbuf(24) = stuf.item_value ' default buy price
    stufbuf(27) = stuf.item_value \ 2 ' default sell price
   END IF
   stuf.st.last = 22
   stuf.max(4) = gen(genMaxItem)
   IF stufbuf(18) > stuf.max(4) THEN stufbuf(18) = 0
   stuf.min(19) = 0
   stuf.max(19) = 3 ' Item sell-type
  CASE 1
   DIM her as HeroDef
   IF reset_name_and_price THEN
    loadherodata her, stufbuf(18)
    stuf.thingname = her.name
    stufbuf(24) = 0 ' default buy price
    stufbuf(27) = 0 ' default sell price
   END IF
   stuf.item_value = 0
   stuf.st.last = 19
   stuf.max(4) = gen(genMaxHero)
   IF stufbuf(18) > gen(genMaxHero) THEN stufbuf(18) = 0
   stuf.min(19) = -1
   stuf.max(19) = gen(genMaxLevel) ' Hero experience level
  CASE ELSE
   'Type 2 was script which was never supported but was allowed for data entry in some ancient versions
   stuf.thingname = "Unsupported"
 END SELECT
END SUB

SUB update_shop_stuff_menu (byref stuf as ShopStuffState, stufbuf() as integer, byval thing_total as integer)

 stuf.menu(0) = "Previous Menu"
 stuf.menu(1) = CHR(27) & "Shop Thing " & stuf.thing & " of " & thing_total & CHR(26)
 stuf.menu(2) = "Name: " & stuf.thingname
 stuf.menu(3) = "Type: "

 DIM typename as string
 SELECT CASE stufbuf(17)
  CASE 0: typename = "Item"
  CASE 1: typename = "Hero"
  CASE ELSE: typename = "???"
 END SELECT
 stuf.menu(3) &= typename

 stuf.menu(4) = typename & " ID: " & stufbuf(18) & " " & stuf.default_thingname
 
 SELECT CASE stufbuf(19)
  CASE IS > 0: stuf.menu(5) = "In Stock: " & stufbuf(19)
  CASE 0: stuf.menu(5) = "In Stock: None"
  CASE -1: stuf.menu(5) = "In Stock: Infinite"
  CASE ELSE: stuf.menu(5) = stufbuf(19) & " ???" 
 END SELECT

 stuf.menu(6) = tag_condition_caption(stufbuf(20), "Buy Require Tag", "Always")
 stuf.menu(7) = tag_condition_caption(stufbuf(21), "Sell Require Tag", "Always")
 stuf.menu(8) = tag_set_caption(stufbuf(22), "Buy Set Tag")
 stuf.menu(9) = tag_set_caption(stufbuf(23), "Sell Set Tag")
 stuf.menu(10) = "Cost: " & stufbuf(24) & " " & readglobalstring(32, "Money")
 IF stufbuf(17) = 0 AND stuf.item_value THEN  'item
  stuf.menu(10) &= " (" & CINT(100.0 * stufbuf(24) / stuf.item_value) & "% of Value)"
 END IF
 stuf.menu(11) = "Must Trade in " & (stufbuf(30) + 1) & " of: " & load_item_name(stufbuf(25),0,0)
 stuf.menu(12) = " (Change Amount)"
 stuf.menu(13) = "Must Trade in " & (stufbuf(32) + 1) & " of: " & load_item_name(stufbuf(31),0,0)
 stuf.menu(14) = " (Change Amount)"
 stuf.menu(15) = "Must Trade in " & (stufbuf(34) + 1) & " of: " & load_item_name(stufbuf(33),0,0)
 stuf.menu(16) = " (Change Amount)"
 stuf.menu(17) = "Must Trade in " & (stufbuf(36) + 1) & " of: " & load_item_name(stufbuf(35),0,0)
 stuf.menu(18) = " (Change Amount)"

 IF stufbuf(17) = 0 THEN

  SELECT CASE stufbuf(26)
   CASE 0: stuf.menu(19) = "Sell type: Don't Change Stock"
   CASE 1: stuf.menu(19) = "Sell type: Acquire Infinite Stock"
   CASE 2:
    stuf.menu(19) = "Sell type: Increment Stock"
    IF stufbuf(19) = -1 THEN
     stuf.menu(19) = "Sell type: Inc Stock (does nothing)"
    END IF
   CASE 3: stuf.menu(19) = "Sell type: Refuse to Buy"
   CASE ELSE: stuf.menu(19) = "Sell type: " & stufbuf(26) & " ???"
  END SELECT

  stuf.menu(20) = "Sell for: " & stufbuf(27) & " " & readglobalstring(32, "Money")
  IF stuf.item_value THEN
   stuf.menu(20) &= " (" & CINT(100.0 * stufbuf(27) / stuf.item_value) & "% of Value)"
  END IF
  stuf.menu(21) = "  and " & (stufbuf(29) + 1) & " of: " & load_item_name(stufbuf(28),0,0)
  stuf.menu(22) = " (Change Amount)"
 ELSE
  stuf.menu(19) = "Experience Level: "
  IF stufbuf(26) = -1 THEN
   stuf.menu(19) &= "default"
  ELSE
   stuf.menu(19) &= stufbuf(26)
  END IF
 END IF
 
 stuf.st.need_update = NO
END SUB

SUB shop_load_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
 flusharray stufbuf(), dimbinsize(binSTF), 0
 setpicstuf stufbuf(), getbinsize(binSTF), -1
 loadset game & ".stf", shop_id * 50 + stuf.thing, 0
 stuf.thingname = readbadbinstring(stufbuf(), 0, 16, 0)
 '---check for invalid data
 IF stufbuf(17) < 0 OR stufbuf(17) > 2 THEN stufbuf(17) = 0
 IF stufbuf(19) < -1 THEN stufbuf(19) = 0
 IF (stufbuf(26) < 0 OR stufbuf(26) > 3) AND stufbuf(17) <> 1 THEN stufbuf(26) = 0
 '--WIP Serendipity custom builds didn't flush shop records when upgrading properly
 FOR i as integer = 32 TO 41
  stufbuf(i) = large(stufbuf(i), 0)
 NEXT
END SUB

SUB shop_save_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
 writebadbinstring stuf.thingname, stufbuf(), 0, 16
 setpicstuf stufbuf(), getbinsize(binSTF), -1
 storeset game & ".stf", shop_id * 50 + stuf.thing, 0
END SUB

FUNCTION newRPGfile (templatefile as string, newrpg as string) as integer
 newRPGfile = 0 ' default return value 0 means failure
 IF newrpg = "" THEN EXIT FUNCTION
 textcolor uilook(uiSelectedDisabled), 0
 printstr "Please Wait...", 0, 40, vpage
 printstr "Creating RPG File", 0, 50, vpage
 setvispage vpage
 IF NOT isfile(templatefile) THEN
  printstr "Error: ohrrpgce.new not found", 0, 60, vpage
  printstr "Press Enter to quit", 0, 70, vpage
  setvispage vpage
  waitforanykey
  EXIT FUNCTION
 END IF
 writeablecopyfile templatefile, newrpg
 printstr "Unlumping", 0, 60, vpage
 setvispage vpage 'refresh
 unlump newrpg, workingdir + SLASH
 '--create archinym information lump
 DIM fh as integer = FREEFILE
 OPEN workingdir + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, "ohrrpgce"
 PRINT #fh, version
 CLOSE #fh
 printstr "Finalumping", 0, 80, vpage
 setvispage vpage 'refresh
 '--re-lump files as NEW rpg file
 dolumpfiles newrpg
 newRPGfile = -1 'return true for success
END FUNCTION

'Try to delete everything in workingdir
FUNCTION empty_workingdir () as integer
 DIM filelist() as string
 findfiles workingdir, ALLFILES, fileTypeFile, NO, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  DIM fname as string = workingdir + SLASH + filelist(i)
  safekill fname
  IF isfile(fname) THEN
   notification "Could not clean up " & workingdir & !"\nYou may have to manually delete its contents."
   RETURN NO
  END IF
 NEXT
 RETURN YES
END FUNCTION

'Returns true on success, false if want to cleanup_and_terminate
FUNCTION makeworkingdir () as integer
 IF NOT isdir(workingdir) THEN
  makedir workingdir
  RETURN YES
 ELSE
  'Does this look like a game, or should we just delete it?
  DIM filelist() as string
  findfiles workingdir, ALLFILES, fileTypeFile, NO, filelist()
  IF UBOUND(filelist) <= 5 THEN
   'Just some stray files that refused to delete last time
   RETURN empty_workingdir
  END IF

  'Recover from an old crash
  RETURN handle_dirty_workingdir
 END IF
END FUNCTION

'Returns true on success, false if want to cleanup_and_terminate
FUNCTION handle_dirty_workingdir () as integer
 DIM cleanup_menu(2) as string
 cleanup_menu(0) = "DO NOTHING"
 cleanup_menu(1) = "RECOVER IT"
 cleanup_menu(2) = "ERASE IT"

 DIM state as MenuState
 state.size = 8
 state.last = UBOUND(cleanup_menu)

 DIM index as integer = 0
 DIM destfile as string
 DO
  destfile = "recovered" & index & ".bak"
  IF NOT isfile(destfile) THEN EXIT DO
  index += 1
 LOOP

 setkeys
 DO
  setwait 55
  setkeys
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = 1 THEN
    IF isfile(workingdir + SLASH + "__danger.tmp") THEN
     textcolor uilook(uiSelectedItem), uilook(uiHighlight) 'FIXME: new uilook for warning text colors?
     printstr "Data is corrupt, not safe to relump", 0, 100, vpage
     setvispage vpage
     waitforanykey
    ELSE '---END UNSAFE
     printstr "Saving as " + destfile, 0, 180, vpage
     printstr "LUMPING DATA: please wait...", 0, 190, vpage
     setvispage vpage
     '--re-lump recovered files as BAK file
     dolumpfiles destfile
     clearpage vpage
     basic_textbox "The recovered data has been saved. If " + CUSTOMEXE + " crashed last time you " _
                   "ran it and you lost work, you may be able to recover it. Make a backup " _
                   "copy of your RPG and then rename " + destfile + !" to gamename.rpg\n" _
                   "If you have questions, ask ohrrpgce-crash@HamsterRepublic.com", _
                   uilook(uiText), vpage
     setvispage vpage
     waitforanykey
     empty_workingdir
     RETURN YES  'continue
    END IF '---END RELUMP
   END IF
   IF state.pt = 2 THEN empty_workingdir : RETURN YES  'continue
   IF state.pt = 0 THEN nocleanup = YES : RETURN NO  'quit
  END IF

  clearpage dpage
  basic_textbox !"A game was found unlumped.\n" _
                 "This may mean that " + CUSTOMEXE + " crashed last time you used it, or it may mean " _
                 "that another copy of " + CUSTOMEXE + " is already running in the background.", _
                 uilook(uiText), dpage
  standardmenu cleanup_menu(), state, 16, 150, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END FUNCTION

SUB secret_menu ()
 DIM menu(...) as string = {"Reload Editor", "Editor Editor", "Conditions and More Tests", "Transformed Quads", "Sprite editor with arbitrary sizes", "Text tests", "Font tests", "Stat Growth Chart", "Resolution Menu"}
 DIM st as MenuState
 st.size = 24
 st.last = UBOUND(menu)

 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF enter_space_click(st) THEN
   IF st.pt = 0 THEN reload_editor
   IF st.pt = 1 THEN editor_editor
   IF st.pt = 2 THEN condition_test_menu
   IF st.pt = 3 THEN quad_transforms_menu
   IF st.pt = 4 THEN arbitrary_sprite_editor
   IF st.pt = 5 THEN text_test_menu
   IF st.pt = 6 THEN font_test_menu
   IF st.pt = 7 THEN stat_growth_chart
   IF st.pt = 8 THEN resolution_menu
  END IF
  usemenu st
  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
END SUB

SUB resolution_menu ()
 DIM menu(...) as string = {"Width=", "Height="}
 DIM st as MenuState
 st.size = 24
 st.last = UBOUND(menu)

 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  SELECT CASE st.pt
   CASE 0: intgrabber(gen(genResolutionX), 0, 320)
   CASE 1: intgrabber(gen(genResolutionY), 0, 200)
  END SELECT
  usemenu st
  menu(0) = "Width: " & gen(genResolutionX)
  menu(1) = "Height:" & gen(genResolutionY)
  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB arbitrary_sprite_editor ()
 DIM tempsets as integer = 0
 DIM tempcaptions(15) as string
 FOR i as integer = 0 to UBOUND(tempcaptions)
  tempcaptions(i) = "frame" & i
 NEXT i
 DIM size as XYPair
 size.x = 20
 size.y = 20
 DIM framecount as integer = 8
 DIM crappy_screenpage_lines as integer
 DIM zoom as integer = 2

 DIM menu(...) as string = {"Width=", "Height=", "Framecount=", "Zoom=", "Sets=", "Start Editing..."}
 DIM st as MenuState
 st.size = 24
 st.last = UBOUND(menu)
 st.need_update = YES

 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  SELECT CASE st.pt
   CASE 0: IF intgrabber(size.x, 0, 160) THEN st.need_update = YES
   CASE 1: IF intgrabber(size.y, 0, 160) THEN st.need_update = YES
   CASE 2: IF intgrabber(framecount, 0, 16) THEN st.need_update = YES
   CASE 3: IF intgrabber(zoom, 0, 4) THEN st.need_update = YES
   CASE 4: IF intgrabber(tempsets, 0, 32000) THEN st.need_update = YES
  END SELECT
  IF enter_space_click(st) THEN
   IF st.pt = 5 THEN
    crappy_screenpage_lines = ceiling(size.x * size.y * framecount / 2 / 320)
    sprite size.x, size.y, tempsets, framecount, crappy_screenpage_lines, tempcaptions(), zoom, -1
    IF isfile(game & ".pt-1") THEN
     debug "Leaving behind """ & game & ".pt-1"""
    END IF
   END IF
  END IF
  usemenu st
  IF st.need_update THEN
   menu(0) = "Width: " & size.x
   menu(1) = "Height:" & size.y
   menu(2) = "Framecount: " & framecount
   menu(3) = "Zoom: " & zoom
   menu(4) = "Sets: " & tempsets
   st.need_update = NO
  END IF
  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys

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
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "condition_test"
  tmp = 0
  IF st.pt = 0 THEN
   IF enter_space_click(st) THEN EXIT DO
  ELSEIF st.pt = 2 THEN
   tmp = cond_grabber(cond1, YES , NO)
  ELSEIF st.pt = 3 THEN
   tmp = cond_grabber(cond2, NO, NO)
  ELSEIF st.pt = 5 THEN
   tmp = cond_grabber(cond3, YES, YES)
  ELSEIF st.pt = 6 THEN
   tmp = cond_grabber(cond4, NO, YES)
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

#include "matrixMath.bi"
#include "gfx_newRenderPlan.bi"
#include "gfx.bi"

declare sub surface_export_bmp24 (f as string, byval surf as Surface Ptr)

SUB quad_transforms_menu ()
 DIM menu(...) as string = {"Arrows: scale X and Y", "<, >: change angle", "[, ]: change sprite"}
 DIM st as MenuState
 st.last = 2
 st.size = 22
 st.need_update = YES
 
 DIM spritemode as integer = -1

 DIM testframe as Frame ptr
 DIM vertices(3) as Float3

 DIM angle as single
 DIM scale as Float2 = (2.0, 2.0)
 DIM position as Float2 = (150, 50)

 'This is used to copy data from vpages(vpage) to vpage32 
 DIM vpage8 as Surface ptr
 gfx_surfaceCreate(320, 200, SF_8bit, SU_Source, @vpage8)

 'This is the actual render Surface
 DIM vpage32 as Surface ptr
 gfx_surfaceCreate(320, 200, SF_32bit, SU_RenderTarget, @vpage32)

 DIM as double drawtime, pagecopytime

 DIM spriteSurface as Surface ptr

 DIM masterPalette as BackendPalette ptr
 gfx_paletteCreate(@masterPalette)
 memcpy(@masterPalette->col(0), @master(0), 256 * 4)
 'Set each colour in the master palette to opaque.
 FOR i as integer = 0 TO 255
  masterPalette->col(i).a = 255
 NEXT

 DO
  setwait 55
  
  if st.need_update then
   if spritemode < -1 then spritemode = 8
   if spritemode > 8 then spritemode = -1

   frame_unload @testframe

   select case spritemode
    case 0 to 8
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

   gfx_surfaceDestroy( spriteSurface )
   gfx_surfaceCreate( testframe->w, testframe->h, SF_8bit, SU_Source, @spriteSurface )
   memcpy(spriteSurface->pPaletteData, testframe->image, testframe->w * testframe->h - 1)
   gfx_surfaceUpdate( spriteSurface )

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
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scLeft)  THEN scale.x -= 0.1
  IF keyval(scRight) THEN scale.x += 0.1
  IF keyval(scUp)    THEN scale.y -= 0.1
  IF keyval(scDown)  THEN scale.y += 0.1
  IF keyval(scLeftCaret)  THEN angle -= 0.1
  IF keyval(scRightCaret) THEN angle += 0.1
  IF keyval(scLeftBracket) > 1 THEN spritemode -= 1: st.need_update = YES
  IF keyval(scRightBracket) > 1 THEN spritemode += 1: st.need_update = YES

  clearpage vpage
  standardmenu menu(), st, 0, 0, vpage
  frame_draw testframe, , 20, 50, 2, , vpages(vpage)  'drawn at 2x scale

  'Can only display the previous frame's time to draw, since we don't currently
  'have any functions to print text to surfaces
  printstr "Drawn in " & FIX(drawtime * 1000000) & " usec, pagecopytime = " & FIX(pagecopytime * 1000000) & " usec", 0, 190, vpage
  debug "Drawn in " & FIX(drawtime * 1000000) & " usec, pagecopytime = " & FIX(pagecopytime * 1000000) & " usec"

  'Copy from vpage (8 bit Frame) to a source Surface, and then from there to the render target surface
  memcpy(vpage8->pPaletteData, vpages(vpage)->image, 320 * 200)
  gfx_surfaceUpdate( vpage8 )
  pagecopytime = TIMER
  gfx_surfaceCopy( NULL, vpage8, masterPalette, NO, NULL, vpage32)
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

  gfx_renderQuadTexture( @pt_vertices(0), spriteSurface, masterPalette, YES, NULL, vpage32 )
  drawtime = TIMER - starttime

  gfx_present( vpage32, NULL )

  'surface_export_bmp24 ("out.bmp", vpage32)
  dowait
 LOOP
 setkeys
 frame_unload @testframe
 gfx_surfaceDestroy(vpage32)
 gfx_surfaceDestroy(vpage8)
 gfx_surfaceDestroy(spriteSurface)
 gfx_paletteDestroy(masterPalette)
END SUB

#ELSE

SUB quad_transforms_menu ()
 notification "Compile with 'scons raster=1' to enable."
END SUB

#ENDIF

SUB text_test_menu
 DIM text as string = load_help_file("texttest")
 DIM mouse as MouseInfo
 hidemousecursor
 DO
  setwait 55
  setkeys
  mouse = readmouse
  IF keyval(scEsc) > 1 THEN EXIT DO
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

  text_layout_dimensions @pos2, text, curspos.charnum, , 280, 0, YES, YES

  clearpage vpage
  edgeboxstyle 10, 10, 300, 185, 0, vpage
  printstr vpages(vpage), text, 20, 20, 280, 0
  rectangle vpages(vpage), 20 + pos2.lastw, 20 + pos2.h - pos2.finalfont->h, 8, pos2.finalfont->h, 5
  printstr CHR(3), mouse.x - 2, mouse.y - 2, vpage
  printstr STR(curspos.charnum), 0, 190, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 unhidemousecursor
END SUB

SUB font_test_menu
 DIM menu(...) as string = {"Font 0", "Font 1", "Font 2", "Font 3"}
 DIM st as MenuState
 st.last = UBOUND(menu)
 st.size = 22

 DIM controls as string = "1: import from 'fonttests/testfont/', 2: import from bmp, 3: create edged font, 4: create shadow font"

 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(sc1) > 1 THEN
   font_loadbmps @fonts(st.pt), "fonttests/testfont", @fonts(st.pt)
  END IF
  IF keyval(sc2) > 1 THEN
   DIM file as string
   file = browse(10, "", "*.bmp", tmpdir, 0, "")
   IF LEN(file) THEN
    font_loadbmp_16x16 @fonts(st.pt), file
   END IF
  END IF
  IF keyval(sc3) > 1 THEN
   DIM choice as integer
   choice = multichoice("Create an edged font from which font?", menu())
   IF choice > -1 THEN
    font_create_edged @fonts(st.pt), @fonts(choice)
   END IF
  END IF
  IF keyval(sc4) > 1 THEN
   DIM choice as integer
   choice = multichoice("Create a drop-shadow font from which font?", menu())
   IF choice > -1 THEN
    font_create_shadowed @fonts(st.pt), @fonts(choice), 2, 2
   END IF
  END IF

  usemenu st

  clearpage vpage
  edgeboxstyle 10, 10, 300, 185, 0, vpage
  standardmenu menu(), st, 0, 0, vpage
  textcolor uilook(uiText), 0
  printstr vpages(vpage), controls, 0, 40, 140, 0

  FOR i as integer = 0 TO 15
   DIM row as string
   FOR j as integer = i * 16 TO i * 16 + 15
    row &= CHR(j)
   NEXT
   printstr vpages(vpage), row, 145, 0 + i * fonts(st.pt).h, , st.pt, YES, NO  'without newlines
  NEXT

  setvispage vpage
  dowait
 LOOP
END SUB
