'OHRRPGCE GAME - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "config.bi"
#include "ver.txt"
#include "udts.bi"
#include "game_udts.bi"
#include "slices.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "savegame.bi"
#include "scriptcommands.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "walkabouts.bi"
#include "menustuf.bi"
#include "bmodsubs.bi"
#include "bmod.bi"
#include "scripting.bi"
#include "sliceedit.bi"
#include "purchase.bi"
#include "game.bi"
#include "gfx.bi"


'local subs and functions
DECLARE SUB checkdoors ()
DECLARE SUB usedoor (byval door_id as integer)
DECLARE FUNCTION want_to_check_for_walls(byval who as integer) as bool
DECLARE SUB update_npcs ()
DECLARE SUB pick_npc_action(npci as NPCInst, npcdata as NPCType)
DECLARE FUNCTION perform_npc_move(byval npcnum as integer, npci as NPCInst, npcdata as NPCType) as integer
DECLARE SUB npchitwall (npci as NPCInst, npcdata as NPCType)
DECLARE FUNCTION find_useable_npc () as integer
DECLARE SUB interpret ()
DECLARE SUB update_heroes(byval force_step_check as integer=NO)
DECLARE SUB doloadgame(byval load_slot as integer)
DECLARE SUB reset_game_final_cleanup()
DECLARE FUNCTION should_skip_this_timer(byval l as integer, t as PlotTimer) as integer
DECLARE SUB update_menu_states ()
DECLARE FUNCTION seek_rpg_or_rpgdir_and_play_it(where as string, gamename as string) as integer
DECLARE SUB misc_debug_menu()
DECLARE SUB battle_formation_testing_menu()
DECLARE SUB queue_music_change (byval song as integer)
DECLARE SUB check_for_queued_music_change ()


'============================== Setup directories =============================

'Note: On Android exename is "sdl" and exepath is "" (currently unimplemented in FB and meaningless for an app anyway)

#IFDEF __FB_ANDROID__
 log_dir = CURDIR & SLASH
#ENDIF

'FIXME: too many directory variables! Clean this nonsense up
DIM app_dir as string  'global
app_dir = exepath

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

start_new_debug
debuginfo long_version & build_info & " " & systeminfo
debuginfo DATE & " " & TIME

'DEBUG debug "randomize timer"
mersenne_twister TIMER

'Global variables which are affected by processcommandline (specifically, game_setoption)
DIM autotestmode as bool = NO
DIM always_enable_debug_keys as bool = NO
DIM speedcontrol as double = 55
DIM autosnap as integer = 0
DIM running_as_slave as bool = NO
DIM custom_version as string  'when running as slave
DIM master_channel as IPCChannel = NULL_CHANNEL  'when running as slave
DIM modified_lumps as string vector  'when running as slave
v_new modified_lumps
DIM force_prefsdir_save as bool = NO

orig_dir = CURDIR()
REDIM cmdline_args() as string
' This can modify log_dir
processcommandline cmdline_args(), @gamecustom_setoption, orig_dir & SLASH & "ohrrpgce_arguments.txt"

'---get temp dir---
set_homedir
set_tmpdir
IF NOT isdir(tmpdir) THEN
 IF makedir(tmpdir) <> 0 THEN fatalerror "Unable to create temp directory " & tmpdir
END IF
'As soon as we create the tmpdir, we want to put a keepalive file in it
refresh_keepalive_file


'============================== Initialise backends ===========================

'DEBUG debug "set mode-X"
setmodex

'DEBUG debug "init sound"
setupmusic


'=================================== Globals ==================================

'DEBUG debug "dim (almost) everything"

'shared module variables
DIM SHARED harmtileflash as integer = NO

'global variables
DIM gam as GameState
DIM txt as TextBoxState
REDIM gen(499) as integer
REDIM gmap(0) as integer  'sized later
DIM gen_reld_doc as DocPtr
DIM persist_reld_doc as DocPtr
REDIM tag(1000) as integer '16000 bitsets
REDIM onetime(1000) as integer '16000 bitsets

REDIM herotags(59) as HeroTagsCache
REDIM itemtags(maxMaxItems) as ItemTagsCache

'Party stuff
REDIM spell(40, 3, 23) as integer
REDIM lmp(40, 7) as integer
REDIM eqstuf(40, 4) as integer

'Hero walkabout (caterpillar) data
'Noninterpolated
REDIM herow(3) as HeroWalkabout
'Interpolated
REDIM catx(15) as integer
REDIM caty(15) as integer
REDIM catz(15) as integer
REDIM catd(15) as integer

REDIM statnames() as string

REDIM maptiles(0) as TileMap
DIM pass as TileMap
DIM foemap as TileMap
DIM zmap as ZoneMap
REDIM tilesets(maplayerMax) as TilesetData ptr  'tilesets is fixed size at the moment. It must always be at least as large as the number of layers on a map
DIM mapsizetiles as XYPair  'for convienence

REDIM master(255) as RGBcolor
REDIM uilook(uiColorLast) as integer
REDIM boxlook(uiBoxLast) as BoxStyle
REDIM current_font(1023) as integer

REDIM pal16(448) as integer
REDIM buffer(16384) as integer 'FIXME: when can we get rid of this?

REDIM inventory(inventoryMax) as InventSlot
DIM gold as integer

REDIM npcs(0) as NPCType
REDIM npc(299) as NPCInst

DIM mapx as integer
DIM mapy as integer
DIM vpage as integer
DIM dpage as integer
DIM fadestate as integer
DIM usepreunlump as integer
DIM lastsaveslot as integer
DIM abortg as integer
DIM resetg as integer
DIM presentsong as integer

DIM err_suppress_lvl as scriptErrEnum
DIM exename as string
DIM game as string
DIM sourcerpg as string
DIM savefile as string
DIM workingdir as string
DIM homedir as string
DIM prefsdir as string

DIM lump_reloading as LumpReloadOptions
lump_reloading.gmap.mode = loadmodeAlways
lump_reloading.maptiles.mode = loadmodeAlways
lump_reloading.passmap.mode = loadmodeAlways
lump_reloading.foemap.mode = loadmodeAlways
lump_reloading.zonemap.mode = loadmodeAlways
lump_reloading.npcl.mode = loadmodeMerge
lump_reloading.npcd.mode = loadmodeAlways
lump_reloading.hsp.mode = loadmodeNever

'Menu Data
DIM menu_set as MenuSet
REDIM menus(0) as MenuDef 'This is an array because it holds a stack of heirarchial menus (resized as required)
REDIM mstates(0) as MenuState
DIM topmenu as integer = -1

DIM fatal as bool
DIM checkfatal as bool
DIM lastformation as integer

DIM vstate as VehicleState
reset_vehicle vstate

REDIM csetup(12) as integer
REDIM carray(13) as integer
REDIM joy(14) as integer
REDIM gotj(2) as integer

DIM backcompat_sound_slot_mode as integer
REDIM backcompat_sound_slots(7) as integer

DIM nowscript as integer
DIM scriptret as integer
DIM scriptctr as integer
DIM numloadedscr as integer    'Number of loaded scripts
DIM totalscrmem as integer     'Total memory used by all loaded scripts, in WORDs
DIM scriptcachemem as integer  'Memory used by script cache, WORDs
DIM scrwatch as integer
DIM next_interpreter_check_time as double
DIM interruption_grace_period as integer
REDIM heap(maxScriptHeap) as integer
REDIM global(maxScriptGlobals) as integer
REDIM retvals(32) as integer
REDIM scrat(maxScriptRunning) as OldScriptState
REDIM scriptinsts(maxScriptRunning) as ScriptInst
REDIM script(scriptTableSize - 1) as ScriptData Ptr
REDIM plotstr(maxScriptStrings) as Plotstring
REDIM lookup1_bin_cache(-1 TO -1) as TriggerData
DIM scrst as Stack
DIM curcmd as ScriptCommand ptr
DIM insideinterpreter as integer
DIM wantimmediate as integer
DIM last_queued_script as QueuedScript ptr

'incredibly frustratingly fbc doesn't export global array debugging symbols
DIM globalp as integer ptr
DIM heapp as integer ptr
DIM scratp as OldScriptState ptr
DIM scriptp as ScriptData ptr ptr
DIM retvalsp as integer ptr
DIM plotslicesp as slice ptr ptr
globalp = @global(0)
heapp = @heap(0)
scratp = @scrat(0)
scriptp = @script(0)
retvalsp = @retvals(0)

'End global variables

'Module local variables
DIM archinym as string



'==============================================================================

'DEBUG debug "Thestart"
DO 'This is a big loop that encloses the entire program (more than it should). The loop is only reached when resetting the program


'====================== (Re)initialise gfx/window/IO options ==================

dpage = 1: vpage = 0
presentsong = -1

gam.current_master_palette = -1
load_default_master_palette master()
DefaultUIColors uilook(), boxlook()
getdefaultfont current_font()
setfont current_font()
close_general_reld()

'-- Init joysticks
FOR i as integer = 0 TO 1
 gotj(i) = readjoy(joy(), i)
NEXT i

gen(genJoy) = 0  'joystick disabled by default
defaultc  'set up default controls

'Read joyset.ini
readjoysettings

DIM gp as GamePadMap
gp.A = scEnter
gp.B = scESC
gp.X = scESC
gp.Y = scESC
gp.L1 = scPageUp
gp.R1 = scPageDown
gp.L2 = scHome
gp.R2 = scEnd
remap_android_gamepad 0, gp
'Make all four gamepads use the same keys by default
FOR i as integer = 1 to 3
 remap_android_gamepad i, gp
NEXT i

'virtual gamepad stuff only has effect on platforms that actually allow
' a virtual gamepad (currently all Android except for OUYA)
hide_virtual_gamepad()
remap_touchscreen_button 0, scEnter
remap_touchscreen_button 1, scESC
remap_touchscreen_button 2, 0
remap_touchscreen_button 3, 0
remap_touchscreen_button 4, 0
remap_touchscreen_button 5, 0

setwindowtitle "O.H.R.RPG.C.E"
unhidemousecursor  'init mouse state

set_safe_zone_margin default_margin()
set_resolution 320, 200
IF overrode_default_zoom = NO THEN
 'If it was set on the commandline, then it should still be set to that; game didn't change it
 set_scale_factor 2
END IF


'=============================== Find the game ================================

gam.autorungame = NO
usepreunlump = NO
DIM rpg_browse_default as string = ""  'local variable

'---get work dir and exe name---
'DEBUG debug "setup directories"
exename = trimextension(trimpath(COMMAND(0)))

IF running_as_slave THEN

 'Check for version compatibility, and get told sourcerpg & workingdir
 'NOTE: normally sourcedir == workingdir if running a preunlumped game, but not in this case!
 handshake_with_master
 gam.autorungame = YES
 usepreunlump = YES

ELSE  'NOT running_as_slave

 workingdir = tmpdir + "playing.tmp"

 'DEBUG debug "create playing.tmp"
 '---If workingdir does not already exist, it must be created---
 IF isdir(workingdir) THEN
  'DEBUG debug workingdir+" already exists"
  'DEBUG debug "erasing "+workingdir+"\"+ALLFILES
  cleanuptemp
 ELSE
  makedir workingdir
 END IF

 '---IF A VALID RPG FILE WAS SPECIFIED ON THE COMMAND LINE, RUN IT, ELSE BROWSE---
 '---ALSO CHECKS FOR GAME.EXE RENAMING

 'DEBUG debug "searching commandline for game"
 FOR i as integer = 0 TO UBOUND(cmdline_args)
  DIM arg as string = cmdline_args(i)
  arg = absolute_path(arg)
 
  IF LCASE(RIGHT(arg, 4)) = ".rpg" AND isfile(arg) THEN
   sourcerpg = arg
   gam.autorungame = YES
   EXIT FOR
  ELSEIF isdir(arg) THEN 'perhaps it's an unlumped folder?
   'check for essentials (archinym.lmp was added long before .rpgdir support)
   IF isfile(arg + SLASH + "archinym.lmp") THEN 'ok, accept it
    gam.autorungame = YES
    usepreunlump = YES
    sourcerpg = trim_trailing_slashes(arg)
    workingdir = arg
   ELSE
    rpg_browse_default = arg
   END IF
   EXIT FOR
  ELSE
   visible_debug "Unrecognised commandline argument " & cmdline_args(i) & " ignored"
  END IF
 NEXT

END IF  'NOT running_as_slave

#IFDEF __FB_LINUX__
IF gam.autorungame = NO THEN
 IF exename <> "ohrrpgce-game" THEN
  DO 'single-pass loop for breaking
   IF starts_with(exepath, "/usr/local") THEN
    IF seek_rpg_or_rpgdir_and_play_it("/usr/local/share/games/" & exename, exename) THEN EXIT DO
    IF seek_rpg_or_rpgdir_and_play_it("/usr/local/share/" & exename, exename) THEN EXIT DO
   END IF
   IF starts_with(exepath, "/usr/games") ORELSE starts_with(exepath, "/usr/bin") THEN
    IF seek_rpg_or_rpgdir_and_play_it("/usr/share/games/" & exename, exename) THEN EXIT DO
    IF seek_rpg_or_rpgdir_and_play_it("/usr/share/" & exename, exename) THEN EXIT DO
   END IF
  EXIT DO : LOOP '--end of single-pass loop for breaking
 END IF
END IF
#ENDIF

#IFDEF __FB_DARWIN__
IF gam.autorungame = NO THEN
 IF ends_with(exepath, ".app/Contents/MacOS") THEN
  DIM appres as string
  appres = exepath & "/../Resources"
  IF isfile(appres & "/bundledgame") THEN
   DIM bundledname as string
   bundledname = TRIM(string_from_first_line_of_file(appres & "/bundledgame"), ANY !" \t\r\n")
   IF seek_rpg_or_rpgdir_and_play_it(exepath & "/../Resources", bundledname) THEN
    force_prefsdir_save = YES
   END IF
  END IF
 END IF
END IF
#ENDIF

IF gam.autorungame = NO THEN
 IF LCASE(exename) <> "game" ANDALSO exename <> "ohrrpgce-game" THEN
  seek_rpg_or_rpgdir_and_play_it exepath, exename
 END IF
END IF

IF gam.autorungame = NO THEN
 'DEBUG debug "browse for RPG"
 show_virtual_gamepad()
 sourcerpg = browse(7, rpg_browse_default, "*.rpg", tmpdir, 1, "game_browse_rpg")
 hide_virtual_gamepad()
 IF sourcerpg = "" THEN exitprogram NO
 IF isdir(sourcerpg) THEN
  usepreunlump = YES
  workingdir = sourcerpg
 END IF
END IF


'======================== Setup game-specific directories =====================

'-- set up prefs dir
set_settings_dir
prefsdir = settings_dir & SLASH & trimextension(trimpath(sourcerpg))
IF NOT isdir(prefsdir) THEN makedir prefsdir
debuginfo "prefsdir=" & prefsdir

end_debug 'delete unimportant messages generated before this point, or from previous game

'-- change current directory, where g_debug will be put; mainly for drag-dropping onto Game in Windows which defaults to homedir
DIM fol as string = trimfilename(sourcerpg)
IF fol <> "" ANDALSO diriswriteable(fol) THEN
 'first choice is game directory
 CHDIR fol
ELSEIF diriswriteable(app_dir) THEN
 CHDIR app_dir
ELSE
 'should prefsdir be used instead?
 CHDIR homedir
END IF

start_new_debug

init_save_system
gam.script_log.filename = log_dir & "script_log.txt"

IF gam.autorungame = NO THEN
 edgeboxstyle 4, 3, 312, 14, 0, vpage
ELSE
 setpal master()
END IF

#IFDEF __FB_ANDROID__
cleanup_other_temp_files
#ENDIF


'==================================== Unlump ==================================

edgeprint "Loading...", xstring("Loading...", 160), 6, uilook(uiText), vpage
setvispage vpage 'refresh

'--pre-extract (if needed) .gen and load it
IF usepreunlump THEN
 archinym = readarchinym(workingdir, sourcerpg)
 xbload workingdir & SLASH & archinym & ".gen", gen(), "general game data missing from " + sourcerpg
ELSE
 copylump sourcerpg, "archinym.lmp", tmpdir, YES
 archinym = readarchinym(tmpdir, sourcerpg)
 copylump sourcerpg, archinym + ".gen", tmpdir, YES
 xbload tmpdir + archinym + ".gen", gen(), "general game data missing from " + sourcerpg
END IF

DIM forcerpgcopy as integer = NO
IF gen(genVersion) > CURRENT_RPG_VERSION THEN
 debug "genVersion = " & gen(genVersion)
 future_rpg_warning  '(fatal error is running_as_slave)
 forcerpgcopy = YES  'If we upgraded an .rpgdir in-place, we would probably damage it
END IF

'---GAME SELECTED, PREPARING TO PLAY---
IF usepreunlump = NO THEN
 unlump sourcerpg, workingdir
ELSEIF NOT running_as_slave THEN  'Won't unlump or upgrade if running as slave
 IF NOT diriswriteable(workingdir) THEN
  'We have to copy the game, otherwise we won't be able to upgrade it
  '(it's too much trouble to properly check whether the game is already
  'fully up to date, which is unlikely anyway): change workingdir!
  debuginfo workingdir + " not writeable"
  forcerpgcopy = YES
 END IF
 IF forcerpgcopy THEN
  workingdir = tmpdir + "playing.tmp"
  copyfiles sourcerpg, workingdir + SLASH
  usepreunlump = NO
 END IF
END IF

debuginfo long_version & build_info
debuginfo "Runtime info: " & gfxbackendinfo & "  " & musicbackendinfo & "  " & systeminfo
debuginfo "Playing game " & sourcerpg & " (" & getdisplayname(" ") & ") " & DATE & " " & TIME
IF running_as_slave THEN debuginfo "Spawned from Custom (" & custom_version & ")"

REDIM gmap(dimbinsize(binMAP)) 'this must be sized here, after the binsize file exists!

'--set game
game = workingdir + SLASH + archinym
game_unique_id = STR(randint(INT_MAX))


'============================== Upgrade the game ==============================

DIM wintitle as string = getdisplayname(trimpath(sourcerpg))
IF running_as_slave THEN wintitle = "Testing " + wintitle
setwindowtitle wintitle

'Perform additional checks for future rpg files or corruption
'FIXME: if a problem was detected, we don't force copy of an .rpgdir
rpg_sanity_checks

xbload game + ".fnt", current_font(), "font missing from " + sourcerpg

'--upgrade obsolete RPG files (if possible)
IF NOT running_as_slave THEN upgrade

'If no version mismatch error has occurred yet, show a warning if the versions aren't identical
IF running_as_slave THEN check_game_custom_versions_match


'======================== Stuff initialised once per .RPG =====================

fadeout 0, 0, 0
'This queue_fade_in apparently does nothing, since the titlescreen, load menu,
'and main loop all override it
queue_fade_in

'Recreate/resize/reposition the window as needed
apply_game_window_settings
set_safe_zone_margin read_ini_int(prefsdir & SLASH & "gameconfig.ini", "gfx.margin", default_margin_for_game())

setfont current_font()
loadglobalstrings
getstatnames statnames()

IF isfile(game + ".hsp") THEN unlump game + ".hsp", tmpdir
'Might be changed by --errlvl commandline option
IF err_suppress_lvl = 0 THEN err_suppress_lvl = bound(gen(genErrorLevel) - 1, 0, 5)
nowscript = -1
numloadedscr = 0
totalscrmem = 0
resetinterpreter
'the old stack used only inbattle
releasestack
setupstack

SetupGameSlices
'beginplay
resetg = NO

'This is called BEFORE the loop, because when the game is quit or a save is loaded, this will be called again there
reset_game_state


'===================== Stuff reinitialised each new/load-game ==================

DO' This loop encloses the playable game for a specific RPG file


gam.current_master_palette = gen(genMasterPal)
loadpalette master(), gam.current_master_palette
LoadUIColors uilook(), boxlook(), gam.current_master_palette
set_speedcontrol

initgamedefaults
fatal = NO
checkfatal = NO
abortg = 0
lastformation = -1
scrwatch = 0
menu_set.menufile = workingdir & SLASH & "menus.bin"
menu_set.itemfile = workingdir & SLASH & "menuitem.bin"
load_lookup1_bin lookup1_bin_cache()

makebackups 'make a few backup lumps

gam.want.box = 0
gam.want.door = 0
gam.want.battle = 0
gam.want.teleport = NO
gam.want.usenpc = 0
gam.want.loadgame = 0

txt.showing = NO
txt.fully_shown = NO
txt.show_lines = 0
txt.sayer = -1
txt.id = -1

'========================== Title and loadgame menu ============================

DIM load_slot as integer = -1
'resetg is YES when we are skipping straight to launching the game
IF readbit(gen(), genBits, 11) = 0 AND resetg = NO THEN
 IF titlescreen() = NO THEN EXIT DO
 IF readbit(gen(), genBits, 12) = 0 THEN load_slot = picksave(1)
ELSE
 IF readbit(gen(), genBits, 12) = 0 AND resetg = NO THEN
  IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
  load_slot = picksave(2)
 END IF
END IF
resetg = NO
'DEBUG debug "picked save slot " & load_slot
queue_music_change -1  'stop music
IF load_slot = -2 THEN
 fadeout 0, 0, 0
 EXIT DO 'resetg
END IF
IF load_slot >= 0 THEN
 fadeout 0, 0, 0
 doloadgame load_slot
ELSE
 refresh_purchases()
 fadeout 0, 0, 0
 clearpage 0
 clearpage 1
 'Add initial hero to party (slot 0)
 addhero 1 + gen(genStartHero), 0
 'Trigger textbox and/or script
 gam.want.box = gen(genStartTextbox)  '0 for no textbox
 IF gen(genNewGameScript) > 0 THEN
  trigger_script gen(genNewGameScript), YES, "newgame", "", scrqBackcompat()
 END IF
 prepare_map
END IF

load_special_tag_caches
evalherotags

'--Reset some stuff related to debug keys
gam.showtext_ticks = 0
gam.debug_showtags = NO
gam.debug_npc_info = NO
gam.walk_through_walls = NO


'================================= Main loop ==================================

queue_fade_in
'DEBUG debug "pre-call update_heroes"
update_heroes(YES)
setkeys

DIM tog as integer
DIM speedcontrol_this_tick as double = speedcontrol
DO
 'DEBUG debug "top of master loop"
 setwait speedcontrol_this_tick
 speedcontrol_this_tick = speedcontrol
 IF running_as_slave THEN try_to_reload_lumps_onmap
 tog = tog XOR 1
 'DEBUG debug "increment play timers"
 playtimer

 'DEBUG debug "read controls"
 update_virtual_gamepad_display()
 setkeys gam.getinputtext_enabled
 gam.mouse = readmouse  'didn't bother to check havemouse()
 control

 'debug "menu key handling:"
 check_menu_tags
 player_menu_keys()
 'debug "after menu key handling:"

 IF menus_allow_gameplay() THEN

 '--Scripts
 IF gmap(15) THEN onkeyscript gmap(15)
 'breakpoint : called after keypress script is run, but don't get called by wantimmediate
 IF scrwatch > 1 THEN breakpoint scrwatch, 4
 'DEBUG debug "enter script interpreter"
 interpret
 'DEBUG debug "increment script timers"
 dotimer(TIMER_NORMAL)

 'DEBUG debug "keyboard handling"
 IF carray(ccMenu) > 1 AND txt.showing = NO AND gam.need_fade_in = NO AND readbit(gen(), genSuspendBits, suspendplayer) = 0 AND vstate.active = NO AND herow(0).xgo = 0 AND herow(0).ygo = 0 THEN
  IF gen(genEscMenuScript) > 0 THEN
   trigger_script gen(genEscMenuScript), NO, "", "", scrqBackcompat()
  ELSEIF allowed_to_open_main_menu() THEN
   add_menu 0
   menusound gen(genAcceptSFX)
  END IF
 END IF
 IF txt.showing = NO AND gam.need_fade_in = NO AND readbit(gen(), genSuspendBits, suspendplayer) = 0 AND vehicle_is_animating() = NO AND menus_allow_player() THEN
  IF herow(0).xgo = 0 AND herow(0).ygo = 0 THEN
   DO
    IF carray(ccUp) > 0 THEN herow(0).ygo = 20: catd(0) = 0: EXIT DO
    IF carray(ccDown) > 0 THEN herow(0).ygo = -20: catd(0) = 2: EXIT DO
    IF carray(ccLeft) > 0 THEN herow(0).xgo = 20: catd(0) = 3: EXIT DO
    IF carray(ccRight) > 0 THEN herow(0).xgo = -20: catd(0) = 1: EXIT DO
    IF carray(ccUse) > 1 AND vstate.active = NO THEN
     usenpc 0, find_useable_npc()
    END IF
    EXIT DO
   LOOP
  END IF
 END IF
 IF txt.fully_shown = YES ANDALSO readbit(gen(), genSuspendBits, suspendboxadvance) = 0 THEN
  IF use_touch_textboxes() THEN
   DIM mouse as MouseInfo
   mouse = readmouse()
   IF (mouse.clickstick AND mouseLeft) THEN
    advance_text_box
   END IF
  END IF
  IF carray(ccUse) > 1 THEN
   advance_text_box
  END IF
 END IF
 'debug "after advance_text_box:"
 IF vstate.active THEN
  'DEBUG debug "evaluate vehicles"
  update_vehicle_state()
 END IF
 IF txt.fully_shown = YES AND txt.box.choice_enabled THEN
  usemenusounds
  usemenu txt.choicestate
 END IF
 'DEBUG debug "hero movement"
 update_heroes()
 'DEBUG debug "NPC movement"
 update_npcs()

 '--Debug keys
 IF always_enable_debug_keys OR readbit(gen(), genBits, 8) = 0 THEN
  'DEBUG debug "evaluate debugging keys"
  IF keyval(scF2) > 1 AND txt.showing = NO THEN
   savegame 32
   gam.showtext = "Quick-saved. Press F3 to quick-load"
   gam.showtext_ticks = 20
  END IF
  IF keyval(scF3) > 1 AND txt.showing = NO THEN
   IF yesno("Load quick-saved game?") THEN gam.want.loadgame = 33
  END IF
  IF keyval(scCtrl) > 0 AND keyval(scF7) > 1 THEN
   catx(0) = (catx(0) \ 20) * 20
   caty(0) = (caty(0) \ 20) * 20
   herow(0).xgo = 0
   herow(0).ygo = 0
  END IF
  IF keyval(scShift) > 0 AND keyval(scTab) > 0 THEN  'speed up while held down
   speedcontrol_this_tick = speedcontrol / 5
  END IF
  IF keyval(scCtrl) > 0 THEN ' holding CTRL
   IF keyval(scF1) > 1 AND txt.showing = NO THEN
    IF teleporttool() THEN 'CTRL + F1
     prepare_map
    END IF
   END IF
   IF gam.debug_showtags = NO THEN
    IF keyval(scNumpadPlus) > 1 OR keyval(scPlus) > 1 THEN  'CTRL +
     speedcontrol = large(speedcontrol - 1, 10.)
     gam.showtext = speedcontrol & "ms/frame"
     gam.showtext_ticks = 60
    END IF
    IF keyval(scNumpadMinus) > 1 OR keyval(scMinus) > 1 THEN  'CTRL -
     speedcontrol = small(speedcontrol + 1, 160.)
     gam.showtext = speedcontrol & "ms/frame"
     gam.showtext_ticks = 60
    END IF
   END IF
   IF keyval(scF4) > 1 THEN
    IF fadestate = 0 THEN setpal master()
    slice_editor SliceTable.Root
   END IF
   IF keyval(scF5) > 1 THEN 'Toggle level-up bug
    IF readbit(gen(), genBits, 9) = 0 THEN
     setbit gen(), genBits, 9, 1
    ELSE
     setbit gen(), genBits, 9, 0
    END IF
   END IF
   IF keyval(scF8) > 1 THEN
    debug "----------------Slice Tree Dump---------------"
    SliceDebugDumpTree SliceTable.Root
    notification "Dumped entire slice tree to g_debug.txt"
   END IF
   IF keyval(scF10) > 1 THEN
    IF gam.script_log.enabled THEN
     gam.script_log.enabled = NO
     gam.showtext = "Script logging disabled."
    ELSE
     gam.showtext = "Script logging enabled."
     start_script_trigger_log
    END IF
    gam.showtext_ticks = 20
   END IF
   IF keyval(scF11) > 1 THEN gam.debug_npc_info = NOT gam.debug_npc_info
  ELSE ' not holding CTRL
   IF keyval(scF1) > 1 AND txt.showing = NO THEN minimap catx(0), caty(0)
   IF keyval(scF4) > 1 THEN gam.debug_showtags = NOT gam.debug_showtags : scrwatch = 0
   IF keyval(scF5) > 1 THEN live_preview_menu
   IF keyval(scF8) > 1 THEN misc_debug_menu
   IF keyval(scF10) > 1 THEN scrwatch = loopvar(scrwatch, 0, 2, 1): gam.debug_showtags = NO
   IF keyval(scF11) > 1 THEN gam.walk_through_walls = NOT gam.walk_through_walls
  END IF
 END IF

 IF gam.want.loadgame > 0 THEN
  'DEBUG debug "loading game slot " & (gam.want.loadgame - 1)
  load_slot = gam.want.loadgame - 1
  gam.want.loadgame = 0
  resetgame
  initgamedefaults
  stopsong
  resetsfx
  fadeout 0, 0, 0
  queue_fade_in 1, YES
  doloadgame load_slot
 END IF

 AdvanceSlice SliceTable.root
 ELSE
  dotimer(TIMER_BLOCKINGMENUS)
 END IF' end menus_allow_gameplay

 'Death handling
 IF checkfatal THEN
  'Note that we only check for death if requested because setting hero HP to zero with a script
  'doesn't end the game, for back-compat.
  'Also, battles set the 'fatal' global directly, because they use a different death condition
  '(a hero with zero max HP counts as dead, while OOB they count as alive)
  fatal OR= checkfordeath
  checkfatal = NO
 END IF
 IF fatal THEN
  '--this is what happens when you die
  txt.showing = NO
  txt.fully_shown = NO
  IF gen(genGameoverScript) > 0 THEN
   trigger_script gen(genGameoverScript), NO, "death", "", scrqBackcompat()
   fatal = NO
   queue_fade_in 1
  ELSE
   fadeout 255, 0, 0
  END IF
 END IF

 'Draw screen
 displayall()

 'Main loop exit test (does this need to be here?)
 IF fatal OR abortg > 0 OR resetg THEN
  resetgame
  'Stop sounds but not music; the title screen might not have any music set, or be set to the same music
  resetsfx
  IF resetg THEN EXIT DO  'skip to new game
  'if skipping title and loadmenu, quit
  IF (readbit(gen(), genBits, 11)) AND (readbit(gen(), genBits, 12) OR abortg = 2 OR count_used_save_slots() = 0) THEN
   EXIT DO, DO ' To game select screen (quit the gameplay and RPG file loops, allowing the program loop to cycle)
  ELSE
   EXIT DO ' To title screen (quit the gameplay loop and allow the RPG file loop to cycle)
  END IF
 END IF

 'DEBUG debug "swap video pages"
 SWAP vpage, dpage
 setvispage vpage
 'DEBUG debug "fade in"
 check_for_queued_music_change
 check_for_queued_fade_in
 'DEBUG debug "tail of main loop"
 dowait
LOOP

LOOP ' This is the end of the DO that encloses a specific RPG file

reset_game_final_cleanup
LOOP ' This is the end of the DO that encloses the entire program.

SUB reset_game_final_cleanup()
 'WARNING: It's a bug to call anything in here that causes something to be cached after
 'the cache has been emptied (such as anything that calls getbinsize after clear_binsize_cache)
 cleanup_text_box
 resetinterpreter 'unload scripts
 unloadmaptilesets tilesets()
 refresh_map_slice_tilesets '--zeroes them out
 unloadtilemaps maptiles()
 unloadtilemap pass
 unloadtilemap foemap
 DeleteZonemap zmap
 'checks for leaks and deallocates them
 sprite_empty_cache
 palette16_empty_cache
 cleanup_game_slices
 SliceDebugDump YES
 cleanup_global_reload_doc
 clear_binsize_cache
 stopsong
 resetsfx
 cleanup_other_temp_files
 IF gam.autorungame THEN exitprogram (NOT abortg) 'exitprogram also calls cleanuptemp
 cleanuptemp
 fadeout 0, 0, 0
 clearpage 0
 clearpage 1
 clearpage 2
 clearpage 3
 sourcerpg = ""
END SUB

SUB doloadgame(byval load_slot as integer)
 loadgame load_slot
 IF gen(genLoadGameScript) > 0 THEN
  trigger_script gen(genLoadGameScript), YES, "loadgame", "", scrqBackcompat()
  '--pass save slot as argument
  IF load_slot = 32 THEN
   trigger_script_arg 0, -1, "slot"  'quickload slot
  ELSE
   trigger_script_arg 0, load_slot, "slot"
  END IF
 END IF
 gam.map.same = YES

 prepare_map NO, YES
 'FIXME: clean this up: setting vstate.id is only backcompat for loading from the old SAV format;
 ' ideally this would be in old_loadgame, but we need to load NPCs in prepare_map
 IF vstate.active THEN
  SELECT CASE npc(vstate.npc).id
   CASE 0:
    debug "Vehicle NPC ref " & vstate.npc - 1 & " in save state does not exist in game anymore"
    vehicle_graceful_dismount
   CASE IS < 0:
    debug "Vehicle NPC ref " & vstate.npc - 1 & " in save state is now disabled by tags"
    vehicle_graceful_dismount
   CASE ELSE
    vstate.id = npcs(npc(vstate.npc).id - 1).vehicle - 1
    create_walkabout_shadow npc(vstate.npc).sl
  END SELECT
 END IF

 party_change_updates
 refresh_purchases()
 
END SUB

SUB displayall()
 ' We need to update walkabout slice positions before calling
 ' setmapxy, in the case where the camera is following a hero
 ' or NPC, but this is pretty wasteful. One alternative
 ' is to only update a single slice as required from setmapxy,
 ' or to stop using walkabout slices there.
 update_walkabout_slices()

 ' Update camera position
 setmapxy
 WITH *(SliceTable.MapRoot)
  .X = mapx * -1
  .Y = mapy * -1
 END WITH

 ' Walkabout slice positions (other than the hero/NPC being
 ' followed) also depend on the camera position, so need to be
 ' repositioned after the camera is updated.
 ' TODO: I think that ideally slices could be set to wrap their children,
 ' that way slices manually parented to a map layer will always
 ' draw in the correct position, whereas currently this breaks on wrapping
 ' maps. This would also remove the need for this
 ' second update_walkabout_slices call.
 update_walkabout_slices()

 ' Beware that map slices behave in unique ways; see comments in DrawMapSlice.
 ' 
 ' Map layers edge handling.
 SELECT CASE gmap(5)
  CASE 1 'Wrap
   setoutside -1
  CASE 0, 2 'Crop, use default edge tile
   'We set an edge tile on crop maps in case the map is smaller than the screen
   setoutside gmap(6)
 END SELECT

 IF readbit(gen(), genSuspendBits, suspendoverlay) THEN
  ChangeMapSlice SliceTable.MapLayer(0), , , , 0   'draw all
  SliceTable.ObsoleteOverhead->Visible = NO
 ELSE
  ChangeMapSlice SliceTable.MapLayer(0), , , , 1   'draw non-overhead only
  SliceTable.ObsoleteOverhead->Visible = YES
 END IF

 update_backdrop_slice

 NumDrawnSlices = 0
 DIM drawtime as double = TIMER
 DrawSlice(SliceTable.Root, dpage)
 drawtime = TIMER - drawtime
 'debuginfo "Drew " & NumDrawnSlices & " slices in " & CINT(drawtime * 1e6) & "us; " & CINT(drawtime * 1e9 / NumDrawnSlices) & "ns/slice average"

 'The order in which we update and draw things is a little strange; I'm just preserving what it was
 animatetilesets tilesets()
 IF harmtileflash = YES THEN
  rectangle 0, 0, vpages(dpage)->w, vpages(dpage)->h, gmap(10), dpage
  harmtileflash = NO
 END IF
 IF txt.showing = YES THEN update_textbox

 'FIXME: Eventually we want to draw the rest of this stuff using slices, but for now draw it on top
 update_menu_states
 FOR i as integer = 0 TO topmenu
  draw_menu menus(i), mstates(i), dpage
 NEXT i
 edgeprint gam.showstring, 0, vpages(dpage)->h - 10, uilook(uiText), dpage
 showplotstrings
 IF gam.showtext_ticks > 0 THEN
  gam.showtext_ticks -= 1
  edgeprint gam.showtext, xstring(gam.showtext, vpages(dpage)->w \ 2), vpages(dpage)->h - 20, uilook(uiText), dpage
 END IF
 IF gam.debug_npc_info THEN npc_debug_display
 IF gam.debug_showtags THEN tagdisplay
 IF scrwatch THEN scriptwatcher scrwatch, -1
END SUB

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

SUB update_heroes(byval force_step_check as integer=NO)
 'note: xgo and ygo are offset of current position from destination, eg +ve xgo means go left
 FOR whoi as integer = 0 TO 3
  IF herow(whoi).speed = 0 THEN
   '--cancel movement, or some of the following code misbehaves
   herow(whoi).xgo = 0
   herow(whoi).ygo = 0
  END IF
  '--if starting movement to a new tile and passibility is enabled ... and some vehicle stuff ...
  IF want_to_check_for_walls(whoi) THEN
   IF readbit(gen(), genSuspendBits, suspendherowalls) = 0 AND vehicle_is_animating() = NO THEN
    '--this only happens if herowalls is on
    '--wrapping passability
    DIM herotile as XYPair
    herotile.x = catx(whoi * 5) \ 20
    herotile.y = caty(whoi * 5) \ 20
    wrappass herotile.x, herotile.y, herow(whoi).xgo, herow(whoi).ygo, vstate.active
   END IF
   IF readbit(gen(), genSuspendBits, suspendobstruction) = 0 AND vehicle_is_animating() = NO THEN
    '--this only happens if obstruction is on
    FOR i as integer = 0 TO UBOUND(npc)
     WITH npc(i)
      IF .id > 0 THEN '---NPC EXISTS---
       DIM id as integer
       id = .id - 1
       IF npcs(id).activation <> 2 THEN '---NPC is not step-on
        IF wrapcollision (.x, .y, .xgo, .ygo, catx(whoi * 5), caty(whoi * 5), herow(whoi).xgo, herow(whoi).ygo) THEN
         IF .not_obstruction = 0 THEN
          herow(whoi).xgo = 0: herow(whoi).ygo = 0
          '--push the NPC
          DIM push as integer = npcs(id).pushtype
          IF push > 0 AND .xgo = 0 AND .ygo = 0 THEN
           IF catd(whoi) = 0 AND (push = 1 OR push = 2 OR push = 4) THEN .ygo = 20
           IF catd(whoi) = 2 AND (push = 1 OR push = 2 OR push = 6) THEN .ygo = -20
           IF catd(whoi) = 3 AND (push = 1 OR push = 3 OR push = 7) THEN .xgo = 20
           IF catd(whoi) = 1 AND (push = 1 OR push = 3 OR push = 5) THEN .xgo = -20
           IF readbit(gen(), genBits2, 0) = 0 THEN ' Only do this if the backcompat bitset is off
            FOR o as integer = 0 TO UBOUND(npc) ' check to make sure no other NPCs are blocking this one
             IF npc(o).id <= 0 THEN CONTINUE FOR 'Ignore empty NPC slots and negative (tag-disabled) NPCs
             IF i = o THEN CONTINUE FOR
             IF npc(o).not_obstruction THEN CONTINUE FOR
             IF wrapcollision (.x, .y, .xgo, .ygo, npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo) THEN
              .xgo = 0
              .ygo = 0
              EXIT FOR
             END IF
            NEXT o
           END IF
          END IF
         END IF
         IF npcs(id).activation = 1 AND whoi = 0 THEN '--NPC is touch-activated
          IF wraptouch(.x, .y, catx(0), caty(0), 20) THEN
           usenpc 1, i
          END IF
         END IF '---touch-activate
        END IF ' ---NPC IS IN THE WAY
       END IF ' ---NPC is not step-on
      END IF '---NPC EXISTS
     END WITH
    NEXT i
   END IF
  END IF'--this only gets run when starting a movement to a new tile
 NEXT whoi

 'Caterpillar hero movement: if enabled and the leader about to move then make others trail
 IF readbit(gen(), genSuspendBits, suspendcaterpillar) = 0 THEN
  IF herow(0).xgo OR herow(0).ygo THEN
   FOR i as integer = 15 TO 1 STEP -1
    catx(i) = catx(i - 1)
    caty(i) = caty(i - 1)
    catd(i) = catd(i - 1)
   NEXT i
   FOR whoi as integer = 0 TO 3
    herow(whoi).wtog = loopvar(herow(whoi).wtog, 0, 3, 1)
   NEXT whoi
  END IF
 ELSE
  FOR whoi as integer = 0 TO 3
   IF herow(whoi).xgo OR herow(whoi).ygo THEN herow(whoi).wtog = loopvar(herow(whoi).wtog, 0, 3, 1)
  NEXT whoi
 END IF

 'Non-caterpillar (normal [xy]go-based) hero movement
 REDIM didgo(0 TO 3) as integer
 FOR whoi as integer = 0 TO 3
  'NOTE: this loop covers the max caterpillar size, and not the current
  ' return value of caterpillar_size() because empty hero slots still
  ' need to be movable on the map. Scripts sometimes want to move a hero
  ' and wait for that hero without first checking if the slot is occupied
  didgo(whoi) = NO
  IF herow(whoi).xgo OR herow(whoi).ygo THEN
   '--this actually updates the hero's coordinates
   'NOTE: if the caterpillar is enabled, then only the leader has nonzero xgo, ygo
   IF herow(whoi).xgo > 0 THEN herow(whoi).xgo -= herow(whoi).speed: catx(whoi * 5) -= herow(whoi).speed
   IF herow(whoi).xgo < 0 THEN herow(whoi).xgo += herow(whoi).speed: catx(whoi * 5) += herow(whoi).speed
   IF herow(whoi).ygo > 0 THEN herow(whoi).ygo -= herow(whoi).speed: caty(whoi * 5) -= herow(whoi).speed
   IF herow(whoi).ygo < 0 THEN herow(whoi).ygo += herow(whoi).speed: caty(whoi * 5) += herow(whoi).speed
   didgo(whoi) = YES
  END IF
  cropmovement catx(whoi * 5), caty(whoi * 5), herow(whoi).xgo, herow(whoi).ygo
 NEXT whoi

 'Update lists of current zones and run zone entry+exit triggers
 'We do this each tick instead of only when completing a step because they need to be
 'rechecked when the hero position changes for any reason (eg. script commands), or when
 'the zone map changes (eg. loading map state), and hooking into all those places is too
 'much of a maintenance burden. Plus also each-step detection sucks.
 FOR whoi as integer = 0 TO caterpillar_size() - 1
  update_hero_zones whoi
 NEXT
 
 FOR whoi as integer = 0 TO caterpillar_size() - 1

  DIM steppingslot as integer = whoi
  '--If caterpillar is not suspended, only the leader's motion determines a step
  '--(a limitation of the caterpillar party)
  IF readbit(gen(), genSuspendBits, suspendcaterpillar) = 0 THEN steppingslot = 0

  IF didgo(steppingslot) = YES AND (herow(steppingslot).xgo MOD 20) = 0 AND (herow(steppingslot).ygo MOD 20) = 0 THEN
   '--Stuff that should only happen when a hero finishs a step

   '--Run each-step zone triggers
   process_zone_eachstep_triggers "hero" & whoi, herow(whoi).curzones

   '--Check for harm tile
   DIM p as integer = readblock(pass, catx(whoi * 5) \ 20, caty(whoi * 5) \ 20)
   IF p AND passHarm THEN

    IF whoi = 0 AND readbit(gen(), genBits, 1) = 0 THEN
     'The caterpillar is disabled, so harm the whole party when the leader steps on a harm tile,
     'for some customisable defintion of 'whole'
     DIM howmany as integer
     IF readbit(gen(), genBits2, 12) THEN  'Harm tiles harm non-caterpillar heroes
      howmany = herocount  'whole active party
     ELSE
      'Old buggy behaviour: just the leader
      howmany = 1
     END IF
     FOR party_slot as integer = 0 TO howmany - 1
      IF gam.hero(party_slot).id >= 0 THEN
       gam.hero(party_slot).stat.cur.hp = large(gam.hero(party_slot).stat.cur.hp - gmap(9), 0)
      END IF
     NEXT
    ELSE
     '--harm single hero
     DIM party_slot as integer = rank_to_party_slot(whoi)
     gam.hero(party_slot).stat.cur.hp = large(gam.hero(party_slot).stat.cur.hp - gmap(9), 0)
    END IF

    IF gmap(10) THEN
     harmtileflash = YES
    END IF
    checkfatal = YES
   END IF

  END IF  'End of harm tile checking
 NEXT whoi

 'If the leader finished a step, check triggers
 IF (herow(0).xgo MOD 20 = 0) AND (herow(0).ygo MOD 20 = 0) AND (didgo(0) = YES OR force_step_check = YES) THEN

  'Trigger step-on NPCs
  IF readbit(gen(), genSuspendBits, suspendobstruction) = 0 THEN
   '--check for step-on NPCS
   FOR i as integer = 0 TO UBOUND(npc)
    WITH npc(i)
     IF .id > 0 THEN '---NPC EXISTS---
      IF vstate.active = NO OR (vstate.dat.enable_npc_activation = YES AND vstate.npc <> i) THEN
       IF npcs(.id - 1).activation = 2 THEN '---NPC is step-on activated
        IF .x = catx(0) AND .y = caty(0) THEN '---YOU ARE ON NPC---
         usenpc 1, i
        END IF '---YOU ARE ON NPC---
       END IF '---NPC IS PASSABLE---
      END IF '--vehicle okay
     END IF '---NPC EXISTS
    END WITH
   NEXT i
  END IF

  'Trigger doors (only if the hero really moved, not just if force_step_check = YES)
  IF didgo(0) = YES THEN
   checkdoors
  END IF

  'Trigger battles
  'No random battle allowed on the first tick before fade-in (?)
  IF gam.need_fade_in = NO AND readbit(gen(), genSuspendBits, suspendrandomenemies) = 0 THEN
   DIM battle_formation_set as integer
   battle_formation_set = readblock(foemap, catx(0) \ 20, caty(0) \ 20)
   IF vstate.active = YES THEN
    '--Riding a vehicle
    IF vstate.dat.random_battles > 0 THEN
     '--This vehicle overrides the random battle formation set
     battle_formation_set = vstate.dat.random_battles
    ELSEIF vstate.dat.random_battles = -1 THEN
     '--This vehicle disables random battles
     battle_formation_set = 0
    END IF
   END IF
   IF battle_formation_set > 0 THEN
    DIM formset as FormationSet
    LoadFormationSet formset, battle_formation_set
    IF istag(formset.tag, YES) THEN
     gam.random_battle_countdown = gam.random_battle_countdown - formset.frequency

     IF gam.random_battle_countdown <= 0 THEN
      gam.random_battle_countdown = range(100, 60)
      DIM battle_formation as integer = random_formation(battle_formation_set)
      IF gmap(13) <= 0 THEN 'if no random battle script is defined
       IF battle_formation >= 0 THEN 'and if the randomly selected battle is valid
        'trigger a normal random battle
        fatal = NO
        gam.wonbattle = battle(battle_formation)
        prepare_map YES
        queue_fade_in 1
       END IF
      ELSE
       'trigger the instead-of-battle script
       trigger_script gmap(13), YES, "instead-of-battle", "triggered at " & (catx(0) \ 20) & "," & (caty(0) \ 20), scrqBackcompat()
       trigger_script_arg 0, battle_formation, "formation"
       trigger_script_arg 1, battle_formation_set, "formation set"
      END IF
     END IF
    END IF
   END IF
  END IF

  'Each step trigger
  IF gmap(14) > 0 THEN
   trigger_script gmap(14), YES, "eachstep", "map " & gam.map.id, scrqBackcompat()
   trigger_script_arg 0, catx(0) \ 20, "tile x"
   trigger_script_arg 1, caty(0) \ 20, "tile y"
   trigger_script_arg 2, catd(0), "direction"
  END IF

 END IF '--End of on-step triggers
END SUB

SUB process_zone_eachstep_triggers(who as string, byval zones as integer vector)
 FOR i as integer = 0 TO v_len(zones) - 1
  'debuginfo who & " step in zone " & zones[i]
 NEXT
END SUB

SUB process_zone_entry_triggers(who as string, byval oldzones as integer vector, byval newzones as integer vector)
 'Check for differences between two sorted lists of zone IDs, and run entry and exit triggers

 DIM oldi as integer = 0  'index in oldzones()
 DIM newi as integer = 0  'index in newzones()
 DO
  DIM oldzone as integer = IIF(oldi < v_len(oldzones), oldzones[oldi], 999999)
  DIM newzone as integer = IIF(newi < v_len(newzones), newzones[newi], 999999)

  IF oldzone = 999999 AND newzone = 999999 THEN EXIT DO

  IF oldzone > newzone THEN
   'Found newly entered zone
   '(add triggers here)
   'debuginfo who & " entered " & newzone
   newi += 1
  ELSEIF oldzone < newzone THEN
   'Left a zone
   'debuginfo who & " left " & oldzone
   oldi += 1
  ELSE
   'Same zone appears in both lists
   newi += 1
   oldi += 1
  END IF
 LOOP
END SUB

SUB update_hero_zones(byval who as integer)
 DIM newzones as integer vector
 v_move newzones, GetZonesAtTile(zmap, catx(who * 5) \ 20, caty(who * 5) \ 20)
 process_zone_entry_triggers "hero" & who, herow(who).curzones, newzones
 v_move herow(who).curzones, newzones
END SUB

SUB update_npc_zones(byval npcref as integer)
 DIM newzones as integer vector
 v_move newzones, GetZonesAtTile(zmap, npc(npcref).x \ 20, npc(npcref).y \ 20)
 process_zone_entry_triggers "npc" & npcref, npc(npcref).curzones, newzones
 v_move npc(npcref).curzones, newzones
END SUB

'NPC movement
'Note that NPC xgo and ygo can also be set from elsewhere, eg. being pushed
SUB update_npcs ()
 FOR o as integer = 0 TO UBOUND(npc)
  IF npc(o).id > 0 THEN
   DIM as integer id = (npc(o).id - 1)

   '--if this is the active vehicle
   IF vstate.active = YES AND vstate.npc = o THEN
    '--if we are not scrambling clearing or aheading
    IF vstate.mounting = NO AND vstate.trigger_cleanup = NO AND vstate.ahead = NO THEN
     '--match vehicle to main hero
     npc(o).x = catx(0)
     npc(o).y = caty(0)
     npc(o).z = catz(0)  'NPC Z value is matched to the hero in update_vehicle_state for simplicity, but
                         'this is here in case of setheroz or setnpcz or loaded map state or other funkiness happens
     npc(o).dir = catd(0)
     npc(o).frame = herow(0).wtog
    END IF
   ELSE
    '--For all NPCs except the active vehicle
    IF txt.sayer <> o AND readbit(gen(), genSuspendBits, suspendnpcs) = 0 AND npc(o).suspend_ai = 0 THEN
     IF npc(o).xgo = 0 AND npc(o).ygo = 0 THEN
      pick_npc_action npc(o), npcs(id)
     END IF
    END IF
   END IF

   DIM finished_step as integer = NO
   IF npc(o).xgo <> 0 OR npc(o).ygo <> 0 THEN finished_step = perform_npc_move(o, npc(o), npcs(id))

   'Recalculate current zones every tick (see update_heroes for rationale)
   update_npc_zones o

   IF finished_step THEN
    process_zone_eachstep_triggers "npc" & o, npc(o).curzones
   END IF

  END IF
 NEXT o
END SUB

'A currently stationary NPC decides what to do.
'Most move types are implemented here, but some are handled upon collision in perform_npc_move
SUB pick_npc_action(npci as NPCInst, npcdata as NPCType)
 DIM as integer movetype = npcdata.movetype
 DIM as integer speedset = npcdata.speed
 DIM as integer temp, rand
 IF movetype > 0 AND (speedset > 0 OR movetype = 8) THEN
  'RANDOM WANDER---
  IF movetype = 1 THEN
   rand = 25
   IF wraptouch(npci.x, npci.y, catx(0), caty(0), 20) THEN rand = 5
   IF randint(100) < rand THEN
    temp = randint(4)
    npci.dir = temp
    IF temp = 0 THEN npci.ygo = 20
    IF temp = 2 THEN npci.ygo = -20
    IF temp = 3 THEN npci.xgo = 20
    IF temp = 1 THEN npci.xgo = -20
   END IF
  END IF '---RANDOM WANDER
  'ASSORTED PACING---
  IF movetype >= 2 AND movetype <= 5 THEN
   IF npci.dir = 0 THEN npci.ygo = 20
   IF npci.dir = 2 THEN npci.ygo = -20
   IF npci.dir = 3 THEN npci.xgo = 20
   IF npci.dir = 1 THEN npci.xgo = -20
  END IF '---ASSORTED PACING
  'CHASE/FLEE---
  IF movetype = 6 OR movetype = 7 THEN
   rand = 100
   IF randint(100) < rand THEN
    IF randint(100) < 50 THEN
     IF caty(0) < npci.y THEN temp = 0
     IF caty(0) > npci.y THEN temp = 2
     IF gmap(5) = 1 AND caty(0) - mapsizetiles.y * 10 > npci.y THEN temp = 0
     IF gmap(5) = 1 AND caty(0) + mapsizetiles.y * 10 < npci.y THEN temp = 2
     IF caty(0) = npci.y THEN temp = randint(4)
    ELSE
     IF catx(0) < npci.x THEN temp = 3
     IF catx(0) > npci.x THEN temp = 1
     IF gmap(5) = 1 AND catx(0) - mapsizetiles.x * 10 > npci.x THEN temp = 3
     IF gmap(5) = 1 AND catx(0) + mapsizetiles.x * 10 < npci.x THEN temp = 1
     IF catx(0) = npci.x THEN temp = randint(4)
    END IF
    IF movetype = 7 THEN temp = loopvar(temp, 0, 3, 2)  'Flee
    npci.dir = temp
    IF temp = 0 THEN npci.ygo = 20
    IF temp = 2 THEN npci.ygo = -20
    IF temp = 3 THEN npci.xgo = 20
    IF temp = 1 THEN npci.xgo = -20
   END IF
  END IF '---CHASE/FLEE
  'WALK IN PLACE---
  IF movetype = 8 THEN
   npci.frame = loopvar(npci.frame, 0, 3, 1)
  END IF '---WALK IN PLACE
 END IF
END SUB

FUNCTION perform_npc_move(byval npcnum as integer, npci as NPCInst, npcdata as NPCType) as integer
 '--npcnum is the npc() index of npci.
 '--Here we attempt to actually update the coordinates for this NPC, checking obstructions
 '--Return true if we finished a step (didgo)
 DIM didgo as integer = NO
 npci.frame = loopvar(npci.frame, 0, 3, 1)
 IF movdivis(npci.xgo) OR movdivis(npci.ygo) THEN
  'About to begin moving to a new tile
  IF readbit(gen(), genSuspendBits, suspendnpcwalls) = 0 AND npci.ignore_walls = 0 THEN
   '--this only happens if NPC walls on
   IF wrappass(npci.x \ 20, npci.y \ 20, npci.xgo, npci.ygo, 0) THEN
    npci.xgo = 0
    npci.ygo = 0
    npchitwall(npci, npcdata)
    GOTO nogo
   END IF
   '--Check for movement zones (treat the edges as walls)
   DIM zone as integer = npcdata.defaultzone
   IF zone = 0 THEN zone = gmap(32)  'fallback to default
   IF zone > 0 ANDALSO wrapzonecheck(zone, npci.x, npci.y, npci.xgo, npci.ygo) = 0 THEN
    npci.xgo = 0
    npci.ygo = 0
    npchitwall(npci, npcdata)
    GOTO nogo
   END IF
   '--Check for avoidance zones (treat as walls)
   zone = npcdata.defaultwallzone
   IF zone = 0 THEN zone = gmap(33)  'fallback to default
   IF zone > 0 ANDALSO wrapzonecheck(zone, npci.x, npci.y, npci.xgo, npci.ygo) THEN
    npci.xgo = 0
    npci.ygo = 0
    npchitwall(npci, npcdata)
    GOTO nogo
   END IF
  END IF
  IF readbit(gen(), genSuspendBits, suspendobstruction) = 0 AND npci.not_obstruction = 0 THEN
   '--this only happens if obstruction is on
   '---Check for NPC-NPC collision
   FOR i as integer = 0 TO UBOUND(npc)
    IF npc(i).id > 0 AND npcnum <> i AND npc(i).not_obstruction = 0 THEN
     IF wrapcollision (npc(i).x, npc(i).y, npc(i).xgo, npc(i).ygo, npci.x, npci.y, npci.xgo, npci.ygo) THEN
      npci.xgo = 0
      npci.ygo = 0
      npchitwall(npci, npcdata)
      GOTO nogo
     END IF
    END IF
   NEXT i
   '---Check for hero-NPC collision
   IF npcdata.activation <> 2 THEN  'Not step-on activated
    IF wrapcollision (npci.x, npci.y, npci.xgo, npci.ygo, catx(0), caty(0), herow(0).xgo, herow(0).ygo) THEN
     npci.xgo = 0
     npci.ygo = 0
     '--a 0-3 tick delay before pacing enemies bounce off hero
     IF npci.frame = 3 THEN
      npchitwall(npci, npcdata)
      GOTO nogo
     END IF
    END IF
   END IF
  END IF
 END IF

 'If we didn't hit any obstacle, actually move
 IF npcdata.speed THEN
  '--change x,y and decrement wantgo by speed
  IF npci.xgo OR npci.ygo THEN
   IF npci.xgo > 0 THEN npci.xgo -= npcdata.speed: npci.x -= npcdata.speed
   IF npci.xgo < 0 THEN npci.xgo += npcdata.speed: npci.x += npcdata.speed
   IF npci.ygo > 0 THEN npci.ygo -= npcdata.speed: npci.y -= npcdata.speed
   IF npci.ygo < 0 THEN npci.ygo += npcdata.speed: npci.y += npcdata.speed
   IF (npci.xgo MOD 20) = 0 AND (npci.ygo MOD 20) = 0 THEN didgo = YES
  END IF
 ELSE
  '--no speed, kill wantgo
  npci.xgo = 0
  npci.ygo = 0
 END IF
 IF cropmovement(npci.x, npci.y, npci.xgo, npci.ygo) THEN npchitwall(npci, npcdata)

 nogo:

 '--Check touch activation (always happens). I have no idea why this is here!
 IF npcdata.activation = 1 AND txt.showing = NO THEN
  IF wraptouch(npci.x, npci.y, catx(0), caty(0), 20) THEN
   usenpc 1, npcnum
  END IF
 END IF

 RETURN didgo
END FUNCTION

SUB npchitwall(npci as NPCInst, npcdata as NPCType)
 IF npci.suspend_ai = 0 THEN
  IF npcdata.movetype = 2 THEN npci.dir = loopvar(npci.dir, 0, 3, 2)  'Pace
  IF npcdata.movetype = 3 THEN npci.dir = loopvar(npci.dir, 0, 3, 1)  'Right Turns
  IF npcdata.movetype = 4 THEN npci.dir = loopvar(npci.dir, 0, 3, -1) 'Left Turns
  IF npcdata.movetype = 5 THEN npci.dir = randint(4)                'Random Turns
 END IF
END SUB

SUB execute_script_fibres
 WHILE nowscript >= 0
  WITH scriptinsts(nowscript)
   IF .waiting THEN
    process_wait_conditions
   END IF
   IF .waiting THEN
    EXIT WHILE
   END IF

   '--interpret script
   insideinterpreter = YES
   wantimmediate = 0
   'May set wantimmediate to -1 to indicate fibre finished, or -2 to indicate fibre
   'finished in way that triggered bug 430
   scriptinterpreter
   insideinterpreter = NO

   IF wantimmediate = -2 THEN
    'IF nowscript < 0 THEN
    ' debug "wantimmediate ended on nowscript = -1"
    'ELSE
    ' debug "wantimmediate would have skipped wait on command " & commandname(scrat(nowscript).curvalue) _
    '       & " in " & scriptname(scrat(nowscript).id) & ", state = " & scrat(nowscript).state
    'END IF
    IF readbit(gen(), genBits2, 17) THEN
     'Reenable bug 430 (see also bug 550), where if two scripts were triggered at once then
     'when the top script ended it would cause the one below it to run for two ticks.
     wantimmediate = -1
    ELSE
     wantimmediate = 0
    END IF
   END IF

   IF wantimmediate = 0 THEN EXIT WHILE
  END WITH
 WEND
END SUB

SUB interpret()
 'It seems like it would be good to call this immediately before scriptinterpreter so that
 'the return values of fightformation and waitforkey are correct, however doing so might
 'break something?
 run_queued_scripts

 execute_script_fibres

 script_log_tick
 gam.script_log.tick += 1

 'Do spawned text boxes, battles, etc.
 'The actual need for these gam.want.* variables is now gone, but they are kept around for backcompat.
 'They could be removed and the implementations moved straight into the command handlers,
 '(and the implicit waits made optional at the same time), but this makes things especially tricky
 'for concurrent fibres.
 'For example if a script changes the map (whether through a textbox, teleporttomap, or door use)
 'it currently prevents any other script from running for the rest of the tick, preventing the potentially
 'disasterous (for scripted games) situation where the map changes and other scripts run before the
 'map autorun script (which might contain important initialisation). 

 'Also note that now if two fibres run two commands like fightformation and usedoor the order in which
 'they occur is independent of the order in which they were called.

 'FIXME: 
 'Currently if a map changes (or even is a game is loaded) there is one tick on the new map
 'before the map autorun or any other scripts can make changes. This transition is hidden by screen fades
 'and now by the delayed music change. But if the map change happens without a fade (teleporttomap, or
 'if we make fades customisable) that one tick delay is undesired.
 'So consider delaying all calls to preparemap (and doloadgame) until the start of the next tick.

 IF gam.want.box > 0 THEN
  loadsay gam.want.box
 END IF
 gam.want.box = 0
 IF gam.want.door > 0 THEN
  usedoor gam.want.door - 1
  gam.want.door = 0
 END IF
 IF gam.want.battle > 0 THEN
  fatal = NO
  gam.wonbattle = battle(gam.want.battle - 1)
  gam.want.battle = 0
  prepare_map YES
  gam.random_battle_countdown = range(100, 60)
  queue_fade_in 2 'FIXME: why 2 ticks?
  setkeys
 END IF
 IF gam.want.teleport THEN
  gam.want.teleport = NO
  prepare_map
  gam.random_battle_countdown = range(100, 60)
 END IF
 IF gam.want.usenpc > 0 THEN
  usenpc 2, gam.want.usenpc - 1
  gam.want.usenpc = 0
 END IF
 'ALSO gam.want.loadgame
END SUB

SUB loadmap_gmap(byval mapnum as integer)
 lump_reloading.gmap.dirty = NO
 lump_reloading.gmap.changed = NO
 loadrecord gmap(), game & ".map", getbinsize(binMAP) / 2, mapnum
 IF gmap(31) = 0 THEN gmap(31) = 2

 loadmaptilesets tilesets(), gmap()
 refresh_map_slice_tilesets
END SUB

SUB loadmap_npcl(byval mapnum as integer)
 lump_reloading.npcl.changed = NO
 lump_reloading.npcl.hash = hash_file(maplumpname(mapnum, "l"))
 LoadNPCL maplumpname(mapnum, "l"), npc()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
END SUB

SUB loadmap_npcd(byval mapnum as integer)
 lump_reloading.npcd.dirty = NO
 lump_reloading.npcd.changed = NO
 lump_reloading.npcd.hash = hash_file(maplumpname(mapnum, "n"))
 LoadNPCD maplumpname(mapnum, "n"), npcs()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
 'load NPC graphics
 reset_npc_graphics
END SUB

SUB loadmap_tilemap(byval mapnum as integer)
 lump_reloading.maptiles.dirty = NO
 lump_reloading.maptiles.changed = NO
 lump_reloading.maptiles.hash = hash_file(maplumpname(mapnum, "t"))
 LoadTileMaps maptiles(), maplumpname(mapnum, "t")
 mapsizetiles.w = maptiles(0).wide
 mapsizetiles.h = maptiles(0).high
 refresh_map_slice

 '--as soon as we know the dimensions of the map, enforce hero position boundaries
 cropposition catx(0), caty(0), 20
END SUB

SUB loadmap_passmap(byval mapnum as integer)
 lump_reloading.passmap.dirty = NO
 lump_reloading.passmap.changed = NO
 lump_reloading.passmap.hash = hash_file(maplumpname(mapnum, "p"))
 LoadTileMap pass, maplumpname(mapnum, "p")
END SUB

SUB loadmap_foemap(byval mapnum as integer)
 lump_reloading.foemap.dirty = NO
 lump_reloading.foemap.changed = NO
 lump_reloading.foemap.hash = hash_file(maplumpname(mapnum, "e"))
 LoadTileMap foemap, maplumpname(mapnum, "e")
END SUB

SUB loadmap_zonemap(byval mapnum as integer)
 lump_reloading.zonemap.dirty = NO
 lump_reloading.zonemap.changed = NO
 '.Z is the only one of the map lumps that has been added in about the last decade
 DIM filename as string = maplumpname(mapnum, "z")
 IF isfile(filename) THEN
  LoadZoneMap zmap, filename
  lump_reloading.zonemap.hash = hash_file(filename)
 ELSE
  CleanZoneMap zmap, mapsizetiles.x, mapsizetiles.y
  lump_reloading.zonemap.hash = 0
 END IF
END SUB

SUB loadmaplumps (byval mapnum as integer, byval loadmask as integer)
 'loads some, but not all the lumps needed for each map
 IF loadmask AND 1 THEN
  loadmap_gmap mapnum
 END IF
 IF loadmask AND 2 THEN
  loadmap_npcl mapnum
 END IF
 IF loadmask AND 4 THEN
  loadmap_npcd mapnum
 END IF
 IF loadmask AND 8 THEN
  loadmap_tilemap mapnum
 END IF
 IF loadmask AND 16 THEN
  loadmap_passmap mapnum
 END IF
 IF loadmask AND 32 THEN
  loadmap_zonemap mapnum
 END IF
END SUB

SUB MenuSound(byval s as integer)
  IF s THEN
    stopsfx s-1
    playsfx s-1, 0
  END IF
END SUB

SUB usemenusounds (byval deckey as integer = scUp, byval inckey as integer = scDown)
  IF keyval(deckey) > 1 ORELSE keyval(inckey) > 1 ORELSE keyval(scPageup) > 1 _
       ORELSE keyval(scPagedown) > 1 ORELSE keyval(scHome) > 1 ORELSE keyval(scEnd) > 1 THEN
    menusound gen(genCursorSFX)
  END IF
END SUB

FUNCTION should_skip_this_timer(byval timercontext as integer, tmr as PlotTimer) as integer
 IF timercontext = TIMER_BATTLE THEN
  'This is happening in battle!
  IF (tmr.flags AND TIMERFLAG_BATTLE) = 0 THEN
   RETURN YES
  END IF
 ELSEIF timercontext = TIMER_BLOCKINGMENUS THEN
  'This is happening in a menu!
  IF (tmr.flags AND TIMERFLAG_MENU) = 0 THEN
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

SUB dotimer(byval timercontext as integer)
  dim i as integer
  for i = 0 to ubound(timers)
    with timers(i)
      if .pause then continue for
      if .speed > 0 then
        if should_skip_this_timer(timercontext, timers(i)) then continue for  'not supposed to run here
        'debug "i=" & i & " timercontext=" & timercontext & " .speed=" & .speed & " .ticks=" & .ticks & " .count=" & .count & " .flags=" & .flags & " .trigger=" & .trigger

        if .st > 0 then
          if plotstr(.st - 1).s = "" then plotstr(.st - 1).s = seconds2str(.count)
        end if

        .ticks += 1
        if .ticks >= .speed then
          .count -= 1
          .ticks = 0
          if .st > 0 and .count >= 0 then plotstr(.st - 1).s = seconds2str(.count)
          if .count < 0 then
            .finished_tick = gam.script_log.tick
            .speed *= -1
            .speed -= 1
            'do something
            if .trigger = TIMERTRIGGER_GAMEOVER then
              'FIXME: possible minor bug: does whether or not a fadeout occurs depend on whether there is a gameover script?
              fatal = YES
              abortg = 1

              exit sub
            end if

            if .trigger = TIMERTRIGGER_DEFAULT then
              'undefined, shouldn't happen
            end if

            if .trigger >= 0 then  'a plotscript
              ' NOTE: this doesn't run until the next tick (a design flaw)
              trigger_script .trigger, NO, "timer", "", scrqBackcompat()
              trigger_script_arg 0, i, "id"
            end if
          end if
        end if
      end if
    end with
  next
end sub

'Returns true if the battle should be exited immediately
function dotimerbattle() as integer
  dotimer TIMER_BATTLE  'no sense duplicating code

  dim i as integer
  for i = 0 to ubound(timers)
    with timers(i)
      if .speed < 0 then 'normally, not valid. but, if a timer expired in battle, this will be -ve, -1
        if .flags AND TIMERFLAG_CRITICAL then return -1
      end if
    end with
  next
  return 0
end function

FUNCTION add_menu (byval record as integer, byval allow_duplicate as integer=NO) as integer
 IF record >= 0 AND allow_duplicate = NO THEN
  'If adding a non-blank menu, first check if the requested menu is already open
  DIM menuslot as integer
  menuslot = find_menu_id(record)
  IF menuslot >= 0 THEN
   'the requested menu is already open, just bring it to the top
   bring_menu_forward menuslot
   RETURN menus(topmenu).handle
  END IF
 END IF
 'Load the menu into a new menu slot
 topmenu = topmenu + 1
 IF topmenu > UBOUND(menus) THEN
  REDIM PRESERVE menus(topmenu) as MenuDef
  REDIM PRESERVE mstates(topmenu) as MenuState
 END IF
 mstates(topmenu).pt = 0
 mstates(topmenu).top = 0
 IF record = -1 THEN
  ClearMenuData menus(topmenu)
 ELSE
  LoadMenuData menu_set, menus(topmenu), record
 END IF
 init_menu_state mstates(topmenu), menus(topmenu)
 IF topmenu > 0 THEN mstates(topmenu - 1).active = NO
 mstates(topmenu).active = YES
 check_menu_tags
 RETURN assign_menu_handles(menus(topmenu))
END FUNCTION

SUB remove_menu (byval slot as integer, byval run_on_close as integer=YES)
 IF slot < 0 OR slot > UBOUND(menus) THEN
  debugc errBug, "remove_menu: invalid slot " & slot
  EXIT SUB
 END IF
 bring_menu_forward slot
 IF menus(topmenu).advance_textbox = YES THEN
  'Advance an open text box.
  'Because this could open other menus, take care to remember this menu's handle
  '(Isn't it impossible for slot to change though?)
  DIM remember_handle as integer = menus(topmenu).handle
  advance_text_box
  slot = find_menu_handle(remember_handle)
  bring_menu_forward slot
 END IF
 IF menus(topmenu).on_close <> 0 AND run_on_close THEN
  trigger_script menus(topmenu).on_close, YES, "menu on-close", "menu " & menus(topmenu).record, scrqBackcompat()
 END IF
 ClearMenuData menus(topmenu)
 topmenu = topmenu - 1
 IF topmenu >= 0 THEN
  REDIM PRESERVE menus(topmenu) as MenuDef
  REDIM PRESERVE mstates(topmenu) as MenuState
  mstates(topmenu).active = YES
 END IF
END SUB

SUB bring_menu_forward (byval slot as integer)
 DIM i as integer
 IF slot < 0 OR slot > UBOUND(menus) OR slot > topmenu THEN
  scripterr "bring_menu_forward: invalid slot " & slot, serrBound
  EXIT SUB
 END IF
 mstates(topmenu).active = NO
 FOR i = slot TO topmenu - 1
  SWAP menus(i), menus(i + 1)
  SWAP mstates(i), mstates(i + 1)
 NEXT i
 mstates(topmenu).active = YES
END SUB

FUNCTION menus_allow_gameplay () as integer
 IF topmenu < 0 THEN RETURN YES
 RETURN menus(topmenu).allow_gameplay
END FUNCTION

FUNCTION menus_allow_player () as integer
 IF topmenu < 0 THEN RETURN YES
 RETURN menus(topmenu).suspend_player = NO
END FUNCTION

SUB update_menu_states ()
 FOR i as integer = 0 TO topmenu
  IF mstates(i).need_update THEN
   mstates(i).need_update = NO
   init_menu_state mstates(i), menus(i)
  END IF
 NEXT i
END SUB

SUB player_menu_keys ()
 DIM i as integer
 DIM activated as integer
 DIM menu_handle as integer
 DIM esc_menu as integer
 DIM save_margin as bool = NO
 IF topmenu >= 0 THEN
  IF menus(topmenu).no_controls = YES THEN EXIT SUB
  menu_handle = menus(topmenu).handle 'store handle for later use
  'Following controls useable on empty menus too

  IF carray(ccMenu) > 1 AND menus(topmenu).no_close = NO THEN
   carray(ccMenu) = 0
   setkeys ' Forget keypress that closed the menu
   esc_menu = menus(topmenu).esc_menu - 1
   remove_menu topmenu
   menusound gen(genCancelSFX)
   IF esc_menu >= 0 THEN
    add_menu esc_menu
   END IF
   EXIT SUB
  END IF

  'Following controls are for non-empty menus only
  IF mstates(topmenu).last = -1 THEN EXIT SUB

  IF game_usemenu(mstates(topmenu)) THEN
   menusound gen(genCursorSFX)
  END IF
  activated = NO
  DIM mi as MenuDefItem '--using a copy of the menu item here is safer (in future) because activate_menu_item() can deallocate it
  mi = *menus(topmenu).items[mstates(topmenu).pt]
  IF mi.disabled THEN EXIT SUB
  IF mi.t = 1 AND mi.sub_t = 11 THEN '--volume
   IF carray(ccLeft) > 1 THEN set_music_volume large(get_music_volume - 1/16, 0.0)
   IF carray(ccRight) > 1 THEN set_music_volume small(get_music_volume + 1/16, 1.0)
  END IF
  IF mi.t = 1 AND mi.sub_t = 14 THEN '--TV safe margin
   IF carray(ccLeft) > 1 THEN
    set_safe_zone_margin large(get_safe_zone_margin() - 1, 0)
    save_margin = YES
   END IF
   IF carray(ccRight) > 1 THEN
    set_safe_zone_margin small(get_safe_zone_margin() + 1, 10)
    save_margin = YES
   END IF
   IF save_margin THEN
    save_margin = NO
    write_ini_value prefsdir & SLASH & "gameconfig.ini", "gfx.margin", get_safe_zone_margin()
   END IF
  END IF
  IF carray(ccUse) > 1 THEN
   activate_menu_item mi, topmenu
  END IF
 END IF
END SUB

FUNCTION activate_menu_item(mi as MenuDefItem, byval menuslot as integer) as integer
 DIM open_other_menu as integer = -1
 DIM menu_text_box as integer = 0
 DIM updatetags as integer = NO
 DIM slot as integer
 DIM activated as integer = YES
 menu_text_box = 0
 DO 'This DO exists to allow EXIT DO
  WITH mi
   SELECT CASE .t
    CASE 0 ' Label
     SELECT CASE .sub_t
      CASE 0 'Selectable
      CASE 1 'Unselectable
       activated = NO
     END SELECT
    CASE 1 ' Special
     SELECT CASE .sub_t
      CASE 0 ' item
       menu_text_box = item_screen()
       IF menu_text_box > 0 THEN
        IF mi.close_if_selected = NO THEN
         remove_menu menuslot, (mi.skip_close_script = NO)
        END IF
        EXIT DO
       END IF
      CASE 1 ' spell
       slot = onwho(readglobalstring(106, "Whose Spells?", 20), 0)
       IF slot >= 0 THEN old_spells_menu slot
      CASE 2 ' status
       slot = onwho(readglobalstring(104, "Whose Status?", 20), 0)
       IF slot >= 0 THEN status_screen slot
      CASE 3 ' equip
       slot = onwho(readglobalstring(108, "Equip Whom?", 20), 0)
       IF slot >= 0 THEN equip slot
      CASE 4 ' order
       hero_swap_menu 0
      CASE 5 ' team
       hero_swap_menu 1
      CASE 6 ' order/team
       hero_swap_menu readbit(gen(), genBits, 5)
      CASE 7,12 ' map
       minimap catx(0), caty(0)
      CASE 8,13 ' save
       slot = picksave(0)
       IF slot >= 0 THEN savegame slot
      CASE 9 ' load
       slot = picksave(1, NO, YES)  'No New Game option, beep if the menu doesn't display
       '(Maybe it would be better to display the load menu even if there are no saves)
       IF slot >= 0 THEN
        gam.want.loadgame = slot + 1
        FOR i as integer = topmenu TO 0 STEP -1
         remove_menu i, NO
        NEXT i
        EXIT DO
       END IF
      CASE 10 ' quit
       menusound gen(genAcceptSFX)
       verify_quit
      CASE 11 ' volume
       activated = NO
      CASE 15 ' purchases
       purchases_menu()
     END SELECT
    CASE 2 ' Menu
     open_other_menu = .sub_t
    CASE 3 ' Text box
     menu_text_box = .sub_t
    CASE 4 ' Run Script
     trigger_script .sub_t, YES, "menuitem", "item '" & get_menu_item_caption(mi, menus(menuslot)) & "' in menu " & menus(menuslot).record, scrqBackcompat()
     IF menus(topmenu).allow_gameplay THEN
      IF .close_if_selected THEN
       'The menu item handle would be useless, so as a special case pass 0
       '(rather than having confusing special case behaviour)
       trigger_script_arg 0, 0, "dummy"
      ELSE
       'Normally, pass a menu item handle
       trigger_script_arg 0, .handle, "item handle"
      END IF
     ELSE
      'but if the topmost menu suspends gameplay, then a handle will always be invalid
      'by the time the script runs, so pass the extra values instead.
      trigger_script_arg 0, .extra(0), "extra0"
      trigger_script_arg 1, .extra(1), "extra1"
      trigger_script_arg 2, .extra(2), "extra2"
     END IF
   END SELECT
  END WITH
  EXIT DO
 LOOP
 IF activated THEN
  IF ABS(mi.settag) > 1 THEN settag mi.settag : updatetags = YES
  IF mi.togtag > 1 THEN settag mi.togtag, NOT istag(mi.togtag, 0) : updatetags = YES
  IF mi.close_if_selected THEN
   remove_menu menuslot, (mi.skip_close_script = NO)

   'WARNING: below this point, mi is invalid

   IF insideinterpreter = NO THEN '--Not inside a script
    carray(ccUse) = 0
    setkeys '--Discard the keypress that triggered the menu item that closed the menu
   END IF
  END IF
 END IF
 IF open_other_menu >= 0 THEN
  add_menu open_other_menu
 END IF
 IF menu_text_box > 0 THEN
  '--player has triggered a text box from the menu--
  loadsay menu_text_box
  menu_text_box = 0
 END IF
 IF updatetags THEN
  evalherotags
  evalitemtags
  tag_updates
 END IF
 RETURN activated
END FUNCTION

'Call this any time a tag is changed!
SUB tag_updates (byval npc_visibility as integer=YES)
 IF npc_visibility THEN visnpc
 check_menu_tags
END SUB

SUB check_menu_tags ()
 DIM i as integer
 DIM j as integer
 DIM old as integer
 DIM changed as integer
 DIM remember as integer
 DIM selecteditem as MenuDefItem ptr
 FOR j = 0 TO topmenu
  WITH menus(j)
   changed = NO
   FOR i = 0 TO .numitems - 1
    WITH *.items[i]
     old = .disabled
     .disabled = NO
     IF NOT (istag(.tag1, YES) AND istag(.tag2, YES)) THEN .disabled = YES
     IF .t = 0 AND .sub_t = 1 THEN .disabled = YES
     IF .t = 1 AND .sub_t = 7 AND gmap(2) = 0 THEN .disabled = YES 'Minimap disabled on this map
     IF .t = 1 AND .sub_t = 8 AND gmap(3) = 0 THEN .disabled = YES 'Save anywhere disabled on this map
     IF .t = 1 AND .sub_t = 14 AND NOT supports_safe_zone_margin() THEN .disabled = YES 'TV Safe Margin disabled on backends that don't support it
     IF .t = 1 AND .sub_t = 15 AND NOT supports_in_app_purchases() THEN .disabled = YES 'Purchases disabled on platforms that don't have a supported store
     IF old <> .disabled THEN changed = YES
    END WITH
   NEXT i
   'FIXME: the following is meant to be handled by init_menu_state... in fact usemenu already does this
   IF changed = YES THEN
    IF mstates(j).pt >= 0 THEN
     selecteditem = .items[mstates(j).pt]
    ELSE
     selecteditem = NULL
    END IF
    SortMenuItems menus(j)
    IF selecteditem THEN
     'first forwards look for the next visible item
     WHILE selecteditem ANDALSO (selecteditem->disabled AND selecteditem->hide_if_disabled)
      selecteditem = selecteditem->trueorder.next
      FOR i = 0 TO .numitems - 1
       IF .items[i] = selecteditem THEN mstates(j).pt = i
      NEXT i
     WEND
     IF selecteditem = NULL THEN
      'then try last visible (ie, look backwards), finally give up
      mstates(j).pt = -1
      FOR i = .numitems - 1 TO 0 STEP -1
       IF (.items[i]->disabled AND .items[i]->hide_if_disabled) = 0 THEN
        mstates(j).pt = i
        EXIT FOR
       END IF
      NEXT
     END IF
    END IF
    mstates(j).need_update = YES
   END IF
  END WITH
 NEXT j
 update_menu_states
END SUB

FUNCTION game_usemenu (state as MenuState) as integer
 DIM oldptr as integer
 DIM oldtop as integer

 WITH state
  oldptr = .pt
  oldtop = .top

  IF carray(ccUp) > 1 THEN .pt = loopvar(.pt, .first, .last, -1) 'UP
  IF carray(ccDown) > 1 THEN .pt = loopvar(.pt, .first, .last, 1)  'DOWN
  IF keyval(scPageup) > 1 THEN .pt = large(.pt - .size, .first)     'PGUP
  IF keyval(scPagedown) > 1 THEN .pt = small(.pt + .size, .last)      'PGDN
  IF keyval(scHome) > 1 THEN .pt = .first                         'HOME
  IF keyval(scEnd) > 1 THEN .pt = .last                          'END
  .top = bound(.top, .pt - .size, .pt)

  IF oldptr <> .pt OR oldtop <> .top THEN RETURN YES
  RETURN NO
 END WITH
END FUNCTION

FUNCTION allowed_to_open_main_menu () as integer
 DIM i as integer
 IF find_menu_id(0) >= 0 THEN RETURN NO 'Already open
 FOR i = topmenu TO 0 STEP -1
  IF menus(i).prevent_main_menu = YES THEN RETURN NO
 NEXT i
 RETURN YES
END FUNCTION

FUNCTION random_formation (byval set as integer) as integer
 DIM formset as FormationSet
 DIM as integer i, num
 STATIC foenext as integer = 0
 LoadFormationSet formset, set
 FOR i = 0 TO UBOUND(formset.formations)
  IF formset.formations(i) >= 0 THEN num += 1
 NEXT
 IF num = 0 THEN RETURN -1

 'surprisingly, this is actually slightly effective at reducing the rate of the
 'same slot being picked consecutively, so I'll leave it be for now
 'FIXME: When this was written, I confused the meaning of range; should improve this
 FOR i = 0 TO randint(range(19, 27))
  DO
   foenext = loopvar(foenext, 0, UBOUND(formset.formations), 1)
  LOOP WHILE formset.formations(foenext) = -1
 NEXT
 RETURN formset.formations(foenext)
END FUNCTION

SUB prepare_map (byval afterbat as integer=NO, byval afterload as integer=NO)
 'DEBUG debug "in preparemap"

 DIM i as integer
 'save data from old map
 IF gam.map.lastmap > -1 THEN
  IF gmap(17) = 1 THEN
   savemapstate_npcd gam.map.lastmap, "map"
   savemapstate_npcl gam.map.lastmap, "map"
  END IF
  IF gmap(18) = 1 THEN
   savemapstate_tilemap gam.map.lastmap, "map"
   savemapstate_passmap gam.map.lastmap, "map"
   savemapstate_zonemap gam.map.lastmap, "map"
  END IF
 END IF
 IF running_as_slave THEN make_map_backups

 gam.map.lastmap = gam.map.id

 '--- Load new map's data

 'load gmap
 loadmapstate_gmap gam.map.id, "map"

 'Play map music
 IF readbit(gen(), genSuspendBits, suspendambientmusic) = 0 THEN
  IF gmap(1) >= 0 THEN
   queue_music_change gmap(1) - 1
  ELSEIF gmap(1) = -1 AND afterbat = YES THEN
   queue_music_change gam.remembermusic
  END IF
 END IF

 gam.map.name = getmapname(gam.map.id)

 IF gmap(18) < 2 THEN
  loadmapstate_tilemap gam.map.id, "map"
  loadmapstate_passmap gam.map.id, "map"
  loadmapstate_zonemap gam.map.id, "map"
 ELSE
  loadmap_tilemap gam.map.id
  loadmap_passmap gam.map.id
  loadmap_zonemap gam.map.id
 END IF
 loadmap_foemap gam.map.id

 IF afterbat = NO THEN
  recreate_map_slices
 END IF

 IF afterbat = NO THEN
  gam.showtext = gam.map.name
  gam.showtext_ticks = gmap(4)
  IF gmap(17) < 2 THEN
   loadmapstate_npcd gam.map.id, "map"
   loadmapstate_npcl gam.map.id, "map"
  ELSE
   loadmap_npcd gam.map.id
   loadmap_npcl gam.map.id
  END IF
 END IF

 loaddoor gam.map.id

 '--- Update/clean up various state

 'Hero/caterpillar party and vehicle
 IF afterbat = NO AND gam.map.same = NO THEN
  forcedismount catd()
 END IF
 IF afterbat = NO AND afterload = NO THEN
  FOR i = 0 TO 15
   catx(i) = catx(0)
   caty(i) = caty(0)
   catd(i) = catd(0)
   catz(i) = 0
  NEXT i
 END IF
 IF afterload = YES THEN
  interpolatecat
  herow(0).xgo = 0
  herow(0).ygo = 0
  herow(0).speed = 4
 END IF
 IF vstate.active = YES AND gam.map.same = YES THEN
  FOR i = 0 TO 3
   catz(i) = vstate.dat.elevation
  NEXT i
  npc(vstate.npc).z = vstate.dat.elevation
  herow(0).speed = vstate.dat.speed
  IF herow(0).speed = 3 THEN herow(0).speed = 10
 END IF

 txt.sayer = -1

 'If following NPC or slice on old map, reset camera
 IF afterbat = NO THEN
  IF gen(genCameraMode) = slicecam ANDALSO valid_plotslice(gen(genCameraArg1), serrIgnore) = NO  _
     OR gen(genCameraMode) = npccam THEN
   '(Note that normally when following an invalid slice we stop the camera instead)
   gen(genCameraMode) = herocam
   gen(genCameraArg1) = 0
  END IF
 END IF

 IF afterbat = NO THEN
  IF gmap(7) > 0 THEN
   trigger_script gmap(7), YES, "map autorun", "map " & gam.map.id, scrqBackcompat()
   trigger_script_arg 0, gmap(8), "arg"
  END IF
 ELSE
  IF gmap(12) > 0 THEN
   trigger_script gmap(12), NO, "afterbattle", "", scrqBackcompat()
   '--afterbattle script gets one arg telling if you won or ran
   trigger_script_arg 0, IIF(gam.wonbattle, 1, 0), "wonbattle"
  END IF
 END IF
 gam.map.same = NO

 'For heroes, we trigger zone exit scripts for the zones the hero was inside
 'on the previous map, and zone entry scripts for the new zones
 FOR whoi as integer = 0 TO caterpillar_size() - 1
  update_hero_zones whoi
 NEXT

 'For NPCs, we don't run zone exit scripts (because the NPCs no longer exist)
 'for the previous map, but we do run the entry scripts for the new map
 'UNLESS (unimplemented, FIXME) restoring a saved map state
 FOR npcref as integer = 0 TO UBOUND(npc)
  IF npc(npcref).id > 0 THEN
   update_npc_zones npcref
  END IF
 NEXT

 'DEBUG debug "end of preparemap"
END SUB

'Return the ID of a door at a tile, or -1 for none
'(There should only be one door on each tile, because the editor doesn't let you place more)
FUNCTION find_door (byval tilex as integer, byval tiley as integer) as integer
 FOR door_id as integer = 0 TO 99
  IF readbit(gam.map.door(door_id).bits(), 0, 0) THEN  'Door exists
   IF gam.map.door(door_id).x = tilex AND gam.map.door(door_id).y = tiley + 1 THEN
    RETURN door_id
   END IF
  END IF
 NEXT door_id
 RETURN -1
END FUNCTION

SUB checkdoors ()
 'If the leader is standing on a door, use it.
 IF vstate.active = YES AND vstate.dat.enable_door_use = NO THEN EXIT SUB 'Doors are disabled by a vehicle
 IF readbit(gen(), genSuspendBits, suspenddoors) = 1 THEN EXIT SUB

 DIM door_id as integer
 door_id = find_door(catx(0) \ 20, caty(0) \ 20)
 IF door_id >= 0 THEN usedoor door_id
END SUB

FUNCTION find_doorlink (byval door_id as integer) as integer
 'Returns the index in gam.map.doorlinks() which is active for this door,
 'or -1 if none are, or if the door does not even exist.

 IF readbit(gam.map.door(door_id).bits(), 0, 0) = 0 THEN RETURN -1

 deserdoorlinks maplumpname(gam.map.id,"d"), gam.map.doorlinks()

 FOR i as integer = 0 TO UBOUND(gam.map.doorlinks)
  WITH gam.map.doorlinks(i)
   IF door_id = .source THEN
    IF istag(.tag1, YES) AND istag(.tag2, YES) THEN 'Check tags to make sure this door is okay
     RETURN i
    END IF
   END IF
  END WITH
 NEXT i
 RETURN -1
END FUNCTION

SUB usedoor (byval door_id as integer)
 DIM linknum as integer = find_doorlink(door_id)
 IF linknum = -1 THEN EXIT SUB

 WITH gam.map.doorlinks(linknum)
  gam.map.same = (.dest_map = gam.map.id)
  gam.map.id = .dest_map
  deserdoors game + ".dox", gam.map.door(), gam.map.id
  catx(0) = gam.map.door(.dest).x * 20
  caty(0) = (gam.map.door(.dest).y - 1) * 20
  fadeout 0, 0, 0
  queue_fade_in 1
  prepare_map
  gam.random_battle_countdown = range(100, 60)
 END WITH
END SUB


'==========================================================================================
'                                        Textboxes
'==========================================================================================


FUNCTION immediate_showtextbox() as bool
 RETURN xreadbit(gen(), 18, genBits2)
END FUNCTION

'Load a textbox and process conditionals that happen immediately, including
'the "instead" conditionals to pick a different box.
SUB loadsay (byval box_id as integer)
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

END SUB

SUB advance_text_box ()
 IF txt.box.backdrop > 0 THEN
  '--backdrop needs resetting
  gen(genTextboxBackdrop) = 0
 END IF
 '---IF MADE A CHOICE---
 IF txt.box.choice_enabled THEN
  MenuSound gen(genAcceptSFX)
  settag txt.box.choice_tag(txt.choicestate.pt)
 END IF
 '---RESET MUSIC----
 IF txt.box.restore_music THEN
  IF gmap(1) > 0 THEN
   wrappedsong gmap(1) - 1
  ELSEIF gmap(1) = 0 THEN
   stopsong
  ELSE
   IF txt.remember_music > -1 THEN
    wrappedsong txt.remember_music
   ELSE
    stopsong
   END IF
  END IF
 END IF
 '---STOP SOUND EFFECT----
 IF txt.box.sound_effect > 0 AND txt.box.stop_sound_after THEN
  stopsfx txt.box.sound_effect - 1
 END IF
 '---GAIN/LOSE CASH-----
 IF istag(txt.box.money_tag, 0) THEN
  gold = gold + txt.box.money
  IF gold > 2000000000 THEN gold = 2000000000
  IF gold < 0 THEN gold = 0
 END IF
 '---SPAWN BATTLE--------
 IF istag(txt.box.battle_tag, 0) THEN
  fatal = NO
  gam.wonbattle = battle(txt.box.battle)
  prepare_map YES
  gam.random_battle_countdown = range(100, 60)
  queue_fade_in 1, YES
 END IF
 '---GAIN/LOSE ITEM--------
 IF istag(txt.box.item_tag, 0) THEN
  IF txt.box.item > 0 THEN getitem txt.box.item - 1
  IF txt.box.item < 0 THEN delitem (-txt.box.item) - 1
 END IF
 '---SHOP/INN/SAVE/ETC------------
 IF istag(txt.box.shop_tag, 0) THEN
  IF txt.box.shop > 0 THEN
   shop txt.box.shop - 1
  ELSEIF txt.box.shop < 0 THEN
   '--Preserve background for display beneath the top-level shop menu
   DIM holdscreen as integer = duplicatepage(vpage)
   IF useinn(-txt.box.shop, holdscreen) THEN
    innRestore
    fadeout 0, 0, 80
    queue_fade_in 1, YES
   END IF
   freepage holdscreen
  ELSEIF txt.box.shop = 0 THEN
   innRestore
  END IF
 END IF
 '---ADD/REMOVE/SWAP/LOCK HERO-----------------
 IF istag(txt.box.hero_tag, 0) THEN add_rem_swap_lock_hero txt.box
 '---FORCE DOOR------
 IF istag(txt.box.door_tag, 0) THEN
  usedoor txt.box.door
 END IF
 '---JUMP TO NEXT TEXT BOX--------
 IF istag(txt.box.after_tag, 0) THEN
  IF txt.box.after < 0 THEN
   trigger_script -txt.box.after, YES, "textbox", "box " & txt.id, scrqBackcompat()
  ELSE
   loadsay txt.box.after
   EXIT SUB
  END IF
 END IF
 '---DONE EVALUATING CONDITIONALS--------
 'Lots of things in this sub directly or indirectly affects tags. Many of the functions
 'called make sure the proper effects occur themselves, but we do it all again for simplicity
 evalitemtags
 evalherotags
 tag_updates
 IF txt.sayer >= 0 AND txt.old_dir <> -1 THEN
  IF npc(txt.sayer).id > 0 THEN
   IF npcs(npc(txt.sayer).id - 1).facetype = 1 THEN  '"Face Player"
    npc(txt.sayer).dir = txt.old_dir
   END IF
  END IF
 END IF
 IF txt.box.backdrop > 0 THEN
  gen(genTextboxBackdrop) = 0
 END IF
 txt.showing = NO
 txt.fully_shown = NO
 txt.sayer = -1
 txt.id = -1
 IF txt.sl THEN DeleteSlice @(txt.sl)
 ClearTextBox txt.box
 setkeys
 flusharray carray(), 7, 0
END SUB

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
  IF heroat > -1 THEN gam.hero(heroat).locked = NO
 END IF '---end if > 0
 '---LOCK HERO---
 IF box.hero_lock < 0 THEN
  DIM heroat as integer = findhero(-box.hero_lock, 0, 40, 1)
  IF heroat > -1 THEN gam.hero(heroat).locked = YES
 END IF '---end if > 0

 '--indirect effects
 party_change_updates
END SUB

SUB init_text_box_slices(txt as TextBoxState)
 IF txt.sl THEN
  '--free any already-loaded textbox
  DeleteSlice @(txt.sl)
 END IF

 'The textbox root slice is parent to the box and choicebox
 txt.sl = NewSliceOfType(slContainer, SliceTable.TextBox, SL_TEXTBOX_ROOT)
 WITH *txt.sl
  'Should not be set to fill, as scripts may expect to be able to move it around.
  'Set the width and height according to SliceTable.TextBox's size and padding.
  .Fill = YES
  .Parent->ChildRefresh(.Parent, txt.sl)
  .Fill = NO
 END WITH

 '--Create a new slice for the text box
 DIM text_box as Slice Ptr

 '--set up box style
 text_box = NewSliceOfType(slRectangle, txt.sl, SL_TEXTBOX_BOX)
 IF txt.box.no_box THEN
  'Invisible box (for the benefit of scripts)
  ChangeRectangleSlice text_box, , , , -2, transHollow
 ELSE
  ChangeRectangleSlice text_box, txt.box.boxstyle, , , , iif(txt.box.opaque, transOpaque, transFuzzy)
 END IF

 '--position and size the text box
 WITH *text_box
  .X = 0
  .Y = 4 + txt.box.vertical_offset * 4
  .Width = 312
  .Height = get_text_box_height(txt.box)
  .PaddingLeft = 4
  .PaddingRight = 4
  .PaddingTop = 3
  .PaddingBottom = 3
  'Horizontal centering
  .AlignHoriz = 1
  .AnchorHoriz = 1
  .AnchorVert = 0
  .AlignVert = 0
 END WITH

 '--Set up the actual text
 DIM col as integer
 col = uilook(uiText)
 IF txt.box.textcolor > 0 THEN col = txt.box.textcolor

 DIM s as string = ""
 FOR i as integer = 0 TO 7
  s &= txt.box.text(i) & CHR(10)
 NEXT i

 DIM text_sl as Slice Ptr
 text_sl = NewSliceOfType(slText, text_box, SL_TEXTBOX_TEXT)
 text_sl->Fill = YES
 ChangeTextSlice text_sl, s, col, YES, NO

 '--start the displayed lines as all hidden. They will be revealed in drawsay
 DIM dat as TextSliceData Ptr
 dat = text_sl->SliceData
 IF dat THEN
  dat->line_limit = -1
 END IF

 '--figure out which portrait to load
 'NOTE: Compare this to customsubs.rbas:load_text_box_portrait()
 'If you update this code, you might need to update that too.
 DIM img_id as integer = -1
 DIM pal_id as integer = -1
 DIM hero_slot as integer = -1
 SELECT CASE txt.box.portrait_type
  CASE 1' Fixed ID number
   img_id = txt.box.portrait_id
   pal_id = txt.box.portrait_pal
  CASE 2' Hero by caterpillar
   hero_slot = rank_to_party_slot(txt.box.portrait_id)
  CASE 3' Hero by party slot
   hero_slot = txt.box.portrait_id
  CASE 4' Hero by ID
   'If the hero is in the party, use their current state.
   'if there are multiple copies, use the first.
   hero_slot = findhero(txt.box.portrait_id + 1, 0, 40, 1)
   IF hero_slot = -1 THEN
    'The hero is not in the party right now, use their default
    DIM her as HeroDef
    loadherodata her, txt.box.portrait_id
    img_id = her.portrait
    pal_id = her.portrait_pal
   END IF
 END SELECT
 IF hero_slot >= 0 ANDALSO hero_slot <= UBOUND(gam.hero) THEN
  IF gam.hero(hero_slot).id >= 0 THEN
   img_id = gam.hero(hero_slot).portrait_pic
   pal_id = gam.hero(hero_slot).portrait_pal
  END IF
 END IF

 IF img_id >= 0 THEN
  '--First set up the box that holds the portrait
  DIM img_box as Slice Ptr
  img_box = NewSliceOfType(slRectangle, text_box, SL_TEXTBOX_PORTRAIT_BOX)
  IF txt.box.portrait_box THEN
   ChangeRectangleSlice img_box, txt.box.boxstyle, , , , transFuzzy
  ELSE
   'Invisible box
   ChangeRectangleSlice img_box, , , , -2, transHollow
  END IF
  img_box->Width = 50
  img_box->Height = 50
  img_box->X = txt.box.portrait_pos.x - 4
  img_box->Y = txt.box.portrait_pos.y - 3
  '--Then load the portrait
  DIM img_sl as Slice Ptr
  img_sl = NewSliceOfType(slSprite, img_box, SL_TEXTBOX_PORTRAIT)
  ChangeSpriteSlice img_sl, 8, img_id, pal_id
 END IF

 '--set up the choice-box (if any)
 IF txt.box.choice_enabled THEN
  'tempy = 100 + (txt.box.vertical_offset * 4) - (txt.box.shrink * 4)
  'IF tempy > 160 THEN tempy = 20
  'centerbox 160, tempy + 12, 10 + large(LEN(txt.box.choice(0)) * 8, LEN(txt.box.choice(1)) * 8), 24, txt.box.boxstyle + 1, dpage
  DIM choice_box as Slice Ptr
  choice_box = NewSliceOfType(slRectangle, txt.sl, SL_TEXTBOX_CHOICE_BOX)
  WITH *choice_box
   '--center the box
   .AnchorHoriz = 1
   .AlignHoriz = 1
   .AnchorVert = 0
   .AlignVert = 0
   '--set box size
   .Width = 10 + large(LEN(txt.box.choice(0)) * 8, LEN(txt.box.choice(1)) * 8)
   .Height = 24
   '--FIXME: This hackyness just reproduces the old method of positioning the choicebox.
   '--FIXME: eventually the game author should have control over this.
   .Y = text_box->Y + text_box->Height + 12
   IF .Y > txt.sl->Height - (.Height + 4) THEN .Y = 32
  END WITH
  ChangeRectangleSlice choice_box, txt.box.boxstyle
  REDIM choice_sl(1) as Slice Ptr
  FOR i as integer = 0 TO 1
   choice_sl(i) = NewSliceOfType(slText, choice_box)
   ChangeTextSlice choice_sl(i), txt.box.choice(i), uilook(uiMenuItem), YES
   WITH *(choice_sl(i))
    .AnchorHoriz = 1
    .AlignHoriz = 1
    .Y = 2 + i * 10
   END WITH
  NEXT i
  choice_sl(0)->Lookup = SL_TEXTBOX_CHOICE0
  choice_sl(1)->Lookup = SL_TEXTBOX_CHOICE1
 END IF

END SUB

'This is used for resetting game state. But only a few of the txt members
'actually need to be cleaned up; most aren't used when no box is up
SUB cleanup_text_box ()
 ClearTextBox txt.box
 WITH txt
  .id = -1
  .showing = NO
  .fully_shown = NO
  .choicestate.pt = 0
  .remember_music = NO
  .show_lines = 0
  .sayer = -1
  .old_dir = 0
 END WITH
 IF txt.sl THEN DeleteSlice @(txt.sl)
END SUB


'==========================================================================================
'                                        Map slices
'==========================================================================================


SUB recreate_map_slices()
 'this destroys and re-creates the map slices. it should only happen when
 'moving from one map to another, but not when a battle ends. (same as when
 'the map autorun script is triggered)

 'First free all NPC slices because we need to make sure the npc(i).sl's
 'don't point to deleted memory, though they would all be deleted anyway,
 'but not soon enough. (and we must do this unconditionally, even if
 'the preference for recreating map slices is turned OFF)
 FOR i as integer = 0 TO UBOUND(npc)
  DeleteSlice @npc(i).sl
 NEXT i

 IF readbit(gen(), genBits2, 11) <> 0 THEN
  '"Recreate map slices when changing maps" = ON

  'Orphan the hero slices to prevent them from being destroyed when we
  'destroy the map layers
  orphan_hero_slices

  'Free the map slices
  FOR i as integer = 0 TO UBOUND(SliceTable.MapLayer)
   DeleteSlice @SliceTable.MapLayer(i)
  NEXT i
  DeleteSlice @SliceTable.ObsoleteOverhead
  DeleteSlice @SliceTable.HeroLayer
  DeleteSlice @SliceTable.NPCLayer
  DeleteSlice @SliceTable.Walkabout

  'Anything else attached to the map
  DeleteSliceChildren SliceTable.MapRoot

  'And then create new ones
  SetupMapSlices UBOUND(maptiles)

  'Reparent the hero slices to the new map
  reparent_hero_slices

  refresh_map_slice_tilesets
  visnpc
 END IF
 refresh_map_slice
END SUB

SUB refresh_map_slice()
 'This updates the size, tilesets, sort order, and visibility of the map slices

 'debug "refresh_map_slice() there are " & UBOUND(maptiles) + 1 & " map layers on map " & gam.map.id

 '--Store info about the map in the map slices
 WITH *(SliceTable.MapRoot)
  .Width = mapsizetiles.x * 20
  .Height = mapsizetiles.y * 20
 END WITH
 FOR i as integer = 0 TO UBOUND(SliceTable.MapLayer)
  IF SliceTable.MapLayer(i) THEN
   SliceTable.MapLayer(i)->Width = mapsizetiles.x * 20
   SliceTable.MapLayer(i)->Height = mapsizetiles.y * 20
  END IF
 NEXT
 SliceTable.ObsoleteOverhead->Width = mapsizetiles.x * 20
 SliceTable.ObsoleteOverhead->Height = mapsizetiles.y * 20

 FOR i as integer = 0 TO UBOUND(maptiles)
  '--reset each layer (the tileset ptr is set in refresh_map_slice_tilesets
  IF SliceTable.MapLayer(i) = 0 THEN
   debug "NULL SliceTable.MapLayer(" & i & ") when reseting tilesets in refresh_map_slice()"
  ELSE
   ChangeMapSlice SliceTable.MapLayer(i), @maptiles(i), @pass
   SliceTable.MapLayer(i)->Visible = IIF(i = 0, YES, readbit(gmap(), 19, i - 1))
  END IF
 NEXT i
 FOR i as integer = UBOUND(maptiles) + 1 TO UBOUND(SliceTable.MapLayer)
  '--if slices exist for the unused layers that this map doesn't have,
  '--we should make them display no tiles
  IF Slicetable.MapLayer(i) <> 0 THEN
   ChangeMapSlice SliceTable.MapLayer(i), NULL, NULL
   SliceTable.MapLayer(i)->Visible = NO
  END IF
 NEXT i
 ChangeMapSlice SliceTable.ObsoleteOverhead, @maptiles(0), @pass

 '--now fix up the order of the slices
 DIM num_layers_under_walkabouts as integer
 '--It's possible for gmap(31) to be larger than the number of map layers
 num_layers_under_walkabouts = bound(gmap(31), 1, UBOUND(maptiles) + 1)
 FOR i as integer = 0 TO UBOUND(maptiles)
  IF SliceTable.Maplayer(i) = 0 THEN
   debug "Null map layer " & i & " when sorting in refresh_map_slice"
  ELSE
   SliceTable.MapLayer(i)->Sorter = IIF(i < num_layers_under_walkabouts, i, i + 1)
  END IF
 NEXT
 FOR i as integer = UBOUND(maptiles) + 1 TO UBOUND(SliceTable.MapLayer)
  'Slices for layers that do not exist on the current map...
  IF SliceTable.MapLayer(i) <> 0 THEN
   '...should be sorted too, if they exist.
   SliceTable.MapLayer(i)->Sorter = i
  END IF
 NEXT i

 SliceTable.Walkabout->Sorter = num_layers_under_walkabouts
 SliceTable.ObsoleteOverhead->Sorter = UBOUND(maptiles) + 2

 CustomSortChildSlices SliceTable.MapRoot, YES
 refresh_walkabout_layer_sort()
END SUB

SUB refresh_map_slice_tilesets()
 FOR i as integer = 0 TO maplayerMax
  '--reset map layer tileset ptrs
  IF SliceTable.MapLayer(i) <> 0 THEN
   ChangeMapSliceTileset SliceTable.MapLayer(i), tilesets(i)
  END IF
 NEXT i
 ChangeMapSliceTileset SliceTable.ObsoleteOverhead, tilesets(0)
END SUB


'==========================================================================================


'--Look in front of the leader for an activatable NPC.
'--WARNING: has side-effects: assumes result is passed to usenpc
FUNCTION find_useable_npc() as integer
 DIM ux as integer = catx(0)
 DIM uy as integer = caty(0)
 wrapaheadxy ux, uy, catd(0), 20, 20

 FOR j as integer = 0 TO 299
  WITH npc(j)
   IF .id > 0 AND (j <> vstate.npc OR vstate.active = NO) THEN
    '--Step-on NPCs cannot be used
    IF npcs(.id - 1).activation = 2 THEN CONTINUE FOR
    IF .suspend_use THEN CONTINUE FOR
    DIM nx as integer = .x
    DIM ny as integer = .y
    IF (nx = ux AND ny = uy) THEN 'not moving NPCs
     RETURN j
    ELSEIF nx MOD 20 <> 0 XOR ny mod 20 <> 0 THEN 'they're moving (i.e. misaligned)
     '--first check the tile the NPC is stepping into
     nx -= .xgo
     ny -= .ygo
     cropposition nx, ny, 20
     '--uncommenting the line below provides a helpful rectangle that shows the activation tile of an NPC
     'rectangle nx - mapx, ny - mapy, 20,20, 1, vpage : setvispage vpage
     IF (nx = ux AND ny = uy) THEN 'check for activation
      RETURN j
     END IF
     '--also check the tile the NPC is leaving
     nx = nx + SGN(.xgo) * 20
     ny = ny + SGN(.ygo) * 20
     '--uncommenting the line below provides a helpful rectangle that shows the activation tile of an NPC
     'rectangle nx - mapx, ny - mapy, 20,20, 4, vpage : setvispage vpage
     IF (nx = ux AND ny = uy) THEN 'check for activation
      '--if activating an NPC that has just walked past us, cause it to back up
      .xgo = SGN(.xgo * -1) * (20 - ABS(.xgo))
      .ygo = SGN(.ygo * -1) * (20 - ABS(.ygo))
      RETURN j
     END IF
    END IF
   END IF
  END WITH
 NEXT
 RETURN -1
END FUNCTION

'Activate npc(npcnum)
SUB usenpc(byval cause as integer, byval npcnum as integer)
 'cause = 0: normal use key
 'cause = 1: touch and step-on
 'cause = 2: scripted
 IF npcnum < 0 THEN EXIT SUB
 IF npc(npcnum).suspend_use ANDALSO cause <> 2 THEN EXIT SUB
 DIM id as integer = npc(npcnum).id - 1

 '---Item from NPC---
 DIM getit as integer = npcs(id).item
 IF getit THEN
  getitem getit - 1
  evalitemtags
  'Delay tag_updates
 END IF
 '---DIRECTION CHANGING-----------------------
 txt.old_dir = -1
 IF cause <> 2 AND npcs(id).facetype <> 2 THEN  'not "Do not face player"
  txt.old_dir = npc(npcnum).dir
  npc(npcnum).dir = catd(0)
  npc(npcnum).dir = loopvar(npc(npcnum).dir, 0, 3, 2)
 END IF
 IF npcs(id).usetag > 0 THEN
  '--One-time-use tag
  settag onetime(), npcs(id).usetag, YES
  'Delay tag_updates
 END IF
 IF npcs(id).script > 0 THEN
  '--summon a script directly from an NPC
  trigger_script npcs(id).script, YES, "NPC", "NPC ID " & id & " at " & npc(npcnum).x & "," & npc(npcnum).y, scrqBackcompat()
  trigger_script_arg 0, npcs(id).scriptarg, "arg"
  trigger_script_arg 1, (npcnum + 1) * -1, "npcref"
 END IF
 DIM vehuse as integer = npcs(id).vehicle
 IF vehuse THEN '---activate a vehicle---
  reset_vehicle vstate
  vstate.id = vehuse - 1
  LoadVehicle game & ".veh", vstate.dat, vstate.id
  '--check mounting permissions first
  IF vehpass(vstate.dat.mount_from, readblock(pass, catx(0) \ 20, caty(0) \ 20), -1) THEN
   vstate.active = YES
   vstate.npc = npcnum
   vstate.old_speed = herow(0).speed
   herow(0).speed = 10
   vstate.mounting = YES '--trigger mounting sequence
   settag vstate.dat.riding_tag, YES
   create_walkabout_shadow npc(vstate.npc).sl
  END IF
 END IF
 IF npcs(id).textbox > 0 THEN
  txt.sayer = npcnum
  loadsay npcs(id).textbox
  'NOTE: don't force NPC tag visibility to be updated after a text box
  '  is displayed because that could cause premature NPC disappearance,
  '  and because tag_updates will always be called when the box advances
  tag_updates NO
 ELSE
  'Several different ways to modify tags in this sub
  tag_updates
 END IF
END SUB

FUNCTION want_to_check_for_walls(byval who as integer) as bool
 'Check hero is at beginning of a movement to a new tile (aligned in at least one direction)...
 IF movdivis(herow(who).xgo) = NO AND movdivis(herow(who).ygo) = NO THEN RETURN NO
 '...and certain conditions aren't met
 IF gam.walk_through_walls = YES THEN RETURN NO
 IF vstate.dat.pass_walls = YES THEN RETURN NO
 IF vstate.active THEN
  DIM thisherotilex as integer = catx(who * 5) \ 20
  DIM thisherotiley as integer = caty(who * 5) \ 20
  IF vehpass(vstate.dat.override_walls, readblock(pass, thisherotilex, thisherotiley), 0) <> 0 THEN RETURN NO
 END IF
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

FUNCTION first_free_slot_in_party() as integer
 DIM slot as integer = -1
 IF free_slots_in_party() > 0 THEN
  slot = first_free_slot_in_active_party()
  IF slot = -1 THEN
   slot = first_free_slot_in_reserve_party()
  END IF
 END IF
 RETURN slot
END FUNCTION

FUNCTION first_free_slot_in_active_party() as integer
 '--returns the first free slot, or -1 if all slots are full
 FOR i as integer = 0 TO 3
  IF gam.hero(i).id = -1 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION first_free_slot_in_reserve_party() as integer
 '--returns the first free slot, or -1 if all slots are full
 IF free_slots_in_party() > 0 THEN
  FOR i as integer = 4 TO 40
   IF gam.hero(i).id = -1 THEN RETURN i
  NEXT i
 END IF
 RETURN -1
END FUNCTION

FUNCTION free_slots_in_party() as integer
 '--Returns the number of free slots in the active+reserve party
 'Note that there can only be 38 heroes total even though there are 41
 'hero slots. This is because 3 reserve slots have to be saved to
 'allow active party members to be swapped out.
 'FIXME: the above would be true except that it has been broken so
 'very long that games could already exist that rely on having 41 heroes

 '--This is the "correct" intended limit that has never been enforced right.
 'RETURN 38 - herocount(40)

 RETURN 41 - herocount(40)

END FUNCTION

SUB cleanup_game_slices ()
 FOR i as integer = 0 TO UBOUND(herow)
  DeleteSlice @herow(i).sl
 NEXT i
 FOR i as integer = 0 TO UBOUND(npc)
  DeleteSlice @npc(i).sl
 NEXT i
 DestroyGameSlices
END SUB

SUB queue_music_change (byval song as integer)
 'Delay map ambient music to give scripts a chance to override it.
 'A delay of two is actually a single tick delay, because it will be decremented
 'the same tick that this is called, at the bottom of the main loop.
 gam.music_change_delay = 2
 gam.delayed_music = song
END SUB

SUB check_for_queued_music_change ()
 IF gam.music_change_delay = 1 THEN
  IF gam.delayed_music >= 0 THEN
   wrappedsong gam.delayed_music
  ELSE
   stopsong
  END IF
 END IF
 gam.music_change_delay = large(0, gam.music_change_delay - 1)
END SUB

'Cause a screen fade in some number of ticks from now.
'script_overridable allows the fade in to be cancelled by a fadescreenout command,
'and is for backcompatibility. See fadescreenout. If you need to increase
'any fade in delays, normally you should set script_overridable = YES
SUB queue_fade_in (delay as integer = 0, script_overridable as bool = NO)
 gam.need_fade_in = YES
 gam.fade_in_delay = delay
 gam.fade_in_script_overridable = script_overridable
END SUB

SUB check_for_queued_fade_in ()
 IF gam.need_fade_in THEN
  IF gam.fade_in_delay <= 0 THEN
   gam.need_fade_in = 0
   fadein
   setkeys
  ELSE
   gam.fade_in_delay -= 1
  END IF
 END IF
END SUB

FUNCTION seek_rpg_or_rpgdir_and_play_it(where as string, gamename as string) as integer
 '--Search to see if a rpg file or an rpgdir of a given name exists
 ' and if so, select it for playing (the browse screen will not appear)
 'Returns YES if found, NO if not found
 IF isfile(where & SLASH & gamename & ".rpg") THEN
  sourcerpg = where & SLASH + gamename & ".rpg"
  gam.autorungame = YES
  RETURN YES
 ELSE
  DIM rpgd as string = where & SLASH & gamename & ".rpgdir"
  IF isdir(rpgd) THEN
   IF isfile(rpgd & SLASH & "archinym.lmp") THEN
    sourcerpg = rpgd
    workingdir = rpgd
    gam.autorungame = YES
    usepreunlump = YES
    RETURN YES
   END IF
  END IF
 END IF
 RETURN NO
END FUNCTION

SUB misc_debug_menu()
 STATIC default as integer = 0
 DIM menu(4) as string
 menu(0) = "Test Battles"
 menu(1) = "View/Edit Slice Tree"
 menu(2) = "Manipulate gen() array"
 menu(3) = "Manipulate gmap() array"
 menu(4) = "Test Slicified Spell Screen"
 DIM result as integer
 result = multichoice("Misc. Debug", menu(), default, , "game_misc_debug")
 IF result = -1 THEN EXIT SUB
 default = result
 SELECT CASE result
  CASE 0: battle_formation_testing_menu
  CASE 1: slice_editor SliceTable.Root
  CASE 2: patcharray gen(), "gen"
  CASE 3: patcharray gmap(), "gmap"
  CASE 4: spell_screen onwho(readglobalstring(106, "Whose Spells?", 20), 0)
 END SELECT
END SUB

SUB battle_formation_testing_menu()

 STATIC defaultval as integer = 0
 DIM form_num as integer
 DIM state as MenuState
 DIM menu as MenuDef
 ClearMenuData menu

 DIM battle_formation_set as integer
 battle_formation_set = readblock(foemap, catx(0) \ 20, caty(0) \ 20)

 IF battle_formation_set = 0 THEN
  append_menu_item(menu, "Formation set: None", 0, 1)
  menu.last->disabled = YES
  menu.last->extra(0) = -1
 ELSE
  DIM formset as FormationSet
  LoadFormationSet formset, battle_formation_set
  append_menu_item(menu, "Formation set: " & battle_formation_set & " freq=" & formset.frequency)
  menu.last->disabled = YES
  menu.last->extra(0) = -1
  FOR i as integer = 0 TO UBOUND(formset.formations)
   form_num = formset.formations(i)
   IF form_num >= 0 THEN
    DIM formdata as Formation
    LoadFormation formdata, form_num
    DIM desc as string = describe_formation(formdata)
    append_menu_item(menu, form_num & ": " & LEFT(desc, 35))
    menu.last->extra(0) = form_num
    IF defaultval = 0 THEN defaultval = 1
   END IF
  NEXT i
 END IF
 
 state.active = YES
 menu.align = -1
 menu.maxrows = 16
 init_menu_state state, menu
 state.pt = defaultval
 menu.anchor.Y = -1
 menu.offset.Y = -90

 'Keep whatever was on the screen already as a background (NOTE: this doesn't always work (not necessarily vpage))
 DIM holdscreen as integer
 holdscreen = allocatepage
 copypage vpage, holdscreen

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "game_formation_testing"

  IF enter_space_click(state) THEN
   form_num = menu.items[state.pt]->extra(0)
   IF form_num >= 0 THEN
    defaultval = state.pt
    fatal = NO
    gam.wonbattle = battle(form_num)
    prepare_map YES
    queue_fade_in 1
   END IF 
   EXIT DO
  END IF
  
  usemenu state

  copypage holdscreen, vpage
  draw_menu menu, state, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 freepage holdscreen
 ClearMenuData menu

END SUB

SUB refresh_keepalive_file ()
 DIM timestamp as string
 'build a timestamp string in the format YYYY-MM-DD hh:mm:ss
 timestamp = MID(DATE, 7, 4) & "-" & MID(DATE, 1, 2) & "-" & MID(DATE, 4, 2) & " " & TIME
 DIM filename as string
 filename = tmpdir & "keepalive.tmp"
 DIM fh as integer = FREEFILE
 OPEN filename FOR BINARY ACCESS WRITE as #fh
 PUT #fh, 1, timestamp
 CLOSE #fh
END SUB

FUNCTION read_keepalive_as_days (keepalive_file as string) as integer
 DIM fh as integer = FREEFILE
 OPEN keepalive_file FOR BINARY ACCESS READ as #fh
 DIM datestr as string = "YYYY-MM-DD"
 GET #fh, 1, datestr
 CLOSE #fh
 RETURN days_since_datestr(datestr)
END FUNCTION

FUNCTION guess_age_by_tmpdir_name(dirname as string) as integer
 'The dirname argument is just the final component of the dirname, not the full path.
 'It will be in one of the following two formats:
 'Old: YYYYMMDDhhmmss.RANDOM.tmp
 'New: ohrrpgceYYYYMMDDhhmmss.RANDOM.tmp
 DIM datestr as string
 IF LEFT(dirname, 8) = "ohrrpgce" THEN
  'New format
  datestr = MID(dirname, 9, 4) & "-" & MID(dirname, 13, 2) & "-" & MID(dirname, 15, 2)
 ELSE
  'Old format
  datestr = MID(dirname, 1, 4) & "-" & MID(dirname, 5, 2) & "-" & MID(dirname, 7, 2)
 END IF
 RETURN days_since_datestr(datestr)
END FUNCTION

SUB cleanup_other_temp_files ()

 DIM tmp_parent as string = trimfilename(tmpdir)
 DIM tmp_cur as string = trimpath(tmpdir)
 
 REDIM filelist() as string
 'Modern tmp dirs would match the pattern "ohrrpgce*.tmp" but this would miss old tmp dirs.
 'The pattern "*.tmp" is too broad because it could match a large number of non-ohrrpgce
 'tmp files on windows (even "*.*.tmp" is more broad than I would like for it to be)
 findfiles tmp_parent, "*.*.tmp", fileTypeDirectory, NO, filelist()

 DIM dirname as string
 DIM dirname_full as string
 DIM keepalive_file as string
 DIM age as integer
 DIM threshhold as integer
 DIM cap as string

 FOR i as integer = 0 TO UBOUND(filelist)
  
  dirname = filelist(i)
  dirname_full = tmp_parent & SLASH & dirname
  keepalive_file = dirname_full & SLASH & "keepalive.tmp"
  IF dirname = tmp_cur THEN
   debuginfo "Ignore " & dirname & " because we are using it"
   CONTINUE FOR
  ELSEIF NOT isdir(dirname_full & SLASH & "playing.tmp") ANDALSO LEFT(dirname, 8) <> "ohrrpgce" THEN
   debuginfo "Ignore " & dirname & " because it does not have playing.tmp and the name does not start with ""ohrrpgce"""
  ELSE
   IF NOT isfile(keepalive_file) THEN
    'Yon tmpdir is olde beyond reckoning
    age = guess_age_by_tmpdir_name(dirname)
    threshhold = 14
   ELSE
    'This is a modern tmpdir with a valid keepalive file
    age = read_keepalive_as_days(keepalive_file)
    threshhold = 3
   END IF
#IFDEF __FB_ANDROID__
   '--Android only permits one running copy of a process, so it is always safe to clean up all tmpdirs
   threshhold = -1
#ENDIF
   IF age > threshhold THEN
    center_edgeboxstyle 160, 65, 25 * 8, 16, 0, vpage, NO, YES
    cap = "Cleaning up files: " & INT(100 / large(UBOUND(filelist), 1) * i) & "%"
    edgeprint cap, xstring(cap, 160), 60, uilook(uiText), vpage
    setvispage vpage
    debuginfo "CLEAN " & dirname_full & " because it has been dead for about " & age & " days"
    killdir dirname_full, YES
   ELSE
    debuginfo "Ignore " & dirname & " because it has only been dead " & age & " days"
   END IF
  END IF
 NEXT i
END SUB

SUB a_script_wants_keys()
 'After running a command that checks for keys, keep the virtual gamepad visible for about half a second
 gam.pad.script_wants_keys = ideal_ticks_per_second() / 2
END SUB

SUB update_virtual_gamepad_display()
 'Based on global state, of the current game, decide whether or not the virual gamepad should be displaying
 IF calc_virtual_gamepad_state() THEN
  IF NOT gam.pad.being_shown THEN
   show_virtual_gamepad()
   gam.pad.being_shown = YES
  END IF
 ELSE
  IF gam.pad.being_shown THEN
   hide_virtual_gamepad()
   gam.pad.being_shown = NO
  END IF
 END IF
END SUB

FUNCTION calc_virtual_gamepad_state() as bool
 'None of this matters unless we are running on a platform that actually uses a virtual gamepad
 IF NOT running_on_mobile() THEN RETURN NO

 IF gam.pad.script_hide_virtual_gamepad THEN RETURN NO
 IF gam.pad.script_show_virtual_gamepad THEN RETURN YES

 'The gamepad might be disabled for this game
 IF should_disable_virtual_gamepad() THEN RETURN NO
 
 'A script command has run recently that is checking for key input
 IF gam.pad.script_wants_keys > 0 THEN
  gam.pad.script_wants_keys -= 1
  RETURN YES
 END IF
 
 'Inside battle mode, force the gamepad visible
 IF gam.pad.in_battle THEN RETURN YES

 'Now check and see if the virtual gamepad should be disabled because of textboxes
 IF use_touch_textboxes() THEN
  IF txt.showing THEN
   'Make an exception when the current textbox has a choicebox
   IF txt.box.choice_enabled THEN RETURN YES
   IF top_menu_allows_controls() THEN RETURN YES
   'No exceptions were found, proceed to hide the virtual gamepad for this textbox
   RETURN NO
  END IF
 END IF
 
 IF readbit(gen(), genSuspendBits, suspendplayer) ANDALSO should_hide_virtual_gamepad_when_suspendplayer() THEN
  'Suspendplayer is active, and this game has the "Hide virtual gamepad when suspendplayer" bitset
  IF top_menu_allows_controls() THEN RETURN YES 'Menus still need the gamepad
  IF txt.showing THEN RETURN YES 'Non-touch textboxes still need the gamepad
  RETURN NO
 END IF
 
 'If no other conditions are met, enabled the virtual gamepad
 RETURN YES
END FUNCTION

FUNCTION top_menu_allows_controls() as bool
 IF topmenu >= 0 THEN
  'If any menus are open, we need to check the top one
  IF menus(topmenu).no_controls = NO THEN
   'The top menu menu allows controls
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION last_active_party_slot() as integer
 RETURN 3
END FUNCTION

FUNCTION is_active_party_slot(byval slot as integer) as integer
 RETURN slot >=0 ANDALSO slot <= last_active_party_slot()
END FUNCTION

FUNCTION active_party_size() as integer
 RETURN last_active_party_slot() + 1
END FUNCTION

FUNCTION loop_active_party_slot(byval slot as integer, byval direction as integer=1) as integer
 'Given a slot number in the active party, return the next empty slot
 IF direction <> 1 ANDALSO direction <> -1 THEN
  RETURN slot
 END IF
 IF herocount() = 0 THEN
  'If the party is somehow empty, return the original slot
  RETURN slot
 END IF
 DO
  slot = loopvar(slot, 0, last_active_party_slot(), direction)
  IF gam.hero(slot).id >= 0 THEN RETURN slot
 LOOP
END FUNCTION
