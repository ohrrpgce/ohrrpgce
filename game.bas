'OHRRPGCE GAME - Main module
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
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmodsubs.bi"
#include "bmod.bi"
#include "hsinterpreter.bi"
#include "sliceedit.bi"
#include "game.bi"


'local subs and functions
DECLARE SUB reset_map_state (map as MapModeState)
DECLARE SUB checkdoors ()
DECLARE SUB usedoor (byval door_id as integer)
DECLARE SUB advance_text_box ()
DECLARE FUNCTION want_to_check_for_walls(byval who as integer) as integer
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
DECLARE SUB reparent_hero_slices()
DECLARE SUB orphan_hero_slices()
DECLARE SUB reparent_npc_slices()
DECLARE SUB orphan_npc_slices()
DECLARE FUNCTION seek_rpg_or_rpgdir_and_play_it(where as string, gamename as string) as integer
DECLARE SUB misc_debug_menu()
DECLARE SUB battle_formation_testing_menu()


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
DIM autotestmode as integer = NO
DIM speedcontrol as integer = 55
DIM autosnap as integer = 0
DIM running_as_slave as integer = NO
DIM custom_version as STRING  'when running as slave
DIM master_channel as IPCChannel = NULL_CHANNEL  'when running as slave
DIM modified_lumps as STRING VECTOR  'when running as slave
v_new modified_lumps
DIM force_prefsdir_save as integer = NO

orig_dir = CURDIR()
processcommandline

'---get temp dir---
set_homedir
tmpdir = acquiretempdir
IF NOT isdir(tmpdir) THEN makedir tmpdir

'DEBUG debug "set mode-X"
setmodex

'DEBUG debug "init sound"
setupmusic

'DEBUG debug "dim (almost) everything"

'shared module variables
DIM SHARED harmtileflash as integer = NO
DIM SHARED wantbox as integer
DIM SHARED wantdoor as integer
DIM SHARED wantbattle as integer
DIM SHARED wantteleport as integer
DIM SHARED wantusenpc as integer
DIM SHARED wantloadgame as integer
DIM SHARED scriptout as string

'global variables
DIM gam as GameState
DIM txt as TextBoxState
REDIM gen(360) as integer
REDIM tag(1000) as integer '16000 bitsets
REDIM onetime(1000) as integer '16000 bitsets

REDIM herotags(59) as HeroTagsCache
REDIM itemtags(maxMaxItems) as ItemTagsCache

'Party stuff
REDIM hero(40) as integer
REDIM spell(40, 3, 23) as integer
REDIM lmp(40, 7) as integer
REDIM names(40) as string
REDIM eqstuf(40, 4) as integer
REDIM hmask(2) as integer  '41 bits

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
REDIM uilook(uiColors) as integer
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
DIM tmpdir as string
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

'Menu Data
DIM menu_set as MenuSet
REDIM menus(0) as MenuDef 'This is an array because it holds a stack of heirarchial menus (resized as required)
REDIM mstates(0) as MenuState
DIM topmenu as integer = -1

DIM fatal as integer
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
DIM numloadedscr as integer
DIM totalscrmem as integer
DIM scrwatch as integer
DIM next_interpreter_check_time as double
DIM interruption_grace_period as integer
REDIM heap(2048) as integer
REDIM global(maxScriptGlobals) as integer
REDIM retvals(32) as integer
REDIM scrat(128) as ScriptInst
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
DIM scratp as ScriptInst ptr
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

'DEBUG debug "Thestart"
DO 'This is a big loop that encloses the entire program (more than it should). The loop is only reached when resetting the program

'----(Re)initialise graphics/window/IO options

dpage = 1: vpage = 0
presentsong = -1

gam.current_master_palette = -1
load_default_master_palette master()
DefaultUIColors uilook()
getdefaultfont current_font()
setfont current_font()

'-- Init joysticks
FOR i as integer = 0 TO 1
 gotj(i) = readjoy(joy(), i)
NEXT i

gen(genJoy) = 0  'joystick disabled by default
defaultc  'set up default controls

'Read joyset.ini
readjoysettings

setwindowtitle "O.H.R.RPG.C.E"
unhidemousecursor  'init mouse state

gam.autorungame = NO
usepreunlump = NO

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
 FOR i as integer = 1 TO UBOUND(cmdline_args)
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
    sourcerpg = arg
    workingdir = arg
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
 sourcerpg = browse(7, "", "*.rpg", tmpdir, 1, "game_browse_rpg")
 IF sourcerpg = "" THEN exitprogram NO
 IF isdir(sourcerpg) THEN
  usepreunlump = YES
  workingdir = sourcerpg
 END IF
END IF

'-- set up prefs dir
IF NOT isdir(settings_dir) THEN debug "Ooops! Why doesn't """ & settings_dir & """ exist yet?"
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
ELSEIF NOT running_as_slave THEN  'Won't upgrade if running as slave
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

REDIM gmap(dimbinsize(binMAP)) as integer 'this must be declared here, after the binsize file exists!

'--set game
game = workingdir + SLASH + archinym
game_unique_id = STR(randint(INT_MAX))
DIM wintitle as STRING = getdisplayname(trimpath(sourcerpg))
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

IF isfile(game + ".hsp") THEN unlump game + ".hsp", tmpdir

fadeout 0, 0, 0
queue_fade_in

setfont current_font()
loadglobalstrings
getstatnames statnames()

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

DO' This loop encloses the playable game for a specific RPG file

gam.current_master_palette = gen(genMasterPal)
loadpalette master(), gam.current_master_palette
LoadUIColors uilook(), gam.current_master_palette
init_default_text_colors

initgamedefaults
fatal = 0
abortg = 0
lastformation = -1
scrwatch = 0
menu_set.menufile = workingdir & SLASH & "menus.bin"
menu_set.itemfile = workingdir & SLASH & "menuitem.bin"
load_lookup1_bin lookup1_bin_cache()

makebackups 'make a few backup lumps

wantbox = 0
wantdoor = 0
wantbattle = 0
wantteleport = 0
wantusenpc = 0
wantloadgame = 0

txt.showing = NO
txt.fully_shown = NO
txt.show_lines = 0
txt.sayer = -1
txt.id = -1

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
stopsong
fadeout 0, 0, 0
IF load_slot = -2 THEN EXIT DO 'resetg
IF load_slot >= 0 THEN
 doloadgame load_slot
ELSE
 clearpage 0
 clearpage 1
 addhero 1, 0
 IF gen(genNewGameScript) > 0 THEN
  trigger_script gen(genNewGameScript), YES, "newgame", "", scrqBackcompat()
 END IF
 prepare_map
END IF

load_special_tag_caches
evalherotags
queue_fade_in
DIM tog as integer

'--Reset some stuff related to debug keys
gam.showtext_ticks = 0
gam.debug_showtags = NO
gam.debug_npc_info = NO
gam.walk_through_walls = NO

'DEBUG debug "pre-call update_heroes"
update_heroes(YES)
setkeys
DO
 'DEBUG debug "top of master loop"
 setwait speedcontrol
 IF running_as_slave THEN try_to_reload_files_onmap
 tog = tog XOR 1
 'DEBUG debug "increment play timers"
 playtimer

 'DEBUG debug "read controls"
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
 dotimer(0)

 'DEBUG debug "keyboard handling"
 IF carray(ccMenu) > 1 AND txt.showing = NO AND gam.need_fade_in = NO AND readbit(gen(), genSuspendBits, suspendplayer) = 0 AND vstate.active = NO AND herow(0).xgo = 0 AND herow(0).ygo = 0 THEN
  IF allowed_to_open_main_menu() THEN
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
 'debug "before advance_text_box:"
 IF carray(ccUse) > 1 AND txt.fully_shown = YES AND readbit(gen(), genSuspendBits, suspendboxadvance) = 0 THEN
  advance_text_box
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
 IF readbit(gen(), genBits, 8) = 0 THEN
  'DEBUG debug "evaluate debugging keys"
  IF keyval(scF2) > 1 AND txt.showing = NO THEN
   savegame 32
   gam.showtext = "Quick-saved. Press F3 to quick-load"
   gam.showtext_ticks = 20
  END IF
  IF keyval(scF3) > 1 AND txt.showing = NO THEN
   IF yesno("Load quick-saved game?") THEN wantloadgame = 33
  END IF
  IF keyval(scCtrl) > 0 AND keyval(scF7) > 1 THEN
   catx(0) = (catx(0) \ 20) * 20
   caty(0) = (caty(0) \ 20) * 20
   herow(0).xgo = 0
   herow(0).ygo = 0
  END IF
  IF keyval(scCtrl) > 0 THEN ' holding CTRL
   IF keyval(scF1) > 1 AND txt.showing = NO THEN
    IF teleporttool() THEN 'CTRL + F1
     prepare_map
    END IF
   END IF
   IF gam.debug_showtags = NO THEN
    IF keyval(scNumpadPlus) > 1 OR keyval(scPlus) > 1 THEN  'CTRL +
     speedcontrol = large(speedcontrol - 1, 10)
     scriptout = STR(speedcontrol)
    END IF
    IF keyval(scNumpadMinus) > 1 OR keyval(scMinus) > 1 THEN  'CTRL -
     speedcontrol = small(speedcontrol + 1, 160)
     scriptout = STR(speedcontrol)
    END IF
   END IF
   IF keyval(scF4) > 1 THEN slice_editor SliceTable.Root
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

 IF wantloadgame > 0 THEN
  'DEBUG debug "loading game slot " & (wantloadgame - 1)
  load_slot = wantloadgame - 1
  wantloadgame = 0
  resetgame scriptout
  initgamedefaults
  stopsong
  resetsfx
  fadeout 0, 0, 0
  queue_fade_in
  doloadgame load_slot
 END IF
 'DEBUG debug "check for death (fatal = " & fatal & ")"
 IF fatal = 1 THEN
  '--this is what happens when you die in battle
  txt.showing = NO
  txt.fully_shown = NO
  IF gen(genGameoverScript) > 0 THEN
   trigger_script gen(genGameoverScript), NO, "death", "", scrqBackcompat()
   fatal = 0
   queue_fade_in 1
  ELSE
   fadeout 255, 0, 0
  END IF
 END IF
 AdvanceSlice SliceTable.root
 END IF' end menus_allow_gameplay

 'Draw screen

 displayall()
 IF fatal = 1 OR abortg > 0 OR resetg THEN
  resetgame scriptout
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
 check_for_queued_fade_in
 'DEBUG debug "tail of main loop"
 dowait
LOOP

LOOP ' This is the end of the DO that encloses a specific RPG file

reset_game_final_cleanup
LOOP ' This is the end of the DO that encloses the entire program.

SUB reset_game_final_cleanup()
 cleanup_text_box
 resetinterpreter 'unload scripts
 unloadmaptilesets tilesets()
 refresh_map_slice_tilesets '--zeroes them out
 unloadtilemaps maptiles()
 unloadtilemap pass
 unloadtilemap foemap
 DeleteZonemap zmap
 'checks for leaks and deallocates them
 sprite_empty_cache()
 palette16_empty_cache()
 cleanup_game_slices()
 SliceDebugDump YES
 cleanup_global_reload_doc
 stopsong
 resetsfx
 IF gam.autorungame THEN exitprogram (NOT abortg)
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
 init_default_text_colors
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

END SUB

SUB displayall()
 update_walkabout_slices()

 IF readbit(gen(), genSuspendBits, suspendoverlay) THEN
  ChangeMapSlice SliceTable.MapLayer(0), , , , 0   'draw all
  SliceTable.ObsoleteOverhead->Visible = NO
 ELSE
  ChangeMapSlice SliceTable.MapLayer(0), , , , 1   'draw non-overhead only
  SliceTable.ObsoleteOverhead->Visible = YES
 END IF
 setmapxy  'Update camera position
 WITH *(SliceTable.MapRoot)
  .X = mapx * -1
  .Y = mapy * -1
 END WITH
 update_backdrop_slice

 DrawSlice(SliceTable.Root, dpage)

 'The order in which we update and draw things is a little strange; I'm just preserving what it was
 animatetilesets tilesets()
 IF harmtileflash = YES THEN
  rectangle 0, 0, 320, 200, gmap(10), dpage
  harmtileflash = NO
 END IF
 IF txt.showing = YES THEN update_textbox

 'FIXME: Eventually we want to draw the rest of this stuff using slices, but for now draw it on top
 update_menu_states
 FOR i as integer = 0 TO topmenu
  draw_menu menus(i), mstates(i), dpage
 NEXT i
 edgeprint scriptout, 0, 190, uilook(uiText), dpage
 showplotstrings
 IF gam.showtext_ticks > 0 THEN
  gam.showtext_ticks -= 1
  edgeprint gam.showtext, xstring(gam.showtext, 160), 180, uilook(uiText), dpage
 END IF
 IF gam.debug_npc_info THEN npc_debug_display
 IF gam.debug_showtags THEN tagdisplay
 IF scrwatch THEN scriptwatcher scrwatch, -1
END SUB

SUB update_heroes(byval force_step_check as integer=NO)
 'note: xgo and ygo are offset of current position from destination, eg +ve xgo means go left
 FOR whoi as integer = 0 TO 3
  IF herow(whoi).speed = 0 THEN
   '--cancel movement, or some of the following code misbehaves
   herow(whoi).xgo = 0
   herow(whoi).ygo = 0
  END IF
  '--if is aligned in at least one direction and passibility is enabled ... and some vehicle stuff ...
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
      IF hero(party_slot) > 0 THEN
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
    fatal = checkfordeath
   END IF

  END IF  'End of harm tile checking
 NEXT whoi

 'If the leader finished a step, check triggers
 IF (herow(0).xgo MOD 20 = 0) AND (herow(0).ygo MOD 20 = 0) AND (didgo(0) = YES OR force_step_check = YES) THEN

  'Trigger NPCs
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
   IF vstate.active = YES AND vstate.dat.random_battles > 0 THEN
    battle_formation_set = vstate.dat.random_battles
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
        fatal = 0
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

SUB update_walkabout_slices()
 update_walkabout_hero_slices()
 update_walkabout_npc_slices()
END SUB

FUNCTION should_hide_hero_caterpillar() as integer
 RETURN vstate.active = YES _
   ANDALSO vstate.mounting = NO _
   ANDALSO vstate.trigger_cleanup = NO _
   ANDALSO vstate.ahead = NO _
   ANDALSO vstate.dat.do_not_hide_leader = NO _
   ANDALSO vstate.dat.do_not_hide_party = NO
END FUNCTION

FUNCTION should_show_normal_caterpillar() as integer
 RETURN readbit(gen(), genBits, 1) = 1 _
   ANDALSO (vstate.active = NO ORELSE vstate.dat.do_not_hide_leader = NO)
END FUNCTION

SUB update_walkabout_hero_slices()

 DIM should_hide as integer = should_hide_hero_caterpillar()
 FOR i as integer = 0 TO UBOUND(herow)
  set_walkabout_vis herow(i).sl, NOT should_hide
 NEXT i

 IF should_show_normal_caterpillar() THEN
  FOR i as integer = 0 TO UBOUND(herow)
   update_walkabout_pos herow(i).sl, catx(i * 5), caty(i * 5), catz(i * 5)
  NEXT i

  DIM cat_slot as integer = 0
  FOR party_slot as integer = 0 TO 3
   IF hero(party_slot) > 0 THEN
    set_walkabout_frame herow(cat_slot).sl, catd(cat_slot * 5), (herow(cat_slot).wtog \ 2)
    cat_slot += 1
   END IF
  NEXT party_slot
  FOR i as integer = cat_slot TO UBOUND(herow)
   set_walkabout_vis herow(i).sl, NO
  NEXT i

 ELSE
  '--non-caterpillar party, vehicle no-hide-leader (or backcompat pref)
  update_walkabout_pos herow(0).sl, catx(0), caty(0), catz(0)
  set_walkabout_frame herow(0).sl, catd(0), (herow(0).wtog \ 2)
  FOR i as integer = 1 TO UBOUND(herow)
   set_walkabout_vis herow(i).sl, NO
  NEXT i
 END IF

END SUB

SUB update_walkabout_npc_slices()
 DIM shadow as Slice Ptr

 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id > 0 THEN '-- if visible
   IF vstate.active AND vstate.npc = i THEN
    '--This NPC is a currently active vehicle, so lets do some extra voodoo.
    IF npc(i).sl <> 0 THEN
     shadow = LookupSlice(SL_WALKABOUT_SHADOW_COMPONENT, npc(i).sl)
     IF shadow <> 0 THEN
      shadow->Visible = (npc(i).z > 0 ANDALSO vstate.dat.disable_flying_shadow = NO)
     END IF
    END IF
   END IF
   update_walkabout_pos npc(i).sl, npc(i).x, npc(i).y, npc(i).z
   IF npc(i).sl <> 0 THEN
    '--default NPC sort is by instance id
    npc(i).sl->Sorter = i
   END IF
  ELSEIF npc(i).id <= 0 THEN
   '--remove unused and hidden NPC slices
   IF npc(i).sl <> 0 THEN
    debug "Sloppy housekeeping: delete npc sl " & i & " [update_walkabout_npc_slices]"
    DeleteSlice @npc(i).sl
   END IF
  END IF
 NEXT i

 '--now apply sprite frame changes
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id > 0 THEN '-- if visible
   set_walkabout_frame npc(i).sl, npc(i).dir, npc(i).frame \ 2
  END IF
 NEXT i

END SUB

SUB update_walkabout_pos (byval walkabout_cont as slice ptr, byval x as integer, byval y as integer, byval z as integer)
 IF walkabout_cont = 0 THEN
  'Exit silently on null slices. It is normal to call this on hero slices that don't exist when the party is non-full
  EXIT SUB
 END IF

 DIM where as XYPair
 '+ gmap(11)
 framewalkabout x, y , where.x, where.y, mapsizetiles.x * 20, mapsizetiles.y * 20, gmap(5)
 WITH *walkabout_cont
  .X = where.x + mapx
  .Y = where.y + mapy
 END WITH

 DIM sprsl as Slice Ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
 IF sprsl = 0 THEN
  debug "update_walkabout_pos: null sprite slice for walkabout slice " & walkabout_cont
 ELSE
  sprsl->Y = gmap(11) - z
 END IF
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
    '--Not the active vehicle
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
 '--Check touch activation. I have no idea why this is here!
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

SUB interpret()
DIM as integer i, n, npcref, temp

'It seems like it would be good to call this immediately before script_interpreter so that
'the return values of fightformation and waitforkey are correct, however doing so might
'break something?
run_queued_scripts

reentersub:
IF nowscript >= 0 THEN
WITH scrat(nowscript)
 SELECT CASE .state
  CASE IS < stnone
   scripterr "illegally suspended script", serrBug
   .state = ABS(.state)
  CASE stnone
   scripterr "script " & nowscript & " became stateless", serrBug
  CASE stwait
   '--evaluate wait conditions
   SELECT CASE .curvalue
    CASE 15, 35, 61'--use door, use NPC, teleport to map
     .state = streturn
    CASE 16'--fight formation
     scriptret = IIF(gam.wonbattle, 1, 0)
     .state = streturn
    CASE 1'--wait number of ticks
     .waitarg -= 1
     IF .waitarg < 1 THEN
      .state = streturn
     END IF
    CASE 2'--wait for all
     n = 0
     FOR i = 0 TO 3
      IF herow(i).xgo <> 0 OR herow(i).ygo <> 0 THEN n = 1
     NEXT i
     IF readbit(gen(), genSuspendBits, suspendnpcs) = 1 THEN
      FOR i = 0 TO 299
       IF npc(i).id > 0 ANDALSO (npc(i).xgo <> 0 OR npc(i).ygo <> 0) THEN n = 1: EXIT FOR
      NEXT i
     END IF
     IF gen(cameramode) = pancam OR gen(cameramode) = focuscam THEN n = 1
     IF n = 0 THEN
      .state = streturn
     END IF
    CASE 3'--wait for hero
     IF .waitarg < 0 OR .waitarg > 3 THEN
      scripterr "waiting for nonexistant hero " & .waitarg, serrBug  'should be bound by waitforhero
      .state = streturn
     ELSE
      IF herow(.waitarg).xgo = 0 AND herow(.waitarg).ygo = 0 THEN
       .state = streturn
      END IF
     END IF
    CASE 4'--wait for NPC
     npcref = getnpcref(.waitarg, 0)
     IF npcref >= 0 ANDALSO .waitarg2 = gam.map.id THEN
      IF npc(npcref).xgo = 0 AND npc(npcref).ygo = 0 THEN
       .state = streturn
      END IF
     ELSE
      '--no reference found, why wait for a non-existant npc?
      .state = streturn
     END IF
    CASE 9'--wait for key
     IF .waitarg >= 0 AND .waitarg <= 5 THEN
      IF carray(.waitarg) > 1 THEN
       .state = streturn
      END IF
      'Because carray(ccMenu) doesn't include it, and we don't want to break scripts
      'doing waitforkey(menu key) followed by looking for key:alt (== scUnfilteredAlt)
      IF .waitarg = ccMenu AND keyval(scUnfilteredAlt) > 1 THEN .state = streturn
     ELSE
      '.waitarg == anykey
      scriptret = anykeypressed()
      'Because anykeypressed doesn't check it, and we don't want to break scripts
      'doing waitforkey(any key) followed by looking for key:alt (== scUnfilteredAlt)
      IF keyval(scUnfilteredAlt) > 1 THEN scriptret = scUnfilteredAlt
      IF scriptret THEN
       .state = streturn
      END IF
     END IF
    CASE 244'--wait for scancode
     IF keyval(.waitarg) > 1 THEN
      .state = streturn
     END IF
    CASE 42'--wait for camera
     IF gen(cameramode) <> pancam AND gen(cameramode) <> focuscam THEN .state = streturn
    CASE 59'--wait for text box
     IF txt.showing = NO OR readbit(gen(), genSuspendBits, suspendboxadvance) = 1 THEN
      .state = streturn
     END IF
    CASE 73, 234, 438'--game over, quit from loadmenu, reset game
    CASE 508'--wait for slice
     IF valid_plotslice(.waitarg, 2) THEN
      IF plotslices(.waitarg)->Velocity.X = 0 ANDALSO plotslices(.waitarg)->Velocity.Y = 0 ANDALSO plotslices(.waitarg)->TargTicks = 0 THEN
       .state = streturn
      END IF
     ELSE
      'If the slice ceases to exist, we should stop waiting for it (after throwing our minor warning)
      .state = streturn
     END IF
    CASE ELSE
     scripterr "illegal wait substate " & .curvalue, serrBug
     .state = streturn
   END SELECT
   IF .state = streturn THEN
    '--this allows us to resume the script without losing a game cycle
    wantimmediate = -1
   END IF
  CASE ELSE
   '--interpret script
   insideinterpreter = YES
   scriptinterpreter
   insideinterpreter = NO
 END SELECT
 IF wantimmediate = -2 THEN
'  IF nowscript < 0 THEN
'   debug "wantimmediate ended on nowscript = -1"
'  ELSE
'   debug "wantimmediate would have skipped wait on command " & scrat(nowscript).curvalue & " in " & scriptname(scrat(nowscript).id) & ", state = " & scrat(nowscript).state
'  END IF
  wantimmediate = 0 'change to -1 to reenable bug
 END IF
 IF wantimmediate = -1 THEN
  '--wow! I hope this doesnt screw things up!
  wantimmediate = 0
  GOTO reentersub
 END IF
END WITH
END IF
script_log_tick
gam.script_log.tick += 1

'--do spawned text boxes, battles, etc.
IF wantbox > 0 THEN
 loadsay wantbox
 wantbox = 0
END IF
IF wantdoor > 0 THEN
 usedoor wantdoor - 1
 wantdoor = 0
END IF
IF wantbattle > 0 THEN
 fatal = 0
 gam.wonbattle = battle(wantbattle - 1)
 wantbattle = 0
 prepare_map YES
 gam.random_battle_countdown = range(100, 60)
 queue_fade_in 2 'FIXME: why 2 ticks?
 setkeys
END IF
IF wantteleport > 0 THEN
 wantteleport = 0
 prepare_map
 gam.random_battle_countdown = range(100, 60)
END IF
IF wantusenpc > 0 THEN
 usenpc 2, wantusenpc - 1
 wantusenpc = 0
END IF
END SUB

'Script commands ('top level', rest is in yetmore.bas)
SUB sfunctions(byval cmdid as integer)
DIM menuslot as integer = ANY
DIM mislot as integer = ANY
DIM npcref as integer = ANY
DIM i as integer = ANY
scriptret = 0
WITH scrat(nowscript)
  'the only commands that belong at the top level are the ones that need
  'access to main-module shared variables (rather few of the commands actually here)
  SELECT CASE as CONST cmdid
   CASE 11'--Show Text Box (box)
    wantbox = retvals(0)
   CASE 15'--use door
    wantdoor = retvals(0) + 1
    .waitarg = 0
    .state = stwait
   CASE 16'--fight formation
    IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) THEN
     wantbattle = retvals(0) + 1
     .waitarg = 0
     .state = stwait
    ELSE
     scriptret = -1
    END IF
   CASE 23'--unequip
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     i = retvals(0)
     unequip i, bound(retvals(1) - 1, 0, 4), gam.hero(i).def_wep, 1
    END IF
   CASE 24'--force equip
    IF valid_hero_party(retvals(0)) THEN
     i = retvals(0)
     IF valid_item(retvals(2)) THEN
      unequip i, bound(retvals(1) - 1, 0, 4), gam.hero(i).def_wep, 0
      doequip retvals(2) + 1, i, bound(retvals(1) - 1, 0, 4), gam.hero(i).def_wep
     END IF
    END IF
   CASE 32'--show backdrop
    gen(genScrBackdrop) = bound(retvals(0) + 1, 0, gen(genNumBackdrops))
   CASE 33'--show map
    gen(genScrBackdrop) = 0
   CASE 34'--dismount vehicle
    forcedismount catd()
   CASE 35'--use NPC
    npcref = getnpcref(retvals(0), 0)
    IF npcref >= 0 THEN
     wantusenpc = npcref + 1
     .waitarg = 0
     .state = stwait
    END IF
   CASE 37'--use shop
    IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxShop) THEN
     shop retvals(0)
    END IF
   CASE 55'--get default weapon
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     scriptret = gam.hero(retvals(0)).def_wep - 1
    ELSE
     scriptret = 0
    END IF
   CASE 56'--set default weapon
    IF valid_hero_party(retvals(0)) THEN
     IF valid_item(retvals(1)) THEN
      '--identify new default weapon
      DIM as integer newdfw = retvals(1) + 1
      '--remember old default weapon
      DIM as integer olddfw = gam.hero(retvals(0)).def_wep
      '--remeber currently equipped weapon
      DIM as integer cureqw = eqstuf(retvals(0), 0)
      '--change default
      gam.hero(retvals(0)).def_wep = newdfw
      '--blank weapon
      unequip retvals(0), 0, olddfw, 0
      IF cureqw <> olddfw THEN
       '--if previously using a weapon, re-equip old weapon
       doequip cureqw, retvals(0), 0, newdfw
      ELSE
       '--otherwize equip new default weapon
       doequip newdfw, retvals(0), 0, newdfw
      END IF
     END IF
    END IF
   CASE 61'--teleport to map
    IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxMap) THEN
     gam.map.id = retvals(0)
     catx(0) = retvals(1) * 20
     caty(0) = retvals(2) * 20
     wantteleport = 1
     .waitarg = 0
     .state = stwait
    END IF
   CASE 63, 169'--resume random enemies
    setbit gen(), genSuspendBits, suspendrandomenemies, 0
    gam.random_battle_countdown = range(100, 60)
   CASE 73'--game over
    abortg = 1
    .state = stwait
   CASE 77'--show value
    scriptout = STR(retvals(0))
   CASE 78'--alter NPC
    IF bound_arg(retvals(1), 0, 16, "NPCstat: constant") THEN
     DIM npcid as integer = get_valid_npc_id(retvals(0), 4)
     IF npcid <> -1 THEN
      DIM as integer writesafe = 1
      IF retvals(1) = 0 THEN
       IF retvals(2) < 0 OR retvals(2) > gen(genMaxNPCPic) THEN
        writesafe = 0
       ELSE
        change_npc_def_sprite npcid, retvals(2)
       END IF
      END IF
      IF retvals(1) = 1 THEN
       change_npc_def_pal npcid, retvals(2)
      END IF
      IF writesafe THEN SetNPCD(npcs(npcid), retvals(1), retvals(2))
      lump_reloading.npcd.dirty = YES
     END IF
    END IF
   CASE 79'--show no value
    scriptout = ""
   CASE 80'--current map
    scriptret = gam.map.id
   CASE 86'--advance text box
    advance_text_box
   CASE 97'--read map block
    IF curcmd->argc = 2 THEN retvals(2) = 0
    IF retvals(2) >= 0 AND retvals(2) <= UBOUND(maptiles) THEN
     scriptret = readblock(maptiles(retvals(2)), bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1))
    END IF
   CASE 98'--write map block
    IF curcmd->argc = 3 THEN retvals(3) = 0
    IF retvals(3) >= 0 AND retvals(3) <= UBOUND(maptiles) AND retvals(2) >= 0 AND retvals(2) <= 255 THEN
     writeblock maptiles(retvals(3)), bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1), retvals(2)
     lump_reloading.maptiles.dirty = YES
    END IF
   CASE 99'--read pass block
    scriptret = readblock(pass, bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1))
   CASE 100'--write pass block
    writeblock pass, bound(retvals(0), 0, mapsizetiles.x-1), bound(retvals(1), 0, mapsizetiles.y-1), bound(retvals(2), 0, 255)
    lump_reloading.passmap.dirty = YES
   CASE 144'--load tileset
    'version that doesn't modify gmap
    IF retvals(0) <= gen(genMaxTile) THEN
     IF retvals(1) < 0 OR curcmd->argc <= 1 THEN
      '0 or 1 args given
      IF retvals(0) < 0 THEN
       'reload all defaults
       loadmaptilesets tilesets(), gmap(), NO
      ELSE
       'change default
       FOR i = 0 TO mapLayerMax
        IF gmap(layer_tileset_index(i)) = 0 THEN loadtilesetdata tilesets(), i, retvals(0)
       NEXT
      END IF
     ELSEIF retvals(1) >= 0 AND retvals(1) <= UBOUND(maptiles) AND retvals(0) >= 0 THEN
      'load tileset for an individual layer.
      loadtilesetdata tilesets(), retvals(1), retvals(0)
     END IF
     '--important to refresh map slices regardless of how the tileset was changed
     refresh_map_slice_tilesets
    END IF
   CASE 305'--change tileset
    'this version of load tileset modifies gmap() for persistent (given map state saving) effects
    IF retvals(0) <= gen(genMaxTile) THEN
     IF retvals(1) < 0 THEN
      IF retvals(0) < 0 THEN
       'reload all defaults
      ELSE
       'change default
       gmap(0) = retvals(0)
      END IF
     ELSEIF retvals(1) >= 0 AND retvals(1) <= UBOUND(maptiles) THEN
      'load tileset for an individual layer
      gmap(layer_tileset_index(retvals(1))) = large(0, retvals(0) + 1)
     END IF
     loadmaptilesets tilesets(), gmap(), NO
     refresh_map_slice_tilesets
    END IF
   CASE 151'--show mini map
    minimap catx(0), caty(0)
   CASE 153'--items menu
    wantbox = items_menu
    'Note script not put into wait state if a textbox is shown
   CASE 155, 170'--save menu
    'ID 155 is a backcompat hack
    scriptret = picksave(0) + 1
    IF scriptret > 0 AND (retvals(0) OR cmdid = 155) THEN
     savegame scriptret - 1
    END IF
   CASE 166'--save in slot
    IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
     savegame retvals(0) - 1
    END IF
   CASE 167'--last save slot
    scriptret = lastsaveslot
   CASE 174'--load from slot
    IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
     IF save_slot_used(retvals(0) - 1) THEN
      wantloadgame = retvals(0)
      .state = stwait
     END IF
    END IF
   CASE 210'--show string
    IF valid_plotstr(retvals(0)) THEN
     scriptout = plotstr(retvals(0)).s
    END IF
   CASE 234'--load menu
    scriptret = picksave(1) + 1
    IF retvals(0) THEN
     IF scriptret = -1 THEN
      abortg = 2  'don't go straight back to loadmenu!
      .state = stwait
      fadeout 0, 0, 0
     ELSEIF scriptret > 0 THEN
      wantloadgame = scriptret
      .state = stwait
     END IF
    END IF
   CASE 245'--save map state
    IF retvals(1) > -1 AND retvals(1) <= 31 THEN
     savemapstate retvals(1), retvals(0), "state"
    ELSEIF retvals(1) = 255 THEN
     savemapstate gam.map.id, retvals(0), "map"
    END IF
   CASE 246'--load map state
    IF retvals(1) > -1 AND retvals(1) <= 31 THEN
     loadmapstate retvals(1), retvals(0), "state", -1
    ELSEIF retvals(1) = 255 THEN
     loadmapstate gam.map.id, retvals(0), "map"
    END IF
   CASE 247'--reset map state
    loadmaplumps gam.map.id, retvals(0)
   CASE 248'--delete map state
    deletemapstate gam.map.id, retvals(0), "map"
   CASE 253'--settileanimationoffset
    IF curcmd->argc < 3 THEN retvals(2) = 0
    IF (retvals(0) = 0 OR retvals(0) = 1) AND retvals(2) >= 0 AND retvals(2) <= UBOUND(maptiles) THEN
     tilesets(retvals(2))->anim(retvals(0)).cycle = retvals(1) MOD 160
    END IF
   CASE 254'--gettileanimationoffset
    IF curcmd->argc < 2 THEN retvals(1) = 0
    IF (retvals(0) = 0 OR retvals(0) = 1) AND retvals(1) >= 0 AND retvals(1) <= UBOUND(maptiles) THEN
     scriptret = tilesets(retvals(1))->anim(retvals(0)).cycle
    END IF
   CASE 255'--animationstarttile
    IF curcmd->argc < 2 THEN retvals(1) = 0
    IF retvals(0) < 160 THEN
     scriptret = retvals(0)
    ELSEIF retvals(0) < 256 AND retvals(1) >= 0 AND retvals(1) <= UBOUND(maptiles) THEN
     scriptret = tilesets(retvals(1))->tastuf(((retvals(0) - 160) \ 48) * 20) + (retvals(0) - 160) MOD 48
    END IF
   CASE 258'--check hero wall
    IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
     DIM as integer tempxgo = 0, tempygo = 0
     IF retvals(1) = 0 THEN tempygo = 20
     IF retvals(1) = 1 THEN tempxgo = -20
     IF retvals(1) = 2 THEN tempygo = -20
     IF retvals(1) = 3 THEN tempxgo = 20
     scriptret = wrappass(catx(retvals(0) * 5) \ 20, caty(retvals(0) * 5) \ 20, tempxgo, tempygo, 0)
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
    add_menu 0
   CASE 274'--open menu
    IF bound_arg(retvals(0), 0, gen(genMaxMenu), "menu ID") THEN
     scriptret = add_menu(retvals(0), (retvals(1) <> 0))
    END IF
   CASE 275'--read menu int
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     scriptret = read_menu_int(menus(menuslot), retvals(1))
    END IF
   CASE 276'--write menu int
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     write_menu_int(menus(menuslot), retvals(1), retvals(2))
     mstates(menuslot).need_update = YES
    END IF
   CASE 277'--read menu item int
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     WITH menus(menuslot)
      scriptret = read_menu_item_int(*.items[mislot], retvals(1))
     END WITH
    END IF
   CASE 278'--write menu item int
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     WITH menus(menuslot)
      write_menu_item_int(*.items[mislot], retvals(1), retvals(2))
     END WITH
     mstates(menuslot).need_update = YES
    END IF
   CASE 279'--create menu
    scriptret = add_menu(-1)
    menus(topmenu).allow_gameplay = YES
   CASE 280'--close menu
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     remove_menu menuslot, NO
    END IF
   CASE 281'--top menu
    IF topmenu >= 0 THEN
     scriptret = menus(topmenu).handle
    END IF
   CASE 282'--bring menu forward
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     bring_menu_forward menuslot
    END IF
   CASE 283'--add menu item
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     append_menu_item(menus(menuslot), "")
     scriptret = assign_menu_item_handle(*menus(menuslot).last)
     mstates(menuslot).need_update = YES
    END IF
   CASE 284'--delete menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     remove_menu_item menus(menuslot), mislot
     mstates(menuslot).need_update = YES
    END IF
   CASE 285'--get menu item caption
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     IF valid_plotstr(retvals(1)) THEN
      plotstr(retvals(1)).s = get_menu_item_caption(*menus(menuslot).items[mislot], menus(menuslot))
     END IF
    END IF
   CASE 286'--set menu item caption
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     IF valid_plotstr(retvals(1)) THEN
      menus(menuslot).items[mislot]->caption = plotstr(retvals(1)).s
     END IF
    END IF
   CASE 287'--get level mp
    IF valid_hero_party(retvals(0)) THEN
     IF bound_arg(retvals(1), 0, 7, "mp level") THEN
      scriptret = lmp(retvals(0), retvals(1))
     END IF
    END IF
   CASE 288'--set level mp
    IF valid_hero_party(retvals(0)) THEN
     IF bound_arg(retvals(1), 0, 7, "mp level") THEN
      lmp(retvals(0), retvals(1)) = retvals(2)
     END IF
    END IF
   CASE 289'--bottom menu
    IF topmenu >= 0 THEN
     scriptret = menus(0).handle
    END IF
   CASE 290'--previous menu
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     menuslot = menuslot - 1
     IF menuslot >= 0 THEN
      scriptret = menus(menuslot).handle
     END IF
    END IF
   CASE 291'--next menu
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     menuslot = menuslot + 1
     IF menuslot <= topmenu THEN
      scriptret = menus(menuslot).handle
     END IF
    END IF
   CASE 292'--menu item by slot
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     scriptret = menu_item_handle_by_slot(menuslot, retvals(1), retvals(2)<>0)
    END IF
   CASE 293'--previous menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     scriptret = menu_item_handle_by_slot(menuslot, mislot - 1, retvals(1)<>0)
    END IF
   CASE 294'--next menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     scriptret = menu_item_handle_by_slot(menuslot, mislot + 1, retvals(1)<>0)
    END IF
   CASE 295'--selected menu item
    IF retvals(0) = -1 THEN
     IF topmenu >= 0 THEN
      scriptret = menu_item_handle_by_slot(topmenu, mstates(topmenu).pt)
     END IF
    ELSE
     menuslot = find_menu_handle(retvals(0))
     IF valid_menuslot(menuslot) THEN
      scriptret = menu_item_handle_by_slot(menuslot, mstates(menuslot).pt)
     END IF
    END IF
   CASE 296'--select menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     mstates(menuslot).pt = mislot
     mstates(menuslot).need_update = YES
    END IF
   CASE 297'--parent menu
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     scriptret = menus(menuslot).handle
    END IF
   CASE 298'--get menu ID
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     scriptret = menus(menuslot).record
    END IF
   CASE 299'--swap menu items
    DIM as integer menuslot2, mislot2
    mislot = find_menu_item_handle(retvals(0), menuslot)
    mislot2 = find_menu_item_handle(retvals(1), menuslot2)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     IF valid_menuslot_and_mislot(menuslot2, mislot2) THEN
      swap_menu_items menus(menuslot), mislot, menus(menuslot2), mislot2
      mstates(menuslot).need_update = YES
      mstates(menuslot2).need_update = YES
     END IF
    END IF
   CASE 300'--find menu item caption
    IF valid_plotstr(retvals(1)) THEN
     menuslot = find_menu_handle(retvals(0))
     DIM start_slot as integer
     IF retvals(2) = 0 THEN
      start_slot = 0
     ELSE
      start_slot = find_menu_item_handle_in_menuslot(retvals(2), menuslot) + 1
     END IF
     IF valid_menuslot_and_mislot(menuslot, start_slot) THEN
      mislot = find_menu_item_slot_by_string(menuslot, plotstr(retvals(1)).s, start_slot, (retvals(3) <> 0))
      IF mislot >= 0 THEN scriptret = menus(menuslot).items[mislot]->handle
     END IF
    END IF
   CASE 301'--find menu ID
    IF bound_arg(retvals(0), 0, gen(genMaxMenu), "menu ID") THEN
     menuslot = find_menu_id(retvals(0))
     IF menuslot >= 0 THEN
      scriptret = menus(menuslot).handle
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
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     scriptret = mislot
    END IF
   CASE 304'--outside battle cure
    'WARNING: This exists for backcompat, but "map cure" should be prefered.
    'See bug 719
    IF bound_arg(retvals(0), 0, gen(genMaxAttack), "attack ID") THEN
     IF valid_hero_party(retvals(1)) THEN
      IF valid_hero_party(retvals(2), -1) THEN
       scriptret = ABS(outside_battle_cure(retvals(0), retvals(1), retvals(2), 0))
      END IF
     END IF
    END IF
   CASE 306'--layer tileset
    IF retvals(0) >= 0 AND retvals(0) <= UBOUND(maptiles) THEN
     scriptret = tilesets(retvals(0))->num
    END IF
   CASE 320'--current text box
    scriptret = -1
    IF txt.showing = YES THEN scriptret = txt.id
   CASE 432 '--use menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     activate_menu_item(*menus(menuslot).items[mislot], menuslot)
    END IF
   CASE 438 '--reset game
    resetg = YES
    .state = stwait
   CASE 490'--use item (id)
    scriptret = 0
    IF valid_item(retvals(0)) THEN
     IF use_item_by_id(retvals(0), wantbox) THEN
      scriptret = 1
     END IF
    END IF
   CASE 491'--use item in slot (slot)
    scriptret = 0
    IF valid_item_slot(retvals(0)) THEN
     DIM consumed as integer '--throwaway, this is not used for anything in this context. use_item_in_slot() just needs it.
     IF use_item_in_slot(retvals(0), wantbox, consumed) THEN
      scriptret = 1
     END IF
    END IF
   CASE 517'--menu item by true slot
    menuslot = find_menu_handle(retvals(0))
    IF valid_menuslot(menuslot) THEN
     DIM menuitem as MenuDefItem ptr = dlist_nth(menus(menuslot).itemlist, retvals(1))
     IF menuitem THEN
      scriptret = menuitem->handle
     ELSE
      scriptret = 0
     END IF
    END IF
   CASE 518'--menu item true slot
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF valid_menuslot_and_mislot(menuslot, mislot) THEN
     scriptret = dlist_find(menus(menuslot).itemlist, menus(menuslot).items[mislot])
     IF scriptret < 0 THEN scripterr "menuitemtrueslot: dlist corruption", serrBug
    END IF

   CASE ELSE '--try all the scripts implemented in subs (insanity!)
    scriptmisc cmdid
    scriptstat cmdid
    '---------
  END SELECT
END WITH
END SUB

FUNCTION valid_item_slot(byval item_slot as integer) as integer
 RETURN bound_arg(item_slot, 0, last_inv_slot(), "item slot")
END FUNCTION

FUNCTION valid_item(byval itemID as integer) as integer
 RETURN bound_arg(itemID, 0, gen(genMaxItem), "item ID")
END FUNCTION

FUNCTION valid_hero_party(byval who as integer, byval minimum as integer=0) as integer
 RETURN bound_arg(who, minimum, 40, "hero party slot")
END FUNCTION

FUNCTION really_valid_hero_party(byval who as integer, byval maxslot as integer=40, byval errlvl as scriptErrEnum = serrBadOp) as integer
 'Defaults to a non-suppressed error
 IF bound_arg(who, 0, maxslot, "hero party slot", , , errlvl) = NO THEN RETURN NO
 IF hero(who) = 0 THEN
  scripterr commandname(curcmd->value) + ": Party hero slot " & who & " is empty", errlvl
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_stat(byval statid as integer) as integer
 RETURN bound_arg(statid, 0, statLast, "stat ID", , , serrBadOp)
END FUNCTION

FUNCTION valid_menuslot(byval menuslot as integer) as integer
 RETURN bound_arg(menuslot, 0, topmenu, "menu handle")
END FUNCTION

FUNCTION valid_menuslot_and_mislot(byval menuslot as integer, byval mislot as integer) as integer
 IF valid_menuslot(menuslot) THEN
  RETURN bound_arg(mislot, 0, menus(menuslot).numitems - 1, "menu item handle")
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotstr(byval n as integer, byval errlvl as scriptErrEnum = serrBound) as integer
 RETURN bound_arg(n, 0, UBOUND(plotstr), "string ID", , , errlvl)
END FUNCTION

FUNCTION valid_formation(byval form as integer) as integer
 RETURN bound_arg(form, 0, gen(genMaxFormation), "formation ID")
END FUNCTION

FUNCTION valid_formation_slot(byval form as integer, byval slot as integer) as integer
 IF bound_arg(form, 0, gen(genMaxFormation), "formation ID") THEN
  RETURN bound_arg(slot, 0, 7, "formation slot")
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_zone(byval id as integer) as integer
 RETURN bound_arg(id, 1, 9999, "zone ID", , , serrBadOp)
END FUNCTION

FUNCTION valid_door(byval id as integer) as integer
 IF bound_arg(id, 0, UBOUND(gam.map.door), "door", , , serrBadOp) = NO THEN RETURN NO
 IF readbit(gam.map.door(id).bits(), 0, 0) = 0 THEN
  'Door doesn't exist
  scripterr commandname(curcmd->value) & ": invalid door id " & id, serrBadOp
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_tile_pos(byval x as integer, byval y as integer) as integer
 IF x < 0 OR y < 0 OR x >= mapsizetiles.x OR y >= mapsizetiles.y THEN
  scripterr commandname(curcmd->value) + ": invalid map position " & x & "," & y & " -- map is " & mapsizetiles.x & "*" & mapsizetiles.y & " tiles", serrBadOp
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

SUB loadmap_gmap(byval mapnum as integer)
 lump_reloading.gmap.dirty = NO
 lump_reloading.gmap.changed = NO
 loadrecord gmap(), game & ".map", getbinsize(binMAP) / 2, mapnum
 IF gmap(31) = 0 THEN gmap(31) = 2

 loadmaptilesets tilesets(), gmap()
 refresh_map_slice_tilesets

 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT
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
 reloadnpc
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

FUNCTION should_skip_this_timer(byval l as integer, t as PlotTimer) as integer
 IF l = 1 THEN
  'This is happening in battle!
  IF (t.flags AND 2) = 0 THEN
   'timerflag:battle bit is OFF
   RETURN YES
  END IF
 ELSEIF l = 2 THEN
  'This is happening in a menu!
  IF (t.flags AND 4) = 0 THEN
   'timerflag:battle bit is OFF
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

SUB dotimer(byval l as integer)
  dim i as integer
  for i = 0 to ubound(timers)
    with timers(i)
      if .pause then continue for
      if .speed > 0 then
        if should_skip_this_timer(l, timers(i)) then continue for 'not supposed to run here
        'debug "i=" & i & " l=" & l & " .speed=" & .speed & " .ticks=" & .ticks & " .count=" & .count & " .flags=" & .flags & " .trigger=" & .trigger

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
            if .trigger = -2 then 'game over
              fatal = 1
              abortg = 1

              exit sub
            end if

            if .trigger = -1 then 'undefined, shouldn't happen
            end if

            if .trigger > -1 then 'plotscript
              trigger_script .trigger, NO, "timer", "", scrqBackcompat()
              trigger_script_arg 0, i, "id"
            end if
          end if
        end if
      end if
    end with
  next
end sub

function dotimerbattle() as integer
  dotimer 1  'no sense duplicating code

  dim i as integer
  for i = 0 to ubound(timers)
    with timers(i)
      if .speed < 0 then 'normally, not valid. but, if a timer expired in battle, this will be -ve, -1
        if .flags AND 1 then return -1
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
       menu_text_box = items_menu
       IF menu_text_box > 0 THEN
        IF mi.close_if_selected = NO THEN
         remove_menu menuslot, (mi.skip_close_script = NO)
        END IF
        EXIT DO
       END IF
      CASE 1 ' spell
       slot = onwho(readglobalstring(106, "Whose Spells?", 20), 0)
       IF slot >= 0 THEN spells_menu slot
      CASE 2 ' status
       slot = onwho(readglobalstring(104, "Whose Status?", 20), 0)
       IF slot >= 0 THEN status slot
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
       slot = picksave(1)
       IF slot >= 0 THEN
        wantloadgame = slot + 1
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

FUNCTION find_menu_id (byval id as integer) as integer
 DIM i as integer
 FOR i = topmenu TO 0 STEP -1
  IF menus(i).record = id THEN
   RETURN i 'return slot
  END IF
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_handle (byval handle as integer) as integer
 DIM i as integer
 FOR i = 0 TO topmenu
  IF menus(i).handle = handle THEN RETURN i 'return slot
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_item_handle_in_menuslot (byval handle as integer, byval menuslot as integer) as integer
 DIM mislot as integer
 WITH menus(menuslot)
  FOR mislot = 0 TO .numitems - 1
   IF .items[mislot]->handle = handle THEN RETURN mislot
  NEXT mislot
 END WITH
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_item_handle (byval handle as integer, byref found_in_menuslot as integer) as integer
 DIM menuslot as integer
 DIM mislot as integer
 DIM found as integer
 FOR menuslot = 0 TO topmenu
  found = find_menu_item_handle_in_menuslot(handle, menuslot)
  IF found >= 0 THEN
   found_in_menuslot = menuslot
   RETURN found
  END IF
 NEXT menuslot
 found_in_menuslot = -1
 RETURN -1 ' Not found
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

FUNCTION menu_item_handle_by_slot(byval menuslot as integer, byval mislot as integer, byval visible_only as integer=YES) as integer
 IF menuslot >= 0 AND menuslot <= topmenu THEN
  WITH menus(menuslot)
   IF mislot >= 0 AND mislot < .numitems THEN
    WITH *.items[mislot]
     IF visible_only AND .disabled AND .hide_if_disabled THEN RETURN 0
     RETURN .handle
    END WITH
   END IF
  END WITH
 END IF
 RETURN 0
END FUNCTION

FUNCTION find_menu_item_slot_by_string(byval menuslot as integer, s as string, byval mislot as integer=0, byval visible_only as integer=YES) as integer
 DIM i as integer
 DIM cap as STRING
 WITH menus(menuslot)
  FOR i = mislot TO .numitems - 1
   WITH *.items[i]
    IF visible_only AND .disabled AND .hide_if_disabled THEN CONTINUE FOR
    cap = get_menu_item_caption(*menus(menuslot).items[i], menus(menuslot))
    IF cap = s THEN
     RETURN i
    END IF
   END WITH
  NEXT i
 END WITH
 RETURN -1 ' not found
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

 'load gmap
 loadmapstate_gmap gam.map.id, "map"

 'Play map music
 IF readbit(gen(), genSuspendBits, suspendambientmusic) = 0 THEN
  IF gmap(1) > 0 THEN
   wrappedsong gmap(1) - 1
  ELSEIF gmap(1) = 0 THEN
   stopsong
  ELSEIF gmap(1) = -1 AND afterbat = YES THEN
   IF gam.remembermusic > -1 THEN wrappedsong gam.remembermusic ELSE stopsong
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

SUB reset_game_state ()
 reset_map_state(gam.map)
 gam.wonbattle = NO
 gam.remembermusic = -1
 gam.random_battle_countdown = range(100, 60)
 gam.mouse_enabled = NO

 'If we are resetting, the old slices will have already been destroyed
 'by cleanup_game_slices() so we just re-assign herow().sl
 FOR i as integer = 0 TO UBOUND(herow)
  herow(i).sl = create_walkabout_slices(hero_layer())
 NEXT i
END SUB

FUNCTION hero_layer() as Slice Ptr
 DIM layer as Slice Ptr
 IF gmap(16) = 2 THEN ' heroes and NPCs together
  layer = SliceTable.Walkabout
 ELSE ' heroes and NPCs on separate layers
  layer = SliceTable.HeroLayer
 END IF
 IF layer = 0 THEN
  debug "Warning: null hero layer, gmap(16)=" & gmap(16)
 END IF
 RETURN layer
END FUNCTION

FUNCTION npc_layer() as Slice Ptr
 DIM layer as Slice Ptr
 IF gmap(16) = 2 THEN ' heroes and NPCs together
  layer = SliceTable.Walkabout
 ELSE ' heroes and NPCs on separate layers
  layer = SliceTable.NPCLayer
 END IF
 IF layer = 0 THEN
  debug "Warning: null npc layer, gmap(16)=" & gmap(16)
 END IF
 RETURN layer
END FUNCTION

FUNCTION create_walkabout_slices(byval parent as Slice Ptr) as Slice Ptr
 DIM sl as Slice Ptr
 sl = NewSliceOfType(slContainer, parent)
 WITH *sl
  .Width = 20
  .Height = 20
  .Protect = YES
 END WITH
 DIM sprsl as Slice Ptr
 sprsl = NewSliceOfType(slSprite, sl, SL_WALKABOUT_SPRITE_COMPONENT)
 WITH *sprsl
  'Anchor and align NPC sprite in the bottom center of the NPC container
  .AnchorHoriz = 1
  .AnchorVert = 2
  .AlignHoriz = 1
  .AlignVert = 2
  .Protect = YES
 END WITH
 RETURN sl
END FUNCTION

SUB reset_map_state (map as MapModeState)
 map.id = gen(genStartMap)
 map.lastmap = -1
 map.same = NO
 map.name = ""
END SUB

SUB checkdoors ()
 'If the leader is standing on a door, use it.
 IF vstate.active = YES AND vstate.dat.enable_door_use = NO THEN EXIT SUB 'Doors are disabled by a vehicle
 FOR door_id as integer = 0 TO 99
  IF readbit(gam.map.door(door_id).bits(),0,0) THEN 'Door is enabled
   IF gam.map.door(door_id).x = catx(0) \ 20 AND gam.map.door(door_id).y = (caty(0) \ 20) + 1 THEN
    usedoor door_id
    EXIT SUB
   END IF
  END IF
 NEXT door_id
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
  fatal = 0
  gam.wonbattle = battle(txt.box.battle)
  prepare_map YES
  gam.random_battle_countdown = range(100, 60)
  queue_fade_in
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
    queue_fade_in
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
 WITH txt.portrait
  IF .sprite THEN frame_unload @.sprite
  IF .pal    THEN palette16_unload @.pal
 END WITH
 txt.showing = NO
 txt.fully_shown = NO
 txt.sayer = -1
 txt.id = -1
 IF txt.sl THEN DeleteSlice @(txt.sl)
 ClearTextBox txt.box
 setkeys
 flusharray carray(), 7, 0
END SUB

SUB init_default_text_colors()
 textcolor uilook(uiText), 0
 FOR i as integer = 0 TO UBOUND(plotstr)
  plotstr(i).Col = uilook(uiText)
 NEXT i
END SUB

SUB init_text_box_slices(txt as TextBoxState)
 IF txt.sl THEN
  '--free any already-loaded textbox
  DeleteSlice @(txt.sl)
 END IF
 txt.sl = NewSliceOfType(slContainer, SliceTable.TextBox, SL_TEXTBOX_ROOT)
 WITH *txt.sl
  '.Fill = YES
  .Width = 320
  .Height = 200
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
  .X = 4
  .Y = 4 + txt.box.vertical_offset * 4
  .Width = 312
  .Height = get_text_box_height(txt.box)
  .PaddingLeft = 4
  .PaddingRight = 4
  .PaddingTop = 3
  .PaddingBottom = 3
 END WITH

 '--Set up the actual text
 DIM col as integer
 col = uilook(uiText)
 IF txt.box.textcolor > 0 THEN col = txt.box.textcolor

 DIM s as STRING = ""
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
 DIM img_id as integer = -1
 DIM pal_id as integer = -1
 DIM hero_id as integer = -1
 DIM her as HeroDef
 SELECT CASE txt.box.portrait_type
  CASE 1' Fixed ID number
   img_id = txt.box.portrait_id
   pal_id = txt.box.portrait_pal
  CASE 2' Hero by caterpillar
   hero_id = herobyrank(txt.box.portrait_id)
  CASE 3' Hero by party slot
   IF txt.box.portrait_id >= 0 AND txt.box.portrait_id <= UBOUND(hero) THEN
    hero_id = hero(txt.box.portrait_id) - 1
   END IF
 END SELECT
 IF hero_id >= 0 THEN
  loadherodata her, hero_id
  img_id = her.portrait
  pal_id = her.portrait_pal
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
   IF .Y > vpages(dpage)->h - (.Height + 4) THEN .Y = 32
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
 WITH txt.portrait
  IF .sprite THEN frame_unload @.sprite
  IF .pal    THEN palette16_unload @.pal
 END WITH
 IF txt.sl THEN DeleteSlice @(txt.sl)
END SUB

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

SUB reparent_hero_slices()
 FOR i as integer = 0 TO UBOUND(herow)
  SetSliceParent herow(i).sl, hero_layer()
 NEXT i
END SUB

SUB orphan_hero_slices()
 FOR i as integer = 0 TO UBOUND(herow)
  OrphanSlice herow(i).sl
 NEXT i
END SUB

SUB reparent_npc_slices()
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).sl THEN
   SetSliceParent npc(i).sl, npc_layer()
  END IF
 NEXT i
END SUB

SUB orphan_npc_slices()
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).sl THEN
   OrphanSlice npc(i).sl
  END IF
 NEXT i
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

SUB refresh_walkabout_layer_sort()
 orphan_hero_slices
 orphan_npc_slices
 IF gmap(16) = 2 THEN ' Heroes and NPCs Together
  DeleteSlice @SliceTable.HeroLayer
  DeleteSlice @SliceTable.NPCLayer
  SliceTable.Walkabout->AutoSort = slAutoSortY
 ELSE
  'Hero and NPC in separate layers
  SliceTable.Walkabout->AutoSort = slAutoSortNone
  IF SliceTable.HeroLayer = 0 THEN
   '--create the hero layer if it is needed
   SliceTable.HeroLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_HERO_LAYER)
   SliceTable.HeroLayer->Fill = YES
   SliceTable.HeroLayer->Protect = YES
   SliceTable.HeroLayer->AutoSort = slAutoSortY
  END IF
  IF SliceTable.NPCLayer = 0 THEN
   SliceTable.NPCLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_NPC_LAYER)
   SliceTable.NPCLayer->Fill = YES
   SliceTable.NPCLayer->Protect = YES
   SliceTable.NPCLayer->AutoSort = slAutoSortCustom
  END IF
  IF gmap(16) = 1 THEN
   SliceTable.HeroLayer->Sorter = 0
   SliceTable.NPCLayer->Sorter = 1
  ELSE
   SliceTable.NPCLayer->Sorter = 0
   SliceTable.HeroLayer->Sorter = 1
  END IF
  CustomSortChildSlices SliceTable.Walkabout, YES
 END IF
 reparent_hero_slices
 reparent_npc_slices
END SUB

FUNCTION vehicle_is_animating() as integer
 WITH vstate
  RETURN .mounting ORELSE .rising ORELSE .falling ORELSE .init_dismount ORELSE .ahead ORELSE .trigger_cleanup
 END WITH
END FUNCTION

SUB reset_vehicle(v as VehicleState)
 v.id = -1
 v.npc = 0
 v.old_speed = 0
 v.active   = NO
 v.mounting = NO
 v.rising   = NO
 v.falling  = NO
 v.init_dismount   = NO
 v.ahead           = NO
 v.trigger_cleanup = NO
 ClearVehicle v.dat
END SUB

SUB dump_vehicle_state()
 WITH vstate
  debug "active=" & .active & " npc=" & .npc & " id=" & .id & " mounting=" & .mounting & " rising=" & .rising & " falling=" & .falling & " dismount=" & .init_dismount & " cleanup=" & .trigger_cleanup & " ahead=" & .ahead
 END WITH
END SUB

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

FUNCTION want_to_check_for_walls(byval who as integer) as integer
 IF movdivis(herow(who).xgo) = 0 AND movdivis(herow(who).ygo) = 0 THEN RETURN NO
 IF gam.walk_through_walls = YES THEN RETURN NO
 IF vstate.dat.pass_walls = YES THEN RETURN NO
 IF vstate.active THEN
  DIM thisherotilex as integer = catx(who * 5) \ 20
  DIM thisherotiley as integer = caty(who * 5) \ 20
  IF vehpass(vstate.dat.override_walls, readblock(pass, thisherotilex, thisherotiley), 0) <> 0 THEN RETURN NO
 END IF
 RETURN YES
END FUNCTION

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
  IF hero(i) = 0 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION first_free_slot_in_reserve_party() as integer
 '--returns the first free slot, or -1 if all slots are full
 IF free_slots_in_party() > 0 THEN
  FOR i as integer = 4 TO 40
   IF hero(i) = 0 THEN RETURN i
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

SUB change_npc_def_sprite (byval npc_id as integer, byval walkabout_sprite_id as integer)
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id - 1 = npc_id THEN
   'found a match!
   set_walkabout_sprite npc(i).sl, walkabout_sprite_id
  END IF
 NEXT i
END SUB

SUB change_npc_def_pal (byval npc_id as integer, byval palette_id as integer)
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id - 1 = npc_id THEN
   'found a match!
   set_walkabout_sprite npc(i).sl, , palette_id
  END IF
 NEXT i
END SUB

SUB create_walkabout_shadow (byval walkabout_cont as Slice Ptr)
 IF walkabout_cont = 0 THEN debug "create_walkabout_shadow: null walkabout container": EXIT SUB
 DIM sprsl as Slice Ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
 IF sprsl = 0 THEN debug "create_walkabout_shadow: null walkabout sprite": EXIT SUB
 DIM shadow as Slice Ptr
 shadow = NewSliceOfType(slEllipse, ,SL_WALKABOUT_SHADOW_COMPONENT)
 WITH *shadow
  .Width = 12
  .Height = 6
  .AnchorHoriz = 1
  .AlignHoriz = 1
  .AnchorVert = 2
  .AlignVert = 2
  .Y = gmap(11) 'foot offset
  .Visible = NO
 END WITH
 ChangeEllipseSlice shadow, uilook(uiShadow), uilook(uiShadow)
 InsertSliceBefore(sprsl, shadow)
END SUB

SUB delete_walkabout_shadow (byval walkabout_cont as Slice Ptr)
 IF walkabout_cont = 0 THEN debug "delete_walkabout_shadow: null walkabout container": EXIT SUB
 DIM shadow as Slice Ptr
 shadow = LookupSlice(SL_WALKABOUT_SHADOW_COMPONENT, walkabout_cont)
 IF shadow = 0 THEN debug "delete_walkabout_shadow: no shadow to delete" : EXIT SUB
 DeleteSlice @shadow
END SUB

SUB cleanup_game_slices ()
 FOR i as integer = 0 TO UBOUND(herow)
  DeleteSlice @herow(i).sl
 NEXT i
 FOR i as integer = 0 TO UBOUND(npc)
  DeleteSlice @npc(i).sl
 NEXT i
 DestroyGameSlices
END SUB

SUB queue_fade_in (byval delay as integer = 0)
 gam.need_fade_in = YES
 gam.fade_in_delay = delay
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
 DIM menu(3) as string
 menu(0) = "Test Battles"
 menu(1) = "View/Edit Slice Tree"
 menu(2) = "Manipulate gen() array"
 menu(3) = "Manipulate gmap() array"
 DIM result as integer
 result = multichoice("Misc. Debug", menu(), default, , "game_misc_debug")
 IF result = -1 THEN EXIT SUB
 default = result
 SELECT CASE result
  CASE 0: battle_formation_testing_menu
  CASE 1: slice_editor SliceTable.Root
  CASE 2: patcharray gen(), "gen"
  CASE 3: patcharray gmap(), "gmap"
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

  IF enter_or_space() THEN
   form_num = menu.items[state.pt]->extra(0)
   IF form_num >= 0 THEN
    defaultval = state.pt
    fatal = 0
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
