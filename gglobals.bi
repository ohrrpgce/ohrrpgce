'OHRRPGCE GAME - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "udts.bi"
#include "game_udts.bi"
#include "battle_udts.bi"
#include "slices.bi"
#include "os.bi"

'Misc game globals
EXTERN prefsdir as string ' currently only used by Linux
EXTERN timing() as integer
EXTERN pal16() as integer
EXTERN speedcontrol as integer
EXTERN autotestmode as integer
EXTERN deferpaint as integer
EXTERN presentsong as integer
EXTERN lastsaveslot as integer
EXTERN abortg as integer
EXTERN resetg as integer
EXTERN usepreunlump as integer
EXTERN fatal as bool
EXTERN checkfatal as bool
EXTERN err_suppress_lvl as scriptErrEnum
EXTERN backcompat_sound_slot_mode as integer
EXTERN backcompat_sound_slots() as integer
EXTERN autosnap as integer
EXTERN running_as_slave as integer
EXTERN custom_version as string
EXTERN master_channel as IPCChannel
EXTERN modified_lumps as string vector
EXTERN lump_reloading as LumpReloadOptions
EXTERN force_prefsdir_save as integer

'Input handling globals
EXTERN as integer carray(), csetup()
EXTERN as integer gotj(), joy()

'Game state globals
EXTERN gam AS GameState
EXTERN txt AS TextBoxState
EXTERN tag() as integer
EXTERN onetime() as integer

'Vehicle globals
EXTERN vstate AS VehicleState

'Hero globals
EXTERN hero() as integer
EXTERN names() as string
EXTERN stat() as integer
EXTERN eqstuf() as integer
EXTERN lmp() as integer
EXTERN spell() as integer
EXTERN hmask() as integer

'Map state globals
EXTERN gmap() as integer
EXTERN maptiles() as TileMap
EXTERN pass as TileMap
EXTERN foemap as TileMap
EXTERN zmap as ZoneMap
EXTERN mapsizetiles as XYPair
EXTERN as integer mapx, mapy  'camera pos
EXTERN tilesets() as TilesetData ptr

'Hero walkabout globals
EXTERN as integer catx(), caty(), catz(), catd()
EXTERN herow() as HeroWalkabout

'NPC globals
EXTERN npcs() as NPCType
EXTERN npc() as NPCInst

'Item globals
EXTERN inventory() as InventSlot
EXTERN gold as integer

'Script globals
EXTERN script() as ScriptData Ptr
EXTERN global() as integer
EXTERN heap() as integer
EXTERN scrat() as ScriptInst
EXTERN retvals() as integer
EXTERN wantimmediate as integer
EXTERN insideinterpreter as integer
EXTERN scrwatch as integer
EXTERN nowscript as integer
EXTERN scriptret as integer
EXTERN numloadedscr as integer
EXTERN totalscrmem as integer
EXTERN scriptctr as integer
EXTERN next_interpreter_check_time as double
EXTERN interruption_grace_period as integer
EXTERN scrst as Stack
EXTERN curcmd as ScriptCommand ptr
EXTERN last_queued_script as QueuedScript ptr
EXTERN scrqFirst() as QueuedScript
EXTERN scrqBackcompat() as QueuedScript
EXTERN scrqLast() as QueuedScript
'debugger aids
EXTERN globalp as integer ptr
EXTERN heapp as integer ptr
EXTERN scratp as ScriptInst ptr
EXTERN scriptp as ScriptData ptr ptr
EXTERN retvalsp as integer ptr
EXTERN plotslicesp as slice ptr ptr

'Script string globals
EXTERN plotstr() as Plotstring

'Battle globals
EXTERN lastformation as integer
EXTERN bstackstart as integer
EXTERN learnmask() as integer
EXTERN atkq() as AttackQueue

EXTERN timers() as PlotTimer

'Menu globals
EXTERN menus() as MenuDef
EXTERN mstates() as MenuState
EXTERN menu_set as MenuSet
EXTERN topmenu as INTEGER

'Slice handles
EXTERN plotslices() as Slice Ptr
