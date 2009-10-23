'OHRRPGCE GAME - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "udts.bi"
#include "game_udts.bi"
#include "slices.bi"

'Misc game globals
EXTERN sourcerpg as string
EXTERN prefsdir as string ' currently only used by Linux
EXTERN savefile as string
EXTERN timing() as integer
EXTERN pal16() as integer
EXTERN speedcontrol as integer
EXTERN deferpaint as integer
EXTERN presentsong as integer
EXTERN foemaph as integer
EXTERN lastsaveslot as integer
EXTERN abortg as integer
EXTERN resetg as integer
EXTERN usepreunlump as integer
EXTERN fatal as integer
EXTERN err_suppress_lvl as integer
EXTERN backcompat_sound_slot_mode as integer
EXTERN backcompat_sound_slots() as integer

'Input handling globals
EXTERN as integer carray(), csetup()
EXTERN as integer gotj(), joy()
EXTERN as integer mouse()

'Game state globals
EXTERN gam AS GameState
EXTERN txt AS TextBoxState
EXTERN tag() as integer

'Vehicle globals
EXTERN vstate AS VehicleState

'Hero globals
EXTERN hero() as integer
EXTERN names() as string
EXTERN stat() as integer
EXTERN eqstuf() as integer
EXTERN lmp() as integer
EXTERN bmenu() as integer
EXTERN spell() as integer
EXTERN exlev() as integer
EXTERN herobits() as integer
EXTERN itembits() as integer
EXTERN hmask() as integer
EXTERN nativehbits() as integer

'Map state globals
EXTERN gmap() as integer
EXTERN scroll() as integer
EXTERN pass() as integer
EXTERN as integer mapx, mapy
EXTERN framex as integer
EXTERN framey as integer
EXTERN tilesets() as TilesetData ptr

'Hero walkabout globals
EXTERN as GraphicPair herow()
EXTERN as integer catx(), caty(), catz(), catd()
EXTERN herospeed() as integer
EXTERN as integer xgo(), ygo()
EXTERN wtog() as integer

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
EXTERN insideinterpreter as integer
EXTERN scrwatch as integer
EXTERN nowscript as integer
EXTERN scriptret as integer
EXTERN numloadedscr as integer
EXTERN totalscrmem as integer
EXTERN scriptctr as integer
EXTERN scrst as Stack
EXTERN curcmd as ScriptCommand ptr
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

EXTERN timers() as PlotTimer

'Menu globals
EXTERN menus() as MenuDef
EXTERN mstates() as MenuState
EXTERN menu_set as MenuSet
EXTERN topmenu as INTEGER

'Slice handles
EXTERN plotslices() as Slice Ptr
