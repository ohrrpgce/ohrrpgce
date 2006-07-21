'OHRRPGCE GAME - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

'$INCLUDE: 'udts.bi'

'Misc game globals
COMMON SHARED game$
COMMON SHARED sourcerpg$
COMMON SHARED tmpdir$
COMMON SHARED exename$
COMMON SHARED timing()
COMMON SHARED pal16()
COMMON SHARED names$()
COMMON SHARED fmvol
COMMON SHARED speedcontrol
COMMON SHARED deferpaint
COMMON SHARED presentsong
COMMON SHARED foemaph
COMMON SHARED lockfile
COMMON SHARED lastsaveslot
COMMON SHARED defbinsize()
COMMON SHARED curbinsize()
COMMON SHARED abortg
COMMON SHARED usepreunlump%

'Input handling globals
COMMON SHARED carray(), csetup()
COMMON SHARED gotj(), joy()
COMMON SHARED keyv()

'Game state globals
COMMON SHARED gen()
COMMON SHARED tag()
COMMON SHARED global()

'Vehicle globals
COMMON SHARED veh()

'Hero globals
COMMON SHARED hero()
COMMON SHARED eqstuf()
COMMON SHARED lmp()
COMMON SHARED bmenu()
COMMON SHARED spell()
COMMON SHARED exlev&()
COMMON SHARED herobits%()
COMMON SHARED itembits%()
COMMON SHARED hmask()
COMMON SHARED gold&
COMMON SHARED nativehbits()

'Map state globals
COMMON SHARED gmap()
COMMON SHARED scroll()
COMMON SHARED mapx, mapy
COMMON SHARED framex
COMMON SHARED framey

'Hero walkabout globals
COMMON SHARED catx(), caty(), catz(), catd()
COMMON SHARED herospeed()
COMMON SHARED xgo(), ygo()
COMMON SHARED wtog()
COMMON SHARED catermask()

'NPC globals
COMMON SHARED npcs()
COMMON SHARED npc() as NPCInst

'Item globals
COMMON SHARED inventory() as InventSlot

'Script globals
COMMON SHARED script()
COMMON SHARED heap()
COMMON SHARED astack()
COMMON SHARED scrat()
COMMON SHARED retvals()
COMMON SHARED nowscript
COMMON SHARED scriptret
COMMON SHARED nextscroff

'Script string globals
COMMON SHARED plotstring$()
COMMON SHARED plotstrX(), plotstrY()
COMMON SHARED plotstrCol()
COMMON SHARED plotstrBGCol()
COMMON SHARED plotstrBits()

'Battle globals
COMMON SHARED battlecaption$
COMMON SHARED battlecaptime
COMMON SHARED battlecapdelay
COMMON SHARED bstackstart
COMMON SHARED learnmask()
