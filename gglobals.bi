'OHRRPGCE GAME - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

'$INCLUDE: 'udts.bi'

'Misc game globals
EXTERN game$
EXTERN sourcerpg$
EXTERN tmpdir$
EXTERN exename$
EXTERN timing()
EXTERN pal16()
EXTERN names$()
EXTERN fmvol
EXTERN speedcontrol
EXTERN deferpaint
EXTERN presentsong
EXTERN foemaph
EXTERN lockfile
EXTERN lastsaveslot
EXTERN abortg
EXTERN usepreunlump%

'Input handling globals
EXTERN carray(), csetup()
EXTERN gotj(), joy()

'Game state globals
EXTERN gen()
EXTERN tag()
EXTERN global()

'Vehicle globals
EXTERN veh()

'Hero globals
EXTERN hero()
EXTERN eqstuf()
EXTERN lmp()
EXTERN bmenu()
EXTERN spell()
EXTERN exlev&()
EXTERN herobits%()
EXTERN itembits%()
EXTERN hmask()
EXTERN gold
EXTERN nativehbits()

'Map state globals
EXTERN gmap()
EXTERN scroll()
EXTERN pass()
EXTERN mapx, mapy
EXTERN framex
EXTERN framey

'Hero walkabout globals
EXTERN catx(), caty(), catz(), catd()
EXTERN herospeed()
EXTERN xgo(), ygo()
EXTERN wtog()
EXTERN catermask()

'NPC globals
EXTERN npcs() as NPCType
EXTERN npc() as NPCInst

'Item globals
EXTERN inventory() as InventSlot

'Script globals
EXTERN script()
EXTERN heap()
EXTERN scrat()
EXTERN retvals()
EXTERN nowscript
EXTERN scriptret
EXTERN nextscroff

'Script string globals
EXTERN plotstring$()
EXTERN plotstrX(), plotstrY()
EXTERN plotstrCol()
EXTERN plotstrBGCol()
EXTERN plotstrBits()

'Battle globals
EXTERN battlecaption$
EXTERN battlecaptime
EXTERN battlecapdelay
EXTERN bstackstart
EXTERN learnmask()
