'OHRRPGCE GAME - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'Misc game globals
COMMON SHARED game$, sourcerpg$, buffer(), master(), gen(), workingdir$, tag(), timing(), global(), carray(), csetup(), gotj(), joy(), veh(), hero(), vpage, dpage, pal16(), names$(), eqstuf(), item(), item$(), lmp(), bmenu(), spell(),  _
exlev&(), gold&, herobits%(), itembits%(), fmvol, hmask(), version$, speedcontrol, deferpaint, tmpdir$, nativehbits(), catx(), caty(), catz(), catd(), herospeed(), xgo(), ygo(), npcl(), mapx, mapy, presentsong, keyv(), fadestate, foemaph, lockfile,  _
lastsaveslot, plotstring$(), plotstrX(), plotstrY(), plotstrCol(), plotstrBGCol(), plotstrBits(), npcs(), wtog(), catermask(), framex, framey, gmap(), scroll(), progdir$, exename$, defbinsize(), curbinsize()

'Only used for ASM keyhandler
COMMON SHARED regs AS Regtype, off9, seg9

'Script globals
COMMON SHARED script(), heap(), astack(), scrat(), retvals(), nowscript, scriptret, nextscroff

'Battle globals
COMMON SHARED battlecaption$, battlecaptime, battlecapdelay, bstackstart, learnmask()

