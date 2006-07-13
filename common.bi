'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

'$INCLUDE: 'util.bi'

DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$, needf = 0)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB fadein (force%)
DECLARE SUB fadeout (red%, green%, blue%, force%)
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION soundfile$ (sfxnum%)
DECLARE SUB getui(f$)
DECLARE SUB safekill (f$)

COMMON SHARED uilook(), vpage, dpage, buffer(), version$, fadestate, master()

