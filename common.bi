'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$, needf = 0)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB fadein (force%)
DECLARE SUB fadeout (red%, green%, blue%, force%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE SUB safekill (f$)
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION soundfile$ (sfxnum%)
DECLARE FUNCTION trimpath$ (filename$)
DECLARE FUNCTION trimextension$ (filename$)
DECLARE SUB getui(f$)

COMMON SHARED uilook(), vpage, dpage, buffer(), version$, fadestate, master()

