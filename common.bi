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
DECLARE FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
DECLARE SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
DECLARE FUNCTION aquiretempdir$ ()
DECLARE SUB copylump(package$, lump$, dest$)
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)

'Global variables
COMMON SHARED uilook()
COMMON SHARED vpage, dpage
COMMON SHARED buffer()
COMMON SHARED version$
COMMON SHARED fadestate
COMMON SHARED master()
COMMON SHARED workingdir$

