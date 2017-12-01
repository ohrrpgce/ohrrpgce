'OHRRPGCE - File Browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef BROWSE_BI
#define BROWSE_BI

DECLARE FUNCTION browse (special as integer, byref default as string, fmask as string = "", helpkey as string = "", needf as bool = NO) as string
DECLARE SUB set_browse_default (default as string)

#ENDIF
