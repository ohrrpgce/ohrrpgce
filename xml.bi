#ifndef XML_BI
#define XML_BI

'OHRRPGCE COMMON - XML related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "libxml/tree.bi"

#include "udts.bi"

DECLARE sub xmlOpen
DECLARE sub xmlClose

DECLARE function compileTextbox(byref txt as Textbox) as xmlNodePtr
DECLARE function createNS(byval ns as string, byval node as xmlNodePtr, byval prefix as string = "") as xmlNsPtr

#endif
