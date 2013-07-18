'OHRRPGCE - reloadedit.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'...although this module is one of the less-crappy ones :)

#IFNDEF RELOADEDIT_BI
#DEFINE RELOADEDIT_BI

'Public functions
DECLARE SUB reload_editor OVERLOAD ()
DECLARE SUB reload_editor OVERLOAD (byref node as Reload.Nodeptr)

#ENDIF
