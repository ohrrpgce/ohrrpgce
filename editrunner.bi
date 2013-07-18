'OHRRPGCE - editrunner.bi
'(C) Copyright 2010 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'...although this module APOLOGISES FOR NOTHING!

#IFNDEF EDITRUNNER_BI
#DEFINE EDITRUNNER_BI

'Public functions
DECLARE SUB editor_runner OVERLOAD (editor_definition_file as string)
DECLARE SUB editor_runner OVERLOAD (byval root as Reload.NodePtr)

#ENDIF
