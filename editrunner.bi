'OHRRPGCE CUSTOM - Editor Editor Runner
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#IFNDEF EDITRUNNER_BI
#DEFINE EDITRUNNER_BI

'Public functions
DECLARE SUB editor_runner OVERLOAD (editor_definition_file as string)
DECLARE SUB editor_runner OVERLOAD (byval root as Reload.NodePtr)

#ENDIF
