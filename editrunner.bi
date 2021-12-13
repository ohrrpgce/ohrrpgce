'OHRRPGCE CUSTOM - Editor Editor Runner
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF EDITRUNNER_BI
#DEFINE EDITRUNNER_BI

'Public functions
DECLARE SUB editor_runner OVERLOAD (editor_definition_file as string)
DECLARE SUB editor_runner OVERLOAD (byval root as Reload.NodePtr)

#ENDIF
