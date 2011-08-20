'OHRRPGCE GAME - HamsterSpeak interpreter interface
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

DECLARE SUB scriptinterpreter ()
DECLARE SUB scriptdump (s as string)
DECLARE SUB breakpoint (mode as integer, callspot as integer)
DECLARE SUB scriptwatcher (mode as integer, drawloop as integer)
DECLARE SUB setScriptArg (arg as integer, value as integer)
DECLARE SUB killallscripts ()
DECLARE SUB resetinterpreter ()
DECLARE SUB delete_scriptdata (byval scriptd as ScriptData ptr)
DECLARE SUB reload_scripts ()
