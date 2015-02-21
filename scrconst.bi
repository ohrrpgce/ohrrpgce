'OHRRPGCE GAME - Script interpreter constants
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#IFNDEF SCRCONST_BI
#DEFINE SCRCONST_BI

'---INTERPRETER STATES---
'suspended scripts have negative states
CONST stnone = 0
CONST stwait = 1
CONST ststart = 2
CONST streturn = 3
CONST stnext = 4
CONST stdoarg = 5
CONST stdone = 6
CONST sttriggered = 7
CONST sterror = 8
CONST stexit = 9
'--SCRIPT STATEMENT TYPES---
CONST tystop = 0      'terminate script (not really legal)
CONST tynumber = 1    'literal number
CONST tyflow = 2      'flow control
CONST tyglobal = 3    'global variable
CONST tylocal = 4     'local variable
CONST tymath = 5      'math function
CONST tyfunct = 6     'function call
CONST tyscript = 7    'script call
CONST tynonlocal = 8  'nonlocal variable
'--FLOW CONTROL TYPES---
CONST flowdo = 0
CONST flowreturn = 3
CONST flowif = 4
CONST flowthen = 5
CONST flowelse = 6
CONST flowfor = 7
CONST flowwhile = 10
CONST flowbreak = 11
CONST flowcontinue = 12
CONST flowexit = 13
CONST flowexitreturn = 14
CONST flowswitch = 15
'--SCRIPT TRIGGER TYPES--
CONST plottrigger = 1
'--SCRIPT DEBUGGER BITS--
CONST stepnext = 1
CONST stepargsdone = 2
CONST stepup = 3
CONST stependscript = 4
CONST stepscript = 8

CONST breakststart = 4
CONST breakstnext = 8
CONST breakstdone = 16
CONST breaklooptop = 32 'above interpret
CONST breakloopbrch = 64 'doesn't work

#ENDIF
