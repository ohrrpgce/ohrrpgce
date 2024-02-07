'OHRRPGCE GAME - Script interpreter constants
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

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
'case and subscript (16 and 17) never appear in compiled scripts
'When adding new flow control remember to update scriptstate and flowname() in scriptcmdname
'--MATH COMMANDS---
CONST mathLAST = 25
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
