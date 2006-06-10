'OHRRPGCE GAME - Script interpreter constants
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'---SCRIPT ALLOCATION CONSTANTS---
CONST scroff = 0     'position of the script in the buffer
CONST scrheap = 1    'position of the script's local vars in the buffer
CONST scrstate = 2   'what the script is doing right now
CONST scrptr = 3     'the execution pointer
CONST scrvars = 4    'variable (including arguments) count
CONST scrret = 5     'the scripts current return value
CONST curkind = 6    'kind of current statement
CONST curvalue = 7   'value of current stament
CONST curargc = 8    'number of args for current statement
CONST curargn = 9    'current arg for current statement
CONST scrdepth = 10  'stack depth of current script
CONST scrid = 11     'id number current script
CONST curwaitarg = 12'wait state argument
CONST scrsize = 13   'amount the script takes up in the buffer
CONST scrargs = 14   'number of arguments
'---INTERPRETER STATES---
'suspended scripts have negative states
CONST stnone = 0
CONST stwait = 1
CONST stread = 2
CONST streturn = 3
CONST stnext = 4
CONST stdoarg = 5
CONST stdone = 6
'--SCRIPT STATEMENT TYPES---
CONST tystop = 0    '0 terminate script
CONST tynumber = 1  '1 literal number
CONST tyflow = 2    '2 flow control
CONST tyglobal = 3  '3 global variable
CONST tylocal = 4   '4 local variable
CONST tymath = 5    '5 math function
CONST tyfunct = 6   '6 function call
CONST tyscript = 7  '7 script call
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
'--SUSPEND--
CONST suspendnpcs = 0
CONST suspendplayer = 1
CONST suspendobstruction = 2
CONST suspendherowalls = 3
CONST suspendnpcwalls = 4
CONST suspendcatapillar = 5
CONST suspendrandomenemies = 6
CONST suspendboxadvance = 7
CONST suspendoverlay = 8
CONST suspendambientmusic = 9
'--CAMERA CONSTANTS--
CONST cameramode = 45
CONST cameraArg = 46
CONST cameraArg2 = 47
CONST cameraArg3 = 48
CONST cameraArg4 = 49
CONST herocam = 0
CONST npccam = 1
CONST pancam = 2
CONST focuscam = 3
CONST stopcam = -1

