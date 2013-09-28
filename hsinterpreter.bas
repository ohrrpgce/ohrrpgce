'OHRRPGCE GAME - Old HamsterSpeak Interpreter
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'This file holds everything specific to the old, original HS interpreter
'Script related things which aren't here: script commands, interpret (handles wait conditions),
'scripterr, runscript, loadscript, freescripts, load/saveglobalvars, commandname, decodetrigger.
'Probably runscript, loadscript, freescripts will eventually be moved here

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "util.bi"
#include "misc.bi"
#include "udts.bi"
#include "scrconst.bi"
#include "gglobals.bi"
#include "allmodex.bi"
#include "game.bi"
#include "moresubs.bi"
#include "hsinterpreter.bi"

'local subs and functions
DECLARE SUB scriptinterpreter_loop ()
DECLARE FUNCTION interpreter_occasional_checks () as integer
DECLARE FUNCTION functiondone () as integer
DECLARE SUB substart (byref si as ScriptInst)
DECLARE SUB subdoarg (byref si as ScriptInst)
DECLARE SUB subreturn (byref si as ScriptInst)
DECLARE SUB unwindtodo (byref si as ScriptInst, byval levels as integer)
DECLARE SUB readstackcommand (byref state as ScriptInst, byref stk as Stack, byref i as integer)
DECLARE FUNCTION localvariablename (byval value as integer, byval scriptargs as integer) as string
DECLARE FUNCTION mathvariablename (byval value as integer, byval scriptargs as integer) as string
DECLARE FUNCTION scriptstate (byval targetscript as integer, byval recurse as integer = -1) as string
DECLARE SUB reloadscript (byref si as ScriptInst, byval updatestats as integer = -1)
DECLARE FUNCTION readscriptvar (byval id as integer) as integer
DECLARE SUB writescriptvar (byval id as integer, byval newval as integer)
DECLARE SUB scriptmath ()

'these variables hold information used by breakpoint to step to the desired position
DIM SHARED waitforscript as integer
DIM SHARED waitfordepth as integer
DIM SHARED stepmode as integer
DIM SHARED lastscriptnum as integer

#MACRO dumpandreturn()
 scrst.pos -= scrat(nowscript).curargn
 scriptret = 0
 scrat(nowscript).state = streturn
#ENDMACRO

SUB scriptinterpreter ()
 WITH scrat(nowscript)
  SELECT CASE .state
   CASE IS < stnone
    scripterr "illegally suspended script", serrBug
    .state = ABS(.state)
   CASE stnone
    scripterr "script " & nowscript & " became stateless", serrBug
   CASE stwait
    EXIT SUB
   CASE ELSE
    scriptinterpreter_loop
  END SELECT
 END WITH
END SUB

SUB scriptinterpreter_loop ()
DIM i as integer
DIM rsr as integer
DIM temp as integer
DIM tmpstate as integer
DIM tmpcase as integer
DIM tmpstart as integer
DIM tmpend as integer
DIM tmpstep as integer
DIM tmpnow as integer
DIM tmpvar as integer
DIM tmpkind as integer

reloadscript scrat(nowscript)
#IFDEF SCRIPTPROFILE
 scrat(nowscript).scr->entered += 1
 TIMER_START(scrat(nowscript).scr->totaltime)
#ENDIF

scrat(nowscript).started = YES
next_interpreter_check_time = TIMER + scriptCheckDelay
interruption_grace_period = YES

interpretloop:
WITH scrat(nowscript)
DO
 SELECT CASE .state
  CASE stnext'---check if all args are done
   IF scrwatch AND breakstnext THEN breakpoint scrwatch, 1
   IF .curargn >= curcmd->argc THEN
    '--pop return values of each arg
    '--evaluate function, math, script, whatever
    '--scriptret would be set here, pushed at return
    SELECT CASE curcmd->kind
     CASE tystop
      scripterr "stnext encountered noop " & curcmd->value & " at " & .ptr & " in " & nowscript, serrError
      killallscripts
      EXIT DO
     CASE tymath, tyfunct
      '--complete math and functions, nice and easy.
      FOR i as integer = curcmd->argc - 1 TO 0 STEP -1
       popstack(scrst, retvals(i))
      NEXT i
      .state = streturn
      IF curcmd->kind = tymath THEN
       scriptmath
       '.state = streturn
      ELSE
       sfunctions(curcmd->value)
       '--nowscript might be changed
       '--unless you have switched to wait mode, return
       'IF scrat(nowscript).state = stnext THEN scrat(nowscript).state = streturn'---return
       GOTO interpretloop 'new WITH pointer
      END IF
     CASE tyflow
      '--finish flow control? tricky!
      SELECT CASE curcmd->value
       CASE flowwhile'--repeat or terminate while
        SELECT CASE .curargn
         CASE 2
          '--if a while statement finishes normally (argn is 2) then it repeats.
          IF interpreter_occasional_checks THEN CONTINUE DO
          scrst.pos -= 2
          .curargn = 0
         CASE ELSE
          scripterr "while fell out of bounds, landed on " & .curargn, serrBug
          killallscripts
          EXIT DO
        END SELECT
       CASE flowfor'--repeat or terminate for
        SELECT CASE .curargn
         CASE 5
          '--normal for termination means repeat
          IF interpreter_occasional_checks THEN CONTINUE DO
          scrst.pos -= 1
          tmpvar = readstack(scrst, -3)
          writescriptvar tmpvar, readscriptvar(tmpvar) + readstack(scrst, 0)
          .curargn = 4
         CASE ELSE
          scripterr "for fell out of bounds, landed on " & .curargn, serrBug
          killallscripts
          EXIT DO
        END SELECT
       CASE flowreturn
        popstack(scrst, .ret)
        .state = streturn'---return
       CASE flowbreak
        popstack(scrst, temp)
        unwindtodo(scrat(nowscript), temp)
        '--for and while need to be broken
        IF curcmd->kind = tyflow AND (curcmd->value = flowfor OR curcmd->value = flowwhile) THEN
         dumpandreturn()
        END IF
       CASE flowcontinue
        '--continue could be used to cause an infinite loop (including in a floating do), so also needs these checks
        IF interpreter_occasional_checks THEN CONTINUE DO
        popstack(scrst, temp)
        unwindtodo(scrat(nowscript), temp)
        IF curcmd->kind = tyflow AND curcmd->value = flowswitch THEN
         '--set state to 2
         scrst.pos -= 2
         pushstack(scrst, 2)
         pushstack(scrst, 0) '-- dummy value
        ELSEIF NOT (curcmd->kind = tyflow AND (curcmd->value = flowfor OR curcmd->value = flowwhile)) THEN
         '--if this do isn't a for's or while's, then just repeat it, discarding the returned value
         scrst.pos -= 1
         .curargn -= 1
        END IF
       CASE flowexit
        killtopscript
       CASE flowexitreturn
        popstack(scrst, .ret)
        killtopscript
       CASE flowswitch
        scrst.pos -= 3
        scriptret = 0
        .state = streturn
       CASE ELSE
        '--do, then, etc... terminate normally
        dumpandreturn()
      END SELECT
      '.state = streturn'---return
     CASE tyscript
      rsr = runscript(curcmd->value, NO, NO, "indirect", 0)
      IF rsr = 1 THEN
       '--fill heap with arguments
       FOR i as integer = .curargc - 1 TO 0 STEP -1   '--be VERY careful... runscript set curargc, WITH points to nowscript-1
        popstack(scrst, temp)
        setScriptArg i, temp
       NEXT i
      END IF
      IF rsr = 0 THEN
       .state = streturn'---return
      END IF
      GOTO interpretloop 'new WITH pointer
     CASE ELSE
      scripterr "illegal kind " & curcmd->kind & " " & curcmd->value & " in stnext", 6
      killallscripts
      EXIT DO
    END SELECT
   ELSE
    IF .curargn = 0 THEN
     '--always need to execute the first argument
     .state = stdoarg
    ELSE 
     '--flow control and logical math are special, for all else, do next arg
     SELECT CASE curcmd->kind
      CASE tyflow
       SELECT CASE curcmd->value
        CASE flowif'--we got an if!
         SELECT CASE .curargn
          CASE 0
           .state = stdoarg'---call conditional
          CASE 1
           IF readstack(scrst, 0) THEN
            'scrst.pos -= 1
            .state = stdoarg'---call then block
           ELSE
            .curargn = 2
            '--if-else needs one extra thing on the stack to account for the then that didnt get used.
            pushstack(scrst, 0)
            .state = stdoarg'---call else block
           END IF
          CASE 2
           '--finished then but not at end of argument list: skip else
           dumpandreturn()
          CASE ELSE
           scripterr "if statement overstepped bounds", serrBug
         END SELECT
        CASE flowwhile'--we got a while!
         SELECT CASE .curargn
          CASE 0
           .state = stdoarg'---call condition
          CASE 1
           IF readstack(scrst, 0) THEN
            .state = stdoarg'---call do block
            '--don't pop: number of words on stack should equal argn (for simplicity when unwinding stack)
           ELSE
            '--break while
            scrst.pos -= 1
            scriptret = 0
            .state = streturn'---return
           END IF
          CASE ELSE
          scripterr "while statement has jumped the curb", serrBug
         END SELECT
        CASE flowfor'--we got a for!
         SELECT CASE .curargn
          '--argn 0 is var
          '--argn 1 is start
          '--argn 2 is end
          '--argn 3 is step
          '--argn 4 is do block
          '--argn 5 is repeat (normal termination)
          CASE 0, 1, 3
           '--get var, start, and later step
           .state = stdoarg
          CASE 2
           '--set variable to start val before getting end
           writescriptvar readstack(scrst, -1), readstack(scrst, 0)
           '---now get end value
           .state = stdoarg
          CASE 4
           IF scrwatch AND breakloopbrch THEN breakpoint scrwatch, 5
           tmpstep = readstack(scrst, 0)
           tmpend = readstack(scrst, -1)
           tmpstart = readstack(scrst, -2)
           tmpvar = readstack(scrst, -3)
           tmpnow = readscriptvar(tmpvar)
           IF (tmpnow > tmpend AND tmpstep > 0) OR (tmpnow < tmpend AND tmpstep < 0) THEN
            '--breakout
            scrst.pos -= 4
            scriptret = 0
            .state = streturn'---return
           ELSE
            .state = stdoarg'---execute the do block
           END IF
          CASE ELSE
           scripterr "for statement is being difficult", serrBug
         END SELECT
        CASE flowswitch
         IF .curargn = 0 THEN
          '--get expression to match
          .state = stdoarg
         ELSEIF .curargn = 1 THEN
          '--set up state - push a 0: not fallen in
          '--assume first statement is a case, run it
          pushstack(scrst, 0)
          .state = stdoarg
         ELSE
          popstack(scrst, tmpcase)
          popstack(scrst, tmpstate)
          DIM as integer doseek = 0 ' whether or not to search argument list for something to execute
          IF tmpstate = 0 THEN
           '--not fallen in, check tmpvar
           IF tmpcase = readstack(scrst, 0) THEN 
            tmpstate = 1
           END IF
           doseek = 1 '--search for a case or do
          ELSEIF tmpstate = 1 THEN
           '--after successfully running a do block, pop off matching value and exit
           scrst.pos -= 1
           scriptret = 0
           .state = streturn'---return
          ELSEIF tmpstate = 2 THEN
           '--continue encountered, fall back in
           tmpstate = 1
           doseek = 1 '--search for a do
          END IF

          WHILE doseek
           tmpkind = .scrdata[curcmd->args(.curargn)]

           IF (tmpstate = 1 AND tmpkind = tyflow) OR (tmpstate = 0 AND (tmpkind <> tyflow OR .curargn = curcmd->argc - 1)) THEN
            '--fall into a do, execute a case, or run default (last arg)
            .state = stdoarg
            pushstack(scrst, tmpstate)
            EXIT WHILE
           END IF
           IF .curargn >= curcmd->argc THEN
            scrst.pos -= 1
            scriptret = 0
            .state = streturn'---return
            EXIT WHILE
           END IF
           .curargn += 1
          WEND
         END IF
        CASE ELSE
         .state = stdoarg'---call argument
       END SELECT
      CASE tymath
       SELECT CASE curcmd->value
        CASE 20'--logand
         IF readstack(scrst, 0) THEN
          .state = stdoarg'---call 2nd argument
         ELSE
          '--shortcut evaluate to false
          scriptret = 0
          '--pop all args
          scrst.pos -= .curargn
          .state = streturn'---return
         END IF
        CASE 21'--logor
         IF readstack(scrst, 0) THEN
          '--shortcut evaluate to true
          scriptret = 1
          '--pop all args
          scrst.pos -= .curargn
          .state = streturn'---return
         ELSE
          .state = stdoarg'---call 2nd argument
         END IF
        CASE ELSE
         .state = stdoarg'---call argument
       END SELECT
      CASE ELSE
       .state = stdoarg'---call argument
     END SELECT
    END IF
   END IF
  CASE streturn'---return
   '--sets stdone if done with entire script, stnext otherwise
   subreturn scrat(nowscript)
  CASE stdoarg'---do argument
   '--evaluate an arg, either directly or by changing state. stnext will be next
   subdoarg scrat(nowscript)
  CASE ststart'---read statement
   '--FIRST STATE
   '--just load the first command
   substart scrat(nowscript)
  CASE stwait'---begin waiting for something
   .curkind = curcmd->kind
   .curvalue = curcmd->value
   .curargc = curcmd->argc
   EXIT DO
  CASE stdone'---script terminates
   '--if resuming a supended script, restore its state (normally stwait)
   '--if returning a value to a calling script, set streturn
   '--if no scripts left, break the loop
   SELECT CASE functiondone
    CASE 1
     EXIT DO
    CASE 2
     IF scrat(nowscript).state <> stwait THEN
'      debug "WANTIMMEDIATE BUG"
'      debug scriptname(scrat(nowscript + 1).id) & " terminated, setting wantimmediate on " & scriptname(scrat(nowscript).id)
      wantimmediate = -2
     ELSE
      wantimmediate = -1
     END IF
   END SELECT
   IF scrwatch AND breakstnext THEN breakpoint scrwatch, 2
   GOTO interpretloop 'new WITH pointer
  CASE sttriggered'---special initial state used just for script trigger logging
   IF gam.script_log.enabled THEN watched_script_triggered *last_queued_script
   .started = YES
   .state = ststart
  CASE sterror'---some error has occurred, crash and burn
   '--note that there's no thought out plan for handling errors
   killallscripts
   EXIT DO
  CASE stexit '--used only to exit this loop
   'Note: this is a bit of a hack: if we get here then nowscript has already
   'changed so we're not really meant to be reading .state
   EXIT DO
 END SELECT
LOOP
END WITH

#IFDEF SCRIPTPROFILE
 IF nowscript >= 0 THEN TIMER_STOP(scrat(nowscript).scr->totaltime)
#ENDIF

END SUB

'Returns true if current interpreter block should be aborted
FUNCTION interpreter_occasional_checks () as integer
 IF TIMER > next_interpreter_check_time THEN
  IF interrupting_keypress THEN
   IF interruption_grace_period THEN
    debuginfo "Script interpreter: ignoring interruption"
    'The call to interruptting keypress causes the new-keypress flags to be cleared, and also has the benefit
    'of processing quit requests, etc
   ELSE
    debug "Script interpreter: received interruption"
    RETURN script_interrupt()
   END IF
  END IF
  next_interpreter_check_time = TIMER + scriptCheckInterval
  interruption_grace_period = NO
 END IF
 RETURN NO
END FUNCTION

SUB killtopscript
 'Forces the topmost script to return
 'Possible to use unwindtodo instead (used to do this) but that can't be done from
 'everywhere, and is slower
 'unwindtodo(scrat(nowscript), 9999)
 WITH scrat(nowscript)
  setstackposition(scrst, .stackbase)
  .state = stdone
 END WITH
END SUB

SUB killscriptthread
 IF insideinterpreter = NO THEN fatalerror "Inappropriate killscriptthread"

 'Hack: in case this function is called from within the interpreter we set the new state of the
 'old script so that the main loop sees it using a stale WITH pointer.
 'Come to think of it, there's no good reason for the interpreter state to be stored in scrat instead
 'of being global.
 scrat(nowscript).state = stdone

 WHILE nowscript >= 0
  WITH scrat(nowscript)
   IF .state < 0 THEN EXIT WHILE
   IF .scr <> NULL THEN .scr->refcount -= 1
  END WITH
  nowscript -= 1
 WEND
 gam.script_log.last_logged = -1

 'Go back a script, let functiondone handle the script exit
 nowscript += 1
 reloadscript scrat(nowscript)  'Avoid possible null ptr deref in functiondone
 setstackposition(scrst, scrat(nowscript).stackbase)

END SUB

SUB killallscripts
'this kills all running scripts.
'for use in cases of massive errors, quiting to titlescreen or loading a game.

 'Hack, see explanation in killscriptthread
 IF nowscript >= 0 THEN scrat(nowscript).state = stexit

 FOR i as integer = nowscript TO 0 STEP -1
  IF scrat(i).scr <> NULL THEN scrat(i).scr->refcount -= 1
 NEXT
 nowscript = -1
 gam.script_log.last_logged = -1

 setstackposition(scrst, 0)

 dequeue_scripts
END SUB

SUB resetinterpreter
'unload all scripts and wipe interpreter state. use when quitting the game.

 killallscripts

 IF numloadedscr > 0 THEN freescripts(0)
END SUB

SUB setScriptArg (byval arg as integer, byval value as integer)
 IF scrat(nowscript).scr->args > arg THEN
  heap(scrat(nowscript).heap + arg) = value
 END IF
END SUB

FUNCTION functiondone () as integer
'returns 0 when returning a value to a caller
'returns 1 when all scripts are finished
'returns 2 when reactivating a suspended script

IF scrat(nowscript).watched THEN watched_script_finished

scrat(nowscript).scr->refcount -= 1
nowscript = nowscript - 1

IF nowscript < 0 THEN
 functiondone = 1'--no scripts are running anymore
#IFDEF SCRIPTPROFILE
 TIMER_STOP(scrat(nowscript + 1).scr->totaltime)
#ENDIF
ELSE
 'check if script needs reloading
 reloadscript scrat(nowscript)
 curcmd = cast(ScriptCommand ptr, scrat(nowscript).scrdata + scrat(nowscript).ptr)
 IF scrat(nowscript).state < 0 THEN
  '--suspended script is resumed
  scrat(nowscript).state = ABS(scrat(nowscript).state)
  IF scrat(nowscript).watched THEN watched_script_resumed
  functiondone = 2'--reactivating a supended script
 ELSE
  scriptret = scrat(nowscript + 1).ret
  scrat(nowscript).state = streturn'---return
  functiondone = 0'--returning a value to a caller
 END IF
#IFDEF SCRIPTPROFILE
 TIMER_STOP(scrat(nowscript + 1).scr->totaltime)
 scrat(nowscript).scr->entered += 1
 TIMER_START(scrat(nowscript).scr->totaltime)
#ENDIF
END IF

END FUNCTION

SUB substart (si as ScriptInst)
'this sets up a new script by preparing to run at the root command (which should be do)
curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
scriptret = 0'--default returnvalue is zero
'si.curargn = 0'--moved to runscript to prevent scriptstate crash
si.state = stnext
si.stackbase = stackposition(scrst)

'+5 just-in-case for extra state stuff pushed to stack (atm just switch, +1 ought to be sufficient)
checkoverflow(scrst, curcmd->argc + 5)

IF curcmd->kind <> tyflow THEN
 scripterr "Root script command not flow, but " & curcmd->kind, 6
 si.state = sterror
END IF
END SUB

SUB subdoarg (si as ScriptInst)
'read/load arguments, evaluating immediate values, in a depth-first manner, until either:
'-all args for a command have been pushed, stnext to evaluate
'-certain flow & math commands need special logic after every evaluated arg, stnext to handle
si.state = stnext

DIM as integer ptr dataptr = si.scrdata

quickrepeat:
DIM as ScriptCommand ptr cmdptr = cast(ScriptCommand ptr, dataptr + curcmd->args(si.curargn))

SELECT CASE cmdptr->kind
 CASE tynumber
  pushstack(scrst, cmdptr->value)
 CASE tyglobal
  IF cmdptr->value < 0 OR cmdptr->value > maxScriptGlobals THEN
   scripterr "Illegal global variable id " & cmdptr->value, 5
   si.state = sterror
   EXIT SUB
  END IF
  pushstack(scrst, global(cmdptr->value))
 CASE tylocal
  pushstack(scrst, heap(si.heap + cmdptr->value))
 CASE IS >= tymath, tyflow
  si.depth += 1
  '2 for state + args + 5 just-in-case for extra state stuff pushed to stack (atm just switch, +1 ought to be sufficient)
  checkoverflow(scrst, 7 + cmdptr->argc)
  pushstack(scrst, si.ptr)
  pushstack(scrst, si.curargn)
  curcmd = cmdptr
  si.ptr = (cast(integer, cmdptr) - cast(integer, dataptr)) shr 2
  si.curargn = 0
  scriptret = 0'--default returnvalue is zero

  'this breakpoint is a perfect duplicate of breakstnext, but originally it also caught
  'streturn on evaluating numbers, locals and globals
  'edit: it's moved about even more now. needs rewriting
  'IF scrwatch AND breakststart THEN breakpoint scrwatch, 3
  'scriptdump "subdoarg"


  'even for flow, first arg always needs evaluation, so don't leave yet!
  'EXIT SUB
  IF curcmd->argc = 0 THEN EXIT SUB
  GOTO quickrepeat
 CASE ELSE
  scripterr "Illegal statement type " & cmdptr->kind, 6
  si.state = sterror
  EXIT SUB
END SELECT

finishedarg:

si.curargn += 1
IF si.curargn >= curcmd->argc THEN
 IF curcmd->kind = tymath THEN
  'Optimisation
/'  Here's the prologue (from a *previous* iteration through the above SELECT)
  si.depth += 1
  pushstack(scrst, si.ptr)
  pushstack(scrst, si.curargn)
  curcmd = cmdptr
  si.ptr = (cast(integer, cmdptr) - cast(integer, dataptr)) shr 2
  si.curargn = 0
  scriptret = 0'--default returnvalue is zero
'/
  IF curcmd->argc = 2 THEN popstack(scrst, retvals(1))
  popstack(scrst, retvals(0))
  scriptmath
  si.depth -= 1
  popstack(scrst, si.curargn)
  popstack(scrst, si.ptr)
  curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
  '--push return value
  pushstack(scrst, scriptret)
  GOTO finishedarg
 END IF
 EXIT SUB
END IF
IF curcmd->kind = tyflow THEN IF curcmd->value = flowif OR curcmd->value >= flowfor THEN EXIT SUB
IF curcmd->kind = tymath THEN IF curcmd->value >= 20 THEN EXIT SUB
GOTO quickrepeat
END SUB

SUB subreturn (si as ScriptInst)
si.depth -= 1
IF si.depth < 0 THEN
 si.state = stdone
ELSE
 popstack(scrst, si.curargn)
 popstack(scrst, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
 '--push return value
 pushstack(scrst, scriptret)
 si.curargn += 1
 si.state = stnext'---try next arg
 IF si.curargn >= curcmd->argc THEN EXIT SUB
 IF curcmd->kind = tyflow THEN IF curcmd->value = flowif OR curcmd->value >= flowfor THEN EXIT SUB
 IF curcmd->kind = tymath THEN IF curcmd->value >= 20 THEN EXIT SUB
 si.state = stdoarg
END IF
END SUB

SUB unwindtodo (byref si as ScriptInst, byval levels as integer)
'unwinds the stack until the specified number of dos have been stripped
'leaves the interpreter as if the last do block had successfully finished
'this means repeat in the case of for and while loops
'note: we assume the calling command has popped its args

WHILE levels > 0
 si.depth -= 1
 IF si.depth < 0 THEN
  si.state = stdone
  EXIT SUB
 END IF

 popstack(scrst, si.curargn)
 popstack(scrst, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)

 IF curcmd->kind = tyflow AND curcmd->value = flowdo THEN
  levels -= 1
  'first pop do's evaluated arguments before stopping
 END IF

 'pop arguments
 IF curcmd->kind = tyflow AND curcmd->value = flowswitch THEN
  'unlike all other flow, switch stack usage != argn
  scrst.pos -= 2 'state, matching value
 ELSE
  scrst.pos -= si.curargn
 END IF
WEND
'return to normality
subreturn si

END SUB

FUNCTION readscriptvar (byval id as integer) as integer
 SELECT CASE id
  CASE IS < 0 'local variable
   readscriptvar = heap(scrat(nowscript).heap - id - 1)
  CASE 0 TO maxScriptGlobals 'global variable
   readscriptvar = global(id)
  CASE ELSE
   scripterr "Cannot read global " & id & ". Out of range", 5
 END SELECT
END FUNCTION

SUB writescriptvar (byval id as integer, byval newval as integer)
 SELECT CASE id
  CASE IS < 0 'local variable
   heap(scrat(nowscript).heap - id - 1) = newval
  CASE 0 TO maxScriptGlobals 'global variable
   global(id) = newval
  CASE ELSE
   scripterr "Cannot write global " & id &  ". Out of range", 5
 END SELECT
END SUB

SUB scriptmath
 SELECT CASE as CONST curcmd->value
  CASE 0' random
   scriptret = INT(retvals(0) + RND * (1.0 + retvals(1) - retvals(0))) 'handles the case max-min = 2^32
  CASE 1' exponent
   IF retvals(0) = 0 and retvals(1) < 0 THEN
    scripterr "Tried to take negative power of zero, 0^" & retvals(1), 5
   ELSE
    scriptret = retvals(0) ^ retvals(1)
   END IF
  CASE 2' modulus
   IF retvals(1) = 0 THEN
    scripterr "division by zero: " & retvals(0) & ",mod,0", 5
   ELSE
    scriptret = retvals(0) MOD retvals(1)
   END IF
  CASE 3' divide
   IF retvals(1) = 0 THEN
    scripterr "division by zero: " & retvals(0) & "/0", 5
   ELSE
    scriptret = retvals(0) \ retvals(1)
   END IF
  CASE 4'multiply
   scriptret = retvals(0) * retvals(1)
  CASE 5'subtract
   scriptret = retvals(0) - retvals(1)
  CASE 6'add
   scriptret = retvals(0) + retvals(1)
  CASE 7'xor
   scriptret = retvals(0) XOR retvals(1)
  CASE 8'or
   scriptret = retvals(0) OR retvals(1)
  CASE 9'and
   scriptret = retvals(0) AND retvals(1)
  CASE 10'equal
   scriptret = (retvals(0) = retvals(1)) * -1
  CASE 11'not equal
   scriptret = (retvals(0) <> retvals(1)) * -1
  CASE 12'less than
   scriptret = (retvals(0) < retvals(1)) * -1
  CASE 13'greater than
   scriptret = (retvals(0) > retvals(1)) * -1
  CASE 14'less than or equal to
   scriptret = (retvals(0) <= retvals(1)) * -1
  CASE 15'greater than or equal to
   scriptret = (retvals(0) >= retvals(1)) * -1
  CASE 16'set variable
   writescriptvar retvals(0), retvals(1)
   scriptret = retvals(1)
  CASE 17'increment
   writescriptvar retvals(0), readscriptvar(retvals(0)) + retvals(1)
  CASE 18'decrement
   writescriptvar retvals(0), readscriptvar(retvals(0)) - retvals(1)
  CASE 19'not
   IF retvals(0) = 0 THEN
    scriptret = 1
   ELSE
    scriptret = 0
   END IF
  CASE 20'&&
   '&& and || are shortcut evaluated, so retvals(0) has already been checked
   IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
  CASE 21'||
   IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
  CASE 22'^^
   IF retvals(0) <> 0 XOR retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
  CASE 23'abs
   scriptret = ABS(retvals(0))
  CASE 24'sign
   scriptret = SGN(retvals(0))
  CASE 25'sqrt
   IF retvals(0) < 0 THEN
    scripterr "Tried to take squareroot of " & retvals(0), 5
   ELSE
    scriptret = SQRT(retvals(0))
   END IF
  CASE ELSE
   scripterr "unsupported math function id " & curcmd->value, 6
 END SELECT
END SUB

SUB reloadscript (byref si as ScriptInst, byval updatestats as integer)
 IF si.scr = NULL THEN
  si.scr = loadscript(si.id)
  IF si.scr = NULL THEN killallscripts: EXIT SUB
  si.scrdata = si.scr->ptr
  si.scr->refcount += 1
  IF updatestats THEN si.scr->totaluse += 1
 END IF
 IF updatestats THEN
  'a rather hackish and not very good attempt to give .lastuse a qualitative use
  'instead of just for sorting; a priority queue is probably a much better solution
  IF si.scr->lastuse <= scriptctr - 10 THEN
   scriptctr += 1
   si.scr->lastuse = scriptctr
  END IF
 END IF
END SUB

SUB scriptdump (s as string)

DIM statestr(7) as string
statestr(0) = "none"
statestr(1) = "wait"
statestr(2) = "read"
statestr(3) = "return"
statestr(4) = "next"
statestr(5) = "doarg"
statestr(6) = "done"
statestr(7) = "error"

DIM indent as string
IF scrat(nowscript).depth >= 0 THEN
  indent = STRING(scrat(nowscript).depth, " ")
ELSE
  indent = STRING(ABS(scrat(nowscript).depth), "<")
END IF

DIM state as string
SELECT CASE scrat(nowscript).state
 CASE 0 TO 7
   state = statestr(scrat(nowscript).state)
 CASE ELSE
   state = "illegal: " & scrat(nowscript).state
END SELECT

debug indent & "[" & s & "]"
debug indent & "script = " & nowscript
debug indent & "id     = " & scrat(nowscript).id & " " & scriptname(scrat(nowscript).id)
debug indent & "ptr    = " & scrat(nowscript).ptr
debug indent & "state  = " & state
debug indent & "kind   = " & curcmd->kind
debug indent & "value  = " & curcmd->value
debug indent & "argn   = " & scrat(nowscript).curargn
debug indent & "argc   = " & curcmd->argc

END SUB

'This function is called at possible breakpoints and decides whether to break into the debugger
'It's rather hard to understand.
SUB breakpoint (byref mode as integer, byval callspot as integer)
' callspot = 1  stnext
' callspot = 2  stdone
' callspot = 3  ststart
' callspot = 4  at top of main loop, after loading onkeypress

DIM argn as integer

IF stepmode = 0 THEN GOTO breakin
IF nowscript = -1 AND stepmode <> stepscript THEN
 stepmode = 0
 waitforscript = 999
 waitfordepth = 999
 EXIT SUB
END IF

'STEPPING LOGIC
'some generic logic for going up scripts/commands
IF waitforscript <> 999 THEN
 IF nowscript > waitforscript THEN 
  EXIT SUB
 ELSEIF nowscript < waitforscript THEN 
  waitforscript = 999
  waitfordepth = 999
 ELSE
  'if final objective is a script, not a depth, stop
  IF waitfordepth = 999 THEN waitforscript = 999
 END IF

 IF scrat(nowscript).depth > waitfordepth THEN
  EXIT SUB
 ELSE
  waitforscript = 999
  waitfordepth = 999
 END IF
END IF

argn = scrat(nowscript).curargn
SELECT CASE stepmode
 CASE stependscript
  GOTO breakin
 CASE stepscript
'  IF callspot = 1 THEN IF curcmd->kind = tyscript THEN GOTO breakin
'  IF callspot = 2 THEN GOTO breakin
  IF lastscriptnum <> nowscript THEN GOTO breakin
 CASE stepargsdone, stepup, stepnext
  IF callspot = 1 THEN
   'IF scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0 THEN EXIT SUB
   'IF  scrat(nowscript).curargn = 0 THEN
   ' debug "skipped b " & curcmd->argc & " flow " &  curcmd->kind
   ' EXIT SUB
   'end if
   IF curcmd->kind = tyflow THEN
    IF curcmd->value = flowif AND argn <> 1 THEN EXIT SUB
    IF curcmd->value = flowfor AND argn <> 4 THEN EXIT SUB
    IF curcmd->value = flowwhile AND argn <> 1 THEN EXIT SUB
   END IF
  END IF
  GOTO breakin
END SELECT

'IF callspot = 1 THEN 'stnext
'' IF (mode AND 4) AND curcmd->kind <> tyscript THEN EXIT SUB
' 'only used to print off evaluated list of arguments
'' IF ((mode AND breakreadcmd) <> 0) AND (scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0) THEN EXIT SUB
' IF scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0 THEN EXIT SUB
'' IF (mode AND breakargsdone) THEN EXIT SUB
' IF curcmd->kind = tyflow AND curcmd->value = flowif THEN EXIT SUB
'END IF

'END STEPPING LOGIC

EXIT SUB

breakin:

'clear breakpoint bits
mode = mode AND 3
stepmode = 0
scriptwatcher mode, 0

END SUB

'The following function is an atrocious mess. Don't worry too much; it'll be totally replaced.
SUB scriptwatcher (byref mode as integer, byval drawloop as integer)
STATIC localsscroll as integer
STATIC globalsscroll as integer
STATIC stringsscroll as integer
STATIC timersscroll as integer
STATIC selectedscript as integer
STATIC bottom as integer
STATIC viewmode as integer
STATIC lastscript as integer
'viewmode: 0 = script state, 1 = local variables, 2 = global variables, 3 = strings, 4 = timers
'mode: 0 = do nothing, 1 = non-interactive (display over game), 2 >= clean and sane
'2 = interactive (display game and step on input), 3 = clean and sane

DIM plots as string
DIM marginstr as string
DIM strings() as string
DIM linelen as integer
DIM page as integer

DIM resetpal as integer = NO  'need setpal master()

IF mode >= 2 THEN
 'In case of a fade out
 REDIM default_palette(255) as RGBcolor
 loadpalette default_palette(), gam.current_master_palette
 setpal default_palette()
 resetpal = YES
END IF

FOR i as integer = 0 TO UBOUND(plotstr)
 plots = plotstr(i).s
 marginstr = LEFT(i & ": ", 3)
 linelen = 0
 FOR j as integer = 0 TO LEN(plots) - 1
  linelen += 1
  IF (linelen = 37 OR plots[j] = ASC(!"\n")) AND j <> LEN(plots) - 1 THEN 
   'notice intentional waste of last string
   REDIM PRESERVE strings(UBOUND(strings) + 1)
   strings(UBOUND(strings) - 1) = marginstr + MID(plots, (j + 2) - linelen, linelen)
   marginstr = "   "
   linelen = 0
  END IF
 NEXT
 REDIM PRESERVE strings(UBOUND(strings) + 1)
 strings(UBOUND(strings) - 1) = marginstr + MID(plots, LEN(plots) + 1 - linelen, linelen)
NEXT
stringsscroll = small(stringsscroll, (UBOUND(strings) - 1) - 19) 'recall one string wasted

IF nowscript >= 0 THEN
 WITH scrat(nowscript)
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH
END IF

'debug "watch mode=" & mode & " callspot = " & callspot & " stepmode = " & stepmode _
'      & " curscript = " & nowscript & " curdepth = " & scrat(nowscript).depth & " waitscr = " & waitforscript & " waitdepth = " & waitfordepth

'initialise state
IF mode = 1 THEN waitforscript = 999: waitfordepth = 999: stepmode = 0

redraw:
'if in stepping mode, make a copy so debug info can be redrawn, need to keep dpage clean 
'if called from displayall, need to keep a clean copy of nearly-drawn page to be used next tick 
IF drawloop AND mode = 1 THEN
 page = dpage
ELSE
 page = vpage
 IF mode = 2 THEN
  clearpage page
 ELSE
  copypage dpage, page
 END IF
END IF

'edgeprint callmode & " " & viewmode & " " & callspot, 140, 4, uilook(uiText), page

selectedscript = bound(selectedscript, 0, nowscript)
IF selectedscript = lastscript THEN selectedscript = nowscript
lastscript = nowscript

DIM hasargs as integer
IF nowscript >= 0 THEN 
 SELECT CASE scrat(nowscript).curkind
  CASE tynumber, tyglobal, tylocal
   hasargs = 0
  CASE ELSE
   hasargs = 1
 END SELECT
END IF

'Note: the colours here are fairly arbitrary
rectangle 0, 0, 320, 4, uilook(uiBackground), page
rectangle 0, 0, (320 / scriptmemMax) * totalscrmem, 2, uilook(uiSelectedItem), page
rectangle 0, 2, (320 / 2048) * scrat(nowscript + 1).heap, 2, uilook(uiSelectedItem + 1), page

DIM ol as integer = 191

IF mode > 1 AND viewmode = 0 THEN
 IF nowscript = -1 THEN
  edgeprint "Extended script debug mode: no scripts", 0, ol, uilook(uiDescription), page
  ol -= 9 
 ELSE
  DIM decmpl as string = scriptstate(selectedscript)
  IF LEN(decmpl) > 200 THEN decmpl = "..." & RIGHT(decmpl, 197)
  FOR i as integer = 0 TO 4
   edgeprint MID(decmpl, i * 40 + 1, 40), 0, ol - (4 - i) * 9, uilook(uiDescription), page
  NEXT
'  FOR i as integer = 5 TO 0 STEP -1
'   IF LEN(decmpl) > i * 40 THEN
'    edgeprint MID(decmpl, i * 40 + 1), 0, ol, uilook(uiDescription), page
'    ol -= 9
'   END IF
'  NEXT
 ' edgeprint "Last return value: " & scriptret, 0, ol, uilook(uiDescription), page
 ' ol -= 9
 END IF
END IF

DIM scriptargs as integer
DIM localno as integer
IF mode > 1 AND viewmode = 1 AND selectedscript >= 0 THEN
 'local variables and return value. Show up to 9 variables at a time
 reloadscript scrat(selectedscript), 0
 WITH scrat(selectedscript)
  IF .scr->vars = 0 THEN
   edgeprint "Has no variables", 0, ol, uilook(uiText), page
   ol -= 9
  ELSE
   scriptargs = .scr->args
   DIM temp as string
   FOR i as integer = small((.scr->vars - localsscroll - 1) \ 3, 2) TO 0 STEP -1
    FOR j as integer = 2 TO 0 STEP -1  'reverse order so the var name is what gets overwritten
     localno = localsscroll + i * 3 + j
     IF localno < .scr->vars THEN
      temp = localvariablename(localno, scriptargs) & "="
      edgeprint temp, j * 96, ol, uilook(uiText), page
      edgeprint STR(heap(.heap + localno)), j * 96 + 8 * LEN(temp), ol, uilook(uiDescription), page
     END IF
    NEXT
    ol -= 9
   NEXT
   IF scriptargs = 999 THEN
    edgeprint .scr->vars & " local variables and arguments:", 0, ol, uilook(uiText), page
   ELSE
    edgeprint scriptargs & " arguments and " & (.scr->vars - scriptargs) & " local variables:", 0, ol, uilook(uiText), page
   END IF
   ol -= 9
  END IF
  edgeprint "Return value = ", 0, ol, uilook(uiText), page
  edgeprint STR(.ret), 15 * 8, ol, uilook(uiDescription), page
  ol -= 9
 END WITH
END IF

DIM globalno as integer
IF mode > 1 AND viewmode = 2 THEN
 'display 60 global variables at a time
 FOR i as integer = 19 TO 0 STEP -1
  FOR j as integer = 2 TO 0 STEP -1   'reverse order so the var name is what gets overwritten
   globalno = globalsscroll + i * 3 + j
   edgeprint globalno & "=", j * 96, ol, uilook(uiText), page
   edgeprint STR(global(globalno)), j * 96 + 8 * LEN(globalno & "="), ol, uilook(uiDescription), page
  NEXT
  ol -= 9
 NEXT
 edgeprint "Global variables:", 0, ol, uilook(uiText), page
 ol -= 9
END IF

IF mode > 1 AND viewmode = 3 THEN
 'display strings, 20 lines at a time
 FOR i as integer = 19 TO 0 STEP -1
  IF MID(strings(i + stringsscroll), 1, 1) <> " " THEN
   'string number text
   edgeprint MID(strings(i + stringsscroll), 1, 3), 0, ol, uilook(uiText), page   
  END IF
  textcolor uilook(uiText), uilook(uiDescription)
  printstr MID(strings(i + stringsscroll), 4), 3*8, ol, page
  ol -= 9
 NEXT
 edgeprint "Plotstrings:", 0, ol, uilook(uiText), page
 ol -= 9
END IF

IF mode > 1 AND viewmode = 4 THEN
 'display timers, 19 lines at a time
 edgeprint "ID Count Speed Flags Str Trigger", 0, ol, uilook(uiText), page
 ol -= 9
 FOR i as integer = small(UBOUND(timers), 18) TO 0 STEP -1
  DIM id as integer = i + timersscroll
  DIM as string text, flags
  WITH timers(id)
   text = LEFT(id & "   ", 3)
   IF .speed < 0 THEN
    IF .finished_tick = gam.script_log.tick THEN
     text &= "Trigg "
    ELSE
     text &= "Done  "
    END IF
   ELSE
    text &= LEFT(.count & "      ", 6)
   END IF
   text &= LEFT(ABS(.speed) & "      ", 6)  'negated if not running
   IF .pause THEN
    flags &= "P "
   END IF
   flags &= .flags
   text &= LEFT(flags & "      ", 6)
   text &= LEFT(.st & "    ", 4)
   IF .trigger = -2 THEN
    text &= "Game Over"
   ELSEIF .trigger >= 0 THEN
    text &= scriptname(.trigger)
   END IF
  END WITH
  textcolor uilook(uiText), 0
  printstr text, 0, ol, page
  ol -= 9
 NEXT
 edgeprint "Timers:", 0, ol, uilook(uiText), page
 ol -= 9
END IF

'IF mode > 1 THEN
' edgeprint "argc=" & scrat(selectedscript).curargc & " argn=" & scrat(selectedscript).curargn & " ptr=" & scrat(selectedscript).ptr, 0, ol, uilook(uiDescription), page
' ol -= 9
'END IF

DIM lastarg as integer
DIM col as integer
DIM waitcause as string

IF mode > 1 AND (viewmode = 0 OR viewmode = 1) THEN
 'show scripts list

 '6 rows up
 ol = 200 - 6 * 9

 edgeprint "# Name           Depth State CmdKn CmdID", 0, ol, uilook(uiText), page
 ol -= 9
 
 IF mode = 1 THEN
  bottom = nowscript - (ol - 6) \ 9
  selectedscript = nowscript
 ELSE
  bottom = small(bottom, selectedscript)
  bottom = large(bottom, selectedscript - (ol - 6) \ 9)
 END IF
 
 FOR i as integer = large(bottom, 0) TO nowscript
  'if script is about to be executed, don't show it as having been already
  IF scrat(i).curargn >= scrat(i).curargc AND i <> nowscript THEN lastarg = -1 ELSE lastarg = 0
 
  IF mode > 1 AND i = selectedscript THEN col = uilook(uiSelectedItem) ELSE col = uilook(uiText)
  edgeprint STR(i), 0, ol, col, page
  edgeprint LEFT(scriptname(scrat(i).id), 17), 16, ol, col, page
  edgeprint STR(scrat(i).depth), 160, ol, col, page
  IF scrat(i).state < 0 THEN
   IF scrat(i).started = NO THEN
    edgeprint "Queued (not started)", 184, ol, col, page
   ELSE
    edgeprint "Suspended", 184, ol, col, page
   END IF
  ELSEIF scrat(i).state = stwait THEN
   waitcause = commandname(scrat(i).curvalue)
   SELECT CASE scrat(i).curvalue
    CASE 1, 3, 4, 9, 244'--wait, wait for hero, wait for NPC, wait for key, wait for scancode
     waitcause += "(" & scrat(i).waitarg & ")"
   END SELECT
   edgeprint waitcause, 184, ol, col, page
  ELSEIF scrat(i).state = stnext AND scrat(i).curkind = tyscript AND lastarg THEN
   edgeprint "Called #" & i + 1, 184, ol, col, page
  ELSEIF scrat(i).state = stnext AND scrat(i).curkind = tyfunct AND scrat(i).curvalue = 176 AND lastarg THEN
   edgeprint "Called #" & i + 1 & " by ID", 184, ol, col, page
  ELSE
   edgeprint STR(scrat(i).state), 184, ol, col, page
   edgeprint STR(scrat(i).curkind), 232, ol, col, page
   edgeprint STR(scrat(i).curvalue), 280, ol, col, page
  END IF
  ol = ol - 9
  IF ol < 6 THEN EXIT FOR
 NEXT i

END IF 'end drawing scripts list

IF mode > 1 AND drawloop = 0 THEN
 setvispage page
 DIM w as integer = waitforanykey
 IF w = scEsc OR w = scF10 THEN
  mode = 0
  clearkey(scF10)
  clearkey(scEsc)
  clearpage page
  setvispage page
 END IF
 IF w = scV THEN viewmode = loopvar(viewmode, 0, 4, 1): GOTO redraw
 IF w = scPageUp THEN
  selectedscript += 1
  localsscroll = 0
  GOTO redraw
 END IF
 IF w = scPageDown THEN
  selectedscript -= 1
  localsscroll = 0
  GOTO redraw
 END IF
 IF w = scMinus OR w = scNumpadMinus THEN
  IF viewmode = 1 THEN localsscroll = large(0, localsscroll - 3): GOTO redraw
  IF viewmode = 2 THEN globalsscroll = large(0, globalsscroll - 21): GOTO redraw
  IF viewmode = 3 THEN stringsscroll = large(0, stringsscroll - 1): GOTO redraw
  IF viewmode = 4 THEN timersscroll = large(0, timersscroll - 4): GOTO redraw
 END IF
 IF w = scPlus OR w = scNumpadPlus THEN
  IF viewmode = 1 THEN localsscroll = small(large(scrat(selectedscript).scr->vars - 8, 0), localsscroll + 3): GOTO redraw
  IF viewmode = 2 THEN globalsscroll = small(maxScriptGlobals - 59, globalsscroll + 21): GOTO redraw
  IF viewmode = 3 THEN stringsscroll = small(stringsscroll + 1, (UBOUND(strings) - 1) - 19): GOTO redraw
  IF viewmode = 4 THEN timersscroll = small(timersscroll + 4, UBOUND(timers) - 18): GOTO redraw
 END IF

 IF w = scF1 THEN
  show_help("game_script_debugger")
  GOTO redraw
 END IF

 IF w = scP THEN 'frame stepping mode
  mode = iif(mode = 2, 3, 2)
  GOTO redraw
 END IF

 'stepping
 IF w = scN THEN
  'step till next script
  mode or= breakstnext OR breakstdone OR breaklooptop
  stepmode = stepscript
  lastscriptnum = nowscript
 END IF
 IF w = scU THEN
  mode or= breakstnext
  stepmode = stepup
  waitfordepth = scrat(nowscript).depth - 1
  waitforscript = nowscript
 END IF
 IF w = scW THEN
  mode or= breakstnext
  waitforscript = selectedscript
  stepmode = stependscript
 END IF
 IF w = scS THEN
  mode or= breakststart OR breakstnext OR breakloopbrch
  stepmode = stepnext
  waitforscript = 999
 END IF
 IF w = scF THEN
  'mode or= breakststart
  mode or= breakstnext OR breakloopbrch
  stepmode = stepargsdone
  waitforscript = nowscript
  waitfordepth = scrat(selectedscript).depth
  'it would be more useful to wait for the calling command to finish
  IF hasargs = 0 THEN waitfordepth -= 1
 END IF
END IF

IF drawloop AND mode > 1 THEN
 'displayall: dpage was copied to vpage
 SWAP dpage, vpage
END IF

'in sane mode, stray keypresses are not passed through
'(the mode = 2 thrown in to prevent an infinite loop, no idea how or why)
IF drawloop = 0 AND mode = 2 THEN GOTO redraw

'just incase was swapped out above
IF nowscript >= 0 THEN
 reloadscript scrat(nowscript), 0
END IF

IF resetpal THEN setpal master()

next_interpreter_check_time = TIMER + scriptCheckDelay
interruption_grace_period = YES

END SUB

SUB readstackcommand (byref state as ScriptInst, byref stk as Stack, byref i as integer)
 state.curargn = readstack(stk, i)
 state.ptr = readstack(stk, i - 1)
 DIM cmdptr as ScriptCommand ptr = cast(ScriptCommand ptr, state.scrdata + state.ptr)
 state.curkind = cmdptr->kind
 state.curvalue = cmdptr->value
 state.curargc = cmdptr->argc
 i -= 2
END SUB

FUNCTION localvariablename (byval value as integer, byval scriptargs as integer) as string
 'get a variable name from a local variable number
 'locals (and args) numbered from 0
 IF scriptargs = 999 THEN
  'old HS file
  RETURN "local" & value
 ELSEIF value < scriptargs THEN
  RETURN "arg" & value
 ELSE
  RETURN "var" & (value - scriptargs)
 END IF
END FUNCTION

FUNCTION mathvariablename (byval value as integer, byval scriptargs as integer) as string
 'get a variable name from an variable id number passed to a math function or for
 'locals (and args) numbered from 0
 IF value >= 0 THEN
  mathvariablename = "global" & value
 ELSEIF scriptargs = 999 THEN
  'old HS file
  mathvariablename = "local" & (-value - 1)
 ELSEIF -value <= scriptargs THEN
  mathvariablename = "arg" & (-value - 1)
 ELSE
  mathvariablename = "var" & (-value - scriptargs - 1)
 END IF
END FUNCTION

'Warning: a nightmare function approaches!
FUNCTION scriptstate (byval targetscript as integer, byval recurse as integer = -1) as string
 IF nowscript <= -1 THEN EXIT FUNCTION

 IF recurse = -1 THEN
  IF targetscript = -1 THEN
   recurse = 2
  ELSE
   recurse = 3
  END IF
 END IF
 'recurse 0 = only top script
 'recurse 1 = top script plus calling scripts
 'recurse 2 = all scripts, including suspended ones
 'recurse 3 = only the specified script

 DIM flowname(15) as string
 DIM flowtype(15) as integer
 DIM flowbrakbrk(15) as integer
 DIM state as ScriptInst
 DIM laststate as ScriptInst

 flowtype(0) = 0:	flowname(0) = "do"
 flowtype(3) = 1:	flowname(3) = "return"
 flowtype(4) = 3:	flowname(4) = "if":		flowbrakbrk(4) = 1
 flowtype(5) = 0:	flowname(5) = "then"
 flowtype(6) = 0:	flowname(6) = "else"
 flowtype(7) = 2:	flowname(7) = "for":		flowbrakbrk(7) = 4
 flowtype(10) = 2:	flowname(10) = "while":		flowbrakbrk(10) = 1
 flowtype(11) = 1:	flowname(11) = "break"
 flowtype(12) = 1:	flowname(12) = "continue"
 flowtype(13) = 1:	flowname(13) = "exit"
 flowtype(14) = 1:	flowname(14) = "exitreturn"
 flowtype(15) = 3:	flowname(15) = "switch"

 DIM mathname(22) as string = {_
         "random", "exponent", "mod", "divide", "multiply", "subtract"_
         ,"add", "xor", "or", "and", "equal", "!equal", "<<", ">>"_
         ,"<=", ">=", "setvar", "inc", "dec", "not", "&&", "||", "^^"_
 }

 DIM stkbottom as integer = -(scrst.pos - scrst.bottom)  'pointer arithmetic seems to be 31-bit signed (breakage on negative diff)!
 DIM stkpos as integer = 0

 DIM wasscript as integer = nowscript
 DIM hideoverride as integer

 DIM cmd as string
 DIM hidearg as integer
 DIM outstr as string
 DIM argnum as integer

 'macro disabled for fb 0.15 compat
 'copyobj(state, scrat(wasscript))
 memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
 state.curkind = curcmd->kind
 state.curvalue = curcmd->value
 state.curargc = curcmd->argc
 memcpy(@(laststate),@(state),LEN(state))

 'so we can grab extra data on the current script
 reloadscript scrat(nowscript), 0

 'debug "state = " & state.state
 'debug "depth = " & state.depth
 'debug "kind = " & state.curkind
 'debug "val = " & state.curvalue
 'debug "argn = " & state.curargn
 'debug "argc = " & state.curargc

 IF state.state = stdoarg THEN GOTO jmpdoarg
 IF state.state = stnext OR state.state = streturn OR state.state = stwait THEN
 'IF recurse <> 3 THEN  'huh?

   IF state.state = stnext THEN 
    'point stkpos before the first argument (they extend above the stack
    stkpos -= state.curargn
   END IF

   'DIM dstr as string = ""
   'FOR i as integer = stkbottom + 1 TO stkpos
   ' dstr = dstr & " " & readstack(scrst,i)
   'NEXT
   'debug "stack contents = " + dstr
   'dstr = ""
   'FOR i as integer = stkpos + 1 TO stkpos + state.curargn
   ' dstr = dstr & " " & readstack(scrst,i)
   'NEXT
   'debug "above stack args = " + dstr


 'END IF
  IF state.curargn = 0 THEN hideoverride = -1
  GOTO jmpnext
 END IF

' FOR i as integer = stkbottom + 1 TO 0
'  dstr = dstr & " " & readstack(scrst,i)
' NEXT
' debug "stack contents = " + dstr

 DO
  jmpreturn:
  jmpwait:
  jmpnext:
  jmpread:

  cmd = ""
  hidearg = 0
  IF hideoverride THEN hidearg = -1: hideoverride = 0
  SELECT CASE state.curkind
    CASE tynumber
     outstr = STR(state.curvalue)
     hidearg = -1
    CASE tyflow
     cmd = flowname(state.curvalue)
     hidearg = -3
     IF state.depth = 0 THEN cmd = scriptname(state.id)
     IF state.state = ststart THEN hidearg = -1

     IF flowtype(state.curvalue) = 0 THEN IF state.curargc = 0 THEN hidearg = -1: cmd += "()"
     IF flowtype(state.curvalue) = 1 THEN hidearg = 0 ': IF state.curargn = 0 THEN cmd += ":"
     IF flowtype(state.curvalue) = 2 THEN
      hidearg = 0
      'IF state.curargn = state.curargc - 1 THEN hidearg = -1: cmd += "()"
      IF state.curvalue = flowwhile AND state.curargn = 0 THEN hidearg = -1
     END IF
     IF state.curvalue = flowif THEN
      hidearg = -1
      IF state.curargn > 0 AND state.curargn < state.curargc THEN cmd += "()"
     END IF
     IF state.curvalue = flowswitch THEN
      hidearg = -1
      IF state.curargn = 0 THEN
       cmd += ":"
      ELSE
       cmd += "(" & readstack(scrst, stkpos + 1) & ")"   ' ????
       IF state.curargn + 1 = state.curargc THEN
        cmd += " else"
        'hack to replace the 'do' with 'else' (hspeak outputs a do instead of an else)
        IF LEN(outstr) > 1 THEN outstr = MID(outstr, 3)
        hidearg = -2
       ELSEIF state.curargn >= state.curargc THEN
        'an extra step the stepper currently pauses on
       ELSEIF laststate.curkind = tyflow AND laststate.curvalue = flowdo THEN
        cmd += " case()"
       ELSE
        cmd += " case"
        IF state.curargn < state.curargc THEN cmd += ":" ELSE cmd += "()"
       END IF
      END IF
     END IF
    CASE tyglobal
     outstr = "global" & state.curvalue
     hidearg = -1
    CASE tylocal
     'locals can only appear in the topmost script, which we made sure is loaded
     outstr = localvariablename(state.curvalue, state.scr->args)
     hidearg = -1
    CASE tymath
     cmd = mathname(state.curvalue)
    CASE tyfunct
     cmd = commandname(state.curvalue)
    CASE tyscript
     'IF recurse < 3 AND state.curargn >= state.curargc THEN
      'currently executing this script (must have already printed it out)
      'cmd = "==>>"
     'ELSE
      cmd = scriptname(state.curvalue)
     'END IF
   END SELECT
   'debug "kind = " + STR(state.curkind)
   'debug "cmd = " + cmd



   IF cmd <> "" THEN
'    IF outstr = "" THEN
'     outstr = cmd
'    ELSE

     IF hidearg = 0 THEN
      argnum = state.curargc
      IF state.curkind = tyflow ANDALSO flowbrakbrk(state.curvalue) <> 0 THEN
       argnum = flowbrakbrk(state.curvalue)
      END IF
      cmd += "("
      FOR i as integer = 1 TO state.curargn
       IF i = 1 ANDALSO ((state.curkind = tymath AND state.curvalue >= 16 AND state.curvalue <= 18) _
                         ORELSE (state.curkind = tyflow AND state.curvalue = flowfor)) THEN
        cmd += mathvariablename(readstack(scrst, stkpos + i), state.scr->args)
       ELSE
        cmd += STR(readstack(scrst, stkpos + i))
       END IF
       IF i <> argnum THEN cmd += ","
      NEXT
      IF state.curargn >= argnum THEN cmd += ")"
      outstr = cmd & outstr
     ELSEIF hidearg = -3 THEN
      IF state.curargn >= state.curargc THEN
       outstr = cmd & "() " & outstr
      ELSEIF (state.curargc = 1) AND (state.curargn = 0) THEN
       outstr = cmd & ": " & outstr
      ELSE
       outstr = cmd & ":" & (state.curargn + 1) & "/" & state.curargc & " " & outstr
      END IF
     ELSEIF hidearg = -2 THEN
      outstr = cmd & outstr
     ELSE
      outstr = cmd & " " & outstr
     END IF
'    END IF
   END IF


   'don't check this because script might be queued up due to timers and triggers but not run, consuming 0 stack
   'IF stkpos <= stkbottom THEN EXIT DO

   state.depth -= 1

   IF state.depth < 0 THEN
    IF recurse = 0 THEN EXIT DO
    'load next script
    wasscript -= 1
    IF wasscript = targetscript THEN outstr = ""
    IF wasscript < targetscript THEN IF recurse <> 2 THEN EXIT DO
    IF wasscript < 0 THEN EXIT DO
    'macro disabled for fb 0.15 compat
    'copyobj(state, scrat(wasscript))
    memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
    reloadscript state, 0
    IF scrat(wasscript).state < 0 THEN
     IF recurse = 2 OR recurse = 3 THEN
      'deal with state   (can only be wait? - goto streturn)
      CONTINUE DO
     ELSE
      EXIT DO
     END IF
    ELSE
     CONTINUE DO
    ' state.depth -= 1  'returning from a script kind or runscriptbyid command
    END IF
   END IF


   memcpy(@(laststate),@(state),LEN(state))

   readstackcommand state, scrst, stkpos

   'debug "stkpos = " & stkpos

  jmpdoarg:

   'ditch arguments
   IF state.curkind = tyflow AND state.curvalue = flowswitch AND state.curargn > 0 THEN
    IF state.curargn >= state.curargc THEN
     'result of last case/do remains (?)
     stkpos -= 3
    ELSE
     stkpos -= 2
    END IF
   ELSE
    stkpos -= state.curargn
   END IF

   'debug "popped stkpos = " & stkpos &  " bottom = " & stkbottom

   'error level 1 because this routine is delicate and will probably break if called from an unusual place
   IF stkpos < stkbottom THEN scripterr("script debugger failed to read script state: stack underflow " & (stkpos - stkbottom), 1): EXIT DO
 LOOP
 IF stkpos > stkbottom AND wasscript < 0 THEN scripterr("script debugger failed to read script state: stack garbage " & (stkpos - stkbottom), 1)

 scriptstate = TRIM(outstr)
 'debug outstr
 reloadscript scrat(nowscript)
END FUNCTION

SUB delete_scriptdata (byval scriptd as ScriptData ptr)
 WITH *scriptd
  'debug "deallocating " & .id & " " & scriptname(.id) & " size " & .size
  totalscrmem -= .size
  numloadedscr -= 1
  deallocate(.ptr)
  IF .next THEN
   .next->backptr = .backptr
  END IF
  *.backptr = .next

  IF .refcount THEN
   FOR j as integer = 0 TO nowscript
    IF scrat(j).scr = scriptd THEN
     'debug "marking scrat(" & j & ") (id = " & scrat(j).id & ") unloaded"
     scrat(j).scr = NULL
     scrat(j).scrdata = NULL
    END IF
   NEXT
  END IF
 END WITH

 deallocate(scriptd)
END SUB

SUB reload_scripts
 IF isfile(game + ".hsp") THEN unlump game + ".hsp", tmpdir

 DIM unfreeable as integer = 0

 FOR i as integer = 0 TO UBOUND(script)
  DIM as ScriptData Ptr scrp = script(i), nextp
  WHILE scrp
   nextp = scrp->next
   WITH *scrp
    IF .refcount = 0 THEN
     delete_scriptdata scrp
    ELSE
     unfreeable += 1
     debuginfo "not reloading script " & scriptname(.id) & " because it's in use: refcount=" & .refcount
    END IF
   END WITH

   scrp = nextp
  WEND
 NEXT

 IF unfreeable THEN
  notification unfreeable & " scripts are in use and couldn't be freed (see g_debug.txt for details)"
 END IF

 'Cause the cache in scriptname() (and also in commandname()) to be dropped
 game_unique_id = STR(randint(INT_MAX))
END SUB
