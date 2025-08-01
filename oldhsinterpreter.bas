'OHRRPGCE GAME - Aspirationally-to-be-old HamsterSpeak Interpreter
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'This file holds everything specific to the old, original HS interpreter
'Script related things which aren't here: script commands, interpret (handles wait conditions),
'scripterr, runscript, loadscript, freescripts, load/saveglobalvars, commandname, decodetrigger.
'Probably runscript, loadscript, freescripts will eventually be moved here

#include "config.bi"
#include "util.bi"
#include "udts.bi"
#include "loading.bi"
#include "scrconst.bi"
#include "gglobals.bi"
#include "allmodex.bi"
#include "scriptcommands.bi"
#include "scripting.bi"
#include "sliceedit.bi"
#include "game.bi"

'local subs and functions
DECLARE FUNCTION scriptinterpreter_loop () as bool
DECLARE FUNCTION interpreter_occasional_checks () as integer
DECLARE FUNCTION functiondone () as integer
DECLARE SUB killtopscript ()
DECLARE SUB substart ()
DECLARE SUB subdoarg ()
DECLARE SUB subreturn ()
DECLARE SUB unwindtodo (byval levels as integer)
DECLARE FUNCTION command_parent_node(which_scrat as OldScriptState ptr) as integer
DECLARE SUB readstackcommand (node as ScriptCommand, state as OldScriptState, byref stk as Stack, byref i as integer)
DECLARE FUNCTION mathvariablename (value as integer, scr as ScriptData) as string
DECLARE FUNCTION scriptstate (byval targetscript as integer, byval recurse as integer = -1) as string
DECLARE FUNCTION readscriptvar (byval id as integer) as integer
DECLARE SUB writescriptvar (byval id as integer, byval newval as integer)
DECLARE SUB scriptmath ()

'these variables hold information used by breakpoint to step to the desired position
DIM SHARED waitforscript as integer
DIM SHARED waitfordepth as integer
DIM SHARED stepmode as integer
DIM SHARED lastscriptnum as integer

'''' Global variables
REDIM heap(maxScriptHeap) as integer
REDIM scrat(maxScriptRunning) as OldScriptState  'Can't resize due to 'hacks' in interpreterloop!
REDIM scriptinsts(maxScriptRunning) as ScriptInst
REDIM script(scriptTableSize - 1) as ScriptData Ptr
DIM scrst as Stack
DIM curcmd as ScriptCommand ptr


#MACRO dumpandreturn()
 scrst.pos -= .curargn
 scriptret = 0
 subreturn
#ENDMACRO

'Returns error string on failure, NULL on success
FUNCTION oldscriptstate_init (index as integer, script as ScriptData ptr) as zstring ptr
 WITH scrat(index)
  'erase state, pointer, return value and depth, set id
  .state = ststart
  .ptr = 0
  .ret = 0
  .depth = 0
  'id negative if stale data
  IF script->id < 0 THEN showbug "Starting a stale script"
  .id = ABS(script->id)
  .stackbase = -1
  .scr = script
  .scrdata = .scr->ptr
  .curargn = 0
  curcmd = cast(ScriptCommand ptr, .scrdata + .ptr) 'just in case it's needed before subread is run

  IF index = 0 THEN
   .frames(0).heap = 0
  ELSE
   .frames(0).heap = scrat(index - 1).heapend
  END IF
  .heapend = .frames(0).heap + .scr->vars
  IF .heapend > maxScriptHeap THEN RETURN @"script heap overflow"
  'Zero out locals
  FOR i as integer = .frames(0).heap TO .heapend
   heap(i) = 0
  NEXT i

  DIM parent as integer = .scr->parent
  'debug "oldscriptstate_init: loading script " & .id & " " & scriptname(.id) & " scrat(" & index & ") nonlocals " & .scr->nonlocals _
  '      & " vars " & .scr->vars & " parent " & parent & " " & scriptname(parent) & " heap " & .frames(0).heap & ":" & .heapend

  IF parent THEN
    'Search up the callstack for ancestors with frames which are referenced by this script.
    '(this will need to be changed once the frame might exist on a different stack)
    'Actually only search for the parent, and copy its ancestors
    DIM tryindex as integer = index - 1
    DO
     IF tryindex < 0 ORELSE scrat(tryindex).state < 0 THEN
      '(If it's a suspended script, it's in the wrong fibre)
      'This error can happen when reloading scripts 
      showerror "Could not find parent call frame on scrat stack. Probably incompatible scripts were reloaded."
      RETURN @"corrupt/unsupported script"
     END IF
     'debug "scrat(" & tryindex &") = " & scrat(tryindex).id
     IF scrat(tryindex).id = parent THEN
      FOR depth as integer = 1 TO .scr->nestdepth
       .frames(depth) = scrat(tryindex).frames(depth - 1)
      NEXT
      EXIT DO
     END IF
     tryindex -= 1
    LOOP

    'debug "  parent frame is scrat(" & tryindex &"), heap = " & .frames(1).heap & ":" & scrat(tryindex).heapend
  END IF

 END WITH
 RETURN NULL
END FUNCTION

'Return true if the script fibre finished
FUNCTION scriptinterpreter () as bool
 WITH *hsvm.cur_scrat
  SELECT CASE .state
   CASE IS < stnone
    showbug "illegally suspended script"
    .state = ABS(.state)
   CASE stnone
    showbug "script became stateless"
   CASE stwait  'Never happens; doesn't get called
    RETURN NO
   CASE ELSE
    RETURN scriptinterpreter_loop
  END SELECT
 END WITH
END FUNCTION

FUNCTION scriptinterpreter_loop () as bool
DIM i as integer
DIM temp as integer
DIM tmpstate as integer
DIM tmpcase as integer
'DIM tmpstart as integer
DIM tmpend as integer
DIM tmpstep as integer
DIM tmpnow as integer
DIM tmpvar as integer
DIM tmpkind as integer
DIM finished_fibre as bool

insideinterpreter = YES
IF scriptprofiling THEN start_fibre_timing

hsvm.cur_scriptinst->started = YES
next_interpreter_check_time = TIMER + scriptCheckDelay
interruption_grace_period = YES

interpretloop:
WITH *hsvm.cur_scrat
DO
 SELECT CASE .state
  CASE stnext'---check if all args are done
   IF gam.debug_scripts AND breakstnext THEN breakpoint gam.debug_scripts, 1
   IF .curargn >= curcmd->argc THEN
    '--pop return values of each arg
    '--evaluate function, math, script, whatever
    '--scriptret would be set here, pushed at return
    SELECT CASE curcmd->kind
     CASE tymath, tyfunct
      'Setup retvals()
      scrst.pos -= curcmd->argc
      retvalsbase = scrst.pos
      IF curcmd->kind = tymath THEN
       'Note, most of the time scriptmath will be called from subdoarg instead
       scriptmath
       subreturn
      ELSE
       .state = streturn
       IF commandprofiling THEN
        timed_script_commands(curcmd->value)
       ELSE
        script_commands(curcmd->value)
       END IF
       'hsvm.cur_scrat might have changed (e.g. "run script by id", or a new script fiber
       'triggered, or an error occurred and the script was stopped).
       'If a new script fiber triggered then subreturn will be delayed, and scriptret
       'needs to preserved in the meantime.
       .saved_scriptret = scriptret  'hack: writing to old hsvm.cur_scrat
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
          showbug "while fell out of bounds, landed on " & .curargn
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
          showbug "for fell out of bounds, landed on " & .curargn
          killallscripts
          EXIT DO
        END SELECT
       CASE flowreturn
        popstack(scrst, .ret)
        subreturn
       CASE flowbreak
        popstack(scrst, temp)
        IF temp <= 0 THEN
         scripterr "break(" & temp & ") is illegal", serrBadOp
         .state = streturn  'Ignore it
        ELSE
         unwindtodo(temp)
         '--for and while need to be broken
         IF curcmd->kind = tyflow ANDALSO (curcmd->value = flowfor ORELSE curcmd->value = flowwhile) THEN
          dumpandreturn()
         END IF
        END IF
        'If the break goes all the way to the root of the script (which is a do()) it is exited (for back-compat)
       CASE flowcontinue
        '--continue could be used to cause an infinite loop (including in a floating do), so also needs these checks
        IF interpreter_occasional_checks THEN CONTINUE DO
        popstack(scrst, temp)
        IF temp <= 0 THEN
         scripterr "continue(" & temp & ") is illegal", serrBadOp
         .state = streturn  'Ignore it
         CONTINUE DO
        ELSE
         unwindtodo(temp)
        END IF
        IF curcmd->kind = tyflow ANDALSO curcmd->value = flowswitch THEN
         '--set state to 2
         scrst.pos -= 2
         pushstack(scrst, 2)
         pushstack(scrst, 0) '-- dummy value
        ELSEIF .depth < 0 THEN
         scripterr "continue used outside of a do(), script will be exited", serrBadOp
        ELSEIF NOT (curcmd->kind = tyflow ANDALSO (curcmd->value = flowfor ORELSE curcmd->value = flowwhile)) THEN
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
        subreturn
       'When adding new flow control remember to update flowtype, etc, in scriptstate
       CASE ELSE
        '--do, then, etc... terminate normally
        dumpandreturn()
      END SELECT
     CASE tyscript
      DIM argc as integer = curcmd->argc
      'No need to check argc <= maxScriptArgs; setScriptArg checks OK
      DIM rsr as RunScriptResult
      rsr = runscript(curcmd->value, NO, NO, "called")
      'WARNING: hsvm.cur_scrat has changed, WITH still points to old value
      IF rsr = rsSuccess THEN
       'On success runscript calls oldscriptstate_init which will
       'set hsvm.cur_scrat->state = ststart
       '--fill heap with arguments (this is an inlined version of:)
       'FOR i as integer = argc - 1 TO 0 STEP -1
       ' popstack(scrst, temp)
       ' setScriptArg i, temp
       'NEXT i
       WITH *hsvm.cur_scrat
        DIM stkpos as integer ptr = scrst.pos - argc
        DIM localvars as integer ptr = @heap(.frames(0).heap)
        'Any additional arguments silently dropped, missing args remain (but neither should ever happen)
        FOR i as integer = 0 TO small(.scr->args, argc) - 1
         localvars[i] = stkpos[i]
        NEXT
        scrst.pos = stkpos
       END WITH

      ELSE
       'runscript may have shown an error, which might change the old hsvm.cur_scrat->state to streturn (in killscriptthread)
       'or stexit (in killallscripts). TODO: don't set .state in so many places when runscript is called
       .state = streturn
      END IF
      GOTO interpretloop 'new WITH pointer
     CASE ELSE
      scripterr "Script data or script interpreter state is corrupt: illegal kind " & curcmd->kind & " " & curcmd->value & " at " & .ptr & " in " & scriptname(.id), serrError
      killallscripts
      EXIT DO
    END SELECT

   ELSE
    'stnext: haven't evaluated all args yet. Special arg handling for flow and logand/logor.

    IF .curargn = 0 THEN
     '--always need to execute the first argument
     subdoarg
    ELSE 
     '--flow control and logical math are special, for all else, do next arg
     SELECT CASE curcmd->kind
      CASE tyflow
       SELECT CASE curcmd->value
        CASE flowif'--we got an if!
         SELECT CASE .curargn
          'CASE 0
          ' subdoarg '--run condition
          CASE 1
           IF readstack(scrst, 0) THEN
            'scrst.pos -= 1
            subdoarg '--run then block
           ELSE
            .curargn = 2
            '--if-else needs one extra thing on the stack to account for the then that didnt get used.
            pushstack(scrst, 0)
            subdoarg '--run else block
           END IF
          CASE 2
           '--finished then but not at end of argument list: skip else
           dumpandreturn()
          CASE ELSE
           showbug "if statement overstepped bounds"
         END SELECT
        CASE flowwhile'--we got a while!
         SELECT CASE .curargn
          'CASE 0
          ' subdoarg '--run condition
          CASE 1
           IF readstack(scrst, 0) THEN
            subdoarg '--run do block
            '--don't pop: number of words on stack should equal argn (for simplicity when unwinding stack)
           ELSE
            '--break while
            scrst.pos -= 1
            scriptret = 0
            subreturn
           END IF
          CASE ELSE
           showbug "while statement has jumped the curb"
         END SELECT
        CASE flowfor'--we got a for!
         SELECT CASE .curargn
          '--argn 0 is var
          '--argn 1 is start
          '--argn 2 is end
          '--argn 3 is step
          '--argn 4 is do block
          '--argn 5 is repeat (normal termination)
          CASE 1, 3 ',0
           '--get var, start, and later step
           subdoarg
          CASE 2
           '--set variable to start val before getting end
           writescriptvar readstack(scrst, -1), readstack(scrst, 0)
           '---now get end value
           subdoarg
          CASE 4
           IF gam.debug_scripts AND breakloopbrch THEN breakpoint gam.debug_scripts, 5
           tmpstep = readstack(scrst, 0)
           tmpend = readstack(scrst, -1)
           'tmpstart = readstack(scrst, -2)
           tmpvar = readstack(scrst, -3)
           tmpnow = readscriptvar(tmpvar)
           IF (tmpnow > tmpend ANDALSO tmpstep > 0) ORELSE (tmpnow < tmpend ANDALSO tmpstep < 0) THEN
            '--breakout
            scrst.pos -= 4
            scriptret = 0
            subreturn
           ELSE
            subdoarg '--run do block
           END IF
          CASE ELSE
           showbug "for statement is being difficult"
         END SELECT
        CASE flowswitch
         'IF .curargn = 0 THEN
         ' '--get expression to match
         ' subdoarg
         IF .curargn = 1 THEN
          '--set up state - push a 0: not fallen in
          '--assume first statement is a case, run it
          pushstack(scrst, 0)
          subdoarg
         ELSE
          popstack(scrst, tmpcase)
          popstack(scrst, tmpstate)
          DIM doseek as bool = NO ' whether or not to search argument list for something to execute
          IF tmpstate = 0 THEN
           '--not fallen in, check whether this case matches tmpvar
           IF tmpcase = readstack(scrst, 0) THEN
            tmpstate = 1
           END IF
           doseek = YES '--search for a case or do
          ELSEIF tmpstate = 1 THEN
           '--after successfully running a do block, pop off matching value and exit
           scrst.pos -= 1
           scriptret = 0
           subreturn
           CONTINUE DO
          ELSEIF tmpstate = 2 THEN
           '--continue encountered, fall back in
           tmpstate = 1
           doseek = YES '--search for a do
          END IF

          WHILE doseek
           tmpkind = .scrdata[*(@curcmd->args(0) + .curargn)]

           IF (tmpstate = 1 ANDALSO tmpkind = tyflow) ORELSE (tmpstate = 0 ANDALSO (tmpkind <> tyflow ORELSE .curargn = curcmd->argc - 1)) THEN
            '--fall into a do, execute a case, or run default (last arg)
            pushstack(scrst, tmpstate)
            subdoarg
            EXIT WHILE
           END IF
           IF .curargn >= curcmd->argc THEN
            scrst.pos -= 1
            scriptret = 0
            subreturn
            EXIT WHILE
           END IF
           .curargn += 1
          WEND
         END IF
        CASE ELSE
         subdoarg '--get next argument
       END SELECT
      CASE tymath
       SELECT CASE curcmd->value
        CASE 20'--logand
         IF readstack(scrst, 0) THEN
          subdoarg '--call 2nd argument
         ELSE
          '--shortcut evaluate to false
          scriptret = 0
          '--pop all args
          scrst.pos -= .curargn
          subreturn
         END IF
        CASE 21'--logor
         IF readstack(scrst, 0) THEN
          '--shortcut evaluate to true
          scriptret = 1
          '--pop all args
          scrst.pos -= .curargn
          subreturn
         ELSE
          subdoarg '--call 2nd argument
         END IF
        CASE ELSE
         subdoarg '--call argument
       END SELECT
      CASE ELSE
       subdoarg '--call argument
     END SELECT
    END IF
   END IF
  CASE streturn'---after evaluating a node return to its parent.
   '--sets stdone if done with entire script, otherwise calls subdoarg, which normally sets stnext
   'subdoarg has a fast path that inlines subreturn, so subreturn isn't called after every ScriptCommand node.
   subreturn
  'CASE stdoarg'---do argument
   '--evaluate an arg, either directly or by changing state. stnext will be next
   'stdoarg no longer exists, because subdoarg is always called directly without going through this SELECT.
   'It will keep evaluating arguments and math operators until hitting something difficult.
   'subdoarg
  CASE ststart'---read statement
   '--FIRST STATE
   '--just load the first command
   substart
  CASE stwait'---begin waiting for something
   WITH *hsvm.cur_scriptinst
    .curkind = curcmd->kind
    .curvalue = curcmd->value
    .curargc = curcmd->argc
   END WITH
   EXIT DO
  CASE stdone'---script terminates
   IF functiondone() = 0 THEN
    '--returning a value to a calling script, caller's .state is streturn
   ELSE
    '--finished the current script fibre, break the loop
    finished_fibre = YES
    EXIT DO
   END IF
   IF gam.debug_scripts AND breakstnext THEN breakpoint gam.debug_scripts, 2
   GOTO interpretloop 'new WITH pointer
  CASE sttriggered'---special initial state used just for script trigger logging
   DIM byref inst as ScriptInst = *hsvm.cur_scriptinst
   IF gam.script_log.enabled THEN
    IF inst.fibre = NULL THEN
     showbug "sttriggered missing fibre ptr"
    ELSE
     watched_script_triggered *inst.fibre
    END IF
   END IF
   inst.started = YES
   .state = ststart
  CASE sterror'---some error has occurred, crash and burn
   '--note that there's no thought out plan for handling errors
   killallscripts
   EXIT DO
  CASE stexit '--used only to exit this loop
   'Note: this is a bit of a hack: if we get here then hsvm.cur_scrat has already
   'changed so we're not really meant to be reading .state
   EXIT DO
 END SELECT
LOOP
END WITH

IF scriptprofiling THEN stop_fibre_timing
insideinterpreter = NO
RETURN finished_fibre

END FUNCTION

'Returns true if current interpreter block should be aborted.
'Gets called at the top of every kind of loop.
FUNCTION interpreter_occasional_checks () as integer
 STATIC calls_since_check as integer
 calls_since_check += 1
 'Cost for calling TIMER under Linux is on order 2us (~30 empty for loop iterations), worse on Windows, so needs to be avoided.
 'This may still lead to delays, as certain script commands might take a long time, and even
 'get called an unlimited number of times between calls to this function.
 'TODO: use a thread to set a flag every scriptCheckInterval milliseconds instead.
 IF calls_since_check < 250 THEN RETURN NO
 calls_since_check = 0
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
 IF hsvm.cur_scrat = NULL THEN EXIT SUB
 'Possible to use unwindtodo instead (used to do this) but that can't be done from
 'everywhere, and is slower
 'unwindtodo(9999)
 WITH *hsvm.cur_scrat
  setstackposition(scrst, .stackbase)
  .state = stdone
 END WITH
END SUB

SUB setScriptArg (byval arg as integer, byval value as integer)
 'No warning on passing in more arguments than the script takes, as they are always optional
 WITH *hsvm.cur_scrat
  IF .scr->args > arg THEN
   heap(.frames(0).heap + arg) = value
  END IF
 END WITH
END SUB

FUNCTION ancestor_script_id(depth as integer) as integer
 'Returns the script ID of a parent or ancestor of the current script.
 'Depth is 0 for itself, 1 for parent, 2 for grandparent, etc.
 'Returns 0 for none.

 DIM si as ScriptInst ptr = hsvm.cur_scriptinst

 WHILE depth > 0
  depth -= 1
  si = si->parent
  'Script stack doesn't go that far down
  IF si = NULL THEN RETURN 0
 WEND

 RETURN si->id
END FUNCTION

FUNCTION functiondone () as integer
'returns 0 when returning a value to a caller
'returns 1 when the last script in the fibre is finished

DIM return_value as integer = hsvm.cur_scrat->ret

'debug "functiondone slot " & hsvm.cur_slot & " id = " & hsvm.cur_scriptinst->id  & " " & scriptname(hsvm.cur_scriptinst->id)

'Pretty useless bookkeeping, could delete
scriptctr += 1
hsvm.cur_script->lastuse = scriptctr

IF scriptprofiling THEN script_return_timing

' Script logging
IF hsvm.cur_scriptinst->watched THEN watched_script_finished

' Cleanup the old script
deref_script hsvm.cur_script
nowscript = nowscript - 1
hsvm.set_cur_script

IF hsvm.cur_script = NULL THEN
 RETURN 1'--no scripts are running anymore
ELSE
 DIM state as OldScriptState ptr = hsvm.cur_scrat

 nowscript_locals = @heap(state->frames(0).heap)
 curcmd = cast(ScriptCommand ptr, state->scrdata + state->ptr)
 IF state->state < 0 THEN
  '--suspended fibre is resumed
  'debug "  resuming fibre in slot " & hsvm.cur_slot
  state->state = ABS(state->state)
  IF state->state = streturn THEN
   'streturn means a script command was interrupted by a triggered
   'script. Restore the scriptret that the command was about to return
   '(otherwise it would replaced by the triggered script's return value)
   '(May wish to do this on other states too?)
   '? "state " & state->state & " RESTORE scriptret " & state->saved_scriptret & ", overwriting " & scriptret
   scriptret = state->saved_scriptret
  END IF
  IF hsvm.cur_scriptinst->watched THEN watched_script_resumed
  IF scriptprofiling THEN start_fibre_timing
  RETURN 1'--reactivating a supended fibre
 ELSE
  scriptret = return_value
  state->state = streturn
  RETURN 0'--returning a value to a caller
 END IF
END IF

END FUNCTION

SUB substart ()
'this sets up a new script by preparing to run at the root command (which should be do)
DIM byref si as OldScriptState = *hsvm.cur_scrat
curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
scriptret = 0'--default returnvalue is zero
'si.curargn = 0'--moved to runscript to prevent scriptstate crash
si.state = stnext
si.stackbase = stackposition(scrst)

'For efficiency only check to expand the stack once, when starting a script. Need an upper
'bound on the stack space needed, doesn't matter that it's excessive. Worst case is a single
'do() containing n copies of the same node: one additional word per argument, each pushes
'one value on the stack.
'+5 just-in-case for extra state stuff pushed to stack (atm just switch, +1 ought to be sufficient)
checkoverflow(scrst, si.scr->size + 5)

IF curcmd->kind <> tyflow THEN
 scripterr "Root script command not flow, but " & curcmd->kind, serrError
 si.state = sterror
END IF
END SUB

SUB subdoarg ()

DIM byref si as OldScriptState = *hsvm.cur_scrat

'read/load arguments, evaluating immediate values, in a depth-first manner, until either:
'-all args for a command have been pushed, stnext to evaluate
'-certain flow & math commands need special logic after every evaluated arg, stnext to handle
si.state = stnext

'We can only evaluate a single arg to most flow (except do/then/else) and logand/logor
'before having to go back to the main loop. Only check that condition after evaluating one arg.
DIM checked_slow as bool = NO

'Cache these values in locals, because the compiler doesn't know it can due to all the
'writes through the stkpos pointer (which it assumes could point anywhere).
'Always set si.curargn = argn and scrst.pos = stkpos when exiting.
DIM argc as integer = curcmd->argc
DIM argn as integer = si.curargn
DIM stkpos as integer ptr = scrst.pos

DIM as integer ptr dataptr = si.scrdata

quickrepeat:
DIM as ScriptCommand ptr cmdptr = cast(ScriptCommand ptr, dataptr + *(@curcmd->args(0) + argn))

' Process an arg here if possible, otherwise stop
SELECT CASE cmdptr->kind
 CASE tynumber
  pushstackptr(stkpos, cmdptr->value)
 CASE tylocal
  'pushstackptr(stkpos, heap(si.frames(0).heap + cmdptr->value))  'Slower alternative
  pushstackptr(stkpos, nowscript_locals[cmdptr->value])
 CASE tynonlocal
  DIM id as integer = cmdptr->value
  pushstackptr(stkpos, heap(si.frames(id SHR 8).heap + (id AND 255)))
 CASE tyglobal
  IF cmdptr->value < 0 ORELSE cmdptr->value > maxScriptGlobals THEN
   scrst.pos = stkpos
   si.curargn = argn
   scripterr "Illegal global variable id " & cmdptr->value, serrError
   si.state = sterror
   EXIT SUB
  END IF
  pushstackptr(stkpos, global(cmdptr->value))
 CASE IS >= tymath, tyflow
  si.depth += 1
  pushstackptr(stkpos, si.ptr)
  pushstackptr(stkpos, argn)
  curcmd = cmdptr
  argc = curcmd->argc
  si.ptr = (cast(intptr_t, cmdptr) - cast(intptr_t, dataptr)) shr 2  ' \ sizeof(int32)
  argn = 0
  scriptret = 0'--default returnvalue is zero
  checked_slow = NO

  'this breakpoint is a perfect duplicate of breakstnext, but originally it also caught
  'streturn on evaluating numbers, locals and globals
  'edit: it's moved about even more now. needs rewriting
  'IF gam.debug_scripts AND breakststart THEN breakpoint gam.debug_scripts, 3
  'scriptdump "subdoarg"

  'Even for flow, first arg always needs evaluation, so don't leave yet!
  'If there are no args, then time to stop and evaluate it (this is not a math command)
  'EXIT SUB
  IF argc = 0 THEN
   scrst.pos = stkpos
   si.curargn = argn
   EXIT SUB
  END IF
  GOTO quickrepeat
 CASE ELSE
  scrst.pos = stkpos
  si.curargn = argn
  scripterr "Illegal statement type " & cmdptr->kind, serrError
  si.state = sterror
  EXIT SUB
END SELECT


finishedarg:
' Move on the the next arg and decide whether to fast track its execution

argn += 1
IF argn >= argc THEN
 IF curcmd->kind = tymath THEN
  'Optimisation: inline scriptmath -> subreturn -> subdoarg calls here, so we
  'can resume evaluating the parent node's args.
  'Doing the same for script_commands would be a mess because hsvm.cur_scrat and si.state can change.
/'  Here's the prologue (from a *previous* iteration through the above SELECT)
  si.depth += 1
  pushstack(scrst, si.ptr)
  pushstack(scrst, si.curargn)
  curcmd = cmdptr
  si.ptr = (cast(integer, cmdptr) - cast(integer, dataptr)) shr 2
  si.curargn = 0
  scriptret = 0'--default returnvalue is zero
'/
  stkpos -= argc
  retvalsbase = stkpos

  'Not used by scriptmath, but in case debugger entered after an error
  scrst.pos = stkpos
  si.curargn = argn

  scriptmath
  si.depth -= 1
  
  popstackptr(stkpos, argn)
  popstackptr(stkpos, si.ptr)
  curcmd = cast(ScriptCommand ptr, dataptr + si.ptr)
  '--push return value
  pushstackptr(stkpos, scriptret)
  argc = curcmd->argc
  checked_slow = NO  'New curcmd

  GOTO finishedarg
 END IF
 scrst.pos = stkpos
 si.curargn = argn
 EXIT SUB
END IF

IF checked_slow = NO THEN
 'Check duplicated from subreturn
 IF (curcmd->kind = tyflow ANDALSO (curcmd->value = flowif ORELSE curcmd->value >= flowfor)) ORELSE _
    (curcmd->kind = tymath ANDALSO (curcmd->value = 20 ORELSE curcmd->value = 21)) THEN
  scrst.pos = stkpos
  si.curargn = argn
  EXIT SUB
 END IF
 checked_slow = YES
END IF

GOTO quickrepeat
END SUB

SUB subreturn ()
DIM byref si as OldScriptState = *hsvm.cur_scrat

si.depth -= 1
IF si.depth < 0 THEN
 si.state = stdone
ELSE
 DIM stkpos as integer ptr = scrst.pos
 DIM argn as integer
 popstackptr(stkpos, argn)
 popstackptr(stkpos, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
 '--push return value
 pushstackptr(stkpos, scriptret)
 scrst.pos = stkpos
 argn += 1
 si.curargn = argn
 'Some flow (if/for/while/switch, make sure not to include do/then/else), logand & logor need handling by stnext
 IF argn >= curcmd->argc ORELSE _
    (curcmd->kind = tyflow ANDALSO (curcmd->value = flowif ORELSE curcmd->value >= flowfor)) ORELSE _
    (curcmd->kind = tymath ANDALSO (curcmd->value = 20 ORELSE curcmd->value = 21)) THEN
  si.state = stnext
 ELSE
  subdoarg
 END IF
END IF
END SUB

SUB unwindtodo (byval levels as integer)
'Unwinds the stack until the specified number of do's have been stripped and
'leaves the interpreter as if the last do block had successfully finished.
'This means if the do belongs to for/while it will loop, or exit the script if the toplevel do.
'Can only be called with levels > 0, and after the calling command has popped its args.

DIM byref si as OldScriptState = *hsvm.cur_scrat
DIM stkpos as integer ptr = scrst.pos

DO
 si.depth -= 1
 IF si.depth < 0 THEN
  scrst.pos = stkpos
  si.state = stdone
  EXIT SUB
 END IF

 popstackptr(stkpos, si.curargn)
 popstackptr(stkpos, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)

 IF levels = 0 THEN
  pushstackptr(stkpos, 0)  'Dummy return value from the do
  scrst.pos = stkpos
  si.curargn += 1
  si.state = stnext
  EXIT SUB
 END IF

 IF curcmd->kind = tyflow ANDALSO curcmd->value = flowdo THEN
  levels -= 1
  'first pop do's evaluated arguments before stopping
 END IF

 'pop arguments
 IF curcmd->kind = tyflow ANDALSO curcmd->value = flowswitch THEN
  'unlike all other flow, switch stack usage != argn
  stkpos -= 2 'state, matching value
 ELSE
  stkpos -= si.curargn
 END IF
LOOP

END SUB

FUNCTION readscriptvar (byval id as integer) as integer
 SELECT CASE id
  CASE -256 TO -1  'local variable (fast path)
   id = -id - 1
   RETURN nowscript_locals[id]
  CASE IS < 0 'local or nonlocal variable
   id = -id - 1
   RETURN heap(hsvm.cur_scrat->frames(id SHR 8).heap + (id AND 255))
  CASE 0 TO maxScriptGlobals 'global variable
   RETURN global(id)
  CASE ELSE
   scripterr "Cannot read global " & id & ". Out of range", serrBadOp
 END SELECT
END FUNCTION

SUB writescriptvar (byval id as integer, byval newval as integer)
 SELECT CASE id
  CASE -256 TO -1  'local (fast path)
   id = -id - 1
   nowscript_locals[id] = newval
  CASE IS < 0 'local/nonlocal variable
   id = -id - 1
   heap(hsvm.cur_scrat->frames(id SHR 8).heap + (id AND 255)) = newval
  CASE 0 TO maxScriptGlobals 'global variable
   global(id) = newval
  CASE ELSE
   scripterr "Cannot write global " & id &  ". Out of range", serrBadOp
 END SELECT
END SUB

SUB scriptmath
 SELECT CASE as CONST curcmd->value
  CASE 0' random
   scriptret = INT(retvals(0) + RND * (1.0 + retvals(1) - retvals(0))) 'handles the case max-min = 2^32
  CASE 1' exponent
   IF retvals(0) = 0 and retvals(1) < 0 THEN
    scripterr "Tried to take negative power of zero, 0^" & retvals(1), serrBadOp
   ELSE
    scriptret = retvals(0) ^ retvals(1)
   END IF
  CASE 2' modulus
   IF retvals(1) = 0 THEN
    scripterr "Division by zero: " & retvals(0) & ",mod,0", serrBadOp
   ELSE
    scriptret = retvals(0) MOD retvals(1)
   END IF
  CASE 3' divide
   IF retvals(1) = 0 THEN
    scripterr "Division by zero: " & retvals(0) & "/0", serrBadOp
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
   DIM ret as integer = readscriptvar(retvals(0)) + retvals(1)
   writescriptvar retvals(0), ret
   scriptret = ret
  CASE 18'decrement
   DIM ret as integer = readscriptvar(retvals(0)) - retvals(1)
   writescriptvar retvals(0), ret
   scriptret = ret
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
    scripterr "Tried to take squareroot of " & retvals(0), serrBadOp
   ELSE
    scriptret = SQRT(retvals(0))
   END IF
  'When adding more math types remember to update mathname() in scriptcmdname
  CASE ELSE
   scripterr "Unsupported math function id " & curcmd->value, serrError
 END SELECT
END SUB

'Returns the srcpos of the current command of the given script, or 0 if that debug info not available.
'The srcpos is relative to the script's .script_position.
FUNCTION script_current_srcpos(which_scrat as OldScriptState ptr) as uinteger
 'Write curcmd out in case the current script is which_scrat
 WITH *hsvm.cur_scriptinst
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH

 WITH *which_scrat
  DIM curnode as ScriptCommand ptr
  curnode = cast(ScriptCommand ptr, .scrdata + .ptr)

  'debug "script_current_srcpos: script " & .scr->id & " hassrcpos = " & .scr->hassrcpos & "  kind/id = " & _
  '      curnode->kind & "/" & curnode->value & " ptr = " & .ptr & " argc = " & curnode->argc
  IF .scr->hassrcpos THEN
   SELECT CASE curnode->kind
    CASE tyflow, tymath, tyfunct, tyscript
     'The srcpos is immediately after the arg list (can't index .args() directly)
     RETURN (@curnode->args(0))[curnode->argc]
    CASE ELSE  'tynumber
     'Numbers don't have srcpos's. Return the srcpos of the parent node instead.
     curnode = cast(ScriptCommand ptr, .scrdata + command_parent_node(which_scrat))
   END SELECT
  END IF
 END WITH
 RETURN 0
END FUNCTION

'Dump interpreter state of current script to g_debug.txt
SUB scriptdump (header as string)
 DIM statestr(9) as string
 statestr(0) = "none"
 statestr(1) = "wait"
 statestr(2) = "start"
 statestr(3) = "return"
 statestr(4) = "next"
 statestr(5) = "doarg"
 statestr(6) = "done"
 statestr(7) = "triggered"
 statestr(8) = "error"
 statestr(9) = "exit"

 WITH *hsvm.cur_scrat
   DIM indent as string
   IF .depth >= 0 THEN
     indent = STRING(.depth, " ")
   ELSE
     indent = STRING(ABS(.depth), "<")
   END IF

   DIM state as string
   SELECT CASE .state
    CASE 0 TO 9
      state = statestr(.state)
    CASE ELSE
      state = "illegal: " & .state
   END SELECT

   debug indent & "[" & header & "]"
   IF .depth < 0 THEN
    debug indent & "depth = " & .depth
   END IF
   'debug indent & "slot   = " & hsvm.cur_slot
   debug indent & "id     = " & .id & " " & scriptname(.id)
   debug indent & "ptr    = " & .ptr
   debug indent & "state  = " & state
   debug indent & "kind   = " & curcmd->kind
   debug indent & "value  = " & curcmd->value
   debug indent & "argn   = " & .curargn
   debug indent & "argc   = " & curcmd->argc
   debug indent & "stkpos = " & (scrst.pos - scrst.bottom)
   debug indent & "node   : kind " & .scrdata[.ptr] & " id " & .scrdata[.ptr + 1]
 END WITH
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
  'We're waiting for some scripts to exit
  EXIT SUB
 ELSEIF nowscript < waitforscript THEN 
  waitforscript = 999
  waitfordepth = 999
 ELSE
  'if final objective is a script, not a depth, stop
  IF waitfordepth = 999 THEN
   'Done
   waitforscript = 999
  ELSE
   IF hsvm.cur_scrat ANDALSO hsvm.cur_scrat->depth > waitfordepth THEN
    'We're waiting for some commands to exit.
    EXIT SUB
   ELSE
    waitforscript = 999
    waitfordepth = 999
   END IF
  END IF
 END IF
END IF

IF hsvm.cur_scrat THEN argn = hsvm.cur_scrat->curargn

SELECT CASE stepmode
 CASE stependscript
  GOTO breakin
 CASE stepscript
'  IF callspot = 1 THEN IF curcmd->kind = tyscript THEN GOTO breakin
'  IF callspot = 2 THEN GOTO breakin
  IF lastscriptnum <> nowscript THEN GOTO breakin
 CASE stepargsdone, stepup, stepnext
  IF callspot = 1 THEN
   'IF hsvm.cur_scrat->curargn < curcmd->argc OR hsvm.cur_scrat->curargn = 0 THEN EXIT SUB
   'IF  hsvm.cur_scrat->curargn = 0 THEN
   ' debug "skipped b " & curcmd->argc & " flow " &  curcmd->kind
   ' EXIT SUB
   'end if
   IF curcmd ANDALSO curcmd->kind = tyflow THEN
    IF curcmd->value = flowif AND argn <> 1 THEN EXIT SUB
    IF curcmd->value = flowfor AND argn <> 4 THEN EXIT SUB
    IF curcmd->value = flowwhile AND argn <> 1 THEN EXIT SUB
   END IF
  END IF
  GOTO breakin
END SELECT

' Warning: the following EXIT SUBs refer to how control flow previously worked in this SUB
'IF callspot = 1 THEN 'stnext
'' IF (mode AND 4) AND curcmd->kind <> tyscript THEN EXIT SUB
' 'only used to print off evaluated list of arguments
'' IF ((mode AND breakreadcmd) <> 0) AND (hsvm.cur_scrat->curargn < curcmd->argc OR hsvm.cur_scrat->curargn = 0) THEN EXIT SUB
' IF hsvm.cur_scrat->curargn < curcmd->argc OR hsvm.cur_scrat->curargn = 0 THEN EXIT SUB
'' IF (mode AND breakargsdone) THEN EXIT SUB
' IF curcmd->kind = tyflow AND curcmd->value = flowif THEN EXIT SUB
'END IF

'END STEPPING LOGIC

EXIT SUB

breakin:

'clear breakpoint bits
mode = mode AND 3
stepmode = 0
scriptwatcher mode

END SUB

'Modify scroll, the top of a view of limit+1 items arranged in a grid of displaylines * displaycols, to move by steplines
'Increments scroll by a multiple of displaycols.
'If displaycols = 1 and the number of items is at least a pageful then won't exceed limit.
SUB scroll_grid_menu(byref scroll as integer, steplines as integer, limit as integer, displaylines as integer, displaycols as integer = 1)
 IF steplines < 0 THEN
  scroll = large(0, scroll + steplines * displaycols)
 ELSE
  DIM displayend as integer = scroll + displaylines * displaycols - 1
  steplines = small(steplines, CEIL((limit - displayend) / displaycols))
  steplines = large(0, steplines)
  scroll += steplines * displaycols
 END IF
END SUB

FUNCTION plus_minus_scroll(byref scroll as integer, startspeed as integer, limit as integer, displaylines as integer, displaycols as integer = 1) as bool

 DIM amount as integer
 amount += accelerating_keydown(scPlus, limit, startspeed)
 amount -= accelerating_keydown(scMinus, limit, startspeed)
' IF pagekeys THEN
  IF keyval(scPageUp) THEN amount = -displaylines + 1
  IF keyval(scPageDown) THEN amount = displaylines - 1
  IF keyval(scHome) THEN amount = -limit
  IF keyval(scEnd) THEN amount = limit
' END IF
 scroll_grid_menu scroll, amount, limit, displaylines, displaycols
 RETURN amount <> 0
END FUNCTION

LOCAL SUB comma_period_scroll(byref scroll as integer, limit as integer, displaylines as integer, displaycols as integer = 1)
END SUB

SUB describe_locals(locals_info() as string, selectedscript as integer, showhandles as bool, datacol as integer)
 DIM displaywidth as integer = vpages(vpage)->w - 4
 DIM curline as string
 CONST tabwidth = 6
 WITH scrat(selectedscript)
  a_append locals_info(), "Return value=" & fgtag(datacol) & .ret
  IF .scr->args THEN curline = "Args: "

  FOR localno as integer = 0 TO .scr->vars - 1
   VAR value = heap(.frames(0).heap + localno)
   DIM temp as string
   IF showhandles THEN
    temp &= describe_handle(value, YES)
   ELSE
    temp &= value
   END IF
   temp = LEFT(localvariablename(localno, *.scr), 18) & "=" & fgtag(datacol, temp)
   'Break lines; distinguish the arguments by adding a line break after the last one
   IF LEN(curline) ANDALSO (localno = .scr->args ORELSE textwidth(curline & temp) > displaywidth) THEN
    a_append locals_info(), curline 
    curline = ""
   END IF
   curline &= temp
   'Add spacing to the next multiple of tabwidth spaces, at least 1 space
   curline &= SPACE(tabwidth - (textwidth(curline) MOD (8 * tabwidth)) \ 8)
  NEXT
 END WITH
 IF LEN(curline) THEN a_append locals_info(), curline
END SUB

'Accessed with F7 in the script debugger
SUB script_debugger_slices()
 DIM slotstr as string
 IF prompt_for_string(slotstr, "Slice number to show? Blank for none. (e.g. ""Cont 14"" is slice 14)", 9) THEN
  DIM slot as integer = 0
  DIM sl as Slice ptr
  parse_int(slotstr, @slot)
  IF slot > 0 ANDALSO slot <= UBOUND(plotslices) ANDALSO plotslices(slot).handle THEN
   sl = plotslices(slot).sl
  ELSEIF LEN(slotstr) THEN  'Not blank
   notification "Invalid slice number"
  END IF
  slice_editor SliceTable.Root, , , , , sl
 END IF
END SUB

'The following function is an atrocious mess. Don't worry too much; it'll be totally replaced.
SUB scriptwatcher (byref mode as integer, byval drawloop as bool = NO)
STATIC localsscroll as integer
STATIC globalsscroll as integer
STATIC stringsscroll as integer
STATIC timersscroll as integer
STATIC selectedscript as integer
STATIC bottomscript as integer
STATIC viewmode as integer
STATIC lastscript as integer
'drawloop: if true, only draw the script debugger overlay (called from displayall); not interactive.
'viewmode: 0 = script state, 1 = local variables, 2 = global variables, 3 = strings, 4 = timers, 5 = old script state (scriptstate())
'mode: 0 = do nothing, 1 = non-interactive (display over game), 2 >= interactive:
'2 = normal mode, 3 = display game and step tick-by-tick on input

' Displayed lines in the plotstring view mode
REDIM stringlines() as string
DIM page as integer

DIM saved_gfxio_state as bool
IF mode >= 2 AND drawloop = NO THEN
 push_and_reset_gfxio_state
 saved_gfxio_state = YES
END IF

DIM displaywidth as integer = vpages(vpage)->w \ 8  'In characters

DIM datacol as integer = uilook(uiDescription) 'Values of variables (but not strings)
DIM shortcutcol as integer = datacol           'Shortcut key
DIM barcol as integer = findrgb(50, 50, 100)   'Lines across the screen

DIM strbgcol as integer = findrgb(0, 128, 0)
FOR i as integer = 0 TO UBOUND(plotstr)
 ' Split each plotstring up onto multiple lines, each line is an element of stringlines
 DIM plots as string = plotstr(i).s
 DIM marginstr as string = rpad(i & ":", , 3)
 ' Wrap the string
 DIM linelen as integer = 0
 FOR j as integer = 0 TO small(500, LEN(plots)) - 1
  linelen += 1
  IF (linelen = displaywidth - LEN(marginstr) ORELSE plots[j] = ASC(!"\n")) AND j <> LEN(plots) - 1 THEN
   a_append stringlines(), marginstr + bgtag(strbgcol, MID(plots, (j + 2) - linelen, linelen))
   marginstr = SPACE(LEN(marginstr))
   linelen = 0
  END IF
 NEXT
 ' Add the final piece
 a_append stringlines(), marginstr + bgtag(strbgcol, MID(plots, LEN(plots) + 1 - linelen, linelen))
NEXT

IF hsvm.cur_scriptinst THEN
 WITH *hsvm.cur_scriptinst
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH
END IF

'debug "watch mode=" & mode & " callspot = " & callspot & " stepmode = " & stepmode _
'      & " curscript = " & nowscript & " curdepth = " & hsvm.cur_scrat->depth & " waitscr = " & waitforscript & " waitdepth = " & waitfordepth

'initialise state
IF mode = 1 THEN waitforscript = 999: waitfordepth = 999: stepmode = 0

redraw:

''''''''''''''''''''''''''''''''''' Display '''''''''''''''''''''''''''''''''''

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

'Height in px of the debugger's F1/F2/... menu bar
CONST menubar_height = 20
'Number of lines of text (9 pixels high) that can fit below the menubar and a
'one-line header (not all headers are)
DIM displaylines as integer = (vpages(page)->h - menubar_height - 12) \ 9

'edgeprint callmode & " " & viewmode & " " & callspot, 140, 4, uilook(uiText), page

selectedscript = bound(selectedscript, 0, nowscript)
IF selectedscript = lastscript THEN selectedscript = nowscript
lastscript = nowscript

/'
DIM hasargs as bool
IF nowscript >= 0 THEN
 SELECT CASE hsvm.cur_scriptinst->curkind
  CASE tynumber, tyglobal, tylocal
   hasargs = NO
  CASE ELSE
   hasargs = YES
 END SELECT
END IF
'/

'Draw header at top of screen
IF mode > 1 THEN
 rectangle 0, 0, rWidth, 18, barcol, page
 edgeprint "F1   F2     F3   F4      F5      F6     F7     F8", 0, 0, shortcutcol, page
 DIM tabnames(...) as string = {"Help", "Source", "Vars", "Globals", "Strings", "Timers", "Slices", "Stack"}
 DIM viewmodes(...) as integer = {-1, 0, 1, 2, 3, 4, -1, 5}
 DIM tabconcat as string
 FOR idx as integer = 0 TO UBOUND(tabnames)
  IF viewmodes(idx) = viewmode THEN
   tabconcat += fgtag(uilook(uiSelectedItem), tabnames(idx)) + " "
  ELSE
   tabconcat += tabnames(idx) + " "
  END IF
 NEXT
 edgeprint tabconcat, 0, 9, uilook(uiText), page, YES
 'rectangle 0, 18, rWidth, 1, barcol, page
END IF

DIM ol as integer = pBottom  'Line output Y position

DIM var_spacing as integer = 160  'Pixels apart to print each column of variables
IF vpages(vpage)->w > 380 THEN var_spacing = 190
CONST local_lines = 5   'Number of lines of local variables
'Number of columns of local or global variables
DIM var_cols as integer = small(vpages(vpage)->w \ var_spacing, 6)

IF mode > 1 AND (viewmode = 0 OR viewmode = 5) THEN
 'Source or Stack, both showing script position
 DIM msg as string
 IF nowscript = -1 THEN
  msg = "Script debugger: no scripts"
 ELSEIF viewmode = 0 THEN  'Source
  DIM posdata as ScriptTokenPos
  IF get_script_line_info(posdata, @scrat(selectedscript)) THEN
   msg = highlighted_script_line(posdata, displaywidth * 3, @scriptinsts(selectedscript))
  ELSE
   msg = !"Script line number unknown.\n"

   DIM hs_header as HSHeader
   load_hsp_header tmpdir & "hs", hs_header
   IF strcmp(STRPTR(hs_header.hspeak_version), STRPTR("3W ")) < 0 THEN
    'Didn't support line numbers
    msg &= !"Recompile your scripts with a more recent version of HSpeak.\n"
   ELSE
    msg &= !"Recompile your scripts without disabling debug info.\n"
   END IF
   msg &= "Press F8 to see the old fall-back script position description instead."
  END IF
 ELSEIF viewmode = 5 THEN  'Stack
  msg = scriptstate(selectedscript)
  msg = text_left(msg, 1200)  'At most 1200 pixels wide (4 lines)
  'msg &= !"\nLast return value: " & scriptret
 END IF
 wrapprint msg, 0, pBottom, uilook(uiText), page
END IF

DIM locals_info() as string
DIM as integer scriptargs = 0, numlocals = 0
IF selectedscript >= 0 THEN
 WITH *scrat(selectedscript).scr
  scriptargs = .args
  numlocals = .vars
 END WITH
END IF

DIM header as string

IF mode > 1 AND viewmode = 1 AND selectedscript >= 0 THEN
 'Display local (but not nonlocal) variables and return value.
 
 describe_locals locals_info(), selectedscript, YES, datacol

 WITH scrat(selectedscript)
  'Return value, args and locals
  FOR i as integer = local_lines - 1 TO 0 STEP -1
   VAR idx = localsscroll + i
   IF idx <= UBOUND(locals_info) THEN
    edgeprint locals_info(idx), 0, ol, uilook(uiText), page, YES
   END IF
   ol -= 9
  NEXT
  ol = pBottom - local_lines * 9 - 1

  'Header
  IF numlocals = 0 THEN
   header = "    Has no variables"
  ELSE
   'Header for the locals section
   IF scriptargs = 999 THEN
    header = "    " & numlocals & " Local variables and args: (`+`/`-` scroll)"
   ELSE
    header = "    " & scriptargs & " Args, " & (numlocals - scriptargs) & " Locals"
    IF .scr->nonlocals > 0 THEN
     header += "; excluding " & .scr->nonlocals & " nonlocals:"
    ELSEIF numlocals > var_cols * local_lines THEN
     header += ": (`+`/`-` scroll)"
    END IF
   END IF
  END IF
 END WITH
END IF

IF mode > 1 AND viewmode = 2 THEN
 'Display global variables
 FOR i as integer = displaylines - 1 TO 0 STEP -1
  FOR j as integer = var_cols - 1 TO 0 STEP -1   'reverse order so the var name is what gets overwritten
   DIM globalno as integer = globalsscroll + i * var_cols + j
   IF globalno > UBOUND(global) THEN CONTINUE FOR
   DIM temp as string = globalno & "=" & fgtag(datacol) & describe_handle(global(globalno), YES)
   edgeprint temp, j * var_spacing, ol, uilook(uiText), page, YES
  NEXT
  ol -= 9
 NEXT
 header = "Global variables:  (`+`/`-` scroll)"
END IF

IF mode > 1 AND viewmode = 3 THEN
 'display stringlines
 FOR i as integer = displaylines - 1 TO 0 STEP -1
  DIM idx as integer = i + stringsscroll
  IF idx > UBOUND(stringlines) THEN CONTINUE FOR
  edgeprint stringlines(idx), 0, ol, uilook(uiText), page, YES
  ol -= 9
 NEXT
 header = "Plotstrings:  (`+`/`-` scroll)"
END IF

IF mode > 1 AND viewmode = 4 THEN
 'display timers
 edgeprint "ID Count Speed Flags Str Trigger", 0, ol, uilook(uiText), page
 ol -= 9
 FOR i as integer = displaylines - 1 TO 0 STEP -1
  DIM id as integer = i + timersscroll
  IF id > UBOUND(timers) THEN CONTINUE FOR
  DIM text as string
  WITH timers(id)
   text = rpad(STR(id), , 3)
   IF .speed < 0 THEN
    IF .finished_tick = gam.script_log.tick THEN
     text &= "Trigg "
    ELSE
     text &= "Done  "
    END IF
   ELSE
    text &= rpad(STR(.count), , 6)
   END IF
   text &= rpad(STR(ABS(.speed)), , 6)  'negated if not running
   text &= rpad(STR(.flags), , 6)
   text &= rpad(STR(.st), , 4)
   IF .trigger = -2 THEN
    text &= "Game Over"
   ELSEIF .trigger >= 0 THEN
    text &= scriptname(.trigger)
   END IF
  END WITH
  edgeprint text, 0, ol, uilook(uiText), page
  ol -= 9
 NEXT
 header = "Timers:  (`+`/`-` scroll)"
END IF

IF LEN(header) THEN
 rectangle 0, ol - 1, rWidth, 10, barcol, page
 edgeprint ticklite(header), 32, ol, uilook(uiText), page, YES
END IF

IF mode > 1 AND (viewmode = 0 OR viewmode = 1 OR viewmode = 5) THEN
 'show scripts list

 'Leave room for locals or source line or scriptstate
 ol = vpages(page)->h - (local_lines + 3) * 9 - 4
 'Stop this far from the top of the screen (top header plus21 line for "Scripts:" header)
 CONST stop_y = menubar_height + 20

 DIM script_rows as integer = (ol - stop_y) \ 9

 DIM as integer statex, commandx  'Where to print state and command
 IF viewmode = 5 THEN  'Stack
  edgeprint "#   Name         Depth State Command", 0, ol, uilook(uiText), page
  statex = 184
  commandx = 232
 ELSE  'Source
  edgeprint "#   Name            State/Command", 0, ol, uilook(uiText), page
  statex = 160
  commandx = 160
 END IF
 'rectangle 0, ol + 9, rWidth, 1, barcol, page
 ol -= 9
 
 IF mode = 1 THEN
  bottomscript = nowscript - (script_rows - 1)
  selectedscript = nowscript
 ELSE
'  bottomscript = bound(bottomscript, selectedscript, selectedscript - (ol - 10) \ 9)
  bottomscript = large(small(bottomscript, selectedscript), selectedscript - (script_rows - 1))
 END IF
 
 FOR i as integer = large(bottomscript, 0) TO nowscript
  'if script is about to be executed, don't show it as having been already
  DIM lastarg as bool = (scrat(i).curargn >= scriptinsts(i).curargc AND i <> nowscript)

  DIM col as integer
  IF mode > 1 AND i = selectedscript THEN col = uilook(uiSelectedItem) ELSE col = uilook(uiText)
  edgeprint STR(i), 0, ol, col, page
  edgeprint LEFT(scriptname(scrat(i).id), 16), 29, ol, col, page
  IF viewmode = 5 THEN
   edgeprint STR(scrat(i).depth), 160, ol, col, page
  END IF

  DIM statemsg as string
  IF scrat(i).state < 0 THEN
   IF scriptinsts(i).started = NO THEN
    statemsg = "Queued (not started)"
   ELSE
    statemsg = "Suspended"
   END IF
  ELSEIF scrat(i).state = stwait THEN
   IF scriptinsts(i).waiting = waitingOnCmd THEN
    statemsg = commandname(scriptinsts(i).curvalue)
    SELECT CASE scriptinsts(i).curvalue
     CASE 1, 3, 4, 9, 244, 508, 575
      '--All wait commands with one arg:
      '--wait, wait for hero, wait for NPC, wait for key, wait for scancode, wait for slice, wait for dissolve
      statemsg += "(" & scriptinsts(i).waitarg & ")"
    END SELECT
   ELSEIF scriptinsts(i).waiting = waitingOnTick THEN
    statemsg = "forced-wait(" & scriptinsts(i).waitarg & ")"
   ELSE
    statemsg = "!WAIT ERROR"
   END IF
   IF menus_allow_gameplay() = NO THEN
    statemsg = "Suspended by menu; " & statemsg
   END IF
  ELSEIF scrat(i).state = stnext AND scriptinsts(i).curkind = tyscript AND lastarg THEN
   statemsg = "Called #" & i + 1
  ELSEIF scrat(i).state = stnext AND scriptinsts(i).curkind = tyfunct AND scriptinsts(i).curvalue = 176 AND lastarg THEN
   'run script by id
   statemsg = "Called #" & i + 1 & " by ID"
  ELSE
   IF viewmode = 5 THEN
    statemsg = STR(scrat(i).state)
    'edgeprint STR(.curkind), 232, ol, col, page
    'edgeprint STR(.curvalue), 280, ol, col, page
   END IF
   WITH scriptinsts(i)
    edgeprint scriptcmdname(.curkind, .curvalue, *.scr), commandx, ol, col, page
   END WITH
  END IF
  edgeprint statemsg, statex, ol, col, page

  ol -= 9
  IF ol < stop_y THEN EXIT FOR
 NEXT i
 ol -= 10
 rectangle 0, ol, rWidth, 19, barcol, page
 edgeprint ticklite("    Scripts:  (`[`/`]` scroll)"), 0, ol, uilook(uiText), page, YES
 ''Run until' is disabled if the topmost script is selected
 DIM temp as string = "un until"
 IF selectedscript = nowscript THEN temp = fgtag(uilook(uiDisabledItem), temp)
 temp = ticklite("`S`tep  `N`ext/`R`" & temp & "/`F`inish script", shortcutcol)
 edgeprint temp, 0, ol + 9, uilook(uiText), page, YES
END IF 'end drawing scripts list


''''''''''''''''''''''''''''''''''' Controls '''''''''''''''''''''''''''''''''''

IF mode > 1 AND drawloop = NO THEN
 setvispage page, NO
 DIM w as KBScancode = waitforanykey(YES, 2, NO)  'Wait for new or repeating key or a screen resize
 IF w = scEsc OR w = scF10 THEN
  mode = 0
  clearkey(scF10)
  clearkey(scEsc)
  clearpage page
  setvispage page
 END IF
 IF w = scResize THEN GOTO redraw

 'Obsolete key kept for muscle memory, for now
 IF w = scV THEN loopvar(viewmode, 0, 5): GOTO redraw

 IF w = scRightBracket THEN 'scPageUp THEN
  selectedscript -= accelerating_keydown(scRightBracket, 1000, 1)
  localsscroll = 0
  'plus_minus_scroll globalsscroll, -7, maxScriptGlobals,    displaylines, var_cols
  GOTO redraw
 END IF
 IF w = scLeftBracket THEN 'scPageDown THEN
  selectedscript += accelerating_keydown(scLeftBracket, 1000, 1)
  localsscroll = 0
  'plus_minus_scroll globalsscroll,  7, maxScriptGlobals,    displaylines, var_cols
  GOTO redraw
 END IF

 DIM update as bool

 ' SELECT CASE viewmode
 '  CASE 0, 1, 7
 '   update OR= period_comma_scroll(globalsscroll,  7, maxScriptGlobals,    displaylines, var_cols)
 ' END SELECT

 ' VAR minus = (w = scMinus OR w = scNumpadMinus)
 ' VAR plus = (w = scPlus OR w = scNumpadPlus)
 ' IF plus OR minus THEN
 '  VAR neg = IIF(minus, -1, 1)
  'IF viewmode = 1 THEN update OR= plus_minus_scroll(localsscroll,  1, numlocals - 1,       local_lines,  var_cols)
  IF viewmode = 1 THEN update OR= plus_minus_scroll(localsscroll,  1, UBOUND(locals_info),   local_lines)
  IF viewmode = 2 THEN update OR= plus_minus_scroll(globalsscroll, 3, maxScriptGlobals,    displaylines - 1, var_cols)
  IF viewmode = 3 THEN update OR= plus_minus_scroll(stringsscroll, 3, UBOUND(stringlines), displaylines)
  IF viewmode = 4 THEN update OR= plus_minus_scroll(timersscroll,  3, UBOUND(timers),      displaylines)
  IF update THEN GOTO redraw
' END IF

 IF w = scF1 THEN
  show_help("game_script_debugger")
  GOTO redraw
 ELSEIF w = scF2 THEN
  viewmode = IIF(viewmode = 0, 5, 0)
  GOTO redraw
 ELSEIF w >= scF3 AND w <= scF6 THEN
  viewmode = 1 + w - scF3   '1 to 4
  GOTO redraw
 ELSEIF w = scF7 THEN
  script_debugger_slices
  GOTO redraw
 ELSEIF w = scF8 THEN
  viewmode = 5
  GOTO redraw
 END IF

 IF w = scC THEN 'frame stepping mode
  mode = IIF(mode = 2, 3, 2)
  GOTO redraw
 END IF

 'stepping
 IF w = scN THEN
  'step till next script
  mode or= breakstnext OR breakstdone OR breaklooptop
  stepmode = stepscript
  lastscriptnum = nowscript
 END IF
 IF w = scU THEN  'Wait for current command to finish
  IF hsvm.cur_scrat THEN
   mode or= breakstnext
   stepmode = stepup
   waitfordepth = hsvm.cur_scrat->depth - 1
   waitforscript = nowscript
  END IF
 END IF
 IF w = scR THEN  'Wait to return to the selected script
  mode or= breakstnext
  waitforscript = selectedscript
  stepmode = stependscript
 END IF
 IF w = scS THEN
  mode or= breakststart OR breakstnext OR breakloopbrch
  stepmode = stepnext
  waitforscript = 999
 END IF
 IF w = scF THEN  'Wait for the current script to finish
  IF hsvm.cur_scrat THEN
   'mode or= breakststart
   mode or= breakstnext OR breakloopbrch
   stepmode = stepargsdone
   waitforscript = nowscript
   waitfordepth = -1
  END IF
 END IF
END IF

IF drawloop AND mode > 1 THEN
 'displayall: dpage was copied to vpage
 SWAP dpage, vpage
END IF

'in sane mode, stray keypresses are not passed through
'(the mode = 2 thrown in to prevent an infinite loop, no idea how or why)
IF drawloop = NO AND mode = 2 THEN GOTO redraw

IF saved_gfxio_state THEN pop_gfxio_state

next_interpreter_check_time = TIMER + scriptCheckDelay
interruption_grace_period = YES

END SUB

SUB readstackcommand (node as ScriptCommand, state as OldScriptState, byref stk as Stack, byref i as integer)
 state.curargn = readstack(stk, i)
 state.ptr = readstack(stk, i - 1)
 node = *cast(ScriptCommand ptr, state.scrdata + state.ptr)
/' DIM cmdptr as ScriptCommand ptr = 
 node.kind = cmdptr->kind
 node.value = cmdptr->value
 node.argc = cmdptr->argc
'/
 i -= 2
END SUB

' Get the ScriptCommand .ptr for the parent node of the current node of a script.
' Inefficient!
' Warning, this may not be robust. Only tested with integer nodes.
FUNCTION command_parent_node(which_scrat as OldScriptState ptr) as integer
 DIM stkpos as integer = 0
 DIM state as OldScriptState = *which_scrat
 DIM node as ScriptCommand

 IF state.state = stnext THEN
  'point stkpos before the first argument (they extend above the stack)
  stkpos -= state.curargn
 END IF

 readstackcommand node, state, scrst, stkpos
 RETURN state.ptr
END FUNCTION

FUNCTION mathvariablename (value as integer, scrdat as ScriptData) as string
 'get a variable name from a "variable ID", used by setvariable/increment/decrement/for
 'locals (and args) numbered from 0
 IF value >= 0 THEN
  RETURN "global" & value
 ELSE
  RETURN localvariablename (-value - 1, scrdat)
 END IF
END FUNCTION

'This function returns a string describing where in the script the script interpreter currently is.
'It's a huge mess because it has to examine the interpreter stack, and the interpreter might
'be in many different states (I think this is buggy in some of them)
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

 DIM flowtype(15) as integer
 DIM flowbrakbrk(15) as integer
 DIM state as OldScriptState
 DIM scrinst as ScriptInst
 DIM node as ScriptCommand
 DIM lastnode as ScriptCommand

 flowtype(flowdo) = 0
 flowtype(flowreturn) = 1
 flowtype(flowif) = 3:     flowbrakbrk(flowif) = 1
 flowtype(flowthen) = 0
 flowtype(flowelse) = 0
 flowtype(flowfor) = 2:    flowbrakbrk(flowfor) = 4
 flowtype(flowwhile) = 2:  flowbrakbrk(flowwhile) = 1
 flowtype(flowbreak) = 1
 flowtype(flowcontinue) = 1
 flowtype(flowexit) = 1
 flowtype(flowexitreturn) = 1
 flowtype(flowswitch) = 3

 DIM stkbottom as integer = -(scrst.pos - scrst.bottom)  'pointer arithmetic seems to be 31-bit signed (breakage on negative diff)!
 DIM stkpos as integer = 0

 DIM wasscript as integer = nowscript
 DIM hideoverride as integer

 DIM cmd as string
 DIM hidearg as integer
 DIM outstr as string
 DIM argnum as integer

 memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
 memcpy(@(scrinst),@(scriptinsts(wasscript)),LEN(scriptinsts(wasscript)))
 node.kind = curcmd->kind
 node.value = curcmd->value
 node.argc = curcmd->argc
 memcpy(@(lastnode),@(node),LEN(ScriptCommand))

 'debug "state = " & state.state
 'debug "depth = " & state.depth
 'debug "kind = " & node.kind
 'debug "val = " & node.value
 'debug "argn = " & state.curargn
 'debug "argc = " & node.argc

 IF state.state = stdoarg THEN GOTO jmpdoarg  'Note: stdoarg no longer used
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

  cmd = scriptcmdname(node.kind, node.value, *scrinst.scr)
  hidearg = 0
  IF hideoverride THEN hidearg = -1: hideoverride = 0
  SELECT CASE node.kind
    CASE tynumber, tyglobal, tylocal
     outstr = cmd
     cmd = ""
     hidearg = -1
    CASE tyflow
     IF node.value < 0 ORELSE node.value > flowLAST THEN
      RETURN "CORRUPT SCRIPT DATA: flow " & node.value
     END IF
     hidearg = -3
     IF state.depth = 0 THEN cmd = scriptname(state.id)
     IF state.state = ststart THEN hidearg = -1

     IF flowtype(node.value) = 0 THEN IF node.argc = 0 THEN hidearg = -1: cmd += "()"
     IF flowtype(node.value) = 1 THEN hidearg = 0 ': IF state.curargn = 0 THEN cmd += ":"
     IF flowtype(node.value) = 2 THEN
      hidearg = 0
      'IF state.curargn = node.argc - 1 THEN hidearg = -1: cmd += "()"
      IF node.value = flowwhile AND state.curargn = 0 THEN hidearg = -1
     END IF
     IF node.value = flowif THEN
      hidearg = -1
      IF state.curargn > 0 AND state.curargn < node.argc THEN cmd += "()"
     END IF
     IF node.value = flowswitch THEN
      hidearg = -1
      IF state.curargn = 0 THEN
       cmd += ":"
      ELSE
       cmd += "(" & readstack(scrst, stkpos + 1) & ")"   ' ????
       IF state.curargn + 1 = node.argc THEN
        cmd += " else"
        'hack to replace the 'do' with 'else' (hspeak outputs a do instead of an else)
        IF LEN(outstr) > 1 THEN outstr = MID(outstr, 3)
        hidearg = -2
       ELSEIF state.curargn >= node.argc THEN
        'an extra step the stepper currently pauses on
       ELSEIF lastnode.kind = tyflow AND lastnode.value = flowdo THEN
        cmd += " case()"
       ELSE
        cmd += " case"
        IF state.curargn < node.argc THEN cmd += ":" ELSE cmd += "()"
       END IF
      END IF
     END IF
    CASE tymath
     IF node.value < 0 ORELSE node.value > mathLAST THEN
      RETURN "CORRUPT SCRIPT DATA: math " & node.value
     END IF
   END SELECT
   'debug "kind = " + STR(node.kind)
   'debug "cmd = " + cmd



   IF cmd <> "" THEN
'    IF outstr = "" THEN
'     outstr = cmd
'    ELSE

     IF hidearg = 0 THEN
      argnum = node.argc
      IF node.kind = tyflow ANDALSO flowbrakbrk(node.value) <> 0 THEN
       argnum = flowbrakbrk(node.value)
      END IF
      cmd += "("
      FOR i as integer = 1 TO state.curargn
       'setvariable, increment, decrement, for
       IF i = 1 ANDALSO ((node.kind = tymath AND node.value >= 16 AND node.value <= 18) _
                         ORELSE (node.kind = tyflow AND node.value = flowfor)) THEN
        cmd += mathvariablename(readstack(scrst, stkpos + i), *scrinst.scr)
       ELSE
        cmd += STR(readstack(scrst, stkpos + i))
       END IF
       IF i <> argnum THEN cmd += ","
      NEXT
      IF state.curargn >= argnum THEN cmd += ")"
      outstr = cmd & outstr
     ELSEIF hidearg = -3 THEN
      IF state.curargn >= node.argc THEN
       outstr = cmd & "() " & outstr
      ELSEIF (node.argc = 1) AND (state.curargn = 0) THEN
       outstr = cmd & ": " & outstr
      ELSE
       outstr = cmd & ":" & (state.curargn + 1) & "/" & node.argc & " " & outstr
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
    memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
    memcpy(@(scrinst),@(scriptinsts(wasscript)),LEN(scriptinsts(wasscript)))

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


   memcpy(@(lastnode),@(node),LEN(node))

   readstackcommand node, state, scrst, stkpos

   'debug "stkpos = " & stkpos

  jmpdoarg:

   'ditch arguments
   IF node.kind = tyflow AND node.value = flowswitch AND state.curargn > 0 THEN
    IF state.curargn >= node.argc THEN
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
   IF stkpos < stkbottom THEN scripterr("script debugger failed to read script state: stack underflow " & (stkpos - stkbottom), serrInfo): EXIT DO
 LOOP
 IF stkpos > stkbottom AND wasscript < 0 THEN scripterr("script debugger failed to read script state: stack garbage " & (stkpos - stkbottom), serrInfo)

 scriptstate = TRIM(outstr)
 'debug outstr
END FUNCTION
