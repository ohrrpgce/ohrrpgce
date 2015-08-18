'OHRRPGCE GAME
'(C) Copyright 1997-2013 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
' This module contains routines to do with HamsterSpeak which are (mostly)
' independent of the specific HS interpreter in use, or abstract between interpreters.
' Implementations of script commands are in game.bas and yetmore.bas, not here.


#include "config.bi"
#include "udts.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "allmodex.bi"
#include "common.bi"
#include "scriptcommands.bi"
#include "yetmore2.bi"
#include "scripting.bi"
#include "sliceedit.bi"
#include "string.bi" 'for format


'------------ Local functions -------------

DECLARE SUB freescripts (byval mem as integer)

'------------ Global variables ------------

'These have to be in the same module as dequeue_scripts for some reason; looks like a FB bug.
REDIM scrqFirst() as QueuedScript
REDIM scrqBackcompat() as QueuedScript
REDIM scrqLast() as QueuedScript

'--------- Module shared variables ---------

'Used by trigger_script
DIM SHARED trigger_script_failure as integer

DIM SHARED timeroverhead as double


'==========================================================================================
'                                   Triggering scripts
'==========================================================================================


SUB trigger_script (byval id as integer, byval double_trigger_check as bool, scripttype as string, trigger_loc as string, scrqueue() as QueuedScript, byval trigger as integer = plottrigger)
 'Add a script to one of the script queues, unless already inside the interpreter.
 'In that case, run immediately.
 'scrqueue should be one of the scrq* arrays
 'double_trigger_check: whether "no double-triggering" should take effect

 STATIC dummy_queued_script as QueuedScript

 IF insideinterpreter THEN
  DIM rsr as integer
  rsr = runscript(id, YES, double_trigger_check, scripttype, trigger)
  trigger_script_failure = (rsr <> 1)
  IF gam.script_log.enabled = NO THEN EXIT SUB

  'Can't call watched_script_triggered until after the trigger_script_args calls
  scriptinsts(nowscript).watched = YES
  scrat(nowscript).state = sttriggered
  last_queued_script = @dummy_queued_script
 ELSE
  REDIM PRESERVE scrqueue(-1 TO UBOUND(scrqueue) + 1)
  last_queued_script = @scrqueue(UBOUND(scrqueue))
 END IF

 WITH *last_queued_script
  IF trigger <> 0 THEN id = decodetrigger(id)
  .id = id
  .scripttype = scripttype
  .log_line = scriptname(id) & "("
  .trigger_loc = trigger_loc
  .double_trigger_check = double_trigger_check
  .argc = 0
 END WITH
END SUB

SUB trigger_script_arg (byval argno as integer, byval value as integer, byval argname as zstring ptr = NULL)
 'Set one of the args for a script that was just triggered
 'Note that after calling trigger_script, script queuing can be in three states:
 'inside interpreter, trigger_script_failure = NO
 '    triggered a script which started immediately
 'inside interpreter, trigger_script_failure = YES
 '    triggered a script which there was an error starting
 'not inside interpreter:
 '    queued a script, can now set the arguments

 IF insideinterpreter THEN
  IF trigger_script_failure = NO THEN
   setScriptArg argno, value
  END IF
  IF gam.script_log.enabled = NO THEN EXIT SUB
 END IF

 WITH *last_queued_script
  IF argno > UBOUND(.args) THEN fatalerror "trigger_script_arg: args queue overflow"
  .args(argno) = value
  .argc = large(.argc, argno + 1)
  IF gam.script_log.enabled THEN
   IF argno <> 0 THEN .log_line += ", "
   IF argname THEN .log_line += *argname + "="
   .log_line &= value
  END IF
 END WITH
END SUB

PRIVATE SUB run_queued_script (script as QueuedScript)
 DIM rsr as integer
 rsr = runscript(script.id, YES, script.double_trigger_check, script.scripttype, 0)
 IF rsr = 1 THEN
  FOR argno as integer = 0 TO script.argc - 1
   setScriptArg argno, script.args(argno)
  NEXT
 END IF

 IF gam.script_log.enabled THEN watched_script_triggered script
END SUB

SUB run_queued_scripts
 'Load the queued scripts into the interpreter.
 'We have to call runscript in the reverse order, because we build the stack up from bottom

 FOR i as integer = UBOUND(scrqFirst) TO 0 STEP -1
  run_queued_script(scrqFirst(i))
 NEXT
 FOR i as integer = 0 TO UBOUND(scrqBackcompat)
  run_queued_script(scrqBackcompat(i))
 NEXT
 FOR i as integer = UBOUND(scrqLast) TO 0 STEP -1
  run_queued_script(scrqLast(i))
 NEXT

 dequeue_scripts
END SUB

SUB dequeue_scripts
 'Wipe the script queues
 last_queued_script = NULL
 REDIM scrqFirst(-1 TO -1)
 REDIM scrqBackcompat(-1 TO -1)
 REDIM scrqLast(-1 TO -1)
END SUB


'==========================================================================================
'                                 Script Trigger Logging
'==========================================================================================


SUB start_script_trigger_log
 gam.script_log.enabled = YES
 safekill gam.script_log.filename
 DIM fh as integer = FREEFILE
 IF OPEN(gam.script_log.filename FOR APPEND AS #fh) THEN
  notification "Could not open " & gam.script_log.filename & ". Script logging disabled."
  EXIT SUB
 END IF
 gam.script_log.enabled = YES

 print #fh, "Script trigger log for " & getdisplayname(trimpath(sourcerpg)) & ", " & DATE & " " & TIME
 print #fh,
 print #fh, "Solid lines '|' show triggered scripts which have already started running but are"
 print #fh, "waiting or paused due to either another script which was triggered (line to the right)"
 print #fh, "or while waiting for a script they called (not shown)."
 print #fh, "Dotted lines ':' show triggered scripts which have no even had a chance to start."
 print #fh,
 print #fh, " Symbols in front of script names:"
 print #fh, "+ -- A script was triggered (queued), possibly also started, possibly also finished" 
 print #fh, "! -- As above, but triggered as a side effect of something the script above it did,"
 print #fh, "     such as running ""close menu"", interrupting that script."
 print #fh, "     (Note: ! is used only if the command didn't cause an implicit 'wait')"
 print #fh, "* -- A queued script was started, possibly also finished" 
 print #fh, "- -- A previously started script finished"
 print #fh,
 CLOSE #fh
END SUB

SUB script_log_out (text as string)
 IF gam.script_log.enabled = NO THEN EXIT SUB
 DIM fh as integer = FREEFILE
 IF OPEN(gam.script_log.filename FOR APPEND AS #fh) THEN
  gam.script_log.enabled = NO
  EXIT SUB
 END IF
 #IFDEF __FB_WIN32__
  'FB opens files in binary mode...
  replacestr text, !"\n", !"\r\n"
 #ENDIF

 print #fh, text;
 CLOSE #fh
 gam.script_log.output_flag = YES
END SUB

FUNCTION script_log_indent (byval upto as integer = -1, byval spaces as integer = 11) as string
 DIM indent as string = SPACE(spaces)
 IF upto = -1 THEN upto = nowscript - 1
 FOR i as integer = 0 TO upto
  WITH scriptinsts(i)
   IF .watched THEN
    IF .started THEN
     indent &= "| "
    ELSE
     indent &= ": "
    END IF
   END IF
  END WITH
 NEXT
 RETURN indent
END FUNCTION

'Called after runscript when running a script which should be watched
SUB watched_script_triggered(script as QueuedScript)
 scriptinsts(nowscript).watched = YES
 IF gam.script_log.last_logged > -1 ANDALSO scriptinsts(gam.script_log.last_logged).started = NO THEN
  script_log_out " (queued)"
 END IF

 DIM logline as string
 logline = !"\n" & script_log_indent()
 IF insideinterpreter THEN
  IF nowscript >= 1 ANDALSO scrat(nowscript - 1).state < 0 THEN
   'The previous script was suspended, therefore this script was triggered as
   'a side effect of something that script did, such as activate an NPC
   logline &= "!"
  ELSE
   'Called normally
   logline &= "\"
  END IF
 ELSE
  'Triggered normally
  logline &= "+"
 END IF

 logline &= script.log_line & ") " & script.scripttype & " script"
 IF LEN(script.trigger_loc) THEN
  logline &= ", " & script.trigger_loc
 END IF
 script_log_out logline

 gam.script_log.last_logged = nowscript

END SUB

'nowscript has been started and resumed and has .watched = YES
SUB watched_script_resumed
 IF gam.script_log.last_logged = nowscript THEN
  'nothing
 ELSEIF scriptinsts(nowscript).started THEN
  'also nothing
 ELSE
  script_log_out !"\n" & script_log_indent() & "*" & scriptname(scriptinsts(nowscript).id) & " started"
  gam.script_log.last_logged = nowscript
 END IF
 scriptinsts(nowscript).started = YES
END SUB

'Called right before the current script terminates and has .watched = YES
SUB watched_script_finished
 DIM logline as string
 IF gam.script_log.last_logged = nowscript THEN
  script_log_out " ... finished"
 ELSE
  script_log_out !"\n" & script_log_indent() & "-" & scriptname(scriptinsts(nowscript).id) & " finished"
 END IF

 gam.script_log.last_logged = -1
END SUB

'Call each tick if script logging is enabled
SUB script_log_tick
 WITH gam.script_log
  DIM doprint as integer = NO
  IF .output_flag THEN doprint = YES

  DIM wait_msg as string = ""
  IF nowscript > -1 THEN
   WITH scriptinsts(nowscript)
    IF .waiting = waitingOnCmd THEN
     wait_msg = "waiting on " & commandname(.curvalue) & " in " & scriptname(.id)
    ELSEIF .waiting = waitingOnTick THEN
     wait_msg = "waiting " & .waitarg & " ticks in " & scriptname(.id)
    END IF
   END WITH
   IF .last_wait_msg <> wait_msg THEN
    .last_wait_msg = wait_msg
    .wait_msg_repeats = 0
   END If
   .wait_msg_repeats += 1
   IF .wait_msg_repeats <= 3 THEN doprint = YES
   IF .wait_msg_repeats = 3 THEN wait_msg = "..."
  END IF

  IF doprint THEN
 '  script_log_out !"\n" & script_log_indent(nowscript) & "   <<tick " & .tick & ">>"
   DIM logline as string
   logline =  !"\ntick " & LEFT(RIGHT(STR(.tick), 5) & "     ", 6) & script_log_indent(nowscript, 0)
   IF LEN(wait_msg) THEN logline &= "     (" & wait_msg & ")"
   script_log_out logline
   .output_flag = NO

   .last_logged = -1
  END IF
 END WITH
END SUB


'==========================================================================================
'                                   Fibre/Script Control
'==========================================================================================


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
   IF .scr <> NULL THEN deref_script(.scr)
  END WITH
  nowscript -= 1
 WEND
 gam.script_log.last_logged = -1

 'Go back a script, let functiondone handle the script exit
 nowscript += 1
 setstackposition(scrst, scrat(nowscript).stackbase)

END SUB

SUB killallscripts
 'this kills all running scripts.
 'for use in cases of massive errors, quiting to titlescreen or loading a game.

 'Hack, see explanation in killscriptthread
 IF nowscript >= 0 THEN scrat(nowscript).state = stexit

 FOR i as integer = nowscript TO 0 STEP -1
  IF scrat(i).scr <> NULL THEN deref_script(scrat(i).scr)
 NEXT
 nowscript = -1
 gam.script_log.last_logged = -1

 setstackposition(scrst, 0)

 dequeue_scripts
END SUB

SUB resetinterpreter
'unload all scripts and wipe interpreter state. use when quitting the game.

 killallscripts

#IFDEF SCRIPTPROFILE
 print_script_profiling
#ENDIF

 IF numloadedscr > 0 THEN freescripts(0)
END SUB

' The current script fibre starts waiting due to the current command, halting execution.
' When forcing a script to wait for an 'external' reason, use script_start_waiting_ticks instead
SUB script_start_waiting(waitarg1 as integer = 0, waitarg2 as integer = 0)
 IF insideinterpreter = NO THEN scripterr "script_start_waiting called outside interpreter", serrBug
 WITH scriptinsts(nowscript)
  'debug commandname(curcmd->value) & ": script_start_waiting(" & waitarg1 & ", " & waitarg2 & ") on scriptinsts(" & nowscript & ") which is " & scriptname(.id)
  IF scrat(nowscript).state <> streturn THEN
   scripterr "script_start_waiting called outside of command handler", serrBug
  END IF
  .waiting = waitingOnCmd
  .waitarg = waitarg1
  .waitarg2 = waitarg2
 END WITH
 scrat(nowscript).state = stwait
END SUB

' Cause a script fibre to start waiting for some number of ticks.
' Unlike script_start_waiting this can be called from outside sfunctions.
' This is NOT the implementation of the wait(x) command, but it has the same effect
' whichscript is scriptinsts() index.
SUB script_start_waiting_ticks(whichscript as integer, ticks as integer)
 WITH scriptinsts(whichscript)
  IF .started THEN
   scripterr "FIXME: script_start_waiting_ticks not tested on .started scripts", serrBug
  END IF
  .waiting = waitingOnTick
  .waitarg = ticks
  .waitarg2 = 0
 END WITH
 ' Preserve value of scrat(whichscript).state
END SUB

' Current script allowed to continue.
' Can set the return value of a command if waitingOnCmd
SUB script_stop_waiting(returnval as integer = 0)
 WITH scriptinsts(nowscript)
  IF .waiting = waitingOnNothing THEN
   scripterr "script_stop_waiting: script isn't waiting", serrBug
  END IF

  IF .waiting = waitingOnTick AND returnval <> 0 THEN
   scripterr "script_stop_waiting: can't set a return value", serrBug
  END IF
  IF .waiting = waitingOnCmd THEN
   WITH scrat(nowscript)
    'debug "script_stop_waiting(" & returnval & ") on scriptinsts(" & nowscript & ") which is " & scriptname(.id)
    IF .state <> stwait THEN
     scripterr "script_stop_waiting: unexpected scrat().state = " & .state, serrBug
    ELSE
     .state = streturn
     scriptret = returnval
    END IF
   END WITH
  END IF
  .waiting = waitingOnNothing
 END WITH
END SUB


'==========================================================================================
'                            Loading/Reloading/Freeing scripts
'==========================================================================================


FUNCTION runscript (byval id as integer, byval newcall as integer, byval double_trigger_check as bool, byval scripttype as zstring ptr, byval trigger as integer) as integer
'newcall: whether his script is triggered rather than called from a script
'double_trigger_check: whether "no double-triggering" should take effect

DIM n as integer
IF trigger <> 0 THEN n = decodetrigger(id) ELSE n = id

IF n = 0 THEN
 runscript = 2 '--quiet failure (though decodetrigger might have shown a scripterr)
 EXIT FUNCTION
END IF

DIM index as integer = nowscript + 1

IF index >= maxScriptRunning THEN
 runscript = 0 '--error
 scripterr "failed to load " + *scripttype + " script " & n & " " & scriptname(n) & ", interpreter overloaded", serrError
 EXIT FUNCTION
END IF

IF double_trigger_check AND index > 0 THEN
 IF n = scriptinsts(index - 1).id AND readbit(gen(), genBits, 10) = 0 THEN
  'fail quietly
  '--scripterr "script " & n & " is already running"
  runscript = 2 '--quiet failure
  EXIT FUNCTION
 END IF
END IF

'--store current command data in scriptinsts (used outside of the inner interpreter)
IF nowscript >= 0 THEN
 WITH scriptinsts(nowscript)
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH
END IF

WITH scriptinsts(index)
 '-- Load the script (or return the reference if already loaded)
 .scr = loadscript(n)
 IF .scr = NULL THEN
  '--failed to load
  runscript = 0'--error
  scripterr "Failed to load " + *scripttype + " script " & n & " " & scriptname(n), serrError
  EXIT FUNCTION
 END IF
 .scr->totaluse += 1
 scriptctr += 1
 .scr->lastuse = scriptctr
 'increment refcount once loading is successful

 .id = n
 .watched = NO
 .started = NO
 .waiting = waitingOnNothing
 .waitarg = 0
 .waitarg2 = 0

 'This is not needed, but clears garbage values to ease debugging
 .curkind = -1
 .curvalue = -1
 .curargc = -1

 DIM errstr as zstring ptr = oldscriptstate_init(index, .scr)
 IF errstr <> NULL THEN
  scripterr "failed to load " + *scripttype + " script " & n & " " & scriptname(n) & ", " & *errstr, serrError
  RETURN 0 '--error
 END IF

 '--suspend the previous script
 IF newcall AND index > 0 THEN
  scrat(index - 1).state *= -1
 END IF

 '--we are successful, so now its safe to increment this
 .scr->refcount += 1
 nowscript += 1
 IF .scr->refcount = 1 THEN
  'Removed from unused scripts cache
  scriptcachemem -= .scr->size
 END IF

 'debug "running " & .id & " " & scriptname(.id) & ", parent = " & .scr->parent & " totaluse = " & .scr->totaluse & " refc = " & .scr->refcount & " lastuse = " & .scr->lastuse
END WITH

#IFDEF SCRIPTPROFILE
IF insideinterpreter THEN 'we have nowscript > 0
 TIMER_STOP(scriptinsts(nowscript - 1).scr->totaltime)
 scriptinsts(nowscript).scr->entered += 1
 TIMER_START(scriptinsts(nowscript).scr->totaltime)
END IF
#ENDIF

RETURN 1 '--success

END FUNCTION

FUNCTION loadscript (byval n as uinteger) as ScriptData ptr
 '-- script() is a hashtable with doubly linked lists as buckets, storing the loaded scripts

 DIM as ScriptData Ptr scrnode = script(n MOD scriptTableSize)
 WHILE scrnode
  IF scrnode->id = n THEN RETURN scrnode
  scrnode = scrnode->next
 WEND

 'debug "loadscript(" & n & " " & scriptname(n) & ")"

 DIM thisscr as ScriptData ptr
 DIM shortvar as short

 '--load the script from file
 DIM scriptfile as string = tmpdir & n & ".hsz"
 IF NOT isfile(scriptfile) THEN
  scriptfile = tmpdir & n & ".hsx"
  IF NOT isfile(scriptfile) THEN
   '--because TMC once suggested that preunlumping the .hsp lump would be a good way to reduce (SoJ) loading time
   scriptfile = workingdir & SLASH & n & ".hsx"
   IF NOT isfile(scriptfile) THEN
    scripterr "script " & n & " " & scriptname(n) & " does not exist", serrError
    RETURN NULL
   END IF
  END IF
 END IF

 DIM f as integer = FREEFILE
 OPEN scriptfile FOR BINARY as #f

 'minimum length of a valid 16-bit .hsx
 IF LOF(f) < 10 THEN
  scripterr "script " & n & " corrupt (too short: " & LOF(f) & " bytes)", serrError
  CLOSE #f
  RETURN NULL
 END IF

 thisscr = callocate(sizeof(ScriptData))
 WITH *thisscr

  GET #f, 1, shortvar
  DIM skip as integer = shortvar

  IF skip < 4 THEN
   scripterr "script " & n & " is corrupt (header length " & skip & ")", serrError
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  'Note that there is no check for the header being longer than expected. Optional
  'fields may be added to the end of the header; if they are mandatory the version number
  'should be incremented.

  GET #f, 3, shortvar
  'some HSX files seem to have an illegal negative number of variables
  .vars = shortvar
  .vars = bound(.vars, 0, 256)
 
  IF skip >= 6 THEN
   GET #f, 5, shortvar
   .args = bound(shortvar, 0, .vars)
  ELSE
   .args = 999
  END IF

  DIM scrformat as integer
  IF skip >= 8 THEN
   GET #f, 7, shortvar
   scrformat = shortvar
  ELSE
   scrformat = 0
  END IF
  IF scrformat > CURRENT_HSZ_VERSION THEN
   scripterr "script " & n & " is in an unsupported format", serrError
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF
  DIM wordsize as integer
  IF scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2

  IF skip >= 12 THEN
   GET #f, 9, .strtable
   IF .strtable THEN .strtable = (.strtable - skip) \ wordsize
  ELSEIF skip = 10 THEN
   GET #f, 9, shortvar
   IF shortvar THEN .strtable = (shortvar - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  IF skip >= 14 THEN
   GET #f, 13, shortvar
   .parent = shortvar
  ELSE
   .parent = 0
  END IF
  IF skip >= 16 THEN
   GET #f, 15, shortvar
   .nestdepth = shortvar
   IF .nestdepth > maxScriptNesting THEN
    scripterr "Corrupt or unsupported script data with nestdepth=" & .nestdepth & "; should be impossible", serrBug
   END IF
  ELSE
   .nestdepth = 0
  END IF
  IF skip >= 18 THEN
   GET #f, 17, shortvar
   .nonlocals = shortvar
  ELSE
   .nonlocals = 0
  END IF


  'set an arbitrary max script buffer size (scriptmemMax in const.bi), individual scripts must also obey
  .size = (LOF(f) - skip) \ wordsize
  IF .size > scriptmemMax THEN
   scripterr "Script " & n & " " & scriptname(n) & " exceeds maximum size by " & .size * 100 \ scriptmemMax - 99 & "%", serrError
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  IF .strtable < 0 OR .strtable > .size THEN
   scripterr "Script " & n & " corrupt; bad string table offset", serrError
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  .ptr = allocate(.size * sizeof(integer))
  IF .ptr = 0 THEN
   scripterr "Could not allocate memory to load script", serrError
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  IF wordsize = 2 THEN
   FOR i as integer = skip TO LOF(f) - wordsize STEP wordsize
    GET #f, 1 + i, shortvar
    .ptr[(i - skip) \ 2] = shortvar
   NEXT
  ELSE
   GET #f, skip + 1, *.ptr, .size
  END IF
  CLOSE #f

  'Sanity check: root node is a do()
  IF .size < 3 ORELSE (.ptr[0] <> 2 OR .ptr[1] <> 0 OR .ptr[2] < 0) THEN
   scripterr "Script " & n & " corrupt; does no start with do()", serrError
   deallocate(thisscr)
   RETURN NULL
  END IF

  .id = n
  .refcount = 0
  .totaluse = 0
  .lastuse = 0
  .totaltime = 0.0
  .entered = 0
  numloadedscr += 1
  totalscrmem += .size
  scriptcachemem += .size  'Has refcount 0, up to caller to remove from cache
 END WITH

 'append to front of doubly linked list
 DIM as ScriptData Ptr Ptr scrnodeptr = @script(n MOD scriptTableSize)
 IF *scrnodeptr THEN
  'already a script there
  (*scrnodeptr)->backptr = @thisscr->next
 END IF
 thisscr->backptr = scrnodeptr 'this is for convenience of easier deleting (in freescripts)
 thisscr->next = *scrnodeptr
 *scrnodeptr = thisscr

 RETURN thisscr
END FUNCTION

SUB delete_scriptdata (byval scriptd as ScriptData ptr)
 WITH *scriptd
  IF .refcount THEN
   fatalerror "delete_scriptdata: nonzero refcount"
   EXIT SUB
  END IF

  'debug "deallocating " & .id & " " & scriptname(ABS(.id)) & " size " & .size
  totalscrmem -= .size
  scriptcachemem -= .size
  numloadedscr -= 1
  deallocate(.ptr)
  IF .next THEN
   .next->backptr = .backptr
  END IF
  *.backptr = .next
 END WITH

 deallocate(scriptd)
END SUB

'Dereference script pointer
SUB deref_script(script as ScriptData ptr)
 script->refcount -= 1
 IF script->refcount = 0 THEN
  scriptcachemem += script->size
  IF scriptcachemem > scriptmemMax THEN
   'Evicting stuff from the script cache is probably pointless, but we've already got it,
   'and it may be useful for the new script interpreter...
   freescripts(scriptmemMax * 0.75)
  END IF
 END IF
END SUB

TYPE ScriptListElmt
 p as ScriptData ptr
 score as integer
END TYPE

'Iterate over all loaded scripts, sort them in descending order according to score
'returned by the callback, and return number of scripts in numscripts
'(LRUlist is dynamic)
SUB sort_scripts(LRUlist() as ScriptListElmt, byref numscripts as integer, scorefunc as function(scr as ScriptData) as integer)
 DIM j as integer
 numscripts = 0
 REDIM LRUlist(-1 TO -1)
 FOR i as integer = 0 TO UBOUND(script)
  DIM scrp as ScriptData Ptr = script(i)
  WHILE scrp
   DIM score as integer = scorefunc(*scrp)
   REDIM PRESERVE LRUlist(-1 TO numscripts)
   FOR j = numscripts - 1 TO 0 STEP -1
    IF score >= LRUlist(j).score THEN EXIT FOR
    LRUlist(j + 1).p = LRUlist(j).p
    LRUlist(j + 1).score = LRUlist(j).score
   NEXT
   LRUlist(j + 1).p = scrp
   LRUlist(j + 1).score = score
   numscripts += 1
   scrp = scrp->next
  WEND
 NEXT
END SUB

FUNCTION freescripts_script_scorer(byref script as ScriptData) as integer
 'this formula has only been given some testing, and doesn't do all that well
 DIM score as integer
 IF script.refcount THEN RETURN 1000000000
 score = script.lastuse - scriptctr
 score = iif(score > -400, score, -400) _
       + iif(script.totaluse < 100, script.totaluse, iif(script.totaluse < 1700, 94 + script.totaluse\16, 200)) _
       - script.size \ (scriptmemMax \ 1024)
 IF script.id < 0 THEN
  'Stale script
  score = -1000000000
 END IF
 RETURN score
END FUNCTION

'Two uses: freescripts(0) frees all scripts, otherwise
'frees unused loaded scripts until at least scriptcachemem <= mem (measured in 4-byte ints) (probably a lot lower)
SUB freescripts (byval mem as integer)
 REDIM LRUlist() as ScriptListElmt
 DIM numscripts as integer

 'give each script a score (the lower, the more likely to throw) and sort them
 'this is roughly a least recently used list
 sort_scripts LRUlist(), numscripts, @freescripts_script_scorer

 FOR i as integer = 0 TO numscripts - 1
  IF mem = 0 THEN
   delete_scriptdata LRUlist(i).p
  ELSE
   IF LRUlist(i).p->refcount <> 0 THEN EXIT SUB
   IF scriptcachemem <= mem THEN EXIT SUB
   'debug "unloading script " & scriptname(ABS(LRUlist(i).p->id)) & " refcount " & LRUlist(i).p->refcount
   delete_scriptdata LRUlist(i).p
  END IF
 NEXT
END SUB

SUB reloadscript (si as ScriptInst, oss as OldScriptState, byval updatestats as bool = YES)
 WITH si
  IF .scr = NULL THEN
   .scr = loadscript(.id)
   IF .scr = NULL THEN killallscripts: EXIT SUB
   oss.scr = .scr
   oss.scrdata = .scr->ptr
   .scr->refcount += 1
   IF updatestats THEN .scr->totaluse += 1
  END IF
  IF updatestats THEN
   'a rather hackish and not very good attempt to give .lastuse a qualitative use
   'instead of just for sorting; a priority queue is probably a much better solution
   IF .scr->lastuse <= scriptctr - 10 THEN
    scriptctr += 1
    .scr->lastuse = scriptctr
   END IF
  END IF
 END WITH
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
     debuginfo "not reloading script " & scriptname(ABS(.id)) & " because it's in use: refcount=" & .refcount
     ' Negate the ID number. This will prevent this script data from being used when starting a new script.
     ' It won't be automatically evicted from the cache, but that's ok.
     .id = ABS(.id) * -1
    END IF
   END WITH

   scrp = nextp
  WEND
 NEXT

 IF unfreeable THEN
  notification unfreeable & " scripts are in use and couldn't be freed (see g_debug.txt for details)"
 END IF
 ' Set changed to NO because there's nothing the user can do by hitting "Force reload scripts";
 ' new instances of the script use the new data already
 lump_reloading.hsp.changed = NO

 'Cause the cache in scriptname() (and also in commandname()) to be dropped
 game_unique_id = STR(randint(INT_MAX))
END SUB


'==========================================================================================
'                                    Script profiling
'==========================================================================================


FUNCTION profiling_script_scorer(byref script as ScriptData) as integer
 'sort by total time
 script.totaltime -= script.entered * timeroverhead
 RETURN script.totaltime * -10000
END FUNCTION

'Print profiling information on scripts to g_debug.txt
SUB print_script_profiling
 FOR i as integer = 0 TO 999
  timeroverhead -= TIMER
  timeroverhead += TIMER
 NEXT
 timeroverhead /= 1000

 REDIM LRUlist() as ScriptListElmt
 DIM numscripts as integer

 'give each script a score (the lower, the more likely to throw) and sort them
 'this is roughly a least recently used list
 sort_scripts LRUlist(), numscripts, @profiling_script_scorer

 DIM entiretime as double
 FOR i as integer = 0 TO numscripts - 1
  entiretime += LRUlist(i).p->totaltime
 NEXT

 debug "script profiling information:"
 debug "#switches is the number of times that the interpreter switched to that script"
 debug "(switching time is relatively neglible and included to help determine"
 debug "calls to other scripts, which are more expensive)"
 debug "Total time recorded in interpreter: " & format(entiretime, "0.000") & "sec   (timer overhead = " & format(timeroverhead*1000000, "0.00") & "us)"
 debug " %time        time    time/call      #calls   #switches  script name"
 FOR i as integer = 0 TO numscripts - 1
 ' debug i & ": " & LRUlist(i).p & " score = " & LRUlist(i).score
  WITH *LRUlist(i).p
   debug " " & format(100 * .totaltime / entiretime, "00.00") _
       & RIGHT(SPACE(9) & format(.totaltime*1000, "0"), 10) & "ms" _
       & RIGHT(SPACE(10) & format(.totaltime*1000000/.totaluse, "0"), 11) & "us" _
       & RIGHT(SPACE(11) & .totaluse, 12) _
       & RIGHT(SPACE(11) & .entered, 12) _
       & "  " & scriptname(ABS(.id)) '& "  " & format(1000*(.totaltime + .entered * timeroverhead), "0.00")

 '  debug "id = " & .id & " " & scriptname(ABS(.id))
 '  debug "refcount = " & .refcount
 '  debug "totaluse = " & .totaluse
 '  debug "lastuse = " & .lastuse
 '  debug "size = " & .size
  END WITH
 NEXT
END SUB


'==========================================================================================
'                                    Other Interfaces
'==========================================================================================


FUNCTION commandname (byval id as integer) as string
 'cmd_default_names array
#include "scrcommands.bi"

 STATIC cache(32) as IntStrPair
 DIM as string ret
 ret = search_string_cache(cache(), id, game_unique_id)
 IF ret <> "" THEN RETURN ret
 IF id >= 0 AND id <= UBOUND(cmd_default_names) THEN ret = cmd_default_names(id)
 IF ret = "" THEN ret = "cmd" & id

 DIM as short headersz, formatv, records, offset

 '--could check workingdir as well like we do in runscript; but doesn't seem necessary
 DIM fh as integer = FREEFILE
 IF OPEN(tmpdir + "commands.bin" FOR BINARY ACCESS READ as fh) THEN
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 GET #fh, , headersz
 GET #fh, , formatv
 GET #fh, , records

 IF formatv > 0 OR id < 0 OR id >= records THEN
  CLOSE fh
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 GET #fh, 1 + headersz + 2 * id, offset

 IF offset = 0 THEN
  CLOSE fh
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 DIM rec(25) as short
 GET #fh, 1 + offset + 2, rec()
 ret = readbinstring(rec(), 0, 50)
 CLOSE fh
 add_string_cache cache(), id, ret
 RETURN ret
END FUNCTION

'Returns script command name if inside a script command handler
FUNCTION current_command_name() as string
 IF insideinterpreter = NO ORELSE curcmd->kind <> tyfunct THEN
  RETURN "(no command)"
 END IF
 RETURN commandname(curcmd->value)
END FUNCTION

'This is called for error messages occurring inside scripts, and gives a description of the current context
FUNCTION interpreter_context_name() as string
 IF insideinterpreter = NO THEN debugc errPromptBug, "script_context_name called outside interpreter"
 IF curcmd->kind = tyfunct THEN
  RETURN commandname(curcmd->value) + ": "
 ELSEIF insideinterpreter THEN
  RETURN ""
 END IF
END FUNCTION

FUNCTION script_call_chain (byval trim_front as integer = YES) as string
 IF nowscript < 0 THEN
  RETURN "Funny... no scripts running!"
 END IF

 DIM scriptlocation as string
 scriptlocation = scriptname(scriptinsts(nowscript).id)
 FOR i as integer = nowscript - 1 TO 0 STEP -1
  IF scrat(i).state < 0 THEN EXIT FOR 'suspended: not part of the call chain
  scriptlocation = scriptname(scriptinsts(i).id) + " -> " + scriptlocation
 NEXT
 IF trim_front AND LEN(scriptlocation) > 150 THEN scriptlocation = " ..." + RIGHT(scriptlocation, 150)
 RETURN "  Call chain (current script last):" + CHR(10) + scriptlocation
END FUNCTION


'==========================================================================================
'                                   Menus and Dialogues
'==========================================================================================


'For errorlevel scheme, see scriptErrEnum in const.bi
SUB scripterr (e as string, byval errorlevel as scriptErrEnum = serrBadOp)
 'mechanism to handle scriptwatch throwing errors
 STATIC as integer recursivecall

 STATIC as integer ignorelist()
 initialize_static_dynamic_array(ignorelist)

 DIM as string errtext()
 DIM as integer scriptcmdhash

 'err_suppress_lvl is always at least serrIgnore
 IF errorlevel <= err_suppress_lvl THEN EXIT SUB

 debug "Scripterr(" & errorlevel & "): " + e

 IF nowscript >= 0 THEN
  scriptcmdhash = scrat(nowscript).id * 100000 + scrat(nowscript).ptr * 10 + scrat(nowscript).depth
  IF int_array_find(ignorelist(), scriptcmdhash) <> -1 THEN EXIT SUB
 END IF

 recursivecall += 1

 IF errorlevel = serrError THEN e = "Script data may be corrupt or unsupported:" + CHR(10) + e
 IF errorlevel >= serrBug THEN e = "PLEASE REPORT THIS POSSIBLE ENGINE BUG" + CHR(10) + e

 e = e + CHR(10) + CHR(10) + script_call_chain
 split(wordwrap(e, 38), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
 ClearMenuData menu
 menu.anchor.y = -1
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Ignore"
 append_menu_item menu, "Don't display any more script errors"
 'append_menu_item menu, "Set error suppression level to " & errorlevel
 append_menu_item menu, "Stop this script"
 append_menu_item menu, "Suppress errors from this source"
 append_menu_item menu, "Exit game (without saving)"
 append_menu_item menu, "Enter slice debugger"
 IF recursivecall = 1 THEN  'don't reenter the debugger if possibly already inside!
  IF scrwatch <> 0 THEN
   append_menu_item menu, "Return to script debugger"
   state.pt = 6
  ELSE
   append_menu_item menu, "Enter script debugger"
  END IF
  IF running_as_slave THEN append_menu_item menu, "Reload scripts"
 END IF

 state.active = YES
 init_menu_state state, menu

 'Modify master() because the script debugger or other menus may setpal 
 REDIM remember_master(255) as RGBcolor
 FOR i as integer = 0 TO 255
  remember_master(i) = master(i)
 NEXT
 loadpalette master(), gam.current_master_palette
 setpal master()

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN 'ignore
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_scripterr")

  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0 'ignore
     EXIT DO
    CASE 1 'hide errors (but not engine bugs)
     err_suppress_lvl = serrError
     EXIT DO
    ' CASE 2 'hide some errors
    '  err_suppress_lvl = errorlevel
    CASE 2
     killscriptthread
     EXIT DO
    CASE 3 'hide errors from this command
     int_array_append(ignorelist(), scriptcmdhash)
     EXIT DO
    CASE 4
     debug "scripterr: User opted to quit"
     exitprogram NO
    CASE 5
     slice_editor SliceTable.Root
    CASE 6 'Script debugger
     scrwatch = 2
     scriptwatcher scrwatch, 0 'clean mode, script state view mode
    CASE 7 'reload scripts
     reload_scripts
     EXIT DO
   END SELECT
  END IF
  
  usemenu state

  clearpage vpage

  centerbox 160, 12, 310, 15, 3, vpage
  textcolor uilook(uiText), 0
  IF errorlevel >= serrBug THEN
   printstr "Impossible error/engine bug!", 160 - 28*4, 7, vpage
  ELSEIF errorlevel >= serrBound THEN
   printstr "Script Error!", 160 - 13*4, 7, vpage
  ELSEIF errorlevel >= serrWarn THEN
   printstr "Script Warning", 160 - 14*4, 7, vpage
  ELSEIF errorlevel = serrInfo THEN
   printstr "Script Diagnostic", 160 - 17*4, 7, vpage
  END IF

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 6 THEN
   textcolor uilook(uiSelectedItem), 0 
   printstr "The debugger is a usability train-wreck!", 0, vpages(vpage)->h - 16, vpage
   printstr "Press F1 inside the debugger to see help", 0, vpages(vpage)->h - 8, vpage
  END IF
  setvispage vpage

  IF autotestmode THEN
    write_checkpoint
    exitprogram NO, 1
  END IF

  dowait
 LOOP
 ClearMenuData menu
 setkeys
 recursivecall -= 1

 FOR i as integer = 0 TO 255
  master(i) = remember_master(i)
 NEXT
 setpal master()
 next_interpreter_check_time = TIMER + scriptCheckDelay

 'Note: when we resume after a script error, the keyboard state changes, which might break a script
 'Not worth worrying about this.
END SUB

FUNCTION script_interrupt () as integer
 DIM as integer ret = NO
 DIM as string errtext()
 DIM as string msg

 msg = "A script may be stuck in an infinite loop. Press F1 for more help" + CHR(10) + CHR(10) + script_call_chain
 debug script_call_chain(NO)
 split(wordwrap(msg, 38), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
 ClearMenuData menu
 menu.anchor.y = -1
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Continue running"
 'append_menu_item menu, "Exit the top-most script"
 append_menu_item menu, "Stop the script fibre"
 append_menu_item menu, "Stop all scripts"
 append_menu_item menu, "Exit game"
 append_menu_item menu, "Enter script debugger"
 IF running_as_slave THEN append_menu_item menu, "Reload scripts"

 state.active = YES
 init_menu_state state, menu

 'Modify master() because the script debugger or other menus may setpal 
 REDIM remember_master(255) as RGBcolor
 FOR i as integer = 0 TO 255
  remember_master(i) = master(i)
 NEXT
 loadpalette master(), gam.current_master_palette
 setpal master()

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN 'continue
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_script_interrupt")

  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0 'continue
     ret = NO
    'CASE 1 'exit topmost  ... probably not too helpful
    ' killtopscript
    ' ret = YES
    CASE 1 'exit whole 'thread'
     killscriptthread
     ret = YES
    CASE 2 'kill everything
     killallscripts
     ret = YES
    CASE 3 'die
     debug "script_interrupt: User opted to quit"
     exitprogram NO
    CASE 4 'script debugger
     scrwatch = 2
     scriptwatcher scrwatch, 0 'clean mode, script state view mode
     ret = YES
    CASE 5 'reload scripts
     reload_scripts
     ret = NO
   END SELECT
   EXIT DO
  END IF
  
  usemenu state

  clearpage vpage

  centerbox 160, 12, 310, 15, 3, vpage
  textcolor uilook(uiText), 0
  printstr "A script is stuck", 160 - 17*4, 7, vpage

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 4 THEN
   textcolor uilook(uiSelectedItem), 0 
   printstr "The debugger is a usability train-wreck!", 0, 184, vpage
   printstr "Press F1 inside the debugger to see help", 0, 192, vpage
  END IF
  setvispage vpage

  dowait
 LOOP
 ClearMenuData menu
 setkeys

 FOR i as integer = 0 TO 255
  master(i) = remember_master(i)
 NEXT
 setpal master()
 clearpage vpage
 setvispage vpage
 next_interpreter_check_time = TIMER + scriptCheckDelay

 'Note: when we resume after a script interruption, the keyboard state changes, which might break a script
 'Not worth worrying about this.
 RETURN ret
END FUNCTION
