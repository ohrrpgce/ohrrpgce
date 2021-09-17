'OHRRPGCE GAME
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
' This module contains routines to do with HamsterSpeak which are (mostly)
' independent of the specific HS interpreter in use, or abstract between interpreters.
' Implementations of script commands are in scriptcommands.bas, not here.


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
#include "loading.bi"
#include "sliceedit.bi"
#include "string.bi" 'for format


'------------ Local functions -------------

DECLARE SUB freescripts (byval mem as integer)
DECLARE FUNCTION loadscript_open_script(n as integer, expect_exists as bool = YES) as integer
DECLARE FUNCTION loadscript_read_header(fh as integer, id as integer) as ScriptData ptr
DECLARE FUNCTION loadscript_read_data(header as ScriptData ptr, fh as integer) as bool
DECLARE FUNCTION scriptcache_find(id as integer) as ScriptData ptr
DECLARE SUB scriptcache_add(id as integer, thisscr as ScriptData ptr)

'------------ Global variables ------------

DEFINE_VECTOR_OF_TYPE(ScriptFibre ptr, ScriptFibre_ptr)

'--------- Module shared variables ---------

'Used by trigger_script
DIM SHARED trigger_script_failure as bool
DIM SHARED last_queued_script as ScriptFibre ptr

DIM SHARED timeroverhead as double


'==========================================================================================
'                                   Triggering scripts
'==========================================================================================


SUB trigger_script (id as integer, numargs as integer, double_trigger_check as bool, scripttype as string, trigger_loc as string, byref fibregroup as ScriptFibre ptr vector, priority as integer = 0)
 'Add a script to one of the script queues, unless already inside the interpreter.
 'In that case, run immediately.
 'After calling this SUB, call trigger_script_arg zero or more times to set the arguments.
 'All arguments must be given, in the right order.
 '
 'id:           either a script ID or a trigger number
 'numargs:      the number of arguments that will be provided
 'double_trigger_check:  whether "no double-triggering" should take effect
 'scripttype:   type of the script (used for debugging/tracing), eg "autorun"
 'trigger_loc:  specific script trigger cause, eg "map 5"
 'fibregroup:   a vector of ScriptFibre ptrs, usually mainFibreGroup

 STATIC dummy_queued_script as ScriptFibre

 IF insideinterpreter THEN
  DIM rsr as RunScriptResult
  rsr = runscript(id, YES, double_trigger_check, scripttype)
  trigger_script_failure = (rsr <> rsSuccess)
  IF gam.script_log.enabled = NO THEN EXIT SUB

  'Can't call watched_script_triggered until after the trigger_script_args calls
  scriptinsts(nowscript).watched = YES
  scrat(nowscript).state = sttriggered
  last_queued_script = @dummy_queued_script
 ELSE
  last_queued_script = NEW ScriptFibre
  'Insert into the queue according to priority
  'Note that the script at the top of the queue is the first to run,
  'so ties are broken by the last triggered being the first to run.
  DIM insertpos as integer = -1
  FOR insertpos = v_len(fibregroup) - 1 TO 0 STEP -1
   IF fibregroup[insertpos]->priority <= priority THEN EXIT FOR
  NEXT
  insertpos += 1
  v_insert fibregroup, insertpos, last_queued_script
 END IF

 'Save information about this script, for use by trigger_script_arg()
 WITH *last_queued_script
  id = decodetrigger(id)
  'If the script was missing, id is now 0, but still queue the script so trigger_script_arg works.
  .id = id
  .scripttype = scripttype
  .log_line = scriptname(id) & "("
  .trigger_loc = trigger_loc
  .double_trigger_check = double_trigger_check
  .priority = priority
  .argc = numargs
  IF numargs > maxScriptArgs THEN
   showbug "trigger_script: too many args: " & numargs
   numargs = maxScriptArgs
  END IF
 END WITH
END SUB

SUB trigger_script_arg (byval argno as integer, byval value as integer, byval argname as zstring ptr = NULL)
 'Set one of the args for a script that was just triggered. They must be in the right order, and all provided.
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
  BUG_IF(argno >= .argc, .scripttype & " triggering is broken: bad arg num " & argno)
  .args(argno) = value
  IF gam.script_log.enabled THEN
   IF argno <> 0 THEN .log_line += ", "
   IF argname THEN .log_line += *argname + "="
   .log_line &= value
  END IF
 END WITH
END SUB

LOCAL SUB run_queued_script (script as ScriptFibre)
 'If the script is missing then .id = 0 and decodetrigger already showed an error
 IF script.id = 0 THEN EXIT SUB

 DIM rsr as RunScriptResult
 rsr = runscript(script.id, YES, script.double_trigger_check, script.scripttype)
 IF rsr = rsSuccess THEN
  FOR argno as integer = 0 TO script.argc - 1
   setScriptArg argno, script.args(argno)
  NEXT
 END IF

 IF gam.script_log.enabled THEN watched_script_triggered script
END SUB

SUB run_queued_scripts
 'Load the queued scripts into the interpreter.

 FOR i as integer = 0 TO v_len(mainFibreGroup) - 1
  run_queued_script(*mainFibreGroup[i])
 NEXT

 dequeue_scripts
END SUB

SUB dequeue_scripts
 'Wipe the script queue
 last_queued_script = NULL
 IF mainFibreGroup = NULL THEN EXIT SUB  'During startup when not initialised yet
 FOR idx as integer = 0 TO v_len(mainFibreGroup) - 1
  DELETE mainFibreGroup[idx]
 NEXT
 v_resize mainFibreGroup, 0
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
  gam.script_log.enabled = NO
  EXIT SUB
 END IF
 gam.script_log.enabled = YES

 print #fh, "Script trigger log for " & getdisplayname(trimpath(sourcerpg)) & ", " & DATE & " " & TIME
 print #fh,
 print #fh, "Solid lines '|' show triggered scripts which have already started running but are"
 print #fh, "waiting or paused due to either another script which was triggered (indicated by a"
 print #fh, "line to its right) or while waiting for a script they called (not shown)."
 print #fh, "Dotted lines ':' show triggered scripts which have not even had a chance to start."
 print #fh, "(...as above...) means that a script continues waiting multiple ticks for the same reason."
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

'Called when resetting game (but not before the first time through a game)
SUB script_log_resetgame
 WITH gam.script_log
  ' Leave .enabled alone, continuing to log
  IF .enabled THEN
   script_log_out !"\n--- Game ended ---\n\n"
  END IF
  .tick = 0
  .wait_msg_repeats = 0
 END WITH
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
SUB watched_script_triggered(script as ScriptFibre)
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
  logline = " ... finished"
 ELSE
  logline = !"\n" & script_log_indent() & "-" & scriptname(scriptinsts(nowscript).id) & " finished"
 END IF
 IF scriptprofiling THEN
  ' This global is set by script_return_timing()
  logline &= "  (took " & format(gam.script_log.last_script_childtime * 1e3, "0.0") & "ms)"
 END IF
 script_log_out logline

 gam.script_log.last_logged = -1
END SUB

'Call each tick if script logging is enabled
SUB script_log_tick
 WITH gam.script_log
  DIM doprint as bool = NO
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
   IF .wait_msg_repeats <= 2 THEN doprint = YES
   IF .wait_msg_repeats = 2 THEN wait_msg = "...as above..."
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


'Kills the currently running script fibre
SUB killscriptthread
 BUG_IF(insideinterpreter = NO ORELSE nowscript < 0, "Inappropriate call")

 debuginfo "Killing script fibre; last script = " & scriptname(scrat(nowscript).id)

 'Hack: we set the state of the old script so that the main loop sees it reads with a stale WITH pointer.
 'Come to think of it, there's no good reason for the interpreter state to be stored in scrat instead
 'of being global.
 scrat(nowscript).state = stdone

 'Remove every script in this fibre, except for the bottommost one
 '(The script below it on the stack will be suspended, state < 0)
 WHILE nowscript > 0 ANDALSO scrat(nowscript - 1).state >= 0
  WITH scrat(nowscript)
   IF .scr <> NULL THEN deref_script(.scr)
  END WITH
  nowscript -= 1
 WEND
 gam.script_log.last_logged = -1

 'Let functiondone handle the fibre exit
 setstackposition(scrst, scrat(nowscript).stackbase)
 scrat(nowscript).state = stdone
END SUB

SUB killallscripts
 'this kills all running scripts.
 'for use in cases of massive errors, quiting to titlescreen or loading a game.

 'Try to compute running times instead of corrupting the timing statistics (untested!)
 stop_fibre_timing

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

 IF numloadedscr > 0 THEN
  IF scriptprofiling THEN print_script_profiling

  freescripts(0)
 END IF
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
' Unlike script_start_waiting this can be called from outside script_functions.
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


FUNCTION runscript (id as integer, newcall as bool, double_trigger_check as bool, scripttype as zstring ptr) as RunScriptResult
'newcall: whether his script is triggered (start a new fibre) rather than called from a script
'double_trigger_check: whether "no double-triggering" should take effect

DIM n as integer = decodetrigger(id)
IF n = 0 THEN RETURN rsQuietFail  '(though decodetrigger might have shown a scripterr)

BUG_IF(insideinterpreter = NO AND newcall = NO, "newcall=NO outside interpreter", rsFail)

DIM index as integer = nowscript + 1

IF index >= maxScriptRunning THEN
 scripterr "Can't load " & *scripttype & " script " & scriptname(n) & ", too many scripts running", serrMajor
 RETURN rsFail
END IF

IF double_trigger_check ANDALSO index > 0 THEN
 IF n = scriptinsts(index - 1).id ANDALSO prefbit(10) = NO THEN  '"Permit double-triggering of scripts" off
  '--scripterr "script " & n & " is already running", serrInfo
  RETURN rsQuietFail
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
  scripterr "Failed to load " + *scripttype + " script " & n & " " & scriptname(n), serrError
  RETURN rsFail
 END IF
 IF scriptprofiling THEN .scr->numcalls += 1
 scriptctr += 1
 .scr->lastuse = scriptctr
 IF newcall THEN .scr->trigger_type = *scripttype
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
  scripterr "Failed to load " + *scripttype + " script " & n & " " & scriptname(n) & ", " & *errstr, serrError
  RETURN rsFail
 END IF

 IF newcall ANDALSO index > 0 THEN
  '--suspend the previous fibre
  IF scriptprofiling THEN stop_fibre_timing  'Must call before suspending
  scrat(index - 1).state *= -1
 END IF

 '--we are successful, so now its safe to increment these
 nowscript = index
 .scr->refcount += 1
 IF .scr->refcount = 1 THEN
  'Removed from unused scripts cache
  unused_script_cache_mem -= .scr->size
 END IF

 'debug "running " & .id & " " & scriptname(.id) & " in slot " & nowscript & " newcall = " _
 '      & newcall & " type " & *scripttype & ", parent = " & .scr->parent & " numcalls = " _
 '      & .scr->numcalls & " refc = " & .scr->refcount & " lastuse = " & .scr->lastuse
END WITH

IF scriptprofiling THEN
 IF newcall THEN
  start_fibre_timing
 ELSE
  script_call_timing
 END IF
END IF

RETURN rsSuccess

END FUNCTION

'Returns an open file handle to an hsz lump, or 0 if not found (which isn't a valid handle).
LOCAL FUNCTION loadscript_open_script (n as integer, expect_exists as bool = YES) as integer
 DIM scriptfile as string = tmpdir & n & ".hsz"
 IF NOT isfile(scriptfile) THEN
  scriptfile = tmpdir & n & ".hsx"
  IF NOT isfile(scriptfile) THEN
   '--because TMC once suggested that preunlumping the .hsp lump would be a good way to reduce (SoJ) loading time
   scriptfile = workingdir & SLASH & n & ".hsx"
   IF NOT isfile(scriptfile) THEN
    IF expect_exists THEN
     scripterr "script " & n & " " & scriptname(n) & " does not exist. (Maybe it was renamed, and the script trigger needs to be updated?)", serrError
    END IF
    RETURN 0
   END IF
  END IF
 END IF

 DIM fh as integer
 OPENFILE(scriptfile, FOR_BINARY + ACCESS_READ, fh)
 RETURN fh
END FUNCTION

'Loads a script or fetchs it from the cache. Returns NULL on failure.
'If loaddata is false, only loads the script header.
FUNCTION loadscript (id as integer, loaddata as bool = YES) as ScriptData ptr
 'debuginfo "loadscript(" & id & " " & scriptname(id) & ", loaddata = " & loaddata & ")"

 DIM fh as integer = 0  'file handle
 DIM header as ScriptData ptr
 header = scriptcache_find(id)

 IF header = NULL THEN
  'Header not loaded yet
  fh = loadscript_open_script(id)
  IF fh = 0 THEN RETURN NULL
  header = loadscript_read_header(fh, id)
  IF header = NULL THEN
   CLOSE #fh
   RETURN NULL
  END IF
  scriptcache_add id, header
  numloadedscr += 1
 END IF

 IF loaddata ANDALSO header->ptr = NULL THEN
  'Only the header is loaded; need to load data
  IF fh = 0 THEN
   fh = loadscript_open_script(id)
   IF fh = 0 THEN RETURN NULL
  END IF
  IF loadscript_read_data(header, fh) = NO THEN
   'The script is already in the cache. Maybe mark it as corrupt?
   CLOSE #fh
   RETURN NULL
  END IF
 END IF
 IF fh THEN CLOSE #fh
 RETURN header
END FUNCTION

'Load a script header from a hsz into a new ScriptData. id is the script id.
'Returns true on success
LOCAL FUNCTION loadscript_read_header(fh as integer, id as integer) as ScriptData ptr
 DIM shortvar as short

 DIM ret as ScriptData ptr = NEW ScriptData
 WITH *ret
  .id = id
  .hash = 0
  'minimum length of a valid 16-bit .hsx
  IF LOF(fh) < 10 THEN
   scripterr "script " & id & " corrupt (too short: " & LOF(fh) & " bytes)", serrError
   DELETE ret
   RETURN NULL
  END IF

  GET #fh, 1, shortvar
  DIM skip as integer = shortvar
  .headerlen = skip

  IF skip < 4 THEN
   scripterr "script " & id & " is corrupt (header length " & skip & ")", serrError
   DELETE ret
   RETURN NULL
  END IF

  'Note that there is no check for the header being longer than expected. Optional
  'fields may be added to the end of the header; if they are mandatory the version number
  'should be incremented.

  GET #fh, 3, shortvar
  'some HSX files seem to have an illegal negative number of variables
  .vars = shortvar
  .vars = bound(.vars, 0, 256)

  IF skip >= 6 THEN
   GET #fh, 5, shortvar
   .args = bound(shortvar, 0, .vars)
  ELSE
   .args = 999
  END IF

  IF skip >= 8 THEN
   GET #fh, 7, shortvar
   .scrformat = shortvar
  ELSE
   .scrformat = 0
  END IF
  IF .scrformat > CURRENT_HSZ_VERSION THEN
   scripterr "script " & id & " is in an unsupported format", serrError
   DELETE ret
   RETURN NULL
  END IF
  DIM wordsize as integer
  IF .scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2

  IF skip >= 12 THEN
   GET #fh, 9, .strtable
   IF .strtable THEN .strtable = (.strtable - skip) \ wordsize
  ELSEIF skip = 10 THEN
   GET #fh, 9, shortvar
   IF shortvar THEN .strtable = (shortvar - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  IF skip >= 14 THEN
   GET #fh, 13, shortvar
   .parent = shortvar
  ELSE
   .parent = 0
  END IF
  IF skip >= 16 THEN
   GET #fh, 15, shortvar
   .nestdepth = shortvar
   IF .nestdepth > maxScriptNesting THEN
    scripterr "Corrupt or unsupported script data with nestdepth=" & .nestdepth & "; should be impossible", serrBug
   END IF
  ELSE
   .nestdepth = 0
  END IF
  IF skip >= 18 THEN
   GET #fh, 17, shortvar
   .nonlocals = shortvar
  ELSE
   .nonlocals = 0
  END IF

  .size = (LOF(fh) - skip) \ wordsize

  IF .strtable < 0 OR .strtable > .size THEN
   scripterr "Script " & id & " corrupt; bad string table offset", serrError
   DELETE ret
   RETURN NULL
  END IF
 END WITH
 RETURN ret
END FUNCTION

'Load the data from a file into the .ptr field of a ScriptData.
'Returns true on success.
LOCAL FUNCTION loadscript_read_data(header as ScriptData ptr, fh as integer) as bool
 DIM shortvar as short

 WITH *header
  .hash = file_hash64(fh)

  DIM wordsize as integer
  IF .scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2
  .ptr = allocate(.size * sizeof(integer))
  IF .ptr = 0 THEN
   scripterr "Could not allocate memory to load script", serrError
   RETURN NO
  END IF

  IF wordsize = 2 THEN
   FOR i as integer = .headerlen TO LOF(fh) - wordsize STEP wordsize
    GET #fh, 1 + i, shortvar
    .ptr[(i - .headerlen) \ 2] = shortvar
   NEXT
  ELSE
   GET #fh, .headerlen + 1, *.ptr, .size
  END IF

  'Sanity check: root node is a do()
  IF .size < 3 ORELSE (.ptr[0] <> 2 OR .ptr[1] <> 0 OR .ptr[2] < 0) THEN
   scripterr "Script " & .id & " corrupt; does not start with do()", serrError
   RETURN NO
  END IF
 END WITH
 RETURN YES
END FUNCTION

LOCAL FUNCTION scriptcache_find(id as integer) as ScriptData ptr
 '-- script() is a hashtable with doubly linked lists as buckets, storing the loaded scripts
 DIM as ScriptData Ptr scrnode = script(id MOD scriptTableSize)
 WHILE scrnode
  IF scrnode->id = id THEN
   RETURN scrnode
  END IF
  scrnode = scrnode->next
 WEND
 RETURN NULL
END FUNCTION

'Add a loaded ScriptData ptr to the cache.
LOCAL SUB scriptcache_add(id as integer, thisscr as ScriptData ptr)
 IF thisscr->ptr THEN
  'Script data is loaded
  totalscrmem += thisscr->size
  unused_script_cache_mem += thisscr->size  'Has refcount 0, up to caller to remove from cache
 END IF

 'append to front of doubly linked list
 DIM as ScriptData Ptr Ptr scrnodeptr = @script(id MOD scriptTableSize)
 IF *scrnodeptr THEN
  'already a script there
  (*scrnodeptr)->backptr = @thisscr->next
 END IF
 thisscr->backptr = scrnodeptr 'this is for convenience of easier deleting (in freescripts)
 thisscr->next = *scrnodeptr
 *scrnodeptr = thisscr
END SUB

'Destruct a ScriptData and remove it from the cache.
SUB delete_ScriptData (byval scriptd as ScriptData ptr)
 WITH *scriptd
  BUG_IF(.refcount, "nonzero refcount=" & .refcount & " for " & scriptname(ABS(.id)))

  IF .ptr THEN
   'debug "deallocating " & .id & " " & scriptname(ABS(.id)) & " size " & .size
   totalscrmem -= .size
   unused_script_cache_mem -= .size
   deallocate(.ptr)
  END IF
  numloadedscr -= 1
  IF .next THEN
   .next->backptr = .backptr
  END IF
  *.backptr = .next
 END WITH

 DELETE scriptd
END SUB

'Dereference script pointer
SUB deref_script(script as ScriptData ptr)
 script->refcount -= 1
 IF script->refcount = 0 THEN
  'scriptcachemem
  unused_script_cache_mem += script->size
  ' If profiling don't delete ScriptDatas, as they include the timing info
  IF scriptprofiling THEN EXIT SUB
  IF unused_script_cache_mem > scriptmemMax THEN
   'Evicting stuff from the script cache is probably pointless, but we've already got it,
   'and it may be useful for the new script interpreter...
   freescripts(scriptmemMax * 0.75)
  END IF
 END IF
END SUB

TYPE ScriptListElmt
 p as ScriptData ptr
 score as double
END TYPE

'Iterate over all loaded scripts, sort them in descending order according to score
'returned by the callback, and return number of scripts in numscripts
'(LRUlist is dynamic)
SUB sort_scripts(LRUlist() as ScriptListElmt, byref numscripts as integer, scorefunc as function(scr as ScriptData) as double, fibres_only as bool = NO)
 DIM j as integer
 numscripts = 0
 REDIM LRUlist(-1 TO -1)
 ' Iterate over the linked-list buckets of the 'script' hashmap.
 FOR i as integer = 0 TO UBOUND(script)
  DIM scrp as ScriptData Ptr = script(i)
  WHILE scrp
   ' A script is a fibre root if it has been started with a call to runscript with a trigger type name.
   IF fibres_only AND LEN(scrp->trigger_type) = 0 THEN
    scrp = scrp->next
    CONTINUE WHILE
   END IF
   DIM score as double = scorefunc(*scrp)
   REDIM PRESERVE LRUlist(-1 TO numscripts)
   ' Insert this script into the ordered list
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

FUNCTION freescripts_script_scorer(byref script as ScriptData) as double
 'this formula has only been given some testing, and doesn't do all that well
 DIM score as integer
 IF script.refcount THEN RETURN 1000000000
 score = script.lastuse - scriptctr
 score = iif(score > 0, -1000000000, score)  'Handle overflow
 score = iif(score > -400, score, -400) _
       - iif(script.ptr, script.size, 0) \ (scriptmemMax \ 1024)
 IF script.id < 0 THEN
  'Stale script
  score = -1000000000
 END IF
 RETURN score
END FUNCTION

'Two uses: freescripts(0) frees all scripts, otherwise
'frees unused loaded scripts until at least unused_script_cache_mem <= mem (measured in 4-byte ints) (probably a lot lower)
SUB freescripts (byval mem as integer)
 REDIM LRUlist() as ScriptListElmt
 DIM numscripts as integer

 'give each script a score (the lower, the more likely to throw) and sort them
 'this is roughly a least recently used list
 sort_scripts LRUlist(), numscripts, @freescripts_script_scorer

 FOR i as integer = 0 TO numscripts - 1
  IF mem = 0 THEN
   delete_ScriptData LRUlist(i).p
  ELSE
   IF LRUlist(i).p->refcount <> 0 THEN EXIT SUB
   IF unused_script_cache_mem <= mem THEN EXIT SUB
   'debug "unloading script " & scriptname(ABS(LRUlist(i).p->id)) & " refcount " & LRUlist(i).p->refcount
   delete_ScriptData LRUlist(i).p
  END IF
 NEXT
END SUB

' No longer used (used to be used when memory used by loaded scripts was strictly bounded,
' so even running scripts could be unloaded)
SUB reloadscript (si as ScriptInst, oss as OldScriptState, byval updatestats as bool = YES)
 WITH si
  IF .scr = NULL THEN
   .scr = loadscript(.id)
   IF .scr = NULL THEN killallscripts: EXIT SUB
   oss.scr = .scr
   oss.scrdata = .scr->ptr
   .scr->refcount += 1
   IF updatestats AND scriptprofiling THEN .scr->numcalls += 1
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

'Re-unlump .hsp lump
SUB load_hsp ()
 DIM header as HSHeader
 header.plotscr_version = ""  'Required, because of strcmp below

 IF isfile(game + ".hsp") THEN
  'TODO: should really delete all existing .hsz files, to catch missing scripts
  unlump game + ".hsp", tmpdir
  load_hsp_header tmpdir & "hs", header
 END IF

 debuginfo "plotscr.hsd version: " & header.plotscr_version
 IF LEN(header.plotscr_version) THEN
  scripts_use_cc_scancodes = strcmp(STRPTR(header.plotscr_version), @"3U ") >= 0
 ELSE
  scripts_use_cc_scancodes = NO
 END IF
END SUB

'Unload all scripts not in use, so they will get loaded again when used.
'force_full_message: even if the player previously asked to hide notifications
'    about reloading failing, show the error rather than just an overlay message.
'TODO: it would be preferable to keep the bookkeeping data by only deleting the script commands and strings
SUB reload_scripts (force_full_message as bool = YES)
 STATIC dont_show_again as bool = NO  'Don't pop up the full error message

 DIM unfreeable as string, still_unfreeable as string
 DIM num_unfreeable as integer

 load_hsp

 ' Iterate over the hashmap bucket chains
 FOR i as integer = 0 TO UBOUND(script)
  DIM as ScriptData Ptr scrp = script(i), nextp
  WHILE scrp
   nextp = scrp->next
   WITH *scrp
    IF .refcount = 0 THEN
     delete_ScriptData scrp
    ELSE
     ' It's in use. But has it actually been changed?
     'debug scriptname(ABS(.id)) & " in use"

     DIM fh as integer
     fh = loadscript_open_script(ABS(.id), NO)
     IF fh = 0 THEN
      debuginfo "reload_scripts: " & scriptname(ABS(.id)) & " no longer exists!"
     ELSE
      DIM newhash as ulongint = file_hash64(fh)
      CLOSE #fh

      'debug scriptname(ABS(.id)) & " old hash=" & HEX(.hash) & " new=" & HEX(newhash)
      IF newhash <> .hash THEN
       num_unfreeable += 1
       IF .id > 0 THEN
        unfreeable &= scriptname(ABS(.id)) & " "
       ELSE
        still_unfreeable &= scriptname(ABS(.id)) & " "
       END IF

       debuginfo "not reloading script " & scriptname(ABS(.id)) & " because it's in use: refcount=" & .refcount
       ' Negate the ID number. This will prevent this script data from being used when starting a new script.
       ' It won't be automatically evicted from the cache, but that's ok.
       .id = ABS(.id) * -1
      ELSEIF .id < 0 ANDALSO newhash = .hash THEN
       ' This script was previously marked as un-reloadable, but the changes were reverted.
       .id = -.id
      END IF
     END IF
    END IF
   END WITH

   scrp = nextp
  WEND
 NEXT

 DIM msg as string
 IF LEN(unfreeable) THEN
  msg = !"These scripts were modified but are in use and can't be reloaded yet:\n" & unfreeable
 END IF
 IF LEN(still_unfreeable) THEN
  IF LEN(msg) THEN msg &= !"\n"
  msg &= !"These scripts modified earlier still can't be reloaded:\n" & still_unfreeable
 END IF
 IF LEN(msg) THEN
  IF dont_show_again AND force_full_message = NO THEN
   show_overlay_message num_unfreeable & " scripts not reloaded (see F5 menu)", 2.5
  ELSE
   dont_show_again = (twochoice(msg, "OK", "Don't tell me again", 0, 0) = 1)
  END IF

  ' "Force reload scripts" is still available in the Reload menu, but
  ' selecting it has no actual effect on script data, other than re-checking, telling
  ' the user which scripts are still in use, and possibly freeing some memory.
 ELSE
  ' All modified scripts unloaded successfully.
  lump_reloading.hsp.changed = NO

  show_overlay_message "Scripts successfully reloaded", 2.5
 END IF

 'Cause the cache in scriptname() (and also in commandname()) to be dropped (yuck)
 game_unique_id = STR(randint(INT_MAX))
END SUB


'==========================================================================================
'                                    Script profiling
'==========================================================================================

' Do script profile accounting when one script calls another (including with runscriptbyid),
' but not when a new script fibre is started.
' nowscript is the new script, nowscript-1 is the calling script.
SUB script_call_timing
 DIM timestamp as double
 READ_TIMER(timestamp)
 'Exclusive time for calling script
 scriptinsts(nowscript - 1).scr->totaltime += timestamp
 WITH *scriptinsts(nowscript).scr
  'debug "script_call_timing: slot " & (nowscript-1) & " id " & scriptinsts(nowscript - 1).scr->id & " called slot " & nowscript & " id " & .id & " calls_in_stack++ =" & .calls_in_stack
  'debug "  caller totaltime now " & scriptinsts(nowscript - 1).scr->totaltime
  .entered += 1
  'Exclusive time
  .totaltime -= timestamp
  'Inclusive time
  IF .calls_in_stack = 0 THEN
   .laststart = timestamp
   'debug "  set laststart=" & timestamp
  END IF
  .calls_in_stack += 1
 END WITH
END SUB

' Called when a script returns and script profiling enabled.
' nowscript is the returning script, and either nowscript-1 called it, or it was triggered/spawned.
SUB script_return_timing
 DIM timestamp as double
 READ_TIMER(timestamp)
 WITH *scriptinsts(nowscript).scr
  'debug "script_return_timing: slot " & nowscript & " id " & .id & " calls_in_stack-- =" & .calls_in_stack & " (returning script)"
  'Exclusive time
  .totaltime += timestamp
  'debug "  id " & .id & " totaltime now " & .totaltime
  'Inclusive time
  .calls_in_stack -= 1
  IF .calls_in_stack = 0 THEN
   'Was not a recursive call, so won't be double-counting time
   .childtime += timestamp - .laststart
   IF scriptinsts(nowscript).watched THEN
    gam.script_log.last_script_childtime = timestamp - .laststart
   END IF
   'debug "  adding to id " & .id & " childtime: " & (timestamp - .laststart)  & " now: " & .childtime
  END IF
 END WITH

 IF nowscript > 0 ANDALSO scrat(nowscript - 1).state >= 0 THEN
  'Was called from another script; time accounting for it
  WITH *scriptinsts(nowscript - 1).scr
   .entered += 1
   .totaltime -= timestamp
  END WITH
 ELSE
  ' Fibre finished.
  timing_fibre = NO
 END IF
END SUB

' Call this when starting/resuming execution of a script fibre;
' used to collect timing statistics.
SUB start_fibre_timing
 IF scriptprofiling = NO THEN EXIT SUB
 IF nowscript < 0 OR insideinterpreter = NO THEN EXIT SUB
 'debug "start_fibre_timing slot " & nowscript & " id " & scrat(nowscript).scr->id
 IF timing_fibre THEN EXIT SUB
 timing_fibre = YES

 scrat(nowscript).scr->entered += 1

 DIM timestamp as double
 READ_TIMER(timestamp)

 ' Exclusive time (in this script)
 scrat(nowscript).scr->totaltime -= timestamp

 ' Error checking
 FOR which as integer = nowscript TO 0 STEP -1
  IF scrat(which).state < 0 THEN EXIT FOR  'Bottom of fibre callstack
  WITH *scrat(which).scr
   IF .calls_in_stack <> 0 THEN showbug "Garbage calls_in_stack=" & .calls_in_stack & " value for script " & .id
  END WITH
 NEXT

 ' Inclusive time (in this script and call tree descendents)
 FOR which as integer = nowscript TO 0 STEP -1
  IF scrat(which).state < 0 THEN EXIT FOR  'Bottom of fibre callstack
  WITH *scrat(which).scr
   .calls_in_stack += 1
   .laststart = timestamp
   'debug "  set slot " & which & " id " & .id & " laststart = " & timestamp & " ++calls_in_stack = " & .calls_in_stack
  END WITH
 NEXT
END SUB

' Call this when execution of the current script fibre stops, e.g. due to a wait
' command or a script error.
' NOTE: if script_return_timing cleans up the last script in a fibre, stop_fibre_timing doesn't get called.
SUB stop_fibre_timing
 IF scriptprofiling = NO THEN EXIT SUB
 IF nowscript < 0 OR insideinterpreter = NO THEN EXIT SUB
 'debug "stop_fibre_timing slot " & nowscript & " id " & scrat(nowscript).scr->id
 IF timing_fibre = NO THEN EXIT SUB
 timing_fibre = NO

 DIM timestamp as double
 READ_TIMER(timestamp)

 ' Exclusive time (in this script)
 scrat(nowscript).scr->totaltime += timestamp
 'debug "  id " & scrat(nowscript).scr->id & " totaltime now " & scrat(nowscript).scr->totaltime

 ' Inclusive time (in this script and call tree descendents)
 FOR which as integer = nowscript TO 0 STEP -1
  IF scrat(which).state < 0 THEN EXIT FOR  'Bottom of fibre callstack
  WITH *scrat(which).scr
   'debug "  id " & .id & " calls_in_stack-- = " & .calls_in_stack
   .calls_in_stack -= 1
   IF .calls_in_stack = 0 THEN
    'Was not a recursive call, so won't be double-counting time
    .childtime += timestamp - .laststart
    'debug "  adding to id " & .id & " childtime: " & (timestamp - .laststart) & " now: " & .childtime
   END IF
  END WITH
 NEXT

 ' Error checking
 FOR which as integer = nowscript TO 0 STEP -1
  IF scrat(which).state < 0 THEN EXIT FOR  'Bottom of fibre callstack
  WITH *scrat(which).scr
   IF .calls_in_stack <> 0 THEN showbug "Garbage calls_in_stack=" & .calls_in_stack & " value for script " & .id & " slot " & which
  END WITH
 NEXT

END SUB

'Sort by total time
LOCAL FUNCTION profiling_script_totaltime_scorer(byref script as ScriptData) as double
 'RETURN (script.totaltime - script.entered * timeroverhead) * -100000
 RETURN -script.totaltime
END FUNCTION

'Sort by child time
LOCAL FUNCTION profiling_script_childtime_scorer(byref script as ScriptData) as double
 RETURN -script.childtime
END FUNCTION

'Print profiling information on scripts to g_debug.txt
SUB print_script_profiling
 DIM timestamp as double
 READ_TIMER(timestamp)
 timeroverhead = -timestamp
 FOR i as integer = 0 TO 1999
  READ_TIMER(timestamp)
 NEXT
 timeroverhead += timestamp
 timeroverhead /= 2000

 REDIM LRUlist() as ScriptListElmt
 DIM numscripts as integer

 'give each script a score (the lower, the more likely to throw) and sort them
 'this is roughly a least recently used list
 sort_scripts LRUlist(), numscripts, @profiling_script_totaltime_scorer

 DIM entiretime as double
 DIM totalswitches as integer
 FOR i as integer = 0 TO numscripts - 1
  entiretime += LRUlist(i).p->totaltime
  totalswitches += LRUlist(i).p->entered
 NEXT

 debug "=== Script profiling information ==="
 debug "'%time' shows the percentage of the total time spent in this script."
 debug "'time' is the time spent in the script (and built-in commands it called)."
 debug "'childtime' is the time spent in a script and all scripts it called, and all"
 debug "  scripts called from those, and so on."
 debug "'#calls' is the number of times the script ran."
 debug "'#switches' is the number of times that the interpreter switched to that"
 debug "  script, which is the sum of #calls, how many other scripts it called and"
 debug "  how many times it waited (switching time is relatively neglible)."
 debug "'type' is the way that a script was last triggered"
 debug ""
 debug "ms is milliseconds (0.001 seconds), us is microseconds (0.000001 seconds)"
 debug ""
 debug "Total time recorded in interpreter: " & format(entiretime, "0.000") & "sec"
 debug "(Timer overhead = " & format(timeroverhead*1e6, "0.00") & "us per measurement)"
 debug "(Estimated time wasted profiling: " & format(timeroverhead * totalswitches, "0.000") & "sec)"
 debug ""
 debug "  -- All scripts sorted by time --"
 debug " %time        time   childtime    time/call      #calls   #switches  script name"
 FOR i as integer = 0 TO numscripts - 1
 ' debug i & ": " & LRUlist(i).p & " score = " & LRUlist(i).score
  WITH *LRUlist(i).p
   debug RIGHT("  " & format(100 * .totaltime / entiretime, "0.00"), 6) _
       & RIGHT(SPACE(9) & format(.totaltime*1000, "0"), 10) & "ms" _
       & RIGHT(SPACE(9) & format(.childtime*1000, "0"), 10) & "ms" _
       & RIGHT(SPACE(10) & format(.totaltime*1000000/.numcalls, "0"), 11) & "us" _
       & RIGHT(SPACE(11) & .numcalls, 12) _
       & RIGHT(SPACE(11) & .entered, 12) _
       & "  " & scriptname(ABS(.id))
       '& "  " & format(1000*(.totaltime + .entered * timeroverhead), "0.00")

 '  debug "id = " & .id & " " & scriptname(ABS(.id))
 '  debug "refcount = " & .refcount
 '  debug "numcalls = " & .numcalls
 '  debug "lastuse = " & .lastuse
 '  debug "size = " & .size
  END WITH
 NEXT

 'Print fibres only
 sort_scripts LRUlist(), numscripts, @profiling_script_childtime_scorer, YES

 debug ""
 debug "  -- Triggered scripts sorted by childtime --"
 debug "%chdtime   chdtime  chdtime/call    #calls                type  script name"
 FOR i as integer = 0 TO numscripts - 1
  WITH *LRUlist(i).p
   DIM percall as string
   debug RIGHT("  " & format(100 * .childtime / entiretime, "0.00"), 6)  _
       & RIGHT(SPACE(9) & format(.childtime*1000, "0"), 10) & "ms" _
       & RIGHT(SPACE(11) & format(.childtime*1000/.numcalls, "0.0"), 12) & "ms" _
       & RIGHT(SPACE(9) & .numcalls, 10) _
       & RIGHT(SPACE(20) & .trigger_type, 20) _
       & "  " & scriptname(ABS(.id))
  END WITH
 NEXT
 debug ""
END SUB

' Normally when you do multiple profiling runs, the stats are cumulative.
' This should not be called from inside the interpreter, at least not while profiling.
SUB clear_profiling_stats
 IF insideinterpreter AND scriptprofiling THEN EXIT SUB

 FOR i as integer = 0 TO UBOUND(script)
  DIM scrp as ScriptData Ptr = script(i)
  WHILE scrp
   scrp->numcalls = 0
   scrp->totaltime = 0.
   scrp->childtime = 0.
   scrp->entered = 0
   scrp = scrp->next
  WEND
 NEXT
END SUB


'==========================================================================================
'                                    Other Interfaces
'==========================================================================================


'Read one of the strings from a script's string table.
FUNCTION script_string_constant(scriptinsts_slot as integer, offset as integer) as string
 WITH *scriptinsts(scriptinsts_slot).scr
  DIM stringp as integer ptr = .ptr + .strtable + offset
  IF .strtable + offset >= .size ORELSE .strtable + (stringp[0] + 3) \ 4 >= .size THEN
   scripterr "script data corrupt: illegal string offset", serrError
  ELSE
   RETURN read32bitstring(stringp)
  END IF
 END WITH
END FUNCTION

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
 DIM fh as integer
 IF OPENFILE(tmpdir + "commands.bin", FOR_BINARY + ACCESS_READ, fh) THEN
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
 IF insideinterpreter = NO THEN
  showbug "interpreter_context_name called outside interpreter"
 ELSEIF curcmd->kind = tyfunct THEN
  RETURN commandname(curcmd->value) + ": "
 END IF
 RETURN ""
END FUNCTION

FUNCTION script_call_chain (byval trim_front as bool = YES) as string
 IF nowscript < 0 THEN
  RETURN "(No scripts running)"
 END IF

 DIM scriptlocation as string
 scriptlocation = scriptname(scriptinsts(nowscript).id)
 FOR i as integer = nowscript - 1 TO 0 STEP -1
  IF scrat(i).state < 0 THEN EXIT FOR 'suspended: not part of the call chain
  scriptlocation = scriptname(scriptinsts(i).id) + " -> " + scriptlocation
 NEXT
 IF trim_front AND LEN(scriptlocation) > 150 THEN scriptlocation = " ..." + RIGHT(scriptlocation, 150)
 RETURN scriptlocation
END FUNCTION


'==========================================================================================
'                                   Menus and Dialogues
'==========================================================================================

FUNCTION should_display_error_to_user(byval errorlevel as scriptErrEnum) as bool
 IF errorlevel >= serrMajor THEN RETURN YES  'Too big to ignore, inc unreadable/unsupported data, engine bugs
 IF gen(genCurrentDebugMode) = 0 THEN 'Release mode, supress most error display
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

'For errorlevel scheme, see scriptErrEnum in const.bi
'NOTE: this function can get called with errors which aren't caused by scripts,
'for example findhero() called from a textbox conditional.
SUB scripterr (e as string, byval errorlevel as scriptErrEnum = serrBadOp)
 'mechanism to handle scriptwatch throwing errors
 STATIC as integer recursivecall

 STATIC as integer ignorelist()

 DIM as string errtext()
 DIM as integer scriptcmdhash, errmsghash

 'err_suppress_lvl is always at least serrIgnore
 IF errorlevel <= err_suppress_lvl THEN EXIT SUB

 DIM as string call_chain
 IF insideinterpreter THEN call_chain = script_call_chain(NO)
 debug "Scripterr(" & errorlevel & "): " + call_chain + ": " + e

 IF NOT should_display_error_to_user(errorlevel) THEN EXIT SUB

 'Is the error ignored? Match errors in two different ways (throw location,
 'error message) to improve odds of the match working.
 IF nowscript >= 0 THEN
  scriptcmdhash = scrat(nowscript).id * 100000 + scrat(nowscript).ptr
  IF a_find(ignorelist(), scriptcmdhash) <> -1 THEN EXIT SUB
 END IF
 errmsghash = strhash(e)
 IF a_find(ignorelist(), errmsghash) <> -1 THEN EXIT SUB

 ' OK, decided to show the error
 stop_fibre_timing

 recursivecall += 1

 IF errorlevel = serrError THEN e = "Script data may be corrupt or unsupported:" + CHR(10) + e
 IF errorlevel >= serrBug THEN e = "PLEASE REPORT THIS POSSIBLE ENGINE BUG" + CHR(10) + e

 e = e + CHR(10) + CHR(10) + "  Call chain (current script last):" + CHR(10) + script_call_chain()
 split(wordwrap(e, large(80, vpages(vpage)->w - 16) \ 8), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
 menu.anchorvert = alignTop
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Ignore", 0
 append_menu_item menu, "Don't display any more script errors", 1
 'append_menu_item menu, "Set error suppression level to " & errorlevel, 9
 IF insideinterpreter THEN
  'Outside the interpreter there's no active fiber, can't call killscriptthread
  append_menu_item menu, "Stop this script", 2
 END IF
 append_menu_item menu, "Hide this error", 3
 append_menu_item menu, "Exit game", 4
 append_menu_item menu, "Enter slice debugger", 5
 IF recursivecall = 1 THEN  'don't reenter the debugger if possibly already inside!
  IF gam.debug_scripts <> 0 THEN
   state.pt = append_menu_item(menu, "Return to script debugger", 6)
  ELSE
   append_menu_item menu, "Enter script debugger", 6
  END IF
  IF running_under_Custom THEN append_menu_item menu, "Reload scripts", 7
 END IF

 state.active = YES
 init_menu_state state, menu

 push_and_reset_gfxio_state
 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN 'ignore
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_scripterr")

  IF enter_space_click(state) THEN
   SELECT CASE menu.items[state.pt]->t
    CASE 0 'ignore
     EXIT DO
    CASE 1 'hide errors (but not engine bugs)
     err_suppress_lvl = serrError
     EXIT DO
    CASE 2
     killscriptthread
     EXIT DO
    CASE 3 'hide this error
     a_append(ignorelist(), scriptcmdhash)
     a_append(ignorelist(), errmsghash)
     EXIT DO
    CASE 4
     debuginfo "scripterr: User opted to quit"
     exitprogram NO, 1
    CASE 5
     slice_editor SliceTable.Root
    CASE 6 'Script debugger
     gam.debug_scripts = 2
     scriptwatcher gam.debug_scripts 'clean mode, script state view mode
     EXIT DO
    CASE 7 'reload scripts
     reload_scripts
     EXIT DO
    'CASE 9 'hide some errors
    ' err_suppress_lvl = errorlevel
   END SELECT
  END IF
  
  usemenu state

  clearpage vpage

  centerbox rCenter, 12, rWidth - 10, 15, 3, vpage
  textcolor uilook(uiText), 0
  DIM header as string
  IF errorlevel >= serrBug THEN
   header = "Impossible error/engine bug!"
  ELSEIF errorlevel >= serrBound THEN
   header = IIF(insideinterpreter, "Script Error!", "Error!")
  ELSEIF errorlevel >= serrWarn THEN
   header = IIF(insideinterpreter, "Script Warning", "Warning")
  ELSEIF errorlevel = serrInfo THEN
   header = IIF(insideinterpreter, "Script Diagnostic", "Diagnostic")
  END IF
  IF LEN(header) THEN
   printstr header, pCentered, 7, vpage
  END IF

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 6 THEN
   textcolor uilook(uiSelectedItem), 0 
   wrapprint !"The debugger is a usability train-wreck!\n" + _
              "Press F1 inside the debugger to see help", 0, pBottom, , vpage , , , fontPlain
  END IF
  setvispage vpage

  IF autotestmode THEN
    write_checkpoint
    exitprogram NO, 1
  END IF

  dowait
 LOOP
 pop_gfxio_state
 recursivecall -= 1

 next_interpreter_check_time = TIMER + scriptCheckDelay
 start_fibre_timing

 'Note: when we resume after a script error, the keyboard state changes, which might break a script
 'Not worth worrying about this.
END SUB

'TODO: there's a lot of code duplicated between this and scripterr
FUNCTION script_interrupt () as integer
 DIM as integer ret = NO
 DIM as string errtext()
 DIM as string msg

 stop_fibre_timing

 msg = "A script may be stuck in an infinite loop. Press F1 for more help" + CHR(10) + CHR(10)
 msg &= "  Call chain (current script last):" + CHR(10) + script_call_chain()
 debug "Script interrupted: " & script_call_chain(NO)
 split(wordwrap(msg, large(80, vpages(vpage)->w - 16) \ 8), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
 menu.anchorvert = alignTop
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Continue running", 0
 'append_menu_item menu, "Exit the top-most script", 10
 append_menu_item menu, "Stop the script fibre", 2
 append_menu_item menu, "Stop all scripts", 3
 append_menu_item menu, "Exit game", 4
 append_menu_item menu, "Enter script debugger", 5
 IF running_under_Custom THEN append_menu_item menu, "Reload scripts", 6

 state.active = YES
 init_menu_state state, menu

 push_and_reset_gfxio_state
 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN 'continue
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_script_interrupt")

  IF enter_space_click(state) THEN
   SELECT CASE menu.items[state.pt]->t
    CASE 0 'continue
     ret = NO
    'CASE 10 'exit topmost  ... probably not too helpful
    ' killtopscript
    ' ret = YES
    CASE 2 'exit fiber
     killscriptthread
     ret = YES
    CASE 3 'stop all fibers
     killallscripts
     ret = YES
    CASE 4 'exit
     debug "script_interrupt: User opted to quit"
     exitprogram NO, 1
    CASE 5 'script debugger
     gam.debug_scripts = 2
     scriptwatcher gam.debug_scripts 'clean mode, script state view mode
     ret = YES
    CASE 6 'reload scripts
     reload_scripts
     ret = NO
   END SELECT
   EXIT DO
  END IF

  usemenu state

  clearpage vpage

  centerbox rCenter, 12, rWidth - 10, 15, 3, vpage
  textcolor uilook(uiText), 0
  printstr "A script is stuck", rCenter - 17*4, 7, vpage

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 4 THEN
   textcolor uilook(uiSelectedItem), 0 
   wrapprint !"The debugger is a usability train-wreck!\n" + _
              "Press F1 inside the debugger to see help", 0, pBottom, , vpage , , , fontPlain
  END IF
  setvispage vpage

  dowait
 LOOP

 pop_gfxio_state
 clearpage vpage
 setvispage vpage
 next_interpreter_check_time = TIMER + scriptCheckDelay
 start_fibre_timing

 'Note: when we resume after a script interruption, the keyboard state changes, which might break a script
 'Not worth worrying about this.
 RETURN ret
END FUNCTION
