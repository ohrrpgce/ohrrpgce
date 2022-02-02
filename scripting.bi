'OHRRPGCE GAME
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef SCRIPTING_BI
#define SCRIPTING_BI

#include "scrconst.bi"

DECLARE SUB trigger_script (id as integer, numargs as integer, double_trigger_check as bool, scripttype as string, trigger_loc as string, byref fibregroup as ScriptFibre ptr vector, priority as integer = 0)

DECLARE SUB trigger_script_arg (byval argno as integer, byval value as integer, byval argname as zstring ptr = NULL)
DECLARE SUB dequeue_scripts ()
DECLARE SUB run_queued_scripts ()

DECLARE SUB start_script_trigger_log ()
DECLARE SUB script_log_tick ()
DECLARE SUB script_log_resetgame
DECLARE SUB script_log_out (text as string)
DECLARE SUB watched_script_triggered (fibre as ScriptFibre)
DECLARE SUB watched_script_resumed ()
DECLARE SUB watched_script_finished ()

DECLARE SUB print_script_profiling ()
DECLARE SUB clear_profiling_stats ()
DECLARE SUB start_fibre_timing ()
DECLARE SUB stop_fibre_timing ()
DECLARE SUB script_call_timing ()
DECLARE SUB script_return_timing ()

DECLARE SUB killallscripts ()
DECLARE SUB killscriptthread ()
DECLARE SUB resetinterpreter ()

DECLARE SUB script_start_waiting(waitarg1 as integer = 0, waitarg2 as integer = 0)
DECLARE SUB script_start_waiting_ticks(whichscript as integer, ticks as integer)
DECLARE SUB script_stop_waiting(returnval as integer = 0)

ENUM 'RunScriptResult
  rsFail = 0
  rsSuccess = 1
  rsQuietFail = 2
END ENUM
TYPE RunScriptResult as integer

DECLARE FUNCTION runscript (id as integer, newcall as bool, double_trigger_check as bool, scripttype as zstring ptr) as RunScriptResult
DECLARE FUNCTION loadscript (id as integer, loaddata as bool = YES) as ScriptData ptr
DECLARE SUB delete_ScriptData (byval scriptd as ScriptData ptr)
DECLARE SUB deref_script (script as ScriptData ptr)
DECLARE SUB reload_scripts (force_full_message as bool = YES)
DECLARE SUB load_hsp ()

DECLARE FUNCTION script_string_constant(scriptinsts_slot as integer, offset as integer) as string
DECLARE FUNCTION commandname (byval id as integer) as string
DECLARE FUNCTION current_command_name() as string
DECLARE FUNCTION interpreter_context_name() as string
DECLARE FUNCTION script_call_chain (byval trim_front as bool = YES) as string
DECLARE FUNCTION should_display_error_to_user(byval errorlevel as scriptErrEnum) as bool
DECLARE SUB scripterr (e as string, byval errorlevel as scriptErrEnum = serrBadOp, context_slice as Slice ptr = NULL)
DECLARE FUNCTION script_interrupt () as integer

' The following are in oldhsinterpreter.bas

DECLARE FUNCTION oldscriptstate_init (index as integer, script as ScriptData ptr) as zstring ptr
DECLARE SUB scriptinterpreter ()
DECLARE SUB breakpoint (byref mode as integer, byval callspot as integer)
DECLARE SUB scriptwatcher (byref mode as integer, byval drawloop as bool = NO)
DECLARE SUB setScriptArg (byval arg as integer, byval value as integer)
DECLARE FUNCTION ancestor_script_id(scriptslot as integer, depth as integer) as integer


#endif
