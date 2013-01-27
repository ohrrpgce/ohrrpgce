#ifndef MORESUBS_BI
#define MORESUBS_BI

#include "scrconst.bi"

DECLARE SUB addhero (byval who as integer, byval slot as integer, byval forcelevel as integer=-1)
DECLARE FUNCTION averagelev () as integer
DECLARE SUB calibrate

DECLARE FUNCTION consumeitem (byval index as integer) as bool
DECLARE FUNCTION countitem (byval item_id as integer) as integer
DECLARE SUB getitem (byval item_id as integer, byval num as integer=1)
DECLARE SUB delitem (byval item_id as integer, byval amount as integer=1)
DECLARE FUNCTION room_for_item (byval itemid as integer, byval num as integer = 1) as bool

DECLARE SUB doswap (byval s as integer, byval d as integer)
DECLARE SUB update_textbox ()
DECLARE SUB party_change_updates ()
DECLARE SUB evalherotags ()
DECLARE SUB evalitemtags ()
DECLARE FUNCTION findhero (byval who as integer, byval first as integer, byval last as integer, byval direction as integer) as integer
DECLARE SUB hero_swap_menu (byval iAll as integer)
DECLARE SUB settag OVERLOAD (byval tagnum as integer, byval value as integer = 4444)
DECLARE SUB settag OVERLOAD (tagbits() as integer, byval tagnum as integer, byval value as integer = 4444)
DECLARE FUNCTION istag OVERLOAD (byval num as integer, byval zero as integer=NO) as integer
DECLARE FUNCTION istag OVERLOAD (tagbits() as integer, byval num as integer, byval zero as integer=NO) as integer
DECLARE SUB loaddoor (byval map as integer)
DECLARE SUB minimap (byval x as integer, byval y as integer)
DECLARE FUNCTION teleporttool () as integer
DECLARE FUNCTION movdivis (byval xygo as integer) as integer
DECLARE FUNCTION onwho (caption as string, byval alone as integer) as integer
DECLARE SUB readjoysettings
DECLARE SUB renamehero (byval who as integer, byval escapable as integer)
DECLARE SUB resetgame (scriptout as string)
DECLARE SUB resetlmp (byval slot as integer, byval lev as integer)

DECLARE SUB trigger_script (byval id as integer, byval double_trigger_check as bool, scripttype as string, trigger_loc as string, scrqueue() as QueuedScript, byval trigger as integer = plottrigger)
DECLARE SUB trigger_script_arg (byval argno as integer, byval value as integer, byval argname as zstring ptr = NULL)
DECLARE SUB dequeue_scripts ()
DECLARE SUB run_queued_scripts ()

DECLARE SUB start_script_trigger_log ()
DECLARE SUB script_log_tick ()
DECLARE SUB script_log_out (text as string)
DECLARE SUB watched_script_triggered (script as QueuedScript)
DECLARE SUB watched_script_resumed ()
DECLARE SUB watched_script_finished ()

DECLARE FUNCTION runscript (byval id as integer, byval newcall as integer, byval double_trigger_check as bool, byval scripttype as zstring ptr, byval trigger as integer) as integer
DECLARE FUNCTION loadscript (byval n as unsigned integer) as ScriptData ptr
DECLARE SUB freescripts (byval mem as integer)

DECLARE FUNCTION commandname (byval id as integer) as string
DECLARE FUNCTION script_call_chain (byval trim_front as integer = YES) as string
DECLARE SUB scripterr (e as string, byval errorlevel as scriptErrEnum = serrBadOp)
DECLARE FUNCTION script_interrupt () as integer
DECLARE FUNCTION settingstring (searchee as string, setting as string, result as string) as integer
DECLARE SUB shop (byval id as integer)
DECLARE FUNCTION useinn (byval price as integer, byval holdscreen as integer) as integer
DECLARE SUB tagdisplay
DECLARE SUB writejoysettings
DECLARE FUNCTION herocount (byval last as integer = 3) as integer
DECLARE FUNCTION caterpillar_size () as integer

#endif
