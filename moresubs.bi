#ifndef MORESUBS_BI
#define MORESUBS_BI

DECLARE SUB addhero (who as integer, slot as integer, forcelevel as integer=-1)
DECLARE FUNCTION averagelev () as integer
DECLARE SUB calibrate
DECLARE FUNCTION consumeitem (index as integer) as integer
DECLARE FUNCTION countitem (it as integer) as integer
DECLARE SUB delitem (it as integer, amount as integer)
DECLARE SUB doswap (s as integer, d as integer)
DECLARE SUB drawsay ()
DECLARE SUB party_change_updates ()
DECLARE SUB evalherotags ()
DECLARE SUB evalitemtags ()
DECLARE FUNCTION findhero (who as integer, f as integer, l as integer, d as integer) as integer
DECLARE SUB hero_swap_menu (iAll as integer)
DECLARE SUB settag (BYVAL tagnum as integer, BYVAL value as integer = 4444)
DECLARE FUNCTION istag (num as integer, zero as integer) as integer
DECLARE SUB loaddoor (map as integer)
DECLARE SUB minimap (x as integer, y as integer)
DECLARE FUNCTION teleporttool () as integer
DECLARE FUNCTION movdivis (xygo as integer) as integer
DECLARE FUNCTION onwho (caption as string, alone as integer) as integer
DECLARE SUB readjoysettings
DECLARE SUB renamehero (byval who as integer, byval escapable as integer)
DECLARE SUB resetgame (scriptout as string)
DECLARE SUB resetlmp (slot as integer, lev as integer)
DECLARE FUNCTION runscript (id as integer, index as integer, newcall as integer, double_trigger_check as integer, er as string, trigger as integer) as integer
DECLARE FUNCTION loadscript (n as unsigned integer) as ScriptData ptr
DECLARE SUB freescripts (mem as integer)
DECLARE FUNCTION commandname (byval id as integer) as string
DECLARE SUB scripterr (e as string, errorlevel as integer = 5)
DECLARE FUNCTION settingstring (searchee as string, setting as string, result as string) as integer
DECLARE SUB shop (id as integer)
DECLARE FUNCTION useinn (inn as integer, price as integer, holdscreen as integer) as integer
DECLARE SUB tagdisplay
DECLARE SUB writejoysettings
DECLARE FUNCTION herocount (last as integer = 3) as integer

#endif
