#ifndef MORESUBS_BI
#define MORESUBS_BI
DECLARE SUB addhero (who as integer, slot as integer, stat() as integer, forcelevel as integer=-1)
DECLARE FUNCTION atlevel (now as integer, a0 as integer, a99 as integer) as integer
DECLARE FUNCTION averagelev (stat() as integer) as integer
DECLARE SUB calibrate
DECLARE FUNCTION consumeitem (index as integer) as integer
DECLARE FUNCTION countitem (it as integer) as integer
DECLARE SUB delitem (it as integer, amount as integer)
DECLARE SUB doswap (s as integer, d as integer, stat() as integer)
DECLARE SUB drawsay ()
DECLARE SUB evalherotag (stat() as integer)
DECLARE SUB evalitemtag
DECLARE FUNCTION findhero (who as integer, f as integer, l as integer, d as integer) as integer
DECLARE SUB heroswap (iAll as integer, stat() as integer)
DECLARE FUNCTION istag (num as integer, zero as integer) as integer
DECLARE SUB loaddoor (map as integer)
DECLARE SUB loadgame (slot as integer, stat() as integer)
DECLARE SUB loadglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB minimap (x as integer, y as integer, tilesets() as TilesetData ptr)
DECLARE FUNCTION teleporttool (tilesets() as TilesetData ptr) as integer
DECLARE FUNCTION movdivis (xygo as integer) as integer
DECLARE FUNCTION onwho (caption as string, alone as integer) as integer
DECLARE SUB readjoysettings
DECLARE FUNCTION readscriptvar (id as integer) as integer
DECLARE SUB renamehero (who as integer)
DECLARE SUB resetgame (stat() as integer, scriptout as string)
DECLARE SUB resetlmp (slot as integer, lev as integer)
DECLARE FUNCTION runscript (id as integer, index as integer, newcall as integer, er as string, trigger as integer) as integer
DECLARE FUNCTION loadscript (n as unsigned integer) as ScriptData ptr
DECLARE SUB freescripts (mem as integer)
DECLARE FUNCTION commandname (byval id as integer) as string
DECLARE SUB savegame (slot as integer, stat() as integer)
DECLARE SUB saveglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB scripterr (e as string, errorlevel as integer = 4)
DECLARE SUB scriptmath
DECLARE FUNCTION settingstring (searchee as string, setting as string, result as string) as integer
DECLARE SUB shop (id as integer, needf as integer, stat() as integer, tilesets() AS TilesetData ptr)
DECLARE FUNCTION useinn (inn as integer, price as integer, needf as integer, stat() as integer, holdscreen as integer) as integer
DECLARE SUB snapshot
DECLARE SUB tagdisplay
DECLARE SUB writejoysettings
DECLARE SUB writescriptvar (BYVAL id as integer, BYVAL newval as integer)
DECLARE FUNCTION herocount (last as integer = 3) as integer
#endif
