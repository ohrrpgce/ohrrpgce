#ifndef MORESUBS_BI
#define MORESUBS_BI
DECLARE SUB addhero (who, slot, stat(), forcelevel=-1)
DECLARE FUNCTION atlevel (now, a0, a99)
DECLARE FUNCTION averagelev (stat())
DECLARE SUB calibrate
DECLARE FUNCTION consumeitem (index)
DECLARE FUNCTION countitem (it)
DECLARE SUB delitem (it, amount)
DECLARE SUB doswap (s, d, stat())
DECLARE SUB drawsay ()
DECLARE SUB evalherotag (stat())
DECLARE SUB evalitemtag
DECLARE FUNCTION findhero (who, f, l, d)
DECLARE SUB getnames (stat$())
DECLARE SUB heroswap (iAll%, stat())
DECLARE FUNCTION howmanyh (f, l)
DECLARE FUNCTION istag (num, zero)
DECLARE SUB loaddoor (map)
DECLARE SUB loadgame (slot, stat())
DECLARE SUB loadglobalvars (slot, first, last)
DECLARE SUB minimap (x, y, tilesets() as TilesetData ptr)
DECLARE FUNCTION teleporttool (tilesets() as TilesetData ptr)
DECLARE SUB teleporttooltend (mini() AS UBYTE, tilemap(), tilesets() AS TilesetData ptr, BYREF zoom, BYVAL map, mapsize AS XYPair, minisize AS XYPair, offset AS XYPair)
DECLARE FUNCTION movdivis (xygo)
DECLARE FUNCTION onwho (w$, alone)
DECLARE SUB readjoysettings
DECLARE FUNCTION readscriptvar (id)
DECLARE SUB renamehero (who)
DECLARE SUB resetgame (stat(), scriptout$)
DECLARE SUB resetlmp (slot, lev)
DECLARE SUB rpgversion (v)
DECLARE FUNCTION runscript (id, index, newcall, er$, trigger)
DECLARE FUNCTION loadscript (n)
DECLARE SUB freescripts (mem)
DECLARE SUB savegame (slot, stat())
DECLARE SUB saveglobalvars (slot, first, last)
DECLARE SUB scripterr (e$)
DECLARE SUB scriptmath
DECLARE FUNCTION settingstring (searchee$, setting$, result$)
DECLARE SUB shop (id, needf, stat(), tilesets() AS TilesetData ptr)
DECLARE FUNCTION useinn (inn, price, needf, stat(), holdscreen)
DECLARE SUB snapshot
DECLARE SUB tagdisplay
DECLARE SUB writejoysettings
DECLARE SUB writescriptvar (BYVAL id, BYVAL newval)
DECLARE FUNCTION getdisplayname$ (default$)
DECLARE FUNCTION herocount () AS INTEGER
DECLARE SUB rebuild_inventory_captions (invent() AS InventSlot)
#endif