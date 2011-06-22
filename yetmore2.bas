'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "reload.bi"
#include "reloadext.bi"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "bmodsubs.bi"

Using Reload
Using Reload.Ext

REM $STATIC

FUNCTION cropmovement (x as integer, y as integer, xgo as integer, ygo as integer) as integer
 'crops movement at edge of map, or wraps
 'returns true if ran into wall at edge
 cropmovement = 0
 IF gmap(5) = 1 THEN
  '--wrap walking
  IF x < 0 THEN x = x + mapsizetiles.x * 20
  IF x >= mapsizetiles.x * 20 THEN x = x - mapsizetiles.x * 20
  IF y < 0 THEN y = y + mapsizetiles.y * 20
  IF y >= mapsizetiles.y * 20 THEN y = y - mapsizetiles.y * 20
 ELSE
  '--crop walking
  IF x < 0 THEN x = 0: xgo = 0: cropmovement = 1
  IF x > (mapsizetiles.x - 1) * 20 THEN x = (mapsizetiles.x - 1) * 20: xgo = 0: cropmovement = 1
  IF y < 0 THEN y = 0: ygo = 0: cropmovement = 1
  IF y > (mapsizetiles.y - 1) * 20 THEN y = (mapsizetiles.y - 1) * 20: ygo = 0: cropmovement = 1
 END IF
END FUNCTION

SUB defaultc
 DIM cconst(12) = {scUp,scDown,scLeft,scRight,scSpace,scEnter,scCtrl,scEsc,scAlt,scEsc,scTab,scJ,scComma}
 DIM joyconst(3) = {150,650,150,650}

 FOR i = 0 TO 12
  csetup(i) = cconst(i)
 NEXT i
 FOR i = 9 TO 12
  joy(i) = joyconst(i - 9)
 NEXT i
 EXIT SUB
END SUB

SUB forcedismount (catd())
IF vstate.active THEN
 '--clear vehicle on loading new map--
 IF vstate.dat.dismount_ahead = YES AND vstate.dat.pass_walls_while_dismounting = NO THEN
  '--dismount-ahead is true, dismount-passwalls is false
  SELECT CASE catd(0)
   CASE 0
    ygo(0) = 20
   CASE 1
    xgo(0) = -20
   CASE 2
    ygo(0) = -20
   CASE 3
    xgo(0) = 20
  END SELECT
 END IF
 IF vstate.dat.on_dismount > 0 THEN
  loadsay vstate.dat.on_dismount
 END IF
 IF vstate.dat.on_dismount < 0 THEN
  rsr = runscript(ABS(vstate.dat.on_dismount), nowscript + 1, -1, "dismount", plottrigger)
 END IF
 IF vstate.dat.riding_tag > 1 THEN setbit tag(), 0, vstate.dat.riding_tag, 0
 herospeed(0) = vstate.old_speed
 IF herospeed(0) = 3 THEN herospeed(0) = 10
 reset_vehicle vstate
 FOR i = 1 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
 NEXT i
 gam.random_battle_countdown = range(100, 60)
END IF
END SUB

'called on each coordinate of a screen position to wrap it around the map so that's it's as close as possible to being on the screen
FUNCTION closestwrappedpos (coord as integer, screenlen as integer, maplen as integer) as integer
 'consider two possibilities: one negative but as large as possible; and the one after that
 DIM as integer lowposs, highposs
 lowposs = (coord MOD maplen) + 10 'center of tile
 IF lowposs >= 0 THEN lowposs -= maplen
 highposs = lowposs + maplen

 'now evaluate which of lowposs or highposs are in or closer to the interval [0, screenlen]
 IF highposs - screenlen < 0 - lowposs THEN RETURN highposs - 10
 RETURN lowposs - 10
END FUNCTION

FUNCTION framewalkabout (x as integer, y as integer, framex as integer, framey as integer, mapwide as integer, maphigh as integer, wrapmode as integer) as integer
'Given an X and a Y returns true if a walkabout at that spot might be on-screen.
'We always return true because with offset variable sized frames and slices
'attached to NPCs, it's practically impossible to tell.
'Also checks wraparound map, and sets framex and framey
'to the position on screen most likely to be the best place to 
'draw the walkabout (closest to the screen edge). (relative to the top-left
'corner of the screen, not the top left corner of the map)
'TODO: improve by taking into account frame offset once that's implemented.

 IF wrapmode = 1 THEN
  framex = closestwrappedpos(x - mapx, vpages(dpage)->w, mapwide)
  framey = closestwrappedpos(y - mapy, vpages(dpage)->h, maphigh)
 ELSE
  framex = x - mapx
  framey = y - mapy
 END IF
 RETURN YES
END FUNCTION

SUB initgamedefaults

lastsaveslot = 0

'--items
CleanInventory inventory()

'--money
gold = gen(genStartMoney)

'--hero's speed
FOR i = 0 TO 3
 herospeed(i) = 4
NEXT i

'--hero's position
FOR i = 0 TO 15
 catx(i) = gen(genStartX) * 20
 caty(i) = gen(genStartY) * 20
 catd(i) = 2
NEXT i

END SUB

SUB innRestore ()

FOR i = 0 TO 3
 IF hero(i) > 0 THEN '--hero exists
  IF gam.hero(i).stat.cur.hp <= 0 AND readbit(gen(), 101, 4) THEN
   '--hero is dead and inn-revive is disabled
  ELSE
   '--normal revive
   gam.hero(i).stat.cur.hp = gam.hero(i).stat.max.hp
   gam.hero(i).stat.cur.mp = gam.hero(i).stat.max.mp
   resetlmp i, gam.hero(i).lev
  END IF
 END IF
NEXT i

END SUB

SUB setmapxy
SELECT CASE gen(cameramode)
 CASE herocam
  mapx = catx(gen(cameraArg)) - (vpages(dpage)->w \ 2 - 10)
  mapy = caty(gen(cameraArg)) - (vpages(dpage)->h \ 2 - 10)
 CASE npccam
  mapx = npc(gen(cameraArg)).x - (vpages(dpage)->w \ 2 - 10)
  mapy = npc(gen(cameraArg)).y - (vpages(dpage)->h \ 2 - 10)
 CASE pancam ' 1=dir, 2=ticks, 3=step
  IF gen(cameraArg2) > 0 THEN
   aheadxy mapx, mapy, gen(cameraArg), gen(cameraArg3)
   gen(cameraArg2) -= 1
  END IF
  IF gen(cameraArg2) <= 0 THEN gen(cameramode) = stopcam
 CASE focuscam ' 1=x, 2=y, 3=x step, 4=y step
  temp = gen(cameraArg) - mapx
  IF ABS(temp) <= gen(cameraArg3) THEN
   gen(cameraArg3) = 0
   mapx = gen(cameraArg)
  ELSE
   mapx += SGN(temp) * gen(cameraArg3)
  END IF
  temp = gen(cameraArg2) - mapy
  IF ABS(temp) <= gen(cameraArg4) THEN
   gen(cameraArg4) = 0
   mapy = gen(cameraArg2)
  ELSE
   mapy += SGN(temp) * gen(cameraArg4)
  END IF
  limitcamera mapx, mapy
  IF gen(cameraArg3) = 0 AND gen(cameraArg4) = 0 THEN gen(cameramode) = stopcam
END SELECT
limitcamera mapx, mapy
END SUB

SUB showplotstrings

FOR i = 0 TO 31
 '-- for each string
 IF plotstr(i).bits AND 1 THEN
  '-- only display visible strings
  IF plotstr(i).bits AND 2 THEN
    '-- flat text
    textcolor plotstr(i).Col, plotstr(i).BGCol
    printstr plotstr(i).s, plotstr(i).X, plotstr(i).Y, dpage
  ELSE
    '-- with outline
    edgeprint plotstr(i).s, plotstr(i).X, plotstr(i).Y, plotstr(i).Col, dpage
  END IF
 END IF
NEXT i

END SUB

'Returns whether the string has changed
FUNCTION strgrabber (s as string, maxl) AS INTEGER
 DIM old AS STRING = s

 '--BACKSPACE support
 IF keyval(scBackspace) > 1 AND LEN(s) > 0 THEN s = LEFT$(s, LEN(s) - 1)

 '--adding chars
 s = LEFT(s + getinputtext, maxl)

 RETURN (s <> old)
END FUNCTION

SUB makebackups
 'what is this for? Since some lumps can be modified at run time, we need to keep a
 'backup copy, and then only edit the copy. The original is never used directly.
 'enemy data
 writeablefilecopy game + ".dt1", tmpdir & "dt1.tmp"
 'formation data
 writeablefilecopy game + ".for", tmpdir & "for.tmp"
 'if you add lump-modding commands, you better well add them here >:(
END SUB

SUB correctbackdrop

IF gen(genTextboxBackdrop) THEN
 '--restore text box backdrop
 loadmxs game + ".mxs", gen(genTextboxBackdrop) - 1, vpages(3)
 EXIT SUB
END IF

IF gen(genScrBackdrop) THEN
 '--restore script backdrop
 loadmxs game + ".mxs", gen(genScrBackdrop) - 1, vpages(3)
 EXIT SUB
END IF

'loadmxs game + ".til", gmap(0), vpages(3)

END SUB

SUB cleanuptemp
 'Delete contents of/clean up workingdir
 DIM filelist() as string
 findfiles workingdir, ALLFILES, fileTypeFile, NO, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  IF usepreunlump = 0 THEN
   'normally delete everything
   safekill workingdir + SLASH + filelist(i)
  ELSE
   'but for preunlumped games only delete specific files
   DIM file_ext AS STRING = justextension$(filelist(i))
   IF file_ext = "tmp" OR file_ext = "bmd" THEN
    safekill workingdir + SLASH + filelist(i)
   END IF
  END IF
 NEXT

 'Delete contents of/clean up tmpdir
 findfiles tmpdir, ALLFILES, fileTypeFile, NO, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  IF NOT isdir(tmpdir & filelist(i)) THEN
   safekill tmpdir & filelist(i)
  END IF
 NEXT
END SUB

FUNCTION checkfordeath () as integer
checkfordeath = 0' --default alive

o = 0
FOR i = 0 TO 3 '--for each slot
 IF hero(i) > 0 THEN '--if hero exists
  o = o + 1
  IF gam.hero(i).stat.cur.hp <= 0 AND gam.hero(i).stat.max.hp > 0 THEN o = o - 1
 END IF
NEXT i
IF o = 0 THEN checkfordeath = 1

END FUNCTION

SUB aheadxy (x, y, direction, distance)
'--alters the input X and Y, moving them "ahead" by distance in direction

IF direction = 0 THEN y = y - distance
IF direction = 1 THEN x = x + distance
IF direction = 2 THEN y = y + distance
IF direction = 3 THEN x = x - distance

END SUB

SUB exitprogram (BYVAL needfade as integer, BYVAL errorout as integer = NO)

'DEBUG debug "Exiting Program"
'DEBUG debug "fade screen"
IF needfade THEN fadeout 0, 0, 0

'DEBUG debug "Cleanup Routine"
'--open files
'DEBUG debug "Close foemap handle"
CLOSE #foemaph

'--script stack
'DEBUG debug "Release script stack"
releasestack
destroystack(scrst)

'--reset audio
closemusic

'--working files
'DEBUG debug "Kill working files"
cleanuptemp
killdir tmpdir + "playing.tmp"
killdir tmpdir

'DEBUG debug "Restore Old Graphics Mode"
restoremode
'DEBUG debug "Terminate NOW (boom!)"
IF errorout = NO THEN end_debug
END errorout

END SUB

SUB verquit
 'copypage dpage, vpage
 DIM page AS INTEGER
 page = compatpage

 quitprompt$ = readglobalstring$(55, "Quit Playing?", 20)
 quityes$ = readglobalstring$(57, "Yes", 10)
 quitno$ = readglobalstring$(58, "No", 10)
 direction = 2
 ptr2 = 0
 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  wtog(0) = loopvar(wtog(0), 0, 3, 1)
  IF carray(ccMenu) > 1 THEN abortg = 0: setkeys: flusharray carray(),7,0: EXIT DO
  IF (carray(ccUse) > 1 AND ABS(ptr2) > 20) OR ABS(ptr2) > 50 THEN
   IF ptr2 < 0 THEN abortg = 1: fadeout 0, 0, 0
   setkeys
   flusharray carray(), 7, 0
   freepage page
   EXIT SUB
  END IF
  IF carray(ccLeft) > 0 THEN ptr2 = ptr2 - 5: direction = 3
  IF carray(ccRight) > 0 THEN ptr2 = ptr2 + 5: direction = 1
  centerbox 160, 95, 200, 42, 15, page
  frame_draw herow(0).sprite + direction * 2 + (wtog(0) \ 2), herow(0).pal, 150 + ptr2, 90, 1, -1, page
  edgeprint quitprompt$, xstring(quitprompt$, 160), 80, uilook(uiText), page
  col = uilook(uiMenuItem): IF ptr2 < -20 THEN col = uilook(uiSelectedItem + tog) '10 + tog * 5
  edgeprint quityes$, 70, 96, col, page
  col = uilook(uiMenuItem): IF ptr2 > 20 THEN col = uilook(uiSelectedItem + tog) '10 + tog * 5
  edgeprint quitno$, 256 - LEN(quitno$) * 8, 96, col, page
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION titlescr () as integer
titlescr = -1 ' default return true for success
loadmxs game + ".mxs", gen(genTitle), vpages(3)
needf = 2
IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
setkeys
DO
 setwait speedcontrol
 setkeys
 control
 IF carray(ccMenu) > 1 THEN
  titlescr = 0 ' return false for cancel
  EXIT DO
 END IF
 IF carray(ccUse) > 1 OR carray(ccMenu) > 1 THEN EXIT DO
 FOR i = 2 TO 88
  IF i <> scNumlock AND i <> scCapslock AND keyval(i) > 1 THEN  'DELETEME: a workaround for bug 619
   EXIT DO
  END IF
 NEXT i
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
  IF gotj(i) THEN
   IF joy(2) = 0 OR joy(3) = 0 THEN
    joy(2) = -1: joy(3) = -1
    readjoysettings
    joy(2) = -1: joy(3) = -1
    EXIT DO
   ELSE
    gotj(i) = 0
   END IF
  END IF
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN
  needf = 0
  fadein
 END IF
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
END FUNCTION

SUB reloadnpc ()
 vishero
 
 '--old-style
 FOR i AS INTEGER = 0 TO UBOUND(npcs)
  with npcs(i)
   if .sprite then frame_unload(@.sprite)
   if .pal then palette16_unload(@.pal)
   .sprite = frame_load(4, .picture)
   .pal = palette16_load(.palette, 4, .picture)
  end with
 NEXT i
 
 '--new-style
 DIM npc_id AS INTEGER
 FOR i AS INTEGER = 0 TO UBOUND(npc)
  npc_id = npc(i).id - 1
  IF npc_id >= 0 THEN
   IF npc_id > UBOUND(npcs) THEN
    debug "reloadnpc: ignore npc " & i & " because npc def " & npc_id & " is out of range (>" & UBOUND(npcs) & ")"
   ELSE
    'Update/load sprite
    set_walkabout_sprite npc(i).sl, npcs(npc_id).picture, npcs(npc_id).palette
    set_walkabout_vis npc(i).sl, (npc(i).id > 0)
   END IF
  END IF
 NEXT i
END SUB

FUNCTION mapstatetemp(mapnum as integer, prefix as string) as string
 RETURN tmpdir & prefix & mapnum
END FUNCTION

SUB savemapstate_gmap(mapnum, prefix$)
 fh = FREEFILE
 OPEN mapstatetemp$(mapnum, prefix$) + "_map.tmp" FOR BINARY AS #fh
 PUT #fh, , gmap()
 CLOSE #fh
END SUB

SUB savemapstate_npcl(mapnum, prefix$)
 DIM filename AS STRING
 filename = mapstatetemp$(mapnum, prefix$) + "_l.reld.tmp"
 save_npc_locations filename, npc()
END SUB

SUB savemapstate_npcd(mapnum, prefix$)
 SaveNPCD mapstatetemp$(mapnum, prefix$), npcs()
END SUB

SUB savemapstate_tilemap(mapnum, prefix$)
 savetilemaps maptiles(), mapstatetemp$(mapnum, prefix$) + "_t.tmp"
END SUB

SUB savemapstate_passmap(mapnum, prefix$)
 savetilemap pass, mapstatetemp$(mapnum, prefix$) + "_p.tmp"
END SUB

SUB savemapstate_zonemap(mapnum, prefix$)
 SaveZoneMap zmap, mapstatetemp$(mapnum, prefix$) + "_z.tmp"
END SUB

SUB savemapstate (mapnum, savemask = 255, prefix$)
fh = FREEFILE
IF savemask AND 1 THEN
 savemapstate_gmap mapnum, prefix$
END IF
IF savemask AND 2 THEN
 savemapstate_npcl mapnum, prefix$
END IF
IF savemask AND 4 THEN
 savemapstate_npcd mapnum, prefix$
END IF
IF savemask AND 8 THEN
 savemapstate_tilemap mapnum, prefix$
END IF
IF savemask AND 16 THEN
 savemapstate_passmap mapnum, prefix$
END IF
IF savemask AND 32 THEN
 savemapstate_zonemap mapnum, prefix$
END IF
END SUB

SUB loadmapstate_gmap (mapnum, prefix$, dontfallback = 0)
 fh = FREEFILE
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_map.tmp") THEN
  IF dontfallback = 0 THEN loadmap_gmap mapnum
  EXIT SUB
 END IF
 OPEN filebase$ + "_map.tmp" FOR BINARY AS #fh
 GET #fh, , gmap()
 CLOSE #fh
 IF gmap(31) = 0 THEN gmap(31) = 2

 loadmaptilesets tilesets(), gmap()
 refresh_map_slice_tilesets
 correctbackdrop
 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT
END SUB

SUB loadmapstate_npcl (mapnum, prefix$, dontfallback = 0)
 '--new-style
 DIM filename AS STRING
 filename = mapstatetemp(mapnum, prefix$) & "_l.reld.tmp"
 IF NOT isfile(filename) THEN
  IF dontfallback = 0 THEN loadmap_npcl mapnum
  EXIT SUB
 END IF

 load_npc_locations filename, npc()

 '--Evaluate whether NPCs should appear or disappear based on tags
 visnpc
END SUB

SUB loadmapstate_npcd (mapnum, prefix$, dontfallback = 0)
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_n.tmp") THEN
  IF dontfallback = 0 THEN loadmap_npcd mapnum
  EXIT SUB
 END IF
 LoadNPCD filebase$ + "_n.tmp", npcs()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
 'load NPC graphics
 reloadnpc
END SUB

SUB loadmapstate_tilemap (mapnum, prefix as string, dontfallback = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_t.tmp") THEN
  IF dontfallback = 0 THEN loadmap_tilemap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "t"), propersize
  GetTilemapInfo filebase + "_t.tmp", statesize

  IF statesize.wide = propersize.wide AND statesize.high = propersize.high THEN
   loadtilemaps maptiles(), filebase + "_t.tmp"
   mapsizetiles.x = maptiles(0).wide
   mapsizetiles.y = maptiles(0).high

   '--as soon as we know the dimensions of the map, enforce hero position boundaries
   cropposition catx(0), caty(0), 20

  ELSE
   DIM errmsg as string = "tried to load saved tilemap state which is size " & statesize.wide & "*" & statesize.high & ", while the map is size " & propersize.wide & "*" & propersize.high
   IF insideinterpreter THEN
    scripterr commandname(curcmd->value) + errmsg, 4
   ELSE
    debug "loadmapstate_tilemap(" + filebase + "_t.tmp): " + errmsg
   END IF
   IF dontfallback = 0 THEN loadmap_tilemap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_passmap (mapnum, prefix as string, dontfallback = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_p.tmp") THEN
  IF dontfallback = 0 THEN loadmap_passmap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "p"), propersize
  GetTilemapInfo filebase + "_p.tmp", statesize

  IF statesize.wide = propersize.wide AND statesize.high = propersize.high THEN
   loadtilemap pass, filebase + "_p.tmp"
  ELSE
   DIM errmsg as string = "tried to load saved passmap state which is size " & statesize.wide & "*" & statesize.high & ", while the map is size " & propersize.wide & "*" & propersize.high
   IF insideinterpreter THEN
    scripterr commandname(curcmd->value) + errmsg, 4
   ELSE
    debug "loadmapstate_passmap(" + filebase + "_p.tmp): " + errmsg
   END IF
   IF dontfallback = 0 THEN loadmap_passmap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_zonemap (mapnum, prefix as string, dontfallback = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_z.tmp") THEN
  IF dontfallback = 0 THEN loadmap_zonemap mapnum
 ELSE
  'Unlike tile- and passmap loading, this doesn't leave the zonemap intact if the
  'saved state is the wrong size; instead the zonemap is blanked

  LoadZoneMap zmap, filebase + "_z.tmp"
  IF zmap.wide <> mapsizetiles.x OR zmap.high <> mapsizetiles.y THEN
   DIM errmsg as string = "tried to load saved zonemap state which is size " & zmap.wide & "*" & zmap.high & ", while the map is size " & mapsizetiles.x & "*" & mapsizetiles.y
   IF insideinterpreter THEN
    scripterr commandname(curcmd->value) + errmsg, 4
   ELSE
    debug "loadmapstate_zonemap(" + filebase + "_z.tmp): " + errmsg
   END IF
   IF dontfallback THEN
    'Get rid of badly sized zonemap
    CleanZoneMap zmap, mapsizetiles.x, mapsizetiles.y
   ELSE
    loadmap_zonemap mapnum
   END IF
  END IF
 END IF
END SUB

SUB loadmapstate (mapnum, loadmask, prefix$, dontfallback = 0)
IF loadmask AND 1 THEN
 loadmapstate_gmap mapnum, prefix$, dontfallback
END IF
IF loadmask AND 2 THEN
 loadmapstate_npcl mapnum, prefix$, dontfallback
END IF
IF loadmask AND 4 THEN
 loadmapstate_npcd mapnum, prefix$, dontfallback
END IF
IF loadmask AND 8 THEN
 loadmapstate_tilemap mapnum, prefix$, dontfallback
END IF
IF loadmask AND 16 THEN
 loadmapstate_passmap mapnum, prefix$, dontfallback
END IF
IF loadmask AND 32 THEN
 loadmapstate_zonemap mapnum, prefix$, dontfallback
END IF
END SUB

SUB deletemapstate (mapnum, killmask, prefix$)
filebase$ = mapstatetemp(mapnum, "map")
IF killmask AND 1 THEN safekill filebase$ + "_map.tmp"
IF killmask AND 2 THEN safekill filebase$ + "_l.tmp"
IF killmask AND 4 THEN safekill filebase$ + "_n.tmp"
IF killmask AND 8 THEN safekill filebase$ + "_t.tmp"
IF killmask AND 16 THEN safekill filebase$ + "_p.tmp"
IF killmask AND 32 THEN safekill filebase$ + "_z.tmp"
END SUB

SUB deletetemps
'deletes game-state temporary files from tmpdir when exiting back to the titlescreen

 DIM filelist() as string
 findfiles tmpdir, ALLFILES, fileTypeFile, YES, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  filename$ = LCASE$(filelist(i))
  IF RIGHT$(filename$,4) = ".tmp" AND (LEFT$(filename$,3) = "map" OR LEFT$(filename$,5) = "state") THEN
   KILL tmpdir + filelist(i)
  END IF
 NEXT
END SUB

'--A similar function exists in customsubs.bas for custom. it differs only in error-reporting
FUNCTION decodetrigger (trigger as integer, trigtype as integer) as integer
 DIM buf(19)
 'debug "decoding " + STR$(trigger) + " type " + STR$(trigtype)
 decodetrigger = trigger  'default
 IF trigger >= 16384 THEN
  fname$ = workingdir + SLASH + "lookup" + STR$(trigtype) + ".bin"
  IF loadrecord (buf(), fname$, 20, trigger - 16384) THEN
   decodetrigger = buf(0)
   IF buf(0) = 0 THEN
    scripterr "Script " + readbinstring(buf(), 1, 36) + " is used but has not been imported", 6
   END IF
  END IF
 END IF
END FUNCTION

SUB debug_npcs ()
 debug "NPC types:"
 FOR i AS INTEGER = 0 TO UBOUND(npcs)
  debug " ID " & i & ": pic=" & npcs(i).picture & " pal=" & npcs(i).palette
 NEXT
 debug "NPC instances:"
 FOR i AS INTEGER = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM AS INTEGER drawX, drawY
    IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawX, drawY, mapsizetiles.x * 20, mapsizetiles.y * 20, gmap(5)) THEN
     debug " " & i & ": ID=" & (ABS(.id) - 1) & iif_string(.id < 0, " (hidden)", "") & " x=" & .x & " y=" & .y & " screenx=" & drawX & " screeny=" & drawY
    ELSE
     debug " " & i & ": ID=" & (ABS(.id) - 1) & iif_string(.id < 0, " (hidden)", "") & " x=" & .x & " y=" & .y
    END IF
   END IF
  END WITH
 NEXT
END SUB

SUB npc_debug_display ()
 DIM temp AS STRING
 STATIC tog
 tog = tog XOR 1
 FOR i AS INTEGER = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM AS INTEGER drawX, drawY
    IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawX, drawY, mapsizetiles.x * 20, mapsizetiles.y * 20, gmap(5)) THEN
     textcolor uilook(uiText), 0
     'the numbers can overlap quite badly, try to squeeze them in
     temp = iif_string(.id < 0, "-", "") & (ABS(.id) - 1)
     printstr MID$(temp, 1, 1), drawX, drawY + 4, dpage
     printstr MID$(temp, 2, 1), drawX + 7, drawY + 4, dpage
     printstr MID$(temp, 3, 1), drawX + 14, drawY + 4, dpage
     textcolor uilook(uiDescription), 0
     temp = STR$(i + 1)
     printstr MID$(temp, 1, 1), drawX, drawY + 12, dpage
     printstr MID$(temp, 2, 1), drawX + 7, drawY + 12, dpage
     printstr MID$(temp, 3, 1), drawX + 14, drawY + 12, dpage
    END IF
   END IF
  END WITH
 NEXT
END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB limitcamera (BYREF x AS INTEGER, BYREF y AS INTEGER)
 IF gmap(5) = 0 THEN
  'when cropping the camera to the map, stop camera movements that attempt to go over the edge
  DIM oldmapx AS INTEGER = x
  DIM oldmapy AS INTEGER = y
  x = bound(x, 0, mapsizetiles.x * 20 - 320)
  y = bound(y, 0, mapsizetiles.y * 20 - 200)
  IF oldmapx <> x THEN
   IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
   IF gen(cameramode) = focuscam THEN gen(cameraArg3) = 0
  END IF
  IF oldmapy <> y THEN
   IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
   IF gen(cameramode) = focuscam THEN gen(cameraArg4) = 0
  END IF
 END IF
 IF gmap(5) = 1 THEN
  'Wrap the camera according to the center, not the top-left
  x += 160
  y += 100
  wrapxy x, y, mapsizetiles.x * 20, mapsizetiles.y * 20
  x -= 160
  y -= 100
 END IF
END SUB

FUNCTION game_setoption(opt as string, arg as string) as integer
 IF opt = "errlvl" THEN
  IF is_int(arg) THEN
   err_suppress_lvl = str2int(arg, 4)
   RETURN 2
  ELSE
   RETURN 1
  END IF
 END IF
 IF opt = "autotest" THEN
  debug "Autotesting mode enabled!"
  autotestmode = YES
  RETURN 1 'arg not used
 END IF
 RETURN 0
END FUNCTION

'return a video page which is a view on vpage that is 320x200 (or smaller) and centred
FUNCTION compatpage() as integer
 DIM fakepage AS INTEGER
 DIM centreview AS Frame ptr
 centreview = frame_new_view(vpages(vpage), (vpages(vpage)->w - 320) / 2, (vpages(vpage)->h - 200) / 2, 320, 200)
 fakepage = registerpage(centreview)
 frame_unload @centreview
 RETURN fakepage
END FUNCTION
