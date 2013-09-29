'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

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
#include "hsinterpreter.bi"
#include "ver.txt"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "savegame.bi"
#include "bmodsubs.bi"

Using Reload
Using Reload.Ext

FUNCTION cropmovement (byref x as integer, byref y as integer, byref xgo as integer, byref ygo as integer) as integer
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
 DIM cconst(12) as integer = {scUp,scDown,scLeft,scRight,scSpace,scEnter,scCtrl,scEsc,scAlt,scEsc,scTab,scJ,scComma}
 DIM joyconst(3) as integer = {150,650,150,650}

 FOR i as integer = 0 TO 12
  csetup(i) = cconst(i)
 NEXT i
 FOR i as integer = 9 TO 12
  joy(i) = joyconst(i - 9)
 NEXT i
 EXIT SUB
END SUB

SUB forcedismount (catd() as integer)
IF vstate.active THEN
 '--clear vehicle on loading new map--
 IF vstate.dat.dismount_ahead = YES AND vstate.dat.pass_walls_while_dismounting = NO THEN
  '--dismount-ahead is true, dismount-passwalls is false
  SELECT CASE catd(0)
   CASE 0
    herow(0).ygo = 20
   CASE 1
    herow(0).xgo = -20
   CASE 2
    herow(0).ygo = -20
   CASE 3
    herow(0).xgo = 20
  END SELECT
 END IF
 IF vstate.dat.on_dismount > 0 THEN
  loadsay vstate.dat.on_dismount
 END IF
 IF vstate.dat.on_dismount < 0 THEN
  trigger_script ABS(vstate.dat.on_dismount), YES, "vehicle dismount", "", scrqBackcompat()
 END IF
 settag vstate.dat.riding_tag, NO
 herow(0).speed = vstate.old_speed
 reset_vehicle vstate
 FOR i as integer = 1 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
 NEXT i
 gam.random_battle_countdown = range(100, 60)
END IF
END SUB

'called on each coordinate of a screen position to wrap it around the map so that's it's as close as possible to being on the screen
FUNCTION closestwrappedpos (byval coord as integer, byval screenlen as integer, byval maplen as integer) as integer
 'consider two possibilities: one negative but as large as possible; and the one after that
 DIM as integer lowposs, highposs
 lowposs = (coord MOD maplen) + 10 'center of tile
 IF lowposs >= 0 THEN lowposs -= maplen
 highposs = lowposs + maplen

 'now evaluate which of lowposs or highposs are in or closer to the interval [0, screenlen]
 IF highposs - screenlen < 0 - lowposs THEN RETURN highposs - 10
 RETURN lowposs - 10
END FUNCTION

FUNCTION framewalkabout (byval x as integer, byval y as integer, byref framex as integer, byref framey as integer, byval mapwide as integer, byval maphigh as integer, byval wrapmode as integer) as integer
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
FOR i as integer = 0 TO 3
 herow(i).speed = 4
NEXT i

'--hero's position
FOR i as integer = 0 TO 15
 catx(i) = gen(genStartX) * 20
 caty(i) = gen(genStartY) * 20
 catd(i) = 2
NEXT i

END SUB

SUB innRestore ()
 FOR i as integer = 0 TO 3
  IF hero(i) > 0 THEN '--hero exists
   IF gam.hero(i).stat.cur.hp <= 0 AND readbit(gen(), genBits, 4) THEN
    '--hero is dead and inn-revive is disabled
   ELSE
    '--normal revive
    gam.hero(i).stat.cur.hp = gam.hero(i).stat.max.hp
    gam.hero(i).stat.cur.mp = gam.hero(i).stat.max.mp
    resetlmp i, gam.hero(i).lev
   END IF
  END IF
 NEXT i
 party_change_updates
END SUB

SUB center_camera_on_walkabout(byval walkabout_cont as Slice ptr)
 IF walkabout_cont = NULL THEN debug "NULL walkabout slice in center_camera_on_walkabout" : EXIT SUB

 DIM sprsl as Slice ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
 RefreshSliceScreenPos sprsl

 mapx = sprsl->ScreenX + sprsl->Width/2 - SliceTable.MapRoot->ScreenX - vpages(dpage)->w \ 2
 mapy = sprsl->ScreenY + sprsl->Height/2 - SliceTable.MapRoot->ScreenY - vpages(dpage)->h \ 2

 ' For compatibility (maybe some old games use setheroz for falling from the sky?)
 ' ignore the Z component of the sprite slice.
 ' FIXME: Add a bit to disable this.
 IF YES THEN
  mapy -= sprsl->Y
 END IF
END SUB

SUB setmapxy
SELECT CASE gen(cameramode)
 CASE herocam
  center_camera_on_walkabout herow(gen(cameraArg)).sl
 CASE npccam
  center_camera_on_walkabout npc(gen(cameraArg)).sl
 CASE pancam ' 1=dir, 2=ticks, 3=step
  IF gen(cameraArg2) > 0 THEN
   aheadxy mapx, mapy, gen(cameraArg), gen(cameraArg3)
   gen(cameraArg2) -= 1
  END IF
  IF gen(cameraArg2) <= 0 THEN gen(cameramode) = stopcam
 CASE focuscam ' 1=x, 2=y, 3=x step, 4=y step
  DIM camdiff as integer
  camdiff = gen(cameraArg) - mapx
  IF ABS(camdiff) <= gen(cameraArg3) THEN
   gen(cameraArg3) = 0
   mapx = gen(cameraArg)
  ELSE
   mapx += SGN(camdiff) * gen(cameraArg3)
  END IF
  camdiff = gen(cameraArg2) - mapy
  IF ABS(camdiff) <= gen(cameraArg4) THEN
   gen(cameraArg4) = 0
   mapy = gen(cameraArg2)
  ELSE
   mapy += SGN(camdiff) * gen(cameraArg4)
  END IF
  limitcamera mapx, mapy
  IF gen(cameraArg3) = 0 AND gen(cameraArg4) = 0 THEN gen(cameramode) = stopcam
END SELECT
limitcamera mapx, mapy
END SUB

SUB showplotstrings

FOR i as integer = 0 TO UBOUND(plotstr)
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
FUNCTION strgrabber (s as string, byval maxl as integer) as integer
 DIM old as string = s

 '--BACKSPACE support
 IF keyval(scBackspace) > 1 AND LEN(s) > 0 THEN s = LEFT(s, LEN(s) - 1)

 '--adding chars
 s = LEFT(s + getinputtext, maxl)

 RETURN (s <> old)
END FUNCTION

SUB makebackups
 'what is this for? Since some lumps can be modified at run time, we need to keep a
 'backup copy, and then only edit the copy. The original is never used directly.
 'enemy data
 writeablecopyfile game + ".dt1", tmpdir & "dt1.tmp"
 'formation data
 writeablecopyfile game + ".for", tmpdir & "for.tmp"
 'if you add lump-modding commands, you better well add them here >:(
END SUB

SUB make_map_backups
 'This back-ups certain map lumps so that when live previewing a game, changes can be three-way merged
 writeablecopyfile maplumpname(gam.map.id, "t"), tmpdir & "mapbackup.t"
 writeablecopyfile maplumpname(gam.map.id, "p"), tmpdir & "mapbackup.p"
 writeablecopyfile maplumpname(gam.map.id, "l"), tmpdir & "mapbackup.l"
 'Global lump which doesn't need to be backed up on every map change... actually maybe doing that interferes
 'with merging? Well, putting this here keeps things simple
 writeablecopyfile game + ".map", tmpdir & "mapbackup.map"
END SUB

SUB update_backdrop_slice
 DIM backdrop as integer
 DIM transparent as integer = NO
 IF gen(genTextboxBackdrop) THEN
  backdrop = gen(genTextboxBackdrop) - 1
  transparent = txt.box.backdrop_trans
 ELSEIF gen(genScrBackdrop) THEN
  backdrop = gen(genScrBackdrop) - 1
 ELSE
  SliceTable.Backdrop->Visible = NO
  EXIT SUB
 END IF
 SliceTable.Backdrop->Visible = YES
 ChangeSpriteSlice SliceTable.Backdrop, sprTypeMXS, backdrop, , , , , transparent
 
END SUB

SUB cleanuptemp
 REDIM filelist() as string
 debuginfo "cleaningup " & workingdir & " and " & tmpdir

 'Delete contents of/clean up workingdir
 '(Length requirement is just a crappy sanity check)
 IF running_as_slave = NO ANDALSO LEN(workingdir) > 5 THEN
  findfiles workingdir, ALLFILES, fileTypeFile, NO, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
   IF usepreunlump = 0 THEN
    'normally delete everything
    safekill workingdir + SLASH + filelist(i)
   ELSE
    'but for preunlumped games only delete specific files
    'FIXME: aside from upgrade(), are any files actually created in workingdir? We definitely SHOULD NOT do that!
    DIM file_ext as string = justextension(filelist(i))
    IF file_ext = "tmp" THEN
     safekill workingdir + SLASH + filelist(i)
    END IF
   END IF
  NEXT
 END IF

 'Delete contents of/clean up tmpdir without actually deleting the tmpdir itself
 'FIXME: I am pretty sure there is no good reason not to call killdir with recursive=YES
 'FIXME: harmless warning?, killdir claims it cannot delete tmpdir because it is not empty
 '       even though it clearly IS empty, and then seems to succeed in deleting it with no
 '       problem
 IF LEN(tmpdir) > 5 THEN
  killdir tmpdir
  MKDIR tmpdir
 END IF
END SUB

FUNCTION checkfordeath () as bool
 RETURN liveherocount = 0
END FUNCTION

SUB aheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer)
'--alters the input X and Y, moving them "ahead" by distance in direction

IF direction = 0 THEN y = y - distance
IF direction = 1 THEN x = x + distance
IF direction = 2 THEN y = y + distance
IF direction = 3 THEN x = x - distance

END SUB

SUB exitprogram (byval need_fade_out as bool = NO, byval errorout as integer = 0)

'DEBUG debug "Exiting Program"
'DEBUG debug "fade screen"
IF need_fade_out THEN fadeout 0, 0, 0

'DEBUG debug "Cleanup Routine"

'--script stack
'DEBUG debug "Release script stack"
releasestack
destroystack(scrst)

'--reset audio
closemusic

'--working files
'DEBUG debug "Kill working files"
cleanuptemp
IF NOT running_as_slave THEN killdir tmpdir + "playing.tmp"
killdir tmpdir

v_free modified_lumps

'DEBUG debug "Restore Old Graphics Mode"
restoremode
'DEBUG debug "Terminate NOW (boom!)"
IF errorout = 0 THEN end_debug
END errorout

END SUB

SUB verify_quit
 'copypage dpage, vpage
 DIM page as integer
 page = compatpage

 DIM quitprompt as string = readglobalstring(55, "Quit Playing?", 20)
 DIM quityes as string = readglobalstring(57, "Yes", 10)
 DIM quitno as string = readglobalstring(58, "No", 10)
 DIM direction as integer = 2
 DIM ptr2 as integer = 0
 DIM tog as integer
 DIM wtog as integer
 DIM col as integer
 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  wtog = loopvar(wtog, 0, 3, 1)
  IF carray(ccMenu) > 1 THEN EXIT DO
  IF (carray(ccUse) > 1 AND ABS(ptr2) > 20) OR ABS(ptr2) > 50 THEN
   IF ptr2 < 0 THEN abortg = 1: fadeout 0, 0, 0
   EXIT DO
  END IF
  IF carray(ccLeft) > 0 THEN ptr2 = ptr2 - 5: direction = 3
  IF carray(ccRight) > 0 THEN ptr2 = ptr2 + 5: direction = 1

  centerbox 160, 95, 200, 42, 15, page
  set_walkabout_frame herow(0).sl, direction, wtog \ 2
  DrawSliceAt LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, herow(0).sl), 150 + ptr2, 90, 20, 20, page, YES
  edgeprint quitprompt, xstring(quitprompt, 160), 80, uilook(uiText), page
  col = uilook(uiMenuItem)
  IF ptr2 < -20 THEN col = uilook(uiSelectedItem + tog)
  edgeprint quityes, 70, 96, col, page
  col = uilook(uiMenuItem)
  IF ptr2 > 20 THEN col = uilook(uiSelectedItem + tog)
  edgeprint quitno, 256 - LEN(quitno) * 8, 96, col, page
  setvispage vpage
  dowait
 LOOP
 setkeys
 flusharray carray(), 7, 0
 freepage page
END SUB

FUNCTION titlescreen () as integer
 loadmxs game + ".mxs", gen(genTitle), vpages(3)
 queue_fade_in 1
 IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
 setkeys
 DO
  setwait speedcontrol
  setkeys
  control
  IF carray(ccMenu) > 1 THEN
   RETURN NO
  END IF
  IF anykeypressed() THEN RETURN YES

  copypage 3, vpage
  setvispage vpage
  check_for_queued_fade_in
  dowait
 LOOP
END FUNCTION

SUB reloadnpc ()
 DIM npc_id as integer
 FOR i as integer = 0 TO UBOUND(npc)
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

FUNCTION mapstatetemp(byval mapnum as integer, prefix as string) as string
 RETURN tmpdir & prefix & mapnum
END FUNCTION

SUB savemapstate_gmap(byval mapnum as integer, prefix as string)
 DIM fh as integer = FREEFILE
 OPEN mapstatetemp(mapnum, prefix) & "_map.tmp" FOR BINARY as #fh
 PUT #fh, , gmap()
 CLOSE #fh
END SUB

SUB savemapstate_npcl(byval mapnum as integer, prefix as string)
 DIM filename as string = mapstatetemp(mapnum, prefix) & "_l.reld.tmp"
 save_npc_locations filename, npc()
END SUB

SUB savemapstate_npcd(byval mapnum as integer, prefix as string)
 SaveNPCD mapstatetemp(mapnum, prefix) & "_n.tmp", npcs()
END SUB

SUB savemapstate_tilemap(byval mapnum as integer, prefix as string)
 savetilemaps maptiles(), mapstatetemp(mapnum, prefix) & "_t.tmp"
END SUB

SUB savemapstate_passmap(byval mapnum as integer, prefix as string)
 savetilemap pass, mapstatetemp(mapnum, prefix) & "_p.tmp"
END SUB

SUB savemapstate_zonemap(byval mapnum as integer, prefix as string)
 SaveZoneMap zmap, mapstatetemp(mapnum, prefix) & "_z.tmp"
END SUB

SUB savemapstate (byval mapnum as integer, byval savemask as integer = 255, prefix as string)
IF savemask AND 1 THEN
 savemapstate_gmap mapnum, prefix
END IF
IF savemask AND 2 THEN
 savemapstate_npcl mapnum, prefix
END IF
IF savemask AND 4 THEN
 savemapstate_npcd mapnum, prefix
END IF
IF savemask AND 8 THEN
 savemapstate_tilemap mapnum, prefix
END IF
IF savemask AND 16 THEN
 savemapstate_passmap mapnum, prefix
END IF
IF savemask AND 32 THEN
 savemapstate_zonemap mapnum, prefix
END IF
END SUB

SUB loadmapstate_gmap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 DIM fh as integer = FREEFILE
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase & "_map.tmp") THEN
  IF dontfallback = 0 THEN loadmap_gmap mapnum
  EXIT SUB
 END IF
 lump_reloading.gmap.dirty = NO  'Not correct, but too much trouble to do correctly
 lump_reloading.gmap.changed = NO

 OPEN filebase & "_map.tmp" FOR BINARY as #fh
 GET #fh, , gmap()
 CLOSE #fh
 IF gmap(31) = 0 THEN gmap(31) = 2

 loadmaptilesets tilesets(), gmap()
 refresh_map_slice_tilesets
 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT
END SUB

SUB loadmapstate_npcl (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 '--new-style
 DIM filename as string
 filename = mapstatetemp(mapnum, prefix) & "_l.reld.tmp"
 IF NOT isfile(filename) THEN
  IF dontfallback = 0 THEN loadmap_npcl mapnum
  EXIT SUB
 END IF

 load_npc_locations filename, npc()

 '--Evaluate whether NPCs should appear or disappear based on tags
 visnpc
END SUB

SUB loadmapstate_npcd (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase & "_n.tmp") THEN
  IF dontfallback = 0 THEN loadmap_npcd mapnum
  EXIT SUB
 END IF
 LoadNPCD filebase & "_n.tmp", npcs()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
 'load NPC graphics
 reloadnpc
END SUB

SUB loadmapstate_tilemap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_t.tmp") THEN
  IF dontfallback = 0 THEN loadmap_tilemap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "t"), propersize
  GetTilemapInfo filebase + "_t.tmp", statesize

  IF statesize.wide = propersize.wide AND statesize.high = propersize.high THEN
   lump_reloading.maptiles.dirty = NO  'Not correct, but too much trouble to do correctly
   lump_reloading.maptiles.changed = NO

   loadtilemaps maptiles(), filebase + "_t.tmp"
   mapsizetiles.x = maptiles(0).wide
   mapsizetiles.y = maptiles(0).high
   refresh_map_slice

   '--as soon as we know the dimensions of the map, enforce hero position boundaries
   cropposition catx(0), caty(0), 20

  ELSE
   DIM errmsg as string = "tried to load saved tilemap state which is size " & statesize.wide & "*" & statesize.high & ", while the map is size " & propersize.wide & "*" & propersize.high
   IF insideinterpreter THEN
    scripterr current_command_name() + errmsg, 4
   ELSE
    debug "loadmapstate_tilemap(" + filebase + "_t.tmp): " + errmsg
   END IF
   IF dontfallback = 0 THEN loadmap_tilemap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_passmap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_p.tmp") THEN
  IF dontfallback = 0 THEN loadmap_passmap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "p"), propersize
  GetTilemapInfo filebase + "_p.tmp", statesize

  IF statesize.wide = propersize.wide AND statesize.high = propersize.high THEN
   lump_reloading.passmap.dirty = NO  'Not correct, but too much trouble to do correctly
   lump_reloading.passmap.changed = NO
   loadtilemap pass, filebase + "_p.tmp"
  ELSE
   DIM errmsg as string = "tried to load saved passmap state which is size " & statesize.wide & "*" & statesize.high & ", while the map is size " & propersize.wide & "*" & propersize.high
   IF insideinterpreter THEN
    scripterr current_command_name() + errmsg, 4
   ELSE
    debug "loadmapstate_passmap(" + filebase + "_p.tmp): " + errmsg
   END IF
   IF dontfallback = 0 THEN loadmap_passmap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_zonemap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_z.tmp") THEN
  IF dontfallback = 0 THEN loadmap_zonemap mapnum
 ELSE
  'Unlike tile- and passmap loading, this doesn't leave the zonemap intact if the
  'saved state is the wrong size; instead the zonemap is blanked

  lump_reloading.zonemap.dirty = NO  'Not correct, but too much trouble to do correctly
  lump_reloading.zonemap.changed = NO
  LoadZoneMap zmap, filebase + "_z.tmp"
  IF zmap.wide <> mapsizetiles.x OR zmap.high <> mapsizetiles.y THEN
   DIM errmsg as string = "tried to load saved zonemap state which is size " & zmap.wide & "*" & zmap.high & ", while the map is size " & mapsizetiles.x & "*" & mapsizetiles.y
   IF insideinterpreter THEN
    scripterr current_command_name() + errmsg, 4
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

SUB loadmapstate (byval mapnum as integer, byval loadmask as integer, prefix as string, byval dontfallback as integer = 0)
IF loadmask AND 1 THEN
 loadmapstate_gmap mapnum, prefix, dontfallback
END IF
IF loadmask AND 2 THEN
 loadmapstate_npcl mapnum, prefix, dontfallback
END IF
IF loadmask AND 4 THEN
 loadmapstate_npcd mapnum, prefix, dontfallback
END IF
IF loadmask AND 8 THEN
 loadmapstate_tilemap mapnum, prefix, dontfallback
END IF
IF loadmask AND 16 THEN
 loadmapstate_passmap mapnum, prefix, dontfallback
END IF
IF loadmask AND 32 THEN
 loadmapstate_zonemap mapnum, prefix, dontfallback
END IF
END SUB

SUB deletemapstate (byval mapnum as integer, byval killmask as integer, prefix as string)
 dim filebase as string = mapstatetemp(mapnum, prefix)
 IF killmask AND 1 THEN safekill filebase + "_map.tmp"
 IF killmask AND 2 THEN safekill filebase + "_l.reld.tmp"
 IF killmask AND 4 THEN safekill filebase + "_n.tmp"
 IF killmask AND 8 THEN safekill filebase + "_t.tmp"
 IF killmask AND 16 THEN safekill filebase + "_p.tmp"
 IF killmask AND 32 THEN safekill filebase + "_z.tmp"
END SUB

'Note a differing number of layers is allowed!
FUNCTION tilemap_is_same_size (lumptype as string, what as string) as integer
 DIM as TilemapInfo newsize
 GetTilemapInfo maplumpname(gam.map.id, lumptype), newsize

 IF newsize.wide <> mapsizetiles.w OR newsize.high <> mapsizetiles.h THEN
  notification "Could not reload " + what + " because the map size has changed. The map must be reloaded. You can do so by pressing F5 to access the Live Preview Debug Menu and selecting 'Reload map'."
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

'gmap is a mess; some of it is data that belongs in a replacement lump .T (eg. tileset stuff).
'So several functions segregate the data.
FUNCTION gmap_index_affects_tiles(byval index as integer) as integer
 SELECT CASE index
  CASE 0, 19, 22 TO 24, 26 TO 31
   RETURN YES
  CASE ELSE
   RETURN NO
 END SELECT
END FUNCTION

#DEFINE debug_reloadmap(what)  debuginfo __FUNCTION__ " " #what ".dirty=" & lump_reloading.what.dirty & " " #what ".changed=" & lump_reloading.what.changed & " " #what ".mode=" & lump_reloading.what.mode

SUB reloadmap_gmap_no_tilesets()
 debug_reloadmap(gmap)
 lump_reloading.gmap.dirty = NO
 lump_reloading.gmap.changed = NO

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_map.tmp" 

 REDIM gmaptmp(dimbinsize(binMAP)) as integer
 loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) \ 2, gam.map.id
 IF gmaptmp(31) = 0 THEN gmaptmp(31) = 2

 FOR i as integer = 0 TO UBOUND(gmap)
  IF gmap_index_affects_tiles(i) = NO THEN gmap(i) = gmaptmp(i)
 NEXT

 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT

 IF gmap(1) > 0 THEN
  wrappedsong gmap(1) - 1
 ELSEIF gmap(1) = 0 THEN
  stopsong
 END IF
END SUB

SUB reloadmap_npcl(byval merge as integer)
 lump_reloading.npcl.changed = NO
 
 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_l.reld.tmp"

 DIM filename as string = maplumpname(gam.map.id, "l")
 lump_reloading.npcl.hash = hash_file(filename)
 IF merge THEN
  REDIM npcnew(UBOUND(npc)) as NPCInst
  REDIM npcbase(UBOUND(npc)) as NPCInst
  LoadNPCL filename, npcnew()
  LoadNPCL tmpdir + "mapbackup.l", npcbase()

  FOR i as integer = 0 TO UBOUND(npc)
   IF memcmp(@npcnew(i), @npcbase(i), SIZEOF(NPCInst)) THEN
    'If an NPC was removed in Custom, and meanwhile it's been replaced in-game, then
    'don't overwrite it.
    IF npc(i).id <> npcbase(i).id AND npcnew(i).id = 0 THEN CONTINUE FOR
    'Otherwise Custom has priority
    CleanNPCInst npc(i)
    npc(i) = npcnew(i)
   END IF
  NEXT
 ELSE
  LoadNPCL filename, npc()
  writeablecopyfile filename, tmpdir + "mapbackup.l"
 END IF

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
END SUB

SUB reloadmap_npcd()
 lump_reloading.npcd.changed = NO
 lump_reloading.npcd.dirty = NO

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_n.tmp"

 DIM filename as string = maplumpname(gam.map.id, "n")
 lump_reloading.npcd.hash = hash_file(filename)
 LoadNPCD filename, npcs()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
 'load NPC graphics
 reloadnpc
END SUB

SUB reloadmap_tilemap_and_tilesets(byval merge as integer)
 debug_reloadmap(maptiles)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_t.tmp"

 IF tilemap_is_same_size("t", "tilemaps") THEN
  lump_reloading.maptiles.changed = NO

  DIM filename as string = maplumpname(gam.map.id, "t")
  lump_reloading.maptiles.hash = hash_file(filename)
  IF merge THEN
   'Note: Its possible for this to fail if the number of layers differs
   MergeTileMaps maptiles(), filename, tmpdir + "mapbackup.t"
  ELSE
   lump_reloading.maptiles.dirty = NO
   LoadTileMaps maptiles(), filename
   writeablecopyfile filename, tmpdir + "mapbackup.t"
  END IF
  refresh_map_slice

  'Now reload tileset and layering info
  REDIM gmaptmp(dimbinsize(binMAP)) as integer
  loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) \ 2, gam.map.id

  FOR i as integer = 0 TO UBOUND(gmap)
   IF gmap_index_affects_tiles(i) THEN gmap(i) = gmaptmp(i)
  NEXT

  loadmaptilesets tilesets(), gmap()
  refresh_map_slice_tilesets
 END IF
END SUB

SUB reloadmap_passmap(byval merge as integer)
 debug_reloadmap(passmap)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_p.tmp"

 IF tilemap_is_same_size("p", "wallmap") THEN
  lump_reloading.passmap.changed = NO

  DIM filename as string = maplumpname(gam.map.id, "p")
  lump_reloading.passmap.hash = hash_file(filename)
  IF merge THEN
   MergeTileMap pass, filename, tmpdir + "mapbackup.p"
  ELSE
   lump_reloading.passmap.dirty = NO
   LoadTileMap pass, filename
   writeablecopyfile filename, tmpdir + "mapbackup.p"
  END IF
 END IF
END SUB

SUB reloadmap_foemap()
 debug_reloadmap(foemap)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_e.tmp"

 IF tilemap_is_same_size("e", "foemap") THEN
  lump_reloading.foemap.changed = NO
  lump_reloading.foemap.dirty = NO
  DIM filename as string = maplumpname(gam.map.id, "e")
  LoadTileMap foemap, filename
  lump_reloading.foemap.hash = hash_file(filename)
 END IF
END SUB

SUB reloadmap_zonemap()
 debug_reloadmap(zonemap)
 lump_reloading.zonemap.changed = NO
 lump_reloading.zonemap.dirty = NO

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_z.tmp"

 '.Z is the only one of the map lumps that has been added in about the last decade
 DIM filename as string = maplumpname(gam.map.id, "z")
 IF isfile(filename) THEN
  LoadZoneMap zmap, filename
  lump_reloading.zonemap.hash = hash_file(filename)
 ELSE
  CleanZoneMap zmap, mapsizetiles.x, mapsizetiles.y
  lump_reloading.zonemap.hash = 0
 END IF
END SUB

SUB deletetemps
'deletes game-state temporary files from tmpdir when exiting back to the titlescreen

 REDIM filelist() as string
 findfiles tmpdir, ALLFILES, fileTypeFile, YES, filelist()
 DIM filename as string
 FOR i as integer = 0 TO UBOUND(filelist)
  filename = LCASE(filelist(i))
  IF RIGHT(filename,4) = ".tmp" AND (LEFT(filename,3) = "map" OR LEFT(filename,5) = "state") THEN
   KILL tmpdir + filelist(i)
  END IF
 NEXT
END SUB

SUB debug_npcs ()
 debug "NPC types:"
 FOR i as integer = 0 TO UBOUND(npcs)
  debug " ID " & i & ": pic=" & npcs(i).picture & " pal=" & npcs(i).palette
 NEXT
 debug "NPC instances:"
 FOR i as integer = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM as integer drawX, drawY
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
 DIM temp as string
 FOR i as integer = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM as integer drawX, drawY
    IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawX, drawY, mapsizetiles.x * 20, mapsizetiles.y * 20, gmap(5)) THEN
     textcolor uilook(uiText), 0
     'the numbers can overlap quite badly, try to squeeze them in
     temp = iif_string(.id < 0, "-", "") & (ABS(.id) - 1)
     printstr MID(temp, 1, 1), drawX, drawY + 4, dpage
     printstr MID(temp, 2, 1), drawX + 7, drawY + 4, dpage
     printstr MID(temp, 3, 1), drawX + 14, drawY + 4, dpage
     textcolor uilook(uiDescription), 0
     temp = STR(i + 1)
     printstr MID(temp, 1, 1), drawX, drawY + 12, dpage
     printstr MID(temp, 2, 1), drawX + 7, drawY + 12, dpage
     printstr MID(temp, 3, 1), drawX + 14, drawY + 12, dpage
    END IF
   END IF
  END WITH
 NEXT
END SUB

SUB limitcamera (byref x as integer, byref y as integer)
 IF gmap(5) = 0 THEN
  'when cropping the camera to the map, stop camera movements that attempt to go over the edge
  DIM oldmapx as integer = x
  DIM oldmapy as integer = y
  x = bound(x, 0, mapsizetiles.x * 20 - 320)
  y = bound(y, 0, mapsizetiles.y * 20 - 200)
  IF oldmapx <> x THEN
   IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
  END IF
  IF oldmapy <> y THEN
   IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
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
 ELSEIF opt = "autotest" THEN
  debug "Autotesting mode enabled!"
  autotestmode = YES
  enable_speed_control NO
  RETURN 1 'arg not used
 ELSEIF opt = "runfast" THEN
  debuginfo "Running without speed control"
  enable_speed_control NO
  RETURN 1 'arg not used
 ELSEIF opt = "autosnap" then
  IF is_int(arg) THEN
   autosnap = str2int(arg)
   debug "Autosnap mode enabled every " & autosnap & " ticks"
   RETURN 2
  ELSE
   debug "WARNING: autosnap argument was ignored because it should be followed by an integer"
   RETURN 1
  END IF
 ELSEIF opt = "slave" THEN
  IF arg = "" THEN
   debug "-slave option ignored because channel not specified"
   RETURN 1
  END IF
  IF channel_open_client(master_channel, arg) THEN
   running_as_slave = YES
   debuginfo "Reading commands from master channel '" & arg & "'"
   RETURN 2
  ELSE
   debug "Failed to open channel '" & arg & "'"
   SYSTEM
   RETURN 1
  END IF
 END IF
 RETURN 0
END FUNCTION

SUB show_wrong_spawned_version_error
 fatalerror !"This version of Game differs from the version of Custom which spawned it and cannot be used for the ""Test Game"" option. Download and place matching versions in the same directory before trying again.\n" _
             "Game is version " + version + " r" & version_revision & !"\n" _
             "Custom is version " + custom_version
END SUB

SUB check_game_custom_versions_match
 DIM game_version as string = version + " r" & version_revision
 IF game_version <> custom_version THEN
  pop_warning !"Warning: This version of Game is not exactly identical to the version of Custom that spawned it. No differences in file format were detected, but this is a bad idea. There's no chance of corrupting your game, but something might go haywire.\n" _
               "Game is version " + game_version + !"\n" _
               "Custom is version " + custom_version
 END IF
END SUB

SUB handshake_with_master ()
 DIM line_in as string
 FOR i as integer = 1 TO 3
  IF channel_input_line(master_channel, line_in) = 0 THEN
   'Custom is meant to have already sent the initialisation messages by now
   debuginfo "handshake_with_master: no message on channel"
   fatalerror "Could not communicate with Custom"
  END IF
  debuginfo "Received message from Custom: " & line_in

  SELECT CASE i
   CASE 1  'Parse version string
    REDIM pieces() as string
    split line_in, pieces(), ","
    IF pieces(0) <> "V OHRRPGCE" THEN
     fatalerror "Could not communicate with Custom"
    END IF
    IF UBOUND(pieces) >= 3 THEN
     custom_version = pieces(3) & " r" & pieces(2)
    ELSE
     custom_version = "<unknown>"
    END IF
    IF pieces(1) <> STR(CURRENT_TESTING_IPC_VERSION) THEN  'wrong protocol version
     show_wrong_spawned_version_error     
    END IF

   CASE 2  'Get sourcerpg
    IF LEFT(line_in, 2) <> "G " THEN
     fatalerror "Unexpected command from Custom"
    END IF
    sourcerpg = MID(line_in, 3)

   CASE 3  'Get workingdir
    IF LEFT(line_in, 2) <> "W " THEN
     fatalerror "Unexpected command from Custom"
    END IF
    workingdir = MID(line_in, 3)
    IF isdir(workingdir) = 0 THEN
     fatalerror !"Communication error with Custom:\n" & workingdir & !"\ndoes not exist"
    END IF
  END SELECT
 NEXT

 'Set this hook to throw an error on any detected write in workingdir;
 'also needed to set a shared lock when reading a file
 set_OPEN_hook @inworkingdir, NO, NULL
END SUB

'Reads and handles messages from Custom, updating modified_lumps
SUB receive_file_updates ()
 'This sub is called from control and a couple other places, so prevent reentering
 STATIC entered as integer = NO
 IF entered THEN EXIT SUB
 entered = YES

 DIM line_in as string
 REDIM pieces() as string

 WHILE channel_input_line(master_channel, line_in)
  debuginfo "msg from Custom: " & RTRIM(line_in)

  IF LEFT(line_in, 2) = "M " THEN  'file modified/created/deleted
   line_in = MID(line_in, 3)
   DIM at as integer = v_find(modified_lumps, line_in)
   IF at = -1 THEN
    v_append modified_lumps, line_in
   END IF

  ELSEIF LEFT(line_in, 3) = "CM " THEN  'please close music file
   DIM songnum as integer = str2int(MID(line_in, 4))
   IF songnum = presentsong THEN music_stop
   'Send confirmation
   channel_write_line(master_channel, line_in)

  ELSEIF LEFT(line_in, 4) = "PAL " THEN  'palette changed (path of least resistance...)
   DIM palnum as integer = str2int(MID(line_in, 5))
   palette16_update_cache(game + ".pal", palnum)

  ELSEIF LEFT(line_in, 1) = "Q" THEN   'quit!
   music_stop
   'DIR might be holding a handle for the last directory on which it was run, which could prevent
   'Custom from deleting workingdir. So reset it.
   #IFDEF __FB_WIN32__
    DIM dummy as string = DIR("C:\")
   #ENDIF
   'Send confirmation
   channel_write_line(master_channel, "Q ")
   channel_close(master_channel)
   EXIT WHILE

  ELSEIF LEFT(line_in, 2) = "P " THEN   'ping

  ELSE
   debug "Did not understand message from Custom: " & line_in
  END IF
 WEND

 IF master_channel = NULL_CHANNEL THEN
  'Opps, it closed. Better quit immediately because workingdir is probably gone (crashy)
  IF yesno("Lost connection to Custom; the game has to be closed. Do you want to save the game first? (WARNING: resulting save might be corrupt)") THEN
   DIM slot as integer = picksave(0)
   IF slot >= 0 THEN savegame slot
  END IF
  exitprogram YES, 0
 END IF

 entered = NO
END SUB

SUB reload_MAP_lump()
 WITH lump_reloading

  'Only compare part of each MAP record... OK, this is getting really perfectionist
  REDIM compare_mask(dimbinsize(binMAP)) as integer
  FOR i as integer = 0 TO UBOUND(compare_mask)
   compare_mask(i) = (gmap_index_affects_tiles(i) = NO)
  NEXT

  'Compare with backup to find the changes
  REDIM changed_records(0) as integer
  IF compare_files_by_record(changed_records(), game + ".map", tmpdir + "mapbackup.map", getbinsize(binMAP) \ 2, @compare_mask(0)) = NO THEN
   debug "reload_MAP_lump: couldn't compare!"
   EXIT SUB
  END IF

  FOR mapno as integer = 0 TO UBOUND(changed_records)
   'delete saved state
   IF changed_records(mapno) THEN
    IF .gmap.mode <> loadmodeNever THEN
     safekill mapstatetemp(mapno, "map") + "_map.tmp"
    END IF
   END IF
  NEXT

  IF changed_records(gam.map.id) THEN
   '--never/always/if unchanged only
   .gmap.changed = YES
   IF .gmap.dirty THEN
    IF .gmap.mode = loadmodeAlways THEN reloadmap_gmap_no_tilesets
   ELSE
    IF .gmap.mode <> loadmodeNever THEN reloadmap_gmap_no_tilesets
   END IF
  END IF      

 END WITH
END SUB

'Check whether a lump is a .PT# or .TIL lump. Reload them. MXS (backdrops) are quite different
'because they don't go in the sprite cache: need different handling in and out of battles
FUNCTION try_reload_gfx_lump(extn as string) as integer
 IF extn = "til" THEN
  sprite_update_cache_tilesets
  RETURN YES
 ELSEIF LEFT(extn, 2) = "pt" THEN
  DIM ptno as integer = str2int(MID(extn, 3), -1)
  IF ptno >= 0 THEN
   sprite_update_cache_pt(ptno)
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

'Check whether a lump is a (supported) map lump, and if so return YES and reload it if needed.
'Also updates lump_reloading flags and deletes mapstate data as required.
'Currently supports: T, P, E, Z, N, L
'Elsewhere: MAP, DOX
'Not going to bother with: MN, D
FUNCTION try_reload_map_lump(basename as string, extn as string) as integer
 DIM typecode as string
 DIM mapnum as integer = -1

 'Check for .X## and map###.X
 DIM extnnum as integer = -1
 IF LEN(extn) = 3 THEN extnnum = str2int(MID(extn, 2), -1)
 DIM basenum as integer = str2int(basename, -1)
 '--Don't bother to actually check basename=archinym
 mapnum = IIF(basenum >= 100 AND extnnum = -1, basenum, extnnum)
 IF mapnum = -1 THEN RETURN NO
 typecode = LEFT(extn, 1)

 WITH lump_reloading

  IF mapnum <> gam.map.id THEN
   'Affects map(s) other then the current one. However, we should still delete saved map state.
   'Not really sure what to do if the mode loadmodeIfUnchanged or loadmodeMerge... deleting seems safest bet.

   dim statefile as string = mapstatetemp(mapnum, "map") + "_" + typecode
   IF typecode = "l" THEN statefile += ".reld.tmp" ELSE statefile += ".tmp"

   SELECT CASE typecode
    CASE "t"
     IF .maptiles.mode <> loadmodeNever THEN safekill statefile
    CASE "p"
     IF .passmap.mode <> loadmodeNever THEN safekill statefile
    CASE "e"
     IF .foemap.mode <> loadmodeNever THEN safekill statefile
    CASE "z"
     IF .foemap.mode <> loadmodeNever THEN safekill statefile
    CASE "l"
     IF .npcl.mode <> loadmodeNever THEN safekill statefile
    CASE "n"
     IF .npcd.mode <> loadmodeNever THEN safekill statefile
    CASE ELSE
     RETURN NO
   END SELECT

   'This is a lump for a specific map other than the current, stop.
   RETURN YES
  END IF

  'This is one of the current map's lumps

  DIM newhash as integer = hash_file(workingdir + basename + "." + extn)

  SELECT CASE typecode
   CASE "t"  '--all modes supported
    IF .maptiles.hash = newhash THEN RETURN YES
    .maptiles.changed = YES
    IF .maptiles.dirty THEN
     IF .maptiles.mode = loadmodeAlways THEN reloadmap_tilemap_and_tilesets NO
     IF .maptiles.mode = loadmodeMerge THEN reloadmap_tilemap_and_tilesets YES
    ELSE
     IF .maptiles.mode <> loadmodeNever THEN reloadmap_tilemap_and_tilesets NO
    END IF

   CASE "p"  '--all modes supported
    IF .passmap.hash = newhash THEN RETURN YES
    .passmap.changed = YES
    IF .passmap.dirty THEN
     IF .passmap.mode = loadmodeAlways THEN reloadmap_passmap NO
     IF .passmap.mode = loadmodeMerge THEN reloadmap_passmap YES
    ELSE
     IF .passmap.mode <> loadmodeNever THEN reloadmap_passmap NO
    END IF

   CASE "e"  '--never/always only
    IF .foemap.hash = newhash THEN RETURN YES
    .foemap.changed = YES
    IF .foemap.mode = loadmodeAlways THEN reloadmap_foemap

   CASE "z"  '--never/always/if unchanged only
    IF .zonemap.hash = newhash THEN RETURN YES
    .zonemap.changed = YES
    IF .zonemap.dirty THEN
     IF .zonemap.mode = loadmodeAlways THEN reloadmap_zonemap
    ELSE
     IF .zonemap.mode <> loadmodeNever THEN reloadmap_zonemap
    END IF

   CASE "l"  '--never/always/merge only
    IF .npcl.hash = newhash THEN RETURN YES
    .npcl.changed = YES
    IF .npcl.mode <> loadmodeNever THEN reloadmap_npcl (.npcl.mode = loadmodeMerge)

   CASE "n"  '--never/always/if unchanged only
    IF .npcd.hash = newhash THEN RETURN YES
    .npcd.changed = YES
    IF .npcd.dirty THEN
     IF .npcd.mode = loadmodeAlways THEN reloadmap_npcd
    ELSE
     IF .npcd.mode <> loadmodeNever THEN reloadmap_npcd
    END IF

   CASE ELSE  '???
    RETURN NO

  END SELECT
 END WITH
 RETURN YES

END FUNCTION

'Returns true (and reloads as needed) if this file is a music file (.## or song##.xxx)
FUNCTION try_reload_music_lump(basename as string, extn as string) as integer
 DIM songnum as integer = str2int(extn, -1)
 IF songnum = -1 THEN
  IF LEFT(basename, 4) = "song" THEN songnum = str2int(MID(basename, 5))
 END IF
 IF songnum = -1 THEN RETURN NO
 IF songnum = presentsong THEN
  stopsong
  playsongnum presentsong
 END IF
 RETURN YES
END FUNCTION

'This sub does lump reloading which is safe to do from anywhere
SUB try_reload_lumps_anywhere ()

 'pal handled with special message
 STATIC ignorable_extns_(...) as string*3 => {"mn", "tmn", "d", "dor", "pal", "sng", "efs"}
 STATIC ignorable_extns as string vector
 IF ignorable_extns = NULL THEN
  v_new ignorable_extns
  FOR i as integer = 0 TO UBOUND(ignorable_extns_)
   v_append(ignorable_extns, ignorable_extns_(i))
  NEXT
 END IF

 receive_file_updates

 DIM i as integer = 0
 WHILE i < v_len(modified_lumps)
  DIM handled as integer = NO
  DIM basename as string = trimextension(modified_lumps[i])
  DIM extn as string = justextension(modified_lumps[i])

  IF v_find(ignorable_extns, extn) > -1 THEN
   handled = YES

  ELSEIF extn = "gen" THEN                                                '.GEN
   REDIM newgen(360) as integer
   xbload game + ".gen", newgen(), "reload lumps: .gen unreadable"
   IF gam.current_master_palette = gen(genMasterPal) _
       AND newgen(genMasterPal) <> gen(genMasterPal) THEN
    gam.current_master_palette = newgen(genMasterPal)
    loadpalette master(), gam.current_master_palette
    setpal master()
    LoadUIColors uilook(), boxlook(), gam.current_master_palette
    'Change color of script strings
    init_default_text_colors
   END IF
   FOR j as integer = 0 TO UBOUND(gen)
    SELECT CASE j
     CASE 44 TO 54, 58, 60
     CASE ELSE
      gen(j) = newgen(j)
    END SELECT
   NEXT
   'FIXME: does anything else need to be reloaded when gen() changes?
   'Number of elements maybe?
   handled = YES

  ELSEIF modified_lumps[i] = "binsize.bin" THEN                           'BINSIZE.BIN
   'We correctly handle an update to binsize.bin, but there's no good reason for it
   'to happen while live previewing
   clear_binsize_cache
   debugc errBug, "Recieved binsize.bin modification, should not happen!"
   handled = YES

  ELSEIF modified_lumps[i] = "palettes.bin" THEN                          'PALETTES.BIN
   loadpalette master(), gam.current_master_palette
   setpal master()
   handled = YES

  ELSEIF modified_lumps[i] = "uicolors.bin" THEN                          'UICOLORS.BIN
   LoadUIColors uilook(), boxlook(), gam.current_master_palette
   'Change color of script strings
   'init_default_text_colors
   handled = YES

  ELSEIF modified_lumps[i] = "menus.bin" THEN                             'MENUS.BIN
   'This is far from complete
   'Cause cache in getmenuname to be dropped
   game_unique_id = STR(randint(INT_MAX))

  ELSEIF try_reload_gfx_lump(extn) THEN                                   '.PT#, .TIL
   handled = YES

  ELSEIF extn = "fnt" THEN                                                '.FNT
   xbload game + ".fnt", current_font(), "Font not loaded"
   setfont current_font()
   handled = YES

  ELSEIF try_reload_music_lump(basename, extn) THEN                       '.## and song##.xxx (music)
   handled = YES

  ELSEIF extn = "itm" THEN                                                '.ITM
   FOR slot as integer = 0 TO last_inv_slot()
    update_inventory_caption slot
   NEXT
   load_special_tag_caches  'includes item tags
   'Does anything else need to be done?
   handled = YES

  ELSEIF extn = "stt" THEN                                                '.STT
   loadglobalstrings
   getstatnames statnames()
   handled = YES

  ELSEIF modified_lumps[i] = "browse.txt" THEN                            'BROWSE.TXT
   handled = YES  'ignore

                                                                          ''' Script stufff

  ELSEIF extn = "hsp" THEN                                                '.HSP
   'Could add an option for automatic reloading, with suitable warnings...
   lump_reloading.hsp.changed = YES
   handled = YES

  ELSEIF modified_lumps[i] = "plotscr.lst" THEN                           'PLOTSCR.LST
   handled = YES  'ignore

  ELSEIF modified_lumps[i] = "lookup1.bin" THEN                           'LOOKUP1.BIN
   load_lookup1_bin lookup1_bin_cache()
   handled = YES

  END IF

  IF handled THEN
   v_delete_slice modified_lumps, i, i + 1
  ELSE
   i += 1
  END IF

 WEND
END SUB

SUB try_to_reload_files_onmap ()
 'calls receive_file_updates
 try_reload_lumps_anywhere

 DIM i as integer = 0
 WHILE i < v_len(modified_lumps)
  DIM handled as integer = NO
  DIM basename as string = trimextension(modified_lumps[i])
  DIM extn as string = justextension(modified_lumps[i])

  IF extn = "map" THEN                                                    '.MAP
   reload_MAP_lump
   handled = YES

  ELSEIF extn = "dox" THEN                                                '.DOX
   DeSerDoors(game + ".dox", gam.map.door(), gam.map.id)
   handled = YES

  ELSEIF try_reload_map_lump(basename, extn) THEN                         '.T, .P, .E, .Z, .N, .L
   handled = YES

  ELSEIF extn = "tap" THEN                                                '.TAP
   reloadtileanimations tilesets(), gmap()
   handled = YES

  ELSE
   debuginfo "did not reload " & modified_lumps[i]
   handled = YES
  END IF

  IF handled THEN
   v_delete_slice modified_lumps, i, i + 1
  ELSE
   i += 1
  END IF
 WEND
END SUB

'return a video page which is a view on vpage that is 320x200 (or smaller) and centred
FUNCTION compatpage() as integer
 DIM fakepage as integer
 DIM centreview as Frame ptr
 centreview = frame_new_view(vpages(vpage), (vpages(vpage)->w - 320) / 2, (vpages(vpage)->h - 200) / 2, 320, 200)
 fakepage = registerpage(centreview)
 frame_unload @centreview
 RETURN fakepage
END FUNCTION

FUNCTION lump_reload_mode_to_string (byval mode as integer) as string
 IF mode = loadmodeNever THEN RETURN "Never"
 IF mode = loadmodeAlways THEN RETURN "Always"
 IF mode = loadmodeIfUnchanged THEN RETURN "If no in-game changes"
 IF mode = loadmodeMerge THEN RETURN "Merge in-game changes"
END FUNCTION

SUB LPM_append_reload_mode_item (menu as MenuDef, what as string, info as LumpReloadState, byval extradata as integer = 0)
 append_menu_item menu, "Reload " + what + ": " + lump_reload_mode_to_string(info.mode)
 menu.last->extra(0) = extradata
END SUB

SUB LPM_append_force_reload_item (menu as MenuDef, tooltips() as string, what as string, info as LumpReloadState, byval extradata as integer = 0, byval ignore_dirtiness as integer = NO)
 append_menu_item menu, "Force reload of " + what
 menu.last->extra(0) = extradata
 REDIM PRESERVE tooltips(menu.numitems - 1)
 DIM tmp as string
 IF info.changed = 0 AND info.dirty = 0 THEN
  tmp = "No changes"
  menu.last->disabled = YES
 ELSE
  tmp = "Modified by"
  IF info.dirty AND ignore_dirtiness = NO THEN
   tmp += " scripts"
   IF info.changed THEN tmp += ", by"
  END IF
  IF info.changed THEN
   tmp += " Custom"
  END IF
 END IF
 tooltips(menu.numitems - 1) = tmp
END SUB

SUB LPM_update (menu1 as MenuDef, st1 as MenuState, tooltips() as string)
 WITH lump_reloading
  DeleteMenuItems menu1
  REDIM tooltips(0)

  append_menu_item menu1, "Exit"        : menu1.last->extra(0) = 1
  append_menu_item menu1, "Reload map"  : menu1.last->extra(0) = 2
  IF running_as_slave THEN
   LPM_append_reload_mode_item menu1, "gen. map data", .gmap, 10
   LPM_append_reload_mode_item menu1, "tilemap", .maptiles, 11
   LPM_append_reload_mode_item menu1, "wallmap", .passmap, 12
   LPM_append_reload_mode_item menu1, "foemap", .foemap, 13
   LPM_append_reload_mode_item menu1, "zonemap", .zonemap, 14
   LPM_append_reload_mode_item menu1, "npc locations", .npcl, 15
   LPM_append_reload_mode_item menu1, "npc defs.", .npcd, 16
  END IF
  LPM_append_force_reload_item menu1, tooltips(), "general map data", .gmap, 100
  LPM_append_force_reload_item menu1, tooltips(), "tiles", .maptiles, 101
  LPM_append_force_reload_item menu1, tooltips(), "wallmap", .passmap, 102
  LPM_append_force_reload_item menu1, tooltips(), "foemap", .foemap, 103
  LPM_append_force_reload_item menu1, tooltips(), "zones", .zonemap, 104
  LPM_append_force_reload_item menu1, tooltips(), "npc locations", .npcl, 105, YES  'NPCL is virtually always dirty
  LPM_append_force_reload_item menu1, tooltips(), "npc definitions", .npcd, 106

  LPM_append_force_reload_item menu1, tooltips(), "scripts", .hsp, 110
  tooltips(UBOUND(tooltips)) += " (Read Help file!)"

  init_menu_state st1, menu1
  REDIM PRESERVE tooltips(menu1.numitems - 1)
 END WITH
END SUB

SUB live_preview_menu ()
 DIM st1 as MenuState
 st1.active = YES
 
 DIM menu1 as MenuDef
 ClearMenuData menu1
 menu1.align = -1
 menu1.boxstyle = 3
 menu1.translucent = YES
 menu1.min_chars = 38

 REDIM tooltips() as string

 setkeys
 DO
  setwait 55
  setkeys
  control
  IF running_as_slave THEN try_to_reload_files_onmap

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "game_live_preview_menu"

  LPM_update menu1, st1, tooltips()

  usemenu st1
  SELECT CASE menu1.items[st1.pt]->extra(0)
   CASE 1  '--exit
    IF carray(ccUse) > 1 THEN EXIT DO
   CASE 2  '--reload map
    IF carray(ccUse) > 1 THEN
     'delete everything
     deletemapstate gam.map.id, -1, "map"
     prepare_map NO, YES
    END IF
   CASE 10  '--gmap reload mode
    st1.need_update OR= intgrabber(lump_reloading.gmap.mode, 0, 2)  '3 --merging not implemented
   CASE 11  '--tile reload mode
    st1.need_update OR= intgrabber(lump_reloading.maptiles.mode, -1, 2)
   CASE 12  '--wallmap reload mode
    st1.need_update OR= intgrabber(lump_reloading.passmap.mode, -1, 2)
   CASE 13  '--foemap reload mode
    st1.need_update OR= intgrabber(lump_reloading.foemap.mode, 0, 1)  'Not even possible to modify, so don't confuse people
   CASE 14  '--zones reload mode
    st1.need_update OR= intgrabber(lump_reloading.zonemap.mode, 0, 2)  'Can't merge zones
   CASE 15  '--npcl reload mode
    st1.need_update OR= intgrabber(lump_reloading.npcl.mode, -1, 1)
   CASE 16  '--npcd reload mode
    st1.need_update OR= intgrabber(lump_reloading.npcd.mode, 0, 2)
   CASE 100  '--force gmap reload
    IF carray(ccUse) > 1 THEN
     reloadmap_gmap_no_tilesets
    END IF
   CASE 101  '--force tile reload
    IF carray(ccUse) > 1 THEN
     reloadmap_tilemap_and_tilesets NO
    END IF
   CASE 102  '--force wallmap reload
    IF carray(ccUse) > 1 THEN
     reloadmap_passmap NO
    END IF
   CASE 103  '--force foemap reload
    IF carray(ccUse) > 1 THEN
     reloadmap_foemap
    END IF
   CASE 104  '--force zonemap reload
    IF carray(ccUse) > 1 THEN
     reloadmap_zonemap
    END IF
   CASE 105  '--force npcl reload
    IF carray(ccUse) > 1 THEN
     reloadmap_npcl NO
    END IF
   CASE 106  '--force npcd reload
    IF carray(ccUse) > 1 THEN
     reloadmap_npcd
    END IF
   CASE 110  '--force scripts reload
    IF carray(ccUse) > 1 THEN
     reload_scripts
    END IF
  END SELECT

  'Draw screen
  displayall
  draw_menu menu1, st1, dpage
  rectangle 0, 188, 320, 12, uilook(uiBackground), dpage
  edgeprint tooltips(st1.pt), 0, 190, uilook(uiText), dpage
  setvispage dpage
  dowait
 LOOP
END SUB
