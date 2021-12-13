'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
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
#include "scripting.bi"

#include "game.bi"
#include "scriptcommands.bi"
#include "yetmore2.bi"
#include "walkabouts.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "savegame.bi"
#include "bmodsubs.bi"
#include "steam.bi"
#include "achievements.bi"

Using Reload
Using Reload.Ext

'Local subs and functions
DECLARE SUB drawants_for_tile(tile as XYPair, byval direction as DirNum)


' This is called after every setkeys unless we're already inside global_setkeys_hook
' It should be fine to call any allmodex function in here, but beware we might
' not have loaded a game yet!
SUB global_setkeys_hook
 ' Process messages from Custom
 IF gam.ingame ANDALSO running_under_Custom THEN try_reload_lumps_anywhere
END SUB

SUB initgamedefaults
'Exists to initialise game state which needs to start at a value
'other than the default initialisation (zero/empty string/etc)
'(Generally you can and should use constructors (or default UDT member values)
'to initialise data to something other than zero)

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
(herox(0)) = gen(genStartX) * 20
(heroy(0)) = gen(genStartY) * 20
(herodir(0)) = dirDown
resetcaterpillar ()

'plotstring colours
FOR i as integer = 0 TO UBOUND(plotstr)
 plotstr(i).col = -1  'default to uilook(uiText)
NEXT i

END SUB

'revive_dead_heroes should -2 for default, or a bool
SUB innRestore (revive_dead_heroes as integer = -2)
 IF revive_dead_heroes = -2 THEN
  revive_dead_heroes = (prefbit(4) = NO)  '"!Inns Revive Dead Heroes"
 END IF
 FOR i as integer = 0 TO 3
  IF gam.hero(i).id >= 0 THEN '--hero exists
   IF gam.hero(i).stat.cur.hp <= 0 ANDALSO revive_dead_heroes = NO THEN
    '--hero is dead and inn-revive is disabled
   ELSE
    '--normal revive
    gam.hero(i).stat.cur.hp = gam.hero(i).stat.max.hp
    gam.hero(i).stat.cur.mp = gam.hero(i).stat.max.mp
    reset_levelmp gam.hero(i)
   END IF
  END IF
 NEXT i
 party_change_updates
END SUB

SUB center_camera_on_slice(byval sl as Slice ptr)
 BUG_IF(sl = NULL, "NULL slice")

 RefreshSliceScreenPos sl

 mapx = sl->ScreenX + sl->Width/2 - SliceTable.MapRoot->ScreenX - vpages(dpage)->w \ 2
 mapy = sl->ScreenY + sl->Height/2 - SliceTable.MapRoot->ScreenY - vpages(dpage)->h \ 2
END SUB

SUB center_camera_on_walkabout(byval walkabout_cont as Slice ptr)
 IF walkabout_cont = NULL THEN debug "NULL walkabout slice in center_camera_on_walkabout" : EXIT SUB

 'Note: the sprite component .Y value is equal to the foot-offset minus the hero/NPC Z.

 'Note: genCameraOnWalkaboutFocus 0 and 2 are identical when hero sprites are 20px high.
 'Option 0 was the old default (early days of walktall).
 'Option 2 became the default in Alectormancy (2012)
 'Option 0 made the default in Fufluns (2019), because it means `put camera(hero pixel x, hero pixel y)`
 'will not move the camera if it was following the leader.

 IF gen(genCameraOnWalkaboutFocus) = 0 THEN  'Center on the container slice (hitbox)/tile
  center_camera_on_slice walkabout_cont
 ELSE  'Center on the walkabout sprite
  DIM sprsl as Slice ptr
  sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
  center_camera_on_slice sprsl

  IF gen(genCameraOnWalkaboutFocus) = 2 THEN 'Center on sprite minus Z/offset
   ' For compatibility (maybe some old games use setheroz for falling from the sky?)
   ' ignore the Z component of the sprite slice, and also the foot offset.
   mapy -= sprsl->Y
  END IF
 END IF
END SUB

'Set the camera position.
SUB setmapxy
 IF gam.debug_camera_pan THEN
  'F7 debug key
  DIM speed as integer = IIF(keyval(scShift) > 0, 50, 15)
  IF carray(ccUp) > 0 THEN mapy -= speed
  IF carray(ccDown) > 0 THEN mapy += speed
  IF carray(ccLeft) > 0 THEN mapx -= speed
  IF carray(ccRight) > 0 THEN mapx += speed
  IF carray(ccCancel) > 0 THEN
   gam.showtext = "Normal camera restored"
   gam.showtext_ticks = 45
   gam.debug_camera_pan = NO
  END IF
 ELSE
  'Normal camera
  SELECT CASE gen(genCameraMode)
   CASE herocam
    DIM sl as Slice ptr = herow(gen(genCameraArg1)).sl
    IF sl ANDALSO sl->Parent <> SliceTable.Reserve THEN center_camera_on_walkabout sl
   CASE npccam
    IF gen(genCameraArg1) > UBOUND(npc) ORELSE npc(gen(genCameraArg1)).id <= 0 THEN
     gen(genCameraMode) = stopcam
    ELSE
     center_camera_on_walkabout npc(gen(genCameraArg1)).sl
    END IF
   CASE slicecam
    'We also check the slice didn't just get deleted after changing map
    DIM sl as Slice ptr
    sl = get_handle_slice(gen(genCameraArg1), serrIgnore)
    IF sl THEN
     center_camera_on_slice sl
    ELSE
     'stopping seems more appropriate than resetting to hero
     gen(genCameraMode) = stopcam
    END IF
   CASE pancam ' 1=dir, 2=ticks, 3=step
    IF gen(genCameraArg2) > 0 THEN
     aheadxy mapx, mapy, gen(genCameraArg1), gen(genCameraArg3)
     gen(genCameraArg2) -= 1
    END IF
    IF gen(genCameraArg2) <= 0 THEN gen(genCameraMode) = stopcam
   CASE focuscam ' 1=x, 2=y, 3=x step, 4=y step
    DIM camdiff as integer
    camdiff = gen(genCameraArg1) - mapx
    IF ABS(camdiff) <= gen(genCameraArg3) THEN
     gen(genCameraArg3) = 0
     mapx = gen(genCameraArg1)
    ELSE
     mapx += SGN(camdiff) * gen(genCameraArg3)
    END IF
    camdiff = gen(genCameraArg2) - mapy
    IF ABS(camdiff) <= gen(genCameraArg4) THEN
     gen(genCameraArg4) = 0
     mapy = gen(genCameraArg2)
    ELSE
     mapy += SGN(camdiff) * gen(genCameraArg4)
    END IF
    limitcamera mapx, mapy
    IF gen(genCameraArg3) = 0 AND gen(genCameraArg4) = 0 THEN gen(genCameraMode) = stopcam
  END SELECT
 END IF
 limitcamera mapx, mapy
END SUB

SUB showplotstrings
 FOR i as integer = 0 TO UBOUND(plotstr)
  WITH plotstr(i)
   IF .bits AND 1 THEN
    '-- only display visible strings
    DIM col as integer = .col
    IF col = -1 THEN col = uilook(uiText)
    textcolor col, .bgcol
    DIM fontnum as integer = IIF(.bits AND 2, fontPlain, fontEdged)
    'Don't autowrap (infinite width), but do support newlines
    wrapprint .s, .x, .y, , dpage, INT_MAX, NO, fontnum  'withtags=NO
   END IF
  END WITH
 NEXT i
END SUB

SUB makebackups
 'what is this for? Since some lumps can be modified at run time, we need to keep a
 'backup copy, and then only edit the copy. The original is never used directly.
 'enemy data
 writeablecopyfile game + ".dt1", tmpdir & "dt1.tmp"
 'formation data
 writeablecopyfile game + ".for", tmpdir & "for.tmp"
 'If you add lump-modding commands, you better well add them here >:(
 'Also, add CASEs to try_to_reload_lumps_* to handle updates to those lumps
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
 DIM transparent as bool = NO
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
 ChangeSpriteSlice SliceTable.Backdrop, sprTypeBackdrop, backdrop, , , , , transparent
END SUB

FUNCTION checkfordeath () as bool
 RETURN liveherocount = 0
END FUNCTION

'Note that this is called both from reset_game_final_cleanup(), in which case lots of stuff
'has already been deallocated, or from exit_gracefully(), in which case no cleanup has been done!
SUB exitprogram(byval need_fade_out as bool = NO, byval errorout as integer = 0)
debuginfo "Cleaning up and terminating " & errorout

gam.ingame = NO

'uncomment for slice debugging
'DestroyGameSlices YES

IF need_fade_out THEN fadeout uilook(uiFadeoutQuit)

releasestack

'--scripts
'Also prints script profiling info
resetinterpreter
destroystack(scrst)

'--reset audio
closemusic

debuginfo "Deleting tmpdir " & tmpdir
killdir tmpdir, YES  'recursively deletes playing.tmp if it exists

v_free modified_lumps

restoremode

Steam.uninitialize()

debuginfo "End."
terminate_program errorout

END SUB

SUB verify_quit
 DIM quitprompt as string = readglobalstring(55, "Quit Playing?", 20)
 DIM quityes as string = readglobalstring(57, "Yes", 10)
 DIM quitno as string = readglobalstring(58, "No", 10)
 DIM direction as DirNum = dirSouth
 DIM box as XYPair = XY(200, 42)
 DIM walkthreshold as integer
 DIM usethreshold as integer
 DIM ptr2 as integer = 0
 DIM tog as integer
 DIM wtog as integer
 DIM col as integer
 DIM holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 show_virtual_gamepad()

 DIM sprsl as Slice ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, herow(0).sl)
 IF sprsl = NULL THEN
  showbug "verify_quit: null slice"
 ELSE
  box.h = large(sprsl->Height, 20) + 22
  'Remove foot offset and hero Z; these will immediately be added back next tick
  sprsl->Y = 0
 END IF

 box.w = small(200, vpages(vpage)->w - 6)
 walkthreshold = box.w \ 4
 usethreshold = box.w \ 10

 DIM walkspeed as integer = large(2, herow(0).speed)

 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  loopvar wtog, 0, max_wtog()

  'Keyboard controls
  IF carray(ccCancel) > 1 THEN EXIT DO
  IF (carray(ccUse) > 1 AND ABS(ptr2) > usethreshold) OR ABS(ptr2) > walkthreshold THEN
   IF ptr2 < 0 THEN gam.quit = YES: fadeout uilook(uiFadeoutQuit)
   EXIT DO
  END IF
  IF carray(ccLeft) > 0 THEN ptr2 = ptr2 - walkspeed: direction = dirLeft
  IF carray(ccRight) > 0 THEN ptr2 = ptr2 + walkspeed: direction = dirRight

  DIM centerx as integer = vpages(vpage)->w \ 2
  DIM centery as integer = vpages(vpage)->h \ 2

  IF get_gen_bool("/mouse/mouse_menus") ANDALSO (carray(ccLeft) = 0 ANDALSO carray(ccRight) = 0) THEN
   'Only do the mouse controls when you are not using the arrow keys
   'The hero walks faster in mouseover mode, because we are counting on a click
   IF rect_collide_point(XYWH(centerx - box.w \ 2, centery - box.h \ 2, box.w \ 2 - usethreshold, box.h), readmouse.pos) THEN
    'Yes (to the left)
    ptr2 = large(ptr2 - 5, -walkthreshold)
    direction = dirLeft
    IF (readmouse.release AND mouseLeft) ANDALSO ptr2 <= 0 THEN
     gam.quit = YES
     fadeout uilook(uiFadeoutQuit)
     EXIT DO
    END IF
   ELSEIF rect_collide_point(XYWH(centerx + usethreshold, centery - box.h \ 2, box.w \ 2 - usethreshold, box.h), readmouse.pos) THEN
    'No (to the right)
    ptr2 = small(ptr2 + 5, walkthreshold)
    direction = dirRight
   END IF
   IF readmouse.release AND (mouseLeft OR mouseRight) THEN
    'A click anywhere other than the "Yes" area
    EXIT DO
   END IF
  END IF

  copypage holdscreen, vpage
  centerbox centerx, centery - 5, box.w, box.h, 15, vpage
  set_walkabout_frame herow(0).sl, direction, wtog_to_frame(wtog)
  DrawSliceAt herow(0).sl, centerx - 10 + ptr2, centery + box.h \ 2 - 21 - 10, 20, 20, vpage, YES
  edgeprint quitprompt, pCentered, centery - box.h \ 2 + 1, uilook(uiText), vpage
  col = uilook(uiMenuItem)
  IF ptr2 < -usethreshold THEN col = uilook(uiSelectedItem + tog)
  edgeprint quityes, rCenter - box.w \ 2 + 10, centery - 4, col, vpage
  col = uilook(uiMenuItem)
  IF ptr2 > usethreshold THEN col = uilook(uiSelectedItem + tog)
  edgeprint quitno, rCenter + box.w \ 2 - 10 + ancRight, centery - 4, col, vpage
  setvispage vpage
  dowait
 LOOP
 freepage holdscreen
 setkeys
END SUB

FUNCTION titlescreen () as bool
 DIM ret as bool = YES
 DIM backdrop as Frame ptr
 backdrop = frame_load(sprTypeBackdrop, gen(genTitle))
 DIM titletext as string = load_titletext()

 IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
 setkeys
 DO
  setwait speedcontrol
  setkeys
  ' Draw the screen at least once and fade in before skipping the title screen
  ' (This is not required to avoid any bug, it simply ensures this function acts consistently.)
  IF gam.need_fade_in = NO THEN
   IF carray(ccCancel) > 1 THEN
    ret = NO
    EXIT DO
   END IF
   IF anykeypressed(YES, YES) THEN EXIT DO  'Joystick and mouse included
  END IF

  clearpage vpage
  frame_draw backdrop, , pCentered, pCentered, NO, vpage
  edgeprint titletext, 8 + showLeft, pBottom, uilook(uiText), vpage
  setvispage vpage
  check_for_queued_fade_in
  dowait
 LOOP
 frame_unload @backdrop
 RETURN ret
END FUNCTION

FUNCTION mapstatetemp(mapnum as integer, prefix as string) as string
 RETURN tmpdir & prefix & mapnum
END FUNCTION

SUB savemapstate_gmap(mapnum as integer, prefix as string)
 DIM fh as integer
 OPENFILE(mapstatetemp(mapnum, prefix) & "_map.tmp", FOR_BINARY + ACCESS_WRITE, fh)
 PUT #fh, , gmap()
 CLOSE #fh
END SUB

SUB savemapstate_npcl(mapnum as integer, prefix as string)
 DIM filename as string = mapstatetemp(mapnum, prefix) & "_l.reld.tmp"
 save_npc_instances filename, npc()
END SUB

SUB savemapstate_npcd(mapnum as integer, prefix as string)
 SaveNPCD mapstatetemp(mapnum, prefix) & "_n.tmp", npool(0).npcs()
END SUB

SUB savemapstate_tilemap(mapnum as integer, prefix as string)
 savetilemaps maptiles(), mapstatetemp(mapnum, prefix) & "_t.tmp"
END SUB

SUB savemapstate_passmap(mapnum as integer, prefix as string)
 savetilemap pass, mapstatetemp(mapnum, prefix) & "_p.tmp"
END SUB

SUB savemapstate_zonemap(mapnum as integer, prefix as string)
 SaveZoneMap zmap, mapstatetemp(mapnum, prefix) & "_z.tmp"
END SUB

'Used only by the "save map state" command
SUB savemapstate_bitmask (mapnum as integer, savemask as integer = 255, prefix as string)
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

SUB loadmapstate_gmap (mapnum as integer, prefix as string, dontfallback as bool = NO)
 DIM fh as integer
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase & "_map.tmp") THEN
  IF dontfallback = NO THEN loadmap_gmap mapnum
  EXIT SUB
 END IF
 lump_reloading.gmap.dirty = NO  'Not correct, but too much trouble to do correctly
 lump_reloading.gmap.changed = NO

 OPENFILE(filebase & "_map.tmp", FOR_BINARY + ACCESS_READ, fh)
 GET #fh, , gmap()
 CLOSE #fh

 gmap_updates
END SUB

SUB loadmapstate_npcl (mapnum as integer, prefix as string, dontfallback as bool = NO)
 '--new-style
 DIM filename as string
 filename = mapstatetemp(mapnum, prefix) & "_l.reld.tmp"
 IF NOT isfile(filename) THEN
  IF dontfallback = NO THEN loadmap_npcl mapnum
  EXIT SUB
 END IF

 load_npc_instances filename, npc()

 '--Evaluate whether NPCs should appear or disappear based on tags
 visnpc
END SUB

SUB loadmapstate_npcd (mapnum as integer, prefix as string, dontfallback as bool = NO)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase & "_n.tmp") THEN
  IF dontfallback = NO THEN loadmap_npcd mapnum
  EXIT SUB
 END IF
 LoadNPCD filebase & "_n.tmp", npool(0).npcs()

 'Evaluate whether NPCs should appear or disappear based on tags
 visnpc
 'load NPC graphics
 reset_npc_graphics
END SUB

SUB loadmapstate_tilemap (mapnum as integer, prefix as string, dontfallback as bool = NO)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_t.tmp") THEN
  IF dontfallback = NO THEN loadmap_tilemap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "t"), propersize
  GetTilemapInfo filebase + "_t.tmp", statesize

  IF statesize.size = propersize.size THEN
   'Changing number of map layers is OK, however
   lump_reloading.maptiles.dirty = NO  'Not correct, but too much trouble to do correctly
   lump_reloading.maptiles.changed = NO

   loadtilemaps maptiles(), filebase + "_t.tmp"
   mapsizetiles = maptiles(0).size
   update_map_slices_for_new_tilemap

   '--as soon as we know the dimensions of the map, enforce hero position boundaries
   cropposition herox(0), heroy(0), 20

  ELSE
   DIM errmsg as string = " Tried to load saved tilemap state which is size " & statesize.size.wh & ", while the map is size " & propersize.size.wh
   IF insideinterpreter THEN
    scripterr current_command_name() + errmsg, 4
   ELSE
    debug "loadmapstate_tilemap(" + filebase + "_t.tmp): " + errmsg
   END IF
   IF dontfallback = NO THEN loadmap_tilemap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_passmap (mapnum as integer, prefix as string, dontfallback as bool = NO)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_p.tmp") THEN
  IF dontfallback = NO THEN loadmap_passmap mapnum
 ELSE
  DIM as TilemapInfo statesize, propersize
  GetTilemapInfo maplumpname(mapnum, "p"), propersize
  GetTilemapInfo filebase + "_p.tmp", statesize

  IF statesize.size = propersize.size THEN
   lump_reloading.passmap.dirty = NO  'Not correct, but too much trouble to do correctly
   lump_reloading.passmap.changed = NO
   loadtilemap pass, filebase + "_p.tmp"
  ELSE
   DIM errmsg as string = "tried to load saved passmap state which is size " & statesize.size.wh & ", while the map is size " & propersize.size.wh
   IF insideinterpreter THEN
    scripterr current_command_name() + errmsg, 4
   ELSE
    debug "loadmapstate_passmap(" + filebase + "_p.tmp): " + errmsg
   END IF
   IF dontfallback = NO THEN loadmap_passmap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_zonemap (mapnum as integer, prefix as string, dontfallback as bool = NO)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF NOT isfile(filebase + "_z.tmp") THEN
  IF dontfallback = NO THEN loadmap_zonemap mapnum
 ELSE
  'Unlike tile- and passmap loading, this doesn't leave the zonemap intact if the
  'saved state is the wrong size; instead the zonemap is blanked

  lump_reloading.zonemap.dirty = NO  'Not correct, but too much trouble to do correctly
  lump_reloading.zonemap.changed = NO
  LoadZoneMap zmap, filebase + "_z.tmp"
  IF zmap.size <> mapsizetiles THEN
   DIM errmsg as string = "tried to load saved zonemap state which is size " & zmap.size.wh & ", while the map is size " & mapsizetiles.wh
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

'This function is used only by the "load map state" command
SUB loadmapstate_bitmask (mapnum as integer, loadmask as integer, prefix as string, dontfallback as bool = NO)
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

SUB deletemapstate (mapnum as integer, killmask as integer, prefix as string)
 DIM filebase as string = mapstatetemp(mapnum, prefix)
 IF killmask AND 1 THEN safekill filebase + "_map.tmp"
 IF killmask AND 2 THEN safekill filebase + "_l.reld.tmp"
 IF killmask AND 4 THEN safekill filebase + "_n.tmp"
 IF killmask AND 8 THEN safekill filebase + "_t.tmp"
 IF killmask AND 16 THEN safekill filebase + "_p.tmp"
 IF killmask AND 32 THEN safekill filebase + "_z.tmp"
END SUB

'Note a differing number of layers is allowed!
FUNCTION tilemap_is_same_size (lumptype as string, what as string) as bool
 DIM as TilemapInfo newsize
 GetTilemapInfo maplumpname(gam.map.id, lumptype), newsize

 IF newsize.size <> mapsizetiles THEN
  notification "Could not reload " + what + " because the map size has changed. The map must be reloaded. You can do so by pressing F5 to access the Live Preview Debug Menu and selecting 'Reload map'."
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

#DEFINE debug_reloadmap(what)  debuginfo __FUNCTION__ " " #what ".dirty=" & lump_reloading.what.dirty & " " #what ".changed=" & lump_reloading.what.changed & " " #what ".mode=" & lump_reloading.what.mode

'Reload gmap() but don't update map layer slices or tilesets. See also reloadmap_tilemap_and_tilesets.
'Ignores changes to tilesets and to enabled layers because:
'1) it must be done after we have the correct number of tilemap layers
'   (when called from reload_MAP_lump)
'2) When called from Live Preview debug menu, maybe only want to update gmap,
'   not tilemaps/tilesets.
SUB reloadmap_gmap_no_tilesets()
 debug_reloadmap(gmap)
 lump_reloading.gmap.dirty = NO
 lump_reloading.gmap.changed = NO

 REDIM gmaptmp(dimbinsize(binMAP)) as integer
 loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) \ 2, gam.map.id

 FOR i as integer = 0 TO UBOUND(gmap)
  IF gmap_index_affects_tiles(i) = NO THEN gmap(i) = gmaptmp(i)
 NEXT

 gmap_updates  'does actually reload tilesets, using those in the old gmap()

 ' Behaviour specific to live-previewing

 'Delete saved gmap state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_map.tmp" 

 IF gmap(1) > 0 THEN
  wrappedsong gmap(1) - 1
 ELSEIF gmap(1) = 0 THEN
  stopsong
 END IF
END SUB

SUB reloadmap_npcl(merge as bool)
 lump_reloading.npcl.changed = NO
 
 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_l.reld.tmp"

 DIM filename as string = maplumpname(gam.map.id, "l")
 lump_reloading.npcl.hash = file_hash64(filename)
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

 'Evaluate whether NPCs should appear or disappear based on tags or validity of pool/ID
 visnpc
END SUB

SUB reloadmap_npcd()
 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_n.tmp"
 loadmap_npcd gam.map.id
END SUB

SUB reloadmap_tilemap_and_tilesets(merge as bool)
 debug_reloadmap(maptiles)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_t.tmp"

 IF tilemap_is_same_size("t", "tilemaps") THEN
  lump_reloading.maptiles.changed = NO

  DIM filename as string = maplumpname(gam.map.id, "t")
  lump_reloading.maptiles.hash = file_hash64(filename)
  IF merge THEN
   'Note: Its possible for this to fail if the number of layers differs
   MergeTileMaps maptiles(), filename, tmpdir + "mapbackup.t"
  ELSE
   lump_reloading.maptiles.dirty = NO
   LoadTileMaps maptiles(), filename
   writeablecopyfile filename, tmpdir + "mapbackup.t"
  END IF

  'Now reload tileset and layering info
  REDIM gmaptmp(dimbinsize(binMAP)) as integer
  loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) \ 2, gam.map.id

  FOR i as integer = 0 TO UBOUND(gmap)
   IF gmap_index_affects_tiles(i) THEN gmap(i) = gmaptmp(i)
  NEXT

  'Calls refresh_map_slice, updating number of layers, tilemaps,
  'layer visibility (gmap(19)) and position of walkabout layer (gmap(31))
  update_map_slices_for_new_tilemap

  loadmaptilesets tilesets(), gmap()
  refresh_map_slice_tilesets
 END IF
END SUB

SUB reloadmap_passmap(merge as bool)
 debug_reloadmap(passmap)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_p.tmp"

 IF tilemap_is_same_size("p", "wallmap") THEN
  lump_reloading.passmap.changed = NO

  DIM filename as string = maplumpname(gam.map.id, "p")
  lump_reloading.passmap.hash = file_hash64(filename)
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
  loadmap_foemap gam.map.id
 END IF
END SUB

SUB reloadmap_zonemap()
 debug_reloadmap(zonemap)

 'Delete saved state to prevent regressions
 safekill mapstatetemp(gam.map.id, "map") + "_z.tmp"

 loadmap_zonemap gam.map.id
END SUB

SUB deletetemps
'deletes game-state temporary files from tmpdir when exiting back to the titlescreen or loading a game

 REDIM filelist() as string
 findfiles tmpdir, ALLFILES, fileTypeFile, YES, filelist()
 DIM filename as string
 FOR i as integer = 0 TO UBOUND(filelist)
  filename = LCASE(filelist(i))
  IF ends_with(filename, ".tmp") ANDALSO (starts_with(filename, "map") ORELSE starts_with(filename, "state")) THEN
   killfile tmpdir + filelist(i)
  END IF
 NEXT
END SUB

'Print all NPCs to g_debug.txt
SUB debug_npcs ()
 FOR p as integer = 0 to 1
  debug IIF(p=1, "Global", "Local") & " NPC types:"
  FOR i as integer = 0 TO UBOUND(npool(p).npcs)
   debug " ID " & i & ": pic=" & npool(p).npcs(i).picture & " pal=" & npool(p).npcs(i).palette
  NEXT
 NEXT p
 debug "NPC instances:"
 FOR i as integer = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM npcinfo as string
    npcinfo = " " & i & ": ID=" & (ABS(.id) - 1) & IIF(.id < 0, " (hidden)", "") & " x=" & .x & " y=" & .y
    DIM where as XYPair
    IF framewalkabout(npc(i).pos + XY(0, gmap(11)), where, mapsizetiles * 20, gmap(5), 0) THEN
     npcinfo &= " screenx=" & where.x & " screeny=" & where.y
    END IF
    debug npcinfo
   END IF
  END WITH
 NEXT
END SUB

FUNCTION describe_npctype(npcid as NPCTypeID, pool as integer) as string
 DIM info as string, appearinfo as string
 IF npcid > UBOUND(npool(pool).npcs) THEN RETURN "NPC Type ID invalid (not loaded)!"
 WITH npool(pool).npcs(npcid)
  info &= fgcol_text("NPC Type: ", uilook(uiSelectedItem)) _
       & "Pic `" & .picture & "` Pal `" & .palette _
       & "` Speed `" & .speed _
       & "` MOVETYPE: `" & npc_movetypes(.movetype) _
       & "`, ACTIVATION: `" & npc_usetypes(.activation) _
       & "`, FACE: `" & npc_facetypes(.facetype) _
       & "`, PUSH: `" & npc_pushtypes(.pushtype) & "`"
  IF .tag1 THEN appearinfo &= " tag `" & ABS(.tag1) & "=" & onoroff(.tag1) & "`"
  IF .tag2 THEN appearinfo &= " tag `" & ABS(.tag2) & "=" & onoroff(.tag2) & "`"
  IF .usetag THEN appearinfo &= " Onetime-use flag `" & .usetag & "`"
  IF LEN(appearinfo) THEN info &= " APPEAR:" & appearinfo
 END WITH
 RETURN ticklite(info, findrgb(160,210,160))
END FUNCTION

FUNCTION describe_npcinst(npcnum as NPCIndex) as string
 DIM info as string
 WITH npc(npcnum)
  DIM npcid as NPCTypeID = ABS(.id) - 1
  'Calculate NPC copy number
  DIM copynum as integer
  FOR i as integer = 0 TO npcnum - 1
   IF npc(i).id - 1 = npcid ANDALSO npc(i).pool = .pool THEN copynum += 1
  NEXT

  info = IIF(.pool=1, "Global", "Local") & " ID `" & npcid & "`"
  IF .id < 0 THEN
   info &= " (DISABLED)"
  ELSE
   info &= " copy `" & copynum & "`"
  END IF
  info &= " npcref `" & (-1 - npcnum) & !"`\n" _
       & describe_npctype(npcid, .pool) & !"\n" _
       & fgcol_text("NPC Inst: ", uilook(uiSelectedItem)) _
       & "At `" & .pos & "` Z `" & .z _
       & "` tile `" & (.pos \ 20) & "` dir `" & CHR(("NESW")[.dir]) & "`"
  IF .sl THEN
   DIM sprite as Slice ptr
   sprite = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, .sl)
   IF sprite ANDALSO sprite->SliceType = slSprite THEN
    info &= " frame `" & sprite->SpriteData->frame & "`"
   END IF
  END IF
  info &= !"\nExtra 0:`" & .extra(0) & "` 1:`" & .extra(1) & "` 2:`" & .extra(2) & !"`\n" _
       & "AI: `" & yesorno(NOT .suspend_ai) _
       & "` Usable: `" & yesorno(NOT .suspend_use) _
       & "` Walls: `" & yesorno(NOT .ignore_walls) _
       & "` Obstruction: `" & yesorno(NOT .not_obstruction) _
       & !"`\nXYgo: `" & .xygo _
       & "` Pathing: "
  WITH .pathover
   IF .override <> NPCOverrideMove.NONE THEN
    IF .override = NPCOverrideMove.NPC THEN
     info &= "to NPC `" & .dest_npc & "` stop when reached: `" & yesorno(.stop_when_npc_reached)
    ELSEIF .override = NPCOverrideMove.POS THEN
     info &= "to `" & .dest_pos
    END IF
    info &= !"`\nCooldown: `" & .cooldown _
         & "` Stillticks: `" & npc(npcnum).stillticks _
         & "`, Stops after: `" & .stop_after_stillticks & "`"
   ELSE
    info &= "`N/A`"
   END IF
  END WITH
 END WITH
 RETURN ticklite(info, findrgb(160,210,160))
END FUNCTION

'Draw tooltip with info about the NPCs under the mouse cursor
'Draws to dpage
PRIVATE SUB npc_debug_display_tooltip ()
 IF readmouse.active = NO THEN EXIT SUB
 DIM pos as XYPair = readmouse.pos + XY(mapx, mapy)
 wrapxy pos, 20

 'Find each NPC at pos
 DIM info as string
 FOR copy as integer = 0 TO 999
  DIM npcidx as NPCIndex = npc_at_pixel(pos, copy, YES)
  IF npcidx = -1 THEN EXIT FOR
  IF LEN(info) THEN info &= !"\n"
  info &= describe_npcinst(npcidx)
 NEXT

 IF LEN(info) THEN
  'info = bgcol_text(info, uilook(uiShadow))
  CONST maxwidth = 280
  DIM boxsize as XYPair = textsize(info, maxwidth, fontEdged)
  DIM as RelPos x = readmouse.x + showLeft, y = readmouse.y + 7 + showTop
  ' If near the bottom of the screen, show the tooltip above the mouse
  IF readmouse.y + 7 + boxsize.h > vpages(dpage)->h THEN
   y = readmouse.y + showTop + ancBottom
  END IF
  trans_rectangle vpages(dpage), Type(x, y, boxsize.w, boxsize.h), curmasterpal(uilook(uiShadow)), .60
  wrapprint info, x, y, uilook(uiText), dpage, maxwidth, , fontEdged
 END IF
END SUB

SUB npc_debug_display (draw_walls as bool)
 DIM temp as string
 FOR i as integer = 0 TO UBOUND(npc)
  WITH npc(i)
   IF .id <> 0 THEN
    DIM where as XYPair
    ' Use a margin of 20 pixels, for one extra tile
    IF framewalkabout(npc(i).pos + XY(0, gmap(11)), where, mapsizetiles * 20, gmap(5), 20) THEN
     IF draw_walls THEN
      ' Draw the neighbouring obstructions for each NPC.
      ' Draw tile edges which the NPC can't pass
      ' Don't bother to draw obstruction for disabled NPCs
      IF .id > 0 THEN
       FOR yoff as integer = -1 TO 1
        FOR xoff as integer = -1 TO 1
         DIM tile as XYPair = npc(i).pos / 20 + XY(xoff, yoff)
         IF npc_collision_check_at(npc(i), tile, dirNorth) THEN drawants_for_tile tile, dirNorth
         IF npc_collision_check_at(npc(i), tile, dirEast)  THEN drawants_for_tile tile, dirEast
         IF npc_collision_check_at(npc(i), tile, dirSouth) THEN drawants_for_tile tile, dirSouth
         IF npc_collision_check_at(npc(i), tile, dirWest)  THEN drawants_for_tile tile, dirWest
        NEXT xoff
       NEXT yoff
      END IF
     END IF

     ' Draw the NPC ID and negative NPC reference number (e.g 7 instead of -7)
     DIM col as integer = IIF(.id < 0, uilook(uiSelectedDisabled), uilook(uiText))
     'the numbers can overlap quite badly, try to squeeze them in
     temp = STR(ABS(.id) - 1) & IIF(.pool = 1, "g", "")
     edgeprint MID(temp, 1, 1), where.x, where.y + 3, col, dpage
     edgeprint MID(temp, 2, 1), where.x + 7, where.y + 3, col, dpage
     edgeprint MID(temp, 3, 1), where.x + 14, where.y + 3, col, dpage
     edgeprint MID(temp, 4, 1), where.x + 21, where.y + 3, col, dpage
     col = uilook(uiDescription)
     temp = STR(i + 1)
     edgeprint MID(temp, 1, 1), where.x, where.y + 11, col, dpage
     edgeprint MID(temp, 2, 1), where.x + 7, where.y + 11, col, dpage
     edgeprint MID(temp, 3, 1), where.x + 14, where.y + 11, col, dpage
     'printstr STR(npc(i).stillticks), where.x, where.y + 20, dpage
    END IF
   END IF
  END WITH
 NEXT

 npc_debug_display_tooltip
END SUB

SUB drawants_for_tile(tile as XYPair, byval direction as DirNum)
 DIM where as XYPair
 IF framewalkabout(tile * 20, where, mapsizetiles * 20, gmap(5), 0) THEN
  SELECT CASE direction
   CASE dirNorth: drawants vpages(dpage), where.x       , where.y       , 20, 1
   CASE dirEast:  drawants vpages(dpage), where.x + 20-1, where.y       , 1, 20
   CASE dirSouth: drawants vpages(dpage), where.x       , where.y + 20-1, 20, 1
   CASE dirWest:  drawants vpages(dpage), where.x       , where.y       , 1, 20
   CASE ELSE
    debuginfo "drawants_for_tile: " & tile.x & " " & tile.y & " invalid direction " & direction
  END SELECT
 END IF
END SUB

SUB limitcamera (byref x as integer, byref y as integer)
 ' The slice the map is drawn "onto"
 DIM mapview as Slice ptr
 mapview = SliceTable.Root
 IF gmap(5) = mapEdgeCrop THEN
  ' When cropping the camera to the map, stop camera movements that attempt to go over the edge
  DIM oldmapx as integer = x
  DIM oldmapy as integer = y
  ' IF the map is smaller than the screen, centre it.
  DIM as integer padleft, padtop
  padleft = -large(0, mapview->Width - mapsizetiles.x * 20) \ 2
  padtop = -large(0, mapview->Height - mapsizetiles.y * 20) \ 2
  ' Need to call large, small in this order rather than using bound
  ' to handle maps which are larger than the screen correctly.
  x = padleft + large(small(x, mapsizetiles.x * 20 - mapview->Width), 0)
  y = padtop + large(small(y, mapsizetiles.y * 20 - mapview->Height), 0)
  IF oldmapx <> x THEN
   IF gen(genCameraMode) = pancam THEN gen(genCameraMode) = stopcam
  END IF
  IF oldmapy <> y THEN
   IF gen(genCameraMode) = pancam THEN gen(genCameraMode) = stopcam
  END IF
 END IF
 IF gmap(5) = mapEdgeWrap THEN
  ' Wrapping map. Wrap the camera according to the center, not the top-left
  x += mapview->Width \ 2
  y += mapview->Height \ 2
  wrapxy x, y, 20
  x -= mapview->Width \ 2
  y -= mapview->Height \ 2
 END IF
END SUB

FUNCTION game_setoption(opt as string, arg as string) as integer
 IF opt = "errlvl" THEN
  IF parse_int(arg, @err_suppress_lvl) THEN
   RETURN 2
  ELSE
   err_suppress_lvl = serrSuspicious  'Hide warnings and 'suspicious' warnings
   RETURN 1
  END IF
 ELSEIF opt = "autotest" THEN
  debug "Autotesting mode enabled!"
  autotestmode = YES
  use_speed_control = NO
  RETURN 1 'arg not used
 ELSEIF opt = "scriptlog" THEN
  gam.script_log.enabled = YES
  RETURN 1
 ELSEIF opt = "debugkeys" THEN
  'Undocumented
  debuginfo "--debugkeys used"
  always_enable_debug_keys = YES
  RETURN 1 'arg not used
 ELSEIF opt = "autosnap" then
  IF parse_int(arg, @autosnap) THEN
   debug "Autosnap mode enabled every " & autosnap & " ticks"
   RETURN 2
  ELSE
   debug "WARNING: autosnap argument was ignored because it should be followed by an integer"
   RETURN 1
  END IF
 ELSEIF opt = "from_Custom" THEN
  IF arg = "" THEN
   debug "-from_Custom option ignored because channel not specified"
   RETURN 1
  END IF
  IF channel_open_client(channel_to_Custom, arg) THEN
   running_under_Custom = YES
   debuginfo "Reading commands from channel_to_Custom '" & arg & "'"
   hook_after_attach_to_Custom YES
   RETURN 2
  ELSE
   debug "Failed to open channel '" & arg & "'"
   hook_after_attach_to_Custom NO
   terminate_program 10
   RETURN 1
  END IF
 ELSEIF opt = "reset_platform_achievements" THEN
  debug "Enqueuing platform achievement reset"
  Achievements.definitions_reset
  RETURN 1
 END IF
 RETURN 0
END FUNCTION

'Decide on the scale factor given a target maximum fraction of the screen size
FUNCTION automatic_scale_factor (screen_fraction as double) as integer
 DIM scale as integer = 2
 DIM as integer screenwidth, screenheight
 get_screen_size screenwidth, screenheight

 debuginfo "automatic_scale_factor(" & screen_fraction & "), screen size: " & screenwidth & "*" & screenheight

 IF screenwidth > 0 AND screenheight > 0 THEN
  scale = small((screen_fraction * screenwidth) / gen(genResolutionX), (screen_fraction * screenheight) / gen(genResolutionY))
  IF scale < 1 THEN scale = 1

  'Reduce the scale until it fits on the monitor
  'Kludge: Allow a little extra margin in the height to allow space for titlebar and taskbar.
  '(Note: under Windows os_get_screen_size already excludes taskbar but not titlebar)
  WHILE scale * gen(genResolutionX) > screenwidth OR scale * gen(genResolutionY) > screenheight - 20
   IF scale = 1 THEN EXIT WHILE
   scale -= 1
  WEND
 END IF

 RETURN scale
END FUNCTION

'Set the game resolution, size of the window, and bitdepth.
'This can be called when live-previewing when resolution settings change
SUB apply_game_window_settings (reloading as bool = NO)

 IF gen(gen32bitMode) THEN switch_to_32bit_vpages ELSE switch_to_8bit_vpages

 'This can happen while live-previewing, or maybe messing around with writegeneral
 IF gen(genResolutionX) < 10 OR gen(genResolutionY) < 10 THEN EXIT SUB

 lock_resolution

 IF XY(gen(genResolutionX), gen(genResolutionY)) <> get_resolution() THEN
  'get_resolution() will be 320x200 if the backend doesn't support anything else
  IF gfx_supports_variable_resolution() = NO THEN
   'Note: This code is duplicated in find_required_dlls
   DIM varresbackends(...) as string = {"sdl2", "sdl", "fb"}
   FOR idx as integer = 0 TO UBOUND(varresbackends)
    DIM name as string = varresbackends(idx)
    IF have_gfx_backend(name) THEN
     debuginfo "Attempting to switch to gfx_" & name & " for flexible resolution"
     switch_gfx name
     EXIT FOR
    END IF
   NEXT
  END IF
  IF gfx_supports_variable_resolution() = NO THEN
   notification "This game requires use of the gfx_sdl/gfx_sdl2/fb backend; other graphics backends do not support customisable resolution. Continuing anyway, but the game will probably be unplayable!"
  ELSE
   'Changes video page size, but not window size immediately
   set_resolution(gen(genResolutionX), gen(genResolutionY))
   IF SliceTable.Root THEN  'This SUB gets called before SetupGameSlices
    'When resolution changes, change Slice Root too
    SliceTable.Root->width = gen(genResolutionX)
    SliceTable.Root->height = gen(genResolutionY)
   END IF
   'Always recenter (need to call setvispage immediately, or the hint may be lost by calling e.g. set_safe_zone_margin)
   gfx_recenter_window_hint()
   setvispage vpage, NO
   'Calling this is only needed when live-previewing
   UpdateScreenSlice()
  END IF
 END IF

 IF overrode_default_zoom = NO THEN
  'Didn't specify scaling on cmdline, so figure out what scale to use.
  'TODO: screen_size_percent == 100 should be treated specially by maximising the window
  'and adding black bars, once any backends support that.
  DIM scale as integer
  IF running_under_Custom THEN
   scale = automatic_scale_factor(0.1 * gen(genLivePreviewWindowSize))
  ELSE
   scale = automatic_scale_factor(0.1 * gen(genWindowSize))
  END IF
  'This should cause backend to automatically recenter window if necessary.
  set_scale_factor scale
 END IF

 IF gam.shared_fullscreen_setting = NO THEN
  gam.fullscreen_config_file = game_config_file
 END IF

 IF supports_fullscreen_well() AND overrode_default_fullscreen = NO AND _
    user_toggled_fullscreen = NO AND running_under_Custom = NO AND _
    gam.shared_fullscreen_setting = NO THEN
  DIM setting as string = read_ini_prefixed_str(gam.fullscreen_config_file, "gfx.fullscreen")
  IF setting = "" THEN setting = read_config_str("gfx.fullscreen")  'Also reads game_config_file, I think that's OK
  DIM fullscreen as bool = str2bool(setting, gen(genFullscreen))
  ' genFullscreen is used only if the player has never customised the setting, and there's no global override
  debuginfo "config gfx.fullscreen = " & setting & ", genFullscreen = " & gen(genFullscreen)
  gfx_setwindowed(fullscreen = NO)
 ELSE
  debuginfo "Preserving fullscreen/windowed state"
 END IF

 IF gam.started_by_run_game THEN
  debuginfo "A previous game set shared_fullscreen_setting"
 ELSE
  debuginfo "genRungameFullscreenIndependent: " & gen(genRungameFullscreenIndependent)
  gam.shared_fullscreen_setting = (gen(genRungameFullscreenIndependent) = 0)
 END IF
END SUB

SUB set_speedcontrol (byval millisec_per_frame as integer)
 ' See also set_animation_framerate
 speedcontrol = bound(millisec_per_frame, 16, 200)
 IF gfx_vsync_supported() = NO THEN
  ' 16ms and 33ms are special-cased to be exactly 60/30fps rather than 62.5/30.3
  ' Disabled under gfx_directx, where have to try to run slightly faster than 60/30
  ' so that vsync can add a wait.
  IF speedcontrol = 16 THEN  '60 FPS
   speedcontrol = 16.666
  ELSEIF speedcontrol = 33 THEN  '30 FPS
   speedcontrol = 33.333
  END IF
 END IF
END SUB

SUB wrong_spawned_version_fatal_error
 fatalerror !"This version of Game differs from the version of Custom which spawned it and cannot be used for the ""Test Game"" option. Download and place matching versions in the same directory before trying again.\n" _
             "Game player is version " + short_version & !"\n" _
             "Editor is version " + custom_version
END SUB

SUB check_Game_Custom_versions_match ()
 IF short_version <> custom_version THEN
  pop_warning !"Warning: This version of Game is not exactly identical to the version of Custom that spawned it. There's no chance of corrupting your game, but something might go haywire.\n" _
               "Game is version " + short_version + !"\n" _
               "Custom is version " + custom_version
 END IF
END SUB

SUB handshake_with_Custom ()
 DIM line_in as string
 FOR i as integer = 1 TO 3
  IF channel_input_line(channel_to_Custom, line_in) = 0 THEN
   'Custom is meant to have already sent the initialisation messages by now
   debuginfo "handshake_with_Custom: no message on channel"
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
     custom_version = pieces(3)
    ELSE
     custom_version = "<unknown>"
    END IF
    IF pieces(1) <> STR(CURRENT_TESTING_IPC_VERSION) THEN  'wrong protocol version
     wrong_spawned_version_fatal_error
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
 'also needed to set a shared lock when reading a file.
 'Also, we disallow LAZYCLOSE to ensure we don't interfere with Custom
 'trying to delete/replace files on Windows (although renamefile
 'should work anyway, so we could allow lazyclose).
 set_OPEN_hook @inworkingdir, NO, NO
 can_write_to_workingdir = NO
END SUB

'Reads and handles messages from Custom, updating modified_lumps
SUB receive_file_updates ()
 'This sub may be called from all sorts of places, so prevent reentering
 STATIC entered as bool = NO
 IF entered THEN EXIT SUB
 entered = YES

 DIM line_in as string
 REDIM pieces() as string

 WHILE channel_input_line(channel_to_Custom, line_in)
  debuginfo "msg: " & line_in
  split RTRIM(line_in), pieces(), " "

  IF pieces(0) = "M" THEN  'file modified/created/deleted
   line_in = MID(line_in, 3)
   DIM at as integer = v_find(modified_lumps, line_in)
   IF at = -1 THEN
    v_append modified_lumps, line_in
   END IF

  ELSEIF pieces(0) = "CM" THEN  'please close music file
   DIM songnum as integer = str2int(pieces(1))
   IF songnum = presentsong THEN music_stop
   'Send confirmation
   channel_write_line(channel_to_Custom, line_in)

  ELSEIF pieces(0) = "PAL" THEN  'palette changed (path of least resistance...)
   DIM palnum as integer = str2int(pieces(1))
   palette16_update_cache(palnum)

  ELSEIF pieces(0) = "SCREEN" THEN  'Write a screenshot to file every tick
   IF pieces(1) = "STOP" THEN
    stop_recording_video
   ELSE
    start_forwarding_screen MID(line_in, 8)
   END IF

  ELSEIF pieces(0) = "Q" THEN   'quit!
   music_stop
   'DIR might be holding a handle for the last directory on which it was run, which could prevent
   'Custom from deleting workingdir. So reset it.
   '(findfiles() now does this automatically, but in case something else calls DIR...)
   #IFDEF __FB_WIN32__
    DIM dummy as string = DIR("C:\")
   #ENDIF
   'Send confirmation
   channel_write_line(channel_to_Custom, "Q ")
   channel_close(channel_to_Custom)
   EXIT WHILE

  ELSEIF pieces(0) = "P" THEN   'ping

  ELSE
   debug "Did not understand message from Custom: " & line_in
  END IF
 WEND

 IF channel_to_Custom = NULL THEN
  'Opps, it closed. Better quit immediately because workingdir is probably gone (crashy)
  /'  This isn't very useful.
  IF yesno("Lost connection to Custom; the game has to be closed. Do you want to save the game first? (WARNING: resulting save might be corrupt)", NO) THEN
   DIM slot as integer = picksave()
   IF slot >= 0 THEN savegame slot
  END IF
  '/
  exitprogram NO, 0
 END IF

 entered = NO
END SUB

'Live-previewing: Try to update stuff after .gen is written to by Custom
'This is very probably far less complete than it could be
SUB reload_gen()
 REDIM newgen(499) as integer
 xbload game + ".gen", newgen(), "reload lumps: .gen unreadable"
 'If "load palette" has been used (sets gam.current_master_palette), don't update palette.
 '(Not that it really matters)
 IF gam.current_master_palette = gen(genMasterPal) _
     AND newgen(genMasterPal) <> gen(genMasterPal) THEN
  gam.current_master_palette = newgen(genMasterPal)
  load_master_and_uicol gam.current_master_palette
  setpal master()
 END IF

 DIM should_reset_window as bool = NO

 FOR j as integer = 0 TO UBOUND(gen)
  IF gen(j) <> newgen(j) THEN
   'Before updating gen()
   SELECT CASE j
    CASE 44 TO 54, genTextboxBackdrop, genJoy  '44-54, 58, 60
     CONTINUE FOR 'Ignore.
     ' Don't need to ignore genMusicVolume, genSFXVolume, since they're only read once
   END SELECT
   gen(j) = newgen(j)
   'After updating gen()
   SELECT CASE j
    CASE genResolutionX, genResolutionY, genWindowSize, genLivePreviewWindowSize, genRungameFullscreenIndependent, gen32bitMode
     should_reset_window = YES
    CASE genMillisecPerFrame
     set_speedcontrol gen(genMillisecPerFrame)
     set_animation_framerate gen(genMillisecPerFrame)
    CASE genInventSlotx1Display
     'Reset inventory slot names
     FOR slot as integer = 0 TO last_inv_slot()
      update_inventory_caption slot
     NEXT
   END SELECT
  END IF
 NEXT
 'TODO: does anything else need to be reloaded when gen() changes?
 'Number of elements maybe?

 IF should_reset_window THEN apply_game_window_settings YES

 REDIM PRESERVE remembered_menu_pts(gen(genMaxMenu))
END SUB

'Live-previewing.
SUB reload_general_reld()
 ' Do not call close_general_reld(), that writes it, which is an error.
 FreeDocument gen_reld_doc
 gen_reld_doc = 0

 LoadUIColors uilook(), boxlook(), gam.current_master_palette, master()

 load_non_elemental_elements gam.non_elemental_elements()

 'Not bothering to reload: button codenames, purchases, default safe zone margin, arrowsets/gamepad settings
END SUB

'Live-previewing: Reload parts of a HeroState when its HeroDef may have changed
'Currently almost nothing is reloaded!
PRIVATE SUB update_hero_state(herost as HeroState, hero as HeroDef)
 IF herost.hand_pos_overridden = NO THEN
  FOR i as integer = 0 TO 1
   herost.hand_pos(i) = hero.hand_pos(i)
  NEXT
 END IF

 herost.rename_on_status = xreadbit(hero.bits(), 25)
END SUB

SUB reload_heroes_reld()
 DIM doc as DocPtr
 doc = LoadDocument(workingdir & SLASH & "heroes.reld")
 IF doc = 0 THEN EXIT SUB  'Shouldn't happen!

 'Update each hero in the party (this time, we can skip over empty party slots)
 FOR slot as integer = 0 TO UBOUND(gam.hero)
  DIM id as integer = gam.hero(slot).id
  IF id < 0 THEN CONTINUE FOR
  DIM heronode as NodePtr = NodeByPath(doc, "/hero[" & id & "]")
  IF heronode THEN
   DIM hero as HeroDef
   load_hero_from_reload hero, heronode, id
   update_hero_state gam.hero(slot), hero
  ELSE
   debug "reload_heroes_reld: missing hero ID " & id
  END IF
 NEXT

 FreeDocument doc

 load_special_tag_caches  'includes hero tags
END SUB

'Ignores changes to tilesets. That is handled by try_reload_map_lump and happens only when .T changes.
SUB reload_MAP_lump()
 WITH lump_reloading

  'Here we only mark stuff to be reloaded (or state to be deleted), actual
  'reloading is in reloadmap_gmap_no_tilesets.

  'Only compare part of each MAP record... OK, this is getting really perfectionist
  'Thank goodness this will be simpler when the map file format is replaced...
  'We ignore changes to tilesets and to enabled layers as this gets called before all
  'map lumps have been reloaded. Tilemaps must be loaded first, so we have right number of Map slices.

  REDIM compare_mask(dimbinsize(binMAP)) as bool
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
   IF changed_records(mapno) <> 0 THEN
    IF .gmap.mode <> loadmodeNever THEN  'Merge/always/if unchanged only
     safekill mapstatetemp(mapno, "map") + "_map.tmp"
    END IF
   END IF
  NEXT

  IF changed_records(gam.map.id) <> 0 THEN
   '--never/always/if unchanged only
   .gmap.changed = YES
   IF .gmap.dirty THEN
    IF .gmap.mode = loadmodeAlways THEN reloadmap_gmap_no_tilesets
   ELSE
    IF .gmap.mode <> loadmodeNever THEN reloadmap_gmap_no_tilesets
   END IF
  END IF

  'Check whether layer settings (eg tilesets) have changed, so
  'we will need to reload .T## even if it hasn't changed
  REDIM gmaptmp(dimbinsize(binMAP)) as integer
  loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) \ 2, gam.map.id
  FOR i as integer = 0 TO UBOUND(gmap)
   IF gmap(i) <> gmaptmp(i) ANDALSO gmap_index_affects_tiles(i) THEN
    debuginfo "reload_MAP_lump: layers changed"
    lump_reloading.maptiles.changed = YES
    EXIT FOR
   END IF
  NEXT

 END WITH
END SUB

'Check whether a lump is an .RGFX, .PT#, .MXS or .TIL lump. Reload them.
FUNCTION try_reload_gfx_lump(lumpname as string, extn as string) as bool
 IF extn = "til" THEN
  sprite_update_cache sprTypeTileset
  RETURN YES
 ELSEIF extn = "mxs" THEN
  sprite_update_cache sprTypeBackdrop
  RETURN YES
 ELSEIF lumpname = "enemies.rgfx" THEN
  'Multiple sprtypes in the same file
  sprite_update_cache sprTypeSmallEnemy
  sprite_update_cache sprTypeMediumEnemy
  sprite_update_cache sprTypeLargeEnemy
  sprite_update_cache sprTypeEnemy
 ELSEIF extn = "rgfx" THEN
  DIM sprtype as integer = a_find(rgfx_lumpnames(), lumpname)
  IF sprtype = -1 THEN RETURN NO
  sprite_update_cache sprtype
  RETURN YES
 ELSEIF LEFT(extn, 2) = "pt" THEN
  DIM ptno as integer
  IF parse_int(MID(extn, 3), @ptno) THEN
   sprite_update_cache ptno
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

'Check whether a lump is a (supported) map lump, and if so return YES and reload it if needed.
'Also updates lump_reloading flags and deletes mapstate data as required.
'Currently supports: T, P, E, Z, N, L
'Elsewhere: MAP (except tilesets), DOX, globalnpcs#.n
'No need to reload: D
'Not going to bother with: MN
'Tilesets are reloaded only when .T changes. Which isn't perfect right now, but will make sense when tilemap format is replaced.
FUNCTION try_reload_map_lump(basename as string, extn as string) as bool
 DIM typecode as string
 DIM mapnum as integer = -1

 'Check for <archinym>.X## and ###.X
 DIM extnnum as integer = -1
 IF LEN(extn) = 3 THEN extnnum = str2int(MID(extn, 2), -1)
 DIM basenum as integer = str2int(basename, -1)
 IF extnnum <> -1 ANDALSO basename = trimpath(game) THEN
  mapnum = extnnum
 ELSEIF basenum >= 100 ANDALSO LEN(extn) = 1 THEN
  mapnum = basenum
 ELSE
  RETURN NO  'Isn't a map lump
 END IF
 typecode = LEFT(extn, 1)
 IF INSTR("tpeznl", typecode) = 0 THEN RETURN NO  'Isn't a recognised/supported map lump

 WITH lump_reloading

  IF mapnum <> gam.map.id THEN
   'Affects map(s) other then the current one. However, we should still delete saved map state.
   'Not really sure what to do if the mode loadmodeIfUnchanged or loadmodeMerge... deleting seems safest bet.

   DIM statefile as string = mapstatetemp(mapnum, "map") + "_" + typecode
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

  DIM newhash as ulongint = file_hash64(workingdir + basename + "." + extn)

  SELECT CASE typecode
   CASE "t"  '--all modes supported
    IF .maptiles.changed = NO THEN
     'reload_MAP_lump sets .maptiles.changed = YES if
     'the tilesets have changed and we need to reload.
     '(Warning: this assumes that the .t lump will always be rewritten by the map
     'editor after modifying .map... which is currently the case. We'll replace
     'the map file formats before that assumption is violated)
     IF .maptiles.hash = newhash THEN RETURN YES
    END IF
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

FUNCTION try_reload_global_npcs(filename as string) as bool
 IF filename <> "globalnpcs1.n" THEN RETURN NO
 /' Future: support multiple NPC pools
 DIM pool_id as integer = -1
 IF extn = "n" ANDALSO starts_with(basename, "globalnpcs") THEN
  pool_id = str2int(MID(basename, 10), -1)
 END IF
 IF pool_id = -1 THEN RETURN NO
 '/
 DIM newhash as ulongint = file_hash64(workingdir + filename)

 WITH lump_reloading
  IF .globalnpcs.hash = newhash THEN RETURN YES
  .globalnpcs.changed = YES
  IF .globalnpcs.dirty THEN
   IF .globalnpcs.mode = loadmodeAlways THEN load_global_npcs
  ELSE
   IF .globalnpcs.mode <> loadmodeNever THEN load_global_npcs
  END IF
 END WITH
 RETURN YES
END FUNCTION

'Returns true (and reloads as needed) if this file is a music file (.## or song##.xxx)
FUNCTION try_reload_music_lump(basename as string, extn as string) as bool
 DIM songnum as integer = str2int(extn, -1)  'BAM songs
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

'Returns true if this file is a sound effect
FUNCTION try_reload_sfx_lump(basename as string, extn as string) as bool
 IF LEFT(basename, 3) <> "sfx" THEN RETURN NO
 DIM sfxnum as integer = str2int(MID(basename, 4))
 IF sfxnum = -1 THEN RETURN NO
 freesfx sfxnum  ' Stop & clear from cache. Don't bother to restart if playing.
 RETURN YES
END FUNCTION

'This sub does lump reloading which is safe to do from anywhere
SUB try_reload_lumps_anywhere ()
 ' This is called from global_setkeys_hook but is not reentrant
 STATIC entered as bool = NO
 IF entered THEN EXIT SUB
 entered = YES

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
  DIM handled as bool = NO
  DIM basename as string = trimextension(modified_lumps[i])
  DIM extn as string = justextension(modified_lumps[i])

  IF v_find(ignorable_extns, extn) > -1 THEN
   handled = YES

  ELSEIF extn = "gen" THEN                                                '.GEN
   reload_gen()
   handled = YES

  ELSEIF modified_lumps[i] = "general.reld" THEN                          'GENERAL.RELD
   reload_general_reld()
   handled = YES

  'We correctly handle binsize.bin & fixbits.bin updates, but there's no good reason for
  'them to happen while live previewing
  ELSEIF modified_lumps[i] = "binsize.bin" THEN                           'BINSIZE.BIN
   clear_binsize_cache
   showbug "Received binsize.bin modification, should not happen!"
   handled = YES

  ELSEIF modified_lumps[i] = "fixbits.bin" THEN                           'FIXBITS.BIN
   clear_fixbits_cache
   showbug "Received fixbits.bin modification, should not happen!"
   handled = YES

  ELSEIF modified_lumps[i] = "palettes.bin" THEN                          'PALETTES.BIN
   loadpalette master(), gam.current_master_palette
   setpal master()
   handled = YES

  ELSEIF modified_lumps[i] = "uicolors.bin" THEN                          'UICOLORS.BIN
   'UI colors are now stored in general.reld, but still written to uicolors.bin
   'for forwards-compatibility. So ignore this file.
   handled = YES

  ELSEIF modified_lumps[i] = "menus.bin" THEN                             'MENUS.BIN
   'This is far from complete
   'Cause cache in getmenuname to be dropped
   game_unique_id = STR(randint(INT_MAX))

  ELSEIF try_reload_gfx_lump(modified_lumps[i], extn) THEN                '.RGFX, .PT#, .TIL, .MXS
   handled = YES

  ELSEIF extn = "fnt" THEN                                                '.FNT
   xbload game + ".fnt", current_font(), "Font not loaded"
   setfont current_font()
   handled = YES

  ELSEIF try_reload_music_lump(basename, extn) THEN                       '.## and song##.xxx (music)
   handled = YES

  ELSEIF try_reload_sfx_lump(basename, extn) THEN                         'sfx##.xxx (sound effects)
   handled = YES

  ELSEIF modified_lumps[i] = "heroes.reld" THEN                           'HEROES.RELD
   reload_heroes_reld
   handled = YES

  ELSEIF extn = "dt0" THEN                                                '.DT0
   'Ignore: old hero data (redundant to heroes.reld) only for compatibility
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

  ELSEIF extn = "veh" THEN                                                '.VEH
   reload_vehicle
   handled = YES

                                                                          ''' Script stufff

  ELSEIF extn = "hsp" THEN                                                '.HSP
   lump_reloading.hsp.changed = YES
   IF lump_reloading.hsp.mode = loadmodeAlways THEN reload_scripts NO
   handled = YES

  ELSEIF modified_lumps[i] = "plotscr.lst" THEN                           'PLOTSCR.LST
   load_script_triggers_and_names  'Reloads both plotscr.lst and lookup1.bin
   handled = YES

  ELSEIF modified_lumps[i] = "lookup1.bin" THEN                           'LOOKUP1.BIN
   load_script_triggers_and_names  'Reloads both plotscr.lst and lookup1.bin
   handled = YES

  END IF

  IF handled THEN
   v_delete_slice modified_lumps, i, i + 1
  ELSE
   i += 1
  END IF

 WEND
 entered = NO
END SUB

SUB try_to_reload_lumps_onmap ()
 'calls receive_file_updates
 try_reload_lumps_anywhere

 DIM i as integer = 0
 WHILE i < v_len(modified_lumps)
  DIM handled as bool = NO
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

  ELSEIF try_reload_global_npcs(modified_lumps[i]) THEN                   'GLOBALNPCS#.N
   handled = YES

  ELSEIF extn = "tap" THEN                                                '.TAP
   reloadtileanimations tilesets(), gmap()
   handled = YES

  ELSEIF extn = "dt1" THEN                                                '.DT1
   ' This wipes all changes to all records due to scripts;
   ' it would be possible to do a record-by-record merge instead,
   ' but enemy (and formation) edits are probably normally done
   ' immediately before a battle.
   writeablecopyfile game + ".dt1", tmpdir & "dt1.tmp"
   handled = YES

  ELSEIF extn = "for" THEN                                                '.FOR
   ' Ditto as for .dt1
   writeablecopyfile game + ".for", tmpdir & "for.tmp"
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

FUNCTION lump_reload_mode_to_string (mode as LoadModeEnum) as string
 IF mode = loadmodeNever THEN RETURN "Never"
 IF mode = loadmodeAlways THEN RETURN "Always"
 IF mode = loadmodeIfUnchanged THEN RETURN "If no in-game changes"
 IF mode = loadmodeMerge THEN RETURN "Merge in-game changes"
END FUNCTION

SUB LPM_append_reload_mode_item (menu as MenuDef, tooltips() as string, what as zstring ptr, info as LumpReloadState, byval extradata as integer = 0)
 append_menu_item menu, "Reload " + *what + ": " + lump_reload_mode_to_string(info.mode)
 menu.last->extra(0) = extradata
 REDIM PRESERVE tooltips(menu.numitems - 1)
END SUB

SUB LPM_append_force_reload_item (menu as MenuDef, tooltips() as string, what as zstring ptr, info as LumpReloadState, byval extradata as integer = 0, byval ignore_dirtiness as bool = NO)
 append_menu_item menu, "Force reload of " + *what
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
  IF running_under_Custom THEN
   LPM_append_reload_mode_item menu1, tooltips(), "gen. map data", .gmap, 10
   LPM_append_reload_mode_item menu1, tooltips(), "tilemap", .maptiles, 11
   LPM_append_reload_mode_item menu1, tooltips(), "wallmap", .passmap, 12
   LPM_append_reload_mode_item menu1, tooltips(), "foemap", .foemap, 13
   LPM_append_reload_mode_item menu1, tooltips(), "zonemap", .zonemap, 14
   LPM_append_reload_mode_item menu1, tooltips(), "npc instances", .npcl, 15
   LPM_append_reload_mode_item menu1, tooltips(), "local npc defs.", .npcd, 16
   LPM_append_reload_mode_item menu1, tooltips(), "global npc defs.", .globalnpcs, 17
   LPM_append_reload_mode_item menu1, tooltips(), "scripts", .hsp, 20
   tooltips(UBOUND(tooltips)) += " (Read Help file!)"
  END IF
  LPM_append_force_reload_item menu1, tooltips(), "general map data", .gmap, 100
  LPM_append_force_reload_item menu1, tooltips(), "tiles", .maptiles, 101
  LPM_append_force_reload_item menu1, tooltips(), "wallmap", .passmap, 102
  LPM_append_force_reload_item menu1, tooltips(), "foemap", .foemap, 103
  LPM_append_force_reload_item menu1, tooltips(), "zones", .zonemap, 104
  LPM_append_force_reload_item menu1, tooltips(), "npc instances", .npcl, 105, YES  'NPCL is virtually always dirty
  LPM_append_force_reload_item menu1, tooltips(), "local npc defs.", .npcd, 106
  LPM_append_force_reload_item menu1, tooltips(), "global npc defs.", .globalnpcs, 107

  IF running_under_Custom THEN 'Not useful otherwise
   LPM_append_force_reload_item menu1, tooltips(), "scripts", .hsp, 110
   tooltips(UBOUND(tooltips)) += " (Read Help file!)"
  END IF

  init_menu_state st1, menu1
  REDIM PRESERVE tooltips(menu1.numitems - 1)
 END WITH
END SUB

SUB live_preview_menu ()
 DIM holdscreen as integer
 holdscreen = duplicatepage(vpage)

 DIM st1 as MenuState
 st1.active = YES
 
 DIM menu1 as MenuDef
 menu1.textalign = alignLeft
 menu1.boxstyle = 3
 menu1.translucent = YES
 menu1.min_chars = 38
 menu1.offset.y = -6

 REDIM tooltips() as string

 push_and_reset_gfxio_state
 DO
  setwait 55
  setkeys
  IF running_under_Custom THEN try_to_reload_lumps_onmap

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "game_live_preview_menu"

  LPM_update menu1, st1, tooltips()

  usemenu st1
  SELECT CASE menu1.items[st1.pt]->extra(0)
   CASE 1  '--exit
    IF enter_space_click(st1) THEN EXIT DO
   CASE 2  '--reload map
    IF enter_space_click(st1) THEN
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
   CASE 17  '--global npcs reload mode
    st1.need_update OR= intgrabber(lump_reloading.globalnpcs.mode, 0, 2)
   CASE 20  '--script reload mode
    st1.need_update OR= intgrabber(lump_reloading.hsp.mode, 0, 1)
   CASE 100  '--force gmap reload
    IF enter_space_click(st1) THEN
     'User asked to reload general map data, not tilemaps, so don't update tilesets and map layers
     reloadmap_gmap_no_tilesets
    END IF
   CASE 101  '--force tile reload
    IF enter_space_click(st1) THEN
     reloadmap_tilemap_and_tilesets NO
    END IF
   CASE 102  '--force wallmap reload
    IF enter_space_click(st1) THEN
     reloadmap_passmap NO
    END IF
   CASE 103  '--force foemap reload
    IF enter_space_click(st1) THEN
     reloadmap_foemap
    END IF
   CASE 104  '--force zonemap reload
    IF enter_space_click(st1) THEN
     reloadmap_zonemap
    END IF
   CASE 105  '--force npcl reload
    IF enter_space_click(st1) THEN
     reloadmap_npcl NO
    END IF
   CASE 106  '--force npcd reload
    IF enter_space_click(st1) THEN
     reloadmap_npcd
    END IF
   CASE 107  '--force global npcs reload
    IF enter_space_click(st1) THEN
     load_global_npcs
    END IF
   CASE 110  '--force scripts reload
    IF enter_space_click(st1) THEN
     reload_scripts
    END IF
  END SELECT

  'Draw screen
  copypage holdscreen, vpage
  draw_menu menu1, st1, vpage
  IF LEN(tooltips(st1.pt)) THEN
   basic_textbox tooltips(st1.pt), , vpage, pBottom, , YES, YES
  END IF
  setvispage vpage
  dowait
 LOOP
 pop_gfxio_state
 freepage holdscreen
END SUB
