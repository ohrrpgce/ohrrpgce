'OHRRPGCE GAME
'(C) Copyright 1997-2015 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
' This module contains code for:
' -creating and updating walkabout slices for heroes and NPCs
' -updating hero/NPC visibility
' -utility functions (not related to slices) for hero/NPC movement/collision/map-wrapping
' -vehicles
' Note that the hero/NPC movement code itself is in game.bas (eg update_npcs())

#include "config.bi"
#include "udts.bi"
#include "gglobals.bi"
#include "common.bi"
#include "loading.bi"
#include "allmodex.bi"
#include "game.bi"
#include "scripting.bi"
#include "moresubs.bi"
#include "walkabouts.bi"

'local subs and functions
' DECLARE FUNCTION should_hide_hero_caterpillar() as integer
' DECLARE FUNCTION should_show_normal_caterpillar() as integer


'==========================================================================================
'                          Creating/modifying walkabout slices
'==========================================================================================


FUNCTION create_walkabout_slices(byval parent as Slice Ptr) as Slice Ptr
 DIM sl as Slice Ptr
 sl = NewSliceOfType(slContainer, parent)
 WITH *sl
  .Width = 20
  .Height = 20
  .Protect = YES
 END WITH
 DIM sprsl as Slice Ptr
 sprsl = NewSliceOfType(slSprite, sl, SL_WALKABOUT_SPRITE_COMPONENT)
 WITH *sprsl
  'Anchor and align NPC sprite in the bottom center of the NPC container
  .AnchorHoriz = 1
  .AnchorVert = 2
  .AlignHoriz = 1
  .AlignVert = 2
  .Protect = YES
 END WITH
 RETURN sl
END FUNCTION

SUB create_walkabout_shadow (byval walkabout_cont as Slice Ptr)
 IF walkabout_cont = 0 THEN debug "create_walkabout_shadow: null walkabout container": EXIT SUB
 DIM sprsl as Slice Ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
 IF sprsl = 0 THEN debug "create_walkabout_shadow: null walkabout sprite": EXIT SUB
 DIM shadow as Slice Ptr
 shadow = NewSliceOfType(slEllipse, ,SL_WALKABOUT_SHADOW_COMPONENT)
 WITH *shadow
  .Width = 12
  .Height = 6
  .AnchorHoriz = 1
  .AlignHoriz = 1
  .AnchorVert = 2
  .AlignVert = 2
  .Y = gmap(11) 'foot offset
  .Visible = NO
 END WITH
 ChangeEllipseSlice shadow, uilook(uiShadow), uilook(uiShadow)
 InsertSliceBefore(sprsl, shadow)
END SUB

SUB delete_walkabout_shadow (byval walkabout_cont as Slice Ptr)
 IF walkabout_cont = 0 THEN debug "delete_walkabout_shadow: null walkabout container": EXIT SUB
 DIM shadow as Slice Ptr
 shadow = LookupSlice(SL_WALKABOUT_SHADOW_COMPONENT, walkabout_cont)
 IF shadow = 0 THEN debug "delete_walkabout_shadow: no shadow to delete" : EXIT SUB
 DeleteSlice @shadow
END SUB

'Change picture and/or palette of a walkabout slice.
'
'Here is an exhaustive list of the conditions under which a walkabout sprite slice gets updated,
'(picture and/or palette is changed), wiping out any modifications by scripts.
'Note everything else such as extra data and child slices of the walkabout sprite and container
'slices always remain untouched, except for the container position, shadow slice, and walking animation.
'
'-When a specific NPC is enabled or disabled by tag changes
' (disabled NPCs have their container slices deleted, not just hidden)
'-When alternpc, changenpcid, etc, is used, affecting a specific NPC/NPC ID
'-When reset_npc_graphics gets called, which happens when loading map state (sometimes after
' a battle, changing maps, loadmapstate) or when live previewing (changes to NPC data)
'ALL hero sprite slices get reloaded (vishero is called) when:
'-calling reset/setheropicture/palette(outsidebattle) on a walkabout party hero
'-the hero party changes, such as when changing the order of heroes
'Unlike NPCs, hero container slices are hidden rather than disabled when a hero slot is empty.
'Hero container slices are never recreated when changing maps, etc.
'
SUB set_walkabout_sprite (byval cont as Slice Ptr, byval pic as integer=-1, byval pal as integer=-2)
 DIM sprsl as Slice Ptr
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_sprite"
 ELSE
  sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, cont)
  IF sprsl = 0 THEN
   debug "null sprite slice in set_walkabout_sprite"
  ELSE
   ChangeSpriteSlice sprsl, 4, pic, pal
  END IF
 END IF
END SUB

SUB set_walkabout_frame (byval cont as Slice Ptr, byval direction as integer, byval frame as integer)
 DIM sprsl as Slice Ptr
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_frame"
 ELSE
  sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, cont)
  IF sprsl = 0 THEN
   debug "null sprite slice in set_walkabout_frame"
  ELSE
   ChangeSpriteSlice sprsl, , , , direction * 2 + frame
  END IF
 END IF
END SUB

SUB set_walkabout_vis (byval cont as Slice Ptr, byval vis as integer)
 IF cont = 0 THEN
  debug "null container slice in set_walkabout_vis"
 ELSE
  cont->Visible = vis
 END IF
END SUB

SUB update_walkabout_pos (byval walkabout_cont as slice ptr, byval x as integer, byval y as integer, byval z as integer)
 IF walkabout_cont = 0 THEN
  'Exit silently on null slices. It is normal to call this on hero slices that don't exist when the party is non-full
  EXIT SUB
 END IF

 DIM where as XYPair
 '+ gmap(11)
 framewalkabout x, y , where.x, where.y, mapsizetiles.x * 20, mapsizetiles.y * 20, gmap(5)
 WITH *walkabout_cont
  .X = where.x + mapx
  .Y = where.y + mapy
 END WITH

 DIM sprsl as Slice Ptr
 sprsl = LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, walkabout_cont)
 IF sprsl = 0 THEN
  debug "update_walkabout_pos: null sprite slice for walkabout slice " & walkabout_cont
 ELSE
  sprsl->Y = gmap(11) - z
 END IF
END SUB


'==========================================================================================
'                           Fixup slices after NPCType changes
'==========================================================================================


'Reset npc sprite slices to match npc definitions.
'Used only when reloading all npcs. See set_walkabout_sprite documentation.
'reset_npc_graphics is always called after visnpc (though it's not necessarily so),
'which enables or disables npcs and also creates the slices for any npcs that were enabled.
SUB reset_npc_graphics ()
 DIM npc_id as integer
 FOR i as integer = 0 TO UBOUND(npc)
  npc_id = npc(i).id - 1
  IF npc_id >= 0 THEN
   IF npc_id > UBOUND(npcs) THEN
    debug "reset_npc_graphics: ignore npc " & i & " because npc def " & npc_id & " is out of range (>" & UBOUND(npcs) & ")"
   ELSE
    'Update/load sprite
    set_walkabout_sprite npc(i).sl, npcs(npc_id).picture, npcs(npc_id).palette
    set_walkabout_vis npc(i).sl, YES
   END IF
  END IF
 NEXT i
END SUB

SUB change_npc_def_sprite (byval npc_id as integer, byval walkabout_sprite_id as integer)
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id - 1 = npc_id THEN
   'found a match!
   set_walkabout_sprite npc(i).sl, walkabout_sprite_id
  END IF
 NEXT i
END SUB

SUB change_npc_def_pal (byval npc_id as integer, byval palette_id as integer)
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id - 1 = npc_id THEN
   'found a match!
   set_walkabout_sprite npc(i).sl, , palette_id
  END IF
 NEXT i
END SUB


'==========================================================================================
'                         Updating slices to match Hero/NPC data
'==========================================================================================


PRIVATE FUNCTION should_hide_hero_caterpillar() as integer
 RETURN vstate.active = YES _
   ANDALSO vstate.mounting = NO _
   ANDALSO vstate.trigger_cleanup = NO _
   ANDALSO vstate.ahead = NO _
   ANDALSO vstate.dat.do_not_hide_leader = NO _
   ANDALSO vstate.dat.do_not_hide_party = NO
END FUNCTION

PRIVATE FUNCTION should_show_normal_caterpillar() as integer
 RETURN readbit(gen(), genBits, 1) = 1 _
   ANDALSO (vstate.active = NO ORELSE vstate.dat.do_not_hide_leader = NO)
END FUNCTION

PRIVATE SUB update_walkabout_hero_slices()

 DIM should_hide as integer = should_hide_hero_caterpillar()
 FOR i as integer = 0 TO UBOUND(herow)
  set_walkabout_vis herow(i).sl, NOT should_hide
 NEXT i

 IF should_show_normal_caterpillar() THEN
  FOR i as integer = 0 TO UBOUND(herow)
   update_walkabout_pos herow(i).sl, catx(i * 5), caty(i * 5), catz(i * 5)
  NEXT i

  DIM cat_slot as integer = 0
  FOR party_slot as integer = 0 TO 3
   IF gam.hero(party_slot).id >= 0 THEN
    set_walkabout_frame herow(cat_slot).sl, catd(cat_slot * 5), (herow(cat_slot).wtog \ 2)
    cat_slot += 1
   END IF
  NEXT party_slot
  FOR i as integer = cat_slot TO UBOUND(herow)
   set_walkabout_vis herow(i).sl, NO
  NEXT i

 ELSE
  '--non-caterpillar party, vehicle no-hide-leader (or backcompat pref)
  update_walkabout_pos herow(0).sl, catx(0), caty(0), catz(0)
  set_walkabout_frame herow(0).sl, catd(0), (herow(0).wtog \ 2)
  FOR i as integer = 1 TO UBOUND(herow)
   set_walkabout_vis herow(i).sl, NO
  NEXT i
 END IF

END SUB

PRIVATE SUB update_walkabout_npc_slices()
 DIM shadow as Slice Ptr

 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id > 0 THEN '-- if visible
   IF vstate.active AND vstate.npc = i THEN
    '--This NPC is a currently active vehicle, so lets do some extra voodoo.
    IF npc(i).sl <> 0 THEN
     shadow = LookupSlice(SL_WALKABOUT_SHADOW_COMPONENT, npc(i).sl)
     IF shadow <> 0 THEN
      shadow->Visible = (npc(i).z > 0 ANDALSO vstate.dat.disable_flying_shadow = NO)
     END IF
    END IF
   END IF
   update_walkabout_pos npc(i).sl, npc(i).x, npc(i).y, npc(i).z
   IF npc(i).sl <> 0 THEN
    '--default NPC sort is by instance id
    npc(i).sl->Sorter = i
   END IF
  ELSEIF npc(i).id <= 0 THEN
   '--remove unused and hidden NPC slices
   IF npc(i).sl <> 0 THEN
    debug "Sloppy housekeeping: delete npc sl " & i & " [update_walkabout_npc_slices]"
    DeleteSlice @npc(i).sl
   END IF
  END IF
 NEXT i

 '--now apply sprite frame changes
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id > 0 THEN '-- if visible
   set_walkabout_frame npc(i).sl, npc(i).dir, npc(i).frame \ 2
  END IF
 NEXT i
 
 '--If NPCs and heroes are in the same layer, move heroes to the end of the
 '--sibling list to make sure they layer over NPCs if their Y is equal
 IF gmap(16) = 2 THEN
  FOR cat_rank as integer = 3 TO 0 STEP -1
   IF herow(cat_rank).sl <> 0 THEN
    SetSliceParent herow(cat_rank).sl, SliceGetParent(herow(cat_rank).sl)
   END IF
  NEXT cat_rank
 END IF

END SUB

'Does not create or delete NPC/hero slices (vishero/visnpc), but only updates their graphical state.
SUB update_walkabout_slices()
 update_walkabout_hero_slices()
 update_walkabout_npc_slices()
END SUB

'Reload party walkabout graphics
SUB vishero ()
 DIM cater_slot as integer = 0
 FOR party_slot as integer = 0 TO 3
  IF gam.hero(party_slot).id >= 0 THEN
   set_walkabout_sprite herow(cater_slot).sl, gam.hero(party_slot).pic, gam.hero(party_slot).pal
   cater_slot += 1
  END IF
 NEXT
END SUB

SUB visnpc()
 'Hide/Unhide NPCs based on tag requirements. (No other function should do so)
 'This SUB will be called when a map is incompletely loaded (NPC instances before definitions
 'or vice versa), and that's hard to avoid, because a script could load them with two separate loadmapstate
 'calls. So we must tolerate invalid NPC IDs and anything else. So here we mark all NPCs as hidden which
 'would otherwise cause problems

 'To scripts, hiding an NPC is like deleting it, and unhiding an NPC is like creating it.
 'Therefore, zone exit triggers *do not* happen when hiding an NPC, and zone entry triggers *do*
 'happen when unhiding an NPC (rather than remembering the old zones)
 'However, we run the zone entry triggers elsewhere (update_npcs), otherwise tags toggled by the
 'triggers would immediately affect NPCs not yet processed (it's better if the order doesn't
 'matter), and worse, visnpc might be called several times per tick!

 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).id = 0 THEN CONTINUE FOR

  DIM npc_id as integer = ABS(npc(i).id) - 1

  IF npc_id > UBOUND(npcs) THEN
   'Invalid ID number; hide. Probably a partially loaded map.
   npc(i).id = -npc_id - 1
   CONTINUE FOR
  END IF
 
  '--check tags
  IF istag(npcs(npc_id).tag1, 1) ANDALSO istag(npcs(npc_id).tag2, 1) ANDALSO istag(onetime(), npcs(npc_id).usetag, 0) = 0 THEN
   npc(i).id = npc_id + 1
  ELSE
   npc(i).id = -npc_id - 1
  END IF
  
  IF npc(i).id > 0 THEN
   '--NPC exists and is visible
   IF npc(i).sl = 0 THEN
    npc(i).sl = create_walkabout_slices(npc_layer())
    'debug "npc(" & i & ").sl=" & npc(i).sl & " [visnpc]"
    '--set sprite
    set_walkabout_sprite npc(i).sl, npcs(npc_id).picture, npcs(npc_id).palette
   END IF
  ELSE
   '--hidden
   IF npc(i).sl <> 0 THEN
    'debug "delete npc sl " & i & " [visnpc]"
    DeleteSlice @npc(i).sl
   END IF
   v_free npc(i).curzones
  END IF

 NEXT i
END SUB


'==========================================================================================
'                                 Walkabout slice layers
'==========================================================================================


FUNCTION hero_layer() as Slice Ptr
 DIM layer as Slice Ptr
 IF gmap(16) = 2 THEN ' heroes and NPCs together
  layer = SliceTable.Walkabout
 ELSE ' heroes and NPCs on separate layers
  layer = SliceTable.HeroLayer
 END IF
 IF layer = 0 THEN
  debug "Warning: null hero layer, gmap(16)=" & gmap(16)
 END IF
 RETURN layer
END FUNCTION

FUNCTION npc_layer() as Slice Ptr
 DIM layer as Slice Ptr
 IF gmap(16) = 2 THEN ' heroes and NPCs together
  layer = SliceTable.Walkabout
 ELSE ' heroes and NPCs on separate layers
  layer = SliceTable.NPCLayer
 END IF
 IF layer = 0 THEN
  debug "Warning: null npc layer, gmap(16)=" & gmap(16)
 END IF
 RETURN layer
END FUNCTION

SUB reparent_hero_slices()
 FOR i as integer = 0 TO UBOUND(herow)
  SetSliceParent herow(i).sl, hero_layer()
 NEXT i
END SUB

SUB orphan_hero_slices()
 FOR i as integer = 0 TO UBOUND(herow)
  OrphanSlice herow(i).sl
 NEXT i
END SUB

SUB reparent_npc_slices()
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).sl THEN
   SetSliceParent npc(i).sl, npc_layer()
  END IF
 NEXT i
END SUB

SUB orphan_npc_slices()
 FOR i as integer = 0 TO UBOUND(npc)
  IF npc(i).sl THEN
   OrphanSlice npc(i).sl
  END IF
 NEXT i
END SUB

SUB refresh_walkabout_layer_sort()
 orphan_hero_slices
 orphan_npc_slices
 IF gmap(16) = 2 THEN ' Heroes and NPCs Together
  DeleteSlice @SliceTable.HeroLayer
  DeleteSlice @SliceTable.NPCLayer
  SliceTable.Walkabout->AutoSort = slAutoSortY
 ELSE
  'Hero and NPC in separate layers
  SliceTable.Walkabout->AutoSort = slAutoSortNone
  IF SliceTable.HeroLayer = 0 THEN
   '--create the hero layer if it is needed
   SliceTable.HeroLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_HERO_LAYER)
   SliceTable.HeroLayer->Fill = YES
   SliceTable.HeroLayer->Protect = YES
   SliceTable.HeroLayer->AutoSort = slAutoSortY
  END IF
  IF SliceTable.NPCLayer = 0 THEN
   SliceTable.NPCLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_NPC_LAYER)
   SliceTable.NPCLayer->Fill = YES
   SliceTable.NPCLayer->Protect = YES
   SliceTable.NPCLayer->AutoSort = slAutoSortCustom
  END IF
  IF gmap(16) = 1 THEN
   SliceTable.HeroLayer->Sorter = 0
   SliceTable.NPCLayer->Sorter = 1
  ELSE
   SliceTable.NPCLayer->Sorter = 0
   SliceTable.HeroLayer->Sorter = 1
  END IF
  CustomSortChildSlices SliceTable.Walkabout, YES
 END IF
 reparent_hero_slices
 reparent_npc_slices
END SUB


'==========================================================================================
'                       Movement, collision & map-wrapping funcs
'==========================================================================================


FUNCTION movdivis (byval xygo as integer) as bool
 IF (xygo \ 20) * 20 = xygo AND xygo <> 0 THEN
  movdivis = YES
 ELSE
  movdivis = NO
 END IF
END FUNCTION

SUB aheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer)
 '--alters the input X and Y, moving them "ahead" by distance in direction

 IF direction = 0 THEN y = y - distance
 IF direction = 1 THEN x = x + distance
 IF direction = 2 THEN y = y + distance
 IF direction = 3 THEN x = x - distance
END SUB

SUB wrapxy (byref x as integer, byref y as integer, byval wide as integer, byval high as integer)
 '--wraps the given X and Y values within the bounds of width and height
 x = ((x MOD wide) + wide) MOD wide  'negative modulo is the devil's creation and never helped me once
 y = ((y MOD high) + high) MOD high
END SUB

'alters X and Y ahead by distance in direction, wrapping if neccisary
'unitsize is 20 for pixels, 1 for tiles
SUB wrapaheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer, byval unitsize as integer)
 aheadxy x, y, direction, distance
 
 IF gmap(5) = 1 THEN
  wrapxy x, y, mapsizetiles.x * unitsize, mapsizetiles.y * unitsize
 END IF
END SUB

SUB cropposition (byref x as integer, byref y as integer, byval unitsize as integer)
 IF gmap(5) = 1 THEN
  wrapxy x, y, mapsizetiles.x * unitsize, mapsizetiles.y * unitsize
 ELSE
  x = bound(x, 0, (mapsizetiles.x - 1) * unitsize)
  y = bound(y, 0, (mapsizetiles.y - 1) * unitsize)
 END IF
END SUB

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

' Returns true if blocked by terrain
FUNCTION wrappass (byval x as integer, byval y as integer, byref xgo as integer, byref ygo as integer, byval isveh as integer) as integer
 wrappass = 0
 REDIM pd(3) as integer
 
 DIM tilex as integer = x
 DIM tiley as integer = y
 DIM p as integer = readblock(pass, tilex, tiley)
 
 FOR i as integer = 0 TO 3
  tilex = x
  tiley = y
  wrapaheadxy tilex, tiley, i, 1, 1
  IF tilex < 0 ORELSE tilex >= pass.wide ORELSE tiley < 0 ORELSE tiley >= pass.high THEN
   pd(i) = 15
  ELSE
   pd(i) = readblock(pass, tilex, tiley)
  END IF
 NEXT i
 
 'debug "wrappass x=" & x & " y=" & y & " xgo=" & xgo & " ygo=" & ygo & " p=" & p & " north=" & pd(0) & " east=" & pd(1) & " south=" & pd(2) & " west=" & pd(3)
 
 IF ygo > 0 ANDALSO movdivis(ygo) ANDALSO ((p AND passNorthWall) = passNorthWall ORELSE (pd(0) AND passSouthWall) = passSouthWall ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(0), 0))) THEN ygo = 0: wrappass = 1
 IF ygo < 0 ANDALSO movdivis(ygo) ANDALSO ((p AND passSouthWall) = passSouthWall ORELSE (pd(2) AND passNorthWall) = passNorthWall ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(2), 0))) THEN ygo = 0: wrappass = 1
 IF xgo > 0 ANDALSO movdivis(xgo) ANDALSO ((p AND passWestWall) = passWestWall   ORELSE (pd(3) AND passEastWall) = passEastWall   ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(3), 0))) THEN xgo = 0: wrappass = 1
 IF xgo < 0 ANDALSO movdivis(xgo) ANDALSO ((p AND passEastWall) = passEastWall   ORELSE (pd(1) AND passWestWall) = passWestWall   ORELSE (isveh ANDALSO vehpass(vstate.dat.blocked_by, pd(1), 0))) THEN xgo = 0: wrappass = 1
END FUNCTION

FUNCTION wrapzonecheck (byval zone as integer, byval x as integer, byval y as integer, byval xgo as integer, byval ygo as integer) as integer
 'x, y in pixels
 'Warning: always wraps! But that isn't a problem on non-wrapping maps.

 x -= xgo
 y -= ygo
 wrapxy (x, y, mapsizetiles.x * 20, mapsizetiles.y * 20)
 RETURN CheckZoneAtTile(zmap, zone, x \ 20, y \ 20)
END FUNCTION

FUNCTION wrapcollision (byval xa as integer, byval ya as integer, byval xgoa as integer, byval ygoa as integer, byval xb as integer, byval yb as integer, byval xgob as integer, byval ygob as integer) as integer
 DIM as integer x1, x2, y1, y2
 x1 = (xa - bound(xgoa, -20, 20)) \ 20
 x2 = (xb - bound(xgob, -20, 20)) \ 20
 y1 = (ya - bound(ygoa, -20, 20)) \ 20
 y2 = (yb - bound(ygob, -20, 20)) \ 20

 IF gmap(5) = 1 THEN
  RETURN (x1 - x2) MOD mapsizetiles.x = 0 AND (y1 - y2) MOD mapsizetiles.y = 0
 ELSE
  RETURN (x1 = x2) AND (y1 = y2)
 END IF
END FUNCTION

FUNCTION wraptouch (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval distance as integer) as integer
 'whether 2 walkabouts are within distance pixels horizontally + vertically
 IF gmap(5) = 1 THEN
  IF ABS((x1 - x2) MOD (mapsizetiles.x * 20 - distance)) <= distance AND ABS((y1 - y2) MOD (mapsizetiles.y * 20 - distance)) <= distance THEN RETURN 1
 ELSE
  IF ABS(x1 - x2) <= 20 AND ABS(y1 - y2) <= 20 THEN RETURN 1
 END IF
 RETURN 0
END FUNCTION

'called on each coordinate of a screen position to wrap it around the map so that's it's as close as possible to being on the screen
PRIVATE FUNCTION closestwrappedpos (byval coord as integer, byval screenlen as integer, byval maplen as integer) as integer
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

'==========================================================================================
'                                        Vehicles
'==========================================================================================


FUNCTION vehicle_is_animating() as bool
 WITH vstate
  RETURN .mounting ORELSE .rising ORELSE .falling ORELSE .init_dismount ORELSE .ahead ORELSE .trigger_cleanup
 END WITH
END FUNCTION

SUB reset_vehicle(v as VehicleState)
 v.id = -1
 v.npc = 0
 v.old_speed = 0
 v.active   = NO
 v.mounting = NO
 v.rising   = NO
 v.falling  = NO
 v.init_dismount   = NO
 v.ahead           = NO
 v.trigger_cleanup = NO
 ClearVehicle v.dat
END SUB

SUB dump_vehicle_state()
 WITH vstate
  debug "active=" & .active & " npc=" & .npc & " id=" & .id & " mounting=" & .mounting & " rising=" & .rising & " falling=" & .falling & " dismount=" & .init_dismount & " cleanup=" & .trigger_cleanup & " ahead=" & .ahead
 END WITH
END SUB

SUB update_vehicle_state ()
 STATIC aheadx as integer
 STATIC aheady as integer

 IF vstate.mounting THEN '--scramble-----------------------
  '--part of the vehicle automount where heros scramble--
  IF npc(vstate.npc).xgo = 0 AND npc(vstate.npc).ygo = 0 THEN
   '--npc must stop before we mount
   IF vehscramble(npc(vstate.npc).x, npc(vstate.npc).y) THEN
    'Finished scramble
    vstate.mounting = NO
    IF vstate.dat.elevation > 0 THEN vstate.rising = YES
   END IF
  END IF
 END IF'--scramble mount
 IF vstate.rising THEN '--rise----------------------
  DIM risen_count as integer = 0
  FOR i as integer = 0 TO 3
   IF catz(i * 5) < vstate.dat.elevation THEN
    catz(i * 5) = catz(i * 5) + large(1, small(4, (vstate.dat.elevation - catz(i * 5) + 1) \ 2))
   ELSE
    risen_count += 1
   END IF
  NEXT i
  IF risen_count = 4 THEN
   vstate.rising = NO
  END IF
 END IF
 IF vstate.falling THEN '--fall-------------------
  DIM fallen_count as integer = 0
  FOR i as integer = 0 TO 3
   IF catz(i * 5) > 0 THEN
    catz(i * 5) = catz(i * 5) - large(1, small(4, (vstate.dat.elevation - catz(i * 5) + 1) \ 2))
   ELSE
    fallen_count += 1
   END IF
  NEXT i
  IF fallen_count = 4 THEN
   FOR i as integer = 0 TO 3
    catz(i * 5) = 0
   NEXT i
   vstate.falling = NO
   vstate.init_dismount = YES
  END IF
 END IF
 IF vstate.init_dismount THEN '--dismount---------------
  vstate.init_dismount = NO
  DIM disx as integer = catx(0) \ 20
  DIM disy as integer = caty(0) \ 20
  IF vstate.dat.dismount_ahead AND vstate.dat.pass_walls_while_dismounting THEN
   '--dismount-ahead is true, dismount-passwalls is true
   aheadxy disx, disy, catd(0), 1
   cropposition disx, disy, 1
  END IF
  IF vehpass(vstate.dat.dismount_to, readblock(pass, disx, disy), -1) THEN
   '--dismount point is landable
   FOR i as integer = 0 TO 15
    catx(i) = catx(0)
    caty(i) = caty(0)
    catd(i) = catd(0)
    catz(i) = 0
   NEXT i
   IF vstate.dat.dismount_ahead = YES THEN
    vstate.ahead = YES
    aheadx = disx * 20
    aheady = disy * 20
   ELSE
    vstate.trigger_cleanup = YES
   END IF
  ELSE
   '--dismount point is unlandable
   IF vstate.dat.elevation > 0 THEN
    vstate.rising = YES '--riseagain
   END IF
  END IF
 END IF
 IF vstate.trigger_cleanup THEN '--clear
  IF vstate.dat.on_dismount < 0 THEN trigger_script ABS(vstate.dat.on_dismount), YES, "vehicle dismount", "", scrqBackcompat()
  IF vstate.dat.on_dismount > 0 THEN loadsay vstate.dat.on_dismount
  settag vstate.dat.riding_tag, NO
  IF vstate.dat.dismount_ahead = YES AND vstate.dat.pass_walls_while_dismounting = NO THEN
   'FIXME: Why is this here, when dismounting is apparently also handled by vehscramble?
   'Does this have to do with Bug 764 - "Blocked by" vehicle setting does nothing ?
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
  herow(0).speed = vstate.old_speed
  npc(vstate.npc).xgo = 0
  npc(vstate.npc).ygo = 0
  npc(vstate.npc).z = 0
  delete_walkabout_shadow npc(vstate.npc).sl
  '--clear vehicle (sets vstate.active=NO, etc)
  reset_vehicle vstate
  FOR i as integer = 0 TO 15   'Why is this duplicated from dismounting?
   catx(i) = catx(0)
   caty(i) = caty(0)
   catd(i) = catd(0)
   catz(i) = 0
  NEXT i
  gam.random_battle_countdown = range(100, 60)
 END IF
 IF vstate.ahead THEN '--dismounting ahead
  IF vehscramble(aheadx, aheady) THEN
   vstate.ahead = NO
   vstate.trigger_cleanup = YES '--clear (happens next tick, maybe not intentionally)
  END IF
 END IF
 IF vstate.active = YES AND vehicle_is_animating() = NO THEN
  IF txt.showing = NO AND readbit(gen(), genSuspendBits, suspendplayer) = 0 THEN
   REDIM button(1) as integer
   button(0) = vstate.dat.use_button
   button(1) = vstate.dat.menu_button
   FOR i as integer = 0 TO 1
    IF carray(ccUse + i) > 1 AND herow(0).xgo = 0 AND herow(0).ygo = 0 THEN
     SELECT CASE button(i)
      CASE -2
       '-disabled
      CASE -1
       add_menu 0
       menusound gen(genAcceptSFX)
      CASE 0
       '--dismount
       vehicle_graceful_dismount
      CASE IS > 0
       trigger_script button(i), YES, "vehicle button " & i, "", scrqBackcompat()
     END SELECT
    END IF
   NEXT i
  END IF
 END IF'--not animating

 IF vstate.active THEN npc(vstate.npc).z = catz(0)
END SUB

SUB vehicle_graceful_dismount ()
 herow(0).xgo = 0
 herow(0).ygo = 0
 IF vstate.dat.elevation > 0 THEN
  vstate.falling = YES
 ELSE
  vstate.init_dismount = YES
 END IF
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

FUNCTION vehpass (byval n as integer, byval tile as integer, byval default as integer) as integer
 '--true means passable
 '--false means impassable

 DIM v as integer = default

 SELECT CASE n
  CASE 1
   v = (tile AND passVehA)
  CASE 2
   v = (tile AND passVehB)
  CASE 3
   v = ((tile AND passVehA) = passVehA) AND ((tile AND passVehB) = passVehB)
  CASE 4
   v = ((tile AND passVehA) = passVehA) OR ((tile AND passVehB) = passVehB)
  CASE 5
   v = NOT ((tile AND passVehA) = passVehA)
  CASE 6
   v = NOT ((tile AND passVehB) = passVehB)
  CASE 7
   v = NOT (((tile AND passVehA) = passVehA) OR ((tile AND passVehB) = passVehB))
  CASE 8
   v = YES
 END SELECT
 
 RETURN v <> 0
END FUNCTION

'Returns true if the scramble is finished
FUNCTION vehscramble(byval targx as integer, byval targy as integer) as bool
 DIM scrambled_heroes as integer = 0
 DIM count as integer = herocount()
 DIM scramx as integer
 DIM scramy as integer
 FOR i as integer = 0 TO 3
  IF i < count THEN
   scramx = catx(i * 5)
   scramy = caty(i * 5)
   IF ABS(scramx - targx) < large(herow(i).speed, 4) THEN
    scramx = targx
    herow(i).xgo = 0
    herow(i).ygo = 0
   END IF
   IF ABS(scramy - targy) < large(herow(i).speed, 4) THEN
    scramy = targy
    herow(i).xgo = 0
    herow(i).ygo = 0
   END IF
   IF ABS(targx - scramx) > 0 AND herow(i).xgo = 0 THEN
    herow(i).xgo = 20 * SGN(scramx - targx)
   END IF
   IF ABS(targy - scramy) > 0 AND herow(i).ygo = 0 THEN
    herow(i).ygo = 20 * SGN(scramy - targy)
   END IF
   IF gmap(5) = 1 THEN
    '--this is a wrapping map
    IF ABS(scramx - targx) > mapsizetiles.x * 20 / 2 THEN herow(i).xgo *= -1
    IF ABS(scramy - targy) > mapsizetiles.y * 20 / 2 THEN herow(i).ygo *= -1
   END IF
   IF scramx - targx = 0 AND scramy - targy = 0 THEN scrambled_heroes += 1
   catx(i * 5) = scramx
   caty(i * 5) = scramy
  END IF
 NEXT i
 IF scrambled_heroes = count THEN
  IF vstate.dat.on_mount < 0 THEN trigger_script ABS(vstate.dat.on_mount), YES, "vehicle on-mount", "", scrqBackcompat()
  IF vstate.dat.on_mount > 0 THEN loadsay vstate.dat.on_mount
  herow(0).speed = vstate.dat.speed
  IF herow(0).speed = 3 THEN herow(0).speed = 10
  '--null out hero's movement
  FOR i as integer = 0 TO 3
   herow(i).xgo = 0
   herow(i).ygo = 0
  NEXT i
  RETURN YES
 END IF
 RETURN NO
END FUNCTION
