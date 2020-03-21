'OHRRPGCE CUSTOM - Enemy/Hero Formation/Formation Set Editors
'(C) Copyright 1997-2018 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "const.bi"
#include "udts.bi"
#include "custom.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "slices.bi"
#include "thingbrowser.bi"
#include "sliceedit.bi"
#include "bcommon.bi"


'Local SUBs
DECLARE SUB formation_editor_main ()
DECLARE SUB draw_formation_slices OVERLOAD (eform as Formation, rootslice as Slice ptr, selected_slot as integer, page as integer)
DECLARE SUB draw_formation_slices OVERLOAD (eform as Formation, hform as HeroFormation, rootslice as Slice ptr, selected_slot as integer, page as integer, byval heromode as bool=NO)
DECLARE SUB load_formation_slices(ename() as string, form as Formation, rootslice as Slice ptr ptr)
DECLARE SUB hero_formation_editor ()
DECLARE SUB formation_init_added_enemy(byref slot as FormationSlot)

DECLARE SUB formation_set_editor_load_preview(state as MenuState, form_id as integer, formset as FormationSet, form as Formation, ename() as string, byref rootslice as Slice Ptr)

' Formation editor slice lookup codes
CONST SL_FORMEDITOR_BACKDROP = 100
CONST SL_FORMEDITOR_ENEMY = 200  '+0 to +7 for 8 slots
CONST SL_FORMEDITOR_LAST_ENEMY = 299  'End of range indicating an enemy slot
CONST SL_FORMEDITOR_CURSOR = 300
CONST SL_FORMEDITOR_HERO = 400  '+0 to +3 for 4 slots

'What hero sprites to use as placeholders, or -1 for a rectangle
DIM SHARED hero_placeholder_sprites(3) as integer = {-1, -1, -1, -1}


'==========================================================================================
'                                  Formation Previewer
'==========================================================================================

'Preview a hero or enemy formation. Select which by setting heromode.
TYPE FormationPreviewer EXTENDS RecordPreviewer
 heromode as bool       'True if previewing hero rather than enemy formations
 eform as Formation
 hform as HeroFormation
 rootslice as Slice ptr

 DECLARE DESTRUCTOR()
 DECLARE SUB update(form_id as integer, force_reload as bool = NO)
 DECLARE SUB draw(xpos as RelPos, ypos as RelPos, page as integer)
END TYPE

DESTRUCTOR FormationPreviewer
 DeleteSlice @rootslice
END DESTRUCTOR

SUB FormationPreviewer.update(form_id as integer, force_reload as bool = NO)
 IF heromode THEN
  eform.background = -1  'Show just a rect
  load_hero_formation hform, form_id
 ELSE
  LoadFormation eform, form_id
  load_hero_formation hform, eform.hero_form
 END IF
 DIM ename(7) as string  'Unused
 load_formation_slices ename(), eform, @rootslice
END SUB

SUB FormationPreviewer.draw(xpos as RelPos, ypos as RelPos, page as integer)
 draw_formation_slices eform, hform, rootslice, -1, page, heromode
END SUB

'==========================================================================================
'                             Top-level formation editor menu
'==========================================================================================

SUB formation_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as FormationBrowser
  b.browse(-1, , @individual_formation_editor)
 ELSE
  individual_formation_editor 0
 END IF
END SUB

FUNCTION formation_picker (recindex as integer = -1) as integer
 DIM b as FormationBrowser
 RETURN b.browse(recindex, , @individual_formation_editor, NO)
END FUNCTION

FUNCTION formation_picker_or_none (recindex as integer = -1) as integer
 DIM b as FormationBrowser
 RETURN b.browse(recindex - 1, YES , @individual_formation_editor, NO) + 1
END FUNCTION

'Total-level menu
SUB formation_editor
 DIM menu(3) as string
 menu(0) = "Return to Main Menu"
 menu(1) = "Edit Individual Enemy Formations..."
 menu(2) = "Construct Formation Sets..."
 menu(3) = "Edit Hero Formations..."

 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "formation_main"
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN formation_editor_main
   IF state.pt = 2 THEN formation_set_editor
   IF state.pt = 3 THEN hero_formation_editor
  END IF

  clearpage dpage
  standardmenu menu(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

'==========================================================================================
'                                   Formation Set Editor
'==========================================================================================

'set_id: which formation set to show. If -1, same as last time.
'Returns the formation set number we were last editing.
FUNCTION formation_set_editor (set_id as integer = -1) as integer
 STATIC remember_set_id as integer = 1
 IF set_id <= 0 THEN
  set_id = remember_set_id
 ELSE
  set_id = bound(set_id, 1, maxFormationSet)
 END IF

 DIM form as Formation
 DIM formset as FormationSet
 DIM form_id as integer
 DIM menu(23) as string
 DIM rootslice as Slice ptr
 DIM state as MenuState
 state.last = UBOUND(menu)
 state.size = 24
 state.need_update = YES
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1

 LoadFormationSet formset, set_id

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN
   SaveFormationSet formset, set_id
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "formation_sets"
  state.need_update OR= usemenu(state)
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN
    SaveFormationSet formset, set_id
    EXIT DO
   END IF
  END IF
  IF state.pt = 1 THEN
   DIM remember_id as integer = set_id
   IF intgrabber(set_id, 1, maxFormationSet) THEN
    SaveFormationSet formset, remember_id
    LoadFormationSet formset, set_id
    state.need_update = YES
   END IF
  END IF
  IF state.pt = 2 THEN intgrabber formset.frequency, 0, 200
  IF state.pt = 3 THEN tag_grabber formset.tag, state
  IF state.pt >= 4 THEN
   '--have form selected
   form_id = formset.formations(state.pt - 4)
   IF intgrabber(form_id, -1, gen(genMaxFormation)) THEN
    state.need_update = YES
   ELSEIF enter_space_click(state) THEN
    form_id = formation_picker_or_none(form_id + 1) - 1
    state.need_update = YES
   END IF
   formset.formations(state.pt - 4) = form_id
  ELSE
   form_id = -1
  END IF
  IF state.need_update THEN
   state.need_update = NO
   formation_set_editor_load_preview state, form_id, formset, form, menu(), rootslice
  END IF
  IF rootslice THEN
   draw_formation_slices form, rootslice, -1, dpage
  ELSE
   clearpage dpage
  END IF
  menu(0) = "Previous Menu"
  menu(1) = CHR(27) & "Formation Set " & set_id & CHR(26)
  menu(2) = "Battle Frequency: " & formset.frequency & " (" & formset_step_estimate(formset.frequency, " steps") & ")"
  menu(3) = tag_condition_caption(formset.tag, "Only if tag", "No tag check")

  standardmenu menu(), state, 0, 0, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 DeleteSlice @rootslice
 remember_set_id = set_id
 RETURN set_id
END FUNCTION

SUB formation_set_editor_load_preview(state as MenuState, form_id as integer, formset as FormationSet, form as Formation, menu() as string, byref rootslice as slice Ptr)
 IF form_id >= 0 THEN
  '--form not empty
  LoadFormation form, form_id
  DIM as string ename(7) 'not used here, but that is okay
  load_formation_slices ename(), form, @rootslice
 ELSE
  DeleteSlice @rootslice
 END IF
 'Also reload the formation descriptions for the menu
 DIM each_form as Formation
 FOR i as integer = 0 TO UBOUND(formset.formations)
  IF formset.formations(i) = -1 THEN
   menu(4 + i) = "Empty"
  ELSE
   LoadFormation each_form, game & ".for", formset.formations(i)
   menu(4 + i) = "Form " & formset.formations(i) & " " & describe_formation(each_form)
  END IF
 NEXT i
END SUB

'==========================================================================================
'                                   Hero Formation Editor
'==========================================================================================

'Prompt user how to add a new formation, or cancel, and update hero_form_id
SUB hero_formation_add_new(byref hero_form_id as integer)
 IF hero_form_id <> last_hero_formation_id() + 1 THEN showbug "Bad hero_formation_add_new call"

 DIM how as integer
 DIM previewer as FormationPreviewer
 previewer.heromode = YES
 how = generic_add_new("hero formation", last_hero_formation_id(), , @previewer, "add_hero_formation_how")
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy

 DIM hform as HeroFormation
 IF how = -1 THEN
  default_hero_formation hform
  save_hero_formation hform, hero_form_id
 ELSEIF how >= 0 THEN
  load_hero_formation hform, how
  save_hero_formation hform, hero_form_id
 ELSE
  hero_form_id = last_hero_formation_id()
 END IF
END SUB

SUB hero_formation_editor ()
 DIM hero_form_id as integer = 0
 DIM test_form_id as integer = 0
 DIM ename(7) as string
 DIM eform as Formation
 DIM hform as HeroFormation
 DIM default_hform as HeroFormation
 DIM rootslice as Slice ptr
 DIM positioning_mode as bool = NO
 DIM as integer bgwait, bgctr

 LoadFormation eform, test_form_id
 load_formation_slices ename(), eform, @rootslice

 DIM menu(6) as string
 DIM state as MenuState
 state.pt = 0
 state.top = 0
 state.first = 0
 state.last = UBOUND(menu)
 state.size = 20
 DIM menuopts as MenuOptions
 menuopts.edged = YES

 CONST first_hero_item = 3
 'slot -1 indicates no hero selected
 DIM slot as integer = state.pt - first_hero_item
 IF slot < 0 THEN slot = -1
 
 default_hero_formation default_hform
 load_hero_formation hform, hero_form_id

 setkeys
 DO
  setwait 55
  setkeys
  IF positioning_mode = YES THEN
   '--hero positioning mode
   IF keyval(ccCancel) > 1 OR enter_or_space() THEN setkeys: positioning_mode = NO
   IF readmouse.release AND mouseRight THEN setkeys: positioning_mode = NO
   IF keyval(scF1) > 1 THEN show_help "hero_formation_editor_placement"
   DIM as integer thiswidth = 0, thisheight = 0, movespeed = 1
   IF keyval(scShift) THEN movespeed = 8
   WITH hform.slots(slot)
    DIM hrect as Slice ptr = LookupSlice(SL_FORMEDITOR_HERO + slot, rootslice)
    IF hrect THEN
     thiswidth = hrect->Width
     thisheight = hrect->Height
    END IF
    IF keyval(ccUp) > 0 THEN .pos.y -= movespeed
    IF keyval(ccDown) > 0 THEN .pos.y += movespeed
    IF keyval(ccLeft) > 0 THEN .pos.x -= movespeed
    IF keyval(ccRight) > 0 THEN .pos.x += movespeed
    IF readmouse.dragging AND mouseLeft THEN
     .pos += (readmouse.pos - readmouse.lastpos)
    END IF
    'Hero positions are the bottom center of the sprite
    .pos.x = bound(.pos.x, -500, gen(genResolutionX) + 500)
    .pos.y = bound(.pos.y, -500, gen(genResolutionY) + 500)
   END WITH
  END IF
  IF positioning_mode = NO THEN
   '--menu mode
   IF keyval(ccCancel) > 1 THEN
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "hero_formation_editor"
   usemenu state
   slot = state.pt - first_hero_item
   IF slot < 0 THEN slot = -1

   IF enter_space_click(state) THEN
    IF state.pt = 0 THEN
     EXIT DO
    END IF
    IF slot <> -1 THEN 'a hero slot
     positioning_mode = YES
    END IF
   END IF
   IF slot <> -1 THEN
    IF keyval(scCtrl) > 0 ANDALSO keyval(scD) > 1 THEN
     'Revert to default
     hform.slots(slot).pos = default_hform.slots(slot).pos
     hero_placeholder_sprites(slot) = -1
    END IF
   END IF
   IF state.pt = 2 THEN
    IF intgrabber(test_form_id, 0, gen(genMaxFormation)) THEN
     'Test with a different enemy formation
     LoadFormation eform, test_form_id
     load_formation_slices ename(), eform, @rootslice
     bgwait = 0
     bgctr = 0
    END IF
   END IF
   IF state.pt = 1 THEN '---SELECT A DIFFERENT HERO FORMATION
    DIM as integer remember_id = hero_form_id
    IF intgrabber_with_addset(hero_form_id, 0, last_hero_formation_id(), 32767, "hero formation") THEN
     save_hero_formation hform, remember_id
     IF hero_form_id > last_hero_formation_id() THEN hero_formation_add_new hero_form_id
     load_hero_formation hform, hero_form_id
     save_hero_formation hform, hero_form_id  'Only needed when adding new
    END IF
   END IF
   IF slot <> -1 THEN
    IF intgrabber(hero_placeholder_sprites(slot), -1, gen(genMaxHeroPic)) THEN
     load_formation_slices ename(), eform, @rootslice
    END IF
   END IF

  END IF '--end positioning_mode=NO

  ' Draw screen

  IF eform.background_frames > 1 AND eform.background_ticks > 0 THEN
   bgwait = (bgwait + 1) MOD eform.background_ticks   'FIXME: off-by-one bug here
   IF bgwait = 0 THEN
    loopvar bgctr, 0, eform.background_frames - 1
    DIM sl as Slice ptr = LookupSlice(SL_FORMEDITOR_BACKDROP, rootslice)
    ChangeSpriteSlice sl, , (eform.background + bgctr) MOD gen(genNumBackdrops)
   END IF
  END IF
  draw_formation_slices eform, hform, rootslice, slot, dpage, YES

  IF positioning_mode THEN
   edgeprint "Arrow keys or mouse-drag", 0, 0, uilook(uiText), dpage
   edgeprint "ESC or right-click when done", 0, pBottom, uilook(uiText), dpage
   edgeprint "x=" & hform.slots(slot).pos.x & " y=" & hform.slots(slot).pos.y, pRight, 0, uilook(uiMenuItem), dpage
  ELSE
   menu(0) = "Previous Menu"
   menu(1) = CHR(27) + "Hero Formation " & hero_form_id & CHR(26)
   menu(2) = "Preview Enemy Formation: " & test_form_id
   FOR i as integer = 0 TO 3
    DIM placeholder as string
    placeholder = IIF(hero_placeholder_sprites(i) = -1, "Rect", "Sprite " & hero_placeholder_sprites(i))
    menu(first_hero_item + i) = "Hero Slot " & i & "  " & CHR(27) & "Preview:" & placeholder & CHR(26)
   NEXT i
   standardmenu menu(), state, 0, 0, dpage, menuopts
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 save_hero_formation hform, hero_form_id
 DeleteSlice @rootslice
END SUB

'==========================================================================================
'                                 Individual Formation editor
'==========================================================================================

'Prompt user how to add a new formation, or cancel, and update form_id
SUB formation_add_new(byref form_id as integer)
 IF form_id <= gen(genMaxFormation) THEN showbug "Bad formation_add_new call"

 DIM how as integer
 DIM previewer as FormationPreviewer
 previewer.heromode = NO
 how = generic_add_new("formation", gen(genMaxFormation), @describe_formation_by_id, @previewer, "add_formation_how")
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy

 DIM form as Formation
 IF how = -1 THEN
  gen(genMaxFormation) += 1
  ClearFormation form
  SaveFormation form, form_id
 ELSEIF how >= 0 THEN
  gen(genMaxFormation) += 1
  LoadFormation form, how
  SaveFormation form, form_id
 END IF
 form_id = gen(genMaxFormation)
END SUB

'form_id: which formation to show. If -1, same as last time. If >= max, adds a new formation (without asking!)
'Returns the formation number we were last editing.
FUNCTION individual_formation_editor (form_id as integer = -1) as integer

 DIM form as Formation

 STATIC remember_form_id as integer = 0
 IF form_id < 0 THEN
  form_id = remember_form_id
 ELSEIF form_id > gen(genMaxFormation) THEN
  formation_add_new form_id
  LoadFormation form, form_id
 END IF

 DIM ename(7) as string
 DIM rootslice as Slice ptr
 DIM positioning_mode as bool = NO
 DIM as integer bgwait, bgctr

 LoadFormation form, form_id
 load_formation_slices ename(), form, @rootslice
 IF form.music >= 0 THEN playsongnum form.music
 DIM last_music as integer = form.music

 DIM menu(16) as string
 DIM state as MenuState
 state.pt = 0
 state.top = 0
 state.first = 0
 state.last = UBOUND(menu)
 state.size = 20
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 
 CONST first_enemy_item = 9
 'slot -1 indicates no enemy selected
 DIM slot as integer = state.pt - first_enemy_item
 IF slot < 0 THEN slot = -1

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scF6) > 1 THEN slice_editor rootslice, SL_COLLECT_EDITOR
  IF positioning_mode = YES THEN
   '--enemy positioning mode
   IF keyval(ccCancel) > 1 OR enter_or_space() THEN setkeys: positioning_mode = NO
   IF readmouse.release AND mouseRight THEN setkeys: positioning_mode = NO
   IF keyval(scF1) > 1 THEN show_help "formation_editor_placement"
   DIM as integer movespeed = 1
   IF keyval(scShift) THEN movespeed = 8
   WITH form.slots(slot)
    DIM sprite as Slice ptr = LookupSlice(SL_FORMEDITOR_ENEMY + slot, rootslice)
    DIM size as XYPair
    IF sprite THEN size = sprite->Size
    ' Note that enemy positions are the top-left corner of the sprite
    ' (which needs to be changed)
    IF keyval(ccUp) > 0 THEN .pos.y -= movespeed
    IF keyval(ccDown) > 0 THEN .pos.y += movespeed
    IF keyval(ccLeft) > 0 THEN .pos.x -= movespeed
    IF keyval(ccRight) > 0 THEN .pos.x += movespeed
    IF readmouse.dragging AND mouseLeft THEN
     .pos += (readmouse.pos - readmouse.lastpos)
    END IF
    ' FIXME: battles are still stuck at 320x200 for the moment, but switch to this later
    ' .pos.x = bound(.pos.x, -size.w\2, gen(genResolutionX) - size.w\2)
    ' .pos.y = bound(.pos.y, -size.h\2, gen(genResolutionY) - size.h\2)
    .pos.x = bound(.pos.x, -size.w\2, 320 - size.w\2)
    .pos.y = bound(.pos.y, -size.h\2, 200 - size.h\2)
   END WITH
  END IF
  IF positioning_mode = NO THEN
   '--menu mode
   IF keyval(ccCancel) > 1 THEN
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "formation_editor"
   IF cropafter_keycombo(state.pt = 1) THEN cropafter form_id, gen(genMaxFormation), 0, game + ".for", 80
   usemenu state
   slot = state.pt - first_enemy_item
   IF slot < 0 THEN slot = -1

   IF enter_space_click(state) THEN
    IF state.pt = 0 THEN
     EXIT DO
    END IF
    IF state.pt = 2 THEN
     DIM backdropb as BackdropSpriteBrowser
     form.background = backdropb.browse(form.background)
     bgwait = 0
     bgctr = 0
     load_formation_slices ename(), form, @rootslice
    END IF
    IF state.pt = 5 THEN
     form.music = song_picker_or_none(form.music + 1) - 1
     state.need_update = YES
    END IF
    IF slot <> -1 THEN 'an enemy
     DIM browse_for_enemy as bool = NO
     DIM in_slot as integer = form.slots(slot).id
     IF in_slot >= 0 THEN
      'This slot has an enemy already
      DIM choices(1) as string = {"Reposition enemy", "Change Which Enemy"}
      SELECT CASE multichoice("Slot " & slot, choices())
       CASE 0: positioning_mode = YES
       CASE 1: browse_for_enemy = YES
      END SELECT
     ELSE
      'Empty slot
      browse_for_enemy = YES
     END IF
     IF browse_for_enemy THEN
      DIM oldenemy as integer = in_slot
      form.slots(slot).id = enemy_picker_or_none(in_slot + 1) - 1
      IF oldenemy <> form.slots(slot).id THEN
       load_formation_slices ename(), form, @rootslice
       IF oldenemy = -1 THEN
        formation_init_added_enemy form.slots(slot)
       END IF
      END IF
     END IF
    END IF
   END IF
   IF state.pt = 2 THEN
    IF intgrabber(form.background, 0, gen(genNumBackdrops) - 1) THEN
     bgwait = 0
     bgctr = 0
     load_formation_slices ename(), form, @rootslice
    END IF
   END IF
   IF state.pt = 3 THEN
    'IF intgrabber(form.background_frames, 1, 50) THEN
    DIM temp as integer = form.background_frames - 1
    IF xintgrabber(temp, 2, 50) THEN
     IF form.background_frames = 1 THEN form.background_ticks = 8  'default to 8 ticks because 1 tick can be really painful
     form.background_frames = temp + 1
     IF bgctr >= form.background_frames THEN
      bgctr = 0
      load_formation_slices ename(), form, @rootslice
     END IF
    END IF
   END IF
   IF state.pt = 4 THEN
    IF intgrabber(form.background_ticks, 0, 1000) THEN
     bgwait = 0
    END IF
   END IF
   IF state.pt = 5 THEN
    IF intgrabber(form.music, -2, gen(genMaxSong)) THEN
     state.need_update = YES
    END IF
   END IF
   IF state.pt = 6 THEN
    tag_set_grabber(form.victory_tag, state)
   END IF
   IF state.pt = 7 THEN
    intgrabber(form.death_action, -1, 0)
   END IF
   IF state.pt = 8 THEN
    intgrabber(form.hero_form, 0, last_hero_formation_id())
   END IF
   IF state.pt = 1 THEN '---SELECT A DIFFERENT FORMATION
    DIM as integer remember_id = form_id
    IF intgrabber_with_addset(form_id, 0, gen(genMaxFormation), maxMaxFormation, "formation") THEN
     SaveFormation form, remember_id
     IF form_id > gen(genMaxFormation) THEN formation_add_new form_id
     LoadFormation form, form_id
     load_formation_slices ename(), form, @rootslice
     state.need_update = YES
     bgwait = 0
     bgctr = 0
    END IF
   END IF'--DONE SELECTING DIFFERENT FORMATION
   IF slot <> -1 THEN
    WITH form.slots(slot)
     DIM oldenemy as integer = .id
     IF form.slots(slot).id >= 0 AND enter_space_click(state) THEN
      'Pressing enter should go to placement mode (handled above)
     ELSEIF enemygrabber(.id, state, 0, -1) THEN
      'This would treat the x/y position as being the bottom middle of enemies, which makes much more
      'sense, but that would change where enemies of different sizes are spawned in slots in existing games
      'See the Plan for battle formation improvements
      '.pos.x += w(slot) \ 2
      '.pos.y += h(slot)
      load_formation_slices ename(), form, @rootslice
      formation_init_added_enemy form.slots(slot)
     END IF
    END WITH
   END IF
  END IF
  
  IF state.need_update THEN
   IF form.music >= 0 THEN
    IF form.music <> last_music THEN
     playsongnum form.music
    END IF
   ELSE
    music_stop
   END IF
   last_music = form.music
   state.need_update = NO
  END IF

  ' Draw screen

  IF form.background_frames > 1 AND form.background_ticks > 0 THEN
   bgwait = (bgwait + 1) MOD form.background_ticks
   IF bgwait = 0 THEN
    loopvar bgctr, 0, form.background_frames - 1
    DIM sl as Slice ptr = LookupSlice(SL_FORMEDITOR_BACKDROP, rootslice)
    ChangeSpriteSlice sl, , (form.background + bgctr) MOD gen(genNumBackdrops)
   END IF
  END IF
  draw_formation_slices form, rootslice, slot, dpage

  IF positioning_mode THEN
   edgeprint "Arrow keys or mouse-drag", 0, 0, uilook(uiText), dpage
   edgeprint "ESC or right-click when done", 0, pBottom, uilook(uiText), dpage
   edgeprint "x=" & form.slots(slot).pos.x & " y=" & form.slots(slot).pos.y, pRight, 0, uilook(uiMenuItem), dpage
  ELSE
   menu(0) = "Previous Menu"
   menu(1) = CHR(27) + "Formation " & form_id & CHR(26)
   menu(2) = "Backdrop: " & form.background
   IF form.background_frames <= 1 THEN
    menu(3) = "Backdrop Animation: none"
    menu(4) = " Ticks per Backdrop Frame: -NA-"
   ELSE
    menu(3) = "Backdrop Animation: " & form.background_frames & " frames"
    menu(4) = " Ticks per Backdrop Frame: " & form.background_ticks
   END IF
   menu(5) = "Battle Music:"
   IF form.music = -2 THEN
     menu(5) &= " -same music as map-"
   ELSEIF form.music = -1 THEN
     menu(5) &= " -silence-"
   ELSEIF form.music >= 0 THEN
     menu(5) &= " " & form.music & " " & getsongname(form.music)
   END IF
   menu(6) = "Victory Tag: " & tag_choice_caption(form.victory_tag)
   menu(7) = "On Death: "
   IF form.death_action = 0 THEN
    menu(7) &= "gameover/death script"
   ELSEIF form.death_action = -1 THEN
    menu(7) &= "continue game"
   END IF
   menu(8) = "Hero Formation: " & form.hero_form

   FOR i as integer = 0 TO 7
    menu(first_enemy_item + i) = "Enemy:" + ename(i)
   NEXT i
   standardmenu menu(), state, 0, 0, dpage, menuopts
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 SaveFormation form, form_id
 music_stop
 DeleteSlice @rootslice
 remember_form_id = form_id
 RETURN form_id
END FUNCTION

SUB formation_init_added_enemy(byref slot as FormationSlot)
 'default to middle of field
 IF slot.pos.x = 0 AND slot.pos.y = 0 THEN
  slot.pos.x = 70
  slot.pos.y = 95
 END IF
END SUB

'==========================================================================================
'                             Shared formation display code
'==========================================================================================

'Deletes previous rootslice if any, then creates a bunch of sprite slices for enemies
'and rectangles for hero positions, but doesn't position them: that's done in
'draw_formation_slices.
'Also loads enemy names into ename().
SUB load_formation_slices(ename() as string, form as Formation, rootslice as Slice ptr ptr)
 DIM sl as Slice ptr
 DeleteSlice rootslice

 ' Root is backdrop
 IF form.background < 0 THEN
  'Used by FormationPreviewer when previewing a hero formation: show a backdrop
  sl = NewSliceOfType(slRectangle)
  sl->Size = XY(320, 200)  'TODO: update when battle resolution can be increased
  ChangeRectangleSlice sl, 0, , , borderLine, transOpaque
 ELSE
  sl = NewSliceOfType(slSprite)
  ChangeSpriteSlice sl, sprTypeBackdrop, form.background
 END IF
 sl->Lookup = SL_FORMEDITOR_BACKDROP
 'sl->AutoSort = slAutoSortBottomY
 sl->AutoSort = slAutoSortCustom
 RealignSlice sl, alignRight, alignBottom, alignRight, alignBottom
 sl->ClampHoriz = alignLeft
 sl->ClampVert = alignTop

 *rootslice = sl

 ' Heroes
 FOR i as integer = 0 TO 3
  IF hero_placeholder_sprites(i) = -1 THEN
   ' Use a rectangle
   sl = NewSliceOfType(slRectangle, *rootslice)
   ChangeRectangleSlice sl, , boxlook(0).bgcol, boxlook(0).edgecol, , transFuzzy, 75
   sl->Width = 32
   sl->Height = 40
  ELSE
   ' Use a hero sprite
   sl = NewSliceOfType(slSprite, *rootslice)
   ChangeSpriteSlice sl, sprTypeHero, bound(hero_placeholder_sprites(i), 0, gen(genMaxHeroPic))
  END IF
  sl->Lookup = SL_FORMEDITOR_HERO + i
  sl->AnchorHoriz = alignCenter
  sl->AnchorVert = alignBottom

  ' Add the party slot number to the center
  DIM num_sl as Slice ptr
  num_sl = NewSliceOfType(slText, sl)
  num_sl->Pos = XY(2,2)
  ChangeTextSlice num_sl, STR(i), , YES
 NEXT

 ' Enemies
 FOR i as integer = 0 TO 7
  ename(i) = "-EMPTY-"
  IF form.slots(i).id >= 0 THEN
   DIM enemy as EnemyDef
   loadenemydata enemy, form.slots(i).id
   WITH enemy
    ename(i) = form.slots(i).id & ":" & .name
    sl = NewSliceOfType(slSprite, *rootslice)
    ChangeSpriteSlice sl, sprTypeSmallEnemy + bound(.size, 0, 2), .pic, .pal
    sl->Lookup = SL_FORMEDITOR_ENEMY + i
   END WITH
  END IF
 NEXT i

 ' Cursor (defaults to invisible)
 sl = NewSliceOfType(slText, *rootslice)
 sl->AlignHoriz = alignCenter
 sl->AnchorHoriz = alignCenter
 sl->Lookup = SL_FORMEDITOR_CURSOR
 ChangeTextSlice sl, CHR(25), -1 - uiSelectedItem2, YES
END SUB

SUB draw_formation_slices(eform as Formation, rootslice as Slice ptr, selected_slot as integer, page as integer)
 DIM hform as HeroFormation
 load_hero_formation hform, eform.hero_form
 draw_formation_slices eform, hform, rootslice, selected_slot, page, NO
END SUB

SUB draw_formation_slices(eform as Formation, hform as HeroFormation, rootslice as Slice ptr, selected_slot as integer, page as integer, byval heromode as bool=NO)
 DIM cursorsl as Slice ptr = LookupSlice(SL_FORMEDITOR_CURSOR, rootslice)
 cursorsl->Visible = NO

 ' Set enemy positions (and maybe parent of cursor slice)
 DIM sl as Slice ptr = rootslice->FirstChild
 WHILE sl
  IF sl->Lookup >= SL_FORMEDITOR_ENEMY AND sl->Lookup <= SL_FORMEDITOR_LAST_ENEMY THEN
   'Is an enemy
   DIM enemy_slot as integer = sl->Lookup - SL_FORMEDITOR_ENEMY
   DIM fslot as FormationSlot ptr = @eform.slots(enemy_slot)
   IF fslot->id < 0 THEN showbug "Formation enemy slice corresponds to an empty slot"
   sl->Pos = fslot->pos
   ' Set layering, like slAutoSortBottomY but break ties according to the order in bslot()
   ' (Enemy slices are anchored by the top-left edge)
   sl->Sorter = (sl->Y + sl->Height) * 1000 + 100 + enemy_slot
   IF NOT heromode THEN
    IF enemy_slot = selected_slot AND cursorsl <> NULL THEN
     cursorsl->Visible = YES
     SetSliceParent cursorsl, sl
    END IF
   END IF
  END IF
  sl = sl->NextSibling
 WEND

 ' Set hero positions (and maybe parent of cursor slice)
 DIM hrect as Slice Ptr
 FOR i as integer = 0 TO 3
  hrect = LookupSlice(SL_FORMEDITOR_HERO + i, rootslice)
  hrect->Pos = hform.slots(i).pos + XY(240, 82)
  ' Set layering, like slAutoSortBottomY but break ties according to the order in bslot()
  ' (Hero slices are anchored by the bottom-center edge)
  hrect->Sorter = hrect->Y * 1000 + i
  IF heromode THEN
   IF i = selected_slot AND cursorsl <> NULL THEN
    cursorsl->Visible = YES
    SetSliceParent cursorsl, hrect
   END IF
  END IF
 NEXT i

 clearpage page
 DrawSlice rootslice, page
END SUB
