'OHRRPGCE CUSTOM - Enemy/Hero Formation/Formation Set Editors
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

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
DECLARE SUB update_formation_background(form as Formation, rootslice as Slice ptr, byref bgctr as integer, byref bgwait as integer)
DECLARE SUB load_formation_slices(ename() as string, form as Formation, rootslice as Slice ptr ptr)
DECLARE SUB hero_formation_editor ()
DECLARE SUB formation_init_added_enemy(byref slot as FormationSlot)

DECLARE SUB formation_set_editor_load_preview(state as MenuState, form_id as integer, formset as FormationSet, form as Formation, ename() as string, byref rootslice as Slice Ptr)

' Formation editor slice lookup codes
CONST SL_FORMEDITOR_BACKDROP = 100
CONST SL_FORMEDITOR_BATTLEFIELD = 101
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
   '-1 is None
   state.need_update OR= formationgrabber(formset.formations(state.pt - 4), state, 0, -1)
   form_id = formset.formations(state.pt - 4)
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
  IF state.pt >= 4 THEN
   edgeprint THINGGRABBER_TOOLTIP, 0, pBottom, uilook(uiMenuItem), dpage
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

'Prompt user how to add a new formation and update hero_form_id and return true,
'or return false if cancelled.
FUNCTION hero_formation_add_new(byref hero_form_id as integer) as bool
 BUG_IF(hero_form_id <> last_hero_formation_id() + 1, "Bad call", NO)

 DIM how as integer
 DIM previewer as FormationPreviewer
 previewer.heromode = YES
 how = generic_add_new("hero formation", last_hero_formation_id(), , @previewer, "add_hero_formation_how")
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy

 DIM hform as HeroFormation
 IF how = -2 THEN
  RETURN NO
 ELSEIF how = -1 THEN
  default_hero_formation hform
  save_hero_formation hform, hero_form_id
 ELSEIF how >= 0 THEN
  load_hero_formation hform, how
  save_hero_formation hform, hero_form_id
 ELSE
  hero_form_id = last_hero_formation_id()
 END IF
 RETURN YES
END FUNCTION

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
    'Hero positions are the bottom center of the sprite. Generous bounds.
    DIM bounds as RectPoints = get_formation_bounds()
    .pos = bound(.pos, bounds.topleft - 100, bounds.bottomright + 100)
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
  update_formation_background eform, rootslice, bgctr, bgwait
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

'Prompt user how to add a new formation, and update form_id and return true,
'or return false if cancelled.
FUNCTION formation_add_new(byref form_id as integer) as bool
 BUG_IF(form_id <> gen(genMaxFormation) + 1, "Bad call", NO)

 DIM how as integer
 DIM previewer as FormationPreviewer
 previewer.heromode = NO
 how = generic_add_new("formation", gen(genMaxFormation), @describe_formation_by_id, @previewer, "add_formation_how")
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy

 DIM form as Formation
 IF how = -2 THEN
  RETURN NO
 ELSEIF how = -1 THEN
  gen(genMaxFormation) += 1
  ClearFormation form
  SaveFormation form, form_id
 ELSEIF how >= 0 THEN
  gen(genMaxFormation) += 1
  LoadFormation form, how
  SaveFormation form, form_id
 END IF
 form_id = gen(genMaxFormation)
 RETURN YES
END FUNCTION

TYPE FormationEditor EXTENDS ModularMenu
 form_id as integer
 form as Formation
 slot as integer = -1      'Which formation slot selected, or -1 for none
 positioning_mode as bool  'Whether positioning an enemy (menu hidden)
 ename(7) as string        'Enemy names
 rootslice as Slice ptr
 bgwait as integer
 bgctr as integer
 remem_pt as integer       'Remember state.pt of top menu while in positioning_mode

 preview_music as bool
 last_music as integer = -1

 DECLARE VIRTUAL FUNCTION each_tick() as bool
 DECLARE VIRTUAL FUNCTION try_exit() as bool
 DECLARE VIRTUAL SUB update()
 DECLARE VIRTUAL SUB draw_underlays()

 DECLARE SUB load_form()
 DECLARE SUB update_music()

 DECLARE FUNCTION each_tick_menu_mode() as bool
 DECLARE SUB each_tick_positioning_mode()
END TYPE

'form_id: which formation to show. If -1, same as last time. If >= max, asks to add a new formation
'Returns the formation number we were last editing, or -1 if cancelled adding a new formation.
FUNCTION individual_formation_editor (form_id as integer = -1) as integer
 STATIC remember_form_id as integer = 0
 IF form_id < 0 THEN
  form_id = remember_form_id
 ELSEIF form_id > gen(genMaxFormation) THEN
  IF formation_add_new(form_id) = NO THEN RETURN -1
 END IF

 DIM editor as FormationEditor
 editor.menuopts.edged = YES
 editor.preview_music = read_config_bool("formedit.preview_music", NO)

 editor.form_id = form_id
 editor.load_form()
 editor.run()
 form_id = editor.form_id

 SaveFormation editor.form, form_id
 music_stop
 DeleteSlice @editor.rootslice
 remember_form_id = form_id
 RETURN form_id
END FUNCTION

SUB FormationEditor.load_form()
 LoadFormation form, form_id
 load_formation_slices ename(), form, @rootslice
 bgwait = 0
 bgctr = 0
 update_music()
END SUB

FUNCTION FormationEditor.try_exit() as bool
 IF positioning_mode THEN
  positioning_mode = NO
  state.pt = remem_pt
  state.need_update = YES
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION FormationEditor.each_tick() as bool
 IF keyval(scF6) > 1 THEN slice_editor rootslice, SL_COLLECT_EDITOR

 IF positioning_mode THEN
  each_tick_positioning_mode()
 ELSE
  IF each_tick_menu_mode() THEN RETURN YES
 END IF

 IF positioning_mode THEN
  helpkey = "formation_editor_placement"
  clear_menu()  'Hide the menu
 ELSE
  helpkey = "formation_editor"
  'Rebuild the menu unconditionally, because each_tick_menu_mode doesn't set need_update
  state.need_update = YES
 END IF
END FUNCTION

SUB FormationEditor.each_tick_positioning_mode()
 'ccCancel is handled by try_exit()
 IF enter_or_space() ORELSE (readmouse.release AND mouseRight) THEN
  positioning_mode = NO
  state.pt = remem_pt
  RETURN
 END IF
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

  ' Allow placing an enemy anywhere onscreen (or just off), not just inside the 'battlefield'
  DIM bounds as RectPoints = get_formation_bounds()
  .pos = bound(.pos, bounds.topleft - size, bounds.bottomright)
 END WITH
END SUB

FUNCTION FormationEditor.each_tick_menu_mode() as bool
 IF state.empty() THEN RETURN NO
 DIM itemid as integer = itemtypes(state.pt)
 slot = -1

 DIM activate as bool = enter_space_click(state)
 'NOTE: we don't set state.need_update (TODO), instead update() called unconditionally.

 IF cropafter_keycombo(itemid = 1) THEN cropafter form_id, gen(genMaxFormation), game + ".for", 80

 SELECT CASE itemid
  CASE 0  'Previous menu
   IF activate THEN RETURN YES
  CASE 1  'Select a different formation
   DIM as integer remember_id = form_id
   IF intgrabber_with_addset(form_id, 0, gen(genMaxFormation), maxMaxFormation, "formation") THEN
    SaveFormation form, remember_id
    IF form_id > gen(genMaxFormation) THEN formation_add_new form_id
    load_form()
   END IF
  CASE 2  'Backdrop
   IF intgrabber(form.background, 0, gen(genNumBackdrops) - 1) THEN
    bgwait = 0
    bgctr = 0
    load_formation_slices ename(), form, @rootslice
   END IF
   IF activate THEN
    DIM backdropb as BackdropSpriteBrowser
    form.background = backdropb.browse(form.background)
    bgwait = 0
    bgctr = 0
    load_formation_slices ename(), form, @rootslice
   END IF
  CASE 3  'Backdrop animation frames
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
  CASE 4  'Backdrop animation ticks
   IF intgrabber(form.background_ticks, 0, 1000) THEN
    bgwait = 0
   END IF
  CASE 5  'Battle music
   IF intgrabber(form.music, -2, gen(genMaxSong)) THEN  '-2: same as map, -1: silence
    update_music
   END IF
   IF activate THEN
    form.music = song_picker_or_none(form.music + 1) - 1
    update_music
   END IF
  CASE 6  'Victory tag
   tag_set_grabber(form.victory_tag, state)
  CASE 7  'On death
   intgrabber(form.death_action, -1, 0)
  CASE 8  'Hero formation
   intgrabber(form.hero_form, 0, last_hero_formation_id())

  CASE IS >= 100
   slot = itemid - 100

   WITH form.slots(slot)
    DIM oldenemy as integer = .id

    IF activate THEN
     'Usually enemygrabber (below) would handle Enter, but we override it here
     DIM browse_for_enemy as bool = NO
     IF .id >= 0 THEN
      'This slot has an enemy already
      DIM choices(1) as string = {"Reposition enemy", "Change Which Enemy"}
      SELECT CASE multichoice("Slot " & slot, choices())
       CASE 0: positioning_mode = YES
       CASE 1: browse_for_enemy = YES
      END SELECT
      remem_pt = state.pt
     ELSE
      'Empty slot
      browse_for_enemy = YES
     END IF
     IF browse_for_enemy THEN
      .id = enemy_picker_or_none(.id + 1) - 1
     END IF
    ELSEIF enemygrabber(.id, state, 0, -1) THEN
    END IF

    IF oldenemy <> .id THEN  'Enemy changed
     load_formation_slices ename(), form, @rootslice
     IF oldenemy = -1 THEN
      formation_init_added_enemy form.slots(slot)
     END IF
     'This would treat the x/y position as being the bottom middle of enemies, which makes much more
     'sense, but that would change where enemies of different sizes are spawned in slots in existing games
     'See the Plan for battle formation improvements
     '.pos.x += w(slot) \ 2
     '.pos.y += h(slot)
    END IF
   END WITH

 END SELECT
END FUNCTION

SUB FormationEditor.update_music()
 IF preview_music ANDALSO form.music <> last_music THEN
  IF form.music >= 0 THEN
   playsongnum form.music
  ELSE
   music_stop
  END IF
 END IF
 last_music = form.music
END SUB

SUB FormationEditor.update()
 IF positioning_mode THEN EXIT SUB

 DIM temp as string
 add_item 0, , "Previous Menu"
 add_item 1, , CHR(27) + "Formation " & form_id & CHR(26)
 add_item 2, , "Backdrop: " & form.background
 IF form.background_frames <= 1 THEN
  add_item 3, , "Backdrop Animation: none"
  add_item 4, , " Ticks per Backdrop Frame: -NA-", NO
 ELSE
  add_item 3, , "Backdrop Animation: " & form.background_frames & " frames"
  add_item 4, , " Ticks per Backdrop Frame: " & form.background_ticks
 END IF
 IF form.music = -2 THEN
  temp = "-same music as map-"
 ELSEIF form.music = -1 THEN
  temp = "-silence-"
 ELSEIF form.music >= 0 THEN
  temp = form.music & " " & getsongname(form.music)
 END IF
 add_item 5, , "Battle Music: " & temp
 add_item 6, , "Victory Tag: " & tag_choice_caption(form.victory_tag)

 temp = ""
 IF form.death_action = 0 THEN
  temp = "gameover/death script"
 ELSEIF form.death_action = -1 THEN
  temp = "continue game"
 END IF
 add_item 7, , "On Death: " & temp
 add_item 8, , "Hero Formation: " & form.hero_form

 FOR i as integer = 0 TO 7
  add_item 100 + i, , "Enemy: " + ename(i)
 NEXT i
END SUB

SUB FormationEditor.draw_underlays()
 update_formation_background form, rootslice, bgctr, bgwait
 draw_formation_slices form, rootslice, slot, vpage

 IF positioning_mode THEN
  edgeprint "Arrow keys or mouse-drag", 0, 0, uilook(uiText), vpage
  edgeprint "ESC or right-click when done", 0, pBottom, uilook(uiText), vpage
  edgeprint "x=" & form.slots(slot).pos.x & " y=" & form.slots(slot).pos.y, pRight, 0, uilook(uiMenuItem), vpage
 END IF
END SUB

SUB formation_init_added_enemy(byref slot as FormationSlot)
 'default to middle of field
 IF slot.pos = 0 THEN
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

 ' Root
 sl = NewSliceOfType(slContainer)
 *rootslice = sl
 sl->Size = get_battle_res()
 sl->Clip = YES  'Trim off parts of the backdrop that are too large
 'In the form editor, show the formation at the bottom-right corner of the screen
 RealignSlice sl, alignRight, alignBottom, alignRight, alignBottom
 sl->ClampHoriz = alignLeft
 sl->ClampVert = alignTop

 ' Battlefield
 DIM battlefield_sl as Slice ptr = NewSliceOfType(slContainer, sl)
 battlefield_sl->Lookup = SL_FORMEDITOR_BATTLEFIELD
 battlefield_sl->Size = get_battlefield_size()
 CenterSlice battlefield_sl

 ' Backdrop
 IF form.background < 0 THEN
  'Used by FormationPreviewer when previewing a hero formation: blank background
  sl = NewSliceOfType(slRectangle, battlefield_sl)
  ChangeRectangleSlice sl, 0, , , borderLine, transOpaque
  sl->Fill = YES
 ELSE
  sl = NewSliceOfType(slSprite, battlefield_sl)
  ChangeSpriteSlice sl, sprTypeBackdrop, form.background
 END IF
 sl->Lookup = SL_FORMEDITOR_BACKDROP
 'sl->AutoSort = slAutoSortBottomY
 sl->AutoSort = slAutoSortCustom
 CenterSlice sl

 ' Heroes
 FOR i as integer = 0 TO 3
  IF hero_placeholder_sprites(i) = -1 THEN
   ' Use a rectangle
   sl = NewSliceOfType(slRectangle, battlefield_sl)
   ChangeRectangleSlice sl, , boxlook(0).bgcol, boxlook(0).edgecol, , transFuzzy, 75
   sl->Width = 32
   sl->Height = 40
  ELSE
   ' Use a hero sprite
   sl = NewSliceOfType(slSprite, battlefield_sl)
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
    sl = NewSliceOfType(slSprite, battlefield_sl)
    ChangeSpriteSlice sl, sprTypeSmallEnemy + bound(.size, 0, 2), .pic, .pal
    sl->Lookup = SL_FORMEDITOR_ENEMY + i
   END WITH
  END IF
 NEXT i

 ' Cursor (defaults to invisible)
 sl = NewSliceOfType(slText, battlefield_sl)
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
 DIM sl as Slice ptr = LookupSliceSafe(SL_FORMEDITOR_BATTLEFIELD, rootslice)->FirstChild
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

SUB update_formation_background(form as Formation, rootslice as Slice ptr, byref bgctr as integer, byref bgwait as integer)
 IF form.background_frames > 1 AND form.background_ticks > 0 THEN
  bgwait = (bgwait + 1) MOD form.background_ticks   'FIXME: off-by-one bug here
  IF bgwait = 0 THEN
   loopvar bgctr, 0, form.background_frames - 1
   DIM sl as Slice ptr = LookupSlice(SL_FORMEDITOR_BACKDROP, rootslice)
   ChangeSpriteSlice sl, , (form.background + bgctr) MOD gen(genNumBackdrops)
  END IF
 END IF
END SUB
