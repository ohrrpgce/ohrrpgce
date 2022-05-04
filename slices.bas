'OHRRPGCE - Slices
'(C) Copyright 1997-2022 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.


'  ==== Explanation of how slice drawing works ====
'DrawSlice simply calls DrawSliceRecurse, which is the central drawing function.
'
'DrawSliceRecurse(sl):
'
'  attach->ChildRefresh(attach, sl)
'   (attach == parent of sl, except for the root slice, then attach == ScreenSlice.
'    sl->Attached is currently unused.)
'   Recomputes sl->ScreenPos from sl->Pos.
'   (Can also adjust sl->Size (if sl->Fill) and sl->Visible, see ChildRefresh details below.)
'
'  If sl->Visible:
'
'    If sl->CoverChildren: UpdateCoverSize(sl)
'
'    If sl->Context: push onto context_stack
'
'    sl->Draw(): (eg DrawSpriteSlice)  [if present]
'     -Type-specific drawing of a single slice
'     (If something has to be drawn over the children instead, done in sl->ChildDraw)
'
'    AutoSortChildren(sl)
'
'    sl->ChildDraw() (eg GridChildDraw, DefaultChildDraw):
'      (Called once to draw all children.)
'      sl->ChildrenRefresh() [if present]
'        (Recomputes Pos, Size and ScreenPos for each child)
'      For each child:
'        Skip if a template slice (and template_slices_shown=NO)
'        shrinkclip to child's support (size minus padding); usually done once for all children
'        If not clipped off:
'          DrawSliceRecurse(child, page, childindex)
'            ...draw the child recursively...
'      Restore cliprect
'      Maybe draw something over all children (Scroll)
'
'    Pop context_stack
'
'
'So unrolling that, each slice `sl` is processed in this order:
'Before DrawSlice called:
' -[NPC, hero, etc, slices] - positions & visibility updated based on in-game logic (done separately)
' -AdvanceSlice() - update position based on movement (done separately - in-game wandering on map only!)
'Before parent is drawn:
' -If parent->CoverChildren sl->Size/sl->Pos are used to update the parent's size
'After parent is drawn:
' -AutoSortChildren() modifies order amongst siblings
' -parent->ChildrenRefresh() updates sl->Pos/sl->Size
' -parent->ChildRefresh() & ChildRefresh() update sl->ScreenPos/sl->Size/sl->Visible
' If sl->Visible:
'   -If sl->CoverChildren, update sl->Size
'   -push sl->Context onto context_stack
'   -sl->Draw()
'   -children autosorted
'   -sl->ChildDraw():
'     -sl->ChildrenRefresh()
'     -For each child:
'       -Skip if a template slice (and template_slices_shown=NO)
'       -shrinkclip to child's support (size minus padding); usually done once for all children
'       -If not clipped off:
'         -DrawSliceRecurse(child)
'           -sl->ChildRefresh(child)
'     -draw scrollbars/etc over children
'   -pop context_stack
'
'[[FIXME: the above reveals several problems
' -parent->CoverChildren happens before most updates to children
' -CoverChildren updates Size after Size has already been used to update ScreenPos
' -it's really hard to see the order things happen from the code
']]
'
'  ==== Slice refreshing details ====
'
'sl->ChildRefresh (called by the slice refresh functions like RefreshSliceScreenPos,
'and called directly or indirectly in lots of places) is responsible for translating a slice's .Pos
'to its .ScreenPos (including effects of clamping), and also applying .Fill (which
'modifies .Size) -- this is probably in the wrong place!
'As an exception, SelectChildRefresh also modifies .Visible. (Although this leads to unnecessary
'refreshes there doesn't seem to a better place for it. See r7792.)
'All sl->ChildRefresh methods calculate the 'support' rect for a child then call ChildRefresh()
'to do the actual update. Grid and Panel slices have non-standard supports (each child
'gets its own instead of all sharing the same one).
'As an exception, if sl->ChildrenRefresh is defined, sl->ChildRefresh isn't (this is probably
'a mistake, and may be changed.)
'
'sl->ChildrenRefresh is used for recomputing child X,Y positions if they are determined
'by the parent slice. Currently only LayoutChildrenRefresh exists. There are no
'Panel/GridChildrenRefresh subs because X,Y positions of children of those aren't relative
'to a certain point.
'sl->ChildrenRefresh is called by slice refresh functions but not by collision detection
'functions!


#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "reloadext.bi"
#include "loading.bi"
#include "slices.bi"
#include "sliceedit.bi"
#include "surface.bi"

#ifdef IS_GAME
 'For plotslices(), next_slice_table_slot, num_reusable_slice_table_slots
 #include "gglobals.bi"
 'For restore_saved_plotslice_handle
 #include "scriptcommands.bi"
#endif

'==============================================================================


'Reload helper functions used by saving/loading
DECLARE Sub SaveProp OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, byval value as integer)
DECLARE Sub SaveProp OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, byval value as double)
DECLARE Sub SaveProp OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, s as string)

DECLARE Sub SavePropAlways OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, byval value as integer)
DECLARE Sub SavePropAlways OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, byval value as double)
DECLARE Sub SavePropAlways OVERLOAD (node as Reload.Nodeptr, propname as zstring ptr, s as string)

EXTERN "C"

DECLARE Function LoadPropStr(node as Reload.Nodeptr, propname as zstring ptr, defaultval as string="") as string
DECLARE Function LoadProp(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as integer=0) as integer
DECLARE Function LoadPropBool(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as bool=NO) as bool
DECLARE Function LoadPropFloat(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as double=0.0) as double

DECLARE Sub SaveDrawOpts(drawopts as DrawOptions, node as Reload.Nodeptr)
DECLARE Sub LoadDrawOpts(drawopts as DrawOptions, node as Reload.Nodeptr)

'Other local subs and functions
DECLARE Sub DrawSliceRecurse(byval s as Slice ptr, byval page as integer, childindex as integer = -1)
DECLARE Function SliceXAlign(sl as Slice Ptr, supportw as integer) as integer
DECLARE Function SliceYAlign(sl as Slice Ptr, supporth as integer) as integer
DECLARE Sub RefreshChild(ch as Slice ptr, support as RectType)
DECLARE Sub RefreshChildClamp(ch as Slice ptr, support as RectType)
DECLARE Sub ApplySliceVelocity(byval s as Slice ptr)
DECLARE Sub SeekSliceTarg(byval s as Slice ptr)

DECLARE Sub report_slice_type_err(sl as Slice ptr, expected as SliceTypes)

#macro ASSERT_SLTYPE(sl, expected, retwhat...)
	if sl->SliceType <> expected then
		report_slice_type_err sl, expected
		return retwhat  'If retwhat isn't given, just "return"
	end if
#endmacro

END EXTERN

'==============================================================================

ReDim Shared SliceDebug(50) as Slice Ptr

'Number of non-trivial drawn slices (Container, Special, Root and invisible excluded)
DIM NumDrawnSlices as integer

'Whether template slices should be reveal - used in the slice editor
DIM template_slices_shown as bool

'ScreenSlice is used by other slices with ->Attach = slScreen
DIM SHARED ScreenSlice as Slice Ptr
ScreenSlice = NewSlice()
#ifdef ENABLE_SLICE_DEBUG
 SliceDebugForget ScreenSlice '--screen slice is magical, ignore it for debugging purposes
#endif
WITH *ScreenSlice
 'Note that .Attach is NOT set to slScreen here. slScreen uses this, not the other way around
 .SliceType = slSpecial
 .X = 0
 .Y = 0
 .ScreenX = 0
 .ScreenY = 0
 .Width = get_resolution().w
 .Height = get_resolution().h
END WITH

DEFINE_VECTOR_OF_TYPE(Slice ptr, Slice_ptr)
DEFINE_VECTOR_OF_TYPE(SliceContext ptr, SliceContext_ptr)

'Built up while inside DrawSlice, otherwise NULL.
'A stack of all the non-NULL .Context ptrs for all the ancestors of the current slice.
Dim Shared context_stack as SliceContext ptr vector

EXTERN "C"


'==General slice code==========================================================

Function Slice.IsShown() as bool
 return this.Visible andalso (this.Template = NO orelse template_slices_shown)
End Function

'Whether this slice should be skipped for certain purposes (e.g. slice collisions, laying out or
'covering children) because it's not visible (if visibleonly=YES), or it's a template.
Private Function ShouldSkipSlice(sl as Slice ptr, visibleonly as bool = NO) as bool
 if sl->Template andalso template_slices_shown = NO then return YES
 return visibleonly andalso sl->Visible = NO
end function

'Stub functions.
'These Null functions are used by Container, Root, and Special slices.
Sub DisposeNullSlice(byval s as Slice ptr) : end sub
Sub CloneNullSlice(byval s as Slice ptr, byval cl as Slice ptr) : end sub
Sub SaveNullSlice(byval s as Slice ptr, byval node as Reload.Nodeptr) : end sub
Sub LoadNullSlice(byval s as Slice ptr, byval node as Reload.Nodeptr) : end sub
'And this is used if ChildrenRefresh is provided instead
Sub NullChildRefresh(byval par as Slice ptr, byval ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES) : end sub

'Computes ScreenX/Y, and also sets the width/height if filling (which is basically an implementation mistake).
'childindex is index of ch among its siblings, ignoring templates. Pass -1 if not known,
'which saves computing it if it's not needed. (Not used by DefaultChildRefresh)
Sub DefaultChildRefresh(byval par as Slice ptr, byval ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 if ch = 0 then debug "DefaultChildRefresh null ptr": exit sub
 if visibleonly andalso ch->Visible = NO then exit sub  'Don't need to exclude template slices
 dim support as RectType = any
 support.xy = par->ScreenPos + XY(par->paddingLeft, par->paddingTop)
 support.wide = par->Width - par->paddingLeft - par->paddingRight
 support.high = par->Height - par->paddingTop - par->paddingBottom
 RefreshChild ch, support
End Sub

'Support is the box (in screen coordinates) which the child is aligned relative to,
'and which it would fill if ch->Fill is true.
'By default it is the size of the parent minus padding.
Sub RefreshChild(ch as Slice ptr, support as RectType)
 with *ch
  .ScreenX = .X + support.x + SliceXAlign(ch, support.wide) - SliceXAnchor(ch)
  .ScreenY = .Y + support.y + SliceYAlign(ch, support.high) - SliceYAnchor(ch)
  if .ClampToScreen then
   dim scr_rect as RectType = any
   dim root_sl as slice Ptr = FindRootSlice(ch)
   scr_rect.xy = root_sl->ScreenPos + XY(root_sl->paddingLeft, root_sl->paddingTop)
   scr_rect.wide = root_sl->Width - root_sl->paddingLeft - root_sl->paddingRight
   scr_rect.high = root_sl->Height - root_sl->paddingTop - root_sl->paddingBottom
   RefreshChildClamp ch, scr_rect
  else
   'Clamp to parent
   RefreshChildClamp ch, support
  end if
  if .Fill then
   if .FillMode = sliceFillFull ORELSE .FillMode = sliceFillHoriz then
    .ScreenX = support.x
    .Width = support.wide
   end if
   if .FillMode = sliceFillFull ORELSE .FillMode = sliceFillVert then
    .ScreenY = support.y
    .Height = support.high
   end if
  end if
 end with
End sub

Sub RefreshChildClamp(ch as Slice ptr, support as RectType)
 with *ch
  select case .ClampHoriz
   case alignLeft:   .ScreenX = large(.ScreenX, support.x)
   case alignRight:  .ScreenX = small(.ScreenX, support.x + support.wide - .Width)
   case alignBoth:
    if .Width > support.wide then
     select case .AnchorHoriz
      case alignLeft:
       .ScreenX = small(.ScreenX, support.x + support.wide - .Width)
       .ScreenX = large(.ScreenX, support.x)
      case alignRight:
       .ScreenX = large(.ScreenX, support.x)
       .ScreenX = small(.ScreenX, support.x + support.wide - .Width)
      case alignCenter:
       .ScreenX = support.x + (support.wide - .Width) \ 2
     end select
    else
     .ScreenX = large(.ScreenX, support.x)
     .ScreenX = small(.ScreenX, support.x + support.wide - .Width)
    end if
  end select
  select case .ClampVert
   case alignTop:    .ScreenY = large(.ScreenY, support.y)
   case alignBottom: .ScreenY = small(.ScreenY, support.y + support.high - .Height)
   case alignBoth:
    if .Height > support.high then
     select case .AnchorVert
      case alignTop:
       .ScreenY = small(.ScreenY, support.y + support.high - .Height)
       .ScreenY = large(.ScreenY, support.y)
      case alignBottom:
       .ScreenY = large(.ScreenY, support.y)
       .ScreenY = small(.ScreenY, support.y + support.high - .Height)
      case alignCenter:
       .ScreenY = support.y + (support.high - .Height) \ 2
     end select
    else
     .ScreenY = large(.ScreenY, support.y)
     .ScreenY = small(.ScreenY, support.y + support.high - .Height)
    end if
  end select
 end with
End Sub

Sub DefaultChildDraw(byval s as Slice Ptr, byval page as integer)
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSliceRecurse which does null check it.
 dim rememclip as ClipState = any
 with *s
  if .ChildrenRefresh then .ChildrenRefresh(s)

  if .Clip then
   rememclip = get_cliprect()
   if shrinkclip(.ScreenX + .paddingLeft, _
                 .ScreenY + .paddingTop, _
                 .ScreenX + .Width - .paddingRight - 1, _
                 .ScreenY + .Height - .paddingBottom - 1, _
                 vpages(page)) = NO then
    'All children are clipped
    get_cliprect() = rememclip
    exit sub
   end if
  end if

  'draw the slice's children
  dim ch as Slice ptr = .FirstChild
  dim childindex as integer = 0  'Index amongst non-template siblings (unless shown)
  do while ch <> 0
   if ch->Template = NO orelse template_slices_shown then  'equiv to ShouldSkipSlice(ch) = NO
    DrawSliceRecurse(ch, page, childindex)
    childindex += 1
   end if
   ch = ch->NextSibling
  loop

  if .Clip then get_cliprect() = rememclip
 end with
End sub

FUNCTION SliceTypeByName (s as string) as SliceTypes
 SELECT CASE s
  CASE "Root":           RETURN slSpecial  'Root slices removed
  CASE "Special":        RETURN slSpecial
  CASE "Container":      RETURN slContainer
  CASE "Rectangle":      RETURN slRectangle
  CASE "Line":           RETURN slLine
  CASE "Sprite":         RETURN slSprite
  CASE "Text":           RETURN slText
  CASE "Map":            RETURN slMap
  CASE "Grid":           RETURN slGrid
  CASE "Ellipse":        RETURN slEllipse
  CASE "Scroll":         RETURN slScroll
  CASE "Select":         RETURN slSelect
  CASE "Panel":          RETURN slPanel
  CASE "Layout":         RETURN slLayout
 END SELECT
 debugerror "Unrecognized slice name """ & s & """"
 RETURN slInvalid
END FUNCTION

END EXTERN

FUNCTION SliceTypeName (sl as Slice Ptr) as string
 IF sl = 0 THEN debug "SliceTypeName null ptr": RETURN "<null ptr>"
 RETURN SliceTypeName(sl->SliceType)
END FUNCTION

FUNCTION SliceTypeName (t as SliceTypes) as string
 SELECT CASE t
  CASE slSpecial:        RETURN "Special"
  CASE slContainer:      RETURN "Container"
  CASE slRectangle:      RETURN "Rectangle"
  CASE slLine:           RETURN "Line"
  CASE slSprite:         RETURN "Sprite"
  CASE slText:           RETURN "Text"
  CASE slMap:            RETURN "Map"
  CASE slGrid:           RETURN "Grid"
  CASE slEllipse:        RETURN "Ellipse"
  CASE slScroll:         RETURN "Scroll"
  CASE slSelect:         RETURN "Select"
  CASE slPanel:          RETURN "Panel"
  CASE slLayout:         RETURN "Layout"
 END SELECT
 RETURN "Unknown"
END FUNCTION

'Called from sliceedit.
FUNCTION SliceLookupCodename (code as integer, slicelookup() as string, use_default as bool = YES) as string
 IF code > 0 ANDALSO code <= UBOUND(slicelookup) ANDALSO LEN(TRIM(slicelookup(code))) THEN
  RETURN slicelookup(code)
 ELSE
  RETURN SliceLookupCodeName(code, use_default)  'Special lookup codes
 END IF
END FUNCTION

FUNCTION SliceLookupCodename (sl as Slice Ptr, use_default as bool = YES) as string
 IF sl = 0 THEN RETURN "[nullptr]"
 RETURN SliceLookupCodename(sl->Lookup, use_default)
END FUNCTION

'use_default: if the code is unnamed, return "Lookup###" instead of ""
FUNCTION SliceLookupCodename (byval code as integer, use_default as bool = YES) as string
 SELECT CASE code
  CASE 0: RETURN ""
'--the following is updated from slices.bi using the misc/sl_lookup.py script
'<SLICE LOOKUP NAMES>
  CASE SL_EDITOR_SPLASH_MENU: RETURN "editor splash menu"
  CASE SL_EDITOR_THINGBROWSER_THINGLIST: RETURN "editor thingbrowser thinglist"
  CASE SL_EDITOR_THINGBROWSER_PLANK_SPRITE: RETURN "editor thingbrowser plank sprite"
  CASE SL_EDITOR_THINGBROWSER_BACK_HOLDER: RETURN "editor thingbrowser back holder"
  CASE SL_EDITOR_THINGBROWSER_MODE_INDICATOR: RETURN "editor thingbrowser mode indicator"
  CASE SL_EDITOR_THINGBROWSER_NEW_HOLDER: RETURN "editor thingbrowser new holder"
  CASE SL_EDITOR_THINGBROWSER_NOSCROLL_AREA: RETURN "editor thingbrowser noscroll area"
  CASE SL_EDITOR_THINGBROWSER_FILTER_HOLDER: RETURN "editor thingbrowser filter holder"
  CASE SL_EDITOR_THINGBROWSER_TYPE_QUERY: RETURN "editor thingbrowser type query"
  CASE SL_EDITOR_THINGBROWSER_FILTER_TEXT: RETURN "editor thingbrowser filter text"
  CASE SL_EDITOR_PROMPT_FOR_STRING_TEXT: RETURN "editor prompt for string text"
  CASE SL_EDITOR_PROMPT_FOR_STRING_CAPTION: RETURN "editor prompt for string caption"
  CASE SL_EDITOR_SSED_LIST: RETURN "editor ssed list"
  CASE SL_EDITOR_SSED_SET_TEMPL: RETURN "editor ssed set templ"
  CASE SL_EDITOR_SSED_FRAME_HOLDER: RETURN "editor ssed frame holder"
  CASE SL_EDITOR_SSED_FRAME_TEMPL: RETURN "editor ssed frame templ"
  CASE SL_EDITOR_SSED_FRAME_SPRITE: RETURN "editor ssed frame sprite"
  CASE SL_EDITOR_SSED_INFO_TEXT: RETURN "editor ssed info text"
  CASE SL_EDITOR_SSED_PALETTE_GRID: RETURN "editor ssed palette grid"
  CASE SL_EDITOR_SSED_PALETTE_TEXT: RETURN "editor ssed palette text"
  CASE SL_EDITOR_SSED_SET_INFO: RETURN "editor ssed set info"
  CASE SL_EDITOR_SSED_SET: RETURN "editor ssed set"
  CASE SL_EDITOR_SSED_PALETTE_ROOT: RETURN "editor ssed palette root"
  CASE SL_EDITOR_SSED_INFO_TEXT_RIGHT: RETURN "editor ssed info text right"
  CASE SL_EDITOR_SSED_CAPTION_TEXT: RETURN "editor ssed caption text"
  CASE SL_EDITOR_ENEMY_SPRITE: RETURN "editor enemy sprite"
  CASE SL_ROOT: RETURN "root"
  CASE SL_TEXTBOX_TEXT: RETURN "textbox text"
  CASE SL_TEXTBOX_PORTRAIT: RETURN "textbox portrait"
  CASE SL_TEXTBOX_CHOICE0: RETURN "textbox choice0"
  CASE SL_TEXTBOX_CHOICE1: RETURN "textbox choice1"
  CASE SL_TEXTBOX_BOX: RETURN "textbox box"
  CASE SL_TEXTBOX_PORTRAIT_BOX: RETURN "textbox portrait box"
  CASE SL_TEXTBOX_CHOICE_BOX: RETURN "textbox choice box"
  CASE SL_TEXTBOX_ROOT: RETURN "textbox root"
  CASE SL_SCRIPT_LAYER: RETURN "script layer"
  CASE SL_TEXTBOX_LAYER: RETURN "textbox layer"
  CASE SL_STRING_LAYER: RETURN "string layer"
  CASE SL_RESERVE: RETURN "reserve"
  CASE SL_MAPROOT: RETURN "maproot"
  CASE SL_OBSOLETE_OVERHEAD: RETURN "obsolete overhead"
  CASE SL_MAP_OVERLAY: RETURN "map overlay"
  CASE SL_WALKABOUT_LAYER: RETURN "walkabout layer"
  CASE SL_HERO_LAYER: RETURN "hero layer"
  CASE SL_NPC_LAYER: RETURN "npc layer"
  CASE SL_WALKABOUT_SPRITE: RETURN "walkabout sprite"
  CASE SL_WALKABOUT_SHADOW: RETURN "walkabout shadow"
  CASE SL_BACKDROP: RETURN "backdrop"
  CASE SL_MAP_LAYER0: RETURN "map layer0"
  CASE SL_MAP_LAYER1: RETURN "map layer1"
  CASE SL_MAP_LAYER2: RETURN "map layer2"
  CASE SL_MAP_LAYER3: RETURN "map layer3"
  CASE SL_MAP_LAYER4: RETURN "map layer4"
  CASE SL_MAP_LAYER5: RETURN "map layer5"
  CASE SL_MAP_LAYER6: RETURN "map layer6"
  CASE SL_MAP_LAYER7: RETURN "map layer7"
  CASE SL_MAP_LAYER8: RETURN "map layer8"
  CASE SL_MAP_LAYER9: RETURN "map layer9"
  CASE SL_MAP_LAYER10: RETURN "map layer10"
  CASE SL_MAP_LAYER11: RETURN "map layer11"
  CASE SL_MAP_LAYER12: RETURN "map layer12"
  CASE SL_MAP_LAYER13: RETURN "map layer13"
  CASE SL_MAP_LAYER14: RETURN "map layer14"
  CASE SL_MAP_LAYER15: RETURN "map layer15"
  CASE SL_STATUS_PORTRAIT: RETURN "status portrait"
  CASE SL_STATUS_WALKABOUT: RETURN "status walkabout"
  CASE SL_STATUS_BATTLESPRITE: RETURN "status battlesprite"
  CASE SL_STATUS_PAGE_SELECT: RETURN "status page select"
  CASE SL_STATUS_STATLIST: RETURN "status statlist"
  CASE SL_STATUS_HIDE_IF_NO_MP: RETURN "status hide if no mp"
  CASE SL_STATUS_HIDE_IF_NO_LMP: RETURN "status hide if no lmp"
  CASE SL_STATUS_HIDE_IF_MAX_LEV: RETURN "status hide if max lev"
  CASE SL_PLANK_HOLDER: RETURN "plank holder"
  CASE SL_STATUS_HIDE_IF_NO_PORTRAIT: RETURN "status hide if no portrait"
  CASE SL_ITEM_ITEMLIST: RETURN "item itemlist"
  CASE SL_ITEM_EXITBUTTON: RETURN "item exitbutton"
  CASE SL_ITEM_SORTBUTTON: RETURN "item sortbutton"
  CASE SL_ITEM_TRASHBUTTON: RETURN "item trashbutton"
  CASE SL_PLANK_MENU_SELECTABLE: RETURN "plank menu selectable"
  CASE SL_SPELL_LISTLIST: RETURN "spell listlist"
  CASE SL_SPELL_SPELLLIST: RETURN "spell spelllist"
  CASE SL_SPELL_HIDE_IF_NO_LIST: RETURN "spell hide if no list"
  CASE SL_SPELL_CANCELBUTTON: RETURN "spell cancelbutton"
  CASE SL_VIRTUAL_KEYBOARD_BUTTON: RETURN "virtual keyboard button"
  CASE SL_VIRTUAL_KEYBOARD_BUTTONTEXT: RETURN "virtual keyboard buttontext"
  CASE SL_VIRTUAL_KEYBOARD_SHIFT: RETURN "virtual keyboard shift"
  CASE SL_VIRTUAL_KEYBOARD_SYMBOLS: RETURN "virtual keyboard symbols"
  CASE SL_VIRTUAL_KEYBOARD_SELECT: RETURN "virtual keyboard select"
  CASE SL_VIRTUAL_KEYBOARD_ENTRYTEXT: RETURN "virtual keyboard entrytext"
  CASE SL_VIRTUAL_KEYBOARD_DEL: RETURN "virtual keyboard del"
  CASE SL_VIRTUAL_KEYBOARD_ENTER: RETURN "virtual keyboard enter"
  CASE SL_SHOP_BUY_INFO_PANEL: RETURN "shop buy info panel"
  CASE SL_STATUS_HIDE_IF_NO_HP: RETURN "status hide if no hp"
  CASE SL_PATHFIND_DEST_DISPLAY: RETURN "pathfind dest display"
  CASE SL_BATTLE_UI_LAYER: RETURN "battle ui layer"
  CASE SL_BATTLE_HERO_INFO_AREA: RETURN "battle hero info area"
  CASE SL_BATTLE_HERO_INFO_TEMPLATE: RETURN "battle hero info template"
  CASE SL_BATTLE_READY_METER: RETURN "battle ready meter"
  CASE SL_BATTLE_READY_METER_PANEL: RETURN "battle ready meter panel"
  CASE SL_BATTLE_HP_METER: RETURN "battle hp meter"
  CASE SL_BATTLE_HP_METER_PANEL: RETURN "battle hp meter panel"
  CASE SL_BATTLE_TEXT_SELECTED_PLAYER: RETURN "battle text selected player"
  CASE SL_BATTLE_MP_METER: RETURN "battle mp meter"
  CASE SL_BATTLE_MP_METER_PANEL: RETURN "battle mp meter panel"
  CASE SL_BATTLE_HERO_INFO_INSTANCE: RETURN "battle hero info instance"
  CASE SL_BATTLE_INDICATOR_AREA: RETURN "battle indicator area"
  CASE SL_BATTLE_POISON_INDICATOR: RETURN "battle poison indicator"
  CASE SL_BATTLE_REGEN_INDICATOR: RETURN "battle regen indicator"
  CASE SL_BATTLE_STUN_INDICATOR: RETURN "battle stun indicator"
  CASE SL_BATTLE_MUTE_INDICATOR: RETURN "battle mute indicator"
  CASE SL_BATTLE_CAPTION_AREA: RETURN "battle caption area"
  CASE SL_BATTLE_PLAYER_TURN_OVERLAY: RETURN "battle player turn overlay"
  CASE SL_BATTLE_TOGGLE_BOUNCE: RETURN "battle toggle bounce"
  CASE SL_BATTLE_TARGETTING_LAYER: RETURN "battle targetting layer"
  CASE SL_BATTLE_TARGETTING_TEMPLATE: RETURN "battle targetting template"
  CASE SL_BATTLE_TARGETTING_OVERLAY: RETURN "battle targetting overlay"
  CASE SL_BATTLE_TARG_HOVER_LAYER: RETURN "battle targ hover layer"
  CASE SL_BATTLE_TARG_HOVER_TEMPLATE: RETURN "battle targ hover template"
  CASE SL_BATTLE_TARG_HOVER_OVERLAY: RETURN "battle targ hover overlay"
  CASE SL_BATTLE_TARG_HOVER_SELECT: RETURN "battle targ hover select"
'</SLICE LOOKUP NAMES>
  CASE IS > 0
   'TODO: cache slicelookup() in memory.
   'This codepath currently only gets called from debug and error reporting code, so it's not much of a problem.
   DIM slicelookup() as string
   load_string_list slicelookup(), workingdir & SLASH & "slicelookup.txt"
   IF code <= UBOUND(slicelookup) ANDALSO LEN(TRIM(slicelookup(code))) THEN
    RETURN slicelookup(code)
   END IF
 END SELECT
 IF use_default THEN RETURN "Lookup" & code
 RETURN ""
END FUNCTION

EXTERN "C"

'Warning/TODO: one reason to call this instead of NewRectangleSlice, etc directly is that those
'constructors overwrite some data with defaults. To fix that, just set the defaults in the UDT directly.
FUNCTION NewSliceOfType (byval t as SliceTypes, byval parent as Slice Ptr=0, byval lookup_code as integer=0) as Slice Ptr
 DIM newsl as Slice Ptr
 SELECT CASE t
  CASE slSpecial:
   newsl = NewSlice(parent)
   newsl->SliceType = slSpecial
   newsl->Protect = YES
  CASE slContainer:
   newsl = NewSlice(parent)
  CASE slRectangle:
   DIM dat as RectangleSliceData
   newsl = NewRectangleSlice(parent, dat)
  CASE slLine:
   DIM dat as LineSliceData
   newsl = NewLineSlice(parent, dat)
  CASE slSprite:
   DIM dat as SpriteSliceData
   newsl = NewSpriteSlice(parent, dat)
  CASE slText
   DIM dat as TextSliceData
   newsl = NewTextSlice(parent, dat)
  CASE slMap:
   DIM dat as MapSliceData
   newsl = NewMapSlice(parent, dat)
   newsl->Protect = YES
  CASE slGrid:
   DIM dat as GridSliceData
   newsl = NewGridSlice(parent, dat)
  CASE slEllipse:
   DIM dat as EllipseSliceData
   newsl = NewEllipseSlice(parent, dat)
  CASE slScroll:
   DIM dat as ScrollSliceData
   newsl = NewScrollSlice(parent, dat)
  CASE slSelect:
   DIM dat as SelectSliceData
   newsl = NewSelectSlice(parent, dat)
  CASE slPanel:
   DIM dat as PanelSliceData
   newsl = NewPanelSlice(parent, dat)
  CASE slLayout:
   DIM dat as LayoutSliceData
   newsl = NewLayoutSlice(parent, dat)
  CASE ELSE
   showbug "NewSliceByType: type " & t & " is invalid"
   newsl = NewSlice(parent)
 END SELECT
 newsl->Lookup = lookup_code
 RETURN newsl
END FUNCTION

'Creates a new Container Slice (which can be changed to Special) and optionally, adds it to the heirarchy somewhere
Function NewSlice(byval parent as Slice ptr = 0) as Slice ptr
 dim ret as Slice Ptr
 ret = new Slice
 if ret = 0 then return 0

 setSliceParent(ret, parent)
 
 ret->SliceType = slContainer
 ret->Visible = YES
 ret->Attached = 0
 ret->Attach = slSlice

 ret->ClampHoriz = alignNone
 ret->ClampVert = alignNone

 ret->Draw = NULL
 ret->Dispose = @DisposeNullSlice
 ret->Clone = @CloneNullSlice
 ret->Save = @SaveNullSlice
 ret->Load = @LoadNullSlice
 ret->ChildRefresh = @DefaultChildRefresh
 ret->ChildrenRefresh = NULL
 ret->ChildDraw = @DefaultChildDraw

 #ifdef ENABLE_SLICE_DEBUG
  SliceDebugRemember ret
 #endif

 return ret
End Function

#ifdef IS_GAME

 'This shows an error and returns false if a slice has a bad TableSlot
 Function CheckTableSlotOK(sl as Slice ptr) as bool
  if sl = 0 then return NO
  if sl->TableSlot > 0 then
   if sl->TableSlot <= ubound(plotslices) then
    if plotslices(sl->TableSlot).sl = sl then
     return YES
    else
     showbug "TableSlot mismatch! Slice " & SlicePath(sl) & " slot is " & sl->TableSlot & " which has " & plotslices(sl->TableSlot).sl
    end if
   else
    showbug "TableSlot for " & SlicePath(sl) & " is invalid: " & sl->TableSlot
   end if
  end if
  return NO
 End Function

#else

 Function CheckTableSlotOK(sl as Slice ptr) as bool
  return sl <> 0
 End Function

#endif

'Deletes a slice and any descendents,  and zeros out *s.
'If debugme is YES, dump some debug info about the slice being freed and all its children
'(debugme > 0 is indentation depth)
Sub DeleteSlice(byval s as Slice ptr ptr, byval debugme as integer=0)

 if s = 0 then exit sub  'can't do anything
 if *s = 0 then exit sub 'already freed

 dim sl as slice ptr = *s

 if debugme = -1 then debugme = 1
 if debugme > 0 then
  debug string(debugme - 1, " ") & DescribeSlice(sl)
  'SliceDebugLinks sl, NO, "deleting", debugme - 1
  debugme += 1
 end if

#ifdef IS_GAME
 'unlink this slice from the table of handles if it has an assigned slice handle
 if CheckTableSlotOK(sl) then
  'Zero out the reference to this slice from the table, but leave .handle alone so
  'it will be incremented next time.
  plotslices(sl->TableSlot).sl = 0
  if sl->TableSlot < next_slice_table_slot then
   num_reusable_slice_table_slots += 1
  end if
 end if
#endif
 
 'Call the slice's type-specific Dispose function
 if sl->Dispose <> 0 then sl->Dispose(sl)
 
 OrphanSlice sl
 DeleteSliceChildren sl, debugme

 #ifdef ENABLE_SLICE_DEBUG
  SliceDebugForget sl
 #endif

 delete sl->Context
 v_free sl->ExtraVec
 delete sl
 *s = 0
End Sub

'Deletes a slice's children but not itself
'If debugme is YES, log debug info about the slices.
Sub DeleteSliceChildren(byval sl as Slice ptr, byval debugme as integer = 0)
 if sl = 0 then debug "DeleteSliceChildren null ptr": exit sub
 dim ch as slice ptr
 ch = sl->FirstChild
 do while ch
  DeleteSlice @ch, debugme
  ch = sl->FirstChild
 loop
End Sub

Sub OrphanSlice(byval sl as Slice ptr)
 '-- Remove a slice from its current parent cleanly,
 '-- adjusting siblings, and leaving itself parentless.
 if sl = 0 then debug "OrphanSlice null ptr": exit sub
 
 dim as Slice ptr nxt, prv, par
 nxt = sl->NextSibling
 prv = sl->PrevSibling
 par = sl->Parent
 
 if nxt then
  nxt->PrevSibling = prv
 end if
 if prv then
  prv->NextSibling = nxt
 end if
 if par then
  if par->FirstChild = sl then
   par->FirstChild = nxt
  end if
  if par->LastChild = sl then
   par->LastChild = prv
  end if
  par->NumChildren -= 1
 end if
 
 sl->NextSibling = 0
 sl->PrevSibling = 0
 sl->Parent = 0
end sub

Sub SetSliceParent(byval sl as Slice ptr, byval parent as Slice ptr)
 'Note: might be reparenting a slice to its parent, to make it the last child
 if sl = 0 then debug "SetSliceParent null ptr": exit sub

 if parent andalso verifySliceLineage(sl, parent) = 0 then
  reporterr "Attempted to parent a slice to itself or descendents!", serrBadOp
  exit sub
 end if

 'first, remove the slice from its existing parent
 OrphanSlice sl
 
 'then, add ourselves to the new parent
 if parent then
  if parent->FirstChild = 0 then
   parent->FirstChild = sl
  end if
  if parent->LastChild then
   parent->LastChild->NextSibling = sl
   sl->PrevSibling = parent->LastChild
  end if
  parent->LastChild = sl
   
  parent->NumChildren += 1
  sl->parent = parent
 end if
end sub

Sub AutoSortChildren(byval s as Slice Ptr)
 if s = 0 then debug "AutoSortChildren: null ptr": exit sub
 select case s->AutoSort
  case slAutoSortCustom:
   CustomSortChildSlices s, NO
  case slAutoSortY:
   YSortChildSlices s
  case slAutoSortTopY:
   EdgeYSortChildSlices s, alignTop
  case slAutoSortCenterY:
   EdgeYSortChildSlices s, alignCenter
  case slAutoSortBottomY:
   EdgeYSortChildSlices s, alignBottom
 end select
End sub

'Orphan all the children of a slice, and insert pointer to them in slice_list(),
'which must have length equal to number of children!
Sub UnlinkChildren(byval parent as Slice Ptr, slice_list() as Slice ptr)
 if parent = 0 then debug "UnlinkChildren: null ptr"
 dim temp_sl as Slice ptr = parent->FirstChild
 parent->FirstChild = 0
 parent->LastChild = 0
 parent->NumChildren = 0
 dim i as integer
 'Convert the children into an unlinked list
 for i = 0 to ubound(slice_list)
  slice_list(i) = temp_sl
  temp_sl = temp_sl->NextSibling
  slice_list(i)->PrevSibling = 0
  slice_list(i)->NextSibling = 0
  slice_list(i)->Parent = 0
 next i
end sub

'Set the children of a slice with no children to be equal to
'contents of array of orphaned child slice pointers.
'NOTE: children need to be orphans, and that's not checked.
Sub RelinkChildren(byval parent as Slice Ptr, slice_list() as Slice ptr)
 BUG_IF(parent = 0, "null ptr")
 BUG_IF(parent->NumChildren <> 0, "Already has children")
 dim i as integer
 parent->FirstChild = slice_list(0)
 parent->LastChild = slice_list(ubound(slice_list))
 parent->NumChildren = ubound(slice_list) + 1
 'Convert back to a doubly linked list
 slice_list(0)->Parent = parent
 for i = 1 to ubound(slice_list)
  slice_list(i - 1)->NextSibling = slice_list(i)
  slice_list(i)->PrevSibling = slice_list(i - 1)
  slice_list(i)->Parent = parent
 next i
end sub

Sub SwapSiblingSlices(byval sl1 as Slice ptr, byval sl2 as Slice ptr)
 'Only intended for use by siblings of the same parent.
 'This is slow, but isn't yet used anywhere where that might be a problem.
 if sl1 = 0 or sl2 = 0 then EXIT SUB ' Exit quietly when an arg is null. Valid use case for attempted swap at the beginning or end of a list
 if sl1 = sl2 then EXIT SUB ' Ignore attempts to swap a slice with itself
 if sl1->Parent <> sl2->Parent then reporterr "SwapSiblingSlices: slices are not siblings": EXIT SUB
 dim parent as Slice ptr = sl1->Parent
 dim slice_list(parent->NumChildren - 1) as Slice ptr
 UnlinkChildren parent, slice_list()
 'Swap the two siblings
 for i as integer = 0 to ubound(slice_list)
  if slice_list(i) = sl1 then
   slice_list(i) = sl2
  elseif slice_list(i) = sl2 then
   slice_list(i) = sl1
  end if
 next i
 RelinkChildren parent, slice_list()
end sub

Sub YSortChildSlices(byval parent as Slice ptr)
 if parent = 0 then debug "YSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as Slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings by Y
 dim temp as Slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Y <= temp->Y then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 RelinkChildren parent, slice_list()
end sub

Sub CustomSortChildSlices(byval parent as Slice ptr, byval wipevals as bool)
 if parent = 0 then debug "CustomSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as Slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings by Sorter
 dim temp as Slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Sorter <= temp->Sorter then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 if wipevals then
  for j as integer = 0 to ubound(slice_list)
   slice_list(j)->Sorter = 0
  next
 end if
 RelinkChildren parent, slice_list()
End sub

Sub EdgeYSortChildSlices(byval parent as Slice ptr, byval edge as AlignType)
 if parent = 0 then debug "EdgeYSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as Slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings all by the same edge/corner
 dim temp as Slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Y - SliceYAnchor(slice_list(i)) + SliceEdgeY(slice_list(i), edge) <= temp->Y - SliceYAnchor(temp) + SliceEdgeY(temp, edge) then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 RelinkChildren parent, slice_list()
end sub

Sub InsertSliceBefore(byval sl as Slice ptr, byval newsl as Slice ptr)
 'newsl will be removed from its current parent (if any) and parented to the same
 'parent as sl as the child before sl
 if sl = 0 then debug "InsertSliceBefore: null sl": EXIT SUB
 if newsl = 0 then debug "InsertSliceBefore: null newsl": EXIT SUB
 if sl = newsl then EXIT SUB ' Fail quietly when trying to insert a slice as a sibling
                             ' of itself because this is normal if you are using this function
                             ' to move a slice to the beginning of its sibling list when it is
                             ' already the first sibling
 if sl->PrevSibling = newsl then EXIT SUB 'already done
 if sl->Parent = 0 then reporterr "InsertSliceBefore: Root shouldn't have siblings" : EXIT SUB

 'Verify the family
 if verifySliceLineage(newsl, sl->Parent) = NO then
  reporterr "InsertSliceBefore: attempted to parent a slice to itself or descendents"
  EXIT SUB
 end if

 if newsl->Parent <> 0 then OrphanSlice newsl

 'Tell the new sibling about its parent
 newsl->Parent = sl->Parent

 'If this new sibling is an eldest child, tell the parent
 '(not possible to be LastChild)
 if sl->Parent->FirstChild = sl then
  sl->Parent->FirstChild = newsl
 end if

 'Tell previous siblings that it has a new sibling.
 if sl->PrevSibling <> 0 then
  sl->PrevSibling->NextSibling = newsl
 end if
 
 'Tell new sibling about its adjacent siblings
 newsl->PrevSibling = sl->PrevSibling
 newsl->NextSibling = sl

 'Tell the supplanted sibling that the new one precedes it
 sl->PrevSibling = newsl

 'One more mouth to feed...
 newsl->Parent->NumChildren += 1
end sub

Sub InsertSliceAfter(byval sl as Slice ptr, byval newsl as Slice ptr)
 'newsl will be removed from its current parent (if any) and parented to the same
 'parent as sl as the child after sl.
 if sl = 0 then debug "InsertSliceAfter: null sl": exit sub
 if newsl = 0 then debug "InsertSliceAfter: null newsl": exit sub
 if sl = newsl then exit sub ' Fail quietly when trying to insert a slice as a sibling
                             ' of itself because this is normal if you are using this function
                             ' to move a slice to the end of its sibling list
 if sl->NextSibling = newsl then exit sub 'already done
 if sl->Parent = 0 then reporterr "InsertSliceAfter: Root shouldn't have siblings" : exit sub

 if sl->NextSibling then
  InsertSliceBefore sl->NextSibling, newsl
 else
  SetSliceParent newsl, sl->Parent
 end if
end sub

Sub ReplaceSliceType(byval sl as Slice ptr, byref newsl as Slice ptr)
 'This takes a new slice (normally from one of the New*Slice functions)
 'and copies its type and type-specific data over an existing tree member.
 'This is rather bizarre, but does make it possible to change into a Class slice.
 'Newsl gets Deleted to prevent it from being used afterwards!
 'Also, this fails if newsl is part of a tree. It must be parentless
 if sl = 0 then debug "ReplaceSliceType null ptr": exit sub
 if newsl = 0 then debug "ReplaceSliceType newsl null ptr": exit sub
 WITH *newsl
  'Make sure that newsl is an orphan already
  BUG_IF(.Parent, "Only works with orphaned slices")
  'Dispose of any old Slice Type specific data that is about to be replaced
  IF sl->SliceData <> 0 THEN sl->Dispose(sl)
  'Copy over slice identity
  sl->SliceType = .SliceType
  sl->Draw      = .Draw
  sl->Dispose   = .Dispose
  sl->Clone     = .Clone
  sl->Save      = .Save
  sl->Load      = .Load
  sl->ChildRefresh = .ChildRefresh
  sl->ChildrenRefresh = .ChildrenRefresh
  sl->ChildDraw = .ChildDraw
  sl->SliceData = .SliceData
  sl->SliceType = .SliceType
  'Break slice connection to data
  .SliceData = 0
  'Now destroy newsl
  DeleteSlice @newsl
 END WITH
End Sub

'If start_sl = NULL: Find first descendent of root_sl (including root_sl itself) which matches lookup and onlytype.
'If start_sl <> NULL: Find next descendent (in depth-first order), after start_sl, of root_sl.
'onlytype: if slInvalid, any type allowed.
Function LookupSlice(byval lookup_code as integer, byval root_sl as Slice ptr, byval onlytype as SliceTypes=slInvalid, start_sl as Slice ptr = NULL) as Slice ptr
  IF root_sl = 0 THEN debug "LookupSlice null root slice": RETURN 0
  IF lookup_code = 0 THEN RETURN 0 '--fail searching for a zero lookup code
  DIM desc as Slice ptr
  IF start_sl = 0 THEN
   desc = root_sl
  ELSE
   desc = NextDescendent(start_sl, root_sl)
  END IF
  WHILE desc
   IF desc->Lookup = lookup_code THEN
    IF onlytype = slInvalid ORELSE onlytype = desc->SliceType THEN
     'If onlytype is invalid (default) then we don't care which type the slice is.
     'If onlytype is set, only match lookup codes on slices of the desired type.
     RETURN desc '--found it!
    END IF
   END IF
   desc = NextDescendent(desc, root_sl)
  WEND
  RETURN 0
End Function

'This is intended for built-in menus, to allow continuing gracefully instead of
'crashing, by creating a dummy slice if the slice is missing, and printing a more
'informative error message; if onlytype isn't specified, you get a container.
Function LookupSliceSafe(lookup_code as integer, root_sl as Slice ptr, onlytype as SliceTypes=slInvalid) as Slice ptr
 dim ret as Slice ptr = LookupSlice(lookup_code, root_sl, onlytype)
 if ret then return ret
 debug "Builtin slice collection missing " & iif(onlytype = slInvalid, "any", SliceTypeName(onlytype)) & _
       " slice with lookup code " & lookup_code
 if onlytype = slInvalid then onlytype = slContainer
 return NewSliceOfType(onlytype, root_sl, lookup_code)
End Function

'Calls showerror if the slice is missing, and if you press F6 at the error message enters the slice editor!
Function LookupSliceOrError(lookup_code as integer, root_sl as Slice ptr, onlytype as SliceTypes = slInvalid, slice_description as zstring ptr = @"slice", errlvl as ErrorLevelEnum = errShowError) as Slice ptr
 dim ret as Slice ptr = LookupSlice(lookup_code, root_sl, onlytype)
 if ret then return ret
 'Use the lookup code & message as the "call site" used for ignoring this error message
 debugc_internal canyptr(lookup_code + slice_description), errlvl, _
                 "Couldn't find " & *slice_description & " with lookup '" & SliceLookupCodename(lookup_code) & "'"
 if keyval(scF6) then
  slice_editor root_sl, SL_COLLECT_EDITOR
 end if
 return NULL
End Function

'Return the root of the tree by going up.
Function FindRootSlice(slc as Slice ptr) as Slice ptr
 dim root as Slice ptr
 while slc <> 0
  root = slc
  slc = root->parent
 wend
 return root
End Function

' Implements depth-first pre-order traversal of a slice subtree rooted at root_sl
' (pre-order meaning a parent is visited before its childen).
' Initialise desc to root_sl (or root_sl->FirstChild to skip the root),
' and then call NextDescendent repeatedly to get the next descendent.
' Returns NULL after the last one.
' Pass visit_children=NO if you want to skip over the children of the current slice.
Function NextDescendent(desc as Slice ptr, root_sl as Slice ptr, visit_children as bool = YES) as Slice ptr
 if desc = NULL then return NULL
 ' First try to go down, then across, otherwise up as far as needed.
 if desc->FirstChild andalso visit_children then return desc->FirstChild
 if desc = root_sl then return NULL  'Only for case root_sl has no children
 while desc->NextSibling = NULL
  desc = desc->Parent
  if desc = root_sl or desc = NULL then return NULL
 wend
 return desc->NextSibling
End Function

'This function returns true if the ancestor slice is a parent or grandparent or great(*) grandparent...
'Returns NO if sl = ancestor.
Function IsAncestor(byval sl as Slice ptr, byval ancestor as Slice ptr) as bool
 if sl = 0 THEN debug "IsAncestor null slice": RETURN NO
 dim parent as Slice ptr = sl->parent
 do while parent
  if parent = ancestor then return YES
  parent = parent->Parent
 loop
 return NO
End Function

'this function ensures that we can't set a slice to be a child of itself (or, a child of a child of itself, etc)
'TODO: This should be deleted and replaced with IsAncestor
Function VerifySliceLineage(byval sl as Slice ptr, parent as Slice ptr) as bool
 dim s as Slice ptr
 if sl = 0 then return NO
 s = parent
 do while s <> 0
  if s = sl then return NO
  s = s->parent
 loop
 return YES
end function

'Returns the 0-based index of this slice among is siblings.
'If including_templates=NO then doesn't count template slices, but beware that if
'called on a template slice it'll share its index with the next sibling.
Function SliceIndexAmongSiblings(sl as Slice Ptr, include_templates as bool = YES) as integer
 if sl = 0 then return 0
 if sl->parent = 0 then return 0  'The root slice
 dim sib as Slice Ptr = sl->parent->FirstChild
 dim index as integer = 0
 while sib
  if sib = sl then return index
  if include_templates orelse sib->Template = NO then
   index += 1
  end if
  sib = sib->NextSibling
 wend
 showbug "slice not a child of its parent"
 return 0
End function

'Returns a path like "/Container[1]/Sprite", similiar to an RELOAD node path, from the root slice to this one.
Function SlicePath(sl as Slice ptr) as string
 dim ret as string
 if sl->Parent then
  ret = SlicePath(sl->Parent) & "[" & SliceIndexAmongSiblings(sl) & "]"
 end if
 ret &= "/" & SliceTypeName(sl)
 if sl->Lookup then ret &= "(" & SliceLookupCodename(sl->Lookup) & ")"
 return ret
end function

'slice accessors
Function SliceGetParent( byval s as Slice ptr ) as Slice ptr
 return s->Parent
End Function
Function SliceGetFirstChild( byval s as Slice ptr ) as Slice ptr
 return s->FirstChild
End Function
Function SliceGetLastChild( byval s as Slice ptr ) as Slice ptr
 return s->LastChild
End Function
Function SliceGetNextSibling( byval s as Slice ptr ) as Slice ptr
 return s->NextSibling
End Function
Function SliceGetPrevSibling( byval s as Slice ptr ) as Slice ptr
 return s->PrevSibling
End Function
Function SliceGetNumChildren( byval s as Slice ptr ) as integer
 return s->NumChildren
End Function
Function SliceGetX( byval s as Slice ptr ) as integer
 return s->X
End Function
Function SliceGetY( byval s as Slice ptr ) as integer
 return s->Y
End Function
Function SliceGetScreenX( byval s as Slice ptr ) as integer
 return s->ScreenX
End Function
Function SliceGetScreenY( byval s as Slice ptr ) as integer
 return s->ScreenY
End Function
Function SliceGetWidth( byval s as Slice ptr ) as integer
 return s->Width
End Function
Function SliceGetHeight( byval s as Slice ptr ) as integer
 return s->Height
End Function
Function SliceIsVisible( byval s as Slice ptr ) as bool
 return s->Visible
End Function
Function SliceIsPaused( byval s as Slice ptr ) as bool
 return s->Paused
End Function
Function SliceIsClipping( byval s as Slice ptr ) as bool
 return s->Clip
End Function
'slice mutators
Sub SliceSetX( byval s as Slice ptr, byval x as integer )
 s->X = x
End Sub
Sub SliceSetY( byval s as Slice ptr, byval y as integer )
 s->Y = y
End Sub
Sub SliceSetWidth( byval s as Slice ptr, byval w as integer )
 s->Width = w
End Sub
Sub SliceSetHeight( byval s as Slice ptr, byval h as integer )
 s->Height = h
End Sub
Sub SliceSetVisibility( byval s as Slice ptr, byval b as bool )
 s->Visible = b
End Sub
Sub SliceSetPaused( byval s as Slice ptr, byval b as bool )
 s->Paused = b
End Sub
Sub SliceSetClipping( byval s as Slice ptr, byval b as bool )
 s->Clip = b
End Sub

'FillMode is so painful to deal with that we need these
'TODO: find a proper solution
Function Slice.FillHoriz() as bool
 return this.Fill andalso this.FillMode <> sliceFillVert
End Function

Function Slice.FillVert() as bool
 return this.Fill andalso this.FillMode <> sliceFillHoriz
End Function

Property Slice.Extra(index as integer) as integer
 return get_extra(ExtraVec, index)
End Property

Property Slice.Extra(index as integer, newval as integer)
 set_extra(ExtraVec, index, newval)
End Property

'=============================================================================
'                                Slice contexts

End Extern

Destructor SliceContext()
End Destructor

Sub SliceContext.save(node as Reload.Nodeptr)
End Sub

Sub SliceContext.load(node as Reload.Nodeptr)
End Sub

Sub SliceCollectionContext.load(node as Reload.Nodeptr)
 name = LoadPropStr(node, "collection_name")
End Sub

Sub SliceCollectionContext.save(node as Reload.Nodeptr)
 SaveProp node, "collection_name", name
 'id not saved
End Sub

Function SliceCollectionContext.description() as string
 dim ret as string = "Collection"
 if id > -1 then ret &= " " & id
 if len(name) then ret &= " " & name
 return ret
End Function

Extern "C"


'=============================================================================
'                                 Slice types


'--Class-based Slice wrapper--------------------------------------------------
'These wrappers will no longer be needed once we switch to a true polymorphic Slice class

Declare Sub InitClassSlicePtrs(sl as Slice ptr)

Function NewClassSlice(parent as Slice ptr, inst as ClassSlice ptr) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0

 ret->SliceType = slSpecial
 ret->ClassInst = inst
 InitClassSlicePtrs ret
 inst->initialize(ret)
 return ret
End Function

'This is called only from SliceLoadFromNode
Local Function InitClassSliceByName(sl as Slice ptr, classname as string) as bool
 if sl->SliceData <> 0 then sl->Dispose(sl)
 dim inst as ClassSlice ptr
 'if classname = "thing" then
 ' inst = new ThingClass()
 'else
  return NO
 'end if
 sl->SliceType = slSpecial
 sl->ClassInst = inst
 InitClassSlicePtrs sl
 inst->initialize(sl)
 return YES
End Function

Sub DisposeClassSlice(sl as Slice ptr)
 if sl = 0 orelse sl->ClassInst = 0 then exit sub
 delete sl->ClassInst
 sl->ClassInst = NULL
End Sub

Sub DrawClassSlice(sl as Slice ptr, page as integer)
 if sl = 0 orelse sl->ClassInst = 0 then debug "DrawClassSlice null ptr": exit sub
 sl->ClassInst->Draw(sl, page)
End Sub

Sub CloneClassSlice(sl as Slice ptr, cl as Slice ptr)
 if sl = 0 orelse sl->ClassInst = 0 then debug "CloneClassSlice null ptr": exit sub
 sl->ClassInst->Clone(sl, cl)
End Sub

Sub SaveClassSlice(sl as Slice ptr, node as Reload.Nodeptr)
 if sl = 0 orelse sl->ClassInst = 0 orelse node = 0 then debug "SaveClassSlice null ptr": exit sub
 sl->ClassInst->Save(sl, node)
End Sub

Sub LoadClassSlice(sl as Slice ptr, node as Reload.Nodeptr)
 if sl = 0 orelse sl->ClassInst = 0 orelse node = 0 then debug "LoadClassSlice null ptr": exit sub
 sl->ClassInst->Load(sl, node)
End Sub

Sub ClassChildRefresh(sl as Slice ptr, ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 if sl = 0 orelse sl->ClassInst = 0 then debug "ClassSliceChildRefresh null ptr": exit sub
 sl->ClassInst->ChildRefresh sl, ch, childindex, visibleonly
End Sub

Sub ClassChildrenRefresh(sl as Slice ptr)
 if sl = 0 orelse sl->ClassInst = 0 then debug "ClassSliceChildrenRefresh null ptr": exit sub
 sl->ClassInst->ChildrenRefresh sl
End Sub

Sub ClassChildDraw(sl as Slice ptr, page as integer)
 if sl = 0 orelse sl->ClassInst = 0 then debug "ClassSliceChildDraw null ptr": exit sub
 sl->ClassInst->ChildDraw sl, page
End Sub

Local Sub InitClassSlicePtrs(sl as Slice ptr)
 sl->Draw = @DrawClassSlice
 sl->Dispose = @DisposeClassSlice
 sl->Clone = @CloneClassSlice
 sl->Save = @SaveClassSlice
 sl->Load = @LoadClassSlice
 sl->ChildRefresh = @ClassChildRefresh
 sl->ChildrenRefresh = @ClassChildrenRefresh
 sl->ChildDraw = @ClassChildDraw
End Sub


'--ClassSlice default methods---------------------------------------------

End Extern

'If part of the initialisation needs the Slice ptr, it can be delayed until this is called
Sub ClassSlice.Initialize(sl as Slice ptr)
End Sub

Destructor ClassSlice()
End Destructor

Sub ClassSlice.Draw(sl as Slice ptr, page as integer)
End Sub

Sub ClassSlice.Clone(sl as Slice ptr, cl as Slice ptr)
 showerror "This ClassSlice can't be cloned"
End Sub

Sub ClassSlice.Save(sl as Slice ptr, node as Reload.Nodeptr)
 showerror "This ClassSlice can't be saved"
End Sub

'Should never be called
Sub ClassSlice.Load(sl as Slice ptr, node as Reload.Nodeptr)
 showerror "This ClassSlice can't be loaded"
End Sub

Sub ClassSlice.ChildRefresh(sl as Slice ptr, ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 DefaultChildRefresh sl, ch, childindex, visibleonly
End Sub

Sub ClassSlice.ChildrenRefresh(sl as Slice ptr)
 'Is NULL by default
End Sub

Sub ClassSlice.ChildDraw(sl as Slice ptr, page as integer)
 DefaultChildDraw sl, page
End Sub

Extern "C"

'--Rectangle--------------------------------------------------------------
Sub DisposeRectangleSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->RectData = 0 then exit sub
 delete sl->RectData
 sl->RectData = 0
end sub

Sub UpdateRectangleSliceStyle(byval dat as RectangleSliceData ptr)
 IF dat->style >= 0 ANDALSO dat->style <= UBOUND(boxlook) THEN
  dat->bgcol = boxlook(dat->style).bgcol
  dat->fgcol = boxlook(dat->style).edgecol
  dat->border = dat->style
  dat->use_raw_box_border = NO
 ELSE
  debug "bad rect style " & dat->style
 END IF
 dat->style_loaded = YES
end sub

Sub DrawRectangleSlice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 with *sl->RectData
  if .style >= 0 and .style_loaded = NO then
   UpdateRectangleSliceStyle sl->RectData
  end if

  dim borderindex as RectBorderTypes
  if .use_raw_box_border then
   borderindex = .raw_box_border
  else
   borderindex = lookup_box_border(.border)
  end if

  draw_box_back vpages(p), sl->ScreenPos, sl->Size, ColorIndex(.bgcol), .translucent, _
                .fuzzfactor, .fuzz_stationary, .fuzz_zoom
  draw_box_border vpages(p), sl->ScreenPos, sl->Size, ColorIndex(.fgcol), borderindex, .translucent
 end with
end sub

Sub CloneRectangleSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneRectangleSlice null ptr": exit sub
 dim dat as RectangleSliceData Ptr
 dat = sl->RectData
 with *cl->RectData
  .style       = dat->style
  .style_loaded= dat->style_loaded  'Doesn't matter
  .fgcol       = dat->fgcol
  .bgcol       = dat->bgcol
  .translucent = dat->translucent
  .border      = dat->border
  .fuzzfactor  = dat->fuzzfactor
  .fuzz_stationary = dat->fuzz_stationary
  .fuzz_zoom = dat->fuzz_zoom
  .use_raw_box_border = dat->use_raw_box_border
  .raw_box_border = dat->raw_box_border
 end with
end sub

Sub SaveRectangleSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveRectangleSlice null ptr": exit sub
 dim dat as RectangleSliceData Ptr
 dat = sl->SliceData
 if dat->style >= 0 then
  SavePropAlways node, "style", dat->style
 else
  SaveProp node, "fg", dat->fgcol
  SaveProp node, "bg", dat->bgcol
  if dat->use_raw_box_border then
   SavePropAlways node, "raw_box_border", dat->raw_box_border
  else
   SavePropAlways node, "border", dat->border
  end if
 end if
 SaveProp node, "trans", dat->translucent
 if dat->fuzzfactor <> 50 then
  SavePropAlways node, "fuzzfactor", dat->fuzzfactor
 end if
 SaveProp node, "fz_stationary", dat->fuzz_stationary
 if dat->fuzz_zoom <> 1 then
  SavePropAlways node, "fz_zoom", dat->fuzz_zoom
 end if
End Sub

Sub LoadRectangleSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadRectangleSlice null ptr": exit sub
 dim dat as RectangleSliceData Ptr
 dat = sl->SliceData
 dat->translucent = LoadProp(node, "trans")
 dat->fuzzfactor = LoadProp(node, "fuzzfactor", 50)
 dat->fuzz_stationary = LoadPropBool(node, "fz_stationary")
 dat->fuzz_zoom = LoadProp(node, "fz_zoom", 1)
 dat->style = LoadProp(node, "style", -1)
 if dat->style >= 0 then
  dat->style_loaded = NO
  UpdateRectangleSliceStyle dat
 else
  dat->fgcol = LoadProp(node, "fg")
  dat->bgcol = LoadProp(node, "bg")
  dat->border = LoadProp(node, "border", borderLine)
  dat->raw_box_border = LoadProp(node, "raw_box_border", -1)
  dat->use_raw_box_border = (dat->raw_box_border > -1)
  if dat->raw_box_border = -1 then dat->raw_box_border = 0
 end if
End Sub

Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as RectangleSliceData ptr = new RectangleSliceData
 *d = dat
 
 ret->SliceType = slRectangle
 ret->SliceData = d
 ret->Draw = @DrawRectangleSlice
 ret->Dispose = @DisposeRectangleSlice
 ret->Clone = @CloneRectangleSlice
 ret->Save = @SaveRectangleSlice
 ret->Load = @LoadRectangleSlice
 
 return ret
end function

Function GetRectangleSliceData(byval sl as Slice ptr) as RectangleSliceData ptr
 if sl = 0 then debug "GetRectangleSliceData null ptr": return 0
 return sl->SliceData
End Function

'All arguments default to no-change
Sub ChangeRectangleSlice(byval sl as Slice ptr,_
                      byval style as integer=-2,_
                      byval bgcol as integer=colInvalid,_
                      byval fgcol as integer=colInvalid,_
                      byval border as RectBorderTypes=borderUndef,_
                      byval translucent as RectTransTypes=transUndef,_
                      byval fuzzfactor as integer=0,_
                      byval raw_box_border as RectBorderTypes=borderUndef)
 if sl = 0 then debug "ChangeRectangleSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slRectangle)
 with *sl->RectData
  'First load the style, if any
  if style > -2 then
   .use_raw_box_border = NO
   .style = style
   .style_loaded = NO
  end if
  if .style >= 0 andalso .style_loaded = NO then
   UpdateRectangleSliceStyle sl->RectData
  end if
  'Then consider all other data as style overrides
  if bgcol <> colInvalid then
   .bgcol = bgcol
   .style = -1
   .style_loaded = NO
  end if
  if fgcol <> colInvalid then
   .fgcol = fgcol
   .style = -1
   .style_loaded = NO
  end if
  if raw_box_border >= 0 then
   .use_raw_box_border = YES
   .raw_box_border = raw_box_border
   .style = -1
   .style_loaded = NO
  elseif raw_box_border > borderUndef then
   'This is a convenience (and to make this behave the same as the script commands)
   border = raw_box_border
  end if
  if border > borderUndef then
   .use_raw_box_border = NO
   .border = border
   .style = -1
   .style_loaded = NO
  end if
  if translucent <> transUndef then .translucent = translucent
  if fuzzfactor > 0 then
   .fuzzfactor = fuzzfactor
  end if
 end with
end sub

'--Line-------------------------------------------------------------------

Sub DisposeLineSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->LineData = 0 then exit sub
 delete sl->LineData
 sl->LineData = 0
end sub

Sub DrawLineSlice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim point2 as XYPair = sl->ScreenPos + sl->Size
 dim col as integer = ColorIndex(sl->LineData->col)
 'if sl->LineData->flipped then
 ' drawline sl->ScreenX, point2.y, point2.x, sl->ScreenY, col, p
 'else
  drawline sl->ScreenX, sl->ScreenY, point2.x, point2.y, col, p
 'end if
end sub

Function GetLineSliceData(byval sl as Slice ptr) as LineSliceData ptr
 if sl = 0 then return 0
 return sl->LineData
End Function

Sub LineSliceData.SetColor(color as integer)
 this.col = color
End Sub

Sub CloneLineSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneLineSlice null ptr": exit sub
 dim dat as LineSliceData Ptr
 dat = sl->LineData
 with *cl->LineData
  .col       = dat->col
  '.flipped   = dat->flipped
 end with
End Sub

Sub SaveLineSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveLineSlice null ptr": exit sub
 dim dat as LineSliceData ptr = sl->SliceData
 SavePropAlways node, "col", dat->col
 'SaveProp node, "flipped", dat->flipped
End Sub

Sub LoadLineSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadLineSlice null ptr": exit sub
 dim dat as LineSliceData ptr = sl->SliceData
 dat->col = LoadProp(node, "col")
 'dat->flipped = LoadPropBool(node, "flipped")
End Sub

Function NewLineSlice(byval parent as Slice ptr, byref dat as LineSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0

 dim d as LineSliceData ptr = new LineSliceData
 *d = dat

 ret->SliceType = slLine
 ret->SliceData = d
 ret->Draw = @DrawLineSlice
 ret->Dispose = @DisposeLineSlice
 ret->Clone = @CloneLineSlice
 ret->Save = @SaveLineSlice
 ret->Load = @LoadLineSlice

 return ret
end function

'--Text-------------------------------------------------------------------

Declare Sub UpdateTextSliceHeight(byval sl as Slice ptr, lines() as string)
Declare Sub NewUpdateTextSlice(byval sl as Slice ptr)

Sub DisposeTextSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as TextSliceData ptr = sl->SliceData
 delete dat
 sl->SliceData = 0
end sub

Sub WrapTextSlice(byval sl as Slice ptr, lines() as string)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim dat as TextSliceData ptr = sl->SliceData
 dim d as string
 if dat->wrap then
  dim wide as integer
  if sl->Width > 7 then
   wide = sl->Width \ 8
  else
   wide = large(1, (get_resolution().w - sl->ScreenX) \ 8)
  end if
  d = wordwrap(dat->s, wide)
 else
  d = dat->s
 end if

 split(d, lines())

 '--set line count based on the current wrapped size
 dat->line_count = UBOUND(lines) + 1
End sub

'Get the 'wide' parameter to render_text, etc
Local Function TextSliceRenderTextWide(sl as Slice ptr, dat as TextSliceData ptr) as integer
 if dat->Wrap then
  if sl->Width > 7 then  'FIXME: this should be reduced for smaller fonts
   return sl->Width
  else
   return large(1, get_resolution().w - sl->ScreenX)
  end if
 else
  return 999999999
 end if
end function

'New render_text-based drawing of Text slices. Only used when dat->use_render_text
Sub NewDrawTextSlice(byval sl as Slice ptr, byval p as integer, col as integer)
 dim dat as TextSliceData ptr = sl->SliceData

 'dat->line_limit is not yet supported (render_text ought to be extended for it)

 'If the slice wraps, then its height changes any time that its width does
 'FIXME: we should update the size in ChildRefresh/ChildrenRefresh() instead,
 'but that's a more difficult fix; this is better than nothing
 'TODO: since this is expensive, don't call this every time we draw, but just when
 'when the size might have changed.
 NewUpdateTextSlice sl

 dim text as string = dat->s
 dim wide as integer = TextSliceRenderTextWide(sl, dat)
 dim fontnum as integer = iif(dat->fontnum, dat->fontnum, iif(dat->outline, fontEdged, fontPlain))
 if fontnum > ubound(fonts) then fontnum = 0  'Silent failure; might have loaded slices from a different game

 dat->insert_tog = dat->insert_tog xor 1

 if dat->show_insert then
  'Figure out where the insert cursor is and draw it
  dim charpos as StringCharPos
  find_text_char_position(@charpos, text, dat->insert, wide, fontnum)
  dim insert_pos as XYPair = sl->ScreenPos + charpos.pos
  rectangle(vpages(p), XY_WH(insert_pos, charpos.size), uilook(uiHighlight + dat->insert_tog))
 end if

 textcolor col, ColorIndex(dat->bgcol)
 wrapprint text, sl->ScreenX, sl->ScreenY, , p, wide, YES, fontnum
end sub

Sub DrawTextSlice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim dat as TextSliceData ptr = sl->SliceData

 dim col as integer = dat->col
 if col = 0 then col = uilook(uiText) '--This is backcompat for before it was possible to choose uiText directly using ColorIndex
 col = ColorIndex(col)

 if dat->use_render_text then
  NewDrawTextSlice sl, p, col
  exit sub
 end if

 dim lines() as string
 WrapTextSlice sl, lines()
 dim line_starts() as integer
 split_line_positions dat->s, lines(), line_starts()

 'If the slice wraps, then its height changes any time that its width does
 'FIXME: we should update the size in ChildRefresh/ChildrenRefresh() instead,
 'but that's a more difficult fix; this is better than nothing
 UpdateTextSliceHeight sl, lines()

 dat->insert_tog = dat->insert_tog xor 1
 dim insert_size as integer = 8
 if dat->outline then insert_size = 9
 dat->first_line = large(0, dat->first_line)
 dim last_line as integer = ubound(lines)
 if dat->line_limit <> -1 then last_line = small(last_line, dat->first_line + dat->line_limit - 1)

 for linenum as integer = dat->first_line to last_line
  dim ypos as integer
  ypos = (linenum - dat->first_line) * 10
  if dat->show_insert then
   dim offset_in_line as integer  '0-based offset
   offset_in_line = dat->insert - line_starts(linenum)
   dim next_line as integer = iif(linenum = last_line, len(dat->s) + 1, line_starts(linenum + 1))
   'The insert cursor might point to a space or newline after the end of the line or end of text
   if offset_in_line >= 0 and dat->insert < next_line then
    rectangle sl->screenx + offset_in_line * 8, sl->screeny + ypos, insert_size, insert_size, uilook(uiHighlight + dat->insert_tog), p
   end if
  end if
  if dat->outline then
   edgeprint lines(linenum), sl->screenx, sl->screeny + ypos, col, p
  else
   textcolor col, ColorIndex(dat->bgcol)
   printstr lines(linenum), sl->screenx, sl->screeny + ypos, p
  end if
 next
end sub

'New render_text-based updating of Text slice size. Only used when dat->use_render_text
Sub NewUpdateTextSlice(byval sl as Slice ptr)
 dim dat as TextSliceData ptr = sl->SliceData

 'dat->line_limit not supported yet
 dim fontnum as integer = iif(dat->outline, fontEdged, fontPlain)
 dim wide as integer = TextSliceRenderTextWide(sl, dat)
 dim size as XYPair = textsize(dat->s, wide, fontnum, YES)
 sl->Height = size.h
 if dat->Wrap = NO then sl->Width = size.w
end sub

'Update the size of text slice. This only happens when you call ChangeTextSlice.
'(Note: this must be called after WrapTextSlice() has set dat->line_count)
Sub UpdateTextSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as TextSliceData ptr = sl->SliceData

 if dat->use_render_text then
  NewUpdateTextSlice sl
  exit sub
 end if

 '--Note that automatic setting of wrapped text height doesn't matter if this slice is set ->Fill = YES the parent fill height will override
 dim lines() as string
 WrapTextSlice sl, lines()
 UpdateTextSliceHeight sl, lines()

 'Update width
 if dat->Wrap = NO then
  sl->Width = textWidth(dat->s)
 else
  '--Wrapped text does not change the slice width. Do that manually (or by setting ->Fill = YES)
 end if
end sub

'Return the position, relative to the slice position, of character in the string
'(Note: this assumes use_render_text; text wrapping may not be identical otherwise)
Function TextSliceCharPos(sl as Slice ptr, charnum as integer) as XYPair
 if sl = 0 orelse sl->SliceData = 0 then return XY(0, 0)
 dim dat as TextSliceData ptr = sl->SliceData

 dim wide as integer = TextSliceRenderTextWide(sl, dat)
 dim fontnum as integer = iif(dat->outline, fontEdged, fontPlain)
 dim charpos as StringCharPos
 find_text_char_position(@charpos, dat->s, charnum, wide, fontnum)
 return charpos.pos
end function

Local Sub UpdateTextSliceHeight(byval sl as Slice ptr, lines() as string)
 dim dat as TextSliceData ptr = sl->SliceData
 dim high as integer
 high = dat->line_count
 if dat->line_limit > -1 then  'If not unlimited
  high = small(high, dat->line_limit)
 end if
 sl->Height = high * 10
end sub

Function GetTextSliceData(byval sl as Slice ptr) as TextSliceData ptr
 if sl = 0 then debug "GetTextSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub CloneTextSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneTextSlice null ptr": exit sub
 dim dat as TextSliceData Ptr
 dat = sl->TextData
 with *cl->TextData
  .s       = dat->s
  .col     = dat->col
  .outline = dat->outline
  .wrap    = dat->wrap
  .bgcol   = dat->bgcol
 end with
end sub

Sub SaveTextSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveTextSlice null ptr": exit sub
 DIM dat as TextSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "s", dat->s
 SaveProp node, "col", dat->col
 SaveProp node, "outline", dat->outline
 SaveProp node, "wrap", dat->wrap
 SaveProp node, "bgcol", dat->bgcol
End Sub

Sub LoadTextSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadTextSlice null ptr": exit sub
 dim dat as TextSliceData Ptr
 dat = sl->SliceData
 dat->s       = LoadPropStr(node, "s")
 dat->col     = LoadProp(node, "col")
 dat->outline = LoadPropBool(node, "outline")
 dat->wrap    = LoadPropBool(node, "wrap")
 dat->bgcol   = LoadProp(node, "bgcol")

 'Ensure that width is correct, because it's currently only set when something changes,
 'and I have seen it saved wrong (e.g. due to a bug in etheldreme)
 UpdateTextSlice sl
End Sub

Function NewTextSlice(byval parent as Slice ptr, byref dat as TextSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as TextSliceData ptr = new TextSliceData
 *d = dat
 
 ret->SliceType = slText
 ret->SliceData = d
 ret->Draw = @DrawTextSlice
 ret->Dispose = @DisposeTextSlice
 ret->Clone = @CloneTextSlice
 ret->Save = @SaveTextSlice
 ret->Load = @LoadTextSlice

 ret->Width = textwidth(d->s)
 'split(d->s, d->lines())
 
 return ret
end function

'All arguments default to no-change
Sub ChangeTextSlice(byval sl as Slice ptr,_
                      s as string=CHR(1) & CHR(255),_
                      byval col as integer=colInvalid,_
                      byval outline as integer=-2,_
                      byval wrap as integer=-2,_
                      byval bgcol as integer=colInvalid)
 if sl = 0 then debug "ChangeTextSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slText)
 with *sl->TextData
  if s <> CHR(1) & CHR(255) then
   .s = s
  end if
  if col <> colInvalid then
   .col = col
  end if
  if bgcol <> colInvalid then
   .bgcol = bgcol
  end if
  if outline > -2 then
   .outline = outline <> 0
  end if
  if wrap > -2 then
   .wrap = wrap <> 0
  end if
 end with
 UpdateTextSlice sl
end sub

Function GetTextSliceString(byval sl as Slice ptr) as string
 if sl = 0 then debug "GetTextSliceString null ptr" : return ""
 ASSERT_SLTYPE(sl, slText, "")
 return sl->TextData->s
End Function

'--Sprite-----------------------------------------------------------------

Declare Sub LoadAssetSprite(sl as Slice ptr, warn_if_missing as bool = YES)

' Frees any memory held by a sprite, leaving in a consistent state, but does not reset its type and other data
Sub UnloadSpriteSlice(byval sl as Slice ptr)
 with *sl->SpriteData
  unload_sprite_and_pal .img
  if .assetfile then
   *.assetfile = ""      ' Frees the string contents
   deallocate .assetfile ' Free the string descriptor
   .assetfile = NULL
  end if
  .loaded = NO
 end with
End Sub

Sub DisposeSpriteSlice(byval sl as Slice ptr)
 if sl = 0 orelse sl->SpriteData = 0 then exit sub
 UnloadSpriteSlice sl
 delete sl->SpriteData
 sl->SpriteData = 0
end sub

' Load a sprite's Frame and Palette16, so that its size is known
' Afterwards, the Frame ptr won't be NULL, unless sl->SliceData doesn't exist.
Sub LoadSpriteSliceImage(byval sl as Slice ptr, warn_if_missing as bool = NO)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 with *sl->SpriteData
  if .img.sprite then
   '.loaded is true
   'Check whether need to reload the graphic because it needs to be re-scaled (.scaled),
   if .scaled then
    if .img.sprite->Size <> sl->Size then .loaded = NO
   else
    'You can't resize a sprite slice, except by using Fill (for backcompat,
    'yuck). (CoverChildren isn't allowed on sprites.) But a spriteset's size can
    'change, or fill mode might change, so we need to reset its size. It's too
    'hard to tell whether a spriteset changed size without storing the old size,
    'so we just unconditionally reset the size here, which happens on every
    'DrawSpriteSlice call.
    'This may not be the ideal place to put this code.
    'NOTE: it's this code alone that prevents a Sprite slice's size from being changed
    'in the slice editor. That's pretty ugly.
    if sl->Fill /'orelse sl->CoverChildren <> coverNone'/ then
     if sl->FillHoriz() = NO /'andalso (sl->CoverChildren and coverHoriz) = 0'/ then
      sl->Width = .img.sprite->w
     end if
     if sl->FillVert() = NO /'andalso (sl->CoverChildren and coverVert) = 0'/ then
      sl->Height = .img.sprite->h
     end if
    else  'sl->CoverChildren = coverNone and sl->Fill = NO and .scaled = NO and zoom = 1.
     sl->Size = .img.sprite->size
    end if
   end if
  end if

  if .loaded then exit sub
  if .spritetype = sprTypeFrame then  'This can happen if you clone a sprite, otherwise shouldn't
   LoadAssetSprite sl, warn_if_missing
  else
   load_sprite_and_pal .img, .spritetype, .record, .pal   'Unloads old sprite/pal
  end if
  .loaded = YES  'Set YES even if loading failed, so we don't try again
  if .img.sprite then
   if .scaled then
    if .img.sprite->size <> sl->Size then
     'Becomes a 32-bit sprite
     frame_assign @.img.sprite, frame_scaled32(.img.sprite, sl->Width, sl->Height, master(), .img.pal)
     palette16_unload @.img.pal
    end if
   else
    sl->Size = .img.sprite->size
   end if
  end if
 end with
end sub

Sub DrawSpriteSlice(byval sl as Slice ptr, byval page as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 with *sl->SpriteData
  LoadSpriteSliceImage sl

  dim spr as Frame ptr
  dim have_copy as bool = NO
  spr = .img.sprite
  if spr = 0 then
   showbug "null slice .sprite ptr"
   sl->Visible = NO  'prevent error loop
   exit sub
  end if

  if .frame >= spr->arraylen or .frame < 0 then
   showbug "out of range frame " & .frame & " for slice " & SlicePath(sl)
   .frame = 0
  end if
  spr = @spr[.frame]

  if spr->image = NULL then
   'This is a Surface-backed slice, and flipping and dissolving aren't supported yet.
   .flipHoriz = NO
   .flipVert = NO
   .dissolving = NO
  end if

  'some redesign needed to prevent this continous flipping
  if .flipHoriz then
   if have_copy = NO THEN spr = frame_duplicate(spr)
   have_copy = YES
   frame_flip_horiz(spr)
  end if
  if .flipVert then
   if have_copy = NO THEN spr = frame_duplicate(spr)
   have_copy = YES
   frame_flip_vert(spr)
  end if

  'Rotozooming
  if .rotate orelse .zoom <> 1. then
   'FIXME: have_copy leak! (Ought to add h/v flip options to the rotozoomer to avoid copies)
   'Negate rotation so angle is clockwise
   spr = frame_rotozoom(spr, .img.pal, -.rotate, .zoom, .zoom, .rz_smooth)
   if spr = 0 then exit sub
   have_copy = YES
   if .dissolving then
    'Kludge: frame_rotozoom returns an 8-bit Surface-backed Frame, which frame_dissolved doesn't yet support.
    'Convert into a normal Frame, but only if needed, because it does a copy.
    frame_drop_surface spr
   end if
  end if

  dim drew as bool = NO

  if .dissolving then
   dim dtime as integer = .d_time
   if dtime = -1 then dtime = (sl->Width + sl->Height) / 10
   if dtime > 0 then
    dim dtick as integer
    if .d_back then
     dtick = dtime - .d_tick
    else
     dtick = .d_tick
    end if
    frame_draw_dissolved spr, .img.pal, sl->ScreenX, sl->ScreenY, .trans, vpages(page), .drawopts, dtime, dtick, .d_type
    drew = YES
    'FIXME: d_auto shouldn't take effect here, but when the slice is 'advanced'
    if .d_auto then
     .d_tick += 1
     if .d_tick > dtime then
      .dissolving = NO
      .d_auto = NO
     end if
    end if
   end if
  end if

  'static watch as SmoothedTimer
  'if .drawopts.with_blending then watch.start()

  if drew = NO then
   frame_draw spr, .img.pal, sl->ScreenX, sl->ScreenY, .trans, page, .drawopts
  end if

  'if .drawopts.with_blending then watch.stop_and_print()

  if have_copy then
   frame_unload(@spr)
  end if
 end with
end sub

' Actually load the asset for a sprite slice, or load a placeholder Frame
' if it's missing. If the assetfile is "", always loads a placeholder with no warning.
Local Sub LoadAssetSprite(sl as Slice ptr, warn_if_missing as bool = YES)
 BUG_IF(sl = 0, "null ptr")

 with *sl->SpriteData
  frame_unload(@.img.sprite)
  palette16_unload(@.img.pal)
  .record = 0
  .pal = -1  'No palette anyway
  .paletted = NO
  .frame = 0
  .loaded = YES  'Even if an error occurs, we create a Frame

  dim assetfile as string
  if .assetfile then assetfile = *.assetfile
  dim filename as string = finddatafile(assetfile, NO)  'Handle missing file below
  if len(filename) then
   if .load_asset_as_32bit then
    .img.sprite = image_import_as_frame_32bit(filename)
   else
    dim transp_color as RGBcolor  'Black. TODO: this should be stored in dat and customisabled
    .img.sprite = image_import_as_frame_8bit(filename, master(), , transp_color)
   end if
  end if
  if .img.sprite then
   if .scaled = NO then
    sl->Width = .img.sprite->w
    sl->Height = .img.sprite->h
   end if
  else
   if warn_if_missing andalso len(assetfile) then
    visible_debug "Data file " & iif(len(filename), "corrupt", "missing") _
                  & !":\ndata/" & assetfile _
                  & !"\nThe OHRRPGCE apparently isn't installed properly. Try reinstalling, or report this error."
   end if
   ' Draw an X (the width and height were hopefully loaded from a .slice file)
   .img.sprite = frame_new(sl->Width, sl->Height, , YES)
   drawline .img.sprite, 0, 0, sl->Width - 1, sl->Height - 1, uilook(uiSelectedItem)
   drawline .img.sprite, sl->Width - 1, 0, 0, sl->Height - 1, uilook(uiSelectedItem)
  end if
 end with
End Sub

' Turn a sprite slice into an 'asset' sprite, meaning it is loaded from an image in the data/ dir.
' assetname should be the name of a file in data/, or can be blank if that isn't decided yet (in slice editor).
Sub SetSpriteToAsset(sl as Slice ptr, assetfile as string, warn_if_missing as bool = YES)
 BUG_IF(sl = 0, "null ptr")

 'Create temp copy, in case assetfile is dat->assetfile, which we're about to delete
 dim filename as string = assetfile
 UnloadSpriteSlice sl
 with *sl->SpriteData
  .spritetype = sprTypeFrame
  .assetfile = callocate(sizeof(string))
  *.assetfile = filename
 end with
 'LoadAssetSprite sl, warn_if_missing
 LoadSpriteSliceImage sl, warn_if_missing
End Sub

' Provide an external image for this sprite slice, instead of one of the game's sprites.
' fr will be unloaded when the sprite is deleted. Use frame_reference() to avoid this.
' Either pass pal16: an already loaded Palette16 (which will be freed when the slice is deleted),
' or pal: a palette number, or neither if the Frame is not paletted (indexes the master palette directly).
Sub SetSpriteToFrame(sl as Slice ptr, fr as Frame ptr, pal16 as Palette16 ptr = NULL, pal as integer = -2)
 if sl = 0 then debug "SetSpriteToFrame null ptr": exit sub

 if pal = -1 then showbug "SetSpriteToFrame: a default palette can't be used" : pal = 0

 UnloadSpriteSlice sl
 with *sl->SpriteData
  .spritetype = sprTypeFrame
  .img.sprite = fr
  .record = -1
  .frame = 0
  '.trans preserved

  if pal16 then
   .img.pal = pal16
   .paletted = YES
  elseif pal <> -2 then
   .img.pal = palette16_load(pal)
   .paletted = YES
  else
   .paletted = NO
  end if
  .pal = pal

  if fr then
   sl->Width = fr->w
   sl->Height = fr->h
   .loaded = YES
  end if

 end with
End Sub

'Cloning sprTypeFrame sprite slices does not work!
Sub CloneSpriteSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneSpriteSlice null ptr": exit sub
 dim dat as SpriteSliceData Ptr = sl->SpriteData
 with *cl->SpriteData
  .spritetype = dat->spritetype
  if dat->assetfile then
   .assetfile = callocate(sizeof(string))
   *.assetfile = *dat->assetfile
  end if
  .load_asset_as_32bit = dat->load_asset_as_32bit
  .record     = dat->record
  .paletted   = dat->paletted
  .pal        = dat->pal
  .frame      = dat->frame
  .flipHoriz  = dat->flipHoriz
  .flipVert   = dat->flipVert
  .scaled     = dat->scaled
  .trans      = dat->trans
  .dissolving = dat->dissolving
  .d_type     = dat->d_type
  .d_time     = dat->d_time
  .d_tick     = dat->d_tick
  .d_back     = dat->d_back
  .d_auto     = dat->d_auto
  .drawopts   = dat->drawopts
  '.img and .loaded remain NULLs, NO  (for no reason. FIXME: what about Frame sprites?)
 end with
end sub

Local Sub SaveDrawOpts(drawopts as DrawOptions, node as Reload.Nodeptr)
 with drawopts
  SaveProp node, "blend", cast(bool, .with_blending)
  SaveProp node, "blend_mode", .blend_mode
  if .opacity < 1. then SavePropAlways node, "opacity", .opacity
  'scale: not saved yet
 end with
end sub

Sub SaveSpriteSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 BUG_IF(sl = 0 or node = 0, "null ptr")
 dim dat as SpriteSliceData Ptr
 dat = sl->SpriteData
 SavePropAlways node, "sprtype", dat->spritetype
 if dat->spritetype = sprTypeFrame then
  ' If it's not an asset sprite, then the Frame came from an unknown source and can't be saved
  BUG_IF(dat->assetfile = NULL, "tried to save Frame sprite")
  SavePropAlways node, "asset", *dat->assetfile
  SaveProp node, "32bit_asset", dat->load_asset_as_32bit
 else
  SavePropAlways node, "rec", dat->record
  if dat->paletted then
   SavePropAlways node, "pal", dat->pal
  end if
  SaveProp node, "frame", dat->frame
 end if
 SaveProp node, "fliph", dat->flipHoriz
 SaveProp node, "flipv", dat->flipVert
 SaveProp node, "scaled", dat->scaled
 SavePropAlways node, "trans", dat->trans
 SaveProp node, "dissolving", dat->dissolving
 SaveProp node, "d_type", dat->d_type
 SaveProp node, "d_time", dat->d_time
 SaveProp node, "d_tick", dat->d_tick
 SaveProp node, "d_back", dat->d_back
 SaveProp node, "d_auto", dat->d_auto
 SaveDrawOpts dat->drawopts, node
end sub

Local Sub LoadDrawOpts(drawopts as DrawOptions, node as Reload.Nodeptr)
 with drawopts
  .with_blending = LoadProp(node, "blend")
  .blend_mode = bound(LoadProp(node, "blend_mode"), 0, blendModeLAST)
  .opacity = bound(LoadPropFloat(node, "opacity", 1.0), 0., 1.)
  'scale: not loaded yet
 end with
end sub

Sub LoadSpriteSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadSpriteSlice null ptr": exit sub
 dim dat as SpriteSliceData Ptr
 dat = sl->SliceData
 dat->spritetype = LoadProp(node, "sprtype")
 if dat->spritetype < sprTypeFirst or dat->spritetype > sprTypeLastPickable then
  reporterr "LoadSpriteSlice: Unknown type " & dat->spritetype, serrError
 end if
 dat->record     = LoadProp(node, "rec")
 dat->paletted   = sprite_sizes(dat->spritetype).paletted
 dat->pal        = LoadProp(node, "pal", -1)
 dat->frame      = LoadProp(node, "frame")
 dat->flipHoriz  = LoadProp(node, "fliph")
 dat->flipVert   = LoadProp(node, "flipv")
 dat->trans      = LoadPropBool(node, "trans", YES)
 dat->scaled     = LoadPropBool(node, "scaled")
 dat->dissolving = LoadPropBool(node, "dissolving")
 dat->d_type     = bound(LoadProp(node, "d_type"), 0, dissolveTypeMax)
 dat->d_time     = LoadProp(node, "d_time")
 dat->d_tick     = bound(LoadProp(node, "d_tick"), -1, large(dat->d_time + 1, 0))
 dat->d_back     = LoadPropBool(node, "d_back")
 dat->d_auto     = LoadPropBool(node, "d_auto")
 LoadDrawOpts dat->drawopts, node

 if dat->spritetype = sprTypeFrame then
  dat->load_asset_as_32bit = LoadPropBool(node, "32bit_asset")
  SetSpriteToAsset sl, LoadPropStr(node, "asset")
 else
  'Load the sprite already in order to ensure the size is correct. This could be
  'skipped, since the slice was probably saved with the correct size...
  LoadSpriteSliceImage sl
 end if
End Sub

Function NewSpriteSlice(byval parent as Slice ptr, byref dat as SpriteSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as SpriteSliceData ptr = new SpriteSliceData
 *d = dat

 'Set non-zero defaults
 d->pal = -1
 d->trans = YES
 d->paletted = YES
 d->zoom = 1.

 ret->SliceType = slSprite
 ret->SliceData = d
 ret->Draw = @DrawSpriteSlice
 ret->Dispose = @DisposeSpriteSlice
 ret->Clone = @CloneSpriteSlice
 ret->Save = @SaveSpriteSlice
 ret->Load = @LoadSpriteSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeSpriteSlice(byval sl as Slice ptr,_
                      byval spritetype as SpriteType = sprTypeInvalid,_
                      byval record as integer=-1,_
                      byval pal as integer = -2,_
                      byval frame as integer = -1,_
                      byval fliph as integer = -2,_
                      byval flipv as integer = -2,_
                      byval trans as integer = -2)
 if sl = 0 then debug "ChangeSpriteSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSprite)
 with *sl->SpriteData
  if spritetype <> sprTypeInvalid then
   BUG_IF(spritetype < sprTypeFirstLoadable orelse spritetype > sprTypeLastLoadable, "Invalid type " & spritetype)
   .spritetype = spritetype
   .loaded = NO
  end if
  if record >= 0 then
   .record = record
   .loaded = NO
  end if
  if pal >= -1 then
   if .paletted = NO then
    reporterr "Attempt to set a palette (" & pal & ") on an unpaletted " & sprite_sizes(.spritetype).name & " sprite slice"
   else
    .pal = pal
    .loaded = NO
   end if
  end if
  if frame >= 0 then .frame = frame
  if fliph > -2 then .flipHoriz = (fliph <> 0)
  if flipv > -2 then .flipVert = (flipv <> 0)
  if trans > -2 then .trans = (trans <> 0)
  if .loaded = NO then
   unload_sprite_and_pal .img
   SpriteSliceUpdate sl
  end if
 end with
end sub

'Called after .spritetype, .record, .palette or .assetfile is changed.
'Internal use only - normally you should call ChangeSpriteSlice instead
Sub SpriteSliceUpdate(sl as Slice ptr)
 if sl = 0 orelse sl->SliceData = 0 orelse sl->SliceType <> slSprite then
  debug "SpriteSliceUpdate: invalid ptr"
  exit sub
 end if

 with *sl->SpriteData
  .paletted = sprite_sizes(.spritetype).paletted   'Note this doesn't apply when using SetSpriteToFrame
  if .spritetype = sprTypeFrame then
   ' Aside from reloading if edited, .assetfile is initially NULL
   ' when switching to sprTypeFrame, so needs to be initialised to "".
   ' Note that if you change to a different sprite type, .assetfile
   ' will still be there, but won't be used or saved
   dim assetfile as string = iif(.assetfile, *.assetfile, "")
   SetSpriteToAsset sl, assetfile, NO

   'If it's a 32-bit sprite (is converted to 32-bit when scaled)...
   if .load_asset_as_32bit orelse .scaled then
    'Transparent 32 bit Surfaces not yet supported
    .trans = NO
    'frame_flip_* and frame_dissolved don't support Surfaces either
    .dissolving = NO
    .flipHoriz = NO
    .flipVert = NO
   end if
  else
   .record = small(.record, sprite_sizes(.spritetype).lastrec)

   'Load the sprite image (and palette) immediately, so that the size of the slice
   'and number of frames are correct
   .loaded = NO  'Force reload
   LoadSpriteSliceImage sl

   .frame = small(.frame, SpriteSliceNumFrames(sl) - 1)
  end if
 end with
end sub

'Cause the sprite to be scaled/stretched to a certain size.
'TODO: once scaled sprites are available in games, uncomment the relevant code in valid_resizeable_slice.
'Size can't be negative (Maybe handle negatives by setting flipVert and flipHoriz?)
Sub ScaleSpriteSlice(sl as Slice ptr, size as XYPair)
 if sl = 0 then debug "ScaleSpriteSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSprite)
 with *sl->SpriteData
  .loaded = NO
  unload_sprite_and_pal .img
  .scaled = YES
  sl->Size = size
  'Reload so that number of frames is known
  LoadSpriteSliceImage sl
 end with
end sub

Sub DissolveSpriteSlice(byval sl as Slice ptr, byval dissolve_type as integer, byval over_ticks as integer=-1, byval start_tick as integer=0, byval backwards as bool=NO, byval auto_animate as bool=YES)
 if sl = 0 then debug "DissolveSpriteSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSprite)
 with *sl->SpriteData
  .dissolving = YES
  '(Note that the bounds checking here and in LoadSpriteSlice is bypassed by the slice editor)
  .d_type = bound(dissolve_type, 0, dissolveTypeMax)
  .d_time = over_ticks
  'Allow -1 (when backwards) and length+1 so that can set Vapourise and Phase Out animations to a totally blank state.
  .d_tick = bound(start_tick, -1, large(over_ticks + 1, 0))
  .d_back = backwards <> 0
  .d_auto = auto_animate <> 0
 end with
end sub

Sub CancelSpriteSliceDissolve(sl as Slice ptr)
 if sl = 0 then debug "CancelSpriteSliceDissolve null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSprite)
 with *sl->SpriteData
  .dissolving = NO
  .d_auto = NO
 end with
end sub

Function SpriteSliceIsDissolving(byval sl as Slice ptr, byval only_auto as bool=YES) as bool
 if sl = 0 then debug "SpriteSliceIsDissolving null ptr" : return NO
 if sl->SliceType <> slSprite then return NO  'Not an error
 with *sl->SpriteData
  if only_auto andalso not .d_auto then return NO
  return .dissolving
 end with
end function

Function SpriteSliceNumFrames(sl as Slice ptr) as integer
 if sl = 0 orelse sl->SliceData = 0 then
  debug "SpriteSliceNumFrames: invalid ptr"
  return 0
 end if
 ASSERT_SLTYPE(sl, slSprite, 0)

 with *sl->SpriteData
  if .loaded = NO then LoadSpriteSliceImage sl
  if .img.sprite = 0 then return 0
  return .img.sprite->arraylen
 end with
end function

'--Map-----------------------------------------------------------------

Sub DisposeMapSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 delete sl->MapData
 sl->MapData = 0
end sub

Sub DrawMapSlice(byval sl as Slice ptr, byval page as integer)
 ' MapSlices are sadly exceptions to the slice system. Their size is ignored.
 ' Instead, they are drawn to cover the whole current clipping region.
 ' Their position corresponds to the camera offset.
 ' Map layer 0 acts as if its size is actually infinitely large, including extending
 ' up/left of 0. The map edge tile is drawn outside the map if the map is set to 'Crop',
 ' despite it appearing as 'N/A' in the map editor.
 ' Higher map layers are actually drawn to match their nominal size, unless the map
 ' is set to 'Wrap', in which case they fill the whole clipping region.

 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 with *sl->MapData
  if .tiles = 0 then exit sub 'tilemap ptr null if the layer doesn't exist. This slice probably shouldn't either.
  if .tileset = 0 then exit sub 'quit silently on a null tileset ptr
  '2nd, 3rd arguments to drawmap are "camera position" of upper left of the screen.

  'static watch as SmoothedTimer
  'if .drawopts.with_blending then watch.start()

  drawmap *.tiles, sl->ScreenX * -1, sl->ScreenY * -1, .tileset, page, .transparent, .overlay, .pass, , , , .drawopts

  'if .drawopts.with_blending then watch.stop_and_print()
 end with
end sub

Sub SaveMapSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveMapSlice null ptr": exit sub
 'FIXME: current MapSlice impl. has no savable properties
end sub

Sub LoadMapSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadMapSlice null ptr": exit sub
 'FIXME: current MapSlice impl. has no savable properties
End Sub

Function NewMapSlice(byval parent as Slice ptr, byref dat as MapSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as MapSliceData ptr = new MapSliceData
 *d = dat

 ret->SliceType = slMap
 ret->SliceData = d
 ret->Draw = @DrawMapSlice
 ret->Dispose = @DisposeMapSlice
 '--No cloning support for Map slice yet
 'ret->Clone = @CloneMapSlice
 ret->Save = @SaveMapSlice
 ret->Load = @LoadMapSlice
 
 return ret
end function

Sub ChangeMapSliceTileset(byval sl as Slice ptr, byval tileset as TilesetData ptr)
 if sl = 0 then debug "ChangeMapSliceTileset null ptr" : exit sub
 ASSERT_SLTYPE(sl, slMap)
 sl->MapData->tileset = tileset
end sub

Sub ChangeMapSlice(byval sl as Slice ptr,_
                   byval tiles as TileMap ptr = cast(TileMap ptr, 1),_
                   byval pass as TileMap ptr = cast(TileMap ptr, 1),_
                   byval transparent as integer=-2,_
                   byval overlay as integer=-1)
 if sl = 0 then debug "ChangeMapSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slMap)
 with *sl->MapData
  if tiles <> cast(TileMap ptr, 1) then
   .tiles = tiles
   if tiles = NULL then
    sl->Width = 0
    sl->Height = 0
   else
    sl->Size = tiles->size * 20
   end if
  end if
  if tiles <> cast(TileMap ptr, 1) then
   '--passmap. If this slice doesn't draw overhead tiles, can set this to NULL
   .pass = pass
  end if
  if transparent >= -1 then
   .transparent = (transparent <> 0) 'boolean
  end if
  if overlay >= 0 and overlay <= 2 then
   '--used for backcompat with overhead tiles on layer 0
   .overlay = overlay 'valid values 0, 1, 2
  end if
 end with
end sub

'--Grid-------------------------------------------------------------------
Sub DisposeGridSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->GridData = 0 then exit sub
 delete sl->GridData
 sl->GridData = 0
end sub

Sub DrawGridSlice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as GridSliceData ptr = sl->SliceData

 if dat->show then
  drawbox sl->screenx, sl->screeny, sl->width, sl->height, uilook(uiText), 1, p
  dim w as integer = sl->width \ large(1, dat->cols)
  dim h as integer = sl->height \ large(1, dat->rows)
  for row as integer = 0 to dat->rows - 1
   for col as integer = 0 to dat->cols - 1
    'drawbox sl->screenx + col * w, sl->screeny + row * h, w, h, uilook(uiText), 1, p
    rectangle sl->screenx + col * w, sl->screeny + row * h, w, 1, uilook(uiText), p
    rectangle sl->screenx + col * w, sl->screeny + row * h, 1, h, uilook(uiText), p
   next col
  next row
 end if
end sub

Sub CloneGridSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneGridSlice null ptr": exit sub
 dim dat as GridSliceData ptr = sl->GridData
 with *cl->GridData
  .cols = dat->cols
  .rows = dat->rows
  .show = dat->show
 end with
end sub

Sub SaveGridSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveGridSlice null ptr": exit sub
 DIM dat as GridSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "cols", dat->cols
 SavePropAlways node, "rows", dat->rows
 SavePropAlways node, "show", dat->show
End Sub

Sub LoadGridSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadGridSlice null ptr": exit sub
 dim dat as GridSliceData Ptr
 dat = sl->SliceData
 dat->cols = large(1, LoadProp(node, "cols", 1))
 dat->rows = large(1, LoadProp(node, "rows", 1))
 dat->show = LoadPropBool(node, "show")
End Sub

'Computes ScreenX/Y, and also sets the width/height if filling
Sub GridChildRefresh(byval par as Slice ptr, byval ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 if ch = 0 then debug "GridChildRefresh null ptr": exit sub
 if visibleonly andalso ch->Visible = NO then exit sub  'Don't need to exclude template slices

 dim dat as GridSliceData ptr = par->GridData
 dim w as integer = par->Width \ large(1, dat->cols)
 dim h as integer = par->Height \ large(1, dat->rows)
 '--Figure out which child this is
 if childindex < 0 then childindex = SliceIndexAmongSiblings(ch, template_slices_shown)
 dim xslot as integer = childindex mod large(1, dat->cols)
 dim yslot as integer = childindex \ large(1, dat->cols)

 dim support as RectType = any
 support.xy = par->ScreenPos + XY(par->paddingLeft + xslot * w, par->paddingTop + yslot * h)
 support.wide = w - par->paddingLeft - par->paddingRight
 support.high = h - par->paddingTop - par->paddingBottom
 RefreshChild ch, support
End sub

Sub GridChildDraw(byval s as Slice Ptr, byval page as integer)
 'NOTE: this Sub only handles the clipping of the children of a Grid slice which
 '      is set to clip. It might seem the logical place to position the children
 '      too, but that's in GridChildRefresh. Drawing
 '      and calculating position are independent.
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSliceRecurse which does null check it.

 if s->SliceType <> slGrid then debug "GridChildDraw illegal slice type": exit sub

 if s->Clip = NO then
  'no special behaviour
  'TODO: it would be better to call DefaultChildDraw in all cases, which we could do
  'if it called a method to get the support of each child.
  DefaultChildDraw s, page
  exit sub
 end if
 
 with *s
  'if .ChildrenRefresh then .ChildrenRefresh(s)  'Always NULL

  dim dat as GridSliceData ptr = .GridData
  dim w as integer = .Width \ large(1, dat->cols)
  dim h as integer = .Height \ large(1, dat->rows)

  'draw the slice's children
  dim ch as Slice ptr = .FirstChild
  dim childindex as integer = 0
  for yslot as integer = 0 to dat->rows - 1
   for xslot as integer = 0 to dat->cols - 1
    while ch andalso ShouldSkipSlice(ch)  'Skip templates
     ch = ch->NextSibling
    wend
    if ch = 0 then exit for, for

    dim clippos as XYPair
    clippos.x = .ScreenX + xslot * w
    clippos.y = .ScreenY + yslot * h
    dim rememclip as ClipState = get_cliprect()
    if shrinkclip(clippos.x + .paddingLeft, _
                  clippos.y + .paddingTop, _
                  clippos.x + w - .paddingRight - 1, _
                  clippos.y + h - .paddingBottom - 1, _
                  vpages(page)) then
     'This child isn't clipped
     DrawSliceRecurse(ch, page, childindex)
    end if

    get_cliprect() = rememclip

    ch = ch->NextSibling
    childindex += 1
   next
  next
 end with
End Sub

Function NewGridSlice(byval parent as Slice ptr, byref dat as GridSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as GridSliceData ptr = new GridSliceData
 *d = dat
 '--Set non-zero defaults here
 d->cols = 1
 d->rows = 1
 
 ret->SliceType = slGrid
 ret->SliceData = d
 ret->Draw = @DrawGridSlice
 ret->Dispose = @DisposeGridSlice
 ret->Clone = @CloneGridSlice
 ret->Save = @SaveGridSlice
 ret->Load = @LoadGridSlice
 ret->ChildRefresh = @GridChildRefresh
 ret->ChildDraw = @GridChildDraw

 return ret
end function

Function GetGridSliceData(byval sl as Slice ptr) as GridSliceData ptr
 if sl = 0 then debug "GetGridSliceData null ptr": return 0
 return sl->SliceData
End Function

'All arguments default to no-change
Sub ChangeGridSlice(byval sl as Slice ptr,_
                      byval rows as integer=0,_
                      byval cols as integer=0,_
                      byval show as integer=-2)
 if sl = 0 then debug "ChangeGridSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slGrid)
 dim dat as GridSliceData Ptr = sl->SliceData
 if rows > 0 then
  dat->rows = rows
 end if
 if cols > 0 then
  dat->cols = cols
 end if
 if show > -2 then
  dat->show = show
 end if
end sub

'--Layout-----------------------------------------------------------------

Sub DisposeLayoutSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as LayoutSliceData ptr = sl->SliceData
 delete dat
 sl->SliceData = 0
end sub

'Skip over hidden slices as needed
Function LayoutSliceData.SkipForward(ch as Slice ptr) as Slice ptr
' ch = ch->NextSibling
 while ch andalso ShouldSkipSlice(ch, skip_hidden)
  ch = ch->NextSibling
 wend
 return ch
end Function

'Calculate offsets of children in a row along axis0, and the breadth of the row.
'Offsets don't including parent's padding.
Sub LayoutSliceData.SpaceRow(par as Slice ptr, first as Slice ptr, axis0 as integer, dir0 as integer, byref offsets as integer vector, byref breadth as integer)

 v_resize offsets, 0  'clear

 dim available_size as integer  'Max length of a row
 available_size = par->Size.n(axis0)
 if axis0 = 0 then
  available_size -= par->PaddingLeft + par->PaddingRight
 else
  available_size -= par->PaddingTop + par->PaddingBottom
 end if

 dim offset as integer = 0 ' along axis 0
 breadth = this.min_row_breadth  ' in axis 1

 first = SkipForward(first)
 dim as Slice ptr ch = first, last
 'Should never happen
 if ch = 0 then showbug "SpaceRow: no children" : exit sub

 while ch
  'Always place at least one child on each row
  if ch <> first then
   if offset + ch->Size.n(axis0) > available_size then
    'Can't fit any more on this row
    exit while
   end if
  end if

  dim temp as integer = dir0 * offset
  if dir0 = -1 then temp += available_size - ch->Size.n(axis0)
  v_append offsets, temp
  offset += ch->Size.n(axis0) + this.primary_padding
  breadth = large(breadth, ch->Size.n(1 xor axis0))

  ch = SkipForward(ch->NextSibling)
 wend
 dim is_last_row as bool = (ch = NULL)

 dim row_length as integer = offset - this.primary_padding
 dim extra_space as integer = available_size - row_length  'at the end of the row

 dim shift as integer = 0  'To add to each offset

 if v_len(offsets) > 1 andalso justified then
  '(This implies extra_space >= 0.)
  'Add extra spacing to cause the children to be spaced out across the row.
  dim spacing as double = extra_space / (v_len(offsets) - 1)
  if is_last_row and last_row_justified = NO then
   'Try to use the same amount of spacing as the row above
   spacing = small(spacing, _previous_row_spacing)
  end if
  _previous_row_spacing = spacing
  for idx as integer = 1 to v_len(offsets) - 1
   offsets[idx] += dir0 * int(idx * spacing)
  next
 else
  'Handle row_alignment, possibly shifting the whole row
  if dir0 = -1 then shift = -extra_space
  select case this.row_alignment
   case alignLeft:
   case alignMiddle: shift += extra_space \ 2
   case alignRight:  shift += extra_space
  end select
 end if

 'Handle parent padding in the primary dir
 select case this.primary_dir
  case dirUp, dirDown:    shift += par->PaddingTop
  case dirLeft, dirRight: shift += par->PaddingLeft
 end select

 if shift <> 0 then
  for idx as integer = 0 to v_len(offsets) - 1
   offsets[idx] += shift
  next
 end if
end Sub

Sub LayoutSliceData.Validate()
 if (this.primary_dir and 1) = (this.secondary_dir and 1) then
  'The directions aren't orthogonal
  this.secondary_dir = (this.primary_dir and 1) xor 1
 end if
 'Could do everything else too...
end Sub

'Layout slices work
Sub LayoutChildrenRefresh(byval par as Slice ptr)
 if par = 0 then debug "LayoutChildrenRefresh null ptr": exit sub

 dim dat as LayoutSliceData ptr
 dat = par->SliceData
 dat->Validate()

 dim as integer axis0, axis1 'primary and secondary axes: 0 or 1 for x or y
 dim as integer dir0, dir1  '1 if offsets are measured from top or left, -1 otherwise
 select case dat->primary_dir
  case dirLeft:  axis0 = 0 : axis1 = 1 : dir0 = -1
  case dirRight: axis0 = 0 : axis1 = 1 : dir0 = 1
  case dirUp:    axis0 = 1 : axis1 = 0 : dir0 = -1
  case dirDown:  axis0 = 1 : axis1 = 0 : dir0 = 1
 end select
 select case dat->secondary_dir
  case dirLeft:  dir1 = -1
  case dirRight: dir1 = 1
  case dirUp:    dir1 = -1
  case dirDown:  dir1 = 1
 end select

 dim offsets as integer vector
 v_new offsets

 dim offset as XYPair  'Offset of the next child, relative to par
 dim breadth as integer
 select case dat->secondary_dir
  case dirDown:  offset.Y = par->PaddingTop
  case dirUp:    offset.Y = par->Height - par->PaddingBottom
  case dirRight: offset.X = par->PaddingLeft
  case dirLeft:  offset.X = par->Width - par->PaddingRight
 end select

 dat->_previous_row_spacing = 0

 'Initial loop over children to process Fill (TODO: maybe clamp should be handled here as well)
 '(Unlike normal Fill, we don't ignore the .X/.Y members of a filling child)
 dim support as RectType = any
 support.wide = par->Width - par->paddingLeft - par->paddingRight
 support.high = par->Height - par->paddingTop - par->paddingBottom
 dim as Slice ptr ch = par->FirstChild
 while ch
  if ch->Fill andalso ch->IsShown then
   if ch->FillMode = sliceFillFull ORELSE ch->FillMode = sliceFillHoriz then
    ch->Width = support.wide
   end if
   if ch->FillMode = sliceFillFull ORELSE ch->FillMode = sliceFillVert then
    ch->Height = support.high
   end if
  end if
  ch = ch->NextSibling
 wend

 'Now calculate positions of each visible child
 ch = dat->SkipForward(par->FirstChild)
 while ch
  dat->SpaceRow(par, ch, axis0, dir0, offsets, breadth)

  'Iterate over each child on this row and set the final position
  for idx as integer = 0 to v_len(offsets) - 1
   with *ch
    offset.n(axis0) = offsets[idx]
    ' offset.n(axis0) = dir0 * offsets[idx]
    ' if dir0 = -1 then offset.n(axis0) += par->Size.n(axis0) - .Size.n(axis0)

    'The child's X/Y offsets it from its computed position,
    'but doesn't affect the positioning out of anything else.
    'Anchor, align points, clamping and Fill are ignored. Probably none of these make sense.
    .ScreenX = par->ScreenX + offset.x + .X
    .ScreenY = par->ScreenY + offset.y + .Y

    dim within_cell_space as integer = breadth - .Size.n(axis1) 'Always positive if Size is non-negative

    if dir1 = -1 then .ScreenPos.n(axis1) -= .Size.n(axis1) + within_cell_space
    select case dat->cell_alignment
     case alignLeft:
     case alignMiddle: .ScreenPos.n(axis1) += within_cell_space \ 2
     case alignRight:  .ScreenPos.n(axis1) += within_cell_space
    end select
   end with
   ch = dat->SkipForward(ch->NextSibling)

   ' ch = ch->NextSibling
   ' if skip_hidden then
   '  while ch andalso ch->IsShown = NO
   '   ch->ScreenPos = par->ScreenPos
   '   ch = ch->NextSibling
   '  wend
   ' end if
  next

  offset.n(axis1) += dir1 * (breadth + dat->secondary_padding)
 wend
 v_free offsets
end sub

Function GetLayoutSliceData(byval sl as Slice ptr) as LayoutSliceData ptr
 if sl = 0 then return 0
 return sl->SliceData
End Function

Sub CloneLayoutSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneLayoutSlice null ptr": exit sub
 *cl->LayoutData = *sl->LayoutData
end sub

Sub SaveLayoutSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveLayoutSlice null ptr": exit sub
 dim dat as LayoutSliceData ptr
 dat = sl->SliceData
 SavePropAlways node, "dir0", dat->primary_dir
 SavePropAlways node, "dir1", dat->secondary_dir
 SaveProp node, "padding0", dat->primary_padding
 SaveProp node, "padding1", dat->secondary_padding
 SaveProp node, "skip_hidden", dat->skip_hidden
 SaveProp node, "min_breadth", dat->min_row_breadth
 SaveProp node, "justified", dat->justified
 if dat->justified then
  SaveProp node, "last_row_justified", dat->last_row_justified
 else
  SaveProp node, "row_align", dat->row_alignment
 end if
 SaveProp node, "cell_align", dat->cell_alignment
End Sub

Sub LoadLayoutSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadLayoutSlice null ptr": exit sub
 dim dat as LayoutSliceData Ptr
 dat = sl->SliceData
 dat->primary_dir = LoadProp(node, "dir0")
 dat->secondary_dir = LoadProp(node, "dir1")
 dat->primary_padding = LoadProp(node, "padding0")
 dat->secondary_padding = LoadProp(node, "padding1")
 dat->skip_hidden = LoadPropBool(node, "skip_hidden")
 dat->justified = LoadPropBool(node, "justified")
 dat->last_row_justified = LoadPropBool(node, "last_row_justified")
 dat->min_row_breadth = LoadProp(node, "min_breadth")
 dat->cell_alignment = LoadProp(node, "cell_align")
 dat->row_alignment = LoadProp(node, "row_align")

 dat->Validate()
End Sub

Function NewLayoutSlice(byval parent as Slice ptr, byref dat as LayoutSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0

 dim d as LayoutSliceData ptr = new LayoutSliceData
 *d = dat

 ret->SliceType = slLayout
 ret->SliceData = d
 ret->Draw = NULL
 ret->Dispose = @DisposeLayoutSlice
 ret->Clone = @CloneLayoutSlice
 ret->Save = @SaveLayoutSlice
 ret->Load = @LoadLayoutSlice
 ret->ChildRefresh = @NullChildRefresh
 ret->ChildrenRefresh = @LayoutChildrenRefresh

 return ret
end function

'--Ellipse----------------------------------------------------------------

Sub DisposeEllipseSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->EllipseData = 0 then exit sub
 frame_unload @sl->EllipseData->frame
 delete sl->EllipseData
 sl->EllipseData = 0
end sub

Sub DrawEllipseSlice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 with *sl->EllipseData

  dim w as integer = ABS(sl->Width)
  dim h as integer = ABS(sl->Height)
  if .frame = 0 _
     ORELSE .last_draw_size.X <> w _
     ORELSE .last_draw_size.Y <> h _
     ORELSE .last_draw_bordercol <> .bordercol _
     ORELSE .last_draw_fillcol <> .fillcol then
   if sl->Width = 0 ORELSE sl->Height = 0 then exit sub
   frame_unload @.frame
   'debug "create new ellipse frame " & w & "x" & h
   .frame = frame_new(w, h, , YES)
   'fuzzyrect .frame, 0, 0, w, h, .fillcol, 37
   dim fillcol as integer = .fillcol
   'EllipseSliceData.fillcol 0 means transparent, while ellipse() uses -1 to mean transparent
   'NOTE: Drawing bordercol or fillcol 0 will be transparent anyway, because when .frame
   'gets drawn to the videopage, colour 0 counts as transparent. So fillcol=-1 is just an optimisation.
   if fillcol = 0 then fillcol = -1 else fillcol = ColorIndex(fillcol)
   dim saveclip as ClipState = get_cliprect()
   ellipse .frame, w / 2 - 0.5, h / 2 - 0.5 , w / 2 - 0.5, ColorIndex(.bordercol), fillcol, h / 2 - 0.5
   get_cliprect() = saveclip
   .last_draw_size.X = w
   .last_draw_size.Y = h
   .last_draw_bordercol = .bordercol
   .last_draw_fillcol = .fillcol
  end if

  BUG_IF(.frame = 0, "null frame ptr")

  frame_draw .frame, , small(sl->screenX, sl->screenX + sl->Width), small(sl->screenY, sl->screenY + sl->Height), , p

' ellipse vpages(p), small(sl->screenX, sl->screenX + sl->Width) + w / 2 - 0.5, small(sl->screenY, sl->screenY + sl->Height) + h / 2 - 0.5 , w / 2 - 0.5, .bordercol, fillcol, h / 2 - 0.5
 end with
end sub

Sub CloneEllipseSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneEllipseSlice null ptr": exit sub
 dim dat as EllipseSliceData Ptr
 dat = sl->EllipseData
 with *cl->EllipseData
  .bordercol  = dat->bordercol
  .fillcol    = dat->fillcol
  '.last_draw_* left at zero to force a redraw
  '.frame will be populated on next draw
 end with
end sub

Sub SaveEllipseSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveEllipseSlice null ptr": exit sub
 DIM dat as EllipseSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "bordercol", dat->bordercol
 SavePropAlways node, "fillcol", dat->fillcol
end sub

Sub LoadEllipseSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadEllipseSlice null ptr": exit sub
 dim dat as EllipseSliceData Ptr
 dat = sl->SliceData
 dat->bordercol = LoadProp(node, "bordercol")
 dat->fillcol   = LoadProp(node, "fillcol")
End Sub

Function NewEllipseSlice(byval parent as Slice ptr, byref dat as EllipseSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as EllipseSliceData ptr = new EllipseSliceData
 *d = dat

 'Set defaults
 d->bordercol = 0
 d->fillcol = 0
 
 ret->SliceType = slEllipse
 ret->SliceData = d
 ret->Draw = @DrawEllipseSlice
 ret->Dispose = @DisposeEllipseSlice
 ret->Clone = @CloneEllipseSlice
 ret->Save = @SaveEllipseSlice
 ret->Load = @LoadEllipseSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeEllipseSlice(byval sl as Slice ptr,_
                      byval bordercol as integer=colInvalid,_
                      byval fillcol as integer=colInvalid)
 if sl = 0 then debug "ChangeEllipseSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slEllipse)
 with *sl->EllipseData
  if bordercol <> colInvalid then
   .bordercol = bordercol
  end if
  if fillcol <> colInvalid then
   .fillcol = fillcol
  end if
 end with
end sub

'--Scroll--------------------------------------------------------------

Sub DisposeScrollSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->ScrollData = 0 then exit sub
 delete sl->ScrollData
 sl->ScrollData = 0
end sub

'Computes the bounding box of all visible descendents of a slice, up to a maximum
'check_depth (1 means just children, 2 is grandchildren, etc, 0 is infinite).
'This function works for any type of slice
Sub CalcSliceContentsSize(sl as Slice ptr, byref min as XYPair, byref max as XYPair, check_depth as integer, cur_depth as integer=0)
 if cur_depth = 0 then
  '0,0 might not be in the min-max range of the contents, so initialise
  min = sl->ScreenPos
  max = sl->ScreenPos + sl->Size
 else
  min.x = small(min.x, sl->ScreenX)
  min.y = small(min.y, sl->ScreenY)
  max.x = large(max.x, sl->ScreenX + sl->Width)
  max.y = large(max.y, sl->ScreenY + sl->Height)
 end if
 if not (check_depth = 0 orelse cur_depth < check_depth) then exit sub

 dim ch as Slice ptr = sl->FirstChild
 do while ch
  if ch->IsShown then
   CalcSliceContentsSize ch, min, max, check_depth, cur_depth + 1
  end if
  ch = ch->NextSibling
 Loop
End Sub

Sub ScrollChildDraw(byval sl as Slice ptr, byval p as integer)
 'NOTE: draws the scrollbars *after* all children have drawn, which is in
 '      stark contrast to how most other slices are drawn.
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSliceRecurse which does null check it.

 'First draw the children normally
 DefaultChildDraw sl, p

 'Then proceed with the scrollbars
 dim dat as ScrollSliceData ptr = sl->SliceData

 dim min as XYPair
 dim max as XYPair
 CalcSliceContentsSize(sl, min, max, dat->check_depth)

 dim axis as integer
 dim other as integer

 for axis = 0 to 1 '0=Horiz 1=Vert
  other = axis XOR 1

  dim off as integer = sl->ScreenPos.n(axis) - min.n(axis)
  dim total as integer = max.n(axis) - min.n(axis)

  if total > sl->Size.n(axis) then
   dim sbar as RectType
   dim slider as RectType
   with sbar
    .topleft.n(axis) = sl->ScreenPos.n(axis)
    .topleft.n(other) = sl->ScreenPos.n(other) + sl->Size.n(other)
    .size.n(axis) = sl->Size.n(axis)
    .size.n(other) = 4
   end with
   rectangle vpages(p), sbar, boxlook(dat->style).bgcol
   with slider
    .topleft = sbar.topleft
    .topleft.n(axis) += sbar.size.n(axis) / total * off
    .size.n(axis) = sbar.size.n(axis) / total * (sl->Size.n(axis) + 1)
    .size.n(other) = 4
   end with
   rectangle vpages(p), slider, boxlook(dat->style).edgecol
  end if
 next axis

end sub

Sub CloneScrollSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneScrollSlice null ptr": exit sub
 dim dat as ScrollSliceData Ptr
 dat = sl->ScrollData
 with *cl->ScrollData
  .style       = dat->style
  .check_depth = dat->check_depth
 end with
end sub

Sub SaveScrollSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveScrollSlice null ptr": exit sub
 DIM dat as ScrollSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "style", dat->style
 SaveProp node, "check_depth", dat->check_depth
End Sub

Sub LoadScrollSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadScrollSlice null ptr": exit sub
 dim dat as ScrollSliceData Ptr
 dat = sl->SliceData
 dat->style = LoadProp(node, "style", 0)
 dat->check_depth = LoadProp(node, "check_depth")
End Sub

Function NewScrollSlice(byval parent as Slice ptr, byref dat as ScrollSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 '--override the default value of Clip
 ret->Clip = YES
 
 dim d as ScrollSliceData ptr = new ScrollSliceData
 *d = dat
 '--Set non-zero defaults here
 'if there were any
 
 ret->SliceType = slScroll
 ret->SliceData = d
 ret->ChildDraw = @ScrollChildDraw
 ret->Dispose = @DisposeScrollSlice
 ret->Clone = @CloneScrollSlice
 ret->Save = @SaveScrollSlice
 ret->Load = @LoadScrollSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeScrollSlice(byval sl as Slice ptr,_
                      byval style as integer=-1,_
                      byval check_depth as integer=-1)
 if sl = 0 then debug "ChangeScrollSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slScroll)
 with *sl->ScrollData
  if style >= 0 then
   .style = style
  end if
  if check_depth >= 0 then
   .check_depth = check_depth
  end if
 end with
end sub

Sub ScrollAllChildren(byval sl as Slice ptr, byval xmove as integer, byval ymove as integer)
 'Shift all children's X/Y by an offset.
 'Doesn't work on Grid slices, which ignore X/Y, but should work on Layout.
 'This is intended for ScrollSlice, but can actually work on any type.
 if sl = 0 then debug "ScrollAllChildren: null scroll slice ptr": exit sub
 if sl->SliceType = slGrid then reporterr "ScrollAllChildren: can't scroll a Grid slice": exit sub
 dim ch as Slice ptr = sl->FirstChild
 do while ch
  'Filling slices ignore X/Y, so don't cause X/Y to go crazy
  if ch->FillHoriz = NO then ch->X += xmove
  if ch->FillVert = NO then ch->Y += ymove
  ch = ch->NextSibling
 loop
end sub

Sub ScrollToChild(byval sl as Slice ptr, byval desc as Slice ptr, byval apply_padding as bool = YES)
 'Similar to SliceClamp, but shifts all children of sl so that desc (which is any desendent
 'of sl) is within the bounds of sl. Ignores padding if apply_padding=NO.
 'This is intended for ScrollSlice, but can actually work on any type except Grid.
 if sl = 0 then debug "ScrollToChild: null scroll slice ptr": exit sub
 if desc = 0 then debug "ScrollToChild: null descendent slice ptr": exit sub
 if not IsAncestor(desc, sl) then reporterr "ScrollToChild: can't scroll to a slice that's not a descendant": exit sub
 'If the child of sl which desc is a descendent of is set to Fill, then
 'this function won't work. Too much trouble to check for though.

 RefreshSliceScreenPos desc

 dim support as RectType = XY_WH(sl->ScreenPos, sl->Size)
 if apply_padding then
  support.x += sl->paddingLeft
  support.y += sl->paddingTop
  support.wide -= sl->paddingLeft + sl->paddingRight
  support.high -= sl->paddingTop + sl->paddingBottom
 end if

 dim xmove as integer = 0
 dim ymove as integer = 0
 dim diff as integer
 diff = (support.y + support.high) - (desc->ScreenY + desc->Height)
 if diff < 0 then ymove = diff
 diff = support.y - desc->ScreenY
 if diff > 0 then ymove = diff
 diff = (support.x + support.wide) - (desc->ScreenX + desc->Width)
 if diff < 0 then xmove = diff
 diff = support.x - desc->ScreenX
 if diff > 0 then xmove = diff

 if xmove <> 0 orelse ymove <> 0 then
  ScrollAllChildren(sl, xmove, ymove)
 end if
end sub

'--Select--------------------------------------------------------------
Sub DisposeSelectSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SelectData = 0 then exit sub
 delete sl->SelectData
 sl->SelectData = 0
end sub

'Updates the Visible property of a child, and then does the default refreshing of
'size and screen position.
Sub SelectChildRefresh(par as Slice ptr, ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 if ch = 0 then debug "SelectChildRefresh null ptr": exit sub
 dim dat as SelectSliceData ptr = par->SliceData
 if dat = 0 then exit sub

 'Skip templates usually. Can't use childindex to do so, nor ShouldSkipSlice
 if ch->Template andalso template_slices_shown = NO then exit sub

 if childindex < 0 then  'Not known by the caller
  childindex = SliceIndexAmongSiblings(ch, template_slices_shown)
 end if

 if dat->override >= 0 then
  ch->Visible = (childindex = dat->override)
 else
  ch->Visible = (childindex = dat->index)
 end if

 'DefaultChildRefresh will check visibleonly, so we don't have to bother.
 DefaultChildRefresh(par, ch, childindex, visibleonly)
end sub

Sub CloneSelectSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "CloneScrollSlice null ptr": exit sub
 dim dat as SelectSliceData Ptr
 dat = sl->SelectData
 with *cl->SelectData
  .index       = dat->index
 end with
end sub

Sub SaveSelectSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveSelectSlice null ptr": exit sub
 DIM dat as SelectSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "index", dat->index
 'override property is never saved. Only used by the Slice Collection Editor
End Sub

Sub LoadSelectSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadSelectSlice null ptr": exit sub
 dim dat as SelectSliceData Ptr
 dat = sl->SliceData
 dat->index = LoadProp(node, "index", 0)
 dat->override = -1
 'override property is never loaded. Only used by the Slice Collection Editor
End Sub

Function NewSelectSlice(byval parent as Slice ptr, byref dat as SelectSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as SelectSliceData ptr = new SelectSliceData
 *d = dat
 '--Set non-zero defaults here
 d->override = -1
 
 ret->SliceType = slSelect
 ret->SliceData = d
 ret->Dispose = @DisposeSelectSlice
 ret->Clone = @CloneSelectSlice
 ret->Save = @SaveSelectSlice
 ret->Load = @LoadSelectSlice
 ret->ChildRefresh = @SelectChildRefresh
 
 return ret
end function

'All arguments default to no-change
Sub ChangeSelectSlice(byval sl as Slice ptr,_
                      byval index as integer=-2,_
                      byval override as integer=-2)
 if sl = 0 then debug "ChangeSelectSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSelect)
 with *sl->SelectData
  if index >= -1 then
   .index = index
  end if
  if override >= -1 then
   .override = override
  end if
 end with
end sub

Sub SelectSliceNext(byval sl as Slice ptr, byval can_loop as bool=YES)
 if sl = 0 then debug "SelectSliceNext null ptr" : exit sub
 ASSERT_SLTYPE(sl, slSelect)
 with *sl->SelectData
  .index += 1
  if .index >= sl->NumChildren then
   if can_loop then
    .index = 0
   else
    .index = sl->NumChildren - 1
   end if
  end if
 end with
end sub

'--Panel-------------------------------------------------------------------
Sub DisposePanelSlice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->PanelData = 0 then exit sub
 delete sl->PanelData
 sl->PanelData = 0
end sub

Sub ClonePanelSlice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "ClonePanelSlice null ptr": exit sub
 dim dat as PanelSliceData Ptr
 dat = sl->PanelData
 with *cl->PanelData
  .vertical = dat->vertical
  .primary = dat->primary
  .pixels = dat->pixels
  .percent = dat->percent
  .padding = dat->padding
 end with
end sub

Sub SavePanelSlice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SavePanelSlice null ptr": exit sub
 DIM dat as PanelSliceData Ptr
 dat = sl->SliceData
 SavePropAlways node, "vertical", dat->vertical
 SavePropAlways node, "primary", dat->primary
 SaveProp node, "pixels", dat->pixels
 'We MUST always save percent, as it gets loaded with wrong default if it's missing
 SavePropAlways node, "percent", dat->percent
 SaveProp node, "padding", dat->padding
End Sub

Sub LoadPanelSlice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadPanelSlice null ptr": exit sub
 dim dat as PanelSliceData Ptr
 dat = sl->SliceData
 dat->vertical = LoadPropBool(node, "vertical")
 dat->primary = bound(LoadProp(node, "primary"), 0, 1)
 dat->pixels = LoadProp(node, "pixels")
 'The default percent value is actually 0.5, but to work around a bug introduced
 'in r9509 which omitted 'percent' if equal to 0, we must it load as 0 if omitted.
 dat->percent = LoadPropFloat(node, "percent")
 dat->padding = LoadProp(node, "padding")
End Sub

'Calculate support (size and position relative to screen) of child 'index' of panel slice 'par'.
Sub CalcPanelSupport (byref support as RectType, byval par as Slice ptr, byval index as integer)

 if par = 0 then debug "CalcPanelArea null par ptr": exit sub

 if index > 1 then
  'Panel only expects 2 children
  support = XY_WH(par->ScreenPos, XY(0, 0))
  exit sub
 end if

 dim dat as PanelSliceData ptr = par->PanelData

 dim axis as integer = 0
 if dat->vertical then axis = 1
 dim other as integer = axis XOR 1

 dim innersize as XYPair = par->Size  'Total space available for both children with all padding subtracted
 dim prsize as integer
 dim prepad as XYPair
 dim postpad as XYPair
 prepad.x = par->paddingLeft
 postpad.x = par->paddingRight
 prepad.y = par->paddingTop
 postpad.y = par->paddingBottom

 innersize.n(axis) -= prepad.n(axis) + postpad.n(axis) + dat->padding
 innersize.n(other) -= prepad.n(other) + postpad.n(other)
 support.wh.n(other) = innersize.n(other)
 support.xy.n(other) = prepad.n(other)
 prsize = int(innersize.n(axis) * dat->percent) + dat->pixels
 if index = dat->primary then
  support.wh.n(axis) = prsize
 else
  support.wh.n(axis) = innersize.n(axis) - prsize
 end if
 if index = 0 then
  support.xy.n(axis) = prepad.n(axis)
 else
  if index = dat->primary then
   support.xy.n(axis) = prepad.n(axis) + (innersize.n(axis) - prsize) + dat->padding
  else
   support.xy.n(axis) = prepad.n(axis) + prsize + dat->padding
  end if
 end if

 support.xy += par->ScreenPos
End Sub

Sub PanelChildRefresh(byval par as Slice ptr, byval ch as Slice ptr, childindex as integer = -1, visibleonly as bool = YES)
 if ch = 0 then debug "PanelChildRefresh null ptr": exit sub
 if visibleonly andalso ch->Visible = NO then exit sub  'Don't need to exclude template slices

 if childindex < 0 then childindex = SliceIndexAmongSiblings(ch, template_slices_shown)
 if childindex > 1 then exit sub  'Panel only expects 2 children

 dim support as RectType = any
 CalcPanelSupport support, par, childindex
 RefreshChild ch, support
End sub

Sub PanelChildDraw(byval s as Slice Ptr, byval page as integer)
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSliceRecurse which does null check it.

 with *s
  'if .ChildrenRefresh then .ChildrenRefresh(s)  'Always NULL

  dim cliprect as RectType = any
  dim rememclip as ClipState = any

  'draw the slice's children
  dim index as integer = 0
  dim ch as Slice ptr = .FirstChild
  do while ch <> 0
   if ShouldSkipSlice(ch) then  'Skip template slices
    ch = ch->NextSibling
    continue do
   end if
   dim needdraw as bool = YES
   if .Clip then
    CalcPanelSupport cliprect, s, index

    rememclip = get_cliprect()
    needdraw = shrinkclip(cliprect.x, cliprect.y, _
                          cliprect.x + cliprect.wide - 1, _
                          cliprect.y + cliprect.high - 1, vpages(page))
   end if

   if needdraw then DrawSliceRecurse(ch, page, index)

   if .Clip then get_cliprect() = rememclip

   index += 1
   if index > 1 then exit do ' Only ever draw the first 2 non-template children!
   ch = ch->NextSibling
  Loop

 end with
End Sub

Function NewPanelSlice(byval parent as Slice ptr, byref dat as PanelSliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0
 
 dim d as PanelSliceData ptr = new PanelSliceData
 *d = dat
 '--Set non-zero defaults here
 d->percent = 0.5
 
 ret->SliceType = slPanel
 ret->SliceData = d
 ret->Dispose = @DisposePanelSlice
 ret->Clone = @ClonePanelSlice
 ret->Save = @SavePanelSlice
 ret->Load = @LoadPanelSlice
 ret->ChildRefresh = @PanelChildRefresh
 ret->ChildDraw = @PanelChildDraw
 
 return ret
end function

'All arguments default to no-change
Sub ChangePanelSlice(byval sl as Slice ptr,_
                      byval vertical as integer=-2,_ 'verical is actually bool, use -2 to signal no change
                      byval primary as integer=-1,_
                      byval pixels as integer=-1,_
                      byval percent as double=-1.0,_
                      byval padding as integer=-1)
 if sl = 0 then debug "ChangePanelSlice null ptr" : exit sub
 ASSERT_SLTYPE(sl, slPanel)
 with *sl->PanelData
  if vertical <> -2 then
   .vertical = vertical <> 0
  end if
  if primary >= 0 then
   .primary = small(primary, 1)
  end if
  if pixels >= 0 then
   .pixels = pixels
  end if
  if percent <> -1.0 then
   .percent = percent
  end if
  if padding >= 0 then
   .padding = padding
  end if
 end with
end sub


'=============================================================================
'                        Slice refreshing & positioning

'Returns the slice which provides the ChildRefresh method, in other words
'the one that determines the screen position.
Function GetSliceRefreshAttachParent(byval sl as Slice Ptr) as Slice Ptr
 if sl = 0 then debug "GetSliceRefreshAttachParent null ptr": return 0
 WITH *sl
  SELECT CASE .Attach
   case slSlice
    if .Attached then
     RETURN .Attached
    elseif .parent then
     RETURN .parent
    else
     'Fall through, use screen
    end if
   case slScreen
    'Fall through, use screen
  END SELECT
 END WITH
 '--When no attached slice is found (or when we are explicitly attached to the screen)
 RETURN ScreenSlice
End Function

Function SliceXAlign(sl as Slice Ptr, supportw as integer) as integer
 if sl = 0 then debug "SliceXAlign null ptr": Return 0
 SELECT CASE sl->AlignHoriz
  CASE alignLeft:   RETURN 0
  CASE alignMiddle: RETURN supportw \ 2
  CASE alignRight:  RETURN supportw
 END SELECT
End Function

Function SliceYAlign(sl as Slice Ptr, supporth as integer) as integer
 if sl = 0 then debug "SliceYAlign null ptr": Return 0
 SELECT CASE sl->AlignVert
  CASE alignTop:    RETURN 0
  CASE alignMiddle: RETURN supporth \ 2
  CASE alignBottom: RETURN supporth
 END SELECT
End Function

Function SliceXAnchor(byval sl as Slice Ptr) as integer
 if sl = 0 then debug "SliceXAnchor null ptr": Return 0
 SELECT CASE sl->AnchorHoriz
  CASE alignLeft:   RETURN 0
  CASE alignMiddle: RETURN sl->Width \ 2
  CASE alignRight:  RETURN sl->Width
 END SELECT
End Function

Function SliceYAnchor(byval sl as Slice Ptr) as integer
 if sl = 0 then debug "SliceYAnchor null ptr": Return 0
 SELECT CASE sl->AnchorVert
  CASE alignTop:    RETURN 0
  CASE alignMiddle: RETURN sl->Height \ 2
  CASE alignBottom: RETURN sl->Height
 END SELECT
End Function

'Like the "realign slice" script command
Sub RealignSlice(sl as Slice ptr, halign as AlignType = -1, valign as AlignType = -1, hanchor as AlignType = -1, vanchor as AlignType = -1)
 if sl = 0 then debug "SliceRealign null ptr" : exit sub
 if halign <> -1  then sl->AlignHoriz  = halign
 if valign <> -1  then sl->AlignVert   = valign
 if hanchor <> -1 then sl->AnchorHoriz = hanchor
 if vanchor <> -1 then sl->AnchorVert  = vanchor
End Sub

'Like "center slice" command
Sub CenterSlice(sl as Slice ptr)
 RealignSlice sl, alignCenter, alignCenter, alignCenter, alignCenter
End Sub

Function SliceEdgeX(byval sl as Slice Ptr, byval edge as AlignType) as integer
 if sl = 0 then debug "SliceEdgeX null ptr": Return 0
 SELECT CASE edge
  CASE alignLeft:   RETURN 0
  CASE alignMiddle: RETURN sl->Width \ 2
  CASE alignRight:  RETURN sl->Width
 END SELECT
End Function

Function SliceEdgeY(byval sl as Slice Ptr, byval edge as AlignType) as integer
 if sl = 0 then debug "SliceEdgeY null ptr": Return 0
 SELECT CASE edge
  CASE alignTop:    RETURN 0
  CASE alignMiddle: RETURN sl->Height \ 2
  CASE alignBottom: RETURN sl->Height
 END SELECT
End Function

'How wide/high does a parent slice have to be to cover a child slice with this position,
'size and alignment?
'position is the X or Y of the top-left corner of the slice relative to its align point,
'size is the width or height, align is AlignHoriz or AlignVert.
Local Function SliceExtent(position as integer, size as integer, align as AlignType) as integer
 if align = alignLeft then
  return position + size
 elseif align = alignRight then
  return -position
 else
  return large(-position * 2, (position + size) * 2)
 end if
end Function

'Called on slices which CoverChildren, in order to update their size.
'If a child is aligned to the left, we only care about it going over the right
'edge, etc. Children center-aligned on the parent matter for both edges.
'And the padding acts as a min size.
Sub UpdateCoverSize(par as Slice ptr)
 'Don't bother checking whether we're filling. You shouldn't be able to set a slice
 'to both fill and cover.

 'Panel, grid, layout are special, and will have to implement covering in their own way if at all.
 if par->SliceType = slPanel orelse par->SliceType = slGrid orelse par->SliceType = slLayout then exit sub

 dim size as XYPair

 dim ch as Slice ptr = par->FirstChild
 while ch
  with *ch
   if .IsShown then
    dim filling_horiz as bool = .Fill andalso .FillMode <> sliceFillVert
    dim filling_vert  as bool = .Fill andalso .FillMode <> sliceFillHoriz

    'Position of the top-left corner of the slice relative to is align point
    dim pos as XYPair = .Pos - XY(SliceXAnchor(ch), SliceYAnchor(ch))

    if not filling_horiz then
     size.w = large(size.w, SliceExtent(pos.X, .Width, .AlignHoriz))
    end if
    if not filling_vert then
     size.h = large(size.h, SliceExtent(pos.Y, .Height, .AlignVert))
    end if
   end if
  end with
  ch = ch->NextSibling
 wend

 with *par
  if .CoverChildren and coverHoriz then
   .Width = large(0, size.w + .PaddingLeft + .PaddingRight)
  end if
  if .CoverChildren and coverVert then
   .Height = large(0, size.h + .PaddingTop + .PaddingBottom)
  end if
 end with
end Sub

'Returns whether it's legal to set this slice to cover its children horizontally or vertically.
Function SliceLegalCoverModes(sl as Slice ptr) as CoverModes
 with *sl
  if .SliceType = slPanel orelse .SliceType = slGrid then return coverNone  'Not implemented
  if .SliceType = slScroll then return coverNone  'That would be daft
  if SlicePossiblyResizable(sl) = NO then return coverNone
  'TODO: once zooming sprites by resizing them is implemented, allow a Sprite to Cover.

  dim ret as CoverModes = coverFull
  if .Fill andalso .FillMode <> sliceFillVert then ret -= coverHoriz    'filling_horiz
  if .Fill andalso .FillMode <> sliceFillHoriz then ret -= coverVert
  return ret
 end with
end Function

'This function does not consider fill or cover mode! A slice set to fill/cover is not resizable either.
Function SlicePossiblyResizable(sl as Slice ptr) as bool
 if sl = 0 then return NO
 select case sl->SliceType
  case slSpecial, slRectangle, slLine, slContainer, slGrid, slEllipse, _
       slSelect, slScroll, slPanel, slLayout
   return YES
  case slText
   if sl->TextData = 0 then return NO
   'Text slices are never resizable vertically (they autoset their height),
   'and resizable horizontally if wrapping.
   return sl->TextData->wrap
  case slSprite
   if sl->SpriteData = 0 then return NO
   return sl->SpriteData->scaled
  ' If you add any more special cases like slText, please also add special case
  ' error messages to valid_resizeable_slice.
  case slMap
   ' Resizing map slices isn't implemented.
   return NO
  case else
   showbug "SliceResizable needs to be updated for type " & sl->SliceType
   return NO
 end select
end Function

'=============================================================================
'                                Slice Velocity

Sub SetSliceTarg(byval s as Slice ptr, byval x as integer, byval y as integer, byval ticks as integer)
 if s = 0 then debug "SetSliceTarg null ptr": exit sub
 with *s
  .TargResidue_X = 0.0
  .TargResidue_Y = 0.0
  .Targ.X = x
  .Targ.Y = y
  .TargTicks = ticks
  'cancel velocity
  .Velocity.X = 0
  .Velocity.Y = 0
  .VelTicks.X = 0
  .VelTicks.Y = 0
 end with
end sub

' Apply slice movement to this slice and descendants
' TODO: dissolves should also be applied here (but not to hidden slices)
Sub AdvanceSlice(byval s as Slice ptr)
 if s = 0 then debug "AdvanceSlice null ptr": exit sub
 if s->Paused = NO andalso ShouldSkipSlice(s) = NO then
  SeekSliceTarg s
  ApplySliceVelocity s
  'advance the slice's children
  dim ch as Slice ptr = s->FirstChild
  do while ch <> 0
   AdvanceSlice(ch)
   ch = ch->NextSibling
  Loop
 end if
end sub

' Apply slice .Targ movement
Local Sub SeekSliceTarg(byval s as Slice ptr)
 'no null check because this is only called from AdvanceSlice
 with *s
  if .TargTicks > 0 then
    dim as double temp
    dim as integer movestep
    temp = s->TargResidue_X + (s->Targ.X - .X) / s->TargTicks
    movestep = temp
    s->TargResidue_X = temp - movestep
    .X += movestep
    temp = s->TargResidue_Y + (s->Targ.Y - .Y) / s->TargTicks
    movestep = temp
    s->TargResidue_Y = temp - movestep
    .Y += movestep

   .TargTicks -= 1
   if .TargTicks = 0 then
    .X = .Targ.X
    .Y = .Targ.Y
   end if
  end if
 end with
end sub

Local Sub ApplySliceVelocity(byval s as Slice ptr)
 'no null check because this is only called from AdvanceSlice
 if s->VelTicks.X <> 0 then s->X += s->Velocity.X
 if s->VelTicks.X > 0 then
  s->VelTicks.X -= 1
  if s->VelTicks.X = 0 then s->Velocity.X = 0
 end if
 if s->VelTicks.Y <> 0 then s->Y += s->Velocity.Y
 if s->VelTicks.Y > 0 then
  s->VelTicks.Y -= 1
  if s->VelTicks.Y = 0 then s->Velocity.Y = 0
 end if
end sub


'=============================================================================


'The context_stack global is built up during a DrawSlice call.
'Use this function to compute the stack if you need it outside of DrawSlice.
'The returned vector must be freed.
Function CalcContextStack(byval sl as Slice ptr) as SliceContext ptr vector
 dim ret as SliceContext ptr vector
 v_new ret
 while sl
  if sl->Context then v_append ret, sl->Context
  sl = sl->Parent
 wend
 v_reverse ret
 return ret
end function

'The central slice drawing function, called regardless of what slice-specific methods have been set.
'(See comments at the top of this file for an overview of slice drawing.)
'childindex is index of s among its siblings, ignoring templates (unless shown). Pass
'childindex = -1 if not known which saves computing it if it's not needed.
Local Sub DrawSliceRecurse(byval s as Slice ptr, byval page as integer, childindex as integer = -1)
 if s = 0 then debug "DrawSliceRecurse null ptr": exit sub

 'This function isn't called if s is a template (unless template_slices_shown),
 'so don't need to check that here.

 'Refresh the slice: calc the size and screen X,Y and possibly visibility (select slices)
 'or other attributes. Refreshing is skipped if the slice isn't visible.
 '(Note: if ChildrenRefresh is set, it was already called from the parent's
 'ChildDraw, and ChildRefresh will do nothing.)
 DIM attach as Slice Ptr
 attach = GetSliceRefreshAttachParent(s)
 if attach then attach->ChildRefresh(attach, s, childindex, YES)

 if s->Visible then
  if s->CoverChildren then UpdateCoverSize(s)

  if s->Context then v_append context_stack, s->Context

  if s->Draw then
   NumDrawnSlices += 1
   s->Draw(s, page)
  end if

  AutoSortChildren(s)

  'ChildDraw normally calls ChildrenRefresh, if it exists, draws children by
  'calling DrawSliceRecurse, and may also draw anything that appears above all
  'children (eg scroll bars).
  s->ChildDraw(s, page)

  if s->Context then v_shrink context_stack
 end if
end sub

'Draw a slice tree
Sub DrawSlice(byval s as Slice ptr, byval page as integer)
 blend_algo = gen(gen8bitBlendAlgo)  'Bit kludgy, but easiest to set this here
 v_new context_stack
 DrawSliceRecurse s, page
 v_free context_stack
end sub

'Draw a slice tree (usually a subtree of the full tree) as if it were parented
'to a container slice with given position and size.
'ignore_offset causes the slice's offset from its parent to be ignored
Sub DrawSliceAt(byval sl as Slice ptr, byval x as integer, byval y as integer, byval w as integer = 100, byval h as integer = 100, byval page as integer, byval ignore_offset as bool = NO)
 BUG_IF(sl = 0, "null ptr")
 dim as Slice ptr dummyparent, prevparent = sl->Parent, prevnextsibling = sl->NextSibling
 dummyparent = NewSliceOfType(slContainer)
 dummyparent->Pos = XY(x, y)
 dummyparent->Size = XY(w, h)
 dummyparent->Visible = YES
 SetSliceParent sl, dummyparent
 dim oldpos as XYPair
 if ignore_offset then
  oldpos = sl->Pos
  sl->Pos = 0
 end if
 DrawSlice dummyparent, page
 'Restore sl
 if prevnextsibling then
  InsertSliceBefore prevnextsibling, sl
 else
  'Was the last child, can use SetSliceParent
  SetSliceParent sl, prevparent
 end if
 if ignore_offset then sl->Pos = oldpos
 DeleteSlice @dummyparent
end sub

Function UpdateRootSliceSize(sl as Slice ptr, page as integer) as bool
 'Update the size of a slice to match the size of a video page.
 'Normally the root slice is set to fill; calling this function is only needed
 'when it isn't.
 'Returns true if the size changed.
 if sl = 0 then return NO
 dim changed as bool
 with *sl
  changed = vpages(page)->size <> .Size
  .Size = vpages(page)->size
 end with
 return changed
end function

Function UpdateScreenSlice(clear_changed_flag as bool = YES) as bool
 'Match ScreenSlice size to window size; returns true if the size changed
 'since the last call with clear_changed_flag=YES.
 static changed as bool = NO
 if UpdateRootSliceSize(ScreenSlice, vpage) then changed = yes
 if clear_changed_flag and changed then
  changed = NO
  return YES
 end if
 return changed
end function

Sub RefreshSliceScreenPos(slc as Slice ptr)
 'This sub quickly updates ScreenX, ScreenY, plus Width and Height when filling,
 'of a slice and its ancestors without needing to do a full refresh of the whole tree
 'and without respect to .Visible (however templates may not be fully updated by their parents)
 if slc = 0 then exit sub
 dim attach as Slice ptr
 attach = GetSliceRefreshAttachParent(slc)
 if attach = 0 then exit sub
 if attach <> ScreenSlice then
  RefreshSliceScreenPos attach
 end if
 dim par as Slice ptr = slc->Parent
 if par andalso par->ChildrenRefresh then par->ChildrenRefresh(par)
 attach->ChildRefresh(attach, slc, -1, NO)  'visibleonly=NO
end sub

'Refresh all descendents of slc (however templates may not be fully updated by their parents)
Local Sub SliceRefreshRecurse(slc as Slice ptr)
 if slc->ChildrenRefresh then slc->ChildrenRefresh(slc)

 dim attach as Slice Ptr
 dim ch as Slice ptr = slc->FirstChild
 dim childindex as integer = 0
 do while ch <> 0
  attach = GetSliceRefreshAttachParent(ch)
  'Note that normally ChildRefresh isn't called on template slices, but make a best effort to update them.
  attach->ChildRefresh(attach, ch, childindex, NO)  'visibleonly=NO
  SliceRefreshRecurse ch
  if ShouldSkipSlice(ch) = NO then
   childindex += 1
  end if
  ch = ch->NextSibling
 loop
end sub

Sub RefreshSliceTreeScreenPos(slc as Slice ptr)
 'Updates ScreenX, ScreenY, plus Width and Height when filling,
 'of a slice tree (specially, all its ancestors and descendents but not siblings)
 'while ignoring .Visible (however templates may not be fully updated by their parents)
 'DrawSliceRecurse skips refreshing nonvisible slices.
 if slc = 0 then exit sub

 'Update slc and ancestors
 RefreshSliceScreenPos slc
 'Update descendents
 SliceRefreshRecurse slc
end sub

Function SliceCollide(byval sl1 as Slice Ptr, sl2 as Slice Ptr) as bool
 'Check for a screen-position collision between slice 1 and slice 2 (regardless of parentage)
 'Note RefreshSliceScreenPos not called here
 if sl1 = 0 or sl2 = 0 then return NO
 'AABB collision test
 if sl1->Width + sl2->Width <= abs(2 * sl1->ScreenX + sl1->Width - 2 * sl2->ScreenX - sl2->Width) then return NO
 if sl1->Height + sl2->Height <= abs(2 * sl1->ScreenY + sl1->Height - 2 * sl2->ScreenY - sl2->Height) then return NO
 return YES
end function

Function SliceCollidePoint(byval sl as Slice Ptr, byval point as XYPair) as bool
 'Check if a point collides with a slice's screen position
 'Note RefreshSliceScreenPos not called here
 if sl = 0 then return NO
 if point.x >= sl->ScreenX andalso point.x < sl->ScreenX + sl->Width andalso _
    point.y >= sl->ScreenY andalso point.y < sl->ScreenY + sl->Height then
  return YES
 end if
 return NO
end function

Function SliceContains(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr) as bool
 'Check if sl2 is completely contained inside sl1
 if sl1 = 0 or sl2 = 0 then return NO
 RefreshSliceScreenPos(sl1)
 RefreshSliceScreenPos(sl2)
 if SliceCollidePoint(sl1, sl2->ScreenPos) then
  if SliceCollidePoint(sl1, sl2->ScreenPos + sl2->Size - 1) then
   'no nonrectangular slices (yet)
   'if SliceCollidePoint(sl1, XY(sl2->ScreenX + sl2->Width-1, sl2->ScreenY)) then
    'if SliceCollidePoint(sl1, XY(sl2->ScreenX, sl2->ScreenY + sl2->Height-1)) then
    'end if
   'end if
   return YES
  end if
 end if
 return NO
end function

Function FindSliceCollision(parent as Slice Ptr, sl as Slice Ptr, byref num as integer, descend as bool, visibleonly as bool = NO) as Slice Ptr
 'Find a child or descendant of parent which is not Special and is not sl which overlaps with sl.
 'descend:     Whether to recurse to decendents of parent.
 '             Note: except for clipping (and maybe invisible) slices, we always check all descendents!
 'visibleonly: Whether to restrict to visible slices (treats parent as visible).
 'num:         0 to return bottommost matching slice, 1 for next, etc.
 '             Is decremented by the number of matching slices.
 'We don't call RefreshSliceScreenPos for efficiency; we expect the calling code to do that
 'and we handle refreshing the descendents of parent (calling ChildRefresh on every slice we visit).
 'Warning: RefreshSliceScreenPos doesn't get called on invisible slices in DrawSlice!!
 if parent = 0 or sl = 0 then debug "FindSliceCollision null ptr": return 0
 dim as Slice ptr s, temp
 dim childindex as integer = 0
 s = parent->FirstChild
 while s
  while s->Template andalso template_slices_shown = NO
   s = s->NextSibling
   if s = NULL then return NULL
  wend
  if s <> sl then
   with *s
    'First refresh to update child visibility, in case the parent is a Select slice.
    'But we unconditionally refresh the child even if not visible for... no well
    'justified reason (see r7792)
    'We don't call ChildrenRefresh if applicable, because we need (and should)
    'only update the screen positions, not do complex positioning recalc.
    parent->ChildRefresh(parent, s, childindex, NO)  'visibleonly=NO

    if visibleonly = NO orelse .Visible then
     dim recurse as bool = descend

     if SliceCollide(s, sl) then
      if .SliceType <> slSpecial then
       if num = 0 then return s
       num -= 1
      end if
     elseif s->Clip then
      'Clipping slices reduce the effective sizes of their descendents, so don't
      'recurse if point not inside
      recurse = NO
     end if

     if recurse then
      temp = FindSliceCollision(s, sl, num, YES, visibleonly)
      if temp then return temp
     end if
    end if
   end with
  end if
  s = s->NextSibling
  childindex += 1
 wend
 return NULL
end function

Function FindSliceAtPoint(parent as Slice Ptr, point as XYPair, byref num as integer, descend as bool, visibleonly as bool = NO) as Slice Ptr
 'Find a child or descendant of parent which is not Special that contains a certain screen position.
 'descend:     Whether to recurse.
 '             Note: except for clipping (and maybe invisible) slices, we always check all descendents!
 'visibleonly: Whether to restrict to visible slices (treats parent as visible).
 'num:         0 for bottommost matching slice, 1 for next, etc. Is decremented by the number of matching slices.
 'We don't call RefreshSliceScreenPos for efficiency; we expect the calling code to do that,
 '(because DrawSliceRecurse doesn't call ChildRefresh or recurse on invisible slices)
 'and we handle refreshing the descendents of parent (calling ChildRefresh on every slice we visit).
 if parent = 0 then debug "FindSliceAtPoint null ptr": return 0
 dim as Slice ptr s, temp
 dim childindex as integer = 0
 s = parent->FirstChild
 while s
  while s->Template andalso template_slices_shown = NO
   s = s->NextSibling
   if s = NULL then return NULL
  wend
  with *s
   'First refresh to update child visibility, in case the parent is a Select slice.
   'But we unconditionally refresh the child even if not visible for... no well
   'justified reason (see r7792)
   'We don't call ChildrenRefresh if applicable, because we need (and should)
   'only update the screen positions, not do complex positioning recalc.
   parent->ChildRefresh(parent, s, childindex, NO)  'visibleonly=NO

   if visibleonly = NO orelse .Visible then
    dim recurse as bool = descend

    if SliceCollidePoint(s, point) then
     if .SliceType <> slSpecial then
      if num = 0 then return s
      num -= 1
     end if
    elseif s->Clip then
     'Clipping slices reduce the effective sizes of their descendents, so don't
     'recurse if point not inside
     recurse = NO
    end if

    if recurse then
     temp = FindSliceAtPoint(s, point, num, YES, visibleonly)
     if temp then return temp
    end if
   end if
  end with
  s = s->NextSibling
  childindex += 1
 wend
 return NULL
end function

Function SliceIsInvisible(byval sl as Slice Ptr) as bool
 if sl = 0 then debug "SliceIsInvisible: null slice": return YES 'Treating a null slice as invisible is probably safest here
 'First check the slice itself
 if sl->IsShown = NO then return YES
 'Then check for any invisible parents
 dim parent as Slice Ptr
 parent = sl->Parent
 do while parent
  if parent->IsShown = NO then return YES
  parent = parent->Parent
 loop
 return NO
End Function

'Check whether a slice is *actually* visible.
'FIXME: this is incorrect: clipping is done from the padded slice bounds, ignored by SliceCollide.
Function SliceIsInvisibleOrClipped(byval sl as Slice Ptr) as bool
 if SliceIsInvisible(sl) then return YES
 'Check for any parents that are clipping this slice
 dim parent as Slice Ptr
 parent = sl->Parent
 do while parent
  if parent->Clip then
   if not SliceCollide(sl, parent) then return YES
  end if
  parent = parent->Parent
 loop
 return NO
End Function

Sub SliceClamp(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr)
 'Don't confuse this with a slice's .Fill member. This is a one-shot attempt
 'to fit sl2 inside sl1 without doing any resizing.
 '(TODO: swap arg order, as it's needlessly the opposite to "clamp slice")
 'NOTE: ignores padding. And doesn't work for Grid slices.
 if sl1 = 0 or sl2 = 0 then exit sub
 if sl2->Fill then reporterr "SliceClamp cannot move slices with .Fill=ON" : exit sub
 RefreshSliceScreenPos(sl1)
 RefreshSliceScreenPos(sl2)
 dim diff as integer
 diff = sl2->ScreenX - sl1->ScreenX
 '--Horizontal clamp
 if diff < 0 then
  sl2->X += abs(diff)
 else
  diff = (sl2->ScreenX + sl2->Width) - (sl1->ScreenX + sl1->Width)
  if diff > 0 then sl2->X -= abs(diff)
 end if
 '--Verical clamp
 diff = sl2->ScreenY - sl1->ScreenY
 if diff < 0 then
  sl2->Y += abs(diff)
 else
  diff = (sl2->ScreenY + sl2->Height) - (sl1->ScreenY + sl1->Height)
  if diff > 0 then sl2->Y -= abs(diff)
 end if
end sub

Function SliceChildByIndex(byval sl as Slice ptr, byval index as integer) as Slice Ptr
 if sl = 0 then debug "SliceChildByIndex null ptr": return 0
 dim ch as Slice ptr = sl->FirstChild
 for i as integer = 0 to sl->NumChildren
  if ch = NULL then exit for
  if i = index THEN return ch
  ch = ch->NextSibling
 next i
 return 0
End Function

Function FindTextSliceStringRecursively(sl as slice ptr, query as string) as Slice Ptr
 'Search this slice, and all its children recursively.
 'Check to see if query is a substring inside any TextString values.
 'Returns the first TextSlice where a match is found, or 0 if no march is found.
 'Compares strings case-insensitively

 'If there is no query, exit immediately
 if query = "" then return 0

 if sl->SliceType = slText then
  'If this slice is text, and the text includes the filter string, Success!
  if instr(lcase(sl->TextData->s), lcase(query)) then return sl
 end if
 'Check all children recursively too until we find one that succeeds
 dim ch as Slice Ptr = sl->FirstChild
 dim result as Slice Ptr
 do while ch
  result = FindTextSliceStringRecursively(ch, query)
  if result <> 0 then return result
  ch = ch->NextSibling
 loop
 'No text was found that matches the query text
 return 0
End Function

Sub RecursivelyUpdateColor(sl as Slice Ptr, newcol as integer, oldcol1 as integer, oldcol2 as integer, oldcol3 as integer=-9999999, lookup_code as integer=0)
 'Recursively replace any color codes that match specific old values
 'Normally this would be used for the negative ui constants, not for positive palette indexes
 'The optional lookup code restricts the changes only to slices that have that code
 if lookup_code = 0 orelse sl->Lookup = lookup_code then
  UpdateColor sl, newcol, oldcol1, oldcol2, oldcol3
 end if
 dim ch as Slice Ptr = sl->FirstChild
 do while ch
  if not ShouldSkipSlice(ch, YES) then
   'Only bother with visible non-template slices
   RecursivelyUpdateColor ch, newcol, oldcol1, oldcol2, oldcol3, lookup_code
  end if
  ch = ch->NextSibling
 loop
End Sub

Sub UpdateColor (sl as Slice Ptr, newcol as integer, oldcol1 as integer, oldcol2 as integer, oldcol3 as integer=-9999999)
 if sl->SliceType = slRectangle then
  with *sl->RectData
   if .fgcol = oldcol1 or .fgcol = oldcol2 or .fgcol = oldcol3 then .fgcol = newcol
   if .bgcol = oldcol1 or .bgcol = oldcol2 or .bgcol = oldcol3 then .bgcol = newcol
  end with
 end if
 if sl->SliceType = slLine then
  with *sl->LineData
   if .col = oldcol1 or .col = oldcol2 or .col = oldcol3 then .col = newcol
  end with
 end if
 if sl->SliceType = slText then
  with *sl->TextData
   if .col = oldcol1 or .col = oldcol2 or .col = oldcol3 then .col = newcol
   if .bgcol = oldcol1 or .bgcol = oldcol2 or .bgcol = oldcol3 then .bgcol = newcol
  end with
 end if
 if sl->SliceType = slEllipse then
  with *sl->EllipseData
   if .bordercol = oldcol1 or .bordercol = oldcol2 or .bordercol = oldcol3 then .bordercol = newcol
   if .fillcol = oldcol1 or .fillcol = oldcol2 or .fillcol = oldcol3 then .fillcol = newcol
  end with
 end if
End Sub

'==Slice cloning===============================================================

Function CloneSliceTree(byval sl as Slice ptr, recurse as bool = YES, copy_special as bool = YES) as Slice ptr
 'clone a duplicate of a slice and, if recurse=YES, all its children.
 'copy_special: copy Special slices and lookup codes. .Protect bit not copied.
 'The resulting clone is parentless
 if sl = NULL orelse sl->SliceType = slMap then return NULL
 dim clone as Slice Ptr
 '--Create another slice of the same type
 dim newtype as SliceTypes = sl->SliceType
 if copy_special = NO then
  if newtype = slSpecial then newtype = slContainer
 end if
 clone = NewSliceOfType(newtype)
 '--Clone all standard properties
 with *clone
  'Parent, siblings, etc. not copied
  'Function ptrs not copied.
  '.Attach and .Attached not copied
  '.TableSlot not copied
  if copy_special then .Protect = sl->Protect  'Otherwise, the copy won't have any special role
  .Lookup = sl->Lookup
  if copy_special = NO and .Lookup < 0 then .Lookup = 0
  .X = sl->X
  .Y = sl->Y
  '.ScreenPos not copied
  .Width = sl->Width
  .Height = sl->Height
  .Visible = sl->Visible
  .Template = sl->Template
  .Paused = sl->Paused
  '.EditorColor not copied
  .EditorHideChildren = sl->EditorHideChildren
  .Clip = sl->Clip
  .Velocity.X = sl->Velocity.X
  .Velocity.Y = sl->Velocity.Y
  .VelTicks.X = sl->VelTicks.X
  .VelTicks.Y = sl->VelTicks.Y
  .Targ.X = sl->Targ.X
  .Targ.Y = sl->Targ.Y
  .TargResidue_X = sl->TargResidue_X
  .TargResidue_Y = sl->TargResidue_Y
  .TargTicks = sl->TargTicks
  .AlignHoriz = sl->AlignHoriz
  .AlignVert = sl->AlignVert
  .AnchorHoriz = sl->AnchorHoriz
  .AnchorVert = sl->AnchorVert
  .PaddingTop = sl->PaddingTop
  .PaddingLeft = sl->PaddingLeft
  .PaddingRight = sl->PaddingRight
  .PaddingBottom = sl->PaddingBottom
  .CoverChildren = sl->CoverChildren
  .ClampHoriz = sl->ClampHoriz
  .ClampVert = sl->ClampVert
  .ClampToScreen = sl->ClampToScreen
  .Fill = sl->Fill
  .FillMode = sl->FillMode
  .AutoSort = sl->AutoSort
  .Sorter = sl->Sorter
  if sl->ExtraVec then
   v_copy .ExtraVec, sl->ExtraVec
  end if
 end with
 '--clone special properties for this slice type
 sl->Clone(sl, clone)
 if recurse = NO then return clone
 '--Now clone all the children
 dim ch_slice as Slice Ptr = sl->FirstChild
 dim ch_clone as Slice Ptr
 do while ch_slice <> 0
  ch_clone = CloneSliceTree(ch_slice, YES, copy_special)
  if ch_clone then SetSliceParent ch_clone, clone
  ch_slice = ch_slice->NextSibling
 loop
 '--return the clone
 return clone
end function

'A variant on CloneSliceTree which is intended to be used on Template slices already
'in a slice tree, for instantiating them, but doesn't need to be.
Function CloneTemplate(byval templatesl as Slice ptr) as Slice ptr
 dim sl as Slice ptr
 sl = CloneSliceTree(templatesl)
 sl->Template = NO  'Descendents which are Templates stay that way
 if templatesl->Parent then
  'Keep the position amongst its siblings
  InsertSliceBefore templatesl, sl
 end if
 return sl
end function


'==Slice saving and loading====================================================

'--saving----------------------------------------------------------------------

End Extern

Sub SavePropAlways(node as Reload.Nodeptr, propname as zstring ptr, byval value as integer)
 if node = 0 then debug "SaveProp null node ptr": Exit Sub
 Reload.SetChildNode(node, propname, CLNGINT(value))
End Sub

'Doesn't save anything = 0
Sub SaveProp(node as Reload.Nodeptr, propname as zstring ptr, byval value as integer)
 if value <> 0 then SavePropAlways node, propname, value
End Sub

Sub SavePropAlways(node as Reload.Nodeptr, propname as zstring ptr, byval value as double)
 if node = 0 then debug "SaveProp null node ptr": Exit Sub
 Reload.SetChildNode(node, propname, value)
End Sub

Sub SaveProp(node as Reload.Nodeptr, propname as zstring ptr, byval value as double)
 if value <> 0. then SavePropAlways node, propname, value
End Sub

Sub SavePropAlways(node as Reload.Nodeptr, propname as zstring ptr, s as string)
 if node = 0 then debug "SaveProp null node ptr": Exit Sub
 Reload.SetChildNode(node, propname, s)
End Sub

Sub SaveProp(node as Reload.Nodeptr, propname as zstring ptr, s as string)
 if len(s) then SavePropAlways node, propname, s
End Sub

Extern "C"

Sub SliceSaveToNode(byval sl as Slice Ptr, node as Reload.Nodeptr, save_handles as bool=NO)
 if sl = 0 then debug "SliceSaveToNode null slice ptr": Exit Sub
 if node = 0 then debug "SliceSaveToNode null node ptr": Exit Sub
 if Reload.NumChildren(node) <> 0 then debug "SliceSaveToNode non-empty node has " & Reload.NumChildren(node) & " children"
 '--Save standard slice properties
 'NOTE: if something has a non-zero default load value, then you must use SavePropAlways
 SaveProp node, "type", SliceTypeName(sl)
 SaveProp node, "template", sl->template
 SaveProp node, "lookup", sl->lookup
 SaveProp node, "x", sl->x
 SaveProp node, "y", sl->Y
 'Have to save size even if set to fill
 SavePropAlways node, "w", sl->Width
 SavePropAlways node, "h", sl->Height
 SavePropAlways node, "vis", sl->Visible
 SaveProp node, "editorhidechildren", sl->EditorHideChildren
 SaveProp node, "paused", sl->Paused
 SaveProp node, "clip", sl->Clip
 SaveProp node, "vx", sl->Velocity.X
 SaveProp node, "vy", sl->Velocity.Y
 SaveProp node, "vtickx", sl->VelTicks.X
 SaveProp node, "vticky", sl->VelTicks.Y
 if sl->TargTicks > 0 then
  SaveProp node, "tx", sl->Targ.X
  SaveProp node, "ty", sl->Targ.Y
  SaveProp node, "ttick", sl->TargTicks
  'No need to save TargResidue
 end if
 SaveProp node, "alignh", sl->AlignHoriz
 SaveProp node, "alignv", sl->AlignVert
 SaveProp node, "anchorh", sl->AnchorHoriz
 SaveProp node, "anchorv", sl->AnchorVert
 if sl->ClampHoriz <> alignNone then SavePropAlways node, "clamph", sl->ClampHoriz
 if sl->ClampVert  <> alignNone then SavePropAlways node, "clampv", sl->ClampVert
 SaveProp node, "clamptoscreen", sl->ClampToScreen
 SaveProp node, "padt", sl->PaddingTop
 SaveProp node, "padl", sl->PaddingLeft
 SaveProp node, "padr", sl->PaddingRight
 SaveProp node, "padb", sl->PaddingBottom
 SaveProp node, "cover", sl->CoverChildren
 SaveProp node, "fill", sl->Fill
 SaveProp node, "fillmode", sl->FillMode
 SaveProp node, "sort", sl->Sorter
 SaveProp node, "autosort", sl->AutoSort
 SaveExtraVector node, "extra", sl->ExtraVec
 #IFDEF IS_GAME
  if save_handles then
   ' This only occurs when saving a game.
   if sl->TableSlot then
    SaveProp node, "tableslot_handle", plotslices(sl->TableSlot).handle
   end if
  end if
 #ENDIF
 '--Save properties specific to this slice type
 sl->Save(sl, node)
 '--Contexts may or may not be savable
 if sl->Context then sl->Context->save(node)
 '--Now save all the children
 if sl->NumChildren > 0 then
  '--make a container node for all the child nodes
  dim children as Reload.NodePtr
  children = Reload.CreateNode(node, "children")
  Reload.AddChild(node, children)
  'now loop through the children of this slice and create a new node for each one
  dim ch_node as Reload.NodePtr
  dim ch_slice as Slice Ptr = sl->FirstChild
  do while ch_slice <> 0
   ch_node = Reload.CreateNode(children, "")
   Reload.AddChild(children, ch_node)
   SliceSaveToNode ch_slice, ch_node, save_handles
   ch_slice = ch_slice->NextSibling
  loop
 end if
End sub

Sub SliceSaveToFile(byval sl as Slice Ptr, filename as string, save_handles as bool=NO)
 
 'First create a reload document
 dim doc as Reload.DocPtr
 doc = Reload.CreateDocument()
 if doc = NULL then exit sub  'Already showed a BUG message
 
 'Create a node, and save the slice tree into it
 dim node as Reload.Nodeptr
 node = Reload.CreateNode(doc, "")
 Reload.SetRootNode(doc, node)
 SliceSaveToNode sl, node, save_handles
 
 'Write the reload document to the file
 Reload.SerializeBin filename, doc
 
 Reload.FreeDocument(doc)
End sub

'--loading---------------------------------------------------------------------

Function LoadPropStr(node as Reload.Nodeptr, propname as zstring ptr, defaultval as string="") as string
 if node = 0 then debug "LoadPropStr null node ptr": return defaultval
 return Reload.GetChildNodeStr(node, propname, defaultval)
End function

Function LoadProp(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as integer=0) as integer
 if node = 0 then debug "LoadProp null node ptr": return defaultval
 return Reload.GetChildNodeInt(node, propname, CLNGINT(defaultval))
End function

Function LoadPropBool(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as bool=NO) as bool
 if node = 0 then debug "LoadPropBool null node ptr": return defaultval
 return Reload.GetChildNodeBool(node, propname, defaultval)
End function

Function LoadPropFloat(node as Reload.Nodeptr, propname as zstring ptr, byval defaultval as double=0.0) as double
 if node = 0 then debug "LoadPropFloat null node ptr": return defaultval
 return Reload.GetChildNodeFloat(node, propname, defaultval)
End function

'Note that this mutates an existing slice, which should be a new slice with no children
'Returns true on (apparent) success; when returning false may be partially
'loaded, such as when there's an unknown slice type.
Function SliceLoadFromNode(byval sl as Slice Ptr, node as Reload.Nodeptr, load_handles as bool=NO) as bool
 BUG_IF(sl = 0, "null slice ptr", NO)
 BUG_IF(node = 0, "null node ptr", NO)
 BUG_IF(sl->NumChildren > 0, "slice already has children", NO)
 dim ret as bool = YES
 '--Load standard slice properties
 'NOTE: if something has a non-zero default value, then you must use SavePropAlways
 sl->Template = LoadPropBool(node, "template")
 sl->Lookup = LoadProp(node, "lookup")
 sl->x = LoadProp(node, "x")
 sl->y = LoadProp(node, "y")
 sl->Width = LoadProp(node, "w")
 sl->Height = LoadProp(node, "h")
 sl->Visible = LoadPropBool(node, "vis")
 sl->EditorHideChildren = LoadPropBool(node, "editorhidechildren")
 sl->Paused = LoadPropBool(node, "paused")
 sl->Clip = LoadPropBool(node, "clip")
 sl->Velocity.X = LoadProp(node, "vx")
 sl->Velocity.Y = LoadProp(node, "vy")
 sl->VelTicks.X = LoadProp(node, "vtickx")
 sl->VelTicks.Y = LoadProp(node, "vticky")
 sl->Targ.X = LoadProp(node, "tx")
 sl->Targ.Y = LoadProp(node, "ty")
 sl->TargTicks = LoadProp(node, "ttick")
 sl->AlignHoriz = LoadProp(node, "alignh")
 sl->AlignVert = LoadProp(node, "alignv")
 sl->AnchorHoriz = LoadProp(node, "anchorh")
 sl->AnchorVert = LoadProp(node, "anchorv")
 sl->ClampHoriz = LoadProp(node, "clamph", alignNone)
 sl->ClampVert = LoadProp(node, "clampv", alignNone)
 sl->ClampToScreen = LoadPropBool(node, "clamptoscreen")
 sl->PaddingTop = LoadProp(node, "padt")
 sl->PaddingLeft = LoadProp(node, "padl")
 sl->PaddingRight = LoadProp(node, "padr")
 sl->PaddingBottom = LoadProp(node, "padb")
 sl->CoverChildren = LoadProp(node, "cover")
 sl->Fill = LoadPropBool(node, "fill")
 sl->FillMode = LoadProp(node, "fillmode")
 sl->Sorter = LoadProp(node, "sort")
 sl->AutoSort = LoadProp(node, "autosort")

 'Load extra data
 dim ex_node as Reload.NodePtr = GetChildByName(node, "extra")
 if ex_node then
  if LoadExtraVector(ex_node, sl->ExtraVec, "slice") = NO then ret = NO
 else
  'Load from old serialization format for extra data
  dim extra as integer vector
  v_new extra, 3
  extra[0] = LoadProp(node, "extra0")
  extra[1] = LoadProp(node, "extra1")
  extra[2] = LoadProp(node, "extra2")
  if extra[0] or extra[1] or extra[2] then
   sl->ExtraVec = extra
  else
   v_free extra
  end if
 end if

 #IFDEF IS_GAME
  if load_handles then
   ' This only occurs when loading a saved game.
   ' Slice handles should never be loaded from a collection in the middle of a game!
   dim tableslot as integer = LoadProp(node, "tableslot_handle")
   if tableslot then restore_saved_plotslice_handle(sl, tableslot)
  end if
 #ENDIF
 'TODO: create a SliceContext so it can load itself. For now, SliceLoadFromFile gives
 'the root slice a SliceCollectionContext.
 'dim contextstr as string = LoadPropStr(node, "context")
 if sl->Context then sl->Context->load(node)
 'now update the type
 dim typestr as string = LoadPropStr(node, "type")
 dim typenum as SliceTypes = SliceTypeByName(typestr)
 if typenum = slInvalid then
  reporterr "Could not load slice: unknown type " & typestr, serrError
  typenum = slContainer
  ret = NO
 end if
 dim classname as string = LoadPropStr(node, "class")
 if len(classname) then
  'ClassSlice. Currently, Class slices are always of subtype slSpecial, but might not be true in future?
  if InitClassSliceByName(sl, classname) = NO then
   reporterr "Could not load slice: unknown class " & classname, serrError
   typenum = slContainer
   ret = NO
  end if
 end if
 if typenum <> sl->SliceType then
  dim newsl as Slice Ptr = NewSliceOfType(typenum)
  ReplaceSliceType sl, newsl
 end if
 '--Load properties specific to this slice type
 sl->Load(sl, node)
 '--Now load all the children
 dim children as Reload.NodePtr
 children = Reload.GetChildByName(node, "children")
 if children then
  'now loop through the children of this node and create a new slice for each one
  dim ch_slice as Slice Ptr
  dim ch_node as Reload.NodePtr = Reload.FirstChild(children)
  do while ch_node <> 0
   ch_slice = NewSlice(sl)
   'If there's a problem loading a child try to keep going
   if SliceLoadFromNode(ch_slice, ch_node, load_handles) = NO then ret = NO
   ch_node = Reload.NextSibling(ch_node)
  loop
 end if
 return ret
End function

'collection_num is used only in-game
'Returns true on (apparent) success; when returning false may be partially
'loaded, such as when there's an unknown slice type.
Function SliceLoadFromFile(byval sl as Slice Ptr, filename as string, load_handles as bool=NO, collection_id as integer=-1) as bool
 dim doc as Reload.DocPtr
 doc = Reload.LoadDocument(filename, optNoDelay)
 if doc = null then return NO  'Already showed an error

 'Always give the root slice a context. The context is used to hold the name of
 'the slice collection, if there is one, and other shared data in future
 BUG_IF(sl->Context, "sl has Context", NO)
 var context = new SliceCollectionContext
 context->id = collection_id
 sl->Context = context

 'Populate the slice tree with data from the reload tree
 dim node as Reload.Nodeptr
 node = Reload.DocumentRoot(doc)
 dim ret as bool = SliceLoadFromNode(sl, node, load_handles)

 Reload.FreeDocument(doc)
 return ret
End function


'==============================================================================
'                              Slice Collections

'Load a slice collection, either a user or special one, e.g. the Status menu,
'either from the game or falling back to a default.
'Returns null if the collection is missing or couldn't be loaded.
'However if the collection was partially loaded (contains unsupported slice types/data)
'then return_partially_loaded tells whether to return it or NULL.
'A missing user collection is not an error, all other failures call showbug or showerror.
Function LoadSliceCollection(collection_kind as integer, collection_num as integer=0, return_partially_loaded as bool=YES) as Slice ptr
 ' Look in game data
 dim srcfile as string
 if collection_kind = SL_COLLECT_USERDEFINED then
  srcfile = workingdir & SLASH & "slicetree_" & collection_kind & "_" & collection_num & ".reld"
  if isfile(srcfile) = NO then return null  'Silent failure
 else
  ' Otherwise look for a default (usually from an embedded file, but can be an external file)
  dim collname as string
  select case collection_kind
   case SL_COLLECT_STATUSSCREEN:           collname = "status_screen"
   case SL_COLLECT_ITEMSCREEN:             collname = "item_screen"
   case SL_COLLECT_SPELLSCREEN:            collname = "spell_screen"
   case SL_COLLECT_VIRTUALKEYBOARDSCREEN:  collname = "virtual_keyboard_screen"
   case else
    showbug "Unknown slice collection kind " & collection_kind
    return null
  end select

  srcfile = finddatafile("sourceslices" SLASH "default_" & collname & ".slice", NO) 'error_if_missing=NO
  if len(srcfile) = 0 then
   showbug "Missing embedded default_" & collname
   return null
  end if
 end if

 ' Load the collection
 'The SliceCollectionContext.id
 dim collection_id as integer = IIF(collection_kind = SL_COLLECT_USERDEFINED, collection_num, -1)
 dim sl as Slice ptr = NewSlice()
 if SliceLoadFromFile(sl, srcfile, , collection_id) = NO then  'showerror's on failure
  if return_partially_loaded = NO then
   DeleteSlice @sl
  end if
 end if
 return sl
End function


'==============================================================================
'                              Slice Debugging

'Return a string like "Sprite slice 11" or "Container slice" or "Rectangle slice (textbox box)"
FUNCTION DescribeSlice(sl as Slice ptr) as string
 IF sl THEN
  DIM sliceinfo as string = SliceTypeName(sl) & " slice"
  IF sl->TableSlot THEN sliceinfo &= " " & sl->TableSlot
  IF sl->Lookup THEN sliceinfo &= " (" & SliceLookupCodename(sl) & ")"
 ELSE
  'Normally this sub shouldn't be called with a null ptr
  RETURN "(invalid slice)"
 END IF
END FUNCTION

Sub report_slice_type_err(sl as Slice ptr, expected as SliceTypes)
 reporterr "Attempt to treat " & DescribeSlice(sl) & " (at " & SlicePath(sl) & ") as a " & SliceTypeName(expected)
End Sub

#ifdef ENABLE_SLICE_DEBUG

SUB SliceDebugRemember(sl as Slice Ptr)
 if sl = 0 then debug "SliceDebugRemember null ptr": exit sub
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = 0 then
   '--found an empty slot in the slice debug table...
   SliceDebug(i) = sl
   exit sub
  end if
 next i
 '--no more room in the slice debug table
 dim newsize as integer = ubound(SliceDebug) + 50
 debuginfo "enlarge slice debug table to " & newsize
 redim preserve SliceDebug(newsize) as Slice Ptr
 SliceDebugRemember sl
END SUB

SUB SliceDebugForget(sl as Slice Ptr)
 if sl = 0 then debug "SliceDebugForget null ptr": exit sub
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = sl then
   '--found the slice to forget
   SliceDebug(i) = 0
   exit sub
  end if
 next i
 debug "WARNING: tried to delete slice " & SlicePath(sl) & " without any record of creating it!"
END SUB

'This is used for hunting down leaked slices
SUB SliceDebugDump(byval noisy as bool = NO)
 debug "===SLICE DEBUG DUMP==="
 dim count as integer = 0
 dim sl as Slice Ptr
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) <> 0 then
   sl = SliceDebug(i)
   debug "[" & i & " Slice " & sl & " " & DescribeSlice(sl) & "]"
   if noisy then
    debug "parent " & sl->parent
    SliceDebugDumpTree sl
   end if
   count += 1
  end if
 next i
 debug count & " slices found in the slice debug table"
END SUB

FUNCTION SliceDebugCheck(sl as Slice Ptr) as integer
 if sl = 0 then RETURN NO
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = sl then RETURN YES
 next i
 RETURN NO
END FUNCTION

#endif

'This is the dump function accessible by an in-game debug key,
'and is intended for seeing the slice tree, not debugging code
SUB SliceDebugDumpTree(sl as Slice Ptr, byval indent as integer = 0)
 if sl = 0 then exit sub
 CheckTableSlotOK(sl)
 dim s as string
 s = string(indent, " ") & SliceTypeName(sl)
 if sl->Protect then
  s &= " (P)"
 end if

 s &= " lookup:" & SliceLookupCodename(sl) & " tableslot:" & sl->TableSlot & " pos:" & sl->Pos & " size:" & sl->Size.wh
 #ifdef IS_GAME
  if sl->TableSlot then s &= " handle:" & plotslices(sl->TableSlot).handle
 #endif
 if sl->Template then s &= " template"
 if sl->Visible = NO then s &= " visible:false"
 if sl->ExtraVec then
  s &= " extra(len=" & v_len(sl->ExtraVec) & "): "
  for idx as integer = 0 to small(10, v_len(sl->ExtraVec)) - 1
   s &= " " & idx & ":" & sl->ExtraVec[idx]
  next
 end if
 debug s
 SliceDebugDumpTree sl->FirstChild, indent + 1
 SliceDebugDumpTree sl->NextSibling, indent
END SUB

'For debugging the pointers between slices. Not used anywhere
/'
Sub SliceDebugLinks(sl as Slice Ptr, recurse as bool = NO, prefix as string = "", indent as integer = 0)
 if sl = 0 then exit sub
 debug prefix & string(indent + 1, " ") & SliceTypeName(sl) & " " & SliceLookupCodename(sl) & " sl=" & sl & " par=" & sl->Parent & " prev=" & sl->PrevSibling & " next=" & sl->NextSibling
 debug prefix & string(indent + 6, " ") & sl->NumChildren & " children, first=" & sl->FirstChild & " last=" & sl->LastChild
 if sl->FirstChild then
  BUG_IF(sl->FirstChild->Parent <> sl, "bad FirstChild")
 end if
 if sl->LastChild then
  BUG_IF(sl->LastChild->Parent <> sl, "bad LastChild")
 end if
 if recurse then
  SliceDebugLinks sl->FirstChild, recurse, prefix, indent + 1
  SliceDebugLinks sl->NextSibling, recurse, prefix, indent
 end if
End Sub
'/

End Extern
