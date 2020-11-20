'OHRRPGCE CUSTOM - Slice Collection Editor
'(C) Copyright 1997-2008 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't especially crappy. Yay!
'
#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "loading.bi"
#include "thingbrowser.bi"
#include "reloadext.bi"
#ifdef IS_CUSTOM
 #include "custom.bi"
#endif

#include "sliceedit.bi"

'This include contains the default slice collections for special screens
#include "sourceslices.bi"

'==============================================================================

ENUM HideMode
 hideNothing = 0
 hideMenuBG = 1
 hideSlices = 2
 hideMenu = 3
 hideLAST = 3
END ENUM

ENUM SliceMenuItemID
 mnidText = 0            'Not editable
 mnidSlice = 1
 mnidExitMenu = 2        'Exit Menu
 mnidEditingFile = 3     'Editing <collection file>
 mnidCollectionID = 4    '<-Slice collection #->
 mnidCollectionName = 5
 mnidSettingsMenu = 6    'Settings/tools (F8)...
END ENUM

TYPE SliceEditMenuItem
 s as string
 handle as Slice Ptr
 id as SliceMenuItemID
END TYPE

TYPE SpecialLookupCode
 code as integer
 caption as string
 kindlimit as integer
END TYPE

' The slice editor has three different modes:
' -editing a slice group in the .rpg. use_index = YES, collection_file = ""
' -editing an external slice (.collection_file). use_index = NO.
' -editing an existing slice tree. editing_existing = YES, use_index = NO,
'  collection_file normally "", or is the filename the collection was loaded from
'  (but is not necessarily still equal to)
TYPE SliceEditState
 collection_group_number as integer  'Which special lookup codes are available, and part of filename if use_index = YES
 collection_number as integer  'Used only if use_index = YES
 collection_file as string     'Used only if use_index = NO: the file we are currently editing (will be autosaved)
 use_index as bool         'If this is the indexed collection editor; if NO then we are editing
                           'either an external file (collection_file), or some given slice tree (editing_existing)
                           'or both.
                           'When true, the collections are always re-saved when quitting.

 editing_existing as bool  'True if editor was given an existing slice tree (edslice) to edit
 'The following is used only if editing_existing AND collection_file<>""
 existing_matches_file as bool 'Whether the slice tree that was passed in was equal to contents of collection_file.

 expand_dimensions as bool
 expand_visible as bool
 expand_alignment as bool
 expand_special as bool
 expand_padding as bool
 expand_extra as bool
 expand_sort as bool

 recursive as bool
 clipboard as Slice Ptr
 draw_root as Slice Ptr    'The slice to actually draw; either edslice or its parent.
 hide_mode as HideMode
 show_root as bool = YES   'Whether to show edslice
 show_ants as bool = YES   'Whether to draw a box around the selected slice
 show_sizes as bool        'Display sizes in the slice list?
 show_positions as bool    'Display screen positions in the slice list?
 privileged as bool        'Whether can edit properties that are normally off-limits. Non-user collections only.

 ' Internal state of lookup_code_grabber
 editing_lookup_name as bool
 last_lookup_name_edit as double  'Time of last edit

 slicelookup(any) as string
 specialcodes(any) as SpecialLookupCode
 slicemenu(any) as SliceEditMenuItem 'The top-level menu (which lists all slices)
 slicemenust as MenuState            'State of slicemenu() menu

 DECLARE FUNCTION curslice() as Slice ptr
END TYPE

CONST kindlimitNOTHING = -1
CONST kindlimitANYTHING = 0
CONST kindlimitGRID = 1
CONST kindlimitSELECT = 2
CONST kindlimitSPRITE = 3
CONST kindlimitPLANKSELECTABLE = 4
CONST kindlimitTEXT = 5
CONST kindlimitPOSITIONING = 6  'Either Grid or Layout

'------------------------------------------------------------------------------

ENUM EditRuleMode
  erNone              'Used for labels and links
  erIntgrabber
  erEnumgrabber       'Must be used for anything that's an Enum
  erShortStrgrabber   'No full-screen text editor
  erStrgrabber        'Press ENTER for full-screen text editor
  erToggle
  erPercentgrabber    'Edits doubles
  erSinglePercentgrabber 'Edits singles
  erLookupgrabber
END ENUM

TYPE EditRule
  dataptr as any ptr  'It scares the heck out of me that I think this is the best solution
  mode as EditRuleMode
  lower as integer    'Interpreted as percent for percent_grabber
  upper as integer    'Interpreted as percent for percent_grabber
  group as integer    'Marks this rule as a member of a numbered group, the meaning of which is defined in the implementation
  helpkey as string   'actually appended to "sliceedit_" to get the full helpkey
END TYPE

'==============================================================================

DIM SHARED remember_draw_root_pos as XYPair

REDIM SHARED editable_slice_types(9) as SliceTypes
editable_slice_types(0) = SlContainer
editable_slice_types(1) = SlSprite
editable_slice_types(2) = SlText
editable_slice_types(3) = SlRectangle
editable_slice_types(4) = SlLine
editable_slice_types(5) = SlEllipse
editable_slice_types(6) = SlScroll
editable_slice_types(7) = SlSelect
editable_slice_types(8) = SlGrid
editable_slice_types(9) = SlPanel
'editable_slice_types(10) = SlLayout

'==============================================================================

CONST slgrPICKTYPE = 1
CONST slgrPICKXY = 2
CONST slgrPICKWH = 4
CONST slgrPICKCOL = 8
CONST slgrUPDATESPRITE = 16
CONST slgrUPDATERECTCUSTOMSTYLE = 32
CONST slgrUPDATERECTSTYLE = 64
CONST slgrPICKLOOKUP = 128
CONST slgrEDITSWITCHINDEX = 256
CONST slgrBROWSESPRITEASSET = 512
CONST slgrBROWSESPRITEID = 1024
CONST slgrBROWSEBOXBORDER = 2048
CONST slgrLAYOUT2NDDIR = 4096
'--This system won't be able to expand forever ... :(

'==============================================================================

DECLARE SUB slice_editor_main (byref ses as SliceEditState, byref edslice as Slice Ptr)

'Functions that might go better in slices.bas ... we shall see
DECLARE SUB DrawSliceAnts (byval sl as Slice Ptr, byval dpage as integer)

'Functions that use awkward adoption metaphors
DECLARE SUB SliceAdoptSister (byval sl as Slice Ptr)
DECLARE SUB AdjustSlicePosToNewParent (byval sl as Slice Ptr, byval newparent as Slice Ptr)
DECLARE SUB SliceAdoptNiece (byval sl as Slice Ptr)

'Functions only used locally
DECLARE FUNCTION find_special_lookup_code(specialcodes() as SpecialLookupCode, code as integer) as integer
DECLARE FUNCTION lookup_code_forbidden(specialcodes() as SpecialLookupCode, code as integer) as bool
DECLARE FUNCTION slice_editor_forbidden_search(byval sl as Slice Ptr, specialcodes() as SpecialLookupCode, errorstr as string = "") as bool
DECLARE FUNCTION slice_editor_mouse_over (edslice as Slice ptr, menu() as SliceEditMenuItem, state as MenuState) as Slice ptr
DECLARE SUB slice_editor_common_function_keys (byref ses as SliceEditState, edslice as Slice ptr, byref state as MenuState, in_detail_editor as bool)
DECLARE SUB slice_editor_refresh (byref ses as SliceEditState, edslice as Slice Ptr, byref cursor_seek as Slice Ptr)
DECLARE SUB slice_editor_refresh_delete (byref index as integer, slicemenu() as SliceEditMenuItem)
DECLARE SUB slice_editor_refresh_append (byref ses as SliceEditState, id as SliceMenuItemID, caption as string, sl as Slice Ptr=0)
DECLARE SUB slice_editor_refresh_recurse (ses as SliceEditState, byref indent as integer, edslice as Slice Ptr, sl as Slice Ptr, hidden_slice as Slice Ptr)
DECLARE SUB slice_edit_updates (sl as Slice ptr, dataptr as any ptr)
DECLARE SUB slice_edit_detail (byref ses as SliceEditState, edslice as Slice ptr, sl as Slice Ptr)
DECLARE SUB slice_edit_detail_refresh (byref ses as SliceEditState, byref state as MenuState, menu() as string, menuopts as MenuOptions, sl as Slice Ptr, rules() as EditRule)
DECLARE SUB slice_edit_detail_keys (byref ses as SliceEditState, byref state as MenuState, sl as Slice Ptr, rules() as EditRule, usemenu_flag as bool)
DECLARE SUB slice_editor_xy (byref x as integer, byref y as integer, byval focussl as Slice Ptr, byval rootsl as Slice Ptr, byref show_ants as bool)
DECLARE FUNCTION slice_editor_filename(byref ses as SliceEditState) as string
DECLARE SUB slice_editor_load(byref ses as SliceEditState, byref edslice as Slice Ptr, filename as string, edit_separately as bool)
DECLARE SUB slice_editor_import_file(byref ses as SliceEditState, byref edslice as Slice Ptr, edit_separately as bool)
DECLARE SUB slice_editor_import_prompt(byref ses as SliceEditState, byref edslice as Slice ptr)
DECLARE SUB slice_editor_export_prompt(byref ses as SliceEditState, byref edslice as Slice ptr)
DECLARE FUNCTION slice_editor_save_when_leaving(byref ses as SliceEditState, edslice as Slice Ptr) as bool
DECLARE FUNCTION slice_lookup_code_caption(byval code as integer, slicelookup() as string) as string
DECLARE FUNCTION lookup_code_grabber(byref code as integer, byref ses as SliceEditState, lowerlimit as integer, upperlimit as integer) as bool
DECLARE FUNCTION edit_slice_lookup_codes(byref ses as SliceEditState, slicelookup() as string, byval start_at_code as integer, byval slicekind as SliceTypes) as integer
DECLARE FUNCTION slice_caption (byref ses as SliceEditState, edslice as Slice Ptr, sl as Slice Ptr) as string
DECLARE SUB slice_editor_copy(byref ses as SliceEditState, byval slice as Slice Ptr, byval edslice as Slice Ptr)
DECLARE SUB slice_editor_paste(byref ses as SliceEditState, byval slice as Slice Ptr, byval edslice as Slice Ptr)
DECLARE SUB slice_editor_reset_slice(byref ses as SliceEditState, sl as Slice ptr)
DECLARE SUB slice_editor_focus_on_slice(byref ses as SliceEditState, edslice as Slice ptr)
DECLARE SUB init_slice_editor_for_collection_group(byref ses as SliceEditState, byval group as integer)
DECLARE SUB append_specialcode (byref ses as SliceEditState, byval code as integer, byval kindlimit as integer=kindlimitANYTHING)
DECLARE FUNCTION special_code_kindlimit_check(byval kindlimit as integer, byval slicekind as SliceTypes) as bool
DECLARE FUNCTION slice_edit_detail_browse_slicetype(byref slice_type as SliceTypes, allowed_types() as SliceTypes) as bool
DECLARE SUB preview_SelectSlice_parents (byval sl as Slice ptr)
DECLARE SUB slice_editor_settings_menu(byref ses as SliceEditState, byref edslice as Slice ptr, in_detail_editor as bool)
DECLARE SUB slice_editor_save_settings(byref ses as SliceEditState)
DECLARE SUB slice_editor_load_settings(byref ses as SliceEditState)
DECLARE FUNCTION collection_context(edslice as Slice ptr) as SliceCollectionContext ptr

'Slice EditRule convenience functions
DECLARE SUB sliceed_rule (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as integer ptr, lower as integer=0, upper as integer=0, group as integer = 0)
DECLARE SUB sliceed_rule_str (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as string ptr, upper as integer=0, group as integer = 0)
DECLARE SUB sliceed_rule_enum (rules() as EditRule, helpkey as string, dataptr as ssize_t ptr, lower as integer=0, upper as integer=0, group as integer = 0)
DECLARE SUB sliceed_rule_double (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as double ptr, lower as integer=0, upper as integer=100, group as integer = 0)
DECLARE SUB sliceed_rule_single (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as single ptr, lower as integer=0, upper as integer=100, group as integer = 0)
DECLARE SUB sliceed_rule_tog (rules() as EditRule, helpkey as string, dataptr as bool ptr, group as integer=0)
DECLARE SUB sliceed_rule_none (rules() as EditRule, helpkey as string, group as integer = 0)

'==============================================================================

DIM HorizCaptions(2) as string
HorizCaptions(0) = "Left"
HorizCaptions(1) = "Center"
HorizCaptions(2) = "Right"
DIM VertCaptions(2) as string
VertCaptions(0) = "Top"
VertCaptions(1) = "Center"
VertCaptions(2) = "Bottom"
REDIM SHARED BorderCaptions(-2 TO -1) as string
BorderCaptions(-2) = "None"
BorderCaptions(-1) = "Line"
REDIM SHARED TransCaptions(0 TO 3) as string
TransCaptions(0) = "Solid"       'transOpaque
TransCaptions(1) = "Fuzzy"       'transFuzzy
TransCaptions(2) = "Hollow"      'transHollow
TransCaptions(3) = "Blend (transparent)" 'transBlend
REDIM SHARED AutoSortCaptions(0 TO 5) as string
AutoSortCaptions(0) = "None"
AutoSortCaptions(1) = "Custom"
AutoSortCaptions(2) = "by Y"
AutoSortCaptions(3) = "by top edge"
AutoSortCaptions(4) = "by center Y"
AutoSortCaptions(5) = "by bottom edge"
REDIM SHARED FillModeCaptions(2) as string
FillModeCaptions(0) = "Full"
FillModeCaptions(1) = "Horizontal"
FillModeCaptions(2) = "Vertical"
REDIM SHARED CoverModeCaptions(3) as string
CoverModeCaptions(0) = "NO"
CoverModeCaptions(1) = "Horizontal"
CoverModeCaptions(2) = "Vertical"
CoverModeCaptions(3) = "Full"
REDIM SHARED DirectionCaptions(3) as string
DirectionCaptions(0) = "Up"
DirectionCaptions(1) = "Right"
DirectionCaptions(2) = "Down"
DirectionCaptions(3) = "Left"
REDIM SHARED BlendModeCaptions(blendModeLAST) as string
BlendModeCaptions(blendModeNormal)   = "Normal"
BlendModeCaptions(blendModeAdd)      = "Add"
BlendModeCaptions(blendModeMultiply) = "Multiply"
REDIM SHARED BlendAlgoCaptions(blendAlgoLAST) as string
BlendAlgoCaptions(blendAlgoDither)     = "Dither"
BlendAlgoCaptions(blendAlgoLessDither) = "Less dither"
BlendAlgoCaptions(blendAlgoNoDither)   = "No dithering"

'==============================================================================

FUNCTION align_caption(align as AlignType, vertical as bool) as string
 IF vertical THEN RETURN VertCaptions(align) ELSE RETURN HorizCaptions(align)
END FUNCTION

FUNCTION dir_align_caption(dirn as DirNum, align as AlignType) as string
 RETURN align_caption(align, dirn = dirUp ORELSE dirn = dirDown)
END FUNCTION

FUNCTION anchor_and_align_string(anchor as AlignType, align as AlignType, vertical as bool) as string
 IF anchor = align THEN RETURN align_caption(anchor, vertical)
 RETURN align_caption(anchor, vertical) & "-" & align_caption(align, vertical)
END FUNCTION

'Grabber to switch between all 9 anchor-align combinations
FUNCTION anchor_and_align_grabber(byref anchor as AlignType, byref align as AlignType) as bool
 DIM temp as integer = anchor * 3 + align
 DIM ret as bool = intgrabber(temp, 0, 8)
 anchor = temp \ 3
 align = temp MOD 3
 RETURN ret
END FUNCTION

FUNCTION clamp_caption(align as AlignType, vertical as bool) as string
 IF align = alignNone THEN RETURN "No"
 RETURN "to " & IIF(vertical, VertCaptions(align), HorizCaptions(align))
END FUNCTION

'==============================================================================

SUB init_slice_editor_for_collection_group(byref ses as SliceEditState, byval group as integer)
 REDIM ses.specialcodes(0) as SpecialLookupCode
 SELECT CASE group
  CASE SL_COLLECT_EDITOR:
   'SL_COLLECT_EDITOR allows access to all lookup codes, but for certain filenames
   'this puts relevant codes at the top, and also limits which slice types can
   'be used with those codes.
   SELECT CASE trimpath(ses.collection_file)
    CASE "choose_rpg.slice"
     append_specialcode ses, SL_EDITOR_SPLASH_MENU, kindlimitANYTHING
    CASE "thingbrowser.slice"
     append_specialcode ses, SL_EDITOR_THINGBROWSER_THINGLIST, kindlimitPOSITIONING
     append_specialcode ses, SL_PLANK_HOLDER, kindlimitANYTHING
     append_specialcode ses, SL_PLANK_MENU_SELECTABLE, kindlimitPLANKSELECTABLE
     append_specialcode ses, SL_EDITOR_THINGBROWSER_NOSCROLL_AREA, kindlimitANYTHING
     append_specialcode ses, SL_EDITOR_THINGBROWSER_BACK_HOLDER, kindlimitANYTHING
     append_specialcode ses, SL_EDITOR_THINGBROWSER_NEW_HOLDER, kindlimitANYTHING
     append_specialcode ses, SL_EDITOR_THINGBROWSER_FILTER_HOLDER, kindlimitANYTHING
     append_specialcode ses, SL_EDITOR_THINGBROWSER_MODE_INDICATOR, kindlimitTEXT
     append_specialcode ses, SL_EDITOR_THINGBROWSER_TYPE_QUERY, kindlimitTEXT
     append_specialcode ses, SL_EDITOR_THINGBROWSER_FILTER_TEXT, kindlimitTEXT
    CASE "prompt_for_string.slice"
     append_specialcode ses, SL_EDITOR_PROMPT_FOR_STRING_TEXT, kindlimitTEXT
     append_specialcode ses, SL_EDITOR_PROMPT_FOR_STRING_CAPTION, kindlimitTEXT
   END SELECT
  CASE SL_COLLECT_STATUSSCREEN:
   append_specialcode ses, SL_STATUS_STATLIST, kindlimitGRID
   append_specialcode ses, SL_STATUS_PAGE_SELECT, kindlimitSELECT
   append_specialcode ses, SL_STATUS_PORTRAIT, kindlimitSPRITE
   append_specialcode ses, SL_STATUS_WALKABOUT, kindlimitSPRITE
   append_specialcode ses, SL_STATUS_BATTLESPRITE, kindlimitSPRITE
   append_specialcode ses, SL_STATUS_HIDE_IF_NO_HP, kindlimitANYTHING
   append_specialcode ses, SL_STATUS_HIDE_IF_NO_MP, kindlimitANYTHING
   append_specialcode ses, SL_STATUS_HIDE_IF_NO_LMP, kindlimitANYTHING
   append_specialcode ses, SL_STATUS_HIDE_IF_MAX_LEV, kindlimitANYTHING
   append_specialcode ses, SL_STATUS_HIDE_IF_NO_PORTRAIT, kindlimitANYTHING
  CASE SL_COLLECT_STATUSSTATPLANK:
   append_specialcode ses, SL_PLANK_HOLDER, kindlimitANYTHING
  CASE SL_COLLECT_ITEMSCREEN:
   append_specialcode ses, SL_ITEM_ITEMLIST, kindlimitGRID
   append_specialcode ses, SL_ITEM_EXITBUTTON, kindlimitANYTHING
   append_specialcode ses, SL_ITEM_SORTBUTTON, kindlimitANYTHING
   append_specialcode ses, SL_ITEM_TRASHBUTTON, kindlimitANYTHING
   append_specialcode ses, SL_PLANK_MENU_SELECTABLE, kindlimitPLANKSELECTABLE
  CASE SL_COLLECT_ITEMPLANK:
   append_specialcode ses, SL_PLANK_HOLDER, kindlimitANYTHING
   append_specialcode ses, SL_PLANK_MENU_SELECTABLE, kindlimitPLANKSELECTABLE
  CASE SL_COLLECT_SPELLSCREEN:
   append_specialcode ses, SL_SPELL_LISTLIST, kindlimitGRID
   append_specialcode ses, SL_SPELL_SPELLLIST, kindlimitGRID
   append_specialcode ses, SL_SPELL_HIDE_IF_NO_LIST, kindlimitANYTHING
   append_specialcode ses, SL_SPELL_CANCELBUTTON, kindlimitANYTHING
  CASE SL_COLLECT_SPELLLISTPLANK:
   append_specialcode ses, SL_PLANK_HOLDER, kindlimitANYTHING
   append_specialcode ses, SL_PLANK_MENU_SELECTABLE, kindlimitPLANKSELECTABLE
  CASE SL_COLLECT_SPELLPLANK:
   append_specialcode ses, SL_PLANK_HOLDER, kindlimitANYTHING
   append_specialcode ses, SL_PLANK_MENU_SELECTABLE, kindlimitPLANKSELECTABLE
  CASE SL_COLLECT_VIRTUALKEYBOARDSCREEN:
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_BUTTON, kindlimitANYTHING
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_BUTTONTEXT, kindlimitTEXT
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_SELECT, kindlimitSELECT
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_ENTRYTEXT, kindlimitTEXT
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_SHIFT, kindlimitPLANKSELECTABLE
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_SYMBOLS, kindlimitPLANKSELECTABLE
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_DEL, kindlimitPLANKSELECTABLE
   append_specialcode ses, SL_VIRTUAL_KEYBOARD_ENTER, kindlimitPLANKSELECTABLE
 END SELECT
END SUB

SUB append_specialcode (byref ses as SliceEditState, byval code as integer, byval kindlimit as integer=kindlimitANYTHING)
 DIM index as integer = -1
 FOR i as integer = 0 TO UBOUND(ses.specialcodes)
  IF ses.specialcodes(i).code = 0 THEN
   index = i
   EXIT FOR
  END IF
 NEXT i
 IF index = -1 THEN
  REDIM PRESERVE ses.specialcodes(UBOUND(ses.specialcodes) + 1) as SpecialLookupCode
  index = UBOUND(ses.specialcodes)
 END IF
 WITH ses.specialcodes(index)
  .code = code
  .caption = SliceLookupCodeName(code)
  .kindlimit = kindlimit
 END WITH
END SUB

LOCAL FUNCTION create_draw_root (ses as SliceEditState) as Slice ptr
 'Instead of parenting to the actual screen slice, parent to a
 'fake screen slice which is the size of the ingame screen.
 'Also, center, so that if you're running at a higher resolution than in-game, the
 'menu doesn't overlap so much.

 DIM use_game_res as bool = ses.collection_group_number <> SL_COLLECT_EDITOR

 DIM rect as RectangleSliceData
 rect.bgcol = uilook(uiBackground)
 rect.border = -2  'None
 DIM ret as Slice ptr = NewRectangleSlice(NULL, rect)
 WITH *ret
  .Pos = remember_draw_root_pos
  IF use_game_res ANDALSO gen(genResolutionX) > 0 THEN  'We might not have loaded a game yet
   .Width = gen(genResolutionX)
   .Height = gen(genResolutionY)
  ELSE
   .Size = get_resolution()
  END IF
  .AlignHoriz = alignRight
  .AlignVert = alignMiddle
  .AnchorHoriz = alignRight
  .AnchorVert = alignMiddle
 END WITH
 ' But if the editor resolution is smaller than the game's, add an offset so that
 ' the top left corner of the 'screen' is visible.
 ' This is crude because the 'screen' will shift if the user resizes the window,
 ' but we can't just recenter it every tick because then F6 won't work.
 RefreshSliceScreenPos ret
 ret->X -= small(0, ret->ScreenX)
 ret->Y -= small(0, ret->ScreenY)
 RETURN ret
END FUNCTION

' Edit a group of slice collections - this is the overload used by the slice editor menus in Custom.
' In this mode, the editor loads and saves collections to disk when you exit
' privileged: true if should be allowed to edit things that are hidden from users
SUB slice_editor (group as integer = SL_COLLECT_USERDEFINED, filename as string = "", privileged as bool = NO)
 DIM ses as SliceEditState
 ses.collection_group_number = group
 ses.collection_file = filename
 ses.use_index = (filename = "")
 ses.privileged = privileged

 'This creates ses.draw_root and loads edslice (if it exists, otherwise
 'a new blank slice)
 DIM edslice as Slice ptr
 slice_editor_load ses, edslice, slice_editor_filename(ses), YES

 slice_editor_main ses, edslice

 remember_draw_root_pos = ses.draw_root->Pos
 DeleteSlice @ses.draw_root
END SUB

' Edit an existing slice tree.
' recursive is true if using Ctrl+F. Probably should not use otherwise.
' privileged: true if should be allowed to edit things that are hidden from users
' filename: useful mainly for group = SL_COLLECT_EDITOR, to define the sub-group,
' and also the file to which to save (doesn't save by default if edslice doesn't match the file)
SUB slice_editor (byref edslice as Slice Ptr, byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", recursive as bool = NO, privileged as bool = NO)
 DIM ses as SliceEditState
 ses.collection_group_number = group
 ses.collection_file = filename
 ses.use_index = NO  'Can't browse collections
 ses.editing_existing = YES
 ses.recursive = recursive
 ses.privileged = privileged

 IF LEN(filename) THEN
  ses.existing_matches_file = slice_collection_has_changed(edslice, filename) = NO
 END IF

 DIM rootslice as Slice ptr

 IF recursive THEN
  'Note that we don't call create_draw_root() to create a temp root slice; this is a bit unfortunate
  'because it may cause some subtle differences
  ses.draw_root = edslice
 ELSE
  ' Temporarily reparent the root of the slice tree!
  rootslice = FindRootSlice(edslice)
  ses.draw_root = create_draw_root(ses)
  SetSliceParent rootslice, ses.draw_root
 END IF

 slice_editor_main ses, edslice

 IF recursive = NO THEN
  OrphanSlice rootslice
  remember_draw_root_pos = ses.draw_root->Pos
  DeleteSlice @ses.draw_root
 END IF
END SUB

' The main function of the slice editor is not called directly, call a slice_editor() overload instead.
SUB slice_editor_main (byref ses as SliceEditState, byref edslice as Slice Ptr)
 init_slice_editor_for_collection_group(ses, ses.collection_group_number)

 slice_editor_load_settings ses

 REDIM PRESERVE editable_slice_types(9)
 IF ses.privileged THEN a_append editable_slice_types(), slLayout

 '--user-defined slice lookup codes
 REDIM ses.slicelookup(10) as string
 load_string_list ses.slicelookup(), workingdir & SLASH & "slicelookup.txt"
 IF UBOUND(ses.slicelookup) < 1 THEN
  REDIM ses.slicelookup(1) as string
 END IF

 REDIM ses.slicemenu(0) as SliceEditMenuItem
 REDIM plainmenu(0) as string 'FIXME: This is a hack because I didn't want to re-implement standardmenu right now

 DIM byref state as MenuState = ses.slicemenust
 WITH state
  .need_update = YES
  .autosize = YES
  .autosize_ignore_pixels = 14
 END WITH
 DIM menuopts as MenuOptions
 WITH menuopts
  .edged = YES
  .itemspacing = -1
  .highlight = YES
 END WITH

 DIM cursor_seek as Slice Ptr = 0

 DIM jump_to_collection as integer

 '--Ensure all the slices are updated before the loop starts
 RefreshSliceTreeScreenPos ses.draw_root

 DIM vpages_were_32bit as bool = vpages_are_32bit()
 push_and_reset_gfxio_state
 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN
   IF ses.hide_mode <> hideNothing THEN
    ses.hide_mode = hideNothing
   ELSE
    IF slice_editor_save_when_leaving(ses, edslice) THEN EXIT DO
   END IF
  END IF
  slice_editor_common_function_keys ses, edslice, state, NO  'F, R, V, F4, F6, F7, F8, Ctrl+F3, Ctrl+F4
  #IFDEF IS_GAME
   IF keyval(scF1) > 1 THEN show_help "sliceedit_game"
  #ELSE
   IF keyval(scF1) > 1 THEN show_help "sliceedit"
  #ENDIF
  IF keyval(scF5) > 1 THEN
   ses.show_root = NOT ses.show_root
   cursor_seek = ses.curslice
   state.need_update = YES
  END IF

  IF state.need_update = NO ANDALSO ses.slicemenu(state.pt).id = mnidCollectionName THEN
   IF strgrabber(collection_context(edslice)->name) THEN state.need_update = YES
  END IF

  IF state.need_update = NO ANDALSO ses.curslice <> NULL THEN
   IF keyval(scH) > 1 THEN
    'Toggle editor visibility of children
    IF ses.curslice->NumChildren > 0 THEN
     ses.curslice->EditorHideChildren XOR= YES
    ELSE
     ses.curslice->EditorHideChildren = NO
    END IF
    state.need_update = YES
   END IF
  END IF

  ' Highlighting and selecting slices with the mouse
  IF state.need_update = NO THEN
   DIM topmost as Slice ptr
   topmost = slice_editor_mouse_over(edslice, ses.slicemenu(), state)
   IF topmost ANDALSO (readmouse().release AND mouseLeft) THEN
    cursor_seek = topmost
    state.need_update = YES
   END IF
  END IF

  ' Activate menu item
  DIM menuitemid as integer = ses.slicemenu(state.pt).id
  IF state.need_update = NO ANDALSO enter_space_click(state) THEN
   IF menuitemid = mnidExitMenu THEN
    IF slice_editor_save_when_leaving(ses, edslice) THEN EXIT DO
   ELSEIF menuitemid = mnidSettingsMenu THEN
    slice_editor_settings_menu ses, edslice, NO
    state.need_update = YES
   ELSEIF menuitemid = mnidEditingFile THEN
    ' Selected the 'Editing <collection file>' menu item
    slice_editor_import_file ses, edslice, YES   'sets need_update
   ELSEIF menuitemid = mnidSlice THEN
    cursor_seek = ses.curslice
    slice_edit_detail ses, edslice, ses.curslice
    state.need_update = YES
   END IF
  END IF

  IF ses.use_index THEN
   IF menuitemid = mnidCollectionID THEN
    '--Browse collections
    jump_to_collection = ses.collection_number
    IF intgrabber(jump_to_collection, 0, 32767, , , , NO) THEN  'Disable copy/pasting
     IF slice_editor_save_when_leaving(ses, edslice) THEN
      ses.collection_file = ""
      ses.collection_number = jump_to_collection
      slice_editor_load ses, edslice, slice_editor_filename(ses), YES
      state.need_update = YES
     END IF
    END IF
   END IF
  END IF
  IF keyval(scF2) > 1 THEN slice_editor_export_prompt ses, edslice
#IFDEF IS_CUSTOM
  '--Overwriting import can't be allowed when there are certain slices expected by the engine,
  '--and no point allowing editing external files in-game, so just disable in-game.
  '--Furthermore, loading new collections when .editing_existing is unimplemented anyway
  '--(checked by slice_editor_export_key())
  IF keyval(scCtrl) = 0 ANDALSO keyval(scF3) > 1 THEN slice_editor_import_prompt ses, edslice
#ENDIF
  IF state.need_update = NO AND (keyval(scPlus) > 1 OR keyval(scNumpadPlus)) THEN
   DIM slice_type as SliceTypes
   IF slice_edit_detail_browse_slicetype(slice_type, editable_slice_types()) THEN
    IF ses.curslice <> NULL ANDALSO ses.curslice <> edslice THEN
     InsertSliceBefore ses.curslice, NewSliceOfType(slice_type)
    ELSE
     cursor_seek = NewSliceOfType(slice_type, edslice)
    END IF
    state.need_update = YES
   END IF
  END IF

  IF state.need_update = NO THEN
   IF copy_keychord() THEN
    slice_editor_copy ses, ses.curslice, edslice
   ELSEIF paste_keychord() THEN
    slice_editor_paste ses, ses.curslice, edslice
    state.need_update = YES
   END IF
  END IF

  'Special handling for the currently selected slice
  preview_SelectSlice_parents ses.curslice

  IF state.need_update = NO ANDALSO ses.curslice <> NULL THEN

   IF keyval(scCtrl) > 0 ANDALSO keyval(scF) > 1 THEN
    'Edit this slice alone ("fullscreen")
    slice_editor_save_settings ses  'So will be loaded by recursive editor
    slice_editor ses.curslice, ses.collection_group_number, ses.collection_file, YES
    slice_editor_load_settings ses
    state.need_update = YES

   ELSEIF keyval(scDelete) > 1 THEN

    IF ses.privileged = NO ANDALSO slice_editor_forbidden_search(ses.curslice, ses.specialcodes()) THEN
     notification "Can't delete special/protected slices!"
     CONTINUE DO
    END IF
    IF ses.curslice = edslice THEN
     notification "Can't delete the root slice!"
     CONTINUE DO
    ELSE
     IF yesno("Delete this " & SliceTypeName(ses.curslice) & " slice?", NO) THEN
      slice_editor_refresh_delete state.pt, ses.slicemenu()
      state.need_update = YES
     END IF
    END IF

   ELSEIF keyval(scShift) > 0 THEN

    IF keyval(ccUp) > 1 THEN
     SwapSiblingSlices ses.curslice, ses.curslice->PrevSibling
     cursor_seek = ses.curslice
     state.need_update = YES
    ELSEIF keyval(ccDown) > 1 THEN
     SwapSiblingSlices ses.curslice, ses.curslice->NextSibling
     cursor_seek = ses.curslice
     state.need_update = YES
    ELSEIF keyval(ccRight) > 1 THEN
     SliceAdoptSister ses.curslice
     cursor_seek = ses.curslice
     state.need_update = YES
    ELSEIF keyval(ccLeft) > 1 THEN
     IF ses.curslice->parent <> edslice THEN
      SliceAdoptNiece ses.curslice
      cursor_seek = ses.curslice
      state.need_update = YES
     END IF
    END IF

   ELSEIF keyval(scCtrl) > 0 THEN '--ctrl, not shift

    IF keyval(ccUp) > 1 THEN
     cursor_seek = ses.curslice->prevSibling
     state.need_update = YES
    ELSEIF keyval(ccDown) > 1 THEN
     cursor_seek = ses.curslice->nextSibling
     state.need_update = YES
    ELSEIF keyval(ccLeft) > 1 THEN
     cursor_seek = ses.curslice->parent
     state.need_update = YES
    ELSEIF keyval(ccRight) > 1 THEN
     cursor_seek = ses.curslice->firstChild
     state.need_update = YES
    END IF

   ELSE '--neither shift nor ctrl

    IF keyval(ccLeft) > 1 THEN
     cursor_seek = (ses.curslice)->parent
     state.need_update = YES
    END IF

   END IF

  END IF '--end IF state.need_update = NO AND ses.curslice

  ' Window size change
  IF UpdateScreenSlice() THEN state.need_update = YES

  DIM topmost as Slice ptr

  IF state.need_update THEN
   slice_editor_refresh(ses, edslice, cursor_seek)
   state.need_update = NO
   cursor_seek = NULL
   topmost = slice_editor_mouse_over(edslice, ses.slicemenu(), state)
  ELSE
   topmost = slice_editor_mouse_over(edslice, ses.slicemenu(), state)
   ' If there's slice under the mouse, clicking should focus on that, not any menu item there.
   ' (Right-clicking still works to select a menu item)
   IF topmost = NULL ORELSE (readmouse.buttons AND mouseLeft) = 0 THEN
    usemenu state
   END IF
  END IF

  draw_background vpages(dpage), bgChequer

  IF ses.hide_mode <> hideSlices THEN
   DrawSlice ses.draw_root, dpage
  END IF
  IF ses.show_ants THEN
   IF ses.curslice THEN
    DrawSliceAnts ses.curslice, dpage
   END IF
   IF topmost THEN
    DrawSliceAnts topmost, dpage
   END IF
  END IF
  IF ses.hide_mode <> hideMenu THEN

   'Determine the colour for each menu item: copy the visible part of the menu into plainmenu()
   REDIM plainmenu(state.last) as string
   FOR i as integer = state.top TO small(UBOUND(plainmenu), state.top + state.size)
    plainmenu(i) = ses.slicemenu(i).s
    DIM sl as Slice ptr = ses.slicemenu(i).handle
    DIM col as integer = -1
    IF sl THEN
     IF sl->Visible = NO THEN
      col = uilook(uiSelectedDisabled + IIF(state.pt = i, global_tog, 0))
     ELSEIF sl->EditorColor ANDALSO state.pt <> i THEN
      'Don't override normal highlight
      col = sl->EditorColor
     END IF
     IF col > -1 THEN
      plainmenu(i) = fgcol_text(plainmenu(i), col)
     END IF
    END IF
   NEXT i

   menuopts.drawbg = (ses.hide_mode <> hideMenuBG)
   standardmenu plainmenu(), state, 8, 0, dpage, menuopts
   draw_fullscreen_scrollbar state, 0, dpage, alignLeft
   wrapprintbg "+ to add a slice. SHIFT+arrows to reorder", 8, pBottom, uilook(uiText), dpage, menuopts.drawbg, 9999  'never wraps
  END IF

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 '--free the clipboard if there is something in it
 IF ses.clipboard THEN DeleteSlice @ses.clipboard

 IF vpages_were_32bit = NO THEN
  switch_to_8bit_vpages
 ELSE
  switch_to_32bit_vpages
 END IF
 pop_gfxio_state

 slice_editor_save_settings ses
END SUB

'Get the SliceCollectionContext in which shared data for this slice collection is stored
'(edslice may be a subtree, so we search up the tree)
FUNCTION collection_context(edslice as Slice ptr) as SliceCollectionContext ptr
 WHILE edslice
  IF *edslice->Context IS SliceCollectionContext THEN
   RETURN CAST(SliceCollectionContext ptr, edslice->Context)
  END IF
  edslice = edslice->Parent
 WEND
 debug "Can't find a SliceCollectionContext"
 RETURN NULL
END FUNCTION

'Note: a lot of this is duplicated, and the keys are documented, in SliceEditSettingsMenu
SUB slice_editor_common_function_keys(byref ses as SliceEditState, edslice as Slice ptr, byref state as MenuState, in_detail_editor as bool)
 IF state.need_update = NO ANDALSO keyval(scCtrl) = 0 THEN  'need_update=NO ensures not a string field
  IF keyval(scF) > 1 THEN
   slice_editor_focus_on_slice ses, edslice
   state.need_update = YES
  END IF
  IF keyval(scR) > 1 THEN
   slice_editor_reset_slice ses, ses.curslice
   state.need_update = YES
  END IF
  IF keyval(scV) > 1 THEN
   'Toggle visibility (does nothing on Select slice children)
   ses.curslice->Visible XOR= YES
   state.need_update = YES
  END IF
 END IF

 IF keyval(scCtrl) = 0 ANDALSO keyval(scF4) > 1 THEN ses.hide_mode = (ses.hide_mode + 1) MOD (hideLAST + 1)
 IF keyval(scF6) > 1 THEN
  'Move around our view on this slice collection.
  'We move around the real rool, not draw_root, as it affects screen positions
  'even if it's not drawn. The root gets deleted when leaving slice_editor, so
  'changes are temporary.
  DIM true_root as Slice ptr = FindRootSlice(edslice)
  slice_editor_xy true_root->X, true_root->Y, ses.draw_root, edslice, ses.show_ants
  state.need_update = YES
 END IF
 IF keyval(scF7) > 1 THEN ses.show_ants = NOT ses.show_ants
 IF keyval(scF8) > 1 THEN
  slice_editor_settings_menu ses, edslice, in_detail_editor
  state.need_update = YES
 END IF
 IF keyval(scCtrl) > 0 ANDALSO keyval(scF3) > 1 THEN
  'Switching to 32 bit color depth allows 32-bit and smooth-scaled sprites,
  'but breaks sprite dissolves
  toggle_32bit_vpages
  state.need_update = YES  'smoothing menu item needs update
 END IF
 IF keyval(scCtrl) > 0 ANDALSO keyval(scF4) > 1 ANDALSO NOT vpages_are_32bit THEN
  loopvar gen(gen8bitBlendAlgo), 0, blendAlgoLAST
  show_overlay_message "Blending with " & BlendAlgoCaptions(gen(gen8bitBlendAlgo)), 1.2
  state.need_update = YES
 END IF
END SUB

FUNCTION SliceEditState.curslice() as Slice ptr
 IF slicemenust.pt > UBOUND(slicemenu) THEN RETURN NULL
 RETURN slicemenu(slicemenust.pt).handle
END FUNCTION

'Sets a slice and all of its ancestors as the selected child of their parent, if a Select slice.
SUB preview_SelectSlice_parents (byval sl as Slice ptr)
 IF sl = 0 THEN EXIT SUB
 DIM par as Slice ptr = sl->parent
 DIM ch as Slice ptr = sl
 DO WHILE par
  IF par->SliceType = slSelect THEN
   ChangeSelectSlice par, , SliceIndexAmongSiblings(ch)
  END IF
  ch = par
  par = par->parent
 LOOP
END SUB

'Sets ->EditorColor for each slice in menu() to highlight the slices that the mouse is over.
'Returns the topmost non-ignored slice that the mouse is over, or NULL if none.
FUNCTION slice_editor_mouse_over (edslice as Slice ptr, slicemenu() as SliceEditMenuItem, state as MenuState) as Slice ptr
 FOR idx as integer = 0 TO UBOUND(slicemenu)
  IF slicemenu(idx).handle THEN
   slicemenu(idx).handle->EditorColor = uilook(uiMenuItem)
  END IF
 NEXT

 DIM parent as Slice ptr = edslice
 'We want to allow finding edslice too (FindSliceAtPoint will ignore parent), but this
 'won't work when editing an existing slice tree.
 IF edslice->Parent THEN parent = edslice->Parent
 DIM byref mouse as MouseInfo = readmouse()
 DIM topmost as Slice ptr = NULL
 DIM idx as integer = 0
 DO
  ' Search for visible slices
  ' (FindSliceAtPoint returns slices starting from the bottommost. We loop through every
  ' slice at this point (indexed by 'idx'))
  DIM temp as integer = idx  'modified byref
  DIM sl as Slice ptr = FindSliceAtPoint(parent, mouse.pos, temp, YES, YES)
  IF sl = 0 THEN EXIT DO

  'Ignore various invisible types of slices. Don't ignore Scroll slices because they may have a scrollbar.
  'Ignore Map slices because transparent overhead layers makes it impossible to
  'click on things parented to map layers below.
  SELECT CASE sl->SliceType
   CASE slRectangle, slSprite, slText, slEllipse, slScroll
    topmost = sl
   CASE slLine
    'TODO: test how close the mouse is to the line, rather than giving it
    'a huge hitbox
    topmost = sl
   CASE slGrid
    IF cptr(GridSliceData ptr, sl->SliceData)->show THEN
     topmost = sl
    END IF
  END SELECT

  sl->EditorColor = uilook(uiText)
  idx += 1
 LOOP

 IF topmost THEN
  topmost->EditorColor = uilook(uiDescription)
 END IF
 RETURN topmost
END FUNCTION

'--Find index in specialcodes(), or -1
FUNCTION find_special_lookup_code(specialcodes() as SpecialLookupCode, code as integer) as integer
 IF code >= 0 THEN RETURN -1
 FOR i as integer = 0 TO UBOUND(specialcodes)
  IF code = specialcodes(i).code THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

'--Returns whether this lookup code is forbidden, meaning it's special and not whitelisted by specialcodes
FUNCTION lookup_code_forbidden(specialcodes() as SpecialLookupCode, code as integer) as bool
 RETURN code < 0 ANDALSO find_special_lookup_code(specialcodes(), code) = -1
END FUNCTION

'--Returns whether one of the descendents is forbidden
FUNCTION slice_editor_forbidden_search(byval sl as Slice Ptr, specialcodes() as SpecialLookupCode, errorstr as string = "") as bool
 IF sl = 0 THEN RETURN NO
 IF sl->Protect THEN
  errorstr = SlicePath(sl) & " is protected"
  RETURN YES
 END IF
 IF lookup_code_forbidden(specialcodes(), sl->Lookup) THEN
  errorstr = SlicePath(sl) & " has forbidden lookup code '" & SliceLookupCodename(sl->Lookup) & "'"
  RETURN YES
 END IF
 IF a_find(editable_slice_types(), cint(sl->SliceType)) < 0 THEN
  errorstr = SlicePath(sl) & " is a disallowed slice type"
  RETURN YES
 END IF
 DIM ch as Slice ptr = sl->FirstChild
 WHILE ch
  IF slice_editor_forbidden_search(ch, specialcodes(), errorstr) THEN RETURN YES
  ch = ch->NextSibling
 WEND
 RETURN NO
END FUNCTION

SUB slice_editor_load(byref ses as SliceEditState, byref edslice as Slice Ptr, filename as string, edit_separately as bool)
 ' Check for programmer error (doesn't work because of the games slice_editor plays with the draw_root)
 BUG_IF(ses.editing_existing, "Can't load when editing existing collection")
 DIM newcollection as Slice Ptr
 newcollection = NewSlice
 WITH *newcollection  'Defaults only
  .SliceType = slContainer
  .Fill = YES
 END WITH
 IF isfile(filename) THEN
  SliceLoadFromFile newcollection, filename, , ses.collection_number
 ELSE
  'Collection root slices should have contexts
  VAR context = NEW SliceCollectionContext
  context->id = ses.collection_number
  newcollection->Context = context
 END IF

 'Special case fix: in the ancient slicetest.rpg file, collection 0 was rooted by a Root slice
 '(now loaded as a Special). Don't know whether other games were affected.
 IF ses.collection_file = "" THEN
  IF newcollection->SliceType = slSpecial THEN
   newcollection->SliceType = slContainer
  END IF
 END IF

 '--You can export slice collections from the in-game slice debugger. These
 '--collections are full of forbidden slices, so we must detect these and
 '--prevent importing. Attempting to do so instead will open a new editor.
 DIM forbidden_error as string
 IF edit_separately = NO ANDALSO ses.privileged = NO ANDALSO _
    slice_editor_forbidden_search(newcollection, ses.specialcodes(), forbidden_error) THEN
  IF ses.collection_file = "" THEN
   'Trying to import a tree into an in-game collection
   '(TODO: Yes, this solution really sucks, maybe clean away special slices instead)
   DIM msg as string
   msg = "The slice collection you're trying to import includes a disallowed " _
         "slice (see below), probably because it's been exported from a game " _
         "or is the wrong type of collection. You can't import this " _
         "collection, but it will be now be opened in a new instance of the " _
         "slice collection editor so that you may view, edit, and reexport " _
         "it. Exit the editor to go back to your previous slice collection. " _
         "Try removing the special slices and exporting the collection; " _
         !"you'll then be able to import normally.\n\n" & forbidden_error
   notification msg
   slice_editor newcollection, ses.collection_group_number
   EXIT SUB
  ELSE
   'If it's already been imported into the game, only warn
   notification "Hmm, your slice collection includes protected/special slices. This " _
                "shouldn't happen, and may be an engine bug! Please report it.\n\n" _
                & forbidden_error
  END IF
 END IF
 IF ses.draw_root THEN
  remember_draw_root_pos = ses.draw_root->Pos
  DeleteSlice @ses.draw_root
 END IF
 ERASE ses.slicemenu  'All the .handle pointers are invalid
 edslice = newcollection
 ses.draw_root = create_draw_root(ses)
 SetSliceParent edslice, ses.draw_root
END SUB

' Browse for a slice collection to import or edit.
' If edit_separately, then we save the current collection and switch to editing the new one,
' otherwise it's imported overwriting the current one.
SUB slice_editor_import_file(byref ses as SliceEditState, byref edslice as Slice Ptr, edit_separately as bool)
 DIM filename as string = browse(browseRELOAD, trimfilename(ses.collection_file), "*.slice", "browse_import_slices")
 IF filename <> "" THEN
  IF edit_separately THEN
   ' We are no longer editing whatever we were before
   IF slice_editor_save_when_leaving(ses, edslice) = NO THEN EXIT SUB  'User can cancel
   ses.collection_file = filename
   ses.use_index = NO
   ses.editing_existing = NO
  END IF
  slice_editor_load ses, edslice, filename, edit_separately
  ses.slicemenust.need_update = YES
  init_slice_editor_for_collection_group(ses, ses.collection_group_number)
 END IF
END SUB

#IFDEF IS_CUSTOM
'Prompt user whether to import. Called when F3 is pressed.
SUB slice_editor_import_prompt(byref ses as SliceEditState, byref edslice as Slice ptr)
 '--Loading new collections when .editing_existing is unimplemented
 IF ses.editing_existing THEN EXIT SUB
 DIM choice as integer
 DIM choices(...) as string = {"Import, overwriting this collection", "Edit it separately"}
 choice = multichoice("Loading a .slice file. Do you want to import it over the existing collection?", choices(), IIF(ses.collection_group_number = SL_COLLECT_EDITOR, 1, 0))
 IF choice >= 0 THEN
  slice_editor_import_file ses, edslice, (choice = 1)
 END IF
END SUB
#ENDIF

'Prompt user whether to export. Called when F2 is pressed.
SUB slice_editor_export_prompt(byref ses as SliceEditState, byref edslice as Slice ptr)
 DIM filename as string
 IF keyval(scCtrl) > 0 AND LEN(ses.collection_file) THEN
  IF yesno("Save, overwriting " & simplify_path_further(ses.collection_file) & "?", NO, NO) THEN
   filename = ses.collection_file
  END IF
 ELSE
  filename = inputfilename("Export slice collection", ".slice", trimfilename(ses.collection_file), "input_filename_export_slices")
  IF filename <> "" THEN filename &= ".slice"
 END IF
 IF filename <> "" THEN
  SliceSaveToFile edslice, filename
 END IF
END SUB

FUNCTION slice_collection_has_changed(sl as Slice ptr, filename as string) as bool
 IF isfile(filename) = NO THEN RETURN YES

 'Load the original slice tree
 DIM olddoc as DocPtr
 olddoc = LoadDocument(filename, optNoDelay)
 IF olddoc = NULL THEN RETURN YES
 DIM oldtree as Nodeptr = DocumentRoot(olddoc)

 'Create a node in a new RELOAD document, and save the slice tree into it
 DIM newdoc as DocPtr
 newdoc = CreateDocument()
 IF newdoc = NULL THEN FreeDocument olddoc : RETURN YES
 DIM newtree as Nodeptr
 newtree = CreateNode(newdoc, "")
 SetRootNode newdoc, newtree
 SliceSaveToNode sl, newtree, NO
 'SerializeBin filename + ".2", newdoc  'For debug

 DIM changed as bool
 changed = Reload.Ext.CompareNodes(newtree, oldtree) = NO  'Check not equal

 FreeDocument olddoc
 FreeDocument newdoc
 RETURN changed
END FUNCTION

' Called when you leave the editor or switch to a different collection: saves if necessary.
' Returns false if the user cancelled rather than made a decision
FUNCTION slice_editor_save_when_leaving(byref ses as SliceEditState, edslice as Slice Ptr) as bool
 DIM filename as string = slice_editor_filename(ses)
 IF ses.use_index THEN
  ' Autosave on quit, unless the collection is empty
  IF edslice->NumChildren > 0 THEN
   '--save non-empty slice collections
   SliceSaveToFile edslice, filename
  ELSE
   '--erase empty slice collections
   '(Note: since you can edit the root slice, this is technically wrong...)
   safekill filename
  END IF
 ELSEIF LEN(ses.collection_file) > 0 THEN
  'If we're editing a menu's slice collection, which it had modified, we better not save its changes
  '(Export instead if you want to save)
  IF ses.editing_existing ANDALSO ses.existing_matches_file = NO THEN RETURN YES

  IF slice_collection_has_changed(edslice, filename) = NO THEN RETURN YES

  IF edslice->NumChildren > 0 THEN
   'Prevent attempt to quit the program, stop and wait for response first
   DIM quitting as bool = getquitflag()
   setquitflag NO
   DIM dowhat as integer
   dowhat = twochoice(!"Slice collection modified.\nSave before leaving, overwriting " & _
                      simplify_path_further(filename) & "?", "Yes", "No", 0, -1)
   IF dowhat = -1 AND quitting = NO THEN RETURN NO  'cancel
   IF dowhat = 0 THEN  'yes
    SliceSaveToFile edslice, filename
   END IF
   IF quitting THEN setquitflag
  END IF
 END IF
 RETURN YES
END FUNCTION

'Copy a slice to the internal clipboard
SUB slice_editor_copy(byref ses as SliceEditState, byval tocopy as Slice Ptr, byval edslice as Slice Ptr)
 IF ses.clipboard THEN DeleteSlice @ses.clipboard
 DIM sl as Slice Ptr
 IF tocopy THEN
  ses.clipboard = NewSliceOfType(slContainer)
  IF ses.privileged = NO ANDALSO slice_editor_forbidden_search(tocopy, ses.specialcodes()) THEN
   'If we're copying some special slices, so sanitise with copy_special=NO,
   'which blanks out all special lookup codes and wipes .Protected
   sl = CloneSliceTree(tocopy, , NO)  'copy_special=NO
  ELSE
   ' Preserve lookup codes, including negative ones which are allowed in this editor
   sl = CloneSliceTree(tocopy)
  END IF
  SetSliceParent sl, ses.clipboard
 ELSE
  ses.clipboard = CloneSliceTree(edslice)
 END IF
END SUB

'Insert pasted slices before 'putbefore'
SUB slice_editor_paste(byref ses as SliceEditState, byval putbefore as Slice Ptr, byval edslice as Slice Ptr)
 IF ses.clipboard THEN
  DIM child as Slice Ptr
  child = ses.clipboard->LastChild
  WHILE child
   DIM copied as Slice Ptr = CloneSliceTree(child)
   IF putbefore <> 0 AND putbefore <> edslice THEN
    InsertSliceBefore putbefore, copied
   ELSE
    SetSliceParent copied, edslice
   END IF
   putbefore = copied
   child = child->PrevSibling
  WEND
 END IF
END SUB

'Editor for an individual slice
SUB slice_edit_detail (byref ses as SliceEditState, edslice as Slice ptr, sl as Slice Ptr)

 STATIC remember_pt as integer
 DIM usemenu_flag as bool

 IF sl = 0 THEN EXIT SUB

 REDIM menu(0) as string
 REDIM rules(0) as EditRule

 DIM state as MenuState
 WITH state
  .pt = remember_pt
  .need_update = YES
  .autosize = YES
 END WITH
 DIM menuopts as MenuOptions
 WITH menuopts
  .edged = YES
  .highlight = YES
 END WITH

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN
   DIM helpkey as string = rules(state.pt).helpkey
   show_help "sliceedit_" & IIF(LEN(helpkey), helpkey, "detail")
  END IF
  IF keyval(scTab) > 1 THEN
   'Expand/collapse all
   WITH ses
    DIM expand as bool
    expand = .expand_dimensions OR .expand_visible OR .expand_alignment OR _
             .expand_special OR .expand_padding OR .expand_extra OR .expand_sort
    expand XOR= YES
    .expand_dimensions = expand
    .expand_visible = expand
    .expand_alignment = expand
    .expand_special = expand
    .expand_padding = expand
    .expand_extra = expand
    .expand_sort = expand
   END WITH
   state.need_update = YES
  END IF

  IF UpdateScreenSlice() THEN state.need_update = YES

  IF state.need_update THEN
   'Invisible slices won't be updated by DrawSlice
   RefreshSliceTreeScreenPos sl

   slice_edit_detail_refresh ses, state, menu(), menuopts, sl, rules()
   state.need_update = NO
  END IF

  usemenu_flag = usemenu(state)
  IF state.pt = 0 AND enter_space_click(state) THEN EXIT DO
  slice_edit_detail_keys ses, state, sl, rules(), usemenu_flag

  'After slice_edit_detail_keys so that can handle text input
  slice_editor_common_function_keys ses, edslice, state, YES  'F, R, V, F4, F6, F7, F8, Ctrl+F3, Ctrl+F4

  draw_background vpages(dpage), bgChequer
  IF ses.hide_mode <> hideSlices THEN
   DrawSlice ses.draw_root, dpage
  END If
  IF ses.show_ants THEN
   DrawSliceAnts sl, dpage
  END IF
  IF ses.hide_mode <> hideMenu THEN
   menuopts.drawbg = (ses.hide_mode <> hideMenuBG)
   standardmenu menu(), state, 0, 0, dpage, menuopts
  END IF

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 remember_pt = state.pt

END SUB

'Reset a slice's position and alignment
SUB slice_editor_reset_slice(byref ses as SliceEditState, sl as Slice ptr)
 WITH *sl
  'Fill Parent changes position, so reset that. Cover Children doesn't.
  .Fill = NO
  .FillMode = sliceFillFull
  .Pos = XY(0, 0)
  .AlignHoriz = alignLeft
  .AlignVert = alignTop
  .AnchorHoriz = alignLeft
  .AnchorVert = alignTop
  slice_edit_updates ses.curslice, @.Fill
  slice_edit_updates ses.curslice, @.CoverChildren
 END WITH
END SUB

'Shift viewport so this slice is just off-center (to the right)
SUB slice_editor_focus_on_slice(byref ses as SliceEditState, edslice as Slice ptr)
 DIM true_root as Slice ptr = FindRootSlice(edslice)  'In case recursively editing a subtree
 DIM focus_pt as XYPair = get_resolution() \ 2  'Where on the screen to put the focused slice
 focus_pt.x += 70
 DIM focus_on as Slice ptr = IIF(ses.curslice, ses.curslice, edslice)
 RefreshSliceScreenPos focus_on
 true_root->Pos += focus_pt - (focus_on->ScreenPos + focus_on->Size \ 2)
END SUB

'Filling is so yuck we need helpers just to turn it off
SUB disable_horiz_fill (sl as Slice ptr)
 WITH *sl
  'When filling, X is effectively 0, so actually set to 0 when disabling fill,
  'to keep slice at same position
  IF .Fill THEN
   IF .FillMode = sliceFillHoriz THEN .Fill = NO : .X = 0
   IF .FillMode = sliceFillFull THEN .FillMode = sliceFillVert : .X = 0
  END IF
 END WITH
END SUB

SUB disable_vert_fill (sl as Slice ptr)
 WITH *sl
  'Ditto: set Y to 0
  IF .Fill THEN
   IF .FillMode = sliceFillVert THEN .Fill = NO : .Y = 0
   IF .FillMode = sliceFillFull THEN .FillMode = sliceFillHoriz : .Y = 0
  END IF
 END WITH
END SUB

'Called after *dataptr is modified, which is one of the members of sl, in order
'to perform any special resulting updates.
'It's simpler to do updates once, here, if there are multiple places a piece of data is changed.
SUB slice_edit_updates (sl as Slice ptr, dataptr as any ptr)
 WITH *sl
  'Stop filling and covering when trying to edit the size
  IF dataptr = @.Width THEN
   disable_horiz_fill(sl)
   .CoverChildren AND= NOT coverHoriz
  END IF
  IF dataptr = @.Height THEN
   disable_vert_fill(sl)
   .CoverChildren AND= NOT coverVert
  END IF
  'NOTE: Sprite slices can't be resized, unless they Fill Parent.
  'That restriction is actually enforced in LoadSpriteSliceImage rather than in sliceedit;
  'ought to be cleaned up (TODO)

  'Covering and Filling are mutually exclusive
  IF dataptr = @.Fill OR dataptr = @.FillMode THEN
   .CoverChildren AND= SliceLegalCoverModes(sl)
  END IF
  IF dataptr = @.CoverChildren THEN
   IF .CoverChildren AND coverHoriz THEN disable_horiz_fill(sl)
   IF .CoverChildren AND coverVert THEN disable_vert_fill(sl)

   'Restrict .CoverChildren to only the allowed modes.
   'It's ugly to do this here when all other slice data editing restrictions are
   'implemented using "rule groups", but this has to be done AFTER filling is
   'disabled, above, or else you need to manually disable filling before covering.
   .CoverChildren AND= SliceLegalCoverModes(sl)
  END IF

  'After the type changes
  IF dataptr = @.SliceType THEN
   .CoverChildren AND= SliceLegalCoverModes(sl)
  END IF

 END WITH
END SUB

SUB slice_edit_detail_keys (byref ses as SliceEditState, byref state as MenuState, sl as Slice Ptr, rules() as EditRule, usemenu_flag as bool)
 DIM rule as EditRule = rules(state.pt)
 SELECT CASE rule.mode
  CASE erIntgrabber
   DIM n as integer ptr = rule.dataptr
   IF intgrabber(*n, rule.lower, rule.upper, , , , , NO) THEN  'Don't autoclamp
    state.need_update = YES
   END IF
  CASE erEnumgrabber
   ' In 64 bit builds, enums are 64 bit.
   DIM n as ssize_t ptr = rule.dataptr
   IF intgrabber(*n, cast(ssize_t, rule.lower), cast(ssize_t, rule.upper), , , , , NO) THEN  'Don't autoclamp
    state.need_update = YES
   END IF
  CASE erToggle
   DIM n as integer ptr = rule.dataptr
   IF boolgrabber(*n, state) THEN
    state.need_update = YES
   END IF
  CASE erShortStrgrabber
   DIM s as string ptr = rule.dataptr
   state.need_update OR= strgrabber(*s, rule.upper)
  CASE erStrgrabber
   DIM s as string ptr = rule.dataptr
   IF keyval(scAnyEnter) > 1 THEN
    *s = multiline_string_editor(*s, "sliceedit_text_multiline", NO)
    state.need_update = YES
   ELSE
    IF strgrabber(*s, rule.upper) THEN
     state.need_update = YES
    END IF
   END IF
  CASE erPercentgrabber
   DIM n as double ptr = rule.dataptr
   state.need_update OR= percent_grabber(*n, "", 0.01 * rule.lower, 0.01 * rule.upper, 4, YES)
  CASE erSinglePercentgrabber
   DIM n as single ptr = rule.dataptr
   state.need_update OR= percent_grabber(*n, "", 0.01 * rule.lower, 0.01 * rule.upper, 4, YES)
  CASE erLookupgrabber
   DIM n as integer ptr = rule.dataptr
   state.need_update OR= lookup_code_grabber(*n, ses, rule.lower, rule.upper)
 END SELECT

 IF rule.group AND slgrPICKTYPE THEN  'Pick slice type
  DIM switchtype as bool = NO
  DIM slice_type as SliceTypes = sl->SliceType
  DIM slice_type_num as integer = -1
  ' First build the list of types that are compatible with this lookup code
  DIM allowed_types() as SliceTypes
  DIM kindlimit as integer = kindlimitANYTHING  'If the lookup isn't special
  IF sl->Lookup < 0 THEN
   DIM which as integer = find_special_lookup_code(ses.specialcodes(), sl->Lookup)
   IF which > -1 THEN
    kindlimit = ses.specialcodes(which).kindlimit
   ELSE 'If the lookup isn't recognised, don't allow changing it
    kindlimit = IIF(ses.privileged, kindlimitANYTHING, kindlimitNOTHING)
   END IF
  ELSEIF sl->Protect ANDALSO ses.privileged = NO THEN
   kindlimit = kindlimitNOTHING  'Can't change protected slices
  END IF
  ' Use kindlimit to filter editable_slice_types()
  FOR i as integer = 0 TO UBOUND(editable_slice_types)
   DIM edtype as SliceTypes = editable_slice_types(i)
   IF special_code_kindlimit_check(kindlimit, edtype) THEN
    a_append allowed_types(), edtype
   END IF
  NEXT i
  slice_type_num = a_find(allowed_types(), slice_type)
  IF slice_type_num > -1 THEN  'Can't change the type if it's a special type
   IF intgrabber(slice_type_num, 0, UBOUND(allowed_types)) THEN
    slice_type = allowed_types(slice_type_num)
    state.need_update = YES
    switchtype = YES
   END IF
   IF enter_space_click(state) THEN
    IF slice_edit_detail_browse_slicetype(slice_type, allowed_types()) THEN
     state.need_update = YES
     switchtype = YES
    END IF
   END IF
  END IF
  IF switchtype THEN
   ReplaceSliceType sl, NewSliceOfType(slice_type)
   slice_edit_updates sl, @sl->SliceType
  END IF
 END IF
 IF rule.group AND slgrPICKXY THEN
  IF enter_space_click(state) THEN
   slice_editor_xy sl->X, sl->Y, sl, ses.draw_root, ses.show_ants
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrPICKWH THEN
  IF enter_space_click(state) THEN
   slice_editor_xy sl->Width, sl->Height, sl, ses.draw_root, ses.show_ants
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrPICKCOL THEN
  IF enter_space_click(state) THEN
   DIM n as integer ptr = rule.dataptr
   *n = color_browser_256(*n)
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrPICKLOOKUP THEN
  ' Ignore scSpace, which is captured by lookup_code_grabber
  IF enter_space_click(state) AND keyval(scSpace) = 0 THEN
   DIM n as integer ptr = rule.dataptr
   *n = edit_slice_lookup_codes(ses, ses.slicelookup(), *n, sl->SliceType)
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrBROWSESPRITEID THEN
  IF enter_space_click(state) THEN
   DIM dat as SpriteSliceData ptr = sl->SliceData
   DIM spriteb as SpriteOfTypeBrowser
   ChangeSpriteSlice sl, , spriteb.browse(dat->record, , dat->spritetype)
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrUPDATESPRITE THEN
  IF state.need_update THEN
   'state.need_update is cleared at the top of the loop
   SpriteSliceUpdate sl
  END IF
 END IF
 IF rule.group AND slgrBROWSESPRITEASSET THEN
  DIM dat as SpriteSliceData ptr = sl->SliceData
  IF enter_space_click(state) THEN
   ' Browse for an asset. Only paths inside data/ are allowed.
   DIM as string filename
   IF dat->assetfile THEN filename = finddatafile(*dat->assetfile, NO)
   IF LEN(filename) = 0 THEN filename = get_data_dir()
   filename = browse(browseImage, filename, , "browse_import_sprite")
   IF LEN(filename) THEN
    filename = filename_relative_to_datadir(filename)
    IF LEN(filename) THEN  'The file was valid
     SetSpriteToAsset sl, filename, NO
     state.need_update = YES
    END IF
   END IF
  END IF
 END IF
 IF rule.group AND slgrBROWSEBOXBORDER THEN
  IF enter_space_click(state) THEN
   DIM dat as RectangleSliceData ptr = sl->SliceData
   DIM boxborderb as BoxborderSpriteBrowser
   dat->raw_box_border = boxborderb.browse(dat->raw_box_border)
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrUPDATERECTCUSTOMSTYLE THEN
  IF state.need_update THEN
   DIM dat as RectangleSliceData Ptr
   dat = sl->SliceData
   dat->style = -1
   dat->style_loaded = NO
  END IF
 END IF
 IF rule.group AND slgrUPDATERECTSTYLE THEN
  IF state.need_update THEN
   DIM dat as RectangleSliceData Ptr
   dat = sl->SliceData
   dat->style_loaded = NO
  END IF
 END IF
 IF rule.group AND slgrEDITSWITCHINDEX THEN
  IF state.need_update THEN
   DIM dat as SelectSliceData Ptr
   dat = sl->SliceData
   dat->override = -1 'Cancel override when we manually change index
  END IF
 END IF
 IF rule.group AND slgrLAYOUT2NDDIR THEN
  DIM dat as LayoutSliceData ptr = sl->SliceData
  DIM byref secdir as DirNum = dat->secondary_dir
  DIM n as integer = 0
  IF secdir = dirRight OR secdir = dirDown THEN n = 1
  IF intgrabber(n, 0, 1) THEN
   state.need_update = YES
   secdir XOR= 2  'Swap dirUp and dirDown, swap dirLeft and dirRight
  END IF
 END IF

 ' Special actions to take after some piece of data has been edited
 IF state.need_update THEN
  ' Because we bypass ChangeTextSlice (really ought to change that)
  IF sl->SliceType = slText THEN UpdateTextSlice sl

  slice_edit_updates sl, rule.dataptr
 END IF

 ' Update transient editing state
 IF ses.last_lookup_name_edit THEN
  ' Don't stay in name-editing mode unexpectantly
  IF ses.last_lookup_name_edit < TIMER - 2.5 OR usemenu_flag THEN
   ses.editing_lookup_name = NO
   state.need_update = YES
  END IF
 END IF
END SUB

FUNCTION slice_editor_filename(byref ses as SliceEditState) as string
 IF LEN(ses.collection_file) THEN
  ' An external file
  RETURN ses.collection_file
 ELSE
  RETURN workingdir & SLASH & "slicetree_" & ses.collection_group_number & "_" & ses.collection_number & ".reld"
 END IF
END FUNCTION

'Editor to visually edit an x/y position or a width/height
SUB slice_editor_xy (byref x as integer, byref y as integer, byval focussl as Slice Ptr, byval rootsl as Slice Ptr, byref show_ants as bool)
 DIM shift as integer = 0
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF enter_or_space() THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "sliceedit_xy"
  IF keyval(scF7) > 1 THEN show_ants = NOT show_ants
  DIM speed as integer = IIF(keyval(scShift) > 0, 10, 1)
  'The following calls to slice_edit_updates only do something if x/y are focussl->Width/Height.
  'Perfectly harmless otherwise.
  IF keyval(ccUp)    > 0 THEN y -= speed : slice_edit_updates focussl, @y
  IF keyval(ccRight) > 0 THEN x += speed : slice_edit_updates focussl, @x
  IF keyval(ccDown)  > 0 THEN y += speed : slice_edit_updates focussl, @y
  IF keyval(ccLeft)  > 0 THEN x -= speed : slice_edit_updates focussl, @x
  draw_background vpages(dpage), bgChequer
  'Invisible slices won't be updated by DrawSlice
  RefreshSliceTreeScreenPos focussl
  DrawSlice rootsl, dpage
  IF show_ants THEN DrawSliceAnts focussl, dpage
  edgeprint "Arrow keys to edit, SHIFT for speed", 0, pBottom, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

'Add a menu item to edit a piece of data (EditRule) to rules()
SUB sliceed_rule (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as integer ptr, lower as integer=0, upper as integer=0, group as integer = 0)
 DIM index as integer = UBOUND(rules) + 1
 REDIM PRESERVE rules(index) as EditRule
 WITH rules(index)
  .dataptr = dataptr
  .mode = mode
  .lower = lower
  .upper = upper
  .group = group
  .helpkey = helpkey
 END WITH
END SUB

'We have a lot of apparently redundant functions to allow error compile-
'and possibly also run-time error checking. In particular, in 64 bit builds,
'enums are 64 bit, so to catch errors we shouldn't allow passing them to sliceed_rule
'by giving it "dataptr as any ptr" argument.

SUB sliceed_rule_enum (rules() as EditRule, helpkey as string, dataptr as ssize_t ptr, lower as integer=0, upper as integer=0, group as integer = 0)
 sliceed_rule rules(), helpkey, erEnumgrabber, cast(integer ptr, dataptr), lower, upper, group
END SUB

'lower and upper are in percent
SUB sliceed_rule_double (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as double ptr, lower as integer=0, upper as integer=100, group as integer = 0)
 sliceed_rule rules(), helpkey, mode, cast(integer ptr, dataptr), lower, upper, group
END SUB

'lower and upper are in percent
SUB sliceed_rule_single (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as single ptr, lower as integer=0, upper as integer=100, group as integer = 0)
 sliceed_rule rules(), helpkey, mode, cast(integer ptr, dataptr), lower, upper, group
END SUB

' upper is the maximum string length
SUB sliceed_rule_str (rules() as EditRule, helpkey as string, mode as EditRuleMode, dataptr as string ptr, upper as integer=0, group as integer = 0)
 sliceed_rule rules(), helpkey, mode, cast(integer ptr, dataptr), 0, upper, group
END SUB

SUB sliceed_rule_none(rules() as EditRule, helpkey as string, group as integer = 0)
 sliceed_rule rules(), helpkey, erNone, 0, 0, 0, group
END SUB

SUB sliceed_rule_tog(rules() as EditRule, helpkey as string, dataptr as bool ptr, group as integer=0)
 sliceed_rule rules(), helpkey, erToggle, dataptr, -1, 0, group
END SUB

SUB sliceed_header(menu() as string, rules() as EditRule, text as string, dataptr as bool ptr = NULL, helpkey as string = "")
 a_append menu(), fgtag(uilook(eduiHeading), text)
 IF dataptr THEN
  sliceed_rule_tog rules(), helpkey, dataptr
 ELSE
  sliceed_rule_none rules(), helpkey
 END IF
END SUB

SUB sliceed_add_blend_edit_rules(menu() as string, rules() as EditRule, drawopts as DrawOptions ptr)
 WITH *drawopts
  a_append menu(), " Blending: " & iif(.with_blending, "Enabled", "Disabled")
  sliceed_rule_tog rules(), "blending", @(.with_blending)
  IF .with_blending THEN
   a_append menu(), "  Opacity: " & format_percent(.opacity)
   sliceed_rule_single rules(), "opacity", erSinglePercentgrabber, @(.opacity), 0, 100
   a_append menu(), "  Blend mode: " & BlendModeCaptions(.blend_mode)
   sliceed_rule rules(), "blend_mode", erIntgrabber, @(.blend_mode), 0, blendModeLAST
   IF vpages_are_32bit = NO THEN
    sliceed_header menu(), rules(), "  [Global]"
    a_append menu(), "   Algorithm: " & BlendAlgoCaptions(gen(gen8bitBlendAlgo))
    sliceed_rule rules(), "blend_algo", erIntgrabber, @gen(gen8bitBlendAlgo), 0, blendAlgoLAST
   END IF
  END IF
 END WITH
END SUB

SUB slice_edit_detail_refresh (byref ses as SliceEditState, byref state as MenuState, menu() as string, menuopts as MenuOptions, sl as Slice Ptr, rules() as EditRule)
 DIM prev_item as string
 IF state.pt <= UBOUND(menu) THEN prev_item = menu(state.pt)

 REDIM menu(0) as string
 REDIM rules(0) as EditRule
 rules(0).helpkey = "detail"
 menu(0) = "Previous Menu"
 WITH *sl

 a_append menu(), "Slice type: " & SliceTypeName(sl)
 sliceed_rule_none rules(), "slicetype", slgrPICKTYPE  'May not be editable; see slgrPICKTYPE

 DIM temp as string
 IF ses.editing_lookup_name THEN temp = fgtag(uilook(uiText), "_")  'Show text editing cursor
 a_append menu(), "Lookup code: " & slice_lookup_code_caption(.Lookup, ses.slicelookup()) & temp
 DIM minlookup as integer = IIF(ses.privileged, -999999999, 0)
 IF ses.privileged ORELSE lookup_code_forbidden(ses.specialcodes(), .Lookup) = NO THEN
  sliceed_rule rules(), "lookup", erLookupgrabber, @.Lookup, minlookup, INT_MAX, slgrPICKLOOKUP
 ELSE
  '--Not allowed to change lookup code at all
  sliceed_rule_none rules(), "lookup"
 END IF
 #IFDEF IS_GAME
  a_append menu(), "Script handle: " & defaultint(.TableSlot, "None", 0)
  sliceed_rule_none rules(), "scripthandle"
 #ENDIF
 IF .Context THEN
  a_append menu(), "Info: " &  .Context->description()
  sliceed_rule_none rules(), "metadata"
 END IF
 IF ses.privileged THEN
  a_append menu(), "Protected: " & yesorno(.Protect)
  sliceed_rule_tog rules(), "protect", @.Protect
 ELSEIF .Protect THEN
  a_append menu(), "Protected"
  sliceed_rule_none rules(), "protect"
 END IF

 sliceed_header menu(), rules(), "[Dimensions]", @ses.expand_dimensions
 IF ses.expand_dimensions THEN
  IF .Fill = NO ORELSE .FillMode = sliceFillVert THEN
   a_append menu(), " X: " & .X
   sliceed_rule rules(), "pos", erIntgrabber, @.X, -9999, 9999, slgrPICKXY
  ELSE
   'a_append menu(), "X: " & fgtag(uilook(uiDisabledItem), "0 (filling)")
   'sliceed_rule_none rules(), "pos"
  END IF
  IF .Fill = NO ORELSE .FillMode = sliceFillHoriz THEN
   a_append menu(), " Y: " & .Y
   sliceed_rule rules(), "pos", erIntgrabber, @.Y, -9999, 9999, slgrPICKXY
  ELSE
   'a_append menu(), "Y: " & fgtag(uilook(uiDisabledItem), "0 (filling)")
   'sliceed_rule_none rules(), "pos"
  END IF
  DIM minsize as integer = IIF(.SliceType = slLine, -9999, 0)
  a_append menu(), " Width: " & .Width
  sliceed_rule rules(), "size", erIntgrabber, @.Width, minsize, 9999, slgrPICKWH
  a_append menu(), " Height: " & .Height
  sliceed_rule rules(), "size", erIntgrabber, @.Height, minsize, 9999, slgrPICKWH
  IF ses.privileged THEN
   a_append menu(), " Cover Children: " & CoverModeCaptions(.CoverChildren)
   sliceed_rule_enum rules(), "cover", @.CoverChildren, 0, 3
  END IF
  a_append menu(), " Fill Parent: " & yesorno(.Fill)
  sliceed_rule_tog rules(), "fill", @.Fill
  IF .Fill THEN
   a_append menu(), "  Fill Type: " & FillModeCaptions(.FillMode)
   sliceed_rule_enum rules(), "fillmode", @.FillMode, 0, 2
  END IF
 END IF

 SELECT CASE .SliceType
  CASE slSpecial, slContainer
  CASE ELSE
   sliceed_header menu(), rules(), "[" & SliceTypeName(sl) & " settings]", @ses.expand_special
 END SELECT

 IF ses.expand_special THEN

  SELECT CASE .SliceType
   CASE slRectangle
    DIM dat as RectangleSliceData Ptr
    dat = .SliceData
    a_append menu(), " Style: " & defaultint(dat->style, "None (custom)")
    sliceed_rule rules(), "rect_style", erIntgrabber, @(dat->style), -1, 14, slgrUPDATERECTSTYLE
    a_append menu(), "  Background color: " & slice_color_caption(dat->bgcol)
    sliceed_rule rules(), "rect_bg", erIntgrabber, @(dat->bgcol), LowColorCode(), 255, (slgrUPDATERECTCUSTOMSTYLE OR slgrPICKCOL)
    a_append menu(), "  Foreground (line) color: " & slice_color_caption(dat->fgcol)
    sliceed_rule rules(), "rect_fg", erIntgrabber, @(dat->fgcol), LowColorCode(), 255, (slgrUPDATERECTCUSTOMSTYLE OR slgrPICKCOL)
    'TODO: Line and None should be border types, not appear under Box Style
    a_append menu(), "  Border type: " & IIF(dat->use_raw_box_border, "Spriteset", "Box Style/Line/None")
    sliceed_rule_tog rules(), "rect_use_raw_box_border", @(dat->use_raw_box_border), slgrUPDATERECTCUSTOMSTYLE
    IF dat->use_raw_box_border THEN
     a_append menu(), "   Raw Spriteset: " & dat->raw_box_border
     sliceed_rule rules(), "rect_raw_box_border", erIntgrabber, @(dat->raw_box_border), 0, gen(genMaxBoxBorder), slgrBROWSEBOXBORDER
    ELSE
     a_append menu(), "   Border Style: " & caption_or_int(BorderCaptions(), dat->border)
     sliceed_rule_enum rules(), "rect_border", @(dat->border), -2, 14, slgrUPDATERECTCUSTOMSTYLE
    END IF
    a_append menu(), " Translucency: " & TransCaptions(dat->translucent)
    sliceed_rule_enum rules(), "rect_trans", @(dat->translucent), 0, transLAST
    IF dat->translucent = transFuzzy THEN
     a_append menu(), "  Fuzziness: " & dat->fuzzfactor & "%"
     sliceed_rule rules(), "rect_fuzzfact", erIntgrabber, @(dat->fuzzfactor), 0, 99
     a_append menu(), "  Fuzzy zoom: " & dat->fuzz_zoom
     sliceed_rule rules(), "rect_fuzzzoom", erIntgrabber, @(dat->fuzz_zoom), 1, 10000  'No need for upper limit
     a_append menu(), "  Stationary pattern: " & yesorno(dat->fuzz_stationary)
     sliceed_rule_tog rules(), "rect_fuzz_stationary", @(dat->fuzz_stationary)
    ELSEIF dat->translucent = transBlend THEN
     a_append menu(), "  Opacity: " & dat->fuzzfactor & "%"
     sliceed_rule rules(), "rect_transfact", erIntgrabber, @(dat->fuzzfactor), 0, 99
    END IF

   CASE slMap
    DIM dat as MapSliceData ptr = .SliceData
    sliceed_add_blend_edit_rules menu(), rules(), @dat->drawopts

   CASE slLine
    DIM dat as LineSliceData ptr = .SliceData
    a_append menu(), "  Color: " & slice_color_caption(dat->col)
    sliceed_rule rules(), "line_col", erIntgrabber, @dat->col, LowColorCode(), 255, slgrPICKCOL
    ' a_append menu(), "  Flipped: " & yesorno(dat->flipped)
    ' sliceed_rule_tog rules(), "line_flipped", @dat->flipped

   CASE slText
    DIM dat as TextSliceData Ptr = .SliceData
    a_append menu(), " Text: " & dat->s
    sliceed_rule_str rules(), "text_text", erStrgrabber, @(dat->s), 128000  'Arbitrary limit
    a_append menu(), " Color: " & slice_color_caption(dat->col, "Default")
    sliceed_rule rules(), "text_color", erIntgrabber, @(dat->col), LowColorCode(), 255, slgrPICKCOL
    IF dat->outline = NO THEN
     a_append menu(), " Background Color: " & slice_color_caption(dat->bgcol, "Transparent")
     sliceed_rule rules(), "text_bg", erIntgrabber, @(dat->bgcol), LowColorCode(), 255, slgrPICKCOL
    END IF
    a_append menu(), " Outline: " & yesorno(dat->outline)
    sliceed_rule_tog rules(), "text_outline", @(dat->outline)
    a_append menu(), " Wrap: " & yesorno(dat->wrap)
    sliceed_rule_tog rules(), "text_wrap", @(dat->wrap)

   CASE slSprite
    DIM dat as SpriteSliceData Ptr = .SliceData
    DIM byref sizeinfo as SpriteSize = sprite_sizes(dat->spritetype)
    a_append menu(), " Type: " & sizeinfo.name
    DIM mintype as SpriteType = IIF(ses.collection_group_number = SL_COLLECT_EDITOR, sprTypeFrame, 0)
    sliceed_rule_enum rules(), "sprite_type", @(dat->spritetype), mintype, sprTypeLastPickable, slgrUPDATESPRITE
    IF dat->spritetype = sprTypeFrame THEN
     IF dat->assetfile = NULL THEN
      a_append menu(), " Raw Frame: " & frame_describe(dat->img.sprite)
      sliceed_rule_none rules(), ""
     ELSE
      a_append menu(), " Asset file: " & *dat->assetfile
      sliceed_rule_str rules(), "sprite_asset", erShortStrgrabber, dat->assetfile, 1024, (slgrUPDATESPRITE OR slgrBROWSESPRITEASSET)
     END IF
     IF ses.privileged THEN
      a_append menu(), "  Load as 32bit Frame: " & yesorno(dat->load_asset_as_32bit)
      sliceed_rule_tog rules(), "sprite_32bit_asset", @(dat->load_asset_as_32bit), slgrUPDATESPRITE
     END IF
    ELSE
     a_append menu(), " Spriteset: " & dat->record
     sliceed_rule rules(), "sprite_rec", erIntgrabber, @(dat->record), 0, sizeinfo.lastrec, (slgrUPDATESPRITE OR slgrBROWSESPRITEID)
     IF dat->paletted THEN
      a_append menu(), " Palette: " & defaultint(dat->pal)
      sliceed_rule rules(), "sprite_pal", erIntgrabber, @(dat->pal), -1, gen(genMaxPal), slgrUPDATESPRITE
     END IF
     DIM nframes as integer = SpriteSliceNumFrames(sl)
     IF nframes > 1 THEN
      a_append menu(), " Frame: " & dat->frame
      sliceed_rule rules(), "sprite_frame", erIntgrabber, @(dat->frame), 0, nframes - 1
     END IF
    END IF
    a_append menu(), " Transparent: " & yesorno(dat->trans)
    sliceed_rule_tog rules(), "sprite_trans", @(dat->trans), slgrUPDATESPRITE

    sliceed_add_blend_edit_rules menu(), rules(), @dat->drawopts

    IF ses.privileged THEN
     'None of these actually need slgrUPDATESPRITE, but it's the right thing to do.
     a_append menu(), " Rotation: " & dat->rotate & " degrees"
     sliceed_rule rules(), "sprite_rotate", erIntGrabber, @(dat->rotate), 0, 359, slgrUPDATESPRITE
     a_append menu(), " Zoom: " & format_percent(dat->zoom)
     sliceed_rule_single rules(), "sprite_zoom", erSinglePercentgrabber, @(dat->zoom), -2000, 2000, slgrUPDATESPRITE

     IF dat->rotate ORELSE dat->zoom <> 1. THEN
      STATIC SmoothCapts(2) as zstring ptr = {@"None", @"Smooth", @"Smoother (scale_surface)"}
      DIM msg as string = safe_captionz(SmoothCapts(), dat->rz_smooth)
      IF dat->rz_smooth ANDALSO vpages_are_32bit = NO THEN msg &= " (ignored: Ctrl-3 to switch to 32bit)"
      a_append menu(), "  Smoothing: " & msg
      sliceed_rule rules(), "sprite_smooth_rotozoom", erIntGrabber, @(dat->rz_smooth), 0, 2, slgrUPDATESPRITE
     END IF
    END IF

    a_append menu(), " Flip horiz.: " & yesorno(dat->flipHoriz)
    sliceed_rule_tog rules(), "sprite_flip", @(dat->flipHoriz),   'slgrUPDATESPRITE
    a_append menu(), " Flip vert.: " & yesorno(dat->flipVert)
    sliceed_rule_tog rules(), "sprite_flip", @(dat->flipVert),   'slgrUPDATESPRITE
    a_append menu(), " Dissolving: " & yesorno(dat->dissolving)
    sliceed_rule_tog rules(), "sprite_dissolve", @(dat->dissolving)
    IF dat->dissolving THEN
     a_append menu(), "  Type: " & dissolve_type_caption(dat->d_type)
     sliceed_rule rules(), "sprite_d_type", erIntGrabber, @(dat->d_type), 0, dissolveTypeMax
     a_append menu(), "  Over Num. ticks: " & defaultint(dat->d_time, "Default (W+H)/10=" & (.Width + .Height) / 10)
     sliceed_rule rules(), "sprite_d_time", erIntGrabber, @(dat->d_time), -1, 999999
     a_append menu(), "  Current tick: " & dat->d_tick
     sliceed_rule rules(), "sprite_d_tick", erIntGrabber, @(dat->d_tick), 0, 999999
     a_append menu(), "  Backwards: " & yesorno(dat->d_back)
     sliceed_rule_tog rules(), "sprite_d_back", @(dat->d_back)
     'FIXME: dissolve is advanced in DrawSpriteSlice, which is wrong, causing the slice to dissolve
     'in the slice editor, making this setting useless.
     'TODO: need to set d_time to 0 to reset the animation if it's already finished, when this is changed to YES
     'a_append menu(), "  Animate: " & yesorno(dat->d_auto)
     'sliceed_rule_tog rules(), "sprite_d_auto", @(dat->d_auto)
    END IF
    IF ses.privileged THEN
     a_append menu(), " Scaled: " & yesorno(dat->scaled)
     sliceed_rule_tog rules(), "sprite_scaled", @(dat->scaled), slgrUPDATESPRITE
    END IF

   CASE slGrid
    DIM dat as GridSliceData Ptr
    dat = .SliceData
    a_append menu(), " Rows: " & dat->rows
    sliceed_rule rules(), "grid_rows", erIntgrabber, @(dat->rows), 0, 99 'FIXME: upper limit of 99 is totally arbitrary
    a_append menu(), " Columns: " & dat->cols
    sliceed_rule rules(), "grid_cols", erIntgrabber, @(dat->cols), 0, 99 'FIXME: upper limit of 99 is totally arbitrary
    a_append menu(), " Show Grid: " & yesorno(dat->show)
    sliceed_rule_tog rules(), "grid_show", @(dat->show)

   CASE slEllipse
    DIM dat as EllipseSliceData Ptr
    dat = .SliceData
    a_append menu(), " Border Color: " & slice_color_caption(dat->bordercol, "Transparent")
    sliceed_rule rules(), "bordercol", erIntgrabber, @(dat->bordercol), LowColorCode(), 255, slgrPICKCOL
    a_append menu(), " Fill Color: " & slice_color_caption(dat->fillcol, "Transparent")
    sliceed_rule rules(), "fillcol", erIntgrabber, @(dat->fillcol), LowColorCode(), 255, slgrPICKCOL

   CASE slScroll
    DIM dat as ScrollSliceData Ptr
    dat = .SliceData
    a_append menu(), " Style: " & dat->style
    sliceed_rule rules(), "scroll_style", erIntgrabber, @(dat->style), 0, 14
    a_append menu(), " Check Depth: " & zero_default(dat->check_depth, "No limit")
    sliceed_rule rules(), "scroll_check_depth", erIntgrabber, @(dat->check_depth), 0, 99 'FIXME: upper limit of 99 is totally arbitrary

   CASE slSelect
    DIM dat as SelectSliceData Ptr
    dat = .SliceData
    a_append menu(), " Selected Child: " & dat->index
    sliceed_rule rules(), "select_index", erIntgrabber, @(dat->index), 0, 9999999, slgrEDITSWITCHINDEX 'FIXME: this is an arbitrary upper limit

   CASE slPanel
    DIM dat as PanelSliceData Ptr
    dat = .SliceData
    a_append menu(), " Orientation: " & IIF(dat->vertical, "Vertical", "Horizontal")
    sliceed_rule_tog rules(), "panel_vertical", @(dat->vertical)
    a_append menu(), " Primary Child Is: " & dat->primary
    sliceed_rule rules(), "panel_primary", erIntgrabber, @(dat->primary), 0, 1
    a_append menu(), "  " & IIF(dat->vertical, "Height", "Width") & ": " & format_percent(dat->percent) & " of panel"
    sliceed_rule_double rules(), "panel_percent", erPercentgrabber, @(dat->percent)
    a_append menu(), "  ...plus: " & dat->pixels & " pixels"
    sliceed_rule rules(), "panel_pixels", erIntgrabber, @(dat->pixels), 0, 9999 'FIXME: upper limit of 9999 is totally arbitrary
    a_append menu(), " Padding Between Children: " & dat->padding
    sliceed_rule rules(), "panel_padding", erIntgrabber, @(dat->padding), 0, 9999 'FIXME: upper limit of 9999 is totally arbitrary

   CASE slLayout
    DIM dat as LayoutSliceData Ptr
    dat = .SliceData
    a_append menu(), " Row grow direction: " & DirectionCaptions(dat->primary_dir)
    sliceed_rule rules(), "layout_primary_dir", erIntgrabber, @dat->primary_dir, 0, 3
    a_append menu(), " Row-stacking direction: " & DirectionCaptions(dat->secondary_dir)
    sliceed_rule_none rules(), "layout_secondary_dir", slgrLAYOUT2NDDIR
    a_append menu(), " Justified: " & yesorno(dat->justified)
    sliceed_rule_tog rules(), "layout_justified", @dat->justified
    IF dat->justified THEN
     a_append menu(), " Justify last row: " & yesorno(dat->last_row_justified)
     sliceed_rule_tog rules(), "layout_last_row_justified", @dat->last_row_justified
    END IF
    a_append menu(), " Row alignment: " & dir_align_caption(dat->primary_dir, dat->row_alignment)
    sliceed_rule_enum rules(), "layout_row_alignment", @dat->row_alignment, 0, 2
    a_append menu(), " Within-row alignment: " & dir_align_caption(dat->secondary_dir, dat->cell_alignment)
    sliceed_rule_enum rules(), "layout_cell_alignment", @dat->cell_alignment, 0, 2
    IF dat->justified THEN
     a_append menu(), " Minimum within-row padding: " & dat->primary_padding
    ELSE
     a_append menu(), " Within-row padding: " & dat->primary_padding
    END IF
    sliceed_rule rules(), "layout_primary_padding", erIntgrabber, @dat->primary_padding, -9999, 9999
    a_append menu(), " Between-row padding: " & dat->secondary_padding
    sliceed_rule rules(), "layout_secondary_padding", erIntgrabber, @dat->secondary_padding, -9999, 9999
    a_append menu(), " Min row thickness: " & dat->min_row_breadth
    sliceed_rule rules(), "layout_min_row_breadth", erIntgrabber, @dat->min_row_breadth, 0, 9999
    a_append menu(), " Skip hidden: " & yesorno(dat->skip_hidden)
    sliceed_rule_tog rules(), "layout_skip_hidden", @dat->skip_hidden
  END SELECT

 END IF  'expand_special

 sliceed_header menu(), rules(), "[Visibility]", @ses.expand_visible
 IF ses.expand_visible THEN
  a_append menu(), " Visible: " & yesorno(.Visible)
  sliceed_rule_tog rules(), "vis", @.Visible
  a_append menu(), " Clip Children: " & yesorno(.Clip)
  sliceed_rule_tog rules(), "clip", @.Clip
 END IF

 IF .Fill = NO ORELSE .FillMode <> sliceFillFull THEN
  sliceed_header menu(), rules(), "[Alignment]", @ses.expand_alignment
 END IF
 IF ses.expand_alignment THEN
  IF .Fill = NO ORELSE .FillMode = sliceFillVert THEN
   a_append menu(), " Align horiz. to: " & HorizCaptions(.AlignHoriz)
   sliceed_rule_enum rules(), "align", @.AlignHoriz, 0, 2
  END IF
  IF .Fill = NO ORELSE .FillMode = sliceFillHoriz THEN
   a_append menu(), " Align vert.  to: " & VertCaptions(.AlignVert)
   sliceed_rule_enum rules(), "align", @.AlignVert, 0, 2
  END IF
  IF .Fill = NO ORELSE .FillMode = sliceFillVert THEN
   a_append menu(), " Anchor horiz. at: " & HorizCaptions(.AnchorHoriz)
   sliceed_rule_enum rules(), "anchor", @.AnchorHoriz, 0, 2
  END IF
  IF .Fill = NO ORELSE .FillMode = sliceFillHoriz THEN
   a_append menu(), " Anchor vert.  at: " & VertCaptions(.AnchorVert)
   sliceed_rule_enum rules(), "anchor", @.AnchorVert, 0, 2
  END IF
  IF ses.privileged THEN
   IF .Fill = NO ORELSE .FillMode = sliceFillVert THEN
    a_append menu(), " Clamp horiz.: " & clamp_caption(.ClampHoriz, NO)
    sliceed_rule_enum rules(), "clamp", @.ClampHoriz, 0, 2
   END IF
   IF .Fill = NO ORELSE .FillMode = sliceFillHoriz THEN
    a_append menu(), " Clamp vert.: " & clamp_caption(.ClampVert, YES)
    sliceed_rule_enum rules(), "clamp", @.ClampVert, 0, 2
   END IF
  END IF
 END IF

 sliceed_header menu(), rules(), "[Padding]", @ses.expand_padding
 IF ses.expand_padding THEN
  a_append menu(), " Top: " & .PaddingTop
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingTop, -9999, 9999
  a_append menu(), " Right: " & .PaddingRight
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingRight, -9999, 9999
  a_append menu(), " Bottom: " & .PaddingBottom
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingBottom, -9999, 9999
  a_append menu(), " Left: " & .PaddingLeft
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingLeft, -9999, 9999
 END IF

 sliceed_header menu(), rules(), "[Extra Data]", @ses.expand_extra
 IF ses.expand_extra THEN
  FOR i as integer = 0 TO 2
   a_append menu(), " extra" & i & ": " & .Extra(i)
   sliceed_rule rules(), "extra", erIntgrabber, @.Extra(i), -2147483648, 2147483647
  NEXT
 END IF

 sliceed_header menu(), rules(), "[Sorting]", @ses.expand_sort
 IF ses.expand_sort THEN
  sliceed_rule_enum rules(), "autosort", @.AutoSort, 0, 5
  a_append menu(), " Auto-sort children: " & AutoSortCaptions(.AutoSort)
  sliceed_rule rules(), "sortorder", erIntgrabber, @.Sorter, INT_MIN, INT_MAX
  DIM sortNA as string
  IF .Parent = NULL ORELSE .Parent->AutoSort <> slAutoSortCustom THEN sortNA = " (N/A)"
  a_append menu(), " Custom sort order" & sortNA & ": " & .Sorter
 END IF

 END WITH

 init_menu_state state, menu(), menuopts

 'Try to find the previously selected setting back, since its index might have changed
 prev_item = LEFT(prev_item, INSTR(prev_item, ":"))
 IF LEN(prev_item) THEN
  FOR idx as integer = 0 TO UBOUND(menu)
   IF LEFT(menu(idx), LEN(prev_item)) = prev_item THEN state.pt = idx
  NEXT idx
 END IF
END SUB

'Pick a slice type in allowed_types(), return YES if didn't cancel
FUNCTION slice_edit_detail_browse_slicetype(byref slice_type as SliceTypes, allowed_types() as SliceTypes) as bool
 IF UBOUND(allowed_types) < 0 THEN RETURN NO
 DIM as integer default, choice
 DIM menu(UBOUND(allowed_types)) as string
 FOR i as integer = 0 TO UBOUND(menu)
  menu(i) = SliceTypeName(allowed_types(i))
  IF allowed_types(i) = slice_type THEN default = i
 NEXT i
 choice = multichoice("What type of slice?", menu(), default, -1, "sliceedit_browse_slicetype")
 IF choice = -1 THEN RETURN NO
 slice_type = allowed_types(choice)
 RETURN YES
END FUNCTION

'Returns a description of a slice, used on the top-level list of slices
FUNCTION slice_caption (byref ses as SliceEditState, edslice as Slice ptr, sl as Slice ptr) as string
 DIM s as string
 WITH *sl
  s = SliceTypeName(sl) & " "
  IF ses.show_positions THEN s &= (.ScreenPos - ses.draw_root->ScreenPos)
  IF ses.show_sizes THEN s &= "(" & .Size.wh & ")"
  IF sl = edslice AND .Lookup <> SL_ROOT THEN
   s &= " [root]"
  END IF
  s = RTRIM(s) & "${K" & uilook(uiText) & "} "
  IF sl->Context THEN
   'Hide the Context of the root slice of a collection because it duplicates collection name, ID
   IF sl <> edslice ORELSE (*sl->Context IS SliceCollectionContext) = NO THEN
    s &= sl->Context->description()
   END IF
  END IF
  s &= SliceLookupCodeName(.Lookup, ses.slicelookup())  'returns "Lookup" & .Lookup if not recognied
 END WITH
 RETURN RTRIM(s)
END FUNCTION

'Update slice states and the menu listing the slices
'NOTE: cursor_seek may be an invalid pointer, e.g. after switching to another slice collection!
SUB slice_editor_refresh (byref ses as SliceEditState, edslice as Slice Ptr, byref cursor_seek as Slice Ptr)
 'DIM timing as double = TIMER
 ERASE ses.slicemenu

 'Refresh positions of all slices
 RefreshSliceTreeScreenPos ses.draw_root

 slice_editor_refresh_append ses, mnidExitMenu, "Exit Menu"

 IF ses.use_index THEN
  slice_editor_refresh_append ses, mnidCollectionID, CHR(27) & " Slice Collection " & ses.collection_number & " " & CHR(26)
 ELSEIF LEN(ses.collection_file) THEN
  DIM msg as string = "Editing "
  IF ses.editing_existing ANDALSO ses.existing_matches_file = NO THEN msg &= "instance of "
  msg &= simplify_path_further(ses.collection_file)
  slice_editor_refresh_append ses, mnidEditingFile, msg
 END IF

 VAR context = collection_context(edslice)
 'Don't show the collection name when editing a subtree
 IF context ANDALSO context = edslice->Context THEN
  slice_editor_refresh_append ses, mnidCollectionName, "Name: " & context->name
 END IF

 slice_editor_refresh_append ses, mnidSettingsMenu, "Settings/tools (F8)..."

 'Show the root (if ses.show_root)
 DIM hidden_slice as Slice Ptr = edslice
 IF ses.show_root THEN hidden_slice = NULL
 slice_editor_refresh_recurse ses, 0, edslice, edslice, hidden_slice
 ses.slicemenust.last = UBOUND(ses.slicemenu)

 IF cursor_seek <> 0 THEN
  FOR i as integer = 0 TO ses.slicemenust.last
   IF ses.slicemenu(i).handle = cursor_seek THEN
    ses.slicemenust.pt = i
    cursor_seek = 0
    EXIT FOR
   END IF
  NEXT i
 END IF

 correct_menu_state ses.slicemenust

 'timing = TIMER - timing
 'debuginfo "refresh in " & cint(timing * 1e6) & "us, slices: " & UBOUND(ses.slicemenu) + 1
END SUB

SUB slice_editor_refresh_delete (byref index as integer, menu() as SliceEditMenuItem)
 DeleteSlice @(menu(index).handle)
 FOR i as integer = index + 1 TO UBOUND(menu)
  SWAP menu(i), menu(i - 1)
 NEXT i
 REDIM PRESERVE menu(UBOUND(menu) - 1)
END SUB

SUB slice_editor_refresh_append (byref ses as SliceEditState, id as SliceMenuItemID, caption as string, sl as Slice Ptr=0)
 DIM index as integer = UBOUND(ses.slicemenu) + 1
 REDIM PRESERVE ses.slicemenu(index) as SliceEditMenuItem
 WITH ses.slicemenu(index)
  .id = id
  .s = caption
  .handle = sl
 END WITH
END SUB

SUB slice_editor_refresh_recurse (ses as SliceEditState, byref indent as integer, edslice as Slice Ptr, sl as Slice Ptr, hidden_slice as Slice Ptr)
 WITH *sl
  DIM caption as string
  caption = STRING(indent, " ")
  IF sl->EditorHideChildren ANDALSO sl->NumChildren THEN
   caption &= "${K" & uilook(uiText) & "}+[" & sl->NumChildren & "]${K-1}"
  END IF
  caption &= slice_caption(ses, edslice, sl)
  IF sl <> hidden_slice THEN
   slice_editor_refresh_append ses, mnidSlice, caption, sl
   indent += 1
  END IF
  IF NOT sl->EditorHideChildren THEN
   'Now append the children
   DIM ch as slice ptr = .FirstChild
   DO WHILE ch <> 0
    slice_editor_refresh_recurse ses, indent, edslice, ch, hidden_slice
    ch = ch->NextSibling
   LOOP
  END IF
  IF sl <> hidden_slice THEN
   indent -= 1
  END IF
 END WITH
END SUB

SUB SliceAdoptSister (byval sl as Slice Ptr)
 DIM newparent as Slice Ptr = sl->PrevSibling
 IF newparent = 0 THEN EXIT SUB ' Eldest sibling can't be adopted
 '--Adopt self to elder sister's family
 SetSliceParent sl, newparent
 AdjustSlicePosToNewParent sl, newparent
END SUB

SUB SliceAdoptNiece (byval sl as Slice Ptr)
 DIM oldparent as Slice Ptr = sl->Parent
 IF oldparent = 0 THEN EXIT SUB ' No parent
 DIM newparent as Slice Ptr = sl->Parent->Parent
 IF newparent = 0 THEN EXIT SUB ' No grandparent
 'Adopt self to parent's family
 InsertSliceAfter oldparent, sl
 AdjustSlicePosToNewParent sl, newparent
END SUB

SUB AdjustSlicePosToNewParent (byval sl as Slice Ptr, byval newparent as Slice Ptr)
 '--Re-adjust ScreenX/ScreenY position for new parent
 IF newparent->SliceType = slGrid OR newparent->SliceType = slPanel OR newparent->SliceType = slLayout THEN
  '--except if the new parent is a grid/panel/layout, which have customised screenpos calc.
  '--Then it would be silly to preserve Screen pos, and it can't actually be done anyway.
  sl->Pos = XY(0,0)
  EXIT SUB
 END IF
 DIM oldpos as XYPair = sl->ScreenPos
 RefreshSliceScreenPos sl
 DIM newpos as XYPair = sl->ScreenPos
 sl->Pos += oldpos - newpos
END SUB

SUB DrawSliceAnts (byval sl as Slice Ptr, byval dpage as integer)
 IF sl = 0 THEN EXIT SUB
 IF sl->Width = 0 OR sl->Height = 0 THEN
  ' A 1x1 flashing pixel is hard to spot
  drawants vpages(dpage), sl->ScreenX - 1, sl->ScreenY - 1, 3, 3
 END IF
 drawants vpages(dpage), sl->ScreenX, sl->ScreenY, sl->Width, sl->Height

 '--Draw gridlines if this is a grid
 IF sl->SliceType = slGrid THEN
  DIM dat as GridSliceData Ptr = sl->SliceData
  IF dat THEN
   DIM w as integer = sl->Width \ large(1, dat->cols)
   DIM h as integer = sl->Height \ large(1, dat->rows)
   '--draw verticals
   FOR idx as integer = 1 TO dat->cols - 1
    drawants vpages(dpage), sl->ScreenX + idx * w, sl->ScreenY, 1, large(ABS(sl->Height), 3)
   NEXT idx
   '--draw horizontals
   FOR idx as integer = 1 TO dat->rows - 1
    drawants vpages(dpage), sl->ScreenX, sl->ScreenY + idx * h, large(ABS(sl->Width), 3), 1
   NEXT idx
  END IF
 END IF

 '--For panels, draw the outlines of the two panel areas
 '--(This is a bit different from outlines for all other slices, because it takes padding,
 '--etc. into account. Arguably Grids could do the same)
 IF sl->SliceType = slPanel THEN
  DIM dat as PanelSliceData Ptr = sl->SliceData
  IF dat THEN
   FOR childindex as integer = 0 TO 1
    DIM support as RectType
    CalcPanelSupport support, sl, childindex
    drawants vpages(dpage), support.x, support.y, support.wide, support.high
   NEXT
  END IF
 END IF
END SUB

FUNCTION slice_lookup_code_caption(byval code as integer, slicelookup() as string) as string
 DIM s as string
 IF code = 0 THEN RETURN "None"
 IF code < 0 THEN
  '--negative codes are hard-coded slice code
  s = "[sl:" & SliceLookupCodeName(code) & "]"
 ELSE
  s = STR(code)
  IF code <= UBOUND(slicelookup) ANDALSO LEN(TRIM(slicelookup(code))) THEN
   s &= " sli:" & slicelookup(code)
  ELSE
   s &= " (Unnamed)"
  END IF
 END IF
 RETURN s
END FUNCTION

'Returns whether this SpecialLookupCode.kindlimit allows a lookup code to be assigned to this slice type
FUNCTION special_code_kindlimit_check(byval kindlimit as integer, byval slicekind as SliceTypes) as bool
 SELECT CASE kindlimit
  CASE kindlimitNOTHING:
  CASE kindlimitANYTHING:
   RETURN YES
  CASE kindlimitGRID:
   IF slicekind = slGrid THEN RETURN YES
  CASE kindlimitPOSITIONING:
   IF slicekind = slGrid ORELSE slicekind = slLayout THEN RETURN YES
  CASE kindlimitSELECT:
   IF slicekind = slSelect THEN RETURN YES
  CASE kindlimitSPRITE:
   IF slicekind = slSprite THEN RETURN YES
  CASE kindlimitPLANKSELECTABLE:
   IF slicekind = slText ORELSE slicekind = slRectangle ORELSE slicekind = slSelect THEN RETURN YES
  CASE kindlimitTEXT:
   IF slicekind = slText THEN RETURN YES
  CASE ELSE
   debug "Unknown slice lookup code kindlimit constant " & kindlimit
 END SELECT
 RETURN NO
END FUNCTION

' Delete blank lookup codes from the end of the list, leaving one blank name
SUB shrink_lookup_list(slicelookup() as string)
 DIM last as integer = 0
 FOR i as integer = UBOUND(slicelookup) TO 0 STEP -1
  IF TRIM(slicelookup(i)) <> "" THEN
   last = i
   EXIT FOR
  END IF
 NEXT i
 last += 1  ' Leave (or add) one blank name
 IF UBOUND(slicelookup) <> last THEN
  REDIM PRESERVE slicelookup(last) as string
 END IF
END SUB

' Allows typing in either a lookup code number, or naming the selected lookup code
' You can only rename lookup codes already existing, but the final entry in
' ses.slicelookup() should normally be blank.
' Returns true if the code was modified.
FUNCTION lookup_code_grabber(byref code as integer, byref ses as SliceEditState, lowerlimit as integer, upperlimit as integer) as bool
 ' To determine whether Backspace and numerals edit the name or the ID code,
 ' we use ses.editing_lookup_name
 IF (ses.editing_lookup_name = NO OR keyval(ccLeft) > 0 OR keyval(ccRight)) _
    ANDALSO intgrabber(code, lowerlimit, upperlimit, , , , , NO) THEN  'autoclamp=NO
  ' Another kludge: Don't wrap around to INT_MAX!
  IF code = upperlimit AND keyval(ccLeft) > 0 THEN code = UBOUND(ses.slicelookup)
  ses.editing_lookup_name = NO
  RETURN YES
 ELSEIF code > 0 AND code <= UBOUND(ses.slicelookup) THEN
  ' Don't allow naming code 0.
  IF strgrabber(ses.slicelookup(code), 40) THEN
   ses.slicelookup(code) = sanitize_script_identifier(ses.slicelookup(code))
   shrink_lookup_list ses.slicelookup()
   IF can_write_to_workingdir THEN  'not live previewing
    save_string_list ses.slicelookup(), workingdir & SLASH & "slicelookup.txt"
   END IF
   ses.editing_lookup_name = YES
   ses.last_lookup_name_edit = TIMER
   RETURN YES
  END IF
 END IF
END FUNCTION

FUNCTION edit_slice_lookup_codes(byref ses as SliceEditState, slicelookup() as string, byval start_at_code as integer, byval slicekind as SliceTypes) as integer

 DIM result as integer
 result = start_at_code

 DIM menu as SimpleMenuItem vector
 v_new menu, 0
 append_simplemenu_item menu, "Previous Menu...", , , -1
 append_simplemenu_item menu, "None", , , 0

 DIM special_header as bool = NO
 FOR i as integer = 0 TO UBOUND(ses.specialcodes)
  WITH ses.specialcodes(i)
   IF .code <> 0 THEN
    IF special_code_kindlimit_check(.kindlimit, slicekind) THEN
     IF NOT special_header THEN
      append_simplemenu_item menu, "Special Lookup Codes", YES, uiLook(uiText)
      special_header = YES
     END IF
     append_simplemenu_item menu, .caption, , , .code
    END IF
   END IF
  END WITH
 NEXT i

 IF ses.collection_group_number = SL_COLLECT_EDITOR THEN
  append_simplemenu_item menu, "All Special Lookup Codes", YES, uiLook(uiText)

'--the following is updated from slices.bi using the misc/sl_lookup.py script
'<SLICE LOOKUP NAMES>
  append_simplemenu_item menu, "editor_splash_menu", , , -100
  append_simplemenu_item menu, "editor_thingbrowser_thinglist", , , -200
  append_simplemenu_item menu, "editor_thingbrowser_plank_sprite", , , -201
  append_simplemenu_item menu, "editor_thingbrowser_back_holder", , , -202
  append_simplemenu_item menu, "editor_thingbrowser_mode_indicator", , , -203
  append_simplemenu_item menu, "editor_thingbrowser_new_holder", , , -204
  append_simplemenu_item menu, "editor_thingbrowser_noscroll_area", , , -205
  append_simplemenu_item menu, "editor_thingbrowser_filter_holder", , , -206
  append_simplemenu_item menu, "editor_thingbrowser_type_query", , , -207
  append_simplemenu_item menu, "editor_thingbrowser_filter_text", , , -208
  append_simplemenu_item menu, "editor_prompt_for_string_text", , , -300
  append_simplemenu_item menu, "editor_prompt_for_string_caption", , , -301
  append_simplemenu_item menu, "editor_ssed_list", , , -400
  append_simplemenu_item menu, "editor_ssed_set_templ", , , -401
  append_simplemenu_item menu, "editor_ssed_frame_holder", , , -402
  append_simplemenu_item menu, "editor_ssed_frame_templ", , , -403
  append_simplemenu_item menu, "editor_ssed_frame_sprite", , , -404
  append_simplemenu_item menu, "editor_ssed_info_text", , , -405
  append_simplemenu_item menu, "editor_ssed_palette_grid", , , -406
  append_simplemenu_item menu, "editor_ssed_palette_text", , , -407
  append_simplemenu_item menu, "editor_ssed_set_info", , , -408
  append_simplemenu_item menu, "editor_ssed_set", , , -409
  append_simplemenu_item menu, "editor_ssed_palette_root", , , -410
  append_simplemenu_item menu, "editor_ssed_info_text_right", , , -411
  append_simplemenu_item menu, "editor_ssed_caption_text", , , -412
  append_simplemenu_item menu, "editor_enemy_sprite", , , -500
  append_simplemenu_item menu, "root", , , -100000
  append_simplemenu_item menu, "textbox_text", , , -100001
  append_simplemenu_item menu, "textbox_portrait", , , -100002
  append_simplemenu_item menu, "textbox_choice0", , , -100003
  append_simplemenu_item menu, "textbox_choice1", , , -100004
  append_simplemenu_item menu, "textbox_box", , , -100016
  append_simplemenu_item menu, "textbox_portrait_box", , , -100017
  append_simplemenu_item menu, "textbox_choice_box", , , -100018
  append_simplemenu_item menu, "textbox_root", , , -100019
  append_simplemenu_item menu, "script_layer", , , -100005
  append_simplemenu_item menu, "textbox_layer", , , -100006
  append_simplemenu_item menu, "string_layer", , , -100007
  append_simplemenu_item menu, "reserve", , , -100021
  append_simplemenu_item menu, "maproot", , , -100008
  append_simplemenu_item menu, "obsolete_overhead", , , -100009
  append_simplemenu_item menu, "map_overlay", , , -100020
  append_simplemenu_item menu, "walkabout_layer", , , -100010
  append_simplemenu_item menu, "hero_layer", , , -100011
  append_simplemenu_item menu, "npc_layer", , , -100012
  append_simplemenu_item menu, "walkabout_sprite", , , -100013
  append_simplemenu_item menu, "walkabout_shadow", , , -100014
  append_simplemenu_item menu, "backdrop", , , -100015
  append_simplemenu_item menu, "map_layer0", , , -101000
  append_simplemenu_item menu, "map_layer1", , , -101001
  append_simplemenu_item menu, "map_layer2", , , -101002
  append_simplemenu_item menu, "map_layer3", , , -101003
  append_simplemenu_item menu, "map_layer4", , , -101004
  append_simplemenu_item menu, "map_layer5", , , -101005
  append_simplemenu_item menu, "map_layer6", , , -101006
  append_simplemenu_item menu, "map_layer7", , , -101007
  append_simplemenu_item menu, "map_layer8", , , -101008
  append_simplemenu_item menu, "map_layer9", , , -101009
  append_simplemenu_item menu, "map_layer10", , , -101010
  append_simplemenu_item menu, "map_layer11", , , -101011
  append_simplemenu_item menu, "map_layer12", , , -101012
  append_simplemenu_item menu, "map_layer13", , , -101013
  append_simplemenu_item menu, "map_layer14", , , -101014
  append_simplemenu_item menu, "map_layer15", , , -101015
  append_simplemenu_item menu, "status_portrait", , , -102000
  append_simplemenu_item menu, "status_walkabout", , , -102001
  append_simplemenu_item menu, "status_battlesprite", , , -102002
  append_simplemenu_item menu, "status_page_select", , , -102003
  append_simplemenu_item menu, "status_statlist", , , -102004
  append_simplemenu_item menu, "status_hide_if_no_mp", , , -102005
  append_simplemenu_item menu, "status_hide_if_no_lmp", , , -102006
  append_simplemenu_item menu, "status_hide_if_max_lev", , , -102007
  append_simplemenu_item menu, "plank_holder", , , -102008
  append_simplemenu_item menu, "status_hide_if_no_portrait", , , -102009
  append_simplemenu_item menu, "item_itemlist", , , -102010
  append_simplemenu_item menu, "item_exitbutton", , , -102011
  append_simplemenu_item menu, "item_sortbutton", , , -102012
  append_simplemenu_item menu, "item_trashbutton", , , -102013
  append_simplemenu_item menu, "plank_menu_selectable", , , -102014
  append_simplemenu_item menu, "spell_listlist", , , -102015
  append_simplemenu_item menu, "spell_spelllist", , , -102016
  append_simplemenu_item menu, "spell_hide_if_no_list", , , -102017
  append_simplemenu_item menu, "spell_cancelbutton", , , -102018
  append_simplemenu_item menu, "virtual_keyboard_button", , , -102019
  append_simplemenu_item menu, "virtual_keyboard_buttontext", , , -102020
  append_simplemenu_item menu, "virtual_keyboard_shift", , , -102021
  append_simplemenu_item menu, "virtual_keyboard_symbols", , , -102022
  append_simplemenu_item menu, "virtual_keyboard_select", , , -102023
  append_simplemenu_item menu, "virtual_keyboard_entrytext", , , -102024
  append_simplemenu_item menu, "virtual_keyboard_del", , , -102025
  append_simplemenu_item menu, "virtual_keyboard_enter", , , -102026
  append_simplemenu_item menu, "shop_buy_info_panel", , , -102027
  append_simplemenu_item menu, "status_hide_if_no_hp", , , -102028
  append_simplemenu_item menu, "pathfind_dest_display", , , -102100
'</SLICE LOOKUP NAMES>

 END IF

 append_simplemenu_item menu, "User Defined Lookup Codes", YES, uiLook(uiText)
 DIM userdef_start as integer = v_len(menu) - 1

 FOR i as integer = 1 TO UBOUND(slicelookup)
  append_simplemenu_item menu, slicelookup(i), , , i
 NEXT i

 DIM st as MenuState
 init_menu_state st, cast(BasicMenuItem vector, menu)

 FOR i as integer = 0 to v_len(menu) - 1
  'Move the cursor to pre-select the current code
  IF v_at(menu, i)->dat = start_at_code THEN
   st.pt = i
   EXIT FOR
  END IF
 NEXT i

 DIM menuopts as MenuOptions
 menuopts.highlight = YES

 DIM curcode as integer = 0

 setkeys YES
 DO
  setwait 55
  setkeys YES

  usemenu st, cast(BasicMenuItem vector, menu)
  curcode = v_at(menu, st.pt)->dat
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "slice_lookup_codes"
  IF keyval(scSpace) = 0 ANDALSO enter_space_click(st) THEN
   IF curcode <> -1 THEN result = curcode  'Not 'Previous Menu'
   EXIT DO
  END IF

  'Special handling that only happens for the user-defined lookup codes
  IF st.pt > userdef_start THEN

   'Edit lookup codes
   IF strgrabber(slicelookup(curcode), 40) THEN
    slicelookup(curcode) = sanitize_script_identifier(slicelookup(curcode))
    v_at(menu, st.pt)->text = slicelookup(curcode)
   END IF

   '--make the list longer if we have selected the last item in the list and it is not blank
   IF st.pt = st.last ANDALSO TRIM(slicelookup(curcode)) <> "" THEN
    REDIM PRESERVE slicelookup(UBOUND(slicelookup) + 1) as string
    append_simplemenu_item menu, "", , , UBOUND(slicelookup)
    st.last += 1
   END IF

  END IF

  clearpage dpage
  draw_fullscreen_scrollbar st, , dpage
  standardmenu cast(BasicMenuItem vector, menu), st, 0, 0, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 '--shrink the end of the list to exclude blank ones.
 shrink_lookup_list slicelookup()

 '--Make sure the 0 string is blank
 slicelookup(0) = ""

 IF can_write_to_workingdir THEN  'not live previewing
  save_string_list slicelookup(), workingdir & SLASH & "slicelookup.txt"
 END IF

 RETURN result
END FUNCTION

FUNCTION slice_color_caption(byval n as integer, ifzero as string="0") as string
 IF n = 0 THEN RETURN ifzero
 'Normal colors
 IF n > 0 ANDALSO n <= 255 THEN RETURN STR(n)
 'uilook colors
 IF n <= -1 ANDALSO n >= LowColorCode() THEN
  RETURN UiColorCaption(n * -1 - 1)
 END IF
 'Invalid values still print, but !?
 RETURN n & "(!?)"
END FUNCTION


'==========================================================================================
'                                       Settings menu
'==========================================================================================

TYPE SliceEditSettingsMenu EXTENDS ModularMenu
 ses as SliceEditState ptr
 edslice as Slice ptr
 in_detail_editor as bool

 DECLARE SUB update ()
 DECLARE FUNCTION each_tick () as bool
END TYPE

SUB SliceEditSettingsMenu.update()
 add_item 0 , , "[Close]"

 IF ses->curslice <> NULL THEN
  header "Selected slice:"
  WITH *ses->curslice
   add_item 1, , "Visible: " & yesorno(.Visible) & " (V)"
   IF .NumChildren > 0 THEN
    add_item 2, , "Collapse (hide) children: " & yesorno(.EditorHideChildren) & IIF(in_detail_editor, "", " (H)")
   END IF
   add_item 3, , "Reset position & alignment (R)"
  END WITH
  'This also works if curslice is NULL; then it focuses on edslice
  add_item 4, , "Focus view on the slice (F)"
 END IF

 IF in_detail_editor = NO THEN
  add_spacer
#IFDEF IS_CUSTOM
  add_item 9, , "Import collection (F2)"
#ENDIF
  add_item 10, , "Export collection (F3)"
 END IF

 header "Editor Settings"
 DIM hide_captions(...) as string = {"Show menu and slices", "Hide menu background", "Hide slices", "Hide menu"}
 IF in_detail_editor = NO THEN
  add_item 7, , "Show positions: " & yesorno(ses->show_positions)
  add_item 8, , "Show sizes: " & yesorno(ses->show_sizes)
 END IF
 add_item 11, , safe_caption(hide_captions(), ses->hide_mode) & " (F4)"
 add_item 12, , "Show root slice: " & yesorno(ses->show_root) & " (F5)"
'IIF(ses->show_root, "Show", "Hide") & " root slice (F5)"
 add_item 13, , "Shift viewport... (F6)"
 add_item 14, , "Show ants: " & yesorno(ses->show_ants) & " (F7)"
 'add_item 15, , "This menu (F8)"
#IFDEF IS_CUSTOM
 add_item 16, , "Global Editor Options (F9)"
#ENDIF
 add_item 17, , "Switch to " & IIF(vpages_are_32bit, 8, 32) & "-bit color mode (Ctrl-F3)"
 IF NOT vpages_are_32bit THEN
  add_item 18, , "Blend algorithm: " & BlendAlgoCaptions(gen(gen8bitBlendAlgo)) & " (Ctrl-F4)"
 END IF
END SUB

FUNCTION SliceEditSettingsMenu.each_tick() as bool
 DIM activate as bool = enter_space_click(state)
 DIM changed as bool
 SELECT CASE itemtypes(state.pt)
  CASE 0
   IF activate THEN RETURN YES
  CASE 1  'Toggle visible
   changed = boolgrabber(ses->curslice->Visible, state)
  CASE 2  'Toggle subtree hidden
   changed = boolgrabber(ses->curslice->EditorHideChildren, state)
  CASE 3  'Reset position/align
   IF activate THEN
    slice_editor_reset_slice *ses, ses->curslice
    RETURN YES
   END IF
  CASE 4  'Focus view on slice
   IF activate THEN
    slice_editor_focus_on_slice *ses, edslice
    RETURN YES  'quit
   END IF

  CASE 7
   changed = boolgrabber(ses->show_positions, state)
  CASE 8
   changed = boolgrabber(ses->show_sizes, state)
#IFDEF IS_CUSTOM
  CASE 9
   IF activate THEN
    slice_editor_import_prompt *ses, edslice
    changed = YES
   END IF
#ENDIF
  CASE 10
   IF activate THEN slice_editor_export_prompt *ses, edslice
  CASE 11  'Hide menu/slices
   changed = intgrabber(ses->hide_mode, 0, hideLAST)
  CASE 12
   changed = boolgrabber(ses->show_root, state)
  CASE 13  'Shift viewport
   IF activate THEN
    DIM true_root as Slice ptr = FindRootSlice(edslice)
    slice_editor_xy true_root->X, true_root->Y, ses->draw_root, edslice, ses->show_ants
   END IF
  CASE 14
   changed = boolgrabber(ses->show_ants, state)
  'CASE 15  'This menu: nothing
#IFDEF IS_CUSTOM
  CASE 16
   IF activate THEN Custom_global_menu
#ENDIF
  CASE 17  '8/32 bit color
   IF activate THEN
    toggle_32bit_vpages
    changed = YES
   END IF
  CASE 18  'Blend algo
   changed = intgrabber(gen(gen8bitBlendAlgo), 0, blendAlgoLAST)
 END SELECT
 state.need_update OR= changed
END FUNCTION

SUB slice_editor_settings_menu(byref ses as SliceEditState, byref edslice as Slice ptr, in_detail_editor as bool)
 DIM menu as SliceEditSettingsMenu
 menu.floating = YES
 menu.menuopts.edged = YES
 menu.ses = @ses
 menu.edslice = edslice
 menu.in_detail_editor = in_detail_editor
 menu.title = "Slice editor settings/tools (F8)"
 menu.helpkey = "sliceedit_settings"
 menu.run()
 edslice = menu.edslice  'Changes eg. when importing

 slice_editor_save_settings ses
END SUB

SUB slice_editor_save_settings(byref ses as SliceEditState)
 write_config "sliceedit.show_positions", yesorno(ses.show_positions)
 write_config "sliceedit.show_sizes", yesorno(ses.show_sizes)
 'show_ants and hide_mode are not saved.
 'sliceedit.show_root was renamed to sliceedit.show_root2 to ignore previous setting
 'While in the recursive slice editor, show_root gets set to YES by default
 IF ses.recursive = NO THEN write_config "sliceedit.show_root2", yesorno(ses.show_root)
END SUB

SUB slice_editor_load_settings(byref ses as SliceEditState)
 ses.show_positions = read_config_bool("sliceedit.show_positions", NO)
 ses.show_sizes = read_config_bool("sliceedit.show_sizes", NO)
 'See above
 IF ses.recursive = NO THEN ses.show_root = read_config_bool("sliceedit.show_root2", YES)
END SUB

'==========================================================================================

'This wrapper around SliceLoadFromFile falls back to a default for the various special slice collections
SUB load_slice_collection (byval sl as Slice Ptr, byval collection_kind as integer, byval collection_num as integer=0)
 DIM filename as string
 filename = workingdir & SLASH & "slicetree_" & collection_kind & "_" & collection_num & ".reld"
 IF isfile(filename) THEN
  SliceLoadFromFile sl, filename, , IIF(collection_kind = SL_COLLECT_USERDEFINED, collection_num, -1)
 ELSE
  SELECT CASE collection_kind
   CASE SL_COLLECT_STATUSSCREEN:
    default_status_screen sl
   CASE SL_COLLECT_STATUSSTATPLANK:
    default_status_stat_plank sl
   CASE SL_COLLECT_ITEMSCREEN:
    default_item_screen sl
   CASE SL_COLLECT_ITEMPLANK:
    default_item_plank sl
   CASE SL_COLLECT_SPELLSCREEN:
    default_spell_screen sl
   CASE SL_COLLECT_SPELLLISTPLANK:
    default_spell_list_plank sl
   CASE SL_COLLECT_SPELLPLANK:
    default_spell_spell_plank sl
   CASE SL_COLLECT_VIRTUALKEYBOARDSCREEN:
    default_virtual_keyboard_screen sl
   CASE ELSE
    debug "WARNING: no default slice collection for collection kind " & collection_kind
  END SELECT
 END IF
END SUB
