'OHRRPGCE GAME - Slices
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef SLICES_BI
#define SLICES_BI

#include "udts.bi"
#include "common.bi"
#include "reload.bi"

CONST ENABLE_SLICE_DEBUG = NO

' Builtin slice lookup codes
'These constants also need to be updated in slices.bas and plotscr.hsd
'and plotdict.xml. You may choose to just update them here and then run
' misc/sl_lookup.py
'to update the other files. Be aware that once you have picked a number
'for one of these, you should not change it, because it could get used
'as a constant in someone's script, and changing it would break any
'non-recompiled script that used it.
'Lookup codes starting with SL_EDITOR are for editor slice collections in
'Custom, and aren't included in plotscr.hsd/plotdict.xml. Other lookup
'codes can also be omitted from plotscr.hsd/plotdict.xml by adding a
'comment containing "Hide".
'Multiple names for a lookup code can be defined by adding a comment
'containing "Alias" in ones after the first. The first name is the main one.
'Other comments are ignored.

'<SLICE LOOKUP CODES>
CONST SL_EDITOR_SPLASH_MENU   = -100
CONST SL_EDITOR_THINGBROWSER_THINGLIST      = -200
CONST SL_EDITOR_THINGBROWSER_PLANK_SPRITE   = -201
CONST SL_EDITOR_THINGBROWSER_BACK_HOLDER    = -202
CONST SL_EDITOR_THINGBROWSER_MODE_INDICATOR = -203
CONST SL_EDITOR_THINGBROWSER_NEW_HOLDER     = -204
CONST SL_EDITOR_THINGBROWSER_NOSCROLL_AREA  = -205
CONST SL_EDITOR_THINGBROWSER_FILTER_HOLDER  = -206
CONST SL_EDITOR_THINGBROWSER_TYPE_QUERY     = -207
CONST SL_EDITOR_THINGBROWSER_FILTER_TEXT    = -208
CONST SL_EDITOR_PROMPT_FOR_STRING_TEXT      = -300
CONST SL_EDITOR_PROMPT_FOR_STRING_CAPTION   = -301
CONST SL_EDITOR_SSED_LIST              = -400
CONST SL_EDITOR_SSED_SET_TEMPL         = -401
CONST SL_EDITOR_SSED_FRAME_HOLDER      = -402
CONST SL_EDITOR_SSED_FRAME_TEMPL       = -403
CONST SL_EDITOR_SSED_FRAME_SPRITE      = -404
CONST SL_EDITOR_SSED_INFO_TEXT         = -405
CONST SL_EDITOR_SSED_PALETTE_GRID      = -406
CONST SL_EDITOR_SSED_PALETTE_TEXT      = -407
CONST SL_EDITOR_SSED_SET_INFO          = -408
CONST SL_EDITOR_SSED_SET               = -409
CONST SL_EDITOR_SSED_PALETTE_ROOT      = -410
CONST SL_EDITOR_SSED_INFO_TEXT_RIGHT   = -411
CONST SL_EDITOR_SSED_CAPTION_TEXT      = -412
CONST SL_EDITOR_ENEMY_SPRITE           = -500
CONST SL_ROOT                 = -100000
CONST SL_TEXTBOX_TEXT         = -100001
CONST SL_TEXTBOX_PORTRAIT     = -100002
CONST SL_TEXTBOX_CHOICE0      = -100003
CONST SL_TEXTBOX_CHOICE1      = -100004
CONST SL_TEXTBOX_BOX          = -100016
CONST SL_TEXTBOX_PORTRAIT_BOX = -100017
CONST SL_TEXTBOX_CHOICE_BOX   = -100018
CONST SL_TEXTBOX_ROOT         = -100019
CONST SL_SCRIPT_LAYER         = -100005
CONST SL_TEXTBOX_LAYER        = -100006
CONST SL_STRING_LAYER         = -100007
CONST SL_RESERVE              = -100021
CONST SL_MAPROOT              = -100008
CONST SL_OBSOLETE_OVERHEAD    = -100009
CONST SL_MAP_OVERLAY          = -100020
CONST SL_WALKABOUT_LAYER      = -100010
CONST SL_HERO_LAYER           = -100011
CONST SL_NPC_LAYER            = -100012
CONST SL_WALKABOUT_SPRITE = -100013
CONST SL_WALKABOUT_SPRITE_COMPONENT = -100013  'Alias
CONST SL_WALKABOUT_SHADOW = -100014
CONST SL_WALKABOUT_SHADOW_COMPONENT = -100014  'Alias
CONST SL_BACKDROP             = -100015
CONST SL_MAP_LAYER0           = -101000
CONST SL_MAP_LAYER1           = -101001
CONST SL_MAP_LAYER2           = -101002
CONST SL_MAP_LAYER3           = -101003
CONST SL_MAP_LAYER4           = -101004
CONST SL_MAP_LAYER5           = -101005
CONST SL_MAP_LAYER6           = -101006
CONST SL_MAP_LAYER7           = -101007
CONST SL_MAP_LAYER8           = -101008
CONST SL_MAP_LAYER9           = -101009
CONST SL_MAP_LAYER10          = -101010
CONST SL_MAP_LAYER11          = -101011
CONST SL_MAP_LAYER12          = -101012
CONST SL_MAP_LAYER13          = -101013
CONST SL_MAP_LAYER14          = -101014
CONST SL_MAP_LAYER15          = -101015
CONST SL_STATUS_PORTRAIT      = -102000  'Hide
CONST SL_STATUS_WALKABOUT     = -102001  'Hide
CONST SL_STATUS_BATTLESPRITE  = -102002  'Hide
CONST SL_STATUS_PAGE_SELECT   = -102003  'Hide
CONST SL_STATUS_STATLIST      = -102004  'Hide
CONST SL_STATUS_HIDE_IF_NO_MP = -102005  'Hide
CONST SL_STATUS_HIDE_IF_NO_LMP = -102006  'Hide
CONST SL_STATUS_HIDE_IF_MAX_LEV = -102007  'Hide
CONST SL_PLANK_HOLDER         = -102008  'Hide
CONST SL_STATUS_HIDE_IF_NO_PORTRAIT = -102009  'Hide
CONST SL_ITEM_ITEMLIST        = -102010  'Hide
CONST SL_ITEM_EXITBUTTON      = -102011  'Hide
CONST SL_ITEM_SORTBUTTON      = -102012  'Hide
CONST SL_ITEM_TRASHBUTTON     = -102013  'Hide
CONST SL_PLANK_MENU_SELECTABLE = -102014  'Hide
CONST SL_SPELL_LISTLIST       = -102015  'Hide
CONST SL_SPELL_SPELLLIST      = -102016  'Hide
CONST SL_SPELL_HIDE_IF_NO_LIST = -102017  'Hide
CONST SL_SPELL_CANCELBUTTON   = -102018  'Hide
CONST SL_VIRTUAL_KEYBOARD_BUTTON     = -102019  'Hide
CONST SL_VIRTUAL_KEYBOARD_BUTTONTEXT = -102020  'Hide
CONST SL_VIRTUAL_KEYBOARD_SHIFT      = -102021  'Hide
CONST SL_VIRTUAL_KEYBOARD_SYMBOLS    = -102022  'Hide
CONST SL_VIRTUAL_KEYBOARD_SELECT     = -102023  'Hide
CONST SL_VIRTUAL_KEYBOARD_ENTRYTEXT  = -102024  'Hide
CONST SL_VIRTUAL_KEYBOARD_DEL        = -102025  'Hide
CONST SL_VIRTUAL_KEYBOARD_ENTER      = -102026  'Hide
CONST SL_SHOP_BUY_INFO_PANEL         = -102027  'Hide
CONST SL_STATUS_HIDE_IF_NO_HP        = -102028  'Hide
CONST SL_PATHFIND_DEST_DISPLAY       = -102100
'</SLICE LOOKUP CODES>

#define edsl(lookupcode_suffix, parent)  LookupSlice(_CONCAT(EDSL_PREFIX, lookupcode_suffix), parent)
#define EDSL_PREFIX SL_EDITOR_

Type SliceTypes as integer
Enum 'SliceTypes
 slInvalid = -1
 '0 was slRoot
 slSpecial = 1
 slContainer
 slRectangle
 slSprite
 slText
 slMap
 slGrid
 slEllipse
 slScroll
 slSelect
 slPanel
 slLayout
 slLine
 'Remember to update slicetype constants in plotscr.hsd
End Enum

Enum AttachTypes
 slSlice
 slScreen
End Enum

Enum AutoSortModes
 slAutoSortNone
 slAutoSortCustom
 slAutoSortY
 slAutoSortTopY
 slAutoSortCenterY
 slAutoSortBottomY
End Enum

Enum FillModes
 sliceFillFull = 0
 sliceFillHoriz = 1
 sliceFillVert = 2
End Enum

Enum CoverModes
 coverNone = 0
 coverHoriz = 1
 coverVert = 2
 coverFull = 3
End Enum

' Stores information about what this slice is used for, if that isn't explained
' by the lookup code.
Type SliceContext Extends Object
 Declare Virtual Destructor()
 Declare Abstract Function description() as string
 ' Contexts can't necessarily be loaded and saved; implementing save/load is optional.
 Declare Virtual Sub save(node as Reload.Nodeptr)
 Declare Virtual Sub load(node as Reload.Nodeptr)
End Type

DECLARE_VECTOR_OF_TYPE(SliceContext ptr, SliceContext_ptr)

' The root slice of slice collections have this Context.
' In future, it would be a good place to store slice animations
' and other data shared across the collection.
Type SliceCollectionContext Extends SliceContext
 Declare Virtual Function description() as string
 Declare Virtual Sub save(node as Reload.Nodeptr)
 Declare Virtual Sub load(node as Reload.Nodeptr)
 name as string
 id as integer = -1      'Only used by user collections. Not saved. -1 means unknown
End Type

Extern "C"
Type SliceFwd as Slice
Type SliceDraw as Sub(Byval as SliceFwd ptr, byval page as integer)
Type SliceDispose as Sub(Byval as SliceFwd ptr)
Type SliceClone as Sub(Byval as SliceFwd ptr, byval as SliceFwd ptr)
Type SliceSave as Sub(Byval as SliceFwd ptr, byval node as Reload.Nodeptr)
Type SliceLoad as Sub(Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
Type SliceChildRefresh as Sub(Byval par as SliceFwd ptr, Byval ch as SliceFwd ptr, childindex as integer = -1, visibleonly as bool = YES)
Type SliceChildrenRefresh as Sub(Byval par as SliceFwd ptr)
Type SliceChildDraw as Sub(Byval s as SliceFwd ptr, Byval page as integer)
End Extern

'Eventually, Slice will be replaced with this OO-based ClassSlice,
'but currently it's only used for certain Special slices.
Type ClassSlice Extends Object
 Declare Virtual Sub Initialize(sl as SliceFwd ptr)
 Declare Virtual Destructor()
 Declare Virtual Sub Draw(sl as SliceFwd ptr, page as integer)
 Declare Virtual Sub Clone(sl as SliceFwd ptr, as SliceFwd ptr)
 Declare Virtual Sub Save(sl as SliceFwd ptr, node as Reload.Nodeptr)
 Declare Virtual Sub Load(sl as SliceFwd ptr, node as Reload.Nodeptr)
 Declare Virtual Sub ChildRefresh(sl as SliceFwd ptr, ch as SliceFwd ptr, childindex as integer = -1, visibleonly as bool = YES)
 Declare Virtual Sub ChildrenRefresh(sl as SliceFwd ptr)
 Declare Virtual Sub ChildDraw(sl as SliceFwd ptr, page as integer)
End Type

Extern "C"

Type RectangleSliceDataFwd as RectangleSliceData
Type LineSliceDataFwd as LineSliceData
Type TextSliceDataFwd as TextSliceData
Type SpriteSliceDataFwd as SpriteSliceData
Type MapSliceDataFwd as MapSliceData
Type GridSliceDataFwd as GridSliceData
Type LayoutSliceDataFwd as LayoutSliceData
Type EllipseSliceDataFwd as EllipseSliceData
Type ScrollSliceDataFwd as ScrollSliceData
Type SelectSliceDataFwd as SelectSliceData
Type PanelSliceDataFwd as PanelSliceData


Type Slice
  Parent as Slice Ptr
  FirstChild as Slice Ptr
  LastChild as Slice Ptr
  NextSibling as Slice Ptr
  PrevSibling as Slice Ptr
  NumChildren as integer
  
  Union
    Type
      X as integer       'The X,Y relative to whatever the slice is attached to
      Y as integer
    End Type
    Pos as XYPair
  End Union

  Union
    Type
      ScreenX as integer 'The actual X,Y, updated every frame
      ScreenY as integer
    End Type
    ScreenPos as XYPair
  End Union

  Union
    Type
      Width as integer
      Height as integer
    End Type
    Size as XYPair
  End Union

  Visible as bool
  Paused as bool  'Whether to not apply target and velocity movement to this slice tree
  Clip as bool
  
  'moving at a constant pixels-per-tick speed (direct setting should cancel targ)
  Velocity as XYPair
  'limit the number of cycles to apply velocity before auto-clearing it (-1 means forever, 0 clears instantly)
  VelTicks as XYPair

  'moving to a destination in constant time. (replaces velocity)
  Targ as XYPair
  TargResidue_X as double
  TargResidue_Y as double
  TargTicks as integer

  Context as SliceContext ptr  'NULL if none
  TableSlot as integer 'which slot in plotslices() holds a reference to this slice, or 0 for none
  Lookup as integer

  EditorColor as integer 'Not saved, used only by slice editor
  EditorHideChildren as bool 'Saved, but only matters for the editor

  AutoSort as AutoSortModes
  Sorter as integer        'Sort order, used by CustomSortChildSlices. Lower to the bottom.

  Extra(2) as integer

  AlignHoriz as AlignType  'Relative to parent. Only used when not filling
  AlignVert as AlignType   'Relative to parent. Only used when not filling
  AnchorHoriz as AlignType 'Relative to self. Only used when not filling
  AnchorVert as AlignType  'Relative to self. Only used when not filling
  ClampHoriz as AlignType  'alignNone for no clamping. Only used when not filling
  ClampVert as AlignType  'alignNone for no clamping. Only used when not filling

  as integer PaddingTop, PaddingLeft, PaddingRight, PaddingBottom

  ' Note that setting a slice to Fill causes its size to be modified, and its
  ' position to be ignored. This inconsistency is unfortunate. It also means
  ' the size of non-resizeable slices can be changed, which is a bug.
  Fill as bool
  FillMode as FillModes
  Declare Function FillHoriz() as bool
  Declare Function FillVert() as bool

  CoverChildren as CoverModes

  'Attach changes which slice is responsible for ChildRefresh, but not ChildDraw
  'or ChildrenRefresh. (It's not possible to support Attach for ChildrenRefresh,
  'because the parent needs to be aware of all the attached slices to lay them out).
  'In other words, changes where the child is drawn but not when.
  Attach as AttachTypes  'Not saved
  Union
   Attached as Slice ptr 'Not saved
  End Union

  'Draws the slice itself, not including its children, if visible.
  Draw as SliceDraw      'NULL for some slice types
  'The following delete, clone or load/save SliceData to a RELOAD node.
  'They aren't responsible for any data in this Slice UDT.
  Dispose as SliceDispose
  Clone as SliceClone
  Save as SliceSave
  Load as SliceLoad
  'Updates the screen position and size of one child, according to parent position,
  'alignment, anchoring, fill and slice-specific placement of children (Grid and Panel).
  'Might also change .Visible (Select slices).
  'For all other types this is DefaultChildRefresh.
  ChildRefresh as SliceChildRefresh
  'Alternative to ChildRefresh, updates the screen positions of all children at once.
  'This should be used if all children need to be updated together.
  'Note this isn't exactly equivalent to doing the work in ChildRefresh instead:
  'FindSliceCollision and FindSliceAtPoint don't call ChildrenRefresh.
  'Also, ChildrenRefresh is always called on the parent of a slice,
  'not the slice it's attached to. Attach isn't supported for that.
  'NOTE: If ChildrenRefresh is implemented, you must set ChildRefresh to NullChildRefresh.
  ChildrenRefresh as SliceChildrenRefresh  'NULL for most slice types
  'Called after Draw. Draws each child (by calling DrawSlice) while handling clipping.
  'For most slice types this is DefaultChildDraw.
  'This function can be overriden to either apply special clipping rules (Grid, Panel)
  'or to draw something on top of the children (Scroll), or to change recursion
  '(Panel only draws the first two children, Grid only draws rows*cols many)
  ChildDraw as SliceChildDraw

  SliceType as SliceTypes

  Union
    SliceData    as any ptr
    ClassInst    as ClassSlice ptr
    RectData     as RectangleSliceDataFwd ptr
    LineData     as LineSliceDataFwd ptr
    TextData     as TextSliceDataFwd ptr
    SpriteData   as SpriteSliceDataFwd ptr
    MapData      as MapSliceDataFwd ptr
    GridData     as GridSliceDataFwd ptr
    LayoutData   as LayoutSliceDataFwd ptr
    EllipseData  as EllipseSliceDataFwd ptr
    ScrollData   as ScrollSliceDataFwd ptr
    SelectData   as SelectSliceDataFwd ptr
    PanelData    as PanelSliceDataFwd ptr
  End Union

  'Protect is used to mark slices that script authors should not be
  'allowed to directly delete or reparent.
  'Note that this is only checked when a slice is directly freed or
  'moved, so if a Protected slice has an unprotected ancestor, then
  'it can still be deleted or moved indirectly.
  Protect as bool

  'NOTE: When adding to this, remember to update CloneSliceTree, SliceLoadFromNode and SliceSaveToNode
End Type

'NOTE: the Slices are not freed when the vector is freed!
DECLARE_VECTOR_OF_TYPE(Slice ptr, Slice_ptr)

'--Data containers for various slice types

Type RectangleSliceData
 'If anything aside from translucent/fuzzfactor is manually changed, set style=-1 and style_loaded=NO
 fgcol as integer
 border as RectBorderTypes = borderLine   'borderNone/borderLine/0-14 for box style's border
 translucent as RectTransTypes
 fuzzfactor as integer = 50     'For transFuzzy and transBlend (as opacity)
 fuzz_stationary as bool
 fuzz_zoom as integer = 1
 bgcol as integer
 'if style is changed then set style_loaded = NO
 style as integer = -1    '-1: None, 0-14: style
 style_loaded as bool 'Used internally flag whether a change of style has been applied to fgcol, bgcol, or border

 'When use_raw_box_border is YES, ignore .border and use .raw_box_border instead.
 use_raw_box_border as bool
 raw_box_border as integer

 'Declare constructor (byval style as integer = -1, byval bgcol as integer=0, byval translucent as bool = NO, byval fgcol as integer = -1, byval border as integer = -1)
End Type

Type LineSliceData
 col as integer
 'flipped as bool

 Declare Sub SetColor(color as integer)
End Type

Type TextSliceData
 col as integer
 bgcol as integer
 outline as bool
 s as string
 s_orig as string 'UNSAVED: Used when expanding ${} codes, so the codes can be re-expanded again later
 wrap as bool     'Whether to wrap the text according to slice width. Otherwise slice width is determined by s.
 'Declare constructor(st as string, byval col as integer = -1, byval ol as bool = YES)

 'All of the following are UNSAVED and not cloned and are not exposed to users in the editor or in scripts
 use_render_text as bool 'Use alternative render_text-based implementation. Enables text markup.
 insert as integer 'char offset of insertion pointer. Zero-based!
 show_insert as bool    ' set to YES to display insertion point
 insert_tog as integer 'flash state of insertion pointer (0 or 1)
 first_line as integer 'Top-most line to show. Used for scrolling
 line_limit as integer = -1 'Number of lines to display. -1 is no limit
 line_count as integer 'automatically populated when the slice changes
End Type

'FIXME: Support for modifying sprites and flipping is pretty tacked on; generalise!
Type SpriteSliceData
 spritetype as SpriteType
 assetfile as string ptr   '(sprTypeFrame only, ignored for other sprites)
                           'Optionally name of the file in data/ from which to load this sprite
                           '(as an 8-bit image). If this is NULL, then a sprTypeFrame was
                           'created from a Frame loaded from elsewhere, and can't be saved.
                           '(The string memory is owned by this slice. Used a string ptr
                           'to minimise overhead for non-asset slices.)
 load_asset_as_32bit as bool
 record as integer  'Spriteset number. Meaningless if spritetype is sprTypeFrame
 frame as integer   'Currently displaying frame number. Must be 0 if spritetype is sptTypeFrame
 paletted as bool   'UNSAVED: YES: 4-bit, NO: 8-bit  (could remove this when 256-colour palettes added, or change meaning)
 pal as integer     '(UNSAVED if unpaletted) Set pal to -1 for the default. Ignored for unpaletted.
                    '-2 if using a custom Palette16 ptr (sprTypeFrame only).
 trans as bool      'Draw with color 0 as transparent?
 loaded as bool     'UNSAVED: Set to NO to force a re-load on the next draw
 img as GraphicPair 'UNSAVED: No need to manually populate this, done in draw (.pal = NULL for unpaletted)

 'Transformations
 flipHoriz as bool  'NO normal, YES horizontally flipped
 flipVert as bool   'NO normal, YES vertically flipped
 scaled as bool     'Scale the sprite to the size of the slice. 32-bit only! Cached. SEPARATE to rotozooming.
 '(experimental rotozoom options:)
 rotate as integer  'UNSAVED: Clockwise angle in degrees, normally 0-359
 zoom as single     'UNSAVED. Zoom ratio. Defaults to 1.
 rz_smooth as integer  'UNSAVED: 0-2 rotozoom smoothness. 0: none, 1: use bi-linear filtering (32-bit only)
                       '2: use scale_surface, better when shrinking (Non-rotated & 32-bit only)

 'Blending/transparency settings
 'drawopts.scale and drawopts.write_mask are unused.
 drawopts as DrawOptions = def_drawoptions

 'dissolve state data
 dissolving as bool
 d_time as integer ' number of ticks that the dissolve should last
 d_tick as integer ' counts which tick the dissolve is in right now
 d_type as integer ' id number of the dissolve animation
 d_back as bool ' backwards: NO dissolve away, YES dissolve back in
 d_auto as bool ' YES if the dissolve is animating automatically
                ' (d_tick advances when drawn) (FIXME: wrong place for that)
End Type

'Shows the currently loaded map at the given slice pos
'Doesn't yet have the ability to load other non-current maps.
'Can NOT be saved, loaded or cloned. (Hmm... cloning could be useful...)
Type MapSliceData
 transparent as bool 'Whether or not color 0 in the tileset is transparent
 drawopts as DrawOptions = def_drawoptions
 overlay as integer  '0, 1, or 2. For backcompat with layers affect by obsolete overhead bits.
 tileset as TilesetData ptr 'NOTE: ptr to the same memory pointed to by the ptrs in the tilesets() array in game.bas (Not owned!)
 tiles as TileMap ptr 'NOTE: ptr to one of maptiles() in game.bas (Not owned!)
 pass as TileMap ptr 'NOTE: ptr to the passmap ('pass' global) (Not owned!) May be NULL for non-overhead layers
End Type

Type GridSliceData
 show as bool     'Whether to draw the lines of the grid
 rows as integer
 cols as integer
End Type

Type LayoutSliceData
 primary_dir as DirNum = dirRight  'Direction that rows grow
 secondary_dir as DirNum = dirDown 'Direction to shift after a row is full (must be perpendicular to primary_dir)
 primary_padding as integer   'Padding between children, in the primary_dir (within rows)
 secondary_padding as integer '...and between rows
 skip_hidden as bool          'Don't leave gaps for hidden children
 min_row_breadth as integer   'Min height/width in pixels of rows, in the secondary_dir
 justified as bool            'Like justified text: add extra padding to rows to be flush against both edges
 last_row_justified as bool   '(Only when justified) If NO, last row justification spacing is no more than row above.
 cell_alignment as AlignType  'Which edge of its 'cell' in a row that children 'sit' on (eg text sits on a line)
 row_alignment as AlignType   'Which edge of the parent each row is aligned to
                              'Note: if justified is true, then row_alignment only affects rows with just 1 child

 'Temporary members, not saved
 _previous_row_spacing as double 'Used internally by last_row_justified

 Declare Function SkipForward(ch as Slice ptr) as Slice ptr
 Declare Sub SpaceRow(par as Slice ptr, first as Slice ptr, axis0 as integer, dir0 as integer, byref offsets as integer vector, byref breadth as integer)
 Declare Sub Validate()
End Type

Type EllipseSliceData
 bordercol as integer
 fillcol as integer

 'A cache is kept of the ellipse drawn to a Frame, for speed. So the following are UNSAVED
 'FIXME: this backfires if the ellipse is larger than the screen
 last_draw_size as XYPair  'used to detect size changes to force a redraw of the frame
 last_draw_bordercol as integer
 last_draw_fillcol as integer
 frame as Frame Ptr
End Type

Type ScrollSliceData
 style as integer       'The box style bg/edge colors used for drawing scroll bars
 check_depth as integer '0 = check all descendants.
                         '1 = children only.
                         '2 = children+grandchildren only.
                         '3 = children+grandchildren+greatgrandchildren only...
End Type

Type SelectSliceData
 index as integer ' The numeric index of the child that is currently visible.
                   ' If out of range, then no child will be visible. Default to 0
 override as integer ' UNSAVED: Overrides the index, used only in the Slice Collection Editor
End Type

Type PanelSliceData
 vertical as bool 'Defaults to horizontal. Becomes vertical if this is YES
 primary as integer '0 or 1, determines if the first or second child is primary
 pixels as integer 'Fixed-pixel size of the primary. Pixels and percent are combined.
 percent as double 'Percent size of the primary. Pixels and percent are combined
                    'stored as a float. 1.0=100% 0.5=50% 0.01=1%
 padding as integer ' pixels of padding between the sub-panels
End Type

DECLARE Function NewSlice(byval parent as Slice ptr = 0) as Slice Ptr
DECLARE Function NewSliceOfType(byval t as SliceTypes, byval parent as Slice Ptr=0, byval lookup_code as integer=0) as Slice Ptr
DECLARE Function NewClassSlice(parent as Slice ptr, inst as ClassSlice ptr) as Slice ptr
DECLARE Sub DeleteSlice(byval s as Slice ptr ptr, byval debugme as integer = 0)
DECLARE Sub DeleteSliceChildren(byval s as Slice ptr, byval debugme as integer = 0)
DECLARE Function CloneSliceTree(byval sl as Slice ptr, recurse as bool = YES, copy_special as bool = YES) as Slice ptr

DECLARE Sub OrphanSlice(byval sl as slice ptr)
DECLARE Sub SetSliceParent(byval sl as slice ptr, byval parent as slice ptr)
DECLARE Sub ReplaceSliceType(byval sl as slice ptr, byref newsl as slice ptr)

'--Saving and loading slices
DECLARE Sub SliceSaveToNode(byval sl as Slice Ptr, node as Reload.Nodeptr, save_handles as bool=NO)
DECLARE Sub SliceSaveToFile(byval sl as Slice Ptr, filename as string, save_handles as bool=NO)
DECLARE Sub SliceLoadFromNode(byval sl as Slice Ptr, node as Reload.Nodeptr, load_handles as bool=NO)
DECLARE Sub SliceLoadFromFile(byval sl as Slice Ptr, filename as string, load_handles as bool=NO, collection_id as integer=-1)

DECLARE Sub DrawSlice(byval s as slice ptr, byval page as integer)
DECLARE Sub DrawSliceAt(byval s as slice ptr, byval x as integer, byval y as integer, byval w as integer = 100, byval h as integer = 100, byval page as integer, byval ignore_offset as bool = NO)

DECLARE Sub SetSliceTarg(byval s as slice ptr, byval x as integer, byval y as integer, byval ticks as integer)
DECLARE Sub AdvanceSlice(byval s as slice ptr)

DECLARE Sub InsertSliceBefore(byval sl as slice ptr, byval newsl as slice ptr)
DECLARE Sub InsertSliceAfter(byval sl as Slice ptr, byval newsl as Slice ptr)
DECLARE Sub SwapSiblingSlices(byval sl1 as slice ptr, byval sl2 as slice ptr)

DECLARE Sub YSortChildSlices(byval parent as slice ptr)
DECLARE Sub EdgeYSortChildSlices(byval parent as slice ptr, byval edge as AlignType)
DECLARE Sub CustomSortChildSlices(byval parent as slice ptr, byval wipevals as bool)
DECLARE Sub AutoSortChildren(byval s as Slice Ptr)

DECLARE Function SliceIndexAmongSiblings(byval sl as slice ptr) as integer
DECLARE Function SliceChildByIndex(byval sl as slice ptr, byval index as integer) as Slice ptr
DECLARE FUNCTION SlicePath(sl as Slice ptr) as string
DECLARE Function LookupSlice(byval lookup_code as integer, byval root_sl as Slice ptr, byval onlytype as SliceTypes=slInvalid, start_sl as Slice ptr=NULL) as Slice ptr
DECLARE Function LookupSliceSafe(lookup_code as integer, root_sl as Slice ptr, onlytype as SliceTypes=slInvalid) as Slice ptr
DECLARE Function FindRootSlice(slc as Slice ptr) as Slice ptr
DECLARE Function NextDescendent(desc as Slice ptr, parent as Slice ptr) as Slice ptr
DECLARE Function IsAncestor(byval sl as slice ptr, byval ancestor as slice ptr) as bool
DECLARE Function VerifySliceLineage(byval sl as slice ptr, parent as slice ptr) as bool
DECLARE Function CalcContextStack(byval sl as Slice ptr) as SliceContext ptr vector
DECLARE Function UpdateRootSliceSize(sl as slice ptr, page as integer) as bool
DECLARE Function UpdateScreenSlice(clear_changed_flag as bool = YES) as bool
DECLARE Sub RefreshSliceScreenPos(byval sl as slice ptr)
DECLARE Sub RefreshSliceTreeScreenPos(slc as Slice ptr)
DECLARE Sub SliceClamp(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr)

DECLARE Function SliceLegalCoverModes(sl as Slice ptr) as CoverModes
DECLARE Function SlicePossiblyResizable(sl as Slice ptr) as bool

DECLARE Function SliceXAnchor(byval sl as Slice Ptr) as integer
DECLARE Function SliceYAnchor(byval sl as Slice Ptr) as integer
DECLARE Function SliceEdgeX(byval sl as Slice Ptr, byval edge as AlignType) as integer
DECLARE Function SliceEdgeY(byval sl as Slice Ptr, byval edge as AlignType) as integer

DECLARE Sub RealignSlice(sl as Slice ptr, halign as AlignType = -1, valign as AlignType = -1, hanchor as AlignType = -1, vanchor as AlignType = -1)
DECLARE Sub CenterSlice(sl as Slice ptr)

DECLARE Function SliceCollide(byval sl1 as Slice Ptr, sl2 as Slice Ptr) as bool
DECLARE Function SliceCollidePoint(byval sl as Slice Ptr, byval point as XYPair) as bool
DECLARE Function SliceContains(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr) as bool
DECLARE Function FindSliceCollision(parent as Slice Ptr, sl as Slice Ptr, byref num as integer, descend as bool, visibleonly as bool = NO) as Slice Ptr
DECLARE Function FindSliceAtPoint(parent as Slice Ptr, point as XYPair, byref num as integer, descend as bool, visibleonly as bool = NO) as Slice Ptr

DECLARE Function SliceIsInvisible(byval sl as Slice Ptr) as bool
DECLARE Function SliceIsInvisibleOrClipped(byval sl as Slice Ptr) as bool

DECLARE Sub ScrollToChild(byval sl as Slice ptr, byval desc as Slice ptr, byval apply_padding as bool = YES)
DECLARE Sub ScrollAllChildren(byval sl as slice ptr, byval xmove as integer, byval ymove as integer)
DECLARE Sub CalcSliceContentsSize(sl as Slice ptr, byref min as XYPair, byref max as XYPair, check_depth as integer, cur_depth as integer=0)

DECLARE Function FindTextSliceStringRecursively(sl as slice ptr, query as string) as Slice Ptr

End Extern

'Declare any overloaded functions here. Overloaded functions can't be accessed from C/C++

DECLARE FUNCTION SliceTypeName OVERLOAD (sl as Slice Ptr) as string
DECLARE FUNCTION SliceTypeName OVERLOAD (t as SliceTypes) as string
DECLARE FUNCTION SliceLookupCodename OVERLOAD (sl as Slice Ptr, use_default as bool = YES) as string
DECLARE FUNCTION SliceLookupCodename OVERLOAD (byval code as integer, use_default as bool = YES) as string
DECLARE FUNCTION SliceLookupCodename OVERLOAD (code as integer, slicelookup() as string, use_default as bool = YES) as string

Extern "C"

'slice accessors
DECLARE Function SliceGetParent( byval s as Slice ptr ) as Slice ptr
DECLARE Function SliceGetFirstChild( byval s as Slice ptr ) as Slice ptr
DECLARE Function SliceGetLastChild( byval s as Slice ptr ) as Slice ptr
DECLARE Function SliceGetNextSibling( byval s as Slice ptr ) as Slice ptr
DECLARE Function SliceGetPrevSibling( byval s as Slice ptr ) as Slice ptr
DECLARE Function SliceGetNumChildren( byval s as Slice ptr ) as integer
DECLARE Function SliceGetX( byval s as Slice ptr ) as integer
DECLARE Function SliceGetY( byval s as Slice ptr ) as integer
DECLARE Function SliceGetWidth( byval s as Slice ptr ) as integer
DECLARE Function SliceGetHeight( byval s as Slice ptr ) as integer
DECLARE Function SliceIsVisible( byval s as Slice ptr ) as bool
DECLARE Function SliceIsPaused( byval s as Slice ptr ) as bool
DECLARE Function SliceIsClipping( byval s as Slice ptr ) as bool
'slice mutators
DECLARE Sub SliceSetX( byval s as Slice ptr, byval x as integer )
DECLARE Sub SliceSetY( byval s as Slice ptr, byval y as integer )
DECLARE Sub SliceSetWidth( byval s as Slice ptr, byval w as integer )
DECLARE Sub SliceSetHeight( byval s as Slice ptr, byval h as integer )
DECLARE Sub SliceSetVisibility( byval s as Slice ptr, byval b as bool )
DECLARE Sub SliceSetPaused( byval s as Slice ptr, byval b as bool )
DECLARE Sub SliceSetClipping( byval s as Slice ptr, byval b as bool )


DECLARE SUB SliceDebugRemember(sl as Slice Ptr)
DECLARE SUB SliceDebugForget(sl as Slice Ptr)
DECLARE SUB SliceDebugDump(byval noisy as bool = NO)
DECLARE SUB SliceDebugDumpTree(sl as Slice Ptr, byval indent as integer = 0)
DECLARE FUNCTION SliceDebugCheck(sl as Slice Ptr) as integer
DECLARE SUB SliceDebugLinks(sl as Slice Ptr, recurse as bool = NO, prefix as string = "", indent as integer = 0)

'''' SliceType-specific functions

DECLARE Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as slice ptr
DECLARE Sub ChangeRectangleSlice(byval sl as slice ptr,_
                      byval style as integer=-2,_
                      byval bgcol as integer=colInvalid,_
                      byval fgcol as integer=colInvalid,_
                      byval border as RectBorderTypes=borderUndef,_
                      byval translucent as RectTransTypes=transUndef,_
                      byval fuzzfactor as integer=0,_
                      byval raw_box_border as RectBorderTypes=borderUndef)

DECLARE Function NewLineSlice(byval parent as Slice ptr, byref dat as LineSliceData) as Slice ptr

DECLARE Function NewTextSlice(byval parent as Slice ptr, byref dat as TextSliceData) as slice ptr
DECLARE Sub UpdateTextSlice(byval sl as slice ptr)
DECLARE Sub ChangeTextSlice(byval sl as slice ptr,_
                      s as string=CHR(1) & CHR(255),_
                      byval col as integer=colInvalid,_
                      byval outline as integer=-2,_
                      byval wrap as integer=-2,_
                      byval bgcol as integer=colInvalid)
DECLARE Function GetTextSliceString(byval sl as slice ptr) as string
DECLARE Function TextSliceCharPos(sl as Slice ptr, charnum as integer) as XYPair

DECLARE Sub DisposeSpriteSlice(byval sl as slice ptr)
DECLARE Sub DrawSpriteSlice(byval sl as slice ptr, byval p as integer)
DECLARE Sub LoadSpriteSliceImage(byval sl as Slice ptr, warn_if_missing as bool = NO)
DECLARE Function GetSpriteSliceData(byval sl as slice ptr) as SpriteSliceData ptr
DECLARE Sub SetSpriteToAsset(sl as Slice ptr, assetfile as string, warn_if_missing as bool = YES)
DECLARE Sub SetSpriteToFrame(sl as slice ptr, fr as Frame ptr, pal16 as Palette16 ptr = NULL, pal as integer = -2)
DECLARE Sub SpriteSliceUpdate(sl as Slice ptr)
DECLARE Function NewSpriteSlice(byval parent as Slice ptr, byref dat as SpriteSliceData) as slice ptr
DECLARE Sub ChangeSpriteSlice(byval sl as slice ptr,_
                      byval spritetype as SpriteType = sprTypeInvalid,_
                      byval record as integer=-1,_
                      byval pal as integer = -2,_
                      byval frame as integer = -1,_
                      byval fliph as integer = -2,_
                      byval flipv as integer = -2,_
                      byval trans as integer = -2)  ' All arguments default to no change
DECLARE Sub ScaleSpriteSlice(sl as Slice ptr, size as XYPair)
DECLARE Sub DissolveSpriteSlice(byval sl as slice ptr, byval dissolve_type as integer, byval over_ticks as integer=-1, byval start_tick as integer=0, byval backwards as bool=NO, byval auto_animate as bool=YES)
DECLARE Sub CancelSpriteSliceDissolve(sl as Slice ptr)
DECLARE Function SpriteSliceIsDissolving(byval sl as slice ptr, byval only_auto as bool=YES) as bool
DECLARE Function SpriteSliceNumFrames(sl as Slice ptr) as integer

DECLARE Sub DisposeMapSlice(byval sl as slice ptr)
DECLARE Sub DrawMapSlice(byval sl as slice ptr, byval page as integer)
DECLARE Function GetMapSliceData(byval sl as slice ptr) as MapSliceData ptr
DECLARE Function NewMapSlice(byval parent as Slice ptr, byref dat as MapSliceData) as slice ptr
DECLARE Sub ChangeMapSliceTileset (byval sl as slice ptr, byval tileset as TilesetData ptr)
DECLARE Sub ChangeMapSlice (byval sl as slice ptr,_
                   byval tiles as TileMap ptr=cast(TileMap ptr, 1),_
                   byval pass as TileMap ptr=cast(TileMap ptr, 1),_
                   byval transparent as integer=-2,_
                   byval overlay as integer=-1) ' All arguments default to no change (explaining weird tiles default)

DECLARE Function NewGridSlice(byval parent as Slice ptr, byref dat as GridSliceData) as slice ptr
DECLARE Sub ChangeGridSlice(byval sl as slice ptr,_
                      byval rows as integer=0,_
                      byval cols as integer=0,_
                      byval show as integer=-2)

DECLARE Function NewLayoutSlice(byval parent as Slice ptr, byref dat as LayoutSliceData) as slice ptr

DECLARE Sub DisposeEllipseSlice(byval sl as slice ptr)
DECLARE Sub DrawEllipseSlice(byval sl as slice ptr, byval p as integer)
DECLARE Function NewEllipseSlice(byval parent as Slice ptr, byref dat as EllipseSliceData) as slice ptr
DECLARE Sub ChangeEllipseSlice(byval sl as slice ptr,_
                      byval bordercol as integer=colInvalid,_
                      byval fillcol as integer=colInvalid)  ' All arguments default to no change

DECLARE Function NewScrollSlice(byval sl as slice ptr, byref dat as ScrollSliceData) as Slice ptr
DECLARE Sub ChangeScrollSlice(byval sl as slice ptr,_
                      byval style as integer=-1,_
                      byval check_depth as integer=-1)
DECLARE Function GetScrollSliceData(byval sl as slice ptr) as ScrollSliceData ptr

DECLARE Function NewSelectSlice(byval sl as slice ptr, byref dat as SelectSliceData) as Slice ptr
DECLARE Sub ChangeSelectSlice(byval sl as slice ptr,_
                      byval index as integer=-2,_
                      byval override as integer=-2) ' All arguments default to no change
DECLARE Sub SelectSliceNext(byval sl as Slice ptr, byval can_loop as bool=YES)
DECLARE Function GetSelectSliceData(byval sl as slice ptr) as SelectSliceData ptr

DECLARE Function NewPanelSlice(byval parent as Slice ptr, byref dat as PanelSliceData) as slice ptr
DECLARE Sub ChangePanelSlice(byval sl as slice ptr,_
                      byval vertical as integer=-2,_ 'verical is actually bool, use -2 to signal no change
                      byval primary as integer=-1,_
                      byval pixels as integer=-1,_
                      byval percent as double=-1.0,_
                      byval padding as integer=-1)
DECLARE Function GetPanelSliceData(byval sl as slice ptr) as PanelSliceData ptr
DECLARE Sub CalcPanelSupport (byref support as RectType, byval par as Slice ptr, byval index as integer)


EXTERN NumDrawnSlices as integer

End Extern


'NEW SLICE TYPE TEMPLATE
'INSTRUCTIONS: Copy the following block into Slices.bas.
' Then, select the block, and use Find and Replace to switch
' <TYPENAME> with whatever name you need. Then, add the drawing code to
' Draw<TYPENAME>Slice if you need a Draw function.
'
' Also, aside from the obvious places, the following will need to be updated:
' -slice editor, eg slice_edit_detail_refresh and editable_slice_types
' -SlicePossiblyResizable
' -SliceLegalCoverModes
' -AdjustSlicePosToNewParent (likely if there is a custom ChildRefresh function)
' -slice_editor_mouse_over, to define whether clickable
/'
'==START OF <TYPENAME>SLICEDATA
Sub Dispose<TYPENAME>Slice(byval sl as Slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as <TYPENAME>SliceData ptr = sl->SliceData
 delete dat
 sl->SliceData = 0
end sub

Sub Draw<TYPENAME>Slice(byval sl as Slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim dat as <TYPENAME>SliceData ptr = sl->SliceData

 '''DRAWING CODE GOES HERE!
end sub

Function Get<TYPENAME>SliceData(byval sl as Slice ptr) as <TYPENAME>SliceData ptr
 if sl = 0 then return 0
 return sl->SliceData
End Function

Sub Clone<TYPENAME>Slice(byval sl as Slice ptr, byval cl as Slice ptr)
 if sl = 0 or cl = 0 then debug "Clone<TYPENAME>Slice null ptr": exit sub
 dim dat as <TYPENAME>SliceData Ptr
 dat = sl->SliceData
 dim clonedat as <TYPENAME>SliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  '.s       = dat->s
 end with
End Sub

Sub Save<TYPENAME>Slice(byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "Save<TYPENAME>Slice null ptr": exit sub
 dim dat as <TYPENAME>SliceData Ptr
 dat = sl->SliceData
 'SaveProp node, "cols", dat->cols
End Sub

Sub Load<TYPENAME>Slice (byval sl as Slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "Load<TYPENAME>Slice null ptr": exit sub
 dim dat as <TYPENAME>SliceData Ptr
 dat = sl->SliceData
 'dat->cols = LoadProp(node, "cols", 1)
End Sub

Function New<TYPENAME>Slice(byval parent as Slice ptr, byref dat as <TYPENAME>SliceData) as Slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then return 0

 dim d as <TYPENAME>SliceData ptr = new <TYPENAME>SliceData
 *d = dat

 'NOTE: if you want to initialise any data members to non-zero values you
 'can do it either here or in the TYPE definition, but must make sure
 'those members are saved and loaded correctly, eg using SavePropAlways

 ret->SliceType = sl<TYPENAME>
 ret->SliceData = d
 ret->Draw = @Draw<TYPENAME>Slice
 ret->Dispose = @Dispose<TYPENAME>Slice
 ret->Clone = @Clone<TYPENAME>Slice
 ret->Save = @Save<TYPENAME>Slice
 ret->Load = @Load<TYPENAME>Slice
 'ret->ChildRefresh = @DefaultChildRefresh
 'ret->ChildDraw = @DefaultChildDraw

 return ret
end function
'==END OF <TYPENAME>SLICEDATA
'/


#endif
