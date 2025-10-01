'OHRRPGCE - sliceedit.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF SLICEEDIT_BI
#DEFINE SLICEEDIT_BI

'Public functions
DECLARE SUB slice_editor OVERLOAD (byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", privileged as bool = NO)
DECLARE SUB slice_editor OVERLOAD (byref edslice as Slice Ptr, byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", recursive as bool = NO, privileged as bool = NO, initial_slice as Slice ptr = NULL)

DECLARE FUNCTION slice_collection_picker(initial_id as integer = 0) as integer

DECLARE FUNCTION slice_collection_has_changed(sl as Slice ptr, filename as string) as bool

DECLARE FUNCTION align_caption(align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_string(anchor as AlignType, align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_grabber(byref anchor as AlignType, byref align as AlignType) as bool

DECLARE FUNCTION slice_color_caption(byval n as integer, ifzero as string="0") as string

DECLARE SUB slice_edit_updates (sl as Slice ptr, dataptr as any ptr)

DECLARE SUB extra_data_editor(byref extra as integer vector)

DECLARE SUB slice_editor_delete_clipboard()

'Globals
EXTERN HorizCaptions(3) as string
EXTERN VertCaptions(3) as string


#IFDEF IS_CUSTOM

#include "reload.bi"
#include "editorkit.bi"

ENUM SlicePropInfoType EXPLICIT
  section     'This is a section header
  prop        'This is an actual slice property
END ENUM

' This is a copy of enum EditorKitDataType
ENUM PropDataType
  pdtypeNone
  pdtypeBool
  pdtypeInt
  pdtypeStr
  pdtypeFloat
END ENUM

' Store information about a slice property gathered from SlicePropertiesEditor.
' Used by the animation editor only.
' This has a lot of overlap with EditorKitItem.
TYPE SlicePropInfo
  infotype as SlicePropInfoType
  sltype as SliceTypes         'slNone if shared by all slice types
  propname as zstring ptr
  dtype as PropDataType
  range_min as integer
  range_max as integer
  range_min_float as double
  range_max_float as double
  value_node as Reload.NodePtr 'The current value, as an orphaned anim_doc node named "value"
  display as string            'Name of the property, e.g. "Screen X"
  caption as string            'Caption for the slice's current value of this property
  custom_caption as bool       'The caption isn't just the value
  helpkey as string
  'description as string

  DECLARE DESTRUCTOR()
END TYPE

DECLARE_VECTOR_OF_TYPE(SlicePropInfo, SlicePropInfo)

TYPE SlicePropertiesEditor EXTENDS EditorKit
  sl as Slice ptr
  ses_draw_root as Slice ptr
  privileged as bool
  slicelookup(any) as string

  'For gather_properties
  cur_animkey as string
  cur_slicetype as SliceTypes
  display_prefix as string
  gather_items as SlicePropInfo vector ptr

  DECLARE CONSTRUCTOR(sl as Slice ptr, ses_draw_root as Slice ptr = NULL)
  DECLARE SUB finish_defitem() OVERRIDE
  DECLARE SUB section(title as zstring ptr, add_to_display as bool = NO)
  DECLARE SUB define_items()
  DECLARE SUB add_blend_items(byref drawopts as DrawOptions)
  DECLARE SUB propkey(prop as zstring ptr, helpkey as zstring ptr = NULL, animkey as zstring ptr = NULL)
  DECLARE SUB set_default(value as integer)
  DECLARE SUB caption_slice_color(ifzero as string = "0")

  DECLARE SUB before_each_tick()
  DECLARE SUB draw_underlays()

  DECLARE SUB gather_properties(byref into_vector as SlicePropInfo vector)
END TYPE

#ENDIF  'IS_CUSTOM

#ENDIF
