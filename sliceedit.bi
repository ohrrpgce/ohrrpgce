'OHRRPGCE - sliceedit.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#IFNDEF SLICEEDIT_BI
#DEFINE SLICEEDIT_BI

CONST SL_COLLECT_EDITOR = -1      'Collections used by Custom itself, not stored in .rpgs
CONST SL_COLLECT_USERDEFINED = 0
CONST SL_COLLECT_ITEMSCREEN = 1
CONST SL_COLLECT_ITEMPLANK = 2
CONST SL_COLLECT_STATUSSCREEN = 3
CONST SL_COLLECT_STATUSSTATPLANK = 4
CONST SL_COLLECT_SPELLSCREEN = 5
CONST SL_COLLECT_SPELLLISTPLANK = 6  'Unused!
CONST SL_COLLECT_SPELLPLANK = 7      'Unused!
' The following do not yet exist
'CONST SL_COLLECT_EQUIPSCREEN = 8
'CONST SL_COLLECT_EQUIPSLOTPLANK = 9
'CONST SL_COLLECT_EQUIPABLEPLANK = 10
'CONST SL_COLLECT_ORDERSCREEN = 11
'CONST SL_COLLECT_ORDERACTIVEPLANK = 12
'CONST SL_COLLECT_ORDERRESERVEPLANK = 13
'CONST SL_COLLECT_HEROPICKSCREEN = 14
'CONST SL_COLLECT_HEROPICKACTIVEPLANK = 15
'CONST SL_COLLECT_HEROPICKRESERVEPLANK = 16
'CONST SL_COLLECT_SAVESCREEN = 17
'CONST SL_COLLECT_SAVEPLANK = 18
'CONST SL_COLLECT_LOADSCREEN = 19
'CONST SL_COLLECT_LOADPLANK = 20
CONST SL_COLLECT_VIRTUALKEYBOARDSCREEN = 21


'Public functions
DECLARE SUB slice_editor OVERLOAD (byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", privileged as bool = NO)
DECLARE SUB slice_editor OVERLOAD (byref edslice as Slice Ptr, byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", recursive as bool = NO, privileged as bool = NO)
DECLARE SUB load_slice_collection (byval sl as Slice Ptr, byval collection_kind as integer, byval collection_num as integer=0)

DECLARE FUNCTION slice_collection_has_changed(sl as Slice ptr, filename as string) as bool

DECLARE FUNCTION align_caption(align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_string(anchor as AlignType, align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_grabber(byref anchor as AlignType, byref align as AlignType) as bool

DECLARE FUNCTION slice_color_caption(byval n as integer, ifzero as string="0") as string

'Globals
EXTERN HorizCaptions(2) as string
EXTERN VertCaptions(2) as string

#ENDIF
