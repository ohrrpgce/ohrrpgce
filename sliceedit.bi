'OHRRPGCE - sliceedit.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF SLICEEDIT_BI
#DEFINE SLICEEDIT_BI

'Public functions
DECLARE SUB slice_editor OVERLOAD (byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", privileged as bool = NO)
DECLARE SUB slice_editor OVERLOAD (byref edslice as Slice Ptr, byval group as integer = SL_COLLECT_USERDEFINED, filename as string = "", recursive as bool = NO, privileged as bool = NO, initial_slice as Slice ptr = NULL)

DECLARE FUNCTION slice_collection_has_changed(sl as Slice ptr, filename as string) as bool

DECLARE FUNCTION align_caption(align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_string(anchor as AlignType, align as AlignType, vertical as bool) as string
DECLARE FUNCTION anchor_and_align_grabber(byref anchor as AlignType, byref align as AlignType) as bool

DECLARE FUNCTION slice_color_caption(byval n as integer, ifzero as string="0") as string

DECLARE SUB extra_data_editor(byref extra as integer vector)

'Globals
EXTERN HorizCaptions(3) as string
EXTERN VertCaptions(3) as string

#ENDIF
