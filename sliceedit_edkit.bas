'OHRRPGCE CUSTOM - Slice properties list
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'This file is #included at the end of sliceedit.bas, in Custom only, because that file
'has a bunch of local shared arrays and functions that this one depends upon.

'This is an unfinished conversion of most of the sliceedit slice detail menu (not any
'submenus) slice_edit_detail_refresh function to EditorKit. Editing most properties is
'functional but some of them need special handling, which is entirely missing in this
'version.
'In future it may replace the original. But for now its main purpose is to provide a list
'of slice properties for the animation editor. What it adds to the detail menu is that
'each property also specifies the node name used to serialize it in .slices and in
'animations.

#include "editorkit.bi"


DEFINE_VECTOR_OF_CLASS(SlicePropInfo, SlicePropInfo)

DESTRUCTOR SlicePropInfo()
  IF value_node THEN
    FreeNode value_node
    value_node = NULL
  END IF
END DESTRUCTOR

SUB SlicePropertiesEditor.caption_slice_color(ifzero as string = "0")
  set_caption slice_color_caption(value, ifzero)
END SUB

' prop is the Reload node name used (for SaveProp) to save this slice property to .slices files.
' It's very often equal to the help key.
' animkey is used in animations, if it differs from prop
SUB SlicePropertiesEditor.propkey(prop as zstring ptr, helpkey as zstring ptr = NULL, animkey as zstring ptr = NULL)
  set_helpkey "sliceedit_" & *IIF(helpkey, helpkey, prop)
  IF animkey = NULL THEN animkey = prop
  cur_animkey = *animkey

  'Insert/F2 to convert to dynamic property
  IF process ANDALSO selected THEN
    IF keyval(scInsert) > 1 ORELSE keyval(scF2) > 1 THEN
      DIM attrname as string
      prompt_for_string(attrname, !"  Create dynamic property...\nName of variant for source data?", 100)
      attrname = sanitize_script_identifier(attrname, NO)
      IF LEN(attrname) THEN
        AddSliceDynamicProp sl, *prop, attrname
        'FIXME: this doesn't actually force the necessary update
        state.need_update = YES
      END IF
    END IF
  END IF

  'Show dynamic properties in the caption
  IF refresh ANDALSO sl->DynamicProps THEN
    FOR idx as integer = 0 TO v_len(sl->DynamicProps) - 1
      WITH sl->DynamicProps[idx]
        IF .propname = *prop THEN
          'Maybe should have an add_note method instead
          set_caption "{" & .attrname & "} = " & form_default_caption()
        END IF
      END WITH
    NEXT
  END IF

  'If a property is dynamic, attempting to edit it... removes the dynamic.
  'Or we could edit value of the attribute, or the name of the attribute to use,
  'or ask the user what to do.
  IF edited ANDALSO sl->DynamicProps THEN
    DIM idx as integer = FindSliceDynamicProp(sl, *prop)
    IF idx > -1 THEN
      v_delete_slice sl->DynamicProps, idx, idx + 1
      state.need_update = YES
    END IF
  END IF
END SUB

SUB SlicePropertiesEditor.set_default(value as integer)
  'TODO
  'Don't really need the default in animedit: it defaults to current value
END SUB

CONSTRUCTOR SlicePropertiesEditor(sl as Slice ptr, ses_draw_root as Slice ptr = NULL)
  this.sl = sl
  this.ses_draw_root = IIF(ses_draw_root, ses_draw_root, sl)
  REDIM slicelookup(10) as string
  load_string_list slicelookup(), workingdir & SLASH & "slicelookup.txt"
  IF UBOUND(slicelookup) < 1 THEN
    REDIM slicelookup(1) as string
  END IF
  menuopts.edged = YES
  menuopts.drawbg = YES
  menuopts.highlight_selection = YES
END CONSTRUCTOR

'Collect a list of properties and section headers into_vector, used by the animation editor
SUB SlicePropertiesEditor.gather_properties(byref into_vector as SlicePropInfo vector)
  gather_items = @into_vector
  ' Call define_items
  'update()
  run_phase(Phases.refreshing)
  gather_items = NULL
END SUB

'add_to_title: Add the section name (e.g. "Padding") to the display names of contained props (e.g. "Left")
SUB SlicePropertiesEditor.section(title as zstring ptr, add_to_display as bool = NO)
  base.section title

  IF gather_items THEN
    IF add_to_display THEN
      display_prefix = *title & " "
    ELSE
      display_prefix = ""
    END IF
    WITH *v_expand(*gather_items)
      .infotype = SlicePropInfoType.section
      '.sltype = cur_slicetype
      .display = *title
    END WITH
  END IF
END SUB

SUB SlicePropertiesEditor.finish_defitem()
  IF started_item = NO THEN EXIT SUB
  DIM custom_caption as bool = LEN(cur_item.caption) > 0
  base.finish_defitem()

  IF gather_items THEN
    IF cur_item.title = prev_menu_text THEN  'Previous Menu
      EXIT SUB
    END IF
    IF cur_animkey = "" THEN
      'Menu items without propkey are not animatable properties
      EXIT SUB
    END IF

    WITH *v_expand(*gather_items)
      .infotype = SlicePropInfoType.prop
      .sltype = cur_slicetype
      SELECT CASE cur_item.dtype
        CASE dtypeBool
          .dtype = pdtypeBool
        CASE dtypeInt
          .dtype = pdtypeInt
          .range_min = cur_item.range_min
          .range_max = cur_item.range_max
        CASE dtypeFloat
          .dtype = pdtypeFloat
          .range_min_float = cur_item.range_min_float
          .range_max_float = cur_item.range_max_float
        CASE dtypeStr
          .dtype = pdtypeStr
      END SELECT
      .value_node = CreateNode(get_anim_doc, "value")
      SELECT CASE cur_item.writer
        CASE writerBoolean
          SetContentBool .value_node, *cur_item.byte_ptr
        CASE writerUByte
          SetContent .value_node, *cur_item.ubyte_ptr
        CASE writerInt  'Includes bools
          SetContent .value_node, *cur_item.int_ptr
        CASE writerSingle
          SetContent .value_node, *cur_item.single_ptr
        CASE writerDouble
          SetContent .value_node, *cur_item.double_ptr
        CASE writerStr
          SetContent .value_node, *cur_item.str_ptr
        CASE ELSE
          debug "SlicePropertiesEditor: Unsupported writer " & cur_item.writer & " for " & cur_item.title
      END SELECT
      .propname = intern_string(cur_animkey)
      .helpkey = cur_item.helpkey
      .display = rtrim(cur_item.title, ":")
      IF LEN(display_prefix) THEN
        .display = display_prefix & untitlecase(.display)
      END IF
      .caption = cur_item.caption
      .custom_caption = custom_caption
      '? "finishdef", .display, GetString(.value_node)
    END WITH
  END IF

  cur_animkey = ""
END SUB

SUB SlicePropertiesEditor.define_items()

  'Initially, listing generic slice properties
  cur_slicetype = slNone

  'TODO: could use edit_zint in a few places

  WITH *sl

    section "Position/Size"
    'IF .FillHoriz = NO THEN
    defint "X:", .X, -9999, 9999
    propkey "x", "pos"
    'set_tooltip "X offset from parent align point"
    'IF .FillVert = NO THEN
    defint "Y:", .Y, -9999, 9999   'slgrPICKXY
    propkey "y", "pos"

    defint "Screen X:", .ScreenX, -9999, 9999
    propkey "screen_x", "screen_pos"   'Not saved
    defint "Screen Y:", .ScreenY, -9999, 9999
    propkey "screen_y", "screen_pos"   'Not saved

    DIM minsize as integer = IIF(.SliceType = slLine, -9999, 0)
    defint "Width:", .Width, minsize, 9999   'slgrPICKWH
    propkey "w", "size"
    defint "Height:", .Height, minsize, 9999   'slgrPICKWH
    propkey "h", "size"
    IF privileged THEN
      defint "Cover Children:", .CoverChildren, 0, 3   'ubyte
      captions CoverModeCaptions()
      propkey "cover"
    END IF
    defbool "Fill parent:", .Fill
    propkey "fill"
    IF .Fill THEN
      defint "Fill type:", .FillMode, 0, 2   'ubyte
      captions FillModeCaptions()
      propkey "fillmode", "fill"
    END IF

    IF .SliceType <> slContainer THEN
      section SliceTypeName(sl) & " settings"
      cur_slicetype = .SliceType
    END IF

    SELECT CASE .SliceType
      CASE slRectangle
        DIM dat as RectangleSliceData Ptr
        dat = .SliceData
        defint "Style:", dat->style, -1, 14   'slgrUPDATERECTSTYLE
        caption_default_or_int -1, "None (custom)"
        propkey "style", "rect_style"
        defint "Background color:", dat->bgcol, LowColorCode(), 255  'slgrUPDATERECTCUSTOMSTYLE OR slgrPICKCOL
        caption_slice_color
        propkey "bg", "rect_bg", "bgcol"
        defint "Border line color:", dat->fgcol, LowColorCode(), 255  'slgrUPDATERECTCUSTOMSTYLE OR slgrPICKCOL
        caption_slice_color
        propkey "fg", "rect_fg", "col"
        'TODO: Line and None should be border types, not appear under Box Style
        'defbool "Border type", dat->use_raw_box_border   'slgrUPDATERECTCUSTOMSTYLE
        'captions_yesno "Box Style/Line/None", "Spriteset"
        'propkey "", "rect_use_raw_box_border"  'Not saved as-is
        IF dat->use_raw_box_border THEN
          '.slice format also uses "raw_box_border" = -1 to encode dat->use_raw_box_border = NO
          defint "Border raw spriteset:", dat->raw_box_border, 0, gen(genMaxBoxBorder)   'slgrBROWSEBOXBORDER
          propkey "raw_box_border", "rect_raw_box_border"
        ELSE
          defint " Border style:", dat->border, -2, 14    'Enum
          captions_or_int BorderCaptions()
          set_default borderLine
          propkey "border", "rect_border"
        END IF
        defint "Translucency:", dat->translucent, 0, transLAST
        captions TransCaptions()
        propkey "trans", "rect_trans", "translucent"
        IF dat->translucent = transFuzzy THEN
          defint "Fuzziness%:", dat->fuzzfactor, 0, 99
          propkey "fuzzfactor", "rect_fuzzfact"
          defint "Fuzzy zoom:", dat->fuzz_zoom, 1, 10000  'No need for upper limit
          propkey "fz_zoom", "rect_fuzzzoom"
          defbool "Stationary pattern:", dat->fuzz_stationary
          propkey "fz_stationary", "rect_fuzz_stationary"
        ELSEIF dat->translucent = transBlend THEN
          defint "Opacity%:", dat->fuzzfactor, 0, 99
          propkey "fuzzfactor", "rect_transfact"
        END IF

      CASE slMap
        DIM dat as MapSliceData ptr = .SliceData
        add_blend_items dat->drawopts

      CASE slLine
        DIM dat as LineSliceData ptr = .SliceData
        defint "Color:", dat->col, LowColorCode(), 255   'slgrPICKCOL
        caption_slice_color
        propkey "col", "line_col"

      CASE slText
        DIM dat as TextSliceData Ptr = .SliceData
        defstr "Text:", dat->s, 128000  'Arbitrary limit
        propkey "s", "text_text"
        defint "Color:", dat->col, LowColorCode(), 255   'slgrPICKCOL
        caption_slice_color "Default"
        propkey "col", "text_color"
        IF dat->outline = NO THEN
          defint "Background Color:", dat->bgcol, LowColorCode(), 255   'slgrPICKCOL
          caption_slice_color "Transparent"
          propkey "bgcol", "text_bg"
        END IF
        defbool "Outline:", dat->outline
        propkey "outline", "text_outline"
        defbool "Wrap:", dat->wrap
        propkey "wrap", "text_wrap"

      CASE slSprite
        DIM dat as SpriteSliceData Ptr = .SliceData
        DIM byref sizeinfo as SpriteSize = sprite_sizes(dat->spritetype)
        'DIM mintype as SpriteType = IIF(ses.collection_group_number = SL_COLLECT_EDITOR, sprTypeFrame, 0)
        DIM mintype as SpriteType = 0
        defint "Sprite type:", dat->spritetype, mintype, sprTypeLastPickable   'slgrUPDATESPRITE
        set_caption sprite_sizes(dat->spritetype).name
        propkey "sprtype", "sprite_type"
        defint "Spriteset:", dat->record, 0, sizeinfo.lastrec   'slgrUPDATESPRITE OR slgrBROWSESPRITEID
        propkey "rec", "sprite_rec"
        IF dat->paletted THEN
          defint "Palette:", dat->pal, -1, gen(genMaxPal)   'slgrUPDATESPRITE
          caption_default_or_int -1, "Default"
          propkey "pal", "sprite_pal"
        END IF
        DIM nframes as integer = dat->get_num_frames(sl)
        IF nframes > 1 THEN
          defint "Frame:", dat->frame, 0, nframes - 1
          propkey "frame", "sprite_frame"
          DIM frameid as integer = dat->get_frameid(sl)  'Make sure not to take address of a stack temporary
          defint "Frame ID:", frameid, 0, 9999
          IF edited THEN dat->set_frameid(sl, value)
          propkey "frameid", "sprite_frameid"
        END IF
        defbool "Transparent:", dat->trans   'slgrUPDATESPRITE
        propkey "trans", "sprite_trans"

        add_blend_items dat->drawopts

        defbool "Flip horiz.:", dat->flipHoriz,   'slgrUPDATESPRITE
        propkey "fliph", "sprite_flip"
        defbool "Flip vert.:", dat->flipVert,   'slgrUPDATESPRITE
        propkey "flipv", "sprite_flip"
        defbool "Dissolving:", dat->dissolving
        propkey "dissolving", "sprite_dissolve"
        IF dat->dissolving THEN
          defint "Type:", dat->d_type, 0, dissolveTypeMax
          set_caption dissolve_type_caption(dat->d_type)
          propkey "d_type", "sprite_d_type"
          defint "Length ticks:", dat->d_time, -1, 999999
          caption_default_or_int -1, "Default"  '"Default (W+H)/10=" & (.Width + .Height) / 10)
          propkey "d_time", "sprite_d_time"
          defint "Current tick:", dat->d_tick, 0, 999999
          propkey "d_tick", "sprite_d_tick"
          defbool "Backwards:", dat->d_back
          propkey "d_back", "sprite_d_back"
          'FIXME: dissolve is advanced in DrawSpriteSlice, which is wrong, causing the slice to dissolve
          'in the slice editor, making this setting useless.
          'TODO: need to set d_time to 0 to reset the animation if it's already finished, when this is changed to YES
          defbool "Animate:", dat->d_auto
          propkey "d_auto", "sprite_d_audo"
        END IF

      CASE slGrid
        DIM dat as GridSliceData Ptr
        dat = .SliceData
        defint "Rows:", dat->rows, 0, 99 'FIXME: upper limit of 99 is totally arbitrary
        propkey "rows", "grid_rows"
        defint "Columns:", dat->cols, 0, 99 'FIXME: upper limit of 99 is totally arbitrary
        propkey "cols", "grid_cols"
        defbool "Show Grid:", dat->show
        propkey "show", "grid_show"

      CASE slEllipse
        DIM dat as EllipseSliceData Ptr
        dat = .SliceData
        defint "Border Color:", dat->bordercol, LowColorCode(), 255   'slgrPICKCOL
        caption_slice_color "Transparent"
        propkey "bordercol", , "col"
        defint "Fill Color:", dat->fillcol, LowColorCode(), 255   'slgrPICKCOL
        caption_slice_color "Transparent"
        propkey "fillcol", , "bgcol"

      CASE slScroll
        DIM dat as ScrollSliceData Ptr
        dat = .SliceData
        defint "Style:", dat->style, 0, 14
        propkey "style", "scroll_style"
        defint "Check Depth:", dat->check_depth, 0, 99 'FIXME: upper limit of 99 is totally arbitrary
        caption_default_or_int 0, "No limit"
        propkey "check_depth", "scroll_check_depth"

      CASE slSelect
        DIM dat as SelectSliceData Ptr
        dat = .SliceData
        defint "Selected Child:", dat->index, 0, 9999999   'slgrEDITSWITCHINDEX 'FIXME: this is an arbitrary upper limit
        propkey "index", "select_index"

      CASE slPanel
        DIM dat as PanelSliceData Ptr
        dat = .SliceData

        defbool "Orientation:", dat->vertical
        captions_yesno "Vertical", "Horizontal"
        propkey "vertical", "panel_vertical"
        defint "Primary child:", dat->primary, 0, 1
        propkey "primary", "panel_primary"
        ' " " & IIF(dat->vertical, "Height", "Width") & "" & format_percent(dat->percent) & " of panel"
        defitem "Percent size:"
        edit_float dat->percent, 0.0, 1.0   'dat->percent is a fraction, NOT a percentage!
        propkey "percent", "panel_percent"
        defint "Pixels size:", dat->pixels, 0, 9999 'FIXME: upper limit of 9999 is totally arbitrary
        propkey "pixels", "panel_pixels"
        defint "Padding between children:", dat->padding, 0, 9999 'FIXME: upper limit of 9999 is totally arbitrary
        propkey "padding", "panel_padding"

      CASE slLayout
        DIM dat as LayoutSliceData Ptr
        dat = .SliceData

        defint "Row grow direction:", dat->primary_dir, 0, 3
        captions DirectionCaptions()
        propkey "dir0", "layout_primary_dir"
        defint "Row-stacking direction:", dat->secondary_dir, 0, 3   'slgrLAYOUT2NDDIR
        captions DirectionCaptions()
        propkey "dir1", "layout_secondary_dir"
        defbool "Justified:", dat->justified
        propkey "justified", "layout_justified"
        IF dat->justified THEN
          defbool "Justify last row:", dat->last_row_justified
          propkey "last_row_justified", "layout_last_row_justified"
        END IF
        defint "Row alignment:", dat->row_alignment, 0, 2   'ubyte
        set_caption dir_align_caption(dat->primary_dir, dat->row_alignment)
        propkey "row_align", "layout_row_alignment"
        defint "Within-row alignment:", dat->cell_alignment, 0, 2   'ubyte
        set_caption dir_align_caption(dat->secondary_dir, dat->cell_alignment)
        propkey "cell_align", "layout_cell_alignment"
        'IF dat->justified THEN ... " Minimum within-row padding" & dat->primary_padding
        defint "Within-row padding:", dat->primary_padding, -9999, 9999
        propkey "padding0", "layout_primary_padding", "padding"
        defint "Between-row padding:", dat->secondary_padding, -9999, 9999
        propkey "padding1", "layout_secondary_padding"
        defint "Min row thickness:", dat->min_row_breadth, 0, 9999
        propkey "min_breadth", "layout_min_row_breadth"
        defbool "Skip hidden:", dat->skip_hidden
        propkey "skip_hidden", "layout_skip_hidden"

    END SELECT

    cur_slicetype = slNone

    'Slice type, Script handle, Context info, Protected omitted,
    'so Lookup by itself, and didn't want it at the top in the anim editor...
    section "Lookup"

    'IF lookup_code_forbidden(ses.specialcodes(), .Lookup) = NO THEN
    IF .Lookup >= 0 THEN
      defint "Lookup code:", .Lookup, 0, INT_MAX   'slgrPICKLOOKUP
      propkey "lookup"
      set_caption slice_lookup_code_caption(.Lookup, slicelookup())
    END IF

    section "Visibility"
    defbool "Visible:", .Visible
    propkey "vis"
    defbool "Clip Children:", .Clip
    propkey "clip"

    section "Alignment"
    DIM as bool fillvert = .FillVert(), fillhoriz = .FillHoriz()
    IF fillhoriz = NO ORELSE fillvert = NO THEN
      IF .FillHoriz() = NO THEN
        defint "Align horiz.:", .AlignHoriz, alignLeft, alignRight   'ubyte
        captions HorizCaptions()
        propkey "alignh", "align"
      END IF
      IF .FillVert() = NO THEN
        defint "Align vert.:", .AlignVert, alignLeft, alignRight   'ubyte
        captions VertCaptions()
        propkey "alignv", "align"
      END IF
      IF .FillHoriz() = NO THEN
        defint "Anchor horiz.:", .AnchorHoriz, alignLeft, alignRight   'ubyte
        captions HorizCaptions()
        propkey "anchorh", "anchor"
      END IF
      IF .FillVert() = NO THEN
        defint "Anchor vert.:", .AnchorVert, alignLeft, alignRight   'ubyte
        captions VertCaptions()
        propkey "anchorv", "anchor"
      END IF
      DIM clamping as bool = NO
      IF .FillHoriz() = NO THEN
        defint "Clamp horiz.:", .ClampHoriz, alignLeft, alignBoth   'ubyte
        set_caption clamp_caption(.ClampHoriz, NO)
        set_default alignNone
        propkey "clamph", "clamp"
        clamping OR= (.ClampHoriz <> alignNone)
      END IF
      IF .FillVert() = NO THEN
        defint "Clamp vert.:", .ClampVert, alignLeft, alignBoth   'ubyte
        set_caption clamp_caption(.ClampVert, YES)
        set_default alignNone
        propkey "clampv", "clamp"
        clamping OR= (.ClampVert <> alignNone)
      END IF
      IF clamping THEN
        defbool "Clamp to:", .ClampToScreen
        captions_yesno "Screen", "Parent"
        propkey "clamptoscreen", "clamp"
      END IF
    END IF

    section "Padding", YES
    defint "Top:", .PaddingTop, -9999, 9999
    propkey "padt", "padding"
    defint "Right:", .PaddingRight, -9999, 9999
    propkey "padr",  "padding"
    defint "Bottom:", .PaddingBottom, -9999, 9999
    propkey "padb",  "padding"
    defint "Left:", .PaddingLeft, -9999, 9999
    propkey "padl",  "padding"

    section "Sorting"
    defint "Auto-sort children:", .AutoSort, 0, slAutoSortLAST   'ubyte
    captions AutoSortCaptions()
    propkey "autosort"
    'DIM sortNA as string
    'IF .Parent = NULL ORELSE .Parent->AutoSort <> slAutoSortCustom THEN caption " (N/A)"
    defint "Custom sort order:", .Sorter, INT_MIN, INT_MAX
    propkey "sort", "sortorder"

    section "Movement"
    defbool "Paused:", .Paused
    propkey "paused"
    defint "Velocity X ticks:", .VelTicks.X, -1, 999999   'slgrVELOCITY
    caption_default_or_int -1, "forever"
    propkey "vtickx", "velocity_ticks"
    'IF .VelTicks.X THEN
    defint "Velocity X:", .Velocity.X, -999999, 999999
    propkey "vx", "velocity"
    defint "Velocity Y ticks:", .VelTicks.Y, -1, 999999   'slgrVELOCITY
    caption_default_or_int -1, "forever"
    propkey "vticky", "velocity_ticks"
    'IF .VelTicks.Y THEN
    defint "Velocity Y:", .Velocity.Y, -999999, 999999
    propkey "vy", "velocity"
    defint "Target ticks:", .TargTicks, 0, 999999   'slgrTARGET
    propkey "ttick", "target_ticks"
    'IF .TargTicks > 0 THEN
    defint "Target X:", .Targ.X, -999999, 999999
    propkey "tx", "target"
    defint "Target Y:", .Targ.Y, -999999, 999999
    propkey "ty", "target"

    section "Attributes"
    IF .Context THEN
      'Each existing attribute. These don't have propkeys. Animations will have
      'separate opcodes to modify attributes.
      FOR idx as integer = 0 TO v_len(.Context->attributes) - 1
        WITH .Context->attributes[idx]
          defitem .name & ":"
          SELECT CASE .dtype
            CASE attyBool
              edit_bool .int_value
            CASE attyInt
              edit_int .int_value, INT_MIN, INT_MAX
            CASE attyStr
              edit_str .str_value
          END SELECT
          set_helpkey "sliceedit_attribute"
          IF delete_action THEN
            RemoveAttribute sl, .name
            state.need_update = YES
          END IF
        END WITH
      NEXT
    END IF
    IF defitem_act("Add new...") THEN
      state.need_update OR= slice_editor_add_attribute_menu(sl)
    END IF
    set_helpkey "sliceedit_add_attribute"

    'Animation, Extra Data, Metadata (except screen pos) omitted

  END WITH

END SUB

SUB SlicePropertiesEditor.add_blend_items(byref drawopts as DrawOptions)
  WITH drawopts
    defbool "Blending:", .with_blending
    propkey "blending"

    IF .with_blending ORELSE gather_items <> NULL THEN
      defitem "Opacity:"
      edit_float .opacity, 0.0, 1.0, 3
      propkey "opacity"
      defint "Blend mode:", .blend_mode, 0, blendModeLAST
      captions BlendModeCaptions()
      propkey "blend_mode"

      IF privileged THEN
        defint "Modulate red:", .argbModifier.R, 0, 255   'ubyte
        propkey "mod_r","modulate"   'Not saved
        defint "Modulate green:", .argbModifier.G, 0, 255  'ubyte
        propkey "mod_g", "modulate"   'Not saved
        defint "Modulate blue:", .argbModifier.B, 0, 255   'ubyte
        propkey "mod_b", "modulate"   'Not saved
      END IF
    END IF
  END WITH
END SUB

SUB SlicePropertiesEditor.draw_underlays()
  draw_background vpages(vpage), bgChequer
  RefreshSliceScreenPos sl  'Invisible slices won't otherwise be updated by DrawSlice
  DrawSlice ses_draw_root, vpage
END SUB

SUB edkit_slice_detail_menu (sl as Slice ptr, ses_draw_root as Slice ptr)
  VAR menu = SlicePropertiesEditor(sl, ses_draw_root)
  menu.run()
END SUB
