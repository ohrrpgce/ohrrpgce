'OHRRPGCE - Animation system
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

' This is a list of property names recognised by set_slice_property for animation
' data. They are almost always equal to the RELOAD node names used in .slices,
' but also includes pseudo properties like "screen_x".
' Names which aren't recognised by set_slice_property could be added to this list if needed.

dim shared slice_property_names(...) as zstring ptr = { _
  @"x",  _
  @"y",  _
  @"screen_x",  _
  @"screen_y",  _
  @"w",  _
  @"h",  _
  @"cover",  _
  @"fill",  _
  @"fillmode",  _
  @"vis",  _
  @"paused",  _
  @"clip",  _
  @"alignh",  _
  @"alignv",  _
  @"anchorh",  _
  @"anchorv",  _
  @"clamph",  _
  @"clampv",  _
  @"clamptoscreen",  _
  @"padt",  _
  @"padr",  _
  @"padb",  _
  @"padl",  _
  @"autosort",  _
  @"sort",  _
  @"lookup",  _
  @"vtickx",  _
  @"vx",  _
  @"vticky",  _
  @"vy",  _
  @"ttick",  _
  @"tx",  _
  @"ty",  _
  /' Add more generic properties here, to keep them contiguous for SELECT CASE AS CONST '/ _
  @"LAST GENERIC PROP",  _
  @"frame",  _
  @"frameid",  _
  @"sprtype",  _
  @"rec",  _
  @"pal",  _
  @"trans",  _
  @"fliph",  _
  @"flipv",  _
  @"dissolving",  _
  @"d_type",  _
  @"d_time",  _
  @"d_tick",  _
  @"d_back",  _
  @"style",  _
  @"border",  _
  @"raw_box_border",  _
  @"translucent",  _
  @"fuzzfactor",  _
  @"fz_zoom",  _
  @"fz_stationary",  _
  @"col",  _
  @"s",  _
  @"bgcol",  _
  @"outline",  _
  @"wrap",  _
  @"rows",  _
  @"cols",  _
  @"show",  _
  @"check_depth",  _
  @"index",  _
  @"vertical",  _
  @"primary",  _
  @"percent",  _
  @"pixels",  _
  @"padding",  _
  @"dir0",  _
  @"dir1",  _
  @"justified",  _
  @"last_row_justified",  _
  @"skip_hidden",  _
  @"row_align",  _
  @"cell_align",  _
  @"padding1",  _
  @"min_breadth",  _
  @"blending",  _
  @"opacity",  _
  @"blend_mode",  _
  @"mod_r",  _
  @"mod_g",  _
  @"mod_b"  _
}

' Index in slice_property_names
type SlicePropertyNumber as integer
enum 'SlicePropertyNumber
  _INVALID_PROP = -1
  _x
  _y
  _screen_x
  _screen_y
  _w
  _h
  _cover
  _fill
  _fillmode
  _vis
  _paused
  _clip
  _alignh
  _alignv
  _anchorh
  _anchorv
  _clamph
  _clampv
  _clamptoscreen
  _padt
  _padr
  _padb
  _padl
  _autosort
  _sort
  _lookup
  _vtickx
  _vx
  _vticky
  _vy
  _ttick
  _tx
  _ty
  /' Add more generic properties here, to keep them contiguous for SELECT CASE AS CONST '/
  _LAST_GENERIC_PROP
  _frame
  _frameid
  _sprtype
  _rec
  _pal
  _trans
  _fliph
  _flipv
  _dissolving
  _d_type
  _d_time
  _d_tick
  _d_back
  _style
  _border
  _raw_box_border
  _translucent
  _fuzzfactor
  _fz_zoom
  _fz_stationary
  _col
  _s
  _bgcol
  _outline
  _wrap
  _rows
  _cols
  _show
  _check_depth
  _index
  _vertical
  _primary
  _percent
  _pixels
  _padding
  _dir0
  _dir1
  _justified
  _last_row_justified
  _skip_hidden
  _row_align
  _cell_align
  _padding1
  _min_breadth
  _blending
  _opacity
  _blend_mode
  _mod_r
  _mod_g
  _mod_b
end enum
