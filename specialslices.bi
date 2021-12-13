'OHRRPGCE - Classes for additional slice types
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef SPECIALSLICES_BI
#define SPECIALSLICES_BI

#include "util.bi"
#include "slices.bi"

Type GraphSlice Extends ClassSlice
 'Colors
 axiscol as integer     'The X and Y axes
 linecol as integer
 labelcol as integer    'Text labels
 gridcol as integer     'Used to draw the grid lines

 default_maxy as double 'What to use for a stationary graph
 int_x_labels as bool   'Print x values as integers
 int_y_labels as bool   'Print x values as integers
 x(any) as double
 y(any) as double
 sl as Slice ptr        'The Slice that owns this instance
 field_sl as Slice ptr  'The graph itself

 showpoint as bool      'Showing a certain X,Y value
 showx as double
 showy as double

 minx as double
 maxx as double
 miny as double
 maxy as double
 xscale as double       'To convert from graphed values to screen pixels
 yscale as double

 Declare Constructor()

 'ClassSlice methods
 Declare Virtual Sub Initialize(sl as Slice ptr)
 Declare Virtual Sub Draw(sl as Slice ptr, page as integer)

 Declare Sub update_bounds()
 Declare Sub mouse_over()

 PRIVATE:
 Declare Function origin() as XYPair
 Declare Function point_to_screen(pointx as double, pointy as double) as XYPair
 Declare Function draw_x_tick(value as double, page as integer, highlight as bool = NO, showlabel as bool = YES) as integer
 Declare Function draw_y_tick(value as double, page as integer, highlight as bool = NO, showlabel as bool = YES) as integer
 Declare Sub draw_regular_ticks(axis as integer, labelsize as integer, page as integer)
End Type

#endif
