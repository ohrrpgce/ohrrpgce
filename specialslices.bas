'OHRRPGCE - Classes for additional slice types
'(C) Copyright 2018 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "const.bi"
#include "uiconst.bi"
#include "slices.bi"
#include "sliceedit.bi"

#include "specialslices.bi"


function highlight_color(col as integer, highlight as bool) as integer
 if highlight = NO then return col
 with master(col)
  return findrgb(.r + 100, .g + 100, .b + 100)
 end with
end function


'==============================================================================
'                                   GraphSlice
'==============================================================================


constructor GraphSlice()
 axiscol = uilook(uiMenuItem)
 linecol = findrgb(255,255,255)
 gridcol = uilook(uiHighlight)
 labelcol = uilook(uiMenuItem)
'findrgb(0,0,160)
 default_maxy = 1
end constructor

sub GraphSlice.Initialize(sl as Slice ptr)
 this.sl = sl
 'This padding tries to estimate the amount of additional space needed
 'for the axes, but it's not accurate
 sl->PaddingLeft = 52
 sl->PaddingRight = 12
 sl->PaddingTop = 5
 sl->PaddingBottom = 14
 field_sl = NewSliceOfType(slContainer, sl)
 field_sl->Fill = YES
 field_sl->Protect = YES
end sub

local sub set_min_and_max(byref minv as double, byref maxv as double, vals() as double, defaultmax as double)
 minv = 1e99
 maxv = -1e99
 for i as integer = 0 to ubound(vals)
  if vals(i) < minv then minv = vals(i)
  if vals(i) > maxv then maxv = vals(i)
 next
 'Decide whether to use 0 at the origin, or minv
 if minv > 0 andalso minv < 0.8 * maxv then minv = 0
 if minv = maxv then
  if minv < 0 then
   maxv = 0
  elseif minv = 0 then
   maxv = defaultmax
  else
   minv = 0
  end if
 end if
end sub

'Updates x/yscale and min/maxx/y
sub GraphSlice.update_bounds()
 set_min_and_max minx, maxx, x(), 1.
 set_min_and_max miny, maxy, y(), default_maxy

 RefreshSliceTreeScreenPos(sl)
 xscale = (field_sl->Width - 1) / (maxx - minx)
 yscale = (field_sl->Height - 1) / (maxy - miny)
end sub

'Screen position of bottom-left corner of field
function GraphSlice.origin() as XYPair
 return XY(field_sl->ScreenX, field_sl->ScreenY + field_sl->Height - 1)
end function

function GraphSlice.point_to_screen(pointx as double, pointy as double) as XYPair
 return origin() + XY((pointx - minx) * xscale, -(pointy - miny) * yscale)
end function

local sub draw_label(label as string, pos as XYPair, col as integer, highlightcol as integer = -1, page as integer)
 if highlightcol > -1 then
  'Unfortunately, just drawing a normal background behind the text doesn't provide any margin.
  'Maybe should add another setting to render_text or Font to tweak that
  dim size as XYPair = textsize(label) + XY(3, 3)
  pos.x = relative_pos(pos.x, 0, size.w)
  pos.y = relative_pos(pos.y, 0, size.h)
  rectangle pos.x, pos.y, size.w, size.h, highlightcol, page
  pos += XY(1, 2)  'Center text
 end if
 edgeprint label, pos.x, pos.y, col, page
end sub

function GraphSlice.draw_x_tick(value as double, page as integer, highlight as bool = NO, showlabel as bool = YES) as integer
 dim pos as XYPair = point_to_screen(value, miny)
 rectangle pos.x, pos.y, 1, 5, axiscol, page
 rectangle pos.x, pos.y, 1, -field_sl->Height, highlight_color(gridcol, highlight), page

 if showlabel = NO then return 0
 dim label as string = format_float(value, 3)
 'if int_x_labels then label = str(int(value))

 pos += XY(ancCenter, 5)
 draw_label label, pos, iif(highlight, linecol, labelcol), iif(highlight, gridcol, -1), page
 'edgeprint label, pos.x + ancCenter, pos.y + 5, labelcol, page
 return textwidth(label)
end function

function GraphSlice.draw_y_tick(value as double, page as integer, highlight as bool = NO, showlabel as bool = YES) as integer
 dim pos as XYPair = point_to_screen(minx, value)
 rectangle pos.x, pos.y, -5, 1, axiscol, page
 rectangle pos.x, pos.y, field_sl->Width, 1, highlight_color(gridcol, highlight), page

 if showlabel = NO then return 0
 dim label as string = format_float(value, 3)
 'if int_y_labels then label = str(int(value))

 pos += XY(ancRight - 5, ancCenter)
 draw_label label, pos, iif(highlight, linecol, labelcol), iif(highlight, gridcol, -1), page
 return 10  'Label height
end function

'This draws all the ticks on an axis except for the min and max ones
sub GraphSlice.draw_regular_ticks(axis as integer, labelsize as integer, page as integer)
 dim as double tickstep, tickfirst, ticklast, scale

 if axis = 0 then
  tickfirst = minx
  ticklast = maxx
  scale = xscale
  'Horizontal spacing is very important
  labelsize += 3
 else
  tickfirst = miny
  ticklast = maxy
  scale = yscale
 end if

 dim as double maxrange, max_next_pow10
 maxrange = large(abs(tickfirst), abs(ticklast))
 if maxrange = 0 then maxrange = 1
 max_next_pow10 = 10 ^ (1 + INT(LOG(maxrange) / LOG(10)))

 'Search through a sequence of increasing ticksteps which are round numbers
 'until we find one which puts the labels far enough apart
 static steps(...) as double = {1, 2, 2.5, 5}
 dim pix_per_label as integer
 for decade as integer = -3 to 0
  for stepi as integer = 0 to ubound(steps)
   tickstep = steps(stepi) * 10 ^ decade * max_next_pow10
   if ABS(tickstep - CINT(tickstep)) > 1e-4 then  'Not an integer value
    if axis = 0 andalso int_x_labels then continue for
    if axis = 1 andalso int_y_labels then continue for
   end if
   pix_per_label = tickstep * scale
   if pix_per_label > labelsize then exit for, for
  next stepi
 next decade

 'Forward from the min to the first regular tick
 dim ticknext as double
 ticknext = (INT(tickfirst / tickstep) + 1) * tickstep

 while ticknext < ticklast
  dim showlabel as bool = YES
  'Skip the label if it's too close to the start or end
  if (ticknext - tickfirst) * scale < labelsize then showlabel = NO
  if (ticklast - ticknext)  * scale < labelsize then showlabel = NO

  if axis = 0 then
   draw_x_tick ticknext, page, , showlabel
  else
   draw_y_tick ticknext, page, , showlabel
  end if
  ticknext += tickstep
 wend
end sub

sub GraphSlice.Draw(sl as Slice ptr, page as integer)
 update_bounds()

 dim as integer labelwidth, labelheight = 10
 ' Draw first and last ticks
 labelwidth = large(draw_x_tick(minx, page), _
                    draw_x_tick(maxx, page))
 sl->PaddingRight = large(12, labelwidth \ 2 + 1)
 draw_y_tick(miny, page)
 draw_y_tick(maxy, page)
 'If not sticking to integer labels, intermediate labels might be
 'longer than the first/last due to decimal places
 '(TODO: this is a poor solution)
 if int_x_labels = NO then labelwidth = large(32, labelwidth)

 ' Draw the other ticks
 draw_regular_ticks(0, labelwidth, page)  'X axis
 draw_regular_ticks(1, labelheight, page)  'Y axis

 if showpoint then
  draw_x_tick showx, page, YES
  draw_y_tick showy, page, YES
 end if

 'Draw the border lines along the axes
 dim orn as XYPair = origin()
 rectangle orn.x, orn.y, 1, -field_sl->Height, axiscol, page
 rectangle orn.x, orn.y, field_sl->Width, 1, axiscol, page

 'Draw the graph
 for i as integer = 1 to ubound(x)
  dim p1 as XYPair = point_to_screen(x(i-1), y(i-1))
  dim p2 as XYPair = point_to_screen(x(i), y(i))
  drawline p1.x, p1.y, p2.x, p2.y, linecol, page
 next
end sub

sub GraphSlice.mouse_over()
 update_bounds()
 dim byref mouse as MouseInfo = readmouse
 if SliceCollidePoint(sl, mouse.pos) = NO then
  showpoint = NO
  exit sub
 end if

 dim besti as integer
 dim bestdist as double = INT_MAX

 if mouse.x >= field_sl->ScreenX then   ' Mouse not over Y axis
  ' Find the nearest point on the graph
  for i as integer = 0 to ubound(x)
   dim dist as double = xypair_distance_squared(mouse.pos, point_to_screen(x(i), y(i)))
   if dist < bestdist then
    bestdist = dist
    besti = i
   end if
  next
 end if

 if bestdist > 10 ^ 2 then
  ' The mouse isn't close to the line, so use the mouse to pick X or Y position instead
  dim mpos as XYPair = mouse.pos - origin()
  mpos.y *= -1
  bestdist = INT_MAX
  for i as integer = 0 to ubound(x)
   dim dist as double
   if mouse.x < field_sl->ScreenX then
    ' Mouse over Y axis
    dist = abs(mpos.y - y(i) * yscale)
   else
    dist = abs(mpos.x - x(i) * xscale)
   end if
   if dist < bestdist then
    bestdist = dist
    besti = i
   end if
  next
 end if

 showpoint = YES
 showx = x(besti)
 showy = y(besti)
end sub
