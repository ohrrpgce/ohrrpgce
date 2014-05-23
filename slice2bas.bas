#include "reload.bi"

Using Reload

CONST verbose = NO

EXTERN already_dimmed() as string
REDIM already_dimmed() as string

Sub mark_dim_used(v as string)
 Redim preserve already_dimmed(ubound(already_dimmed) + 1) as string
 already_dimmed(ubound(already_dimmed)) = v
End sub

Function is_dim_used(v as string) as bool
 for i as integer = 0 to ubound(already_dimmed)
  if v = already_dimmed(i) then return YES
 next i
 return NO
End function

Function escape_basic_str (byval s as string) as string
 'Escapes a value for !"" style strings
 'This probably breaks messily on non-printable characters :(
 dim result as string = ""
 dim ch as string
 for i as integer = 1 to len(s)
  ch = mid(s, i, 1)
  select case ch
   case """" 'A single quote char
    result &= "\"""
   case !"\n" 'A line feed
    result &= "\n"
   case else
    result &= ch
  end select
 next i
 return result
End function

Sub WrInt (byval fh as integer, byval n as NodePtr, byval d as integer, prop as string, alt_node_prop as string="", byval default as integer=0, pointerv as string="")
 if pointerv = "" then pointerv = "sl" & d
 dim nprop as string
 if alt_node_prop <> "" then nprop = alt_node_prop else nprop = prop
 dim v as integer = GetChildNodeInt(n, nprop, default)
 if not verbose then if v = default then exit sub 'Don't bother writing default values
 dim in as string = String(d, " ")
 print #fh, in & pointerv & "->" & prop & " = " & v
End Sub

Sub WrStr (byval fh as integer, byval n as NodePtr, byval d as integer, prop as string, alt_node_prop as string="", default as string="", pointerv as string="")
 if pointerv = "" then pointerv = "sl" & d
 dim nprop as string
 if alt_node_prop <> "" then nprop = alt_node_prop else nprop = prop
 dim v as string = GetChildNodeStr(n, nprop, default)
 if not verbose then if v = default then exit sub 'Don't bother writing default values
 dim in as string = String(d, " ")
 print #fh, in & pointerv & "->" & prop & " = !""" & escape_basic_str(v) & """"
End Sub

Sub WrFl (byval fh as integer, byval n as NodePtr, byval d as integer, prop as string, alt_node_prop as string="", byval default as double=0.0, pointerv as string="")
 if pointerv = "" then pointerv = "sl" & d
 dim nprop as string
 if alt_node_prop <> "" then nprop = alt_node_prop else nprop = prop
 dim v as double = GetChildNodeFloat(n, nprop, default)
 if not verbose then if v = default then exit sub 'Don't bother writing default values
 dim in as string = String(d, " ")
 print #fh, in & pointerv & "->" & prop & " = " & v
End Sub

Sub WriteSliceNodeAsBasic(byval fh as integer, byval n as NodePtr, byval d as integer=1, byval is_root as bool=YES)

 dim in as string = String(d, " ")
 dim typename as string = GetChildNodeStr(n, "type")
 if not is_dim_used("sl" & d) then
  print #fh, in & "dim sl" & d & " as Slice Ptr = NewSliceOfType(sl" & typename & ")"
  mark_dim_used("sl" & d)
 end if
 WrInt fh, n, d, "lookup"
 WrInt fh, n, d, "x"
 WrInt fh, n, d, "y"
 WrInt fh, n, d, "width", "w"
 WrInt fh, n, d, "height", "h"
 WrInt fh, n, d, "mobile", , YES
 WrInt fh, n, d, "clip"
 WrInt fh, n, d, "velocity.x", "vx"
 WrInt fh, n, d, "velocity.y", "vy"
 WrInt fh, n, d, "velticks.x", "vtickx"
 WrInt fh, n, d, "velticks.y", "vticky"
 WrInt fh, n, d, "targ.x", "tx"
 WrInt fh, n, d, "targ.y", "ty"
 WrInt fh, n, d, "targticks", "ttick"
 WrInt fh, n, d, "alignhoriz", "alignh"
 WrInt fh, n, d, "alignvert", "alignv"
 WrInt fh, n, d, "anchorhoriz", "anchorh"
 WrInt fh, n, d, "anchorvert", "anchorv"
 WrInt fh, n, d, "paddingtop", "padt"
 WrInt fh, n, d, "paddingleft", "padl"
 WrInt fh, n, d, "paddingright", "padr"
 WrInt fh, n, d, "paddingbottom", "padb"
 WrInt fh, n, d, "fill"
 WrInt fh, n, d, "fillmode"
 WrInt fh, n, d, "sorter", "sort"
 WrInt fh, n, d, "autosort"
 WrInt fh, n, d, "extra(0)", "extra0"
 WrInt fh, n, d, "extra(1)", "extra1"
 WrInt fh, n, d, "extra(2)", "extra2"

 dim datv as string = lcase(typename) & "dat"
 if typename <> "Container" then
  if not is_dim_used(datv) then
   print #fh, in & "dim " & datv & " as " & typename & "SliceData Ptr = sl" & d & "->SliceData"
   mark_dim_used(datv)
  end if
 end if
 
 select case typename
  'Needs to support the same types supported by sliceedit.bas
  case "Container"
   'Container has no extra data
  case "Rectangle"
   WrInt fh, n, d, "style", , -1, datv
   WrInt fh, n, d, "fgcol", "fg", , datv
   WrInt fh, n, d, "bgcol", "bg", , datv
   WrInt fh, n, d, "translucent", "trans", , datv
   WrInt fh, n, d, "border", , -1, datv
   WrInt fh, n, d, "fuzzfactor", , 50, datv
  case "Sprite"
   WrInt fh, n, d, "spritetype", "sprtype", , datv
   WrInt fh, n, d, "record", "rec", , datv
   WrInt fh, n, d, "pal", , -1, datv
   WrInt fh, n, d, "frame", , , datv
   WrInt fh, n, d, "fliphoriz", "fliph", , datv
   WrInt fh, n, d, "flipvert", "flipv", , datv
   WrInt fh, n, d, "trans", , 1, datv
   print #fh, in & datv & "->paletted = (" & datv & "->spritetype <> sprTypeMXS)"
  case "Text"
   WrStr fh, n, d, "s", , , datv
   WrInt fh, n, d, "col", , , datv
   WrInt fh, n, d, "outline", , , datv
   WrInt fh, n, d, "wrap", , , datv
   WrInt fh, n, d, "bgcol", , , datv
  case "Grid"
   WrInt fh, n, d, "cols", , 1, datv
   WrInt fh, n, d, "rows", , 1, datv
   WrInt fh, n, d, "show", , , datv
  case "Ellipse"
   WrInt fh, n, d, "bordercol", , , datv
   WrInt fh, n, d, "fillcol", , , datv
  case "Scroll"
   WrInt fh, n, d, "style", , , datv
   WrInt fh, n, d, "check_depth", , , datv
  case "Panel"
   WrInt fh, n, d, "vertical", , , datv
   WrInt fh, n, d, "primary", , , datv
   WrInt fh, n, d, "pixels", , , datv
   WrFl fh, n, d, "percent", , , datv
   WrInt fh, n, d, "padding", , , datv
  case "Select"
   WrInt fh, n, d, "index", , , datv
  case else
   print #fh, "WARNING: slice2bas doesn't support slices of type """ & typename & """"
 end select

 'Now recurse for all children
 dim children as NodePtr
 children = GetChildByName(n, "children")
 dim ch as NodePtr = FirstChild(children)
 do while ch <> 0
  WriteSliceNodeAsBasic fh, ch, d + 1, NO
  ch = NextSibling(ch)
 loop

 if not is_root then
  print #fh, in & "SetSliceParent(sl" & d & ", sl" & d - 1 & ")" 
 end if
 
End Sub

Sub Main ()
 dim as string funcname, filename, outfile
 dim as integer validargs = NO, debugging = NO, i = 1

 while command(i) <> ""
  if funcname = "" then
   funcname = command(i)
  elseif filename = "" then
   filename = command(i)
   validargs = YES
  elseif outfile = "" then
   outfile = command(i)
  else
   validargs = NO
  end if
  i += 1
 wend

 if isfile(filename) = 0 or validargs = NO then
  print "Convert a Slice collection saved as a Reload file into .bas source code"
  print "  which creates that same slice collection."
  print ""
  print "  The first argument is the name of the function in the .bas file"
  print "  which will return the collection."
  print ""
  print "  Specify - as outfile to print to console."
  print ""
  print "Usage: slice2bas functionname collectionfilename filename.bas"
  print "   or: slice2bas functionname collectionfilename - > filename.bas"
  exit sub
 end if

 if outfile = "" then outfile = "-"

 dim as double startTime = timer, realStart = timer

 dim fh as integer
 fh = freefile
 if outfile = "-" then
  open cons for output as fh
 else
  if open(outfile for output as fh) then
   print "Could not open " & outfile
   exit sub
  end if
 end if
 
 dim doc as DocPtr
 doc = LoadDocument(filename, optNoDelay)

 if outfile <> "-" then print "Loaded Slice Collection RELOAD document in " & int((timer - starttime) * 1000) & " ms"
 starttime = timer

 Dim n as NodePtr = DocumentRoot(doc)
 print #fh, "'######## This file was auto-generated by the slice2bas tool! ########"
 print #fh, "'######## Rather than editing this file, it may be better to  ########"
 print #fh, "'######## edit the reload file in the slice collection editor ########"
 print #fh, "'######## and then re-convert it with slice2bas               ########"
 print #fh, ""
 print #fh, "#include ""slices.bi"""
 print #fh, ""
 print #fh, "Function " & funcname & " () as Slice Ptr"
  WriteSliceNodeAsBasic fh, n, 1
 print #fh, " Return sl1"
 print #fh, "End Function"

 if outfile <> "-" then print "Wrote .bas source in " & int((timer - starttime) * 1000) & " ms"
 starttime = timer

 FreeDocument(doc)
End sub

'--------Module Level Code Starts here-----------------

Main()
