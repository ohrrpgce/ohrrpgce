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

Function DatInt(byval node as NodePtr, nname as string, byval default as integer=0, byval skip_if as bool=NO) as string
 if skip_if then return ""
 dim n as integer = GetChildNodeInt(node, nname, default)
 if n = default then return ""
 return STR(n)
End Function

Function DatStr(byval node as NodePtr, nname as string, default as string="", byval skip_if as bool=NO) as string
 if skip_if then return ""
 dim s as string = GetChildNodeStr(node, nname)
 if s = default then return ""
 return "!""" & escape_basic_str(s) & """"
End Function

Function DatFlo(byval node as NodePtr, nname as string, byval default as double=0.0, byval skip_if as bool=NO) as string
 if skip_if then return ""
 dim n as double = GetChildNodeFloat(node, nname, default)
 if n = default then return ""
 return STR(n)
End Function

Sub WriteSliceNodeAsBasic(byval fh as integer, byval n as NodePtr, byval d as integer=1, byval is_root as bool=YES)

 dim in as string = String(d, " ")
 dim typename as string = GetChildNodeStr(n, "type")
 if is_root then
  print #fh, in & "ReplaceSliceType(sl1, NewSliceOfType(sl" & typename & "))"
 else
  if not is_dim_used("sl" & d) then
   print #fh, in & "dim sl" & d & " as Slice Ptr = NewSliceOfType(sl" & typename & ")"
   mark_dim_used("sl" & d)
  else
   print #fh, in & "sl" & d & " = NewSliceOfType(sl" & typename & ")"
  end if
 end if
 WrInt fh, n, d, "lookup"
 if GetChildNodeInt(n, "fill") then
  if GetChildNodeInt(n, "fillmode") = 1 then
   'fill horizontal
   WrInt fh, n, d, "y"
   WrInt fh, n, d, "height", "h"
  end if
  if GetChildNodeInt(n, "fillmode") = 2 then
   'fill vertical
   WrInt fh, n, d, "x"
   WrInt fh, n, d, "width", "w"
  end if
 else
  'Not filling
  WrInt fh, n, d, "x"
  WrInt fh, n, d, "y"
  WrInt fh, n, d, "width", "w"
  WrInt fh, n, d, "height", "h"
 end if
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

 select case typename
  'Needs to support the same types supported by sliceedit.bas
  case "Container"
   'Container has no extra data
  case "Rectangle"
   dim style as string = DatInt(n, "style", -1)
   print #fh, in & "ChangeRectangleSlice sl" & d & ", " _
     & style & ", " _
     & DatInt(n, "fg", , style <> "") & ", " _
     & DatInt(n, "bg", , style <> "") & ", " _
     & DatInt(n, "border", -1, style <> "")  & ", " _
     & DatInt(n, "trans") & ", " _
     & DatInt(n, "fuzzfactor", 50)
  case "Sprite"
   print #fh, in & "ChangeSpriteSlice sl" & d & ", " _
     & DatInt(n, "sprtype") & ", " _
     & DatInt(n, "rec") & ", " _
     & DatInt(n, "pal", -1) & ", " _
     & DatInt(n, "frame") & ", " _
     & DatInt(n, "fliph") & ", " _
     & DatInt(n, "flipv") & ", " _
     & DatInt(n, "trans", -1)
  case "Text"
   print #fh, in & "ChangeTextSlice sl" & d & ", " _
     & DatStr(n, "s") & ", " _
     & DatInt(n, "col") & ", " _
     & DatInt(n, "outline") & ", " _
     & DatInt(n, "wrap") & ", " _
     & DatInt(n, "bgcol")
  case "Grid"
   print #fh, in & "ChangeGridSlice sl" & d & ", " _
     & DatInt(n, "rows", 1) & ", " _
     & DatInt(n, "cols", 1) & ", " _
     & DatInt(n, "show")
  case "Ellipse"
   print #fh, in & "ChangeEllipseSlice sl" & d & ", " _
     & DatInt(n, "bordercol") & ", " _
     & DatInt(n, "fillcol")
  case "Scroll"
   print #fh, in & "ChangeScrollSlice sl" & d & ", " _
     & DatInt(n, "style") & ", " _
     & DatInt(n, "check_depth")
  case "Panel"
   print #fh, in & "ChangePanelSlice sl" & d & ", " _
     & DatInt(n, "vertical") & ", " _
     & DatInt(n, "primary") & ", " _
     & DatInt(n, "pixels") & ", " _
     & DatFlo(n, "percent", 0.5) & ", " _
     & DatInt(n, "padding")
  case "Select"
   print #fh, in & "ChangeSelectSlice sl" & d & ", " _
     & DatInt(n, "index")
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
  print "  The first argument is the name of the sub in the .bas file"
  print "  which will create the collection."
  print ""
  print "  Specify - as outfile to print to console."
  print ""
  print "Usage: slice2bas subname collectionfilename filename.bas"
  print "   or: slice2bas subname collectionfilename - > filename.bas"
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
 print #fh, "Sub " & funcname & " (byval sl1 as Slice Ptr)"
  WriteSliceNodeAsBasic fh, n, 1
 print #fh, "End Sub"

 if outfile <> "-" then print "Wrote .bas source in " & int((timer - starttime) * 1000) & " ms"
 starttime = timer

 FreeDocument(doc)
End sub

'--------Module Level Code Starts here-----------------

Main()
