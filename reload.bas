'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "reload.bi"
DECLARE SUB debug (s AS STRING)

Namespace Reload

Function verifyNodeLineage(byval sl as NodePtr, parent as NodePtr) as integer
 dim s as NodePtr
 if sl = 0 then return no
 s = parent
 do while s <> 0
  if s = sl then return no
  s = s->parent
 loop
 return yes
end function

Function verifyNodeSiblings(byval sl as NodePtr, family as NodePtr) as integer
 dim s as NodePtr
 if sl = 0 then return no
 s = family
 do while s <> 0
  if s = sl then return no
  s = s->prevSib
 loop
 s = family
 do while s <> 0
  if s = sl then return no
  s = s->nextSib
 loop
 return yes
end function

Function CreateDocument() as DocPtr
	dim ret as DocPtr
	
	ret = Callocate(1, sizeof(Doc))
	
	ret->version = 1
	ret->root = null
	
	return ret
End function

Function CreateNode(doc as DocPtr, nam as string) as NodePtr
	dim ret as NodePtr
	
	if doc = null then return null
	
	ret = New Node
	
	ret->doc = doc
	ret->name = nam
	ret->nodeType = rltNull
	ret->numChildren = 0
	ret->children = null
	
	return ret
End function

sub FreeNode(nod as NodePtr)
	if nod = null then exit sub
	
	dim tmp as NodePtr
	do while nod->children <> 0
		FreeNode(nod->children)
	loop
	
	if nod->parent then
		'if nod->parent->nodeType = rltChildren then
		if nod->parent->children = nod then
			nod->parent->children = nod->nextSib
		end if
		nod->parent->numChildren -= 1
		'end if
		
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
		end if
		
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
		end if
	end if
	
	delete nod
end sub

sub FreeDocument(doc as DocPtr)
	if doc = null then return
	
	if doc->root then
		FreeNode(doc->root)
		doc->root = null
	end if
	
	delete doc
end sub

sub SetContent (nod as NodePtr, dat as string)
	if nod = null then exit sub
	'if nod->nodeType = rltChildren then
		'we need to free the children
		'FreeNode(nod->Children)
		'nod->Children = null
		'nod->NumChildren = 0
	'end if
	nod->nodeType = rltString
	nod->str = dat
end sub

sub SetContent(nod as NodePtr, dat as longint)
	if nod = null then exit sub
	'if nod->nodeType = rltChildren then
		'we need to free the children
		'FreeNode(nod->Children)
		'nod->Children = null
		'nod->NumChildren = 0
	'end if
	nod->nodeType = rltInt
	nod->num = dat
end sub

sub SetContent(nod as NodePtr, dat as double)
	if nod = null then exit sub
	'if nod->nodeType = rltChildren then
		'we need to free the children
		'FreeNode(nod->Children)
		'nod->Children = null
		'nod->NumChildren = 0
	'end if
	nod->nodeType = rltFloat
	nod->flo = dat
end sub

sub SetContent(nod as NodePtr)
	if nod = null then exit sub
	'if nod->nodeType = rltChildren then
		'we need to free the children
		'FreeNode(nod->Children)
		'nod->Children = null
		'nod->NumChildren = 0
	'end if
	nod->nodeType = rltNull
end sub

Sub RemoveParent(nod as NodePtr)
	if nod->parent then
		if nod->parent->children = nod then
			nod->parent->children = nod->nextSib
			nod->parent = null
		end if
		nod->parent->numChildren -= 1
		
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
			nod->nextSib = null
		end if
		
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
			nod->prevSib = null
		end if
	end if
end sub

function AddChild(par as NodePtr, nod as NodePtr) as NodePtr
	
	if verifyNodeLineage(nod, par) = NO then return nod
	
	'first, remove us from our old parent
	RemoveParent(nod)
	
	'next, add us to our new parent
	if par then
		nod->parent = par
		par->numChildren += 1
		
		'par->nodeType = rltChildren
		
		if par->children = null then
			par->children = nod
		else
			dim s as NodePtr
			s = par->children
			do while s->NextSib <> 0
				s = s->NextSib
			loop
			s->NextSib = nod
			nod->prevSib = s
			
		end if
	end if
	
	return nod
end function

function AddSiblingAfter(sib as NodePtr, nod as NodePtr) as NodePtr

	if verifyNodeSiblings(nod, sib) = NO then return nod
	
	if sib = 0 then return nod
	
	nod->prevSib = sib
	nod->nextSib = sib->nextSib
	sib->nextSib = nod
	if nod->nextSib then nod->nextSib->prevSib = nod
	
	nod->parent = sib->parent
	sib->parent->numChildren += 1
	
	return nod
end function

function AddSiblingBefore(sib as NodePtr, nod as NodePtr) as NodePtr

	if verifyNodeSiblings(nod, sib) = NO then return nod
	
	if sib = 0 then return nod
	
	nod->nextSib = sib
	nod->prevSib = sib->prevSib
	sib->prevSib = nod
	if nod->prevSib then nod->prevSib->nextSib = nod
	
	nod->parent = sib->parent
	sib->parent->numChildren += 1
	
	return nod
end function

sub SetRootNode(doc as DocPtr, nod as NodePtr)
	if verifyNodeLineage(nod, doc->root) = YES and verifyNodeLineage(doc->root, nod) = YES then
		FreeNode(doc->root)
	end if
	
	doc->root = nod
	
end sub

Function FindStringInTable(st as string, table() as string) as integer
	if st = "" then return 0
	for i as integer = lbound(table) to ubound(table)
		if table(i) = st then return i + 1
	next
	return -1
end function

Function AddStringToTable(st as string, table() as string) as integer
	dim ret as integer
	
	ret = FindStringInTable(st, table())
	
	if ret <> -1 then return ret
	
	if table(0) = "" then
		table(0) = st
		return 1
	else
		redim preserve table(ubound(table) + 1)
		table(ubound(table)) = st
		return ubound(table) + 1
	end if
end function

sub BuildStringTable(nod as NodePtr, table() as string)
	static first as integer, start as NodePtr
	
	if nod = null then exit sub
	
	if first = no then
		redim table(0)
		start = nod
		first = yes
	end if
	
	AddStringToTable(nod->name, table())
	
	dim n as NodePtr
	'if nod->nodeType = rltChildren then
	n = nod->children
	do while n <> 0
		BuildStringTable(n, table())
		n = n->nextSib
	loop
	'end if
	
	if start = nod then
		first = no
		start = null
	end if
end sub

sub SerializeXML (doc as DocPtr)
	if doc = null then exit sub
	
	serializeXML(doc->root)
end sub

sub serializeXML (nod as NodePtr, ind as integer = 0)
	if nod = null then exit sub
	
	print string(ind, "	");
	if nod->nodeType <> rltNull or nod->numChildren <> 0 then
		if nod->name <> "" then
			print "<" & nod->name & ">";
		end if
	elseif nod->nodeType = rltNull and nod->numChildren = 0 then
		print "<" & nod->name & " />"
		exit sub
	end if
	
	if nod->nodeType <> rltNull and nod->numChildren <> 0 then print
	
	select case nod->nodeType
		case rltInt
			print "" & nod->num;
		case rltFloat
			print "" & nod->flo;
		case rltString
			print "" & nod->str;
		'case rltNull
		'	print ;
		'case rltChildren
	end select
	
	if nod->numChildren <> 0 then print
	
	dim n as NodePtr = nod->children
	
	do while n <> null
		serializeXML(n, ind + 1)
		n = n->nextSib
	loop
	
	if nod->numChildren <> 0 then print string(ind, "	");
	
	if nod->nodeType <> rltNull or nod->numChildren <> 0 then
		
		if nod->name <> "" then
			print "</" & nod->name & ">"
		else
			print
		end if
	end if
	
end sub

sub SerializeBin(file as string, doc as DocPtr)
	if doc = null then exit sub
	
	dim f as integer = freefile
	dim table() as string
	
	BuildStringTable(doc->root, table())
	
	kill file & ".tmp"
	open file & ".tmp" for binary as #f
	
	dim i as uinteger, b as ubyte
	put #f, , "RELD"
	b = 1
	put #f, , b 'version
	i = 13
	put #f, , i 'size of header
	i = 0 
	put #f, , i 'we're going to fill this in later
	
	serializeBin(doc->root, f, table())
	
	i = seek(f) - 1
	put #f, 10, i 'filling in the string table position
	
	seek f, i + 1
	
	dim s as short
	s = ubound(table) - lbound(table) + 1
	put #f, , s
	for i = lbound(table) to ubound(table)
		s = len(table(i))
		put #f, , s
		put #f, , table(i)
	next
	close #f
	
	kill file
	rename file & ".tmp", file
	kill file & ".tmp"
end sub

sub serializeBin(nod as NodePtr, f as integer, table() as string)
	dim i as integer, us as ushort, ub as ubyte
	
	dim as integer siz, here = 0, here2, dif
	siz = seek(f)
	put #f, , here 'will fill this in later
	
	here = seek(f)
	
	us = FindStringInTable(nod->name, table())
	if us = -1 then
		print "ERROR, THIS SHOULD NOT HAPPEN"
		exit sub
	end if
	
	WriteVLI(f, us)
	'put #f, , us
	
	select case nod->nodeType
		case rltNull
			'yeah, no
			ub = rliNull
			put #f, , ub
		case rltInt 'this is good enough, don't need VLI for this
			if nod->num > 2147483647 or nod->num < -2147483648 then
				ub = rliLong
				put #f, , ub
				put #f, , nod->num
			elseif nod->num > 32767 or nod->num < -32768 then
				ub = rliInt
				put #f, , ub
				i = nod->num
				put #f, , i
			elseif nod->num > 127 or nod->num < -128 then
				ub = rliShort
				put #f, , ub
				dim s as short = nod->num
				put #f, , s
			else
				ub = rliByte
				put #f, , ub
				dim b as byte = nod->num
				put #f, , b
			end if
		case rltFloat
			ub = rliFloat
			put #f, , ub
			put #f, , nod->flo
		case rltString
			ub = rliString
			put #f, , ub
			i = len(nod->str)
			'put #f, , i
			WriteVLI(f, i)
			put #f, , nod->str
			
	end select
	
	'dim tmp as integer = nod->numChildren
	'put #f, , tmp
	WriteVLI(f, nod->numChildren)
	
	dim n as NodePtr
	n = nod->children
	do while n <> null
		serializeBin(n, f, table())
		n = n->nextSib
	loop
	here2 = seek(f)
	dif = here2 - here
	put #f, siz, dif
	'print "size: " & dif
	seek #f, here2
end sub

Function FindChildByName(nod as NodePtr, nam as string) as NodePtr
	if nod = null then return null
	if nod->name = nam then return nod
	'if nod->nodeType <> rltChildren then return null
	dim child as NodePtr
	dim ret as NodePtr
	child = nod->children
	while child <> null
		ret = FindChildByName(child, nam)
		if ret <> null then return ret
		child = child->nextSib
	wend
	return null
End Function

Function LoadNode(f as integer, doc as DocPtr) as NodePtr
	dim ret as NodePtr
	
	ret = CreateNode(doc, "!")
	
	dim size as integer
	
	dim as integer here, here2
	get #f, , size
	
	here = seek(f)
	
	'get #f, , ret->namenum
	ret->namenum = cshort(ReadVLI(f))
	get #f, , ret->nodetype
	
	select case ret->nodeType
		case rliNull
		case rliByte
			dim b as byte
			get #f, , b
			ret->num = b
			ret->nodeType = rltInt
		case rliShort
			dim s as short
			get #f, , s
			ret->num = s
			ret->nodeType = rltInt
		case rliInt
			dim i as integer
			get #f, , i
			ret->num = i
			ret->nodeType = rltInt
		case rliLong
			get #f, , ret->num
			ret->nodeType = rltInt
		case rliFloat
			get #f, , ret->flo
			ret->nodeType = rltFloat
		case rliString
			dim size as integer
			'get #f, , size
			size = cint(ReadVLI(f))
			ret->str = string(size, " ")
			get #f, , ret->str
			ret->nodeType = rltString
		case else
			delete ret
			return null
	end select
	
	dim nod as nodeptr
	'get #f, , ret->numChildren
	ret->numChildren = ReadVLI(f)
	print ret->numChildren
	for i as integer = 0 to ret->numChildren - 1
		nod = LoadNode(f, doc)
		if nod = null then
			freenode(ret)
			return null
		end if
		ret->numChildren -= 1
		AddChild(ret, nod)
	next
	
	if seek(f) - here <> size then
		print "OHFUCK read " & (seek(f) - here) & " bytes instead of " & size & "!"
		end 1
	end if
	
	return ret
End Function

Sub LoadStringTable(f as integer, table() as string)
	dim as ushort count, size
	
	get #f, , count
	
	if count <= 0 then exit sub
	
	for i as integer = 0 to count - 1
		redim preserve table(i)
		get #f, , size
		table(i) = string(size, " ")
		get #f, , table(i)
	next
end sub

function FixNodeName(nod as nodeptr, table() as string) as integer
	if nod = null then return -1
	
	if nod->namenum > ubound(table) + 1 or nod->namenum < 0 then
		return -1
	end if
	
	if nod->namenum > 0 then
		nod->name = table(nod->namenum - 1)
	else
		nod->name = ""
	end if
	
	'if nod->nodetype = rltChildren then
		dim tmp as nodeptr = nod->children
		do while tmp <> null
			FixNodeName(tmp, table())
			tmp = tmp->nextSib
		loop
	'end if
end function

Function LoadDocument(fil as string) as DocPtr
	dim ret as DocPtr
	dim f as integer = freefile
	
	if open(fil, for binary, as #f) then
		return null
	end if
	
	dim as ubyte ver
	dim as integer headSize, datSize
	dim as string magic = "    "
	
	dim b as ubyte, i as integer
	
	get #f, , magic
	
	if magic <> "RELD" then
		close #f
		return null
	end if
	
	get #f, , ver
	
	select case ver
		case 1 ' no biggie
			get #f, , headSize
			
			if headSize <> 13 then 'uh oh, the header is the wrong size
				close #f
				return null
			end if
			
			get #f, , datSize
			
		case else ' dunno. Let's quit.
			close #f
			return null
	end select
	
	'if we got here, the document is... not yet corrupt. I guess.
	
	ret = CreateDocument()
	ret->version = ver
	
	ret->root = LoadNode(f, ret)
	
	if ret->root = null then
		close #f
		delete ret
		return null
	end if
	
	'now, we load the string table
	dim table() as string
	
	LoadStringTable(f, table())
	
	print FixNodeName(ret->root, table())
	
	close #f
	
	return ret
End Function

Function GetString(node as nodeptr) as string
	if node = null then return ""
	
	select case node->nodeType
		case rltInt
			return str(node->num)
		case rltFloat
			return str(node->flo)
		case rltNull
			return ""
		case rltString
			return node->str
		'case rltChildren
		'	return "<" & node->name & ">"
		case else
			return "Unknown value: " & node->nodeType
	end select
End Function

Function GetInteger(node as nodeptr) as LongInt
	if node = null then return 0
	
	select case node->nodeType
		case rltInt
			return node->num
		case rltFloat
			return cint(node->flo)
		case rltNull
			return 0
		case rltString
			return cint(node->str)
		'case rltChildren
		'	return 0
		case else
			return 0
	end select
End Function

Function GetDouble(node as nodeptr) as Double
	if node = null then return 0.0
	
	select case node->nodeType
		case rltInt
			return cdbl(node->num)
		case rltFloat
			return node->flo
		case rltNull
			return 0.0
		case rltString
			return cdbl(node->str)
		'case rltChildren
		'	return 0.0
		case else
			return 0.0
	end select
End Function

Sub WriteVLI(f as integer, v as Longint)
	dim o as ubyte
	dim neg as integer
	
	if o < 0 then
		neg = yes
		v = abs(v)
	end if
	
	o = v and &b111111
	v /= 64
	
	if neg then   o AND=  &b1000000
	
	if v > 0 then o AND= &b10000000
	
	put #f, , o
	
	do while v > 0
		o = v and &b1111111
		v /= 128
		
		if v > 0 then o AND= &b10000000
		
		put #f, , o
	loop

end sub

function ReadVLI(f as integer) as longint
	dim o as ubyte
	dim ret as longint
	dim neg as integer
	
	get #f, , o
	
	if o AND &b1000000 then neg = yes
	
	ret = o AND &b111111
	
	if o AND &b10000000 then
		ret *= 64
	end if
	
	do while o AND &b10000000
		get #f, , o
		
		ret += o AND &b1111111
		
		if o and &b10000000 then
			ret *= 128
		end if
	loop
	
	if neg then ret *= -1
	
	return ret
	
end function


End Namespace
