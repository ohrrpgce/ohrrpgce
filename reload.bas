'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "reload.bi"

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
	
	ret = Callocate(1, sizeof(Node))
	
	ret->doc = doc
	ret->name = nam
	ret->nodeType = 0
	ret->numChildren = 0
	ret->children = null
	
	return ret
End function

sub FreeNode(nod as NodePtr)
	if nod = null then exit sub
	
	dim tmp as NodePtr
	
	if nod->nodeType = rliChildren then
		do while nod->children <> 0
			FreeNode(nod->children)
		loop
	end if
	
	if nod->parent then
		if nod->parent->nodeType = rliChildren then
			if nod->parent->children = nod then
				nod->parent->children = nod->nextSib
			end if
			nod->parent->numChildren -= 1
		end if
		
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
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	nod->nodeType = rliString
	nod->str = dat
end sub

sub SetContent(nod as NodePtr, dat as longint)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	if dat > 2147483647 or dat < -2147483648 then
		nod->nodeType = rliLong
	elseif dat > 32767 or dat < -32768 then
		nod->nodeType = rliInt
	elseif dat > 127 or dat < -128 then
		nod->nodeType = rliShort
	else
		nod->nodeType = rliByte
	end if
	nod->num = dat
end sub

sub SetContent(nod as NodePtr, dat as double)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	nod->nodeType = rliFloat
	nod->flo = dat
end sub

sub SetContent(nod as NodePtr)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	nod->nodeType = rliNull
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
		
		par->nodeType = rliChildren
		
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
	if nod->nodeType = rliChildren then
		n = nod->children
		do while n <> 0
			BuildStringTable(n, table())
			n = n->nextSib
		loop
	end if
	
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
	
	
	select case nod->nodeType
		case rliNull
			print string(ind, "  ") & "<" & nod->name & " />"
		case rliByte, rliShort, rliInt, rliLong
			print string(ind, "  ");
			if 1 or nod->name <> "" then
				print "<" & nod->name & ">";
			end if
			print nod->num;
			if 1 or nod->name <> "" then
				print "</" + nod->name + ">"
			else
				print
			end if
		case rliFloat
			print string(ind, "  ");
			if 1 or nod->name <> "" then
				print "<" & nod->name & ">";
			end if
			print nod->flo;
			if 1 or nod->name <> "" then
				print "</" + nod->name + ">"
			else
				print
			end if
		case rliString
			print string(ind, "  ");
			if 1 or nod->name <> "" then
				print "<" & nod->name & ">";
			end if
			print nod->str;
			if 1 or nod->name <> "" then
				print "</" + nod->name + ">"
			else
				print
			end if
		case rliChildren
			dim n as NodePtr
			print string(ind, "  ") & "<" & nod->name & ">"
			n = nod->children
			do while n <> null
				serializeXML(n, ind + 1)
				n = n->nextSib
			loop
			print string(ind, "  ") & "</" + nod->name + ">"
	end select
	
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
	dim i as integer, us as ushort
	us = FindStringInTable(nod->name, table())
	if us = -1 then
		print "ERROR, THIS SHOULD NOT HAPPEN"
		exit sub
	end if
	put #f, , us
	put #f, , nod->nodeType
	
	select case nod->nodeType
		case rliNull
			'yeah, no
		case rliByte
			dim b as byte = nod->num
			put #f, , b
		case rliShort
			dim s as short = nod->num
			put #f, , s
		case rliInt
			i = nod->num
			put #f, , i
		case rliLong
			put #f, , nod->num
		case rliFloat
			put #f, , nod->flo
		case rliString
			i = len(nod->str)
			put #f, , i
			put #f, , nod->str
		case rliChildren
			dim as integer here, here2, dif
			here = 0
			put #f, , here 'will fill this in later
			put #f, , cint(nod->numChildren)
			here = seek(f)
			dim n as NodePtr
			n = nod->children
			do while n <> null
				serializeBin(n, f, table())
				n = n->nextSib
			loop
			here2= seek(f)
			dif = here2 - here
			seek #f, here - 8
			put #f, , dif
			seek #f, here2
			
	end select
end sub

Function FindChildByName(nod as NodePtr, nam as string) as NodePtr
	return null
End Function

Function LoadNode(f as integer, doc as DocPtr) as NodePtr
	dim ret as NodePtr
	
	ret = CreateNode(doc, "!")
	
	get #f, , ret->namenum
	get #f, , ret->nodetype
	
	select case ret->nodeType
		case 0 'null
		case 1 'byte
			dim b as byte
			get #f, , b
			ret->num = b
		case 2 'short
			dim s as short
			get #f, , s
			ret->num = s
		case 3 'integer
			dim i as integer
			get #f, , i
			ret->num = i
		case 4 'long
			get #f, , ret->num
		case 5 'float
			get #f, , ret->flo
		case 6 'string
			dim size as integer
			get #f, , size
			ret->str = string(size, " ")
			get #f, , ret->str
		case 7 'children
			dim nod as nodeptr
			dim size as integer
			get #f, , size
			get #f, , ret->numChildren
			for i as integer = 0 to ret->numChildren - 1
				nod = LoadNode(f, doc)
				if nod = null then
					freenode(ret)
					return null
				end if
				AddChild(ret, nod)
			next
		case else
			delete ret
			return null
	end select
	
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
	
	if nod->namenum > 0 then nod->name = table(nod->namenum - 1)
	if nod->nodetype = rliChildren then
		dim tmp as nodeptr
		tmp = nod->children
		do while tmp <> null
			FixNodeName(tmp, table())
			tmp = tmp->nextSib
		loop
	end if
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
		delete ret
		return null
	end if
	
	'now, we load the string table
	dim table() as string
	
	LoadStringTable(f, table())
	
	FixNodeName(ret->root, table())
	
	return ret
End Function

Function GetString(node as nodeptr) as string
	if node = null then return ""
	
	select case node->nodeType
		case rliByte, rliShort, rliInt, rliLong
			return str(node->num)
		case rliFloat
			return str(node->flo)
		case rliNull
			return ""
		case rliString
			return node->str
		case rliChildren
			return "<" & node->name & ">"
		case else
			return "Unknown value: " & node->nodeType
	end select
End Function

Function GetInteger(node as nodeptr) as LongInt
	if node = null then return 0
	
	select case node->nodeType
		case rliByte, rliShort, rliInt, rliLong
			return node->num
		case rliFloat
			return cint(node->flo)
		case rliNull
			return 0
		case rliString
			return cint(node->str)
		case rliChildren
			return 0
		case else
			return 0
	end select
End Function

Function GetDouble(node as nodeptr) as Double
	if node = null then return 0.0
	
	select case node->nodeType
		case rliByte, rliShort, rliInt, rliLong
			return cdbl(node->num)
		case rliFloat
			return node->flo
		case rliNull
			return 0.0
		case rliString
			return cdbl(node->str)
		case rliChildren
			return 0.0
		case else
			return 0.0
	end select
End Function

End Namespace
