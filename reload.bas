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
	
	ret = new Doc
	
	ret->version = 1
	ret->root = null
	
	return ret
End function

Function CreateNode(doc as DocPtr, nam as string) as NodePtr
	dim ret as NodePtr
	
	if doc = null or nam = "" then return null
	
	ret = new Node
	
	ret->doc = doc
	ret->name = nam
	ret->nodeType = 0
	ret->numChildren = 0
	ret->children = null
	
	return ret
End function

sub FreeNode(nod as NodePtr)
	if nod = null then exit sub
	if nod->parent then
		if nod->parent->children = nod then
			nod->parent->children = nod->nextSib
		end if
		nod->parent->numChildren -= 1
		
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

sub DocSetRootNode(doc as DocPtr, nod as NodePtr)
	if verifyNodeLineage(nod, doc->root) = YES and verifyNodeLineage(doc->root, nod) = YES then
		FreeNode(doc->root)
	end if
	
	doc->root = nod
	
end sub

Function FindStringInTable(st as string, table() as string) as integer
	for i as integer = lbound(table) to ubound(table)
		if table(i) = st then return i
	next
	return -1
end function

Function AddStringToTable(st as string, table() as string) as integer
	dim ret as integer
	
	ret = FindStringInTable(st, table())
	
	if ret <> -1 then return ret
	
	if table(0) = "" then
		table(0) = st
		return 0
	else
		redim preserve table(ubound(table) + 1)
		table(ubound(table)) = st
		return ubound(table)
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
	
	print string(ind, "  ") & "<" & nod->name & ">"
	select case nod->nodeType
		case rliNull
		case rliByte, rliShort, rliInt, rliLong
		
			print string(ind + 1, "  ") & nod->num
		case rliFloat
			print string(ind + 1, "  ") & nod->flo
		case rliString
			print string(ind + 1, "  ") & nod->str
		case rliChildren
			dim n as NodePtr
			n = nod->children
			do while n <> null
				serializeXML(n, ind + 1)
				n = n->nextSib
			loop
	end select
	print string(ind, "  ") & "</" + nod->name + ">"
end sub

sub SerializeBin(doc as DocPtr)
	if doc = null then exit sub
	
	dim f as integer = freefile
	dim table() as string
	
	BuildStringTable(doc->root, table())
	
	kill "test.rld"
	open "test.rld" for binary as #f
	
	dim i as integer, b as ubyte
	put #f, , "RELD"
	b = 1
	put #f, , b
	i = 13
	put #f, , i
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
			put #f, , cint(nod->numChildren)
			dim n as NodePtr
			n = nod->children
			do while n <> null
				serializeBin(n, f, table())
				n = n->nextSib
			loop
	end select
end sub

Function FindChildByName(nod as NodePtr, nam as string) as NodePtr
	return null
End Function

End Namespace
