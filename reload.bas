'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "reload.bi"

Function verifyNodeLineage(byval sl as reloadNodePtr, parent as reloadNodePtr) as integer
 dim s as reloadNodePtr
 if sl = 0 then return no
 s = parent
 do while s <> 0
  if s = sl then return no
  s = s->parent
 loop
 return yes
end function

Function verifyNodeSiblings(byval sl as reloadNodePtr, family as reloadNodePtr) as integer
 dim s as reloadNodePtr
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

Function CreateReloadDocument() as ReloadDocPtr
	dim ret as ReloadDocPtr
	
	ret = new ReloadDoc
	
	ret->version = 1
	ret->root = null
	
	return ret
End function

Function CreateReloadNode(doc as ReloadDocPtr, nam as string) as ReloadNodePtr
	dim ret as ReloadNodePtr
	
	if doc = null or nam = "" then return null
	
	ret = new ReloadNode
	
	ret->doc = doc
	ret->name = nam
	ret->nodeType = 0
	ret->numChildren = 0
	ret->children = null
	
	return ret
End function

sub FreeReloadNode(nod as ReloadNodePtr)
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

sub FreeReloadDocument(doc as ReloadDocPtr)
	if doc = null then return
	
	if doc->root then
		FreeReloadNode(doc->root)
		doc->root = null
	end if
	
	delete doc
end sub

sub ReloadSetContent (nod as ReloadNodePtr, dat as string)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeReloadNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	nod->nodeType = rliString
	nod->str = dat
end sub

sub ReloadSetContent(nod as ReloadNodePtr, dat as longint)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeReloadNode(nod->Children)
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

sub ReloadSetContent(nod as ReloadNodePtr, dat as double)
	if nod = null then exit sub
	if nod->nodeType = rliChildren then
		'we need to free the children
		FreeReloadNode(nod->Children)
		nod->Children = null
		nod->NumChildren = 0
	end if
	nod->nodeType = rliFloat
	nod->flo = dat
end sub

Sub ReloadRemoveParent(nod as ReloadNodePtr)
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

function ReloadAddChild(par as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr
	
	if verifyNodeLineage(nod, par) = NO then return nod
	
	'first, remove us from our old parent
	ReloadRemoveParent(nod)
	
	'next, add us to our new parent
	if par then
		nod->parent = par
		par->numChildren += 1
		
		par->nodeType = rliChildren
		
		if par->children = null then
			par->children = nod
		else
			dim s as ReloadNodePtr
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

function ReloadAddSiblingAfter(sib as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr

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

function ReloadAddSiblingBefore(sib as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr

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

sub ReloadDocSetRootNode(doc as ReloadDocPtr, nod as ReloadNodePtr)
	if verifyNodeLineage(nod, doc->root) = YES and verifyNodeLineage(doc->root, nod) = YES then
		FreeReloadNode(doc->root)
	end if
	
	doc->root = nod
	
end sub

sub SerializeXML (doc as ReloadDocPtr)
	if doc = null then exit sub
	
	serializeXML(doc->root)
end sub

sub serializeXML (nod as ReloadNodePtr, ind as integer = 0)
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
			dim n as ReloadNodePtr
			n = nod->children
			do while n <> null
				serializeXML(n, ind + 1)
				n = n->nextSib
			loop
	end select
	print string(ind, "  ") & "</" + nod->name + ">"
end sub

sub SerializeBin(doc as ReloadDocPtr)
	if doc = null then exit sub
	
	dim f as integer = freefile
	
	kill "test.rld"
	open "test.rld" for binary as #f
	
	dim i as integer
	put #f, , "RELD"
	i = 1
	put #f, , i
	i = 12
	put #f, , i
	
	serializeBin(doc->root, f)
	
	close #f
end sub

sub serializeBin(nod as ReloadNodePtr, f as integer)
	dim i as integer, us as ushort
	us = 0
	put #f, , us
	put #f, , nod->nodeType
	
	select case nod->nodeType
		case rliNull
			
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
			dim n as ReloadNodePtr
			n = nod->children
			do while n <> null
				serializeBin(n, f)
				n = n->nextSib
			loop
	end select
end sub

