'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "reload.bi"
#include "util.bi"

#ifdef IS_GAME
DECLARE SUB debug (s AS STRING)
#else
#ifdef IS_CUSTOM
DECLARE SUB debug (s AS STRING)
#else
SUB debug (s AS STRING)
 print "debug: " & s
END SUB
#endif
#endif

Namespace Reload

'this checks to see if a node is part of a tree, for example before adding to a new parent
Function verifyNodeLineage(byval nod as NodePtr, byval parent as NodePtr) as integer
	if nod = null then return no
	do while parent <> null
		if nod = parent then return no
		parent = parent->parent
	loop
	return yes
end function

'this checks to see whether a node is part of a given family or not
Function verifyNodeSiblings(byval sl as NodePtr, byval family as NodePtr) as integer
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

'creates and initializes a blank document
Function CreateDocument() as DocPtr
	dim ret as DocPtr
	
	'Holy crap! allocating memory with malloc (and friends), and freeing it with delete?!
	'never, ever do that! In this case, it probably didn't hurt anything, since Doc doesn't
	'have a constructor or destructor. But, if it did... bad things! *shudder*
	' -- Mike, Apr 6, 2010
	' PS: It was me who did this :'(
	
	'ret = Callocate(1, sizeof(Doc))
	ret = New Doc
	
	if ret then
		ret->version = 1
		ret->root = null
	end if
	
	return ret
End function

'creates and initilalizes an empty node with a given name.
'it associates the node with the given document, and cannot be added to another one!
Function CreateNode(byval doc as DocPtr, nam as string) as NodePtr
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

'destroys a node and any children still attached to it.
'if it's still attached to another node, it will be removed from it
sub FreeNode(byval nod as NodePtr)
	if nod = null then
		debug "FreeNode ptr already null"
		exit sub
	end if
	
	dim tmp as NodePtr
	do while nod->children <> 0
		FreeNode(nod->children)
	loop
	
	'If this node has a parent, we should remove this node from
	'its list of children
	if nod->parent then
		dim par as NodePtr = nod->parent
		
		if par->children = nod then
			par->children = nod->nextSib
		end if
		
		par->numChildren -= 1
		
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
		end if
		
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
		end if
	end if
	
	delete nod
end sub

'This frees an entire document, its root node, and any of its children
'NOTE! This does not free any nodes that are not currently attached to the
'root node! Beware!
sub FreeDocument(byval doc as DocPtr)
	if doc = null then return
	
	if doc->root then
		FreeNode(doc->root)
		doc->root = null
	end if
	
	delete doc
end sub

'This marks a node as a string type and sets its data to the provided string
sub SetContent (byval nod as NodePtr, dat as string)
	if nod = null then exit sub
	nod->nodeType = rltString
	nod->str = dat
end sub

'This marks a node as an integer, and sets its data to the provided integer
sub SetContent(byval nod as NodePtr, byval dat as longint)
	if nod = null then exit sub
	nod->nodeType = rltInt
	nod->num = dat
end sub

'This marks a node as a floating-point number, and sets its data to the provided double
sub SetContent(byval nod as NodePtr, byval dat as double)
	if nod = null then exit sub
	nod->nodeType = rltFloat
	nod->flo = dat
end sub

'This marks a node as a null node. It leaves the old data (but it's no longer accessible)
sub SetContent(byval nod as NodePtr)
	if nod = null then exit sub
	nod->nodeType = rltNull
end sub

'This removes a node from its parent node (eg, pruning it)
'It updates its parent and siblings as to their new relatives
Sub RemoveParent(byval nod as NodePtr)
	if nod->parent then
		'if we are the first child of the parent, special case!
		if nod->parent->children = nod then
			nod->parent->children = nod->nextSib
		end if
		
		'disown our parent
		nod->parent->numChildren -= 1
		nod->parent = null
		
		'update our brethren
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
			nod->nextSib = null
		end if
		
		'them too
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
			nod->prevSib = null
		end if
	end if
end sub

'This adds a node as a child to another node, updating their relatives
function AddChild(byval par as NodePtr, byval nod as NodePtr) as NodePtr
	
	'If a node is part of the tree already, we can't add it again
	if verifyNodeLineage(nod, par) = NO then return nod
	
	'first, remove us from our old parent
	RemoveParent(nod)
	
	'next, add us to our new parent
	if par then
		nod->parent = par
		par->numChildren += 1
		
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

'This adds the given node as a sibling *after* another node.
function AddSiblingAfter(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
	
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

'This adds the given node as a sibling *before* another node.
function AddSiblingBefore(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
	
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

'This promotes a node to Root Node status (which, really, isn't that big a deal.)
'NOTE: It automatically frees the old root node (unless it's the same as the new root node)
sub SetRootNode(byval doc as DocPtr, byval nod as NodePtr)
	if doc = null then return
	
	if doc->root = nod then return
	
	if verifyNodeLineage(nod, doc->root) = YES and verifyNodeLineage(doc->root, nod) = YES then
		FreeNode(doc->root)
	end if
	
	doc->root = nod
	
end sub

'Internal function
'Locates a string in the string table. If it's not there, returns -1
Function FindStringInTable(st as string, table() as string) as integer
	if st = "" then return 0
	for i as integer = lbound(table) to ubound(table)
		if table(i) = st then return i + 1
	next
	return -1
end function

'Adds a string to the string table. If it already exists, return the index
'If it doesn't already exist, add it, and return the new index
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

'This traverses a node tree, and gathers all the node names into a string table
sub BuildStringTable(byval nod as NodePtr, table() as string)
	static first as integer, start as NodePtr
	
	if nod = null then exit sub
	
	if first = no then
		redim table(0)
		start = nod
		first = yes
	end if
	
	AddStringToTable(nod->name, table())
	
	dim n as NodePtr
	
	n = nod->children
	do while n <> 0
		BuildStringTable(n, table())
		n = n->nextSib
	loop
	
	if start = nod then
		first = no
		start = null
	end if
end sub

'Serializes a document as XML to standard out
sub SerializeXML (byval doc as DocPtr)
	if doc = null then exit sub
	
	serializeXML(doc->root)
end sub

'serializes a node as XML to standard out.
'It pretty-prints it by adding indentation.
sub serializeXML (byval nod as NodePtr, byval ind as integer = 0)
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

'This serializes a document as a binary file. This is where the magic happens :)
sub SerializeBin(file as string, byval doc as DocPtr)
	if doc = null then exit sub
	
	dim f as integer = freefile
	dim table() as string
	
	BuildStringTable(doc->root, table())
	
	'In case things go wrong, we serialize to a temporary file first
	kill file & ".tmp"
	open file & ".tmp" for binary as #f
	
	dim i as uinteger, b as ubyte
	put #f, , "RELD"
	b = 1
	put #f, , b 'version
	i = 13
	put #f, , i 'size of header
	i = 0 
	put #f, , i 'we're going to fill this in later. it is the string table post relative to the beginning of the file.
	
	serializeBin(doc->root, f, table())
	
	i = seek(f) - 1
	put #f, 10, i 'filling in the string table position
	
	seek f, i + 1
	
	dim s as longint
	s = ubound(table) - lbound(table) + 1
	writeVLI(f, s)
	for i = lbound(table) to ubound(table)
		s = len(table(i))
		writeVLI(f, s)
		put #f, , table(i)
	next
	close #f
	
	kill file
	rename file & ".tmp", file
	kill file & ".tmp"
end sub

'This serializes a node to a binary file.
sub serializeBin(byval nod as NodePtr, byval f as integer, table() as string)
	if nod = 0 then
		debug "serializeBin null node ptr"
		exit sub
	end if
	dim i as integer, strno as longint, ub as ubyte
	
	dim as integer siz, here = 0, here2, dif
	siz = seek(f)
	put #f, , here 'will fill this in later, this is node content size
	
	here = seek(f)
	
	strno = FindStringInTable(nod->name, table())
	if strno = -1 then
		debug "failed to find string " & nod->name & " in string table"
		exit sub
	end if
	
	WriteVLI(f, strno)
	
	select case nod->nodeType
		case rltNull
			'Nulls have no data, but convey information by existing or not existing.
			'They can also have children.
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
			WriteVLI(f, len(nod->str))
			put #f, , nod->str
			
	end select
	
	WriteVLI(f, cast(longint, nod->numChildren)) 'is this cast necessary?
	
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

Function FindChildByName(byval nod as NodePtr, nam as string) as NodePtr
	'recursively searches for a child by name, depth-first
	'can also find self
	if nod = null then return null
	if nod->name = nam then return nod
	dim child as NodePtr
	dim ret as NodePtr
	child = nod->children
	while child <> null
		ret = FindChildByName(child, nam)
		if ret <> null then return ret
		child = child->nextSib
	wend
	return null
End function

Function GetChildByName(byval nod as NodePtr, nam as string) as NodePtr
	'Not recursive!
	'does not find self.
	if nod = null then return null
	dim child as NodePtr
	dim ret as NodePtr
	child = nod->children
	while child <> null
		if child->name = nam then return child
		child = child->nextSib
	wend
	return null
End Function

'Loads a node from a binary file, into a document
Function LoadNode(f as integer, byval doc as DocPtr) as NodePtr
	dim ret as NodePtr
	
	ret = CreateNode(doc, "!") '--the "!" indicates no tag name has been loaded for this node yet
	
	dim size as integer
	
	dim as integer here, here2
	get #f, , size
	
	here = seek(f)
	
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
			dim mysize as integer
			'get #f, , size
			mysize = cint(ReadVLI(f))
			ret->str = string(mysize, " ")
			get #f, , ret->str
			ret->nodeType = rltString
		case else
			debug "unknown node type " & ret->nodeType
			delete ret
			return null
	end select
	
	
	
	dim nod as nodeptr
	
	ret->numChildren = ReadVLI(f)
	
	for i as integer = 0 to ret->numChildren - 1
		nod = LoadNode(f, doc)
		if nod = null then
			freenode(ret)
			debug "child " & i & " node load failed"
			return null
		end if
		ret->numChildren -= 1
		AddChild(ret, nod)
	next
	
	if seek(f) - here <> size then
		freenode(ret)
		debug "GOSH-diddly-DARN-it! Why did we read " & (seek(f) - here) & " bytes instead of " & size & "!?"
		return null
	end if
	
	return ret
End Function

'This loads the string table from a binary document (as if the name didn't clue you in)
'Please, please make sure you pass a dynamic array as the table :(
Sub LoadStringTable(f as integer, table() as string)
	dim as uinteger count, size
	
	count = cint(ReadVLI(f))
	'get #f, , count
	
	if count <= 0 then exit sub
	
	redim preserve table(count - 1) 'why on earth was this INSIDE the loop?!
	                                'why not? ;) --Mike
	
	for i as integer = 0 to count - 1
		size = cint(ReadVLI(f))
		'get #f, , size
		table(i) = string(size, " ")
		if size > 0 then
			get #f, , table(i)
		end if
	next
end sub

'After loading a binary document, the in-memory nodes don't have names, only numbers represting entries
'in the string table. This function fixes that by copying out of the string table
function FixNodeName(byval nod as nodeptr, table() as string) as integer
	if nod = null then return -1
	
	if nod->namenum > ubound(table) + 1 or nod->namenum < 0 then
		return -1
	end if
	
	if nod->namenum > 0 then
		nod->name = table(nod->namenum - 1)
	else
		nod->name = ""
	end if
	
	dim tmp as nodeptr = nod->children
	do while tmp <> null
		FixNodeName(tmp, table())
		tmp = tmp->nextSib
	loop
	
	return 0
end function

Function LoadDocument(fil as string) as DocPtr
	dim ret as DocPtr
	dim f as integer = freefile
	
	if open(fil, for binary, as #f) then
		debug "failed to open file " & fil
		return null
	end if
	
	dim as ubyte ver
	dim as integer headSize, datSize
	dim as string magic = "    "
	
	dim b as ubyte, i as integer
	
	get #f, , magic
	
	if magic <> "RELD" then
		close #f
		debug "No RELD magic"
		return null
	end if
	
	get #f, , ver
	
	select case ver
		case 1 ' no biggie
			get #f, , headSize
			
			if headSize <> 13 then 'uh oh, the header is the wrong size
				close #f
				debug "Reload header is " & headSize & "instead of 13"
				return null
			end if
			
			get #f, , datSize
			
		case else ' dunno. Let's quit.
			close #f
			debug "Reload version " & ver & " not supported"
			return null
	end select
	
	'if we got here, the document is... not yet corrupt. I guess.
	
	ret = CreateDocument()
	ret->version = ver
	
	ret->root = LoadNode(f, ret)
	
	'Is is possible to serialize a null root? I mean, I don't know why you would want to, but...
	'regardless, if it's null here, it's because of an error
	if ret->root = null then
		close #f
		delete ret
		return null
	end if
	
	'now, we load the string table
	dim table() as string
	
	LoadStringTable(f, table())
	
	'String table: Apply directly to the document tree
	'String table: Apply directly to the document tree
	'String table: Apply directly to the document tree
	FixNodeName(ret->root, table())
	
	close #f
	
	return ret
End Function

'This returns a node's content in string form.
Function GetString(byval node as nodeptr) as string
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
		case else
			return "Unknown value: " & node->nodeType
	end select
End Function

'This returns a node's content in integer form. If the node is a string, and the string
'does not represent an integer of some kind, it will likely return 0.
'Also, null nodes are worth 0
Function GetInteger(byval node as nodeptr) as LongInt
	if node = null then return 0
	
	select case node->nodeType
		case rltInt
			return node->num
		case rltFloat
			return clngint(node->flo)
		case rltNull
			return 0
		case rltString
			return cint(node->str)
		case else
			return 0
	end select
End Function

'This returns a node's content in floating point form. If the node is a string, and the string
'does not represent a number of some kind, it will likely return 0.
'Also, null nodes are worth 0
Function GetFloat(byval node as nodeptr) as Double
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
		case else
			return 0.0
	end select
End Function

'Sets the child node of name n to a null value. If n doesn't exist, it adds it
Function SetChildNode(parent as NodePtr, n as string) as NodePtr
	if parent = 0 then return 0
	
	'first, check to see if this node already exists
	dim ret as NodePtr = GetChildByName(parent, n)
	
	'it doesn't, so add a new one
	if ret = 0 then
		ret = CreateNode(parent->doc, n)
		AddChild(parent, ret)
	end if
	
	SetContent(ret)
	
	return ret
end Function

'Sets the child node of name n to an integer value. If n doesn't exist, it adds it
Function SetChildNode(parent as NodePtr, n as string, val as longint) as NodePtr
	if parent = 0 then return 0
	
	'first, check to see if this node already exists
	dim ret as NodePtr = GetChildByName(parent, n)
	
	'it doesn't, so add a new one
	if ret = 0 then
		ret = CreateNode(parent->doc, n)
		AddChild(parent, ret)
	end if
	
	SetContent(ret, val)
	
	return ret
end Function

'Sets the child node of name n to a floating point value. If n doesn't exist, it adds it
Function SetChildNode(parent as NodePtr, n as string, val as double) as NodePtr
	if parent = 0 then return 0
	
	'first, check to see if this node already exists
	dim ret as NodePtr = GetChildByName(parent, n)
	
	'it doesn't, so add a new one
	if ret = 0 then
		ret = CreateNode(parent->doc, n)
		AddChild(parent, ret)
	end if
	
	SetContent(ret, val)
	
	return ret
end Function

'Sets the child node of name n to a string value. If n doesn't exist, it adds it
Function SetChildNode(parent as NodePtr, n as string, val as string) as NodePtr
	if parent = 0 then return 0
	
	'first, check to see if this node already exists
	dim ret as NodePtr = GetChildByName(parent, n)
	
	'it doesn't, so add a new one
	if ret = 0 then
		ret = CreateNode(parent->doc, n)
		AddChild(parent, ret)
	end if
	
	SetContent(ret, val)
	
	return ret
end Function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeInt(parent as NodePtr, n as string, d as longint) as longint
	if parent = 0 then return d
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetInteger(nod) 'yes, I realize I don't check for null. GetInteger does, though.
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeFloat(parent as NodePtr, n as string, d as double) as Double
	if parent = 0 then return d
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	
	return GetFloat(nod) 'yes, I realize I don't check for null. GetInteger does, though.
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeStr(parent as NodePtr, n as string, d as string) as string
	if parent = 0 then return d
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	
	return GetString(nod) 'yes, I realize I don't check for null. GetInteger does, though.
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeBool(parent as NodePtr, n as string, d as integer) as integer
	if parent = 0 then return d
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d <> 0
	
	return GetInteger(nod) <> 0 'yes, I realize I don't check for null. GetInteger does, though.
end function

'looks for a child node of the name n, and returns whether it finds it or not. For "flags", etc
Function GetChildNodeExists(parent as NodePtr, n as string) as integer
	if parent = 0 then return 0
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	return nod <> 0
end function

Function RPathCompile(query as string) as RPathCompiledQuery Ptr
	dim tok() as string
	split(trim(query), tok(), "/")
	
	if ubound(tok) = 0 AND tok(0) = "" then
		return 0
	end if
	
	dim ret as RPathCompiledQuery Ptr = new RPathCompiledQuery
	
	if ret = 0 then return 0
	
	ret->numFragments = ubound(tok) + 1
	ret->fragment = new RPathFragment[ret->numFragments]
	
	if ret->fragment = 0 then
		delete ret
		return 0
	end if
	
	for i as integer = 0 to ubound(tok)
		print tok(i)
		
		ret->fragment[i].nodename = tok(i)
		
	next
	
	return ret
End Function

sub RPathFreeCompiledQuery(byval rpf as RPathCompiledQuery ptr)
	if rpf = 0 then exit sub
	
	if rpf->fragment then
		delete[] rpf->fragment
		rpf->fragment = 0
	end if
	
	delete rpf
end sub

Function RPathSearch(byval query as RPathCompiledQuery ptr, byval depth as integer, byval from as NodePtr, results() as nodePtr) as integer
	if from = 0 or query = 0 then
		print "from: " ; from; " query: " ; query
		return 0
	end if
	
	dim found as integer = 0
	
	if depth >= query->numFragments or depth < 0 then return 0
	
	if from->name = query->fragment[depth].nodename then
		if depth = query->numFragments - 1 then
			if results(0) = null then
				redim results(0)
				results(0) = from
				'for i as integer = lbound(results) to ubound(results)
				'	print i, results(i)
				'next
				'print
			else
				redim preserve results(ubound(results) + 1)
				'print from
				results(ubound(results)) = from
				'for i as integer = lbound(results) to ubound(results)
				'	print i, results(i)
				'next
				'print
			end if
			found += 1
		else
			dim n as nodeptr = from->Children
			do while n <> null
				found += RPathSearch(query, depth + 1, n, results())
				n = n->nextSib
			loop
		end if
	end if
	
	dim n as nodeptr = from->Children
	do while n <> null
		found += RPathSearch(query, 0, n, results())
		n = n->nextSib
	loop
	
	return found
End Function

Function RPathQuery(byval query as RPathCompiledQuery Ptr, byval context as NodePtr) as NodeSetPtr
	if query = 0 then return 0
	
	dim ret as NodeSetPtr = new NodeSet
	dim found as integer
	Redim nodes(0) as nodePtr
	
	found = RPathSearch(query, 0, context, nodes())
	
	print "found: " ; found
	
	if found > 0 then
		ret->nodes = new NodePtr[found]
		
		for i as integer = 0 to found - 1
			ret->nodes[i] = nodes(i)
			'print ret->nodes[i], nodes(i)
		next
		
		ret->numNodes = found
		ret->doc = context->doc
		
		return ret
	else
		delete ret
		return null
	end if
end Function

Function RPathQuery(query as String, byval context as NodePtr) as NodeSetPtr
	dim rpf as RPathCompiledQuery ptr = RPathCompile(query)
	dim ret as NodeSetPtr = RPathQuery(rpf, context)
	RPathFreeCompiledQuery(rpf)
	return ret
End Function

sub FreeNodeSet(byval nodeset as NodeSetPtr)
	if nodeset = null then exit sub
	
	if nodeset->nodes then
		delete[] nodeset->nodes
	end if
	
	delete nodeset
end sub

'This writes an integer out in such a fashion as to minimize the number of bytes used. Eg, 36 will
'be stored in one byte, while 365 will be stored in two, 10000 in three bytes, etc
Sub WriteVLI(byval f as integer, byval v as Longint)
	dim o as ubyte
	dim neg as integer = 0
	
	if o < 0 then
		neg = yes
		v = abs(v)
	end if
	
	o = v and &b111111 'first, extract the low six bits
	v = v SHR 6
	
	if neg then   o OR=  &b1000000 'bit 6 is the "number is negative" bit
	
	if v > 0 then o OR= &b10000000 'bit 7 is the "omg there's more data" bit
	
	put #f, , o
	
	do while v > 0
		o = v and &b1111111 'extract the next 7 bits
		v = v SHR 7
		
		if v > 0 then o OR= &b10000000
		
		put #f, , o
	loop

end sub

'This reads the number back in again
function ReadVLI(byval f as integer) as longint
	dim o as ubyte
	dim ret as longint = 0
	dim neg as integer = 0
	dim bit as integer = 0
	
	get #f, , o
	
	if o AND &b1000000 then neg = yes
	
	ret OR= (o AND &b111111) SHL bit
	bit += 6
	
	do while o AND &b10000000
		get #f, , o
		
		ret OR= (o AND &b1111111) SHL bit
		bit += 7
	loop
	
	if neg then ret *= -1
	
	return ret
	
end function


End Namespace
