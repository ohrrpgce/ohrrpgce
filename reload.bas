'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#define RELOADINTERNAL

'if you find yourself debugging heap issues, define this. If the crashes go away, then I (Mike Caron)
'somehow fscked up the private heap implementation. Or, someone else touched something without
'understanding how it works...

'#define RELOAD_NOPRIVATEHEAP

#include "reload.bi"
#include "reloadext.bi"
#include "util.bi"
#include "cutil.bi"
#include "lumpfile.bi"
#include "common_base.bi"

#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
        include_windows_bi()
#endif

Namespace Reload

Type hashFunction as Function(byval k as ZString ptr) as uinteger

'These are in addition to the 'f as integer' overloads in reload.bi
Declare Function ReadVLI(byval f as FILE ptr) as longint
'Can add the FILE* overload back when you actually need it...
Declare Sub WriteVLI(byval f as BufferedFile ptr, byval v as Longint)

Declare Function AddStringToTable (st as string, byval doc as DocPtr) as integer
Declare Function FindStringInTable overload(st as string, byval doc as DocPtr) as integer

Declare Function CreateHashTable(byval doc as Docptr, byval hashFunc as hashFunction, byval b as integer = 65) as Hashptr
Declare Sub DestroyHashTable(byval h as HashPtr)
Declare Function FindItem(byval h as HashPtr, byval key as ZString ptr, byval num as integer = 1) as any ptr
Declare Sub AddItem(byval h as HashPtr, byval key as ZString ptr, byval item as any ptr)
Declare Sub RemoveKey(byval h as HashPtr, byval key as zstring ptr, byval num as integer = 1)


'I am aware of the hash table implementation in util.bas. However, this is tuned
'for this purpose. Plus, I want everything contained on the private heap (if applicable)
Type ReloadHashItem
	key as zstring ptr 
	item as any ptr 'this doesn't have to be a pointer...
	nxt as ReloadHashItem ptr
End Type

Type ReloadHash
	bucket as ReloadHashItem ptr ptr
	numBuckets as uinteger
	numItems as uinteger
	doc as DocPtr
	hashFunc as hashFunction
end Type


'===================================================================================================
'= Private Heap abstraction
'= On Windows, we can create a private heap to manage our memory. The advantage is that when the
'= document is eventually freed, we can just nuke the private heap, rather than deallocating
'= everything manually. This is abstracted away 
'===================================================================================================

function RHeapInit(byval doc as docptr) as integer
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	doc->heap = HeapCreate(0, 0, 0)
	return doc->heap <> 0
#else
	'nothing, use the default heap
	return 1
#endif
end function

Function RHeapDestroy(byval doc as docptr) as integer
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	HeapDestroy(doc->heap) 'poof
	doc->heap = 0
	return 0
#else
	'they need to free memory manually
	return 1
#endif
end function

Function RCallocate(byval s as integer, byval doc as docptr) as any ptr
	dim ret as any ptr
	
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	ret = HeapAlloc(doc->heap, HEAP_ZERO_MEMORY, s)
#else
	ret = CAllocate(s)
#endif
	
	return ret
end function

Function RReallocate(byval p as any ptr, byval doc as docptr, byval newsize as integer) as any ptr
	dim ret as any ptr
	
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	ret = HeapReAlloc(doc->heap, HEAP_ZERO_MEMORY, p, newsize)
#else
	ret = Reallocate(p, newsize)
#endif
	
	return ret
end function

Sub RDeallocate(byval p as any ptr, byval doc as docptr)
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	HeapFree(doc->heap, 0, p)
#else
	Deallocate(p)
#endif
End Sub

'Fed a list of 132010 english words, this produced 131957 unique hashes.
'The old hash produced only 1931 unique hashes
'If this changes, reload_HashZString in reloadbasic.py needs to be updated too
Function HashZString(byval st as ZString ptr) as uinteger
	dim as uinteger ret = 0, i = 0
	
	do while st[i]
		ret += (ret shl 15) + *cast(short ptr, @st[i])
		if st[i + 1] = 0 then return ret
		i += 2
	loop
	
	return ret
end function

'creates and initializes a blank document
Function CreateDocument() as DocPtr
	dim ret as DocPtr
	
	'Holy crap! allocating memory with malloc (and friends), and freeing it with delete?!
	'never, ever do that! In this case, it probably didn't hurt anything, since Doc doesn't
	'have a constructor or destructor. But, if it did... bad things! *shudder*
	' -- Mike, Apr 6, 2010
	' PS: It was me who did this :'(
	
	ret = New Doc
	
	if ret then
		if 0 = RHeapInit(ret) then
			debug "Unable to create heap on Document :("
			delete ret
			return null
		end if
		ret->version = 1
		ret->root = null
		
		'The initial string table has one entry: ""
		ret->strings = RCallocate(sizeof(StringTableEntry), ret)
		ret->strings[0].str = RCallocate(1, ret)
		*ret->strings[0].str = "" 'this is technically redundant.
		ret->numStrings = 1
		ret->numAllocStrings = 1
		ret->stringHash = CreateHashTable(ret, @HashZString)
		ret->delayLoading = no
		ret->nameIndexTable = NULL
		ret->nameIndexTableLen = 0
		
		'add the blank string to the hash
		AddItem(ret->stringHash, ret->strings[0].str, cast(any ptr, 0))
	end if
	
	return ret
End function

'creates and initilalizes an empty node with a given name.
'it associates the node with the given document, and cannot be added to another one!
Function CreateNode(byval doc as DocPtr, nam as string) as NodePtr
	dim ret as NodePtr
	
	if doc = null then return null
	
	ret = RCallocate(sizeof(Node), doc)
	
	ret->doc = doc
	
	ret->namenum = AddStringToTable(nam, doc)
	
	ret->name = doc->strings[ret->namenum].str
	doc->strings[ret->namenum].uses += 1
	
	ret->nodeType = rltNull
	ret->numChildren = 0
	ret->children = null
	ret->lastChild = null
	ret->flags = 0
	
	return ret
End function

Function CreateNode(byval nod as NodePtr, nam as string) as NodePtr
	return CreateNode(nod->doc, nam)
end function

'FIXME: the old name is never freed
sub RenameNode(byval nod as NodePtr, newname as string)
	nod->namenum = AddStringToTable(newname, nod->doc)
	
	nod->name = nod->doc->strings[nod->namenum].str
	nod->doc->strings[nod->namenum].uses += 1
end sub

'Efficiently free the children of a node
sub FreeChildren(byval nod as NodePtr)
	if nod = NULL then
		debug "FreeChildren ptr already null"
		exit sub
	end if

	if 0 = (nod->flags and nfNotLoaded) then
		dim as NodePtr child = nod->children, nextchild
		do while child <> NULL
			nextchild = child->nextSib
			child->parent = NULL
			FreeNode(child)
			child = nextchild	
		loop
		nod->numChildren = 0
		nod->children = NULL
		nod->lastChild = NULL
	else
		'FIXME: what's the best thing to do if the children aren't loaded?
		nod->flags and= not nfNotLoaded
		nod->numChildren = 0
	end if
end sub

'destroys a node and any children still attached to it.
'if it's still attached to another node, it will be removed from it
'The purpose of the 'options' parameter is a mystery, and it's never used.
'FIXME: the old name is never freed
sub FreeNode(byval nod as NodePtr, byval options as integer)
	if nod = null then
		debug "FreeNode ptr already null"
		exit sub
	end if

	FreeChildren(nod)
	
	'If this node has a parent, we should remove this node from
	'its list of children
	if nod->parent <> 0 and (options and 1) = 0 then
		dim par as NodePtr = nod->parent
		
		if par->children = nod then
			par->children = nod->nextSib
		end if
		if par->lastChild = nod then
			par->lastChild = nod->prevSib
		end if
		
		par->numChildren -= 1
		
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
		end if
		
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
		end if
	end if
	if (options and 1) = 0 then
		if nod->nodeType = rltString and nod->str <> 0 then RDeallocate(nod->str, nod->doc)
		RDeallocate(nod, nod->doc)
	end if
end sub

'This frees an entire document, its root node, and any of its children
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
'NOTE: this frees ALL nodes that were ever attached to this document!
#else
'NOTE! This does not free any nodes that are not currently attached to the
'root node! Beware!
#endif
sub FreeDocument(byval doc as DocPtr)
	if doc = null then return
	
	if doc->fileHandle then
		'debuginfo "reload: closing file " & doc->fileName
		fclose(doc->fileHandle)
	end if
	
	RDeallocate(doc->nameIndexTable, doc)
	'RDeallocate(doc->nameIndexTableBits, doc)
	RDeallocate(doc->RBFuncBits, doc)

	if RHeapDestroy(doc) then
		if doc->root then
			FreeNode(doc->root)
			doc->root = null
		end if
		
		if doc->strings then
			for i as integer = 0 to doc->numAllocStrings - 1
				if doc->strings[i].str then
					RDeallocate(doc->strings[i].str, doc)
				end if
			next
			RDeallocate(doc->strings, doc)
			doc->strings = null
		end if
		
		if doc->stringHash then
			DestroyHashTable(doc->stringHash)
			doc->stringHash = null
		end if
	end if

	'regardless of what heap is in use, doc is on the default heap
	delete doc
end sub

'Loads a node from a binary file, into a document
'If force_recurse is true, load recursively even if document marked for delayed loading.
'
Function LoadNode(byval f as FILE ptr, byval doc as DocPtr, byval force_recursive as bool) as NodePtr
	dim ret as NodePtr
	
	dim size as integer
	
	dim as integer here, here2
	fread(@size, 4, 1, f)
	
	here = ftell(f)
	
	ret = CreateNode(doc, "")
	
	ret->namenum = cshort(ReadVLI(f))
	
	if ret->namenum < 0 or ret->namenum >= doc->numStrings then
		debug "Node has invalid name: #" & ret->namenum
		ret->namenum = 0
	else
		'debug "Node has valid name: #" & ret->namenum & " " & *doc->strings[ret->namenum].str
		ret->name = doc->strings[ret->namenum].str
		doc->strings[ret->namenum].uses += 1
	end if
	
	ret->nodetype = fgetc(f)
	
	select case ret->nodeType
		case rliNull
		case rliByte
			ret->num = cbyte(fgetc(f))
			ret->nodeType = rltInt
		case rliShort
			dim s as short
			fread(@s, 2, 1, f)
			ret->num = s
			ret->nodeType = rltInt
		case rliInt
			dim i as integer
			fread(@i, 4, 1, f)
			ret->num = i
			ret->nodeType = rltInt
		case rliLong
			fread(@(ret->num), 8, 1, f)
			ret->nodeType = rltInt
		case rliFloat
			fread(@(ret->flo), 8, 1, f)
			ret->nodeType = rltFloat
		case rliString
			dim mysize as integer
			ret->strSize = cint(ReadVLI(f))
			ret->str = RCallocate(ret->strSize + 1, doc)
			fread(ret->str, 1, ret->strSize, f)
			ret->nodeType = rltString
		case else
			debug "unknown node type " & ret->nodeType
			FreeNode(ret)
			return null
	end select
	
	dim nod as nodeptr
	
	ret->numChildren = ReadVLI(f)
	
	if doc->delayLoading and force_recursive = NO then
		ret->fileLoc = ftell(f)
		ret->flags OR= nfNotLoaded
		
		fseek(f, size + here, 0)
	else
		for i as integer = 0 to ret->numChildren - 1
			nod = LoadNode(f, doc, force_recursive)
			if nod = null then
				FreeNode(ret)
				debug "LoadNode: node @" & here & " child " & i & " node load failed"
				return null
			end if
			ret->numChildren -= 1
			AddChild(ret, nod)
		next
		
		if ftell(f) - here <> size then
			FreeNode(ret)
			debug "GOSH-diddly-DARN-it! Why did we read " & (ftell(f) - here) & " bytes instead of " & size & "!?"
			return null
		end if
	end if
	
	return ret
End Function

'This loads a node's children if loading has been delayed, either recursively or not, returning success
'Note: won't do a recursive load if the node is loaded already but its child aren't, so you will have to
'call LoadNode before the node's children are first accessed!
Function LoadNode(byval ret as nodeptr, byval recursive as bool = YES) as bool
	if ret = null then return no
	if (ret->flags AND nfNotLoaded) = 0 then return yes
	
	dim f as FILE ptr = ret->doc->fileHandle
	
	fseek(f, ret->fileLoc, 0)
	
	for i as integer = 0 to ret->numChildren - 1
		dim nod as nodeptr = LoadNode(f, ret->doc, recursive)
		if nod = null then
			debug "LoadNode: node @" & ret->fileLoc & " child " & i & " node load failed"
			return no
		end if
		ret->numChildren -= 1
		AddChild(ret, nod)
	next
	
	ret->flags AND= NOT nfNotLoaded
	
	return yes
End Function

'This loads the string table from a binary document (as if the name didn't clue you in)
Sub LoadStringTable(byval f as FILE ptr, byval doc as docptr)
	dim as uinteger count, size
	
	count = cint(ReadVLI(f))
	
	if count <= 0 then exit sub
	
	for i as integer = 1 to doc->numAllocStrings - 1
		if doc->strings[i].str then RDeallocate(doc->strings[i].str, doc)
	next
	
	doc->strings = RReallocate(doc->strings, doc, (count + 1) * sizeof(StringTableEntry))
	doc->numStrings = count + 1
	doc->numAllocStrings = count + 1
	
	for i as integer = 1 to count
		size = cint(ReadVLI(f))
		'get #f, , size
		doc->strings[i].str = RCallocate(size + 1, doc)
		dim zs as zstring ptr = doc->strings[i].str
		if size > 0 then
			fread(zs, 1, size, f)
		end if
		
		AddItem(doc->stringHash, doc->strings[i].str, cast(any ptr, i))
	next
end sub

Function LoadDocument(fil as string, byval options as LoadOptions) as DocPtr
	dim starttime as double = timer
	dim ret as DocPtr
	dim f as FILE ptr
	
	f = fopen(fil, "rb")
	if f = 0 then
		debug "failed to open file " & fil
		return null
	end if
	
	dim as ubyte ver
	dim as integer headSize, datSize
	dim as string magic = "    "
	
	dim b as ubyte, i as integer
	
	fread(strptr(magic), 1, 4, f)
	
	if magic <> "RELD" then
		fclose(f)
		debug "Failed to load " & fil & ": No magic RELD signature"
		return null
	end if
	
	ver = fgetc(f)
	
	select case ver
		case 1 ' no biggie
			fread(@headSize, 4, 1, f)
			if headSize <> 13 then 'uh oh, the header is the wrong size
				fclose(f)
				debug "Failed to load " & fil & ": Reload header is " & headSize & "instead of 13"
				return null
			end if
			
			fread(@datSize, 4, 1, f)
			
		case else ' dunno. Let's quit.
			fclose(f)
			debug "Failed to load " & fil & ": Reload version " & ver & " not supported"
			return null
	end select
	
	'if we got here, the document is... not yet corrupt. I guess.
	
	ret = CreateDocument()
	ret->version = ver
	'ret->fileName = fil
	'debuginfo "reload: opened " & fil
	
	if options and optNoDelay then
		ret->delayLoading = no
	else
		ret->delayLoading = yes
		ret->fileHandle = f
	end if
	
	'We'll load the string table first, to assist in debugging.
	
	fseek(f, datSize, 0)
	LoadStringTable(f, ret)
	
	fseek(f, headSize, 0)
	
	ret->root = LoadNode(f, ret, NO)
	
	'Is it possible to serialize a null root? I mean, I don't know why you would want to, but...
	'regardless, if it's null here, it's because of an error
	if ret->root = null then
		fclose(f)
		FreeDocument(ret)
		return null
	end if
	
	if options and optNoDelay then
		fclose(f)
	end if
	debug_if_slow(starttime, 0.1, fil)
	return ret
End Function

'Internal function
'Locates a string in the string table. If it's not there, returns -1
Function FindStringInTable (st as string, byval doc as DocPtr) as integer
	'if st = "" then return 0
	'for i as integer = 0 to doc->numStrings - 1
	'	if *doc->strings[i].str = st then return i
	'next
	
	if st = "" then return 0
	
	dim ret as integer = cint(FindItem(doc->stringhash, st))
	
	if ret = 0 then return -1
	return ret
end function

'Adds a string to the string table. If it already exists, return the index
'If it doesn't already exist, add it, and return the new index
Function AddStringToTable(st as string, byval doc as DocPtr) as integer
	dim ret as integer
	
	ret = cint(FindStringInTable(st, doc))
	
	if ret <> -1 then
		return ret
	end if
	
	if doc->numAllocStrings = 0 then 'This should never run.
		debugc errBug, "ERROR! Unallocated string table!"
		doc->strings = RCallocate(16 * sizeof(StringTableEntry), doc)
		doc->numAllocStrings = 16
		
		doc->strings[0].str = RCallocate(1, doc)
		*doc->strings[0].str = ""
	end if
	
	if doc->numStrings >= doc->numAllocStrings then 'I hope it's only ever equals...
		dim s as StringTableEntry ptr = RReallocate(doc->strings, doc, sizeof(StringTableEntry) * (doc->numAllocStrings * 2))
		if s = 0 then 'panic
			debugc errPromptBug, "Error resizing string table"
			return -1
		end if
		for i as integer = doc->numAllocStrings to doc->numAllocStrings * 2 - 1
			s[i].str = 0
			s[i].uses = 0
		next
		
		doc->strings = s
		doc->numAllocStrings = doc->numAllocStrings * 2
	end if
	
	
	doc->strings[doc->numStrings].str = RCallocate(len(st) + 1, doc)
	*doc->strings[doc->numStrings].str = st
	
	AddItem(doc->stringHash, doc->strings[doc->numStrings].str, cast(any ptr, doc->numStrings))
	
	doc->numStrings += 1
	
	return doc->numStrings - 1
end function

'RELOADBASIC internal function
sub BuildNameIndexTable(byval doc as DocPtr, nodenames() as RBNodeName, byval func_num as integer, byval func_bits_size as integer, byval signature as integer, byval total_num_names as integer)
	'debug "BuildNameIndexTable, func_num = " & func_num & " doc->numStrings = " & doc->numStrings
	dim allocated_table as bool = NO

	if doc->RBSignature <> signature then
		'We need to clear/recreate the nameIndexTable, and clear the RBFuncBits table,
                'so that all functions will get their nodenames re-added to the table
		doc->RBSignature = signature
		RDeallocate(doc->nameIndexTable, doc)
		'We might add more strings; worst case
		doc->nameIndexTableLen = doc->numStrings + total_num_names
		doc->nameIndexTable = RCallocate(doc->nameIndexTableLen * sizeof(short), doc)
		allocated_table = YES
		'RDeallocate(doc->nameIndexTableBits, doc)
		'doc->nameIndexTableBits = RCallocate(((doc->numStrings + 31) \ 32) * 4, doc)
		RDeallocate(doc->RBFuncBits, doc)
		doc->RBFuncBits = RCallocate(func_bits_size, doc)

		'debug "BuildNameIndexTable(signature=" & signature & ", func_num=" & func_num & ", doc=" & doc & "): creating new table, size=" & doc->nameIndexTableLen
	end if

	'Optimisation: If this function's nodenames table has been built before, skip
	if doc->RBFuncBits[func_num \ 32] and (1 shl (func_num mod 32)) then exit sub
	doc->RBFuncBits[func_num \ 32] or= (1 shl (func_num mod 32))

	if allocated_table = NO then
		'We might add more strings; worst case size
		doc->nameIndexTableLen = doc->numStrings + total_num_names
		doc->nameIndexTable = RReallocate(doc->nameIndexTable, doc, doc->nameIndexTableLen * sizeof(short))

		'debug "BuildNameIndexTable(signature=" & signature & ", func_num=" & func_num & ", doc=" & doc & "): updating table, size=" & doc->nameIndexTableLen
	end if

	'memset(@table(0), &hff, sizeof(integer) * doc->numStrings)  'fills with -1

	dim h as HashPtr = doc->stringHash
	for i as integer = 0 to ubound(nodenames)
		with nodenames(i)
			'This is most of FindItem
			dim b as ReloadHashItem ptr = h->bucket[.hash mod h->numBuckets]

			do while b
				if *b->key = *.name then
					doc->nameIndexTable[cast(integer, b->item)] = .nameindex
					'debug "RB: mapping string " & *.name & ", namenum=" & cast(integer, b->item) & " nameidx=" & .nameindex
					continue for
				end if
				b = b->nxt
			loop

			'The string isn't in the table. Add it so that nameIndexTable doesn't
			'become invalid if it is added.
			dim namenum as integer = AddStringToTable(*.name, doc)
			'debug "RB: adding new string " & *.name & ", namenum=" & namenum & " nameidx=" & .nameindex
			doc->nameIndexTable[namenum] = .nameindex
		end with
	next
end sub

Declare sub serializeBin(byval nod as NodePtr, byval f as BufferedFile ptr, byval doc as DocPtr)

'This serializes a document as a binary file. This is where the magic happens :)
sub SerializeBin(file as string, byval doc as DocPtr)
	dim starttime as double = timer
	if doc = null then exit sub
	
	RemoveProvisionalNodes(doc->root)
	'BuildStringTable(doc->root, doc)

	'In case things go wrong, we serialize to a temporary file first
	safekill file & ".tmp"
	
	dim f as BufferedFile ptr
	f = Buffered_open(file & ".tmp")
	
	if f = NULL then
		debug "SerializeBin: Unable to open " & file & ".tmp"
		exit sub
	end if
	
	dim i as uinteger, b as ubyte
	
	Buffered_write(f, @"RELD", 4) 'magic signature
	
	Buffered_putc(f, 1) 'version
	
	i = 13 'the size of the header (i.e., offset to the data)
	Buffered_write(f, @i, 4)
	
	i = 0 'we're going to fill this in later. it is the string table post relative to the beginning of the file.
	Buffered_write(f, @i, 4)
	
	'write out the body
	serializeBin(doc->root, f, doc)
	
	'this is the location of the string table (immediately after the data)
	i = Buffered_tell(f)
	
	Buffered_seek(f, 9)
	Buffered_write(f, @i, 4) 'filling in the string table position
	
	'jump back to the string table
	Buffered_seek(f, i)
	
	'first comes the number of strings
	writeVLI(f, doc->numStrings - 1)
	
	'then, write out each string, size then body
	for i = 1 to doc->numStrings - 1
		dim zs as zstring ptr = doc->strings[i].str
		dim zslen as integer = len(*zs)
		writeVLI(f, zslen)
		Buffered_write(f, zs, zslen)
	next
	Buffered_close(f)

	if doc->fileHandle then
		'In the process of serializing the document, all nodes would have been loaded,
		'therefore we can close the source file.
		'Now it's very likely that we're writing back to the original file, which means
		'that on Windows we have to close this file, otherwise we can't delete it!
		'debuginfo "reload: closing file " & doc->fileName
		fclose(doc->fileHandle)
		doc->fileHandle = NULL
	end if

	safekill file
	if local_file_move(file & ".tmp", file) then
		debug "SerializeBin: could not rename " & file & ".tmp to " & file
		exit sub  'don't delete the data
	end if
	send_lump_modified_msg(file)
	safekill file & ".tmp"
	debug_if_slow(starttime, 0.1, file)
end sub

sub serializeBin(byval nod as NodePtr, byval f as BufferedFile ptr, byval doc as DocPtr)
	if nod = 0 then
		debug "serializeBin null node ptr"
		exit sub
	end if
	dim i as integer, strno as longint, ub as ubyte
	
	'first, if a node isn't loaded, we need to do so.
	if nod->flags AND nfNotLoaded then
		LoadNode(nod, YES)
	end if
	
	dim as integer siz, here = 0, here2, dif
	'siz = seek(f)
	siz = Buffered_tell(f)
	'put #f, , here 'will fill this in later, this is node content size
	Buffered_write(f, @here, 4)
	
	'here = seek(f)
	here = Buffered_tell(f)
	
	'strno = FindStringInTable(nod->name, doc)
	strno = nod->namenum
	if strno = -1 then
		debug "failed to find string " & *nod->name & " in string table"
		exit sub
	end if
	
	WriteVLI(f, strno)
	
	select case nod->nodeType
		case rltNull
			'Nulls have no data, but convey information by existing or not existing.
			'They can also have children.
			ub = rliNull
			Buffered_putc(f, ub)
		case rltInt 'this is good enough, don't need VLI for this
			if nod->num > 2147483647 or nod->num < -2147483648 then
				ub = rliLong
				Buffered_putc(f, ub)
				Buffered_write(f, @(nod->num), 8)
			elseif nod->num > 32767 or nod->num < -32768 then
				ub = rliInt
				Buffered_putc(f, ub)
				i = nod->num
				Buffered_write(f, @i, 4)
			elseif nod->num > 127 or nod->num < -128 then
				ub = rliShort
				Buffered_putc(f, ub)
				dim s as short = nod->num
				Buffered_write(f, @s, 2)
			else
				ub = rliByte
				Buffered_putc(f, ub)
				dim b as byte = nod->num
				Buffered_putc(f, b)
			end if
		case rltFloat
			ub = rliFloat
			Buffered_putc(f, ub)
			Buffered_write(f, @(nod->flo), 8)
		case rltString
			ub = rliString
			Buffered_putc(f, ub)
			WriteVLI(f, nod->strSize)
			Buffered_write(f, nod->str, nod->strSize)
	end select
	
	WriteVLI(f, nod->numChildren)
	dim n as NodePtr
	n = nod->children
	do while n <> null
		serializeBin(n, f, doc)
		n = n->nextSib
	loop
	
	here2 = Buffered_tell(f)
	dif = here2 - here
	Buffered_seek(f, siz)
	Buffered_write(f, @dif, 4)
	Buffered_seek(f, here2)
end sub

'For each provisional node in the given subtree:
'delete if they have no children, or unmark as provisional otherwise
sub RemoveProvisionalNodes(byval nod as NodePtr)
	if nod = null then exit sub
	if nod->flags AND nfProvisional then
		if nod->numChildren = 0 then
			FreeNode(nod)
			exit sub
		else
			nod->flags AND= NOT nfProvisional
		end if
	end if

	dim as NodePtr n, nextn
	n = nod->children
	do while n <> null
		nextn = n->nextSib
		RemoveProvisionalNodes(n)
		n = nextn
	loop
end sub

sub MarkProvisional(byval nod as NodePtr)
	if nod = NULL then
		debug "MarkProvisional null node ptr"
		exit sub
	end if
	nod->flags OR= nfProvisional
end sub

'this private function checks to see if a node is part of a tree, for example before adding to a new parent
Function verifyNodeLineage(byval nod as NodePtr, byval parent as NodePtr) as integer
	if nod = null then return no
	do while parent <> null
		if nod = parent then return no
		parent = parent->parent
	loop
	return yes
end function

'this public function tells if a node has a particular ancestor
Function NodeHasAncestor(byval nod as NodePtr, byval possible_parent as NodePtr) as integer
	if nod = null then return no
	if possible_parent = null then return no
	dim parent as NodePtr = NodeParent(nod)
	do while parent <> null
		if parent = possible_parent then return yes
		parent = NodeParent(parent)
	loop
	return no
end function

'this checks to see whether a node is part of a given family or not
'FIXME: this looks like a slow debug routine to me, why is it used?
'JAMES: sanity checking pointers to prevent horrible crashes is always a good idea, even if slow (PS, I didn't write this function)
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

'This marks a node as a string type and sets its data to the provided string
sub SetContent (byval nod as NodePtr, dat as string)
	if nod = null then exit sub
	if nod->nodeType = rltString then
		if nod->str then RDeallocate(nod->str, nod->doc)
		nod->str = 0
	end if
	nod->nodeType = rltString
	nod->str = RCallocate(len(dat) + 1, nod->doc)
	nod->strSize = len(dat)
	*nod->str = dat
end sub

'This marks a node as a string type and sets its data to the provided binary blob
'Passing zstr = NULL is valid, and result in uninitialised data
sub SetContent(byval nod as NodePtr, byval zstr as zstring ptr, byval size as integer)
	if nod = null then exit sub
	if nod->nodeType = rltString then
		if nod->str then RDeallocate(nod->str, nod->doc)
		nod->str = 0
	end if
	nod->nodeType = rltString
	nod->str = RCallocate(size + 1, nod->doc)
	nod->str[size] = 0
	nod->strSize = size
	if zstr <> NULL andalso size <> 0 then memcpy(nod->str, zstr, size)
end sub

'This marks a node as an integer, and sets its data to the provided integer
sub SetContent(byval nod as NodePtr, byval dat as longint)
	if nod = null then exit sub
	if nod->nodeType = rltString then
		if nod->str then RDeallocate(nod->str, nod->doc)
		nod->str = 0
	end if
	nod->nodeType = rltInt
	nod->num = dat
end sub

'This marks a node as a floating-point number, and sets its data to the provided double
sub SetContent(byval nod as NodePtr, byval dat as double)
	if nod = null then exit sub
	if nod->nodeType = rltString then
		if nod->str then RDeallocate(nod->str, nod->doc)
		nod->str = 0
	end if
	nod->nodeType = rltFloat
	nod->flo = dat
end sub

'This marks a node as a null node. It leaves the old data (but it's no longer accessible*)
'addendum: * - unless it was a string, in which case it's gone.
sub SetContent(byval nod as NodePtr)
	if nod = null then exit sub
	if nod->nodeType = rltString then
		if nod->str then RDeallocate(nod->str, nod->doc)
		nod->str = 0
	end if
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
		'also again, special case!
		if nod->parent->lastChild = nod then
			nod->parent->lastChild = nod->prevSib
		end if
		
		'disown our parent
		nod->parent->numChildren -= 1
		nod->parent = null
		
		'update our brethren
		if nod->nextSib then
			nod->nextSib->prevSib = nod->prevSib
		end if
		
		'them too
		if nod->prevSib then
			nod->prevSib->nextSib = nod->nextSib
		end if
		
		'disown out siblings only after we have connected them to each other
		nod->nextSib = null
		nod->prevSib = null
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
			dim s as NodePtr = par->lastChild
			s->NextSib = nod
			nod->prevSib = s
		end if
		par->lastChild = nod
	end if
	
	return nod
end function

'This adds nod as a sibling *after* another node, sib.
function AddSiblingAfter(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
	
	if sib = DocumentRoot(sib->doc) then return nod 'no siblings for root!
	
	if verifyNodeSiblings(nod, sib) = NO then return nod
	
	if sib = 0 then return nod
	
	'first, remove us from our old parent
	RemoveParent(nod)
	
	nod->prevSib = sib
	nod->nextSib = sib->nextSib
	sib->nextSib = nod
	if nod->nextSib then
		nod->nextSib->prevSib = nod
	else
		sib->parent->lastChild = nod
	end if
	
	nod->parent = sib->parent
	sib->parent->numChildren += 1
	
	return nod
end function

'This adds nod as a sibling *before* another node, sib.
function AddSiblingBefore(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
	
	if sib = DocumentRoot(sib->doc) then return nod 'no siblings for root!
	
	if verifyNodeSiblings(nod, sib) = NO then return nod
	
	if sib = 0 then return nod
	
	'first, remove us from our old parent
	RemoveParent(nod)
	
	nod->nextSib = sib
	nod->prevSib = sib->prevSib
	sib->prevSib = nod
	if nod->prevSib then
		nod->prevSib->nextSib = nod
	else
		sib->parent->children = nod
	end if
	
	nod->parent = sib->parent
	sib->parent->numChildren += 1
	
	return nod
end function

'This promotes a node to Root Node status (which, really, isn't that big a deal.)
'NOTE: It automatically frees the old root node (unless it's the same as the new root node)
'NOTE: the node must not have a parent
sub SetRootNode(byval doc as DocPtr, byval nod as NodePtr)
	if doc = null then return
	
	if doc->root = nod then return

	if nod->parent then return
	
	if verifyNodeLineage(nod, doc->root) = YES and verifyNodeLineage(doc->root, nod) = YES then
		FreeNode(doc->root)
	end if

	if nod->doc <> doc then
		debugc errPromptBug, "SetRootNode: node was created in the context of another RELOAD doc"
	end if

	doc->root = nod
end sub

'This is from xml2reload: is a node representable as a longint?
private function NodeCompressible(byval node as nodeptr) as integer
	if (ValLng(GetString(node)) <> 0 AND ValLng(GetString(node) & "1") <> ValLng(GetString(node))) or GetString(node) = "0" then
		return 1
	elseif (Val(GetString(node)) <> 0 AND Val(GetString(node) & "1") <> Val(GetString(node))) or GetString(node) = "0" then
		return 1
	end if
	return 0
end function

'Check whether a node's content can be represented faithfully in XML. Return value:
' 0 - No encoding needed
' 1 - Lead/trailing whitespace, and if debugging = YES whether type will be lost, eg "" -> null
' 2 - Binary
' 3 - Long string or data, print hash
private function NodeNeedsEncoding(byval node as nodeptr, byval debugging as bool, byval shortform as bool) as integer
	if node = null then return 0

	if node->nodeType <> rltString then
		return 0
	end if

	if shortform and node->nodeType = rltString andalso node->strSize > 300 then
		return 3
	end if

	dim dat as ubyte ptr = node->str
	for i as integer = 0 to node->strSize - 1
		if dat[i] < 32 then
			if dat[i] <> asc(!"\n") and dat[i] <> asc(!"\r") and dat[i] <> asc(!"\t") then return 2
		end if
	next

	dim repr as string = GetString(node)
	if repr <> trim(repr, any !" \t\n\r") then return 1

	if debugging then
		if node->strSize = 0 orelse NodeCompressible(node) then return 1
	end if

	'Will UNIX/DOS newline differences cause problems?

	return 0
end function

'Escape < and & characters in a string
private function EscapeXMLString(s as string) as string
	dim ret as string

	for i as integer = 0 to len(s) - 1
		if s[i] = asc("&") then
			ret += "&amp;"
		elseif s[i] = asc("<") then
			ret += "&lt;"
		else
			ret += chr(s[i])
		end if
	next

	return ret
end function

'Returns a Base64 encoded string, for XML serialization
private function GetBase64EncodedString(byval node as nodeptr) as string
	if node = null orelse node->nodeType <> rltString then return ""

	dim outbuf as zstring ptr
	dim outlen as integer
	outlen = base64_encode_alloc(node->str, node->strSize, @outbuf)
	if outbuf = NULL then
		debug "XML serialization: base64 encoding failure!"
		return ""
	end if
	
	dim ret as string = *outbuf  'This step is inefficient, but so is everything else about going to/from XML
	deallocate outbuf
	return ret
end function

#define INDENTTAB !"\t"

'Serializes a document as XML to a file
sub SerializeXML (byval doc as DocPtr, byval fh as integer, byval debugging as bool = NO, byval shortform as bool = NO)
	if doc = null then exit sub
	
	print #fh, "<?xml version=""1.0"" encoding=""iso-8859-1"" ?>"
	SerializeXML(doc->root, fh, debugging, shortform)
end sub

'serializes a node as XML to a file.
'It pretty-prints it by adding indentation.
'debugging:  If true, then strings are printed so that they will not be optimized when reloaded.
'shortform:  If true, print only hash of long zstrings.
'ind:        Indentation amount.
sub SerializeXML (byval nod as NodePtr, byval fh as integer, byval debugging as bool, byval shortform as bool, byval ind as integer = 0)
	if nod = null then exit sub
	
	if nod->flags AND nfNotLoaded then
		LoadNode(nod, YES)
	end if
	
	dim closetag as integer = YES

	dim needsencoding as integer = NodeNeedsEncoding(nod, debugging, shortform)

	'no-name nodes aren't valid xml
	dim xmlname as string
	if len(*nod->name) = 0 then
		xmlname = "r:_"
	else
		xmlname = *nod->name
	end if
	
	print #fh, string(ind, INDENTTAB);
	if nod->nodeType = rltNull and nod->numChildren = 0 then
		print #fh, "<" & xmlname & " />"
		exit sub

/'  Currently these no-name nodes are eaten by xml2reload (and all but the last are lost), so we never see these
	elseif debugging = NO andalso nod->nodeType <> rltNull andalso nod->numChildren = 0 andalso *nod->name = "" then
		'A no-name node like this is typically created when translating from xml;
		'so hide the tags
		ind -= 1
		closetag = NO
'/
	else
		print #fh, "<" & xmlname;
		
		'find the attribute children and print them
		dim n as NodePtr = nod->children
		do while n <> null
			if n->name[0] = asc("@") then
				print #fh, " " & *(n->name + 1) & "=""";
				print #fh, GetString(n);
				print #fh, """";
			end if
			n = n->nextSib
		loop

		if ind = 0 then
			'This is the root node. Tell the world about the RELOAD namespace
			print #fh, " xmlns:r=""http://hamsterrepublic.com/ohrrpgce/RELOAD""";
		end if

		if needsencoding = 2 then
			print #fh, " r:encoding=""base64""";
		end if

		print #fh, ">";
	end if

	if nod->nodeType <> rltNull then
		dim outstr as string
		if needsencoding = 1 then
			'It makes me sick
			outstr = "<r:ws>" & EscapeXMLString(GetString(nod)) & "</r:ws>"
		elseif needsencoding = 2 then
			outstr = GetBase64EncodedString(nod)
		elseif needsencoding = 3 then
			outstr = "(## ZSTRING length " & nod->strSize & " hash " & hex(stringhash(nod->str, nod->strSize)) & " ##)"
		else
			outstr = EscapeXMLString(GetString(nod))
		end if
		if nod->numChildren = 0 then
			print #fh, outstr;
		else
			'print #fh,
			'print #fh, string(ind + 1, INDENTTAB);
			print #fh, outstr
		end if
	else
		print #fh,
	end if
	
	dim n as NodePtr = nod->children
	
	do while n <> null
		'we've already printed attributes, above
		if n->name[0] <> asc("@") then
			SerializeXML(n, fh, debugging, shortform, ind + 1)
		end if
		n = n->nextSib
	loop
	
	if nod->numChildren <> 0 then print #fh, string(ind, INDENTTAB);
	
	if closetag then
		print #fh, "</" & xmlname & ">"
	else
		print #fh,
	end if
end sub

Function FindChildByName(byval nod as NodePtr, nam as string) as NodePtr
	'recursively searches for a child by name, depth-first
	'can also find self
	if nod = null then return null
	if *nod->name = nam then return nod
	
	if nod->flags AND nfNotLoaded then LoadNode(nod, YES)
	
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

Function GetChildByName(byval nod as NodePtr, byval nam as zstring ptr) as NodePtr
	'Not recursive!
	'does not find self.
	if nod = null then return null
	
	if nod->flags AND nfNotLoaded then LoadNode(nod, NO)
	dim child as NodePtr = nod->children

	if nod->numChildren >= 10 then  'cutoff chosen with reloadtest speed tests
		dim namenum as integer = cast(integer, FindItem(nod->doc->stringhash, nam))

		while child <> null
			if child->namenum = namenum then return child
			child = child->nextSib
		wend
	else
		while child <> null
			if *child->name = *nam then return child
			child = child->nextSib
		wend
	end if
	return null
End Function

'RELOADBASIC internal function
Function GetChildByNameIndex(byval nod as NodePtr, byval nameindex as integer) as NodePtr
	if nod = null then return null
	
	if nod->flags AND nfNotLoaded then LoadNode(nod, NO)

	dim table as short ptr = nod->doc->nameIndexTable
	dim child as NodePtr = nod->children
	while child <> null
		if table[child->namenum] = nameindex then return child
		child = child->nextSib
	wend
	return null
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
			'FB's string assignment will always do a strlen on zstring arguments, so we need to
			'manually copy the data into a string, in case it is a binary blob containing null bytes
			return blob_to_string(node->str, node->strSize)
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
			return cint(*node->str)
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
			return cdbl(*node->str)
		case else
			return 0.0
	end select
End Function

'This returns a node's content in ZString form (i.e., a blob of data.) If the node
'is not a string already, it will return null.
Function GetZString(byval node as nodeptr) as ZString ptr
	if node = null then return 0
	
	if node->nodeType <> rltString then
		return 0
	end if
	
	return node->str
End Function

Function GetZStringSize(byval node as nodeptr) as integer
	if node = null then return 0
	
	if node->nodeType <> rltString then
		return 0
	end if
	
	return node->strSize
End Function

'This resizes a node's string blob thing. If the node is not a string, it will
'return 0 and not do anything. Otherwise, it will resize it and return the new
'memory location. If it fails, it will return 0.
'If it succeeds, the old pointer is now invalid. Use the new pointer. (I.e., it follows
'the same rules as realloc()!
'Finally, the new memory block will be bigger than newsize by 1 byte. This is for the
'null terminator, in case you're storing an actual string in here. Please try not
'to overwrite it :)
Function ResizeZString(byval node as nodeptr, byval newsize as integer) as ZString ptr
	if node = null then return 0
	
	if node->nodeType <> rltString then
		return 0
	end if
	
	dim n as zstring ptr = node->str
	
	n = RReallocate(n, node->doc, newsize + 1)
	
	if n = 0 then return 0
	
	for i as integer = node->strSize to newsize
		n[i] = 0
	next
	
	node->str = n
	node->strSize = newsize
	
	return n
	
end function

'Return pointer to a child node if it exists, otherwise create it (as a null node)
Function GetOrCreateChild(byval parent as NodePtr, n as string) as NodePtr
	if parent = NULL then return NULL
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	'first, check to see if this node already exists
	dim ret as NodePtr = GetChildByName(parent, n)
	
	'it doesn't, so add a new one
	if ret = NULL then
		ret = CreateNode(parent->doc, n)
		AddChild(parent, ret)
	end if
	
	return ret
end Function

'Sets the child node of name n to a null value (doesn't affect children). If n doesn't exist, it adds it
Function SetChildNode(byval parent as NodePtr, n as string) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
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
Function SetChildNode(byval parent as NodePtr, n as string, byval val as longint) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
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
Function SetChildNode(byval parent as NodePtr, n as string, byval val as double) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
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
Function SetChildNode(byval parent as NodePtr, n as string, val as string) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
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

'Toggle a node to a zero/nonzero value. Create the node if it does not exist
Sub ToggleBoolChildNode(byval parent as NodePtr, n as string)
	if parent = 0 then exit sub
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	'first, check to see if this node already exists
	dim ch as NodePtr = GetChildByName(parent, n)
	
	if ch = 0 then
		'it does not exist, so add it (and toggle it)
		SetChildNode(parent, n, YES)
	else
		SetChildNode(parent, n, NOT GetInteger(ch))
	end if
	
end Sub

'If the child node exists, delete it. If it does not exist, create an empty node
Sub ToggleChildNode(byval parent as NodePtr, n as string)
	if parent = 0 then exit sub
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	'first, check to see if this node already exists
	dim ch as NodePtr = GetChildByName(parent, n)
	
	if ch then
		'it exists, so remove it... to be safe, also check for duplicates
		do
			FreeNode ch
			ch = GetChildByName(parent, n)
			if ch then
				debugc errBug, "ToggleChildNode: unexpected duplicate node: " & Reload.Ext.GetNodePath(parent) & "/" & n
			end if
		loop while ch
	else
		'it does not exist, so add it
		SetChildNode(parent, n)
	end if
end Sub

'If the child node exists, delete it.
Sub FreeChildNode(byval parent as NodePtr, n as string)
	if parent = 0 then exit sub
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	'first, check to see if this node already exists
	dim ch as NodePtr = GetChildByName(parent, n)
	
	if ch then
		'it exists, so remove it
		FreeNode ch
	end if
end Sub


'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeInt(byval parent as NodePtr, n as string, byval d as longint) as longint
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetInteger(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeFloat(byval parent as NodePtr, n as string, byval d as double) as Double
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetFloat(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeStr(byval parent as NodePtr, n as string, d as string) as string
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetString(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeBool(byval parent as NodePtr, n as string, byval d as integer) as integer
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetInteger(nod) <> 0
end function

'looks for a child node of the name n, and returns whether it finds it or not. For "flags", etc
Function GetChildNodeExists(byval parent as NodePtr, n as string) as integer
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	return nod <> 0
end function

'Appends a child node of name n with a null value.
Function AppendChildNode(byval parent as NodePtr, n as string) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr
	ret = CreateNode(parent->doc, n)
	AddChild(parent, ret)
	
	SetContent(ret)
	
	return ret
end Function

'Appends a child node of name n to with integer value.
Function AppendChildNode(byval parent as NodePtr, n as string, byval val as longint) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

'Appends a child node of name n with a floating point value.
Function AppendChildNode(byval parent as NodePtr, n as string, byval val as double) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

'Appends a child node of name n with a string value.
Function AppendChildNode(byval parent as NodePtr, n as string, val as string) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

Function ChildByIndex(byval parent as NodePtr, byval index as integer) as NodePtr
	'Return the index'th child node, or 0 if no such child exists
	'This could be slow for long child lists, so don't use it unless you really need it
	if parent = 0 then return 0
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	dim i as integer
	dim ch as Node Ptr
	ch = parent->children
	do while ch
		if i = index then return ch
		ch = ch->nextsib
		i += 1
	loop
	return 0 ' no child matches index
end Function

Function DocumentRoot(byval doc as DocPtr) as NodePtr
	return doc->root
end Function

Function GetDocument(byval nod as NodePtr) as DocPtr
	return nod->doc
end Function

Function NumChildren(byval nod as NodePtr) as Integer
	return nod->numChildren
end Function

Function NodeParent(byval nod as NodePtr) as NodePtr
	return nod->parent
end Function

Function FirstChild(byval nod as NodePtr, byval withname as zstring ptr = null) as NodePtr
	if nod = null then return null
	if nod->flags AND nfNotLoaded then LoadNode(nod, NO)
	dim ret as NodePtr = nod->children
	if ret = null then return null
	if withname then
		'Could search in the string table for withname first, but you normally
		'expect ret already has the right name
		while ret andalso *ret->name <> *withname
			ret = ret->nextSib
		wend
	end if
	return ret
end Function

Function NextSibling(byval nod as NodePtr, byval withname as zstring ptr = null) as NodePtr
	if nod = null then return null
	dim ret as NodePtr = nod->nextSib
	if ret = null then return null
	if withname then
		while ret andalso *ret->name <> *withname
			ret = ret->nextSib
		wend
	end if
	return ret
End Function

Function PrevSibling(byval nod as NodePtr, byval withname as zstring ptr = null) as NodePtr
	if nod = null then return null
	dim ret as NodePtr = nod->prevSib
	if ret = null then return null
	if withname then
		while ret andalso *ret->name <> *withname
			ret = ret->prevSib
		wend
	end if
	return ret
End Function

Function NodeType(byval nod as NodePtr) as NodeTypes
	return nod->nodeType
End Function

Function NodeName(byval nod as NodePtr) as String
	return *nod->name
End Function

Sub SwapSiblingNodes(byval nod1 as NodePtr, byval nod2 as NodePtr)
	if nod1 = 0 or nod2 = 0 then debug "SwapSiblingNodes: null node": exit sub
	if nod1 = nod2 then debug "SwapSiblingNodes: don't swap with self": exit sub
	if NodeParent(nod1) <> NodeParent(nod2) then debug "SwapSiblingNodes: can't swap non-siblings": exit sub
	
	dim par as NodePtr
	par = NodeParent(nod1)
	if par = 0 then debug "SwapSiblingNodes: null parent" : exit sub
	
	'debug "swap " & NodeName(nod1) & " with sibling " & NodeName(nod2)
	
	dim holder(par->numChildren - 1) as Nodeptr
	dim index as integer = 0
	dim p1 as integer = -1
	dim p2 as integer = -1
	dim ch as NodePtr
	ch = par->children
	while ch
		holder(index) = ch
		if ch = nod1 then p1 = index
		if ch = nod2 then p2 = index
		index += 1
		ch = NextSibling(ch)
	wend
	if p1 = -1 or p2 = -1 then debug "SwapSiblingNodes: sanity fail, siblings not found in parent's children": exit sub
	swap holder(p1), holder(p2)
	
	for i as integer = 0 to ubound(holder) - 1
		holder(i)->nextSib = holder(i + 1)
	next i
	holder(ubound(holder))->nextSib = 0
	
	for i as integer = 1 to ubound(holder)
		holder(i)->prevSib = holder(i - 1)
	next i
	holder(0)->prevSib = 0
	
	par->children = holder(0)
	par->lastChild = holder(ubound(holder))
End Sub

sub SwapNodePrev(byval node as Nodeptr)
	if node = 0 then exit sub
	dim sib as NodePtr
	sib = PrevSibling(node)
	IF sib = 0 then exit sub
	SwapSiblingNodes(node, sib)
end sub

sub SwapNodeNext(byval node as Nodeptr)
	if node = 0 then exit sub
	dim sib as NodePtr
	sib = NextSibling(node)
	if sib = 0 then exit sub
	SwapSiblingNodes(node, sib)
end sub

'This clones a node and all its children and returns the cloned (parentless) node.
'The doc is an optional doc ptr that new new node should belong to. If omitted, the clone
'will be in the same doc as the original node
Function CloneNodeTree(byval nod as NodePtr, byval doc as DocPtr=0) as NodePtr
	if nod = null then
		debug "CloneNodeTree: null node pointer"
		return null
	end if
	dim n as NodePtr
	if doc then
		n = CreateNode(doc, NodeName(nod))
	else
		n = CreateNode(nod, NodeName(nod))
	end if
	select case NodeType(nod)
		case rltInt:
			SetContent(n, GetInteger(nod))
		case rltFloat:
			SetContent(n, GetFloat(nod))
		case rltString:
			SetContent(n, GetString(nod))
	end select
	dim ch as NodePtr
	ch = FirstChild(nod)
	while ch
		AddChild(n, CloneNodeTree(ch, doc))
		ch = NextSibling(ch)
	wend
	return n
End Function

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

Sub WriteVLI(byval f as BufferedFile ptr, byval v as Longint)
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
	
	Buffered_putc(f, o)
	
	do while v > 0
		o = v and &b1111111 'extract the next 7 bits
		v = v SHR 7
		
		if v > 0 then o OR= &b10000000
		
		Buffered_putc(f, o)
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

function ReadVLI(byval f as FILE ptr) as longint
	dim tmp as integer
	dim o as ubyte
	dim ret as longint = 0
	dim neg as integer = 0
	dim bit as integer = 0
	
	'get #f, , o
	tmp = fgetc(f)
	
	if tmp = -1 then return 0
	
	o = tmp
	
	if o AND &b1000000 then neg = yes
	
	ret OR= (o AND &b111111) SHL bit
	bit += 6
	
	do while o AND &b10000000
		'get #f, , o
		tmp = fgetc(f)
		if tmp = -1 then return 0
		
		o = tmp
		
		ret OR= (o AND &b1111111) SHL bit
		bit += 7
	loop
	
	if neg then ret *= -1
	
	return ret
	
end function



Function CreateHashTable(byval doc as Docptr, byval hashFunc as hashFunction, byval b as integer) as ReloadHash ptr
	dim ret as HashPtr = RCallocate(sizeof(ReloadHash), doc)
	
	with *ret
		.bucket = RCallocate(sizeof(ReloadHashItem ptr) * b, doc)
		.numBuckets = b
		.numItems = 0
		.doc = doc
		.hashFunc = hashFunc
	end with
	
	return ret
End Function

Sub DestroyHashTable(byval h as HashPtr)
	if h = 0 then return
	
	for i as integer = 0 to h->numBuckets - 1
		do while h->bucket[i]
			dim t as ReloadHashItem ptr
			t = h->bucket[i]->nxt
			RDeallocate(h->bucket[i], h->doc)
			h->bucket[i] = t
		loop
	next
	RDeallocate(h->bucket, h->doc)
	
	RDeallocate(h, h->doc)
end sub

Function FindItem(byval h as HashPtr, byval key as ZString ptr, byval num as integer) as any ptr
	dim b as ReloadHashItem ptr
	
	dim hash as uinteger = h->hashFunc(key)
	
	b = h->bucket[hash mod h->numBuckets]
	
	do while b
		if *b->key = *key then
			num -= 1
			if num <= 0 then return b->item
		end if
		b = b->nxt
	loop
	
	return 0
End Function

Sub AddItem(byval h as HashPtr, byval key as ZString ptr, byval item as any ptr)
	dim hash as uinteger = h->hashFunc(key)
	
	dim as ReloadHashItem ptr b, newitem = RCallocate(sizeof(ReloadHashItem), h->doc)
	
	newitem->key = key
	newitem->item = item
	newitem->nxt = 0
	
	b = h->bucket[hash mod h->numBuckets]
	
	if b then
		do while b->nxt
			b = b->nxt
		loop
		b->nxt = newitem
	else
		h->bucket[hash mod h->numBuckets] = newitem
	end if
end Sub

Sub RemoveKey(byval h as HashPtr, byval key as zstring ptr, byval num as integer)
	dim as ReloadHashItem ptr b, prev
	
	dim hash as uinteger = h->hashFunc(key)
	
	b = h->bucket[hash mod h->numBuckets]
	
	prev = 0
	do while b
		if *b->key = *key then
			if num <> -1 then
				num -= 1
				if num = 0 then
					if prev then
						prev->nxt = b->nxt
					end if
					
					RDeallocate(b, h->doc)
					return
				end if
			else
				if prev then
					prev->nxt = b->nxt
				end if
				
				RDeallocate(b, h->doc)
			end if
		end if
		prev = b
		b = b->nxt
	loop
end sub

Function DocumentMemoryUsage(byval doc as DocPtr) as longint
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	dim ret as longint = 0
	if 0 = HeapLock(doc->heap) then return 0
	
	dim entry as PROCESS_HEAP_ENTRY
	
	entry.lpData = null
	do while HeapWalk(doc->heap, @entry) <> FALSE
		if entry.wFlags AND PROCESS_HEAP_ENTRY_BUSY then
			ret += entry.cbData
		end if
	loop
	
	HeapUnlock(doc->heap)
	
	return ret
#else
	return 0 'who knows?
#endif
end function

End Namespace
