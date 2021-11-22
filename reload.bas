'OHRRPGCE - RELOAD related functions
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#define RELOADINTERNAL

'if you find yourself debugging heap issues, define this. If the crashes go away, then I (Mike Caron)
'somehow fscked up the private heap implementation. Or, someone else touched something without
'understanding how it works...

'#define RELOAD_NOPRIVATEHEAP

#include "config.bi"

#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
        include_windows_bi()
#endif

#include "reload.bi"
#include "reloadext.bi"
#include "util.bi"
#include "cutil.bi"
#include "lumpfile.bi"
#include "common_base.bi"

Namespace Reload

Type hashFunction as Function(byval k as zstring ptr) as uinteger

Declare Function AddStringToTable (st as zstring ptr, byval doc as DocPtr) as integer
Declare Function FindStringInTable overload(st as zstring ptr, byval doc as DocPtr) as integer

Declare Function CreateHashTable(doc as DocPtr, hashFunc as hashFunction, numbuckets as integer = 65) as HashPtr
Declare Sub DestroyHashTable(byval h as HashPtr)
Declare Function FindItem(h as HashPtr, key as zstring ptr, copynumber as integer = 1) as intptr_t
Declare Sub AddItem(h as HashPtr, key as zstring ptr, item as intptr_t)
Declare Sub RemoveKey(byval h as HashPtr, byval key as zstring ptr, byval num as integer = 1)


'I am aware of the hash table implementation in util.bas. However, this is tuned
'for this purpose. Plus, I want everything contained on the private heap (if applicable)
'NOTE: This is actually a multimap
Type ReloadHashItem
	key as zstring ptr 
	item as intptr_t  'Fits a pointer or an int
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

function RHeapInit(byval doc as DocPtr) as bool
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	doc->heap = HeapCreate(0, 0, 0)
	BUG_IF(doc->heap = null, "HeapCreate failed", NO)
	return YES
#else
	'nothing, use the default heap
	return YES
#endif
end function

Function RHeapDestroy(byval doc as DocPtr) as bool
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	HeapDestroy(doc->heap) 'poof
	doc->heap = null
	return YES
#else
	'they need to free memory manually
	return NO
#endif
end function

Function RCallocate(byval s as integer, byval doc as DocPtr) as any ptr
	dim ret as any ptr
	
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	ret = HeapAlloc(doc->heap, HEAP_ZERO_MEMORY, s)
#else
	ret = CAllocate(s)
#endif
	
	return ret
end function

Function RReallocate(byval p as any ptr, byval doc as DocPtr, byval newsize as integer) as any ptr
	dim ret as any ptr
	
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	ret = HeapReAlloc(doc->heap, HEAP_ZERO_MEMORY, p, newsize)
#else
	ret = Reallocate(p, newsize)
#endif
	
	return ret
end function

Sub RDeallocate(byval p as any ptr, byval doc as DocPtr)
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
	HeapFree(doc->heap, 0, p)
#else
	Deallocate(p)
#endif
End Sub

'Fed a list of 132010 english words, this produced 131957 unique hashes.
'The old hash produced only 1931 unique hashes
'If this changes, reload_HashZString in reloadbasic.py needs to be updated too
Function HashZString(byval st as zstring ptr) as uinteger
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
		if RHeapInit(ret) = NO then
			delete ret
			return null
		end if
		ret->version = 1
		ret->root = null
		ret->filename = "<In-memory Reload.Doc>"

		'The initial string table has one entry: ""
		ret->strings = RCallocate(sizeof(StringTableEntry), ret)
		ret->strings[0].str = RCallocate(1, ret)
		*ret->strings[0].str = "" 'this is technically redundant.
		ret->numStrings = 1
		ret->numAllocStrings = 1
		ret->stringHash = CreateHashTable(ret, @HashZString)
		ret->delayLoading = NO
		ret->nameIndexTable = NULL
		ret->nameIndexTableLen = 0
		
		'add the blank string to the hash
		AddItem(ret->stringHash, ret->strings[0].str, 0)
	end if
	
	return ret
End function

'creates and initilalizes an empty node with a given name.
'it associates the node with the given document, and cannot be added to another one!
Function CreateNode(byval doc as DocPtr, nam as zstring ptr) as NodePtr
	dim ret as NodePtr

	BUG_IF(doc = null, "no doc", null)

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

Function CreateNode(byval nod as NodePtr, nam as zstring ptr) as NodePtr
	return CreateNode(nod->doc, nam)
end function

'FIXME: the old name is never freed
sub RenameNode(byval nod as NodePtr, newname as zstring ptr)
	nod->namenum = AddStringToTable(newname, nod->doc)
	
	nod->name = nod->doc->strings[nod->namenum].str
	nod->doc->strings[nod->namenum].uses += 1
end sub

'Efficiently free the children of a node
sub FreeChildren(byval nod as NodePtr)
	BUG_IF(nod = NULL, "ptr already null")

	if (nod->flags and nfNotLoaded) = 0 then
		dim as NodePtr child = nod->children, nextchild
		do while child
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
'(TODO: node names are never freed from the string table. It doesn't matter)
sub FreeNode(byval nod as NodePtr)
	BUG_IF(nod = null, "ptr already null")

	FreeChildren(nod)

	'If this node has a parent, we should remove this node from
	'its list of children
	if nod->parent <> 0 then
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
	if nod->nodeType = rltString and nod->str <> 0 then RDeallocate(nod->str, nod->doc)
	if nod->doc->root = nod then
		nod->doc->root = null
	end if
	RDeallocate(nod, nod->doc)
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
		vfclose(doc->fileHandle)
		doc->fileHandle = null
	end if
	
	RDeallocate(doc->nameIndexTable, doc)
	'RDeallocate(doc->nameIndexTableBits, doc)
	RDeallocate(doc->RBFuncBits, doc)

	if RHeapDestroy(doc) = NO then
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
Function LoadNode(byval vf as VFile ptr, byval doc as DocPtr, byval force_recursive as bool) as NodePtr
	dim size as integer
	vfread(@size, 4, 1, vf)

	dim as integer here
	here = vftell(vf)

	dim ret as NodePtr
	ret = CreateNode(doc, "")
	ret->namenum = cshort(ReadVLI(vf))

	if ret->namenum < 0 or ret->namenum >= doc->numStrings then
		reporterr doc->filename & " corrupt: node has invalid name #" & ret->namenum, serrMajor
		ret->namenum = 0
	else
		'debug "Node has valid name: #" & ret->namenum & " " & *doc->strings[ret->namenum].str
		ret->name = doc->strings[ret->namenum].str
		doc->strings[ret->namenum].uses += 1
	end if

	ret->nodetype = vfgetc(vf)

	select case ret->nodeType
		case rliNull
		case rliByte
			ret->num = cbyte(vfgetc(vf))
			ret->nodeType = rltInt
		case rliShort
			dim s as short
			vfread(@s, 2, 1, vf)
			ret->num = s
			ret->nodeType = rltInt
		case rliInt
			dim i as integer
			vfread(@i, 4, 1, vf)
			ret->num = i
			ret->nodeType = rltInt
		case rliLong
			vfread(@(ret->num), 8, 1, vf)
			ret->nodeType = rltInt
		case rliFloat
			vfread(@(ret->flo), 8, 1, vf)
			ret->nodeType = rltFloat
		case rliString
			dim mysize as integer
			ret->strSize = cint(ReadVLI(vf))
			ret->str = RCallocate(ret->strSize + 1, doc)
			vfread(ret->str, 1, ret->strSize, vf)
			ret->nodeType = rltString
		case else
			reporterr doc->filename & " corrupt: unknown node type " & ret->nodeType, serrMajor
			FreeNode(ret)
			return null
	end select

	ret->numChildren = ReadVLI(vf)

	if doc->delayLoading and force_recursive = NO then
		ret->fileLoc = vftell(vf)
		ret->flags OR= nfNotLoaded

		vfseek(vf, size + here, SEEK_SET)
	else
		for i as integer = 0 to ret->numChildren - 1
			dim nod as NodePtr
			nod = LoadNode(vf, doc, force_recursive)
			if nod = null then
				FreeNode(ret)
				'debug "LoadNode: node @" & here & " child " & i & " node load failed"
				return null
			end if
			ret->numChildren -= 1
			AddChild(ret, nod)
		next

		if vftell(vf) - here <> size then
			FreeNode(ret)
			reporterr doc->filename & " corrupt? GOSH-diddly-DARN-it! Why did we read " & (vftell(vf) - here) & " bytes instead of " & size, serrMajor
			return null
		end if
	end if
	
	return ret
End Function

'This loads a node's children if loading has been delayed, either recursively or not, returning success
'Note: won't do a recursive load if the node is loaded already but its child aren't, so you will have to
'call LoadNode before the node's children are first accessed!
Function LoadNode(byval ret as NodePtr, byval recursive as bool = YES) as bool
	if ret = null then return NO
	if (ret->flags AND nfNotLoaded) = 0 then return YES

	dim vf as VFile ptr = ret->doc->fileHandle

	vfseek(vf, ret->fileLoc, SEEK_SET)

	for i as integer = 0 to ret->numChildren - 1
		dim nod as NodePtr = LoadNode(vf, ret->doc, recursive)
		if nod = null then
			'debug "LoadNode: node @" & ret->fileLoc & " child " & i & " node load failed"
			return NO
		end if
		ret->numChildren -= 1
		AddChild(ret, nod)
	next
	
	ret->flags AND= NOT nfNotLoaded
	
	return YES
End Function

'This loads the string table from a binary document (as if the name didn't clue you in)
Sub LoadStringTable(byval vf as VFile ptr, byval doc as DocPtr)
	dim as uinteger count, size
	
	count = cint(ReadVLI(vf))
	
	if count <= 0 then exit sub
	
	for i as integer = 1 to doc->numAllocStrings - 1
		if doc->strings[i].str then RDeallocate(doc->strings[i].str, doc)
	next
	
	doc->strings = RReallocate(doc->strings, doc, (count + 1) * sizeof(StringTableEntry))
	doc->numStrings = count + 1
	doc->numAllocStrings = count + 1
	
	for i as integer = 1 to count
		size = cint(ReadVLI(vf))
		doc->strings[i].str = RCallocate(size + 1, doc)
		dim zs as zstring ptr = doc->strings[i].str
		if size > 0 then
			vfread(zs, 1, size, vf)
		end if
		
		AddItem(doc->stringHash, doc->strings[i].str, i)
	next
end sub

'fil may be a res:// path.
Function LoadDocument(fil as string, byval options as LoadOptions = optNone) as DocPtr
	ERROR_IF(len(fil) = 0, "empty filename", NULL)  'Maybe finddatafile failed, which isn't a bug
	dim starttime as double = timer
	dim ret as DocPtr
	dim vf as VFile ptr

	log_openfile fil
	vf = vfopen(fil, "rb")
	if vf = 0 then
		if (options and optIgnoreMissing) = 0 then
			reporterr "Couldn't open " & fil & " (probably missing)", serrMajor
		end if
		return null
	end if

	dim as string magic = "    "
	vfread(strptr(magic), 1, 4, vf)

	if magic <> "RELD" then
		vfclose(vf)
		reporterr "Couldn't load " & fil & ": Not a RELOAD file", serrMajor
		return null
	end if

	dim as ubyte ver
	dim as integer headSize, datSize
	ver = vfgetc(vf)

	select case ver
		case 1
			vfread(@headSize, 4, 1, vf)
			if headSize <> 13 then
				vfclose(vf)
				reporterr fil & " corrupt: wrong header size " & headSize, serrMajor
				return null
			end if
			vfread(@datSize, 4, 1, vf)

		case else ' dunno. Let's quit.
			vfclose(vf)
			reporterr "Couldn't load " & fil & ": RELOAD version " & ver & " not supported", serrMajor
			return null
	end select
	
	'if we got here, the document is... not yet corrupt. I guess.
	
	ret = CreateDocument()
	ret->version = ver
	ret->fileName = fil
	'debuginfo "reload: opened " & fil
	
	if options and optNoDelay then
		ret->delayLoading = NO
	else
		ret->delayLoading = YES
		ret->fileHandle = vf
	end if
	
	'We'll load the string table first, to assist in debugging.
	
	vfseek(vf, datSize, SEEK_SET)
	LoadStringTable(vf, ret)
	
	vfseek(vf, headSize, SEEK_SET)
	
	ret->root = LoadNode(vf, ret, NO)
	
	'Is it possible to serialize a null root? I mean, I don't know why you would want to, but...
	'regardless, if it's null here, it's because of an error
	if ret->root = null then
		vfclose(vf)
		FreeDocument(ret)
		return null
	end if
	
	if options and optNoDelay then
		vfclose(vf)
	end if
	debug_if_slow(starttime, 0.1, fil)
	return ret
End Function

'Internal function
'Locates a string in the string table. If it's not there, returns -1
Function FindStringInTable (st as zstring ptr, byval doc as DocPtr) as integer
	if len(*st) = 0 then return 0

	dim ret as integer = FindItem(doc->stringhash, st)

	if ret = 0 then return -1
	return ret
end function


'Adds a string to the string table. If it already exists, return the index
'If it doesn't already exist, add it, and return the new index
Function AddStringToTable(name as zstring ptr, byval doc as DocPtr) as integer
	dim ret as integer
	ret = FindStringInTable(name, doc)
	if ret <> -1 then
		return ret
	end if

	BUG_IF(doc->numAllocStrings = 0, "Unallocated string table!", 0)

	if doc->numStrings >= doc->numAllocStrings then 'I hope it's only ever equals...
		dim s as StringTableEntry ptr = RReallocate(doc->strings, doc, sizeof(StringTableEntry) * (doc->numAllocStrings * 2))
		if s = 0 then 'panic
			showbug "Error resizing string table"
			return -1
		end if
		for i as integer = doc->numAllocStrings to doc->numAllocStrings * 2 - 1
			s[i].str = 0
			s[i].uses = 0
		next
		
		doc->strings = s
		doc->numAllocStrings = doc->numAllocStrings * 2
	end if
	
	ret = doc->numStrings
	doc->numStrings += 1
	doc->strings[ret].str = RCallocate(len(*name) + 1, doc)
	*doc->strings[ret].str = *name
	
	AddItem(doc->stringHash, doc->strings[ret].str, ret)

	return ret
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
					'debug "RB: mapping string " & *.name & ", namenum=" & b->item & " nameidx=" & .nameindex
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
	BUG_IF(doc = NULL, "null doc")
	BUG_IF(doc->root = NULL, "null root")

	RemoveProvisionalNodes(doc->root)
	'BuildStringTable(doc->root, doc)

	'In case things go wrong, we serialize to a temporary file first
	safekill file & ".tmp"

	dim f as BufferedFile ptr
	f = Buffered_open(file & ".tmp")
	ERROR_IF(f = NULL, "Unable to open " & file & ".tmp")

	dim i as uinteger
	
	Buffered_write(f, @"RELD", 4) 'magic signature
	
	Buffered_putc(f, 1) 'version
	
	i = 13 'the size of the header (i.e., offset to the data)
	Buffered_write(f, @i, 4)
	
	i = 0 'we're going to fill this in later. it is the string table post relative to the beginning of the file.
	Buffered_write(f, @i, 4)
	
	'write out the body
	serializeBin(doc->root, f, doc)
	
	'this is the location of the string table (immediately after the data)
	dim table_loc as integer
	table_loc = Buffered_tell(f)
	
	Buffered_seek(f, 9)
	Buffered_write(f, @table_loc, 4) 'filling in the string table position
	
	'jump back to the string table
	Buffered_seek(f, table_loc)
	
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
		vfclose(doc->fileHandle)
		doc->fileHandle = NULL
	end if

	safekill file
	if renamefile(file & ".tmp", file) = NO then
		showerror "SerializeBin: could not rename " & file & ".tmp to " & file
		exit sub  'don't delete the data
	end if
	debug_if_slow(starttime, 0.1, file)
end sub

sub serializeBin(byval nod as NodePtr, byval f as BufferedFile ptr, byval doc as DocPtr)
	BUG_IF(nod = NULL, "null node ptr")

	'first, if a node isn't loaded, we need to do so.
	if nod->flags AND nfNotLoaded then
		LoadNode(nod, YES)
	end if

	dim as integer size_loc, content_start_loc = 0, content_end_loc
	size_loc = Buffered_tell(f)
	' Will fill this in later, this is the node content size
	Buffered_write(f, @content_start_loc, 4)

	content_start_loc = Buffered_tell(f)

	BUG_IF(nod->namenum = -1, "node without valid name index")
	WriteVLI(f, nod->namenum)

	select case nod->nodeType
		case rltNull
			'Nulls have no data, but convey information by existing or not existing.
			'They can also have children.
			Buffered_putc(f, rliNull)
		case rltInt 'this is good enough, don't need VLI for this
			if nod->num > 2147483647 or nod->num < -2147483648 then
				Buffered_putc(f, rliLong)
				Buffered_write(f, @(nod->num), 8)
			elseif nod->num > 32767 or nod->num < -32768 then
				Buffered_putc(f, rliInt)
				dim temp as long = nod->num
				Buffered_write(f, @temp, 4)
			elseif nod->num > 127 or nod->num < -128 then
				Buffered_putc(f, rliShort)
				dim temp as short = nod->num
				Buffered_write(f, @temp, 2)
			else
				Buffered_putc(f, rliByte)
				Buffered_putc(f, nod->num)
			end if
		case rltFloat
			Buffered_putc(f, rliFloat)
			Buffered_write(f, @(nod->flo), 8)
		case rltString
			Buffered_putc(f, rliString)
			WriteVLI(f, nod->strSize)
			Buffered_write(f, nod->str, nod->strSize)
	end select

	WriteVLI(f, nod->numChildren)
	dim n as NodePtr = nod->children
	do while n <> null
		serializeBin(n, f, doc)
		n = n->nextSib
	loop

	content_end_loc = Buffered_tell(f)
	dim size as long = content_end_loc - content_start_loc
	Buffered_seek(f, size_loc)
	Buffered_write(f, @size, 4)
	Buffered_seek(f, content_end_loc)
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

'Make a node provisional, which means it will be deleted before the doc is
'serialised if it has no children.
sub MarkProvisional(byval nod as NodePtr)
	BUG_IF(nod = NULL, "null node ptr")
	nod->flags OR= nfProvisional
end sub

'Whether a node has a particular ancestor. Returns YES if nod = possible_parent.
Function NodeHasAncestor(byval nod as NodePtr, byval possible_parent as NodePtr) as bool
	if possible_parent = null then return NO
	do while nod <> null
		if nod = possible_parent then return YES
		nod = nod->parent
	loop
	return NO
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
Sub AddChild(byval par as NodePtr, byval nod as NodePtr)
	BUG_IF(par = 0, "null parent")
	BUG_IF(nod = 0, "null node")
	BUG_IF(NodeHasAncestor(par, nod), "creating a loop!")  'includes par = nod
	BUG_IF(nod->doc->root = nod, "can't reparent the root")

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
end sub

'This adds nod as a sibling *after* another node, sib.
sub AddSiblingAfter(byval sib as NodePtr, byval nod as NodePtr)
	BUG_IF(sib = 0, "null sib")
	BUG_IF(nod = 0, "null node")
	BUG_IF(NodeHasAncestor(sib, nod), "creating a loop!")  'íncludes sib = nod
	BUG_IF(nod->doc->root = nod, "can't reparent the root")
	BUG_IF(sib->parent = 0, "sib has no parent")

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
end sub

'This adds nod as a sibling *before* another node, sib.
sub AddSiblingBefore(byval sib as NodePtr, byval nod as NodePtr)
	BUG_IF(sib = 0, "null sib")
	BUG_IF(nod = 0, "null node")
	BUG_IF(NodeHasAncestor(sib, nod), "creating a loop!")  'íncludes sib = nod
	BUG_IF(nod->doc->root = nod, "can't reparent the root")
	BUG_IF(sib->parent = 0, "sib has no parent")

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
end sub

'This promotes a node to Root Node status (which, really, isn't that big a deal.)
'There's no way to make a node no longer the root, except to free it.
'NOTE: It automatically frees the old root node (unless it's the same as the new root node)
'NOTE: the node must not have a parent
sub SetRootNode(byval doc as DocPtr, byval nod as NodePtr)
	BUG_IF(doc = null, "null doc")
	BUG_IF(nod = null, "null node")
	BUG_IF(nod->parent, "has parent")
	BUG_IF(nod->doc <> doc, "node was created in the context of another RELOAD doc")

	if doc->root = nod then return

	if doc->root then
		FreeNode(doc->root)  'Won't delete nod, it has no parent
	end if

	doc->root = nod
end sub

'This is from xml2reload: is a node representable as a longint?
local function NodeCompressible(byval node as NodePtr) as integer
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
local function NodeNeedsEncoding(byval node as NodePtr, byval debugging as bool, byval shortform as bool) as integer
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
local function EscapeXMLString(s as string) as string
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
local function GetBase64EncodedString(byval node as NodePtr) as string
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

	dim closetag as bool = YES

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

Function FindDescendentByName(byval nod as NodePtr, nam as zstring ptr) as NodePtr
	'recursively searches for a child by name, depth-first
	'can also find self
	if nod = null then return null
	if *nod->name = *nam then return nod
	
	if nod->flags AND nfNotLoaded then LoadNode(nod, YES)
	
	dim child as NodePtr
	dim ret as NodePtr
	child = nod->children
	while child <> null
		ret = FindDescendentByName(child, nam)
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
		dim namenum as integer = FindItem(nod->doc->stringhash, nam)

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

'Find first (or last, if 'reverse') node matching both content and (optionally) name
'Other overloads unimplemented
Function GetChildByContent(byval nod as NodePtr, content as longint, name as zstring ptr = null, reverse as bool = NO) as NodePtr
	if nod = null then return null
	if nod->flags AND nfNotLoaded then LoadNode(nod, NO)

	dim child as NodePtr
	child = iif(reverse, nod->lastChild, nod->children)
	while child
		if child->nodeType = rltInt andalso child->num = content then
			if name = null orelse *child->name = *name then
				return child
			end if
		end if
		child = iif(reverse, child->prevSib, child->nextSib)
	wend
	return null
End Function

'This returns a node's content in string form.
Function GetString(byval node as NodePtr) as string
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
Function GetInteger(byval node as NodePtr) as longint
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
Function GetFloat(byval node as NodePtr) as double
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
Function GetZString(byval node as NodePtr) as zstring ptr
	if node = null then return 0
	
	if node->nodeType <> rltString then
		return 0
	end if
	
	return node->str
End Function

Function GetZStringSize(byval node as NodePtr) as integer
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
Function ResizeZString(byval node as NodePtr, byval newsize as integer) as zstring ptr
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
Function GetOrCreateChild(byval parent as NodePtr, n as zstring ptr) as NodePtr
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
Function SetChildNode(byval parent as NodePtr, n as zstring ptr) as NodePtr
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
Function SetChildNode(byval parent as NodePtr, n as zstring ptr, byval val as longint) as NodePtr
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
Function SetChildNode(byval parent as NodePtr, n as zstring ptr, byval val as double) as NodePtr
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
Function SetChildNode(byval parent as NodePtr, n as zstring ptr, val as string) as NodePtr
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

'Sets teh child node of name n to a double value. If n doesn't exist, it adds it.
'Also, adds a child of n called "str" with a formatted date string
Function SetChildNodeDate(byval parent as NodePtr, n as zstring ptr, val as double) as NodePtr
	dim node as NodePtr = SetChildNode(parent, n, val)

	if node then
		SetChildNode(node, "str", format_date(val))
	end if

	return node
end Function

'Toggle a node to a zero/nonzero value (sets it to 0 or 1). Creates the node if it does not exist
Sub ToggleBoolChildNode(byval parent as NodePtr, n as zstring ptr)
	if parent = 0 then exit sub
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	'first, check to see if this node already exists
	dim ch as NodePtr = GetChildByName(parent, n)

	if ch = 0 then
		'it does not exist, so add it (and toggle it)
		SetChildNode(parent, n, 1)
	else
		SetChildNode(parent, n, iif(GetInteger(ch), 0, 1))
	end if
end Sub

'If the child node exists, delete it. If it does not exist, create an empty node
Sub ToggleChildNode(byval parent as NodePtr, n as zstring ptr)
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
Sub FreeChildNode(byval parent as NodePtr, n as zstring ptr)
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
Function GetChildNodeInt(byval parent as NodePtr, n as zstring ptr, byval d as longint) as longint
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetInteger(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeFloat(byval parent as NodePtr, n as zstring ptr, byval d as double) as double
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetFloat(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeStr(byval parent as NodePtr, n as zstring ptr, d as string) as string
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetString(nod)
end function

'looks for a child node of the name n, and retrieves its value. d is the default, if n doesn't exist
Function GetChildNodeBool(byval parent as NodePtr, n as zstring ptr, byval d as integer) as integer
	if parent = 0 then return d
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	if nod = 0 then return d
	return GetInteger(nod) <> 0
end function

'looks for a child node of the name n, and returns whether it finds it or not. For "flags", etc
Function GetChildNodeExists(byval parent as NodePtr, n as zstring ptr) as bool
	if parent = 0 then return NO

	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim nod as NodePtr = GetChildByName(parent, n)
	
	return nod <> 0
end function

'Appends a child node of name n with a null value.
Function AppendChildNode(byval parent as NodePtr, n as zstring ptr) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr
	ret = CreateNode(parent->doc, n)
	AddChild(parent, ret)

	'SetContent(ret)  'Does nothing

	return ret
end Function

'Appends a child node of name n to with integer value.
Function AppendChildNode(byval parent as NodePtr, n as zstring ptr, byval val as longint) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

'Appends a child node of name n with a floating point value.
Function AppendChildNode(byval parent as NodePtr, n as zstring ptr, byval val as double) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

'Appends a child node of name n with a string value.
Function AppendChildNode(byval parent as NodePtr, n as zstring ptr, val as string) as NodePtr
	if parent = 0 then return 0
	
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	
	dim ret as NodePtr = AppendChildNode(parent, n)
	SetContent(ret, val)
	
	return ret
end Function

Function ChildByIndex(byval parent as NodePtr, byval index as integer, byval withname as zstring ptr = NULL) as NodePtr
	'Return the index'th child node, or 0 if no such child exists
	'This could be slow for long child lists, so don't use it unless you really need it
	if parent = 0 then return 0
	if parent->flags AND nfNotLoaded then LoadNode(parent, NO)
	dim i as integer
	dim ch as Node Ptr
	ch = parent->children
	do while ch
		if withname = NULL orelse *ch->name = *withname then
			if i = index then return ch
			i += 1
		end if
		ch = ch->nextsib
	loop
	return 0 ' no child matches index
end Function

Function DocumentRoot(byval doc as DocPtr) as NodePtr
	if doc = null then return null
	return doc->root
end Function

Function GetDocument(byval nod as NodePtr) as DocPtr
	if nod = null then return null
	return nod->doc
end Function

Function NumChildren(byval nod as NodePtr) as Integer
	if nod = null then return 0
	return nod->numChildren
end Function

'Return number of children with this name
Function CountChildren(byval nod as NodePtr, byval withname as zstring ptr) as integer
	if nod = null then return 0
	if nod->flags AND nfNotLoaded then LoadNode(nod, NO)
	dim count as integer = 0
	dim ch as NodePtr = nod->children
	while ch
		if *ch->name = *withname then count += 1
		ch = ch->nextSib
	wend
	return count
end Function

Function NodeParent(byval nod as NodePtr) as NodePtr
	if nod = null then return null
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
	if nod = null then return null
	return nod->nodeType
End Function

Function NodeName(byval nod as NodePtr) as string
	if nod = null then return ""
	return *nod->name
End Function

Sub SwapSiblingNodes(byval nod1 as NodePtr, byval nod2 as NodePtr)
	BUG_IF(nod1 = 0 orelse nod2 = 0, "null node")
	BUG_IF(nod1 = nod2, "don't swap with self")
	BUG_IF(NodeParent(nod1) <> NodeParent(nod2), "can't swap non-siblings")

	dim par as NodePtr
	par = NodeParent(nod1)
	BUG_IF(par = 0, "null parent")

	'debug "swap " & NodeName(nod1) & " with sibling " & NodeName(nod2)

	dim holder(par->numChildren - 1) as NodePtr
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
	BUG_IF(p1 = -1 orelse p2 = -1, "sanity fail, siblings not found in parent's children")
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

sub SwapNodePrev(byval node as NodePtr)
	if node = 0 then exit sub
	dim sib as NodePtr
	sib = PrevSibling(node)
	if sib = 0 then exit sub
	SwapSiblingNodes(node, sib)
end sub

sub SwapNodeNext(byval node as NodePtr)
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
	BUG_IF(nod = NULL, "null node ptr", NULL)
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


'==========================================================================================
'                                           VLI
'==========================================================================================

#macro WRITEBYTE_BufferedFile(SRC)
        Buffered_putc(outfile, SRC)
#endmacro

#macro WRITEBYTE_FB(SRC)
        put #outfile, , SRC
#endmacro

#macro _WriteVLI(WRITEBYTE)
	dim byt as ubyte
	dim neg as bool = NO

	if v < 0 then
		neg = YES
		v = not v
                ' v is now non-negative, so all shifts are effectively unsigned
	end if

	byt = v and &b111111 'first, extract the low six bits
	v = v SHR 6

	if neg then   byt OR=  &b1000000 'bit 6 is the "number is negative" bit

	if v > 0 then byt OR= &b10000000 'bit 7 is the "omg there's more data" bit

        WRITEBYTE(byt)

	do while v > 0
		byt = v and &b1111111 'extract the next 7 bits
		v = v SHR 7
		
		if v > 0 then byt OR= &b10000000
		
                WRITEBYTE(byt)
	loop
#endmacro

'This writes an integer out in such a fashion as to minimize the number of bytes used. Eg, 36 will
'be stored in one byte, while 365 will be stored in two, 10000 in three bytes, etc
Sub WriteVLI(outfile as BufferedFile ptr, v as longint)
        _WriteVLI(WRITEBYTE_BufferedFile)
end sub

Sub WriteVLI(outfile as integer, v as longint)
        _WriteVLI(WRITEBYTE_FB)
end sub


#macro READBYTE_stdio(DEST)
        scope
		dim tmp as integer = vfgetc(infile)
		if tmp = -1 then return 0
		DEST = tmp
        end scope
#endmacro

#macro READBYTE_FB(DEST)
        if get(#infile, , DEST) then return 0
#endmacro

#macro _ReadVLI(READBYTE)
	dim byt as ubyte
	dim ret as longint = 0
	dim neg as bool = NO
	dim bitnum as integer = 0

	READBYTE(byt)
	if byt and &b1000000 then neg = YES

	ret or= (byt and &b111111) shl bitnum
	bitnum += 6

	do while byt and &b10000000
                READBYTE(byt)
		ret or= cast(longint, byt and &b1111111) shl bitnum
		bitnum += 7
	loop

	if neg then ret = not ret
	return ret
#endmacro

'This reads the number back in again
'Returns 0 on error.
function ReadVLI(infile as VFile ptr) as longint
        _ReadVLI(READBYTE_stdio)
end function

'Using a FB file handler. This is currently used only in reloadtest,
'but VLIs are potentially useful elsewhere.
function ReadVLI(infile as integer) as longint
        _ReadVLI(READBYTE_FB)
end function


'==========================================================================================
'                                Hash table for node names
'==========================================================================================

Function CreateHashTable(doc as DocPtr, hashFunc as hashFunction, numbuckets as integer) as ReloadHash ptr
	dim ret as HashPtr = RCallocate(sizeof(ReloadHash), doc)
	
	with *ret
		.bucket = RCallocate(sizeof(ReloadHashItem ptr) * numbuckets, doc)
		.numBuckets = numbuckets
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

'copynumber: which copy of the item to return. 1 is first, etc,
Function FindItem(h as HashPtr, key as zstring ptr, copynumber as integer = 1) as intptr_t
	if key = NULL then return 0

	dim b as ReloadHashItem ptr
	
	dim hash as uinteger = h->hashFunc(key)
	
	b = h->bucket[hash mod h->numBuckets]
	
	do while b
		if *b->key = *key then
			copynumber -= 1
			if copynumber <= 0 then return b->item
		end if
		b = b->nxt
	loop
	
	return 0
End Function

Sub AddItem(h as HashPtr, key as zstring ptr, item as intptr_t)
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


'==========================================================================================

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
