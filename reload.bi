#ifndef RELOAD_BI
#define RELOAD_BI

'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "config.bi"
#include "util.bi"


#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
#if defined(RELOADINTERNAL)
        '#include "windows.bi"'
        'Reduce namespace pollution (this macro is from config.bi)
        include_windows_bi()
#elseif __FB_DEBUG__
        type HANDLE as any ptr
#endif
#endif

#ifndef null
#define null 0
#endif

Namespace Reload

ENUM NodeInTypes
	rliNull = 0
	rliByte = 1
	rliShort = 2
	rliInt = 3
	rliLong = 4
	rliFloat = 5
	rliString = 6
END ENUM

ENUM NodeTypes
	rltNull
	rltInt
	rltFloat
	rltString
END ENUM

ENUM LoadOptions
	optNone = 0
	optNoDelay = 1
END ENUM

TYPE DocPtr as Doc ptr
TYPE NodePtr as Node ptr

#if defined(RELOADINTERNAL) or __FB_DEBUG__
	TYPE Hashptr as ReloadHash ptr
	
	Type StringTableEntry
		str as Zstring Ptr
		uses as integer
	End Type
	
	TYPE Doc
		version as integer
		root as NodePtr
		strings as StringTableEntry ptr
		numStrings as integer
		numAllocStrings as integer
#if defined(__FB_WIN32__) and not defined(RELOAD_NOPRIVATEHEAP)
		heap as HANDLE
#endif
		stringHash as HashPtr
		delayLoading as integer
		fileHandle as FILE ptr
		'fileName as string

		'The following members are used only by RELOADBASIC
		RBSignature as integer
		nameIndexTable as short ptr   'pointer to an array
		nameIndexTableLen as integer
		'nameIndexTableBits as uinteger ptr
		RBFuncBits as uinteger ptr
	END TYPE
	
	ENUM NodeFlags
		nfNotLoaded = 1   'Children of this node haven't been loaded. NOTE: numChildren has real value!
		nfProvisional = 2 'When saving, ignore this node if has no children
	END ENUM
	
	TYPE Node
		'name as string
		name as zstring ptr
		namenum as short   'in the string table, used while loading
		nodeType as ubyte
		Union 'this saves sizeof(Double) bytes per node!
			num as LongInt
			flo as Double
			str as zstring ptr
		end Union
		strSize as integer
		numChildren as integer
		children as NodePtr   'aka firstChild
		lastChild as NodePtr
		doc as DocPtr
		parent as NodePtr
		nextSib as NodePtr
		prevSib as NodePtr
		flags as integer
		fileLoc as integer
	END TYPE
#else
	TYPE Doc
		thisIsPrivate as ubyte
	End Type
	TYPE Node
		thisIsPrivate as ubyte
	End Type
#endif


Declare Function CreateDocument() as DocPtr
Declare Function CreateNode overload(byval doc as DocPtr, nam as string) as NodePtr
Declare Function CreateNode(byval nod as NodePtr, nam as string) as NodePtr
Declare sub FreeChildren(byval nod as NodePtr)
Declare sub FreeNode(byval nod as NodePtr, byval options as integer = 0) 'don't use options.
Declare sub FreeDocument(byval doc as DocPtr)
Declare sub RenameNode(byval nod as NodePtr, newname as string)
Declare sub RemoveProvisionalNodes(byval nod as NodePtr)
Declare sub MarkProvisional(byval nod as NodePtr)
Declare sub SetContent Overload (byval nod as NodePtr, dat as string)
Declare sub SetContent(byval nod as NodePtr, byval zstr as zstring ptr, byval size as integer)
Declare sub SetContent(byval nod as NodePtr, byval dat as longint)
Declare sub SetContent(byval nod as NodePtr, byval dat as double)
Declare sub SetContent(byval nod as NodePtr)
Declare Function AddSiblingBefore(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
Declare Function AddSiblingAfter(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
Declare Function AddChild(byval par as NodePtr, byval nod as NodePtr) as NodePtr
Declare sub SetRootNode(byval doc as DocPtr, byval nod as NodePtr)

Declare Function LoadDocument(fil as string, byval options as LoadOptions = optNone) as DocPtr
Declare Function LoadNode overload(byval ret as nodeptr, byval recursive as bool = YES) as bool

Declare sub SerializeXML overload (byval doc as DocPtr, byval fh as integer, byval debugging as integer = NO)
Declare sub SerializeXML (byval nod as NodePtr, byval fh as integer, byval debugging as integer, byval ind as integer = 0)

Declare sub SerializeBin overload (file as string, byval doc as DocPtr)

Declare Function GetString(byval node as nodeptr) as string
Declare Function GetInteger(byval node as nodeptr) as LongInt
Declare Function GetFloat(byval node as nodeptr) as Double
Declare Function GetZString(byval node as nodeptr) as ZString ptr
Declare Function ResizeZString(byval node as nodeptr, byval newsize as integer) as ZString ptr
Declare Function GetZStringSize(byval node as nodeptr) as integer

Declare Function GetChildByName(byval nod as NodePtr, byval nam as zstring ptr) as NodePtr 'NOT recursive
Declare Function FindChildByName(byval nod as NodePtr, nam as string) as NodePtr 'recursive depth first search

Declare Function DocumentRoot(byval doc as DocPtr) as NodePtr
Declare Function NumChildren(byval nod as NodePtr) as Integer
Declare Function NodeParent(byval nod as NodePtr) as NodePtr
Declare Function FirstChild(byval nod as NodePtr, byval name as zstring ptr = null) as NodePtr
Declare Function NextSibling(byval nod as NodePtr, byval name as zstring ptr = null) as NodePtr
Declare Function PrevSibling(byval nod as NodePtr, byval name as zstring ptr = null) as NodePtr
Declare Function NodeType(byval nod as NodePtr) as NodeTypes
Declare Function NodeName(byval nod as NodePtr) as String
Declare Sub SwapSiblingNodes(byval nod1 as NodePtr, byval nod2 as NodePtr)
Declare Function CloneNodeTree(byval nod as NodePtr, byval doc as DocPtr=0, byval nod_for_doc as NodePtr=0) as NodePtr
Declare Function NodeHasAncestor(byval nod as NodePtr, byval possible_parent as NodePtr) as integer

'Helper functions:
Declare Function SetChildNode Overload (byval parent as NodePtr, n as string) as NodePtr
Declare Function SetChildNode(byval parent as NodePtr, n as string, byval val as longint) as NodePtr
Declare Function SetChildNode(byval parent as NodePtr, n as string, byval val as double) as NodePtr
Declare Function SetChildNode(byval parent as NodePtr, n as string, val as string) as NodePtr
Declare Function GetChildNodeInt(byval parent as NodePtr, n as string, byval d as longint = 0) as longint
Declare Function GetChildNodeFloat(byval parent as NodePtr, n as string, byval d as double = 0.0) as Double
Declare Function GetChildNodeStr(byval parent as NodePtr, n as string, d as string = "") as string
Declare Function GetChildNodeBool(byval parent as NodePtr, n as string, byval d as integer = 0) as integer
Declare Function GetChildNodeExists(byval parent as NodePtr, n as string) as integer
Declare Function AppendChildNode Overload (byval parent as NodePtr, n as string) as NodePtr
Declare Function AppendChildNode(byval parent as NodePtr, n as string, byval val as longint) as NodePtr
Declare Function AppendChildNode(byval parent as NodePtr, n as string, byval val as double) as NodePtr
Declare Function AppendChildNode(byval parent as NodePtr, n as string, val as string) as NodePtr

Declare function ReadVLI overload(byval f as integer) as longint
declare Sub WriteVLI overload(byval f as integer, byval v as Longint)

Declare Function MemoryUsage(byval doc as DocPtr) as LongInt

#if defined(RELOADINTERNAL) or __FB_DEBUG__
	'ReloadBasic stuff

	Type RBNodeName
		nameindex as integer
		hash as uinteger
		name as zstring ptr
	End Type

	Declare Sub BuildNameIndexTable(byval doc as DocPtr, nodenames() as RBNodeName, byval func_num as integer, byval func_bits_size as integer, byval signature as integer, byval total_num_names as integer)
	Declare Function GetChildByNameIndex(byval nod as NodePtr, byval nameindex as integer) as NodePtr
#endif


End Namespace

#endif
