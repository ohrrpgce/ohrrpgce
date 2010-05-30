#ifndef RELOAD_BI
#define RELOAD_BI

'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "udts.bi"

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
	rliArray = 7
END ENUM

ENUM NodeTypes
	rltNull
	rltInt
	rltFloat
	rltString
	rltArray	'never actually appears; who knows whether it works?
END ENUM

ENUM LoadOptions
	optNone = 0
	optNoDelay = 1
END ENUM

TYPE DocPtr as Doc ptr
TYPE NodePtr as Node ptr

#if defined(RELOADINTERNAL) or __FB_DEBUG__
#if defined(__FB_WIN32__)
#include "windows.bi"
#endif
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
	END TYPE

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
Declare sub FreeNode(byval nod as NodePtr, byval options as integer = 0) 'don't use options.
Declare sub FreeDocument(byval doc as DocPtr)
Declare sub SetContent Overload (byval nod as NodePtr, dat as string)
Declare sub SetContent(byval nod as NodePtr, byval dat as longint)
Declare sub setContent(byval nod as NodePtr, byval dat as double)
Declare sub setContent(byval nod as NodePtr)
Declare Function AddSiblingBefore(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
Declare Function AddSiblingAfter(byval sib as NodePtr, byval nod as NodePtr) as NodePtr
Declare Function AddChild(byval par as NodePtr, byval nod as NodePtr) as NodePtr
Declare sub SetRootNode(byval doc as DocPtr, byval nod as NodePtr)

Declare Function LoadDocument(fil as string, byval options as LoadOptions = optNone) as DocPtr

Declare sub SerializeXML overload (byval doc as DocPtr)
Declare sub serializeXML (byval nod as NodePtr, byval ind as integer = 0)

Declare sub SerializeBin overload (file as string, byval doc as DocPtr)

Declare Function GetString(byval node as nodeptr) as string
Declare Function GetInteger(byval node as nodeptr) as LongInt
Declare Function GetFloat(byval node as nodeptr) as Double
Declare Function GetZString(byval node as nodeptr) as ZString ptr
Declare Function ResizeZString(byval node as nodeptr, byval newsize as integer) as ZString ptr
Declare Function GetZStringSize(byval node as nodeptr) as integer

Declare Function GetChildByName(byval nod as NodePtr, nam as string) as NodePtr 'NOT recursive
Declare Function FindChildByName(byval nod as NodePtr, nam as string) as NodePtr 'recursive depth first search

Declare Function DocumentRoot(byval doc as DocPtr) as NodePtr
Declare Function NumChildren(byval nod as NodePtr) as Integer
Declare Function FirstChild(byval nod as NodePtr) as NodePtr
Declare Function NextSibling(byval nod as NodePtr) as NodePtr
Declare Function PrevSibling(byval nod as NodePtr) as NodePtr
Declare Function NodeType(byval nod as NodePtr) as NodeTypes
Declare Function NodeName(byval nod as NodePtr) as String

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

Declare Sub TestStringTables()

End Namespace

#endif
