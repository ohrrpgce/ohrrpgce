#ifndef RELOAD_BI
#define RELOAD_BI

'OHRRPGCE COMMON - XML related functions
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
TYPE NodeSetPtr as NodeSet Ptr

#if defined(RELOADINTERNAL)
	TYPE Doc
		version as integer
		root as NodePtr
		strings as string ptr
		numStrings as integer
		numAllocStrings as integer
	END TYPE

	TYPE Node
		name as string
		namenum as short 'in the string table, used while loading
		nodeType as ubyte
		str as string 'I'd throw this into the union too, but can't :(
		Union 'this saves sizeof(Double) bytes per node!
			num as LongInt
			flo as Double
		end Union
		numChildren as integer
		children as NodePtr
		doc as DocPtr
		parent as NodePtr
		nextSib as NodePtr
		prevSib as NodePtr
	END TYPE
#else
	TYPE Doc
		thisIsPrivate as ubyte
	End Type
	TYPE Node
		thisIsPrivate as ubyte
	End Type
#endif

Type NodeSet
	numNodes as integer
	doc as DocPtr
	nodes as NodePtr Ptr
End Type

Type RPathFragment
	nodename as string
end Type

Type RPathCompiledQuery
	numFragments as integer
	fragment as RPathFragment ptr
End Type

Declare Function CreateDocument() as DocPtr
Declare Function CreateNode overload(byval doc as DocPtr, nam as string) as NodePtr
Declare Function CreateNode(byval nod as NodePtr, nam as string) as NodePtr
Declare sub FreeNode(byval nod as NodePtr)
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
Declare sub serializeBin (byval nod as NodePtr, byval f as integer = 0, byval doc as DocPtr)

Declare Function GetString(byval node as nodeptr) as string
Declare Function GetInteger(byval node as nodeptr) as LongInt
Declare Function GetFloat(byval node as nodeptr) as Double

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
Declare Function SetChildNode Overload (parent as NodePtr, n as string) as NodePtr
Declare Function SetChildNode(parent as NodePtr, n as string, val as longint) as NodePtr
Declare Function SetChildNode(parent as NodePtr, n as string, val as double) as NodePtr
Declare Function SetChildNode(parent as NodePtr, n as string, val as string) as NodePtr
Declare Function GetChildNodeInt(parent as NodePtr, n as string, d as longint = 0) as longint
Declare Function GetChildNodeFloat(parent as NodePtr, n as string, d as double = 0.0) as Double
Declare Function GetChildNodeStr(parent as NodePtr, n as string, d as string = "") as string
Declare Function GetChildNodeBool(parent as NodePtr, n as string, d as integer = 0) as integer
Declare Function GetChildNodeExists(parent as NodePtr, n as string) as integer


Declare function ReadVLI(byval f as integer) as longint
declare Sub WriteVLI(byval f as integer, byval v as Longint)

Declare Function RPathCompile(query as string) as RPathCompiledQuery Ptr
Declare Sub RPathFreeCompiledQuery(byval rpf as RPathCompiledQuery ptr)

Declare Function RPathQuery Overload(query as String, byval context as NodePtr) as NodeSetPtr
Declare Function RPathQuery Overload(byval query as RPathCompiledQuery Ptr, byval context as NodePtr) as NodeSetPtr

End Namespace

#endif
