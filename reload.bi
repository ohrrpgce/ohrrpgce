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
	rliChildren = 7
END ENUM

ENUM NodeTypes
	rltNull
	rltInt
	rltFloat
	rltString
	rltChildren
END ENUM

TYPE DocPtr as Doc ptr
TYPE NodePtr as Node ptr

TYPE Doc
	version as integer
	root as NodePtr
END TYPE

TYPE Node
	name as string
	namenum as short 'in the string table, used while loading
	nodeType as ubyte
	str as string
	num as LongInt
	flo as Double
	numChildren as integer
	children as NodePtr
	doc as DocPtr
	parent as NodePtr
	nextSib as NodePtr
	prevSib as NodePtr
END TYPE

Declare Function CreateDocument() as DocPtr
Declare Function CreateNode(doc as DocPtr, nam as string) as NodePtr
Declare sub FreeNode(nod as NodePtr)
Declare sub FreeDocument(doc as DocPtr)
Declare sub SetContent Overload (nod as NodePtr, dat as string)
Declare sub SetContent(nod as NodePtr, dat as longint)
Declare sub setContent(nod as NodePtr, dat as double)
Declare sub setContent(nod as NodePtr)
Declare Function AddSiblingBefore(sib as NodePtr, nod as NodePtr) as NodePtr
Declare Function AddSiblingAfter(sib as NodePtr, nod as NodePtr) as NodePtr
Declare Function AddChild(par as NodePtr, nod as NodePtr) as NodePtr
Declare sub SetRootNode(doc as DocPtr, nod as NodePtr)

Declare Function LoadDocument(fil as string) as DocPtr

Declare sub SerializeXML overload (doc as DocPtr)
Declare sub serializeXML (nod as NodePtr, ind as integer = 0)

Declare sub SerializeBin overload (file as string, doc as DocPtr)
Declare sub serializeBin (nod as NodePtr, f as integer = 0, table() as string)

Declare Function GetString(node as nodeptr) as string
Declare Function GetInteger(node as nodeptr) as LongInt
Declare Function GetFloat(node as nodeptr) as Double


End Namespace

#endif
