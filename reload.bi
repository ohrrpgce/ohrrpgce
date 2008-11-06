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

ENUM NodeInTypes
	rliNull = 0
	rliByte = 1
	rliShort = 2
	rliInt = 3
	rliLong = 4
	rliFloat = 5
	rliString = 6
	rliChildren = 8
END ENUM

ENUM NodeTypes
	rltNull
	rltInt
	rltFloat
	rltString
	rltChildren
END ENUM

TYPE ReloadDocPtr as ReloadDoc ptr
TYPE ReloadNodePtr as ReloadNode ptr

TYPE ReloadDoc
	version as integer
	root as ReloadNodePtr
END TYPE

TYPE ReloadNode
	name as string
	nodeType as ubyte
	str as string
	num as LongInt
	flo as Double
	numChildren as integer
	children as ReloadNodePtr
	doc as ReloadDocPtr
	parent as ReloadNodePtr
	nextSib as ReloadNodePtr
	prevSib as ReloadNodePtr
END TYPE

Declare Function CreateReloadDocument() as ReloadDocPtr
Declare Function CreateReloadNode(doc as ReloadDocPtr, nam as string) as ReloadNodePtr
Declare sub FreeReloadNode(nod as ReloadNodePtr)
Declare sub FreeReloadDocument(doc as ReloadDocPtr)
Declare sub ReloadSetContent Overload (nod as ReloadNodePtr, dat as string)
Declare sub ReloadSetContent(nod as ReloadNodePtr, dat as longint)
Declare sub ReloadsetContent(nod as ReloadNodePtr, dat as double)
Declare Function ReloadAddSiblingBefore(sib as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr
Declare Function ReloadAddSiblingAfter(sib as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr
Declare Function ReloadAddChild(par as ReloadNodePtr, nod as ReloadNodePtr) as ReloadNodePtr
Declare sub ReloadDocSetRootNode(doc as ReloadDocPtr, nod as ReloadNodePtr)


Declare sub SerializeXML overload (doc as ReloadDocPtr)
Declare sub serializeXML (nod as ReloadNodePtr, ind as integer = 0)

Declare sub SerializeBin overload (doc as ReloadDocPtr)
Declare sub serializeBin (nod as ReloadNodePtr, f as integer = 0, table() as string)

#endif
