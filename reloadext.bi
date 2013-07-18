#ifndef RELOADEXT_BI
#define RELOADEXT_BI

'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "reload.bi"

Namespace Reload.Ext
Declare Sub CreateBitset(byval node as Nodeptr)
Declare Sub SetBitset(byval node as Nodeptr, byval bit as integer, byval v as integer)
Declare Function GetBitset(byval node as Nodeptr, byval bit as integer) as integer
Declare sub LoadBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
Declare sub SaveBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
Declare Function GetNodePath(byval node as NodePtr) as string
Declare Function NodeByPath Overload (byval doc as DocPtr, path as string) as NodePtr
Declare Function NodeByPath(byval node as NodePtr, path as string) as NodePtr
Declare Function CompareNodes(byval nod1 as nodeptr, byval nod2 as nodeptr, byval pedantic as integer) as integer

Declare Function SetKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, byval value as integer = 0, valuename as string = "int") as NodePtr
Declare Function GetKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, valuename as string = "int") as NodePtr
Declare Function ReadKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, byval default as integer, valuename as string = "int") as integer

End Namespace

#endif
