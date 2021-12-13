'OHRRPGCE - RELOAD related functions
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef RELOADEXT_BI
#define RELOADEXT_BI

#include "reload.bi"

Namespace Reload.Ext
Declare Sub CreateBitset(byval node as Nodeptr)
Declare Sub SetBitset(byval node as Nodeptr, byval bitnum as integer, byval value as bool)
Declare Function GetBitset(byval node as Nodeptr, byval bitnum as integer) as bool
Declare sub LoadBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
Declare sub SaveBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
Declare Function GetNodePath(byval node as NodePtr) as string
Declare Function NodeByPath overload (byval doc as DocPtr, path as zstring ptr, byval create as bool=NO) as NodePtr
Declare Function NodeByPath overload (byval node as NodePtr, path as zstring ptr, byval create as bool=NO) as NodePtr
Declare Function CompareNodes(nod1 as NodePtr, nod2 as NodePtr, pedantic as bool = NO, logdiffs as bool = NO) as bool

Declare Function SetKeyValueNode (byval parent as NodePtr, keyname as zstring ptr, byval key as integer, byval value as integer = 0, valuename as zstring ptr = @"int") as NodePtr
Declare Function GetKeyValueNode (byval parent as NodePtr, keyname as zstring ptr, byval key as integer, valuename as zstring ptr = @"int") as NodePtr
Declare Function ReadKeyValueNode (byval parent as NodePtr, keyname as zstring ptr, byval key as integer, byval default as integer, valuename as zstring ptr = @"int") as integer

End Namespace

#endif
