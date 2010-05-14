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

End Namespace

#endif