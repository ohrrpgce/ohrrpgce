'OHRRPGCE COMMON - RELOAD related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "reload.bi"
#include "reloadext.bi"
#include "common_base.bi"

Namespace Reload.Ext

'This turns a node into a bitset (which is just a blob of binary data).
'If it contains any data already, it's gone now.
Sub CreateBitset(byval node as Nodeptr)
	if node = 0 then return
	
	SetContent(node, "")
end sub

Sub SetBitset(byval node as Nodeptr, byval bit as integer, byval v as integer)
	if node = 0 then return
	
	if NodeType(node) <> rltString then return
	
	dim as integer byt, b
	
	byt = bit \ 8
	b = bit mod 8
	
	if byt >= GetZStringSize(node) then
		if 0 = ResizeZString(node, byt + 1) then return 'memory failure...
	end if

	'Work around FB bug #662 gen gcc: zstring ptrs are indexed as char* instead of unsigned char* 
	dim d as ubyte ptr = GetZString(node)
	
	if v then
		d[byt] = d[byt] or (2 ^ b)
	else 
		d[byt] = d[byt] and not(2 ^ b)
	end if
End sub

Function GetBitset(byval node as Nodeptr, byval bit as integer) as integer
	if node = 0 then return 0
	
	if NodeType(node) <> rltString then return 0
	
	dim as integer byt, b
	
	byt = bit \ 8
	b = bit mod 8
	
	if byt >= GetZStringSize(node) then
		return 0
	end if
	
	dim d as ubyte ptr = GetZString(node)
	
	return 0 <> (d[byt] and (2 ^ b))
End function

'due to the needs of the client, although sizeof(bitset(0)) is 4, we are only using the lower 2 bytes.
sub LoadBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
	if node = 0 then return
	
	if NodeType(node) <> rltString then return

	dim d as ubyte ptr = GetZString(node)
	
	for i as integer = 0 to size * 2 step 2
		if i < GetZStringSize(node) then
			bs(i \ 2) = d[i]
			if i + 1 < GetZStringSize(node) then
				bs(i \ 2) or= d[i + 1] * 256
			end if
		else
			bs(i \ 2) = 0
		end if
	next
end sub

'due to the needs of the client, although sizeof(bitset(0)) is 4, we are only using the lower 2 bytes.
sub SaveBitsetArray(byval node as NodePtr, bs() as integer, byval size as integer)
	if node = 0 then return
	
	CreateBitset(node)
	
	ResizeZString(node, size * 2)
	
	dim d as ubyte ptr = GetZString(node)
	
	for i as integer = 0 to size - 1
		d[i * 2] = bs(i) and &hff
		d[i * 2 + 1] = (bs(i) \ 256) and &hff
	next
end sub

Function GetNodePath(byval node as NodePtr) as string
	dim prefix as string
	if NodeParent(node) then
		prefix = GetNodePath(NodeParent(node))
	end if
	return prefix & "/" & NodeName(node)
End Function

Function NodeByPath(byval doc as DocPtr, path as string) as NodePtr
	Return NodeByPath(DocumentRoot(doc), path)
End Function

Function NodeByPath(byval node as NodePtr, path as string) as NodePtr
	if node = null then return null
	if path = "" then return null
	if mid(path, 1, 1) <> "/" then
		debug "malformed path segment " & path
		return null
	end if
	
	dim remainder as string
	dim segment as string
	dim sep_pos as integer
	sep_pos = instr(2, path, "/")
	if sep_pos then
		segment = mid(path, 2, sep_pos - 2)
		remainder = mid(path, sep_pos)
	else
		segment = mid(path, 2)
	end if

	dim index as integer = 0
	dim use_index as integer = NO

	sep_pos = instr(segment, "[")
	if sep_pos then
	
		dim end_pos as integer
		end_pos = instr(segment, "]")
		if end_pos = 0 orelse end_pos <> len(segment) then
			debug "malformed path index " & segment
			return null
		end if
		
		dim index_str as string = mid(segment, sep_pos + 1, len(segment) - sep_pos - 1)
		index = str2int(index_str)
		if str(index) <> index_str then
			debug "malformed path index " & segment
			return null
		end if
		
		segment = mid(segment, 1, sep_pos - 1)
		
		use_index = YES
	end if

	dim child as NodePtr
	
	if use_index then
		child = FirstChild(node, segment)
		do while child
			if GetInteger(child) = index then exit do
			child = NextSibling(child, segment)
		loop
		if child = null then return null
	else
		child = GetChildByName(node, segment)
	end if
 
	if remainder <> "" then
		return NodeByPath(child, remainder)
	else
		return child
	end if

End Function

'For debug purposes: check that two RELOAD trees are equal
'pedantic: Insist on equality of types
'FIXME: the order of child nodes usually does not matter, would be good to have an option to ignore such differences
Function CompareNodes(byval nod1 as nodeptr, byval nod2 as nodeptr, byval pedantic as integer) as integer
	if NodeName(nod1) <> NodeName(nod2) then
		debug "Names of nodes differ! '" & GetNodePath(nod1) & "' vs '" & GetNodePath(nod2) & "'"
		return 1
	end if
	
	if pedantic then
		if NodeType(nod1) <> NodeType(nod2) then
			debug "Types of node " & GetNodePath(nod1) & " differ! " & NodeType(nod1) & " vs " & NodeType(nod2)
			return 1
		end if
		
		select case NodeType(nod1)
			case rltNull
			case rltInt
				if GetInteger(nod1) <> GetInteger(nod2) then
					debug "Value of node " & GetNodePath(nod1) & " differ! " & GetInteger(nod1) & " vs " & GetInteger(nod2)
					return 1
				end if
			case rltFloat
				if GetFloat(nod1) <> GetFloat(nod2) then
					debug "Value of node " & GetNodePath(nod1) & " differ! " & GetFloat(nod1) & " vs " & GetFloat(nod2)
					return 1
				end if
			case rltString
				if GetString(nod1) <> GetString(nod2) then
					debug "Value of node " & GetNodePath(nod1) & " differ! """ & GetString(nod1) & """ vs """ & GetString(nod2) & """"
					return 1
				end if
		end select
	else
		'This is too easy
		if GetString(nod1) <> GetString(nod2) then
			debug "Value of node " & GetNodePath(nod1) & " differ! """ & GetString(nod1) & """ vs """ & GetString(nod2) & """"
			return 1
		end if
	end if
			
	
	if NumChildren(nod1) <> NumChildren(nod2) then
		debug "Number of children on node " & GetNodePath(nod1) & " differ! " & NumChildren(nod1) & " vs " & NumChildren(nod2)
		return 1
	end if
	
	dim numkids as integer = NumChildren(nod1)
	dim ret as integer = 0  'I GUESS they're the same...
	
	nod1 = FirstChild(nod1)
	nod2 = FirstChild(nod2)
	for i as integer = 0 to numkids - 1
		if CompareNodes(nod1, nod2, pedantic) then
			'keep going, find all differing children (but we don't keep descending when a difference is found)
			ret = 1
		end if
		
		nod1 = NextSibling(nod1)
		nod2 = NextSibling(nod2)
	next
	
	return ret
End Function

'Sets (or creates, if it doesn't exist) a node holding an int-to-int key-value pair parented to 'parent', like so:
' * parent
'  * "<keyname>" int - <key>
'   * "int" int - <value>
'"int" can be optionally overridden.
'Returns a pointer to the 'value' node, so you can stuff in more data.
Function SetKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, byval value as integer = 0, valuename as string = "int") as NodePtr
	if parent = NULL then
		debug "SetKeyValueNode: NULL node ptr"
		return NULL
	end if

	dim n as NodePtr
	n = FirstChild(parent, keyname)
	while n
		if GetInteger(n) = key then
			'this key already exists: modify this node
			FreeChildren(n)
			return AppendChildNode(n, valuename, value)
		end if
		n = NextSibling(n, keyname)
	wend
	n = AppendChildNode(parent, keyname, key)
	return AppendChildNode(n, valuename, value)
End Function

'See SetKeyValueNode. Returns a pointer to the 'value' node.
Function GetKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, valuename as string = "int") as NodePtr
	if parent = NULL then
		debug "GetKeyValueNode: NULL node ptr"
		return NULL
	end if

	dim n as NodePtr
	n = FirstChild(parent, keyname)
	while n
		if GetInteger(n) = key then
			dim retnode as NodePtr = GetChildByName(n, valuename)
			if retnode = NULL then
				debug "GetKeyValueNode(" & NodeName(parent) & ", " & keyname & ", " & key & "): no '" & valuename & "' child!"
				return NULL
			end if
			return retnode
		end if
		n = NextSibling(n, keyname)
	wend
	return NULL
End Function

'More convenient form of GetKeyValueNode
Function ReadKeyValueNode (byval parent as NodePtr, keyname as string, byval key as integer, byval default as integer, valuename as string = "int") as integer
	dim n as NodePtr = GetKeyValueNode(parent, keyname, key, valuename)
	if n = NULL then
		return default
	else
		return GetInteger(n)
	end if
End Function

End Namespace
