#include "reload.bi"
#include "cutil.bi"

#include "libxml/tree.bi"
#include "libxml/parser.bi"

using Reload

declare function chug(node as xmlNodeptr, dc as DocPtr, base64_encoded as integer = 0) as NodePtr
declare sub optimize(node as nodePtr)


dim as string infile, outfile

LIBXML_TEST_VERSION( )

infile = command(1)
outfile = command(2)

if infile = "" then
	print "Usage:"
	print command(0) & " infile.xml outfile.rld"
	end
end if

if outfile = "" then
	print "Usage:"
	print command(0) & " infile.xml outfile.rld"
	end
end if

dim as double startTime = Timer, realStart = Timer

dim xmlDoc as xmlDocPtr
xmlDoc = xmlReadFile(infile, 0, 0)

if xmlDoc = null then
	print "Could not read document!"
	end
end if

print "Loaded XML document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

dim rldDoc as Docptr
rldDoc = CreateDocument()

print "Memory usage: " & MemoryUsage(rldDoc)

dim xmlRoot as xmlNodeptr
xmlRoot = xmlDocGetRootElement(xmlDoc)

dim rldRoot as NodePtr
rldRoot = chug(xmlRoot, rldDoc)

print "Parsed XML document in " & int((timer - starttime) * 1000) & " ms"

print "Memory usage: " & MemoryUsage(rldDoc)

starttime = timer

xmlFreeDoc(xmlDoc)

print "Freed XML document in " & int((timer - starttime) * 1000) & " ms"

starttime = timer

optimize(rldRoot)

print "Optimised document in " & int((timer - starttime) * 1000) & " ms"

print "Memory usage: " & MemoryUsage(rldDoc)

starttime = timer

SetRootNode(rldDoc, rldRoot)

SerializeBin(outfile, rldDoc)

print "Serialized document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

FreeDocument(rldDoc)

print "Tore down memory in " & int((timer - starttime) * 1000) & " ms"

print "Finished in " & int((timer - realStart) * 1000) & " ms"


'This sub sets a node's content to binary data, calling the Base64 decoder which is in base64.c
sub SetContent_base64(byval this as nodeptr, byval encoded as zstring ptr)
	'This does not compute the exact length (may overestimate), find that out later
	dim outlen as integer = 3 * (len(*encoded) \ 4) + 2

	'Change to a string, then reserve enough space
	SetContent(this, NULL, outlen)  'An uninitialised binary blob

	if base64_decode(encoded, len(*encoded), GetZString(this), @outlen) = 0 then
		print "Malformed Base64 string, decode failure after " & outlen & " bytes!"
		end
	end if

	'Now we set the length correctly
	ResizeZString(this, outlen)

	'optimize will still try to process this node, but w/e. This is the only decently fast code in this file
end sub

'reload2xml slaps a iso-8859-1 (aka Latin 1) header on things, but libxml will parse it into unicode and feed us UTF8.
'Go back to Latin 1 to undo that mess (in the process, foil any attempts to create unicode RELOAD documents if the file
'was something other than iso-8859-1)
sub SetContent_utf8_garbage(byval this as nodeptr, byval garbled as zstring ptr)
	'Change to a string, then reserve enough space - length of the decoded string is less than or
	'equal to the length of the source string, so use that as estimation
	SetContent(this, NULL, len(*garbled))  'An uninitialised binary blob

	dim outlen as integer = this->strSize
	dim inlen as integer = len(*garbled)  'what's the point of passing this by pointer?

	outlen = UTF8Toisolat1(this->str, @outlen, garbled, @inlen)

	if outlen = -2 then
		print "Warning: this XML contains unicode not expressible in the Latin-1 encoding. Importing a string as raw UTF8"
		*this->str = *garbled
	elseif outlen = -1 then
		print "UTF8Toisolat1 unspecified failure!"
		end
	end if

	'Now we set the length correctly
	ResizeZString(this, outlen)
end sub


''''     libxml-tree mini-documentation
'
'The following xml:
'
'  <foo attr="42">bar <spam>more spam</spam></foo>
'
'is parsed by libxml to the following tree:
'
'  ELEMENT:<name = "foo", properties = {
'    ATTRIBUTE:<name = "attr", children = {
'      TEXT:<content = "42">
'    }>
'  }, children = {
'    TEXT:<content = "bar ">,
'    ELEMENT:<name = "spam", children = {
'      TEXT:<content = "more spam">
'    }>
'  }>
'
'where FOO:<a = b, c = {d}> means an xmlNode of type XML_FOO_NODE where the value of a is b, and
'c points to a doubly linked list. content & name are "" if not specified, and children and
'properties are NULL if not specified.

function in_reload_ns(node as xmlNodeptr) as integer
	'If you're picky, you could instead check *node->ns->href = "http://hamsterrepublic.com/ohrrpgce/RELOAD"
	return node->ns andalso *node->ns->prefix = "reload"
end function

' This function takes an XML node and creates a RELOAD node based on it.
function chug(node as xmlNodeptr, dc as DocPtr, base64_encoded as integer = 0) as NodePtr

	dim this as nodeptr

	select case node->type
		case XML_ELEMENT_NODE, XML_ATTRIBUTE_NODE 'this is container: either a '<tag>' or an 'attribute="..."'
			'create the RELOAD node
			if node->type = XML_ATTRIBUTE_NODE then
				'this is an attribute: <foo bar="1" />
				'Except, RELOAD doesn't do attributes. So, we reserve @ for those
				this = CreateNode(dc, "@" & *node->name)
			else
				if *node->name = "_" andalso in_reload_ns(node) then  'work around RELOAD supporting no-name nodes
					'work around some more problems with libxml2 inserting no-name wrapper nodes
					this = CreateNode(dc, "$")  'this is not a valid XML node name, so we can recognise it later in optimize
				else
					this = CreateNode(dc, *node->name)
				end if
			end if
			
			'take a look at the attributes
			dim is_base64_node as integer = 0
			dim cur_node as xmlNodePtr = cast(xmlNodePtr, node->properties)
			do while cur_node <> null
				dim ch as nodeptr
				if *cur_node->name = "encoding" andalso in_reload_ns(cur_node) then
					'How terribly bothersome. Get the (TEXT) value of this attribute
					ch = chug(cur_node->children, dc)
					if GetString(ch) = "base64" then
						is_base64_node = -1
						FreeNode(ch)
					else
						print "Unknown encoding '" & GetString(ch) & "'"
						end
					end if
				else
					ch = chug(cur_node, dc)
					
					'add the new child to the document tree
					AddChild(this, ch)
				end if
				cur_node = cur_node->next
			loop

			'and the children
			cur_node = node->children
			do while cur_node <> null
				'recurse to parse the children
				dim ch as nodeptr = chug(cur_node, dc, is_base64_node)
				
				'add the new child to the document tree
				AddChild(this, ch)
				
				'move to the next child
				cur_node = cur_node->next
			loop
		case XML_TEXT_NODE 'this is any text data - aka, the content of "<foo>...</foo>"
			'if the text node is blank, we don't care about it
			if xmlIsBlankNode(node) = 0 then
				'otherwise, create a node with a null name
				this = CreateNode(dc, "")

				if base64_encoded then
					'Trim whitespace, which the decode library doesn't like
					SetContent_base64(this, trim(*node->content, any !" \t\n\r"))
				else
					'and, set the content to the value of this node, less any padding of spaces, tabs or new lines
					SetContent_utf8_garbage(this, trim(*node->content, any !" \t\n\r"))
				end if
			end if
		case XML_PI_NODE 'we don't support these.
		case else
			'Let's see, comments, CDATA sections, <?tags?> etc
			print "??? " & node->type
	end select
	
	
	
	return this
end function

'since all XML nodes are strings, this function figures out which can be represented by simpler data types
'it also fixes the <foo><>content</></foo> thing
'and the null-name node thing
sub optimize(node as nodePtr)
	if NodeType(node) = rltString then
		'Basically, if the string can be parsed as a number, it will be. We need to back off a little bit
		'Eg, FB will parse "1234 dots on the door!" as 1234
		'I will parse it as a string
		if (ValLng(GetString(node)) <> 0 AND ValLng(GetString(node) & "1") <> ValLng(GetString(node))) or GetString(node) = "0" then
			SetContent(node, ValLng(GetString(node)))
		elseif (Val(GetString(node)) <> 0 AND Val(GetString(node) & "1") <> Val(GetString(node))) or GetString(node) = "0" then
			SetContent(node, Val(GetString(node)))
		end if
	end if

	'This is meant to be a genuine no-name RELOAD node
	if NodeName(node) = "$" then RenameNode(node, "")

	if NumChildren(node) >= 1 ANDALSO NodeName(FirstChild(node)) = "" then  'this is the <>text</> wrapper (notice must be first child!)
		dim c as nodeptr = FirstChild(node)
		select case NodeType(c) 'figure out what kind of wrapper it is, and make it so
			case rltInt 'hoist the number up a level
				SetContent(node, GetInteger(c))
				FreeNode(c)
				optimize(node)
			case rltFloat 'lift the double
				SetContent(node, GetFloat(c))
				FreeNode(c)
				optimize(node)
			case rltString 'raise the (underlying z)string
				SetContent(node, GetZString(c), GetZStringSize(c))
				FreeNode(c)
				optimize(node)
			case rltNull 'uh... remove all content.
				SetContent(node)
				FreeNode(c)
				optimize(node)
		end select
	else 'whelp, nothing we can do here.
		dim tmp as nodeptr
		tmp = FirstChild(node)
		do while tmp <> null
			'print tmp
			'we do want to optimize the children, though.
			optimize(tmp)
			tmp = NextSibling(tmp)
		loop
	end if
end sub
