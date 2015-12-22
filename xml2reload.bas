#include "reload.bi"
#include "cutil.bi"

#include "libxml/tree.bi"
#include "libxml/parser.bi"

using Reload

Enum encoding_t
	encNone
	encWS
	encBase64
end enum

declare function chug(node as xmlNodeptr, dc as DocPtr, encoded as encoding_t) as NodePtr
declare sub optimize(node as nodePtr)

dim shared reloadns as xmlNsPtr

dim as string infile, outfile

xmlCheckVersion(LIBXML_VERSION)

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

print "Memory usage: " & DocumentMemoryUsage(rldDoc)

dim xmlRoot as xmlNodeptr
xmlRoot = xmlDocGetRootElement(xmlDoc)

reloadns = xmlSearchNsByHref(xmlDoc, xmlRoot, @"http://hamsterrepublic.com/ohrrpgce/RELOAD")

dim rldRoot as NodePtr
rldRoot = chug(xmlRoot, rldDoc, encNone)

print "Parsed XML document in " & int((timer - starttime) * 1000) & " ms"

print "Memory usage: " & DocumentMemoryUsage(rldDoc)

starttime = timer

xmlFreeDoc(xmlDoc)

print "Freed XML document in " & int((timer - starttime) * 1000) & " ms"

starttime = timer

optimize(rldRoot)

print "Optimised document in " & int((timer - starttime) * 1000) & " ms"

print "Memory usage: " & DocumentMemoryUsage(rldDoc)

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


' This function takes an XML node and creates a RELOAD node based on it.
function chug(node as xmlNodeptr, dc as DocPtr, encoded as encoding_t) as NodePtr

	dim this as nodeptr

	select case node->type
		case XML_ELEMENT_NODE, XML_ATTRIBUTE_NODE 'this is container: either a '<tag>' or an 'attribute="..."'
			dim child_enc as encoding_t = encNone
			dim nodename as zstring ptr = cast(zstring ptr, node->name)

			'create the RELOAD node
			if node->type = XML_ATTRIBUTE_NODE then
				'this is an attribute: <foo bar="1" />
				'Except, RELOAD doesn't do attributes. So, we reserve @ for those
				this = CreateNode(dc, "@" & *nodename)
			else
				if node->ns = reloadns andalso *nodename = "_" then  'work around RELOAD supporting no-name nodes
					this = CreateNode(dc, "")
				elseif node->ns = reloadns andalso *nodename = "ws" then  'work around clobbering of whitespace
					this = CreateNode(dc, "$")  'this node will be squashed later
					child_enc = encWS
				else
					this = CreateNode(dc, *nodename)
				end if

				'take a look at the attributes
				dim cur_attr as xmlAttrPtr = node->properties
				do while cur_attr <> null
					dim ch as nodeptr
					if *cast(zstring ptr, cur_attr->name) = "encoding" andalso cur_attr->ns = reloadns then
						'How terribly bothersome. Get the (TEXT) value of this attribute
						ch = chug(cur_attr->children, dc, encNone)
						if GetString(ch) = "base64" then
							child_enc = encBase64
							FreeNode(ch)
						else
							print "Unknown encoding '" & GetString(ch) & "'"
							end
						end if
					else
						ch = chug(cast(xmlNodePtr, cur_attr), dc, encNone)
						
						'add the new child to the document tree
						AddChild(this, ch)
					end if
					cur_attr = cur_attr->next
				loop
			end if

			'and the children
			dim cur_node as xmlNodePtr = node->children
			do while cur_node <> null
				'recurse to parse the children
				dim ch as nodeptr = chug(cur_node, dc, child_enc)
				
				'add the new child to the document tree
				AddChild(this, ch)
				
				'move to the next child
				cur_node = cur_node->next
			loop

			'This is a hack to support SerializeXML debugging option: <r:ws></r:ws> results
			'in no child being appended
			if child_enc = encWS and this->numChildren = 0 then
				AppendChildNode(this, "$", "")
			end if
		case XML_TEXT_NODE 'this is any text data - aka, the content of "<foo>...</foo>"
			'if the text node is blank, we don't care about it unless we're inside <ws></ws>
			if xmlIsBlankNode(node) = 0 orelse encoded = encWS then
				dim content as zstring ptr = node->content
				if encoded = encBase64 then
				'create a node with a special name
					this = CreateNode(dc, "$")  'to be squashed
					'Trim whitespace, which the decode library doesn't like
					SetContent_base64(this, trim(*content, any !" \t\n\r"))
				elseif encoded = encWS then
					'Preserve whitespace and string status
					this = CreateNode(dc, "$$")  'to be squashed
					SetContent_utf8_garbage(this, *content)
				elseif encoded = encNone then
					'and, set the content to the value of this node, less any padding of spaces, tabs or new lines
					this = CreateNode(dc, "$")  'to be squashed
					SetContent_utf8_garbage(this, trim(*content, any !" \t\n\r"))
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
'it also squashes <foo><>content</></foo> wrappers
sub optimize(node as nodePtr)
	if NodeName(node) <> "$$" and NodeType(node) = rltString then  'preserve <r:ws> contents as strings
		'Basically, if the string can be parsed as a number, it will be. We need to back off a little bit
		'Eg, FB will parse "1234 dots on the door!" as 1234
		'I will parse it as a string
		if (ValLng(GetString(node)) <> 0 AND ValLng(GetString(node) & "1") <> ValLng(GetString(node))) or GetString(node) = "0" then
			SetContent(node, ValLng(GetString(node)))
		elseif (Val(GetString(node)) <> 0 AND Val(GetString(node) & "1") <> Val(GetString(node))) or GetString(node) = "0" then
			SetContent(node, Val(GetString(node)))
		end if
	end if

	dim as nodeptr c, nextc
	c = FirstChild(node)
	do while c <> null
		nextc = NextSibling(c)

		optimize(c)

		if NodeName(c) = "$" or NodeName(c) = "$$" then  'this is a <>text</> or <r:ws>text</r:ws> wrapper
			select case NodeType(c) 'figure out what kind of wrapper it is, and make it so
				case rltInt 'hoist the number up a level
					SetContent(node, GetInteger(c))
					FreeNode(c)
				case rltFloat 'lift the double
					SetContent(node, GetFloat(c))
					FreeNode(c)
				case rltString 'raise the string
					SetContent(node, GetString(c))
					FreeNode(c)
				case rltNull 'uh... remove all content.
					SetContent(node)
					FreeNode(c)
			end select
		end if

		c = nextc
	loop
end sub
