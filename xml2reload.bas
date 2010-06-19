#include "reload.bi"

#include "libxml/tree.bi"
#include "libxml/parser.bi"

using Reload

declare function chug(as xmlNodeptr, as DocPtr) as NodePtr
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
function chug(node as xmlNodeptr, dc as DocPtr) as NodePtr

	dim this as nodeptr

	select case node->type
		case XML_ELEMENT_NODE, XML_ATTRIBUTE_NODE 'this is container: either a <tag> or a <tag attribute="...">
			'create the RELOAD node
			if node->type = XML_ATTRIBUTE_NODE then
				'this is an attribute: <foo bar="1" />
				'Except, RELOAD doesn't do attributes. So, we reserve @ for those
				this = CreateNode(dc, "@" & *node->name)
			else
				this = CreateNode(dc, *node->name)
			end if
			
			'take a look at the attributes
			dim cur_node as xmlNodePtr = cast(xmlNodePtr, node->properties)
			do while cur_node <> null
				dim ch as nodeptr = chug(cur_node, dc)
				
				'add the new child to the document tree
				AddChild(this, ch)
				cur_node = cur_node->next
			loop

			'and the children
			cur_node = node->children
			do while cur_node <> null
				'recurse to parse the children
				dim ch as nodeptr = chug(cur_node, dc)
				
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
				'and, set the content to the value of this node, less any padding of spaces, tabs or new lines
				SetContent(this, trim(*node->content, any !" \t\n\r"))
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
	
	if NumChildren(node) = 1 ANDALSO NodeName(FirstChild(node)) = "" then 'this is the <>text</> wrapper
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
			case rltString 'raise the string
				SetContent(node, GetString(c))
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
