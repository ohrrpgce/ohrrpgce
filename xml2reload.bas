#include "libxml/tree.bi"
#include "libxml/parser.bi"

#include "reload.bi"

using Reload

declare function chug(as xmlNodeptr, as DocPtr, as integer = 0) as NodePtr
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

dim xmlRoot as xmlNodeptr
xmlRoot = xmlDocGetRootElement(xmlDoc)

dim rldRoot as NodePtr
rldRoot = chug(xmlRoot, rldDoc)

print "Parsed XML document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

optimize(rldRoot)

print "Optimised document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

SetRootNode(rldDoc, rldRoot)

SerializeBin(outfile, rldDoc)

print "Serialized document in " & int((timer - starttime) * 1000) & " ms"
starttime = timer

FreeDocument(rldDoc)

xmlFreeDoc(xmlDoc)

print "Tore down memory in " & int((timer - starttime) * 1000) & " ms"

print "Finished in " & int((timer - realStart) * 1000) & " ms"

' This function takes an XML node and creates a RELOAD node based on it.
function chug(node as xmlNodeptr, dc as DocPtr, ind as integer = 0) as NodePtr
	
	dim this as nodeptr
	
	select case node->type
		case XML_ELEMENT_NODE 'this is something like "<foo>...</foo>"
			
			'create the RELOAD node
			this = CreateNode(dc, *node->name)
			
			'take a look at the children
			dim cur_node as xmlNodePtr = node->children
			do while cur_node <> null
				'recurse to parse the children
				dim ch as nodeptr = chug(cur_node, dc, ind + 1)
				
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
		case XML_ATTRIBUTE_NODE 'this is an attribute: <foo bar="1" />
			'Except, RELOAD doesn't do attributes. So, we reserve @ for those
			print "@" & *node->name
			this = CreateNode(dc, "@" & *node->name)
			SetContent(this, *node->content)
		case else
			'Let's see, comments, CDATA sections, <?tags?> etc
			print "??? " & node->type
	end select
	
	
	
	return this
end function

'since all XML nodes are strings, this function figures out which can be represented by simpler data types
'it also fixes the <foo><>content</></foo> thing
sub optimize(node as nodePtr)
	if node->nodeType = rltString then
		'Basically, if the string can be parsed as a number, it will be. We need to back off a little bit
		'Eg, FB will parse "1234 dots on the door!" as 1234
		'I will parse it as a string
		if (ValLng(node->str) <> 0 AND ValLng(node->str & "1") <> ValLng(node->str)) or node->str = "0" then
			SetContent(node, ValLng(node->str))
		elseif (Val(node->str) <> 0 AND Val(node->str & "1") <> Val(node->str)) or node->str = "0" then
			SetContent(node, Val(node->str))
		end if
	end if
	
	if node->numChildren = 1 AND node->children->name = "" then 'this is the <>text</> wrapper
		select case node->children->nodeType 'figure out what kind of wrapper it is, and make it so
			case rltInt 'hoist the number up a level
				SetContent(node, node->children->num)
				FreeNode(node->children)
				node->children = null
				optimize(node)
			case rltFloat 'lift the double
				SetContent(node, node->children->flo)
				FreeNode(node->children)
				node->children = null
				optimize(node)
			case rltString 'raise the string
				dim s as string
				s = node->children->str
				SetContent(node, s)
				FreeNode(node->children)
				node->children = null
				optimize(node)
			case rltNull 'uh... remove all content.
				SetContent(node)
				FreeNode(node->children)
				node->children = null
				optimize(node)
		end select
	else 'whelp, nothing we can do here.
		dim tmp as nodeptr
		tmp = node->children
		do while tmp <> null
			print tmp
			'we do want to optimize the children, though.
			optimize(tmp)
			tmp = tmp->nextSib
		loop
	end if
end sub
