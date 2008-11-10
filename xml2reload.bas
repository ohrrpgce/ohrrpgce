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

function chug(node as xmlNodeptr, dc as DocPtr, ind as integer = 0) as NodePtr
	
	dim this as nodeptr
	
	select case node->type
		case XML_ELEMENT_NODE
			this = CreateNode(dc, *node->name)
			dim cur_node as xmlNodePtr = node->children
			do while cur_node <> null
				'print string(ind, " ") & *cur_node->name
				
				dim ch as nodeptr = chug(cur_node, dc, ind + 1)
				AddChild(this, ch)
				
				cur_node = cur_node->next
			loop
		case XML_TEXT_NODE
			if xmlIsBlankNode(node) = 0 then
				this = CreateNode(dc, "")
				SetContent(this, trim(*node->content, any !" \t\n"))
			end if
		case XML_ATTRIBUTE_NODE
			print "@" & *node->name
			this = CreateNode(dc, "@" & *node->name)
			SetContent(this, *node->content)
		case else
			print "??? " & node->type
	end select
	
	
	
	return this
end function

sub optimize(node as nodePtr)
	select case node->nodeType
		case rliString
			if Str(ValLng(node->str)) = node->str then
				SetContent(node, ValLng(node->str))
			end if
		case rliChildren
			if node->numChildren = 1 AND node->children->name = "" then
				select case node->children->nodeType
					case rliByte, rliInt, rliLong, rliShort
						SetContent(node, node->children->num)
						optimize(node)
					case rliFloat
						SetContent(node, node->children->flo)
						optimize(node)
					case rliString
						dim s as string
						s = node->children->str
						SetContent(node, s)
						optimize(node)
					case rliNull
						SetContent(node)
						optimize(node)
					case rliChildren 'invalid, but whatever.
						dim tmp as nodeptr
						node->children->children->parent = node
						tmp = node->children->children
						FreeNode(node->children)
						AddChild(node, tmp)
						
						optimize(node)
				end select
			else
				dim tmp as nodeptr
				tmp = node->children
				do while tmp <> null
					optimize(tmp)
					tmp = tmp->nextSib
				loop
			end if
	end select
end sub
