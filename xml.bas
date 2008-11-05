'OHRRPGCE COMMON - XML related functions
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "libxml/tree.bi"
#include "libxml/parser.bi"

#include "xml.bi"

const nsTextbox = "http://hamsterrepublic.com/ohrrpgce/textbox" 'to be changed...

function createNS(byval ns as string, byval node as xmlNodePtr, byval prefix as string = "") as xmlNsPtr
	dim as xmlnsptr n
	if prefix <> "" then
		n = xmlNewNS(node, ns, prefix)
	else
		n = xmlNewNS(node, ns, 0) 'default namespace
	end if
	xmlSetNS(node, n)
	return n
end function

function compileTextbox(byref txt as Textbox) as xmlNodePtr
	dim as xmlDocPtr doc = 0
	dim as xmlNodePtr root = 0
	dim as xmlNsPtr ns = 0
	
	'doc = xmlNewDoc("1.0")
	
	root = xmlNewNode(0, "textbox")
	'ns = createNS(nsTextbox, root, "say")

	'xmlDocSetRootElement(doc, root)
	
	dim as xmlNodePtr textNode = xmlNewNode(ns, "text")
	
	for i as integer = 0 to 7
		if txt.text(i) <> "" then
			dim as xmlNodePtr node = xmlNewNode(ns, "line")
			xmlAddChild(node, xmlNewText(txt.text(i)))
			xmlAddChild(textNode, node)
		end if
	next
	if textNode->children = 0 then
		xmlFreeNode(textNode)
		textNode = 0
	else
		textNode = xmlAddChild(root, textNode)
	end if
	
	return root
end function

sub xmlClose 'call at program finish
	xmlCleanupParser()
end sub

sub xmlOpen 'call at program start

end sub
