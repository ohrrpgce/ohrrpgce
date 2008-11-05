
#include "xml.bi"
#include "libxml/tree.bi"

xmlOpen

dim as textbox txt
txt.text(0) = "Test!"

dim as xmlDocPtr doc = xmlNewDoc("1.0")

dim as xmlNodePtr root = xmlNewNode(0, "textboxes")

xmlDocSetRootElement(doc, root)

dim as xmlNodePtr node = compileTextbox(txt)
xmlSetProp(node, "id", "1")
xmlAddChild(root, node)
node = compileTextbox(txt)
xmlSetProp(node, "id", "2")
xmlAddChild(root, node)


xmlSaveFormatFileEnc("-", doc, "UTF-8", 1)
xmlFreeDoc(doc)

xmlClose