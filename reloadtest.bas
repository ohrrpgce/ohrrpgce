
#include "reload.bi"

Using Reload

dim doc as DocPtr

doc = CreateDocument()

dim nod as NodePtr

nod = CreateNode(doc, "test")

DocSetRootNode(doc, nod)

nod = AddChild(nod, CreateNode(doc, "foo"))

nod = AddChild(nod, CreateNode(doc, "bar"))

SetContent(nod, 123)

nod = AddChild(nod->parent, CreateNode(doc, "bar"))

SetContent(nod, 456.789)

nod = AddChild(nod->parent->parent, CreateNode(doc, "bar"))

SetContent(nod, "The Rain in Spain Falls Mainly on the Plain")

serializeBin(doc)

FreeDocument(doc)
