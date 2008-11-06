
#include "reload.bi"

dim doc as reloadDocPtr

doc = CreateReloadDocument()

dim nod as reloadNodePtr

nod = CreateReloadNode(doc, "test")

ReloadDocSetRootNode(doc, nod)

nod = ReloadAddChild(nod, CreateReloadNode(doc, "foo"))

nod = ReloadAddChild(nod, CreateReloadNode(doc, "bar"))

ReloadSetContent(nod, 123)

nod = ReloadAddChild(nod->parent, CreateReloadNode(doc, "bar"))

ReloadSetContent(nod, 456.789)

nod = ReloadAddChild(nod->parent->parent, CreateReloadNode(doc, "bar"))

ReloadSetContent(nod, "The Rain in Spain Falls Mainly on the Plain")

serializeBin(doc)

FreeReloadDocument(doc)
