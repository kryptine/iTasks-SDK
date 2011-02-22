definition module GinEditor

import iTasks
import GinSyntax
import GinDomain

ginSetup :: Task Void
ginEditor :: Task Void

//Bimaps on GModule
diagramView :: GModule -> ORYXEditor
diagramUpdate :: ORYXEditor GModule -> GModule
typeView :: GModule -> [GTypeDefinition]
typeUpdate :: [GTypeDefinition] GModule -> GModule
codeView :: GModule -> (Note, Note)
codeUpdate :: (Note, Note) GModule -> GModule
