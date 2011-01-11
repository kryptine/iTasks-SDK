definition module GinFlowLibrary

from GinSyntax import ::GModule, ::GImport
from GinParser import ::GParseState
from GinBindings import ::Bindings, ::Binding

addDefaultLibrary :: GModule -> GModule

importBindings :: GImport -> GParseState Bindings

