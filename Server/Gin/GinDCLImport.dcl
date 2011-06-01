definition module GinDCLImport

from GinSyntax import ::GModule
import Error

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)
