definition module GinDCLImport

import GinSyntax
import GinTypes

from syntax import ::Type
//import Error

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)

mapType :: Type -> GTypeExpression
