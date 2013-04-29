definition module GinDCLImport

import GinSyntax
import GinTypes

from syntax import ::Type
from Error import :: MaybeErrorString, :: MaybeError

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)

mapType :: Type -> GTypeExpression
