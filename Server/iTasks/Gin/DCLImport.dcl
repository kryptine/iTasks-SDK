definition module iTasks.Gin.DCLImport

import iTasks.Gin.Syntax
import iTasks.Gin.Types

from syntax import ::Type
from Data.Error import :: MaybeErrorString, :: MaybeError

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)

mapType :: Type -> GTypeExpression
