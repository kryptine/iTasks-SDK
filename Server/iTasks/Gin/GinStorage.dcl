definition module iTasks.Gin.Storage

from Error import :: MaybeErrorString, :: MaybeError

//import OSError
//import Maybe
//import Void
from iTasks.Framework.Task import ::Task

import iTasks.Gin.Syntax
import iTasks.Gin.Config

searchPathModules :: !GinConfig !*World -> ([String], *World)

readModule :: !GinConfig !String !*World -> (MaybeErrorString GModule, *World)
importModules :: !GinConfig ![String] !*World -> (MaybeErrorString [GModule], *World)
writeModule :: !GinConfig !String !GModule -> Task Void
newModuleName :: !GinConfig -> Task String
chooseModule :: !GinConfig -> Task (Maybe (!String, !GModule))

