definition module GinStorage

from Error import :: MaybeErrorString, :: MaybeError

//import OSError
//import Maybe
//import Void
from Task import ::Task

import GinSyntax
import GinConfig

searchPathModules :: !GinConfig !*World -> ([String], *World)

readModule :: !GinConfig !String !*World -> (MaybeErrorString GModule, *World)
importModules :: !GinConfig ![String] !*World -> (MaybeErrorString [GModule], *World)
writeModule :: !GinConfig !String !GModule -> Task Void
newModuleName :: !GinConfig -> Task String
chooseModule :: !GinConfig -> Task (Maybe (!String, !GModule))

