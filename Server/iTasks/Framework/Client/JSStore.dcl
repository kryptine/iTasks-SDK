definition module iTasks.Framework.Client.JSStore

import StdString
import Data.Maybe
from iTasks.Framework.IWorld import :: IWorld

jsStoreValue	:: !String !String !a !*IWorld ->            *IWorld
jsLoadValue 	:: !String !String    !*IWorld -> (!Maybe a,!*IWorld)
jsDeleteValue	:: !String !String	  !*IWorld -> 		     *IWorld
jsDeleteValues 	:: !String !String    !*IWorld -> 		     *IWorld
