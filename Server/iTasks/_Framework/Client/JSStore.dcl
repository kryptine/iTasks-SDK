definition module iTasks._Framework.Client.JSStore

import StdString
import Data.Maybe
from iTasks._Framework.IWorld import :: IWorld

jsStoreValue	:: !String !String !a !*IWorld ->            *IWorld
jsLoadValue 	:: !String !String    !*IWorld -> (!Maybe a,!*IWorld)
jsDeleteValue	:: !String !String	  !*IWorld -> 		     *IWorld
jsDeleteValues 	:: !String !String    !*IWorld -> 		     *IWorld
