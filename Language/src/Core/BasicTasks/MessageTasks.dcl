definition module MessageTasks

import StdList
import Html, TSt

from InteractionTasks import class html(..)

showInstruction 		:: !String !instruction		-> Task Void | html instruction
showInstructionAbout 	:: !String !instruction b 	-> Task Void | html instruction & iTask b