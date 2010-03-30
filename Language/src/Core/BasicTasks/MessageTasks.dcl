definition module MessageTasks

import StdList
import Html, TSt

from InteractionTasks import class html(..)

displayInstruction 		:: !String !instruction		-> Task Void | html instruction
displayInstructionAbout :: !String !instruction b 	-> Task Void | html instruction & iTask b