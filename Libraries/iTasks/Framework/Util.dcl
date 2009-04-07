definition module Util

from TSt 			import :: Task, :: Options, ::RaiseCondition
from Types			import :: TaskNr
from StdOverloaded	import class ==, class <
import iDataForms

derive gForm 	Task, Dynamic, RaiseCondition						
derive gUpd 	Task, Dynamic, RaiseCondition
derive gPrint 	Task, Dynamic, RaiseCondition
derive gParse 	Task, Dynamic, RaiseCondition
derive read 	Task, Dynamic, RaiseCondition
derive write 	Task, Dynamic, RaiseCondition

// Here follow some commonly used internal functions

/* Task number is used to generate a unique id
iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
*/

iTaskId 			:: !TaskNr !String 	-> String

// general iTask store, session store, page store, store but no form generation

cFormId 			:: !Options !String !a -> FormId a
sessionFormId 		:: !Options !String !a -> FormId a
pageFormId 			:: !Options !String !a -> FormId a
storageFormId 		:: !Options !String !a -> FormId a


