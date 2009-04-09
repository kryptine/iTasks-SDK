definition module Util

from TSt 			import :: Task, :: Options, ::ChangeCondition
from Types			import :: TaskNr
from StdOverloaded	import class ==, class <
import iDataForms

derive gForm 	Task, Dynamic, ChangeCondition						
derive gUpd 	Task, Dynamic, ChangeCondition
derive gPrint 	Task, Dynamic, ChangeCondition
derive gParse 	Task, Dynamic, ChangeCondition
derive read 	Task, Dynamic, ChangeCondition
derive write 	Task, Dynamic, ChangeCondition

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


