definition module Util

from TSt 			import :: Task
from Types			import :: TaskNr
from Void			import :: Void

import GenPrint, GenParse

//TODO: Create visualization and update for tasks
derive gPrint 	Task, Dynamic, Maybe, Void, (,)
derive gParse 	Task, Dynamic, Maybe, Void, (,)

/* Task number is used to generate a unique id
iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
*/

iTaskId 			:: !TaskNr !String 	-> String
