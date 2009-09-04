definition module Util

from TSt 			import :: Task
from Types			import :: TaskNr
from Void			import :: Void

import GenPrint, GenParse, GenVisualize, GenUpdate

//TODO: Create visualization and update for tasks
derive gPrint 	Task, Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)
derive gParse 	Task, Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)

derive gVisualize Task
derive gUpdate Task

/* Task number is used to generate a unique id
iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
*/

iTaskId 			:: !TaskNr !String 	-> String

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a

readfile			:: !String !*World -> (!String,!*World)
writefile			:: !String !String !*World -> *World