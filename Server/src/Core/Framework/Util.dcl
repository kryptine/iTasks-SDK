definition module Util

from TSt 			import :: Task
from Types			import :: TaskNr, :: User
from Void			import :: Void

import GenPrint, GenParse, GenVisualize, GenUpdate

derive gPrint 		Task, User, Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)
derive gParse 		Task, User, Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)

derive gVisualize	Task, User
derive gUpdate		Task, User

iTaskId 			:: !TaskNr !String 	-> String

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a

readfile			:: !String !*World -> (!String,!*World)
writefile			:: !String !String !*World -> *World