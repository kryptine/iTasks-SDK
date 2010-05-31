definition module Util

from Types			import :: TaskNr
from Void			import :: Void

import GenPrint, GenParse, GenVisualize, GenUpdate, GenVerify

derive gPrint 		Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)
derive gParse 		Dynamic, Maybe, Void, (,), (,,), (,,,), (,,,,)

iTaskId 			:: !TaskNr !String 	-> String

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a

readfile			:: !String !*World -> (!String,!*World)
writefile			:: !String !String !*World -> *World