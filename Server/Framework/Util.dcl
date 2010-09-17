definition module Util

from Types			import :: TaskNr
from Void			import :: Void
from Either			import :: Either

import GenVisualize, GenUpdate, GenVerify

derive JSONEncode	Dynamic, Void, Either, Timestamp
derive JSONDecode	Dynamic, Void, Either, Timestamp

derive gVisualize	Timestamp
derive gUpdate		Timestamp
derive gVerify		Timestamp

class iTaskId a
where
	iTaskId 			:: !a !String 	-> String
	
instance iTaskId TaskNr
instance iTaskId TaskId

(+++>) infixr 5		:: !a	!String	-> String | gVisualize{|*|} a
(<+++) infixl 5		:: !String	!a	-> String | gVisualize{|*|} a

readfile			:: !String !*World -> (!String,!*World)
writefile			:: !String !String !*World -> *World

fileExtension		:: !String -> String
baseName			:: !String -> String

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

pad					:: Int Int -> String
decFormat			:: Int -> String

mapSt				:: (a *st -> (b,*st)) [a] *st -> ([b],*st)

// Functions for accessing dates and times
currentTime 	:: !*World -> (!Time,!*World)
currentDate 	:: !*World -> (!Date,!*World)
currentDateTime :: !*World -> (!DateTime,!*World)