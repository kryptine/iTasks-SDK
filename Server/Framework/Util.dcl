definition module Util

import Types

readfile			:: !String !*World -> (!String,!*World)
writefile			:: !String !String !*World -> *World

fileExtension		:: !String -> String
baseName			:: !String -> String

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])
instance toString (Maybe a) | toString a

pad					:: Int Int -> String
decFormat			:: Int -> String

mapSt				:: (a *st -> (b,*st)) [a] *st -> ([b],*st)

// Functions for accessing dates and times
currentTime 	:: !*World -> (!Time,!*World)
currentDate 	:: !*World -> (!Date,!*World)
currentDateTime :: !*World -> (!DateTime,!*World)