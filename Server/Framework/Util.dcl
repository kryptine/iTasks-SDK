definition module Util

import StdGeneric, Types

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

// Utility functions for generics
fromOBJECT	:: (OBJECT x)	-> x
fromCONS	:: (CONS x)		-> x
fromFIELD	:: (FIELD x)	-> x
fromPAIRX	:: (PAIR x y)	-> x
fromPAIRY	:: (PAIR x y)	-> y
