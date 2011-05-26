definition module Util

import StdGeneric, Types, Error

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])
instance toString (Maybe a) | toString a
voidNothing 		:: Maybe Void

mb2error			:: !e !(Maybe a) -> MaybeError e a

pad					:: !Int !Int -> String
decFormat			:: !Int -> String

app					:: !(.a -> .b) !.a -> .b
seqSt				:: !(a .st -> .st)			![a] !.st -> .st
mapSt				:: !(a .st -> (!b,!.st))	![a] !.st -> (![b],!.st)

// Functions for accessing dates and times
currentTime 			:: !*IWorld -> (!Time,!*IWorld)
currentDate 			:: !*IWorld -> (!Date,!*IWorld)
currentDateTime 		:: !*IWorld -> (!DateTime,!*IWorld)
currentTimestamp		:: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestampError	:: !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
currentDateTimeWorld	:: !*World	-> (!DateTime,!*World)

// Utility functions for tuples
tuple	:: !a !b	-> (!a,!b)
tuple3	:: !a !b !c	-> (!a,!b,!c)

appFst	:: (.a -> .c) !(.a,.b) -> (.c,.b)
appSnd	:: (.b -> .c) !(.a,.b) -> (.a,.c)

appFst3 :: (.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appSnd3 :: (.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appThd3 :: (.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)

// Utility functions for generics
fromOBJECT	:: !(OBJECT x)	-> x
fromCONS	:: !(CONS x)	-> x
fromFIELD	:: !(FIELD x)	-> x
fromPAIRX	:: !(PAIR x y)	-> x
fromPAIRY	:: !(PAIR x y)	-> y

isRecordType	:: !GenericTypeDefDescriptor	-> Bool
isRecordCons	:: !GenericConsDescriptor		-> Bool

//List utility functions
replaceInList	:: !(a a -> Bool) !a ![a]	-> [a]
splitWith		:: !(a -> Bool) ![a]		-> (![a],![a])
sortByIndex		:: ![(!Int,!a)]				-> [a]
intersperse		:: !a ![a]					-> [a]
getItems		:: ![a] ![Int]				-> [a]