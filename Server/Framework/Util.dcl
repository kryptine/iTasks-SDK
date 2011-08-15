definition module Util

import StdGeneric, SystemTypes, Error, GenEq

mb2list				:: !(Maybe [a]) -> [a]
list2mb				:: ![a] -> (Maybe [a])

voidNothing 		:: Maybe Void

camelCaseToWords 	:: !String -> String

instance toString (Maybe a) | toString a

mb2error			:: !e !(Maybe a) -> MaybeError e a

pad					:: !Int !Int -> String
decFormat			:: !Int -> String

// Functions for accessing dates and times
currentTime 			:: !*IWorld -> (!Time,!*IWorld)
currentDate 			:: !*IWorld -> (!Date,!*IWorld)
currentDateTime 		:: !*IWorld -> (!DateTime,!*IWorld)
currentTimestamp		:: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestampError	:: !*IWorld -> (!MaybeErrorString Timestamp,!*IWorld)
currentDateTimeWorld	:: !*World	-> (!DateTime,!*World)
timestampToGmDateTime	:: !Timestamp -> DateTime




