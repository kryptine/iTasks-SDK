definition module GenUpdate

import StdGeneric, Maybe, Void, Either, Store, SystemTypes
from Map	import :: Map

:: *USt =
	{ searchPath		:: !DataPath
	, currentPath		:: !DataPath
	, update			:: !JSONNode
	, consPath			:: ![ConsPos]
	, oldMask			:: ![InteractionMask]
	, newMask			:: ![InteractionMask]
	, iworld			:: !*Maybe IWorld
	}

:: UpdateMode a = UDSearch !a | UDCreate

generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

derive gUpdate UNIT, PAIR, EITHER, CONS of {gcd_arity}, OBJECT of {gtd_num_conses,gtd_conses}, RECORD of {grd_arity}, FIELD
derive gUpdate Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gUpdate Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Wrapper functions for updating
defaultValue			:: a																		| gUpdate{|*|} a
updateValueAndMask  	:: !DataPath !JSONNode !a !InteractionMask !*IWorld -> (!a,!InteractionMask,!*IWorld)	| gUpdate{|*|} a

//Utility functions for working accessing the iWorld in a USt
appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
accIWorldUSt :: !.(*IWorld -> *(!.a,!*IWorld))!*USt -> (!.a,!*USt)



class GenMask m
where
	popMask 			:: ![m] -> (!m, ![m])
	appendToMask 		:: ![m] !m -> [m]
	childMasks			:: !m -> [m]
	childMasksN			:: !m !Int -> [m]
	isTouched			:: !m -> Bool

instance GenMask InteractionMask

// utility functions for custom gUpdate definitions

/**
* Makes sure the value is not updated.
*
* @param The current update mode
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
noUpdate :: !(UpdateMode a) a !*USt -> *(!a,!*USt)

/**
* Updates a value.
*
* @param The current update mode
* @param A function defining how to update the value given an update
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The current update mode
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
/**
* Creates a default value.
* (Same as basicUpdate with UDCreate as first parameter.)
*
* @param The created default value
* @param USt
*
* @return modified USt
*/
basicCreate :: !a !*USt -> *(!a,!*USt)
/**
* Search in a value and possibly apply update.
* (Same as basicUpdate with (UDSearch a) as first parameter.)
*
* @param The value to search in
* @param A function defining how calculate a new value from an update
* @param USt
*
* @return modified USt
*/
basicSearch :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
