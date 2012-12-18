definition module GenUpdate

import StdGeneric, Maybe, Void, Either, Store, SystemTypes
from Map	import :: Map

/**
* Creates default values
*
* @param Conspos path, this may be passed to create a specific constructor of an ADT.
*        If you simply want to create the first constructor you can pass an empty list.
*/
generic gDefault a :: [ConsPos] -> a

derive	gDefault UNIT, PAIR, EITHER, CONS, OBJECT, RECORD, FIELD
derive	gDefault Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive	gDefault Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

:: *USt =
	{ searchPath		:: !DataPath
	, currentPath		:: !DataPath
	, update			:: !JSONNode
	, consPath			:: ![ConsPos]
	, oldMask			:: ![InteractionMask]
	, newMask			:: ![InteractionMask]
	}

//Update an existing value and its interaction mask
generic gUpdate a | gDefault a :: !a !*USt -> (!a,!*USt)

derive gUpdate UNIT, PAIR, EITHER, CONS of {gcd_arity}, OBJECT of {gtd_num_conses,gtd_conses}, RECORD of {grd_arity}, FIELD
derive gUpdate Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gUpdate Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

//Wrapper functions for updating
defaultValue			:: a																	| gDefault{|*|} a
updateValueAndMask  	:: !DataPath !JSONNode !(!a,!InteractionMask) -> (!a,!InteractionMask)	| gUpdate{|*|} a

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
noUpdate :: !a !*USt -> *(!a,!*USt)
/**
* Updates a value.
*
* @param The value to update
* @param A function defining how to update the value given an update
* @param USt
*
* @return modified USt
*/
basicUpdate :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The value to update
* @param USt
*
* @return modified USt
*/
basicUpdateSimple :: !a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a

