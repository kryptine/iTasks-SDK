definition module iTasks.Framework.GenUpdate

import StdGeneric
from Data.Map	import :: Map
from Data.Maybe import :: Maybe
from Data.Void import :: Void
from Data.Either import :: Either

import iTasks.Framework.Store
import iTasks.API.Core.SystemTypes

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

//Update an existing value and its interaction mask
generic gUpdate a | gDefault a, JSONDecode a :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask])

derive gUpdate UNIT, PAIR, EITHER, OBJECT of {gtd_num_conses,gtd_conses}, CONS of {gcd_arity,gcd_index}, RECORD of {grd_arity}, FIELD
derive gUpdate Int, Real, Char, Bool, String, [], (,), (,,), (,,,), (->), Dynamic
derive gUpdate Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

// Wrapper functions for updating
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

// Utility functions for custom gUpdate definitions

/**
* Convenient wrapper which automatically updates the interaction mask.
*
* @param A function defining how to update the value given an update
*
* @return The modified value
*/
basicUpdate :: !(upd a -> a) ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask]) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The value to update
*
* @return The modified value
*/
basicUpdateSimple :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask]) | JSONDecode{|*|} a
