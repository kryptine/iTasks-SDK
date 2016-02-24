definition module iTasks._Framework.Generic.Interaction

from StdGeneric import :: UNIT,::EITHER,::PAIR,::OBJECT,::CONS,::RECORD,::FIELD,::ConsPos, generic bimap, :: Bimap
from iTasks._Framework.IWorld import :: IWorld
from iTasks.UI.Definition import :: UIAttributes, :: UILocalChange
from iTasks.API.Core.Types import :: TaskId, :: DataPath, :: Verification, :: VerifiedValue, :: EditableList

from iTasks._Framework.Generic.Visualization import generic gText, :: TextFormat
from iTasks._Framework.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Map import :: Map
from System.Time import :: Timestamp
from iTasks._Framework.SDS import :: RWShared

from iTasks.UI.Editor import :: Editor, :: VSt(..), :: EditMask, :: Masked, :: EditMeta, :: USt
/**
* Main eneric editor function
*/
generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a

derive gEditor
	UNIT,
	EITHER with ex _ dx _ _ ey _ dy _ _,
	PAIR with ex _ _ _ _ ey _ _ _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ex _ _ _ _,
	CONS of {gcd_index,gcd_arity} with ex _ _ _ _,
	RECORD of {grd_arity} with ex _ _ _ _,
	FIELD of {gfd_name} with ex _ _ _ _

derive gEditor Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (,,,,,), (->), Dynamic
derive gEditor Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp
derive gEditor EditableList
derive gEditor RWShared

derive bimap Editor

/**
* Type-dependent meta data useful for generating editors
*/
generic gEditMeta a :: a -> [EditMeta]

derive gEditMeta
	UNIT,
	EITHER with fx fy,
	PAIR with fx fy,
	OBJECT with fx,
	CONS with fx,
	RECORD with fx,
	FIELD of {gfd_name} with fx
	
derive gEditMeta Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (,,,,,), (->), Dynamic
derive gEditMeta Maybe, Either, MaybeError,  Map, JSONNode, HtmlTag, Timestamp
derive gEditMeta EditableList
derive gEditMeta RWShared

//Check a value to see if it is ok
generic gVerify a :: !VerifyOptions (Masked a) -> Verification

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS of {gcd_arity}, RECORD of {grd_arity}, FIELD
derive gVerify Int, Real, Char, Bool, String, [], (), (,), (,,),(,,,), (,,,,), (,,,,,), (->), Dynamic
derive gVerify Maybe, Either, MaybeError,  Map, JSONNode, HtmlTag, Timestamp
derive gVerify EditableList
derive gVerify RWShared

//Support types for generating editors
:: VerifyOptions =
	{ optional		:: !Bool
	, disabled		:: !Bool
	}

//Utility functions making specializations of gEditor
checkMask			:: !EditMask a -> Maybe a
checkMaskValue      :: !EditMask a -> Maybe JSONNode | JSONEncode{|*|} a

stdAttributes 		:: String Bool EditMask -> UIAttributes
stdAttributeChanges :: String Bool EditMask EditMask -> [UILocalChange]

/**
* Verify a value.
*/
verifyValue :: !a -> Verification | gVerify{|*|} a
/**
* Verify a form based on the value and its update mask.
*/
verifyMaskedValue :: !(Masked a) -> Verification | gVerify{|*|} a
/**
* Based on the verification of a value, determine if it is valid.
* A compound value is valid if the verification contains no invalid parts.
*/
isValid :: !Verification -> Bool

/**
* Verifies a value which is always valid.
* No hint message is shown.
*
*/
alwaysValid :: !(Masked a) -> Verification
/**
* Verifies a value which is always valid if filled in (e.g. a basic value).
*
*/
simpleVerify :: !VerifyOptions !(Masked a) -> Verification
/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* 
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value
*/
customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (Masked a) -> Verification

/**
* Convenient wrapper which automatically updates the interaction mask.
*
* @param A function defining how to update the value given an update
*
* @return The modified value
*/
basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !a !EditMask !*USt -> *(!a, !EditMask, !*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The value to update
*
* @return The modified value
*/
basicUpdateSimple :: !DataPath !JSONNode !a !EditMask !*USt -> *(!a,!EditMask,!*USt) | JSONDecode{|*|} a
