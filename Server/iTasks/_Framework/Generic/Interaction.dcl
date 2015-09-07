definition module iTasks._Framework.Generic.Interaction

from StdGeneric import :: UNIT,::EITHER,::PAIR,::OBJECT,::CONS,::RECORD,::FIELD,::ConsPos, generic bimap, :: Bimap
from iTasks._Framework.IWorld import :: IWorld
from iTasks.UI.Diff import :: UIControl, :: UIAttributes
from iTasks.API.Core.Types import :: TaskId, :: DataPath, :: InteractionMask, :: MaskedValue, :: Verification, :: VerifiedValue, :: EditableList
from iTasks.UI.Layout import :: LayoutRules

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

from iTasks.UI.Editor import :: VSt(..), :: VisualizationResult, :: EditMeta, :: Editor
/**
* Main eneric editor function
*/

/*
generic gEditor a | gText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a 
				  :: !DataPath !(VerifiedValue a) ![EditMeta] !*VSt -> (!VisualizationResult,!*VSt)
*/

generic gEditor a | gText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a :: Editor a
/*
derive gEditor
	UNIT,
	EITHER with ve1 _ _ em1 _ _ ve2 _ _ em2 _ _,
	PAIR with ve1 _ _ em1 _ _ ve2 _ _ em2 _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ve1 _ _ em1 _ _,
	CONS of {gcd_index,gcd_arity} with ve1 _ _ em1 _ _,
	RECORD of {grd_arity} with ve1 _ _ em1 _ _,
	FIELD of {gfd_name} with ve1 _ _ em1 _ _
	
derive gEditor Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (->), Dynamic
derive gEditor Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp
derive gEditor EditableList
derive gEditor RWShared
*/
derive gEditor
	UNIT,
	EITHER with ve1 _ _ em1 _ _ ve2 _ _ em2 _ _,
	PAIR with ve1 _ _ em1 _ _ ve2 _ _ em2 _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ve1 _ _ em1 _ _,
	CONS of {gcd_index,gcd_arity} with ve1 _ _ em1 _ _,
	RECORD of {grd_arity} with ve1 _ _ em1 _ _,
	FIELD of {gfd_name} with ve1 _ _ em1 _ _

derive gEditor Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (->), Dynamic
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
	
derive gEditMeta Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (->), Dynamic
derive gEditMeta Maybe, Either, MaybeError,  Map, JSONNode, HtmlTag, Timestamp
derive gEditMeta EditableList
derive gEditMeta RWShared

//Check a value to see if it is ok
generic gVerify a :: !VerifyOptions (MaskedValue a) -> Verification

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS of {gcd_arity}, RECORD of {grd_arity}, FIELD
derive gVerify Int, Real, Char, Bool, String, [], (), (,), (,,),(,,,), (,,,,),(->), Dynamic
derive gVerify Maybe, Either, MaybeError,  Map, JSONNode, HtmlTag, Timestamp
derive gVerify EditableList
derive gVerify RWShared

//Update an existing value and its interaction mask
generic gUpdate a | gDefault a, JSONEncode a, JSONDecode a :: !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt)

derive gUpdate UNIT, PAIR, EITHER, OBJECT of {gtd_num_conses,gtd_conses}, CONS of {gcd_arity,gcd_index}, RECORD of {grd_arity}, FIELD
derive gUpdate Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (->), Dynamic
derive gUpdate Maybe, Either, MaybeError,  Map, JSONNode, HtmlTag, Timestamp
derive gUpdate EditableList
derive gUpdate RWShared

//Wrapper functions for generating editors
visualizeAsEditor   :: !(VerifiedValue a) !TaskId !LayoutRules !*IWorld	-> (![(!UIControl,!UIAttributes)],!*IWorld)	| gEditor{|*|} a & gEditMeta{|*|} a
updateValueAndMask  :: !TaskId !DataPath !JSONNode !(MaskedValue a) !*IWorld -> (!MaskedValue a,!*IWorld) | gUpdate{|*|} a

//Support types for generating editors
:: *USt =
    { taskId            :: !String
    , editorId          :: !String
    , iworld            :: !*IWorld
    }

:: VerifyOptions =
	{ optional		:: !Bool
	, disabled		:: !Bool
	}

//Utility functions making specializations of gEditor
checkMask			:: !InteractionMask a -> Maybe a
checkMaskValue      :: !InteractionMask a -> Maybe JSONNode | JSONEncode{|*|} a

editorAttributes	:: !(VerifiedValue a) [EditMeta] -> UIAttributes


/**
* Verify a value.
*/
verifyValue :: !a -> Verification | gVerify{|*|} a
/**
* Verify a form based on the value and its update mask.
*/
verifyMaskedValue :: !(MaskedValue a) -> Verification | gVerify{|*|} a
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
alwaysValid :: !(MaskedValue a) -> Verification
/**
* Verifies a value which is always valid if filled in (e.g. a basic value).
*
*/
simpleVerify :: !VerifyOptions !(MaskedValue a) -> Verification
/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* 
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value
*/
customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (MaskedValue a) -> Verification

/**
* Convenient wrapper which automatically updates the interaction mask.
*
* @param A function defining how to update the value given an update
*
* @return The modified value
*/
basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The value to update
*
* @return The modified value
*/
basicUpdateSimple :: !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt) | JSONDecode{|*|} a
