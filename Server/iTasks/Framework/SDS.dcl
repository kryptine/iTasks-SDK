definition module iTasks.Framework.SDS

import System.FilePath, Data.Void, Data.Maybe, Data.Error, System.Time, Text.JSON
from iTasks.Framework.IWorld import :: IWorld

:: RWShared r w
	= E.b:			BasicSource		!(BasicSource b r w)
	| E.rx wy:		ComposedRead	!(RWShared rx w) !(rx -> MaybeErrorString (RWShared r wy))
	| E.r` w` w``:	ComposedWrite	!(RWShared r w`) !(w -> MaybeErrorString (RWShared r` w``)) !(w r` -> MaybeErrorString [WriteShare])

:: BasicSource b r w =
	{ read			:: !*IWorld -> *(!MaybeErrorString (!r,!ChangeNotification), !*IWorld)
	, write			:: !w *IWorld -> *(!MaybeErrorString Void, !*IWorld)
	, mbId			:: !Maybe BasicShareId
	}
	
:: ChangeNotification = OnWrite
						| Predictable	!Timestamp
						| Polling		!Timestamp !(*IWorld -> *(!CheckRes,!*IWorld))
							
:: CheckRes = Changed | CheckAgain Timestamp
						
:: BasicShareId :== String	
:: WriteShare = E.r w: Write !w !(RWShared r w)
	
:: ROShared a 	:== RWShared a Void
:: WOShared a 	:== RWShared Void a
:: Hash				:== String

:: ReadWriteShared r w  :== RWShared r w
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a
:: Shared a				:== ReadWriteShared a a

class registerSDSDependency msg :: !BasicShareId msg !*IWorld -> *IWorld
class reportSDSChange msg :: !BasicShareId !(msg -> Bool) !*IWorld -> *IWorld

registerSDSPredictableChange	:: !Timestamp 										    !BasicShareId !*IWorld -> *IWorld
registerSDSCheckForChange		:: !Timestamp !Hash !(*IWorld -> (!CheckRes,!*IWorld))	!BasicShareId !*IWorld -> *IWorld
	
createChangeOnWriteSDS ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w
	
createPollingSDS ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString (!r, !Timestamp, !(*IWorld -> *(!CheckRes,!*IWorld))), !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w

createReadOnlySDS ::
	!(*IWorld -> *(!r, !*IWorld))
	->
	ROShared r
	
createReadOnlySDSError ::
	!(*IWorld -> *(!MaybeErrorString r, !*IWorld))
	->
	ROShared r
	
createReadOnlySDSPredictable ::
	!String
	!String
	!(*IWorld -> *(!(!r, !Timestamp), !*IWorld))
	->
	ROShared r
	
createReadOnlySDSErrorPredictable ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString (!r, !Timestamp), !*IWorld))
	->
	ROShared r

read			::						!(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
readRegister	:: !msg					!(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)		| registerSDSDependency msg
write			:: !w					!(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)	
writeFilterMsg	:: !w !(msg -> Bool)	!(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)	| reportSDSChange msg

/** core combinators **/
// read combinator
(>?>) infixl 6 :: !(RWShared rx wx) !(rx -> MaybeErrorString (RWShared ry wy)) -> RWShared ry wx
// write combinator
(>!>) infixl 6 :: !(RWShared r w`) !(!w -> MaybeErrorString (RWShared r` w``), !w r` -> MaybeErrorString [WriteShare]) -> RWShared r w

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared r w) -> RWShared r` w
mapWrite		:: !(w` r -> Maybe w)			!(RWShared r w) -> RWShared r w`
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared r w) -> RWShared r` w`

mapReadError		:: !(r -> MaybeErrorString r`)										!(RWShared r w) -> RWShared r` w
mapWriteError		:: !(w` r -> MaybeErrorString (Maybe w))							!(RWShared r w) -> RWShared r w`
mapReadWriteError	:: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w))	!(RWShared r w) -> RWShared r` w`

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) (wx,wy)
(>+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wx
(|+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wy
(|+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) Void

toReadOnly :: !(RWShared r w) -> ROShared r

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared a a) !(RWShared b b) -> (!RWShared a a, !RWShared b b)

// null share
null		:: WOShared a
// constant share, value does never change
constShare	:: !a -> ROShared a

// Use the value of one share as parameter for another
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1

toJSONShared	:: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
fromJSONShared	:: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w

newURL 		:: !*IWorld -> (!String, !*IWorld)
getURLbyId 	:: !String !*IWorld -> (!String, !*IWorld)

