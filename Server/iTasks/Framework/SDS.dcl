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

createSDS ::
	!(Maybe BasicShareId)
	!(*IWorld -> *(!MaybeErrorString (!r, !ChangeNotification), !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w

read			::						!(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
readRegister	:: !msg					!(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)		| registerSDSDependency msg
write			:: !w					!(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)	
writeFilterMsg	:: !w !(msg -> Bool)	!(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)	| reportSDSChange msg

toJSONShared	:: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
fromJSONShared	:: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w

newURL 		:: !*IWorld -> (!String, !*IWorld)
getURLbyId 	:: !String !*IWorld -> (!String, !*IWorld)

