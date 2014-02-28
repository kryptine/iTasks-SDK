definition module iTasks.Framework.SDS

import System.FilePath, Data.Void, Data.Maybe, Data.Either, Data.Error, System.Time, Text.JSON
from iTasks.Framework.IWorld import :: IWorld
from iTasks.API.Core.Types import :: InstanceNo

:: RWShared p r w
	= 			            SDSSource		!(SDSSource p r w)
    //'NEW' COMPOSITIONS
    | E.rs ws:              SDSProjection   !(RWShared p rs ws) !(SDSLens rs ws r w)
	| E.ps:		            SDSTranslation  !(RWShared ps r w)  !(p -> ps) & TC ps
    | E.ps pn:              SDSSplit        !(RWShared ps r w)                          (SDSSplit p ps pn r w) & TC ps & TC pn
    | E.p1 p2:              SDSMerge        !(RWShared p1 r w)   !(RWShared p2 r w)     (SDSMerge p p1 p2 r w) & TC p1 & TC p2
    | E.p1 r1 w1 p2 r2 w2:  SDSParallel     !(RWShared p1 r1 w1) !(RWShared p2 r2 w2)   (SDSParallel p1 r1 w1 p2 r2 w2 p r w) & TC p1 & TC p2
    | E.rw1 p2 r2 w2:       SDSSequence     !(RWShared p rw1 rw1) !(RWShared p2 r2 w2)  (SDSSequence rw1 p2 r2 w2 r w) & TC p2
    //'OLD' COMPOSITIONS
	| E.rx wy:		ComposedRead	!(RWShared p rx w) !(rx -> MaybeErrorString (RWShared p r wy))
	| E.r` w` w``:	ComposedWrite	!(RWShared p r w`) !(w -> MaybeErrorString (RWShared p r` w``)) !(w r` -> MaybeErrorString [WriteShare p])

:: SDSSource p r w =
	{ mbId          :: !Maybe BasicShareId
    , read			:: p *IWorld -> *(!MaybeErrorString (!r,!ChangeNotification), !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld)
	}

//A notification is function predictate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== p -> Bool
:: SDSNotifyPredIWorld p    :== p *IWorld -> *(!Bool,!*IWorld)

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { reqid         :: Int
    , sdsid         :: SDSIdentity
    , param         :: Dynamic
    , taskInstance  :: InstanceNo
    }
:: SDSIdentity  :== String

//Lens maps values from a source (s) domain to a new target (t) domain
:: SDSLens rs ws rt wt =
    { read         :: rs -> rt
    , write        :: rs wt -> ws
    }

//Split divides a domain into two subdomains by introducing a new parameter
:: SDSSplit p ps pn r w =
    { param        :: p -> (ps,pn)
    , read         :: pn r -> r
    , write        :: pn r w -> (w, SDSNotifyPred pn)
    }

//Merge two sources by selecting one based on the parameter
:: SDSMerge p p1 p2 r w =
    { select        :: p -> Either p1 p2
    , notifyl       :: p1 r w -> SDSNotifyPred p2
    , notifyr       :: p2 r w -> SDSNotifyPred p1
    }

//Read from and write to two independent SDS's
:: SDSParallel p1 r1 w1 p2 r2 w2 p r w =
    { param         :: p -> (p1,p2)
    , read          :: (r1,r2) -> r
    , write         :: w -> (w1,w2)
    }

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence rw1 p2 r2 w2 r w =
    { param         :: rw1 -> p2
    , read          :: (rw1,r2) -> r
    , write         :: w -> (rw1,w2)
    }

:: ChangeNotification = OnWrite
						| Predictable	!Timestamp
						| Polling		!Timestamp !(*IWorld -> *(!CheckRes,!*IWorld))
							
:: CheckRes = Changed | CheckAgain Timestamp
						
:: BasicShareId :== String	
:: WriteShare p = E.r w: Write !w !(RWShared p r w)
	
:: ROShared p a 	:== RWShared p a Void
:: WOShared p a 	:== RWShared p Void a
:: Hash			    :== String

:: ReadWriteShared r w  :== RWShared Void r w
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a
:: Shared a				:== ReadWriteShared a a

registerSDSDependency   :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
reportSDSChange         :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld

registerSDSPredictableChange	:: !Timestamp 										    !BasicShareId !*IWorld -> *IWorld
registerSDSCheckForChange		:: !Timestamp !Hash !(*IWorld -> (!CheckRes,!*IWorld))	!BasicShareId !*IWorld -> *IWorld
	
createChangeOnWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w

createPollingSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString (!r, !Timestamp, !(*IWorld -> *(!CheckRes,!*IWorld))), !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	ROShared p r
	
createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	->
	ROShared p r
	
createReadOnlySDSPredictable ::
	!String
	!String
	!(p *IWorld -> *(!(!r, !Timestamp), !*IWorld))
	->
	ROShared p r
	
createReadOnlySDSErrorPredictable ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString (!r, !Timestamp), !*IWorld))
	->
	ROShared p r

createSDS ::
	!(Maybe BasicShareId)
	!(p *IWorld -> *(!MaybeErrorString (!r, !ChangeNotification), !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w

read			::						    !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
readRegister	:: !InstanceNo              !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
write			:: !w					    !(RWShared Void r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)	
writeFilterMsg	:: !w !(InstanceNo -> Bool)	!(RWShared Void r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)

//Dependency administration
addShareRegistration		:: !BasicShareId !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations		:: !InstanceNo !*IWorld -> *IWorld
addOutdatedOnShareChange	:: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedInstances		:: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld

//Exposing shares for external nodes
toJSONShared	:: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
fromJSONShared	:: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w
newURL 		    :: !*IWorld -> (!String, !*IWorld)
getURLbyId 	    :: !String !*IWorld -> (!String, !*IWorld)

