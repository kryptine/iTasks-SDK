definition module iTasks.Framework.SDS

import GenEq
import System.FilePath, Data.Void, Data.Maybe, Data.Either, Data.Error, System.Time, Text.JSON
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Task import :: TaskException
from iTasks.API.Core.Types import :: InstanceNo, :: TaskId

:: RWShared p r w
	= 			            SDSSource		!(SDSSource p r w)
    | E.rs ws:              SDSProjection   !(RWShared p rs ws)                         (SDSProjection rs ws r w)
	| E.ps:		            SDSTranslation  !(RWShared ps r w)                          (SDSTranslation p ps) & TC ps
    | E.ps pn rs ws:        SDSSplit        !(RWShared ps rs ws)                        (SDSSplit p ps pn rs ws r w) & TC ps & TC pn & gEq{|*|} ps
    | E.p1 p2:              SDSSelect       !(RWShared p1 r w)   !(RWShared p2 r w)     (SDSSelect p p1 p2 r w) & TC p1 & TC p2
    | E.p1 r1 w1 p2 r2 w2:  SDSParallel     !(RWShared p1 r1 w1) !(RWShared p2 r2 w2)   (SDSParallel p1 r1 w1 p2 r2 w2 p r w) & TC p1 & TC p2
    | E.r1 w1 p2 r2 w2:     SDSSequence     !(RWShared p  r1 w1) !(RWShared p2 r2 w2)   (SDSSequence p r1 w1 p2 r2 w2 r w) & TC p2
							// USE IT CAREFULLY, IT CAN BREAK NOTIFICATION!
    |						SDSDynamic		!(p *IWorld -> *(MaybeError TaskException (RWShared p r w), *IWorld))

//A notification is function predictate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== p -> Bool
:: SDSNotifyPredIWorld p    :== p *IWorld -> *(!Bool,!*IWorld)

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { taskInstance  :: InstanceNo
    , reqNo         :: Int
    , sdsId         :: SDSIdentity
    , param         :: Dynamic
    }

:: SDSIdentity  :== String

//Notification requests are identified by this identity
sdsIdentity :: !(RWShared p r w) -> SDSIdentity

//Sources provide direct access to a data source
:: SDSSource p r w =
	{ name          :: String
    , read			:: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

//Project maps values from a source (s) domain to a new target (t) domain
:: SDSProjection rs ws rt wt =
    { read         :: SDSReadProjection rs rt
    , write        :: SDSWriteProjection rs ws wt
    }

:: SDSReadProjection rs rt
    = SDSLensRead      (rs -> MaybeError TaskException rt)      //Read lens-like
    | SDSConstRead     rt                               //No need to read the original source

:: SDSWriteProjection rs ws wt
    = SDSLensWrite     (rs wt   -> MaybeError TaskException (Maybe ws)) //Write lens-like
    | SDSBlindWrite    (wt      -> MaybeError TaskException (Maybe ws)) //No-need to read the original source
    | SDSNoWrite

:: SDSTranslation p ps =
    { name         :: String
    , param        :: p -> ps
    }

//Split divides a domain into two subdomains by introducing a new parameter
:: SDSSplit p ps pn rs ws r w =
    { name         :: String
    , param        :: p -> (ps,pn)
    , read         :: pn rs -> r
    , write        :: pn rs w -> (ws, SDSNotifyPred pn)
    }

//Merge two sources by selecting one based on the parameter
:: SDSSelect p p1 p2 r w =
    { name         :: String
    , select        :: p -> Either p1 p2
    , notifyl       :: p1 r w -> SDSNotifyPred p2
    , notifyr       :: p2 r w -> SDSNotifyPred p1
    }

//Read from and write to two independent SDS's
:: SDSParallel p1 r1 w1 p2 r2 w2 p r w =
    { name         :: String
    , param         :: p -> (p1,p2)
    , read          :: (r1,r2) -> r
    , writel        :: SDSWriteProjection r1 w1 w
    , writer        :: SDSWriteProjection r2 w2 w
    }

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p r1 w1 p2 r2 w2 r w =
    { name         :: String
    , param         :: p r1 -> p2
    , read          :: (r1,r2) -> r
    , writel        :: SDSWriteProjection r1 w1 w
    , writer        :: SDSWriteProjection r2 w2 w
    }

:: BasicShareId :== String	
:: WriteShare p = E.r w: Write !w !(RWShared p r w)
	
:: ROShared p a 	:== RWShared p a Void
:: WOShared p a 	:== RWShared p Void a

:: ReadWriteShared r w  :== RWShared Void r w
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a
:: Shared a				:== ReadWriteShared a a

reportSDSChange         :: !String !*IWorld -> *IWorld
	
createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	ROShared p r

createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	ROShared p r

read			::						    !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
readRegister	:: !TaskId                  !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
write			:: !w					    !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException Void, !*IWorld)	

//Dependency administration
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld

:: JSONShared :== RWShared JSONNode JSONNode JSONNode

//Exposing shares for external nodes
toJSONShared    :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & TC p
fromJSONShared  :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
newURL 		    :: !*IWorld -> (!String, !*IWorld)
getURLbyId 	    :: !String !*IWorld -> (!String, !*IWorld)

