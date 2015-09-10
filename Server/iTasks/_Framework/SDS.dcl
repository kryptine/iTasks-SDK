definition module iTasks._Framework.SDS

import GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.JSON
from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.Task import :: TaskException
from iTasks._Framework.Generic import class iTask, generic gEditor, generic gEditMeta, generic gVerify, generic gEq, generic gDefault, generic gText
from iTasks._Framework.Generic import :: VSt, :: VisualizationResult, :: EditMeta, :: VerifiedValue, :: DataPath, :: Verification, :: InteractionMask
from iTasks._Framework.Generic import :: USt, :: MaskedValue, :: VerifyOptions, :: TextFormat
from iTasks.API.Core.Types import :: InstanceNo, :: TaskId
from iTasks.UI.Editor import :: Editor

:: RWShared p r w
	= 			            SDSSource		!(SDSSource p r w)
    | E.ps rs ws:           SDSLens         !(RWShared ps rs ws)                        (SDSLens p r w ps rs ws) & iTask ps
    | E.p1 p2:              SDSSelect       !(RWShared p1 r w)   !(RWShared p2 r w)     (SDSSelect p p1 p2 r w) & iTask p1 & iTask p2
    | E.p1 r1 w1 p2 r2 w2:  SDSParallel     !(RWShared p1 r1 w1) !(RWShared p2 r2 w2)   (SDSParallel p1 r1 w1 p2 r2 w2 p r w) & iTask p1 & iTask p2
    | E.r1 w1 p2 r2 w2:     SDSSequence     !(RWShared p  r1 w1) !(RWShared p2 r2 w2)   (SDSSequence p r1 w1 p2 r2 w2 r w) & iTask p2
							// USE IT CAREFULLY, IT CAN BREAK NOTIFICATION!
    |						SDSDynamic		!(p *IWorld -> *(MaybeError TaskException (RWShared p r w), *IWorld))

//A notification is function predictate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== p -> Bool

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { reqTaskId 	:: TaskId		//Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
    , reqSDSId      :: SDSIdentity  //Id of the actual SDS used to create this request (may be a derived one)

    , cmpSDSId      :: SDSIdentity  //Id of the SDS we are saving for comparison
    , cmpParam      :: Dynamic      //Parameter we are saving for comparison
    , cmpParamText  :: String       //String version of comparison parameter for tracing
    }

:: SDSIdentity  :== String

//Sources provide direct access to a data source
:: SDSSource p r w =
	{ name          :: String
    , read			:: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

:: SDSLens p r w ps rs ws =
    { name         :: String
    , param        :: p -> ps
    , read         :: SDSLensRead p r rs
    , write        :: SDSLensWrite p w rs ws
    , notify       :: SDSLensNotify p w rs
    }

:: SDSLensRead p r rs
    = SDSRead       (p rs -> MaybeError TaskException r)  //Read original source and transform
    | SDSReadConst  (p -> r)                              //No need to read the original source

:: SDSLensWrite p w rs ws
    = SDSWrite      (p rs w  -> MaybeError TaskException (Maybe ws)) //Read original source, and write updated version
    | SDSWriteConst (p w     -> MaybeError TaskException (Maybe ws)) //No need to read the original source

:: SDSLensNotify p w rs
    = SDSNotify         (p rs w -> SDSNotifyPred p)
    | SDSNotifyConst    (p w    -> SDSNotifyPred p)

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
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p r1 w1 p2 r2 w2 r w =
    { name         :: String
    , param         :: p r1 -> p2
    , read          :: (r1,r2) -> r
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

:: BasicShareId :== String	
:: WriteShare p = E.r w: Write !w !(RWShared p r w)
	
:: ROShared p a 	:== RWShared p a ()
:: WOShared p a 	:== RWShared p () a

:: ReadWriteShared r w  :== RWShared () r w
:: ReadOnlyShared a		:== ReadWriteShared a ()
:: WriteOnlyShared a	:== ReadWriteShared () a
:: Shared a				:== ReadWriteShared a a
	
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

//Normal access functions

//Just read an SDS
read			::						    !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
//Read an SDS and register a taskId to be notified when it is written
readRegister	:: !TaskId                  !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
//Write an SDS (and queue evaluation of those task instances which contained tasks that registered for notification)
write			:: !w					    !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)	
//Read followed by write. The 'a' typed value is a result that is returned
modify          :: !(r -> (!a,!w))          !(RWShared () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld)

//Force notify (queue evaluation of task instances that registered for notification)
notify          ::                          !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)

//Clear all registrations for a given task instance.
//This is normally called by the queueRefresh functions, because once an instance is queued
//for evaluation anyway, it no longer make sense to notify it again.
clearInstanceSDSRegistrations :: ![InstanceNo] !*IWorld -> *IWorld

//List all current registrations (for debugging purposes)
listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String

:: JSONShared :== RWShared JSONNode JSONNode JSONNode

//Exposing shares for external nodes
toJSONShared    :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & iTask p
fromJSONShared  :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
newURL 		    :: !*IWorld -> (!String, !*IWorld)
getURLbyId 	    :: !String !*IWorld -> (!String, !*IWorld)

