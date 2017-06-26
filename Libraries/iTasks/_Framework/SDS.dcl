definition module iTasks._Framework.SDS

import GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.JSON
from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.Task import :: TaskException
from iTasks._Framework.Generic import class iTask, generic gEditor, generic gEq, generic gDefault, generic gText
from iTasks._Framework.Generic.Visualization import :: TextFormat
from iTasks.API.Core.Types import :: InstanceNo, :: TaskId
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
import iTasks.SDS.Definition

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { reqTaskId 	:: TaskId		//Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
    , reqSDSId      :: SDSIdentity  //Id of the actual SDS used to create this request (may be a derived one)

    , cmpSDSId      :: SDSIdentity  //Id of the SDS we are saving for comparison
    , cmpParam      :: Dynamic      //Parameter we are saving for comparison
    , cmpParamText  :: String       //String version of comparison parameter for tracing
    }
:: SDSIdentity  :== String

//:: WriteShare p = E.r w: Write !w !(RWShared p r w)

//Internal creation functions:

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

//Internal access functions

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

