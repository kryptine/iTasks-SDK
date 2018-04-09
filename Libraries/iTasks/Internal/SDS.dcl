definition module iTasks.Internal.SDS

import Data.GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.GenJSON
from Data.Set import :: Set
from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Visualization import :: TextFormat

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: TaskException, :: TaskId, :: InstanceNo
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

:: DeferredWrite = E. p r w sds: DeferredWrite !p !w !(sds p r w) & iTask p & TC r & TC w & RWShared sds

instance Identifiable SDSSource
instance Readable SDSSource
instance Writable SDSSource
instance Registrable SDSSource

instance Identifiable SDSLens
instance Readable SDSLens
instance Writable SDSLens
instance Registrable SDSLens

instance Identifiable SDSCache
instance Readable SDSCache
instance Writable SDSCache
instance Registrable SDSCache

instance Identifiable SDSSequence
instance Readable SDSSequence
instance Writable SDSSequence
instance Registrable SDSSequence

instance Identifiable SDSSelect
instance Readable SDSSelect
instance Writable SDSSelect
instance Registrable SDSSelect

instance Identifiable SDSParallel
instance Readable SDSParallel
instance Writable SDSParallel
instance Registrable SDSParallel

//Internal creation functions:

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	SDSSource p r ()

createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	SDSSource p r ()

//Internal access functions

//Just read an SDS
read			::						    !(sds () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r & Readable sds

//Read an SDS and register a taskId to be notified when it is written
readRegister	:: !TaskId                  !(sds () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r & Readable, Registrable sds

//Write an SDS (and queue evaluation of those task instances which contained tasks that registered for notification)
write			:: !w					    !(sds () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)	| TC r & TC w & Writable sds

//Read followed by write. The 'a' typed value is a result that is returned
modify          :: !(r -> (!a,!w))          !(sds () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld) | TC r & TC w & Readable sds & Writable sds

//Clear all registrations for the given tasks.
//This is normally called by the queueRefresh functions, because once a task is queued
//for evaluation anyway, it no longer make sense to notify it again.
clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld

//List all current registrations (for debugging purposes)
listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String

//Flush all deffered/cached writes of
flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)

