definition module iTasks.Internal.SDS

import Data.GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.GenJSON
from Data.Set import :: Set
from iTasks.Internal.IWorld import :: IWorld, :: ConnectionId
from iTasks.Internal.Generic.Visualization import :: TextFormat

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: TaskException, :: TaskId, :: InstanceNo
import iTasks.SDS.Definition

instance Identifiable (SDSSource p r w)
instance Readable (SDSSource p r w) p r
instance Writeable (SDSSource p r w) p w
instance Modifiable (SDSSource p r w) p r w
instance Registrable (SDSSource p r w) p r

instance Identifiable (SDSLens p r w)
instance Readable (SDSLens p r w) p r
instance Writeable (SDSLens p r w) p w
instance Modifiable (SDSLens p r w) p r w
instance Registrable (SDSLens p r w) p r

instance Identifiable (SDSCache p r w)
instance Readable (SDSCache p r w) p r
instance Writeable (SDSCache p r w) p w
instance Modifiable (SDSCache p r w) p r w
instance Registrable (SDSCache p r w) p r

instance Identifiable (SDSSequence p r w)
instance Readable (SDSSequence p r w) p r
instance Writeable (SDSSequence p r w) p w
instance Modifiable (SDSSequence p r w) p r w
instance Registrable (SDSSequence p r w) p r

instance Identifiable (SDSSelect p r w)
instance Readable (SDSSelect p r w) p r
instance Writeable (SDSSelect p r w) p w
instance Modifiable (SDSSelect p r w) p r w
instance Registrable (SDSSelect p r w) p r

instance Identifiable (SDSParallel p r w)
instance Readable (SDSParallel p r w) p r
instance Writeable (SDSParallel p r w) p w
instance Modifiable (SDSParallel p r w) p r w
instance Registrable (SDSParallel p r w) p r

instance Identifiable (SDSRemoteService p r w)
instance Readable (SDSRemoteService p r w) p r
instance Writeable (SDSRemoteService p r w) p w
instance Modifiable (SDSRemoteService p r w) p r w
instance Registrable (SDSRemoteService p r w) p r

instance Identifiable (SDSRemoteSource p r w)
instance Readable (SDSRemoteSource p r w) p r
instance Writeable (SDSRemoteSource p r w) p w
instance Modifiable (SDSRemoteSource p r w) p r w
instance Registrable (SDSRemoteSource p r w) p r

instance Identifiable (SDSDebug p r w)
instance Readable (SDSDebug p r w) p r
instance Writeable (SDSDebug p r w) p w
instance Modifiable (SDSDebug p r w) p r w
instance Registrable (SDSDebug p r w) p r

:: DeferredWrite = E. p r w sds: DeferredWrite !p !w !(sds p r w) & iTask p & TC r & TC w & RWShared sds p r w

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

// Helper to immediately get the read value from a share. Use only when reading in an EmptyContext.
directResult :: (AsyncRead r w) -> r

//Internal access functions
sdsIdentity :: !sds -> SDSIdentity | Identifiable sds

:: AsyncRead r w = ReadingDone r
	| E. sds: Reading !sds & TC r & TC w & Registrable sds () r

/*
 * Read the SDS. TaskContext is used to determine whether a read is done in the
 * context of a task. The read is performed asynchronously when there is a task
 * context and the share is a asynchronous share.
 *
 * @return A ReadResult, either Queued (a asynchronous read is queued and the
 *	task will be notified when it is ready), or a direct result in the case of
 *	a blocking read.
 */
read 			:: !sds 			!TaskContext !*IWorld -> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable sds () r

//Read an SDS and register a taskId to be notified when it is written
readRegister	:: !TaskId                  !sds !*IWorld -> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Registrable sds () r

:: AsyncWrite r w = WritingDone
	| E. sds: Writing !sds & Writeable sds () w & TC r & TC w

//Write an SDS (and queue evaluation of those task instances which contained tasks that registered for notification)
write			:: !w					    !sds !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncWrite r w), !*IWorld)	| TC r & TC w & Writeable sds () w


:: AsyncModify r w = ModifyingDone w & TC w
	| E. sds: Modifying !sds (r -> w) & Modifiable sds () r w & TC r & TC w

//Read followed by write. The 'a' typed value is a result that is returned
modify :: !(r -> w)          !sds !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncModify r w), !*IWorld) | TC r & TC w & Modifiable sds () r w

//Clear all registrations for the given tasks.
//This is normally called by the queueRefresh functions, because once a task is queued
//for evaluation anyway, it no longer make sense to notify it again.
clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld

queueNotifyEvents :: !String !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !*IWorld -> *IWorld

//List all current registrations (for debugging purposes)
listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)

formatSDSRegistrationsList :: [SDSNotifyRequest] -> String

//Flush all deffered/cached writes of
flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)

// Used to turn any read/write/modified operation (with all arguments except the environment
// curried in) into one which returns a dynamic. Use to store sdsEvalStates in the environment.
dynamicResult :: (*IWorld -> (MaybeError TaskException a, !*IWorld)) !*IWorld -> (MaybeError TaskException Dynamic, !*IWorld) | TC a
