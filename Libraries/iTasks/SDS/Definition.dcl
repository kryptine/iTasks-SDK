definition module iTasks.SDS.Definition
/**
* This module provides the types that define a shared data source
*/
from iTasks.WF.Definition import :: TaskException, class iTask, :: TaskId
from iTasks.Internal.IWorld import :: IWorld, :: ConnectionId

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import Data.GenEq, Internet.HTTP

from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from Data.Set import :: Set

derive gText SDSNotifyRequest, RemoteNotifyOptions

:: SDSIdentity  :== String

:: TaskContext = EmptyContext // Used in the internals of the iTasks system
	| TaskContext TaskId // Used when a local task is reading from a share
	// Used when a remote task is reading from a share locally
	| RemoteTaskContext
		!TaskId  // The id of the original task reading the share
		!TaskId // The id of the current task handling the request
		!SDSIdentity // The id of the share on the remote server
		!String // The hostname of the original task reading the share
		!Int  // The port to which to send a refresh notification

:: ReadResult p r w = E. sds: ReadResult r (sds p r w) & RWShared sds & TC r & TC w
	| E. sds: AsyncRead (sds p r w) & RWShared sds & TC r & TC w

:: WriteResult p r w = E. sds: WriteResult (Set SDSNotifyRequest) (sds p r w) & TC r & TC w & RWShared sds
	/**
	 * Denotes that writing to a SDS had lead to some asynchronous action. A set of notify requests is also returned,
	 * because it could be the case that a part of the operation has completed (yielding tasks to be notified),
	 * but some other part is still waiting for a result. We choose that those SDS'es which are written to are
	 * immediately notified, instead of deferring the notification until the whole SDS is written to.
	 *
	 * The SDS is required to be a Readable AND Writeable, because writing to a SDS may require reading from another.
	 */
	| E. sds: AsyncWrite (sds p r w) & RWShared sds & TC r & TC w

:: ModifyResult p r w = E.sds: ModifyResult r w (sds p r w) & TC r & TC w & RWShared sds
	// We include the modify function so that async operations can be resumed later.
	// TODO: f should be removed, as it is not the responsibility of the modify function to return the modifier function
	| E. sds: AsyncModify (sds p r w) (r -> MaybeError TaskException w) & RWShared sds

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
	{ reqTaskId     :: TaskId       //Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
	, reqSDSId      :: SDSIdentity  //Id of the actual SDS used to create this request (may be a derived one)
	, cmpParam      :: Dynamic      //Parameter we are saving for comparison
	, cmpParamText  :: String       //String version of comparison parameter for tracing
	, remoteOptions :: (Maybe RemoteNotifyOptions)
	}

instance < SDSNotifyRequest

:: RemoteNotifyOptions =
	{ hostToNotify :: String
	, portToNotify :: Int
	, remoteSdsId :: String
	}

//This is the definition of a shared data source
class Identifiable sds
where
	nameSDS          :: (sds p r w) [String] -> [String]

class Readable sds | Identifiable sds
where
	/* Read from a sds
	 * @param sds to read from
	 * @param context in which to read. Async shares use the context to retrieve the task id.
	 * @param When Just, denotes reading + registering for changes.
	 * @param Identity of the sds to read, not guaranteed to be the identify of the current sds we're reading from. (lenses, sequences, etc.)
	 */
	readSDS          :: !(sds p r w) p !TaskContext !(Maybe TaskId) !SDSIdentity !*IWorld -> *(!MaybeError TaskException !(ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Registrable sds | Identifiable sds
where
	readRegisterSDS      :: !(sds p r w) p !TaskContext !TaskId !*IWorld -> *(!MaybeError TaskException !(ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Writeable sds | Identifiable sds
where
	writeSDS         :: !(sds p r w) !p !TaskContext !w !*IWorld -> *(!MaybeError TaskException !(WriteResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Modifiable sds | Readable, Writeable sds
where
	/**
	 * Modify the SDS with the given function
	 * @param The original SDS, without partial evaluation of asynchronous shares.
	 * @param Function to apply to the SDS value
	 * @param The sds including partial evaluation of asynchronous shares.
	 * @param parameter
	 * @param The context in which to read/write to the SDS
	 */
	modifySDS :: !(r -> MaybeError TaskException w) !(sds p r w) p !TaskContext !*IWorld -> *(!MaybeError TaskException !(ModifyResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class RWShared sds | Readable, Writeable, Modifiable, Registrable sds

:: SDSShareOptions =
	{ domain :: String
	, port :: Int
	}

instance toString (WebServiceShareOptions r)

:: WebServiceShareOptions r = HttpShareOptions HTTPRequest (HTTPResponse -> Either String r)
	| TcpShareOptions String (String -> Either String r)

//For notification we need a predicate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== Timespec p -> Bool

//Sources provide direct access to a data source
:: SDSSource p r w = SDSSource (SDSSourceOptions p r w)

	// Allows for some keeping of local state. Writing to a SDS may require reading from that SDS.
	// In the case that this reading is asynchronous, writing could also be asynchronous. This
	// option allows to temporarily store the read result, so that we can start rewriting in order
	//  to write to the SDS, using the stored read value.
	| E. sds: SDSValue Bool r (sds p r w) & RWShared sds & TC p & TC r & TC w

:: SDSSourceOptions p r w =
	{ name          :: String
	, read          :: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write         :: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}
//Lenses select and transform data
:: SDSLens p r w  = E. ps rs ws sds: SDSLens (sds ps rs ws) (SDSLensOptions p r w ps rs ws) & RWShared sds & gText{|*|} ps & TC ps & TC rs & TC ws
:: SDSLensOptions p r w ps rs ws =
	{ name         :: String
	, param        :: p -> ps
	, read         :: SDSLensRead p r rs
	, write        :: SDSLensWrite p w rs ws
	, notify       :: SDSLensNotify p p w rs
	, reducer      :: Maybe (SDSReducer p ws w)
	}

:: SDSReducer p ws w :== p ws  -> MaybeError TaskException w

:: SDSLensRead p r rs
	= SDSRead       (p rs -> MaybeError TaskException r)  //Read original source and transform
	| SDSReadConst  (p -> r)                              //No need to read the original source

:: SDSLensWrite p w rs ws
	= SDSWrite      (p rs w  -> MaybeError TaskException (Maybe ws)) //Read original source, and write updated version
	| SDSWriteConst (p w     -> MaybeError TaskException (Maybe ws)) //No need to read the original source

:: SDSLensNotify pw pq w rs
	= SDSNotify         (pw rs w -> SDSNotifyPred pq)
	| SDSNotifyConst    (pw w    -> SDSNotifyPred pq)

//Merge two sources by selecting one based on the parameter
:: SDSSelect p r w = E. p1 p2 sds1 sds2: SDSSelect (sds1 p1 r w) (sds2 p2 r w) (SDSSelectOptions p r w p1 p2) & RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r & TC w
:: SDSSelectOptions p r w p1 p2 =
	{ name          :: String
	, select        :: p -> Either p1 p2
	, notifyl       :: SDSLensNotify p1 p2 w r
	, notifyr       :: SDSLensNotify p2 p1 w r
	}

//Read from and write to two independent SDS's
:: SDSParallel p r w = E. p1 r1 w1 p2 r2 w2 sds1 sds2: SDSParallel (sds1 p1 r1 w1) (sds2 p2 r2 w2) (SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w) & RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2
:: SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w =
	{ name          :: String
	, param         :: p -> (p1,p2)
	, read          :: (r1,r2) -> r
	, writel        :: SDSLensWrite p w r1 w1
	, writer        :: SDSLensWrite p w r2 w2
	}

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p r w = E. p1 r1 w1 p2 r2 w2 sds1 sds2: SDSSequence (sds1 p1 r1 w1) (sds2 p2 r2 w2) (SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w) & RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2
:: SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w =
	{ name          :: String
	, paraml        :: p -> p1
	, paramr        :: p r1 -> p2
	, read          :: p r1 -> Either r ((r1,r2) -> r)
	, writel        :: SDSLensWrite p w r1 w1
	, writer        :: SDSLensWrite p w r2 w2
	}

// TODO: For some reason, gText{|*|} p & TC p is not sufficient and causes overloading errors in the implementation of Readable and Writeable for SDSCache. iTask p seems to solve this for unknown reasons.
:: SDSCache p r w = E. sds: SDSCache (SDSSource p r w) (SDSCacheOptions p r w) & iTask p & TC r & TC w
:: SDSCacheOptions p r w  =
	{ write        :: p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)
	}

:: SDSCacheWrite = WriteNow | WriteDelayed | NoWrite

:: SDSRemoteSource p r w = E. sds: SDSRemoteSource (sds p r w) SDSShareOptions & RWShared sds
	// When evaluating an SDS has to be done asynchronously, the connection id can be used to query
	// the IO states in IWorld for a result. It is not guaranteed that this result is available, as the asynchronous
	// operation may not have been completed yet.
	| SDSRemoteSourceQueued ConnectionId (SDSRemoteSource p r w) SDSShareOptions

:: SDSRemoteService p r w = SDSRemoteService (WebServiceShareOptions r)
	| SDSRemoteServiceQueued ConnectionId (SDSRemoteService p r w)