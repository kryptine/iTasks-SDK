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

:: SDSIdentity  :== String

:: TaskContext = EmptyContext // Used in the internals of the iTasks system
    | TaskContext TaskId // Used when a local task is reading from a share
    | RemoteTaskContext TaskId String Int // Used when a remote task is reading from a share locally

:: AsyncResult a = Result a | Queued ConnectionId

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { reqTaskId     :: TaskId       //Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
    , reqSDSId      :: SDSIdentity  //Id of the actual SDS used to create this request (may be a derived one)
    , reqTimespec   :: Timespec
    , cmpSDSId      :: SDSIdentity  //Id of the SDS we are saving for comparison
    , cmpParam      :: Dynamic      //Parameter we are saving for comparison
    , cmpParamText  :: String       //String version of comparison parameter for tracing
    , remoteOptions :: (Maybe RemoteNotifyOptions)
    }

:: RemoteNotifyOptions = RemoteNotifyOptions String Int

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
     * @param Identify of the sds to read, not guaranteed to be the identify of the current sds we're reading from. (lenses, sequences, etc.)
     */ 
    readSDS          :: (sds p r w) p !TaskContext !(Maybe TaskId) !SDSIdentity !*IWorld -> *(!MaybeError TaskException (AsyncResult r), !*IWorld) | gText{|*|} p & TC p & TC r

class Registrable sds | Identifiable sds
where
    readRegisterSDS      :: (sds p r w) p !TaskContext !TaskId !*IWorld -> *(!MaybeError TaskException (AsyncResult r), !*IWorld) | gText{|*|} p & TC p & TC r

class Writable sds | Identifiable sds
where
    writeSDS         :: (sds p r w) p !TaskContext w *IWorld -> *(!MaybeError TaskException (Set SDSNotifyRequest), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class RWShared sds | Readable, Writable, Registrable sds
class ROShared sds | Readable sds
class WOShared sds | Writable sds

:: SDSShareOptions = { domain :: String
    , port :: Int
    }    

instance toString (WebServiceShareOptions r)

:: WebServiceShareOptions r = HttpShareOptions HTTPRequest (HTTPResponse -> Either String r)
    | TcpShareOptions String (String -> Either String r)

//For notification we need a predicate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== Timespec p -> Bool

//Sources provide direct access to a data source
:: SDSSource p r w = 
	{ name          :: String
    , read			:: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

//Lenses select and transform data
:: SDSLens p r w  = E. ps rs ws sds: SDSLens (sds ps rs ws) (SDSLensOptions p r w ps rs ws) & RWShared sds & gText{|*|} ps & TC ps & TC rs & TC ws
:: SDSLensOptions p r w ps rs ws = 
    { name         :: String
    , param        :: p -> ps
    , read         :: SDSLensRead p r rs
    , write        :: SDSLensWrite p w rs ws
    , notify       :: SDSLensNotify p p w rs
    }

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

:: SDSCache p r w = SDSCache (SDSSource p r w) (SDSCacheOptions p r w) & iTask p & TC r & TC w 
:: SDSCacheOptions p r w  =
	{ write        :: p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)
	}

:: SDSCacheWrite = WriteNow | WriteDelayed | NoWrite

:: SDSRemoteSource p r w = E. sds: SDSRemoteSource (sds p r w) SDSShareOptions & RWShared sds

:: SDSRemoteService p r w = SDSRemoteService (WebServiceShareOptions r)