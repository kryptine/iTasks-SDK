definition module iTasks.SDS.Definition
/**
* This module provides the types that define a shared data source
*/
from iTasks.WF.Definition import :: TaskException, class iTask, :: TaskId
from iTasks.Internal.IWorld import :: IWorld

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import Data.GenEq

from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from Data.Set import :: Set

:: SDSIdentity  :== String

//This is the definition of a shared data source
class Identifiable sds
where
    nameSDS          :: (sds p r w) [String] -> [String]

class Readable sds | Identifiable sds
where
    readSDS          :: (sds p r w) p !(Maybe TaskId) !SDSIdentity !*IWorld -> *(!MaybeError TaskException r, !*IWorld) | iTask p & TC r

class Registrable sds | Identifiable sds
where
    readRegisterSDS      :: (sds p r w) p *TaskId !*IWorld -> *(!MaybeError TaskException r, !*IWorld) | iTask p & TC r

class Writable sds | Identifiable sds
where
     writeSDS         :: (sds p r w) p w *IWorld -> *(!MaybeError TaskException (Set TaskId), !*IWorld) | iTask p & TC r & TC w

class RWShared sds | Readable, Writable, Registrable sds
class ROShared sds | Readable sds
class WOShared sds | Writable sds

//For notification we need a predicate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== p -> Bool

//Sources provide direct access to a data source
:: SDSSource p r w =
	{ name          :: String
    , read			:: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

//Lenses select and transform data
:: SDSLens p r w  = E. ps rs ws sds: SDSLens (sds ps rs ws) (SDSLensOptions p r w ps rs ws) & RWShared sds & iTask ps & TC rs & TC ws
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
:: SDSSelect p r w = E. p1 p2 sds1 sds2: SDSSelect (sds1 p1 r w) (sds2 p2 r w) (SDSSelectOptions p r w p1 p2) & RWShared sds1 & RWShared sds2 & iTask p1 & iTask p2 & TC r & TC w
:: SDSSelectOptions p r w p1 p2 =
    { name          :: String
    , select        :: p -> Either p1 p2
    , notifyl       :: SDSLensNotify p1 p2 w r
    , notifyr       :: SDSLensNotify p2 p1 w r
    }

//Read from and write to two independent SDS's
:: SDSParallel p r w = E. p1 r1 w1 p2 r2 w2 sds1 sds2: SDSParallel (sds1 p1 r1 w1) (sds2 p2 r2 w2) (SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w) & RWShared sds1 & RWShared sds2 & iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
:: SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w =
    { name          :: String
    , param         :: p -> (p1,p2)
    , read          :: (r1,r2) -> r
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p r w = E. p1 r1 w1 p2 r2 w2 sds1 sds2: SDSSequence (sds1 p1 r1 w1) (sds2 p2 r2 w2) (SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w) & RWShared sds1 & RWShared sds2 & iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
:: SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w =
    { name          :: String
	, paraml        :: p -> p1
	, paramr        :: p r1 -> p2
	, read          :: p r1 -> Either r ((r1,r2) -> r)
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

:: SDSCache p r w = SDSCache (SDSSource p r w) (SDSCacheOptions p r w)
:: SDSCacheOptions p r w  =
	{ write        :: p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)
	}

:: SDSCacheWrite = WriteNow | WriteDelayed | NoWrite