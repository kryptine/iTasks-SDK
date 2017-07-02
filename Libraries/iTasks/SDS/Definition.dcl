definition module iTasks.SDS.Definition
/**
* This module provides the types that define a shared data source
*/
from iTasks.WF.Definition import :: TaskException, class iTask
from iTasks._Framework.IWorld import :: IWorld

import iTasks._Framework.Generic.Visualization
import iTasks._Framework.Generic.Defaults
import iTasks.UI.Editor.Generic
import GenEq

from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe

//This is the definition of a shared data source

:: SDS p r w
	= 			            SDSSource		!(SDSSource p r w)
    | E.ps rs ws:           SDSLens         !(RWShared ps rs ws)                        (SDSLens p r w ps rs ws) & iTask ps
    | E.p1 p2:              SDSSelect       !(RWShared p1 r w)   !(RWShared p2 r w)     (SDSSelect p p1 p2 r w) & iTask p1 & iTask p2
    | E.p1 r1 w1 p2 r2 w2:  SDSParallel     !(RWShared p1 r1 w1) !(RWShared p2 r2 w2)   (SDSParallel p1 r1 w1 p2 r2 w2 p r w) & iTask p1 & iTask p2
    | E.r1 w1 p2 r2 w2:     SDSSequence     !(RWShared p  r1 w1) !(RWShared p2 r2 w2)   (SDSSequence p r1 w1 p2 r2 w2 r w) & iTask p2
							// USE IT CAREFULLY, IT CAN BREAK NOTIFICATION!
    |						SDSDynamic		!(p *IWorld -> *(MaybeError TaskException (RWShared p r w), *IWorld)) //TODO: Remove


// Common aliases
:: RWShared p r w       :== SDS p r w
:: ROShared p a         :== SDS p a ()
:: WOShared p a         :== SDS p () a

:: ReadWriteShared r w  :== SDS () r w
:: ReadOnlyShared a     :== SDS () a ()
:: WriteOnlyShared a    :== SDS () () a
:: Shared a             :== SDS () a a

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

