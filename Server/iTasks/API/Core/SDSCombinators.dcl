definition module iTasks.API.Core.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: SDSReadProjection(..), :: SDSWriteProjection(..), :: SDSNotifyPred, :: SDSSelect, :: SDSParallel, ::SDSSequence, :: WriteShare
from iTasks.Framework.Task import :: TaskException
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError, :: MaybeErrorString
from GenEq import generic gEq

// Projection of the domain with a lens
sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w

// Translate the parameter space
sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps

// Introduce a new parameter
sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | TC ps & TC pn & gEq{|*|} ps

// Choose between two SDS's based on the parameter.
// Because there may be overlap in the parameter spaces of the two SDS's
// a write to the merged SDS can invalidate both SDS's even though only one is chosen to write to.
sdsSelect :: !String !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by simultaneous access to two independent SDS's
sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by sequential access to two dependent SDS's
sdsSequence :: !String !(p r1 -> p2) !((r1,r2) -> r) !(SDSWriteProjection r1 w1 w) !(SDSWriteProjection r2 w2 w) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2
