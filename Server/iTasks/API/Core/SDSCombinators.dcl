definition module iTasks.API.Core.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: SDSReadProjection, :: SDSWriteProjection, :: SDSNotifyPred, :: SDSMerge, :: SDSParallel, ::SDSSequence, :: WriteShare
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError, :: MaybeErrorString

//'OLD' core combinators

// read combinator
(>?>) infixl 6 :: !(RWShared p rx wx) !(rx -> MaybeErrorString (RWShared p ry wy)) -> RWShared p ry wx
// write combinator
(>!>) infixl 6 :: !(RWShared p r w`) !(!w -> MaybeErrorString (RWShared p r` w``), !w r` -> MaybeErrorString [WriteShare p]) -> RWShared p r w

//'NEW' core combinators

// Projection of the domain with a lens
sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w

// Translate the parameter space
sdsTranslate :: !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps

// Introduce a new parameter
sdsSplit :: !(p -> (ps,pn)) !(pn r -> r) !(pn r w -> (w,SDSNotifyPred pn)) !(RWShared ps r w) -> RWShared p r w | TC ps & TC pn

// Choose between two SDS's based on the parameter.
// Because there may be overlap in the parameter spaces of the two SDS's
// a write to the merged SDS can invalidate both SDS's even though only one is chosen to write to.
sdsMerge :: !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by simultaneous access to two independent SDS's
sdsParallel :: !(p -> (p1,p2)) !((r1,r2) -> r) !(w -> (w1,w2)) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by sequential access to two dependent SDS's
sdsSequence :: !(rw1 -> p2) !((rw1,r2) -> r) !(w -> (rw1,w2)) !(RWShared p rw1 rw1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2

