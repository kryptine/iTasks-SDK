definition module iTasks.API.Core.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: SDSLens, :: SDSSplit, :: SDSMerge, :: SDSParallel, ::SDSSequence, :: WriteShare
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString

//'OLD' core combinators

// read combinator
(>?>) infixl 6 :: !(RWShared p rx wx) !(rx -> MaybeErrorString (RWShared p ry wy)) -> RWShared p ry wx
// write combinator
(>!>) infixl 6 :: !(RWShared p r w`) !(!w -> MaybeErrorString (RWShared p r` w``), !w r` -> MaybeErrorString [WriteShare p]) -> RWShared p r w

//'NEW' core combinators

// Projection of the domain with a lens
sdsProject :: !(RWShared p rs ws) !(SDSLens rs ws r w) -> RWShared p r w

// Translate the parameter space
sdsTranslate :: !(RWShared ps r w) !(p -> ps) -> RWShared p r w | TC ps

// Introduce a new parameter
sdsSplit :: !(RWShared ps r w) !(SDSSplit p ps pn r w) -> RWShared p r w | TC ps & TC pn

// Choose between two SDS's based on the parameter.
// Because there may be overlap in the parameter spaces of the two SDS's
// a write to the merged SDS can invalidate both SDS's even though only one is chosen to write to.
sdsMerge :: !(RWShared p1 r w) !(RWShared p2 r w) !(SDSMerge p p1 p2 r w) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by simultaneous access to two independent SDS's
sdsParallel :: !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) !(SDSParallel p1 r1 w1 p2 r2 w2 p r w) -> RWShared p r w | TC p1 & TC p2

// Create a new SDS by sequential access to two dependent SDS's
sdsSequence :: !(RWShared p rw1 rw1) !(RWShared p2 r2 w2) !(SDSSequence rw1 p2 r2 w2 r w) -> RWShared p r w | TC p2
