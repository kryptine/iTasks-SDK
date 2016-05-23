definition module iTasks.API.Core.SDSCombinators

from iTasks._Framework.SDS import :: RWShared, :: ROShared, :: SDSNotifyPred, :: SDSLens, :: SDSLensRead, :: SDSLensWrite, :: SDSLensNotify, :: SDSSelect, :: SDSParallel, ::SDSSequence, :: WriteShare
from iTasks._Framework.Task import :: TaskException
from iTasks._Framework.Generic import class iTask, generic gEditor, generic gEq, generic gDefault, generic gText, :: TextFormat
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError, :: MaybeErrorString
from GenEq import generic gEq

//Apply a parametric lens
sdsLens :: !String (p -> ps) (SDSLensRead p r rs) (SDSLensWrite p w rs ws) (SDSLensNotify p w rs) !(RWShared ps rs ws) -> RWShared p r w | iTask ps

// Choose between two SDS's based on the parameter.
// Because there may be overlap in the parameter spaces of the two SDS's
// a write to the merged SDS can invalidate both SDS's even though only one is chosen to write to.
sdsSelect :: !String !(p -> Either p1 p2) !(p1 r w -> SDSNotifyPred p2) !(p2 r w -> SDSNotifyPred p1) !(RWShared p1 r w) !(RWShared p2 r w) -> RWShared p r w | iTask p1 & iTask p2

// Create a new SDS by simultaneous access to two independent SDS's
sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(RWShared p1 r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | iTask p1 & iTask p2

// Create a new SDS by sequential access to two dependent SDS's
sdsSequence :: !String !(p r1 -> p2) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | iTask p2
