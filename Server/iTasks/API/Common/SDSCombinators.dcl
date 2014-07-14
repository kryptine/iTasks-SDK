definition module iTasks.API.Common.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: WriteShare, :: SDSNotifyPred
from iTasks.Framework.Task import :: TaskException
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString

from Text.JSON import generic JSONEncode, :: JSONNode
from GenEq import generic gEq

:: SDSReadProjection rs rt
    = SDSLensRead      (rs -> MaybeError TaskException rt) //Read lens-like
    | SDSConstRead     rt                                  //No need to read the original source

:: SDSWriteProjection rs ws wt
    = SDSLensWrite     (rs wt   -> MaybeError TaskException (Maybe ws)) //Write lens-like
    | SDSBlindWrite    (wt      -> MaybeError TaskException (Maybe ws)) //No-need to read the original source
    | SDSNoWrite

// Fix a focus parameter
sdsFocus     :: !p !(RWShared p r w) -> (RWShared p` r w) | TC p & JSONEncode{|*|} p

// Projection of the domain with a lens
sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w | TC p

// Translate the parameter space
sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | TC ps

// Introduce a new parameter
sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | TC ps & TC pn & gEq{|*|} ps

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared p r w) -> RWShared p r` w | TC p
mapWrite		:: !(w` r -> Maybe w)			!(RWShared p r w) -> RWShared p r w` | TC p
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared p r w) -> RWShared p r` w` | TC p

mapReadError		:: !(r -> MaybeError TaskException r`)								!(RWShared p r w) -> RWShared p r` w | TC p
mapWriteError		:: !(w` r -> MaybeError TaskException (Maybe w))					!(RWShared p r w) -> RWShared p r w` | TC p
mapReadWriteError	:: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w))	!(RWShared p r w) -> RWShared p r` w` | TC p

toReadOnly :: !(RWShared p r w) -> ROShared p r | TC p

//Map a list SDS of one element to the element itsel
mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w) | TC p

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy)     | TC p
(>+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx          | TC p
(|+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy          | TC p
(|+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) Void        | TC p

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared p a a) !(RWShared p b b) -> (!RWShared p a a, !RWShared p b b) | TC p

