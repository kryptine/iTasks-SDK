definition module iTasks.API.Common.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: WriteShare
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead			:: !(r -> r`)					!(RWShared r w) -> RWShared r` w
mapWrite		:: !(w` r -> Maybe w)			!(RWShared r w) -> RWShared r w`
mapReadWrite	:: !(!r -> r`,!w` r -> Maybe w)	!(RWShared r w) -> RWShared r` w`

mapReadError		:: !(r -> MaybeErrorString r`)										!(RWShared r w) -> RWShared r` w
mapWriteError		:: !(w` r -> MaybeErrorString (Maybe w))							!(RWShared r w) -> RWShared r w`
mapReadWriteError	:: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w))	!(RWShared r w) -> RWShared r` w`

toReadOnly :: !(RWShared r w) -> ROShared r

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) (wx,wy)
(>+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wx
(|+<) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) wy
(|+|) infixl 6 :: !(RWShared rx wx) !(RWShared ry wy) -> RWShared (rx,ry) Void

// Use the value of one share as parameter for another
(>+>) infixl 6 :: !(RWShared r0 w0) !(r0 -> (RWShared r1 w1)) -> RWShared r1 w1

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
symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared a a) !(RWShared b b) -> (!RWShared a a, !RWShared b b)

