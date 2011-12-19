definition module SharedCombinators

import Void, Error, GenEq
from Shared import :: ReadWriteShared

// special cases for symmetric shareds (same read/write type) & read only shareds
:: Shared			a :== ReadWriteShared a a
:: ReadOnlyShared	a :== ReadWriteShared a Void

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapSharedRead	:: !(r -> r`)													!(ReadWriteShared r w) -> ReadWriteShared r` w
mapSharedWrite	:: !(w` r -> w)													!(ReadWriteShared r w) -> ReadWriteShared r w`
mapShared		:: !(!r -> r`,!w` r -> w)										!(ReadWriteShared r w) -> ReadWriteShared r` w`
mapSharedError	:: !(!r -> MaybeErrorString r`, !w` r -> MaybeErrorString w)	!(ReadWriteShared r w) -> ReadWriteShared r` w`
/**
* Converts a shared reference to a read only shared.
* Writing to a read only shared has no effect.
*
* @param A reference to shared data
* @return A read-only reference to shared data providing the same data, but with write type Void
*/
toReadOnlyShared :: !(ReadWriteShared r w) -> ReadOnlyShared r

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) (w0,w1)
(>+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w0
(|+<) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadWriteShared (r0,r1) w1
(|+|) infixl 6 :: !(ReadWriteShared r0 w0) !(ReadWriteShared r1 w1) -> ReadOnlyShared (r0,r1)

// Compose symmetric shared references and only write to one of the shares if the value changed (read shared <> value to write).
(>&<) infixl 6 :: !(Shared a) !(Shared b) -> (Shared (a,b)) | gEq{|*|} a & gEq{|*|} b

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param ReadWriteShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a) !(Shared b) -> (!Shared a,!Shared b)