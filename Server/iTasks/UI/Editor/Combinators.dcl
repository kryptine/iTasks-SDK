definition module iTasks.UI.Editor.Combinators
/**
* This module provides combinator functions for combining editors
*/
import iTasks.UI.Editor
import Data.Error

/**
* Adds hint attributes to an editor by checking the edit mask
*/
withHintAttributes :: String (Editor a) -> Editor a

/**
* Using an alternative editor when editing is disabled
*/
whenDisabled :: (Editor a) (Editor a) -> Editor a

/**
* Lift an editor to another (isomorphic) domain
*/
liftEditor :: (b -> a) (a -> b) (Editor a) -> Editor b

/**
* Lift an editor to another domain which is 'bigger' than the original domain
* so conversion back to the original is not always possible
*/
liftEditorAsymmetric :: (b -> a) (a -> MaybeErrorString b) (Editor a) -> Editor b

/**
* An editor with a constant model value
*/
constEditor :: a (Editor a) -> (Editor a)
