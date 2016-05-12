definition module iTasks.UI.Editor.Combinators
/**
* This module provides combinator functions for combining editors
*/
import iTasks.UI.Editor

/**
* Adds hint attributes to an editor by checking the edit mask
*/
withHintAttributes :: String (Editor a) -> Editor a

/**
* Using an alternative editor when editing is disabled
*/
whenDisabled :: (Editor a) (Editor a) -> Editor a

/**
* Lift an editor to another domain
*/
liftEditor :: (a -> b) (b -> a) (Editor a) -> (Editor b)
