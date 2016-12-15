definition module iTasks.UI.Editor.Combinators
/**
* This module provides combinator functions for combining editors
*/
import iTasks.UI.Editor, iTasks.UI.Definition
import Data.Error

/**
* Adds UI attributes to an editor
*/
withAttributes :: UIAttributes (Editor a) -> Editor a

/**
* Adds an attribute that marks edit type: view, enter, update a
*/
withEditMode :: (Editor a) -> Editor a

/**
* Adds hint attributes to an editor by checking the edit mask
*/
withHintAttributes :: String (Editor a) -> Editor a

/**
* Adds a label property
*/
withLabel :: String (Editor a) -> Editor a

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

/**
* Create a composition of two editors 
*/
composeEditors :: UINodeType (Editor a) (Editor b) -> Editor (a,b)
