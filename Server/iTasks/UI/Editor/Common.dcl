definition module iTasks.UI.Editor.Common
/**
* This module provides some convenient editors
*/
from iTasks.UI.Editor import :: Editor
from Data.Maybe import :: Maybe

/**
* Editor that does nothing
*/
emptyEditor :: Editor a

/**
* Editor for lists
*/
listEditor :: (Maybe ([a] -> a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a]
