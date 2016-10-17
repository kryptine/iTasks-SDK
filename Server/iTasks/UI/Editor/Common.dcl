definition module iTasks.UI.Editor.Common
/**
* This module provides some convenient editors
*/
from iTasks.UI.Editor import :: Editor
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, :: JSONNode

/**
* Editor that does nothing
*/
emptyEditor :: Editor a

/**
* Editor for lists
*/
listEditor :: (Maybe ([a] -> a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a] | JSONEncode{|*|} a

//Version without overloading, for use in generic case
listEditor_ :: (Bool a -> [JSONNode]) (Maybe ([a] -> a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a] 
