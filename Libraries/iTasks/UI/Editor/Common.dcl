definition module iTasks.UI.Editor.Common
/**
* This module provides some convenient editors
*/
from iTasks.UI.Editor import :: Editor
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, :: JSONNode
import iTasks.Internal.Generic.Defaults

/**
* Editor that does nothing
*/
emptyEditor :: Editor a

/**
* Show Editor for lists
*
* @param Add:		        Determines whether new elements can be added.
*                           If this is the case a function on the current values determines the element to add,
*                           where the result of the function means:
*                               Nothing: a new value has to be entered for the new element (Enter mode)
*                               Just x:  a new element with value 'x' is added and can be updated (Update mode)
* @param Remove:            Can elements be removed?
* @param Reorder:           Can elements be reordered?
* @param Summary:           Optionally generates a summary of the list (e.g. the nr of items)
* @param Children editor:   The editor for the children
*
* @return					The list editor
*/
listEditor :: (Maybe ([a] -> Maybe a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a] | JSONEncode{|*|}, gDefault{|*|} a

//Version without overloading, for use in generic case
//The first two argument should be JSONEncode{|*|} and gDefault{|*|} which cannot be used by overloading within generic functions
listEditor_ :: (Bool a -> [JSONNode]) a (Maybe ([a] -> Maybe a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a]
