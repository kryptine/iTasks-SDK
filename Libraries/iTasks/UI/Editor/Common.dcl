definition module iTasks.UI.Editor.Common
/**
* This module provides some convenient editors
*/
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Definition import :: UI, :: UIChildChange
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, :: JSONNode
import iTasks.Internal.Generic.Defaults
from Data.Generics.GenEq import generic gEq

/**
* Editor that does nothing
*/
emptyEditor :: Editor a

/**
* Determines the diff between an old and a new list of children,
* consisting of insert, remove and move change instructions.
* If possible move instructors are generated instead of a remove/insert combination.
* The worst-case time complexity is O(nm) (where n is the length of the old
* and m the length of the new children list). The complexity however decreases with
* more similar old and new lists and is O(n) for equal lists.
*
* @param: Old:   The previous child list.
* @param: New:   The new child list.
* @param: To UI: A function to map children to UIs.
* @return        A list of index/change pairs as expected by 'iTasks.UI.Definition.ChangeUI'.
*/
diffChildren :: ![a] ![a] !(a -> UI) -> [(!Int, !UIChildChange)] | gEq{|*|} a

/**
* Simple dropdown that edits an index by choosing from a list of labels
*/
chooseWithDropdown :: [String] -> Editor Int

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

