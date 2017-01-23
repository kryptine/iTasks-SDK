definition module iTasks.UI.Layout
/**
* This module provides a simple DSL for creating layouts.
* Layouts are stateful transformations on a stream of UIChange events.
* They rearrange UI's when they are initially created and modify incremental
* updates that are later applied accordingly.
*/

from iTasks.API.Core.TaskCombinators import class tune
from iTasks.UI.Definition import :: UI, :: UINodeType, :: UIAttributes, :: UIChange

from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Text.JSON import :: JSONNode

// When a layout changes the stucture of the UI, changes to the UI have to be
// changed too to route the changes to the correct place in the structure
:: Layout      :== LayoutFun JSONNode
:: LayoutFun s :== (UIChange,s) -> (UIChange,s)

// These types are used to control when to apply layout in a task composition
:: ApplyLayout	= ApplyLayout Layout

instance tune	ApplyLayout //Apply a modification after a layout has been run

// In specifications of layouts, sub-parts of UI's are commonly addressed as 
// a path of child selections in the UI tree.
:: NodePath :== [Int]

//Basic DSL for creating more complex layouts

// == Changing node types ===
setNodeType :: UINodeType -> Layout

// == Changing attributes ===
setAttributes :: UIAttributes -> Layout
delAttributes :: [String] -> Layout
copyAttributes :: [String] NodePath NodePath -> Layout
copyAllAttributes :: NodePath NodePath -> Layout
modifyAttribute :: String (JSONNode -> UIAttributes) -> Layout

// === Changing the structure of the tree ===

//* Create a new UI node which has the original UI as its only child.
wrapUI :: UINodeType -> Layout

//* Replace the UI by its first child. 
unwrapUI :: Layout

//* Flatten the tree of children in pre-order
flattenUI :: Layout

//* Reorder a static part of a UI
reorderUI :: (UI -> UI) -> Layout 

// Operations on single specific sub-UI's indicated by a path
insertSubAt :: NodePath UI       -> Layout
removeSubAt :: NodePath          -> Layout
moveSubAt   :: NodePath NodePath -> Layout

// Group operations on selections of sub-UI's

/**
* Remove all elements that match the predicate. Further changes to these elements are discarded.
* When new elements are added dynamically they are also tested against the predicate
*/
removeSubsMatching :: NodePath (UI -> Bool)          -> Layout
/*
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubsMatching   :: NodePath (UI -> Bool) NodePath -> Layout
//* Same as moveSubsMatching, but only for direct children
moveChildren       :: NodePath (UI -> Bool) NodePath -> Layout
/**
* Remove all elements that match the predicate, but keep the removed elements in the state.
* Further changes to these elements are processed in the background. When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate
*/
hideSubsMatching   :: NodePath (UI -> Bool)          -> Layout

// Composition of layouts
sequenceLayouts   :: [Layout]               -> Layout
selectLayout      :: [(UI -> Bool, Layout)] -> Layout
conditionalLayout :: (UI -> Bool) Layout    -> Layout

layoutSubAt        :: NodePath Layout   -> Layout
layoutSubsMatching :: NodePath (UI -> Bool) Layout -> Layout
layoutSubsOfType   :: NodePath [UINodeType] Layout -> Layout
layoutChildrenOf   :: NodePath Layout -> Layout

// Easier debugging
traceLayout :: String Layout -> Layout

//TYPES EXPORTED FOR TESTING
:: NodeMoves :== [(Int,NodeMove)] 
:: NodeMove = BranchMoved
            | BranchHidden UI
            | ChildBranchesMoved NodeMoves

//This type records the states of layouts applied somewhere in a ui tree
:: NodeLayoutStates :== [(Int,NodeLayoutState)]
:: NodeLayoutState
	= BranchLayout JSONNode
	| ChildBranchLayout NodeLayoutStates
	
:: TaskHost a = InTaskHost | NoTaskHost
