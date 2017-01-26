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
:: UIPath :== [Int]

// This type is a mini query language to describe a selection
// of nodes in a UI (use for removing, moving, hiding or layouting)
// We use a data type instead of a function of type (UI -> Bool) because
// we want to keep only minimal state. Using an opaque function would require
// keeping track of the full state

:: UISelection
	= SelectByPath UIPath //Direct addressing
	| SelectChildren
	| SelectByType UINodeType
	| SelectByNumChildren Int //Mostly to match containers with 0 or 1 children
	| SelectByHasChildrenOfType UINodeType //E.g. to check if a step/parallel has actions
	//Set operations
	| SelectAND UISelection UISelection //Intersection
	| SelectOR UISelection UISelection //Union
	| SelectNOT UISelection //Inverse

//Basic DSL for creating more complex layouts

// == Changing node types ===
setNodeType :: UINodeType -> Layout

// == Changing attributes ===
setAttributes :: UIAttributes -> Layout
delAttributes :: [String] -> Layout
copyAttributes :: [String] UIPath UIPath -> Layout
copyAllAttributes :: UIPath UIPath -> Layout
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
insertSubAt :: UIPath UI     -> Layout
removeSubAt :: UIPath        -> Layout
moveSubAt   :: UIPath UIPath -> Layout

// Group operations on selections of sub-UI's

/**
* Remove all elements that match the predicate. Further changes to these elements are discarded.
* When new elements are added dynamically they are also tested against the predicate
*/
removeSubsMatching :: UIPath (UI -> Bool)          -> Layout
/*
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubsMatching   :: UIPath (UI -> Bool) UIPath -> Layout
//* Same as moveSubsMatching, but only for direct children
moveChildren       :: UIPath (UI -> Bool) UIPath -> Layout
/**
* Remove all elements that match the predicate, but keep the removed elements in the state.
* Further changes to these elements are processed in the background. When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate
*/
hideSubsMatching   :: UIPath (UI -> Bool)          -> Layout

// Composition of layouts
sequenceLayouts   :: [Layout]               -> Layout
selectLayout      :: [(UI -> Bool, Layout)] -> Layout

conditionalLayout :: (UI -> Bool) Layout    -> Layout

layoutSubAt        :: UIPath Layout   -> Layout
layoutSubsMatching :: UIPath (UI -> Bool) Layout -> Layout
layoutSubsOfType   :: UIPath [UINodeType] Layout -> Layout
layoutChildrenOf   :: UIPath Layout -> Layout

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
