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
from Data.Map  import :: Map
from Text.JSON import :: JSONNode

// When a layout changes the stucture of the UI, changes to the UI have to be
// changed too to route the changes to the correct place in the structure
:: Layout =
	{ apply   :: UI                     -> (UIChange,LayoutState) // Modify the UI layout to the existing UI
	, adjust  :: (UIChange,LayoutState) -> (UIChange,LayoutState) // Rewrite changes to the UI to accomodate for the changes caused by the layout
	, restore :: LayoutState -> UIChange                          // Modify the UI to a state as if the layout had never been applied
	}

:: LayoutState
	= LSNone                                           //No state is tracked for a layout
	| LSWrap UI                                        //State for unwrap tasks
	| LSUnwrap UI                                      //State for unwrap tasks
	| LSSequence LayoutState LayoutState               //Combined state of two sequenced layouts
	| LSLayoutSubUIs UI (LayoutTree LayoutState ())    //States of layouts applied to sub-ui's 
	| LSRemoveSubUIs UI (LayoutTree LayoutRemoval LayoutRestores) //UI's that were removed by the layout

:: LayoutTree a b
	= UIModified a
	| SubUIsModified b [(Int,LayoutTree a b)]

:: LayoutRemoval
	= LRRemoved Int        //When a UI is first removed, we mark it with this constructor
						   //The Int is the number of children of this node that were already moved when this node was removed
    | LRMoved UIChange     //When a removed UI is inserted somewhere else, we mark it with this constructor

:: LayoutRestores :== Map Int Int //When layouts that were moved, are no longer moved we need to track that. They can then be removed at the destination.
	
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
	| SelectRoot
	| SelectChildren
	| SelectDescendents
	| SelectByType UINodeType
	| SelectByAttribute String JSONNode
	| SelectByHasAttribute String
	| SelectByNumChildren Int //Mostly to match containers with 0 or 1 children
	| SelectByHasChildrenOfType UINodeType //E.g. to check if a step/parallel has actions
	//Relative selection
	| SelectRelative UIPath UISelection
	//Set operations
	| SelectAND UISelection UISelection //Intersection
	| SelectOR UISelection UISelection //Union
	| SelectNOT UISelection //Inverse

:: UIAttributeSelection
	= SelectAll
	| SelectKeys ![String]

// Basic DSL for creating layouts

// == Do nothing ==
idLayout :: Layout 

// == Changing node types ==
setUIType :: UINodeType -> Layout

// == Changing attributes ==
setUIAttributes      :: UIAttributes -> Layout
delUIAttributes      :: [String] -> Layout
modifyUIAttributes   :: String (JSONNode -> UIAttributes) -> Layout

copySubUIAttributes  :: UIAttributeSelection UIPath UIPath -> Layout

// == Changing the structure of a UI ==

//* Create a new UI node which has the original UI as its only child.
wrapUI :: UINodeType -> Layout

//* Replace the UI by its first child. 
unwrapUI :: Layout

//* Flatten the tree of children in pre-order
flattenUI :: Layout

/*
* Insert a (static) element into a UI
*/
insertSubUI  :: UIPath UI     -> Layout
/**
* Remove all elements that match the predicate, but keep the removed elements in the state.
* Further changes to these elements are processed in the background. When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate
*/
removeSubUIs   :: UISelection -> Layout 
/**
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubUIs   :: UISelection UIPath -> Layout

// == Composition of layouts ==
/**
* Apply a layout locally to parts of a UI
*/
layoutSubUIs :: UISelection Layout -> Layout
/**
* Apply multiple layouts sequentially. The UI changes that have been transformed by one layout are further transformed by the next layout
*/
sequenceLayouts :: Layout Layout -> Layout

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
	= BranchLayout LayoutState
	| ChildBranchLayout NodeLayoutStates
	
:: TaskHost a = InTaskHost | NoTaskHost
