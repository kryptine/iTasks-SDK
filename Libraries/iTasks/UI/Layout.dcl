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
from Data.Either import :: Either

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
	| LSType !UI                                       //State for layouts that change the type
	| LSAttributes !UIAttributes                       //State for layouts that modify attributes
	| LSAttributeChanges !UIAttributes !UIAttributes   //A more extended state for layouts that modify attributes
	| LSWrap !UI                                       //State for unwrap layouts
	| LSUnwrap !UI                                     //State for unwrap layouts
	| LSInsert !Int                                    //State for inserting layouts
	| LSSequence !LayoutState !LayoutState             //Combined state of two sequenced layouts
	| LSLayoutSubUIs !UI (LayoutTree LayoutState ())   //States of layouts applied to sub-ui's 
	| LSRemoveSubUIs !MvUI                             //UI's that were removed by the layout
	| LSReference !UI

:: LayoutTree a b
	= UIModified !a
	| SubUIsModified !b ![(Int,LayoutTree a b)]

// This is an extended version of UI that annotates UI's with additional information about nodes that were removed, moved or restored.
:: MvUI = { type      :: UINodeType        //From UI
		  , attr      :: UIAttributes      //From UI
          , removed   :: Bool              //Do we hide this node downstream?
		  , moved     :: Bool              //Have we moved this node to another node?
                                           //They were inserted somewhere, so we should know that we have to remove them there
		  , dstChange :: UIChange          //If we have moved an item, we need to store local changes such that they can be applied in the target location
		  , children  :: [MvUIChild]       //Either items original nodes, or additional marks
		  }

:: MvUIChild
	= MvUIItem MvUI           //Upstream UI nodes with their annotations
	| MvUIMoveDestination Int //A marker for the segment in the upstream ui where the moved nodes have been inserted (n should equal the amount of moved nodes)
	| MvUINoLongerMoved Int   //A marker that indicates that at this location in the UI there were previously 'moved' nodes.
                              //A RemoveChild or ReplaceUI change has removed them.
	
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
	| SelectDescendents
	| SelectByType UINodeType
	| SelectByAttribute String JSONNode
	| SelectByHasAttribute String
	| SelectByNumChildren Int //Mostly to match containers with 0 or 1 children
	//Relative selection
	| SelectRelative UIPath UISelection
	//Check if another (sub)-selection exists
	| SelectByContains UISelection
	//No-op
	| SelectNone
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
delUIAttributes      :: UIAttributeSelection -> Layout
modifyUIAttributes   :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout

copySubUIAttributes  :: UIAttributeSelection UIPath UIPath -> Layout

// == Changing the structure of a UI ==

//* Create a new UI node which has the original UI as its only child.
wrapUI :: UINodeType -> Layout

//* Replace the UI by its first child. 
unwrapUI :: Layout

/*
* Insert a (static) element into a UI
*/
insertChildUI :: Int UI -> Layout
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
moveSubUIs   :: UISelection UIPath Int -> Layout

// == Composition of layouts ==
/**
* Apply a layout locally to parts of a UI
*/
layoutSubUIs :: UISelection Layout -> Layout
/**
* Apply multiple layouts sequentially. The UI changes that have been transformed by one layout are further transformed by the next layout
*/
sequenceLayouts :: Layout Layout -> Layout

/**
* This layout can apply any transformation on UI's, but it replaces everything on each change.
* Use this only as a debugging tool, because it will effectively remove the minimal data exchange of editors with UIChanges
*/
referenceLayout :: (UI -> UI) -> Layout

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
