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

setNodeType :: UINodeType -> Layout

changeNodeType :: (UI -> UI) -> Layout

changeNodeAttributes :: (UIAttributes -> UIAttributes) -> Layout

//Changing tree depth

/**
* Create a new UI node which has the original UI as its only child.
*/
wrapUI :: UINodeType -> Layout
/**
* Replace the UI by its first child. 
*/
unwrapUI :: Layout

/**
* Flatten the tree of children in pre-order
*/
flattenUI :: Layout

//Operations on single specific sub-UI's indicated by a path
insertSubAt :: NodePath UI       -> Layout
removeSubAt :: NodePath          -> Layout
moveSubAt   :: NodePath NodePath -> Layout
layoutSubAt :: NodePath Layout   -> Layout

//Group operations on selections of sub-UI's
removeSubsMatching :: NodePath (UI -> Bool)          -> Layout
moveSubsMatching   :: NodePath (UI -> Bool) NodePath -> Layout
layoutSubsMatching :: NodePath (UI -> Bool) Layout   -> Layout

moveChildren :: NodePath (UI -> Bool) NodePath -> Layout
layoutChildrenOf :: NodePath Layout -> Layout

setAttributes :: UIAttributes -> Layout
copyAttributes :: NodePath NodePath -> Layout

//Composition of layouts
sequenceLayouts   :: [Layout]               -> Layout
selectLayout      :: [(UI -> Bool, Layout)] -> Layout
conditionalLayout :: (UI -> Bool) Layout    -> Layout

