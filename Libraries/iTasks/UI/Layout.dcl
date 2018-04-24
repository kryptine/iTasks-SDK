definition module iTasks.UI.Layout
/**
* This module provides a simple DSL for creating layouts.
* Layouts are stateful transformations on a stream of UIChange events.
* They rearrange UI's when they are initially created and modify incremental
* updates that are later applied accordingly.
*/

from iTasks.UI.Definition import :: UI, :: UIType, :: UIAttribute, :: UIAttributes, :: UIAttributeKey, :: UIChange, :: UIChildChange

from Data.Maybe import :: Maybe
from Data.Map  import :: Map
from Data.Set import :: Set
from Data.Either import :: Either

from Text.GenJSON import :: JSONNode
from StdOverloaded import class <

// This type is a mini query language to describe a selection
// of nodes in a UI (use for removing, moving, hiding or layouting)
// We use a data type instead of a function of type (UI -> Bool) because
// we want to keep only minimal state. Using an opaque function would require
// keeping track of the full state

//Only match children
SelectChildren :== SelectByDepth 1
:: UISelection
	//Select only nodes matching the exact path
	= SelectByPath UIPath
	//Only match nodes at a given depth
	| SelectByDepth Int
	//Match any descendents of any depth
	| SelectDescendents
	//Match nodes of a certain type
	| SelectByType UIType
	//Match nodes that have a matching attribute
	| SelectByAttribute String (JSONNode -> Bool)
	//Match nodes that have the attribute
	| SelectByHasAttribute String
	//Match nodes with exactly the given number of children
	| SelectByNumChildren Int
	//Match nodes that match the given selection on traversal of the given path
	| SelectRelative UIPath UISelection
	//Check if another (sub)-selection exists
	//For example, to select child nodes that have a UIAction child you use:
	//SelectAND
	//	 SelectChildren
	//	(SelectByContains
	//		SelectAND
	//			(SelectByType UIAction)
	//			(SelectByDepth 2)
	//	)
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

// In specifications of layouts, sub-parts of UI's are commonly addressed as 
// a path of child selections in the UI tree.
:: UIPath :== [Int]

// Basic DSL for creating layouts

// == Changing node types ==
setUIType type :== setUITypeRule type

// == Changing attributes ==
setUIAttributes extraAttr :== setUIAttributesRule extraAttr
delUIAttributes selection :== delUIAttributesRule selection
modifyUIAttributes selection modifier :== modifyUIAttributesRule selection modifier
copySubUIAttributes selection src dst :== copySubUIAttributesRule selection src dst

// == Changing the structure of a UI ==
wrapUI type :== wrapUIRule type
unwrapUI :== unwrapUIRule

/*
* Insert a (static) element into a UI
*/
insertChildUI position insertion :== insertChildUIRule position insertion

/**
* Remove all elements that match the predicate, but keep the removed elements in the state.
* Further changes to these elements are processed in the background. When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate
*/
removeSubUIs selection :== removeSubUIsRule selection

/**
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubUIs selection path pos :== moveSubUIsRule selection path pos

applyLayoutRule :: LayoutRule UI -> UI 

//Reference layouts of all core layouts for testing
setUITypeRef_            :: UIType -> (UI -> UI)
setUIAttributesRef_      :: UIAttributes -> (UI -> UI)
delUIAttributesRef_      :: UIAttributeSelection -> (UI -> UI)
modifyUIAttributesRef_   :: UIAttributeSelection (UIAttributes -> UIAttributes) -> (UI -> UI)
copySubUIAttributesRef_  :: UIAttributeSelection UIPath UIPath -> (UI -> UI)
wrapUIRef_               :: UIType -> (UI -> UI)
unwrapUIRef_             :: (UI -> UI)
insertChildUIRef_        :: Int UI -> (UI -> UI)
removeSubUIsRef_         :: UISelection -> (UI -> UI)
moveSubUIsRef_           :: UISelection UIPath Int -> (UI -> UI)
layoutSubUIsRef_         :: UISelection LayoutRule -> (UI -> UI)
sequenceLayoutsRef_      :: LayoutRule LayoutRule -> (UI -> UI)

//Experimental type that encodes all changes that are in effect by layouts
//From this data structure both the UI with, and without the layout effects, can be deduced
:: LUI
	//UI nodes (with upstream changes)
	= LUINode UIType UIAttributes [LUI] LUIChanges LUIEffects
	//Placeholder nodes
	| LUIShiftDestination LUIShiftID
	| LUIMoveSource LUINo Int //Target, position
	| LUIMoveDestination LUINo Int //Target, position

//Upstream UI changes
:: LUIChanges =
	{ toBeInserted  :: !Bool
	, toBeRemoved   :: !Bool
	, toBeReplaced  :: !Maybe LUI
	, toBeShifted   :: !Maybe LUIShiftID
	, setAttributes :: UIAttributes
	, delAttributes :: Set UIAttributeKey
	}

:: LUIEffects =
	{ overwrittenType       :: LUIEffectStage UIType
	, overwrittenAttributes :: Map UIAttributeKey (LUIEffectStage JSONNode)
	, hiddenAttributes      :: Map UIAttributeKey (LUIEffectStage ())
	, additional            :: LUIEffectStage LUINo
	, hidden                :: LUIEffectStage LUINo
	, moved                 :: LUIEffectStage LUINo
	, wrapper               :: LUIEffectStage LUINo
	, unwrapped             :: LUIEffectStage LUINo
	}

//Layout rules determine that an effect should according to that rule be applied or restored.
//This desired state change can be undone by a later rule
//Only when the downstream changes have been collected is an effect marked as 'applied'
:: LUIEffectStage a
	= ESNotApplied
	| ESToBeApplied a
	| ESPartiallyApplied a // Extra intermediate stage for moved nodes
	| ESApplied a
	| ESToBeUpdated a a
	| ESToBeRemoved a
	| ESPartiallyRemoved a // Extra intermediate stage for moved nodes

//Nodes that are moved by a moveSubUIs rule need to be accesible both in their source location (to apply changes)
//and in their destination location (to apply further effects).
//To make this possible, we put those nodes in a separate table and put references in the tree

:: LUIMoves :== Map (LUINo,Int) LUI

noChanges :: LUIChanges
noEffects :: LUIEffects

//When layout rules make changes, it must be tracable which layout rule caused the change
:: LUINo = LUINo [Int]

instance < LUINo
instance == LUINo
instance toString LUINo

//When shifting children, it must be tracable which source connects to which destination
:: LUIShiftID :== Int

//A layout rule is simply a function that applies (or undoes) an effect to a LUI tree
:: LayoutRule :== LUINo (LUI,LUIMoves) -> (LUI, LUIMoves)

initLUI :: Bool UI -> LUI
initLUIMoves :: LUIMoves

applyUpstreamChange :: UIChange (LUI,LUIMoves) -> (LUI,LUIMoves)

extractDownstreamChange :: (LUI,LUIMoves) -> (!UIChange,!(LUI,LUIMoves))

extractUIWithEffects :: (LUI,LUIMoves) -> (!UI,!(LUI,LUIMoves))

//Rules
setUITypeRule :: UIType -> LayoutRule
setUIAttributesRule :: UIAttributes -> LayoutRule
delUIAttributesRule :: UIAttributeSelection -> LayoutRule
modifyUIAttributesRule :: UIAttributeSelection (UIAttributes -> UIAttributes) -> LayoutRule
copySubUIAttributesRule :: UIAttributeSelection UIPath UIPath -> LayoutRule
insertChildUIRule :: Int UI -> LayoutRule
removeSubUIsRule :: UISelection -> LayoutRule
moveSubUIsRule :: UISelection UIPath Int -> LayoutRule
wrapUIRule :: UIType -> LayoutRule
unwrapUIRule :: LayoutRule
layoutSubUIsRule :: UISelection LayoutRule -> LayoutRule
sequenceLayoutsRule :: [LayoutRule] -> LayoutRule
 
//Helper functions (exported for unit testing)
adjustIndex_ :: LUINo Int [LUI] LUIMoves -> Int
selectNode_ :: LUINo UIPath (LUI,LUIMoves) -> Maybe LUI
updateNode_ :: LUINo UIPath ((LUI,LUIMoves) -> (LUI,LUIMoves)) (LUI,LUIMoves) -> (LUI,LUIMoves)
scanToPosition_ :: LUINo Int [LUI] LUIMoves -> (Int,Bool,Maybe LUI)
selectAttributes_ :: UIAttributeSelection Bool LUI -> UIAttributes
overwriteAttribute_ :: UIAttribute (Map UIAttributeKey (LUIEffectStage JSONNode)) -> (Map UIAttributeKey (LUIEffectStage JSONNode))
hideAttribute_ :: (UIAttributeKey -> Bool) UIAttributeKey (Map UIAttributeKey (LUIEffectStage ())) -> (Map UIAttributeKey (LUIEffectStage ()))

