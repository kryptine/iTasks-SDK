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
	| LSSequence !LayoutState !LayoutState             //Combined state of two sequenced layouts
	| LSLayoutSubUIs !UI (LayoutTree LayoutState ())   //States of layouts applied to sub-ui's 
	| LSReference !UI
	| LSRule !LUI

:: LayoutTree a b
	= UIModified !a
	| SubUIsModified !b ![(Int,LayoutTree a b)]

// Task layouting is specified using a small DSL that's comparable with CSS
:: LayoutExpression
	= SetUIType UIType
	| SetUIAttributes UIAttributes
	| DelUIAttributes UIAttributeSelection
	| ModifyUIAttributes UIAttributeSelection (UIAttributes -> UIAttributes) //TODO: Defunctionalize further
	| CopySubUIAttributes UIAttributeSelection UIPath UIPath
	| WrapUI UIType
	| UnwrapUI
	| InsertChildUI Int UI
	| RemoveSubUIs UISelection
	| MoveSubUIs UISelection UIPath Int
	| LayoutSubUIs UISelection LayoutExpression
	| SequenceLayouts [LayoutExpression]

// In specifications of layouts, sub-parts of UI's are commonly addressed as 
// a path of child selections in the UI tree.
:: UIPath :== [Int]

// This type is a mini query language to describe a selection
// of nodes in a UI (use for removing, moving, hiding or layouting)
// We use a data type instead of a function of type (UI -> Bool) because
// we want to keep only minimal state. Using an opaque function would require
// keeping track of the full state

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

//Only match children
SelectChildren :== SelectByDepth 1

:: UIAttributeSelection
	= SelectAll
	| SelectKeys ![String]

// Basic DSL for creating layouts

// == Do nothing ==
idLayout :: LayoutExpression

// == Changing node types ==
setUIType type :== SetUIType type

// == Changing attributes ==
setUIAttributes extraAttr :== SetUIAttributes extraAttr
delUIAttributes selection :== DelUIAttributes selection
modifyUIAttributes selection modifier :== ModifyUIAttributes selection modifier
copySubUIAttributes selection src dst :== CopySubUIAttributes selection src dst

// == Changing the structure of a UI ==
wrapUI type :== WrapUI type
unwrapUI :== UnwrapUI

/*
* Insert a (static) element into a UI
*/
insertChildUI position insertion :== InsertChildUI position insertion

/**
* Remove all elements that match the predicate, but keep the removed elements in the state.
* Further changes to these elements are processed in the background. When the predicate no longer holds, the elements are inserted back into the UI.
* When new elements are added dynamically they are also tested against the predicate
*/
removeSubUIs selection :== RemoveSubUIs selection

/**
* Move all elements that match the predicate to a particular location in the tree.
* Further changes to these elements are rewritten to target the new location.
* When new elements are added dynamically they are also tested against the predicate
*/
moveSubUIs selection path pos :== MoveSubUIs selection path pos

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

applyLayout :: Layout UI -> UI 

//Reference layouts of all core layouts for testing
setUITypeRef_            :: UIType -> Layout
setUIAttributesRef_      :: UIAttributes -> Layout
delUIAttributesRef_      :: UIAttributeSelection -> Layout
modifyUIAttributesRef_   :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
copySubUIAttributesRef_  :: UIAttributeSelection UIPath UIPath -> Layout
wrapUIRef_               :: UIType -> Layout
unwrapUIRef_             :: Layout
insertChildUIRef_        :: Int UI -> Layout
removeSubUIsRef_         :: UISelection -> Layout 
moveSubUIsRef_           :: UISelection UIPath Int -> Layout
layoutSubUIsRef_         :: UISelection Layout -> Layout
sequenceLayoutsRef_      :: Layout Layout -> Layout

//When layout rules make changes, it must be tracable which layout rule caused the change
:: LayoutRuleNo :== Int

// A layout rule is the function that annotates the desired effects in an LUI tree
:: LayoutRule :== LayoutRuleNo LUI -> LUI

//Experimental type that encodes all changes that are in effect by layouts
//From this data structure both the UI with, and without the layout effects, can be deduced
:: LUI
	//UI nodes (with upstream changes)
	= LUINode UIType UIAttributes [LUI] LUIChanges LUIEffects
	//Placeholder nodes
	| LUIShiftDestination SID
	| LUIMoveDestination LayoutRuleNo Int

//Upstream UI changes
:: LUIChanges =
	{ toBeInserted  :: !Bool
	, toBeRemoved   :: !Bool
	, toBeReplaced  :: !Maybe LUI
	, toBeShifted   :: !Maybe SID
	, setAttributes :: UIAttributes
	, delAttributes :: Set UIAttributeKey
	}

:: LUIEffects =
	{ overwrittenType       :: LUIEffectStage UIType
	, overwrittenAttributes :: Map UIAttributeKey (LUIEffectStage JSONNode)
	, hiddenAttributes      :: Map UIAttributeKey (LUIEffectStage ())
	, additional            :: LUIEffectStage LayoutRuleNo
	, hidden                :: LUIEffectStage LayoutRuleNo
	, moved                 :: LUIEffectStage LayoutRuleNo
	, containsMovesBy       :: Map LayoutRuleNo Int
	, wrapper               :: LUIEffectStage LayoutRuleNo
	, unwrapped             :: LUIEffectStage LayoutRuleNo
	}

//Layout rules determine that an effect should according to that rule be applied or restored.
//This desired state change can be undone by a later rule
//Only when the downstream changes have been collected is an effect marked as 'applied'
:: LUIEffectStage a
	= ESNotApplied
	| ESToBeApplied a
	| ESApplied a
	| ESToBeUpdated a a
	| ESToBeRemoved a

noChanges :: LUIChanges
noEffects :: LUIEffects

//When shifting children, it must be tracable which source connects to which destination
:: SID :== Int

//When extracting downstream changes we need to track some state
:: LUIExtractState =
	{ movedChanges :: Map LayoutRuleNo [(Int,UIChildChange)]
	, movedUIs :: Map LayoutRuleNo [UI]
	}

initLUI :: Bool UI -> LUI
initLUIExtractState :: LUIExtractState

applyUpstreamChange :: UIChange LUI -> LUI
applyLayoutEffects :: LayoutExpression LUI -> LUI
extractDownstreamChange :: LUI LUIExtractState -> (!UIChange,!LUI)

//A layout rule can be created from a layout rule
ruleBasedLayout :: LayoutExpression -> Layout

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
layoutSubUIsRule :: UISelection LayoutExpression -> LayoutRule
sequenceLayoutsRule :: [LayoutExpression] -> LayoutRule
 
//Helper functions (exported for testing)
adjustIndex_ :: Int [LUI] -> Int
selectNode_ :: UIPath LUI -> Maybe LUI 
updateNode_ :: UIPath (LUI -> LUI) LUI -> LUI 
selectAttributes_ :: UIAttributeSelection Bool LUI -> UIAttributes
overwriteAttribute_ :: UIAttribute (Map UIAttributeKey (LUIEffectStage JSONNode)) -> (Map UIAttributeKey (LUIEffectStage JSONNode))
hideAttribute_ :: (UIAttributeKey -> Bool) UIAttributeKey (Map UIAttributeKey (LUIEffectStage ())) -> (Map UIAttributeKey (LUIEffectStage ()))
extractUIWithEffects :: LUI LUIExtractState -> (!UI,!LUI)

