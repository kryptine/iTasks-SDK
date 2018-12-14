module CheckProperties
/*
  First experiments to use Gast to verify properties of iTasks UI layouting algorithms
  This should be integrated with the unit-test framework at some point
*/
import StdEnv
import iTasks.UI.Layout
import iTasks.UI.Definition
import iTasks.UI.Editor.Common
import iTasks.Util.Trace
import Gast.Testable
import Gast.GenLibTest
import Gast.Gen
import Gast.StdProperty

import StdGeneric
from StdFunc import o, flip
import StdEnum, StdBool, StdTuple, StdDebug
from Data.Map import :: Map
import qualified Data.Map as Map
import Text.GenJSON
import Text
import Data.Functor, Data.List

instance == UI where (==) x y = x === y
//Derive the necessary generic functions
derive ggen UI, /* UINodeType,*/ UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, JSONNode
derive gLess UI, UINodeType,  UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, Map, JSONNode
derive genShow UI, UINodeType, UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, Map, JSONNode
derive bimap []

derive gPrettyTrace UI, UINodeType
derive gPrettyTrace JSONNode

ggen{|Map|} fk fv s = [newMap]

ggen{|UINodeType|} s = [UIContainer] //When testing structure changes, limit the amount of different node types...

//Some arbitrary constants...
mediumUI = (UI UIContainer Tip [(UI UIContainer Tip [(UI UIContainer Tip [(UI UIContainer Tip [(UI UIContainer Tip [])]),(UI UIContainer Tip [(UI UIContainer Tip [])]),(UI UIContainer Tip []),(UI UIContainer Tip [])]),(UI UIContainer Tip [(UI UIContainer Tip [])]),(UI UIContainer Tip []),(UI UIContainer Tip []),(UI UIContainer Tip [])]),(UI UIContainer Tip [(UI UIContainer Tip [(UI UIContainer Tip [])]),(UI UIContainer Tip []),(UI UIContainer Tip []),(UI UIContainer Tip [])]),(UI UIContainer Tip [(UI UIContainer Tip []),(UI UIContainer Tip [(UI UIContainer Tip [])])]),(UI UIContainer Tip [])]) //Start = ggen{|*|} genState !! 10000000

// Properties that should hold for every layout

/* If you apply a layout, and after a series of changes restore it, it should be as if the layout was never applied */
applyAndRevert layout changes ui = withLayout === withoutLayout
where
	withLayout
		//Apply the layout
		# (achange, state) = layout.Layout.apply ui
		# ui = applyUIChange achange ui
		//Transform all further changes and apply them
		# (ui,state) = applyChanges changes (ui,state)
		//Revert the layout
		= applyUIChange (layout.Layout.restore state) ui
	where
		applyChanges [] (ui,state) = (ui,state)
		applyChanges [c:cs] (ui,state) 
			# (c,state) = layout.Layout.adjust (c,state)
			# ui = applyUIChange c ui
			= applyChanges cs (ui,state)
 
	withoutLayout = foldl (flip applyUIChange) ui changes

/* Every change that would be 'valid' (e.g. only targeting existing parts of the ui) without the layout, should be valid
   with the layout. In other words, a layout should not introduce invalid changes.
*/
remainValid layout changes ui
	# withLayout = applyChangesWithLayout layout changes ui
	# withoutLayout = applyChangesWithoutLayout changes ui
	= fst withoutLayout ==> fst withLayout

/* All optimized incremental layouts should have the same final effect as simpler
   reference layouts
*/
compareToReference reference layout changes ui
	//= applyChangesWithLayout reference changes ui =.= applyChangesWithLayout layout changes ui
	= (applyChangesWithLayout reference changes ui) == (applyChangesWithLayout layout changes ui)

uiSize :: UI -> Int
uiSize (UI _ _ items) = foldr (\i s -> s + uiSize i) 1 items

applyChangesWithLayout :: Layout [UIChange] UI -> (Bool,UI)
applyChangesWithLayout layout changes ui
	//Apply the layout
	# (achange, state) = layout.Layout.apply ui
	# ui = applyUIChange achange ui
	//Transform all further changes and apply them
	# (ok,ui,state) = applyChanges changes (True,ui,state)
	= (ok,ui)
where
	applyChanges [] (ok,ui,state) = (ok,ui,state)
	applyChanges [c:cs] (ok,ui,state) 
		# (c,state) = layout.Layout.adjust (c,state)
		# ok = ok && isValidChange ui c
		# ok = True
		# ui = applyUIChange c ui
		= applyChanges cs (ok,ui,state)

applyChangesWithoutLayout :: [UIChange] UI -> (Bool,UI)
applyChangesWithoutLayout changes ui
	= foldl (\(o,u) c -> (o && isValidChange u c, applyUIChange c u)) (True,ui) changes

//Check if the change is applicable to this UI (e.g. It does not address nodes that don't exist)
isValidChange :: UI UIChange -> Bool
isValidChange _ NoChange = True
isValidChange _ (ReplaceUI _) = True
isValidChange (UI _ _ items) (ChangeUI _ childChanges) = validChildChanges childChanges items
where
	validChildChanges [] items = True
	validChildChanges [(i,ChangeChild change):is] items
		| i < 0 || i >= length items  = False 
		| not (isValidChange (items !! i) change) = False
												  = validChildChanges is (updateAt i (applyUIChange change (items !! i)) items)

	validChildChanges [(i,InsertChild item):is] items 
		| i < 0 || i > length items  = False 
									 = validChildChanges is (insertAt i item items)

	validChildChanges [(i,RemoveChild):is] items 
		| i < 0 || i >= length items  = False 
									  = validChildChanges is (removeAt i items)

	validChildChanges [(i,MoveChild d):is] items 
		| i < 0 || i >= length items || d < 0 || d >= length items = False
			= validChildChanges is (insertAt d (items !! i) (removeAt i items))

// Tests for every core layout

// == Changing node types ==
checkSetUIType :: UINodeType [UIChange] UI -> Bool
//checkSetUIType type changes ui = applyAndRevert (setUIType type) changes ui
checkSetUIType type changes ui = compareToReference (setUITypeRef_ type) (setUIType type) changes ui

checkSetUIAttributes :: UIAttributes [UIChange] UI -> Bool
//checkSetUIAttributes attr changes ui = applyAndRevert (setUIAttributes attr) changes ui
checkSetUIAttributes attr changes ui = compareToReference (setUIAttributesRef_ attr) (setUIAttributes attr) changes ui

checkDelUIAttributes :: UIAttributeSelection [UIChange] UI -> Bool
//checkDelUIAttributes sel changes ui = applyAndRevert (delUIAttributes sel) changes ui
checkDelUIAttributes sel changes ui = compareToReference (delUIAttributesRef_ sel) (delUIAttributes sel) changes ui

checkModifyUIAttributes :: UIAttributeSelection [UIChange] UI -> Bool
//checkModifyUIAttributes sel changes ui = applyAndRevert (modifyUIAttributes sel id) changes ui //TODO use other functions than 'id'
checkModifyUIAttributes sel changes ui = compareToReference (modifyUIAttributesRef_ sel id) (modifyUIAttributes sel id) changes ui //TODO use other functions than 'id'

checkCopySubUIAttributes :: UIAttributeSelection UIPath UIPath [UIChange] UI -> Bool
//checkCopySubUIAttributes sel src dst changes ui = applyAndRevert (copySubUIAttributes sel src dst) changes ui
checkCopySubUIAttributes sel src dst changes ui = compareToReference (copySubUIAttributesRef_ sel src dst) (copySubUIAttributes sel src dst) changes ui

checkWrapUI :: UINodeType [UIChange] UI -> Bool
//checkWrapUI type changes ui = applyAndRevert (wrapUI type) changes ui
checkWrapUI type changes ui = compareToReference (wrapUIRef_ type) (wrapUI type) changes ui

checkUnwrapUI :: [UIChange] UI -> Bool
//checkUnwrapUI changes ui = applyAndRevert unwrapUI changes ui
checkUnwrapUI changes ui = compareToReference unwrapUIRef_ unwrapUI changes ui

checkInsertChildUI :: Int UI [UIChange] UI -> Bool
//checkInsertChildUI idx insert changes ui = applyAndRevert (insertChildUI idx insert) changes ui
checkInsertChildUI idx insert changes ui = compareToReference (insertChildUIRef_ idx insert) (insertChildUI idx insert) changes ui

checkRemoveSubUIs :: UISelection [UIChange] UI -> Bool
//checkRemoveSubUIs selection changes ui = applyAndRevert (removeSubUIs selection) changes ui
checkRemoveSubUIs selection changes ui = compareToReference (removeSubUIsRef_ selection) (removeSubUIs selection) changes ui

checkMoveSubUIs :: UISelection UIPath [UIChange] UI -> Bool
//checkMoveSubUIs selection dst changes ui = applyAndRevert (moveSubUIs selection dst 0) changes ui
checkMoveSubUIs selection dst changes ui = compareToReference (moveSubUIsRef_ selection dst 0) (moveSubUIs selection dst 0) changes ui

//If the target path exists in a ui, then moving elements around should not affect the number of elements
checkSizeMoveSubUIs :: UISelection UIPath Int [UIChange] UI -> Property
checkSizeMoveSubUIs selection dst pos changes ui
	# withLayout = applyChangesWithLayout (moveSubUIs selection dst pos) changes ui
	# withoutLayout = applyChangesWithoutLayout changes ui
	= (dstExists dst pos (snd withoutLayout)) ==> uiSize (snd withLayout) == uiSize (snd withoutLayout)
where

	dstExists :: UIPath Int UI -> Bool
	dstExists [] pos (UI _ _ items) = pos >= 0 && pos <= length items
	dstExists [s:ss] pos (UI _ _ items)
		| s >= 0 && s < length items = dstExists ss pos (items !! s)
								     = False
//Check valid 
checkValidMoveSubUIs :: UISelection UIPath Int [UIChange] UI -> Property
checkValidMoveSubUIs selection dst pos changes ui = remainValid (moveSubUIs selection dst pos) changes ui

checkLayoutSubUIs :: UISelection [UIChange] UI -> Bool
checkLayoutSubUIs selection changes ui = compareToReference (layoutSubUIsRef_ selection (setUIType (UIDebug))) (layoutSubUIs selection (setUIType (UIDebug))) changes ui


//Bug:

bug1 changes = compareToReference bug1ref bug1sut changes mediumUI
bug1sut = sequenceLayouts (insertChildUI 0 (ui UIComponent)) (layoutSubUIs (SelectByPath [0]) (setUIType UIDebug))
bug1ref = sequenceLayoutsRef_ (insertChildUIRef_ 0 (ui UIComponent)) (layoutSubUIsRef_ (SelectByPath [0]) (setUITypeRef_ UIDebug))

bug2 changes = compareToReference bug2ref bug2sut changes mediumUI
bug2sut = (layoutSubUIs (SelectByPath [0]) (setUIType UIDebug))
bug2ref = (layoutSubUIsRef_ (SelectByPath [0]) (setUITypeRef_ UIDebug))

bug3 changes = compareToReference bug3ref bug3sut changes mediumUI
bug3sut = insertChildUI 0 (ui UIComponent)
bug3ref = insertChildUIRef_ 0 (ui UIComponent)
//Tests for composite layouts
NUM :== 1000000

//Start = testn NUM checkSetUIType
//Start = testn NUM checkSetUIAttributes
//Start = testn NUM checkDelUIAttributes
//Start = testn NUM checkModifyUIAttributes
//Start = testn NUM checkCopySubUIAttributes
//Start = testn NUM checkWrapUI
//Start = testn NUM checkUnwrapUI
//Start = testn NUM checkInsertChildUI
//Start = testn NUM checkRemoveSubUIs
//Start = testn NUM (\s p c -> checkMoveSubUIs s p c mediumUI)
//Start = applyChangesWithLayout (moveSubUIs SelectChildren [] 0) [ReplaceUI (UI UIContainer Tip [UI UIContainer Tip []])] mediumUI 
//Start = testn NUM checkSizeMoveSubUIs
//Start = testn NUM checkValidMoveSubUIs
//Start = (applyUIChange refchange mediumUI) == (applyUIChange change mediumUI)

//Start = test (checkMoveSubUIs SelectChildren [1] [] mediumUI)
//Start = Test [Tests NUM, RandomSeed 1984] checkLayoutSubUIs 
Start = Test [Tests NUM, RandomSeed 1982] bug1  
//Start = Test [Tests NUM, RandomSeed 1984] (bug2 [ChangeUI [] [(1,RemoveChild),(1,RemoveChild)]])
/*
Start = sideBySideTrace ("Reference", applyChangesWithLayout bug3ref [ChangeUI [] [(0,MoveChild 0),(0,RemoveChild)]] mediumUI)
						("SUT", applyChangesWithLayout bug3sut [ChangeUI [] [(0,MoveChild 0),(0,RemoveChild)]] mediumUI)
*/
// PROPERTY FOR iTasks.UI.Editor.Common.diffChildren
//Start = testn 1000000 correctDiffChildren

// TODO: How many distinct children to generate? As only the number of elements and order matters,
// it seems that two distinct children are enough
:: Child = A | B | C// | D | E | F | G | H | I | J

derive gEq Child
derive ggen Child
derive genShow Child, UI, Map, JSONNode, UIType
derive gPrint Child, UI, Map, JSONNode, UIType

instance == UI where
    == x y = x === y

// incrementally updating the UI list from the old to the new list of children,
// results in the new list of UIs
correctDiffChildren :: [Child] [Child] -> Property
correctDiffChildren old new =
    newUIs =.= simulateUpdate ( diffChildren old
                                             new
                                             (\x y -> if (x === y) NoChildUpdateRequired ChildUpdateImpossible)
                                             dummyUI
                              )
                              oldUIs
where
    // the actual update is performed on the client and implemented in javascript,
    // so it is simulated here
    simulateUpdate :: [(Int, UIChildChange)] [UI] -> [UI]
    simulateUpdate updates uis = foldl simulateUpdateInstruction uis updates

    simulateUpdateInstruction :: [UI] (Int, UIChildChange) -> [UI]
    simulateUpdateInstruction uis (idx, change) = case change of
        RemoveChild      = removeAt idx uis
        InsertChild ui   = insertAt idx ui uis
        MoveChild newIdx = insertAt newIdx (uis !! idx) (removeAt idx uis)

    oldUIs :: [UI]
    oldUIs = dummyUI <$> old

    newUIs :: [UI]
    newUIs = dummyUI <$> new

    // it doesn't really matter how to do this translation,
    // but it should be a bijection between children and UIs
    dummyUI :: a -> UI | genShow{|*|} a
    dummyUI x = UI UIEmpty ('Map'.singleton (show1 x) JSONNull) []

