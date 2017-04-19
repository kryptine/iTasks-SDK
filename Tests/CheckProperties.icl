module CheckProperties
/*
  First experiments to use Gast to verify properties of iTasks UI layouting algorithms
  This should be integrated with the unit-test framework at some point
*/
import iTasks.UI.Layout
import iTasks.UI.Definition
import Gast.Testable
import Gast.GenLibTest
import Gast.Gen
import Gast.StdProperty

import StdGeneric
from StdFunc import o, flip
import StdEnum, StdBool, StdTuple
import Data.Map
import Text.JSON
import Text

derive bimap []

/*
:: A = A | B X | C
:: X = X [X]

derive ggen A, X

Start :: [A]
Start = take 1000 (generateAll aStream)
*/

//Derive the necessary generic functions
derive ggen UI, UINodeType, UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, JSONNode
derive gLess UI, UINodeType, UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, Map, JSONNode
derive genShow UI, UINodeType, UIChange, UIChildChange, UIAttributeChange, UISelection, UIAttributeSelection, Map, JSONNode

ggen{|Map|} fk fv s = [newMap]

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
/**
* When moving things around, the size of a UI should not change (if the target location exists)
*/


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
checkSetUIType type changes ui = applyAndRevert (setUIType type) changes ui

checkSetUIAttributes :: UIAttributes [UIChange] UI -> Bool
checkSetUIAttributes attr changes ui = applyAndRevert (setUIAttributes attr) changes ui

checkDelUIAttributes :: UIAttributeSelection [UIChange] UI -> Bool
checkDelUIAttributes sel changes ui = applyAndRevert (delUIAttributes sel) changes ui

checkModifyUIAttributes :: UIAttributeSelection [UIChange] UI -> Bool
checkModifyUIAttributes sel changes ui = applyAndRevert (modifyUIAttributes sel id) changes ui //TODO use other functions than 'id'

checkCopySubUIAttributes :: UIAttributeSelection UIPath UIPath [UIChange] UI -> Bool
checkCopySubUIAttributes sel src dst changes ui = applyAndRevert (copySubUIAttributes sel src dst) changes ui

checkWrapUI :: UINodeType [UIChange] UI -> Bool
checkWrapUI type changes ui = applyAndRevert (wrapUI type) changes ui

checkUnwrapUI :: [UIChange] UI -> Bool
checkUnwrapUI changes ui = applyAndRevert unwrapUI changes ui

checkInsertChildUI :: Int UI [UIChange] UI -> Bool
checkInsertChildUI idx insert changes ui = applyAndRevert (insertChildUI idx insert) changes ui

checkRemoveSubUIs :: UISelection [UIChange] UI -> Bool
checkRemoveSubUIs selection changes ui = applyAndRevert (removeSubUIs selection) changes ui

checkMoveSubUIs :: UISelection UIPath [UIChange] UI -> Bool
checkMoveSubUIs selection dst changes ui = applyAndRevert (moveSubUIs selection dst 0) changes ui

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

//Tests for composite layouts
NUM :== 100000

//Start = testn NUM checkSetUIType
//Start = testn NUM checkSetUIAttributes
//Start = testn NUM checkDelUIAttributes
//Start = testn NUM checkModifyUIAttributes
//Start = testn NUM checkCopySubUIAttributes
//Start = testn NUM checkWrapUI
//Start = testn NUM checkUnwrapUI
//Start = testn NUM checkInsertChildUI
//Start = testn NUM checkRemoveSubUIs
Start = Test [Tests NUM, RandomSeed 1984] (\s t u -> checkSizeMoveSubUIs s [0] 0 t u)
//Start = testn NUM checkValidMoveSubUIs

