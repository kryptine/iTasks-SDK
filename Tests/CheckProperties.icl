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
import StdEnum, StdBool
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

ggen{|Map|} fk fv n rnd = [newMap]

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

/**
* When moving things around, the size of a UI should not change (if the target location exists)
*/


uiSize :: UI -> Int
uiSize (UI _ _ items) = foldr (\i s -> s + uiSize i) 1 items

pathExists :: UIPath UI -> Bool
pathExists [] _ = True
pathExists [s:ss] (UI _ _ items)
	| s >= 0 && s < length items = pathExists ss (items !! s)
								 = False

applyChangesWithLayout :: Layout [UIChange] UI -> UI
applyChangesWithLayout layout changes ui
	//Apply the layout
	# (achange, state) = layout.Layout.apply ui
	# ui = applyUIChange achange ui
	//Transform all further changes and apply them
	# (ui,state) = applyChanges changes (ui,state)
	= ui
where
	applyChanges [] (ui,state) = (ui,state)
	applyChanges [c:cs] (ui,state) 
		# (c,state) = layout.Layout.adjust (c,state)
		# ui = applyUIChange c ui
		= applyChanges cs (ui,state)

applyChangesWithoutLayout :: [UIChange] UI -> UI
applyChangesWithoutLayout changes ui
	= foldl (flip applyUIChange) ui changes

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
checkSizeMoveSubUIs :: UISelection UIPath /*[UIChange]*/ UI -> Property
checkSizeMoveSubUIs selection dst /*changes*/ ui
	# changes = []
	# withLayout = applyChangesWithLayout (moveSubUIs selection dst 0) changes ui
	# withoutLayout = applyChangesWithoutLayout changes ui
	= (pathExists dst withoutLayout) ==> uiSize withLayout == uiSize withoutLayout

//Tests for composite layouts
NUM :== 200

//Start = testn NUM checkSetUIType
//Start = testn NUM checkSetUIAttributes
//Start = testn NUM checkDelUIAttributes
//Start = testn NUM checkModifyUIAttributes
//Start = testn NUM checkCopySubUIAttributes
//Start = testn NUM checkWrapUI
//Start = testn NUM checkUnwrapUI
//Start = testn NUM checkInsertChildUI
//Start = testn NUM checkRemoveSubUIs
Start = testn NUM checkSizeMoveSubUIs


