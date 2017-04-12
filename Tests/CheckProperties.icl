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

import StdGeneric
from StdFunc import o, flip
import StdEnum
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

checkInsertSubUI :: UIPath UI [UIChange] UI -> Bool
checkInsertSubUI path insert changes ui = applyAndRevert (insertSubUI path insert) changes ui

checkRemoveSubUIs :: UISelection [UIChange] UI -> Bool
checkRemoveSubUIs selection changes ui = applyAndRevert (removeSubUIs selection) changes ui

checkMoveSubUIs :: UISelection UIPath [UIChange] UI -> Bool
checkMoveSubUIs selection dst changes ui = applyAndRevert (moveSubUIs selection dst) changes ui

//Tests for composite layouts
NUM :== 100

//Start = testn NUM checkSetUIType
//Start = testn NUM checkSetUIAttributes
//Start = testn NUM checkDelUIAttributes
//Start = testn NUM checkModifyUIAttributes
Start = testn NUM checkCopySubUIAttributes
//Start = testn NUM checkWrapUI
//Start = testn NUM checkUnwrapUI
//Start = testn NUM checkInsertSubUI
//Start = testn NUM checkRemoveSubUIs
//Start = testn NUM checkMoveSubUIs


