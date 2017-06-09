module Layout
import iTasks._Framework.Test.Definition
import iTasks._Framework.Test.Stubs

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Layout.Common
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import Data.List
import StdMisc

derive JSONEncode NodeLayoutState, LayoutState, LayoutTree, MvUI, MvUIChild
derive gEq LayoutState, LayoutTree, MvUI, MvUIChild
derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UINodeType, LayoutState, LayoutTree, JSONNode, MaybeError
derive gPrettyTrace EditMask, FieldMask, CompoundMask, MvUI, MvUIChild
derive gDefault MvUI, MvUIChild

Start world = execTestSuite (testsuite "Layout" "Tests for the layout functions"
	[skip testChangeNodeType
	,skip testChangeAttributes
	,skip testWrap
	,skip testUnwrap
	,skip testInsertSubAt
	,skip testRemoveSubAt
	,skip testLayoutSubAt
	,skip testMoveSubAt
	,testRemoveSubsMatchingOnReplace
	,testRemoveSubsMatchingOnChildChange
	,testRemoveSubsMatchingOnReplaceAfterRemove 
	,testRemoveSubsMatchingOnReplaceMultipleAfterRemove
	,testRemoveSubsMatchingOnRemove
	,skip testLayoutSubsMatching
	,skip testMoveSubsMatchingInitial
	,skip testMoveSubsMatchingInitial2
	,skip testMoveSubsMatchingNewRoutes
	,skip testMoveSubsMatchingNewRoutes2
	,skip testMoveSubsMatchingNewRoutes3
	,skip testSequenceLayouts
	,skip testSelectLayout
	//Common patterns
	,skip testAutoInteractionLayoutInitial
	,skip testAutoInteractionLayoutEditorValueChange
	,skip testMoveTaskToWindow
	,skip testFlatteningOfNestedRecords
	//Complex combination
	,testCombination1
	,skip testCombination2
	,skip testDynamicBehaviour1
	//Common layouts
	,testSideBarInitial
	]) world

//Tests for the core operations of the layout library
testChangeNodeType = fail "testChangeNodeType: Changing node type"
testChangeAttributes = fail "testChangeAttributes: Changing attributes"

testWrap = fail "testWrap: Wrapping a UI in a panel"
testUnwrap = fail "testUnwrap: Unwrapping a UI from a container"

testInsertSubAt = fail "testInsertSubAt: Inserting a sub-UI at a specific path"
testRemoveSubAt = fail "testRemoveSubAt: Remove a sub-UI from a specific path"
testLayoutSubAt = fail "testLayoutSubAt: Applying another layout at a specific path (by setting attribute)"
testMoveSubAt = fail "testMoveSubAt: Moving a node from one place to another"

testRemoveSubsMatchingOnReplace = assertEqual "testRemoveSubsMatchingOnReplace: Removing everything that matches, when replacing a UI" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (ReplaceUI initUI,initState)
		= c
	exp = ReplaceUI expUI

	//Initial UI	
	initUI = uic UIPanel [ui UIContainer, ui UIEmpty, uic UIContainer [ui UIEmpty, ui UITextView], ui UIAction]
	initState = snd (sutLayout.Layout.apply initUI)

	//Expected final UI
	expUI = uic UIPanel [ui UIContainer, uic UIContainer [ui UITextView] ,ui UIAction]

testRemoveSubsMatchingOnChildChange = assertEqual "testRemoveSubsMatchingOnChildChange: Removing everything that matches, when changing a child" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (_,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
	initUI = uic UIPanel [ui UIContainer, ui UIEmpty, uic UIContainer [ui UIEmpty, ui UITextView ], ui UIAction]
	initChange = ReplaceUI initUI
	initState = snd (sutLayout.Layout.apply initUI)
	changeToReRoute = ChangeUI [] [(2,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	//Expected reroute change 
	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

testRemoveSubsMatchingOnReplaceAfterRemove = assertEqual "testRemoveSubsMatchingOnReplaceAfterRemove: Removing everything that matches, then replacing a part" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
	initUI = uic UIPanel [ui UIContainer, uic UIContainer [ui UIEmpty, ui UITextView], ui UIAction]
	initChange = ReplaceUI initUI
	initState = snd (sutLayout.Layout.apply initUI)
	changeToReRoute = ChangeUI [] [(1,ChangeChild (ReplaceUI (ui UIPanel)))]

	//Expected reroute change 
	expChange = ChangeUI [] [(1,ChangeChild (ReplaceUI (ui UIPanel)))]

testRemoveSubsMatchingOnReplaceMultipleAfterRemove = assertEqual "testRemoveSubsMatchingOnReplaceMultipleAfterRemove: Removing everything that matches, then replacing multiple parts" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
//	initChange = ReplaceUI (uic UIPanel [ui UIEmpty, ui UIContainer, uic UIContainer [ui UIEmpty, ui UIViewString], ui UIEmpty, ui UIAction])
	initUI = uic UIStep [uic UIParallel [uic UIParallel [uic UIStep [uic UIInteract [ui UIEmpty, ui UIGrid],ui UIAction, ui UIAction], uic UIStep [ui UIEmpty]]]]
	initChange = ReplaceUI initUI
	initState = snd (sutLayout.Layout.apply initUI)

	changeToReRoute = ChangeUI [] [(0,ChangeChild (ChangeUI [] 
									[(0,ChangeChild (ChangeUI []
										[(0,ChangeChild (ChangeUI []
											[(0,ChangeChild (ChangeUI []
												[(1, ChangeChild (ReplaceUI (ui UITextView)))])) ]))]))]))]

	//Expected reroute change 
//	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,ChangeChild (ReplaceUI (ui UIEditString)))])),(2,ChangeChild (ReplaceUI (ui UIActionButton)))]
	expChange = ChangeUI [] [(0,ChangeChild (ChangeUI [] 
									[(0,ChangeChild (ChangeUI []
										[(0,ChangeChild (ChangeUI []
											[(0,ChangeChild (ChangeUI []
												[(0, ChangeChild (ReplaceUI (ui UITextView)))])) ]))]))]))]

testRemoveSubsMatchingOnRemove = assertEqual "testRemoveSubsMatchingOnRemove: Removing everything that matches, then explicitly remove something" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	initUI = uic UIPanel [ui UIContainer, ui UIEmpty]
	initChange = ReplaceUI initUI
	initState = snd (sutLayout.Layout.apply initUI)

	changeToReRoute = ChangeUI [] [(0,RemoveChild),(0,RemoveChild)]
	expChange = ChangeUI [] [(0,RemoveChild)]

testLayoutSubsMatching = fail "testLayoutSubsMatching: Applying another layout to all matching nodes"

testMoveSubsMatchingInitial = assertEqual "testMoveSubsMatchingInitial: Moving nodes matching a predicate -> initial move" exp sut
where
	sutLayout = moveSubUIs (SelectAND SelectChildren (SelectOR (SelectByType UIAction) (SelectByType UIEmpty))) [0] 0

	sut = let (change,state) = sutLayout.Layout.apply initUI in (applyUIChange change initUI, state)

	exp = (expUI, expState)

	//Initial UI	
	initUI = uic UIStep [ui UIContainer, ui UIAction, ui UIEmpty]

	//Expected final UI
	expUI = uic UIStep [uic UIContainer [ui UIAction, ui UIEmpty]]

	expState = LSRemoveSubUIs defaultValue /*undef LSRemoveSubUIs initUI
					(SubUIsModified [] [(1,UIModified (LRMoved NoChange)),(2,UIModified (LRMoved NoChange))]) */
		


testMoveSubsMatchingInitial2 = assertEqual "testMoveSubsMatchingInitial2: Moving nodes matching a predicate -> initial move" exp sut
where
	sutLayout = moveSubUIs (SelectRelative [0] (SelectAND SelectDescendents (SelectByType UIAction))) [1] 0

	sut = let (change,state) = sutLayout.Layout.apply initUI in (applyUIChange change initUI, state)
	exp = (expUI,expState)

	//Initial UI	
	initUI = uic UIPanel [uic UIContainer [ui UIAction, ui UIEmpty, ui UIAction], ui UIContainer]

	//Expected final UI
	expUI = uic UIPanel [uic UIContainer [ui UIEmpty], uic UIContainer [ui UIAction, ui UIAction]]
	expState = LSRemoveSubUIs defaultValue /*LSRemoveSubUIs initUI
		(SubUIsModified [] [(0,SubUIsModified [] [(0,UIModified (LRMoved NoChange)),(2,UIModified (LRMoved NoChange))])])
		*/

testMoveSubsMatchingNewRoutes = assertEqual "testMoveSubsMatchingNewRoutes: Moving nodes matching a predicate -> check if changes are moved too" exp sut
where
	sutLayout = moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [0] 0

	sut = sutLayout.Layout.adjust (sutChange,initState)
	exp = (expChange,expState)

	//Initial state
	initShadowUI = uic UIStep [ui UIContainer, ui UIAction, ui UIAction]
	//initRemovals = (SubUIsModified [] [(1,UIModified (LRMoved NoChange)),(2,UIModified (LRMoved NoChange))])

	initState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs initShadowUI initRemovals */

	//The change that should be re-routed
	sutChange = ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))
							,(2,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "baz")] []))]

	//Expected rerouted change 
	expChange = ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))
														 ,(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "baz")] [])) ]))] 

	//In the state, the attributes should have been applied the 'shadow' administration
	expState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs (applyUIChange sutChange initShadowUI) initRemovals */
import StdDebug

testMoveSubsMatchingNewRoutes2 = assertEqual "testMoveSubsMatchingNewRoutes2: Moving nodes matching a predicate -> check if changes are moved too" exp sut
where
	sutLayout = moveSubUIs (SelectRelative [0] (SelectAND SelectDescendents (SelectByType UIAction))) [1] 0

	sut = sutLayout.adjust (sutChange,initState)
	exp = (expChange,expState)

	initShadowUI = uic UIPanel [uic UIContainer [ui UIAction, ui UIEmpty, ui UIAction], ui UIContainer]
	//initRemovals = SubUIsModified [] [(0,SubUIsModified [] [(0,UIModified (LRMoved NoChange)),(2,UIModified (LRMoved NoChange))])]
	
	initState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs initShadowUI initRemovals*/

	sutChange = ChangeUI [] [(0,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	//Expected rerouted change 
	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	//In the state, the attributes should have been applied the 'shadow' administration
	expState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs (applyUIChange sutChange initShadowUI) initRemovals */

testMoveSubsMatchingNewRoutes3 = assertEqual "testMoveSubsMatchingNewRoutes3: Moving nodes matching a predicate -> check if changes are moved too" exp sut
where
	sutLayout = moveSubUIs (SelectRelative [0] (SelectAND SelectDescendents (SelectByType UIAction))) [1] 0

	sut = sutLayout.adjust (sutChange,initState)
	exp = (expChange,expState)

	initShadowUI = uic UIPanel [uic UIContainer [ui UIAction, ui UIEmpty, ui UIAction], ui UIContainer]
	//initRemovals = SubUIsModified [] [(0,SubUIsModified [] [(0,UIModified (LRMoved NoChange)),(2,UIModified (LRMoved NoChange))])]
	
	initState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs initShadowUI initRemovals */

	//Test crude replace, first remove than re-insert
	sutChange = ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,RemoveChild),(0,InsertChild (ui UIAction)),(2,RemoveChild),(2,InsertChild (ui UIAction))] ))]

	//Expected rerouted change 
	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,RemoveChild),(0,InsertChild (ui UIAction)),(1,RemoveChild),(1,InsertChild (ui UIAction))] ))]

	//In the state, the UI should be identical
	expState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs initShadowUI initRemovals */


testSequenceLayouts = fail "testSequenceLayouts: Test sequencing multiple layouts"
testSelectLayout = fail "testSelectLayout: Test selecting of a layout"

//Tests for the common layout patterns

testMoveTaskToWindow = fail "testMoveTaskToWindow: Moving a task UI to a separate window"

::TestRecOuter =
	{ a :: TestRecInner
	, b :: TestRecInner
	}
:: TestRecInner =
	{ c :: String
	, d :: String
	}
derive class iTask TestRecInner, TestRecOuter
derive gPrettyTrace TestRecInner, TestRecOuter

testAutoInteractionLayoutInitial = assertEqual "testAutoInteractionLayoutInitial: Test if the auto interaction layout correctly turns an editor into a form" exp sut
where
	exp = ReplaceUI (uic UIContainer [stdPrompt,expIntForm])
	sut = fst (finalizeInteract.Layout.adjust ((ReplaceUI (uic UIContainer [stdPrompt,stdIntEditor])),LSNone))
	
	stdPrompt = ui UIEmpty //STUB Don't care what the prompt is!
	stdIntEditor = uia UIIntegerField
		('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "info"),("hint",JSONString"Please enter a whole number (this value is required)")
						,("taskId",JSONString "STUB"),("editorId",JSONString "v")])
			

	expIntForm = uic UIContainer [uic UIContainer [ui UIEmpty,intControl,expIcon]]
	expIcon = uia UIIcon ('DM'.fromList [("margins",JSONString "0 0 0 5"),("iconCls",JSONString "icon-info")
										,("tooltip",JSONString "Please enter a whole number (this value is required)")])

	intControl = uia UIIntegerField
		('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "info"),("hint",JSONString"Please enter a whole number (this value is required)")
						,("taskId",JSONString "STUB"),("editorId",JSONString "v")])

testAutoInteractionLayoutEditorValueChange = fail "testAutoInteractionLayoutEditorValueChange: Test if the auto interaction layout correctly maps changes in the editor to the form item"

testFlatteningOfNestedRecords = assertEqualWorld "testFlatteningOfNestedRecords: Auto interact layout should flatten a nested-record structure" exp sut
where
	//We expect a change to the control with index 3, because the autoAccuInteract flattens the form
	exp = (Ok (ChangeUI [] [(3, ChangeChild (ChangeUI [SetAttribute "setEditorValue" (JSONString "bax")] []))],newFieldMask)
          , { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}})

	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,val,vst) = gEditor{|*|}.Editor.onRefresh [] { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}} {a={c="foo",d="foo"}, b = { c = "bar", d = "bax"}} newFieldMask vst
		# world = fromStubIWorld (fromStubVSt vst)
		= ((res,val),world)


testCombination1 = assertEqual "testCombination1: Complex combination layout with insert events" exp sut
where
	sutLayout = foldl1 sequenceLayouts
        [//First stage 
		 foldl1 sequenceLayouts
        	[arrangeWithSideBar3
        	,layoutSubUIs (SelectByPath [1]) arrangeWithSideBar3
        	]
		//Second stage
        ,removeSubUIs (SelectAND SelectDescendents (SelectByType UIInteract))
        ]
	where
		arrangeWithSideBar3 :: Layout
		arrangeWithSideBar3 = foldl1 sequenceLayouts
			[wrapUI UIDebug //Push the current container down a level
			,insertChildUI 0 (ui UIComponent) //Make sure we have a target for the move
			,moveSubUIs (SelectByPath [1,0]) [0,0] 0 //Key difference
			,layoutSubUIs (SelectByPath [0]) unwrapUI //Remove the temporary wrapping panel
			]

	sut
		//Initial, followed by an event in the new structure
		# (c1,s1) = sutLayout.Layout.adjust (ReplaceUI initUI,initState)
		# (c2,s2) = sutLayout.Layout.adjust (changeToModify,s1)
		= c2

	exp = expModifiedChange

	//Initial UI	
	initUI = uic UIPanel [uic UIContainer [ui UIInteract], uic UIMenu [ui UIInteract]]
	initState = snd (sutLayout.Layout.apply initUI)

	//First rendering
	renderedUI = uic UIDebug [uic UIContainer [], uic UIDebug [uic UIMenu [], uic UIPanel []] ]
	renderedState = JSONArray [JSONArray [sSideBar,sSub1], sRmInteract]
	where
		sSideBar = JSONArray []
/*
		sSideBar = JSONArray [JSONNull,toJSON (BranchLayout JSONNull),toJSON moves, toJSON (ChildBranchLayout [(0,BranchLayout (JSONBool False))])]
		where
			moves = [(1,ChildBranchesMoved [(0,BranchMoved)])]
*/

		sSub1 = toJSON (ChildBranchLayout [(1,BranchLayout sSideBar)])

		sRmInteract = JSONNull //toJSON [(0,ChildBranchesMoved [(0,BranchMoved)]),(1,ChildBranchesMoved [(0,ChildBranchesMoved [(0,BranchMoved)])])]

	//Expected reroute change 
	changeToModify = ChangeUI [] [(2,InsertChild (uic UIToolBar [ui UIInteract]))]
	expModifiedChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar []))]))]))]

testCombination2 = assertEqual "testCombination2: Insert after global removal" exp sut
where
	sut
		//Initial, followed by an event in the new structure
		# (c2,s2) = sutLayout.Layout.adjust (changeToModify,initState)
		= c2
	exp = expModifiedChange

	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIInteract))

	initState = LSRemoveSubUIs defaultValue /* LSRemoveSubUIs (ui UIDebug) (SubUIsModified [] [(0,SubUIsModified [] [(0,UIModified (LRRemoved 0))])
					   ,(1,SubUIsModified [] [(0,SubUIsModified [] [(0,UIModified (LRRemoved 0))])])]) */

	//Change after first transform 
	changeToModify = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar [ui UIInteract]))]))]))]
	expModifiedChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar []))]))]))]


testDynamicBehaviour1 = assertEqual "testDynamicBehaviour1: Dynamic (late) activation of layouts" exp sut
where
	sutLayout = layoutSubUIs (SelectByHasAttribute "x") markFirstChild
	where
		markFirstChild = layoutSubUIs (SelectByPath [0]) (setUIAttributes ('DM'.fromList [("y",JSONBool True)]))
	
	sut
		# (c1,s1) = sutLayout.Layout.adjust (change1,initState)
		# (c2,s2) = sutLayout.Layout.adjust (change2,s1)
		= c2

	initUI = uic UIContainer [ui UIContainer]
	initState = snd (sutLayout.Layout.apply initUI)

	//Begin with a UI without attribute "x" at the root
	change1 = ReplaceUI initUI
	//Now set the attribute, this means the layout should now match the root node
	change2 = ChangeUI [SetAttribute "x" (JSONBool True)] []

	exp = ChangeUI [SetAttribute "x" (JSONBool True)] [(0,ChangeChild (ChangeUI [SetAttribute "y" (JSONBool True)] []))]

testSideBarInitial = assertEqual "testSideBarInitial: arrangeWithSideBar -> initial arrangement" exp sut
where
	sutLayout = arrangeWithSideBar 0 LeftSide 100 False

	sut = let (change,_) = sutLayout.Layout.apply initUI in (applyUIChange change initUI)

	exp = expUI 

	//Initial UI	
	initUI = uic UIStep [ui UIButton, ui UIAction, ui UITextField]

	//Expected final UI
	expUI = uiac UIPanel (directionAttr Horizontal) [uia UIButton (sizeAttr (ExactSize 100) FlexSize), uic UIStep [ui UIAction, ui UITextField]]
