implementation module Tests.Unit.Layout
import TestFramework
import Tests.Unit.FrameworkStubs

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import Data.List
import StdMisc

derive JSONEncode NodeMove, NodeLayoutState

testLayout :: TestSuite
testLayout = testsuite "Layout" "Tests for the layout functions"
	[testChangeNodeType
	,testChangeAttributes
	,testWrap
	,testUnwrap
	,testInsertSubAt
	,testRemoveSubAt
	,testLayoutSubAt
	,testMoveSubAt
	,testRemoveSubsMatchingOnReplace
	,testRemoveSubsMatchingOnChildChange
	,testRemoveSubsMatchingOnReplaceAfterRemove 
	,testRemoveSubsMatchingOnReplaceMultipleAfterRemove
	,testRemoveSubsMatchingOnRemove
	,testLayoutSubsMatching
	,testMoveSubsMatchingInitial
	,testMoveSubsMatchingInitial2
	,testMoveSubsMatchingNewRoutes
	,testMoveSubsMatchingNewRoutes2
	,testSequenceLayouts
	,testSelectLayout
	//Common patterns
	,testAutoInteractionLayoutInitial
	,testAutoInteractionLayoutEditorValueChange
	,testMoveTaskToWindow
	,testFlatteningOfNestedRecords
	//Complex combination
	,testCombination1
	,testCombination2
	,testDynamicBehaviour1
	]

//Tests for the core operations of the layout library
testChangeNodeType = skip (fail "Changing node type")
testChangeAttributes = skip (fail "Changing attributes")

testWrap = skip (fail "Wrapping a UI in a panel")
testUnwrap = skip (fail "Unwrapping a UI from a container")

testInsertSubAt = skip (fail "Inserting a sub-UI at a specific path")
testRemoveSubAt = skip (fail "Remove a sub-UI from a specific path")
testLayoutSubAt = skip (fail "Applying another layout at a specific path (by setting attribute)")
testMoveSubAt = skip (fail "Moving a node from one place to another")

testRemoveSubsMatchingOnReplace = assertEqual "Removing everything that matches, when replacing a UI" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (ReplaceUI initUI,initState)
		= c
	exp = ReplaceUI expUI

	//Initial UI	
	initUI = uic UIPanel [ui UIContainer, ui UIEmpty, uic UIContainer [ui UIEmpty, ui UITextView], ui UIAction]
	initState = JSONNull
	//Expected final UI
	expUI = uic UIPanel [ui UIContainer, uic UIContainer [ui UITextView] ,ui UIAction]

testRemoveSubsMatchingOnChildChange = assertEqual "Removing everything that matches, when changing a child" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (_,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
	initChange = ReplaceUI (uic UIPanel [ui UIContainer, ui UIEmpty, uic UIContainer [ui UIEmpty, ui UITextView ], ui UIAction])
	initState = JSONNull
	changeToReRoute = ChangeUI [] [(2,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	//Expected reroute change 

	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

testRemoveSubsMatchingOnReplaceAfterRemove = assertEqual "Removing everything that matches, then replacing a part" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
	initChange = ReplaceUI (uic UIPanel [ui UIContainer, uic UIContainer [ui UIEmpty, ui UITextView], ui UIAction])
	initState = JSONNull
	changeToReRoute = ChangeUI [] [(1,ChangeChild (ReplaceUI (ui UIPanel)))]

	//Expected reroute change 
	expChange = ChangeUI [] [(1,ChangeChild (ReplaceUI (ui UIPanel)))]

testRemoveSubsMatchingOnReplaceMultipleAfterRemove = assertEqual "Removing everything that matches, then replacing multiple parts" exp sut
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
	initChange = ReplaceUI (uic UIStep [uic UIParallel [uic UIParallel [uic UIStep [uic UIInteract [ui UIEmpty, ui UIGrid],ui UIAction, ui UIAction], uic UIStep [ui UIEmpty]]]])

	initState = JSONNull
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

testRemoveSubsMatchingOnRemove = assertEqual "Removing everything that matches, then explicitly remove somehting" exp sut
where
	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))
	sut
		//Initial, followed by an event in the new structure
		# (c,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	initState = JSONNull

	initChange = ReplaceUI (uic UIPanel [ui UIContainer, ui UIEmpty])

	changeToReRoute = ChangeUI [] [(0,RemoveChild),(0,RemoveChild)]
	expChange = ChangeUI [] [(0,RemoveChild)]

testLayoutSubsMatching = skip (fail "Applying another layout to all matching nodes")

testMoveSubsMatchingInitial = assertEqual "Moving nodes matching a predicate -> initial move" exp sut
where
	sutLayout = (moveSubUIs (SelectAND SelectChildren (SelectOR (SelectByType UIAction) (SelectByType UIEmpty))) [0,0]) 
	sut = sutLayout.Layout.adjust (ReplaceUI initUI,initState)

	exp = (ReplaceUI expUI,expState)

	//Initial UI	
	initUI = uic UIStep [ui UIContainer, ui UIAction, ui UIEmpty]
	initState = JSONNull
	//Expected final UI
	expUI = uic UIStep [uic UIContainer [ui UIAction, ui UIEmpty]]
	expState = toJSON [(1,BranchMoved),(2,BranchMoved)]

	isTarget (UI type _ _) = (type =: UIAction) || (type =: UIEmpty)

testMoveSubsMatchingInitial2 = assertEqual "Moving nodes matching a predicate -> initial move" exp sut
where
	sutLayout = (moveSubUIs (SelectRelative [0] (SelectAND SelectDescendents (SelectByType UIAction))) [1,0]) 
	sut = sutLayout.Layout.adjust (ReplaceUI initUI,initState)

	exp = (ReplaceUI expUI,expState)

	//Initial UI	
	initUI = uic UIPanel [uic UIContainer [ui UIAction, ui UIEmpty, ui UIAction], ui UIContainer]
	initState = JSONNull
	//Expected final UI
	expUI = uic UIPanel [uic UIContainer [ui UIEmpty], uic UIContainer [ui UIAction, ui UIAction]]
	expState = toJSON [(0,ChildBranchesMoved [(0,BranchMoved),(2,BranchMoved)])]

testMoveSubsMatchingNewRoutes = assertEqual "Moving nodes matching a predicate -> check if changes are moved too" exp sut
where
	sutLayout = (moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [0,0]) 
	sut
		//Initial, followed by an event in the new structure
		# (_,s) = sutLayout.Layout.adjust (initChange,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= c
	exp = expChange

	//Initial UI	
	initChange = ReplaceUI (uic UIStep [ui UIContainer, ui UIAction, ui UIAction])
	initState = JSONNull
	changeToReRoute = ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))
								  ,(2,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "baz")] []))]

	//Expected reroute change 
	expChange = ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))
														 ,(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "baz")] [])) ]))] 

	isAction (UI type _ _) = type =: UIAction

testMoveSubsMatchingNewRoutes2 = assertEqual "Moving nodes matching a predicate -> check if changes are moved too" exp sut
where
	sutLayout = (moveSubUIs (SelectRelative [0] (SelectAND SelectDescendents (SelectByType UIAction))) [1,0]) 
	sut
		//Initial, followed by an event in the new structure
		# (_,s) = sutLayout.Layout.adjust (ReplaceUI initUI,initState)
		# (c,s) = sutLayout.Layout.adjust (changeToReRoute,s)
		= (c,s)

	exp = (expChange,expState)

	//Initial UI	
	initUI = uic UIPanel [uic UIContainer [ui UIAction, ui UIEmpty, ui UIAction], ui UIContainer]
	initState = JSONNull

	changeToReRoute = ChangeUI [] [(0,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	//Expected reroute change 
	expChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "foo" (JSONString "bar")] []))]))]

	expState = toJSON [(0,ChildBranchesMoved [(0,BranchMoved),(2,BranchMoved)])]

	isAction (UI type _ _) = type =: UIAction


testSequenceLayouts = skip (fail "Test sequencing multiple layouts")
testSelectLayout = skip (fail "Test selecting of a layout")

//Tests for the common layout patterns

testMoveTaskToWindow = skip (fail "Moving a task UI to a separate window")

::TestRecOuter =
	{ a :: TestRecInner
	, b :: TestRecInner
	}
:: TestRecInner =
	{ c :: String
	, d :: String
	}
derive class iTask TestRecInner, TestRecOuter

testAutoInteractionLayoutInitial = skip (assertEqual "Test if the auto interaction layout correctly turns an editor into a form" exp sut)
where
	exp = ReplaceUI (uic UIContainer [stdPrompt,expIntForm])
	sut = fst (finalizeInteract.Layout.adjust ((ReplaceUI (uic UIContainer [stdPrompt,stdIntEditor])),JSONNull))
	
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

testAutoInteractionLayoutEditorValueChange = skip (fail "Test if the auto interaction layout correctly maps changes in the editor to the form item")

testFlatteningOfNestedRecords = skip (assertEqualWorld "Auto interact layout should flatten a nested-record structure" exp sut)
where
	//We expect a change to the control with index 3, because the autoAccuInteract flattens the form
	exp = (Ok (ChangeUI [] [(3, ChangeChild (ChangeUI [SetAttribute "setEditorValue" (JSONString "bax")] []))],newFieldMask)
          , { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}})

	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,val,vst) = gEditor{|*|}.Editor.onRefresh [] { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}} {a={c="foo",d="foo"}, b = { c = "bar", d = "bax"}} newFieldMask vst
		# world = fromStubIWorld (fromStubVSt vst)
		= ((res,val),world)


testCombination1 = assertEqual "Complex combination layout with insert events" exp sut
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
			,insertSubUI [0] (ui UIComponent) //Make sure we have a target for the move
			,moveSubUIs (SelectByPath [1,0]) [0,0] //Key difference
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
	initState = JSONNull

	//First rendering
	renderedUI = uic UIDebug [uic UIContainer [], uic UIDebug [uic UIMenu [], uic UIPanel []] ]
	renderedState = JSONArray [JSONArray [sSideBar,sSub1], sRmInteract]
	where
		sSideBar = JSONArray [JSONNull,toJSON (BranchLayout JSONNull),toJSON moves, toJSON (ChildBranchLayout [(0,BranchLayout (JSONBool False))])]
		where
			moves = [(1,ChildBranchesMoved [(0,BranchMoved)])]

		sSub1 = toJSON (ChildBranchLayout [(1,BranchLayout sSideBar)])

		sRmInteract = toJSON [(0,ChildBranchesMoved [(0,BranchMoved)]),(1,ChildBranchesMoved [(0,ChildBranchesMoved [(0,BranchMoved)])])]

	//Expected reroute change 
	changeToModify = ChangeUI [] [(2,InsertChild (uic UIToolBar [ui UIInteract]))]
	expModifiedChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar []))]))]))]

testCombination2 = assertEqual "Insert after global removal" exp sut
where
	sut
		//Initial, followed by an event in the new structure
		# (c2,s2) = sutLayout.Layout.adjust (changeToModify,initState)
		= c2
	exp = expModifiedChange

	sutLayout = removeSubUIs (SelectAND SelectDescendents (SelectByType UIInteract))

	initState = toJSON [(0,ChildBranchesMoved [(0,BranchMoved)])
					   ,(1,ChildBranchesMoved [(0,ChildBranchesMoved [(0,BranchMoved)])])]

	//Change after first transform 
	changeToModify = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar [ui UIInteract]))]))]))]
	expModifiedChange = ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (uic UIToolBar []))]))]))]


testDynamicBehaviour1 = assertEqual "Dynamic (late) activation of layouts" exp sut
where
	sutLayout = layoutSubUIs (SelectByHasAttribute "x") markFirstChild
	where
		markFirstChild = layoutSubUIs (SelectByPath [0]) (setUIAttributes ('DM'.fromList [("y",JSONBool True)]))
	
	sut
		# (c1,s1) = sutLayout.Layout.adjust (change1,JSONNull)
		# (c2,s2) = sutLayout.Layout.adjust (change2,JSONNull)
		= c2

	initState = JSONNull

	//Begin with a UI without attribute "x" at the root
	change1 = ReplaceUI (uic UIContainer [ui UIContainer])
	//Now set the attribute, this means the layout should now match the root node
	change2 = ChangeUI [SetAttribute "x" (JSONBool True)] []

	exp = ChangeUI [SetAttribute "x" (JSONBool True)] [(0,ChangeChild (ChangeUI [SetAttribute "y" (JSONBool True)] []))]


