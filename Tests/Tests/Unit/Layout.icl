implementation module Tests.Unit.Layout
import TestFramework
import Tests.Unit.FrameworkStubs

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks._Framework.Generic.Interaction
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import StdMisc


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
	,testLayoutSubsMatching
	,testMoveSubsMatching
	,testSequenceLayouts
	,testSelectLayout
	//Common patterns
	,testAutoInteractionLayoutInitial
	,testAutoInteractionLayoutEditorValueChange
	,testMoveTaskToWindow
	,testFlatteningOfNestedRecords]

//Tests for the core operations of the layout library
testChangeNodeType = skip "Changing node type"
testChangeAttributes = skip "Changing attributes"

testWrap = skip "Wrapping a UI in a panel"
testUnwrap = skip "Unwrapping a UI from a container"

testInsertSubAt = skip "Inserting a sub-UI at a specific path"
testRemoveSubAt = skip "Remove a sub-UI from a specific path"
testLayoutSubAt = skip "Applying another layout at a specific path (by setting attribute)"
testMoveSubAt = skip "Moving a node from one place to another"

testRemoveSubsMatchingOnReplace = assertEqual "Removing everything that matches, when replacing a UI" exp sut
where
	exp = (ReplaceUI expUI,expState)
	expUI = uic UIInteract [ui UIForm]
	expState = JSONArray [JSONString "NR",JSONArray [],JSONArray [JSONArray 
							[JSONInt 0,JSONArray [JSONString "NR",JSONArray [JSONArray [JSONInt 0,JSONArray[JSONString "Nothing"]]],JSONArray []]]]]
	sut = (removeSubsMatching [] isEmptyNode) (ReplaceUI initUI,JSONNull)
	initUI = uic UIInteract [uic UIForm [ui UIEmpty]]

isEmptyNode (UI UIEmpty _ _) = True
isEmptyNode _ = False

testRemoveSubsMatchingOnChildChange = assertEqual "Removing everything that matches, when changing a child" exp sut
where
	exp = (ChangeUI [] [(0,ChangeChild (ChangeUI [] []))],expState) //The change is removed because it applies to a removed branch
	expState = sutState //No change in the state, because we are not adding or removing anything

	sut = (removeSubsMatching [] isEmptyNode)  (ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,ChangeChild NoChange)]))],sutState)
	//Initial state: A UI in which the node at 0/0 was removed
	sutState = JSONArray [JSONString "NR",JSONArray [],JSONArray [JSONArray 
							[JSONInt 0,JSONArray [JSONString "NR",JSONArray [JSONArray [JSONInt 0,JSONArray[JSONString "Nothing"]]],JSONArray []]]]]

testLayoutSubsMatching = skip "Applying another layout to all matching nodes"
testMoveSubsMatching = skip "Moving all matching nodes"

testSequenceLayouts = skip "Test sequencing multiple layouts"
testSelectLayout = skip "Test selecting of a layout"

//Tests for the common layout patterns

testMoveTaskToWindow = skip "Moving a task UI to a separate window"

::TestRecOuter =
	{ a :: TestRecInner
	, b :: TestRecInner
	}
:: TestRecInner =
	{ c :: String
	, d :: String
	}
derive class iTask TestRecInner, TestRecOuter

testAutoInteractionLayoutInitial = assertEqual "Test if the auto interaction layout correctly turns an editor into a form" exp sut
where
	exp = ReplaceUI (uic UICompoundContent [stdPrompt,expIntForm])
	sut = fst (finalizeInteract ((ReplaceUI (uic UICompoundContent [stdPrompt,stdIntEditor])),JSONNull))
	
	stdPrompt = ui UIEmpty //STUB Don't care what the prompt is!
	stdIntEditor = uia UIEditInt
		('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "info"),("hint",JSONString"Please enter a whole number (this value is required)")
						,("taskId",JSONString "STUB"),("editorId",JSONString "v")])
			

	expIntForm = uic UIForm [uic UIFormItem [ui UIEmpty,intControl,expIcon]]
	expIcon = uia UIIcon ('DM'.fromList [("margins",JSONString "0 0 0 5"),("iconCls",JSONString "icon-info")
										,("tooltip",JSONString "Please enter a whole number (this value is required)")])

	intControl = uia UIEditInt 
		('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "info"),("hint",JSONString"Please enter a whole number (this value is required)")
						,("taskId",JSONString "STUB"),("editorId",JSONString "v")])

testAutoInteractionLayoutEditorValueChange = skip "Test if the auto interaction layout correctly maps changes in the editor to the form item"

testFlatteningOfNestedRecords = skip "Auto interact layout should flatten a nested-record structure"
	//= assertEqualWorld "Auto interact layout should flatten a nested-record structure" exp sut
where
	//We expect a change to the control with index 3, because the autoAccuInteract flattens the form
	exp = ChangeUI [] [(3, ChangeChild (ChangeUI [("setEditorValue", [JSONString "bax"])] []))] 

	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.Editor.updUI [] { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}} Untouched {a={c="foo",d="foo"}, b = { c = "bar", d = "bax"}} Untouched vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (res,world)

