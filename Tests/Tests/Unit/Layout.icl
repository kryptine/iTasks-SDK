implementation module Tests.Unit.Layout
import TestFramework
import Tests.Unit.FrameworkStubs

import iTasks.UI.Editor, iTasks.UI.Diff
import iTasks._Framework.Generic.Interaction

import iTasks.UI.Editor, iTasks.UI.Diff, iTasks.UI.Layout
import iTasks._Framework.Generic.Interaction
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import StdMisc


testLayout :: TestSuite
testLayout = testsuite "Layout" "Tests for the layout functions"
	[testAutoInteractionLayoutInitial
	,testAutoInteractionLayoutEditorValueChange
	,testMoveTaskToWindow
	,testFlatteningOfNestedRecords]

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
	exp = ReplaceUI (UICompoundContent [stdPrompt,expIntForm])
	sut = fst (autoLayoutInteract ((ReplaceUI (UICompoundContent [stdPrompt,stdIntEditor])),()))
	
	stdPrompt = UIEmpty //STUB Don't care what the prompt is!
	stdIntEditor = UIEditor {UIEditor|optional=False,attributes='DM'.fromList
		[("hint-type","info"),("hint","Please enter a whole number (this value is required)")]}
			intControl

	expIntForm = UIForm [UIFormItem UIEmpty (UIControl intControl) (UIControl expIcon)]
	expIcon = UIIcon {UIFSizeOpts|margins=Just{top=0,right=0,bottom=0,left=5}} {UIIconOpts|iconCls="icon-info",tooltip = Just "Please enter a whole number (this value is required)"}

	intControl = UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
                   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing}

testAutoInteractionLayoutEditorValueChange = skip "Test if the auto interaction layout correctly maps changes in the editor to the form item"

testFlatteningOfNestedRecords = skip "Auto interact layout should flatten a nested-record structure"
	//= assertEqualWorld "Auto interact layout should flatten a nested-record structure" exp sut
where
	//We expect a change to the control with index 3, because the autoAccuInteract flattens the form
	exp = ChangeUI [] [ChangeChild 3 (ChangeUI [("setEditorValue", [JSONString "bax"])] [])] 

	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.genDiff [] { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}} Untouched {a={c="foo",d="foo"}, b = { c = "bar", d = "bax"}} Untouched vst
		# world = fromStubIWorld (fromStubVSt vst)
		//Apply the autoAccuInteract layout to the diff
		# res = autoLayoutInteract res
		= (res,world)

