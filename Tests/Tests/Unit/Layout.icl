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
	[testMoveTaskToWindow,testFlatteningOfNestedRecords]

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

testFlatteningOfNestedRecords = assertEqualWorld "Auto interact layout should flatten a nested-record structure" exp sut
where
	//We expect a change to the control with index 3, because the autoAccuInteract flattens the form
	exp = ChangeUI [] [ChangeChild 3 (ChangeUI [("setEditorValue", [JSONString "bax"])] [])] 

	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.genDiff [] { a = {c="foo",d="foo"}, b = { c = "bar", d = "baz"}} {a={c="foo",d="foo"}, b = { c = "bar", d = "bax"}} vst
		# world = fromStubIWorld (fromStubVSt vst)
		//Apply the autoAccuInteract layout to the diff
		# res = autoAccuInteract.ContentLayout.route res
		= (res,world)

