implementation module Tests.Interactive.CustomEditors

import iTasks, iTasks.Internal.Test.Definition
import iTasks.UI.Definition, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import iTasks.Extensions.DateTime
import qualified Data.Map as DM

testCustomEditors :: TestSuite
testCustomEditors = testsuite "Custom editors" "These tests check if customization of editors works"
	[testColoredTextField
	,testLabeledTextField
	,testCombinedTextFields
	,testMixedCombinedTextFields
	,testDateField
	]

testColoredTextField = itest "Colored text field" "Check if the textfield is pink" "You should be able to edit" tut
where
    tut :: Task String
    tut = testEditor (withAttributes (styleAttr "background-color: pink") textField) "Hello world" Update

testLabeledTextField = itest "Labeled text field" "Check if the textfield has label foo" "You should be able to edit" tut
where
    tut :: Task String
    tut = testEditor (withLabelAttr "Foo" textField) "Hello world" Update

testCombinedTextFields = itest "Combined text fields" "Check if both text fields work" "You should be able to edit" tut
where
    tut :: Task (String,String)
    tut = testEditor (container2 textField textField) ("Hello","world") Update

testMixedCombinedTextFields = itest "Mixed combined text fields" "Check if both text fields work" "You should be able to edit" tut
where
    tut :: Task (String,String)
    tut = testEditor editor ("Hello","world") Update
	
	editor = container2 username password
	username = pink (withLabelAttr "Username" textField)
	password = pink (withLabelAttr "Password" passwordField)
	pink e = withAttributes (styleAttr "background-color: pink") e

testDateField = itest "Date field" "Check if the date editor checks the format" "You should be able to edit" tut
where
    tut :: Task Date
    tut = testEditor gEditor{|*|} {Date|year=2003,mon=1,day=13} Update

