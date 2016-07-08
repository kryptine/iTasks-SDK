implementation module Tests.Interactive.BuiltinEditors

import iTasks, TestFramework
import iTasks.UI.Editor.Builtin, iTasks.UI.Definition
import qualified Data.Map as DM
import Text.HTML

derive class iTask ChoiceGrid, ChoiceNode

testBuiltinEditors :: TestSuite
testBuiltinEditors = testsuite "Builtin editors" "These tests check if the builtin editors work"
	[testTextField,testTextArea,testPasswordField
	,testIntegerField,testDecimalField,testDocumentField
	,testCheckbox,testSlider,testButton,testLabel,testIcon
	,testTextView,testHtmlView,testProgressBar
	,testDropdown,testCheckGroup,testChoiceList,testGrid,testTree
	]

testTextField = itest "Text field" "Check if the textfield is ok" "You should be able to edit" tut
where
	tut :: Task String
	tut = testEditor (textField 'DM'.newMap) "Hello world" Update

testTextArea = itest "Text area" "Check if the textarea is ok" "You should be able to edit" tut
where
	tut :: Task String
	tut = testEditor (textArea 'DM'.newMap) "Hello world" Update

testPasswordField = itest "Password field" "Check if the password field is ok" "You should be able to edit" tut
where
	tut :: Task String
	tut = testEditor (passwordField 'DM'.newMap) "Hello world" Update

testIntegerField = itest "Integer field" "Check if the integer field is ok" "You should be able to edit" tut
where
	tut :: Task Int
	tut = testEditor (integerField 'DM'.newMap) 42 Update

testDecimalField = itest "Decimal field" "Check if the decimal field is ok" "You should be able to edit" tut
where
	tut :: Task Real
	tut = testEditor (decimalField 'DM'.newMap) 3.14 Update

testDocumentField = itest "Document field" "Check if the decimal field is ok" "You should be able to edit" tut
where
	tut :: Task (!String,!String,!String,!String,!Int)
	tut = testEditor (documentField 'DM'.newMap) defaultValue Enter

testCheckbox = itest "Checkbox" "Check if the checkbox is ok" "You should be able to edit" tut
where
	tut :: Task Bool
	tut = testEditor (checkBox 'DM'.newMap) False Update

testSlider = itest "Slider" "Check if the slider is ok" "You should be able to edit" tut
where
	tut :: Task Int
	tut = testEditor (slider ('DM'.union (minAttr 1) (maxAttr 5))) 3 Update

testButton = itest "Button" "Check if the button is ok" "You should be able to edit" tut
where
	tut :: Task Bool
	tut = testEditor (button (textAttr "Click")) False Update

testLabel = itest "Label" "Check if the label looks ok" "You cannot edit a label" tut
where
	tut :: Task String
	tut = testEditor (label 'DM'.newMap) "Hello world" Update

testIcon = itest "Icon" "Check if the icon looks ok" "You cannot edit an icon" tut
where
	tut :: Task (String,Maybe String)
	tut = testEditor (icon 'DM'.newMap) ("icon-valid",Just "Icon with a tooltip!") Update

testTextView = itest "Text view" "Check if text looks ok" "You cannot edit a text view" tut
where
	tut :: Task String
	tut = testEditor (textView 'DM'.newMap) "Hello World" Update

testHtmlView = itest "Html view" "Check if html looks ok" "You cannot edit an html view" tut
where
	tut :: Task HtmlTag
	tut = testEditor (htmlView 'DM'.newMap) (H2Tag [] [Text "Hello World"]) Update

testProgressBar = itest "Progress bar" "Check if hte progress bar looks ok" "You cannot edit the progress" tut
where
	tut :: Task (Maybe Int,Maybe String)
	tut = testEditor (progressBar 'DM'.newMap) (Just 90,Just "Almost done") Update

testDropdown = itest "Dropdown" "Check if the dropdown works" "You should be able to edit" tut
where
	tut :: Task ([String],[Int])
	tut = testEditor (dropdown (multipleAttr False)) (["A","B","C"],[]) Update

testCheckGroup = itest "Check group" "Check if the checkgroup works" "You should be able to edit" tut
where
	tut :: Task ([String],[Int])
	tut = testEditor (checkGroup (multipleAttr False)) (["A","B","C"],[]) Update

testChoiceList = itest "Choice list" "Check if the choice list works" "You should be able to edit" tut
where
	tut :: Task ([String],[Int])
	tut = testEditor (choiceList (multipleAttr False)) (["A","B","C"],[]) Update

testGrid = itest "Grid" "Check if the grid works" "You should be able to edit" tut
where
	tut :: Task (ChoiceGrid,[Int])
	tut = testEditor (grid (multipleAttr False)) ({ChoiceGrid|header=["Key","Value"],rows=[[Text "A",Text "1"],[Text "B",Text "2"],[Text "C",Text "3"]]},[]) Update

testTree = itest "Tree" "Check if the tree works" "You should be able to edit" tut
where
	tut :: Task ([ChoiceNode],[Int])
	tut = testEditor (tree (multipleAttr False))
		([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[]) Update
