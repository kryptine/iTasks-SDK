implementation module Tests.Interactive.BuiltinEditors

import iTasks, TestFramework
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common, iTasks.UI.Definition
import qualified Data.Map as DM
import Text.HTML

derive class iTask ChoiceText, ChoiceGrid, ChoiceRow, ChoiceNode

testBuiltinEditors :: TestSuite
testBuiltinEditors = testsuite "Builtin editors" "These tests check if the builtin editors work"
	[testTextField,testTextArea,testPasswordField
	,testIntegerField,testDecimalField,testDocumentField
	,testCheckbox,testSlider,testButton,testLabel,testIcon
	,testTextView,testHtmlView,testProgressBar
	,testDropdown,testCheckGroup,testChoiceList,testGrid,testTree
	,testCheckGroupMulti,testChoiceListMulti,testGridMulti,testTreeMulti
	,testList
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
	tut :: Task ([ChoiceText],[Int])
	tut = testEditor (dropdown (multipleAttr False)) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

testCheckGroup = itest "Check group" "Check if the checkgroup works" "You should be able to edit" tut
where
	tut :: Task ([ChoiceText],[Int])
	tut = testEditor (checkGroup (multipleAttr False)) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

testChoiceList = itest "Choice list" "Check if the choice list works" "You should be able to edit" tut
where
	tut :: Task ([ChoiceText],[Int])
	tut = testEditor (choiceList (multipleAttr False)) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

testGrid = itest "Grid" "Check if the grid works" "You should be able to edit" tut
where
	tut :: Task (ChoiceGrid,[Int])
	tut = testEditor (grid (multipleAttr False)) ({ChoiceGrid|header=["Key","Value"],rows=rows},[]) Update

	rows = [{ChoiceRow|id=1,cells=[Text "A",Text "1"]},{ChoiceRow|id=2,cells=[Text "B",Text "2"]},{ChoiceRow|id=3,cells=[Text "C",Text "3"]}]

testTree = itest "Tree" "Check if the tree works" "You should be able to edit" tut
where
	tut :: Task ([ChoiceNode],[Int])
	tut = testEditor (tree (multipleAttr False))
		([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[]) Update

testCheckGroupMulti = itest "Check group (multiple)" "Check if the checkgroup works" "You should be able to select multiple" tut
where
	tut :: Task ([ChoiceText],[Int])
	tut = testEditor (checkGroup (multipleAttr True)) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

testChoiceListMulti = itest "Choice list (multiple)" "Check if the choice list works" "You should be able to select multiple" tut
where
	tut :: Task ([ChoiceText],[Int])
	tut = testEditor (choiceList (multipleAttr True)) ([{ChoiceText|id=0,text="A"},{ChoiceText|id=1,text="B"},{ChoiceText|id=2,text="C"}],[]) Update

testGridMulti = itest "Grid (multiple)" "Check if the grid works" "You should be able to select multiple" tut
where
	tut :: Task (ChoiceGrid,[Int])
	tut = testEditor (grid (multipleAttr True)) ({ChoiceGrid|header=["Key","Value"],rows=rows},[]) Update

	rows = [{ChoiceRow|id=1,cells=[Text "A",Text "1"]},{ChoiceRow|id=2,cells=[Text "B",Text "2"]},{ChoiceRow|id=3,cells=[Text "C",Text "3"]}]

testTreeMulti = itest "Tree (multiple)" "Check if the tree works" "You should be able to select multiple" tut
where
	tut :: Task ([ChoiceNode],[Int])
	tut = testEditor (tree (multipleAttr True))
		([{ChoiceNode|id=1,label="A",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=2,label="B",icon=Nothing,expanded=False,children=[]}
        ,{ChoiceNode|id=3,label="C",icon=Nothing,expanded=False,children=[]}
        ],[]) Update

testBuiltinEditorsWithShares :: TestSuite
testBuiltinEditorsWithShares = testsuite "Builtin editors with shares" "These tests check if the builtin editors sync with shared data"
	[testGridShare]

testGridShare = itest "Grid (shared)" "Check if the grid works" "The grid should update if you change the model" tut
where
	tut :: Task (ChoiceGrid,[Int])
	tut = (testEditorWithShare (grid (multipleAttr True)) ({ChoiceGrid|header=["Key","Value"],rows=rows},[]) Update)

	rows = [{ChoiceRow|id=1,cells=[Text "A",Text "1"]},{ChoiceRow|id=2,cells=[Text "B",Text "2"]},{ChoiceRow|id=3,cells=[Text "C",Text "3"]}]


testList = itest "List of strings" "Check if the list editor works" "You should be able to edit a list of strings" tut
where
	tut :: Task [String]
	tut = testEditor (listEditor (Just (const "New item")) True True (Just (\items -> length items +++> " items")) (textField 'DM'.newMap)) [] Update

