implementation module Tests.Interactive.BuiltinContainers
/**
* Tests for the builtin containers in the client library
*/
import iTasks, TestFramework
import iTasks.UI.Definition, iTasks.UI.Layout, iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators
import qualified Data.Map as DM

testBuiltinContainers :: TestSuite
testBuiltinContainers = testsuite "Builtin Containers" "These tests let you check the containers that are used in layouts"
	[testPanel
	,testTabSet
	,testWindow
	,testMenu
	,testToolBar
	,testButtonBar
	,testDebug
	]

content :: Task String
content = viewInformation () [] "This is the content of the container"

buttons :: Task (Bool,Bool)
buttons = enterInformation () [EnterUsing id editor] <<@ ApplyLayout (sequenceLayouts [removeSubAt [0],unwrapUI,setAttributes (textAttr "Sub menu")])
where
	editor = composeEditors UIMenu (button ('DM'.unions[textAttr "Button a",iconClsAttr "icon-ok"])) (button (textAttr "Button b")) 

testPanel = itest "Panel" "Check if the panel looks ok" "You cannot do anything with a panel" tut
where
	tut = content <<@ ApplyLayout (wrapUI UIPanel) <<@ ApplyLayout (setAttributes (titleAttr "Panel with title"))

testTabSet = itest "Tab set" "Check if the tab panel looks ok" "There is only one tab" tut
where
	tut = content <<@ ApplyLayout (setAttributes (titleAttr "Tab title")) <<@ ApplyLayout (wrapUI UITabSet)

testWindow = itest "Window" "Check if the window looks ok" "It should be a basic floating window" tut
where
	tut = content <<@ ApplyLayout (wrapUI UIWindow) <<@ ApplyLayout (setAttributes (titleAttr "Window with title"))

testMenu = itest "Menu" "Check if the menu looks ok" "It should be a top-level menu that can expand on clicking" tut
where
	tut = buttons <<@ ApplyLayout (wrapUI UIMenu) <<@ ApplyLayout (setAttributes (textAttr "Open menu"))

testToolBar = itest "Tool bar" "Check if the tool bar looks ok" "You cannot do anything with a tool bar" tut
where
	tut = content <<@ ApplyLayout (wrapUI UIToolBar) 

testButtonBar = itest "Button bar" "Check if the button bar looks ok" "You cannot do anything with a button bar" tut
where
	tut = content <<@ ApplyLayout (wrapUI UIButtonBar) 

testDebug = itest "Debug wrapper" "Check if the debug wrapper looks ok" "You cannot do anything with the debug wrapper" tut
where
	tut = content <<@ ApplyLayout (wrapUI UIDebug) 

