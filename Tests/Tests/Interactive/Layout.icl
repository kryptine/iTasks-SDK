implementation module Tests.Interactive.Layout
import iTasks.Internal.Test.Definition
import iTasks.UI.Definition, iTasks.UI.Layout
import qualified Data.Map as DM
import Data.Maybe

testLayoutI :: TestSuite
testLayoutI = testsuite "Layout" "Test for layout functions" 
	[testWindow,testForeverLoop,testNestedSteps]

testWindow = itest "Window test" "Press the button" "A window should open" sut
where
	sut = viewInformation "Press the button to open a window" [] ()
		>>| taskInWindow
		>>| viewInformation "Done" [] ()

	taskInWindow = (viewInformation (Title "Test window") [] "Hello!" >>* [OnAction ActionClose (always (return ()))]) <<@ InWindow

testForeverLoop = itest "Forever loop" "Keep pressing continue" "You should be alternating between two diffent texts" sut
where
	sut = forever (
				viewInformation () [] "From one screen..." 
			>>| viewInformation () [] "To the next..."
			>>| return ()
		)

testNestedSteps = itest "Nested steps" "Test nested steps" "You sheed be able to click to three screens by pressing continue (you should see only one continue button)" sut
where
	sut =   viewInformation () [] "Step 1"
		>>| viewInformation () [] "Step 2"
		>>| viewInformation () [] "Step 3"
		>>| viewInformation () [] "Step 4"

layoutTestTasks :: [PublishedTask]
layoutTestTasks =
	[publish "/layout-tests/set-type" (const testSetTypeTask)
	,publish "/layout-tests/set-attributes" (const testSetAttributesTask)
	,publish "/layout-tests/del-attributes" (const testDelAttributesTask)
	,publish "/layout-tests/modify-attributes" (const testModifyAttributesTask)
	,publish "/layout-tests/copy-attributes" (const testCopySubAttributesTask)
	,publish "/layout-tests/wrap" (const testWrapTask)
	,publish "/layout-tests/unwrap" (const testUnwrapTask)
	,publish "/layout-tests/insert-sub" (const testInsertSubTask)
	,publish "/layout-tests/remove-subs" (const testRemoveSubsTask)
	,publish "/layout-tests/move-subs" (const testMoveSubsTask)
	,publish "/layout-tests/layout-subs" (const testLayoutSubsTask)
	,publish "/layout-tests/sequence-layouts" (const testSequenceLayoutsTask)
	]

taskToLayout title = updateInformation () [] title @! () >>= return

testSetTypeTask :: Task ()
testSetTypeTask = taskToLayout "Test for setting a UI type" <<@ ApplyLayout layout
where
	layout = setUIType UIDebug

testSetAttributesTask :: Task ()
testSetAttributesTask = taskToLayout "Test for setting an attribute" <<@ ApplyLayout layout
where
	layout = setUIAttributes (styleAttr "background: #f0f")

testDelAttributesTask :: Task ()
testDelAttributesTask = taskToLayout "Test for deleting an attribute" <<@ ApplyLayout layout
where
	layout = delUIAttributes (SelectKeys ["direction"])

testModifyAttributesTask :: Task ()
testModifyAttributesTask = taskToLayout "Test for modifying attributes" <<@ ApplyLayout layout
where
	layout = modifyUIAttributes (SelectKeys ["direction"]) f 
	f attr = maybe 'DM'.newMap (\(JSONString dir) -> optionalAttr (dir == "horizontal")) ('DM'.get "direction" attr)

testCopySubAttributesTask :: Task ()
testCopySubAttributesTask = taskToLayout "Test for copying an attribute" <<@ ApplyLayout layout
where
	layout = copySubUIAttributes SelectAll [0] [1]

testWrapTask :: Task ()
testWrapTask = taskToLayout "Test for wrapping a ui" <<@ ApplyLayout layout
where
	layout = wrapUI UIDebug

testUnwrapTask :: Task ()
testUnwrapTask = taskToLayout "Test for unwrapping a ui" <<@ ApplyLayout layout
where
	layout = unwrapUI

testInsertSubTask :: Task ()
testInsertSubTask = taskToLayout "Test for inserting a sub ui" <<@ ApplyLayout layout
where
	layout = insertChildUI 1 (ui UIDebug)

testRemoveSubsTask :: Task ()
testRemoveSubsTask = taskToLayout "Test for removing a sub ui" <<@ ApplyLayout layout
where
	layout = removeSubUIs (SelectByPath [1])

testMoveSubsTask :: Task ()
testMoveSubsTask = taskToLayout "Test for moving a sub ui" <<@ ApplyLayout layout
where
	layout = moveSubUIs (SelectByPath [1]) [0] 0

testLayoutSubsTask :: Task ()
testLayoutSubsTask = taskToLayout "Test for layouting a sub ui" <<@ ApplyLayout layout
where
	layout = layoutSubUIs (SelectByPath [0]) (setUIType UIDebug)

testSequenceLayoutsTask :: Task ()
testSequenceLayoutsTask = taskToLayout "Test for sequencing layouts" <<@ ApplyLayout layout
where
	layout = sequenceLayouts (setUIType UIStep) (setUIAttributes (styleAttr "background: #ff0"))
