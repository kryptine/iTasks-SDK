implementation module Tests.Interactive.Layout
import TestFramework

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
