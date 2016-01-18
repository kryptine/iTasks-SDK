implementation module Tests.Interactive.Layout
import TestFramework

testLayout :: TestSuite
testLayout = testsuite "Layout" "Test for layout functions" 
	[testWindow,testForeverLoop]

testWindow = itest "Window test" "Press the button" "A window should open" sut
where
	sut = viewInformation "Press the button to open a window" [] ()
		>^* [OnAction ActionNext (always taskInWindow)]

	taskInWindow = (viewInformation (Title "Test window") [] "Hello!" >>* [OnAction ActionClose (always (return ()))]) <<@ InWindow

testForeverLoop = itest "Forever loop" "Keep pressing continue" "You should be alternating between two diffent texts" sut
where
	sut = forever (
				viewInformation () [] "From one screen..." 
			>>| viewInformation () [] "To the next..."
			>>| return ()
		)

