implementation module Tests.Interactive.Layout
import TestFramework

testLayout :: TestSuite
testLayout = testsuite "Layout" "Test for layout functions" 
	[testWindow]

testWindow = skip "Window test" /*itest "Window test" "Press the button" "A window should open" sut
where
	sut = viewInformation "Press the button to open a window" [] ()
		>^* [OnAction ActionNext (always taskInWindow)]

	taskInWindow = (viewInformation (Title "Test window") [] "Hello!" >>* [OnAction ActionClose (always (return ()))]) <<@ InWindow
*/
