implementation module Tests.Unit.Layout
import TestFramework

testLayout :: TestSuite
testLayout = testsuite "Layout" "Tests for the layout functions"
	[testMoveTaskToWindow]

testMoveTaskToWindow = skip "Moving a task UI to a separate window"
