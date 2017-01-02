implementation module Tests.Interactive.CoreTasks
import iTasks, TestFramework

testCoreTasksI :: TestSuite
testCoreTasksI = testsuite "Core tasks" "These tests check if the core tasks work"
	[testCallProcess
	]

testCallProcess = itest "Call process test" "Press the button" "You should get to see the result of the `date` executable" sut
where
	sut = viewInformation "Press the button to run an OS process" [] ()
		>>| callProcess "Run slow project" [] "/bin/date" [] Nothing
