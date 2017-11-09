implementation module Tests.Interactive.CoreTasks
import iTasks, iTasks.Internal.Test.Definition, Data.Error

testCoreTasksI :: TestSuite
testCoreTasksI = testsuite "Core tasks" "These tests check if the core tasks work"
	[testCallProcess
	]

testCallProcess = itest "Call process test" "Press the button" "You should get to see the result of the `date` executable" sut
where
	sut = viewInformation "Press the button to run an OS process" [] ()
		>>| withShared []
		    \io -> (externalProcess "/bin/date" [] Nothing io False {onStartup=onStartup,onOutData=onOutData,onErrData=onErrData,onShareChange=onShareChange,onExit=onExit}
					-|| viewSharedInformation "OUTPUT: " [] io
					)

	onStartup r = (Ok r, Nothing, [], False)

	onOutData data l r  = (Ok [data:l], Just [data:r], [], False)
	onErrData _ l r = (Ok l, Nothing, [], False)
	onShareChange l r = (Ok l, Nothing, [], False)

	onExit c l r = (Ok l, Nothing)
