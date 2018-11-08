module minimal

import iTasks
import iTasks.Extensions.Distributed.API

Start iworld = doTasks task iworld
where
	task = enterInformation "Choose role" []
		>>* [ OnAction (Action "Domain server") (always (domainServer))
		    , OnAction (Action "Client") (always (client))]

	server = viewSharedInformation "Tasks in the domain" domainTasks
		-&&- viewSharedInformation "Users in the domain" userTasks
		-&&- viewSharedInformation "Hosting domain" domainName
		-&&- viewSharedInformation "Devices in domain" domainDevices
		<@@ ArrangeHorizontally

	client = enterInformation "Enter domain" []
		>>= \domain. enterChoice "Choose a task to add in the domain" [] [T1, T2, T3]
		>>= \task. case dummyTask of
			T1 = appendDomainTask
			T2 = appendDomainTask
			T3 = appendDomainTask

:: DummyTask = T1 | T2 | T3