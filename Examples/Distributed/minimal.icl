module minimal

import iTasks
import iTasks.Extensions.Distributed.API

import qualified Data.Map
import StdMisc, StdDebug

derive class iTask DummyTask

testDomain = Domain "localhost" 9090
testUser = AnonymousUser "D. Knuth"

Start iworld = doTasks task iworld
where
	task :: Task ()
	task = viewInformation "Choose role" [] "choose"
		>>* [ OnAction (Action "Domain server") (always server)
		    , OnAction (Action "Client") (always client)]

	server :: Task ()
	server = set (Just testDomain) domainName
		>>= \_. (viewStatus -&&- selectTask) @! ()

	selectTask = forever (enterChoiceWithShared "Select task" [] (sdsFocus testUser taskInstancesForUser)
		>>= \{TaskInstance|instanceNo}. workOn instanceNo @! ())

	viewStatus = (viewSharedInformation "Hosting domain" [] domainName
			-&&- viewSharedInformation "Tasks" [] currentProcesses)
			<<@ ArrangeHorizontal

	client :: Task ()
	client = (addTaskToDomain testDomain
		-&&- addTaskToUserDomain testDomain) @! ()

	addTaskToDomain :: Domain -> Task ()
	addTaskToDomain domain = forever (enterChoice "Choose a task to add in the domain" [] [T1, T2, T3]
		>>= \task. appendDomainTask (toTask task) domain
		>>= \taskId. viewInformation "Added task" [] taskId @! ())

	addTaskToUserDomain :: Domain -> Task ()
	addTaskToUserDomain domain = forever (enterChoice "Choose a task to add in the domain" [] [T1, T2, T3]
		>>= \task. appendDomainTaskForUser testUser (toTask task) domain
		>>= \taskId. viewTaskResult taskId domain (toTask task)
		>>* [OnAction ActionContinue (always (return ()))])

:: DummyTask = T1 | T2 | T3

toTask :: DummyTask -> Task Int
toTask t = case t of
	T1 = enterInformation "HUEHUE1" []
	T2 = enterInformation "HUEHUE2" []
	T3 = enterInformation "HUEHUE3" []

attributes title = 'Data.Map'.fromList
			[ ("title",      title)
			, ("createdBy",  "me")
			, ("createdAt",  "now")
			, ("priority",   toString 5)
			]