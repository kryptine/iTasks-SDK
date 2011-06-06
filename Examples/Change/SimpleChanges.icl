implementation module SimpleChanges

import iTasks

changeExamples :: [Workflow]
changeExamples =
	[ 	workflow "Examples/Changes/Change priority" "Change the priority of a task"  (try changePrio catch)
	,	workflow "Examples/Changes/Add warning" "Add a warning message to a task"  (try changeWarningTask catch)
	,	workflow "Examples/Changes/Duplicate task" "Duplicate a task" (try duplicateTask catch)
	,	workflow "Examples/Changes/Show result" "Show result when task finishes"  (try informTask catch)
	,	workflow "Examples/Changes/Check task when finished" "Wait until a task is finished"  (try checkTask catch)
	,	workflow "Examples/Changes/Cancel task" "Cancel a task" (try cancelTask catch)
 	,	workflow "Examples/Changes/Reassign task" "Reassing the task to another user"  (try reassignTask catch)
 	,	workflow "Examples/Changes/Restart task" "Restart a task from the beginning"  (try restartTask catch)
  	]
where
	catch :: String -> Task Void
	catch message  = showInformation ("Error!",message) [] Void

//Simple change which will run once and change the priority of all tasks to high
changePriority :: TaskPriority -> ChangeDyn
changePriority priority =
	dynamic change :: A.a: Change a | iTask a
where
	change :: ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Just {ProcessProperties|props & managerProperties = {props.ProcessProperties.managerProperties & priority = priority}},Nothing, Just (changePriority priority))

//Add a big red warning message prompt to the running task
addWarning :: String -> ChangeDyn
addWarning msg = 
	dynamic change  :: A.a: Change a | iTask a
where
	change :: ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Nothing, Just (((showInformation ("Warning!",redText msg) [] Void >>+ noActions) ||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: User User String -> ChangeDyn
duplicate me user topics =
	dynamic change me user topics :: A.a: Change a | iTask a
where
	change :: User User String ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change me user topics props t t0 
		= 	( Just {ProcessProperties|props & managerProperties = {props.ProcessProperties.managerProperties & worker = me}}
			, Just (me @:
							(anyTask 	[ (Title topics @>> props.ProcessProperties.managerProperties.ManagerProperties.worker @: t) 
										, (Title topics @>> user @: t)
										]
							)
							<<@ Title ("Duplicated " +++ topics))
			, Nothing )

//inform will inform a user that some process has ended.
inform :: User String -> ChangeDyn
inform user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> spawnProcess True {ManagerProperties|initManagerProperties & worker = user} (showInformation ("Process ended","Process " +++ procName +++ " ended!") [Get id] res) >>| return res), Nothing)

//check will pass the result to the indicated user who can change the result in an editor before it passed.
check :: User String -> ChangeDyn
check user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> assign {worker = user, priority = HighPriority, deadline = Nothing, status = Active} (updateInformation ("Verification","Please verify result of " +++ procName) [] res)), Nothing)

//cancel stop the process, and give the indicated user the responsibility to fill in the result
cancel ::  String ProcessId -> ChangeDyn
cancel  procName pid  =
	dynamic change  :: A.b: Change b | iTask b
where
	change p  t t0 = (Nothing, Just (		deleteProcess pid 
										>>| 		return defaultValue
										), Nothing)

//reassign the work to someone else
reassign :: User String ProcessId -> ChangeDyn
reassign user procName pid  =
	dynamic change user :: A.b: Change b | iTask b
where
	change :: User ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 
		= (Just {ProcessProperties|props & managerProperties = {props.ProcessProperties.managerProperties & worker = user}},Nothing, Nothing)

//restart starts the task from scratch and assigns it to the indicated user
restart :: User String -> Dynamic
restart user procName =
	dynamic change user procName :: A.a: Change a | iTask a
where
	change :: User String ProcessProperties (Task a) (Task a) -> (Maybe ProcessProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user procName props t t0 = (Nothing, Just (assign {worker = user, priority = HighPriority, deadline = Nothing, status = Active} (Title procName @>> t0)), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	enterInformation ("New priority","What should the new priority be?") []
	>>= \priority ->applyChangeToProcess proc (changePriority priority) (CLPersistent "priority")

changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation ("Warning","Type in warning you want to show to all:") []
	>>= \warning ->	chooseProcess "Which process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "Which process do you want to duplicate?"
	>>= \procId ->	getProcess procId
	>>= \process ->	chooseUserA "Select the user you want to work on it as well:"
	>>= \user ->	get currentUser
	>>= \me ->		applyChangeToProcess procId (duplicate me user (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title) CLTransient

informTask :: Task Void
informTask
	=				chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	chooseUserA "Select the user you want this result to see:"
	>>= \user ->	getProcess procId
	>>= \process ->applyChangeToProcess procId (inform user (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title) CLTransient
	
checkTask :: Task Void
checkTask
	=				chooseProcess "The result of which process do you want to be checked?"
	>>= \procId ->	getProcess procId
	>>= \process -> chooseUserA "Select the user which has to check it:"
	>>= \user ->	applyChangeToProcess procId (check user (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title) CLTransient
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select the task you want to cancel:"
	>>= \procId ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title procId) CLTransient

reassignTask :: Task Void
reassignTask
	=				chooseProcess "Select the task you want to reassign to someone else:"
	>>= \procId ->	chooseUserA "Who should continue with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (reassign user (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title procId) CLTransient

restartTask :: Task Void
restartTask
	=				chooseProcess "Select the task you want to restart from scratch:"
	>>= \procId ->	chooseUserA "Who should start with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (restart user (fromJust process).Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title) CLTransient

//Utility
chooseUserA :: !question -> Task User | html question
chooseUserA question
	=		enterSharedChoice ("Choose user",question) [] users
		>?*	[ (ActionCancel,	Always	(throw "choosing a user has been cancelled"))
			, (ActionOk,		IfValid	(\user -> return user))
			]							

chooseProcess :: String -> Task ProcessId
chooseProcess question
	=				getProcessesWithStatus [Running] [Active]
	>>= \procs ->	enterChoice question []
					[	( proc.Process.properties.ProcessProperties.systemProperties.SystemProperties.taskId
						, proc.Process.properties.ProcessProperties.taskProperties.taskDescription.TaskDescription.title
						, proc.Process.properties.ProcessProperties.managerProperties.ManagerProperties.priority
						, proc.Process.properties.ProcessProperties.managerProperties.ManagerProperties.worker)
						\\ proc <- procs]
	>?*				[ (ActionCancel,	Always	(throw "choosing a process has been cancelled"))
					, (ActionOk,		IfValid	(\(pid,_,_,_) -> return (toInt pid)))
					]

	
	
	