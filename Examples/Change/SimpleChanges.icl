implementation module SimpleChanges

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..),::WorkerProperties(..),::ManagerProperties(..), :: SystemProperties(..), :: TaskProgress, :: TaskParallelType(..)

changeExamples :: [Workflow]
changeExamples =
	[ 	workflow "Examples/Changes/Change priority" "Change the priority of a task" (Title "Change priority" @>> (try changePrio catch))
	,	workflow "Examples/Changes/Add warning" "Add a warning message to a task" (Title "Add warning" @>> (try changeWarningTask catch))
	,	workflow "Examples/Changes/Duplicate task" "Duplicate a task" (Title "Duplicate task" @>> (try duplicateTask catch))
	,	workflow "Examples/Changes/Show result" "Show result when task finishes" (Title "Show result when task finishes" @>> (try informTask catch))
	,	workflow "Examples/Changes/Check task when finished" "Wait until a task is finished" (Title "Check task when finished" @>> (try checkTask catch))
	,	workflow "Examples/Changes/Cancel task" "Cancel a task" (Title "Cancel task" @>> (try cancelTask catch))
 	,	workflow "Examples/Changes/Reassign task" "Reassing the task to another user" (Title "Reassign task" @>> (try reassignTask catch))
 	,	workflow "Examples/Changes/Restart task" "Restart a task from the beginning" (Title "Restart task" @>> (try restartTask catch))
  	]
where
	catch :: String -> Task Void
	catch message  = showMessage ("Error!",message) Void

//Simple change which will run once and change the priority of all tasks to high
changePriority :: TaskPriority -> ChangeDyn
changePriority priority =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Just {props & managerProperties = {props.managerProperties & priority = priority}},Nothing, Just (changePriority priority))

//Add a big red warning message prompt to the running task
addWarning :: String -> ChangeDyn
addWarning msg = 
	dynamic change  :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Nothing, Just (((getDefaultValue >>= showStickyMessage ("Warning!",redText msg)) -||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: User User String -> ChangeDyn
duplicate me user topics =
	dynamic change me user topics :: A.a: Change a | iTask a
where
	change :: User User String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change me user topics props t t0 
		= 	( Just {props & managerProperties = {props.managerProperties & worker = me}}
			, Just (assign me
							(anyProc 	[ props.managerProperties.ManagerProperties.worker @>> Title topics @>> t 
										, user @>> Title topics @>> t
										] Open
							)
							<<@ Title ("Duplicated " +++ topics))
			, Nothing )

//inform will inform a user that some process has ended.
inform :: User String -> ChangeDyn
inform user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> spawnProcess True True (user @>> showMessageAbout ("Process ended","Process " +++ procName +++ " ended!") res) >>| return res), Nothing)

//check will pass the result to the indicated user who can change the result in an editor before it passed.
check :: User String -> ChangeDyn
check user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> assign user (HighPriority @>> (updateInformation ("Verification","Please verify result of " +++ procName) res))), Nothing)

//cancel stop the process, and give the indicated user the responsibility to fill in the result
cancel ::  String ProcessId -> ChangeDyn
cancel  procName pid  =
	dynamic change  :: A.b: Change b | iTask b
where
	change p  t t0 = (Nothing, Just (		deleteProcess pid 
										>>| 		mkDefault t
										), Nothing)
	mkDefault :: (Task a) -> (Task a) | iTask a
	mkDefault _ = getDefaultValue

//reassign the work to someone else
reassign :: User String ProcessId -> ChangeDyn
reassign user procName pid  =
	dynamic change user :: A.b: Change b | iTask b
where
	change :: User TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 
		= (Just {props & managerProperties = {props.managerProperties & worker = user}},Nothing, Nothing)

//restart starts the task from scratch and assigns it to the indicated user
restart :: User String -> Dynamic
restart user procName =
	dynamic change user procName :: A.a: Change a | iTask a
where
	change :: User String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user procName props t t0 = (Nothing, Just (assign user (Title procName @>> HighPriority @>> t0 )), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	enterInformation ("New priority","What should the new priority be?")
	>>= \priority ->applyChangeToProcess proc (changePriority priority) (CLPersistent "priority")

changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation ("Warning","Type in warning you want to show to all:")
	>>= \warning ->	chooseProcess "Which process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "Which process do you want to duplicate?"
	>>= \procId ->	getProcess procId
	>>= \process ->	chooseUserA "Select the user you want to work on it as well:"
	>>= \user ->	getCurrentUser
	>>= \me ->		applyChangeToProcess procId (duplicate me user (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title) CLTransient

informTask :: Task Void
informTask
	=				chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	chooseUserA "Select the user you want this result to see:"
	>>= \user ->	getProcess procId
	>>= \process ->applyChangeToProcess procId (inform user (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title) CLTransient
	
checkTask :: Task Void
checkTask
	=				chooseProcess "The result of which process do you want to be checked?"
	>>= \procId ->	getProcess procId
	>>= \process -> chooseUserA "Select the user which has to check it:"
	>>= \user ->	applyChangeToProcess procId (check user (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title) CLTransient
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select the task you want to cancel:"
	>>= \procId ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title procId) CLTransient

reassignTask :: Task Void
reassignTask
	=				chooseProcess "Select the task you want to reassign to someone else:"
	>>= \procId ->	chooseUserA "Who should continue with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (reassign user (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title procId) CLTransient

restartTask :: Task Void
restartTask
	=				chooseProcess "Select the task you want to restart from scratch:"
	>>= \procId ->	chooseUserA "Who should start with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (restart user (fromJust process).Process.properties.managerProperties.taskDescription.TaskDescription.title) CLTransient

//Utility
chooseUserA :: !question -> Task User | html question
chooseUserA question
	= 						getUsers
	>>= \users ->			enterChoiceA ("Choose user",question) id buttons users
	>>= \res ->				case app2 (fst,id) res of
								(ActionOk,Just user)	-> return user
								_						-> throw "choosing a user has been cancelled"
										

chooseProcess :: String -> Task ProcessId
chooseProcess question
	=								getCurrentProcessId
	>>= \mypid ->					getProcessesWithStatus [Active]
	>>= \procs ->					enterChoiceA question id buttons
										[	( proc.Process.taskId
											, proc.Process.properties.managerProperties.taskDescription.TaskDescription.title
											, proc.Process.properties.managerProperties.ManagerProperties.priority
											, proc.Process.properties.managerProperties.ManagerProperties.worker)
											\\ proc <- procs | proc.Process.taskId <> mypid]
	>>= \res ->						case app2 (fst,id) res of
										(ActionOk,Just (pid,_,_,_))	-> return pid
										_							-> throw "choosing a process has been cancelled"

buttons = [(ActionCancel, always), (ActionOk, ifvalid)]	
	
	
	