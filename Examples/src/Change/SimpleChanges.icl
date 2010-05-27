implementation module SimpleChanges

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..),::WorkerProperties(..),::ManagerProperties(..), :: SystemProperties(..), :: TaskProgress, :: TaskParallelType(..)

changeExamples :: [Workflow]
changeExamples =
	[ 	workflow "Examples/Changes/Change priority" ("Change priority" @>> (try changePrio catch))
	,	workflow "Examples/Changes/Add warning" ("Add warning" @>> (try changeWarningTask catch))
	,	workflow "Examples/Changes/Duplicate task" ("Duplicate task" @>> (try duplicateTask catch))
	,	workflow "Examples/Changes/Show result when task finishes" ("Show result when task finishes" @>> (try informTask catch))
	,	workflow "Examples/Changes/Check task when finished" ("Check task when finished" @>> (try checkTask catch))
	,	workflow "Examples/Changes/Cancel task" ("Cancel task" @>> (try cancelTask catch))
 	,	workflow "Examples/Changes/Reassign task" ("Reassign task" @>> (try reassignTask catch))
 	,	workflow "Examples/Changes/Restart task" ("Restart task" @>> (try restartTask catch))
  	]
where
	catch :: String -> Task Void
	catch message  = showMessage message

//Simple change which will run once and change the priority of all tasks to high
changePriority :: TaskPriority -> ChangeDyn
changePriority priority =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Just {TaskProperties| props & managerProps = {props.managerProps & priority = priority}},Nothing, Just (changePriority priority))

//Add a big red warning message prompt to the running task
addWarning :: String -> ChangeDyn
addWarning msg = 
	dynamic change  :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Nothing, Just (((showStickyMessage (redText msg) >>| getDefaultValue) -||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: User User String -> ChangeDyn
duplicate me user topics =
	dynamic change me user topics :: A.a: Change a | iTask a
where
	change :: User User String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change me user topics props t t0 
		= 	( Just {TaskProperties | props & managerProps = {ManagerProperties | props.managerProps & worker = toUserName me}}
			, Just (assign (toUserName me) NormalPriority Nothing 
							(anyProc 	[ {AssignedTask| user = props.managerProps.ManagerProperties.worker , task = t <<@ topics} 
										, {AssignedTask| user = toUserName user, task = t <<@ topics}
										] Open
							)
							<<@ ("Duplicated " +++ topics))
			, Nothing )

//inform will inform a user that some process has ended.
inform :: User String -> ChangeDyn
inform user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> spawnProcess (toUserName user) True (showMessageAbout ("Process " +++ procName +++ " ended!") res) >>| return res), Nothing)

//check will pass the result to the indicated user who can change the result in an editor before it passed.
check :: User String -> ChangeDyn
check user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> assign user HighPriority Nothing (updateInformation ("Please verify result of " +++ procName) res)), Nothing)

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
		# username = (toUserName user)
		= (Just {TaskProperties | props & managerProps = {ManagerProperties | props.managerProps & worker = username}},Nothing, Nothing)

//restart starts the task from scratch and assigns it to the indicated user
restart :: User String -> Dynamic
restart user procName =
	dynamic change user procName :: A.a: Change a | iTask a
where
	change :: User String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user procName props t t0 = (Nothing, Just (assign (toUserName user) HighPriority Nothing (t0 <<@ procName)), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	enterInformation "What should the new priority be?"
	>>= \priority ->applyChangeToProcess proc (changePriority priority) (CLPersistent "priority")

changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation "Type in warning you want to show to all:"
	>>= \warning ->	chooseProcess "Which process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "Which process do you want to duplicate?"
	>>= \procId ->	getProcess procId
	>>= \process ->	chooseUserA "Select the user you want to work on it as well:"
	>>= \user ->	getCurrentUser
	>>= \me ->		applyChangeToProcess procId (duplicate me user (fromJust process).Process.properties.managerProps.subject) CLTransient

informTask :: Task Void
informTask
	=				chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	chooseUserA "Select the user you want this result to see:"
	>>= \user ->	getProcess procId
	>>= \process ->applyChangeToProcess procId (inform user (fromJust process).Process.properties.managerProps.subject) CLTransient
	
checkTask :: Task Void
checkTask
	=				chooseProcess "The result of which process do you want to be checked?"
	>>= \procId ->	getProcess procId
	>>= \process -> chooseUserA "Select the user which has to check it:"
	>>= \user ->	applyChangeToProcess procId (check user (fromJust process).Process.properties.managerProps.subject) CLTransient
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select the task you want to cancel:"
	>>= \procId ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel (fromJust process).Process.properties.managerProps.subject procId) CLTransient

reassignTask :: Task Void
reassignTask
	=				chooseProcess "Select the task you want to reassign to someone else:"
	>>= \procId ->	chooseUserA "Who should continue with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (reassign user (fromJust process).Process.properties.managerProps.subject procId) CLTransient

restartTask :: Task Void
restartTask
	=				chooseProcess "Select the task you want to restart from scratch:"
	>>= \procId ->	chooseUserA "Who should start with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (restart user (fromJust process).Process.properties.managerProps.subject) CLTransient

//Utility
chooseUserA :: !question -> Task User | html question
chooseUserA question
	= 						getUsers
	>>= \users ->			enterChoiceA question buttons users
	>>= \(action,user) ->	case action of
										ActionCancel -> throw "choosing a user has been cancelled"
										_ ->			return user

chooseProcess :: String -> Task ProcessId
chooseProcess question
	=								getCurrentProcessId
	>>= \mypid ->					getProcessesWithStatus [Active]
	>>= \procs ->					enterChoiceA question buttons [	( proc.Process.processId
																	, proc.Process.properties.managerProps.subject
																	, proc.Process.properties.managerProps.priority
																	, proc.Process.properties.managerProps.ManagerProperties.worker)
																	\\ proc <- procs | proc.Process.processId <> mypid]
	>>= \(action,(pid,_,_,_)) ->	case action of
										ActionCancel -> throw "choosing a process has been cancelled"
										_ ->			return pid

buttons = [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]	
	
	
	