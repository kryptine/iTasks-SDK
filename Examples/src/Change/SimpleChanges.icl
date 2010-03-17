implementation module SimpleChanges

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..),::TaskWorkerProperties(..),::TaskManagerProperties(..), :: TaskSystemProperties(..), :: TaskProgress, :: TaskParallelType(..)


changeExamples :: [Workflow]
changeExamples =
	[ 	{ name		= "Examples/Changes/Change priority"
		, label		= "Change priority"
		, roles		= []
		, mainTask	= try changePrio catch
		},
		{ name		= "Examples/Changes/Add warning"
		, label		= "Add warning"
		, roles		= []
		, mainTask	= try changeWarningTask catch
		},
		{ name		= "Examples/Changes/Duplicate task"
		, label		= "Duplicate task"
		, roles		= []
  		, mainTask	= try duplicateTask catch
  		},
 		{ name		= "Examples/Changes/Show result when task finishes"
		, label		= "Show result when task finishes"
		, roles		= []
  		, mainTask	= try informTask catch
  		},
 		{ name		= "Examples/Changes/Check task when finished"
		, label		= "Check task when finished"
		, roles		= []
  		, mainTask	= try checkTask catch
  		},
 		{ name		= "Examples/Changes/Cancel task"
		, label		= "Cancel task"
		, roles		= []
  		, mainTask	= try cancelTask catch
  		},
 		{ name		= "Examples/Changes/Reassign task"
		, label		= "Reassign task"
		, roles		= []
  		, mainTask	= try reassignTask catch
  		},
 		{ name		= "Examples/Changes/Restart task"
		, label		= "Restart task"
		, roles		= []
  		, mainTask	= try restartTask catch
  		}
  	]
where
	catch :: String -> Task Void
	catch message  = showMessage message

//Simple change which will run once and change the priority of all tasks to high
changePriority :: TaskPriority -> Dynamic
changePriority priority =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Just {TaskProperties| props & managerProps = {props.managerProps & priority = priority}},Nothing, Just (changePriority priority))

//Add a big red warning message prompt to the running task
addWarning :: String -> Dynamic
addWarning msg = 
	dynamic change  :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (((showStickyMessage (redText msg) >>| getDefaultValue) -||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: User User String -> Dynamic
duplicate me user topics =
	dynamic change topics :: A.a: Change a | iTask a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change topics p t t0 = (Nothing
							, Just (assign me.userName NormalPriority Nothing 
											(anyTaskExt [(p.managerProps.worker , t <<@ topics) 
														,(user.userName, t <<@ topics)
														] Open
											)
											<<@ ("Duplicated " +++ topics))
							, Nothing )

//inform will inform a user that some process has ended.
inform :: User String -> Dynamic
inform user procName =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (t >>= \res -> spawnProcess user.userName True (showMessageAbout ("Process " +++ procName +++ " ended!") res) >>| return res), Nothing)

//check will pass the result to the indicated user who can change the result in an editor before it passed.
check :: User String -> Dynamic
check user procName =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (t >>= \res -> assign user.userName HighPriority Nothing (updateInformation ("Please verify result of " +++ procName) res)), Nothing)

//cancel stop the process, and give the indicated user the responsibility to fill in the result
cancel :: User String ProcessId -> Dynamic
cancel user procName pid  =
	dynamic change :: A.b: Change b | iTask b
where
//	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (			deleteProcess pid 
										>>| 		mkDefault t
										>>= \def -> assign user.userName HighPriority Nothing (updateInformation ("Please define the result of " +++ procName) def) 
										), Nothing)
	mkDefault :: (Task a) -> (Task a) | iTask a
	mkDefault _ = getDefaultValue

//reassign the work to someone else
reassign :: User String ProcessId -> Dynamic
reassign user procName pid  =
	dynamic change :: A.b: Change b | iTask b
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Just {TaskProperties| props & managerProps = {props.managerProps & worker = user.userName}},Nothing, Nothing)

//restart starts the task from scratch and assigns it to the indicated user
restart :: User String -> Dynamic
restart user procName =
	dynamic change procName :: A.a: Change a | iTask a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change procName props t t0 = (Nothing, Just (assign user.userName HighPriority Nothing (t0 <<@ procName)), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	enterInformation "What should the new priority be?"
	>>= \priority ->applyChangeToProcess proc (changePriority priority) (CLPersistent "priority")

changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation "Type in warning you want to show to all:"
	>>= \warning ->	chooseProcess "What process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "What process do you want to duplicate?"
	>>= \procId ->	getProcess procId
	>>= \process ->	chooseUserA "Select the user you want to work on it as well:"
	>>= \user ->	getCurrentUser
	>>= \me ->		applyChangeToProcess procId (duplicate me user (fromJust process).properties.managerProps.subject) CLTransient

informTask :: Task Void
informTask
	=				chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	chooseUserA "Select the user you want this result to see:"
	>>= \user ->	getProcess procId
	>>= \process ->applyChangeToProcess procId (inform user (fromJust process).properties.managerProps.subject) CLTransient
	
checkTask :: Task Void
checkTask
	=				chooseProcess "The result of which process do you want to be checked?"
	>>= \procId ->	getProcess procId
	>>= \process -> chooseUserA "Select the user which has to check it:"
	>>= \user ->	applyChangeToProcess procId (check user (fromJust process).properties.managerProps.subject) CLTransient
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select task you want to cancel:"
	>>= \procId ->	chooseUserA "Select the user who will define the result instead:"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel user (fromJust process).properties.managerProps.subject procId) CLTransient

reassignTask :: Task Void
reassignTask
	=				chooseProcess "Select task you want to reassign to someone else:"
	>>= \procId ->	chooseUserA "Who should continue with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (reassign user (fromJust process).properties.managerProps.subject procId) CLTransient

restartTask :: Task Void
restartTask
	=				chooseProcess "Select task you want to restart from scratch:"
	>>= \procId ->	chooseUserA "Who should start with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (restart user (fromJust process).properties.managerProps.subject) CLTransient

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
																	, proc.properties.managerProps.subject
																	, proc.properties.managerProps.priority
																	, proc.properties.managerProps.worker)
																	\\ proc <- procs | proc.Process.processId <> mypid]
	>>= \(action,(pid,_,_,_)) ->	case action of
										ActionCancel -> throw "choosing a process has been cancelled"
										_ ->			return pid

buttons = [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)]	
	
	
	