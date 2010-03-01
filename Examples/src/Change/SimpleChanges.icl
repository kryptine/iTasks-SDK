implementation module SimpleChanges

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..),::TaskWorkerProperties(..),::TaskManagerProperties(..), :: TaskSystemProperties(..), :: TaskProgress


changeExamples :: [Workflow]
changeExamples =
	[ 	{ name		= "Examples/Changes/Change priorities of all"
		, label		= "Change priorities of all"
		, roles		= []
		, mainTask	= changePrio
		},
		{ name		= "Examples/Changes/Add warning to all"
		, label		= "Add warning to all"
		, roles		= []
		, mainTask	= changeWarningTask
		},
		{ name		= "Examples/Changes/Duplicate task"
		, label		= "Duplicate task"
		, roles		= []
  		, mainTask	= duplicateTask
  		},
 		{ name		= "Examples/Changes/Show result when task finishes"
		, label		= "Show result when task finishes"
		, roles		= []
  		, mainTask	= informTask
  		},
 		{ name		= "Examples/Changes/Check task when finished"
		, label		= "Check task when finsihed"
		, roles		= []
  		, mainTask	= checkTask
  		},
 		{ name		= "Examples/Changes/Cancel task"
		, label		= "Cancel task"
		, roles		= []
  		, mainTask	= cancelTask
  		}
	]

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
duplicate :: User String -> Dynamic
duplicate user topics =
	dynamic change topics :: A.a: Change a | iTask a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change topics p t t0 = (Nothing, Just (t -||- assign user.userName HighPriority Nothing (t <<@ topics)), Nothing )

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
	change props t t0 = (Nothing, Just (t >>= \res -> assign user.userName HighPriority Nothing (updateInformation ("Please verify result of " +++ procName) res) >>| return res), Nothing)

//cancel stop the process, and give the indicated user the responsibility to fill in the result
cancel :: User String ProcessId -> Dynamic
cancel user procName pid  =
	dynamic change :: A.b: Change b | iTask b
where
//	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (			deleteProcess pid 
										>>| 		mkDefault t
										>>= \def -> assign user.userName HighPriority Nothing (updateInformation ("Please define the result of " +++ procName) def) 
										>>| 		return def), Nothing)
	mkDefault :: (Task a) -> (Task a) | iTask a
	mkDefault _ = getDefaultValue


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
	>>= \process ->	chooseUser "Select the user you want to work on it as well:"
	>>= \user ->	applyChangeToProcess procId (duplicate user (fromJust process).properties.managerProps.subject) CLTransient

informTask :: Task Void
informTask
	=				chooseUser "Select the user you want to show the result of a process:"
	>>= \user ->	chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	getProcess procId
	>>= \process ->	applyChangeToProcess procId (inform user (fromJust process).properties.managerProps.subject) CLTransient
	
checkTask :: Task Void
checkTask
	=				chooseUser "Select the user you want to check the result of a process:"
	>>= \user ->	chooseProcess "The result of which process do you want to check?"
	>>= \procId ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (check user (fromJust process).properties.managerProps.subject) CLTransient
	
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select task you want to cancel"
	>>= \procId ->	chooseUser "Select the user who will define the result:"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel user (fromJust process).properties.managerProps.subject procId) CLTransient


//Utility
chooseProcess :: String -> Task ProcessId
chooseProcess question
	=				getProcessesWithStatus [Active]
	>>= \procs ->	enterChoice question procs
	>>= \proc ->	return proc.Process.processId