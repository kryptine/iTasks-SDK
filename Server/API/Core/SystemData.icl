implementation module SystemData

import SystemTypes, Time, Shared, SharedCombinators, Util, Text, Task, Tuple
import Random
import StdList
from StdFunc	import o, seq
from IWorld		import :: IWorld(..), :: Control
from Util		import qualified currentDate, currentTime, currentDateTime, currentTimestamp
from UserDB		import qualified class UserDB(..), instance UserDB IWorld
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from ProcessDB	import :: Process
from SessionDB	import qualified class SessionDB(..), instance SessionDB IWorld
from WorkflowDB	import qualified class WorkflowDB(..), instance WorkflowDB IWorld
from WorkflowDB	import :: WorkflowDescription

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared "SystemData_currentDateTime" 'Util'.currentDateTime 'Util'.currentTimestamp
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared "SystemData_currentTime" 'Util'.currentTime 'Util'.currentTimestamp
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared "SystemData_currentDate" 'Util'.currentDate 'Util'.currentTimestamp

// Users
users :: ReadOnlyShared [User]
users = makeReadOnlyShared "SystemData_users" 'UserDB'.getUsers 'UserDB'.lastChange

usersWithRole	:: !Role -> ReadOnlyShared [User]
usersWithRole role = makeReadOnlyShared ("SystemData_usersWithRole-" +++ toString role) ('UserDB'.getUsersWithRole role) 'UserDB'.lastChange

userDetails :: !User -> Shared UserDetails
userDetails user = ReadWriteShared ["userDetails-" +++ toString user] read write (appFst Ok o 'UserDB'.lastChange)
where
	read iworld	= appFst (mb2error "user not in database") ('UserDB'.getUserDetails user iworld)
	write details iworld
		# (_,iworld) = 'UserDB'.updateUser user details iworld
		= (Ok Void,iworld)

currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared "SystemData_currentUser" (\iworld=:{currentUser} -> (currentUser,iworld)) (\iworld -> (Timestamp 0, iworld))
	
currentUserDetails :: ReadOnlyShared (Maybe UserDetails)
currentUserDetails = makeReadOnlyShared "SystemData_currentUserDetails" (\iworld=:{currentUser} -> 'UserDB'.getUserDetails currentUser iworld) (\iworld -> (Timestamp 0, iworld))

// Sessions
sessions :: ReadOnlyShared [Session]
sessions = makeReadOnlyShared "SystemData_sessions" 'SessionDB'.getSessions 'SessionDB'.lastChange

// Available workflows
workflows :: ReadOnlyShared [WorkflowDescription]
workflows = makeReadOnlyShared "SystemData_workflows" 'WorkflowDB'.getWorkflowDescriptions 'WorkflowDB'.lastChange

allowedWorkflows :: ReadOnlyShared [WorkflowDescription]
allowedWorkflows = mapSharedRead filterAllowed (workflows |+| (currentUser |+| currentUserDetails))
where
	filterAllowed (workflows,(user,mbDetails)) = filter (isAllowedWorkflow user mbDetails) workflows
	
workflowTree :: ReadOnlyShared (Tree WorkflowDescription)
workflowTree = mapSharedRead mkFlowTree workflows

allowedWorkflowTree :: ReadOnlyShared (Tree WorkflowDescription)
allowedWorkflowTree = mapSharedRead mkFlowTree allowedWorkflows

mkFlowTree :: ![WorkflowDescription] -> Tree WorkflowDescription
mkFlowTree workflows = Tree (seq (map insertWorkflow workflows) [])
where
	insertWorkflow descr=:{WorkflowDescription|path} nodeList = insertWorkflow` (split "/" path) nodeList
	where
		insertWorkflow` [] nodeList = nodeList
		insertWorkflow` [title] nodeList = nodeList ++ [Leaf descr]
		insertWorkflow` path=:[nodeP:pathR] [node=:(Node nodeL nodes):nodesR]
			| nodeP == nodeL	= [Node nodeL (insertWorkflow` pathR nodes):nodesR]
			| otherwise			= [node:insertWorkflow` path nodesR]
		insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
		insertWorkflow` [nodeP:pathR] [] = [Node nodeP (insertWorkflow` pathR [])]
		
workflowTask :: !WorkflowId -> ReadOnlyShared WorkflowTaskContainer
workflowTask wid = makeReadOnlySharedError ("SystemData_workflowTask_" +++ (toString wid)) getTask ((appFst Ok) o 'WorkflowDB'.lastChange)
where
	getTask iworld
		# (mbWorkflow,iworld) = 'WorkflowDB'.getWorkflow wid iworld
		= case mbWorkflow of
			Just {task}	= (Ok task, iworld)
			_			= (Error ("could not find workflow " +++ (toString wid)), iworld)
		
// Workflow processes
topLevelTasks :: (TaskList Void)
topLevelTasks = GlobalTaskList

currentProcessId :: ReadOnlyShared ProcessId
currentProcessId = makeReadOnlyShared "SystemData_currentProcess" (\iworld=:{currentProcess} -> (currentProcess, iworld)) ('ProcessDB'.lastChange)

currentProcesses ::ReadOnlyShared [Process]
currentProcesses = makeReadOnlyShared "SystemData_processes" ('ProcessDB'.getProcesses [Running] [Active]) 'ProcessDB'.lastChange

processesForCurrentUser	:: ReadOnlyShared [Process]
processesForCurrentUser = makeReadOnlyShared "SystemData_processesForCurrentUser" (\iworld=:{currentUser} -> 'ProcessDB'.getProcessesForUser currentUser [Running] [Active] iworld) 'ProcessDB'.lastChange

applicationName :: ReadOnlyShared String
applicationName = makeReadOnlyShared "SystemData_applicationName" appName (\iworld -> (Timestamp 0, iworld))
where
	appName iworld=:{IWorld|application} = (application,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared "SystemData_randomInt" randomInt 'Util'.currentTimestamp
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})
		
null :: ReadWriteShared Void a
null = ReadWriteShared ["SystemData_null"] read write getTimestamp
where
	read iworld			= (Ok Void,iworld)
	write _ iworld		= (Ok Void,iworld)
	getTimestamp iworld	= (Ok (Timestamp 0),iworld)