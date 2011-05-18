implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdMisc, StdArray, StdTuple, StdList, Error, Shared
import TSt, Time, Store
from ProcessDB	import :: Process{..}, :: Menu
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld
from UserDB		import class UserDB(..), instance UserDB TSt
from Types		import :: ProcessId
	
derive gVisualize	Process, ProcessProperties, TaskProgress, SystemProperties, TaskProperties, TaskStatus, TaskDescription, TaskContainerType, InteractionTaskType, OutputTaskType
derive gUpdate		Process, ProcessProperties, TaskProgress, SystemProperties, TaskProperties, TaskStatus, TaskDescription, TaskContainerType, InteractionTaskType, OutputTaskType
derive gDefaultMask	Process, ProcessProperties, TaskProgress, SystemProperties, TaskProperties, TaskStatus, TaskDescription, TaskContainerType, InteractionTaskType, OutputTaskType
derive gVerify		Process, ProcessProperties, TaskProgress, SystemProperties, TaskProperties, TaskStatus, TaskDescription, TaskContainerType, InteractionTaskType, OutputTaskType
derive bimap Maybe,(,)

// generic functions for menus not needed because only functions generating menus (no actual menu structures) are serialised
gVisualize{|Menu|} _ _		= abort "not implemented"
gVisualize{|MenuItem|} _ _	= abort "not implemented"
gUpdate{|Menu|} _ _			= abort "not implemented"
gUpdate{|MenuItem|} _ _		= abort "not implemented"
gDefaultMask{|Menu|} _		= abort "not implemented"
gDefaultMask{|MenuItem|} _	= abort "not implemented"
gVerify{|Menu|} _ _			= abort "not implemented"
gVerify{|MenuItem|} _ _		= abort "not implemented"

getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkInstantTask ("Get process", "Read a process from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcess pid tst))

getProcessForUser :: !User !ProcessId -> Task (Maybe Process)
getProcessForUser user pid = mkInstantTask ("Get process for user", "Read a process from the database with a check if a user needs to work on it.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessForUser user pid tst))

getProcesses :: ![ProcessId] -> Task [Process]
getProcesses ids = mkInstantTask ("Get processes", "Read a set of processes from the database.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesById ids tst))

getProcessesWithStatus :: ![TaskStatus] ![RunningTaskStatus] -> Task [Process]
getProcessesWithStatus statuses runningStatuses = mkInstantTask ("Get processes by status", "Read all processes from the database with a specific status.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcesses statuses runningStatuses tst))

getProcessesForUser	:: !User ![TaskStatus] ![RunningTaskStatus] -> Task [Process]
getProcessesForUser user statuses runningStatuses = mkInstantTask ("Get processes for user", "Read all processes from the database that a user needs to work on.")
	(mkTaskFunction (\tst -> 'ProcessDB'.getProcessesForUser user statuses runningStatuses tst))

getProcessOwner :: !ProcessId -> Task (Maybe User)
getProcessOwner pid = mkInstantTask ("Get process owner", "Determine the user working on the task.") getProcessStatus`
where
	getProcessStatus` tst 
	# (process,tst)	= 'ProcessDB'.getProcess pid tst
	# owner 		= if (isNothing process) Nothing (Just (fromJust process).Process.properties.ProcessProperties.managerProperties.worker)
	= (TaskFinished owner,tst)
	
setProcessOwner :: !User !ProcessId -> Task Void
setProcessOwner user pid = mkInstantTask ("Set process owner", "Set the user working on the task.") setProcessOwner`
where
	setProcessOwner` tst=:{staticInfo}
		# (_,tst) = 'ProcessDB'.setProcessOwner user pid tst
		= (TaskFinished Void,tst)

getProcessStatus :: !ProcessId -> Task (TaskStatus,RunningTaskStatus)
getProcessStatus pid = mkInstantTask ("Get process status", "Determine the status of a process.") getProcessStatus`
where
	getProcessStatus` tst
		# (mbProcess,tst)	= 'ProcessDB'.getProcess pid tst
		= case mbProcess of
			Just proc	= (TaskFinished (proc.Process.properties.systemProperties.SystemProperties.status,proc.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status), tst)
			Nothing		= (TaskFinished (Deleted,Active), tst)
			

updateManagerProperties :: !ProcessId !(ManagerProperties -> ManagerProperties) -> Task Void
updateManagerProperties pid updateF = mkInstantTask ("Update manager properties","Update the manager properties of a process.") updateManagerProperties`
where
	updateManagerProperties` tst
		# (_,tst) = 'ProcessDB'.updateProcessProperties pid (\p -> {ProcessProperties|p & managerProperties = updateF p.ProcessProperties.managerProperties}) tst
		= (TaskFinished Void,tst)
		
deleteProcess :: !ProcessId -> Task Void
deleteProcess pid = mkInstantTask ("Delete process", "Delete a process from the database.") deleteProcess`
where
	deleteProcess` tst
		# (_,tst)	= 'ProcessDB'.deleteProcess pid tst
		= (TaskFinished Void,tst)
