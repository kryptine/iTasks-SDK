implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdMisc, StdArray, StdTuple, StdList, Error, Map, Time
import Task, Store, TaskContext, Shared, Util
from SystemTypes	import :: ProcessId
from StdFunc		import id
from ProcessDB		import :: Process{..}
from ProcessDB		import qualified class ProcessDB(..), instance ProcessDB IWorld

import GenVisualize
	
derive gVisualizeText	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive gVisualizeHtml	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive gVisualizeEditor	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive gUpdate			Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive gDefaultMask		Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive gVerify			Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription, InteractionTaskType, OutputTaskType
derive bimap Maybe,(,)
	
getProcess :: !ProcessId -> Task (Maybe Process)
getProcess pid = mkInstantTask ("Get process", "Read a process from the database.") eval
where
	eval taskNr iworld = appFst TaskFinished ( 'ProcessDB'.getProcess pid iworld)
	
getProcessForUser :: !User !ProcessId -> Task (Maybe Process)
getProcessForUser user pid = mkInstantTask ("Get process for user", "Read a process from the database with a check if a user needs to work on it.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessForUser user pid iworld)
	
getProcesses :: ![ProcessId] -> Task [Process]
getProcesses ids = mkInstantTask ("Get processes", "Read a set of processes from the database.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessesById ids iworld)
	
getProcessesWithStatus :: ![TaskStatus] ![RunningTaskStatus] -> Task [Process]
getProcessesWithStatus statuses runningStatuses = mkInstantTask ("Get processes by status", "Read all processes from the database with a specific status.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcesses statuses runningStatuses iworld)

getProcessesForUser	:: !User ![TaskStatus] ![RunningTaskStatus] -> Task [Process]
getProcessesForUser user statuses runningStatuses = mkInstantTask ("Get processes for user", "Read all processes from the database that a user needs to work on.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessesForUser user statuses runningStatuses iworld)
	
getProcessOwner :: !ProcessId -> Task (Maybe User)
getProcessOwner pid = mkInstantTask ("Get process owner", "Determine the user working on the task.") eval
where
	eval taskNr iworld
		# (process,iworld)	= 'ProcessDB'.getProcess pid iworld
		# owner 			= if (isNothing process) Nothing (Just (fromJust process).Process.properties.ProcessProperties.managerProperties.worker)
		= (TaskFinished owner,iworld)
	
setProcessOwner :: !User !ProcessId -> Task Void
setProcessOwner user pid = mkInstantTask ("Set process owner", "Set the user working on the task.") eval
where
	eval taskNr iworld
		# (_,iworld) = 'ProcessDB'.setProcessOwner user pid iworld
		= (TaskFinished Void, iworld)
		
getProcessStatus :: !ProcessId -> Task (TaskStatus,RunningTaskStatus)
getProcessStatus pid = mkInstantTask ("Get process status", "Determine the status of a process.") eval
where
	eval taskNr iworld
		# (mbProcess,iworld)	= 'ProcessDB'.getProcess pid iworld
		= case mbProcess of
			Just proc	= (TaskFinished (proc.Process.properties.systemProperties.SystemProperties.status,proc.Process.properties.ProcessProperties.managerProperties.ManagerProperties.status), iworld)
			Nothing		= (TaskFinished (Deleted,Active), iworld)
	
updateManagerProperties :: !ProcessId !(ManagerProperties -> ManagerProperties) -> Task Void
updateManagerProperties pid updateF = mkInstantTask ("Update manager properties","Update the manager properties of a process.") eval
where
	eval taskNr iworld
		# (_,iworld) = 'ProcessDB'.updateProcessProperties pid (\p -> {ProcessProperties|p & managerProperties = updateF p.ProcessProperties.managerProperties}) iworld
		= (TaskFinished Void,iworld)
		
deleteProcess :: !ProcessId -> Task Void
deleteProcess pid = mkInstantTask ("Delete process", "Delete a process from the database.") eval
where
	eval taskNr iworld
		# (_,iworld) = 'ProcessDB'.deleteProcess pid iworld
		= (TaskFinished Void,iworld)
