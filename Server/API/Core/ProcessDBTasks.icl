implementation module ProcessDBTasks

import StdOverloaded, StdClass, StdInt, StdMisc, StdArray, StdTuple, StdList, Error, Map, Time, Tuple
import Task, Store, TaskContext, Shared, Util
from SystemTypes	import :: ProcessId
from StdFunc		import id
from ProcessDB		import qualified class ProcessDB(..), instance ProcessDB IWorld

import GenVisualize
	
derive gVisualizeText	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive gVisualizeHtml	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive gVisualizeEditor	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive gUpdate			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive gDefaultMask		ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive gVerify			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus, InteractionTaskType, OutputTaskType
derive bimap Maybe,(,)
	
getProcess :: !ProcessId -> Task (Maybe TaskInstanceMeta)
getProcess pid = mkInstantTask ("Get process", "Read a process from the database.") eval
where
	eval taskNr iworld = appFst TaskFinished ( 'ProcessDB'.getProcess pid iworld)
	
getProcessForUser :: !User !ProcessId -> Task (Maybe TaskInstanceMeta)
getProcessForUser user pid = mkInstantTask ("Get process for user", "Read a process from the database with a check if a user needs to work on it.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessForUser user pid iworld)
	
getProcesses :: ![ProcessId] -> Task [TaskInstanceMeta]
getProcesses ids = mkInstantTask ("Get processes", "Read a set of processes from the database.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessesById ids iworld)
	
getProcessesWithStatus :: ![TaskStatus] -> Task [TaskInstanceMeta]
getProcessesWithStatus statuses = mkInstantTask ("Get processes by status", "Read all processes from the database with a specific status.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcesses statuses iworld)

getProcessesForUser	:: !User ![TaskStatus] -> Task [TaskInstanceMeta]
getProcessesForUser user statuses = mkInstantTask ("Get processes for user", "Read all processes from the database that a user needs to work on.") eval
where
	eval taskNr iworld = appFst TaskFinished ('ProcessDB'.getProcessesForUser user statuses iworld)
	
getProcessStatus :: !ProcessId -> Task TaskStatus
getProcessStatus pid = mkInstantTask ("Get process status", "Determine the status of a process.") eval
where
	eval taskNr iworld
		# (mbProcess,iworld)	= 'ProcessDB'.getProcess pid iworld
		= case mbProcess of
			Just proc	= (TaskFinished (proc.TaskInstanceMeta.progressMeta.ProgressMeta.status), iworld)
			Nothing		= (TaskFinished Deleted, iworld)

deleteProcess :: !ProcessId -> Task Void
deleteProcess pid = mkInstantTask ("Delete process", "Delete a process from the database.") eval
where
	eval taskNr iworld
		# iworld = 'ProcessDB'.deleteProcess pid iworld
		= (TaskFinished Void,iworld)
