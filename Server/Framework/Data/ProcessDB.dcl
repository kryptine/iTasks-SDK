definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe, Types, TSt
from TaskTree	import :: TaskProperties, :: TaskParallelType
from Time		import :: Timestamp

derive class iTask Process, TaskPriority, TaskParallelType, TaskProperties, WorkerProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription

/**
* Our local process type
*/
:: Process =	{ taskId			:: !TaskId						// The process identification				  
				//Public process meta data
				, properties		:: !TaskProperties				// The properties of the main task node of this process
				//System internal information
				, changeCount		:: !Int							// The number of task changes that have been applied
				, mutable			:: !Bool						// May the process be evaluated further (required for duplication of processes by changes)
				, inParallelType	:: !(Maybe TaskParallelType)	// The type of parallel, if the process is part of one
					}

class ProcessDB st
where
	createProcess			:: !Process												!*st -> (!ProcessId,		!*st)
	deleteProcess			:: !TaskId												!*st -> (!Bool,				!*st)
	getProcess				:: !TaskId												!*st -> (!Maybe Process,	!*st)
	getProcessForUser		:: !User !TaskId										!*st -> (!Maybe Process,	!*st)
	getProcessForManager 	:: !User !TaskId 										!*st -> (!Maybe Process, 	!*st)
	getProcesses 			:: ![TaskStatus] 										!*st -> (![Process], 		!*st)
	getProcessesById		:: ![TaskId]											!*st -> (![Process],		!*st)
	getProcessesForUser		:: !User ![TaskStatus]									!*st -> (![Process],		!*st)
	
	setProcessOwner			:: !User !TaskId										!*st -> (!Bool,				!*st)
	setProcessStatus		:: !TaskStatus !TaskId									!*st -> (!Bool,				!*st)
	
	updateProcess			:: !TaskId (Process -> Process)							!*st -> (!Bool,				!*st)
	updateProcessProperties	:: !TaskId (TaskProperties -> TaskProperties)			!*st -> (!Bool,				!*st)
	
	removeFinishedProcesses :: 														!*st -> (!Bool, 			!*st)
	
	setImmutable			:: !TaskId												!*st -> *st
	copySubProcesses		:: !TaskId !TaskId										!*st -> *st
	deleteSubProcesses		:: !TaskId												!*st -> *st

instance ProcessDB IWorld
instance ProcessDB TSt
