definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import Maybe, Types, TSt, Task
from Time import :: Timestamp

derive JSONEncode	Process
derive JSONDecode	Process
derive gEq			Process

/**
* Our local process type
*/
:: Process 		= {	taskId			:: !TaskId						// The process identification				  
				  //Public process meta data
				  , properties		:: !ProcessProperties			// The properties of the main task node of this process
				  //System internal information
				  , dependents		:: ![TaskId]					// Other process that are to be evaluated on completion of this task
				  , changeCount		:: !Int							// The number of task changes that have been applied
				  , mutable			:: !Bool						// May the process be evaluated further (required for duplication of processes by changes)
				  }

class ProcessDB st
where
	createProcess			:: !Process												!*st -> (!ProcessId,		!*st)
	deleteProcess			:: !TaskId												!*st -> (!Bool,				!*st)
	getProcess				:: !TaskId												!*st -> (!Maybe Process,	!*st)
	getProcessForUser		:: !User !TaskId										!*st -> (!Maybe Process,	!*st)
	getProcesses 			:: ![TaskStatus] ![RunningTaskStatus]					!*st -> (![Process], 		!*st)
	getProcessesById		:: ![TaskId]											!*st -> (![Process],		!*st)
	getProcessesForUser		:: !User ![TaskStatus] ![RunningTaskStatus]				!*st -> (![Process],		!*st)
	
	setProcessOwner			:: !User !TaskId										!*st -> (!Bool,				!*st)
	setProcessStatus		:: !TaskStatus !TaskId									!*st -> (!Bool,				!*st)
	
	updateProcess			:: !TaskId (Process -> Process)							!*st -> (!Bool,				!*st)
	updateProcessProperties	:: !TaskId (ProcessProperties -> ProcessProperties)		!*st -> (!Bool,				!*st)
	
	removeFinishedProcesses :: 														!*st -> (!Bool, 			!*st)
	
	setImmutable			:: !TaskId												!*st -> *st
	addDependency			:: !TaskId !TaskId										!*st -> (!Bool,				!*st)	
	
	copySubProcesses		:: !TaskId !TaskId										!*st -> *st
	deleteSubProcesses		:: !TaskId												!*st -> *st

instance ProcessDB IWorld
instance ProcessDB TSt
