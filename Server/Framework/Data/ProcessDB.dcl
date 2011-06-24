definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import Maybe, SystemTypes, Task, TaskContext
from Time import :: Timestamp

derive JSONEncode	Process
derive JSONDecode	Process
derive gEq			Process

/**
* Our local process type
*/
:: Process 		= {	processId		:: !ProcessId					// The process identification				  
				  , properties		:: !ProcessProperties			// The properties of the main task node of this process
				  , subprocesses	:: ![Process]					// The sub processes of this process
				  }

class ProcessDB st
where
	getNextProcessId 		::														!*st -> (!ProcessId,		!*st)

	getProcess				:: !ProcessId											!*st -> (!Maybe Process,	!*st)
	getProcessContext		:: !ProcessId											!*st -> (!Maybe TaskContext,!*st)
	
	getProcessForUser		:: !User !ProcessId										!*st -> (!Maybe Process,	!*st)
	getProcesses 			:: ![TaskStatus] ![RunningTaskStatus]					!*st -> (![Process], 		!*st)
	getProcessesById		:: ![ProcessId]											!*st -> (![Process],		!*st)
	getProcessesForUser		:: !User ![TaskStatus] ![RunningTaskStatus]				!*st -> (![Process],		!*st)
	
	setProcessContext		:: !ProcessId !TaskContext								!*st -> *st

	deleteProcess			:: !ProcessId											!*st -> (!Bool,				!*st)
	
	/**
	* Gets the timestamp of the last change of the workflow database.
	*
	* @param A unique database handle
	*
	* @return The timestamp
	* @retrun The database handle 
	*/
	lastChange				::														!*st -> (!Timestamp,!*st)

	
	//DEPRECATED
	setProcessOwner			:: !User !ProcessId										!*st -> (!Bool,				!*st)
	setProcessStatus		:: !TaskStatus !ProcessId								!*st -> (!Bool,				!*st)
	updateProcess			:: !ProcessId (Process -> Process)						!*st -> (!Bool,				!*st)
	updateProcessProperties	:: !ProcessId (ProcessProperties -> ProcessProperties)	!*st -> (!Bool,				!*st)
	
	
instance ProcessDB IWorld