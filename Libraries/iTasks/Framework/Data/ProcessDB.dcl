definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe
import Types, HSt
from TSt import :: Workflow
from TaskTree import :: TaskProperties
from Time import :: Time

/**
* Our local process type
*/
:: Process 		= {	processId		:: !Int				//The process identification
				  , processType		:: !ProcessType		//The type of process
				  
				  , status			:: !ProcessStatus	//The status of the process (updated after each run)
				  , parent			:: !ProcessId		//The (direct) parent process
				  , properties		:: !TaskProperties	//The properties of the main task node of this process
				  , result			:: !Maybe DynamicId	//Possibly a stored process result
				  }

:: ProcessType	= StaticProcess !String					//A static process (name of the workflow)
				| DynamicProcess !DynamicId				//A dynamic process (id of the main task)
				| EmbeddedProcess !ProcessId !TaskId	//An embedded process/maintask (id of the (indirect) parent process, and taskid of the anchorpoint)
				

:: ProcessStatus =	Active
				 |	Suspended
				 |	Finished
				 |	Deleted
				 
class ProcessDB st
where
	createProcess			:: !Process													!*st -> (!ProcessId,	!*st)
	deleteProcess			:: !ProcessId												!*st -> (!Bool,			!*st)
	getProcess				:: !ProcessId												!*st -> (!Maybe Process,!*st)
	getProcessForUser		:: !UserId !ProcessId										!*st -> (!Maybe Process,!*st)
	getProcesses			:: ![ProcessStatus]											!*st -> (![Process],	!*st)
	getProcessesById		:: ![ProcessId]												!*st -> (![Process],	!*st)
	getProcessesForUser		:: !UserId ![ProcessStatus]	Bool							!*st -> (![Process],	!*st)
	getSubProcess			:: !ProcessId !TaskId										!*st -> (!Maybe Process,!*st)

	setProcessOwner			:: !(UserId,String) !(UserId,String) !ProcessId				!*st -> (!Bool,			!*st)
	setProcessStatus		:: !ProcessStatus !ProcessId								!*st -> (!Bool,			!*st)
	setProcessResult		:: !DynamicId !ProcessId									!*st -> (!Bool,			!*st)

	updateProcess			:: !ProcessId (Process -> Process)							!*st -> (!Bool,			!*st)
	updateProcessProperties	:: !ProcessId (TaskProperties -> TaskProperties)			!*st -> (!Bool,			!*st)

instance ProcessDB HSt
/*
* Utility functions for creating process database entries.
*/
mkStaticProcessEntry	:: Workflow			Time (UserId,String) (UserId,String) ProcessStatus				-> Process
mkDynamicProcessEntry	:: String DynamicId	Time (UserId,String) (UserId,String) ProcessStatus ProcessId	-> Process
mkEmbeddedProcessEntry	:: ProcessId TaskId		TaskProperties ProcessStatus ProcessId						-> Process

instance toString ProcessStatus
