definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe
import Types, TSt
from TaskTree	import :: TaskProperties
from Time		import :: Timestamp

/**
* Our local process type
*/
:: Process 		= {	processId		:: !ProcessId				// The process identification				  
				  , status			:: !ProcessStatus			// The status of the process (updated after each run)
				  , parent			:: !ProcessId				// The (direct) parent process
				  , properties		:: !TaskProperties			// The properties of the main task node of this process
				  , changes			:: ![(String,DynamicId)]	// Optionally a list of labeled change functions
				  , changeNr		:: !Int						// The number of task changes that have been applied
				  }				

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
	getProcesses 			:: ![ProcessStatus] 										!*st -> (![Process], 	!*st)
	getProcessesById		:: ![ProcessId]												!*st -> (![Process],	!*st)
	getProcessesForUser		:: !UserId ![ProcessStatus]									!*st -> (![Process],	!*st)

	setProcessOwner			:: !(UserId,String) !(UserId,String) !ProcessId				!*st -> (!Bool,			!*st)
	setProcessStatus		:: !ProcessStatus !ProcessId								!*st -> (!Bool,			!*st)
	//setProcessResult		:: !DynamicId !ProcessId									!*st -> (!Bool,			!*st)

	updateProcess			:: !ProcessId (Process -> Process)							!*st -> (!Bool,			!*st)
	updateProcessProperties	:: !ProcessId (TaskProperties -> TaskProperties)			!*st -> (!Bool,			!*st)

	removeFinishedProcesses :: 															!*st -> (!Bool, 		!*st)

instance ProcessDB TSt
/*
* Utility function for creating process database entries.
*/
//mkStaticProcessEntry	:: Workflow			Timestamp (UserId,String) (UserId,String) ProcessStatus				-> Process
//mkDynamicProcessEntry	:: String DynamicId	Timestamp (UserId,String) (UserId,String) ProcessStatus ProcessId	-> Process
//mkEmbeddedProcessEntry	:: ProcessId TaskId		TaskProperties ProcessStatus ProcessId						-> Process

mkProcessEntry :: String Timestamp (UserId, String) (UserId, String) ProcessStatus ProcessId -> Process

instance toString ProcessStatus
instance == ProcessStatus
