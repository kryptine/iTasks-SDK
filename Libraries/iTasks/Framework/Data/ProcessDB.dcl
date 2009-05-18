definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe
import Types, HSt
from TSt import :: Workflow
from TaskTree import :: TaskProperties

/**
* Our local process type
*/
:: Process 		= {	processId		:: !Int				//The process identification
				  , processType		:: !ProcessType		//The type of process
				  
				  , status			:: !ProcessStatus	//The status of the process (updated after each run)
				  , parent			:: !ProcessId		//The (direct) parent process
				  , properties		:: !TaskProperties	//The properties of the main task node of this process
				  , result			:: !Maybe DynamicId	//Possibly a stored process result
			
				  , users			:: ![UserId]		//OBSOLETE //A list of users involved in the process (updated after each run)
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
	createProcess		:: !Process													!*st -> (!ProcessId,	!*st)
	deleteProcess		:: !ProcessId												!*st -> (!Bool,			!*st)
	getProcess			:: !ProcessId												!*st -> (!Maybe Process,!*st)
	getProcessForUser	:: !UserId !ProcessId										!*st -> (!Maybe Process,!*st)
	getProcesses		:: ![ProcessStatus]											!*st -> (![Process],	!*st)
	getProcessesById	:: ![ProcessId]												!*st -> (![Process],	!*st)
	getProcessesForUser	:: !UserId ![ProcessStatus]	Bool								!*st -> (![Process],	!*st)
	getEmbeddedProcess	:: !ProcessId !TaskId										!*st -> (!Maybe Process,!*st)

	setProcessOwner		:: !UserId !UserId !ProcessId								!*st -> (!Bool,			!*st)
	setProcessStatus	:: !ProcessStatus !ProcessId								!*st -> (!Bool,			!*st)
	setProcessResult	:: !DynamicId !ProcessId									!*st -> (!Bool,			!*st)
	updateProcess		:: !ProcessStatus !(Maybe DynamicId) ![UserId] !ProcessId	!*st -> (!Bool,			!*st) 

instance ProcessDB HSt
/*
* Utility functions for creating process database entries.
*/
mkStaticProcessEntry	:: Workflow			UserId UserId ProcessStatus				-> Process
mkDynamicProcessEntry	:: String DynamicId	UserId UserId ProcessStatus ProcessId	-> Process
mkEmbeddedProcessEntry	:: ProcessId TaskId	TaskProperties ProcessStatus ProcessId	-> Process

instance toString ProcessStatus
