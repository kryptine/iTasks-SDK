definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe
import Types, HSt
from TSt import :: Workflow

/**
* Our local process type
*/
:: Process 		= {	id			:: !Int				//The process identification
				  , label		:: !String			//A descriptive label of the process
				  , owner		:: !UserId			//The "main" user of the process
				  , delegator	:: !UserId			//The user who issued the process
				  , status		:: !ProcessStatus	//The status of the process (updated after each run)
				  , users		:: ![UserId]		//A list of users involved in the process (updated after each run)
				  , process		:: !EITHER StaticProcessEntry DynamicProcessEntry
				  }

:: ProcessStatus =	Active
				 |	Suspended
				 |	Finished
				 |	Deleted
				 
:: StaticProcessEntry = {	workflow	:: !String	//The name of the workflow contained in the process
						}

:: DynamicProcessEntry = { result	:: !String		//A serialized final value of the task performed by the process 
						 , task		:: !String		//A serialized function of the task performed by the process
						 , parent	:: !ProcessId	//The process that created the current process
						 }

class ProcessDB st
where
	createProcess		:: !Process												!*st -> (!ProcessId,	!*st)
	deleteProcess		:: !ProcessId											!*st -> (!Bool,			!*st)
	getProcess			:: !ProcessId											!*st -> (!Maybe Process,!*st)
	getProcessForUser	:: !UserId !ProcessId									!*st -> (!Maybe Process,!*st)
	getProcesses		:: ![ProcessStatus]										!*st -> (![Process],	!*st)
	getProcessesById	:: ![ProcessId]											!*st -> (![Process],	!*st)
	getProcessesForUser	:: !UserId ![ProcessStatus]								!*st -> (![Process],	!*st)
	setProcessOwner		:: !UserId !UserId !ProcessId							!*st -> (!Bool,			!*st)
	setProcessStatus	:: !ProcessStatus !ProcessId							!*st -> (!Bool,			!*st)
	setProcessResult	:: !String !ProcessId									!*st -> (!Bool,			!*st)
	updateProcess		:: !ProcessStatus !(Maybe String) ![UserId] !ProcessId	!*st -> (!Bool,			!*st) 

instance ProcessDB HSt
/*
* Utility functions for creating process database entries.
*/
mkStaticProcessEntry	:: Workflow UserId UserId ProcessStatus					-> Process
mkDynamicProcessEntry	:: String String UserId UserId ProcessStatus ProcessId	-> Process

instance toString ProcessStatus
