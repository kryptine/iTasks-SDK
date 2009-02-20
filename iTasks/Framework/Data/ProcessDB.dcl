definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdEnv, StdGeneric, StdMaybe
import Types, HSt, TSt

/**
* Our local process type
*/
:: Process 		= {	id			:: !Int				//The process identification
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

:: DynamicProcessEntry = { label	:: !String		//A descriptive label of the process
						 , result	:: !String		//A serialized final value of the task performed by the process 
						 , task		:: !String		//A serialized function of the task performed by the process
						 , parent	:: !Int			//The process that created the current process
						 }

/**
* A process database handle
*/
:: *ProcessDB

/**
* Overloaded functions for opening a process database in different
* environments
*/
class ProcessDBEnv env
where
	openProcessDB		:: !*env -> (!*ProcessDB, !*env)
	closeProcessDB		:: !*ProcessDB !*env -> *env

instance ProcessDBEnv HSt

createProcess		:: !Process					!*ProcessDB	-> (!Int, 			!*ProcessDB)
deleteProcess		:: !Int						!*ProcessDB	-> (!Bool,			!*ProcessDB)
getProcess			:: !Int						!*ProcessDB -> (!Maybe Process,	!*ProcessDB)
getProcessForUser	:: !Int !Int				!*ProcessDB	-> (!Maybe Process,	!*ProcessDB)
getProcesses		:: ![ProcessStatus]			!*ProcessDB -> (![Process],		!*ProcessDB)
getProcessesById	:: ![Int]					!*ProcessDB -> (![Process],		!*ProcessDB)
getProcessesForUser	:: !Int ![ProcessStatus]	!*ProcessDB -> (![Process],		!*ProcessDB)
setProcessOwner		:: !Int !Int !Int			!*ProcessDB	-> (!Bool,			!*ProcessDB)
setProcessStatus	:: !ProcessStatus !Int		!*ProcessDB	-> (!Bool,			!*ProcessDB)
setProcessResult	:: !String !Int				!*ProcessDB -> (!Bool,			!*ProcessDB)

//Combined update of status, result, and users
updateProcess		:: !ProcessStatus !(Maybe String) ![UserId] !ProcessId !*ProcessDB -> (!Bool,!*ProcessDB) 

/*
* Utility functions for creating process database entries.
*/
createStaticProcessEntry	:: Workflow UserId UserId ProcessStatus -> Process
createDynamicProcessEntry	:: String String UserId UserId ProcessStatus Int -> Process

instance toString ProcessStatus
