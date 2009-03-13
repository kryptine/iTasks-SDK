definition module InternaliTasksThreadHandling

// *********************************************************************************************************************************
// internally used function for Ajax and Client thread handling
// *********************************************************************************************************************************

import TSt

:: ThreadTable	:== [TaskThread]						// thread table is used for Ajax and OnClient options
:: TaskThread	=	{ thrTaskNr			:: !TaskNr		// task number to recover
					, thrUserId			:: !UserId		// which user has to perform the task
					, thrOptions		:: !Options		// options of the task
					, thrCallback		:: !String		// serialized callback function for the server
					, thrCallbackClient	:: !String		// serialized callback function for the client (optional, empty if not applicable)
					, thrKind			:: !ThreadKind 	// kind of thread
					, thrVersionNr		:: !Int			// version number of application when thread was created
					}
:: ThreadKind	=	ServerThread						// Thread which can only be executed on Server
				|	ClientServerThread					// Thread preferably to be executed on Client, but also runs on Server
				|	ClientThread						// Thread which can only be executed on the Client 
				|	ExceptionHandler					// Exception handler only works on server
				|	AnyThread							// Used for garbage collection
:: GlobalInfo	=	{ versionNr			:: !Int			// latest querie number of a user
					, newThread			:: !Bool		// is a new thread assigned to this user (used for Ajax)?
					, deletedThreads	:: ![TaskNr]	// are there threads deleted (used for Ajax)?
					}

instance == ThreadKind

// *********************************************************************************************************************************
// calculateTasks calculates the task tree, either from the root of the task tree or from the root of the parent thread
// depending on the IF_Ajax setting
// parameters: 	id of the user, global info of user, trace on?, main task expression to calculate, initial state
// returns: 	toServer,thrOwner,event,thrinfo,threads
// *********************************************************************************************************************************

// calculateTasks ::  currentUserId pversion traceOn maintask -> 
calculateTasks :: !Int !GlobalInfo !(LabeledTask a) !Int !*TSt -> ((!Bool,!Int,!TaskNr,!String,![TaskNr]),*TSt) | iData a		

// Setting of global information for a particular user

setPUserNr 					:: !Int !(Int -> Int) 				!*HSt 			-> (!GlobalInfo,!*HSt) 
clearIncPUser 				:: !Int !(Int -> Int) 				!*HSt 			-> (!GlobalInfo,!*HSt) 

// Session version management

setSVersionNr :: !Int !(Int -> Int) !*HSt -> (!Int,!*HSt) 

// Displaying thread information

showThreadNr 				:: !TaskNr 											-> String
showThreadTable 			:: !*TSt 											-> (![HtmlTag],!*TSt)					// watch it: the actual threadnumber stored is one level deaper, so [-1:nr] instead of nr !!

// Thread creation
administrateNewThread 		:: !UserId 					!*TSt 					-> *TSt
mkTaskThread 				:: !EvaluationOption !(Task a) 						-> Task a 	| iData a						

// Finding threads and evaluation of a thread

findThreadInTable 			:: !ThreadKind !TaskNr 		!*TSt 					-> *(Maybe (!Int,!TaskThread),!*TSt)	// find thread that belongs to given tasknr
findParentThread 			:: !TaskNr 					!*TSt 					-> *([TaskThread],*TSt)					// finds parent thread closest to given set of task numbers
evalTaskThread 				:: !TaskThread 										-> Task a 								// execute the thread !!!!

// Thread table management
/
insertNewThread 			:: !TaskThread 				!*TSt 					-> *TSt									// insert new thread in table
deleteSubTasksAndThreads 	:: !TaskNr 					!*TSt 					-> *TSt
/
// Version number control
setAppversion 				:: !(Int -> Int) 						!*HSt 		-> (!Int,!*HSt) 						// set new version number
getCurrentAppVersionNr 		:: 							!*TSt 					-> (!Int,!*TSt)							// get current version number




