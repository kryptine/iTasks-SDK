definition module InternaliTasksThreadHandling

// *********************************************************************************************************************************
// internally used function for Ajax and Client thread handling
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import InternaliTasksCommon

:: ThreadTable	:== [TaskThread]						// thread table is used for Ajax and OnClient options
:: TaskThread	=	{ thrTaskNr			:: !TaskNr		// task number to recover
					, thrUserId			:: !UserId		// which user has to perform the task
					, thrWorkflowLink	:: !WorkflowLink// what was the name of workflow process it was part off
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

showThreadNr :: !TaskNr -> String
showThreadTable :: *TSt -> (HtmlCode,*TSt)	// watch it: the actual threadnumber stored is one level deaper, so [-1:nr] instead of nr !!

setPUser :: !Int !(GlobalInfo -> GlobalInfo) !*HSt -> (!GlobalInfo,!*HSt) 
setPUserNr :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 
clearIncPUser :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 

administrateNewThread :: UserId *TSt -> *TSt
mkTaskThread 				:: !SubPage !(Task a) -> Task a 	| iData a										
mkTaskThread2 				:: !ThreadKind !(Task a) -> Task a 										// execute a thread
evalTaskThread 				:: !TaskThread -> Task a 												// execute the thread !!!!
ThreadTableStorage 			:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)					// used to store Tasknr of callbackfunctions / threads
ServerThreadTableStorage	:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)					// used to store Tasknr of callbackfunctions / threads
ClientThreadTableStorage	:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)					// used to store Tasknr of callbackfunctions / threads
ThreadTableStorageGen 		:: !String !Lifespan !(ThreadTable -> ThreadTable) -> (Task ThreadTable)// used to store Tasknr of callbackfunctions / threads
copyThreadTableToClient 	::  !*TSt -> !*TSt														// copies all threads for this user from server to client thread table
splitServerThreadsByUser 	:: !*TSt -> !(!(!ThreadTable,!ThreadTable),!*TSt)						// get all threads from a given user from the server thread table
copyThreadTableFromClient 	:: !GlobalInfo !*TSt -> !*TSt											// copies all threads for this user from client to server thread table
findThreadInTable 			:: !ThreadKind !TaskNr *TSt -> *(Maybe (!Int,!TaskThread),*TSt)			// find thread that belongs to given tasknr
insertNewThread 			:: !TaskThread *TSt -> *TSt												// insert new thread in table
deleteThreads 				:: !TaskNr !*TSt -> *TSt
findParentThread 			::  !TaskNr !*TSt -> *([TaskThread],*TSt)								// finds parent thread closest to given set of task numbers

serializeThread 			:: !.(Task .a) -> .String
deserializeThread 			:: .String -> .(Task .a)
serializeThreadClient 		:: !(Task a) -> String
deserializeThreadClient 	:: .String -> .(Task .a)


deleteSubTasksAndThreads 	:: !TaskNr TSt -> TSt
deleteAllSubTasksAndThreads :: ![TaskNr] TSt -> TSt

