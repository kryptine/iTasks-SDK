implementation module InternaliTasksThreadHandling

// *********************************************************************************************************************************
// internally used function for Ajax and Client thread handling
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdFunc, StdEnv
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import iDataTrivial, iDataFormlib
import InternaliTasksCommon, iTasksSettings, iTasksBasicCombinators, iTasksLiftingCombinators

derive gForm 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind, []
derive gUpd 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind, []
derive gParse 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind
derive gPrint 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind
derive gerda 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind
derive read 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind
derive write 	Lifespan, GarbageCollect, StorageFormat, Mode, Options, GlobalInfo, TaskThread, ThreadKind

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

// ******************************************************************************************************

	
instance == ThreadKind
where
	(==) ServerThread     		ServerThread 	   		= True
	(==) ClientThread    		ClientThread 	   		= True
	(==) ClientServerThread    	ClientServerThread 	   	= True
	(==) ExceptionHandler 		ExceptionHandler		= True
	(==) AnyThread    			_				 	   	= True
	(==) _ 						_ 						= False

instance toString ThreadKind
where
	toString ServerThread     	= "ServerThread"
	toString ClientThread    	= "ClientThread"
	toString ClientServerThread	= "ClientServerThread"
	toString ExceptionHandler 	= "ExceptionHandler"
	toString AnyThread    		= "AnyThread"
	toString _    				= "??? print error in thread"

// ******************************************************************************************************

setPUserNr :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 
setPUserNr user f hst	= setPUser user (\r -> {r & versionNr = f r.versionNr}) hst

clearIncPUser :: !Int !(Int -> Int) !*HSt -> (!GlobalInfo,!*HSt) 
clearIncPUser user f hst	= setPUser user (\r -> {r & newThread = False, deletedThreads = [], versionNr = f r.versionNr}) hst

setPUserNewThread :: !Int !*HSt -> (!GlobalInfo,!*HSt) 
setPUserNewThread user hst	= setPUser user (\r -> {r & newThread = True}) hst

addPUserDeletedThread :: !Int !TaskNr !*HSt -> (!GlobalInfo,!*HSt) 
addPUserDeletedThread user thread hst	= setPUser user (\r -> {r & deletedThreads = [thread:r.deletedThreads]}) hst

setPUser :: !Int !(GlobalInfo -> GlobalInfo) !*HSt -> (!GlobalInfo,!*HSt) 
setPUser user f hst	
= IF_ClientTasks
	(\hst -> (defaultGlobalInfo,hst))		// persistent version number cannot be set by client
	(\hst -> myStoreForm user f hst)
	hst
where
	myStoreForm user f hst 
	# (form,hst) = mkStoreForm (Init, pFormId (userVersionNr user) defaultGlobalInfo <@ NoForm) f hst
	= (form.value,hst)

	defaultGlobalInfo = { versionNr = 0, newThread = False, deletedThreads = []}


// ******************************************************************************************************
// Event handling for Ajax calls and Sapl handling on the client
// ******************************************************************************************************

// The following functions are defined to support "Ajax technologie" and Client site evaluation of i-Tasks.
// To make this possible, a part of the iTask task tree must be assigened to be a thread such that it can be evaluated as a stand-alone i-Task.
// The programmer has to decide which iTask should become a thread.
// For each event (iData triplet), the system will search for the thread to handle it.
// If a thread task is finished, the parent thread task is activated, and so on.
// Any action requiering the calculation of the Task Tree from scratch will be done one the server
// Watch it: the Client cannot create new Server threads

startAjaxApplication :: !Int !GlobalInfo !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),!*TSt) 		// determines which threads to execute and calls them..
startAjaxApplication thisUser versioninfo maintask tst=:{tasknr,options,html,trace,userId}
# tst					= copyThreadTableFromClient	versioninfo tst				// synchronize thread tables of client and server, if applicable

// first determine whether we should start calculating the task tree from scratch starting at the root

# (mbevent,tst)			= getTripletTaskNrs tst									// see if there are any events, i.e. triplets received
| isNothing mbevent																// no events
						= startFromRoot versioninfo tasknr [tasknr] "No events, page refreshed" maintask tst			
# event					= fromJust mbevent										// event found
# (table,tst)			= ThreadTableStorage id tst								// read thread table
| isEmpty table																	// events, but no threads, evaluate main application from scratch
						= startFromRoot versioninfo event [tasknr] "No threads, page refreshed" maintask tst			
# (mbthread,tst)		= findParentThread event tst							// look for thread to evaluate
| isEmpty mbthread																// no thread can be found, happens e.g. when one switches from tasks
						= startFromRoot versioninfo event [tasknr] "No matching thread, page refreshed" maintask tst			
# thread 				= hd mbthread											// thread found
| isMember thread.thrTaskNr versioninfo.deletedThreads							// thread has been deleted is some past, version conflict
	# tst				= copyThreadTableToClient tst							// copy thread table to client
	= ((True,defaultUser,event,"Task does not exist anymore, please refresh",[tasknr]), tst)
| versioninfo.newThread															// newthread added by someone
						= startFromRoot versioninfo event [tasknr] "New tasks added, page refreshed" maintask tst			
| not (isEmpty versioninfo.deletedThreads) 										// some thread has been deleted										
						= startFromRoot versioninfo event [tasknr] "Tasks deleted, page refreshed" maintask tst			
| thread.thrUserId <> thisUser													// updating becomes too complicated
						= startFromRoot versioninfo event [tasknr] ("Thread of user " <+++ thread.thrUserId <+++ ", page refreshed") maintask tst			

// ok, we have found a matching thread

# (_,tst=:{activated}) 	= evalTaskThread thread {tst & html = BT []}			// evaluate the thread
| not activated																	// thread / task not yet finished
	# tst				= copyThreadTableToClient tst							// copy thread table to client
	= ((False,thisUser,event,"",[thread.thrTaskNr]),tst)						// no further evaluation, aks user for more input

# (mbthread,tst)		= findParentThread (tl thread.thrTaskNr) tst			// look for thread to evaluate
= doParent mbthread maintask event [thread.thrTaskNr] {tst & html = BT [], options = options}				// more to evaluate, call thread one level higher
where
	doParent [] maintask event accu tst											// no more parents of current event, do main task
						= startFromRoot versioninfo event [tasknr:accu] "No more threads, page refreshed" maintask {tst & html = BT []}			

	doParent [parent:next] maintask event accu tst								// do parent of current thread
	| parent.thrUserId <> thisUser												// updating becomes too complicated
						= startFromRoot versioninfo event [tasknr:accu] ("Parent thread of user " <+++ parent.thrUserId <+++ ", page refreshed") maintask {tst & html = BT []}			

	# (_,tst=:{activated}) 	= evalTaskThread parent {tst & html = BT []}		// start parent
	| not activated																// parent thread not yet finished
		# tst				= copyThreadTableToClient tst						// copy thread table to client
		= ((False,thisUser,event, "",[parent.thrTaskNr:accu]),tst)				// no further evaluation, aks user for more input
	# (mbthread,tst)		= findParentThread (tl parent.thrTaskNr) tst		// look for thread to evaluate
	= doParent mbthread maintask event [parent.thrTaskNr:accu] {tst & options = options}// continue with grand parent ...

	getTripletTaskNrs :: !*TSt -> *(Maybe TaskNr,*TSt)								// get list of tasknr belonging to events received
	getTripletTaskNrs tst=:{hst = hst=:{states}}
	# (triplets,states) = getAllTriplets states
	= (lowestTaskNr [mkTasknr (getDigits s) \\ ((s,_,_),_) <- triplets | s%(0,5) == "iTask_"],{tst & hst = {hst & states = states}})
	where
		getDigits s = takeWhile ((<>) '-') (stl (dropWhile ((<>) '_') (mkList s)))
	
		mkTasknr list = reverse (map digitToInt [c \\ c <- list | isDigit c])
	
		lowestTaskNr [] 	= Nothing
		lowestTaskNr [x:xs] = Just (lowest x xs)									// lowest number gives highest position in tree
	
		lowest :: TaskNr [TaskNr] -> TaskNr
		lowest x [] 	= x
		lowest x [y:ys]
		| x < y = lowest x ys
		= lowest y ys

startFromRoot :: !GlobalInfo !TaskNr ![TaskNr] !String !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
startFromRoot versioninfo eventnr tasknrs message maintask tst
=	IF_ClientServer																// we are running client server
		(IF_ClientTasks
			(stopClient eventnr tasknrs message)								// client cannot evaluate from root of task tree, give it up
			(evaluateFromRoot versioninfo eventnr tasknrs message maintask) tst	// sever can evaluate from scratch
		)
	(evaluateFromRoot versioninfo eventnr tasknrs message maintask tst)			// ajax can evaluate from scratch as well
where
	stopClient :: !TaskNr ![TaskNr]  !String  !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
	stopClient eventnr tasknrs message tst
	= ((True,defaultUser,eventnr,message,tasknrs), tst)
	
	evaluateFromRoot :: !GlobalInfo !TaskNr ![TaskNr] !String !(Task a) !*TSt -> ((!Bool,!Int,TaskNr,!String,![TaskNr]),*TSt)
	evaluateFromRoot versioninfo eventnr tasknrs message maintask tst
	# tst					= deleteAllSubTasks versioninfo.deletedThreads tst	// delete subtasks being obsolute
	# (_,tst) 				= maintask tst										// evaluate main application from scratch
	# tst=:{activated}		= copyThreadTableToClient tst						// copy thread table to client, if applicable
	# message				= if activated "iTask application finished" message
	= (((True,defaultUser,eventnr,message,tasknrs), {tst & activated = activated}))

// ******************************************************************************************************
// Thread Creation and Deletion
// ******************************************************************************************************

mkTaskThread :: !SubPage !(Task a) -> Task a 	| iData a										
// wil only be called with IF_Ajax enabled
mkTaskThread UseAjax taska 
= IF_Ajax 																		// create an thread only if Ajax is enabled
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			(abort "Cannot make Server thread on Client\n")						// cannot create server thread on client
			(newTask "Server Thread" (mkTaskThread2 ServerThread taska))		// create client thread, but executed on server
		)
		(newTask "Ajax Thread" (mkTaskThread2 ServerThread taska))				// create a server thread, no clients
	)
	taska 																		// no threads made at all
mkTaskThread OnClient taska 
= IF_Ajax 																		// create threads only if Ajax is enabled
	(IF_ClientServer															// we running both client and server
		(IF_ClientTasks												
			(newTask "Client Thread" (mkTaskThread2 ClientThread  taska))		// create and execute client thread on client
			(newTask "Client Thread" (mkTaskThread2 ClientServerThread taska)) 	// create client thread, but executed on server
		)
		(newTask "Ajax Thread (no Client)" (mkTaskThread2 ServerThread taska))	// create a server thread, no clients
	)
	taska 																		// no threads made at all

mkTaskThread2 :: !ThreadKind !(Task a) -> Task a 								// execute a thread
mkTaskThread2 threadkind task = evalTask																
where
	evalTask tst=:{tasknr,activated,options,userId,staticInfo,workflowLink}		// thread - task is not yet finished
	# (mbthread,tst)	= findThreadInTable threadkind tasknr tst				// look if there is an entry for this task
	| isNothing mbthread														// not yet, insert new entry		
		# options 			= {options & tasklife = case threadkind of
													ServerThread 		= options.tasklife // staticInfo.threadTableLoc
													ClientServerThread 	= Client
													ClientThread 		= Client
													ExceptionHandler 	= options.tasklife  // staticInfo.threadTableLoc
													else 				= abort "Storing unexpected thread kind"}
		# (versionNr,tst)	= getCurrentAppVersionNr tst						// get current version number of the application
		# tst = insertNewThread 	{ thrTaskNr 		= tasknr
									, thrUserId 		= userId
									, thrWorkflowLink	= workflowLink
									, thrOptions 		= options
									, thrCallback 		= serializeThread task	
									, thrCallbackClient = serializeThreadClient task 
									, thrKind			= threadkind
									, thrVersionNr		= versionNr
									} tst 
		= evalTask tst															// try it again, entry point should now be there
	# (_,thread)		= fromJust mbthread										// entry point found
	# tst				= if (options.tasklife == Client && 					// if iTasks for this thread are stored on client
								(thread.thrOptions.tasklife <> Client ||		// but new thread is not to be stored on client 
								 staticInfo.currentUserId <> userId))			// or new thread is for someone else
							forceEvalutionOnServer id tst						// storing on client is no longer possible
	= evalTaskThread thread tst													// and evaluate it

forceEvalutionOnServer tst
=	IF_ClientServer																// we running both client and server
		(IF_ClientTasks												
			id																	// on client we cannot do anything
			forceEvalutionOnServer`												// force evaluation on server
			tst
		)
	tst
where
	forceEvalutionOnServer` tst=:{userId,tasknr} 
	# (mbparent,tst=:{hst})	= findNoClientParentThread tasknr tst
	| isNothing mbparent = {tst & hst = hst}									// cannot find parent, we should abort ????
	# parent 	= fromJust mbparent												// parent thread found which lifespan should be modified 
	# hst		= changeLifespanIData (iTaskId userId (tl parent.thrTaskNr) "") Client parent.thrOptions.tasklife hst
	# tst 		= changeLifespanThreadTable parent.thrTaskNr parent.thrOptions.tasklife {tst & hst = hst}
	= tst

	findNoClientParentThread tasknr tst
	# (mbparent,tst) 	= findParentThread tasknr tst
	| isEmpty mbparent 	= (Nothing,tst)
	# parent 			= hd mbparent										// thread found
	| parent.thrOptions.tasklife == Client = findNoClientParentThread (tl parent.thrTaskNr) tst
	= (Just parent,tst)

	changeLifespanThreadTable :: !TaskNr !Lifespan *TSt -> *TSt						// change lifespan of of indicated thread in threadtable
	changeLifespanThreadTable tasknr lifespan tst
	# (table,tst)	= ThreadTableStorage id tst										// read thread table on server
	# revtasknr		= reverse (tl tasknr)									
	# ntable 		= [{thread & thrOptions.tasklife = if (isChild revtasknr thread.thrTaskNr) lifespan thread.thrOptions.tasklife} \\ thread <- table]
	# (_,tst)		= ThreadTableStorage (\_ -> ntable) tst							// store thread table
	= tst

evalTaskThread :: !TaskThread -> Task a 										// execute the thread !!!!
evalTaskThread entry=:{thrTaskNr,thrUserId,thrOptions,thrCallback,thrCallbackClient,thrKind} = evalTaskThread` 
where
	evalTaskThread` tst=:{tasknr,options,userId,staticInfo,html}									
	# newThrOptions					= if (thrOptions.tasklife == Client && thrUserId <> staticInfo.currentUserId) 
											{thrOptions & tasklife = Temp}		// the information is not intended for this client, so dot store
											thrOptions
			
	# (a,tst=:{activated,html=nhtml}) 	
		= IF_ClientTasks	
			(case thrKind of		// we are running on Client, assume that IF_ClientServer and IF_Ajax is set
				 ClientThread 		= deserializeThreadClient thrCallbackClient
				 ClientServerThread	= deserializeThreadClient thrCallbackClient
				 ServerThread 		= abort "Cannot evaluate Server thread on Client\n"
				 else 				= abort "Thread administration error in evalTaskThread"
			)
			(case thrKind of		// we are running on the Server
				 ClientThread 		= abort "Cannot evaluate Client thread on Server\n"
				 ClientServerThread	= deserializeThread thrCallback
				 ServerThread 		= deserializeThread thrCallback
				 else 				= abort "Thread administration error in evalTaskThread"
			)
			{tst & tasknr = thrTaskNr, options = newThrOptions, userId = thrUserId,html = BT []} 
	| activated																	// thread is finished, delete the entry...
		# tst =  deleteThreads thrTaskNr {tst & html = html +|+ nhtml}			// remove thread from administration
		= (a,{tst & tasknr = tasknr, options = options, userId = userId})		// remove entry from table
	= (a,{tst & tasknr = tasknr, options = options, userId = userId,html = html +|+ DivCode (showTaskNr thrTaskNr) nhtml})

	
administrateNewThread :: !UserId !*TSt -> *TSt
administrateNewThread ouserId tst =: {tasknr,userId,options}
| ouserId == userId		= tst
# newTaskId				= iTaskId userId tasknr "_newthread"
# (chosen,tst=:{hst})	= liftHst (mkStoreForm  (Init,storageFormId options newTaskId False) id) tst	// first time here ?
| not chosen.value
	# (_,hst) 			= setPUserNewThread userId hst													// yes, new thread created
	# (_,tst)			= liftHst (mkStoreForm  (Init,storageFormId options newTaskId False) (\_ -> True)) {tst & hst = hst}
	= tst
= tst	

// ******************************************************************************************************
// Thread Table Storage Manipulation functions
// ******************************************************************************************************

// TO DO : Currently an unordered list is used, should become an ordered tree someday...
// TO DO: Put this stuf in another module

ThreadTableStorage :: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)		// used to store Tasknr of callbackfunctions / threads
ThreadTableStorage fun = handleTable
where
	handleTable tst 
	= IF_Ajax 																	// threads only used when Ajax is enabled
		(IF_ClientServer														// we running both client and server
			(IF_ClientTasks												
				ClientThreadTableStorage										// thread table on client
				ServerThreadTableStorage										// threadtable on server
				fun tst
			)
			(ServerThreadTableStorage fun tst)									// thread table on server when ajax used
		)
		(ServerThreadTableStorage fun tst)										// thread table used for exception handling only ???
//		(abort "Thread table storage only used when Ajax enabled")				// no threads made at all

ServerThreadTableStorage:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)	// used to store Tasknr of callbackfunctions / threads
ServerThreadTableStorage fun = handleTable
where
	handleTable tst=:{staticInfo} = ThreadTableStorageGen serverThreadTableId staticInfo.threadTableLoc fun tst 

	serverThreadTableId 		= "Application" +++  "-ThreadTable"

ClientThreadTableStorage:: !(ThreadTable -> ThreadTable) -> (Task ThreadTable)	// used to store Tasknr of callbackfunctions / threads
ClientThreadTableStorage fun = handleTable
where
	handleTable tst=:{staticInfo} = ThreadTableStorageGen (clientThreadTableId staticInfo.currentUserId) Client fun tst 

	clientThreadTableId userid	= "User" <+++ userid  <+++ "-ThreadTable"

ThreadTableStorageGen :: !String !Lifespan !(ThreadTable -> ThreadTable) -> (Task ThreadTable)		// used to store Tasknr of callbackfunctions / threads
ThreadTableStorageGen tableid lifespan fun = handleTable						// to handle the table on server as well as on client
where
	handleTable tst
	# (table,tst) = liftHst (mkStoreForm (Init,storageFormId 
						{ tasklife 		= lifespan
						, taskstorage 	= PlainString 
						, taskmode		= NoForm
						, gc			= Collect} tableid []) fun) tst
	= (table.value,tst)

copyThreadTableToClient ::  !*TSt -> !*TSt										// copies all threads for this user from server to client thread table
copyThreadTableToClient tst
=	IF_ClientServer										
		(IF_ClientTasks id copyThreadTableToClient` tst)						// only if we are on the server the copied can be made
		tst

copyThreadTableToClient` :: !*TSt -> !*TSt										// copies all threads for this user from server to client thread table
copyThreadTableToClient` tst
# ((mythreads,_),tst)	= splitServerThreadsByUser tst							// get thread table on server
# (clientThreads,tst)	= ClientThreadTableStorage (\_ -> mythreads) tst		// and store in client
= tst

splitServerThreadsByUser :: !*TSt -> !(!(!ThreadTable,!ThreadTable),!*TSt)		// get all threads from a given user from the server thread table
splitServerThreadsByUser tst=:{staticInfo}
# userid 				= staticInfo.currentUserId
# (serverThreads,tst)	= ServerThreadTableStorage id tst						// get thread table on server
# splitedthreads		= filterZip (\thr -> thr.thrUserId == userid &&			// only copy relevant part of thread table to client
							      (thr.thrKind == ClientServerThread || thr.thrKind == ClientThread)) serverThreads ([],[])
= (splitedthreads,tst)
where
	filterZip pred [] accu = accu
	filterZip pred [x:xs] (yes,no)
	| pred x = filterZip pred xs ([x:yes],no)
	| otherwise = filterZip pred xs (yes,[x:no])

copyThreadTableFromClient :: !GlobalInfo !*TSt -> !*TSt							// copies all threads for this user from client to server thread table
copyThreadTableFromClient versioninfo tst
=	IF_ClientServer										
		(IF_ClientTasks id (copyThreadTableFromClient` versioninfo) tst)		// only iff we are on the server the copied can be made
		tst

copyThreadTableFromClient` :: !GlobalInfo !*TSt -> !*TSt						// copies all threads for this user from client to server thread table
copyThreadTableFromClient` {newThread,deletedThreads} tst
# ((clienttableOnServer,otherClientsTable),tst)
						= splitServerThreadsByUser tst							// get latest thread table stored on server
# (clienttableOnClient,tst)		
						= ClientThreadTableStorage id tst						// get latest thread table stored on client
# clienttableOnClient	= case deletedThreads of
								[] -> 	clienttableOnClient						// remove threads in client table which have been deleted by global effects											
								_  -> 	[client 
										\\ client <- clienttableOnClient | not (isChildOf client.thrTaskNr deletedThreads) 
										]
# (clienttableOnClient,tst)		
						= ClientThreadTableStorage (\_ -> []) tst				// clear thread table stored on client
# tst					= deleteAllSubTasks deletedThreads tst					// remove corresponding tasks
# thrNrsActiveOnClient	= [thread.thrTaskNr \\ thread <- clienttableOnClient]	// all active thread numbers on client
# newClientsOnServer	= [thread \\ thread <- clienttableOnServer | not (isMember (thread.thrTaskNr) thrNrsActiveOnClient)]
# newtable				= newClientsOnServer ++ clienttableOnClient ++ otherClientsTable			// determine new thread situation
# (serverThreads,tst)	= ServerThreadTableStorage (\_ -> newtable) tst			// store table on server
= tst

findThreadInTable :: !ThreadKind !TaskNr !*TSt -> *(Maybe !(!Int,!TaskThread),!*TSt)// find thread that belongs to given tasknr
findThreadInTable threadkind tasknr tst
# (table,tst)	= ThreadTableStorage id tst										// read thread table
# pos			= lookupThread tasknr 0 table									// look if there is an entry for this task
| pos < 0		= (Nothing, tst)
= (Just (pos,table!!pos),tst) 
where
	lookupThread :: !TaskNr !Int !ThreadTable -> Int
	lookupThread tableKey n []			
		= -1																	// no, cannot find thread
	lookupThread tasknrToFind n [entry:next]
		| (showTaskNr tasknrToFind == showTaskNr entry.thrTaskNr &&	foundThread threadkind entry.thrKind) =  n	// yes, thread is administrated
		= lookupThread tasknrToFind (inc n) next

// TODO foundThread kan niet kloppen !!!

	foundThread ServerThread     		ServerThread 	   		= True
	foundThread ServerThread     		ClientServerThread 	   	= True
	foundThread ServerThread     		ClientThread	 	   	= True
	foundThread ClientThread    		ClientThread 	   		= True
	foundThread ClientThread    		ServerThread   			= True
	foundThread ClientThread    		ClientServerThread 	   	= True
	foundThread ClientServerThread    	ClientServerThread 	   	= True
	foundThread ClientServerThread    	ServerThread 	   		= True // IF_ClientServer (IF_ClientTasks False True) True
	foundThread ClientServerThread    	ClientThread 	   		= True
	foundThread ExceptionHandler 		ExceptionHandler		= True
	foundThread AnyThread    			_				 	   	= True
	foundThread _ 						_ 						= abort "ZOU NIET MOGEN\n" //False

insertNewThread :: !TaskThread !*TSt -> *TSt										// insert new thread in table
insertNewThread thread tst		
# (table,tst)	= ThreadTableStorage id tst										// read thread table
# (_,tst) 		= ThreadTableStorage (\_ -> [thread:table]) tst 				// insert the new thread
= tst

deleteThreads :: !TaskNr !*TSt -> *TSt
deleteThreads tasknr tst														// delete a thread and all its children
# (mbthread,tst)		= findThreadInTable AnyThread tasknr tst				// find the thread entry in the table
# mytasknr				= reverse tasknr
| isNothing mbthread	= deleteChildren mytasknr tst							// no entry, but delete children
# (pos,_)				= fromJust mbthread
# (_,tst)				= ThreadTableStorage (\table -> removeAt pos table) tst // remove entry
= deleteChildren mytasknr tst													// and all children
where
	deleteChildren mytasknr tst=:{staticInfo}
	# (table,tst)	 		= ThreadTableStorage id tst							// read thread table
	# allChildsPos			= [pos \\ entry <- table & pos <- [0..] | isChild mytasknr entry.thrTaskNr ]
	| isEmpty allChildsPos	= tst
	# otherUsersThreads		= [ ((table!!entry).thrUserId,(table!!entry).thrTaskNr) \\ entry <- allChildsPos | (table!!entry).thrUserId <> staticInfo.currentUserId]
	# tst					= administrateDeletedThreads otherUsersThreads tst 
	# table					= deleteChilds (reverse (sort allChildsPos)) table	// delete highest entries first !
	# (table,tst)	 		= ThreadTableStorage (\_ -> table) tst				// read thread table
	= tst

	deleteChilds [] table 			= table
	deleteChilds [pos:next] table 	= deleteChilds next (removeAt pos table)

isChild mytasknr mbchild = take (length mytasknr) (reverse mbchild) == mytasknr

isChildOf mytasknr [] = False
isChildOf mytasknr [x:xs] = isChild (reverse mytasknr) x || isChildOf mytasknr xs

administrateDeletedThreads [] tst = tst
administrateDeletedThreads [(user,tasknr):users] tst=:{hst}
# (_,hst)	= addPUserDeletedThread user tasknr hst								// administrate deleted thread in user administration
= administrateDeletedThreads users {tst & hst = hst}							// such that they are forced to recalculate the whole page


findParentThread ::  !TaskNr !*TSt -> *([TaskThread],*TSt)						// finds parent thread closest to given set of task numbers
findParentThread tasknr tst
# (table,tst)		= ThreadTableStorage id tst									// read thread table
| isEmpty table		= ([], tst)													// nothing in table, no parents
| length tasknr <= 1 = ([], tst)												// no tasks left up
# revtasknr			= reverse (tl tasknr)										// not relevant
# entries 			= filter (\entry -> revtasknr%(0,length entry.thrTaskNr - 2) == (reverse (tl entry.thrTaskNr))) table			// finds thread closest to this one
| isEmpty entries		= ([], tst)
= (sortBy compare entries,tst)
where
	compare :: !TaskThread !TaskThread -> Bool
	compare x y = length x.thrTaskNr > length y.thrTaskNr
	
// ******************************************************************************************************
// Serialization and De-Serialization of tasks to threads
// ******************************************************************************************************
//
// Watch it:  only works for one and the same application !!!!!
// Each time the application is recompiled, the existing administration has to be removed !!!!
// 
// The Client cannot make Server threads, so we have the following options:
// Server thread, made by Server, must be executed on Server
// Client thread, made by Server, also a Server thread is made: thread can be excuted either on Client or Server
// Sapl   thread, made by Client, can only be executed on Client
//
// IF_Ajax 
//	(IF_ClientServer 
//		(IF_ClientTasks (we are running in sapl on the client) 
//				 (we are running in clean on the server)
//		) (there is no client, all threads are on server) 
//  ) (there are no threads)

serializeThread :: !.(Task .a) -> .String
serializeThread task 
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks ""													// cannot create server thread on client
				 (copy_to_string task)									// store server thread
		)
		(copy_to_string task)											// store server thread
	)
	(abort "Threads cannot be created, Ajax is switched off\n")			// this call should not happen
						
deserializeThread :: !.String -> .(Task .a)
deserializeThread thread 
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (abort "Cannot de-serialize Server thread on Client\n")// this call should not happen 
				 (fst (copy_from_string {c \\ c <-: thread} ))			// retrieve server thread
		)
		(fst (copy_from_string {c \\ c <-: thread} ))					// retrieve server thread
	)
	(abort "De-serialization not possible when not running Ajax\n")		// this call should not happen



serializeThreadClient :: !(Task a) -> String
serializeThreadClient task
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (graph_to_sapl_string task)							// create client thread on client
				 (graph_to_sapl_string task)							// create client thread on server
		)
		""																// no clients, no need to create client thread
	)
	(abort "Threads cannot be created, Ajax is switched off\n")			// this call should not happen

deserializeThreadClient :: !.String -> .(Task .a)
deserializeThreadClient thread
= IF_Ajax 
	(IF_ClientServer 
		(IF_ClientTasks (deserializeSapl thread)								// retrieve client thread
				 (abort "Cannot de-serialize Client thread on Server\n")// this call should not happen
		)
		(abort "Cannot de-serialize Client thread on Server\n")			// this call should not happen
	)
	(abort "Threads should not be evaluated, Ajax is switched off\n")	// this call should not happen

deserializeSapl thread = string_to_graph thread

deleteSubTasksAndThreads :: !TaskNr !*TSt -> *TSt
deleteSubTasksAndThreads tasknr tst 
# tst=:{hst,userId,options}	= deleteThreads tasknr tst
| options.gc == NoCollect 	= tst
| otherwise					= {tst & hst = deleteIData (iTaskId userId tasknr "") hst}

deleteAllSubTasksAndThreads :: ![TaskNr] !*TSt -> *TSt
deleteAllSubTasksAndThreads [] tst = tst
deleteAllSubTasksAndThreads [tx:txs] tst 
# tst = deleteSubTasksAndThreads tx tst
= deleteAllSubTasksAndThreads txs tst

showThreadTable :: !*TSt -> (!HtmlCode,!*TSt)	// watch it: the actual threadnumber stored is one level deaper, so [-1:nr] instead of nr !!
showThreadTable tst=:{staticInfo}
# thisUser		= staticInfo.currentUserId
# (tableS,tst)	= ThreadTableStorage id tst													// read thread table from server
# tableS		= sortBy (\e1=:{thrTaskNr = t1} e2=:{thrTaskNr =t2} = t1 < t2) tableS
# (tableC,tst)	= IF_ClientServer
					(\tst -> ClientThreadTableStorage id tst)								// read thread table from client
					(\tst -> ([],tst)) tst

# tableC		= sortBy (\e1=:{thrTaskNr = t1} e2=:{thrTaskNr =t2} = t1 < t2) tableC
# bodyS			= 	if (isEmpty tableS)
					[]
					[showLabel "Server Thread Table: ",
					STable []	(   [[showTrace "UserNr:", showTrace "Kind:", showTrace "TaskNr:", showTrace "Created:"
									 ,showTrace "Storage"]] ++
									[	[ showText (toString entry.thrUserId)
										, showText (toString entry.thrKind)
										, showText (showThreadNr entry.thrTaskNr)
										, showText (toString entry.thrVersionNr)
										, showText (toString entry.thrOptions.tasklife)
										] 
										\\ entry <- tableS
									]
								),
					Hr []
					]
# bodyC			= if (isEmpty tableC)
					[]
					[showLabel ("Client User " +++ toString thisUser +++ " Thread Table: "),
					STable []	(   [[showTrace "UserNr:", showTrace "Kind:", showTrace "TaskNr:", showTrace "Created:"
									 ,showTrace "Storage"]] ++
									[	[ showText (toString entry.thrUserId)
										, showText (toString entry.thrKind)
										, showText (showThreadNr entry.thrTaskNr)
										, showText (toString entry.thrVersionNr)
										, showText (toString entry.thrOptions.tasklife)
										] 
										\\ entry <- tableC
									]
								),
					Hr []
					]
= (bodyS ++ bodyC,tst)

showThreadNr :: !TaskNr -> String
showThreadNr [-1]		= "Root"
showThreadNr [-1:is]	= showTaskNr is
showThreadNr else		= "*" <+++ showTaskNr else

