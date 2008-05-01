implementation module iTasksExceptionHandling

// *********************************************************************************************************************************
// This module contains iTask combinators for Exception Handling
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdArray, StdTuple, StdFunc
import dynamic_string
import iTasksHandler, InternaliTasksThreadHandling, iTasksBasicCombinators

serializeExceptionHandler :: !.(!Dynamic -> Task .a) -> .String 
serializeExceptionHandler task = IF_ClientServer
									(IF_ClientTasks (abort "Cannot serialize exception handler on Client\n") (copy_to_string task))
									(copy_to_string task)				

deserializeExceptionHandler :: .String -> .(!Dynamic -> Task a.)
deserializeExceptionHandler thread = IF_ClientServer
										(IF_ClientTasks (abort "Cannot de-serialize exception handler thread on Client\n") (fetchException thread))	
										(fetchException thread)

fetchException thread = fst (copy_from_string {c \\ c <-: thread})


Raise :: e -> Task a | iCreate a & TC e	
Raise e = RaiseDyn (dynamic e)

(<^>) infix  1  :: !(e -> a) !(Task a) -> Task a | iData a & TC e			// create an exception Handler
(<^>) exceptionfun task = newTask "exceptionHandler" evalTask			
where
	evalTask tst=:{tasknr=mytasknr,options=myoptions,userId=myuserId,workflowLink}	// thread - task is not yet finished
	# (mbthread,tst)		= findThreadInTable ExceptionHandler mytasknr tst	// look if there is an exceptionhandler for this task
	| isNothing mbthread														// not yet, insert new entry		
		# (versionNr,tst)	= getCurrentAppVersionNr tst						// get current version number of the application
		# tst = insertNewThread 	{ thrTaskNr 		= mytasknr
									, thrUserId 		= myuserId
									, thrWorkflowLink	= workflowLink
									, thrOptions 		= myoptions
									, thrCallback 		= serializeExceptionHandler (Try exceptionfun)
									, thrCallbackClient = ""
									, thrKind			= ExceptionHandler
									, thrVersionNr		= versionNr
									} tst 
		= task tst																// do the regular task
	= task tst																	// do the regular task
	where
		Try :: !(e -> a) !Dynamic  -> Task a |  iCreateAndPrint a & TC e
		Try exceptionfun (exception :: e^) = catch1 							// handler for this type found
		with 
			catch1 tst=:{tasknr,userId,options}
			# tst 		= deleteSubTasksAndThreads mytasknr tst					// remove all work administrated below handler
			= return_V (exceptionfun exception) tst								// return exceptional result
		Try _ dynamicValue = catch2												// wrong handler
		with
			catch2 tst=:{tasknr} 
			# tst = deleteSubTasksAndThreads (tl tasknr) tst					// delete handler + task
			= RaiseDyn dynamicValue tst											// look for another handler

RaiseDyn :: !Dynamic -> Task a | iCreate a
RaiseDyn dynamicValue = raise
where
	raise tst=:{tasknr,staticInfo,activated}
	| not activated = (createDefault,tst)	
	# (mbthread,tst=:{hst})	= findParentThread tasknr tst						// look for parent threads
	# (version,hst)	 		= setPUserNr staticInfo.currentUserId id hst		// inspect global effects administration
	# mbthread				= [thread \\ thread <- mbthread 
									| thread.thrKind == ExceptionHandler		// which are exceptionhandlers
									&& not (isMember thread.thrTaskNr version.deletedThreads) // and not deleted by some global actions	
							  ] 
	| isEmpty mbthread		= abort	("\nException raised, but no handler installed or activa anymore\n")	// no handler installed
	= evalException (hd mbthread) dynamicValue {tst & html = BT [], hst = hst}	// yes, *finally*, we heave found an handler

evalException :: !TaskThread !Dynamic -> Task a 								// execute the thread !!!!
evalException entry=:{thrTaskNr,thrUserId,thrOptions,thrCallback,thrCallbackClient} dynval = evalException` 
where
	evalException` tst=:{tasknr,options,userId,html}									
	# (doClient,noThread)  				= IF_ClientTasks (True,thrCallbackClient == "") (False,False)  
	| doClient && noThread				= abort "Cannot execute thread on Client\n" 
	= IF_ClientTasks
		(abort "exception handling not implemeneted") 							//(deserializeThreadClient thrCallbackClient)
		(deserializeExceptionHandler thrCallback dynval	{tst & tasknr = thrTaskNr, options = thrOptions, userId = thrUserId,html = BT []})
