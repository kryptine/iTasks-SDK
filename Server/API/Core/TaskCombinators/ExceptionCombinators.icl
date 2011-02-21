implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath
import TSt, ProcessDB, Util

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e
try normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) exceptionTask
where
	exceptionTask tst=:{taskNr,iworld=iworld=:{IWorld|store,world}}
		# key			= iTaskId (tl taskNr) "exception"
		# (mbEx,tst)	= accIWorldTSt (loadValue key) tst
		= case mbEx of
			Just ex	
				= applyTask (handlerTask ex) tst
			Nothing			
				# (result, tst)	= applyTask normalTask tst
				= case result of
					//Handle exception if it matches
					TaskException (ex :: e^)
						# tst	= deleteTaskStates taskNr tst 						//Garbage collect
						# tst	= deleteSubProcesses (taskNrToString taskNr) tst
						# tst	= appIWorldTSt (storeValueAs SFDynamic key ex) tst	//Store the exception
						= applyTask (handlerTask ex) (resetSequence tst)			//Run the handler
					//Just pass through the result
					_
						= (result,tst) 
									
throw :: !e -> Task a | iTask a & TC e	
throw e = mkMonitorTask "Throw an exception" throw`
where
	throw` tst
		# tst		= setStatus (H1Tag [] [Text "Error, an uncaught exception was thrown"]) tst
		= (TaskException (dynamic e),tst) 