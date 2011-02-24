implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath
import TSt, ProcessDB, Util

derive class iTask FileException, ParseException, CallException, DirectoryException, SharedException, FileError
derive bimap Maybe,(,)

try :: !(Task a) !(e -> Task a) -> Task a | iTask a & iTask e
try normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) (exceptionTaskE,exceptionTaskC)
where
	exceptionTaskE tst
		# (mbEx,tst) = getException tst
		# (_,tst) = case mbEx of
			Just ex	= applyTaskEdit (handlerTask ex) tst
			Nothing = applyTaskEdit normalTask tst
		= tst

	exceptionTaskC tst=:{taskNr,iworld=iworld=:{IWorld|store,world}}
		# (mbEx,tst) = getException tst
		= case mbEx of
			Just ex	
				= applyTaskCommit (handlerTask ex) tst
			Nothing			
				# (result, tst)	= applyTaskCommit normalTask tst
				= case result of
					//Handle exception if it matches
					TaskException (ex :: e^)
						# tst	= deleteTaskStates taskNr tst 						//Garbage collect
						# tst	= deleteSubProcesses (taskNrToString taskNr) tst
						# tst	= appIWorldTSt (storeValueAs SFDynamic (key taskNr) ex) tst	//Store the exception
						= applyTaskCommit (handlerTask ex) (resetSequence tst)			//Run the handler
					//Just pass through the result
					_
						= (result,tst) 
						
	getException tst=:{taskNr} = accIWorldTSt (loadValue (key taskNr)) tst
	
	key :: !TaskNr -> String
	key taskNr = iTaskId (tl taskNr) "exception"
									
throw :: !e -> Task a | iTask a & TC e	
throw e = mkInstantTask "Throw an exception" throw`
where
	throw` tst = (TaskException (dynamic e),tst)
