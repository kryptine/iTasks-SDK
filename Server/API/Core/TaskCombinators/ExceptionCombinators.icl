implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath
import TSt, ProcessDB, Util

derive class iTask FileException, ParseException, CallException, DirectoryException, SharedException, FileError, RPCException
derive bimap Maybe,(,)

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask e
try normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) (exceptionTaskE,exceptionTaskC)
where
	exceptionTaskE tst=:{taskNr}
		# (mbEx,tst) = getException tst
		# (_,tst) = case mbEx of
			Just ex	= applyTaskEdit (handlerTask ex) tst
			Nothing = applyTaskEdit normalTask tst
		= tst

	exceptionTaskC tst=:{taskNr}
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
						# tst	= setException ex tst								//Store the exception
						= applyTaskCommit (handlerTask ex) (resetSequence tst)		//Run the handler
					//Just pass through the result
					_
						= (result,tst)
									
throw :: !e -> Task a | iTask a & TC e	
throw e = mkInstantTask "Throw an exception" throw`
where
	throw` tst = (TaskException (dynamic e),tst)

catchAll :: !(Task a) (Task a) -> Task a | iTask a
catchAll normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) (exceptionTaskE,exceptionTaskC)
where
	exceptionTaskE tst=:{taskNr}
		# (mbEx,tst) = getException tst
		# (_,tst) = case mbEx of
			Just True	= applyTaskEdit handlerTask tst
			Nothing		= applyTaskEdit normalTask tst
		= tst

	exceptionTaskC tst=:{taskNr}
		# (mbEx,tst) = getException tst
		= case mbEx of
			Just True
				= applyTaskCommit handlerTask tst
			Nothing
				# (result, tst)	= applyTaskCommit normalTask tst
				= case result of
					//Handle exception
					TaskException _
						# tst	= deleteTaskStates taskNr tst 						//Garbage collect
						# tst	= deleteSubProcesses (taskNrToString taskNr) tst
						# tst	= setException True tst								//set exception flag
						= applyTaskCommit handlerTask (resetSequence tst)			//Run the handler
					//Just pass through the result
					_
						= (result,tst)

getException :: !*TSt -> (!Maybe e,!*TSt) | JSONEncode{|*|}, JSONDecode{|*|}, TC e
getException tst=:{taskNr} = accIWorldTSt (getTaskStoreFor (tl taskNr) "exception") tst

setException :: !e !*TSt -> *TSt | JSONEncode{|*|}, JSONDecode{|*|}, TC e
setException ex tst=:{taskNr} = appIWorldTSt (setTaskStoreFor (tl taskNr) "exception" ex) tst

