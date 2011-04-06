implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath
import TSt, ProcessDB, Util
from iTasks import JSONEncode, JSONDecode

instance toString FileException
where
	toString (FileException path error) = case error of
		CannotOpen	= "Cannot open file '" +++ path +++ "'"
		CannotClose	= "Cannot close file '" +++ path +++ "'"
		IOError		= "Error reading/writing file '" +++ path +++ "'"
	
instance toString ParseException
where
	toString (CannotParse err) = "Parser error: " +++ err
	
instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err
	
instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err
	
instance toString RPCException
where
	toString (RPCException err) = "Error performing RPC call: " +++ err
	
instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

try :: !(Task a) (e -> Task a) -> Task a | iTask a & TC, toString e
try normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) (exceptionTaskE,exceptionTaskC)
where
	exceptionTaskE tst=:{taskNr}
		# (mbEx,tst) = getException tst
		# (_,tst) = case mbEx of
			Just ex	= applyTaskEdit (handlerTask ex) {tst & taskNr = incTaskNr taskNr}
			Nothing = applyTaskEdit normalTask tst
		= tst

	exceptionTaskC tst=:{taskNr}
		# (mbEx,tst) = getException tst
		= case mbEx of
			Just ex	
				= applyTaskCommit (handlerTask ex) Nothing {tst & taskNr = incTaskNr taskNr}
			Nothing			
				# (result, tst)	= applyTaskCommit normalTask Nothing tst
				= case result of
					//Handle exception if it matches
					TaskException (ex :: e^) _
						# tst	= deleteTaskStates taskNr tst 													//Garbage collect
						# tst	= deleteSubProcesses (taskNrToString taskNr) tst
						# tst	= setException ex tst															//Store the exception
						= applyTaskCommit (handlerTask ex) Nothing {(resetSequence tst) & taskNr = incTaskNr taskNr}	//Run the handler
					//Just pass through the result
					_
						= (result,tst)
									
throw :: !e -> Task a | iTask a & TC, toString e
throw e = mkInstantTask "Throw an exception" throw`
where
	throw` tst = (TaskException (dynamic e) (toString e),tst)

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll normalTask handlerTask = mkSequenceTask (taskTitle normalTask, taskDescription normalTask) (exceptionTaskE,exceptionTaskC)
where
	exceptionTaskE tst=:{taskNr}
		# (mbEx,tst) = getException tst
		# (_,tst) = case mbEx of
			Just err	= applyTaskEdit (handlerTask err) {tst & taskNr = incTaskNr taskNr}
			Nothing		= applyTaskEdit normalTask tst
		= tst

	exceptionTaskC tst=:{taskNr}
		# (mbEx,tst) = getException tst
		= case mbEx of
			Just err
				= applyTaskCommit (handlerTask err) Nothing {tst & taskNr = incTaskNr taskNr}
			Nothing
				# (result, tst)	= applyTaskCommit normalTask Nothing tst
				= case result of
					//Handle exception
					TaskException _ str
						# tst	= deleteTaskStates taskNr tst 													//Garbage collect
						# tst	= deleteSubProcesses (taskNrToString taskNr) tst
						# tst	= setException str tst															//set exception string
						= applyTaskCommit (handlerTask str) Nothing {(resetSequence tst) & taskNr = incTaskNr taskNr}	//Run the handler
					//Just pass through the result
					_
						= (result,tst)

getException :: !*TSt -> (!Maybe e,!*TSt) | TC  e
getException tst=:{taskNr}
	# (mbEx,tst) = accIWorldTSt (getTaskStoreFor (tl taskNr) "exception") tst
	# mbEx = case mbEx of
		Nothing			= Nothing
		Just ex = case ex of
			(ex :: e^)	= Just ex
			_			= Nothing
	= (mbEx,tst)
			
setException :: !e !*TSt -> *TSt | TC e
setException ex tst=:{taskNr} = appIWorldTSt (setTaskStoreFor (tl taskNr) "exception" (dynamic ex)) tst
