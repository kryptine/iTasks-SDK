implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath, Map, JSON
import Task, TaskContext, ProcessDB, Util

derive class iTask FileException, ParseException, CallException, SharedException, RPCException, OSException, ChoiceException
derive class iTask FileError
derive bimap Maybe,(,)

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
	
instance toString ChoiceException
where
	toString _ = "Cannot choose from empty option list"

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try normalTask handlerTaskFun = mkTask (taskTitle normalTask, taskDescription normalTask) init edit eval
where
	normalTaskFuncs = toTaskFuncs normalTask
	
	init taskNr iworld
		= normalTaskFuncs.initFun [0:taskNr] iworld

	edit taskNr ([0:steps],path,val) context=:(TCTry (Left cxtNormal)) iworld
		# (newCxtNormal,iworld) = normalTaskFuncs.editEventFun [0:taskNr] (steps,path,val) cxtNormal iworld
		= (TCTry (Left newCxtNormal), iworld)

	edit taskNr ([1:steps],path,val) context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case (fromJSON encEx) of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (newCxtHandler,iworld) = handler.editEventFun [1:taskNr] (steps,path,val) cxtHandler iworld
				= (TCTry (Right (encEx, newCxtHandler)), iworld)
			Nothing
				= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)

	//Normal execution still possible
	eval taskNr event tuiTaskNr imerge pmerge context=:(TCTry (Left cxtNormal)) iworld
		# (result, iworld) = normalTaskFuncs.evalTaskFun [0:taskNr] (stepCommitEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge cxtNormal iworld
		= case result of
			TaskBusy tui newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException (ex :: e^) str 
				//Run the handler immediately
				# handler = toTaskFuncs (handlerTaskFun ex)
				# (cxtHandler,iworld) = handler.initFun [1:taskNr] iworld
				# (result,iworld) = handler.evalTaskFun [1:taskNr] (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtHandler iworld
				= case result of
					TaskBusy tui newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCTry (Right (toJSON ex,newCxtHandler))), iworld)
					TaskFinished a				= (TaskFinished a,iworld)
					TaskException e str			= (TaskException e str, iworld)
			//Other exception (pass through
			TaskException ex str
				= (TaskException ex str, iworld)
			
	//Handling the exception
	eval taskNr event tuiTaskNr imerge pmerge context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (result,iworld) = handler.evalTaskFun [1:taskNr] (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtHandler iworld
				= case result of
					TaskBusy tui newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a				= (TaskFinished a,iworld)
					TaskException e str			= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in try" ,iworld)
	eval taskNr event tuiTaskNr imerge pmerge context iworld
		= (taskException "Corrupt task context in try", iworld)
	
tuiOk i [] tui		= tui
tuiOk i [t:ts] tui	
	| i == t	= tui
	| otherwise	= Nothing
									
throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask "Throw an exception" eval
where
	eval taskNr iworld = (TaskException (dynamic e) (toString e),iworld)


//TODO: Rewrite to a common base function for try and catchAll 	
catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll normalTask handlerTaskFun = mkTask (taskTitle normalTask, taskDescription normalTask) init edit eval
where
	normalTaskFuncs = toTaskFuncs normalTask

	init taskNr iworld
		= normalTaskFuncs.initFun [0:taskNr] iworld

	edit taskNr ([0:steps],path,val) context=:(TCTry (Left cxtNormal)) iworld
		# (newCxtNormal,iworld) = normalTaskFuncs.editEventFun [0:taskNr] (steps,path,val) cxtNormal iworld
		= (TCTry (Left newCxtNormal), iworld)

	edit taskNr ([1:steps],path,val) context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case (fromJSON encEx) of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (newCxtHandler,iworld) = handler.editEventFun [1:taskNr] (steps,path,val) cxtHandler iworld
				= (TCTry (Right (encEx, newCxtHandler)), iworld)
			Nothing
				= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)

	//Normal execution still possible
	eval taskNr event tuiTaskNr imerge pmerge context=:(TCTry (Left cxtNormal)) iworld
		# (result, iworld) = normalTaskFuncs.evalTaskFun [0:taskNr] (stepCommitEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge cxtNormal iworld
		= case result of
			TaskBusy tui newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException _ str 
				//Run the handler immediately
				# handler = toTaskFuncs (handlerTaskFun str)
				# (cxtHandler,iworld) = handler.initFun [1:taskNr] iworld
				# (result,iworld) = handler.evalTaskFun [1:taskNr] (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtHandler iworld
				= case result of
					TaskBusy tui newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCTry (Right (toJSON str,newCxtHandler))), iworld)
					TaskFinished a				= (TaskFinished a,iworld)
					TaskException e str			= (TaskException e str, iworld)

	//Handling the exception
	eval taskNr event tuiTaskNr imerge pmerge context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (result,iworld) = handler.evalTaskFun [1:taskNr] (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtHandler iworld
				= case result of
					TaskBusy tui newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a				= (TaskFinished a,iworld)
					TaskException e str			= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in try" ,iworld)
	eval taskNr event tuiTaskNr imerge pmerge context iworld
		= (taskException "Corrupt task context in try", iworld)
									
