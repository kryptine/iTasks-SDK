implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath, Map, JSON
import Task, TaskContext, ProcessDB, Util

derive class iTask FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
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
	
instance toString WorkOnException
where
	toString WorkOnProcessNotFound		= "Error working on process: cannot find process"
	toString WorkOnDependencyCycle		= "Error working on process: cycle in dependencies detected"
	
try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try normalTask handlerTaskFun = mkTask (taskTitle normalTask, taskDescription normalTask) init edit eval
where
	normalTaskFuncs = toTaskFuncs normalTask
	
	init taskNr iworld
		# (cxt,iworld)	= normalTaskFuncs.initFun [0:taskNr] iworld
		= (TCTry (Left cxt), iworld)
		
	edit taskNr (TaskEvent [0:steps] (path,val)) context=:(TCTry (Left cxtNormal)) iworld
		# (newCxtNormal,iworld) = normalTaskFuncs.editEventFun [0:taskNr] (TaskEvent steps (path,val)) cxtNormal iworld
		= (TCTry (Left newCxtNormal), iworld)

	edit taskNr (TaskEvent [1:steps] (path,val)) context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case (fromJSON encEx) of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (newCxtHandler,iworld) = handler.editEventFun [1:taskNr] (TaskEvent steps (path,val)) cxtHandler iworld
				= (TCTry (Right (encEx, newCxtHandler)), iworld)
			Nothing
				= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)

	//Normal execution still possible
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context=:(TCTry (Left cxtNormal)) iworld
		# (result, iworld) = normalTaskFuncs.evalTaskFun [0:taskNr] normalTask.Task.properties (stepEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge mmerge cxtNormal iworld
		= case result of
			TaskBusy tui actions newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) actions (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException (ex :: e^) str 
				//Run the handler immediately
				# handler				= handlerTaskFun ex
				# handlerFuncs			= toTaskFuncs handler
				# (cxtHandler,iworld)	= handlerFuncs.initFun [1:taskNr] iworld
				# (result,iworld)		= handlerFuncs.evalTaskFun [1:taskNr] handler.Task.properties (stepEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (toJSON ex,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			//Other exception (pass through
			TaskException ex str
				= (TaskException ex str, iworld)
			
	//Handling the exception
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler		= handlerTaskFun e
				# handlerFuncs	= toTaskFuncs handler
				# (result,iworld) = handlerFuncs.evalTaskFun [1:taskNr] handler.Task.properties (stepEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in try" ,iworld)
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context iworld
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
		# (cxt, iworld) = normalTaskFuncs.initFun [0:taskNr] iworld
		= (TCTry (Left cxt), iworld)
		
	edit taskNr (TaskEvent [0:steps] (path,val)) context=:(TCTry (Left cxtNormal)) iworld
		# (newCxtNormal,iworld) = normalTaskFuncs.editEventFun [0:taskNr] (TaskEvent steps (path,val)) cxtNormal iworld
		= (TCTry (Left newCxtNormal), iworld)

	edit taskNr (TaskEvent [1:steps] (path,val)) context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case (fromJSON encEx) of
			Just e
				# handler = toTaskFuncs (handlerTaskFun e)
				# (newCxtHandler,iworld) = handler.editEventFun [1:taskNr] (TaskEvent steps (path,val)) cxtHandler iworld
				= (TCTry (Right (encEx, newCxtHandler)), iworld)
			Nothing
				= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)

	//Normal execution still possible
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context=:(TCTry (Left cxtNormal)) iworld
		# (result, iworld) = normalTaskFuncs.evalTaskFun [0:taskNr] normalTask.Task.properties (stepEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge mmerge cxtNormal iworld
		= case result of
			TaskBusy tui actions newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) actions (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException _ str 
				//Run the handler immediately
				# handler				= handlerTaskFun str
				# handlerFuncs			= toTaskFuncs handler
				# (cxtHandler,iworld)	= handlerFuncs.initFun [1:taskNr] iworld
				# (result,iworld)		= handlerFuncs.evalTaskFun [1:taskNr] handler.Task.properties (stepEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (toJSON str,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)

	//Handling the exception
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler			= handlerTaskFun e
				# handlerFuncs		= toTaskFuncs handler
				# (result,iworld)	= handlerFuncs.evalTaskFun [1:taskNr] handler.Task.properties (stepEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in catchAll" ,iworld)
				
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context iworld
		= (taskException "Corrupt task context in catchAll", iworld)
			