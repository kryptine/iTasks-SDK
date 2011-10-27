implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, OSError, File, FilePath, Map, JSON
import Task, TaskContext, TaskStore, Util

derive class iTask FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
derive class iTask FileError

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
	toString WorkOnNotFound				= "Error working on process: cannot find process"
	toString WorkOnEvalError			= "Error working on process: evaluation error"
	toString WorkOnDependencyCycle		= "Error working on process: cycle in dependencies detected"
	
try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try normalTask handlerTaskFun = mkTask (taskMeta normalTask) init edit eval
where
	normalTaskFuncs = taskFuncs normalTask
	
	init taskNr iworld
		# (cxt,iworld)	= normalTaskFuncs.initFun [0:taskNr] iworld
		= (TCTry (Left cxt), iworld)
		
	edit taskNr event context=:(TCTry (Left cxtNormal)) iworld
		= case stepEvent 0 (Just event) of
			Nothing
				= (context, iworld)
			Just event 
				# (newCxtNormal,iworld) = normalTaskFuncs.editFun [0:taskNr] event cxtNormal iworld
				= (TCTry (Left newCxtNormal), iworld)

	edit taskNr event context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case stepEvent 1 (Just event) of
			Nothing
				= (context, iworld)
			Just event
				= case (fromJSON encEx) of
					Just e
						# handler = taskFuncs (handlerTaskFun e)
						# (newCxtHandler,iworld) = handler.editFun [1:taskNr] event cxtHandler iworld
						= (TCTry (Right (encEx, newCxtHandler)), iworld)
					Nothing
						= (context, iworld)


	//Normal execution still possible
	eval taskNr _ event tuiTaskNr _ _ context=:(TCTry (Left cxtNormal)) iworld
		# (ilayout,playout)	= taskLayouters normalTask 
		# (result, iworld)	= normalTaskFuncs.evalFun [0:taskNr] normalTask.Task.meta (stepEvent 0 event) (stepTarget 0 tuiTaskNr) ilayout playout cxtNormal iworld
		= case result of
			TaskBusy tui actions newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) actions (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException (ex :: e^) str 
				//Run the handler immediately
				# handler				= handlerTaskFun ex
				# handlerFuncs			= taskFuncs handler
				# (ilayout,playout)		= taskLayouters handler
				# (cxtHandler,iworld)	= handlerFuncs.initFun [1:taskNr] iworld
				# (result,iworld)		= handlerFuncs.evalFun [1:taskNr] handler.Task.meta (stepEvent 1 event) (stepTarget 1 tuiTaskNr) ilayout playout cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (toJSON ex,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			//Other exception (pass through
			TaskException ex str
				= (TaskException ex str, iworld)
			
	//Handling the exception
	eval taskNr _ event tuiTaskNr _ _ context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler			= handlerTaskFun e
				# handlerFuncs		= taskFuncs handler
				# (ilayout,playout)	= taskLayouters handler
				# (result,iworld)	= handlerFuncs.evalFun [1:taskNr] handler.Task.meta (stepEvent 1 event) (stepTarget 1 tuiTaskNr) ilayout playout cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in try" ,iworld)
	eval taskNr _ event tuiTaskNr _ _ context iworld
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
catchAll normalTask handlerTaskFun = mkTask (taskMeta normalTask) init edit eval
where
	normalTaskFuncs = taskFuncs normalTask

	init taskNr iworld
		# (cxt, iworld) = normalTaskFuncs.initFun [0:taskNr] iworld
		= (TCTry (Left cxt), iworld)
		
	edit taskNr (TaskEvent [0:steps] (path,val)) context=:(TCTry (Left cxtNormal)) iworld
		# (newCxtNormal,iworld) = normalTaskFuncs.editFun [0:taskNr] (TaskEvent steps (path,val)) cxtNormal iworld
		= (TCTry (Left newCxtNormal), iworld)

	edit taskNr (TaskEvent [1:steps] (path,val)) context=:(TCTry (Right (encEx, cxtHandler))) iworld
		= case (fromJSON encEx) of
			Just e
				# handler = taskFuncs (handlerTaskFun e)
				# (newCxtHandler,iworld) = handler.editFun [1:taskNr] (TaskEvent steps (path,val)) cxtHandler iworld
				= (TCTry (Right (encEx, newCxtHandler)), iworld)
			Nothing
				= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)

	//Normal execution still possible
	eval taskNr _ event tuiTaskNr _ _ context=:(TCTry (Left cxtNormal)) iworld
		# (ilayout,playout)	= taskLayouters normalTask
		# (result, iworld)	= normalTaskFuncs.evalFun [0:taskNr] normalTask.Task.meta (stepEvent 0 event) (stepTarget 0 tuiTaskNr) ilayout playout cxtNormal iworld
		= case result of
			TaskBusy tui actions newCxtNormal
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) actions (TCTry (Left newCxtNormal)), iworld)
			TaskFinished a
				= (TaskFinished a, iworld)
			//Matching exception
			TaskException _ str 
				//Run the handler immediately
				# handler				= handlerTaskFun str
				# handlerFuncs			= taskFuncs handler
				# (ilayout,playout)		= taskLayouters handler
				# (cxtHandler,iworld)	= handlerFuncs.initFun [1:taskNr] iworld
				# (result,iworld)		= handlerFuncs.evalFun [1:taskNr] handler.Task.meta (stepEvent 1 event) (stepTarget 1 tuiTaskNr) ilayout playout cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (toJSON str,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)

	//Handling the exception
	eval taskNr _ event tuiTaskNr _ _ context=:(TCTry (Right (encEx,cxtHandler))) iworld
		= case fromJSON encEx of
			Just e
				# handler			= handlerTaskFun e
				# handlerFuncs		= taskFuncs handler
				# (ilayout,playout)	= taskLayouters handler
				# (result,iworld)	= handlerFuncs.evalFun [1:taskNr] handler.Task.meta (stepEvent 1 event) (stepTarget 1 tuiTaskNr) ilayout playout cxtHandler iworld
				= case result of
					TaskBusy tui actions newCxtHandler	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCTry (Right (encEx,newCxtHandler))), iworld)
					TaskFinished a						= (TaskFinished a,iworld)
					TaskException e str					= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt exception value in catchAll" ,iworld)
				
	eval taskNr _ event tuiTaskNr _ _ context iworld
		= (taskException "Corrupt task context in catchAll", iworld)
			