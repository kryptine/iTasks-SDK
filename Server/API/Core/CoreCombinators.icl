implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, Util, HTTP, GenUpdate, UserDB, Store, Types, Time, Text, Shared
import iTaskClass
from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB IWorld
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from CoreTasks			import return
from TuningCombinators	import :: Tag

derive class iTask ParallelTaskInfo, Control
// Generic functions for menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"
gEq{|Menu|} _ _				= abort "not implemented"
gEq{|MenuItem|} _ _			= abort "not implemented"
gVisualize{|Menu|} _ _		= abort "not implemented"
gVisualize{|MenuItem|} _ _	= abort "not implemented"
gVerify{|Menu|} _ _			= abort "not implemented"
gVerify{|MenuItem|} _ _		= abort "not implemented"
gUpdate{|Menu|} _ _			= abort "not implemented"
gUpdate{|MenuItem|} _ _		= abort "not implemented"
gDefaultMask{|Menu|} _		= abort "not implemented"
gDefaultMask{|MenuItem|} _	= abort "not implemented"

JSONEncode{|TaskContainer|} _ c		= dynamicJSONEncode c
JSONDecode{|TaskContainer|} _ [j:c]	= (dynamicJSONDecode j,c)
gUpdate{|TaskContainer|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= (InBodyTask (\_ _ -> return Void), ust)
gUpdate{|TaskContainer|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust
gDefaultMask{|TaskContainer|} _ _ = [Touched []]
gVerify{|TaskContainer|} _ _ vst = alwaysValid vst
gVisualize{|TaskContainer|} _ _ vst = ([TextFragment "task container"],vst)
gEq{|TaskContainer|} _ _ _ = False // containers are never equal

//Partial definition of equality of Control used only by (isMember StopParallel controls)
instance == (Control a) where
	(==) StopParallel	StopParallel	= True
	(==) _				_				= False

derive bimap Maybe, (,)

//Standard monadic bind
(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbfun = mkTask (taskTitle taska, taskDescription taska) init edit eval
where
	init taskNr iworld
		# (inita,iworld) = taska.initFun [0:taskNr] iworld
		= (TCBind (Left inita), iworld)
		
	//Event is targeted at first task of the bind
	edit taskNr ([0:steps],path,val) context=:(TCBind (Left cxta)) iworld	
		# (newCxta,iworld) = taska.editEventFun [0:taskNr] (steps,path,val) cxta iworld
		= (TCBind (Left newCxta), iworld)
	//Event is targeted at second task of the bind
	edit taskNr ([1:steps],path,val) context=:(TCBind (Right (vala,cxtb))) iworld
		//Compute the second task based on the result of the first
		= case fromJSON vala of
			Just a
				# taskb = taskbfun a
				# (newCxtb,iworld)	= taskb.editEventFun [1:taskNr] (steps,path,val) cxtb iworld
				= (TCBind (Right (vala,newCxtb)), iworld)
			Nothing
				= (context,iworld)
	//Event is targeted incorrectly, simply ignore and return
	edit taskNr event context iworld = (context, iworld)
	
	//Evaluate first task
	eval taskNr event tuiTaskNr imerge pmerge (TCBind (Left cxta)) iworld 
		//Adjust the target of a possible event
		# (resa, iworld) = taska.evalTaskFun [0:taskNr] (stepCommitEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge cxta iworld
		= case resa of
			TaskBusy tui newCxta
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) (TCBind (Left newCxta)), iworld)
			TaskFinished a
				//Directly continue with the second task
				# taskb = taskbfun a
				# (cxtb,iworld)		= taskb.initFun [1:taskNr] iworld
				# (resb,iworld)		= taskb.evalTaskFun [1:taskNr] Nothing (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtb iworld 
				= case resb of
					TaskBusy tui newCxtb	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCBind (Right (toJSON a,newCxtb))),iworld)
					TaskFinished b			= (TaskFinished b, iworld)
					TaskException e str		= (TaskException e str, iworld)
			TaskException e str
				= (TaskException e str, iworld)	
	//Evaluate second task
	eval taskNr event tuiTaskNr imerge pmerge (TCBind (Right (vala,cxtb))) iworld
		= case fromJSON vala of
			Just a
				# taskb				= taskbfun a
				# (resb, iworld)	= taskb.evalTaskFun [1:taskNr] (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge cxtb iworld 
				= case resb of
					TaskBusy tui newCxtb	= (TaskBusy (tuiOk 1 tuiTaskNr tui) (TCBind (Right (vala,newCxtb))),iworld)
					TaskFinished b			= (TaskFinished b, iworld)
					TaskException e str		= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt task value in bind", iworld)
	//Incorred state
	eval taskNr event tuiTaskNr imerge pmerge context iworld
		= (taskException "Corrupt task context in bind", iworld)
	
	//Check that when we want the TUI of a sub task that it is on the path
	tuiOk i [] tui		= tui
	tuiOk i [t:ts] tui	
		| i == t	= tui
		| otherwise	= Nothing 
	
(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

// Parallel composition
STATEKEY id 	:== iTaskId id "-state"
INFOKEY id		:== iTaskId id "-info"
CONTROLKEY id	:== iTaskId id "-control"

:: ResultSet
	= RSException !Dynamic !String
	| RSStopped
	| RSResults !ParallelMeta ![(!Int, !TaskResult Void, !SubTaskContext)]

parallel :: !d !s (ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d
parallel description initState resultFun initTasks = mkTask description init edit eval 
where
	//Create initial set of tasks and initial state
	init taskNr iworld=:{IWorld|timestamp}
		# (subContexts, nextIdx, iworld)	= initSubContexts taskNr 0 initTasks iworld  
		# meta								= {nextIdx = nextIdx, stop = False, stateChanged = timestamp, infoChanged = timestamp}
		# encState							= encodeState initState initState
		= (TCParallel encState meta subContexts, iworld)
	where
		initSubContexts taskNr i [] iworld = ([],i,iworld)
		initSubContexts taskNr i [t:ts] iworld
			# (s,iworld)			= initSubContext taskNr i t iworld
			# (ss, nextIdx, iworld) = initSubContexts taskNr (i + 1) ts iworld
			= ([(i,s):ss], nextIdx, iworld)
			
	//Direct the event to the right place
	edit taskNr ([s:steps],path,val) context=:(TCParallel encState meta subs) iworld	
		//Add the current state to the parallelVars scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskNr state meta iworld
		//Put the initial parallel task info overview in the parallelVars scope in iworld
		# iworld						= addParTaskInfo taskNr subs meta.infoChanged iworld 
		//Evaluate sub
		# (TCParallel encState meta subs,iworld)
			= case [sub \\ (i,sub) <- subs | i == s] of
			[sub] = case sub of
				//Active InBody task
				STCBody props (Just (encTask,subCxt))
					# task = fromJust (dynamicJSONDecode encTask)	//TODO: Add case for error
					# (newSubCxt,iworld) = task.editEventFun [s:taskNr] (steps,path,val) subCxt iworld
					# newSub = STCBody props (Just (encTask,newSubCxt))
					= (TCParallel encState meta [if (i == s) (i,newSub) (i,sub) \\(i,sub) <- subs],iworld)
				//Active Detached task
				STCDetached props (Just (encTask,subCxt))
					//Same pattern as inbody tasks
					# task = fromJust (dynamicJSONDecode encTask)	//TODO: Also add case for error
					# (newSubCxt,iworld) = task.editEventFun [s:taskNr] (steps,path,val) subCxt iworld
					# newSub = STCDetached props (Just (encTask,newSubCxt))
					= (TCParallel encState meta [if (i == s) (i,newSub) (i,sub) \\(i,sub) <- subs],iworld)
				//Task is either completed already or hidden
				_ 
					= (context,iworld)
			//The event is mistargeted
			_
				= (context,iworld)
		//Remove the current state from the parallelVars scope in iworld
		# (state,meta,iworld)	= removeParState taskNr meta iworld
		//Remove the task info overview
		# iworld						= removeParTaskInfo taskNr iworld
		//Process controls
		# (controls, iworld)			= removeControls initState taskNr iworld
		# (meta,_,subs,iworld)			= processControls taskNr meta controls [] subs iworld
		//Encode state
		# encState	= encodeState state initState
		= (TCParallel encState meta subs,iworld)
				
	edit taskNr event context iworld
		= (context,iworld)
	
	//Eval all tasks in the set (in left to right order)
	eval taskNr event tuiTaskNr imerge pmerge context=:(TCParallel encState meta subs) iworld
		//Add the current state to the parallelVars scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskNr state meta iworld
		//Put the initial parallel task info overview in the parallelVars scope in iworld
		# iworld						= addParTaskInfo taskNr subs meta.infoChanged iworld 
		//Evaluate the sub tasks
		# (resultset,iworld)			= evalSubTasks taskNr event tuiTaskNr imerge pmerge meta [] subs iworld
		//Remove the current state from the parallelVars scope in iworld
		# (state,meta,iworld)			= removeParState taskNr meta iworld
		//Remove the task info overview
		# iworld						= removeParTaskInfo taskNr iworld 
		//Remove parallel task info from the iworld
		= case resultset of
			//Exception
			RSException e str	= (TaskException e str, iworld)
			RSStopped 			= (TaskFinished (resultFun Stopped state), iworld)
			RSResults meta results 
				| allFinished results
					= (TaskFinished (resultFun AllRunToCompletion state), iworld)
				| otherwise
					# encState	= encodeState state initState
					# tui		= mergeTUIs pmerge tuiTaskNr (toDescr description) results
					# subs		= mergeContexts results
					= (TaskBusy tui (TCParallel encState meta subs), iworld)
		
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks taskNr event tuiTaskNr imerge pmerge meta results [] iworld
		//Check the stop flag
		| meta.stop = (RSStopped, iworld)
		| otherwise	= (RSResults meta results, iworld)
	evalSubTasks taskNr event tuiTaskNr imerge pmerge meta results [(idx,stcontext):stasks] iworld
		//Check the stop flag
		| meta.stop	= (RSStopped, iworld)
		//Evaluate subtask
		# (result,stcontext,iworld)	= case stcontext of
			(STCHidden props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# (result,iworld)	= task.evalTaskFun [idx:taskNr] (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge context iworld 
				= case result of
					TaskBusy tui context	= (TaskBusy tui context, STCHidden props (Just (hd (dynamicJSONEncode task), context)), iworld)
					TaskFinished _			= (TaskFinished Void, STCHidden props Nothing, iworld)
					TaskException e str		= (TaskException e str, STCHidden props Nothing, iworld)
			(STCBody props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# (result,iworld)	= task.evalTaskFun [idx:taskNr] (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge context iworld 
				= case result of
					TaskBusy tui context	= (TaskBusy tui context, STCBody props (Just (hd (dynamicJSONEncode task), context)), iworld)
					TaskFinished _			= (TaskFinished Void, STCBody props Nothing, iworld)
					TaskException e str		= (TaskException e str, STCBody props Nothing, iworld)
			(STCDetached props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				//Evaluate the task with a different current worker set
				# (curUser,iworld)	= switchCurrentUser props.ProcessProperties.managerProperties.worker iworld
				# (result,iworld)	= task.evalTaskFun [idx:taskNr] (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge context iworld 
				# (_,iworld)		= switchCurrentUser curUser iworld
				= case result of
					TaskBusy tui context	= (TaskBusy tui context, STCDetached props (Just (hd (dynamicJSONEncode task), context)), iworld)
					TaskFinished _			= (TaskFinished Void, STCDetached props Nothing, iworld)
					TaskException e str		= (TaskException e str, STCDetached props Nothing, iworld)		
			_
				//This task is already completed
				= (TaskFinished Void, stcontext, iworld)
		//Check for exception
		| isException result
			# (TaskException dyn str) = result
			= (RSException dyn str, iworld)
		//Append current result to results so far
		# results	= results ++ [(idx,result,stcontext)]
		//Process controls
		# (controls, iworld)			= removeControls initState taskNr iworld
		# (meta,results,stasks,iworld)	= processControls taskNr meta controls results stasks iworld
		//Evaluate remaining subtasks
		= evalSubTasks taskNr event tuiTaskNr imerge pmerge meta results stasks iworld
		
	initSubContext taskNr i taskContainer iworld=:{IWorld|timestamp}
		# subTaskNr = [i:taskNr]
		# stateShare = mkStateShare taskNr
		# controlShare = mkControlShare taskNr
		= case taskContainer of
			DetachedTask managerProps menu taskfun
				# task			= taskfun stateShare controlShare
				# processProps	= initProcessProperties subTaskNr timestamp managerProps menu task
				# (cxt,iworld)	= task.initFun subTaskNr iworld
				= (STCDetached processProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			//Window & dialogue now implemented as simple InBody tasks
			WindowTask title menu taskfun 
				# task			= taskfun stateShare controlShare
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= task.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			DialogTask title taskfun
				# task			= taskfun stateShare controlShare
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= task.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			InBodyTask taskfun
				# task			= taskfun stateShare controlShare
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= task.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			HiddenTask taskfun
				# task			= taskfun stateShare controlShare
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= task.initFun subTaskNr iworld
				= (STCHidden taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)	

	//Initialize a process properties record for administration of detached tasks
	initProcessProperties taskNr timestamp managerProps menu {Task|properties}
		= {ProcessProperties
		  |taskProperties = properties
		  ,managerProperties = managerProps
		  ,systemProperties
		  	= {SystemProperties
		  	  |taskId = taskNrToString taskNr
		  	  ,status = Running
		  	  ,issuedAt = timestamp
			  ,firstEvent = Nothing
			  ,latestEvent = Nothing
			  ,menu = menu
			  }
		   }
		   
	//Initialize a task properties record for administration of all other tasks
	initTaskProperties {Task|properties} = properties

	//IMPORTANT: The second argument is never used, but passed just to solve overloading
	encodeState :: !s s -> JSONNode | JSONEncode{|*|} s
	encodeState val _ = toJSON val
	
	//IMPORTANT: The second argument is never used, but passed just to solve overloading
	decodeState :: !JSONNode s -> s | JSONDecode{|*|} s
	decodeState encState _ = case fromJSON encState of
		Just val 	= val
		_			= abort "Could not decode parallel state"
		
	//Put the shared state in the scope
	addParState :: !TaskNr !s !ParallelMeta *IWorld -> *IWorld | TC s
	addParState taskNr state {stateChanged} iworld=:{parallelVars}
		= {iworld & parallelVars = 'Map'.put (STATEKEY taskNr) (dynamic (state,stateChanged) :: (s^,Timestamp)) parallelVars}
	
	removeParState :: !TaskNr !ParallelMeta !*IWorld -> (!s,!ParallelMeta,!*IWorld) | TC s
	removeParState taskNr meta iworld=:{parallelVars}
		= case 'Map'.get (STATEKEY taskNr) parallelVars of
			Just ((state,ts) :: (s^,Timestamp))	= (state,{meta & stateChanged = ts},iworld)
			_									= abort "Could not read parallel state"
	
	//Put a datastructure in the scope with info on all processes in this set
	addParTaskInfo :: !TaskNr ![(!Int,!SubTaskContext)] !Timestamp !*IWorld -> *IWorld
	addParTaskInfo taskNr subs ts iworld=:{parallelVars}
		= {iworld & parallelVars = 'Map'.put (INFOKEY taskNr) (dynamic (mkInfo subs,ts) :: ([ParallelTaskInfo],Timestamp) ) parallelVars}
	where
		mkInfo subs = [{ParallelTaskInfo|index = i, properties = props sub} \\ (i,sub) <- subs]
		props (STCHidden p _)	= Left p
		props (STCBody p _)		= Left p
		props (STCDetached p _)	= Right p		
	
	removeParTaskInfo :: !TaskNr !*IWorld -> *IWorld
	removeParTaskInfo taskNr iworld=:{parallelVars}
		= {iworld & parallelVars = 'Map'.del (INFOKEY taskNr) parallelVars}

	switchCurrentUser :: !User !*IWorld -> (!User,!*IWorld)
	switchCurrentUser newUser iworld=:{IWorld|currentUser}
		= (currentUser,{IWorld|iworld & currentUser = newUser})
	
	//IMPORTANT: first argument is never used, but passed just to solve overloading
	//Remove and return control values from the scope
	removeControls :: s !TaskNr !*IWorld -> ([Control s],!*IWorld) | iTask s
	removeControls _ taskNr iworld=:{parallelVars}
		= case 'Map'.get (CONTROLKEY taskNr) parallelVars of
			Just (controls :: [Control s^])
				= (controls, {iworld & parallelVars = 'Map'.del (CONTROLKEY taskNr) parallelVars})
			_
				= ([],iworld)
		
	processControls :: !TaskNr !ParallelMeta [Control s] [(!Int, !TaskResult Void, !SubTaskContext)] [(Int,!SubTaskContext)] !*IWorld -> (!ParallelMeta,![(!Int, !TaskResult Void, !SubTaskContext)], ![(Int,!SubTaskContext)],!*IWorld) | iTask s
	processControls taskNr meta [] results remaining iworld = (meta,results,remaining,iworld)
	processControls taskNr meta=:{nextIdx} [c:cs] results remaining iworld
		= case c of
			StopParallel
				= ({meta & stop = True},results,remaining,iworld)	//We don't process the remaining controls!
			AppendTask taskContainer
				# (context,iworld)	= initSubContext taskNr nextIdx taskContainer iworld
				# meta				= {meta & nextIdx = nextIdx + 1}
				# remaining			= remaining ++ [(nextIdx,context)]
				= processControls taskNr meta cs results remaining iworld
			RemoveTask idx
				# results			= [result \\ result=:(i,_,_) <- results | i <> idx]
				# remaining			= [sub \\ sub=:(i,_) <- remaining | i <> idx]
				= processControls taskNr meta cs results remaining iworld
			UpdateProperties idx newProps
				# results			= [if (i == idx) (i,res,updateProperties newProps context)
													 (i,res,context)
									  \\ (i,res,context) <- results]
				# remaining			= [if (i == idx) (i,updateProperties newProps context)
													 (i,context)
									  \\ (i,context) <- remaining]				 			
				= processControls taskNr meta cs results remaining iworld
			_
				= processControls taskNr meta cs results remaining iworld 		
	
	isException (TaskException _ _)	= True
	isException _					= False
	
	allFinished []							= True
	allFinished [(_,TaskFinished _,_):rs]	= allFinished rs
	allFinished _							= False
	
	updateProperties mprops (STCDetached props scontext)
		= (STCDetached {ProcessProperties|props & managerProperties = mprops} scontext)
	updateProperties _ context = context
	
	mergeContexts contexts
		= [(i,context) \\ (i,_,context) <- contexts]

	//User the parallel merger function to combine the user interfaces of all InBody tasks		
	mergeTUIs pmerge tuiTaskNr desc contexts
		= case tuiTaskNr of
			[]	= Just (pmerge {TUIParallel
				 		 |title = desc.TaskDescription.title
						 ,description = desc.TaskDescription.description
				 		 ,items = [tui \\ (i,TaskBusy (Just tui) _,STCBody _ _) <- contexts]
				 		 })
			//We want to show the TUI of a task inside the parallel set
			[t:ts]
				= case [tui \\ (i,TaskBusy (Just tui) _,_) <- contexts | i == t] of
					[tui]	= Just tui
					_		= Nothing
					
	//Create the access share for the shared state in the iworld's parallelVars
	mkStateShare :: TaskNr -> SymmetricShared s | iTask s
	mkStateShare taskNr = Shared read write timestamp
	where
		read iworld=:{parallelVars}
			= case 'Map'.get (STATEKEY taskNr) parallelVars of
				Just ((val,_) :: (s^,Timestamp))	= (Ok val, iworld)
				_									= (Error ("Could not read shared parallel state of task " +++ taskNrToString taskNr),iworld)

		write val iworld=:{parallelVars,timestamp}
			= (Ok Void, {iworld & parallelVars = 'Map'.put (STATEKEY taskNr) (dynamic (val,timestamp) :: (s^,Timestamp)) parallelVars })
		
		timestamp iworld=:{parallelVars}
			= case 'Map'.get (STATEKEY taskNr) parallelVars of
				Just ((_,ts) :: (s,Timestamp))		= (Ok ts, iworld)
				_									= (Error ("Could not read timestamp for shared parallel state of task " +++ taskNrToString taskNr),iworld)

	//Create the info/control share in the iworld's parallelVars
	mkControlShare :: TaskNr -> Shared [ParallelTaskInfo] [Control s] | iTask s
	mkControlShare taskNr = Shared read write timestamp
	where
		read iworld=:{parallelVars}
			= case 'Map'.get (INFOKEY taskNr) parallelVars of
				Just ((val,_) :: ([ParallelTaskInfo],Timestamp))	= (Ok val, iworld)
				_													= (Error ("Could not read parallel task info of " +++ taskNrToString taskNr), iworld)
		
		write val iworld=:{parallelVars}
			= (Ok Void, {iworld & parallelVars = 'Map'.put (CONTROLKEY taskNr) (dynamic val :: [Control s^]) parallelVars })				
		timestamp iworld=:{parallelVars}
			= case 'Map'.get (INFOKEY taskNr) parallelVars of
				Just ((_,ts) :: ([ParallelTaskInfo],Timestamp))	= (Ok ts, iworld)
				_												= (Error ("Could not read timestamp for parallel task info of " +++ taskNrToString taskNr), iworld)
			
spawnProcess :: !Bool !ManagerProperties !ActionMenu !(Task a) -> Task ProcessId | iTask a
spawnProcess gcWhenDone managerProperties menu task = undef /*TODO mkTask ("Spawn process", "Spawn a new task instance") id spawnProcess`
where
	spawnProcess` tst
		# (pid,_,tst)	= createTaskInstance (createThread task) True gcWhenDone managerProperties menu tst
		= (TaskFinished pid, tst)
*/
killProcess :: !ProcessId -> Task Void
killProcess pid = undef
/* TODO mkTask ("Kill process", "Kill a running task instance") init
where
	killProcess` tst 
		# tst = deleteTaskInstance pid tst
		= (TaskFinished Void, tst)
*/