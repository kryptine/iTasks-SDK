implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, Util, HTTP, GenUpdate, UserDB, Store, SystemTypes, Time, Text, Shared
import iTaskClass, InteractionTasks
from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..)
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB IWorld
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from CoreTasks			import return
from TuningCombinators	import :: Tag

:: TaskList s
	= GlobalTaskList			//The global list of task instances
	| ParallelTaskList !TaskId	//The list of task instances of a parallel node

topLevelTasks :: (TaskList Void)
topLevelTasks = GlobalTaskList

derive class iTask ParallelTaskInfo, ParallelControl, Control, TaskList, TaskGUI
derive bimap Maybe, (,)

instance toString (TaskList s)
where
	toString GlobalTaskList				= "global"
	toString (ParallelTaskList taskid)	= "parallel_" +++ taskid
	
//Standard monadic bind
(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbfun = mkTask (taskTitle taska, taskDescription taska) init edit eval
where
	taskaFuncs = toTaskFuncs taska
	
	init taskNr iworld
		# (inita,iworld) = taskaFuncs.initFun [0:taskNr] iworld
		= (TCBind (Left inita), iworld)
		
	//Event is targeted at first task of the bind
	edit taskNr ([0:steps],path,val) context=:(TCBind (Left cxta)) iworld	
		# (newCxta,iworld) = taskaFuncs.editEventFun [0:taskNr] (steps,path,val) cxta iworld
		= (TCBind (Left newCxta), iworld)
	//Event is targeted at second task of the bind
	edit taskNr ([1:steps],path,val) context=:(TCBind (Right (vala,cxtb))) iworld
		//Compute the second task based on the result of the first
		= case fromJSON vala of
			Just a
				# taskbfun = toTaskFuncs (taskbfun a)
				# (newCxtb,iworld)	= taskbfun.editEventFun [1:taskNr] (steps,path,val) cxtb iworld
				= (TCBind (Right (vala,newCxtb)), iworld)
			Nothing
				= (context,iworld)
	//Event is targeted incorrectly, simply ignore and return
	edit taskNr event context iworld = (context, iworld)
	
	//Evaluate first task
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge (TCBind (Left cxta)) iworld 
		//Adjust the target of a possible event
		# (resa, iworld) = taskaFuncs.evalTaskFun [0:taskNr] taska.Task.properties (stepCommitEvent 0 event) (stepTUITaskNr 0 tuiTaskNr) imerge pmerge mmerge cxta iworld
		= case resa of
			TaskBusy tui actions newCxta
				= (TaskBusy (tuiOk 0 tuiTaskNr tui) actions (TCBind (Left newCxta)), iworld)
			TaskFinished a
				//Directly continue with the second task
				# taskb				= taskbfun a
				# taskbfuncs		= toTaskFuncs taskb
				# (cxtb,iworld)		= taskbfuncs.initFun [1:taskNr] iworld
				# (resb,iworld)		= taskbfuncs.evalTaskFun [1:taskNr] taskb.Task.properties Nothing (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtb iworld 
				= case resb of
					TaskBusy tui actions newCxtb	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCBind (Right (toJSON a,newCxtb))),iworld)
					TaskFinished b					= (TaskFinished b, iworld)
					TaskException e str				= (TaskException e str, iworld)
			TaskException e str
				= (TaskException e str, iworld)	
	//Evaluate second task
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge (TCBind (Right (vala,cxtb))) iworld
		= case fromJSON vala of
			Just a
				# taskb				= taskbfun a
				# taskbfuncs		= toTaskFuncs taskb
				# (resb, iworld)	= taskbfuncs.evalTaskFun [1:taskNr] taskb.Task.properties (stepCommitEvent 1 event) (stepTUITaskNr 1 tuiTaskNr) imerge pmerge mmerge cxtb iworld 
				= case resb of
					TaskBusy tui actions newCxtb	= (TaskBusy (tuiOk 1 tuiTaskNr tui) actions (TCBind (Right (vala,newCxtb))),iworld)
					TaskFinished b					= (TaskFinished b, iworld)
					TaskException e str				= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt task value in bind", iworld)
	//Incorred state
	eval taskNr _ event tuiTaskNr imerge pmerge mmerge context iworld
		= (taskException "Corrupt task context in bind", iworld)
	
	//Check that when we want the TUI of a sub task that it is on the path
	tuiOk i [] tui		= tui
	tuiOk i [t:ts] tui	
		| i == t	= tui
		| otherwise	= Nothing 

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

(>>+) infixl 1 :: !(Task a) !(TermFunc a b) -> Task b | iTask a & iTask b
(>>+) task=:{Task|type} termF = case type of
	ActionTask actionTaskF	= {Task|task & type = NormalTask (actionTaskF termF)}
	_						= task >>= \r -> showInformation (taskTitle task,taskDescription task) [] r >>+ termF
	
noActions :: (TermFunc a b) | iTask a & iTask b
noActions = const (UserActions [])

// Parallel composition
STATEKEY id 	:== "parallel_" +++ taskNrToString id +++ "-state"
INFOKEY id		:== "parallel_" +++ taskNrToString id +++ "-info"
CONTROLKEY id	:== "parallel_" +++ taskNrToString id +++ "-control"

:: ResultSet
	= RSException !Dynamic !String
	| RSStopped
	| RSResults !ParallelMeta ![(!Int, !TaskResult ParallelControl, !SubTaskContext)]
	
:: Control s
	= AppendTask		!Int !(TaskContainer s)			// append and additional task to be run in parallel as well
	| RemoveTask		!Int							// remove the task with indicated index from the set
	| UpdateProperties	!Int !ManagerProperties			// update the properties of a task


parallel :: !d !s (ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d
parallel description initState resultFun initTasks = mkTask description init edit eval 
where
	//Create initial set of tasks and initial state
	init taskNr iworld=:{IWorld|timestamp}
		# (subContexts, nextIdx, iworld)	= initSubContexts taskNr 0 initTasks iworld  
		# meta								= {nextIdx = nextIdx, stateChanged = timestamp, infoChanged = timestamp}
		# encState							= encodeState initState initState
		= (TCParallel encState meta subContexts, iworld)
	where
		initSubContexts taskNr i [] iworld = ([],i,iworld)
		initSubContexts taskNr i [t:ts] iworld
			# (s,iworld)			= initSubContext taskNr i t iworld
			# (ss, nextIdx, iworld) = initSubContexts taskNr (i + 1) ts iworld
			= ([(i,s):ss], nextIdx, iworld)
			
	//Direct the event to the right place
	edit taskNr ([s:steps],path,val) context=:(TCParallel encState meta subs) iworld=:{IWorld|latestEvent=parentLatestEvent}
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
					# taskfuncs = toTaskFuncs` task
					# (newSubCxt,iworld) = taskfuncs.editEventFun [s:taskNr] (steps,path,val) subCxt iworld
					# newSub = STCBody props (Just (encTask,newSubCxt))
					= (TCParallel encState meta [if (i == s) (i,newSub) (i,sub) \\(i,sub) <- subs],iworld)
				//Active Detached task
				STCDetached props (Just (encTask,subCxt))
					//Same pattern as inbody tasks
					# task = fromJust (dynamicJSONDecode encTask)	//TODO: Also add case for error
					# taskfuncs = toTaskFuncs` task
					// change latest event timestamp for detached process
					# iworld = {IWorld|iworld & latestEvent = props.systemProperties.SystemProperties.latestEvent}
					# (newSubCxt,iworld) = taskfuncs.editEventFun [s:taskNr] (steps,path,val) subCxt iworld
					# iworld = {IWorld|iworld & latestEvent = parentLatestEvent}
					# newSub = STCDetached props (Just (encTask,newSubCxt))
					= (TCParallel encState meta [if (i == s) (i,newSub) (i,sub) \\(i,sub) <- subs],iworld)
				//Task is either completed already or hidden
				_ 
					= (context,iworld)
			//The event is mistargeted
			_
				= (context,iworld)
		//Remove the current state from the parallelVars scope in iworld
		# (state,meta,iworld)			= removeParState taskNr meta iworld
		//Remove the task info overview
		# iworld						= removeParTaskInfo taskNr iworld
		//Encode state
		# encState	= encodeState state initState
		= (TCParallel encState meta subs,iworld)
				
	edit taskNr event context iworld
		= (context,iworld)
	
	//Eval all tasks in the set (in left to right order)
	eval taskNr {taskDescription} event tuiTaskNr imerge pmerge mmerge context=:(TCParallel encState meta subs) iworld
		//Add the current state to the parallelVars scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskNr state meta iworld
		//Add the initial control structure to the parallelVars scope in iworld
		# iworld						= addParControl state taskNr meta iworld
		//Put the initial parallel task info overview in the parallelVars scope in iworld
		# iworld						= addParTaskInfo taskNr subs meta.infoChanged iworld
		//Evaluate the sub tasks
		# (resultset,iworld)			= evalSubTasks taskNr event tuiTaskNr imerge pmerge mmerge meta [] subs iworld
		//Remove the current state from the parallelVars scope in iworld
		# (state,meta,iworld)			= removeParState taskNr meta iworld
		//Remove the control structure from the parallelVars scope in iworld
		# (meta,iworld)					= removeParControl state taskNr meta iworld
		//Remove the task info overview from the parallelVars scropr in iworld
		# iworld						= removeParTaskInfo taskNr iworld 
		//Remove parallel task info
		= case resultset of
			//Exception
			RSException e str	= (TaskException e str, iworld)
			RSStopped 			= (TaskFinished (resultFun Stopped state), iworld)
			RSResults meta results 
				| allFinished results
					= (TaskFinished (resultFun AllRunToCompletion state), iworld)
				| otherwise
					# encState			= encodeState state initState
					# (tui,actions)		= mergeTUIs pmerge mmerge tuiTaskNr taskDescription results
					# subs				= mergeContexts results
					= (TaskBusy tui actions (TCParallel encState meta subs), iworld)
		
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks taskNr event tuiTaskNr imerge pmerge mmerge meta results [] iworld
		= (RSResults meta results, iworld)
	evalSubTasks taskNr event tuiTaskNr imerge pmerge mmerge meta results [(idx,stcontext):stasks] iworld=:{IWorld|latestEvent=parentLatestEvent,timestamp}
		//Evaluate subtask
		# (result,stcontext,iworld)	= case stcontext of
			(STCHidden props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# taskfuncs			= toTaskFuncs` task
				# (result,iworld)	= taskfuncs.evalTaskFun [idx:taskNr] task.Task.properties (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge mmerge context iworld 
				= case result of
					TaskBusy tui actions context	= (TaskBusy tui actions context, STCHidden props (Just (encTask, context)), iworld)
					TaskFinished r					= (TaskFinished r, STCHidden props Nothing, iworld)
					TaskException e str				= (TaskException e str, STCHidden props Nothing, iworld)
			(STCBody props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# taskfuncs			= toTaskFuncs` task
				# (result,iworld)	= taskfuncs.evalTaskFun [idx:taskNr] task.Task.properties (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge mmerge context iworld 
				= case result of
					TaskBusy tui actions context	= (TaskBusy tui actions context, STCBody props (Just (encTask, context)), iworld)
					TaskFinished r					= (TaskFinished r, STCBody props Nothing, iworld)
					TaskException e str				= (TaskException e str, STCBody props Nothing, iworld)
			(STCDetached props (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# taskfuncs			= toTaskFuncs` task
				//Evaluate the task with a different current worker set & changed latest event timestamp
				# (curUser,iworld)	= switchCurrentUser props.ProcessProperties.managerProperties.worker iworld
				# iworld			= {IWorld|iworld & latestEvent = props.systemProperties.SystemProperties.latestEvent}
				# (result,iworld)	= taskfuncs.evalTaskFun [idx:taskNr] task.Task.properties (stepCommitEvent idx event) (stepTUITaskNr idx tuiTaskNr) imerge pmerge mmerge context iworld 
				# (_,iworld)		= switchCurrentUser curUser iworld
				# iworld			= {IWorld|iworld & latestEvent = parentLatestEvent}
				//Update first/latest event if request is targeted at this detached process
				# props = case tuiTaskNr of
					[t] | t == idx	=	{props & systemProperties =
						{ props.systemProperties
						& firstEvent	= Just (fromMaybe timestamp props.systemProperties.firstEvent)
						, latestEvent	= Just timestamp
						}}
					_				= props
				= case result of
					TaskBusy tui actions context	= (TaskBusy tui actions context, STCDetached props (Just (encTask, context)), iworld)
					TaskFinished r					= (TaskFinished r, STCDetached (markFinished props) Nothing, iworld)
					TaskException e str				= (TaskException e str, STCDetached (markExcepted props) Nothing, iworld)		
			_
				//This task is already completed
				= (TaskFinished Continue, stcontext, iworld)
		//Check for exception
		| isException result
			# (TaskException dyn str) = result
			= (RSException dyn str, iworld)
		//Check for stop result (discards any pending additions/removals to the set, since they will be pointless anyway)
		| isStopped result
			= (RSStopped, iworld)
		//Append current result to results so far
		# results	= results ++ [(idx,result,stcontext)]
		//Process controls
		# (controls, iworld)			= getControls initState taskNr iworld
		# (meta,results,stasks,iworld)	= processControls taskNr meta controls results stasks iworld
		//Evaluate remaining subtasks
		= evalSubTasks taskNr event tuiTaskNr imerge pmerge mmerge meta results stasks iworld
		
	initSubContext taskNr i taskContainer iworld=:{IWorld|timestamp}
		# subTaskNr = [i:taskNr]
		= case taskContainer of
			(DetachedTask managerProps, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# processProps	= initProcessProperties subTaskNr timestamp managerProps task
				# (cxt,iworld)	= funcs.initFun subTaskNr iworld
				= (STCDetached processProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			//Window & dialogue now implemented as simple InBody tasks
			(WindowTask title, taskfun) 
				# (task,funcs)	= mkSubTask taskfun
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= funcs.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			(DialogTask title, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= funcs.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			(BodyTask, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= funcs.initFun subTaskNr iworld
				= (STCBody taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
			(HiddenTask, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# taskProps		= initTaskProperties task
				# (cxt,iworld)	= funcs.initFun subTaskNr iworld
				= (STCHidden taskProps (Just (hd (dynamicJSONEncode task), cxt)), iworld)
	where
		// apply task list reference to taskfun & convert to 'normal' (non-action) tasks
		mkSubTask taskfun
			# task			= taskfun (ParallelTaskList (taskNrToString taskNr)) 
			# funcs			= toTaskFuncs task
			# task			= {Task|task & type = NormalTask funcs}
			= (task,funcs)

	//Initialize a process properties record for administration of detached tasks
	initProcessProperties taskNr timestamp managerProps {Task|properties}
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
			Just ((state,ts) :: (s^,Timestamp))	= (state,{meta & stateChanged = ts},{iworld & parallelVars = 'Map'.del (STATEKEY taskNr) parallelVars})
			_									= abort "Could not read parallel state"
	
	//IMPORTANT: first argument is never used, but passed just to solve overloading
	//Put the initial control structure in the scope
	addParControl :: s !TaskNr !ParallelMeta *IWorld -> *IWorld | TC s
	addParControl _ taskNr meta=:{nextIdx} iworld=:{parallelVars}
		= {iworld & parallelVars = 'Map'.put (CONTROLKEY taskNr) (dynamic (nextIdx,[]) :: (Int,[Control s^])) parallelVars}

	removeParControl :: s !TaskNr !ParallelMeta *IWorld -> (!ParallelMeta,!*IWorld) | TC s
	removeParControl _ taskNr meta iworld=:{parallelVars}
		= case 'Map'.get (CONTROLKEY taskNr) parallelVars of
			Just ((nextIdx,_) :: (Int,[Control s^]))	= ({meta & nextIdx = nextIdx},{iworld & parallelVars = 'Map'.del (CONTROLKEY taskNr) parallelVars})
			_											= abort "Could not read parallel control"
	
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
	
	//Remove and return control values from the scope
	//IMPORTANT: first argument is never used, but passed just to solve overloading
	getControls :: s !TaskNr !*IWorld -> ([Control s], !*IWorld) | TC s
	getControls _ taskNr iworld=:{parallelVars}
		= case 'Map'.get (CONTROLKEY taskNr) parallelVars of
			Just ((nextIdx, controls) :: (Int,[Control s^]))
				= (controls, {iworld & parallelVars = 'Map'.put (CONTROLKEY taskNr) (dynamic (nextIdx,[]) :: (Int,[Control s^])) parallelVars})
			_
				= abort "Could not load parallel control data"
	
	processControls :: !TaskNr !ParallelMeta [Control s] [(!Int, !TaskResult ParallelControl, !SubTaskContext)] [(Int,!SubTaskContext)] !*IWorld -> (!ParallelMeta,![(!Int, !TaskResult ParallelControl, !SubTaskContext)], ![(Int,!SubTaskContext)],!*IWorld) | iTask s
	processControls taskNr meta [] results remaining iworld = (meta,results,remaining,iworld)
	processControls taskNr meta [c:cs] results remaining iworld
		= case c of
			AppendTask idx taskContainer
				# (context,iworld)	= initSubContext taskNr idx taskContainer iworld
				# remaining			= remaining ++ [(idx,context)]
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
	
	isStopped	(TaskFinished Stop)	= True
	isStopped	_					= False
	
	allFinished []							= True
	allFinished [(_,TaskFinished _,_):rs]	= allFinished rs
	allFinished _							= False
	
	markFinished properties=:{ProcessProperties|systemProperties}
		= {ProcessProperties|properties & systemProperties = {SystemProperties|systemProperties & status = Finished}}
	markExcepted properties=:{ProcessProperties|systemProperties}
		= {ProcessProperties|properties & systemProperties = {SystemProperties|systemProperties & status = Excepted}}
	
	updateProperties mprops (STCDetached props scontext)
		= (STCDetached {ProcessProperties|props & managerProperties = mprops} scontext)
	updateProperties _ context = context
	
	mergeContexts contexts
		= [(i,context) \\ (i,_,context) <- contexts]

	//User the parallel merger function to combine the user interfaces of all InBody tasks		
	mergeTUIs pmerge mmerge tuiTaskNr desc contexts
		= case tuiTaskNr of
			[]
				# tuis		= [if (isHidden subContext) Nothing (Just tui) \\ (i,TaskBusy (Just tui) _ _,subContext) <- contexts | not (isDetached subContext)]
				# actions	= [a \\ (i,TaskBusy (Just tui) a _,subContext) <- contexts | not (isDetached subContext)]
				# (tui,actions) =
					pmerge {TUIParallel
				 		 |title = desc.TaskDescription.title
						 ,description = desc.TaskDescription.description
				 		 ,items = zip (tuis,actions)
				 		 }
				= (Just tui, actions)
				where
					isHidden (STCHidden _ _) = True
					isHidden _ = False
					isDetached (STCDetached _ _) = True
					isDetached _ = False
			//We want to show the TUI of one of the detached tasks in this set
			[t]
				= case [(tui,actions,props) \\ (i,TaskBusy (Just tui) actions _,STCDetached props _) <- contexts | i == t] of
					[(tui,actions,props)]
						= (Just (mmerge {TUIMain|content = tui, actions = actions, properties = props}),[])
					_
						= (Nothing,[])
				
				
			//We want to show the TUI of a task inside the parallel set
			[t:ts]
				= case [(tui,actions) \\ (i,TaskBusy (Just tui) actions _,_) <- contexts | i == t] of
					[(tui,actions)]	= (Just tui,actions)
					_				= (Nothing,[])
					
	//Create the info/control share in the iworld's parallelVars
	mkControlShare :: TaskNr -> ReadOnlyShared [ParallelTaskInfo]
	mkControlShare taskNr = ReadWriteShared ["parallelControl_" +++ taskNrToString taskNr] read write timestamp
	where
		read iworld=:{parallelVars}
			= case 'Map'.get (INFOKEY taskNr) parallelVars of
				Just ((val,_) :: ([ParallelTaskInfo],Timestamp))	= (Ok val, iworld)
				_													= (Error ("Could not read parallel task info of " +++ taskNrToString taskNr), iworld)
		
		write _ iworld
			= (Ok Void, iworld)
						
		timestamp iworld=:{parallelVars}
			= case 'Map'.get (INFOKEY taskNr) parallelVars of
				Just ((_,ts) :: ([ParallelTaskInfo],Timestamp))	= (Ok ts, iworld)
				_												= (Error ("Could not read timestamp for parallel task info of " +++ taskNrToString taskNr), iworld)
			
	toTaskFuncs` {Task|type} = case type of
		NormalTask fs	= fs
		_				= abort "action task in parallel"


/**
* Get the shared state of a task list
*/
taskListState :: (TaskList s) -> Shared s | TC s
taskListState tasklist = ReadWriteShared [identity] read write timestamp
where
	identity 	= toString tasklist +++ "-state"
	
	read iworld=:{parallelVars}
		= case 'Map'.get identity parallelVars of
				Just ((val,_) :: (s^,Timestamp))	= (Ok val, iworld)
				_									= (Error ("Could not read shared parallel state of task list " +++ identity),iworld)
	
	write val iworld=:{parallelVars,timestamp}
			= (Ok Void, {iworld & parallelVars = 'Map'.put identity (dynamic (val,timestamp) :: (s^,Timestamp)) parallelVars })
	
	timestamp iworld=:{parallelVars}
			= case 'Map'.get identity parallelVars of
				Just ((_,ts) :: (s,Timestamp))		= (Ok ts, iworld)
				_									= (Error ("Could not read timestamp for shared state of task list " +++ identity),iworld)

/**
* Get the properties share of a task list
*/
taskListProperties	:: (TaskList s) -> Shared [ParallelTaskInfo]
taskListProperties tasklist = ReadWriteShared [identity] read write timestamp
where
	identity	= toString tasklist +++ "-info"
	
	read iworld=:{parallelVars}
		= case 'Map'.get identity parallelVars of
			Just ((val,_) :: ([ParallelTaskInfo],Timestamp))	= (Ok val, iworld)
			_													= (Error ("Could not read parallel task info of " +++ identity), iworld)
		
	write val iworld=:{parallelVars,timestamp} //TODO
		= (Ok Void, iworld)
			
	timestamp iworld=:{parallelVars}
		= case 'Map'.get identity parallelVars of
			Just ((_,ts) :: ([ParallelTaskInfo],Timestamp))	= (Ok ts, iworld)
			_												= (Error ("Could not read timestamp for parallel task info of " +++ identity), iworld)
		


/**
* Add a task to a task list
*/
appendTask :: !(TaskContainer s) !(TaskList s)	-> Task Int | TC s
appendTask container tasklist = mkInstantTask "Append a task to a task list" appendTask`
where
	identity	= toString tasklist
	key			= identity +++ "-control"
	
	appendTask` taskNr iworld=:{parallelVars}
		= case 'Map'.get key parallelVars of
			Just ((nextIdx,controls) :: (Int,[Control s^])) 
				= (TaskFinished nextIdx, {iworld & parallelVars = 'Map'.put key (dynamic (nextIdx + 1, controls ++ [AppendTask nextIdx container] ) :: (Int,[Control s^])) parallelVars })
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !Int !(TaskList s) -> Task Void | TC s
removeTask idx tasklist = mkInstantTask "Append a task to a task list" (removeTask` idx tasklist)
where
	removeTask` :: !Int !(TaskList s) TaskNr *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` idx tasklist taskNr iworld=:{parallelVars}
		= case 'Map'.get key parallelVars of
			Just ((nextIdx,controls) :: (Int,[Control s^])) 
				= (TaskFinished Void, {iworld & parallelVars = 'Map'.put key (dynamic (nextIdx, controls ++ [RemoveTask idx] ) :: (Int,[Control s^])) parallelVars })
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	where
		identity	= toString tasklist
		key			= identity +++ "-control"
		
spawnProcess :: !Bool !ManagerProperties !(Task a) -> Task ProcessId | iTask a
spawnProcess gcWhenDone managerProperties task = abort "spawn process is broken"

killProcess :: !ProcessId -> Task Void
killProcess pid = abort "kill process is broken"
