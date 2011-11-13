implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, TaskStore, Util, HTTP, GenUpdate, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List
import iTaskClass, InteractionTasks
from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..), :: Control(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from CoreTasks			import return
from TuningCombinators	import :: Tag

derive class iTask ParallelTaskMeta, ParallelControl, ParallelTaskType

//Standard monadic bind
(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbfun = mkTask (taskMeta taska) init edit eval
where
	init taskNr iworld
		# taskaFuncs		= taskFuncs taska
		# (inita,iworld)	= taskaFuncs.initFun [0:taskNr] iworld
		= (TCBind (Left inita), iworld)
		
	//Event is targeted at first task of the bind
	edit taskNr event context=:(TCBind (Left cxta)) iworld	
		= case stepEvent 0 (Just event) of
			Nothing
				= (context, iworld)
			Just event
				# taskaFuncs		= taskFuncs taska
				# (newCxta,iworld)	= taskaFuncs.editFun [0:taskNr] event cxta iworld
				= (TCBind (Left newCxta), iworld)
	//Event is targeted at second task of the bind
	edit taskNr event context=:(TCBind (Right (vala,cxtb))) iworld
		= case stepEvent 1 (Just event) of
			Nothing	= (context,iworld)	//Mismatching event
			Just event
				//Compute the second task based on the result of the first
				= case fromJSON vala of
					Just a
						# taskbfun = taskFuncs (taskbfun a)
						# (newCxtb,iworld)	= taskbfun.editFun [1:taskNr] event cxtb iworld
						= (TCBind (Right (vala,newCxtb)), iworld)
					Nothing
						= (context, iworld)

	//Evaluate first task
	eval taskNr _ event tuiTaskNr repAs (TCBind (Left cxta)) iworld 
		//Adjust the target of a possible event
		# taskaFuncs		= taskFuncs taska
		# repAsA			= case repAs of
			(RepAsTUI _ _)	= let (ilayout,playout)	= taskLayouters taska in RepAsTUI ilayout playout
			_				= RepAsService
		# (resa, iworld) 	= taskaFuncs.evalFun [0:taskNr] taska.Task.meta (stepEvent 0 event) (stepTarget 0 tuiTaskNr) repAsA cxta iworld
		= case resa of
			TaskBusy rep actions newCxta
				= (TaskBusy (repOk 0 tuiTaskNr rep) actions (TCBind (Left newCxta)), iworld)
			TaskFinished a
				//Directly continue with the second task
				# taskb				= taskbfun a
				# taskbfuncs		= taskFuncs taskb
				# repAsB			= case repAs of
					(RepAsTUI _ _)	= let (ilayout,playout)	= taskLayouters taskb in RepAsTUI ilayout playout
					_				= RepAsService
				# (cxtb,iworld)		= taskbfuncs.initFun [1:taskNr] iworld
				# (resb,iworld)		= taskbfuncs.evalFun [1:taskNr] taskb.Task.meta Nothing (stepTarget 1 tuiTaskNr) repAsB cxtb iworld 
				= case resb of
					TaskBusy rep actions newCxtb	= (TaskBusy (repOk 1 tuiTaskNr rep) actions (TCBind (Right (toJSON a,newCxtb))),iworld)
					TaskFinished b					= (TaskFinished b, iworld)
					TaskException e str				= (TaskException e str, iworld)
			TaskException e str
				= (TaskException e str, iworld)	
	//Evaluate second task
	eval taskNr _ event tuiTaskNr repAs (TCBind (Right (vala,cxtb))) iworld
		= case fromJSON vala of
			Just a
				# taskb				= taskbfun a
				# taskbfuncs		= taskFuncs taskb
				# repAsB			= case repAs of
					(RepAsTUI _ _)	= let (ilayout,playout) = taskLayouters taskb in RepAsTUI ilayout playout
					_				= RepAsService
				# (resb, iworld)	= taskbfuncs.evalFun [1:taskNr] taskb.Task.meta (stepEvent 1 event) (stepTarget 1 tuiTaskNr) repAsB cxtb iworld 
				= case resb of
					TaskBusy rep actions newCxtb	= (TaskBusy (repOk 1 tuiTaskNr rep) actions (TCBind (Right (vala,newCxtb))),iworld)
					TaskFinished b					= (TaskFinished b, iworld)
					TaskException e str				= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt task value in bind", iworld)
	//Incorred state
	eval taskNr _ event tuiTaskNr _ context iworld
		= (taskException "Corrupt task context in bind", iworld)
	
	//Check that when we want the TUI of a sub task that it is on the path
	repOk i [] rep		= rep
	repOk i [t:ts] rep	
		| i == t	= rep
		| otherwise	= NoRep

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

(>>+) infixl 1 :: !(Task a) !(TermFunc a b) -> Task b | iTask a & iTask b
(>>+) task=:{Task|def} termF = case def of
	ActionTask actionTaskF	= {Task|task & def = NormalTask (actionTaskF termF)}
	_						= task >>= \r -> viewInformation (taskMeta task) [] r >>+ termF
	
noActions :: (TermFunc a b) | iTask a & iTask b
noActions = const (UserActions [])

returnAction :: Action -> (TermFunc a a) | iTask a
returnAction action = \{modelValue,localValid} -> UserActions [(action, if localValid (Just modelValue) Nothing)]

constActions :: ![(Action,b)] -> (TermFunc a b) | iTask a & iTask b
constActions actions = const (UserActions [(a,Just v) \\ (a,v) <- actions])

// Parallel composition
INFOKEY id		:== "parallel_" +++ taskNrToString id +++ "-info"

:: ResultSet
	= RSException !Dynamic !String
	| RSStopped
	| RSResults ![(!Int, !Int, !TaskResult ParallelControl, !SubTaskContext)]
	
parallel :: !d !s (ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d
parallel description initState resultFun initTasks = mkTask description init edit eval 
where
	//Create initial set of tasks and initial state
	init taskNr iworld=:{IWorld|timestamp}
		# (subContexts, nextIdx, iworld)	= initSubContexts taskNr taskList 0 (length initTasks - 1) initTasks iworld  
		# meta								= {nextIdx = nextIdx, stateId = taskNrToString taskNr, stateChanged = timestamp, infoChanged = timestamp}
		# encState							= encodeState initState initState
		= (TCParallel encState meta subContexts, iworld)
	where
		taskList	= ParallelTaskList (taskNrToString taskNr)
		
		initSubContexts taskNr taskList i o [] iworld = ([],i,iworld)
		initSubContexts taskNr taskList i o [t:ts] iworld
			# (s,iworld)			= initSubContext taskNr taskList i t iworld
			# (ss, nextIdx, iworld) = initSubContexts taskNr taskList (i + 1) (o - 1) ts iworld
			= ([(i,o,s):ss], nextIdx, iworld)
			
			
	//Direct the event to the right place
	edit taskNr event context=:(TCParallel encState meta subs) iworld
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskNr state meta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParTaskInfo taskNr subs meta.infoChanged iworld 
		//Evaluate sub(s)
		# (TCParallel encState meta subs,iworld)
			= case event of
				(TaskEvent [] ("top",JSONInt top))
					= (TCParallel encState meta (reorder top subs), iworld)
				(TaskEvent [s:steps] val)
					//Evaluate only the matching sub-context
					= case [(i,o,sub) \\ (i,o,sub) <- subs | i == s] of	
						[(i,o,sub)] 
							# ((i,o,newSub),iworld) = editSub taskNr (TaskEvent steps val) (i,o,sub) iworld
							= (TCParallel encState meta [if (i == s) (i,o,newSub) (i,o,sub) \\(i,o,sub) <- subs],iworld)
						_ 
							= (context,iworld)
				(ProcessEvent path val)
					//Evaluate all sub-contexts
					# (subs, iworld) = mapSt (editSub taskNr event) subs iworld
					= (TCParallel encState meta subs, iworld)
				_	
					//The event is mistargeted, do nothing
					= (context,iworld)
		//Remove the current state from the parallelStates scope in iworld
		# (state,meta,iworld)			= removeParState taskNr meta iworld
		//Remove the task info overview
		# iworld						= removeParTaskInfo taskNr iworld
		//Encode state
		# encState	= encodeState state initState
		= (TCParallel encState meta subs,iworld)
				
	edit taskNr event context iworld
		= (context,iworld)
	
	editSub taskNr event (i,o,sub) iworld=:{IWorld|latestEvent=parentLatestEvent}
		 = case sub of
			//Active Embedded task
			STCEmbedded tmeta (Just (encTask,subCxt))
				# task = fromJust (dynamicJSONDecode encTask)	//TODO: Add case for error
				# taskfuncs = taskFuncs` task
				# (newSubCxt,iworld) = taskfuncs.editFun [i:taskNr] event subCxt iworld
				= ((i,o,STCEmbedded tmeta (Just (encTask,newSubCxt))), iworld)		
			STCDetached taskId tmeta pmeta mmeta (Just (encTask,subCxt))
			//Same pattern as inbody tasks
				# task = fromJust (dynamicJSONDecode encTask)	//TODO: Also add case for error
				# taskfuncs = taskFuncs` task
				// change latest event timestamp for detached process
				# iworld = {IWorld|iworld & latestEvent = pmeta.ProgressMeta.latestEvent}
				# (newSubCxt,iworld) = taskfuncs.editFun [i:taskNr] event subCxt iworld
				# iworld = {IWorld|iworld & latestEvent = parentLatestEvent}
				= ((i,o,STCDetached taskId tmeta pmeta mmeta (Just (encTask,newSubCxt))), iworld)
			//Task is either completed already or hidden
			_
				= ((i,o,sub),iworld)
	
	
	//Eval all tasks in the set (in left to right order)
	eval taskNr meta event tuiTaskNr repAs context=:(TCParallel encState pmeta subs) iworld
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskNr state pmeta iworld
		//Add the initial control structure to the parallelStates scope in iworld
		# iworld						= addParControl taskNr pmeta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParTaskInfo taskNr subs pmeta.infoChanged iworld
		//Evaluate the sub tasks
		# (resultset,iworld)			= evalSubTasks taskNr event tuiTaskNr pmeta [] subs iworld
		//Remove the current state from the parallelStates scope in iworld
		# (state,pmeta,iworld)			= removeParState taskNr pmeta iworld
		//Remove the control structure from the parallelStates scope in iworld
		# (pmeta,iworld)				= removeParControl taskNr pmeta iworld
		//Remove the task info overview from the parallelStates scropr in iworld
		# iworld						= removeParTaskInfo taskNr iworld 
		//Remove parallel task info
		= case resultset of
			//Exception
			RSException e str	= (TaskException e str, iworld)
			RSStopped 			= (TaskFinished (resultFun Stopped state), iworld)
			RSResults results
				| allFinished results
					= (TaskFinished (resultFun AllRunToCompletion state), iworld)
				| otherwise
					# encState			= encodeState state initState
					# (rep,actions)		= case repAs of
						(RepAsTUI _ playout)	= (mergeTUIs taskNr playout tuiTaskNr meta results)
						(RepAsService)			= (ServiceRep [],[]) //TODO
					# subs				= mergeContexts results
					= (TaskBusy rep actions (TCParallel encState pmeta subs), iworld)
		
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks taskNr event tuiTaskNr meta results [] iworld
		= (RSResults results, iworld)
	evalSubTasks taskNr event tuiTaskNr meta results [(idx,order,stcontext):stasks] iworld=:{IWorld|latestEvent=parentLatestEvent,localDateTime}
		//Evaluate subtask
		# (result,stcontext,iworld)	= case stcontext of
			(STCEmbedded tmeta (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# taskfuncs			= taskFuncs` task
				# (ilayout,playout)	= taskLayouters task
				# (result,iworld)	= taskfuncs.evalFun [idx:taskNr] task.Task.meta (stepEvent idx event) (stepTarget idx tuiTaskNr) (RepAsTUI ilayout playout) context iworld 
				= case result of
					TaskBusy tui actions context	= (TaskBusy tui actions context, STCEmbedded tmeta (Just (encTask, context)), iworld)
					TaskFinished r					= (TaskFinished r, STCEmbedded tmeta Nothing, iworld)
					TaskException e str				= (TaskException e str, STCEmbedded tmeta Nothing, iworld)
			(STCDetached taskId tmeta pmeta mmeta (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# taskfuncs			= taskFuncs` task
				# (ilayout,playout)	= taskLayouters task
				//Update changed latest event timestamp
				# iworld			= {IWorld|iworld & latestEvent = pmeta.ProgressMeta.latestEvent}
				# (result,iworld)	= taskfuncs.evalFun [idx:taskNr] task.Task.meta (stepEvent idx event) (stepTarget idx tuiTaskNr) (RepAsTUI ilayout playout) context iworld 
				# iworld			= {IWorld|iworld & latestEvent = parentLatestEvent}
				//Update first/latest event if request is targeted at this detached process
				# pmeta = case tuiTaskNr of
					[t] | t == idx	= {pmeta & firstEvent = Just (fromMaybe localDateTime pmeta.firstEvent), latestEvent = Just localDateTime}
					_				= pmeta
				= case result of
					TaskBusy rep actions context	= (TaskBusy rep actions context, STCDetached taskId tmeta pmeta mmeta (Just (encTask, context)), iworld)
					TaskFinished r					= (TaskFinished r, STCDetached taskId tmeta (markFinished pmeta) mmeta Nothing, iworld)
					TaskException e str				= (TaskException e str, STCDetached taskId tmeta (markExcepted pmeta) mmeta Nothing, iworld)		
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
		# results	= results ++ [(idx,order,result,stcontext)]
		//Process controls
		# (controls, iworld)			= getControls meta iworld
		# (meta,results,stasks,iworld)	= processControls initState taskNr meta controls results stasks iworld
		//Evaluate remaining subtasks
		= evalSubTasks taskNr event tuiTaskNr meta results stasks iworld
		
	initSubContext taskNo taskList i taskContainer iworld=:{IWorld|timestamp,localDateTime,currentUser}
		# subTaskNo = [i:taskNo]
		= case taskContainer of
			(Embedded, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# tmeta			= initTaskMeta task
				# (cxt,iworld)	= funcs.initFun subTaskNo iworld
				= (STCEmbedded tmeta (Just (dynamicJSONEncode task, cxt)), iworld)
			(Detached mmeta, taskfun)
				# (task,funcs)	= mkSubTask taskfun
				# tmeta			= initTaskMeta task
				# pmeta			= initProgressMeta localDateTime currentUser
				# taskId		= taskNrToString subTaskNo
				# (cxt,iworld)	= funcs.initFun subTaskNo iworld
				= (STCDetached taskId tmeta pmeta mmeta (Just (dynamicJSONEncode task, cxt)), iworld)
	where
		// apply task list reference to taskfun & convert to 'normal' (non-action) tasks
		mkSubTask taskfun
			# task			= taskfun taskList
			# funcs			= taskFuncs task
			# task			= {Task|task & def = NormalTask funcs}
			= (task,funcs)

	//Initialize a process properties record for administration of detached tasks
	initProgressMeta now user
		= {ProgressMeta|status=Running,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing}
		
	//Initialize a task properties record for administration of all other tasks
	initTaskMeta {Task|meta} = meta

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
	addParState taskNr state {stateId,stateChanged} iworld=:{parallelStates}
		= {iworld & parallelStates = 'Map'.put (toString (ParallelTaskList stateId)) (dynamic (state,stateChanged) :: (s^,Timestamp)) parallelStates}
	
	removeParState :: !TaskNr !ParallelMeta !*IWorld -> (!s,!ParallelMeta,!*IWorld) | TC s
	removeParState taskNr meta=:{stateId} iworld=:{parallelStates}
		= case 'Map'.get (toString (ParallelTaskList stateId)) parallelStates of
			Just ((state,ts) :: (s^,Timestamp))	= (state,{meta & stateChanged = ts},{iworld & parallelStates = 'Map'.del (toString (ParallelTaskList stateId)) parallelStates})
			_									= abort "Could not read parallel state"
	
	//IMPORTANT: first argument is never used, but passed just to solve overloading
	//Put the initial control structure in the scope
	addParControl :: !TaskNr !ParallelMeta *IWorld -> *IWorld
	addParControl taskNr meta=:{nextIdx,stateId} iworld=:{parallelControls}
		= {iworld & parallelControls = 'Map'.put key (nextIdx,[]) parallelControls}
	where
		key = toString (ParallelTaskList stateId)
		
	removeParControl :: !TaskNr !ParallelMeta *IWorld -> (!ParallelMeta,!*IWorld)
	removeParControl taskNr meta=:{stateId} iworld=:{parallelControls}
		= case 'Map'.get key parallelControls of
			Just (nextIdx,_)	= ({meta & nextIdx = nextIdx},{iworld & parallelControls = 'Map'.del key parallelControls})
			_					= abort "Could not read parallel control"
	where
		key	= toString (ParallelTaskList stateId)
	
	//Put a datastructure in the scope with info on all processes in this set (TODO: Use parallel meta for identification)
	addParTaskInfo :: !TaskNr ![(!SubTaskId,!SubTaskOrder,!SubTaskContext)] !Timestamp !*IWorld -> *IWorld
	addParTaskInfo taskNr subs ts iworld=:{parallelStates}
		= {iworld & parallelStates = 'Map'.put (INFOKEY taskNr) (dynamic (mkInfo subs,ts) :: ([ParallelTaskMeta],Timestamp) ) parallelStates}
	where
		mkInfo subs = [meta i sub \\ (i,o,sub) <- subs]
	
		meta i (STCEmbedded tmeta _)
			= {ParallelTaskMeta
			  |index = i
			  ,taskId = taskNrToString [i:taskNr]
			  ,taskMeta = tmeta
			  ,progressMeta = Nothing
			  ,managementMeta = Nothing
			  }
		meta i (STCDetached taskId tmeta pmeta mmeta _)
			= {ParallelTaskMeta
			  |index = i
			  ,taskId = taskId
			  ,taskMeta = tmeta
			  ,progressMeta = Just pmeta
			  ,managementMeta = Just mmeta
			  }	
	
	removeParTaskInfo :: !TaskNr !*IWorld -> *IWorld
	removeParTaskInfo taskNr iworld=:{parallelStates}
		= {iworld & parallelStates = 'Map'.del (INFOKEY taskNr) parallelStates}
	
	//Remove and return control values from the scope
	getControls :: !ParallelMeta !*IWorld -> ([Control], !*IWorld)
	getControls meta=:{stateId} iworld=:{parallelControls}
		= case 'Map'.get key parallelControls of
			Just (nextIdx, controls)
				= (controls, {iworld & parallelControls = 'Map'.put key (nextIdx,[]) parallelControls})
			_
				= abort "Could not load parallel control data"
	where
		key	= toString (ParallelTaskList stateId)
		
	processControls :: s !TaskNr !ParallelMeta [Control] [(!Int, !Int, !TaskResult ParallelControl, !SubTaskContext)] [(Int, !Int, !SubTaskContext)] !*IWorld -> (!ParallelMeta,![(!Int, !Int, !TaskResult ParallelControl, !SubTaskContext)], ![(!Int,!Int,!SubTaskContext)],!*IWorld) | iTask s
	processControls s taskNr meta [] results remaining iworld = (meta,results,remaining,iworld)
	processControls s taskNr meta [c:cs] results remaining iworld
		# taskList = ParallelTaskList meta.ParallelMeta.stateId
		= case c of
			AppendTask idx user (taskContainer :: (TaskContainer s^))
				# (context,iworld)	= initSubContext taskNr taskList idx taskContainer iworld
				# remaining			= remaining ++ [(idx,idx,context)]
				= processControls s taskNr meta cs results remaining iworld
			RemoveTask idx
				# results			= [result \\ result=:(i,_,_,_) <- results | i <> idx]
				# remaining			= [sub \\ sub=:(i,_,_) <- remaining | i <> idx]
				= processControls s taskNr meta cs results remaining iworld
			_
				= processControls s taskNr meta cs results remaining iworld 		
	
	isException (TaskException _ _)	= True
	isException _					= False
	
	isStopped	(TaskFinished Stop)	= True
	isStopped	_					= False
	
	allFinished []							= True
	allFinished [(_,_,TaskFinished _,_):rs]	= allFinished rs
	allFinished _							= False
	
	markFinished pmeta
		= {ProgressMeta|pmeta & status = Finished}
	markExcepted pmeta
		= {ProgressMeta|pmeta & status = Excepted}
	
	updateProperties nmeta (STCDetached taskId tmeta pmeta mmeta scontext)
		= (STCDetached taskId tmeta mmeta nmeta scontext)
	updateProperties _ context = context
	
	mergeContexts contexts
		= [(i,o,context) \\ (i,o,_,context) <- contexts]

	//User the parallel merger function to combine the user interfaces of all InBody tasks		
	mergeTUIs taskNr pmerge tuiTaskNr pmeta contexts
		= case tuiTaskNr of
			[]
				# items		= [(i,o,getMeta subContext,getTui subContext mbTui,actions) \\(i,o,TaskBusy mbTui actions _,subContext) <- contexts | not (isDetached subContext)]
				# (tui,actions) =
					pmerge {TUIParallel
				 		 |taskId = taskNrToString taskNr
				 		 ,title = pmeta.TaskMeta.title
						 ,instruction = pmeta.TaskMeta.instruction
				 		 ,items = items
				 		 }
				= (TUIRep tui, actions)
				where
					isHidden (STCHidden _ _) = True
					isHidden _ = False
					isDetached (STCDetached _ _ _ _ _) = True
					isDetached _ = False
					
					getMeta (STCHidden meta _)	= meta
					getMeta (STCEmbedded meta _)	= meta		
					
					getTui (STCHidden _ _) _	= Nothing
					getTui (STCEmbedded _ _) (TUIRep tui)	= Just tui
					getTui (STCEmbedded _ _) _				= Nothing
					
			//We want to show the TUI of one of the detached tasks in this set
			[t]
				= case [(tui,actions) \\ (i,o,TaskBusy (TUIRep tui) actions _,STCDetached _ _ _ _ _) <- contexts | i == t] of
					[(tui,actions)]
						= (TUIRep tui,actions)
					_
						= (NoRep,[])
				
				
			//We want to show the TUI of a task inside the parallel set
			[t:ts]
				= case [(tui,actions) \\ (i,o,TaskBusy (TUIRep tui) actions _,_) <- contexts | i == t] of
					[(tui,actions)]	= (TUIRep tui,actions)
					_				= (NoRep,[])
	
	taskFuncs` {Task|def} = case def of
		NormalTask fs	= fs
		_				= abort "action task in parallel"
		
	//Change the order of the subtask such that the indicated sub becomes top and the others
	//maintain their relative ordering
	reorder :: SubTaskId [(!SubTaskId,!SubTaskOrder,!SubTaskContext)] -> [(!SubTaskId,!SubTaskOrder,!SubTaskContext)]
	reorder top subs
		= let (tsubs,rsubs)	= splitWith isTop (sortByTaskOrder subs)
			in (sortByTaskId [(i,o,c) \\ (i,_,c) <- (rsubs ++ tsubs) & o <- [0..]])
	where							
		isTop (i,_,_) = i == top
		sortByTaskId subs = sortBy ( \(a,_,_) (b,_,_) -> a < b) subs
		sortByTaskOrder subs = sortBy ( \(_,a,_) (_,b,_) -> a < b) subs

/**
* Get the shared state of a task list
*/
taskListState :: (TaskList s) -> Shared s | TC s
taskListState tasklist = ReadWriteShared [identity] read write timestamp
where
	identity 	= toString tasklist
	
	read iworld=:{parallelStates}
		= case 'Map'.get identity parallelStates of
				Just ((val,_) :: (s^,Timestamp))	= (Ok val, iworld)
				_									= (Error ("Could not read shared parallel state of task list " +++ identity),iworld)
	
	write val iworld=:{parallelStates,timestamp}
			= (Ok Void, {iworld & parallelStates = 'Map'.put identity (dynamic (val,timestamp) :: (s^,Timestamp)) parallelStates })
	
	timestamp iworld=:{parallelStates}
			= case 'Map'.get identity parallelStates of
				Just ((_,ts) :: (s,Timestamp))		= (Ok ts, iworld)
				_									= (Error ("Could not read timestamp for shared state of task list " +++ identity),iworld)

/**
* Get the properties share of a task list
*/
taskListMeta :: (TaskList s) -> Shared [ParallelTaskMeta]
taskListMeta tasklist = ReadWriteShared [identity] read write timestamp
where
	identity	= toString tasklist +++ "-info"
	
	read iworld=:{parallelStates}
		= case 'Map'.get identity parallelStates of
			Just ((val,_) :: ([ParallelTaskMeta],Timestamp))	= (Ok val, iworld)
			_													= (Error ("Could not read parallel task meta of " +++ identity), iworld)
		
	write val iworld=:{parallelStates,timestamp} //TODO
		= (Ok Void, iworld)
			
	timestamp iworld=:{parallelStates}
		= case 'Map'.get identity parallelStates of
			Just ((_,ts) :: ([ParallelTaskMeta],Timestamp))	= (Ok ts, iworld)
			_												= (Error ("Could not read timestamp for parallel task meta of " +++ identity), iworld)
		
/**
* Add a task to a task list
*/
appendTask :: !(TaskContainer s) !(TaskList s)	-> Task Int | TC s
appendTask container tasklist = mkInstantTask "Append a task to a task list" appendTask`
where
	identity	= toString tasklist
	appendTask` taskNr iworld=:{parallelControls,currentUser}
		= case 'Map'.get identity parallelControls of
			Just (nextIdx,controls)
				//For the global tasklist we don't use the internal counter, but get the index from the
				//process database
				# (nextIdx, iworld) = case tasklist of
					GlobalTaskList
						# (WorkflowProcess next,iworld) = newWorkflowId iworld
						= (next,iworld)
					_				= (nextIdx,iworld)
				# parallelControls = 'Map'.put identity (nextIdx + 1, controls ++ [AppendTask nextIdx currentUser (dynamic container :: TaskContainer s^)]) parallelControls 
				= (TaskFinished nextIdx, {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !Int !(TaskList s) -> Task Void | TC s
removeTask idx tasklist = mkInstantTask "Append a task to a task list" (removeTask` idx tasklist)
where
	removeTask` :: !Int !(TaskList s) TaskNr *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` idx tasklist taskNr iworld=:{parallelControls}
		= case 'Map'.get identity parallelControls of
			Just (nextIdx,controls)
				# parallelControls = 'Map'.put identity (nextIdx, controls ++ [RemoveTask idx]) parallelControls
				= (TaskFinished Void, {iworld & parallelControls = parallelControls })
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	where
		identity	= toString tasklist

/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs user task=:{Task|def,layout} = case def of
	NormalTask funs
		# funs = {initFun = init funs.initFun
				 ,editFun = edit funs.editFun
				 ,evalFun = eval funs.evalFun
				 }
		= {Task|task & def = NormalTask funs, layout = layout}
	ActionTask fun
		= {Task|task & def = ActionTask (action fun), layout = layout}
where
	action f tfun
		# funs = f tfun
		= {initFun = init funs.initFun
		  ,editFun = edit funs.editFun
		  ,evalFun = eval funs.evalFun
		  }

	init f taskNr iworld=:{currentUser}
		# (context,iworld) = f taskNr {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})

	edit f taskNr event context iworld=:{currentUser}
		# (context,iworld) = f taskNr event context {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})
	
	eval f taskNr props event target repInput context iworld=:{currentUser}
		# (result,iworld) = f taskNr props event target repInput context {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})
	
		