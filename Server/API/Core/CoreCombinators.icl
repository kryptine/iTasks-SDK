implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, TaskStore, Util, HTTP, GenUpdate, GenEq, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List
import iTaskClass, InteractionTasks, LayoutCombinators, TUIDefinition

from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..), :: Control(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from CoreTasks			import return

derive class iTask ParallelTaskMeta, ParallelResult, ParallelTaskType

transform :: ((Maybe a) -> Maybe b) (Task a) -> Task b | iTask a & iTask b 
transform f task=:{Task|evalFun} = {Task|task & evalFun = evalFun`}
where
	evalFun` taskNo event target repAs cxt iworld
		= case evalFun taskNo event target repAs cxt iworld of
			(TaskInstable mba rep cxt, iworld)	= (TaskInstable (f mba) rep cxt, iworld)
			(TaskStable a rep cxt, iworld)		= case f (Just a) of
				(Just b)	= (TaskStable b rep cxt, iworld)
				Nothing		= (taskException "Task with permanent invalid result", iworld)
			(TaskException e str, iworld)		= (TaskException e str, iworld)

project	:: ((Maybe a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a
project projection share taska = mkTask init edit eval
where
	init taskNr iworld
		# (inita,iworld)	= taska.initFun [0:taskNr] iworld
		= (TCProject JSONNull inita, iworld)
	
	//Pass along edit events
	edit taskNr event context=:(TCProject prev cxta) iworld
		= case stepEvent 0 (Just event) of
			Just sevent
				# (ncxta,iworld)	= taska.editFun [0:taskNr] sevent cxta iworld
				= (TCProject prev ncxta, iworld)
			_
				= (context,iworld)
		
	//Eval task and check for change
	eval taskNr event tuiTaskNr repAs (TCProject prev cxta) iworld 
		# taskaRepAs		= subRepAs repAs taska
		# (resa, iworld) 	= taska.evalFun [0:taskNr] (stepEvent 0 event) (stepTarget 0 tuiTaskNr) taskaRepAs cxta iworld
		= case resa of
			TaskInstable mba rep ncxta 
				| changed prev mba	
					= projectOnShare mba (TaskInstable mba rep (TCProject (toJSON mba) ncxta)) iworld
				| otherwise
					= (TaskInstable mba rep (TCProject prev ncxta), iworld)
			TaskStable a rep ncxta
				| changed prev (Just a)
					= projectOnShare (Just a) (TaskStable a rep (TCProject (toJSON (Just a)) ncxta)) iworld
				| otherwise
					= (TaskStable a rep (TCProject prev ncxta), iworld)
			TaskException e str
				= (TaskException e str,iworld)
	
	subRepAs RepAsService _ 					= RepAsService
	subRepAs (RepAsTUI clayout)	task=:{layout}	= case clayout of
		Nothing			= RepAsTUI layout
		_				= RepAsTUI clayout
	
	changed encprev cur = case fromJSON encprev of
		Nothing		= True	//Consider changed when parsing fails
		Just prev	= prev =!= cur
	
	projectOnShare mba result iworld
		= case maybeUpdateShared share (projection mba) iworld of
			(Ok _,iworld)		= (result,iworld)
			(Error e,iworld)	= (taskException e,iworld)
	
step :: (Task a) [TaskStep a b] -> Task b | iTask a & iTask b
step taska conts = mkTask init edit eval
where
	init taskNr iworld
		# (inita,iworld)	= taska.initFun [0:taskNr] iworld
		= (TCStep (Left inita), iworld)
	//Edit left-hand side
	edit taskNr event context=:(TCStep (Left cxta)) iworld
		= case stepEvent 0 (Just event) of
			Just sevent
				# (ncxta,iworld) = taska.editFun [0:taskNr] sevent cxta iworld
				= (TCStep (Left ncxta), iworld)
			_
				= (context,iworld)
	//Edit right-hand side
	edit taskNr event context=:(TCStep (Right (enca, sel, cxtb))) iworld
		= case stepEvent 1 (Just event) of
			Just sevent
				# mbTaskb = case conts !! sel of
					(AnyTime _ taskbf)		= fmap taskbf (fromJSON enca)
					(WithResult	_ _ taskbf)	= fmap taskbf (fromJSON enca)
					(WithoutResult _ taskb)	= Just taskb
					(WhenStable taskbf)		= fmap taskbf (fromJSON enca)
					(Catch taskbf)			= fmap taskbf (fromJSON enca)
					(CatchAll taskbf)		= fmap taskbf (fromJSON enca)
				= case mbTaskb of
					Just taskb
						# (ncxtb,iworld)	= taskb.editFun [1:taskNr] sevent cxtb iworld
						= (TCStep (Right (enca, sel, ncxtb)), iworld)
					Nothing
						= (context, iworld)
			_	= (context, iworld)
	edit taskNr event context iworld
		= (context, iworld)
	//Eval left-hand side
	eval taskNr event tuiTaskNr repAs (TCStep (Left cxta)) iworld 
		# taskaRepAs		= subRepAs repAs taska
		# (resa, iworld) 	= taska.evalFun [0:taskNr] (stepEvent 0 event) (stepTarget 0 tuiTaskNr)taskaRepAs cxta iworld
		# mbcommit			= case event of
			(Just (TaskEvent [] action))	= Just action
			_								= Nothing
		# mbCont			= case resa of
			TaskInstable mba rep ncxta = case searchContInstable mba mbcommit conts of
				Nothing			= Left (TaskInstable Nothing (addStepActions taskNr repAs rep mba)  (TCStep (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskStable a rep ncxta = case searchContStable a mbcommit conts of
				Nothing			= Left (TaskInstable Nothing (addStepActions taskNr repAs rep (Just a)) (TCStep (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskException e str = case searchContException e str conts of
				Nothing			= Left (TaskException e str)
				Just rewrite	= Right rewrite
		= case mbCont of
			Left res = (res,iworld)
			Right (sel,taskb,enca)
				# taskbRepAs	= subRepAs repAs taskb
				# (cxtb,iworld)		= taskb.initFun [1:taskNr] iworld
				# (resb,iworld)		= taskb.evalFun [1:taskNr] Nothing (stepTarget 1 tuiTaskNr) taskbRepAs cxtb iworld 
				= case resb of
					TaskInstable mbb rep ncxtb	= (TaskInstable mbb rep (TCStep (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep ncxtb, iworld)
					TaskException e str			= (TaskException e str, iworld)
	//Eval right-hand side
	eval taskNr event tuiTaskNr repAs (TCStep (Right (enca,sel,cxtb))) iworld
		# mbTaskb = case conts !! sel of
			(AnyTime _ taskbf)		= fmap taskbf (fromJSON enca)
			(WithResult	_ _ taskbf)	= fmap taskbf (fromJSON enca)
			(WithoutResult _ taskb)	= Just taskb
			(WhenStable taskbf)		= fmap taskbf (fromJSON enca)
			(Catch taskbf)			= fmap taskbf (fromJSON enca)
			(CatchAll taskbf)		= fmap taskbf (fromJSON enca)
		= case mbTaskb of
			Just taskb
				# taskbRepAs		= subRepAs repAs taskb
				# (resb, iworld)	= taskb.evalFun [1:taskNr] (stepEvent 1 event) (stepTarget 1 tuiTaskNr) taskbRepAs cxtb iworld 
				= case resb of
					TaskInstable mbb rep ncxtb	= (TaskInstable mbb rep (TCStep (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep ncxtb, iworld)
					TaskException e str			= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt task value in step", iworld) 	
	//Incorred state
	eval taskNr event tuiTaskNr _ context iworld
		= (taskException "Corrupt task context in step", iworld)

	searchContInstable mba mbcommit conts = search mba mbcommit 0 Nothing conts
	where
		search _ _ _ mbmatch []							= mbmatch	//First matching trigger wins
		search (Just a) mbcommit i mbmatch [WhenValid pred f:cs]
			| pred a									= Just (i, f a, toJSON a)
														= search (Just a) mbcommit (i + 1) mbmatch cs
		search mba (Just commit) i Nothing [AnyTime action f:cs]
			| commit == actionName action				= search mba (Just commit) (i + 1) (Just (i, f mba, toJSON mba)) cs
														= search mba (Just commit) (i + 1) Nothing cs
		search Nothing (Just commit) i Nothing [WithoutResult action taskb:cs]
			| commit == actionName action				= search Nothing (Just commit) (i + 1) (Just (i, taskb, JSONNull)) cs
														= search Nothing (Just commit) (i + 1) Nothing cs
		search (Just a) (Just commit) i Nothing [WithResult action pred taskb:cs]
			| commit == actionName action && pred a		= search (Just a) (Just commit) (i + 1) (Just (i, taskb a, toJSON a)) cs
														= search (Just a) (Just commit) (i + 1) Nothing cs
		search mba mbcommit i mbmatch [_:cs]			= search mba mbcommit (i + 1) mbmatch cs
		
	searchContStable a mbcommit conts = search a mbcommit 0 Nothing conts
	where
		search _ _ _ mbmatch []							= mbmatch
		search a mbcommit i mbmatch [WhenStable f:_]	= Just (i,f a, toJSON a)	
		search a (Just commit) i Nothing [AnyTime action f:cs]
			| commit == actionName action				= search a (Just commit) (i + 1) (Just (i, f (Just a), toJSON (Just a))) cs
														= search a (Just commit) (i + 1) Nothing cs
		search a (Just commit) i Nothing [WithResult action pred f:cs]
			| commit == actionName action && pred a		= search a (Just commit) (i + 1) (Just (i, f a, toJSON a)) cs
														= search a (Just commit) (i + 1) Nothing cs
		search a mbcommit i mbmatch [_:cs]				= search a mbcommit (i + 1) mbmatch cs
		
	searchContException dyn str conts = search dyn str 0 Nothing conts
	where
		search _ _ _ catchall []					= catchall													//Return the maybe catchall
		search dyn str i catchall [Catch f:cs] = case (match f dyn) of
			Just (taskb,enca)						= Just (i, taskb, enca)										//We have a match
			_										= search dyn str (i + 1) catchall cs						//Keep searching
		search dyn str i Nothing [CatchAll f:cs]	= search dyn str (i + 1) (Just (i, f str, toJSON str)) cs 	//Keep searching (at least we have a catchall)
		search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs						//Keep searching
				
		match :: (e -> Task b) Dynamic -> Maybe (Task b, JSONNode) | iTask e
		match f (e :: e^)	= Just (f e, toJSON e)
		match _ _			= Nothing 
	
	addStepActions taskNr RepAsService rep mba = case rep of
		(ServiceRep (parts,actions))
			= ServiceRep (parts,actions ++ stepActions taskNr mba)
		_	= rep
	addStepActions taskNr (RepAsTUI layout) rep mba = case rep of
		(TUIRep gui)
			# (Layout layoutfun) = fromMaybe DEFAULT_LAYOUT layout
			= TUIRep (layoutfun [gui] (stepActions taskNr mba) [(TASK_ATTRIBUTE, taskNrToString taskNr)])	//TODO: Add attributes from task
		_	= rep
	
	stepActions taskNo mba = stepActions` conts
	where
		taskId = taskNrToString taskNo
		stepActions` [] = []
		stepActions` [AnyTime action _:cs]			= [(taskId,action,True):stepActions` cs]
		stepActions` [WithResult action pred _:cs]	= [(taskId,action,maybe False pred mba):stepActions` cs]
		stepActions` [WithoutResult action _:cs]	= [(taskId,action,isNothing mba):stepActions` cs]
		stepActions` [_:cs]							= stepActions` cs

	subRepAs RepAsService _ = RepAsService
	subRepAs (RepAsTUI clayout) {Task|layout} = case layout of
		Nothing				= (RepAsTUI clayout)
		_					= (RepAsTUI layout)
		
		
// Parallel composition
METAKEY taskNo	:== "parallel_" +++ taskNrToString taskNo +++ "-info"

:: ResultSet
	= RSException !Dynamic !String
	| RSStopped
	| RSResults ![(!Int, !Int, !TaskResult ParallelResult, !SubTaskContext)]
	
parallel :: !d !a ![TaskContainer a] -> Task a | descr d & iTask a
parallel desc initState initTasks = mkTask init edit eval 
where
	//Create initial set of tasks and initial state
	init taskNr iworld=:{IWorld|timestamp}
		# meta								= {nextIdx = length initTasks, stateId = taskNrToString taskNr, stateChanged = timestamp, infoChanged = timestamp}
		# iworld							= addParState taskNr initState meta iworld
		# iworld							= addParTaskInfo taskNr [] meta.infoChanged iworld 
		# (subContexts,iworld)				= initSubContexts taskNr taskList 0 (length initTasks - 1) initTasks iworld  
		# (state,meta,iworld)				= removeParState taskNr meta iworld
		# iworld							= removeParTaskInfo taskNr iworld
		# encState							= encodeState state initState
		= (TCParallel encState meta subContexts, iworld)
	where
		taskList	= ParallelTaskList (taskNrToString taskNr)
		
		//Use incrementing identifiers (i) and decrementing order values (o)
		//To make sure that the first initial subtask has the highest order value
		//(this will ensure the first tab is active, or the first window will be on top)
		initSubContexts taskNr taskList i o [] iworld = ([],iworld)
		initSubContexts taskNr taskList i o [t:ts] iworld
			# (s,iworld)	= initSubContext taskNr taskList i t iworld
			# (ss, iworld) 	= initSubContexts taskNr taskList (i + 1) (o - 1) ts iworld
			= ([(i,o,s):ss], iworld)
			
			
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
				//Event targeted at the parallel set, to move a task to front in the ordering
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
			STCEmbedded (Just (encTask,subCxt))
				# task = fromJust (dynamicJSONDecode encTask)	//TODO: Add case for error
				# (newSubCxt,iworld) = task.editFun [i:taskNr] event subCxt iworld
				= ((i,o,STCEmbedded (Just (encTask,newSubCxt))), iworld)		
			STCDetached taskId pmeta mmeta (Just (encTask,subCxt))
			//Same pattern as inbody tasks
				# task = fromJust (dynamicJSONDecode encTask)	//TODO: Also add case for error
				// change latest event timestamp for detached process
				# iworld = {IWorld|iworld & latestEvent = pmeta.ProgressMeta.latestEvent}
				# (newSubCxt,iworld) = task.editFun [i:taskNr] event subCxt iworld
				# iworld = {IWorld|iworld & latestEvent = parentLatestEvent}
				= ((i,o,STCDetached taskId pmeta mmeta (Just (encTask,newSubCxt))), iworld)
			//Task is either completed already or hidden
			_
				= ((i,o,sub),iworld)
	
	//Eval all tasks in the set (in left to right order)
	eval taskNr event tuiTaskNr repAs context=:(TCParallel encState pmeta subs) iworld
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
			RSException e str
				= (TaskException e str, iworld)
			RSStopped
				= (TaskStable state NoRep (TCBasic (toJSON state) True), iworld)
			RSResults results
				# encState			= encodeState state initState
				# rep				= case repAs of
					(RepAsTUI layout)
						# (Layout layoutfun) = fromMaybe DEFAULT_LAYOUT layout
						= TUIRep (layoutfun (taskReps results) [] ([(TASK_ATTRIBUTE, taskNrToString taskNr)] ++ initAttributes desc))
					(RepAsService)
						= ServiceRep ([],[]) //TODO
				# subs				= mergeContexts results		
				| allStable results
					= (TaskStable state rep (TCParallel encState pmeta subs), iworld)
				| otherwise
					= (TaskInstable (Just state) rep (TCParallel encState pmeta subs), iworld)
	
	//When the parallel has been stopped, we have the state encoded in a TCBasic node
	eval taskNr event tuiTaskNr repAs context=:(TCBasic encState True) iworld
		= case fromJSON encState of
			Just state	= (TaskStable state NoRep context, iworld)
			Nothing		= (taskException "Corrupt task context in parallel", iworld)
			
	eval _ _ _ _ _ iworld
		= (taskException "Corrupt task context in parallel", iworld)
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks taskNr event tuiTaskNr meta results [] iworld
		= (RSResults results, iworld)
	evalSubTasks taskNr event tuiTaskNr meta results [(idx,order,stcontext):stasks] iworld=:{IWorld|latestEvent=parentLatestEvent,localDateTime}
		//Evaluate subtask
		# (result,stcontext,iworld)	= case stcontext of
			(STCEmbedded (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				# (result,iworld)	= task.evalFun [idx:taskNr] (stepEvent idx event) (stepTarget idx tuiTaskNr) (RepAsTUI task.layout) context iworld 
				= case result of
					TaskInstable mbr rep context	= (TaskInstable mbr rep context, STCEmbedded (Just (encTask, context)), iworld)
					TaskStable r rep context		= (TaskStable r rep context, STCEmbedded Nothing, iworld)
					TaskException e str				= (TaskException e str, STCEmbedded Nothing, iworld)
			(STCDetached taskId pmeta mmeta (Just (encTask,context)))
				# task				= fromJust (dynamicJSONDecode encTask)
				//Update changed latest event timestamp
				# iworld			= {IWorld|iworld & latestEvent = pmeta.ProgressMeta.latestEvent}
				# (result,iworld)	= task.evalFun [idx:taskNr] (stepEvent idx event) (stepTarget idx tuiTaskNr) (RepAsTUI task.layout) context iworld 
				# iworld			= {IWorld|iworld & latestEvent = parentLatestEvent}
				//Update first/latest event if request is targeted at this detached process
				# pmeta = case tuiTaskNr of
					[t] | t == idx	= {pmeta & firstEvent = Just (fromMaybe localDateTime pmeta.firstEvent), latestEvent = Just localDateTime}
					_				= pmeta
				= case result of
					TaskInstable mbr rep context	= (TaskInstable mbr rep context, STCDetached taskId pmeta mmeta (Just (encTask, context)), iworld)
					TaskStable r rep context		= (TaskStable r rep context, STCDetached taskId (markFinished pmeta) mmeta Nothing, iworld)
					TaskException e str				= (TaskException e str, STCDetached taskId (markExcepted pmeta) mmeta Nothing, iworld)		
			_
				//This task is already completed
				= (TaskStable Keep NoRep TCEmpty, stcontext, iworld)
		//Check for exception
		| isException result
			# (TaskException dyn str) = result
			= (RSException dyn str, iworld)
		//Check for Stop result (discards any pending additions/removals to the set, since they will be pointless anyway)
		| isStopped result
			= (RSStopped, iworld)
		//Append current result to results so far if it is not a stable Remove result
		# results	= if (isRemove result) results (results ++ [(idx,order,result,stcontext)])
		//Process controls
		# (controls, iworld)			= getControls meta iworld
		# (meta,results,stasks,iworld)	= processControls initState taskNr meta controls results stasks iworld
		//Evaluate remaining subtasks
		= evalSubTasks taskNr event tuiTaskNr meta results stasks iworld
		
	initSubContext taskNo taskList i taskContainer iworld=:{IWorld|timestamp,localDateTime,currentUser}
		# subTaskNo = [i:taskNo]
		= case taskContainer of
			(Embedded, taskfun)
				# task			= taskfun taskList
				# (cxt,iworld)	= task.initFun subTaskNo iworld
				= (STCEmbedded (Just (dynamicJSONEncode task, cxt)), iworld)
			(Detached mmeta, taskfun)
				# task			= taskfun taskList
				# pmeta			= initProgressMeta localDateTime currentUser
				# taskId		= taskNrToString subTaskNo
				# (cxt,iworld)	= task.initFun subTaskNo iworld
				= (STCDetached taskId pmeta mmeta (Just (dynamicJSONEncode task, cxt)), iworld)
		
	//Initialize a process properties record for administration of detached tasks
	initProgressMeta now user
		= {ProgressMeta|status=Running,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing}
		
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
		= {iworld & parallelStates = 'Map'.put (METAKEY taskNr) (dynamic (mkInfo subs,ts) :: ([ParallelTaskMeta],Timestamp) ) parallelStates}
	where
		mkInfo subs = [meta i sub \\ (i,o,sub) <- subs]
	
		meta i (STCEmbedded _)
			= {ParallelTaskMeta
			  |index = i
			  ,taskId = taskNrToString [i:taskNr]
			  ,progressMeta = Nothing
			  ,managementMeta = Nothing
			  }
		meta i (STCDetached taskId pmeta mmeta _)
			= {ParallelTaskMeta
			  |index = i
			  ,taskId = taskId
			  ,progressMeta = Just pmeta
			  ,managementMeta = Just mmeta
			  }	
	
	removeParTaskInfo :: !TaskNr !*IWorld -> *IWorld
	removeParTaskInfo taskNr iworld=:{parallelStates}
		= {iworld & parallelStates = 'Map'.del (METAKEY taskNr) parallelStates}
	
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
		
	processControls :: s !TaskNr !ParallelMeta [Control] [(!Int, !Int, !TaskResult ParallelResult, !SubTaskContext)] [(Int, !Int, !SubTaskContext)] !*IWorld -> (!ParallelMeta,![(!Int, !Int, !TaskResult ParallelResult, !SubTaskContext)], ![(!Int,!Int,!SubTaskContext)],!*IWorld) | iTask s
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
	
	isRemove (TaskStable Remove _ _)= True
	isRemove _						= False
	
	isStopped (TaskStable Stop _ _)	= True
	isStopped _						= False
	
	allStable []							= True
	allStable [(_,_,TaskStable _ _ _,_):rs]	= allStable rs
	allStable _								= False
	
	markFinished pmeta
		= {ProgressMeta|pmeta & status = Finished}
	markExcepted pmeta
		= {ProgressMeta|pmeta & status = Excepted}
	
	updateProperties nmeta (STCDetached taskId pmeta mmeta scontext)
		= (STCDetached taskId mmeta nmeta scontext)
	updateProperties _ context = context
	
	mergeContexts contexts
		= [(i,o,context) \\ (i,o,_,context) <- contexts]

	taskReps results
		= [appThd3 (kvSet STACK_ATTRIBUTE (toString o)) gui \\ (i,o,TaskInstable _ (TUIRep gui) _,subContext) <- results]
		
	/*
	//Use the parallel merger function to combine the user interfaces of all embedded tasks		
	mergeTUIs taskNr pmerge tuiTaskNr pmeta contexts
		= case tuiTaskNr of
			[]
				# items		= [(i,o,getMeta subContext,getTui subContext rep,getActions rep) \\(i,o,TaskInstable _ rep _,subContext) <- contexts | not (isDetached subContext)]
				# (tui,actions) =
					pmerge {TUIParallel
				 		 |taskId = taskNrToString taskNr
				 		 ,title = pmeta.TaskMeta.title
						 ,instruction = pmeta.TaskMeta.instruction
				 		 ,items = items
				 		 }
				= TUIRep (Just tui, actions, [])
				where
					isHidden (STCHidden _ _) = True
					isHidden _ = False
					isDetached (STCDetached _ _ _ _ _) = True
					isDetached _ = False
					
					getMeta (STCHidden meta _)	= meta
					getMeta (STCEmbedded meta _)	= meta		
					
					getTui (STCHidden _ _) _	= Nothing
					getTui (STCEmbedded _ _) (TUIRep (mbTui,_,_))	=	mbTui
					getTui (STCEmbedded _ _) _						= Nothing
					
					getActions (TUIRep (_,actions,_))	= actions
					getActions _						= []
			//We want to show the TUI of one of the detached tasks in this set
			[t]
				= case [(tui,actions,attributes) \\ (i,o,TaskInstable _ (TUIRep (tui, actions, attributes)) _,STCDetached _ _ _ _ _) <- contexts | i == t] of
					[(tui,actions,attributes)]	= TUIRep (tui, actions, attributes)
					_							= NoRep
				
				
			//We want to show the TUI of a task inside the parallel set
			[t:ts]
				= case [(tui,actions,attributes) \\ (i,o,TaskInstable _ (TUIRep (tui,actions,attributes)) _,_) <- contexts | i == t] of
					[(tui,actions,attributes)]	= TUIRep (tui, actions, attributes)
					_							= NoRep
		*/
			
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
appendTask container tasklist = mkInstantTask appendTask`
where
	identity	= toString tasklist
	appendTask` taskNr iworld=:{parallelControls,currentUser}
		= case 'Map'.get identity parallelControls of
			Just (nextIdx,controls)
				//For the global tasklist we don't use the internal counter, but get the index from the
				//process database
				# (nextIdx, iworld) = case tasklist of
					TopLevelTaskList
						# (WorkflowProcess next,iworld) = newWorkflowId iworld
						= (next,iworld)
					_				= (nextIdx,iworld)
				# parallelControls = 'Map'.put identity (nextIdx + 1, controls ++ [AppendTask nextIdx currentUser (dynamic container :: TaskContainer s^)]) parallelControls 
				= (TaskStable nextIdx NoRep TCEmpty, {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !Int !(TaskList s) -> Task Void | TC s
removeTask idx tasklist = mkInstantTask (removeTask` idx tasklist)
where
	removeTask` :: !Int !(TaskList s) TaskNr *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` idx tasklist taskNr iworld=:{parallelControls}
		= case 'Map'.get identity parallelControls of
			Just (nextIdx,controls)
				# parallelControls = 'Map'.put identity (nextIdx, controls ++ [RemoveTask idx]) parallelControls
				= (TaskStable Void NoRep TCEmpty, {iworld & parallelControls = parallelControls })
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
workAs user task
	= {Task|task & initFun = init task.initFun, editFun = edit task.editFun, evalFun = eval task.evalFun}
where
	init f taskNr iworld=:{currentUser}
		# (context,iworld) = f taskNr {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})

	edit f taskNr event context iworld=:{currentUser}
		# (context,iworld) = f taskNr event context {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})
	
	eval f taskNr event target repInput context iworld=:{currentUser}
		# (result,iworld) = f taskNr event target repInput context {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})

/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a

instance tune Layout
where tune layout task				= {Task|task & layout = Just layout}

instance tune (Layout -> Layout)
where tune f task=:{Task|layout}	= {Task|task & layout = Just (f (fromMaybe DEFAULT_LAYOUT layout))} 

instance tune Attribute
where tune (Attribute k v) task	= task //TODO
instance tune Title
where tune (Title t) task	= task //TODO
instance tune Icon
where tune (Icon i) task	= task //TODO

instance tune Window
where tune _ task			= task //TODO
