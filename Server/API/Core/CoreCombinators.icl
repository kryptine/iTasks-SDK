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

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{evalStack=[TaskId topNo _:_],nextTaskNo} = (TaskId topNo nextTaskNo, {iworld & nextTaskNo = nextTaskNo + 1})
getNextTaskId iworld = abort "Empty evaluation stack"

transform :: ((Maybe a) -> Maybe b) (Task a) -> Task b | iTask a & iTask b 
transform f task=:{Task|evalFun} = {Task|task & evalFun = evalFun`}
where
	evalFun` eEvent cEvent repAs cxt iworld
		= case evalFun eEvent cEvent repAs cxt iworld of
			(TaskInstable mba rep cxt, iworld)	= (TaskInstable (f mba) rep cxt, iworld)
			(TaskStable a rep cxt, iworld)		= case f (Just a) of
				(Just b)	= (TaskStable b rep cxt, iworld)
				Nothing		= (taskException "Task with permanent invalid result", iworld)
			(TaskException e str, iworld)		= (TaskException e str, iworld)

project	:: ((Maybe a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a
project projection share taska = mkTask init edit eval
where
	init _ iworld
		# (taskId,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskId iworld
		= (TCProject JSONNull inita, iworld)
	
	//Pass along edit events
	edit event context=:(TCProject prev cxta) iworld
		# (ncxta,iworld)	= taska.editFun event cxta iworld
		= (TCProject prev ncxta, iworld)
		
	//Eval task and check for change
	eval eEvent cEvent repAs (TCProject prev cxta) iworld
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (subRepAs repAs taska) cxta iworld
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
	
	subRepAs (RepAsService target) _  			= (RepAsService target)
	subRepAs (RepAsTUI target mbLayout) task=:{layout} = case mbLayout of
		Nothing			= RepAsTUI target layout
		Just overwrite	= RepAsTUI target (Just overwrite)
		
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
	init taskId iworld
		# (taskIdA,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskIdA iworld
		= (TCStep taskId (Left inita), iworld)
		
	//Edit left-hand side
	edit event context=:(TCStep taskId (Left cxta)) iworld
		# (ncxta,iworld) = taska.editFun event cxta iworld
		= (TCStep taskId (Left ncxta), iworld)
	
	//Edit right-hand side
	edit event context=:(TCStep taskId (Right (enca, sel, cxtb))) iworld
		# mbTaskb = case conts !! sel of
			(AnyTime _ taskbf)		= fmap taskbf (fromJSON enca)
			(WithResult	_ _ taskbf)	= fmap taskbf (fromJSON enca)
			(WithoutResult _ taskb)	= Just taskb
			(WhenStable taskbf)		= fmap taskbf (fromJSON enca)
			(Catch taskbf)			= fmap taskbf (fromJSON enca)
			(CatchAll taskbf)		= fmap taskbf (fromJSON enca)
		= case mbTaskb of
			Just taskb
				# (ncxtb,iworld)	= taskb.editFun event cxtb iworld
				= (TCStep taskId (Right (enca, sel, ncxtb)), iworld)
			Nothing
				= (context, iworld)
			
	edit event context iworld
		= (context, iworld)
		
	//Eval left-hand side
	eval eEvent cEvent repAs (TCStep taskId (Left cxta)) iworld 
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (subRepAs repAs taska) cxta iworld
		# mbcommit			= case cEvent of
			(Just (TaskEvent t action))
				| t == taskId	= Just action
			_					= Nothing
		# mbCont			= case resa of
			TaskInstable mba rep ncxta = case searchContInstable mba mbcommit conts of
				Nothing			= Left (TaskInstable Nothing (addStepActions taskId repAs rep mba) (TCStep taskId (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskStable a rep ncxta = case searchContStable a mbcommit conts of
				Nothing			= Left (TaskInstable Nothing (addStepActions taskId repAs rep (Just a)) (TCStep taskId (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskException e str = case searchContException e str conts of
				Nothing			= Left (TaskException e str)
				Just rewrite	= Right rewrite
		= case mbCont of
			Left res = (res,iworld)
			Right (sel,taskb,enca)
				# (taskIdB,iworld)	= getNextTaskId iworld
				# (cxtb,iworld)		= taskb.initFun taskIdB iworld
				# (resb,iworld)		= taskb.evalFun Nothing Nothing (subRepAs repAs taskb) cxtb iworld 
				= case resb of
					TaskInstable mbb rep ncxtb	= (TaskInstable mbb rep (TCStep taskId (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep ncxtb, iworld)
					TaskException e str			= (TaskException e str, iworld)
	//Eval right-hand side
	eval eEvent cEvent repAs (TCStep taskId (Right (enca,sel,cxtb))) iworld
		# mbTaskb = case conts !! sel of
			(AnyTime _ taskbf)		= fmap taskbf (fromJSON enca)
			(WithResult	_ _ taskbf)	= fmap taskbf (fromJSON enca)
			(WithoutResult _ taskb)	= Just taskb
			(WhenStable taskbf)		= fmap taskbf (fromJSON enca)
			(Catch taskbf)			= fmap taskbf (fromJSON enca)
			(CatchAll taskbf)		= fmap taskbf (fromJSON enca)
		= case mbTaskb of
			Just taskb
				# (resb, iworld)	= taskb.evalFun eEvent cEvent (subRepAs repAs taskb) cxtb iworld 
				= case resb of
					TaskInstable mbb rep ncxtb	= (TaskInstable mbb rep (TCStep taskId (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep ncxtb, iworld)
					TaskException e str			= (TaskException e str, iworld)
			Nothing
				= (taskException "Corrupt task value in step", iworld) 	
	//Incorred state
	eval eEvent cEvent _ context iworld
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
	
	addStepActions taskId (RepAsService _) rep mba = case rep of
		(ServiceRep (parts,actions))
			= ServiceRep (parts,actions ++ stepActions taskId mba)
		_	= rep
	addStepActions taskId (RepAsTUI Nothing layout) rep mba = case rep of
		(TUIRep gui)
			# layoutfun = fromMaybe DEFAULT_LAYOUT layout
			# fixme = []
			= TUIRep (layoutfun [gui] (stepActions taskId mba) [(TASK_ATTRIBUTE, toString taskId):fixme])	//TODO: Add attributes from task
		_	= rep
	addStepActions taskId (RepAsTUI (Just _) layout) rep mba
		= rep
	
	stepActions taskId mba = stepActions` conts
	where
		stepActions` [] = []
		stepActions` [AnyTime action _:cs]			= [(toString taskId,action,True):stepActions` cs]
		stepActions` [WithResult action pred _:cs]	= [(toString taskId,action,maybe False pred mba):stepActions` cs]
		stepActions` [WithoutResult action _:cs]	= [(toString taskId,action,isNothing mba):stepActions` cs]
		stepActions` [_:cs]							= stepActions` cs

	subRepAs (RepAsService target) _				= RepAsService target
	subRepAs (RepAsTUI target _) {Task|layout} 		= RepAsTUI target layout
		
// Parallel composition
:: ResultSet
	= RSException !Dynamic !String
	| RSStopped
	| RSResults ![(!TaskResult ParallelResult,!ParallelItem)]

parallel :: !d !a ![TaskContainer a] -> Task a | descr d & iTask a
parallel desc initState initTasks = mkTask init edit eval 
where
	//Create initial set of tasks and initial state
	init taskId iworld=:{IWorld|timestamp}
		# meta								= {nextIdx = length initTasks, stateVersion = 0, metaVersion = 0}
		# iworld							= addParState taskId initState meta iworld
		# iworld							= addParList taskId [] meta.metaVersion iworld 
		# (items,iworld)					= initParallelItems taskList (length initTasks - 1) initTasks iworld  
		# (state,meta,iworld)				= removeParState taskId meta iworld
		# iworld							= removeParList taskId iworld
		# encState							= encodeState state initState
		= (TCParallel taskId encState meta items, iworld)
	where
		taskList	= ParallelTaskList taskId		
		//Use decrementing stack order values (o)
		//To make sure that the first initial subtask has the highest order value
		//(this will ensure the first tab is active, or the first window will be on top)
		initParallelItems taskList o [] iworld = ([],iworld)
		initParallelItems taskList o [t:ts] iworld
			# (s,iworld)	= initParallelItem taskList o t iworld
			# (ss,iworld) 	= initParallelItems taskList (o - 1) ts iworld
			= ([s:ss], iworld)
					
	//Direct the event to the right place
	edit event context=:(TCParallel taskId encState meta items) iworld
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskId state meta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParList taskId items meta.metaVersion iworld 
		//Evaluate sub(s)
		# (TCParallel taskId encState meta items, iworld)
			= case event of
				//Event targeted at the parallel set, to move a task to front in the ordering
				(TaskEvent t ("top",JSONString top))
					| t == taskId
						= (TCParallel taskId encState meta (reorder (fromString top) items), iworld)
				_
					//Evaluate all sub-contexts
					# (items, iworld) = mapSt (editParallelItem event) items iworld
					= (TCParallel taskId encState meta items, iworld)
		//Remove the current state from the parallelStates scope in iworld
		# (state,meta,iworld)			= removeParState taskId meta iworld
		//Remove the task info overview
		# iworld						= removeParList taskId iworld
		//Encode state
		# encState	= encodeState state initState
		= (TCParallel taskId encState meta items,iworld)
				
	edit event context iworld
		= (context,iworld)
	
	editParallelItem event item=:{ParallelItem|task=(task :: Task ParallelResult),state} iworld=:{IWorld|latestEvent=parentLatestEvent}
		 # (state,iworld) = task.editFun event state iworld
		 = ({ParallelItem|item & state = state}, iworld)
		 
	//Eval all tasks in the set (in left to right order)
	eval eEvent cEvent repAs context=:(TCParallel taskId encState pmeta subs) iworld
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState taskId state pmeta iworld
		//Add the initial control structure to the parallelStates scope in iworld
		# iworld						= addParControl taskId pmeta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParList taskId subs pmeta.metaVersion iworld
		//Evaluate the sub tasks
		# (resultset,iworld)			= evalSubTasks taskId eEvent cEvent repAs pmeta [] subs iworld
		//Remove the current state from the parallelStates scope in iworld
		# (state,pmeta,iworld)			= removeParState taskId pmeta iworld
		//Remove the control structure from the parallelStates scope in iworld
		# (pmeta,iworld)				= removeParControl taskId pmeta iworld
		//Remove the task info overview from the parallelStates scropr in iworld
		# iworld						= removeParList taskId iworld 
		//Remove parallel task info
		= case resultset of
			//Exception
			RSException e str
				= (TaskException e str, iworld)
			RSStopped
				= (TaskStable state NoRep (TCBasic taskId (toJSON state) True), iworld)
			RSResults results
				# encState			= encodeState state initState
				# rep				= case repAs of
					(RepAsTUI target layout)
						= mergeReps taskId target (fromMaybe DEFAULT_LAYOUT layout) ([(TASK_ATTRIBUTE,toString taskId)] ++ initAttributes desc) results
					(RepAsService target)
						# fixme = ([],[])
						= ServiceRep fixme
				# items				= mergeStates results		
				| allStable results
					= (TaskStable state rep (TCParallel taskId encState pmeta items), iworld)
				| otherwise
					= (TaskInstable (Just state) rep (TCParallel taskId encState pmeta items), iworld)
	
	//When the parallel has been stopped, we have the state encoded in a TCBasic node
	eval eEvent cEvent repAs context=:(TCBasic taskId encState True) iworld
		= case fromJSON encState of
			Just state	= (TaskStable state NoRep context, iworld)
			Nothing		= (taskException "Corrupt task context in parallel", iworld)
			
	eval _ _ _ _ iworld
		= (taskException "Corrupt task context in parallel", iworld)
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks taskId eEvent cEvent repAs meta results [] iworld
		= (RSResults results, iworld)
	evalSubTasks taskId eEvent cEvent repAs meta results [item=:{ParallelItem|task=(task :: Task ParallelResult),state}:items] iworld=:{IWorld|latestEvent=parentLatestEvent,localDateTime}
		# (result,iworld)	= task.evalFun eEvent cEvent (subRepAs repAs taskId task) state iworld 
		# item	= case result of
			TaskInstable mbr rep state	= {ParallelItem|item & state = state}
			TaskStable r rep state		= {ParallelItem|item & state = state}
			TaskException e str			= item
		//Check for exception
		| isException result
			# (TaskException dyn str) = result
			= (RSException dyn str, iworld)
		//Check for Stop result (discards any pending additions/removals to the set, since they will be pointless anyway)
		| isStopped result
			= (RSStopped, iworld)
		//Append current result to results so far if it is not a stable Remove result
		# results	= if (isRemove result) results (results ++ [(result,item)])
		//Process controls
		# (controls, iworld)			= getControls taskId iworld
		# (meta,results,items,iworld)	= processControls initState taskId meta controls results items iworld
		//Evaluate remaining subtasks
		= evalSubTasks taskId eEvent cEvent repAs meta results items iworld
		
	initParallelItem taskList stack container iworld=:{IWorld|timestamp,localDateTime,currentUser}
		= case container of
			(Embedded, taskfun)
				# task				= taskfun taskList
				# (taskId,iworld)	= getNextTaskId iworld
				# (state,iworld)	= task.initFun taskId iworld
				= ({taskId = taskId, stack = stack, detached = False, progress = Nothing, management = Nothing
				   ,task = (dynamic task), state = state}, iworld)
			(Detached management, taskfun)
				# task				= taskfun taskList
				# progress			= initProgressMeta localDateTime currentUser
				# (taskId,iworld)	= getNextTaskId iworld
				# (state,iworld)	= task.initFun taskId iworld
				= ({taskId = taskId, stack = stack, detached = False, progress = Just progress, management = Just management
				   ,task = (dynamic task), state = state}, iworld)
		
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
	addParState :: !TaskId !s !ParallelMeta *IWorld -> *IWorld | TC s
	addParState taskId state {stateVersion} iworld=:{parallelStates} 
		= {iworld & parallelStates = 'Map'.put taskId (stateVersion, dynamic state :: s^) parallelStates}
	
	removeParState :: !TaskId !ParallelMeta !*IWorld -> (!s,!ParallelMeta,!*IWorld) | TC s
	removeParState taskId meta iworld=:{parallelStates}
		= case 'Map'.get taskId parallelStates of
			Just (version,state :: s^)	= (state,{meta & stateVersion = version},{iworld & parallelStates = 'Map'.del taskId parallelStates})
			_							= abort "Could not read parallel state"
	
	//Put the list representation in the scope
	//Put a datastructure in the scope with info on all processes in this set (TODO: Use parallel meta for identification)
	addParList :: !TaskId ![ParallelItem] !Int !*IWorld -> *IWorld
	addParList taskId items version iworld=:{parallelLists}
		= {iworld & parallelLists = 'Map'.put taskId (version,map toMeta items) parallelLists}
	where
		toMeta {ParallelItem|taskId,progress,management}
			= {ParallelTaskMeta|taskId = taskId, taskMeta = [],progressMeta = progress, managementMeta = management}
	
	removeParList :: !TaskId !*IWorld -> *IWorld
	removeParList taskId iworld=:{parallelLists}
		= {iworld & parallelLists = 'Map'.del taskId parallelLists}
	
	//Put the initial control structure in the scope
	addParControl :: !TaskId !ParallelMeta *IWorld -> *IWorld
	addParControl taskId meta=:{nextIdx} iworld=:{parallelControls}
		= {iworld & parallelControls = 'Map'.put taskId (nextIdx,[]) parallelControls}
		
	removeParControl :: !TaskId !ParallelMeta *IWorld -> (!ParallelMeta,!*IWorld)
	removeParControl taskId meta iworld=:{parallelControls}
		= case 'Map'.get taskId parallelControls of
			Just (nextIdx,_)	= ({meta & nextIdx = nextIdx},{iworld & parallelControls = 'Map'.del taskId parallelControls})
			_					= abort "Could not read parallel control"

	//Remove and return control values from the scope
	getControls :: !TaskId !*IWorld -> ([Control], !*IWorld)
	getControls taskId iworld=:{parallelControls}
		= case 'Map'.get taskId parallelControls of
			Just (nextIdx, controls)
				= (controls, {iworld & parallelControls = 'Map'.put taskId (nextIdx,[]) parallelControls})
			_
				= abort "Could not load parallel control data"
	
	processControls :: s !TaskId !ParallelMeta [Control] [(!TaskResult ParallelResult, !ParallelItem)] [ParallelItem] !*IWorld -> (!ParallelMeta, ![(!TaskResult ParallelResult, !ParallelItem)], ![ParallelItem],!*IWorld) | iTask s
	processControls s taskId meta [] results remaining iworld = (meta,results,remaining,iworld)
	processControls s taskId meta [c:cs] results remaining iworld
		# taskList = ParallelTaskList taskId
		= case c of
			AppendTask idx user (taskContainer :: (TaskContainer s^))
				# (state,iworld)	= initParallelItem taskList idx taskContainer iworld
				# remaining			= remaining ++ [state]
				= processControls s taskId meta cs results remaining iworld
			RemoveTask id
				# results			= [result \\ result=:(r,i) <- results | i.ParallelItem.taskId <> id]
				# remaining			= [item \\ item <- remaining | item.ParallelItem.taskId <> id]
				= processControls s taskId meta cs results remaining iworld
			_
				= processControls s taskId meta cs results remaining iworld 		
	
	subRepAs (RepAsTUI Nothing _) taskId task=:{layout}			= RepAsTUI Nothing layout
	subRepAs (RepAsTUI (Just target) _) taskId task=:{layout}
		| target == taskId										= RepAsTUI Nothing layout
																= RepAsTUI (Just target) layout			
	subRepAs (RepAsService Nothing) taskId task					= RepAsService Nothing 
	subRepAs (RepAsService (Just target)) taskId task
		| target == taskId										= RepAsService Nothing
																= RepAsService (Just target)
	
	taskMeta (TUIRep (_,_,attr))	= attr
	taskMeta _						= []
	
	isException (TaskException _ _)	= True
	isException _					= False
	
	isRemove (TaskStable Remove _ _)= True
	isRemove _						= False
	
	isStopped (TaskStable Stop _ _)	= True
	isStopped _						= False
	
	allStable []							= True
	allStable [(TaskStable _ _ _,_):rs]		= allStable rs
	allStable _								= False
	
	mergeStates :: [(!TaskResult ParallelResult, !ParallelItem)] -> [ParallelItem]
	mergeStates results = map snd results

	mergeReps taskId Nothing layout attributes results
		# parts = [appThd3 (kvSet STACK_ATTRIBUTE (toString stack) o kvSet TASK_ATTRIBUTE (toString taskId)) gui
					 \\ (TaskInstable _ (TUIRep gui) _,{ParallelItem|taskId,stack}) <- results]	
		= TUIRep (layout parts [] attributes)
	mergeReps taskId (Just target) layout attributes results
		//This parallel is the target
		| taskId == target
			# parts = [appThd3 (kvSet STACK_ATTRIBUTE (toString stack) o kvSet TASK_ATTRIBUTE (toString taskId)) gui
					 \\ (TaskInstable _ (TUIRep gui) _,{ParallelItem|taskId,stack}) <- results]	
			= TUIRep (layout parts [] attributes)
		| otherwise
			//If a target is set, only one of the branches should have a TUIRep representation
			= case [gui \\ (TaskInstable _ (TUIRep gui) _,_) <- results] of
				[part]	= TUIRep part
				_		= NoRep
					
	//Change the order of the subtask such that the indicated sub becomes top and the others
	//maintain their relative ordering
	reorder :: !TaskId ![ParallelItem] -> [ParallelItem]
	reorder top items
		= let (titems,ritems)	= splitWith isTop (sortByStack items)
			in (sortByTaskId [{ParallelItem|i & stack = o} \\ i <- (ritems ++ titems) & o <- [0..]])
	where							
		isTop {ParallelItem|taskId} = taskId == top
		sortByTaskId items = sortBy ( \{ParallelItem|taskId=a} {ParallelItem|taskId=b} -> a < b) items
		sortByStack items = sortBy ( \{ParallelItem|stack=a} {ParallelItem|stack=b} -> a < b) items
/**
* Get the shared state of a task list
*/
taskListState :: (TaskList s) -> Shared s | TC s
taskListState tasklist = ReadWriteShared [toString tasklist] read write getVersion
where
	taskId	= case tasklist of
		TopLevelTaskList = TaskId 0 0
		ParallelTaskList taskId = taskId

	read iworld=:{parallelStates}
		= case 'Map'.get taskId parallelStates of
				Just (_,val :: s^)	= (Ok val, iworld)
				_					= (Error ("Could not read shared parallel state of task list " +++ toString tasklist),iworld)
	
	write val iworld=:{parallelStates}
		# (mbv,iworld)	= getVersion iworld
		# version		= case mbv of (Ok v) = v ; _ = 0
		= (Ok Void, {iworld & parallelStates = 'Map'.put taskId (version + 1, (dynamic val :: s^)) parallelStates})
	
	getVersion iworld=:{parallelStates}
			= case 'Map'.get taskId parallelStates of
				Just (v,_)			= (Ok v, iworld)
				_					= (Error ("Could not read timestamp for shared state of task list " +++ toString tasklist),iworld)

/**
* Get the properties share of a task list
*/
taskListMeta :: (TaskList s) -> Shared [ParallelTaskMeta]
taskListMeta tasklist = ReadWriteShared [toString tasklist] read write getVersion
where
	taskId	= case tasklist of
		TopLevelTaskList = TaskId 0 0
		ParallelTaskList taskId = taskId
	
	read iworld=:{parallelLists}
		= case 'Map'.get taskId parallelLists of
			Just (_,list)	= (Ok list, iworld)
			_				= (Error ("Could not read parallel task list of " +++ toString tasklist), iworld)
		
	write val iworld=:{parallelLists}
		# fixme = Void //TODO
		= (Ok fixme, iworld)
			
	getVersion iworld=:{parallelLists}
		= case 'Map'.get taskId parallelLists of
			Just (version,_)	= (Ok version, iworld)
			_					= (Error ("Could not read timestamp for parallel task list of " +++ toString tasklist), iworld)
		
/**
* Add a task to a task list
*/
appendTask :: !(TaskContainer s) !(TaskList s)	-> Task Int | TC s
appendTask container tasklist = mkInstantTask eval
where
	eval _ iworld=:{parallelControls,currentUser}
		# identity	= toString tasklist
		# taskId	= case tasklist of
			TopLevelTaskList = TaskId 0 0
			ParallelTaskList taskId = taskId
		= case 'Map'.get taskId parallelControls of
			Just (nextIdx,controls)
				//For the global tasklist we don't use the internal counter, but get the index from the
				//process database
				# (nextIdx, iworld) = case tasklist of
					TopLevelTaskList	= newTopNo iworld
					_					= (nextIdx,iworld)
				# parallelControls = 'Map'.put taskId (nextIdx + 1, controls ++ [AppendTask nextIdx currentUser (dynamic container :: TaskContainer s^)]) parallelControls 
				= (TaskStable nextIdx NoRep (TCEmpty taskId), {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (taskException ("Task list " +++ identity +++ " is not in scope"), iworld)
	
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(TaskList s) -> Task Void | TC s
removeTask remId tasklist = mkInstantTask (removeTask` remId tasklist)
where
	removeTask` :: !TaskId !(TaskList s) TaskId *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` remId tasklist listId iworld=:{parallelControls}
		= case 'Map'.get listId parallelControls of
			Just (nextIdx,controls)
				# parallelControls = 'Map'.put listId (nextIdx, controls ++ [RemoveTask remId]) parallelControls
				= (TaskStable Void NoRep (TCEmpty listId), {iworld & parallelControls = parallelControls })
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
	init f taskId iworld=:{currentUser}
		# (context,iworld) = f taskId {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})

	edit f eevent context iworld=:{currentUser}
		# (context,iworld) = f eevent context {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})
	
	eval f eEvent cEvent repAs context iworld=:{currentUser}
		# (result,iworld) = f eEvent cEvent repAs context {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})

/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a

instance tune SetLayout
where tune (SetLayout layout) task				= {Task|task & layout = Just layout}

instance tune ModifyLayout
where tune (ModifyLayout f) task=:{Task|layout}	= {Task|task & layout = Just (f (fromMaybe DEFAULT_LAYOUT layout))} 
