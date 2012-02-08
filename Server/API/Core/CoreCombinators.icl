implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, TaskStore, Util, HTTP, GenUpdate, GenEq, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List
import iTaskClass, InteractionTasks, LayoutCombinators, TUIDefinition

from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..), :: Control(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from TaskEval			import taskListShare
from CoreTasks			import return
from SharedDataSource	import :: RWRes(..), readWrite, getIds, :: ShareId

derive class iTask ParallelResult, ParallelTaskType

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{evalStack=[TaskId topNo _:_],nextTaskNo} = (TaskId topNo nextTaskNo, {IWorld|iworld & nextTaskNo = nextTaskNo + 1})
getNextTaskId iworld = abort "Empty evaluation stack"

transform :: ((Maybe a) -> Maybe b) !(Task a) -> Task b | iTask a & iTask b 
transform f task=:{Task|evalFun} = {Task|task & evalFun = evalFun`}
where
	evalFun` eEvent cEvent repAs cxt iworld
		= case evalFun eEvent cEvent repAs cxt iworld of
			(TaskUnstable mba rep cxt, iworld)	= (TaskUnstable (f mba) rep cxt, iworld)
			(TaskStable a rep cxt, iworld)		= case f (Just a) of
				(Just b)	= (TaskStable b rep cxt, iworld)
				Nothing		= (taskException "Task with permanent invalid result", iworld)
			(TaskException e str, iworld)		= (TaskException e str, iworld)

project	:: ((Maybe a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a
project projection share taska = mkTask init edit eval
where
	init taskId iworld
		# (taskIdA,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskId iworld
		= (TCProject taskId JSONNull inita, iworld)
	
	//Pass along edit events
	edit event context=:(TCProject taskId prev cxta) iworld
		# (ncxta,iworld)	= taska.editFun event cxta iworld
		= (TCProject taskId prev ncxta, iworld)
		
	//Eval task and check for change
	eval eEvent cEvent repAs (TCProject taskId prev cxta) iworld
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		= case resa of
			TaskUnstable mba rep ncxta 
				| changed prev mba	
					= projectOnShare mba (TaskUnstable mba rep (TCProject taskId (toJSON mba) ncxta)) iworld
				| otherwise
					= (TaskUnstable mba rep (TCProject taskId prev ncxta), iworld)
			TaskStable a rep ncxta
				| changed prev (Just a)
					= projectOnShare (Just a) (TaskStable a rep (TCProject taskId (toJSON (Just a)) ncxta)) iworld
				| otherwise
					= (TaskStable a rep (TCProject taskId prev ncxta), iworld)
			TaskException e str
				= (TaskException e str,iworld)
	
	subRepAs (RepAsService target) _  			= (RepAsService target)
	subRepAs (RepAsTUI target mbLayout) task=:{layout} = case mbLayout of
		Nothing			= RepAsTUI target layout
		Just overwrite	= RepAsTUI target (Just overwrite)
		
	changed encprev cur = case fromJSON encprev of
		Nothing		= fixme where fixme = True	//THIS ASSUMPTION DOES NOT ALWAYS HOLD! Consider changed when parsing fails
		Just prev	= prev =!= cur
	
	projectOnShare mba result iworld
		= case readWrite (\r _ -> case projection mba r of Just w = Write w Void; Nothing = YieldResult Void) share iworld of
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
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		# mbcommit			= case cEvent of
			(Just (TaskEvent t action))
				| t == taskId	= Just action
			_					= Nothing
		# mbCont			= case resa of
			TaskUnstable mba rep ncxta = case searchContInstable mba mbcommit conts of
				Nothing			= Left (TaskUnstable Nothing (addStepActions taskId repAs rep mba) (TCStep taskId (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskStable a rep ncxta = case searchContStable a mbcommit conts of
				Nothing			= Left (TaskUnstable Nothing (addStepActions taskId repAs rep (Just a)) (TCStep taskId (Left ncxta)))
				Just rewrite	= Right rewrite
			TaskException e str = case searchContException e str conts of
				Nothing			= Left (TaskException e str)
				Just rewrite	= Right rewrite
		= case mbCont of
			Left res = (res,iworld)
			Right (sel,taskb,enca)
				# (taskIdB,iworld)	= getNextTaskId iworld
				# (cxtb,iworld)		= taskb.initFun taskIdB iworld
				# (resb,iworld)		= taskb.evalFun Nothing Nothing (matchTarget (subRepAs repAs taskb) taskId) cxtb iworld 
				= case resb of
					TaskUnstable mbb rep ncxtb	= (TaskUnstable mbb rep (TCStep taskId (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep (TCStep taskId (Right (enca,sel,ncxtb))), iworld)
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
				# (resb, iworld)	= taskb.evalFun eEvent cEvent (matchTarget (subRepAs repAs taskb) taskId) cxtb iworld 
				= case resb of
					TaskUnstable mbb rep ncxtb	= (TaskUnstable mbb rep (TCStep taskId (Right (enca,sel,ncxtb))),iworld)
					TaskStable b rep ncxtb		= (TaskStable b rep (TCStep taskId (Right (enca,sel,ncxtb))), iworld)
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
		(ServiceRep (parts,actions,attributes))
			= ServiceRep (parts,actions ++ stepActions taskId mba,attributes)
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

parallel :: !d !a ![(!ParallelTaskType,!ParallelTask a)] -> Task a | descr d & iTask a
parallel desc initState initTasks = mkTask init edit eval 
where
	//Create initial set of tasks and initial state
	init taskId iworld=:{IWorld|timestamp}
		# meta								= {nextIdx = length initTasks, stateVersion = 0, metaVersion = 0}
		# iworld							= addParState listId initState meta iworld
		# iworld							= addParList listId [] meta.metaVersion iworld 
		# (items,iworld)					= initParallelItems (length initTasks - 1) initTasks iworld  
		# (state,meta,iworld)				= removeParState listId meta iworld
		# iworld							= removeParList listId iworld
		# encState							= encodeState state initState
		= (TCParallel taskId encState meta items, iworld)
	where
		listId = toString (ParallelTaskList taskId)
		//Use decrementing stack order values (o)
		//To make sure that the first initial subtask has the highest order value
		//(this will ensure the first tab is active, or the first window will be on top)
		initParallelItems o [] iworld = ([],iworld)
		initParallelItems o [(t,p):ts] iworld
			# (s,iworld)		= initParallelItem taskList o t p iworld
			# (ss,iworld) 		= initParallelItems (o - 1) ts iworld
			= ([s:ss], iworld)
			
		taskList = taskListShare (ParallelTaskList taskId)				
	//Direct the event to the right place
	edit event context=:(TCParallel taskId encState meta items) iworld
		# listId						= toString (ParallelTaskList taskId)
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState listId state meta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParList listId items meta.metaVersion iworld 
		//Evaluate sub(s)
		# (TCParallel taskId encState meta items, iworld)
			= case event of
				//Event targeted at the parallel set, to move a task to front in the ordering
				(TaskEvent t ("top",JSONString top))
					| t == taskId
						= (TCParallel taskId encState meta (reorder (fromString top) items), iworld)
				_
					//Evaluate all sub-contexts
					# (items, iworld) = mapSt (editParallelItem state taskId event) items iworld
					= (TCParallel taskId encState meta items, iworld)
		//Remove the current state from the parallelStates scope in iworld
		# (state,meta,iworld)			= removeParState listId meta iworld
		//Remove the task info overview
		# iworld						= removeParList listId iworld
		//Encode state
		# encState	= encodeState state initState
		= (TCParallel taskId encState meta items,iworld)
				
	edit event context iworld
		= (context,iworld)

	editParallelItem :: a !TaskId !EditEvent !ParallelItem !*IWorld -> (!ParallelItem,!*IWorld) | iTask a
	editParallelItem _ taskId event item=:{ParallelItem|task=(parTask :: ParallelTask a^),state} iworld=:{IWorld|latestEvent=parentLatestEvent}
		# taskList			= taskListShare (ParallelTaskList taskId)
		# task				= parTask taskList
		# (state,iworld)	= task.editFun event state iworld
		= ({ParallelItem|item & state = state}, iworld)
		 
	//Eval all tasks in the set (in left to right order)
	eval eEvent cEvent repAs context=:(TCParallel taskId encState pmeta subs) iworld
		# listId						= toString (ParallelTaskList taskId)
		//Add the current state to the parallelStates scope in iworld
		# state							= decodeState encState initState 
		# iworld						= addParState listId state pmeta iworld
		//Add the initial control structure to the parallelStates scope in iworld
		# iworld						= addParControl listId pmeta iworld
		//Put the initial parallel task info overview in the parallelStates scope in iworld
		# iworld						= addParList listId subs pmeta.metaVersion iworld
		//Evaluate the sub tasks
		# (resultset,iworld)			= evalSubTasks state taskId eEvent cEvent repAs pmeta [] subs iworld
		//Remove the current state from the parallelStates scope in iworld
		# (state,pmeta,iworld)			= removeParState listId pmeta iworld
		//Remove the control structure from the parallelStates scope in iworld
		# (pmeta,iworld)				= removeParControl listId pmeta iworld
		//Remove the task info overview from the parallelStates scropr in iworld
		# iworld						= removeParList listId iworld 
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
						# fixme = ([],[],[])
						= ServiceRep fixme
				# items				= mergeStates results		
				| allStable results
					= (TaskStable state rep (TCParallel taskId encState pmeta items), iworld)
				| otherwise
					= (TaskUnstable (Just state) rep (TCParallel taskId encState pmeta items), iworld)
	
	//When the parallel has been stopped, we have the state encoded in a TCBasic node
	eval eEvent cEvent repAs context=:(TCBasic taskId encState True) iworld
		= case fromJSON encState of
			Just state	= (TaskStable state NoRep context, iworld)
			Nothing		= (taskException "Corrupt task context in parallel", iworld)
			
	eval _ _ _ _ iworld
		= (taskException "Corrupt task context in parallel", iworld)
	//Keep evaluating tasks and updating the state until there are no more subtasks
	//subtasks
	evalSubTasks :: a !TaskId !(Maybe EditEvent) !(Maybe CommitEvent) !TaskRepTarget !ParallelMeta ![(!TaskResult ParallelResult,!ParallelItem)] ![ParallelItem] !*IWorld -> (!ResultSet,!*IWorld) | iTask a
	evalSubTasks s taskId eEvent cEvent repAs meta results [] iworld
		= (RSResults results, iworld)
	evalSubTasks s taskId eEvent cEvent repAs meta results [item=:{ParallelItem|task=(parTask :: ParallelTask a^),state}:items] iworld=:{IWorld|latestEvent=parentLatestEvent,localDateTime}
		# listId			= toString (ParallelTaskList taskId)
		# taskList			= taskListShare (ParallelTaskList taskId)
		# task				= parTask taskList
		# (result,iworld)	= task.evalFun eEvent cEvent (subRepAs repAs taskId task) state iworld 
		# item	= case result of
			TaskUnstable mbr rep state	= {ParallelItem|item & state = state}
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
		# (controls, iworld)			= getControls listId iworld
		# (meta,results,items,iworld)	= processControls s taskId meta controls results items iworld
		//Evaluate remaining subtasks
		= evalSubTasks s taskId eEvent cEvent repAs meta results items iworld
	evalSubTasks _ _ _ _ _ _ _ _ _ = abort "Corrupt task context in parallel"
	
	initParallelItem :: !(SharedTaskList s) !Int !ParallelTaskType !((SharedTaskList s) -> Task ParallelResult) !*IWorld -> (!ParallelItem,!*IWorld) | TC s
	initParallelItem taskList stack parType parTask iworld=:{IWorld|timestamp,localDateTime,currentUser}
		= case parType of
			Embedded
				# task				= parTask taskList
				# (taskId,iworld)	= getNextTaskId iworld
				# (state,iworld)	= task.initFun taskId iworld
				= ({taskId = taskId, stack = stack, detached = False, progress = Nothing, management = Nothing
				   ,task = (dynamic parTask :: ParallelTask s^), state = state, attributes = []}, iworld)
			Detached management
				# task				= parTask taskList
				# progress			= initProgressMeta localDateTime currentUser
				# (taskId,iworld)	= getNextTaskId iworld
				# (state,iworld)	= task.initFun taskId iworld
				= ({taskId = taskId, stack = stack, detached = True, progress = Just progress, management = Just management
				   ,task = (dynamic parTask :: ParallelTask s^), state = state, attributes = []}, iworld)
		
	//Initialize a process properties record for administration of detached tasks
	initProgressMeta now user
		= {ProgressMeta|status=Unstable,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing}
		
	//IMPORTANT: The second argument is never used, but passed just to solve overloading
	encodeState :: !s s -> JSONNode | JSONEncode{|*|} s
	encodeState val _ = toJSON val
	
	//IMPORTANT: The second argument is never used, but passed just to solve overloading
	decodeState :: !JSONNode s -> s | JSONDecode{|*|} s
	decodeState encState _ = case fromJSON encState of
		Just val 	= val
		_			= abort "Could not decode parallel state"
		
	//Put the shared state in the scope
	addParState :: !String !s !ParallelMeta *IWorld -> *IWorld | TC s
	addParState listId state {stateVersion} iworld=:{parallelStates} 
		= {iworld & parallelStates = 'Map'.put listId (stateVersion, dynamic state :: s^) parallelStates}
	
	removeParState :: !String !ParallelMeta !*IWorld -> (!s,!ParallelMeta,!*IWorld) | TC s
	removeParState listId meta iworld=:{parallelStates}
		= case 'Map'.get listId parallelStates of
			Just (version,state :: s^)	= (state,{meta & stateVersion = version},{iworld & parallelStates = 'Map'.del listId parallelStates})
			_							= abort "Could not read parallel state"
	
	//Put the list representation in the scope
	//Put a datastructure in the scope with info on all processes in this set (TODO: Use parallel meta for identification)
	addParList :: !String ![ParallelItem] !Int !*IWorld -> *IWorld
	addParList listId items version iworld=:{parallelLists}
		= {iworld & parallelLists = 'Map'.put ("taskList:" +++ listId) (version,map toMeta items) parallelLists}
	where
		toMeta {ParallelItem|taskId,progress,management,state,attributes}
			= {TaskListItem|taskId = taskId, taskMeta = attributes, progressMeta = progress, managementMeta = management, subItems = stateToTaskListItems state}
	
	removeParList :: !String !*IWorld -> *IWorld
	removeParList listId iworld=:{parallelLists}
		= {iworld & parallelLists = 'Map'.del ("taskList:" +++ listId) parallelLists}
	
	//Put the initial control structure in the scope
	addParControl :: !String !ParallelMeta *IWorld -> *IWorld
	addParControl listId meta=:{nextIdx} iworld=:{parallelControls}
		= {iworld & parallelControls = 'Map'.put ("taskList:" +++ listId) (nextIdx,[]) parallelControls}
		
	removeParControl :: !String !ParallelMeta *IWorld -> (!ParallelMeta,!*IWorld)
	removeParControl listId meta iworld=:{parallelControls}
		= case 'Map'.get ("taskList:" +++ listId) parallelControls of
			Just (nextIdx,_)	= ({meta & nextIdx = nextIdx},{iworld & parallelControls = 'Map'.del ("taskList:" +++ listId) parallelControls})
			_					= abort "Could not read parallel control"

	//Remove and return control values from the scope
	getControls :: !String !*IWorld -> ([Control], !*IWorld)
	getControls listId iworld=:{parallelControls}
		= case 'Map'.get ("taskList:" +++ listId) parallelControls of
			Just (nextIdx, controls)
				= (controls, {iworld & parallelControls = 'Map'.put ("taskList:" +++ listId) (nextIdx,[]) parallelControls})
			_
				= abort "Could not load parallel control data"
	
	processControls :: s !TaskId !ParallelMeta [Control] [(!TaskResult ParallelResult, !ParallelItem)] [ParallelItem] !*IWorld -> (!ParallelMeta, ![(!TaskResult ParallelResult, !ParallelItem)], ![ParallelItem],!*IWorld) | iTask s
	processControls s taskId meta [] results remaining iworld = (meta,results,remaining,iworld)
	processControls s taskId meta [c:cs] results remaining iworld
		# taskList = taskListShare (ParallelTaskList taskId)
		= case c of
			AppendTask idx user parType (parTask :: ParallelTask s^)
				# (state,iworld)	= initParallelItem taskList idx parType parTask iworld
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

	mergeReps taskId mbTarget layout attributes results
		//This parallel is the target or no target is set
		| show mbTarget
			# parts = [appThd3 (kvSet STACK_ATTRIBUTE (toString stack) o kvSet TASK_ATTRIBUTE (toString taskId)) gui
					 \\ (TaskUnstable _ (TUIRep gui) _,{ParallelItem|taskId,stack,detached}) <- results | not detached]	
			= TUIRep (layout parts [] attributes)
		| otherwise
			//If a target is set, only one of the branches should have a TUIRep representation
			= case [gui \\ (TaskUnstable _ (TUIRep gui) _,_) <- results] of
				[part]	= TUIRep part
				parts	= NoRep
	where
		show Nothing			= True
		show (Just target)	= target == taskId	
						
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

import StdDebug
		
//Derived shares
taskListState :: !(SharedTaskList s) -> Shared s 
taskListState tasklist = mapRead (\{TaskList|state} -> state) tasklist

taskListMeta :: !(SharedTaskList s) -> ReadOnlyShared [TaskListItem]
taskListMeta tasklist = mapReadWrite (\{TaskList|items} -> items, \_ {TaskList|state} -> Just state) tasklist

/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask s) !(SharedTaskList s) -> Task Int | TC s
appendTask parType parTask tasklist = mkInstantTask eval
where
	listId = hd (getIds tasklist)
	eval taskId iworld=:{parallelControls,currentUser}
		= case 'Map'.get listId parallelControls of
			Just (nextIdx,controls)
				//For the global tasklist we don't use the internal counter, but get the index from the
				//process database
				# (nextIdx, iworld) = case listId of
					"taskList:tasklist-top"	= newTopNo iworld
					_								= (nextIdx,iworld)
				# parallelControls = 'Map'.put listId (nextIdx + 1, controls ++ [AppendTask nextIdx currentUser parType (dynamic parTask :: ParallelTask s^)]) parallelControls 
				= (TaskStable nextIdx NoRep (TCEmpty taskId), {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (taskException ("Task list " +++ listId +++ " is not in scope"), iworld)
	
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList s) -> Task Void | TC s
removeTask remId tasklist = mkInstantTask (removeTask` remId tasklist)
where
	listId = hd (getIds tasklist)
	removeTask` :: !TaskId !(SharedTaskList s) TaskId *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` remId _ taskId iworld=:{parallelControls}
		= case 'Map'.get listId parallelControls of
			Just (nextIdx,controls)
				# parallelControls = 'Map'.put listId (nextIdx, controls ++ [RemoveTask remId]) parallelControls
				= (TaskStable Void NoRep (TCEmpty taskId), {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (taskException ("Task list " +++ listId +++ " is not in scope"), iworld)

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

/**
* Helper function that sets the target task id to Nothing if it matches the given task id.
* This ensures that a representation for the full sub-tree is generated when determining the representation target for sub trees.
*/
matchTarget :: !TaskRepTarget !TaskId -> TaskRepTarget
matchTarget repAs=:(RepAsTUI (Just target) layout) taskId	= if (target == taskId) (RepAsTUI Nothing layout) repAs
matchTarget repAs=:(RepAsService (Just target)) taskId		= if (target == taskId) (RepAsService Nothing) repAs
matchTarget repAs taskId									= repAs