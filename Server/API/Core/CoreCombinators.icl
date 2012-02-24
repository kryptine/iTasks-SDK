implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskContext, TaskStore, Util, HTTP, GenUpdate, GenEq, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List
import iTaskClass, InteractionTasks, LayoutCombinators, TUIDefinition

from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from TaskEval			import taskListShare
from CoreTasks			import return
from SharedDataSource	import :: RWRes(..), readWrite, getIds, :: ShareId

derive class iTask ParallelTaskType

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{evalStack=[TaskId topNo _:_],nextTaskNo} = (TaskId topNo nextTaskNo, {IWorld|iworld & nextTaskNo = nextTaskNo + 1})
getNextTaskId iworld = abort "Empty evaluation stack"

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f task=:{Task|evalFun} = {Task|task & evalFun = evalFun`}
where
	evalFun` eEvent cEvent repAs cxt iworld
		= case evalFun eEvent cEvent repAs cxt iworld of
			(ValueResult val lastEvent rep cxt,iworld)	= (ValueResult (f val) lastEvent rep cxt, iworld)	//TODO: guarantee stability
			(ExceptionResult e str, iworld)				= (ExceptionResult e str, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a
project projection share taska = mkTask init eval
where
	init taskId iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskId iworld
		= (TCProject taskId (toJSON (initPrev taska)) inita, iworld)
	where
		initPrev :: (Task a) -> TaskValue a //Solve overloading
		initPrev _ = NoValue
		
	//Eval task and check for change
	eval eEvent cEvent repAs (TCProject taskId encprev cxta) iworld
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		= case resa of
			ValueResult val lastEvent rep ncxta
				| val =!= (fromJust (fromJSON encprev))
					= projectOnShare val (ValueResult val lastEvent rep (TCProject taskId (toJSON val) ncxta)) iworld
				| otherwise
					= (ValueResult val lastEvent rep (TCProject taskId encprev ncxta), iworld)
			ExceptionResult e str
				= (ExceptionResult e str,iworld)
	
	subRepAs (RepAsService target) _ = (RepAsService target)
	subRepAs (RepAsTUI target mbLayout) task=:{layout} = case mbLayout of
		Nothing			= RepAsTUI target layout
		Just overwrite	= RepAsTUI target (Just overwrite)
		
	projectOnShare val result iworld
		= case readWrite (\r _ -> case projection val r of Just w = Write w Void; Nothing = YieldResult Void) share iworld of
			(Ok _,iworld)		= (result,iworld)
			(Error e,iworld)	= (exception e,iworld)


step :: (Task a) [TaskStep a b] -> Task b | iTask a & iTask b
step taska conts = mkTask init eval
where
	init taskId iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskIda iworld
		= (TCStep taskId (Left inita), iworld)		

	//Eval left-hand side
	eval eEvent cEvent repAs (TCStep taskId (Left cxta)) iworld 
		# (resa, iworld) 	= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		# mbcommit			= case cEvent of
			(Just (TaskEvent t action))
				| t == taskId	= Just action
			_					= Nothing
		# mbCont			= case resa of
			ValueResult val lastEvent rep ncxta = case searchContValue val mbcommit conts of
				Nothing			= Left (ValueResult NoValue lastEvent (addStepActions taskId repAs rep val) (TCStep taskId (Left ncxta)) )
				Just rewrite	= Right rewrite
			ExceptionResult e str = case searchContException e str conts of
				Nothing			= Left (ExceptionResult e str)
				Just rewrite	= Right rewrite
		= case mbCont of
			Left res = (res,iworld)
			Right (sel,taskb,enca)
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (cxtb,iworld)		= taskb.initFun taskIdb iworld
				# (resb,iworld)		= taskb.evalFun Nothing Nothing (matchTarget (subRepAs repAs taskb) taskId) cxtb iworld 
				= case resb of
					ValueResult val lastEvent rep ncxtb		= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,ncxtb))),iworld)
					ExceptionResult e str					= (ExceptionResult e str, iworld)
	//Eval right-hand side
	eval eEvent cEvent repAs (TCStep taskId (Right (enca,sel,cxtb))) iworld
		# mbTaskb = case conts !! sel of
			(OnValue _ taskbf)			= fmap taskbf (fromJSON enca)
			(OnAction _ _ taskbf)		= fmap taskbf (fromJSON enca)
			(OnException taskbf)		= fmap taskbf (fromJSON enca)
			(OnAllExceptions taskbf)	= fmap taskbf (fromJSON enca)
		= case mbTaskb of
			Just taskb
				# (resb, iworld)	= taskb.evalFun eEvent cEvent (matchTarget (subRepAs repAs taskb) taskId) cxtb iworld 
				= case resb of
					ValueResult val lastEvent rep ncxtb	= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,ncxtb))), iworld)
					ExceptionResult e str				= (ExceptionResult e str, iworld)
			Nothing
				= (exception "Corrupt task value in step", iworld) 	
	//Incorred state
	eval eEvent cEvent _ context iworld
		= (exception "Corrupt task context in step", iworld)

	searchContValue val mbcommit conts = search val mbcommit 0 Nothing conts
	where
		search _ _ _ mbmatch []							= mbmatch									//No matching OnValue steps were found, return the potential match
		search val mbcommit i mbmatch [OnValue pred f:cs]
			| pred val									= Just (i, f val, toJSON val)				//Don't look any further, first matching trigger wins
														= search val mbcommit (i + 1) mbmatch cs	//Keep search
		search val (Just commit) i Nothing [OnAction action pred f:cs]
			| pred val && commit == actionName action	= search val (Just commit) (i + 1) (Just (i, f val, toJSON val)) cs //We found a potential winner (if no OnValue values are in cs)
														= search val (Just commit) (i + 1) Nothing cs						//Keep searching
		search val mbcommit i mbmatch [_:cs]			= search val mbcommit (i + 1) mbmatch cs							//Keep searching
		
	searchContException dyn str conts = search dyn str 0 Nothing conts
	where
		search _ _ _ catchall []					= catchall													//Return the maybe catchall
		search dyn str i catchall [OnException f:cs] = case (match f dyn) of
			Just (taskb,enca)						= Just (i, taskb, enca)										//We have a match
			_										= search dyn str (i + 1) catchall cs						//Keep searching
		search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, toJSON str)) cs 	//Keep searching (at least we have a catchall)
		search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs						//Keep searching
				
		match :: (e -> Task b) Dynamic -> Maybe (Task b, JSONNode) | iTask e
		match f (e :: e^)	= Just (f e, toJSON e)
		match _ _			= Nothing 
	
	addStepActions taskId (RepAsService _) rep val = case rep of
		(ServiceRep (parts,actions,attributes))
			= ServiceRep (parts,actions ++ stepActions taskId val,attributes)
		_	= rep
	addStepActions taskId (RepAsTUI Nothing layout) rep val = case rep of
		(TUIRep gui)
			# layoutfun = fromMaybe DEFAULT_LAYOUT layout
			# fixme = []
			= TUIRep (layoutfun SequentialComposition [gui] (stepActions taskId val) [(TASK_ATTRIBUTE, toString taskId):fixme])	//TODO: Add attributes from task
		_	
			# layoutfun = fromMaybe DEFAULT_LAYOUT layout
			= TUIRep (layoutfun SequentialComposition [] (stepActions taskId val) [(TASK_ATTRIBUTE, toString taskId)])
	addStepActions taskId (RepAsTUI (Just _) layout) rep val
		= rep
	
	stepActions taskId val = [(toString taskId,action,pred val)\\ OnAction action pred _ <- conts]
	
	subRepAs (RepAsService target) _				= RepAsService target
	subRepAs (RepAsTUI target _) {Task|layout} 		= RepAsTUI target layout

// Parallel composition
:: ResultSet a
	= RSException !Dynamic !String
	| RSResults	![(!ParallelItem,!TaskResult a)]

//Phantom type wrapper for task id's used to solve overloading
:: ListId a = ListId TaskId

parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
parallel desc initTasks = mkTask init eval 
where
	//Create initial set of tasks and initial state
	init taskId iworld=:{IWorld|timestamp}	
		//Create the list of initial parallel items
		# (items,iworld)					= mkParallelItems listId initTasks (length initTasks - 1) iworld
		//Initialize all initial items
		# (items,iworld)					= initParallelItems listId 0 items 0 iworld  
		= (TCParallel taskId listMeta items, iworld)
	where
		listId		= ListId taskId //Passed to subfunctions to solve overloading
		listMeta	= {nextIdx = length initTasks, listVersion = 0}	

	eval eEvent cEvent repAs context=:(TCParallel taskId meta items) iworld
		//If a reorder edit event is given, reorder the stack 
		# items = case eEvent of 
			Just (TaskEvent t ("top",JSONString top))	= reorder (fromString top) items
			_											= items
		//Eval all subtasks 
		# (results,nextIdx,iworld)	= evalParallelItems (listId taskId initTasks) eEvent cEvent repAs 0 (zip (items,repeat Nothing)) meta.nextIdx iworld
		= case results of
			RSException e str
				= (ExceptionResult e str, iworld)
			RSResults itemsAndResults
				# rep				= mergeTaskReps taskId desc repAs itemsAndResults
				# (items,results)	= unzip itemsAndResults
				# values			= [(lastEvent,val) \\ ValueResult val lastEvent _ _ <- results]	
				# stable			= if (all (isStable o snd) values) Stable Unstable
				# lastEvent			= foldr max 0 (map fst values)
				= (ValueResult (Value values stable) lastEvent rep (TCParallel taskId meta items), iworld)			
	where
		isStable (Value _ Stable)	= True
		isStable _					= False
		
		listKey = toString (ParallelTaskList taskId)
	//Fallback
	eval _ _ _ _ iworld
		= (exception "Corrupt task context in parallel", iworld)
		
	//Helper function to unify ListId type parameter with type parameter of initial task set
	listId :: !TaskId [(!ParallelTaskType,!ParallelTask a)] -> ListId a	
	listId taskId _ = ListId taskId

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
			
	evalParallelItems :: !(ListId a) !(Maybe EditEvent) !(Maybe CommitEvent) !TaskRepTarget !Int ![(ParallelItem, Maybe (TaskResult a))] !Int !*IWorld -> (!ResultSet a, !Int, !*IWorld) | iTask a
	evalParallelItems listId=:(ListId listTaskId) eEvent cEvent repAs index items nextIdx iworld
		| index >= length items
			//All done, return result set and remove list from scope
			# iworld = unshareParallelList listId iworld
			# iworld = disableControl listId iworld
			= (RSResults [(item,result) \\ (item,Just result) <- items], nextIdx, iworld)
		| otherwise
			//Update scope
			# iworld		= shareParallelList listId (map fst items) version iworld 
			//Enable control
			# iworld		= enableControl listId nextIdx iworld
			//Evaluate item at index
			# (result,item,iworld)		= evalParallelItem listId eEvent cEvent repAs (fst (items !! index)) iworld
			//Process additions and removals from the list
			# (controls,nextIdx,iworld)	= takeControl listId iworld
			# (items,iworld)			= processControls listId controls (updateAt index (item,Just result) items) iworld
			# index						= if (isEmpty controls) index (resetIndex items) 	//If items have been removed, the index may not be correct anymore
			= case result of
				ExceptionResult e str	//Clean scope and return exeption
					# iworld = unshareParallelList listId iworld
					= (RSException e str, nextIdx, iworld)
				_					//Evaluate other items
					= evalParallelItems listId eEvent cEvent repAs (index + 1) items nextIdx iworld
	where
		version = fixme where fixme = 0
		
	evalParallelItem :: !(ListId a) !(Maybe EditEvent) !(Maybe CommitEvent) !TaskRepTarget !ParallelItem !*IWorld -> (!TaskResult a,!ParallelItem,!*IWorld) | iTask a
	evalParallelItem listId=:(ListId listTaskId) eEvent cEvent repAs item=:{ParallelItem|task=(parTask :: ParallelTask a^),state} iworld
		# (result, iworld)	= task.evalFun eEvent cEvent (subRepAs repAs listTaskId task) state iworld
		# item	= case result of
			ValueResult val _ rep state		= {ParallelItem|item & state = state, lastValue = toJSON val} //TODO: set lastAttributes
			ExceptionResult e str			= item
		= (result,item,iworld)
	where
		task = parTask listShare
		listShare = taskListShare (ParallelTaskList listTaskId)
	
	processControls :: !(ListId a) ![ParallelControl] ![(ParallelItem,Maybe (TaskResult a))] !*IWorld -> (![(ParallelItem,Maybe (TaskResult a))],!*IWorld) | iTask a
	processControls listId [] items iworld = (items,iworld)
	processControls listId [AppendTask item:controls] items iworld
		//Initialize the new item and append for later evaluation			//TODO: Maybe initialize should only happen just before evaluation?
		# (item,iworld)	= initParallelItem listId item iworld	
		= processControls listId controls (items ++ [(item,Nothing)]) iworld
	processControls listId [RemoveTask taskId:controls] items iworld
		//Filter the item from the set
		# items = [item \\ item <- items | (fst item).ParallelItem.taskId <> taskId]
		= processControls listId controls items iworld
	
	resetIndex :: [(ParallelItem,Maybe (TaskResult a))] -> Int
	resetIndex items = reset 0 items
	where
		reset idx [(_,Just _):items]	= reset (idx + 1) items //Keep searching
		reset idx _						= idx - 1				//The previous index was apparently the last Just
		
	enableControl :: !(ListId a) !Int *IWorld -> *IWorld
	enableControl (ListId taskId) nextIdx iworld=:{parallelControls}
		= {iworld & parallelControls = 'Map'.put ("taskList:" +++ listKey) (nextIdx,[]) parallelControls}
	where
		listKey = toString (ParallelTaskList taskId)

	takeControl	:: !(ListId a) !*IWorld -> (![ParallelControl],!Int,!*IWorld)
	takeControl (ListId taskId) iworld=:{parallelControls}
		= case 'Map'.get ("taskList:" +++ listKey) parallelControls of
			Just (nextIdx,controls)	= (controls,nextIdx,iworld)
			_						= abort "Could not read parallel controls"
	where
		listKey = toString (ParallelTaskList taskId)
		
	disableControl :: !(ListId a) *IWorld -> *IWorld
	disableControl (ListId taskId) iworld=:{parallelControls}
		= {iworld & parallelControls = 'Map'.del ("taskList:" +++ listKey) parallelControls}
	where
		listKey = toString (ParallelTaskList taskId)
	
	mergeTaskReps :: !TaskId !d !TaskRepTarget ![(!ParallelItem,!TaskResult a)] -> TaskRep | descr d
	mergeTaskReps taskId desc (RepAsTUI target layout) results
		# layout		= (fromMaybe DEFAULT_LAYOUT layout)
		# attributes	= [(TASK_ATTRIBUTE,toString taskId) : initAttributes desc]
		| maybe True (\t -> t == taskId) target
			//Show if target is Nothing or taskId matches
			# parts = [(t,g,ac,kvSet STACK_ATTRIBUTE (toString stack) (kvSet TASK_ATTRIBUTE (toString taskId) at))
					 \\ ({ParallelItem|taskId,stack,detached},ValueResult val _ (TUIRep (t,g,ac,at)) _) <- results | not detached && not (isStable val)]	
			= TUIRep (layout ParallelComposition parts [] attributes)
		| otherwise
			//If a target is set, only one of the branches should have a TUIRep representation
			= case [gui \\ (_,ValueResult _ _ (TUIRep gui) _) <- results] of
				[part]	= TUIRep part
				parts	= NoRep	
	mergeTaskReps _ _ (RepAsService target) results
		# fixme = ([],[],[])
		= ServiceRep fixme

	isStable (Value _ Stable) 	= True
	isStable _					= False

	subRepAs :: !TaskRepTarget !TaskId !(Task a) -> TaskRepTarget
	subRepAs (RepAsTUI Nothing _) taskId task=:{layout}			= RepAsTUI Nothing layout
	subRepAs (RepAsTUI (Just target) _) taskId task=:{layout}
		| target == taskId										= RepAsTUI Nothing layout
																= RepAsTUI (Just target) layout			
	subRepAs (RepAsService Nothing) taskId task					= RepAsService Nothing 
	subRepAs (RepAsService (Just target)) taskId task
		| target == taskId										= RepAsService Nothing
																= RepAsService (Just target)

//SHARED HELPER FUNCTIONS
	
//Use decrementing stack order values (o)
//To make sure that the first initial subtask has the highest order value
//(this will ensure the first tab is active, or the first window will be on top)
mkParallelItems :: !(ListId a) ![(!ParallelTaskType,!ParallelTask a)] !Int *IWorld -> (![ParallelItem],!*IWorld) | TC a & JSONEncode{|*|} a
mkParallelItems listId [] stackOrder iworld = ([],iworld)
mkParallelItems listId [(parType,parTask):tasks] stackOrder iworld=:{IWorld|taskTime,localDateTime,currentUser}
	# (taskId,iworld)		= getNextTaskId iworld
	# (listItems,iworld)	= mkParallelItems listId tasks (stackOrder - 1) iworld
	= ([mkParallelItem taskTime parType taskId stackOrder currentUser localDateTime parTask :listItems], iworld)

mkParallelItem :: !TaskTime !ParallelTaskType !TaskId !Int !User !DateTime !(ParallelTask a) -> ParallelItem | TC a & JSONEncode{|*|} a
mkParallelItem taskTime parType taskId stackOrder user now parTask
	# (progress,management)	= case parType of
		Embedded				= (Nothing,Nothing)
		(Detached management)	= (Just {ProgressMeta|status=Unstable,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing},Just management)
	= {taskId = taskId, stack = stackOrder, detached = False, progress = progress, management = management
	   ,task = (dynamic parTask :: ParallelTask a^), state = TCEmpty taskId taskTime, lastValue = toJSON (initValue parTask), lastAttributes = []}
	where
		initValue :: (ParallelTask a) -> TaskValue a //Solve overloading
		initValue _ = NoValue
		
initParallelItems :: !(ListId a) !Int ![ParallelItem] !Int !*IWorld -> (![ParallelItem],!*IWorld) | iTask a
initParallelItems listId=:(ListId taskId) index items version iworld
	| index >= length items
		= (items, unshareParallelList listId iworld)									//Remove shared data from scope
	# iworld		= shareParallelList listId items version iworld 					//Update shared data	
	# (item,iworld)	= initParallelItem listId (items !! index) iworld					//Init item at index
	= initParallelItems listId (index + 1) (updateAt index item items) version iworld

initParallelItem :: !(ListId a) !ParallelItem !*IWorld -> (!ParallelItem,!*IWorld) | iTask a
initParallelItem (ListId listTaskId) item=:{ParallelItem|taskId, task = (parTask :: ParallelTask a^)} iworld
	# (state,iworld)	= (parTask listShare).initFun taskId iworld
	= ({ParallelItem|item & state = state}, iworld)
where
	listShare = taskListShare (ParallelTaskList listTaskId)

shareParallelList :: !(ListId a) ![ParallelItem] !Int !*IWorld -> *IWorld
shareParallelList (ListId taskId) items version iworld=:{parallelLists}
	= {iworld & parallelLists = 'Map'.put ("taskList:" +++ listKey) (version,items) parallelLists}
where
	listKey		= toString (ParallelTaskList taskId)

unshareParallelList :: !(ListId a) !*IWorld -> *IWorld
unshareParallelList (ListId taskId) iworld=:{parallelLists}
	= {iworld & parallelLists = 'Map'.del ("taskList:" +++ listKey) parallelLists}
where
	listKey		= toString (ParallelTaskList taskId)

//Derived shares
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\{TaskList|state} -> state) tasklist

taskListMeta :: !(SharedTaskList a) -> ReadOnlyShared [TaskListItem]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist
/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task Int | TC a & JSONEncode{|*|} a
appendTask parType parTask tasklist = mkInstantTask eval
where
	listId = hd (getIds tasklist)
	eval taskId iworld=:{taskTime,parallelControls,currentUser,localDateTime}
		= case 'Map'.get listId parallelControls of
			Just (nextIdx,controls)
				//For the global tasklist we don't use the internal counter, but get the index from the
				//process database
				# (newIdx,newTaskId,nextIdx,iworld) = case listId of
					"taskList:tasklist-top"
						# (newIdx,iworld)		= newTopNo iworld
						= (newIdx,TaskId newIdx 0,nextIdx, iworld)
					_						
						# (newTaskId,iworld)	= getNextTaskId iworld	
						= (nextIdx,newTaskId,nextIdx + 1,iworld)		
				# newItem					= mkParallelItem taskTime parType newTaskId newIdx currentUser localDateTime parTask
				# parallelControls			= 'Map'.put listId (nextIdx, controls ++ [AppendTask newItem]) parallelControls
				= (ValueResult (Value newIdx Stable) taskTime NoRep (TCEmpty taskId taskTime), {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (exception ("Task list " +++ listId +++ " is not in scope"), iworld)
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList s) -> Task Void | TC s
removeTask remId tasklist = mkInstantTask (removeTask` remId tasklist)
where
	listId = hd (getIds tasklist)
	removeTask` :: !TaskId !(SharedTaskList s) TaskId *IWorld -> (!TaskResult Void,!*IWorld) | TC s
	removeTask` remId _ taskId iworld=:{taskTime,parallelControls}
		= case 'Map'.get listId parallelControls of
			Just (nextIdx,controls)
				# parallelControls = 'Map'.put listId (nextIdx, controls ++ [RemoveTask remId]) parallelControls
				= (ValueResult (Value Void Stable) taskTime NoRep (TCEmpty taskId taskTime), {iworld & parallelControls = parallelControls, readShares = Nothing})
			_
				= (exception ("Task list " +++ listId +++ " is not in scope"), iworld)

/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs user task
	= {Task|task & initFun = init task.initFun, evalFun = eval task.evalFun}
where
	init f taskId iworld=:{currentUser}
		# (context,iworld) = f taskId {iworld & currentUser = user}
		= (context,{iworld & currentUser = currentUser})
	
	eval f eEvent cEvent repAs context iworld=:{currentUser}
		# (result,iworld) = f eEvent cEvent repAs context {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = mkTask init eval
where
	init taskId iworld
		# (taskIda,iworld)			= getNextTaskId iworld
		# iworld					= shareValue taskId initial initial 0 iworld
		# (inita,iworld)			= (stask (localShare taskId)).initFun taskId iworld
		# (value,version,iworld)	= unshareValue taskId initial iworld
		= (TCShared taskId (toJSON value) version inita, iworld)

	eval eEvent cEvent repAs context=:(TCShared taskId encv version cxta) iworld
		# iworld					= shareValue taskId initial (fromJust (fromJSON encv)) version iworld
		# taska						= stask (localShare taskId)
		# (resa,iworld)				= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		# (value,version,iworld)	= unshareValue taskId initial iworld
		= case resa of
			ValueResult NoValue lastEvent rep ncxta		= (ValueResult NoValue lastEvent rep (TCShared taskId (JSONNull) version ncxta),iworld)
			ValueResult (Value stable val) lastEvent rep ncxta	= (ValueResult (Value stable val) lastEvent rep (TCShared taskId (toJSON val) version ncxta),iworld)
			ExceptionResult e str						= (ExceptionResult e str,iworld)
	eval _ _ _ _ iworld
		= (exception "Corrupt task context in withShared", iworld)
	
	subRepAs (RepAsService target) _  			= (RepAsService target)
	subRepAs (RepAsTUI target mbLayout) task=:{layout} = case mbLayout of
		Nothing			= RepAsTUI target layout
		Just overwrite	= RepAsTUI target (Just overwrite)

	shareValue :: !TaskId s !s !Int !*IWorld -> *IWorld | iTask s	//With bogus argument to solve overloading
	shareValue taskId _ value version iworld=:{localShares}
		= {iworld & localShares = 'Map'.put ("localShare:" +++ toString taskId) (version,dynamic value :: s^) localShares}
	
	unshareValue :: !TaskId s !*IWorld -> (!s,!Int,!*IWorld) | iTask s //With bogus argument to solve overloading
	unshareValue taskId _ iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ toString taskId) localShares of
			Just (version,(value :: s^))
				= (value,version,{iworld & localShares = 'Map'.del ("localShare:" +++ toString taskId) localShares})
			_	= abort "Shared value not in scope"

localShare :: !TaskId -> (Shared s) | iTask s
localShare taskId = (makeUnsafeShare "localShare" shareKey read write getVersion)
where
	shareKey = toString taskId
	read iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ shareKey) localShares of
			Just (_,(value:: s^))	= (Ok value, iworld)
			_						= (Error ("Could not read local shared stated " +++ shareKey), iworld)
	
	write value iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ shareKey) localShares of
			Just (version,_)		= (Ok Void, {iworld & localShares = 'Map'.put ("localShare:" +++ shareKey) (version + 1, dynamic value :: s^) localShares})
			_						= (Error ("Could not read timestamp for local shared stated " +++ shareKey),iworld)
	getVersion iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ shareKey) localShares of
			Just (version,_)		= (Ok version, iworld)
			_						= (Error ("Could not read timestamp for local shared stated " +++ shareKey),iworld)

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

