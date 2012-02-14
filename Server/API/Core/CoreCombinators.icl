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
		# (taskIda,iworld)	= getNextTaskId iworld
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
		# (taskIda,iworld)	= getNextTaskId iworld
		# (inita,iworld)	= taska.initFun taskIda iworld
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
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (cxtb,iworld)		= taskb.initFun taskIdb iworld
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
			= TUIRep (layoutfun SequentialComposition [gui] (stepActions taskId mba) [(TASK_ATTRIBUTE, toString taskId):fixme])	//TODO: Add attributes from task
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
:: ResultSet a
	= RSException !Dynamic !String
	| RSResults	![(!ParallelItem,!TaskResult a)]

//Phantom type wrapper for task id's used to solve overloading
:: ListId a = ListId TaskId

parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [Maybe a] | descr d & iTask a
parallel desc initTasks = mkTask init edit eval 
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
	


	//Direct the event to the right place
	edit event context=:(TCParallel taskId meta items) iworld
		//Put list in scope
		# iworld		= shareParallelList (listId taskId initTasks) items meta.listVersion iworld
		//Evaluate sub(s)
		# (TCParallel taskId meta items, iworld)
			= case event of
				//Event targeted at the parallel set, to move a task to front in the ordering
				(TaskEvent t ("top",JSONString top))
					| t == taskId
						= (TCParallel taskId meta (reorder (fromString top) items), iworld)
				_
					//Evaluate all sub-contexts
					# (items, iworld) = mapSt (editParallelItem (listId taskId initTasks) event) items iworld
					= (TCParallel taskId meta items, iworld)
		//Remove the task info overview
		# iworld						= unshareParallelList (listId taskId initTasks) iworld
		= (TCParallel taskId meta items,iworld)
	//Fallback
	edit event context iworld
		= (context,iworld)

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

	editParallelItem :: !(ListId a) !EditEvent !ParallelItem !*IWorld -> (!ParallelItem,!*IWorld) | iTask a
	editParallelItem (ListId taskId) event item=:{ParallelItem|task=(parTask :: ParallelTask a^),state} iworld
		# task				= parTask (taskListShare (ParallelTaskList taskId))
		# (state,iworld)	= task.editFun event state iworld
		= ({ParallelItem|item & state = state}, iworld)
	
	eval eEvent cEvent repAs context=:(TCParallel taskId meta items) iworld
		//Eval all subtasks 
		# (results,nextIdx,iworld)	= evalParallelItems (listId taskId initTasks) eEvent cEvent repAs 0 (zip (items,repeat Nothing)) meta.nextIdx iworld
		= case results of
			RSException e str
				= (TaskException e str, iworld)
			RSResults itemsAndResults
				# rep				= mergeTaskReps taskId desc repAs itemsAndResults
				# (items,results)	= unzip itemsAndResults
				# (val,stable)		= mergeValues results
				| stable
					= (TaskStable val rep (TCParallel taskId meta items), iworld)
				| otherwise
					= (TaskUnstable (Just val) rep (TCParallel taskId meta items), iworld)
	where
		listKey = toString (ParallelTaskList taskId)
	//Fallback
	eval _ _ _ _ iworld
		= (taskException "Corrupt task context in parallel", iworld)
			
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
				TaskException e str	//Clean scope and return exeption
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
			TaskUnstable mbr rep state	= {ParallelItem|item & state = state, lastValue = toJSON mbr} 		//TODO: set lastAttributes
			TaskStable r rep state		= {ParallelItem|item & state = state, lastValue = toJSON (Just r)}	//TODO: set lastAttributes
			TaskException e str			= item
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
					 \\ ({ParallelItem|taskId,stack,detached},TaskUnstable _ (TUIRep (t,g,ac,at)) _) <- results | not detached]	
			= TUIRep (layout ParallelComposition parts [] attributes)
		| otherwise
			//If a target is set, only one of the branches should have a TUIRep representation
			= case [gui \\ (_,TaskUnstable _ (TUIRep gui) _) <- results] of
				[part]	= TUIRep part
				parts	= NoRep	
	mergeTaskReps _ _ (RepAsService target) results
		# fixme = ([],[],[])
		= ServiceRep fixme

	mergeValues :: ![TaskResult a] -> (![Maybe a],!Bool)
	mergeValues []							= ([],True)
	mergeValues [TaskStable v _ _:is]		= let (vs,stable) = mergeValues is in ([Just v:vs],stable)
	mergeValues [TaskUnstable mbv _ _:is]	= ([mbv:fst (mergeValues is)], False)
	
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
mkParallelItems :: !(ListId a) ![(!ParallelTaskType,!ParallelTask a)] !Int *IWorld -> (![ParallelItem],!*IWorld) | TC a
mkParallelItems listId [] stackOrder iworld = ([],iworld)
mkParallelItems listId [(parType,parTask):tasks] stackOrder iworld=:{IWorld|localDateTime,currentUser}
	# (taskId,iworld)		= getNextTaskId iworld
	# (listItems,iworld)	= mkParallelItems listId tasks (stackOrder - 1) iworld
	= ([mkParallelItem parType taskId stackOrder currentUser localDateTime parTask :listItems], iworld)

mkParallelItem :: !ParallelTaskType !TaskId !Int !User !DateTime !(ParallelTask a) -> ParallelItem | TC a
mkParallelItem parType taskId stackOrder user now parTask
	# (progress,management)	= case parType of
		Embedded				= (Nothing,Nothing)
		(Detached management)	= (Just {ProgressMeta|status=Unstable,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing},Just management)
	= {taskId = taskId, stack = stackOrder, detached = False, progress = progress, management = management
	   ,task = (dynamic parTask :: ParallelTask a^), state = TCEmpty taskId, lastValue = JSONNull, lastAttributes = []}

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
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [Maybe a] 
taskListState tasklist = mapRead (\{TaskList|state} -> state) tasklist

taskListMeta :: !(SharedTaskList a) -> ReadOnlyShared [TaskListItem]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist
/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task Int | TC a
appendTask parType parTask tasklist = mkInstantTask eval
where
	listId = hd (getIds tasklist)
	eval taskId iworld=:{parallelControls,currentUser,localDateTime}
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
				# newItem					= mkParallelItem parType newTaskId newIdx currentUser localDateTime parTask
				# parallelControls			= 'Map'.put listId (nextIdx, controls ++ [AppendTask newItem]) parallelControls
				= (TaskStable newIdx NoRep (TCEmpty taskId), {iworld & parallelControls = parallelControls, readShares = Nothing})
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

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = mkTask init edit eval
where
	init taskId iworld
		# (taskIda,iworld)			= getNextTaskId iworld
		# iworld					= shareValue taskId initial initial 0 iworld
		# (inita,iworld)			= (stask (localShare taskId)).initFun taskId iworld
		# (value,version,iworld)	= unshareValue taskId initial iworld
		= (TCShared taskId (toJSON value) version inita, iworld)
	
	edit event context=:(TCShared taskId encv version cxta) iworld
		# iworld					= shareValue taskId initial (fromJust (fromJSON encv)) version iworld
		# (cxta,iworld)				= (stask (localShare taskId)).editFun event cxta iworld
		# (value,version,iworld)	= unshareValue taskId initial iworld
		= (TCShared taskId (toJSON value) version cxta, iworld)
	edit _ context iworld
		= (context,iworld)
	
	eval eEvent cEvent repAs context=:(TCShared taskId encv version cxta) iworld
		# iworld					= shareValue taskId initial (fromJust (fromJSON encv)) version iworld
		# taska						= stask (localShare taskId)
		# (resa,iworld)				= taska.evalFun eEvent cEvent (matchTarget (subRepAs repAs taska) taskId) cxta iworld
		# (value,version,iworld)	= unshareValue taskId initial iworld
		= case resa of
			TaskUnstable mba rep ncxta	= (TaskUnstable mba rep (TCShared taskId (toJSON value) version ncxta),iworld)
			TaskStable a rep ncxta		= (TaskStable a rep (TCShared taskId (toJSON value) version ncxta),iworld)
			TaskException e str			= (TaskException e str,iworld)
	eval _ _ _ _ iworld
		= (taskException "Corrupt task context in withShared", iworld)
	
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