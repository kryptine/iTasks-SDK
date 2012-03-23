implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskState, TaskStore, Util, HTTP, GenUpdate, GenEq, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List
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
transform f (Task evala) = Task eval
where
	eval eEvent cEvent refresh repAs cxt iworld = case evala eEvent cEvent refresh repAs cxt iworld of
		(ValueResult val lastEvent rep cxt,iworld)	= (ValueResult (f val) lastEvent rep cxt, iworld)	//TODO: guarantee stability
		(ExceptionResult e str, iworld)				= (ExceptionResult e str, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a
project projection share (Task evala) = Task eval
where
	eval eEvent cEvent refresh repAs state iworld
		# (taskId,prev,statea) = case state of
			(TCInit taskId _)					= (taskId,NoValue,state) 
			(TCProject taskId encprev statea)	= (taskId,fromJust (fromJSON encprev),statea)
			
		# (resa, iworld) 	= evala eEvent cEvent refresh (matchTarget repAs taskId) statea iworld
		= case resa of
			ValueResult val ts rep ncxta
				# result = ValueResult val ts rep (TCProject taskId (toJSON val) ncxta)
				| val =!= prev
					= projectOnShare val result iworld
				| otherwise
					= (result,iworld)
			ExceptionResult e str
				= (ExceptionResult e str,iworld)
	
	projectOnShare val result iworld
		= case readWrite (\r _ -> case projection val r of Just w = Write w Void; Nothing = YieldResult Void) share iworld of
			(Ok _,iworld)		= (result,iworld)
			(Error e,iworld)	= (exception e,iworld)


step :: (Task a) [TaskStep a b] -> Task b | iTask a & iTask b
step (Task evala) conts = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval eEvent cEvent refresh repAs (TCStep taskId (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval eEvent cEvent refresh repAs (TCStep taskId (Left statea)) iworld=:{taskTime}
		# (resa, iworld) 	= evala eEvent cEvent refresh (matchTarget repAs taskId) statea iworld
		# mbcommit			= case cEvent of
			(Just (TaskEvent t action))
				| t == taskId && not refresh	= Just action
			_									= Nothing
		# mbCont			= case resa of
			ValueResult val lastEvent rep nstatea = case searchContValue val mbcommit conts of
				Nothing			= Left (ValueResult NoValue lastEvent (addStepActions taskId repAs rep val) (TCStep taskId (Left nstatea)) )
				Just rewrite	= Right rewrite
			ExceptionResult e str = case searchContException e str conts of
				Nothing			= Left (ExceptionResult e str)
				Just rewrite	= Right rewrite
		= case mbCont of
			Left res = (res,iworld)
			Right (sel,Task evalb,enca)
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb Nothing Nothing refresh (matchTarget repAs taskId) (TCInit taskIdb taskTime) iworld 
				= case resb of
					ValueResult val lastEvent rep nstateb	= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,nstateb))),iworld)
					ExceptionResult e str					= (ExceptionResult e str, iworld)
	//Eval right-hand side
	eval eEvent cEvent refresh repAs (TCStep taskId (Right (enca,sel,stateb))) iworld
		# mbTaskb = case conts !! sel of
			(OnValue _ taskbf)			= fmap taskbf (fromJSON enca)
			(OnAction _ _ taskbf)		= fmap taskbf (fromJSON enca)
			(OnException taskbf)		= fmap taskbf (fromJSON enca)
			(OnAllExceptions taskbf)	= fmap taskbf (fromJSON enca)
		= case mbTaskb of
			Just (Task evalb)
				# (resb, iworld)	= evalb eEvent cEvent refresh (matchTarget repAs taskId) stateb iworld 
				= case resb of
					ValueResult val lastEvent rep nstateb	= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,nstateb))), iworld)
					ExceptionResult e str					= (ExceptionResult e str, iworld)
			Nothing
				= (exception "Corrupt task value in step", iworld) 	
	//Incorred state
	eval eEvent cEvent refresh _ state iworld
		= (exception ("Corrupt task state in step:" +++ (toString (toJSON state))), iworld)

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
	addStepActions taskId repAs=:(RepAsTUI Nothing _ _) rep val = case rep of
		(TUIRep gui)
			# fixme = []
			= TUIRep ((repLayout repAs) SequentialComposition [gui] (stepActions taskId val) [(TASK_ATTRIBUTE, toString taskId):fixme])	//TODO: Add attributes from task
		_	
			= TUIRep ((repLayout repAs) SequentialComposition [] (stepActions taskId val) [(TASK_ATTRIBUTE, toString taskId)])
	addStepActions taskId (RepAsTUI (Just _) _ _) rep val
		= rep
	
	stepActions taskId val = [(toString taskId,action,pred val)\\ OnAction action pred _ <- conts]

// Parallel composition
:: ResultSet a
	= RSException !Dynamic !String
	| RSResults	![(!ParallelItem,!TaskResult a)]

//Phantom type wrapper for task id's used to solve overloading
:: ListId a = ListId TaskId

parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
parallel desc initTasks = Task eval 
where
	//Create initial set of tasks and initial state
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		//Create the list of initial parallel items
		# (items,iworld)					= mkParallelItems listId ts initTasks (length initTasks - 1) iworld
		//Initialize all initial items
		= eval eEvent cEvent refresh repAs (TCParallel taskId listMeta items) iworld
	where
		listId		= ListId taskId //Passed to subfunctions to solve overloading
		listMeta	= {nextIdx = length initTasks, listVersion = 0}	

	eval eEvent cEvent refresh repAs (TCParallel taskId meta items) iworld
		//If a reorder edit event is given, reorder the stack 
		# items = case eEvent of 
			Just (TaskEvent t ("top",JSONString top))	= reorder (fromString top) items
			_											= items
		//Eval all subtasks 
		# (results,nextIdx,iworld)	= evalParallelItems (listId taskId initTasks) eEvent cEvent refresh repAs 0 (zip (items,repeat Nothing)) meta.nextIdx iworld
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
	eval _ _ _ _ _ iworld
		= (exception "Corrupt task state in parallel", iworld)
		
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
			
	evalParallelItems :: !(ListId a) !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !TaskRepTarget !Int ![(ParallelItem, Maybe (TaskResult a))] !Int !*IWorld -> (!ResultSet a, !Int, !*IWorld) | iTask a
	evalParallelItems listId=:(ListId listTaskId) eEvent cEvent refresh repAs index items nextIdx iworld
		| index >= length items
			//All done, return result set and remove list from scope
			# iworld = unshareParallelList listId iworld
			# iworld = disableControl listId iworld
			= (RSResults [(item,result) \\ (item,Just result) <- items], nextIdx, iworld)
		| otherwise
			//Update scope
			# iworld					= shareParallelList listId (map fst items) iworld 
			//Enable control
			# iworld					= enableControl listId nextIdx iworld
			//Evaluate item at index
			# (result,item,iworld)		= evalParallelItem listId eEvent cEvent refresh repAs (fst (items !! index)) iworld
			//Process additions and removals from the list
			# (controls,nextIdx,iworld)	= takeControls listId iworld
			# (items,iworld)			= processControls listId controls (updateAt index (item,Just result) items) iworld
			# index						= if (isEmpty controls) index (resetIndex items) 	//If items have been removed, the index may not be correct anymore
			= case result of
				ExceptionResult e str	//Clean scope and return exeption
					# iworld = unshareParallelList listId iworld
					= (RSException e str, nextIdx, iworld)
				_					//Evaluate other items
					= evalParallelItems listId eEvent cEvent refresh repAs (index + 1) items nextIdx iworld
		
	evalParallelItem :: !(ListId a) !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !TaskRepTarget !ParallelItem !*IWorld -> (!TaskResult a,!ParallelItem,!*IWorld) | iTask a
	evalParallelItem listId=:(ListId listTaskId) eEvent cEvent refresh repAs item=:{ParallelItem|task=(parTask :: ParallelTask a^),state} iworld
		# (result, iworld)	= eval eEvent cEvent refresh (subRepAs repAs listTaskId) state iworld
		# item	= case result of
			ValueResult val _ rep state		= {ParallelItem|item & state = state, lastValue = toJSON val} //TODO: set lastAttributes
			ExceptionResult e str			= item
		= (result,item,iworld)
	where
		(Task eval) = parTask listShare
		listShare = taskListShare (ParallelTaskList listTaskId)
	
	processControls :: !(ListId a) ![ParallelControl] ![(ParallelItem,Maybe (TaskResult a))] !*IWorld -> (![(ParallelItem,Maybe (TaskResult a))],!*IWorld) | iTask a
	processControls listId [] items iworld = (items,iworld)
	processControls listId [AppendTask item:controls] items iworld=:{taskTime}
		//Append for later evaluation	
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

	takeControls	:: !(ListId a) !*IWorld -> (![ParallelControl],!Int,!*IWorld)
	takeControls (ListId taskId) iworld=:{parallelControls}
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
	mergeTaskReps taskId desc repAs=:(RepAsTUI target _ _) results
		# layout		= repLayout repAs
		# attributes	= [(TASK_ATTRIBUTE,toString taskId) : initAttributes desc]
		| maybe True (\t -> t == taskId) target
			//Show if target is Nothing or taskId matches
			# parts = [(t,g,ac,kvSet STACK_ATTRIBUTE (toString stack) (kvSet TASK_ATTRIBUTE (toString taskId) at))
					 \\ ({ParallelItem|taskId,stack,management},ValueResult val _ (TUIRep (t,g,ac,at)) _) <- results | isNothing management && not (isStable val)]	
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

	subRepAs :: !TaskRepTarget !TaskId -> TaskRepTarget
	subRepAs (RepAsTUI Nothing _ _) taskId 			= RepAsTUI Nothing Nothing Nothing
	subRepAs (RepAsTUI (Just target) _ _) taskId
		| target == taskId							= RepAsTUI Nothing Nothing Nothing
													= RepAsTUI (Just target) Nothing Nothing	
																	
	subRepAs (RepAsService Nothing) taskId 			= RepAsService Nothing 
	subRepAs (RepAsService (Just target)) taskId
		| target == taskId							= RepAsService Nothing
													= RepAsService (Just target)
//SHARED HELPER FUNCTIONS
	
//Use decrementing stack order values (o)
//To make sure that the first initial subtask has the highest order value
//(this will ensure the first tab is active, or the first window will be on top)
mkParallelItems :: !(ListId a) !TaskTime  ![(!ParallelTaskType,!ParallelTask a)] !Int *IWorld -> (![ParallelItem],!*IWorld) | TC a & JSONEncode{|*|} a
mkParallelItems listId taskTime [] stackOrder iworld = ([],iworld)
mkParallelItems listId taskTime [(parType,parTask):tasks] stackOrder iworld=:{IWorld|currentDateTime,currentUser}
	# (taskId,iworld)		= getNextTaskId iworld
	# (listItems,iworld)	= mkParallelItems listId taskTime tasks (stackOrder - 1) iworld
	= ([mkParallelItem taskTime parType taskId stackOrder currentUser currentDateTime parTask : listItems], iworld)

mkParallelItem :: !TaskTime !ParallelTaskType !TaskId !Int !User !DateTime !(ParallelTask a) -> ParallelItem | TC a & JSONEncode{|*|} a
mkParallelItem taskTime parType taskId stackOrder user now parTask
	# (progress,management)	= case parType of
		Embedded				= (Nothing,Nothing)
		(Detached management)	= (Just {ProgressMeta|status=Unstable,issuedAt=now,issuedBy=user,firstEvent=Nothing,latestEvent=Nothing},Just management)
	= {taskId = taskId, stack = stackOrder, progress = progress, management = management
	   ,task = (dynamic parTask :: ParallelTask a^), state = TCInit taskId taskTime, lastValue = toJSON (initValue parTask), lastAttributes = []}
	where
		initValue :: (ParallelTask a) -> TaskValue a //Solve overloading
		initValue _ = NoValue
		
shareParallelList :: !(ListId a) ![ParallelItem] !*IWorld -> *IWorld
shareParallelList (ListId taskId) items iworld=:{parallelLists}
	= {iworld & parallelLists = 'Map'.put ("taskList:" +++ listKey) items parallelLists}
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
appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | TC a & JSONEncode{|*|} a
appendTask parType parTask tasklist = mkInstantTask eval
where
	listId = hd (getIds tasklist)
	eval taskId iworld=:{taskTime,parallelControls,currentUser,currentDateTime}
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
				# newItem					= mkParallelItem taskTime parType newTaskId newIdx currentUser currentDateTime parTask
				# parallelControls			= 'Map'.put listId (nextIdx, controls ++ [AppendTask newItem]) parallelControls
				= (ValueResult (Value newTaskId Stable) taskTime NoRep (TCEmpty taskId taskTime), {iworld & parallelControls = parallelControls, readShares = Nothing})
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
workAs user (Task eval) = Task eval`
where
	eval` eEvent cEvent refresh repAs state iworld=:{currentUser}
		# (result,iworld) = eval eEvent cEvent refresh repAs state {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task eval
where	
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		# (taskIda,iworld)			= getNextTaskId iworld
		= eval eEvent cEvent refresh repAs  (TCShared taskId (toJSON initial) (TCInit taskIda ts)) iworld
		
	eval eEvent cEvent refresh repAs (TCShared taskId encsvalue cxta) iworld
		# iworld					= shareValue taskId initial (fromJust (fromJSON encsvalue)) iworld
		# (Task evala)				= stask (localShare taskId)
		# (resa,iworld)				= evala eEvent cEvent refresh (matchTarget repAs taskId) cxta iworld
		# (svalue,iworld)			= unshareValue taskId initial iworld
		= case resa of
			ValueResult NoValue lastEvent rep ncxta				= (ValueResult NoValue lastEvent rep (TCShared taskId (toJSON svalue) ncxta),iworld)
			ValueResult (Value stable val) lastEvent rep ncxta	= (ValueResult (Value stable val) lastEvent rep (TCShared taskId (toJSON svalue) ncxta),iworld)
			ExceptionResult e str								= (ExceptionResult e str,iworld)
	eval _ _ _ _ _ iworld
		= (exception "Corrupt task state in withShared", iworld)
		
	shareValue :: !TaskId s !s !*IWorld -> *IWorld | iTask s	//With bogus argument to solve overloading
	shareValue taskId _ value iworld=:{localShares}
		= {iworld & localShares = 'Map'.put ("localShare:" +++ toString taskId) (dynamic value :: s^) localShares}
	
	unshareValue :: !TaskId s !*IWorld -> (!s,!*IWorld) | iTask s //With bogus argument to solve overloading
	unshareValue taskId _ iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ toString taskId) localShares of
			Just (value :: s^)
				= (value,{iworld & localShares = 'Map'.del ("localShare:" +++ toString taskId) localShares})
			_	= abort "Shared value not in scope"

localShare :: !TaskId -> (Shared s) | iTask s
localShare taskId = (makeUnsafeShare "localShare" shareKey read write)
where
	shareKey = toString taskId
	read iworld=:{localShares}
		= case 'Map'.get ("localShare:" +++ shareKey) localShares of
			Just (value:: s^)	= (Ok value, iworld)
			_					= (Error ("Could not read local shared stated " +++ shareKey), iworld)
	
	write value iworld=:{localShares}
		= (Ok Void, {iworld & localShares = 'Map'.put ("localShare:" +++ shareKey) (dynamic value :: s^) localShares})

/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a

instance tune SetLayout
where
	tune (SetLayout layout) (Task eval)	= Task eval`
	where
		eval` eEvent cEvent refresh (RepAsTUI target Nothing mod) state iworld
			= eval eEvent cEvent refresh (RepAsTUI target (Just ((fromMaybe id mod) layout)) Nothing) state iworld 
		eval` eEvent cEvent refresh (RepAsTUI target (Just layout) mod) state iworld
			= eval eEvent cEvent refresh (RepAsTUI target (Just layout) Nothing) state iworld 
		eval` eEvent cEvent refresh repAs state iworld
			= eval eEvent cEvent refresh repAs state iworld 

instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task eval)	= Task eval`
	where
		eval` eEvent cEvent refresh (RepAsTUI target layout Nothing) state iworld
			= eval eEvent cEvent refresh (RepAsTUI target layout (Just f)) state iworld 
		eval` eEvent cEvent refresh (RepAsTUI target layout (Just g)) state iworld
			= eval eEvent cEvent refresh (RepAsTUI target layout (Just (g o f))) state iworld 	
		eval` eEvent cEvent refresh repAs state iworld
			= eval eEvent cEvent refresh repAs state iworld 
	
/**
* Helper function that sets the target task id to Nothing if it matches the given task id.
* This ensures that a representation for the full sub-tree is generated when determining the representation target for sub trees.
*/
matchTarget :: !TaskRepTarget !TaskId -> TaskRepTarget
matchTarget repAs=:(RepAsTUI (Just target) layout mod) taskId	= if (target == taskId) (RepAsTUI Nothing layout mod) repAs
matchTarget repAs=:(RepAsService (Just target)) taskId			= if (target == taskId) (RepAsService Nothing) repAs
matchTarget repAs taskId										= repAs
