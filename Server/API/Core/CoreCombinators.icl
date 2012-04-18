implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskState, TaskStore, TaskEval, Util, HTTP, GenUpdate, GenEq_NG, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List_NG
import iTaskClass, InteractionTasks, LayoutCombinators, TUIDefinition

from Map				import qualified get, put, del
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from TaskEval			import localShare, parListShare, topListShare
from CoreTasks			import return
from SharedDataSource	import write, read

derive class iTask ParallelTaskType, WorkOnStatus

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{currentInstance,nextTaskNo} = (TaskId currentInstance nextTaskNo, {IWorld|iworld & nextTaskNo = nextTaskNo + 1})

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f (Task evala) = Task eval
where
	eval eEvent cEvent refresh repAs tree iworld = case evala eEvent cEvent refresh repAs tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e str, iworld)				= (ExceptionResult e str, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a
project projection share (Task evala) = Task eval
where
	eval eEvent cEvent refresh repAs (TCDestroy (TCProject taskId encprev treea)) iworld	//Cleanup duty simply passed to inner task
		= evala eEvent cEvent refresh repAs (TCDestroy treea) iworld

	eval eEvent cEvent refresh repAs state iworld
		# (taskId,prev,statea) = case state of
			(TCInit taskId _)					= (taskId,NoValue,state) 
			(TCProject taskId encprev statea)	= (taskId,fromJust (fromJSON encprev),statea)
			
		# (resa, iworld) 	= evala eEvent cEvent refresh repAs statea iworld
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
		# (er, iworld) = read share iworld
		= case er of
			Ok r = case projection val r of
				Just w
					# (ew, iworld) = write w share iworld
					= case ew of
						Ok _	= (result, iworld)
						Error e	= (exception e, iworld)
				Nothing = (result, iworld)
			Error e = (exception e, iworld)

step :: !(Task a) [TaskStep a b] -> Task b | iTask a & iTask b
step (Task evala) conts = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval eEvent cEvent refresh repAs (TCStep taskId (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval eEvent cEvent refresh repAs (TCStep taskId (Left treea)) iworld=:{taskTime}
		# (resa, iworld) 	= evala eEvent cEvent refresh repAs treea iworld
		# mbcommit			= case cEvent of
			(Just (TaskEvent t action))
				| t == taskId && not refresh	= Just action
			_									= Nothing
		# mbCont			= case resa of
			ValueResult val lastEvent rep ntreea = case searchContValue val mbcommit conts of
				Nothing			= Left (ValueResult NoValue lastEvent (addStepActions taskId repAs rep val) (TCStep taskId (Left ntreea)) )
				Just rewrite	= Right (rewrite,Just ntreea)
			ExceptionResult e str = case searchContException e str conts of
				Nothing			= Left (ExceptionResult e str)
				Just rewrite	= Right (rewrite,Nothing)		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,enca),mbTreeA)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala Nothing Nothing refresh repAs (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb Nothing Nothing refresh repAs (TCInit taskIdb taskTime) iworld 
				= case resb of
					ValueResult val lastEvent rep nstateb	= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,nstateb))),iworld)
					ExceptionResult e str					= (ExceptionResult e str, iworld)
	//Eval right-hand side
	eval eEvent cEvent refresh repAs (TCStep taskId (Right (enca,sel,treeb))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb eEvent cEvent refresh repAs treeb iworld 
				= case resb of
					ValueResult val lastEvent rep ntreeb	= (ValueResult val lastEvent rep (TCStep taskId (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e str					= (ExceptionResult e str, iworld)
			Nothing
				= (exception "Corrupt task value in step", iworld) 	
	
	//Cleanup
	eval eEvent cEvent refresh repAs (TCDestroy (TCStep taskId (Left treea))) iworld
		= case evala eEvent cEvent refresh repAs (TCDestroy treea) iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e str,iworld)	= (ExceptionResult e str,iworld)
			(ValueResult _ _ _ _,iworld)	= (exception "Destroy failed in step",iworld)
	
	eval eEvent cEvent refresh repAs (TCDestroy (TCStep taskId (Right(enca,sel,treeb)))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)	= evalb eEvent cEvent refresh repAs (TCDestroy treeb) iworld
			Nothing				= (exception "Corrupt task value in step", iworld)
			
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
		search _ _ _ catchall []					= catchall														//Return the maybe catchall
		search dyn str i catchall [OnException f:cs] = case (match f dyn) of
			Just (taskb,enca)						= Just (i, taskb, enca)											//We have a match
			_										= search dyn str (i + 1) catchall cs							//Keep searching
		search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, toJSON str)) cs 	//Keep searching (at least we have a catchall)
		search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs							//Keep searching
				
		match :: (e -> Task b) Dynamic -> Maybe (Task b, JSONNode) | iTask e
		match f (e :: e^)	= Just (f e, toJSON e)
		match _ _			= Nothing 
	
	restoreTaskB sel enca = case conts !! sel of
		(OnValue _ taskbf)			= fmap taskbf (fromJSON enca)
		(OnAction _ _ taskbf)		= fmap taskbf (fromJSON enca)
		(OnException taskbf)		= fmap taskbf (fromJSON enca)
		(OnAllExceptions taskbf)	= fmap taskbf (fromJSON enca)
	
	addStepActions taskId repAs (TaskRep gui parts) val 
		# fixme = []
		= TaskRep ((repLayout repAs) SequentialComposition [gui] (stepActions taskId val) [(TASK_ATTRIBUTE, toString taskId):fixme]) parts	//TODO: Add attributes from task
	
	stepActions taskId val = [(toString taskId,action,pred val)\\ OnAction action pred _ <- conts]

// Parallel composition
parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
parallel desc initTasks = Task eval 
where
	//Create initial task list
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld=:{IWorld|localLists}
		//Append the initial tasks to the list 
		# iworld	= foldl append {iworld & localLists = 'Map'.put taskId [] localLists} initTasks
		//Evaluate the parallel
		= eval eEvent cEvent refresh repAs (TCParallel taskId) iworld
	where
		append iworld t = snd (appendTaskToList taskId t iworld)

	//Evaluate the task list
	eval eEvent cEvent refresh repAs (TCParallel taskId) iworld=:{taskTime}
		//Update the tasktime if an explicit reorder event of tabs/windows is targeted at the parallel 
		# iworld = case eEvent of
			Just (TaskEvent t ("top",JSONString top))	
				| t == taskId && not refresh	= updateListEntryTime taskId (fromString top) taskTime iworld
												= iworld
			_	= iworld

		//Evaluate all parallel tasks in the list
		= case evalParTasks taskId eEvent cEvent refresh repAs iworld of
			(Just res=:(ExceptionResult e str),iworld)	= (res,iworld)
			(Just res=:(ValueResult _ _ _ _),iworld)	= (exception "parallel evaluation yielded unexpected result",iworld)
			(Nothing,iworld=:{localLists})
				//Create the task value
				# entries			= fromMaybe [] ('Map'.get taskId localLists) 
				//Filter out removed entries and destroy their state
				# (removed,entries)	= splitWith (\{TaskListEntry|removed} -> removed) entries
				= case foldl destroyParTask (Nothing,iworld)removed of
					(Just (ExceptionResult e str),iworld)	= (ExceptionResult e str,iworld)	//An exception occurred	
					(Just result,iworld)					= (fixOverloading result initTasks (exception "Destroy failed in step"),iworld)
					(Nothing,iworld)
						//Destruction is ok, build parallel result
						# rep				= parallelRep desc taskId repAs entries
						# values			= map toValueAndTime entries 
						# stable			= if (all (isStable o snd) values) Stable Unstable
						# ts				= foldr max 0 (map fst values)
						= (ValueResult (Value values stable) ts rep (TCParallel taskId),{iworld & localLists = 'Map'.put taskId entries localLists})
	//Cleanup
	eval eEvent cEvent refresh repAs (TCDestroy (TCParallel taskId)) iworld=:{localLists}
		# entries = fromMaybe [] ('Map'.get taskId localLists)
		= case foldl destroyParTask (Nothing,iworld) entries of
			(Nothing,iworld)						= (DestroyedResult,{iworld & localLists = 'Map'.del taskId localLists})			//All destroyed
			(Just (ExceptionResult e str),iworld)	= (ExceptionResult e str,{iworld & localLists = 'Map'.del taskId localLists})	//An exception occurred	
			(Just result,iworld)					= (fixOverloading result initTasks (exception "Destroy failed in step"),iworld)
	//Fallback
	eval _ _ _ _ _ iworld
		= (exception "Corrupt task state in parallel", iworld)
	
	evalParTasks :: !TaskId !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !TaskRepOpts !*IWorld -> (!Maybe (TaskResult [(TaskTime,TaskValue a)]),!*IWorld) | iTask a
	evalParTasks taskId eEvent cEvent refresh repAs iworld=:{localLists}
		= evalFrom 0 (fromMaybe [] ('Map'.get taskId localLists)) iworld
	where
		evalFrom n list iworld = case foldl (evalParTask taskId eEvent cEvent refresh repAs) (Nothing,iworld) (drop n list) of
			(Just (ExceptionResult e str),iworld)	= (Just (ExceptionResult e str),iworld)
			(Nothing,iworld=:{localLists})			
				# nlist = fromMaybe [] ('Map'.get taskId localLists)
				| length nlist > length list	= evalFrom (length list) nlist iworld	//Extra branches were added -> evaluate these as well 
												= (Nothing,iworld)						//Done
			//IMPORTANT: This last rule should never match, but it helps to solve overloading solves overloading
			(Just (ValueResult val ts rep tree),iworld) = (Just (ValueResult (Value [(ts,val)] Unstable) ts rep tree),iworld)
	
	evalParTask :: !TaskId !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !TaskRepOpts !(!Maybe (TaskResult a),!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),!*IWorld) | iTask a
	//Evaluate embedded tasks
	evalParTask taskId eEvent cEvent refresh repAs (Nothing,iworld) {TaskListEntry|entryId,state=EmbeddedState (Task evala :: Task a^),result=ValueResult _ _ _ tree, removed=False}
		# (result,iworld) = evala eEvent cEvent refresh (TaskRepOpts Nothing Nothing) tree iworld
		= case result of
			ExceptionResult _ _	= (Just result,iworld)	
			_					= (Nothing,updateListEntryEmbeddedResult taskId entryId result iworld)				
	//Copy the last stored result of detached tasks
	evalParTask taskId eEvent cEvent refresh repAs (Nothing,iworld) {TaskListEntry|entryId,state=DetachedState instanceNo _ _,removed=False}
		= case loadTaskInstance instanceNo iworld of
			(Error _, iworld)	= (Nothing,iworld)	//TODO: remove from parallel if it can't be loaded (now it simply keeps the last known result)
			(Ok (meta,_,res), iworld)
				= (Nothing,updateListEntryDetachedResult taskId entryId res meta.TIMeta.progress meta.TIMeta.management iworld)

	//Do nothing if an exeption occurred or marked as removed
	evalParTask taskId eEvent cEvent refresh repAs (result,iworld) entry = (result,iworld) 

	destroyParTask :: (!Maybe (TaskResult a),!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),!*IWorld) | iTask a
	//Destroy embedded tasks
	destroyParTask (_,iworld) {TaskListEntry|entryId,state=EmbeddedState (Task evala :: Task a^),result=ValueResult _ _ _ tree}
		# (result,iworld) = evala Nothing Nothing False (TaskRepOpts Nothing Nothing) (TCDestroy tree) iworld
		= case result of
			DestroyedResult		= (Nothing,iworld)
			_					= (Just result,iworld)
	
	//Destroy detached tasks (Just delete the instance)
	destroyParTask (_,iworld) {TaskListEntry|entryId,state=DetachedState instanceNo _ _}
		= (Nothing,deleteTaskInstance instanceNo iworld)
		
	toValueAndTime :: !TaskListEntry -> (!TaskTime,TaskValue a) | iTask a
	toValueAndTime {TaskListEntry|result=ValueResult val _ _ _,time}	= (time,deserialize val)	
	where
		deserialize (Value json stable) = case fromJSON json of
			Nothing = NoValue
			Just a	= Value a stable
		deserialize NoValue	= NoValue
	toValueAndTime {TaskListEntry|time}									= (time,NoValue)
	
	parallelRep :: !d !TaskId !TaskRepOpts ![TaskListEntry] -> TaskRep | descr d
	parallelRep desc taskId repAs entries
		# layout		= repLayout repAs
		# listId		= toString taskId
		# attributes	= [(TASK_ATTRIBUTE,listId) : initAttributes desc]
		# parts = [(t,g,ac,kvSet TIME_ATTRIBUTE (toString time) (kvSet TASK_ATTRIBUTE (toString entryId) (kvSet LIST_ATTRIBUTE listId at)))
					 \\ {TaskListEntry|entryId,state=EmbeddedState _,result=ValueResult val _ (TaskRep (t,g,ac,at) _) _,time,removed=False} <- entries | not (isStable val)]	
		= TaskRep (layout ParallelComposition parts [] attributes) []
	
	isStable (Value _ Stable) 	= True
	isStable _					= False
	
	//Helper function to help type inferencing a little
	fixOverloading :: (TaskResult a) [(!ParallelTaskType,!ParallelTask a)] !b -> b
	fixOverloading _ _ x = x
							
//SHARED HELPER FUNCTIONS
appendTaskToList :: !TaskId !(!ParallelTaskType,!ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
appendTaskToList taskId=:(TaskId parent _) (parType,parTask) iworld=:{taskTime,currentUser,currentDateTime}
	# (list,iworld) = loadTaskList taskId iworld 
	# (taskIda,state,iworld) = case parType of
		Embedded
			# (taskIda,iworld)	= getNextTaskId iworld
			# task		= parTask (parListShare taskId)
			= (taskIda,EmbeddedState (dynamic task :: Task a^),iworld)
		Detached management
			# task									= parTask (parListShare taskId)
			# progress								= {issuedAt=currentDateTime,issuedBy=currentUser,status=Unstable,firstEvent=Nothing,latestEvent=Nothing}
			# (taskIda=:TaskId instanceNo _,iworld)	= createPersistentInstance task management currentUser parent iworld
			= (taskIda,DetachedState instanceNo progress management, iworld)
	# result	= ValueResult NoValue taskTime (TaskRep (SingleTask,Just (stringDisplay "Task not evaluated yet"),[],[]) []) (TCInit taskIda taskTime)
	# entry		= {entryId = taskIda, state = state, result = result, time = taskTime, removed = False}
	# iworld	= storeTaskList taskId (list ++ [entry]) iworld
	= (taskIda, iworld)		

updateListEntryEmbeddedResult :: !TaskId !TaskId (TaskResult a) !*IWorld -> *IWorld | iTask a
updateListEntryEmbeddedResult listId entryId result iworld
	= updateListEntry listId entryId (\e=:{TaskListEntry|time} -> {TaskListEntry|e & result = serialize result, time = maxTime time result}) iworld
where
	serialize (ValueResult val ts rep tree) = ValueResult (fmap toJSON val) ts rep tree
	serialize (ExceptionResult e str)		= ExceptionResult e str

	maxTime cur (ValueResult _ ts _ _)		= max cur ts
	maxTime cur _							= cur

updateListEntryDetachedResult :: !TaskId !TaskId TIResult !ProgressMeta !ManagementMeta !*IWorld -> *IWorld
updateListEntryDetachedResult listId entryId result progress management iworld
	= updateListEntry listId entryId update iworld
where
	update e=:{TaskListEntry|state=DetachedState no _ _}
		= {TaskListEntry| e & state = DetachedState no progress management,result = taskres result}
	update e = e

	taskres (TIValue val ts)	= ValueResult val ts (TaskRep (SingleTask,Nothing,[],[]) []) TCNop
	taskres (TIException e str)	= ExceptionResult e str
	
updateListEntryTime :: !TaskId !TaskId !TaskTime !*IWorld -> *IWorld
updateListEntryTime listId entryId ts iworld
	= updateListEntry listId entryId (\e -> {TaskListEntry|e & time = ts}) iworld

markListEntryRemoved :: !TaskId !TaskId !*IWorld -> *IWorld
markListEntryRemoved listId entryId iworld
	= updateListEntry listId entryId (\e -> {TaskListEntry|e & removed = True}) iworld

updateListEntry :: !TaskId !TaskId !(TaskListEntry -> TaskListEntry) !*IWorld -> *IWorld
updateListEntry listId entryId f iworld
	# (list,iworld) = loadTaskList listId iworld
	# iworld		= storeTaskList listId [if (e.TaskListEntry.entryId == entryId) (f e) e \\ e <- list] iworld
	= iworld

loadTaskList :: !TaskId !*IWorld -> (![TaskListEntry],!*IWorld)	
loadTaskList taskId=:(TaskId instanceNo taskNo) iworld=:{currentInstance,localLists}
	| instanceNo == currentInstance
		= (fromMaybe [] ('Map'.get taskId localLists),iworld)
	| otherwise
		= case loadTaskReduct instanceNo iworld of
			(Ok {TIReduct|lists},iworld)	= (fromMaybe [] ('Map'.get taskId lists),iworld)
			(_,iworld)						= ([],iworld)

storeTaskList :: !TaskId ![TaskListEntry] !*IWorld -> *IWorld	
storeTaskList taskId=:(TaskId instanceNo taskNo) list iworld=:{currentInstance,localLists}
	| instanceNo == currentInstance
		= {iworld & localLists = 'Map'.put taskId list localLists}
	| otherwise
		= case loadTaskReduct instanceNo iworld of
			(Ok reduct=:{TIReduct|lists},iworld)	= storeTaskReduct instanceNo {TIReduct|reduct & lists = 'Map'.put taskId list lists} iworld
			(_,iworld)								= iworld
			
readListId :: (SharedTaskList a) *IWorld -> (MaybeErrorString (TaskListId a),*IWorld)	| iTask a
readListId slist iworld = case read slist iworld of
	(Ok l,iworld)		= (Ok l.TaskList.listId, iworld)
	(Error e, iworld)	= (Error e, iworld)

//Derived shares
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\{TaskList|items} -> [value \\ {TaskListItem|value} <- items]) tasklist

taskListMeta :: !(SharedTaskList a) -> ReadOnlyShared [TaskListItem a]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# (taskIda,iworld) = append listId parType parTask iworld
				= (ValueResult (Value taskIda Stable) taskTime (TaskRep (SingleTask,Nothing,[],[]) []) TCNop, iworld)
			(Error e,iworld)
				= (exception e, iworld)
								
	append :: !(TaskListId a) !ParallelTaskType !(ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
	append TopLevelTaskList parType parTask iworld=:{currentUser}
		# meta						= case parType of Embedded = noMeta; Detached meta = meta;
		# task						= parTask topListShare
		= createPersistentInstance task meta currentUser 0 iworld
	append (ParallelTaskList parId) parType parTask iworld
		= appendTaskToList parId (parType,parTask) iworld

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task Void | iTask a
removeTask entryId slist = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# iworld = remove listId entryId iworld
				= (ValueResult (Value Void Stable) taskTime (TaskRep (SingleTask,Nothing,[],[]) []) TCNop, iworld)
			(Error e,iworld)
				= (exception e, iworld)

	remove :: !(TaskListId a) !TaskId !*IWorld -> *IWorld
	remove TopLevelTaskList (TaskId instanceNo 0) iworld
		= deleteTaskInstance instanceNo iworld
	remove (ParallelTaskList parId) entryId iworld
		= markListEntryRemoved parId entryId iworld
	remove _ _ iworld = iworld

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId instanceNo taskNo) = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld=:{currentInstance,currentUser}
		# iworld = setTaskWorker currentUser instanceNo iworld
		# iworld = addTaskInstanceObserver currentInstance instanceNo iworld
		= eval eEvent cEvent refresh repAs (TCBasic taskId ts JSONNull False) iworld
		
	eval eEvent cEvent refresh repAs tree=:(TCBasic taskId ts _ _) iworld=:{currentUser}
		//Load instance
		# (meta,iworld)		= loadTaskMeta instanceNo iworld
		# (result,iworld)	= loadTaskResult instanceNo iworld
		# (rep,iworld)		= loadTaskRep instanceNo iworld
		= case (meta,result,rep) of
			(_,Ok (TIValue (Value _ Stable) _),_)
				= (ValueResult (Value WOFinished Stable) ts (TaskRep (SingleTask, Nothing, [],[]) []) tree, iworld)
			(_,Ok (TIException _ _),_)
				= (ValueResult (Value WOExcepted Stable) ts (TaskRep (SingleTask, Nothing, [], []) []) tree, iworld)
			(Ok {TIMeta|worker=Just worker},_,Ok rep)
				| worker == currentUser
					= (ValueResult (Value WOActive Unstable) ts rep tree, iworld)
				| otherwise
					= (ValueResult (Value (WOInUse worker) Unstable) ts (TaskRep (SingleTask,Just (stringDisplay (toString worker +++ " is working on this task")),[],[]) []) tree, iworld)		
			_
				= (ValueResult (Value WODeleted Stable) ts (TaskRep (SingleTask, Nothing, [],[]) []) tree, iworld)

	eval eEvent cEvent refresh repAs (TCDestroy (TCBasic taskId _ _ _)) iworld
		//TODO: Remove this workon from the observers
		= (DestroyedResult,iworld)
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
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld=:{localShares}
		# localShares				= 'Map'.put taskId (toJSON initial) localShares
		# (taskIda,iworld)			= getNextTaskId iworld
		= eval eEvent cEvent refresh repAs  (TCShared taskId (TCInit taskIda ts)) {iworld & localShares = localShares}
		
	eval eEvent cEvent refresh repAs (TCShared taskId treea) iworld
		# (Task evala)				= stask (localShare taskId)
		# (resa,iworld)				= evala eEvent cEvent refresh repAs treea iworld
		= case resa of
			ValueResult NoValue lastEvent rep ntreea			= (ValueResult NoValue lastEvent rep (TCShared taskId ntreea),iworld)
			ValueResult (Value stable val) lastEvent rep ntreea	= (ValueResult (Value stable val) lastEvent rep (TCShared taskId ntreea),iworld)
			ExceptionResult e str								= (ExceptionResult e str,iworld)
	
	eval eEvent cEvent refresh repAs (TCDestroy (TCShared taskId treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala)					= stask (localShare taskId)
		# (resa,iworld=:{localShares})	= evala eEvent cEvent refresh repAs (TCDestroy treea) iworld
		= (resa,{iworld & localShares = 'Map'.del taskId localShares})
	
	eval _ _ _ _ _ iworld
		= (exception "Corrupt task state in withShared", iworld)	
/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a

instance tune SetLayout
where
	tune (SetLayout layout) (Task eval)	= Task eval`
	where
		eval` eEvent cEvent refresh (TaskRepOpts Nothing mod) state iworld
			= eval eEvent cEvent refresh (TaskRepOpts (Just ((fromMaybe id mod) layout)) Nothing) state iworld 
		eval` eEvent cEvent refresh (TaskRepOpts (Just layout) mod) state iworld
			= eval eEvent cEvent refresh (TaskRepOpts (Just layout) Nothing) state iworld 
		eval` eEvent cEvent refresh repAs state iworld
			= eval eEvent cEvent refresh repAs state iworld 

instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task eval)	= Task eval`
	where
		eval` eEvent cEvent refresh (TaskRepOpts layout Nothing) state iworld
			= eval eEvent cEvent refresh (TaskRepOpts layout (Just f)) state iworld 
		eval` eEvent cEvent refresh (TaskRepOpts layout (Just g)) state iworld
			= eval eEvent cEvent refresh (TaskRepOpts layout (Just (g o f))) state iworld 	
		eval` eEvent cEvent refresh repAs state iworld
			= eval eEvent cEvent refresh repAs state iworld 
	