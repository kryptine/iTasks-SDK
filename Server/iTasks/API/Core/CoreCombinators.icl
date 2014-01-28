implementation module iTasks.API.Core.CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList

import Internet.HTTP, GenEq, System.Time, Text, Data.Func, Data.Tuple, Data.List, Data.Error, Data.Either, Text.JSON
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskStore, iTasks.Framework.TaskEval
import iTasks.Framework.Util, iTasks.Framework.Shared, iTasks.Framework.Store
import iTasks.Framework.Generic, iTasks.Framework.UIDefinition
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators

import iTasks.Framework.Client.Override

from Data.Map						import qualified get, put, del, newMap, toList, fromList
from StdFunc					import id, const, o, seq
from iTasks						import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from iTasks.Framework.IWorld	import :: IWorld(..)
from iTasks.Framework.TaskEval	import localShare, parListShare, topListShare
from iTasks.API.Core.CoreTasks	import return
from Data.SharedDataSource			import write, writeFilterMsg, read, readRegister

derive class iTask ParallelTaskType, WorkOnStatus

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f (Task evala) = Task eval
where
	eval event repOpts tree iworld = case evala event repOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e str, iworld)				= (ExceptionResult e str, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a
project projection share (Task evala) = Task eval
where
	eval event repOpts (TCDestroy (TCProject taskId encprev treea)) iworld	//Cleanup duty simply passed to inner task
		= evala event repOpts (TCDestroy treea) iworld

	eval event repOpts state iworld
		# (taskId,prev,statea) = case state of
			(TCInit taskId _)					= (taskId,NoValue,state)
			(TCProject taskId encprev statea)	= (taskId,fromJust (fromJSON encprev),statea)
			
		# (resa, iworld) 	= evala event repOpts statea iworld
		= case resa of
			ValueResult val ts rep ncxta
				# result = ValueResult val ts rep (TCProject taskId (toJSON val) ncxta)
				| val =!= prev
					= projectOnShare val result iworld
				| otherwise
					= (result,iworld)
			ExceptionResult e str
				= (ExceptionResult e str,iworld)
	
	projectOnShare val result iworld=:{current={taskInstance}}
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

step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskStep a b] -> Task b | iTask a & iTask b
step (Task evala) lhsValFun conts = Task eval
where
	eval event repOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event repOpts (TCStep taskId ts (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval event repOpts (TCStep taskId ts (Left treea)) iworld=:{current={taskTime}}
		# (resa, iworld) 	= evala event repOpts treea iworld
		# ts				= case event of
							(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
		# mbcommit			= case event of
			(ActionEvent _ t action)
				| t == taskId 		= Just action
			_						= Nothing
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbcommit conts of
				Nothing			
					# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
                    # value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
					= Left (ValueResult value info (doStepLayout taskId repOpts rep val) (TCStep taskId info.TaskInfo.lastEvent (Left ntreea)) )
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskInfo.lastEvent)
			ExceptionResult e str = case searchContException e str conts of
				Nothing			= Left (ExceptionResult e str)
				Just rewrite	= Right (rewrite,Nothing, ts)		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala (toRefresh event) repOpts (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb (toRefresh event) repOpts (TCInit taskIdb lastEvent) iworld 
				= case resb of
					ValueResult val info rep nstateb	
						# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
						= (ValueResult val info (finalizeRep repOpts rep) (TCStep taskId info.TaskInfo.lastEvent (Right (d_json_a,sel,nstateb))),iworld)
					ExceptionResult e str				= (ExceptionResult e str, iworld)
	//Eval right-hand side
	eval event repOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{current={taskTime}}
		# ts				= case event of
							(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb event repOpts treeb iworld
				= case resb of
					ValueResult val info rep ntreeb
						# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
						= (ValueResult val info (finalizeRep repOpts rep) (TCStep taskId info.TaskInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e str			= (ExceptionResult e str, iworld)
			Nothing
				= (exception "Corrupt task value in step", iworld)	
	
	//Cleanup
	eval event repOpts (TCDestroy (TCStep taskId ts (Left treea))) iworld
		= case evala event repOpts (TCDestroy treea) iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e str,iworld)	= (ExceptionResult e str,iworld)
			(ValueResult _ _ _ _,iworld)	= (exception "Destroy failed in step",iworld)
	
	eval event repOpts (TCDestroy (TCStep taskId ts (Right(enca,sel,treeb)))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)	= evalb event repOpts (TCDestroy treeb) iworld
			Nothing				= (exception "Corrupt task value in step", iworld)
			
	//Incorred state
	eval event _ state iworld
		= (exception ("Corrupt task state in step:" +++ (toString (toJSON state))), iworld)

	searchContValue val mbcommit conts = search val mbcommit 0 Nothing conts
	where
		search _ _ _ mbmatch []							= mbmatch									//No matching OnValue steps were found, return the potential match
		search val mbcommit i mbmatch [OnValue f:cs]
			= case f val of
				Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
				Nothing		= search val mbcommit (i + 1) mbmatch cs	//Keep search
		search val (Just commit) i Nothing [OnAction action f:cs]
			| commit == actionName action
				= case f val of
					Just cont	= search val (Just commit) (i + 1) (Just (i, cont, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
					Nothing		= search val (Just commit) (i + 1) Nothing cs								//Keep searching
			| otherwise
								= search val (Just commit) (i + 1) Nothing cs								//Keep searching														
		search val mbcommit i mbmatch [_:cs]			= search val mbcommit (i + 1) mbmatch cs			//Keep searching
		
	searchContException dyn str conts = search dyn str 0 Nothing conts
	where
		search _ _ _ catchall []					= catchall														//Return the maybe catchall
		search dyn str i catchall [OnException f:cs] = case (match f dyn) of
			Just (taskb,enca)						= Just (i, taskb, enca)											//We have a match
			_										= search dyn str (i + 1) catchall cs							//Keep searching
		search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
		search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs							//Keep searching
				
		match :: (e -> Task b) Dynamic -> Maybe (Task b, DeferredJSON) | iTask e
		match f (e :: e^)	= Just (f e, DeferredJSON e)
		match _ _			= Nothing 
	
	restoreTaskB sel d_json_a = case conts !! sel of
		(OnValue taskbf)			= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnAction _ taskbf)			= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnException taskbf)		= call_with_DeferredJSON taskbf d_json_a
		(OnAllExceptions taskbf)	= call_with_DeferredJSON taskbf d_json_a
	
	doStepLayout taskId repOpts NoRep val
		= finalizeRep repOpts (TaskRep ((repLayoutRules repOpts).LayoutRules.accuStep {UIDef|content=UIActionSet [],windows=[]} (stepActions taskId val)) [])
	doStepLayout taskId repOpts (TaskRep def parts) val
		= finalizeRep repOpts (TaskRep ((repLayoutRules repOpts).LayoutRules.accuStep def (stepActions taskId val)) parts)
	stepActions taskId val = [{UIAction|taskId=toString taskId,action=action,enabled=isJust (taskbf val)}\\ OnAction action taskbf <- conts]

	call_with_DeferredJSON_TaskValue :: ((TaskValue a) -> (Maybe (Task .b))) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
	call_with_DeferredJSON_TaskValue f_tva_tb d_json_tva=:(DeferredJSON tva)
        = f_tva_tb (cast_to_TaskValue tva)
	
	call_with_DeferredJSON_TaskValue f_tva_tb (DeferredJSONNode json)
		= case fromJSON json of
			Just a ->  f_tva_tb a
			Nothing -> Nothing
	
	call_with_DeferredJSON :: (a -> Task .b) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
    call_with_DeferredJSON f_tva_tb d_json_tva=:(DeferredJSON tva)
        = Just (f_tva_tb (cast tva))

	call_with_DeferredJSON f_tva_tb (DeferredJSONNode json)
		= case fromJSON json of
			Just a ->  Just (f_tva_tb a)
			Nothing -> Nothing

// Parallel composition
parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
parallel desc initTasks = Task eval
where
	//Create initial task list
	eval event repOpts (TCInit taskId ts) iworld=:{IWorld|current=current=:{localLists}}
		//Append the initial tasks to the list
		# iworld	= foldl append {iworld & current = {current & localLists = 'Data.Map'.put taskId [] localLists}} initTasks
		//Evaluate the parallel
		= eval event repOpts (TCParallel taskId ts) iworld
	where
		append iworld t = snd (appendTaskToList taskId t iworld)

	//Evaluate the task list
	eval event repOpts (TCParallel taskId ts) iworld=:{current={taskTime}}
		//Evaluate all parallel tasks in the list
		= case evalParTasks taskId event repOpts iworld of
			(Just res=:(ExceptionResult e str),_,iworld)	= (res,iworld)
			(Just res=:(ValueResult _ _ _ _),_,iworld)		= (exception "parallel evaluation yielded unexpected result",iworld)
			(Nothing,results,iworld=:{current=current=:{localLists}})
                # entries = [(e,r) \\ e <- (fromMaybe [] ('Data.Map'.get taskId localLists)) & r <- results]
				//Filter out removed entries and destroy their state
				# (removed,entries)	= splitWith (\({TaskListEntry|removed},_) -> removed) entries
				= case foldl destroyParTask (Nothing,iworld) (map fst removed) of
					(Just (ExceptionResult e str),iworld)	= (ExceptionResult e str,iworld)	//An exception occurred	
					(Just result,iworld)					= (fixOverloading result initTasks (exception "Destroy failed in parallel"),iworld)
					(Nothing,iworld=:{current=current=:{localLists}})
						//Destruction is ok, build parallel result
						# rep				= parallelRep desc taskId repOpts entries
						# values			= map (toValueAndTime o fst) entries
						# stable			= all (isStable o snd) values
						# refreshSensitive	= foldr (\(e,_) s -> s || refreshSensitive e) False entries
						# ts				= foldr max 0 [ts:map fst values]
						# ts				= case event of
							(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
						= (ValueResult (Value values stable) {TaskInfo|lastEvent=ts,refreshSensitive=refreshSensitive}
							(finalizeRep repOpts rep) (TCParallel taskId ts),{iworld & current = {current & localLists = 'Data.Map'.put taskId (map fst entries) localLists}})
	//Cleanup
	eval event repOpts (TCDestroy (TCParallel taskId ts)) iworld=:{current={localLists}}
		# entries = fromMaybe [] ('Data.Map'.get taskId localLists)
		= case foldl destroyParTask (Nothing,iworld) entries of
			(Nothing,iworld=:{current=current=:{localLists}}) //All destroyed
				= (DestroyedResult,{iworld & current = {current & localLists = 'Data.Map'.del taskId localLists}})
			(Just (ExceptionResult e str),iworld=:{current=current=:{localLists}}) //An exception occurred
				= (ExceptionResult e str,{iworld & current = {current & localLists = 'Data.Map'.del taskId localLists}})
			(Just result,iworld)
                = (fixOverloading result initTasks (exception "Destroy failed in step"),iworld)
	//Fallback
	eval _ _ _ iworld
		= (exception "Corrupt task state in parallel", iworld)
	
	evalParTasks :: !TaskId !Event !TaskRepOpts !*IWorld -> (!Maybe (TaskResult [(TaskTime,TaskValue a)]),![Maybe TaskRep],!*IWorld) | iTask a
	evalParTasks taskId event repOpts iworld=:{current={localLists,eventRoute}}
		= evalFrom 0 [] (fromMaybe [] ('Data.Map'.get taskId localLists)) ('Data.Map'.get taskId eventRoute) repOpts iworld
	where
		evalFrom n acc list mbEventIndex repOpts iworld = case foldl (evalParTask taskId event mbEventIndex repOpts) (Nothing,acc,iworld) [(i,e) \\ e <- drop n list & i <- [n..]]  of
			(Just (ExceptionResult e str),acc,iworld)	= (Just (ExceptionResult e str),acc,iworld)
			(Nothing,acc,iworld=:{current={localLists}})			
				# nlist = fromMaybe [] ('Data.Map'.get taskId localLists)
				# lenlist = length list
				| length nlist > lenlist	= evalFrom lenlist acc nlist Nothing repOpts iworld	//Extra branches were added -> evaluate these as well 
											= (Nothing,acc,iworld)						        //Done
			//IMPORTANT: This last rule should never match, but it helps to solve overloading 
			(Just (ValueResult val info=:{TaskInfo|lastEvent} rep tree),acc,iworld) = (Just (ValueResult (Value [(lastEvent,val)] False) info rep tree),acc,iworld)
	
	evalParTask :: !TaskId !Event !(Maybe Int) !TaskRepOpts !(!Maybe (TaskResult a),![Maybe TaskRep],!*IWorld) !(!Int,!TaskListEntry) -> (!Maybe (TaskResult a),![Maybe TaskRep],!*IWorld) | iTask a
	//Evaluate embedded tasks
	evalParTask taskId event mbEventIndex repOpts (Nothing,acc,iworld=:{current={localTasks}}) (index,{TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult jsonval info rep tree, removed=False})
		# evalNeeded = case mbEventIndex of
			Nothing	= True //We don't know the event index, so we just have to try
			Just eventIndex
				| eventIndex == index	= True								//The event is targeted at this branch, we evaluate
										= info.TaskInfo.refreshSensitive	//Also evaluate if the branch is refresh sensitive
		| evalNeeded
			//Evaluate the branch
			= case 'Data.Map'.get entryId localTasks of
                Just dtask
                    # (Task evala) = unwrapTask dtask
				//Just (Task evala :: Task a^)
					# (result,iworld) = evala event {TaskRepOpts|useLayout=Nothing,modLayout=Nothing,noUI=repOpts.noUI} tree iworld
					= case result of
						ExceptionResult _ _
							= (Just result,acc,iworld)	
						ValueResult val info rep tree
							# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId result iworld
							= (Nothing, acc++[Just rep],iworld)
				_
					= (Nothing,acc,iworld)	
		| otherwise
			# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId (ValueResult jsonval info rep tree) iworld
			= (Nothing, acc++[Just rep],iworld)
					
	//Copy the last stored result of detached tasks
	evalParTask taskId=:(TaskId curInstanceNo _) event mbEventIndex noUI (Nothing,acc,iworld) (index,{TaskListEntry|entryId,state=DetachedState instanceNo _ _,removed=False})
		# (mbMeta,iworld)	= readRegister curInstanceNo (taskInstanceMeta instanceNo) iworld
		# (mbValue,iworld)	= readRegister curInstanceNo (taskInstanceValue instanceNo) iworld
		= case (mbMeta,mbValue) of
			(Ok meta,Ok value)
				# (entry,iworld) = updateListEntryDetachedResult taskId entryId value meta.TIMeta.progress meta.TIMeta.management iworld
				= (Nothing,acc++[Nothing],iworld)
			_	
                =  (Nothing,acc,iworld)	//TODO: remove from parallel if it can't be loaded (now it simply keeps the last known result)

	//Do nothing if an exeption occurred or marked as removed
	evalParTask taskId event mbEventIndex noUI (result,acc,iworld) (index,entry) = (result,acc,iworld) 

	destroyParTask :: (!Maybe (TaskResult a),!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),!*IWorld) | iTask a
	//Destroy embedded tasks
	destroyParTask (_,iworld=:{current={localTasks}}) {TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult _ _ _ tree}
		= case 'Data.Map'.get entryId localTasks of
			Just (Task evala :: Task a^)
				# (result,iworld=:{current=current=:{localTasks}}) = evala (RefreshEvent Nothing) {TaskRepOpts|useLayout=Nothing,modLayout=Nothing,noUI=True} (TCDestroy tree) iworld
				# iworld = {iworld & current = {current & localTasks = 'Data.Map'.del entryId localTasks}}
				= case result of
					DestroyedResult		= (Nothing,iworld)
					_					= (Just result,iworld)
			_
				= (Nothing,iworld)
	
	//Destroy detached tasks (Just delete the instance)
	destroyParTask (_,iworld) {TaskListEntry|entryId,state=DetachedState instanceNo _ _}
		= (Nothing,deleteInstance instanceNo iworld)
		
	toValueAndTime :: !TaskListEntry -> (!TaskTime,TaskValue a) | iTask a
	toValueAndTime {TaskListEntry|lastEval=ValueResult val _ _ _,lastEvent}	= (lastEvent,deserialize val)	
	where
		deserialize (Value json stable) = case fromJSON json of
			Nothing = NoValue
			Just a	= Value a stable
		deserialize NoValue	= NoValue
	toValueAndTime {TaskListEntry|lastEvent}						= (lastEvent,NoValue)
	
	parallelRep :: !d !TaskId !TaskRepOpts ![(!TaskListEntry,!Maybe TaskRep)] -> TaskRep | descr d
	parallelRep desc taskId repOpts entries
		# layout		= repLayoutRules repOpts
		# listId		= toString taskId
		# parts = [(uiDefSetAttribute LAST_EVENT_ATTRIBUTE (toString lastEvent) (uiDefSetAttribute CREATED_AT_ATTRIBUTE (toString createdAt) (uiDefSetAttribute TASK_ATTRIBUTE (toString entryId) def)))
					 \\ ({TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult val _ _ _,createdAt,lastEvent,removed=False},Just (TaskRep def _)) <- entries | not (isStable val)]	
		= TaskRep (layout.LayoutRules.accuParallel (toPrompt desc) parts) []

	isStable (Value _ stable) 	= stable
	isStable _					= False

	refreshSensitive {TaskListEntry|lastEval=ValueResult _ {TaskInfo|refreshSensitive} _ _} = refreshSensitive
	refreshSensitive _ = True
	
	//Helper function to help type inferencing a little
	fixOverloading :: (TaskResult a) [(!ParallelTaskType,!ParallelTask a)] !b -> b
	fixOverloading _ _ x = x
						
//SHARED HELPER FUNCTIONS
appendTaskToList :: !TaskId !(!ParallelTaskType,!ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
appendTaskToList taskId (parType,parTask) iworld=:{current={taskTime,user,attachmentChain,localDateTime}}
	# (list,iworld) = loadTaskList taskId iworld
	# progress = {value=None, issuedAt=localDateTime,issuedBy=user,stable=True,firstEvent=Nothing,latestEvent=Nothing} //FIXME : value should not be None
	# (taskIda,name,state,iworld) = case parType of
		Embedded
			# (taskIda,iworld=:{current=current=:{localTasks}})	= getNextTaskId iworld
			# task		= parTask (parListShare taskId taskIda)
			= (taskIda, Nothing, EmbeddedState, {iworld & current = {current & localTasks = 'Data.Map'.put taskIda (dynamic task :: Task a^) localTasks}})
        NamedEmbedded name
			# (taskIda,iworld=:{current=current=:{localTasks}})	= getNextTaskId iworld
			# task		= parTask (parListShare taskId taskIda)
			= (taskIda, Just name, EmbeddedState, {iworld & current = {current & localTasks = 'Data.Map'.put taskIda (dynamic task :: Task a^) localTasks}})
		Detached management evalDirect
            # (instanceNo,iworld)                   = newInstanceNo iworld
			# task									= parTask (parListShare taskId (TaskId instanceNo 0))
			# (taskIda,iworld)	                    = createDetachedTaskInstance task (Just instanceNo) Nothing management user taskId (if evalDirect (Just attachmentChain) Nothing) iworld
			= (taskIda,Nothing,DetachedState instanceNo progress management, iworld)
	    NamedDetached name management evalDirect
            # (instanceNo,iworld)                   = newInstanceNo iworld
			# task									= parTask (parListShare taskId (TaskId instanceNo 0))
			# (taskIda,iworld)	                    = createDetachedTaskInstance task (Just instanceNo) (Just name) management user taskId (if evalDirect (Just attachmentChain) Nothing) iworld
			= (taskIda,Just name,DetachedState instanceNo progress management, iworld)
	# lastEval	= ValueResult NoValue {TaskInfo|lastEvent=taskTime,refreshSensitive=True} NoRep (TCInit taskIda taskTime)
	# entry		= {entryId = taskIda, name = name, state = state, lastEval = lastEval, attributes = 'Data.Map'.newMap, createdAt = taskTime, lastEvent = taskTime, removed = False}
	# iworld	= storeTaskList taskId (list ++ [entry]) iworld
	= (taskIda, iworld)	

updateListEntryEmbeddedResult :: !TaskId !TaskId (TaskResult a) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
updateListEntryEmbeddedResult listId entryId result iworld
	= updateListEntry listId entryId (\e=:{TaskListEntry|state,lastEvent} ->
		{TaskListEntry|e & lastEval= wrap result, attributes = newAttr result, lastEvent = maxTime lastEvent result}) iworld
where
	wrap (ValueResult val info=:{TaskInfo|refreshSensitive=True} _ tree) //When we know for certain that we'll recompute the task on the next event, 
		= ValueResult (fmap toJSON val) info NoRep tree					 //don't bother storing the task representation
	wrap (ValueResult val info rep tree)	= ValueResult (fmap toJSON val) info rep tree
	wrap (ExceptionResult e str)			= ExceptionResult e str

	newAttr (ValueResult _ _ (TaskRep def _) _)					= uiDefAttributes def
	newAttr _													= 'Data.Map'.newMap
	
	maxTime cur (ValueResult _ {TaskInfo|lastEvent} _ _)		= max cur lastEvent
	maxTime cur _												= cur

updateListEntryDetachedResult :: !TaskId !TaskId TIValue !ProgressMeta !ManagementMeta !*IWorld -> (!TaskListEntry,!*IWorld)
updateListEntryDetachedResult listId entryId lastValue progress management iworld
	= updateListEntry listId entryId update iworld
where
	update e=:{TaskListEntry|state=DetachedState no _ _}
        # lastEval = case lastValue of
            TIValue val = ValueResult val info NoRep TCNop
            TIException e str = ExceptionResult e str
		= {TaskListEntry| e & state = DetachedState no progress management, lastEval = lastEval}
	update e = e

    info = {refreshSensitive=True,lastEvent=0} //FIXME probably a bad idea to construct this nonsense info that may or may not be used

markListEntryRemoved :: !TaskId !TaskId !*IWorld -> *IWorld
markListEntryRemoved listId entryId iworld
	= snd (updateListEntry listId entryId (\e -> {TaskListEntry|e & removed = True}) iworld)

updateListEntry :: !TaskId !TaskId !(TaskListEntry -> TaskListEntry) !*IWorld -> (!TaskListEntry,!*IWorld)
updateListEntry listId entryId f iworld
	# (list,iworld) = loadTaskList listId iworld
	# list			= [if (e.TaskListEntry.entryId == entryId) (f e) e \\ e <- list]	//TODO: MERGE AND OPTIZE WITH ITEM SEARCH
	# [item:_]		= [e \\ e <- list | (e.TaskListEntry.entryId == entryId)]
	# iworld		= storeTaskList listId list iworld
	= (item,iworld)

loadTaskList :: !TaskId !*IWorld -> (![TaskListEntry],!*IWorld)	
loadTaskList taskId=:(TaskId instanceNo taskNo) iworld=:{current={taskInstance,localLists}}
	| instanceNo == taskInstance
		= (fromMaybe [] ('Data.Map'.get taskId localLists),iworld)
	| otherwise
		= case read (taskInstanceReduct instanceNo) iworld of
			(Ok {TIReduct|lists},iworld)	= (fromMaybe [] ('Data.Map'.get taskId lists),iworld)
			(_,iworld)						= ([],iworld)

storeTaskList :: !TaskId ![TaskListEntry] !*IWorld -> *IWorld	
storeTaskList taskId=:(TaskId instanceNo taskNo) list iworld=:{current=current=:{taskInstance,localLists}}
	| instanceNo == taskInstance
		= {iworld & current = {current & localLists = 'Data.Map'.put taskId list localLists}}
	| otherwise
		= case read (taskInstanceReduct instanceNo) iworld of
			(Ok reduct=:{TIReduct|lists},iworld)	
				# (_,iworld) = write {TIReduct|reduct & lists = 'Data.Map'.put taskId list lists} (taskInstanceReduct instanceNo) iworld
				= iworld
			(_,iworld)								= iworld
			
readListId :: (SharedTaskList a) *IWorld -> (MaybeErrorString (TaskListId a),*IWorld)	| iTask a
readListId slist iworld = case read slist iworld of
	(Ok l,iworld)		= (Ok l.TaskList.listId, iworld)
	(Error e, iworld)	= (Error e, iworld)

//Derived shares
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\{TaskList|items} -> [value \\ {TaskListItem|value} <- items]) (toReadOnly tasklist)

taskListMeta :: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,ManagementMeta)]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist

taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId
taskListSelfId tasklist = mapRead (\{TaskList|selfId} -> selfId) (toReadOnly tasklist)

taskListSelfManagement :: !(SharedTaskList a) -> Shared ManagementMeta
taskListSelfManagement tasklist = mapReadWriteError (toPrj,fromPrj) tasklist
where
    toPrj {TaskList|selfId,items} = case [m \\ m=:{TaskListItem|taskId} <- items | taskId == selfId] of
        []                              = trace_n (length items) (Error "Task id not found in self management share")
        [{managementMeta=Nothing}:_]    = Error "Self management share is only possible for detached tasks"
        [{managementMeta=Just meta}:_]  = Ok meta

    fromPrj meta {TaskList|selfId}
        = Ok (Just [(selfId,meta)])

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# (taskIda,iworld) = append listId parType parTask iworld
				= (Ok taskIda, iworld)
			(Error e,iworld)
				= (Error (dynamic e,e), iworld)
								
	append :: !(TaskListId a) !ParallelTaskType !(ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
	append TopLevelTaskList parType parTask iworld=:{current={user,attachmentChain}}
		# (name,meta,evalDirect) = case parType of
            (Embedded)                              = (Nothing,defaultValue,False)
            (NamedEmbedded name)                    = (Just name,defaultValue,False)
            (Detached meta evalDirect)              = (Nothing,meta,evalDirect)
            (NamedDetached name meta evalDirect)    = (Just name,meta,evalDirect)
		# task						= parTask topListShare
		= createDetachedTaskInstance task Nothing name meta user (TaskId 0 0) (if evalDirect (Just attachmentChain) Nothing) iworld
	append (ParallelTaskList parId) parType parTask iworld
		= appendTaskToList parId (parType,parTask) iworld

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task Void | iTask a
removeTask entryId slist = mkInstantTask eval
where
	eval taskId iworld
		= case readListId slist iworld of
			(Ok listId,iworld)
				# iworld = remove listId entryId iworld
				= (Ok Void, iworld)
			(Error e,iworld)
				= (Error (dynamic e,e), iworld)

	remove :: !(TaskListId a) !TaskId !*IWorld -> *IWorld
	remove TopLevelTaskList (TaskId instanceNo 0) iworld
		= deleteInstance instanceNo iworld
	remove (ParallelTaskList parId) entryId iworld
		= markListEntryRemoved parId entryId iworld
	remove _ _ iworld = iworld

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId instanceNo taskNo) = Task eval
where
	eval event repOpts (TCInit taskId ts) iworld=:{current={attachmentChain,user}}
		# (meta,iworld)		= read (taskInstanceMeta instanceNo) iworld
		= case meta of
			Ok meta
                //Just steal the instance, TODO, make stealing optional
				# (_,iworld)	= write {TIMeta|meta & instanceType=AttachedInstance [taskId:attachmentChain] user} (taskInstanceMeta instanceNo) iworld
				# iworld		= queueUrgentEvaluate instanceNo iworld
				= eval event repOpts (TCBasic taskId ts JSONNull False) iworld
			Error e
				= (ExceptionResult (dynamic e) e,iworld)
		
	eval event repOpts tree=:(TCBasic taskId ts _ _) iworld=:{current={taskInstance,user}}
		//Load instance
		# (meta,iworld)		= readRegister taskInstance (taskInstanceMeta instanceNo) iworld
		# (rep,iworld)	    = readRegister taskInstance (taskInstanceRep instanceNo) iworld
		# layout			= repLayoutRules repOpts
		= case (meta,rep) of
			(Ok meta=:{TIMeta|progress,instanceType=AttachedInstance _ worker},Ok rep)
                | progress.ProgressMeta.value === Exception
				    = (ValueResult (Value WOExcepted True) {TaskInfo|lastEvent=ts,refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)
                | progress.ProgressMeta.value === Stable
				    = (ValueResult (Value WOFinished True) {TaskInfo|lastEvent=ts,refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)
				| worker == user
                    # rep = case rep of
                        (TaskRep def parts) = TaskRep (layout.LayoutRules.accuWorkOn def meta) parts
                        _                   = NoRep
					# rep = finalizeRep repOpts rep
					= (ValueResult (Value WOActive False) {TaskInfo|lastEvent=ts,refreshSensitive=True} rep tree, iworld)
				| otherwise
                    # rep = case rep of
                        (TaskRep def parts) = TaskRep (layout.LayoutRules.accuWorkOn (inUseDef worker) meta) parts
                        _                   = NoRep
					# rep = finalizeRep repOpts rep
					= (ValueResult (Value (WOInUse worker) False) {TaskInfo|lastEvent=ts,refreshSensitive=False} rep tree, iworld)		
			_
				= (ValueResult (Value WODeleted True) {TaskInfo|lastEvent=ts,refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)

	eval event repOpts (TCDestroy (TCBasic taskId _ _ _)) iworld
        # (meta,iworld) = read fullInstanceMeta iworld //FIXME: Figure out how to get the right share notifications for the released instances
        = case meta of
            Ok instances
                # (_,iworld) = write ('Data.Map'.fromList [(n,release taskId m) \\ (n,m) <- 'Data.Map'.toList instances]) fullInstanceMeta iworld
                = (DestroyedResult,iworld)
		    _   = (DestroyedResult,iworld)

    release taskId meta=:{TIMeta|instanceType=AttachedInstance attachment worker}
        | isMember taskId attachment    = {meta & instanceType = DetachedInstance}
                                        = meta
    release taskId meta = meta

	inUseDef worker
		= {UIDef|content=UIControlStack {UIControlStack|attributes='Data.Map'.newMap,controls=[(stringDisplay (toString worker +++ " is working on this task"),'Data.Map'.newMap)]},windows=[]}
/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs asUser (Task eval) = Task eval`
where
	eval` event repOpts state iworld=:{current=current=:{user}}
		# (result,iworld=:{current}) = eval event repOpts state {iworld & current = {current & user = asUser}}
		= (result,{iworld & current = {current & user = user}})

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task eval
where	
	eval event repOpts (TCInit taskId ts) iworld
		# (taskIda,iworld=:{current=current=:{localShares}})
                                    = getNextTaskId iworld
		# localShares				= 'Data.Map'.put taskId (toJSON initial) localShares
		= eval event repOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & current = {current & localShares = localShares}}
		
	eval event repOpts (TCShared taskId ts treea) iworld=:{current={taskTime}}
		# ts						= case event of
			(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
			_						= ts
		# (Task evala)				= stask (localShare taskId)
		# (resa,iworld)				= evala event repOpts treea iworld
		= case resa of
			ValueResult NoValue info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult NoValue info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ValueResult (Value stable val) info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult (Value stable val) info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ExceptionResult e str								= (ExceptionResult e str,iworld)
	
	eval event repOpts (TCDestroy (TCShared taskId ts treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala)					= stask (localShare taskId)
		# (resa,iworld=:{current=current=:{localShares}})
            = evala event repOpts (TCDestroy treea) iworld
		= (resa,{iworld & current = {current & localShares = 'Data.Map'.del taskId localShares}})
	
	eval _ _ _ iworld
		= (exception "Corrupt task state in withShared", iworld)

import StdDebug

exposeShared :: !(ReadWriteShared r w) !(String (ReadWriteShared r w) -> Task a) -> Task a | iTask a & iTask r & iTask w
exposeShared shared stask = Task eval
where	
	eval event repOpts (TCInit taskId ts) iworld=:{exposedShares}
		# (url, iworld)		= newURL iworld
		// Trick to make it work until John fixes the compiler
		# exposedShares 	= 'Data.Map'.put url (dynamic shared :: RWShared r^ w^ *IWorld, toJSONShared shared) exposedShares
		# (taskIda,iworld)	= trace_n ("SDS is exposed as "+++url) (getNextTaskId iworld)
		= eval event repOpts (TCExposedShared taskId ts url (TCInit taskIda ts)) {iworld & exposedShares = exposedShares}
		
	eval event repOpts (TCExposedShared taskId ts url treea) iworld=:{current={taskTime}}
		# ts						= case event of
			(FocusEvent _ focusId)	= if (focusId == taskId) taskTime ts
			_						= ts
		# (Task evala)				= stask url (exposedShare url)
		# (resa,iworld)				= evala event repOpts treea iworld
		= case resa of
			ValueResult value info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult value info rep (TCExposedShared taskId info.TaskInfo.lastEvent url ntreea),iworld)
			ExceptionResult e str					
				= (ExceptionResult e str,iworld)
	
	eval event repOpts (TCDestroy (TCExposedShared taskId ts url treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala)					= stask url (exposedShare url)
		# (resa,iworld)					= evala event repOpts (TCDestroy treea) iworld
		= (resa,{iworld & exposedShares = 'Data.Map'.del url iworld.exposedShares})
	
	eval _ _ _ iworld
		= (exception "Corrupt task state in exposeShared", iworld) 
		
/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

instance tune SetLayout
where
	tune (SetLayout layout) (Task eval)	= Task eval`
	where
		eval` event repOpts=:{useLayout=Nothing,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just ((fromMaybe id modLayout) layout), modLayout = Nothing} state iworld 
		eval` event repOpts=:{useLayout=Just _,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just layout, modLayout = Nothing} state iworld 
	
instance tune AfterLayout
where
	tune (AfterLayout f) (Task eval) = Task eval`
	where
		eval` event repOpts state iworld = case eval event repOpts state iworld of
	        (ValueResult value info rep tree,iworld) = (ValueResult value info (updRep rep) tree, iworld)
            (res,iworld) = (res,iworld)

        updRep NoRep               = TaskRep (f {UIDef|content=UIAttributeSet 'Data.Map'.newMap,windows=[]}) []
        updRep (TaskRep def parts) = TaskRep (f def) parts
		
instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task eval)	= Task eval`
	where
		eval` event repOpts=:{modLayout=Nothing} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just f} state iworld 
		eval` event repOpts=:{modLayout=Just g} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just (g o f)} state iworld 	

instance tunev SetValueAttribute a
where
    tunev (SetValueAttribute attr f) (Task eval) = Task eval`
    where
		eval` event repOpts state iworld
            = case (eval event repOpts state iworld) of
		    	(ValueResult value=:(Value v _) info rep tree,iworld) = (ValueResult value info (updRep v rep) tree, iworld)
		    	(res,iworld) = (res,iworld)

        updRep v NoRep	             = TaskRep ({UIDef|content=UIAttributeSet ('Data.Map'.put attr (f v) 'Data.Map'.newMap),windows=[]}) []
        updRep v (TaskRep def parts) = TaskRep (uiDefSetAttribute attr (f v) def) parts
	
instance tune LazyRefresh
where
	tune _ (Task eval) = Task eval`
	where
		eval` event repOpts state iworld
			= case (eval event repOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)
