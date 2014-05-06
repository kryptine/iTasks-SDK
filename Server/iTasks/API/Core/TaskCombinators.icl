implementation module iTasks.API.Core.TaskCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList

import Internet.HTTP, GenEq, System.Time, Text, Data.Func, Data.Tuple, Data.List, Data.Error, Data.Either, Text.JSON
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskStore, iTasks.Framework.TaskEval
import iTasks.Framework.Util, iTasks.Framework.Store
import iTasks.Framework.Generic, iTasks.Framework.UIDefinition
import iTasks.API.Core.Types, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.IWorld

import iTasks.Framework.Client.Override

from Data.Map						    import qualified get, put, del, newMap, toList, fromList
from StdFunc					        import id, const, o, seq
from iTasks						        import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from iTasks.Framework.TaskEval	        import localShare, parListShare, topListShare
from iTasks.Framework.SDS               import write, writeFilterMsg, read, readRegister
from iTasks.API.Core.Tasks	            import return
from iTasks.API.Core.SDSCombinators     import sdsFocus
from iTasks.API.Common.SDSCombinators   import toReadOnly, mapRead, mapReadWriteError

derive class iTask ParallelTaskType, WorkOnStatus

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f (Task mdi evala) = Task mdi eval
where
	eval event repOpts tree iworld = case evala event repOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e, iworld)				    = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a
project projection share (Task mdi evala) = Task mdi eval
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
			ExceptionResult e
				= (ExceptionResult e,iworld)
	
	projectOnShare val result iworld=:{current={TaskEvalState|taskInstance}}
		# (er, iworld) = read share iworld
   		= case er of
			Ok r = case projection val r of
				Just w
					# (ew, iworld) = write w share iworld
					= case ew of
						Ok _	= (result, iworld)
						Error e	= (ExceptionResult e, iworld)
				Nothing = (result, iworld)
			Error e = (ExceptionResult e, iworld)

step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | iTask a & iTask b
step (Task _ evala) lhsValFun conts = Task Nothing eval
where
	eval event repOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event repOpts (TCStep taskId ts (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval event repOpts (TCStep taskId ts (Left treea)) iworld=:{current={taskTime}}
		# (resa, iworld) 	= evala event repOpts treea iworld
        # mbAction          = matchAction taskId event
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbAction conts of
				Nothing			
					# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
                    # value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
					= Left (ValueResult value info (doStepLayout taskId repOpts rep val) (TCStep taskId info.TaskInfo.lastEvent (Left ntreea)) )
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskInfo.lastEvent)
			ExceptionResult e = case searchContException e conts of
				Nothing			= Left (ExceptionResult e)
				Just rewrite	= Right (rewrite,Nothing, ts)		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task _ evalb,d_json_a),mbTreeA, lastEvent)
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
					ExceptionResult e = (ExceptionResult e, iworld)
	//Eval right-hand side
	eval event repOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{current={taskTime}}
		= case restoreTaskB sel enca of
			Just (Task _ evalb)
				# (resb, iworld)	= evalb event repOpts treeb iworld
				= case resb of
					ValueResult val info rep ntreeb
						# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
						= (ValueResult val info (finalizeRep repOpts rep) (TCStep taskId info.TaskInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)
			Nothing
				= (ExceptionResult (exception "Corrupt task value in step"), iworld)	
	
	//Cleanup
	eval event repOpts (TCDestroy (TCStep taskId ts (Left treea))) iworld
		= case evala event repOpts (TCDestroy treea) iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e,iworld)	    = (ExceptionResult e,iworld)
			(ValueResult _ _ _ _,iworld)	= (ExceptionResult (exception "Destroy failed in step"),iworld)
	
	eval event repOpts (TCDestroy (TCStep taskId ts (Right(enca,sel,treeb)))) iworld
		= case restoreTaskB sel enca of
			Just (Task _ evalb)	= evalb event repOpts (TCDestroy treeb) iworld
			Nothing				= (ExceptionResult (exception "Corrupt task value in step"), iworld)
			
	//Incorrect state
	eval event _ state iworld
		= (ExceptionResult (exception ("Corrupt task state in step:" +++ (toString (toJSON state)))), iworld)

	restoreTaskB sel d_json_a = case conts !! sel of
		(OnValue taskbf)			= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnAction _ taskbf)			= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnException taskbf)		= call_with_DeferredJSON taskbf d_json_a
		(OnAllExceptions taskbf)	= call_with_DeferredJSON taskbf d_json_a
	
	doStepLayout taskId repOpts NoRep val
		= finalizeRep repOpts (TaskRep ((repLayoutRules repOpts).LayoutRules.accuStep {UIDef|content=UIEmpty {UIEmpty|actions=[]},windows=[]} (contActions taskId val conts)) [])
	doStepLayout taskId repOpts (TaskRep def parts) val
		= finalizeRep repOpts (TaskRep ((repLayoutRules repOpts).LayoutRules.accuStep def (contActions taskId val conts)) parts)

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

matchAction :: TaskId Event -> Maybe String
matchAction taskId (ActionEvent _ matchId action)
    | matchId == taskId     = Just action
                            = Nothing
matchAction taskId _        = Nothing

contActions :: TaskId (TaskValue a) [TaskCont a b]-> [UIAction]
contActions taskId val conts = [{UIAction|taskId=toString taskId,action=action,enabled=isJust (taskbf val)}\\ OnAction action taskbf <- conts]

searchContValue :: (TaskValue a) (Maybe String) [TaskCont a b] -> Maybe (!Int, !b, !DeferredJSON) | TC a & JSONEncode{|*|} a
searchContValue val mbAction conts = search val mbAction 0 Nothing conts
where
    search _ _ _ mbMatch []							= mbMatch		//No matching OnValue steps were found, return the potential match
	search val mbAction i mbMatch [OnValue f:cs]
	    = case f val of
			Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
			Nothing		= search val mbAction (i + 1) mbMatch cs	//Keep search
    search val mbAction=:(Just actionEvent) i Nothing [OnAction action f:cs]
	    | actionEvent == actionName action
		    = case f val of
                Just cont	= search val mbAction (i + 1) (Just (i, cont, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
                Nothing		= search val mbAction (i + 1) Nothing cs								//Keep searching
        | otherwise
                            = search val mbAction (i + 1) Nothing cs								//Keep searching														
    search val mbAction i mbMatch [_:cs]			= search val mbAction (i + 1) mbMatch cs		//Keep searching

searchContException :: (Dynamic,String) [TaskCont a b] -> Maybe (Int, !b, !DeferredJSON)
searchContException (dyn,str) conts = search dyn str 0 Nothing conts
where
    search _ _ _ catchall []					= catchall														//Return the maybe catchall
    search dyn str i catchall [OnException f:cs] = case (match f dyn) of
        Just (taskb,enca)						= Just (i, taskb, enca)											//We have a match
        _										= search dyn str (i + 1) catchall cs							//Keep searching
    search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
    search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs							//Keep searching
				
    match :: (e -> b) Dynamic -> Maybe (b, DeferredJSON) | iTask e
    match f (e :: e^)	= Just (f e, DeferredJSON e)
    match _ _			= Nothing

// Parallel composition
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | iTask a
parallel initTasks conts = Task Nothing eval
where
	//Create initial task list
	eval event repOpts (TCInit taskId ts) iworld=:{IWorld|current=current=:{localLists}}
		//Append the initial tasks to the list
		# iworld	= foldl append {iworld & current = {current & localLists = 'Data.Map'.put taskId [] localLists}} initTasks
		//Evaluate the parallel
		= eval event repOpts (TCParallel taskId ts) iworld
	where
		append iworld t = snd (addTaskToList taskId t Nothing iworld)

	//Evaluate the task list
	eval event repOpts (TCParallel taskId ts) iworld=:{current={taskTime}}
		//Evaluate all parallel tasks in the list
		= case evalParTasks taskId event repOpts conts iworld of
			(Left e,iworld)	    = (ExceptionResult e,iworld)
			(Right results,iworld=:{current=current=:{localLists}})
                # entries = [(e,r) \\ e <- (fromMaybe [] ('Data.Map'.get taskId localLists)) & (_,r) <- results]
                # actions = contActions taskId (Value (map fst results) False) conts
				//Filter out removed entries and destroy their state
				# (removed,entries)	= splitWith (\({TaskListEntry|removed},_) -> removed) entries
				= case foldl destroyParTask (Nothing,iworld) (map fst removed) of
					(Just (ExceptionResult e),iworld)	= (ExceptionResult e,iworld)	//An exception occurred	
					(Just result,iworld)				= (fixOverloading result initTasks (ExceptionResult (exception "Destroy failed in parallel")),iworld)
					(Nothing,iworld=:{current=current=:{localLists}})
						//Destruction is ok, build parallel result
						# rep				= parallelRep taskId repOpts entries actions
						# values			= map (toValueAndTime o fst) entries
						# stable			= all (isStable o snd) values
						# refreshSensitive	= foldr (\(e,_) s -> s || refreshSensitive e) False entries
						# ts				= foldr max 0 [ts:map fst values]
                        # involvedUsers     = foldr (\(e,_) i -> involvedUsers e ++ i) [] entries
						= (ValueResult (Value values stable) {TaskInfo|lastEvent=ts,involvedUsers=involvedUsers,refreshSensitive=refreshSensitive}
							(finalizeRep repOpts rep) (TCParallel taskId ts),{iworld & current = {current & localLists = 'Data.Map'.put taskId (map fst entries) localLists}})
	//Cleanup
	eval event repOpts (TCDestroy (TCParallel taskId ts)) iworld=:{current={localLists}}
		# entries = fromMaybe [] ('Data.Map'.get taskId localLists)
		= case foldl destroyParTask (Nothing,iworld) entries of
			(Nothing,iworld=:{current=current=:{localLists}}) //All destroyed
				= (DestroyedResult,{iworld & current = {current & localLists = 'Data.Map'.del taskId localLists}})
			(Just (ExceptionResult e),iworld=:{current=current=:{localLists}}) //An exception occurred
				= (ExceptionResult e,{iworld & current = {current & localLists = 'Data.Map'.del taskId localLists}})
			(Just result,iworld)
                = (fixOverloading result initTasks (ExceptionResult (exception "Destroy failed in step")),iworld)
	//Fallback
	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in parallel"), iworld)
	
	evalParTasks :: !TaskId !Event !TaskRepOpts [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] !*IWorld
                    -> (!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a), Maybe TaskRep)],!*IWorld) | iTask a
	evalParTasks taskId event repOpts conts iworld=:{current={localLists,eventRoute}}
		= evalFrom 0 [] (fromMaybe [] ('Data.Map'.get taskId localLists)) ('Data.Map'.get taskId eventRoute) (matchAction taskId event) repOpts iworld
	where
		evalFrom n acc list mbEventIndex mbAction repOpts iworld
            = case foldl (evalParTask taskId event mbEventIndex repOpts conts) (Right acc,iworld) [(i,e) \\ e <- drop n list & i <- [n..]] of
			(Left (e,str), iworld)	= (Left (e,str), iworld)
			(Right acc,iworld=:{current={localLists}})			
				# nlist = fromMaybe [] ('Data.Map'.get taskId localLists)
				# lenlist = length list
                //Check if extra branches were added -> evaluate these as well
				| length nlist > lenlist	= evalFrom lenlist acc nlist Nothing mbAction repOpts iworld	
                //Check if for matching continations -> add them and continue evaluation
                = case searchContValue (Value (map fst acc) False) mbAction conts of
                    Nothing     = (Right acc,iworld) //Done
                    Just (_,extension,_) //TODO: Add multiple matches at once, not just one?
                        # (_,iworld) = addTaskToList taskId extension Nothing iworld
                        = evalFrom lenlist acc nlist Nothing Nothing repOpts iworld	
	
	evalParTask :: !TaskId !Event !(Maybe Int) !TaskRepOpts [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] !(!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a),Maybe TaskRep)],!*IWorld) !(!Int,!TaskListEntry) -> (!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a), Maybe TaskRep)],!*IWorld) | iTask a
	//Evaluate embedded tasks
	evalParTask taskId event mbEventIndex repOpts conts (Right acc,iworld=:{current={taskTime,localTasks}}) (index,{TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult jsonval info rep tree,lastFocus,removed=False})
        //Update focus if there is a focus event
        # lastFocus = case event of
            (FocusEvent _ focusId)  = if (focusId == entryId) (Just taskTime) lastFocus
            _                       = lastFocus
		# evalNeeded = case mbEventIndex of
			Nothing	= True //We don't know the event index, so we just have to try
			Just eventIndex
				| eventIndex == index	= True								//The event is targeted at this branch, we evaluate
										= info.TaskInfo.refreshSensitive	//Also evaluate if the branch is refresh sensitive
		| evalNeeded
			//Evaluate the branch
			= case fmap unwrapTask ('Data.Map'.get entryId localTasks) of
                Just (Task _ evala)
					# (result,iworld) = evala event {TaskRepOpts|useLayout=Nothing,modLayout=Nothing,noUI=repOpts.noUI} tree iworld
					= case result of
						ExceptionResult e
                            //Check if we have an exception handler the continuations
                            = case searchContException e conts of
                                Nothing = (Left e,iworld) //No handler, unfortunately
                                Just (_,handler=:(_,parTask),_) //Replace tasklist entry and try again
                                    # (entry,iworld) = addTaskToList taskId handler (Just index) iworld
                                    = evalParTask taskId event mbEventIndex repOpts conts (Right acc,iworld) (index,entry)
						ValueResult val info rep tree
							# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId result lastFocus iworld
							= (Right (acc++[((info.TaskInfo.lastEvent,val),Just rep)]),iworld)
				_
					= (Right acc,iworld)	
		| otherwise
			# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId (ValueResult jsonval info rep tree) lastFocus iworld
			= (Right (acc++[((info.TaskInfo.lastEvent,fromJSONTaskValue jsonval),Just rep)]),iworld)
					
	//Copy the last stored result of detached tasks
	evalParTask taskId=:(TaskId curInstanceNo _) event mbEventIndex noUI conts (Right acc,iworld) (index,{TaskListEntry|entryId,state=DetachedState instanceNo _ _,removed=False})
		# (mbMeta,iworld)	= readRegister curInstanceNo (sdsFocus instanceNo taskInstanceMeta) iworld
		# (mbValue,iworld)	= readRegister curInstanceNo (taskInstanceValue instanceNo) iworld
		= case (mbMeta,mbValue) of
			(Ok meta,Ok value=:(TIValue jsonval))
				# (entry,iworld) = updateListEntryDetachedResult taskId entryId value meta.TIMeta.progress meta.TIMeta.attributes iworld
				= (Right (acc++[((entry.TaskListEntry.lastEvent,fromJSONTaskValue jsonval),Nothing)]),iworld)
            //TODO deal with detached exception case (we now possibly have an exception handler)
			_	
                = (Right acc,iworld)	//TODO: remove from parallel if it can't be loaded (now it simply keeps the last known result)

	//Do nothing if an exeption occurred or marked as removed
	evalParTask taskId event mbEventIndex noUI conts (result,iworld) (index,entry) = (result,iworld)

    fromJSONTaskValue NoValue = NoValue
    fromJSONTaskValue (Value j s) = maybe NoValue (\v -> Value v s) (fromJSON j)

	destroyParTask :: (!Maybe (TaskResult a),!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),!*IWorld) | iTask a
	//Destroy embedded tasks
	destroyParTask (_,iworld=:{current={localTasks}}) {TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult _ _ _ tree}
		= case 'Data.Map'.get entryId localTasks of
			Just (Task _ evala :: Task a^)
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
	
	parallelRep :: !TaskId !TaskRepOpts ![(!TaskListEntry,!Maybe TaskRep)] [UIAction] -> TaskRep
	parallelRep taskId repOpts entries actions
		# layout		= repLayoutRules repOpts
		# listId		= toString taskId
		# parts = [( uiDefSetAttribute LAST_EVENT_ATTRIBUTE (toString lastEvent)
                   o (maybe id (\f -> uiDefSetAttribute LAST_FOCUS_ATTRIBUTE (toString f)) lastFocus)
                   o uiDefSetAttribute CREATED_AT_ATTRIBUTE (toString createdAt)
                   o uiDefSetAttribute TASK_ATTRIBUTE (toString entryId)
                   ) def
				   \\ ({TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult val _ _ _,createdAt,lastEvent,lastFocus,removed=False},Just (TaskRep def _)) <- entries | not (isStable val)]	
		= TaskRep (layout.LayoutRules.accuParallel parts actions) []

	isStable (Value _ stable) 	= stable
	isStable _					= False

	refreshSensitive {TaskListEntry|lastEval=ValueResult _ {TaskInfo|refreshSensitive} _ _} = refreshSensitive
	refreshSensitive _ = True

	involvedUsers {TaskListEntry|lastEval=ValueResult _ {TaskInfo|involvedUsers} _ _} = involvedUsers
    involvedUsers _ = []	

	//Helper function to help type inferencing a little
	fixOverloading :: (TaskResult a) [(!ParallelTaskType,!ParallelTask a)] !b -> b
	fixOverloading _ _ x = x
						
//SHARED HELPER FUNCTIONS

addTaskToList :: !TaskId !(!ParallelTaskType,!ParallelTask a) !(Maybe Int) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
addTaskToList taskId (parType,parTask) mbPos iworld=:{current={taskTime,user,attachmentChain},clocks={localDate,localTime}}
	# (list,iworld) = loadTaskList taskId iworld
	# progress = {ProgressMeta|value=None, issuedAt=DateTime localDate localTime,issuedBy=user,involvedUsers=[],firstEvent=Nothing,latestEvent=Nothing}
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
	# lastEval	= ValueResult NoValue {TaskInfo|lastEvent=taskTime,involvedUsers=[],refreshSensitive=True} NoRep (TCInit taskIda taskTime)
	# entry		= {entryId = taskIda, name = name, state = state, lastEval = lastEval, uiAttributes = 'Data.Map'.newMap, createdAt = taskTime, lastFocus = Nothing, lastEvent = taskTime, removed = False}
    # list      = maybe (list++[entry]) (\pos -> updateAt pos entry list) mbPos
	# iworld	= storeTaskList taskId list iworld
	= (entry, iworld)	

updateListEntryEmbeddedResult :: !TaskId !TaskId (TaskResult a) (Maybe TaskTime) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
updateListEntryEmbeddedResult listId entryId result lastFocus iworld
	= updateListEntry listId entryId (\e=:{TaskListEntry|state,lastEvent} ->
		{TaskListEntry|e & lastEval= wrap result, uiAttributes = newAttr result, lastEvent = maxTime lastEvent result, lastFocus = lastFocus }) iworld
where
	wrap (ValueResult val info=:{TaskInfo|refreshSensitive=True} _ tree) //When we know for certain that we'll recompute the task on the next event, 
		= ValueResult (fmap toJSON val) info NoRep tree					 //don't bother storing the task representation
	wrap (ValueResult val info rep tree)	= ValueResult (fmap toJSON val) info rep tree
	wrap (ExceptionResult e)			    = ExceptionResult e

	newAttr (ValueResult _ _ (TaskRep def _) _)					= uiDefAttributes def
	newAttr _													= 'Data.Map'.newMap
	
	maxTime cur (ValueResult _ {TaskInfo|lastEvent} _ _)		= max cur lastEvent
	maxTime cur _												= cur

updateListEntryDetachedResult :: !TaskId !TaskId TIValue !ProgressMeta !TaskAttributes !*IWorld -> (!TaskListEntry,!*IWorld)
updateListEntryDetachedResult listId entryId lastValue progress attributes iworld
	= updateListEntry listId entryId update iworld
where
	update e=:{TaskListEntry|state=DetachedState no _ _}
        # lastEval = case lastValue of
            TIValue val = ValueResult val info NoRep TCNop
            TIException e str = ExceptionResult (e,str)
		= {TaskListEntry| e & state = DetachedState no progress attributes, lastEval = lastEval}
	update e = e

    info = {refreshSensitive=True,involvedUsers=[],lastEvent=0} //FIXME probably a bad idea to construct this nonsense info that may or may not be used

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
			
readListId :: (SharedTaskList a) *IWorld -> (MaybeError TaskException (TaskListId a),*IWorld) | iTask a
readListId slist iworld = case read slist iworld of
	(Ok l,iworld)		= (Ok l.TaskList.listId, iworld)
	(Error e, iworld)	= (Error e, iworld)

//Derived shares
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\{TaskList|items} -> [value \\ {TaskListItem|value} <- items]) (toReadOnly tasklist)

taskListMeta :: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,TaskAttributes)]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist

taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId
taskListSelfId tasklist = mapRead (\{TaskList|selfId} -> selfId) (toReadOnly tasklist)

taskListSelfManagement :: !(SharedTaskList a) -> Shared TaskAttributes
taskListSelfManagement tasklist = mapReadWriteError (toPrj,fromPrj) tasklist
where
    toPrj {TaskList|selfId,items} = case [m \\ m=:{TaskListItem|taskId} <- items | taskId == selfId] of
        []                              = Error (exception "Task id not found in self management share")
        [{TaskListItem|attributes}:_]   = Ok attributes

    fromPrj attributes {TaskList|selfId}
        = Ok (Just [(selfId,attributes)])

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# (taskIda,iworld) = append listId parType parTask iworld
				= (Ok taskIda, iworld)
			(Error e,iworld)
				= (Error e, iworld)
								
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
	    # ({TaskListEntry|entryId},iworld) = addTaskToList parId (parType,parTask) Nothing iworld
        = (entryId,iworld)

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
				= (Error e, iworld)

	remove :: !(TaskListId a) !TaskId !*IWorld -> *IWorld
	remove TopLevelTaskList (TaskId instanceNo 0) iworld
		= deleteInstance instanceNo iworld
	remove (ParallelTaskList parId) entryId iworld
		= markListEntryRemoved parId entryId iworld
	remove _ _ iworld = iworld

focusTask :: !TaskId !(SharedTaskList a) -> Task Void | iTask a
focusTask entryId slist = mkInstantTask eval
where
    eval taskId iworld=:{IWorld|current={taskTime}}
        = case readListId slist iworld of
            (Ok (ParallelTaskList listId),iworld)
	            # (_,iworld) = updateListEntry listId entryId (\e -> {TaskListEntry|e & lastFocus = Just taskTime}) iworld
                = (Ok Void, iworld)
            (Ok _,iworld)
                = (Ok Void, iworld)
			(Error e,iworld)
				= (Error e, iworld)

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId instanceNo taskNo) = Task Nothing eval
where
	eval event repOpts (TCInit taskId ts) iworld=:{current={attachmentChain,user}}
		# (meta,iworld)		= read (sdsFocus instanceNo taskInstanceMeta) iworld
		= case meta of
			Ok meta
                //Just steal the instance, TODO, make stealing optional
				# (_,iworld)	= write {TIMeta|meta & instanceType=AttachedInstance [taskId:attachmentChain] user} (sdsFocus instanceNo taskInstanceMeta) iworld
				# iworld		= queueUrgentRefresh [instanceNo] iworld
				= eval event repOpts (TCBasic taskId ts JSONNull False) iworld
			Error e
				= (ExceptionResult e,iworld)
		
	eval event repOpts tree=:(TCBasic taskId ts _ _) iworld=:{current={taskInstance,user}}
		//Load instance
		# layout			= repLayoutRules repOpts
		# (meta,iworld)		= readRegister taskInstance (sdsFocus instanceNo taskInstanceMeta) iworld
		= case meta of
			(Ok meta=:{TIMeta|progress,instanceType=AttachedInstance _ worker,instanceKey})
                | progress.ProgressMeta.value === Exception
				    = (ValueResult (Value WOExcepted True) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)
                | progress.ProgressMeta.value === Stable
				    = (ValueResult (Value WOFinished True) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)
				| worker == user
                    # rep = finalizeRep repOpts (TaskRep (layout.LayoutRules.accuWorkOn (embedTaskDef instanceNo instanceKey) meta) [])
					= (ValueResult (Value WOActive False) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} rep tree, iworld)
				| otherwise
					# rep = finalizeRep repOpts (TaskRep (layout.LayoutRules.accuWorkOn (inUseDef worker) meta) [])
					= (ValueResult (Value (WOInUse worker) False) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} rep tree, iworld)		
			_
				= (ValueResult (Value WODeleted True) {TaskInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep repOpts NoRep) tree, iworld)

	eval event repOpts (TCDestroy (TCBasic taskId _ _ _)) iworld
        /*
        # (meta,iworld) = read fullInstanceMeta iworld //FIXME: Figure out how to get the right share notifications for the released instances
        = case meta of
            Ok instances
                # (_,iworld) = write (map (release taskId) instances) fullInstanceMeta iworld
                = (DestroyedResult,iworld)
		    _   = (DestroyedResult,iworld)
        */
        = (DestroyedResult,iworld)

    release taskId meta=:{TIMeta|instanceType=AttachedInstance attachment worker}
        | isMember taskId attachment    = {TIMeta|meta & instanceType = DetachedInstance}
                                        = meta
    release taskId meta = meta

    embedTaskDef instanceNo instanceKey
		= {UIDef|content=UIForm {UIForm|attributes='Data.Map'.newMap,controls=[(UIEmbedding embedSize {UIEmbeddingOpts|instanceNo=instanceNo,instanceKey=instanceKey},'Data.Map'.newMap)],size=embedSize},windows=[]}

    embedSize = {UISizeOpts|defaultSizeOpts & width= Just FlexSize, height=Just FlexSize}

	inUseDef worker
		= {UIDef|content=UIForm {UIForm|attributes='Data.Map'.newMap,controls=[(stringDisplay (toString worker +++ " is working on this task"),'Data.Map'.newMap)],size=defaultSizeOpts},windows=[]}


/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs asUser (Task mdi eval) = Task mdi eval`
where
	eval` event repOpts state iworld=:{current=current=:{user}}
		# (result,iworld=:{current}) = eval event repOpts state {iworld & current = {current & user = asUser}}
		= (addInvolvedUser asUser result,{iworld & current = {current & user = user}})

    addInvolvedUser asUser (ValueResult val info=:{TaskInfo|involvedUsers} rep tree)
        = ValueResult val {TaskInfo|info & involvedUsers= [asUser:involvedUsers]} rep tree
    addInvolvedUser user res = res

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task Nothing eval
where	
	eval event repOpts (TCInit taskId ts) iworld
		# (taskIda,iworld=:{current=current=:{localShares}})
                                    = getNextTaskId iworld
		# localShares				= 'Data.Map'.put taskId (toJSON initial) localShares
		= eval event repOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & current = {current & localShares = localShares}}
		
	eval event repOpts (TCShared taskId ts treea) iworld=:{current={taskTime}}
		# (Task _ evala)			= stask (localShare taskId)
		# (resa,iworld)				= evala event repOpts treea iworld
		= case resa of
			ValueResult NoValue info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult NoValue info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ValueResult (Value stable val) info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult (Value stable val) info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ExceptionResult e   = (ExceptionResult e,iworld)
	
	eval event repOpts (TCDestroy (TCShared taskId ts treea)) iworld //First destroy inner task, then remove shared state
		# (Task _ evala) = stask (localShare taskId)
		# (resa,iworld=:{current=current=:{localShares}})
            = evala event repOpts (TCDestroy treea) iworld
		= (resa,{iworld & current = {current & localShares = 'Data.Map'.del taskId localShares}})
	
	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in withShared"), iworld)

import StdDebug

exposeShared :: !(RWShared p r w) !(String (RWShared p r w) -> Task a) -> Task a | iTask a & iTask r & iTask w & JSONDecode{|*|} p & JSONEncode{|*|} p & TC p
exposeShared shared stask = Task Nothing eval
where	
	eval event repOpts (TCInit taskId ts) iworld=:{exposedShares}
		# (url, iworld)		= newURL iworld
		// Trick to make it work until John fixes the compiler
		# exposedShares 	= 'Data.Map'.put url (dynamic shared :: RWShared p^ r^ w^, toJSONShared shared) exposedShares
		# (taskIda,iworld)	= trace_n ("SDS is exposed as "+++url) (getNextTaskId iworld)
		= eval event repOpts (TCExposedShared taskId ts url (TCInit taskIda ts)) {iworld & exposedShares = exposedShares}
		
	eval event repOpts (TCExposedShared taskId ts url treea) iworld=:{current={taskTime}}
		# exposedSDS				= exposedShare url
		# (Task _ evala)			= stask url exposedSDS
		# (resa,iworld)				= evala event repOpts treea iworld
		= case resa of
			ValueResult value info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult value info rep (TCExposedShared taskId info.TaskInfo.lastEvent url ntreea),iworld)
			ExceptionResult e
				= (ExceptionResult e,iworld)
	
	eval event repOpts (TCDestroy (TCExposedShared taskId ts url treea)) iworld //First destroy inner task, then remove shared state
		# (Task _ evala)				= stask url (exposedShare url)
		# (resa,iworld)					= evala event repOpts (TCDestroy treea) iworld
		= (resa,{iworld & exposedShares = 'Data.Map'.del url iworld.exposedShares})
	
	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in exposeShared"), iworld)
		
/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

instance tune SetLayout
where
	tune (SetLayout layout) (Task mdi eval)	= Task mdi eval`
	where
		eval` event repOpts=:{useLayout=Nothing,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just ((fromMaybe id modLayout) layout), modLayout = Nothing} state iworld 
		eval` event repOpts=:{useLayout=Just _,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just layout, modLayout = Nothing} state iworld 
	
instance tune AfterLayout
where
	tune (AfterLayout f) (Task mdi eval) = Task mdi eval`
	where
		eval` event repOpts state iworld = case eval event repOpts state iworld of
	        (ValueResult value info rep tree,iworld) = (ValueResult value info (updRep rep) tree, iworld)
            (res,iworld) = (res,iworld)

        updRep NoRep               = TaskRep (f {UIDef|content=UIEmpty {UIEmpty|actions=[]},windows=[]}) []
        updRep (TaskRep def parts) = TaskRep (f def) parts
		
instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task mdi eval)	= Task mdi eval`
	where
		eval` event repOpts=:{modLayout=Nothing} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just f} state iworld 
		eval` event repOpts=:{modLayout=Just g} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just (g o f)} state iworld 	

instance tune LazyRefresh
where
	tune _ (Task mdi eval) = Task mdi eval`
	where
		eval` event repOpts state iworld
			= case (eval event repOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)

instance tune TaskDefInfo
where
  tune tdi (Task _ eval) = Task (Just tdi) eval
