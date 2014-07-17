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
from iTasks.Framework.SDS               import write, read, readRegister
from iTasks.API.Core.Tasks	            import return
from iTasks.API.Common.SDSCombinators   import sdsFocus, sdsSplit, toReadOnly, mapRead, mapReadWriteError, mapSingle

derive class iTask ParallelTaskType, WorkOnStatus

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f (Task evala) = Task eval
where
	eval event evalOpts tree iworld = case evala event evalOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e, iworld)				    = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a
project projection share (Task evala) = Task eval
where
	eval event evalOpts (TCDestroy (TCProject taskId encprev treea)) iworld	//Cleanup duty simply passed to inner task
		= evala event evalOpts (TCDestroy treea) iworld

	eval event evalOpts state iworld
		# (taskId,prev,statea) = case state of
			(TCInit taskId _)					= (taskId,NoValue,state)
			(TCProject taskId encprev statea)	= (taskId,fromJust (fromJSON encprev),statea)
			
		# (resa, iworld) 	= evala event evalOpts statea iworld
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
step (Task evala) lhsValFun conts = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event evalOpts (TCStep taskId ts (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval event evalOpts (TCStep taskId ts (Left treea)) iworld=:{current={taskTime}}
		# (resa, iworld) 	= evala event (extendCallTrace taskId evalOpts) treea iworld
        # mbAction          = matchAction taskId event
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbAction conts of
				Nothing			
					# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
                    # value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
					= Left (ValueResult value info (doStepLayout taskId evalOpts rep val) (TCStep taskId info.TaskEvalInfo.lastEvent (Left ntreea)) )
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskEvalInfo.lastEvent)
			ExceptionResult e = case searchContException e conts of
				Nothing			= Left (ExceptionResult e)
				Just rewrite	= Right (rewrite,Nothing, ts)		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala (toRefresh event) (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb (toRefresh event) (extendCallTrace taskId evalOpts) (TCInit taskIdb lastEvent) iworld
				= case resb of
					ValueResult val info rep nstateb	
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
						= (ValueResult val info (finalizeRep evalOpts rep) (TCStep taskId info.TaskEvalInfo.lastEvent (Right (d_json_a,sel,nstateb))),iworld)
					ExceptionResult e = (ExceptionResult e, iworld)
	//Eval right-hand side
	eval event evalOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{current={taskTime}}
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb event (extendCallTrace taskId evalOpts) treeb iworld
				= case resb of
					ValueResult val info rep ntreeb
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
						= (ValueResult val info (finalizeRep evalOpts rep) (TCStep taskId info.TaskEvalInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)
			Nothing
				= (ExceptionResult (exception "Corrupt task value in step"), iworld)	
	
	//Cleanup
	eval event evalOpts (TCDestroy (TCStep taskId ts (Left treea))) iworld
		= case evala event (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e,iworld)	    = (ExceptionResult e,iworld)
			(ValueResult _ _ _ _,iworld)	= (ExceptionResult (exception "Destroy failed in step"),iworld)
	
	eval event evalOpts (TCDestroy (TCStep taskId ts (Right(enca,sel,treeb)))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)	= evalb event (extendCallTrace taskId evalOpts) (TCDestroy treeb) iworld
			Nothing				= (ExceptionResult (exception "Corrupt task value in step"), iworld)
			
	//Incorrect state
	eval event _ state iworld
		= (ExceptionResult (exception ("Corrupt task state in step:" +++ (toString (toJSON state)))), iworld)

	restoreTaskB sel d_json_a = case conts !! sel of
		(OnValue taskbf)			= callWithDeferredJSONTaskValue taskbf d_json_a
		(OnAction _ taskbf)			= callWithDeferredJSONTaskValue taskbf d_json_a
		(OnException taskbf)		= callWithDeferredJSON taskbf d_json_a
		(OnAllExceptions taskbf)	= callWithDeferredJSON taskbf d_json_a
	
	doStepLayout taskId evalOpts NoRep val
		= finalizeRep evalOpts (TaskRep ((repLayoutRules evalOpts).LayoutRules.accuStep {UIDef|content=UIEmpty {UIEmpty|actions=[]},windows=[]} (contActions taskId val conts)) [])
	doStepLayout taskId evalOpts (TaskRep def parts) val
		= finalizeRep evalOpts (TaskRep ((repLayoutRules evalOpts).LayoutRules.accuStep def (contActions taskId val conts)) parts)

	callWithDeferredJSONTaskValue :: ((TaskValue a) -> (Maybe (Task .b))) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
	callWithDeferredJSONTaskValue f_tva_tb d_json_tva=:(DeferredJSON tva)
        = f_tva_tb (cast_to_TaskValue tva)
	
	callWithDeferredJSONTaskValue f_tva_tb (DeferredJSONNode json)
		= case fromJSON json of
			Just a ->  f_tva_tb a
			Nothing -> Nothing
	
	callWithDeferredJSON :: (a -> Task .b) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
    callWithDeferredJSON f_tva_tb d_json_tva=:(DeferredJSON tva)
        = Just (f_tva_tb (cast tva))

	callWithDeferredJSON f_tva_tb (DeferredJSONNode json)
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
parallel initTasks conts = Task eval
where
	//Create initial task list
	eval event evalOpts (TCInit taskId ts) iworld=:{IWorld|current=current=:{localLists}}
		//Append the initial tasks to the list
		# iworld	= foldl append {iworld & current = {current & localLists = 'Data.Map'.put taskId [] localLists}} initTasks
		//Evaluate the parallel
		= eval event evalOpts (TCParallel taskId ts) iworld
	where
		append iworld t = snd (addTaskToList taskId t Nothing iworld)

	//Evaluate the task list
	eval event evalOpts (TCParallel taskId ts) iworld=:{current={taskTime}}
		//Evaluate all parallel tasks in the list
		= case evalParTasks taskId event evalOpts conts iworld of
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
						# rep				= parallelRep taskId evalOpts entries actions
						# values			= map (toValueAndTime o fst) entries
						# stable			= all (isStable o snd) values
						# refreshSensitive	= foldr (\(e,_) s -> s || refreshSensitive e) False entries
						# ts				= foldr max 0 [ts:map fst values]
                        # involvedUsers     = foldr (\(e,_) i -> involvedUsers e ++ i) [] entries
						= (ValueResult (Value values stable) {TaskEvalInfo|lastEvent=ts,involvedUsers=involvedUsers,refreshSensitive=refreshSensitive}
							(finalizeRep evalOpts rep) (TCParallel taskId ts),{iworld & current = {current & localLists = 'Data.Map'.put taskId (map fst entries) localLists}})
	//Cleanup
	eval event evalOpts (TCDestroy (TCParallel taskId ts)) iworld=:{current={localLists}}
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
	
	evalParTasks :: !TaskId !Event !TaskEvalOpts [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] !*IWorld
                    -> (!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a), Maybe TaskRep)],!*IWorld) | iTask a
	evalParTasks taskId event evalOpts conts iworld=:{current={localLists,eventRoute}}
		= evalFrom 0 [] (fromMaybe [] ('Data.Map'.get taskId localLists)) ('Data.Map'.get taskId eventRoute) (matchAction taskId event) evalOpts iworld
	where
		evalFrom n acc list mbEventIndex mbAction evalOpts iworld
            = case foldl (evalParTask taskId event mbEventIndex evalOpts conts) (Right acc,iworld) [(i,e) \\ e <- drop n list & i <- [n..]] of
			(Left (e,str), iworld)	= (Left (e,str), iworld)
			(Right acc,iworld=:{current={localLists}})			
				# nlist = fromMaybe [] ('Data.Map'.get taskId localLists)
				# lenlist = length list
                //Check if extra branches were added -> evaluate these as well
				| length nlist > lenlist	= evalFrom lenlist acc nlist Nothing mbAction evalOpts iworld	
                //Check if for matching continations -> add them and continue evaluation
                = case searchContValue (Value (map fst acc) False) mbAction conts of
                    Nothing     = (Right acc,iworld) //Done
                    Just (_,extension,_) //TODO: Add multiple matches at once, not just one?
                        # (_,iworld) = addTaskToList taskId extension Nothing iworld
                        = evalFrom lenlist acc nlist Nothing Nothing evalOpts iworld	
	
	evalParTask :: !TaskId !Event !(Maybe Int) !TaskEvalOpts [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] !(!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a),Maybe TaskRep)],!*IWorld) !(!Int,!TaskListEntry) -> (!Either (!Dynamic,!String) [((!TaskTime,!TaskValue a), Maybe TaskRep)],!*IWorld) | iTask a
	//Evaluate embedded tasks
	evalParTask taskId event mbEventIndex evalOpts conts (Right acc,iworld=:{current={taskTime,localTasks}}) (index,{TaskListEntry|entryId,state=EmbeddedState,lastEval=ValueResult jsonval info rep tree,lastFocus,removed=False})
        //Update focus if there is a focus event
        # lastFocus = case event of
            (FocusEvent _ focusId)  = if (focusId == entryId) (Just taskTime) lastFocus
            _                       = lastFocus
		# evalNeeded = case mbEventIndex of
			Nothing	= True //We don't know the event index, so we just have to try
			Just eventIndex
				| eventIndex == index	= True								//The event is targeted at this branch, we evaluate
										= info.TaskEvalInfo.refreshSensitive	//Also evaluate if the branch is refresh sensitive
		| evalNeeded
			//Evaluate the branch
			= case fmap unwrapTask ('Data.Map'.get entryId localTasks) of
                Just (Task evala)
					# (result,iworld) = evala event (extendCallTrace taskId {TaskEvalOpts|evalOpts & useLayout=Nothing,modLayout=Nothing}) tree iworld
					= case result of
						ExceptionResult e
                            //Check if we have an exception handler the continuations
                            = case searchContException e conts of
                                Nothing = (Left e,iworld) //No handler, unfortunately
                                Just (_,handler=:(_,parTask),_) //Replace tasklist entry and try again
                                    # (entry,iworld) = addTaskToList taskId handler (Just index) iworld
                                    = evalParTask taskId event mbEventIndex evalOpts conts (Right acc,iworld) (index,entry)
						ValueResult val info rep tree
							# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId result lastFocus iworld
							= (Right (acc++[((info.TaskEvalInfo.lastEvent,val),Just rep)]),iworld)
				_
					= (Right acc,iworld)	
		| otherwise
			# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId (ValueResult jsonval info rep tree) lastFocus iworld
			= (Right (acc++[((info.TaskEvalInfo.lastEvent,fromJSONTaskValue jsonval),Just rep)]),iworld)
					
	//Copy the last stored result of detached tasks
	evalParTask taskId=:(TaskId curInstanceNo _) event mbEventIndex noUI conts (Right acc,iworld) (index,{TaskListEntry|entryId,state=DetachedState instanceNo _ _,removed=False})
		# (mbMeta,iworld)	= readRegister taskId (sdsFocus instanceNo taskInstanceMeta) iworld
		# (mbValue,iworld)	= readRegister taskId (taskInstanceValue instanceNo) iworld
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
			Just (Task evala :: Task a^)
				# (result,iworld=:{current=current=:{localTasks}}) = evala (RefreshEvent Nothing) {TaskEvalOpts|useLayout=Nothing,modLayout=Nothing,noUI=True,callTrace=[]} (TCDestroy tree) iworld
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
	
	parallelRep :: !TaskId !TaskEvalOpts ![(!TaskListEntry,!Maybe TaskRep)] [UIAction] -> TaskRep
	parallelRep taskId evalOpts entries actions
		# layout		= repLayoutRules evalOpts
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

	refreshSensitive {TaskListEntry|lastEval=ValueResult _ {TaskEvalInfo|refreshSensitive} _ _} = refreshSensitive
	refreshSensitive _ = True

	involvedUsers {TaskListEntry|lastEval=ValueResult _ {TaskEvalInfo|involvedUsers} _ _} = involvedUsers
    involvedUsers _ = []	

	//Helper function to help type inferencing a little
	fixOverloading :: (TaskResult a) [(!ParallelTaskType,!ParallelTask a)] !b -> b
	fixOverloading _ _ x = x
						
//SHARED HELPER FUNCTIONS

addTaskToList :: !TaskId !(!ParallelTaskType,!ParallelTask a) !(Maybe Int) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
addTaskToList taskId (parType,parTask) mbPos iworld=:{current={taskTime,user,attachmentChain},clocks={localDate,localTime}}
	# (list,iworld) = loadTaskList taskId iworld
	# progress = {ProgressMeta|value=None, issuedAt=DateTime localDate localTime,issuedBy=user,involvedUsers=[],attachedTo=Nothing,firstEvent=Nothing,lastEvent=Nothing,connectedTo=Nothing,lastIO=Nothing}
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
			# (taskIda,iworld)	                    = createDetachedTaskInstance task (Just instanceNo) management user taskId evalDirect iworld
			= (taskIda,Nothing,DetachedState instanceNo progress management, iworld)
	    NamedDetached name management evalDirect
            # (instanceNo,iworld)                   = newInstanceNo iworld
			# task									= parTask (parListShare taskId (TaskId instanceNo 0))
			# (taskIda,iworld)	                    = createDetachedTaskInstance task (Just instanceNo) ('Data.Map'.put "name" name management) user taskId evalDirect iworld
			= (taskIda,Just name,DetachedState instanceNo progress management, iworld)
	# lastEval	= ValueResult NoValue {TaskEvalInfo|lastEvent=taskTime,involvedUsers=[],refreshSensitive=True} NoRep (TCInit taskIda taskTime)
	# entry		= {entryId = taskIda, name = name, state = state, lastEval = lastEval, uiAttributes = 'Data.Map'.newMap, createdAt = taskTime, lastFocus = Nothing, lastEvent = taskTime, removed = False}
    # list      = maybe (list++[entry]) (\pos -> updateAt pos entry list) mbPos
	# iworld	= storeTaskList taskId list iworld
	= (entry, iworld)	

updateListEntryEmbeddedResult :: !TaskId !TaskId (TaskResult a) (Maybe TaskTime) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
updateListEntryEmbeddedResult listId entryId result lastFocus iworld
	= updateListEntry listId entryId (\e=:{TaskListEntry|state,lastEvent} ->
		{TaskListEntry|e & lastEval= wrap result, uiAttributes = newAttr result, lastEvent = maxTime lastEvent result, lastFocus = lastFocus }) iworld
where
	wrap (ValueResult val info=:{TaskEvalInfo|refreshSensitive=True} _ tree) //When we know for certain that we'll recompute the task on the next event, 
		= ValueResult (fmap toJSON val) info NoRep tree					 //don't bother storing the task representation
	wrap (ValueResult val info rep tree)	= ValueResult (fmap toJSON val) info rep tree
	wrap (ExceptionResult e)			    = ExceptionResult e

	newAttr (ValueResult _ _ (TaskRep def _) _)					= uiDefAttributes def
	newAttr _													= 'Data.Map'.newMap
	
	maxTime cur (ValueResult _ {TaskEvalInfo|lastEvent} _ _)		= max cur lastEvent
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

taskListEntryMeta :: !(SharedTaskList a) -> RWShared TaskId (TaskListItem a) TaskAttributes
taskListEntryMeta tasklist = mapSingle (sdsSplit "taskListEntryMeta" param read write tasklist)
where
    param p = (Void,p)
    read p {TaskList|items} = [i \\ i=:{TaskListItem|taskId} <- items | taskId == p]
    write p _ attributes    = ([(p,a) \\ a <- attributes], (==) p)

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
		# (attributes,evalDirect) = case parType of
            (Embedded)                              = (defaultValue,False)
            (NamedEmbedded name)                    = ('Data.Map'.put "name" name defaultValue,False)
            (Detached meta evalDirect)              = (meta,evalDirect)
            (NamedDetached name meta evalDirect)    = ('Data.Map'.put "name" name meta,evalDirect)
		# task						= parTask topListShare
		= createDetachedTaskInstance task Nothing attributes user (TaskId 0 0) evalDirect iworld
	append (ParallelTaskList parId) parType parTask iworld
	    # ({TaskListEntry|entryId},iworld) = addTaskToList parId (parType,parTask) Nothing iworld
        = (entryId,iworld)

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task () | iTask a
removeTask entryId slist = mkInstantTask eval
where
	eval taskId iworld
		= case readListId slist iworld of
			(Ok listId,iworld)
				# iworld = remove listId entryId iworld
				= (Ok (), iworld)
			(Error e,iworld)
				= (Error e, iworld)

	remove :: !(TaskListId a) !TaskId !*IWorld -> *IWorld
	remove TopLevelTaskList (TaskId instanceNo 0) iworld
		= deleteInstance instanceNo iworld
	remove (ParallelTaskList parId) entryId iworld
		= markListEntryRemoved parId entryId iworld
	remove _ _ iworld = iworld

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask entryId=:(TaskId entryInstanceNo _) parTask slist = mkInstantTask eval
where
    eval taskId iworld=:{IWorld|current={taskTime}}
        = case readListId slist iworld of
            (Ok TopLevelTaskList,iworld)
                = case replaceTaskInstance entryInstanceNo (parTask topListShare) iworld of
                    (Ok (), iworld) = (Ok (), iworld)
                    (Error e, iworld) = (Error (exception e), iworld)
			(Ok (ParallelTaskList listId),iworld)
                //Reset state
	            # (_,iworld) = updateListEntry listId entryId (resetState entryId taskTime) iworld
                //Update local task function
                # iworld = updateTaskFun listId entryId parTask iworld
				= (Ok (), iworld)
			(Error e,iworld)
				= (Error e, iworld)

    resetState entryId taskTime e =
       {TaskListEntry|e & lastEval = ValueResult NoValue {TaskEvalInfo|lastEvent=taskTime,involvedUsers=[],refreshSensitive=True} NoRep (TCInit entryId taskTime)}

    updateTaskFun listId entryId task iworld=:{current=current=:{localTasks}}
        # task		= parTask (parListShare listId entryId)
        = {iworld & current = {current & localTasks = 'Data.Map'.put entryId (dynamic task :: Task a^) localTasks}}

focusTask :: !TaskId !(SharedTaskList a) -> Task () | iTask a
focusTask entryId slist = mkInstantTask eval
where
    eval taskId iworld=:{IWorld|current={taskTime}}
        = case readListId slist iworld of
            (Ok (ParallelTaskList listId),iworld)
	            # (_,iworld) = updateListEntry listId entryId (\e -> {TaskListEntry|e & lastFocus = Just taskTime}) iworld
                = (Ok (), iworld)
            (Ok _,iworld)
                = (Ok (), iworld)
			(Error e,iworld)
				= (Error e, iworld)

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId instanceNo taskNo) = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld=:{current={attachmentChain,user}}
		# (meta,iworld)		= read (sdsFocus instanceNo taskInstanceMeta) iworld
		= case meta of
			Ok meta
                //Just steal the instance, TODO, make stealing optional
                # progress      = {ProgressMeta|meta.TIMeta.progress & attachedTo = Just (user,[taskId:attachmentChain])}
				# (_,iworld)	= write {TIMeta|meta & progress = progress} (sdsFocus instanceNo taskInstanceMeta) iworld
				# iworld		= queueUrgentRefresh [instanceNo] iworld
				= eval event evalOpts (TCBasic taskId ts JSONNull False) iworld
			Error e
				= (ExceptionResult e,iworld)
		
	eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{server={buildID},current={taskInstance,user}}
		//Load instance
		# layout			= repLayoutRules evalOpts
		# (meta,iworld)		= readRegister taskId (sdsFocus instanceNo taskInstanceMeta) iworld
		= case meta of
			(Ok meta=:{TIMeta|progress=progress=:{ProgressMeta|attachedTo=Just (worker,_),value},instanceKey,build})
                | build <> buildID //Check version
				    = (ValueResult (Value WOIncompatible True) {TaskEvalInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)
                | value === Exception
				    = (ValueResult (Value WOExcepted True) {TaskEvalInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)
				| worker == user
                    # rep       = finalizeRep evalOpts (TaskRep (layout.LayoutRules.accuWorkOn (embedTaskDef instanceNo instanceKey) meta) [])
                    # stable    = value === Stable
					= (ValueResult (Value (WOAttached stable) stable) {TaskEvalInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=True} rep tree, iworld)
				| otherwise
					# rep = finalizeRep evalOpts (TaskRep (layout.LayoutRules.accuWorkOn (inUseDef worker) meta) [])
					= (ValueResult (Value (WOInUse worker) False) {TaskEvalInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} rep tree, iworld)		
			_
				= (ValueResult (Value WODeleted True) {TaskEvalInfo|lastEvent=ts,involvedUsers=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)

	eval event evalOpts (TCDestroy (TCBasic taskId _ _ _)) iworld
        /*
        # (meta,iworld) = read fullInstanceMeta iworld //FIXME: Figure out how to get the right share notifications for the released instances
        = case meta of
            Ok instances
                # (_,iworld) = write (map (release taskId) instances) fullInstanceMeta iworld
                = (DestroyedResult,iworld)
		    _   = (DestroyedResult,iworld)
        */
        = (DestroyedResult,iworld)

    release taskId meta=:{TIMeta|progress=progress=:{ProgressMeta|attachedTo=Just (worker,attachment)}}
        | isMember taskId attachment    = {TIMeta|meta & progress = {ProgressMeta|meta.TIMeta.progress & attachedTo = Nothing}}
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
workAs asUser (Task eval) = Task eval`
where
	eval` event evalOpts state iworld=:{current=current=:{user}}
		# (result,iworld=:{current}) = eval event evalOpts state {iworld & current = {current & user = asUser}}
		= (addInvolvedUser asUser result,{iworld & current = {current & user = user}})

    addInvolvedUser asUser (ValueResult val info=:{TaskEvalInfo|involvedUsers} rep tree)
        = ValueResult val {TaskEvalInfo|info & involvedUsers= [asUser:involvedUsers]} rep tree
    addInvolvedUser user res = res

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task eval
where	
	eval event evalOpts (TCInit taskId ts) iworld
		# (taskIda,iworld=:{current=current=:{localShares}})
                                    = getNextTaskId iworld
		# localShares				= 'Data.Map'.put taskId (toJSON initial) localShares
		= eval event evalOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & current = {current & localShares = localShares}}
		
	eval event evalOpts (TCShared taskId ts treea) iworld=:{current={taskTime}}
		# (Task evala)			= stask (localShare taskId)
		# (resa,iworld)				= evala event (extendCallTrace taskId evalOpts) treea iworld
		= case resa of
			ValueResult NoValue info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult NoValue info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),iworld)
			ValueResult (Value stable val) info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult (Value stable val) info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),iworld)
			ExceptionResult e   = (ExceptionResult e,iworld)
	
	eval event evalOpts (TCDestroy (TCShared taskId ts treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala) = stask (localShare taskId)
		# (resa,iworld=:{current=current=:{localShares}})
            = evala event (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld
		= (resa,{iworld & current = {current & localShares = 'Data.Map'.del taskId localShares}})
	
	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in withShared"), iworld)

import StdDebug

exposeShared :: !(RWShared p r w) !(String (RWShared p r w) -> Task a) -> Task a | iTask a & iTask r & iTask w & JSONDecode{|*|} p & JSONEncode{|*|} p & TC p
exposeShared shared stask = Task eval
where	
	eval event evalOpts (TCInit taskId ts) iworld=:{exposedShares}
		# (url, iworld)		= newURL iworld
		// Trick to make it work until John fixes the compiler
		# exposedShares 	= 'Data.Map'.put url (dynamic shared :: RWShared p^ r^ w^, toJSONShared shared) exposedShares
		# (taskIda,iworld)	= trace_n ("SDS is exposed as "+++url) (getNextTaskId iworld)
		= eval event evalOpts (TCExposedShared taskId ts url (TCInit taskIda ts)) {iworld & exposedShares = exposedShares}
		
	eval event evalOpts (TCExposedShared taskId ts url treea) iworld=:{current={taskTime}}
		# exposedSDS				= exposedShare url
		# (Task evala)			= stask url exposedSDS
		# (resa,iworld)				= evala event (extendCallTrace taskId evalOpts) treea iworld
		= case resa of
			ValueResult value info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult value info rep (TCExposedShared taskId info.TaskEvalInfo.lastEvent url ntreea),iworld)
			ExceptionResult e
				= (ExceptionResult e,iworld)
	
	eval event evalOpts (TCDestroy (TCExposedShared taskId ts url treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala)				= stask url (exposedShare url)
		# (resa,iworld)					= evala event (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld
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
	tune (SetLayout layout) (Task eval)	= Task eval`
	where
		eval` event evalOpts=:{useLayout=Nothing,modLayout,callTrace} state iworld
			= eval event {TaskEvalOpts|evalOpts & useLayout = Just ((fromMaybe id modLayout) layout), modLayout = Nothing,callTrace=callTrace} state iworld
		eval` event evalOpts=:{useLayout=Just _,modLayout,callTrace} state iworld
			= eval event {TaskEvalOpts|evalOpts & useLayout = Just layout, modLayout = Nothing,callTrace=callTrace} state iworld
	
instance tune AfterLayout
where
	tune (AfterLayout f) (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld = case eval event evalOpts state iworld of
	        (ValueResult value info rep tree,iworld) = (ValueResult value info (updRep rep) tree, iworld)
            (res,iworld) = (res,iworld)

        updRep NoRep               = TaskRep (f {UIDef|content=UIEmpty {UIEmpty|actions=[]},windows=[]}) []
        updRep (TaskRep def parts) = TaskRep (f def) parts
		
instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task eval)	= Task eval`
	where
		eval` event evalOpts=:{modLayout=Nothing} state iworld
			= eval event {TaskEvalOpts|evalOpts & modLayout = Just f} state iworld 
		eval` event evalOpts=:{modLayout=Just g} state iworld
			= eval event {TaskEvalOpts|evalOpts & modLayout = Just (g o f)} state iworld 	

instance tune LazyRefresh
where
	tune _ (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld
			= case (eval event evalOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskEvalInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)
