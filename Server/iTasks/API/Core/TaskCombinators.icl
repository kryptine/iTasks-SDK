implementation module iTasks.API.Core.TaskCombinators

import StdList, StdTuple, StdMisc, StdBool, StdList, StdOrdList

import Internet.HTTP, GenEq, System.Time, Text, Data.Func, Data.Tuple, Data.List, Data.Error, Data.Either, Text.JSON
import iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskStore, iTasks._Framework.TaskEval
import iTasks._Framework.Util, iTasks._Framework.Store
import iTasks._Framework.Generic, iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.UI.Layout
import iTasks._Framework.IWorld
import iTasks._Framework.Tonic.Shares
import iTasks._Framework.Client.Override

import qualified Data.Map as DM
from StdFunc					        import id, const, o, seq
from iTasks						        import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from iTasks._Framework.TaskStore         import localShare, parallelTaskList, topLevelTaskList
from iTasks._Framework.SDS               import write, read, readRegister
import iTasks.API.Core.Types
from iTasks.API.Common.SDSCombinators   import sdsFocus, sdsSplit, sdsTranslate, toReadOnly, mapRead, mapReadWriteError, mapSingle

derive class iTask ParallelTaskType, AttachmentStatus
derive gEq ParallelTaskChange

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
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskEvalInfo.lastEvent,info.TaskEvalInfo.removedTasks)
			ExceptionResult e = case searchContException e conts of
				Nothing			= Left (ExceptionResult e)
				Just rewrite	= Right (rewrite,Nothing,ts,[])		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent,removedTasks)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala (toRefresh event) (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb (toRefresh event) (extendCallTrace taskId evalOpts) (TCInit taskIdb lastEvent) iworld
				= case resb of
					ValueResult val info rep nstateb	
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent, removedTasks = removedTasks ++ info.TaskEvalInfo.removedTasks}
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
    eval event evalOpts (TCDestroy (TCInit _ _)) iworld
        = (DestroyedResult,iworld) //Removed before first evaluation...

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
		= finalizeRep evalOpts (TaskRep ((repLayoutRules evalOpts).LayoutRules.accuStep (UIEmpty {UIEmpty|actions=[]}) (contActions taskId val conts)) NoChange)
	doStepLayout taskId evalOpts (TaskRep def diffs) val
		= finalizeRep evalOpts (TaskRep ((repLayoutRules evalOpts).LayoutRules.accuStep def (contActions taskId val conts)) diffs)

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
matchAction taskId (ActionEvent matchId action)
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

setParallel taskId evalOpts = {evalOpts & tonicOpts = {evalOpts.tonicOpts & inParallel = Just taskId}}

import StdDebug

// Parallel composition
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | iTask a
parallel initTasks conts = Task eval
where
    //Create initial task list
    eval event evalOpts (TCInit taskId ts) iworld=:{IWorld|current}
      //Create the states for the initial tasks
      # (mbParTasks,iworld) = initParallelTasks (setParallel taskId (extendCallTrace taskId evalOpts)) taskId 0 initTasks iworld
      = case mbParTasks of
          Ok (taskList,embeddedTasks)
            //Write the local task list
            # taskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
            # (e,iworld) = write taskList (sdsFocus (taskId,taskListFilter) taskInstanceParallelTaskList) iworld
            | isError e = (ExceptionResult (fromError e),iworld)
            //Write the local embedded tasks
            # (e,iworld) = writeAll embeddedTasks taskInstanceEmbeddedTask iworld
            | isError e = (ExceptionResult (fromError e),iworld)
            //Evaluate the parallel
            = eval event (setParallel taskId (extendCallTrace taskId evalOpts)) (TCParallel taskId ts []) iworld
          Error err = (ExceptionResult err, iworld)
      where
      writeAll [] sds iworld = (Ok (),iworld)
      writeAll [(f,w):ws] sds iworld = case write w (sdsFocus f sds) iworld of
          (Ok _,iworld) = writeAll ws sds iworld
          err = err

    //Evaluate the task list
    eval event evalOpts (TCParallel taskId ts taskTrees) iworld=:{current=current=:{taskTime}}
        //Evaluate the branches of the parallel set
        # (mbResults,iworld)  = evalParallelTasks taskId ('DM'.fromList taskTrees) event evalOpts conts [] [] iworld
        = case mbResults of
            (Error e)
                = (ExceptionResult e, iworld)
            (Ok [ExceptionResult e:_])          //Stopped because of an unhandled exception
                = (ExceptionResult e,iworld)
            (Ok results)
                //Construct the result
                # results   = reverse results //(the results are returned in reverse order)
                # value     = genParallelValue results
                # evalInfo  = genParallelEvalInfo results
                # rep       = genParallelRep evalOpts (contActions taskId value conts) results
                # taskTrees = [(fromOk (taskIdFromTaskTree tree),tree) \\ ValueResult _ _ _ tree <- results | isOk (taskIdFromTaskTree tree)]
                = (ValueResult value evalInfo rep (TCParallel taskId ts taskTrees),iworld)
    //Cleanup
    eval event evalOpts (TCDestroy (TCParallel taskId ts taskTrees)) iworld=:{current}
        //Mark all tasks as deleted and use the standar evaluation function to clean up
        # taskListFilter         = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
        # (mbError,iworld)       = modify (\ptss -> ((),map (\pts -> {ParallelTaskState|pts & change=Just RemoveParallelTask}) ptss)) (sdsFocus (taskId,taskListFilter) taskInstanceParallelTaskList) iworld
        | mbError =:(Error _)    = (ExceptionResult (fromError mbError),iworld)
        # (mbResults,iworld)     = evalParallelTasks taskId ('DM'.fromList taskTrees) event evalOpts conts [] [] iworld
        = (DestroyedResult,iworld)
    //Fallback
    eval _ _ _ iworld
    	= (ExceptionResult (exception "Corrupt task state in parallel"), iworld)

//Parallel helper functions
initParallelTasks :: !TaskEvalOpts !TaskId !Int ![(!ParallelTaskType,!ParallelTask a)] !*IWorld -> (!MaybeError TaskException ([ParallelTaskState],[(TaskId,Task a)]),!*IWorld) | iTask a
initParallelTasks _ _ _ [] iworld = (Ok ([],[]),iworld)
initParallelTasks evalOpts listId index [(parType,parTask):parTasks] iworld
  # (mbStateMbTask, iworld) = initParallelTask evalOpts listId index parType parTask iworld
  = case mbStateMbTask of
      Ok (state,mbTask)
        # (mbStateTasks, iworld) = initParallelTasks evalOpts listId (index + 1) parTasks iworld
        = case mbStateTasks of
            Ok (states,tasks)
              = (Ok ([state:states], maybe tasks (\task -> [task:tasks]) mbTask), iworld)	
            err = (err, iworld)
      err = (liftError err, iworld)

initParallelTask :: !TaskEvalOpts !TaskId !Int !ParallelTaskType !(ParallelTask a) !*IWorld -> (!MaybeError TaskException (ParallelTaskState,Maybe (TaskId,Task a)),!*IWorld) | iTask a
initParallelTask evalOpts=:{tonicOpts = {callTrace}} listId index parType parTask iworld=:{current={taskTime},clocks={localDate,localTime}}
  # (mbTaskStuff,iworld) = case parType of
                             Embedded           = mkEmbedded 'DM'.newMap iworld
                             NamedEmbedded name = mkEmbedded ('DM'.singleton "name" name) iworld
                             Detached           attributes evalDirect = mkDetached attributes evalDirect iworld
                             NamedDetached name attributes evalDirect = mkDetached ('DM'.put "name" name attributes) evalDirect iworld
  = case mbTaskStuff of
      Ok (taskId,attributes,mbTask)
        # state       = { ParallelTaskState
                        | taskId     = taskId
                        , index      = index
                        , detached   = isNothing mbTask
                        , attributes = attributes
                        , value      = NoValue
                        , createdAt  = taskTime
                        , lastFocus  = Nothing
                        , lastEvent  = taskTime
                        , change     = Nothing
                        }
        = (Ok (state,mbTask),iworld)
      err = (liftError err, iworld)
  where
  mkEmbedded attributes iworld
    # (taskId,iworld) = getNextTaskId iworld
    # task            = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
    = (Ok (taskId, attributes, Just (taskId,task)), iworld)
  mkDetached attributes evalDirect iworld
    # (mbInstanceNo,iworld) = newInstanceNo iworld
    = case mbInstanceNo of
        Ok instanceNo
          # isTopLevel        = listId == TaskId 0 0
          # listShare         = if isTopLevel topLevelTaskList (sdsTranslate "setTaskAndList" (\listFilter -> (listId,TaskId instanceNo 0,listFilter)) parallelTaskList)
          # (mbTaskId,iworld) = createDetachedTaskInstance (parTask listShare) isTopLevel evalOpts instanceNo attributes listId evalDirect iworld
          = case mbTaskId of
              Ok taskId
                = (Ok (taskId, attributes, Nothing), iworld)
              err       = (liftError err, iworld)
        err = (liftError err, iworld)

evalParallelTasks :: TaskId (Map TaskId TaskTree) !Event !TaskEvalOpts [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] [TaskResult a] [ParallelTaskState] !*IWorld -> (MaybeError TaskException [TaskResult a],!*IWorld) | iTask a
evalParallelTasks listId taskTrees event evalOpts conts completed [] iworld
    //(re-)read the tasklist to check if it contains items we have not yet evaluated
    # taskListFilter         = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
    # (mbList,iworld)       = read (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
    | mbList =:(Error _)    = (Error (fromError mbList),iworld)
    = case drop (length completed) (fromOk mbList) of
        //We are done, unless we have continuations that extend the set
        []  = case searchContValue (genParallelValue (reverse completed)) (matchAction listId event) conts of
            Nothing //We have evaluated all branches and nothing is added
                //Remove all entries that are marked as removed from the list, they have been cleaned up by now
                # taskListFilter        = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
                # (mbError,iworld)      = modify (\l -> ((),[x \\ x <- l | x.ParallelTaskState.change =!= Just RemoveParallelTask])) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
                | mbList =:(Error _)    = (Error (fromError mbList),iworld)
                = (Ok completed,iworld)
            Just (_,(type,task),_) //Add extension
                # (mbStateMbTask,iworld)     = initParallelTask evalOpts listId 0 type task iworld
                = case mbStateMbTask of
                    Ok (state,mbTask)
                      //Update the task list (TODO, be specific about what we are writing here)
                      # taskListFilter            = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
                      # (mbError,iworld)          = modify (\states -> ((),states ++ [{ParallelTaskState|state & index = length states}])) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
                      | mbError =:(Error _)       = (liftError mbError,iworld)
                      # taskId                    = state.ParallelTaskState.taskId
                      //Store the task function
                      # (mbError,iworld)          = write (snd (fromJust mbTask)) (sdsFocus taskId taskInstanceEmbeddedTask) iworld
                      | mbError =:(Error _)       = (liftError mbError,iworld)
                      = evalParallelTasks listId taskTrees (RefreshEvent "Refresh in new parallel branch") evalOpts conts completed [state] iworld //Continue
                    err = (liftError err, iworld)
        todo    = evalParallelTasks listId taskTrees event evalOpts conts completed todo iworld     //Evaluate the remaining items

//Evaluate an embedded parallel task
evalParallelTasks listId taskTrees event evalOpts conts completed [{ParallelTaskState|taskId,detached=False,createdAt,lastFocus,value,change}:todo] iworld=:{current={taskTime}}
    //Lookup task evaluation function
    # (mbTask,iworld)   = read (sdsFocus taskId taskInstanceEmbeddedTask) iworld
    | mbTask =:(Error _) = (Error (fromError mbTask),iworld)
    # (Task evala)      = fromOk mbTask
    # tree              = fromMaybe (TCInit taskId taskTime) ('DM'.get taskId taskTrees)
    //Evaluate or destroy branch
    | change === Just RemoveParallelTask
        # (result,iworld) = evala (RefreshEvent "Destroying parallel branch") {mkEvalOpts & noUI = True} (TCDestroy tree) iworld
        //TODO: remove the task evaluation function
        = evalParallelTasks listId taskTrees event evalOpts conts [result:completed] todo iworld
    | otherwise
        # evalOpts        = {evalOpts & tonicOpts = {evalOpts.tonicOpts & captureParallel = evalOpts.tonicOpts.inParallel == Just listId
                                                                        , inParallel      = Just listId}}

        # (result,iworld) = evala event (setParallel listId (extendCallTrace taskId {TaskEvalOpts|evalOpts & useLayout=Nothing})) tree iworld
        # iworld          = if (evalOpts.tonicOpts.captureParallel && evalOpts.tonicOpts.currBlueprintExprId <> [] && evalOpts.tonicOpts.currBlueprintTaskId <> TaskId 0 0)
                              (storeTaskOutputViewer result evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId taskId iworld)
                              iworld
        = case result of
            //If an exception occured, check if we can handle it at this level
            ExceptionResult e
                //TODO Check exception
                //If the exception can not be handled, don't continue evaluating just stop
                = (Ok [result:completed],iworld)
            ValueResult val evalInfo=:{TaskEvalInfo|lastEvent,removedTasks} rep tree
                //Check for a focus event targeted at this branc
                # mbNewFocus= case event of
                    (FocusEvent focusId)  = if (focusId == taskId) (Just taskTime) Nothing
                    _                       = Nothing
                # lastFocus     = maybe lastFocus Just mbNewFocus
                //Add some attributes to the user interface that are needed to generate complex
                //parallel layouts such as a set of tabs
                # rep = case rep of
                    NoRep = NoRep
                    TaskRep def diffs
                        # def = uiDefSetAttribute TASK_ATTRIBUTE (toString taskId) def
                        # def = uiDefSetAttribute CREATED_AT_ATTRIBUTE (toString createdAt) def
                        # def = uiDefSetAttribute LAST_EVENT_ATTRIBUTE (toString lastEvent) def
                        # def = maybe def (\f -> uiDefSetAttribute LAST_FOCUS_ATTRIBUTE (toString f) def) lastFocus
                        = TaskRep def diffs
                # result = ValueResult val evalInfo rep tree
                //Check if the value changed
                # newValue = encode val
                # valueChanged = newValue =!= value
                //Write updated value, and optionally the new lastFocus time to the tasklist
                # (mbError,iworld) = if valueChanged
                    (modify (\pts -> ((),{ParallelTaskState|pts & value = encode val, lastFocus = maybe pts.ParallelTaskState.lastFocus Just mbNewFocus}))
                        (sdsFocus (listId,taskId,True) taskInstanceParallelTaskListItem) iworld)
                    (modify (\pts -> ((),{ParallelTaskState|pts & lastFocus = maybe pts.ParallelTaskState.lastFocus Just mbNewFocus}))
                        (sdsFocus (listId,taskId,False) taskInstanceParallelTaskListItem) iworld)
                | mbError =:(Error _) = (Error (fromError mbError),iworld)
                //Add the current result before checking for removals
                # completed = [result:completed]
                //Check if in the branch tasks from this list were removed but that were already evaluated
                # removed = [t \\ (l,t=:(TaskId _ n)) <- removedTasks | l == listId  && n <= taskNo]
                # (completed,iworld) = destroyRemoved removed completed iworld
                = evalParallelTasks listId taskTrees event evalOpts conts completed todo iworld
where
    encode NoValue      = NoValue
    encode (Value v s)  = Value (toJSON v) s

    (TaskId instanceNo taskNo)   = taskId

    taskIdFromResult (ValueResult _ _ _ tree)   = taskIdFromTaskTree tree
    taskIdFromResult _                          = Error (exception "No ValueResult in taskIdFromResult")

    destroyRemoved removed [] iworld = ([],iworld)
    destroyRemoved removed [r=:(ValueResult _ _ _ tree):rs] iworld
      = case taskIdFromResult r of
          Ok taskId
            | isMember taskId removed
                # (mbTask,iworld)    = read (sdsFocus taskId taskInstanceEmbeddedTask) iworld
                | mbTask =:(Error _) = ([ExceptionResult (fromError mbTask):rs],iworld) //TODO figure out what to do with this exception
                # (Task evala)       = fromOk mbTask
                //TODO: remove the task evaluation function
                # evalOpts           = {mkEvalOpts & noUI = True}
                # (r,iworld)         = evala (RefreshEvent "Destroying removed parallel branch") evalOpts (TCDestroy tree) iworld
                # (rs,iworld)        = destroyRemoved removed rs iworld
                = ([r:rs],iworld)
            | otherwise
                # (rs,iworld) = destroyRemoved removed rs iworld
                = ([r:rs],iworld)
          _
            # (rs,iworld) = destroyRemoved removed rs iworld
            = ([r:rs],iworld)
    destroyRemoved removed [r:rs] iworld
        # (rs,iworld) = destroyRemoved removed rs iworld
        = ([r:rs],iworld)

//Retrieve result of detached parallel task
evalParallelTasks listId taskTrees event evalOpts conts completed [{ParallelTaskState|taskId=taskId=:(TaskId instanceNo _),detached=True}:todo] iworld
    # (mbValue,iworld) = readRegister listId (sdsFocus instanceNo taskInstanceValue) iworld
    # result = case mbValue of
        Error e
            = ExceptionResult e
        Ok (TIException dyn msg)
            = ExceptionResult (dyn,msg)
        Ok (TIValue encValue)
            //Decode value value
            # mbValue = case encValue of
                NoValue           = Just NoValue
                Value json stable = fmap (\dec -> Value dec stable) (fromJSON json)
            //TODO: use global tasktime to be able to compare event times between instances
            # evalInfo = {TaskEvalInfo|lastEvent=0,removedTasks=[],refreshSensitive=True}
            = maybe (ExceptionResult (exception "Could not decode task value of detached task"))
                (\val -> ValueResult val evalInfo NoRep TCNop) mbValue
    = evalParallelTasks listId taskTrees event evalOpts conts [result:completed] todo iworld

genParallelValue :: [TaskResult a] -> TaskValue [(!TaskTime,!TaskValue a)]
genParallelValue results = Value [(lastEvent,val) \\ ValueResult val {TaskEvalInfo|lastEvent} _ _ <- results] False

genParallelRep :: !TaskEvalOpts [UIAction] [TaskResult a] -> TaskRep
genParallelRep evalOpts actions results
	= TaskRep ((repLayoutRules evalOpts).LayoutRules.accuParallel [def \\ ValueResult _ _ (TaskRep def diffs) _ <- results] actions) NoChange

genParallelEvalInfo :: [TaskResult a] -> TaskEvalInfo
genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[],refreshSensitive=False} results
where
    addResult (ValueResult _ i1 _ _) i2
        # lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
        # refreshSensitive = i1.TaskEvalInfo.refreshSensitive || i2.TaskEvalInfo.refreshSensitive
        # removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
        = {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks,refreshSensitive=refreshSensitive}
    addResult _ i = i

readListId :: (SharedTaskList a) *IWorld -> (MaybeError TaskException TaskId,*IWorld) | iTask a
readListId slist iworld = case read (sdsFocus taskListFilter slist) iworld of
	(Ok (listId,_),iworld)	= (Ok listId, iworld)
	(Error e, iworld)	    = (Error e, iworld)
where
    taskListFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval _ iworld=:{current={taskTime}}
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (mbListId,iworld)
        # listId = fromOk mbListId
        //Check if someone is trying to add an embedded task to the topLevel list
        | listId == TaskId 0 0 && (parType =:(Embedded) || parType =:(NamedEmbedded _))
            = (Error (exception "Embedded tasks can not be added to the top-level task list"),iworld)
        # (mbStateMbTask,iworld)  = initParallelTask mkEvalOpts listId 0 parType parTask iworld
        = case mbStateMbTask of
            Ok (state,mbTask)
              # taskId = state.ParallelTaskState.taskId
              | listId == TaskId 0 0 //For the top-level list, we don't need to do anything else
                  //TODO: Make sure we don't lose the attributes!
                  = (Ok taskId, iworld)
              //Update the task list
              # taskListFilter      = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
              # (mbError,iworld)    =  modify (\states -> ((),states ++ [{ParallelTaskState|state & index = nextIndex states}])) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
              | mbError =:(Error _) = (liftError mbError,iworld)
              //If the task is an embedded one, we also need to store the task function
              | mbTask =:(Just _)
                  # (mbError,iworld) = write (snd (fromJust mbTask)) (sdsFocus taskId taskInstanceEmbeddedTask) iworld
                  | mbError =:(Error _) = (liftError mbError,iworld)
                  = (Ok taskId, iworld)
              | otherwise
                  = (Ok taskId, iworld)
            err = (liftError err, iworld)
    where
        //To determine the next index we need to disregard states that are marked as removed
        nextIndex states = length [p\\p=:{ParallelTaskState|change} <- states | change =!= Just RemoveParallelTask]

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task () | iTask a
removeTask removeId=:(TaskId instanceNo taskNo) slist = Task eval
where
	eval _ evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //If we are removing from the top-level task list, just remove the instance
        | listId == TaskId 0 0
            # iworld = deleteTaskInstance instanceNo iworld
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        //Mark the task as removed, and update the indices of the tasks afterwards
        # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
        # (mbError,iworld)      = modify (\xs -> ((),markAsRemoved removeId xs)) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
        | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
        //If it is a detached task, remove the detached instance, if it is embedded, pass notify the currently evaluating parallel
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            # iworld = deleteTaskInstance instanceNo iworld
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        | otherwise
            //Pass removal information up
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[(listId,removeId)],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval _ evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) state, iworld)
    eval _ _ (TCDestroy _) iworld
        = (DestroyedResult,iworld)

    //When a task is marked as removed, the index of the tasks after that are decreased
    markAsRemoved removeId [] = []
    markAsRemoved removeId [s=:{ParallelTaskState|taskId}:ss]
        | taskId == removeId = [{ParallelTaskState|s & change = Just RemoveParallelTask}
                               :[{ParallelTaskState|s` & index = index - 1} \\ s`=:{ParallelTaskState|index} <- ss]]
        | otherwise          = [s:markAsRemoved removeId ss]

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask replaceId=:(TaskId instanceNo taskNo) parTask slist = Task eval
where
	eval _ evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //Replace the full instance task
        | listId == TaskId 0 0
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //If it is a detached task, replacee the detached instance, if it is embedded schedule the change in the parallel task state
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //Schedule the change in the parallel task state
        | otherwise
            # task                  = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
            # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
            # (mbError,iworld)      = modify (\ts -> ((),scheduleReplacement replaceId task ts)) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
            | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval _ evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) state, iworld)
    eval _ _ (TCDestroy _) iworld
        = (DestroyedResult,iworld)

    scheduleReplacement replaceId task [] = []
    scheduleReplacement replaceId task [s=:{ParallelTaskState|taskId}:ss]
        | taskId == replaceId   = [{ParallelTaskState|s & change = Just (ReplaceParallelTask (dynamic task :: Task a^))}:ss]
        | otherwise             = [s:scheduleReplacement replaceId task ss]


focusTask :: !TaskId !(SharedTaskList a) -> Task () | iTask a
focusTask focusId slist = mkInstantTask eval
where
    eval taskId iworld=:{IWorld|current={taskTime}}
        # (mbListId,iworld)     = readListId slist iworld
        | mbListId =:(Error _)  = (liftError mbListId, iworld)
        # listId                = fromOk mbListId
        | listId == TaskId 0 0
            = (Ok (), iworld)
        # (mbError,iworld)      = modify (\pts -> ((),{ParallelTaskState|pts & lastFocus = Just taskTime})) (sdsFocus (listId,focusId,False) taskInstanceParallelTaskListItem) iworld
        | mbError =:(Error _)   = (liftError mbError, iworld)
        = (Ok (), iworld)

attach :: !TaskId -> Task AttachmentStatus
attach (TaskId instanceNo taskNo) = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld=:{current={attachmentChain}}
		# (progress,iworld)		= read (sdsFocus instanceNo taskInstanceProgress) iworld
		= case progress of
			Ok progress
                //Just steal the instance, TODO, make stealing optional
                # progress      = {InstanceProgress|progress & attachedTo = [taskId:attachmentChain]}
				# (_,iworld)	= write progress (sdsFocus instanceNo taskInstanceProgress) iworld
				# iworld		= queueRefresh [(instanceNo,"attach of " <+++ instanceNo <+++ " requires refresh")] iworld
				= eval event evalOpts (TCBasic taskId ts JSONNull False) iworld
			Error e
				= (ExceptionResult e,iworld)
		
	eval event evalOpts tree=:(TCBasic taskId ts _ _) iworld=:{server={buildID},current={taskInstance}}
		//Load instance
		# layout			    = repLayoutRules evalOpts
        # (constants,iworld)    = read (sdsFocus instanceNo taskInstanceConstants) iworld
		# (progress,iworld)	    = readRegister taskId (sdsFocus instanceNo taskInstanceProgress) iworld
		# (attributes,iworld)	= readRegister taskId (sdsFocus instanceNo taskInstanceAttributes) iworld
		= case (constants,progress,attributes) of
			(Ok {InstanceConstants|instanceKey,build},Ok progress=:{InstanceProgress|attachedTo=[attachedId],value},Ok attributes)
                | build <> buildID //Check version
				    = (ValueResult (Value ASIncompatible True) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)
                | value === Exception
				    = (ValueResult (Value ASExcepted True) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)
				| attachedId == taskId
                    # rep       = finalizeRep evalOpts (TaskRep (layout.LayoutRules.accuWorkOn (embedTaskDef instanceNo instanceKey) attributes) NoChange)
                    # stable    = value === Stable
					= (ValueResult (Value (ASAttached stable) stable) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} rep tree, iworld)
				| otherwise
					# rep = finalizeRep evalOpts (TaskRep (layout.LayoutRules.accuWorkOn inUseDef attributes) NoChange)
					= (ValueResult (Value (ASInUse attachedId) False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} rep tree, iworld)		
			_
				= (ValueResult (Value ASDeleted True) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} (finalizeRep evalOpts NoRep) tree, iworld)

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

    release taskId meta=:{TIMeta|progress=progress=:{InstanceProgress|attachedTo=Just (worker,attachment)}}
        | isMember taskId attachment    = {TIMeta|meta & progress = {InstanceProgress|meta.TIMeta.progress & attachedTo = Nothing}}
                                        = meta
    release taskId meta = meta

    embedTaskDef instanceNo instanceKey
		= UIForm {UIForm|attributes='DM'.newMap,controls=[(UIEmbedding embedSize {UIEmbeddingOpts|instanceNo=instanceNo,instanceKey=instanceKey},'DM'.newMap)],size=embedSize}

    embedSize = {UISizeOpts|defaultSizeOpts & width= Just FlexSize, height=Just FlexSize}

	inUseDef
		= UIForm {UIForm|attributes='DM'.newMap,controls=[(stringDisplay "This task is already in use",'DM'.newMap)],size=defaultSizeOpts}

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task eval
where	
	eval event evalOpts (TCInit taskId ts) iworld
        # (taskIda,iworld)  = getNextTaskId iworld
        # (e,iworld)        = write (toJSON initial) (sdsFocus taskId localShare) iworld
        | isError e
            = (ExceptionResult (fromError e),iworld)
        | otherwise
		    = eval event evalOpts (TCShared taskId ts (TCInit taskIda ts)) iworld
		
	eval event evalOpts (TCShared taskId ts treea) iworld=:{current={taskTime}}
		# (Task evala)			= stask (sdsFocus taskId localShare)
		# (resa,iworld)			= evala event (extendCallTrace taskId evalOpts) treea iworld
		= case resa of
			ValueResult NoValue info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult NoValue info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),iworld)
			ValueResult (Value stable val) info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult (Value stable val) info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),iworld)
			ExceptionResult e   = (ExceptionResult e,iworld)
	
	eval event evalOpts (TCDestroy (TCShared taskId=:(TaskId instanceNo _) ts treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala) = stask (sdsFocus taskId localShare)
		# (resa,iworld)
            = evala event (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld
        //Remove share from reduct
        # (e,iworld) = modify (\shares -> ((),'DM'.del taskId shares)) (sdsFocus instanceNo taskInstanceShares) iworld
        | isError e
            = (ExceptionResult (fromError e), iworld)
		= (resa,iworld)
	
	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in withShared"), iworld)

exposeShared :: !(RWShared p r w) !(String (RWShared p r w) -> Task a) -> Task a | iTask a & iTask r & iTask w & iTask p
exposeShared shared stask = Task eval
where	
	eval event evalOpts (TCInit taskId ts) iworld=:{exposedShares}
		# (url, iworld)		= newURL iworld
		// Trick to make it work until John fixes the compiler
		# exposedShares 	= 'DM'.put url (dynamic shared :: RWShared p^ r^ w^, toJSONShared shared) exposedShares
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
		= (resa,{iworld & exposedShares = 'DM'.del url iworld.exposedShares})
	
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
		eval` event evalOpts=:{useLayout=Nothing,modLayout} state iworld
			= eval event {TaskEvalOpts|evalOpts & useLayout = Just ((fromMaybe id modLayout) layout), modLayout = Nothing} state iworld
		eval` event evalOpts=:{useLayout=Just _,modLayout} state iworld
			= eval event {TaskEvalOpts|evalOpts & useLayout = Just layout, modLayout = Nothing} state iworld
	
instance tune AfterLayout
where
	tune (AfterLayout f) (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld = case eval event evalOpts state iworld of
	        (ValueResult value info rep tree,iworld) = (ValueResult value info (updRep rep) tree, iworld)
            (res,iworld) = (res,iworld)

        updRep NoRep          		= TaskRep (f (UIEmpty {UIEmpty|actions=[]})) NoChange
        updRep (TaskRep def diffs)  = TaskRep (f def) diffs
		
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

withTaskId :: (Task a) -> Task (a, TaskId)
withTaskId (Task eval) = Task eval`
  where
  eval` event evalOpts state iworld
    = case eval event evalOpts state iworld of
        (ValueResult (Value x st) info rep tree, iworld) -> case taskIdFromTaskTree tree of
                                                              Ok tid -> (ValueResult (Value (x, tid) st) info rep tree, iworld)
                                                              _      -> (ValueResult (Value (x, TaskId 0 0) st) info rep tree, iworld)
        (ValueResult NoValue info rep tree, iworld) -> (ValueResult NoValue info rep tree, iworld)
        (ExceptionResult te, iworld) -> (ExceptionResult te, iworld)
        (DestroyedResult, iworld) -> (DestroyedResult, iworld)
