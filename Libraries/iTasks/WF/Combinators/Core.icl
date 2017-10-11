implementation module iTasks.WF.Combinators.Core

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition

import iTasks.Engine
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import iTasks.Internal.Tonic.Shares
import iTasks.Internal.Client.Override

from iTasks.SDS.Combinators.Common import sdsFocus, sdsSplit, sdsTranslate, toReadOnly, mapRead, mapReadWriteError, mapSingle
from iTasks.Internal.SDS import write, read, readRegister, modify

import StdList, StdBool, StdTuple
import qualified Data.Map as DM
import qualified Data.Queue as DQ

import Data.Maybe, Data.Either, Data.Error
import Text.JSON
from Data.Functor import <$>

derive gEq ParallelTaskChange

:: Action	= Action !String //Locally unique identifier for actions

:: ParallelTaskType	
	= Embedded                                    //Simplest embedded
    | NamedEmbedded !String                       //Embedded with name
	| Detached !TaskAttributes !Bool              //Management meta and flag whether the task should be started at once
    | NamedDetached !String !TaskAttributes !Bool //Detached with name

:: ParallelTask a	:== (SharedTaskList a) -> Task a

// Data available to parallel sibling tasks
:: TaskList a :== (!TaskId,![TaskListItem a])
:: SharedTaskList a	:==	SDS TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]

:: TaskListItem a =
	{ taskId			:: !TaskId
    , listId            :: !TaskId
    , detached          :: !Bool
    , self              :: !Bool
	, value				:: !TaskValue a
	, attributes        :: !TaskAttributes
	, progress		    :: !Maybe InstanceProgress //Only possible for detached tasks
	}

:: TaskListFilter =
    //Which rows to filter
    { onlyIndex         :: !Maybe [Int]
    , onlyTaskId        :: !Maybe [TaskId]
    , onlySelf          :: !Bool
    //What to include
    , includeValue      :: !Bool
    , includeAttributes :: !Bool
    , includeProgress   :: !Bool
    }

instance toString AttachException
where
	toString InstanceNotFound	= "Cannot find task instance to attach"
	toString InstanceEvalError	= "Error in attached task instance "

derive class iTask AttachException

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b
transform f (Task evala) = Task eval
where
	eval event evalOpts tree iworld = case evala event evalOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e, iworld)				    = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC a & JSONDecode{|*|} a & JSONEncode{|*|} a
step (Task evala) lhsValFun conts = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event evalOpts (TCStep taskId ts (Left (TCInit taskIda ts,[]))) iworld

	//Eval left-hand side
	eval event evalOpts (TCStep taskId ts (Left (treea,prevEnabledActions))) iworld=:{current={taskTime}}
		# (resa, iworld) 	= evala event (extendCallTrace taskId evalOpts) treea iworld
        # mbAction          = matchAction taskId event
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbAction conts of
				Nothing			
					# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
                    # value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
					# actions = contActions taskId val conts
					# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
					= Left (ValueResult value info (doStepLayout taskId evalOpts event actions prevEnabledActions rep val)
								(TCStep taskId info.TaskEvalInfo.lastEvent (Left (ntreea,curEnabledActions))))
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskEvalInfo.lastEvent,info.TaskEvalInfo.removedTasks)
			ExceptionResult e = case searchContException e conts of
				Nothing			= Left (ExceptionResult e)
				Just rewrite	= (Right (rewrite,Nothing,ts,[]))		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent,removedTasks)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala (toRefresh event) (extendCallTrace taskId evalOpts) (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb ResetEvent (extendCallTrace taskId evalOpts) (TCInit taskIdb lastEvent) iworld
				= case resb of
					ValueResult val info (ReplaceUI ui) nstateb	
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent, removedTasks = removedTasks ++ info.TaskEvalInfo.removedTasks}
						= (ValueResult val info (ReplaceUI ui) (TCStep taskId info.TaskEvalInfo.lastEvent (Right (d_json_a,sel,nstateb))),iworld)
					ValueResult val info change nstateb	
						= (ExceptionResult (exception ("Reset event of task in step failed to produce replacement UI: ("+++ toString (toJSON change)+++")")), iworld) 
					ExceptionResult e = (ExceptionResult e, iworld)
	//Eval right-hand side
	eval event evalOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{current={taskTime}}
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb event (extendCallTrace taskId evalOpts) treeb iworld
				= case resb of
					ValueResult val info rep ntreeb
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
						= (ValueResult val info rep (TCStep taskId info.TaskEvalInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)
			Nothing
				= (ExceptionResult (exception "Corrupt task value in step"), iworld)	
	
	//Cleanup
    eval event evalOpts (TCDestroy (TCInit _ _)) iworld
        = (DestroyedResult,iworld) //Removed before first evaluation...

	eval event evalOpts (TCDestroy (TCStep taskId ts (Left (treea,_)))) iworld
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
	
	doStepLayout taskId evalOpts event actions prevEnabled change val
		= case (event,change) of
			//On reset generate a new step UI
			(ResetEvent,ReplaceUI rui)  
				= ReplaceUI (uic UIStep [rui:contActions taskId val conts])
			//Otherwise create a compound change definition
			_ 	
				= ChangeUI [] [(0,ChangeChild change):actionChanges]
	where
		actionChanges = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [1..]]
		where
			switch True name = if (isMember name prevEnabled) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
			switch False name = if (isMember name prevEnabled) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange

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

    toRefresh :: Event -> Event
    toRefresh (EditEvent _ _ _)		= RefreshEvent "Converted from Edit"
    toRefresh (ActionEvent _ _)		= RefreshEvent "Converted from Action"
    toRefresh (FocusEvent _)		= RefreshEvent "Converted from Focus"
    toRefresh (RefreshEvent reason)	= RefreshEvent reason
    toRefresh (ResetEvent)          = RefreshEvent "Converted from Reset"

isEnabled (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "enabled" attr)
actionId (UI _ attr _) = maybe "" (\(JSONString s) -> s) ('DM'.get "actionId" attr)

matchAction :: TaskId Event -> Maybe String
matchAction taskId (ActionEvent matchId action)
    | matchId == taskId     = Just action
                            = Nothing
matchAction taskId _        = Nothing

contActions :: TaskId (TaskValue a) [TaskCont a b]-> [UI]
contActions taskId val conts = [actionUI (isJust (taskbf val)) action\\ OnAction action taskbf <- conts]
where
	actionUI enabled action=:(Action actionId)
		= uia UIAction ('DM'.unions [enabledAttr enabled, taskIdAttr (toString taskId), actionIdAttr actionId])

searchContValue :: (TaskValue a) (Maybe String) [TaskCont a b] -> Maybe (!Int, !b, !DeferredJSON) | TC a & JSONEncode{|*|} a
searchContValue val mbAction conts = search val mbAction 0 Nothing conts
where
    search _ _ _ mbMatch []							= mbMatch		//No matching OnValue steps were found, return the potential match
	search val mbAction i mbMatch [OnValue f:cs]
	    = case f val of
			Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
			Nothing		= search val mbAction (i + 1) mbMatch cs	//Keep search
    search val mbAction=:(Just actionEvent) i Nothing [OnAction (Action actionName) f:cs]
	    | actionEvent == actionName
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
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!Int,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!Int,!TaskValue a)] | iTask a
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
            = eval event (setParallel taskId (extendCallTrace taskId evalOpts)) (TCParallel taskId ts [] []) iworld
          Error err = (ExceptionResult err, iworld)
      where
      	writeAll [] sds iworld = (Ok (),iworld)
      	writeAll [(f,w):ws] sds iworld = case write w (sdsFocus f sds) iworld of
          (Ok _,iworld) = writeAll ws sds iworld
          err = err

    //Evaluate the task list
    eval event evalOpts (TCParallel taskId ts taskTrees prevEnabledActions) iworld=:{current=current=:{taskTime}}
		//We need to know how many branches there are before evaluation to be able to determine the correct UI update instructions
		# prevNumBranches = length taskTrees
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
				# actions 	= contActions taskId value conts
                # rep       = genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
                # taskTrees = [(fromOk (taskIdFromTaskTree tree),tree) \\ ValueResult _ _ _ tree <- results | isOk (taskIdFromTaskTree tree)]
                # curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
                = (ValueResult value evalInfo rep (TCParallel taskId ts taskTrees curEnabledActions),iworld)
    //Cleanup
    eval event evalOpts (TCDestroy (TCParallel taskId ts taskTrees _)) iworld=:{current}
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
setParallel taskId evalOpts = {evalOpts & tonicOpts = {evalOpts.tonicOpts & inParallel = Just taskId}}

initParallelTasks ::
	!TaskEvalOpts
	!TaskId
	!Int
	![(!ParallelTaskType,!ParallelTask a)]
	!*IWorld
	->
	(!MaybeError TaskException ([ParallelTaskState]
	,[(TaskId,Task a)])
	,!*IWorld)
	| iTask a
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

initParallelTask ::
	!TaskEvalOpts
	!TaskId
	!Int
	!ParallelTaskType
	!(ParallelTask a)
	!*IWorld
	->
	(!MaybeError TaskException (ParallelTaskState
	,Maybe (TaskId,Task a))
	,!*IWorld)
	| iTask a
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

evalParallelTasks :: 
	TaskId
	(Map TaskId TaskTree)
	!Event !TaskEvalOpts
	[TaskCont [(!TaskTime,!TaskValue a)]
	(!ParallelTaskType,!ParallelTask a)]
	[TaskResult a]
	[ParallelTaskState]
	!*IWorld 
	->
	(MaybeError TaskException [TaskResult a],!*IWorld)
	| iTask a
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
                # (mbError,iworld)      = modify (\l -> ((),[x \\ x <- l | not (isRemoved x)]))
											(sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
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
                      = evalParallelTasks listId taskTrees ResetEvent evalOpts conts completed [state] iworld //Continue
                    err = (liftError err, iworld)
        todo    = evalParallelTasks listId taskTrees event evalOpts conts completed todo iworld     //Evaluate the remaining items
where
	isRemoved {ParallelTaskState|change=Just RemoveParallelTask} = True
	isRemoved _ = False

//Evaluate an embedded parallel task
evalParallelTasks listId taskTrees event evalOpts conts completed [{ParallelTaskState|taskId,detached=False,createdAt,lastFocus,value,change}:todo] iworld=:{current={taskTime}}
    //Lookup task evaluation function and task evaluation state
    # (mbTask,iworld)   = read (sdsFocus taskId taskInstanceEmbeddedTask) iworld
    | mbTask =:(Error _) = (Error (fromError mbTask),iworld)
    # (Task evala)      = fromOk mbTask
    # (tree,newBranch)    = maybe (TCInit taskId taskTime,True) (\tree -> (tree,False)) ('DM'.get taskId taskTrees)
    //Evaluate or destroy branch
    | change === Just RemoveParallelTask
        # (result,iworld) = evala (RefreshEvent "Destroying parallel branch") {mkEvalOpts & noUI = True} (TCDestroy tree) iworld
        //TODO: remove the task evaluation function
        = evalParallelTasks listId taskTrees event evalOpts conts [result:completed] todo iworld
    | otherwise
        # evalOpts        = {evalOpts & tonicOpts = {evalOpts.tonicOpts & captureParallel = evalOpts.tonicOpts.inParallel == Just listId
                                                                        , inParallel      = Just listId}}

		//Evaluate new branches with a reset event
        # (result,iworld) = evala (if newBranch ResetEvent event) (setParallel listId (extendCallTrace taskId evalOpts)) tree iworld
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
				/*
                # rep = case rep of
                    NoRep = NoRep
                    TaskRep def diffs
                        # def = uiDefSetAttribute TASK_ATTRIBUTE (toString taskId) def
                        # def = uiDefSetAttribute CREATED_AT_ATTRIBUTE (toString createdAt) def
                        # def = uiDefSetAttribute LAST_EVENT_ATTRIBUTE (toString lastEvent) def
                        # def = maybe def (\f -> uiDefSetAttribute LAST_FOCUS_ATTRIBUTE (toString f) def) lastFocus
                        = TaskRep def diffs
				*/
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
                Value json stable = (\dec -> Value dec stable) <$> fromJSON json
            //TODO: use global tasktime to be able to compare event times between instances
            # evalInfo = {TaskEvalInfo|lastEvent=0,removedTasks=[],refreshSensitive=True}
            = maybe (ExceptionResult (exception "Could not decode task value of detached task"))
                (\val -> ValueResult val evalInfo NoChange TCNop) mbValue
    = evalParallelTasks listId taskTrees event evalOpts conts [result:completed] todo iworld

genParallelValue :: [TaskResult a] -> TaskValue [(!TaskTime,!TaskValue a)]
genParallelValue results = Value [(lastEvent,val) \\ ValueResult val {TaskEvalInfo|lastEvent} _ _ <- results] False

genParallelRep :: !TaskEvalOpts !Event [UI] [String] [TaskResult a] Int -> UIChange
genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
	= case event of
		ResetEvent
			= ReplaceUI (uic UIParallel ([def \\ ValueResult _ _ (ReplaceUI def) _ <- results] ++ actions))
		_ 
			# (idx,iChanges) = itemChanges 0 prevNumBranches results
			# aChanges       = actionChanges idx
			= ChangeUI [] (iChanges ++ aChanges)
		/*
		ResetEvent
			= ReplaceUI (uic UIParallel [uic UICompoundContent [def \\ ValueResult _ _ (ReplaceUI def) _ <- results]
					 				    ,uic UICompoundContent (map (\x -> ui (UIAction x)) actions)
									    ])
		_ 
			# (n,changes) = itemChanges 0 prevNumBranches results
			= ChangeUI [] [(0,ChangeChild (ChangeUI [] changes))
                          ,(1,ChangeChild (ChangeUI [] (actionChanges 0)))
                          ]
		*/
where
	itemChanges i numExisting [] = (i,[])
	itemChanges i numExisting [ValueResult _ _ change _:rs]
		| i < numExisting
			# (i`,changes) = itemChanges (i + 1) numExisting rs
			= (i`,[(i,ChangeChild change):changes]) 	//Update an existing branch
		| otherwise			= case change of
			(ReplaceUI def)
				# (i`,changes) = itemChanges (i + 1) (numExisting + 1) rs
				= (i`,[(i,InsertChild def):changes]) 	//Add a new branch
			_
				= itemChanges (i + 1) (numExisting + 1) rs //Skip if we don't get a blank UI
	
	itemChanges i numExisting [DestroyedResult:rs]
		| i < numExisting
			# (i`,changes) = itemChanges i (numExisting - 1) rs
			= (i`,[(i,RemoveChild):changes])
		| otherwise
			= itemChanges i numExisting rs //No need to destroy a branch that was not yet in the UI

	itemChanges i numExisting [ExceptionResult e:rs]
		| i < numExisting
			# (i`,changes) = itemChanges i (numExisting - 1) rs
			= (i`,[(i,RemoveChild):changes])
		| otherwise
			= itemChanges i numExisting rs

	actionChanges startIdx = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [startIdx..]]
	where
		switch True name = if (isMember name prevEnabledActions) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
		switch False name = if (isMember name prevEnabledActions) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange


genParallelEvalInfo :: [TaskResult a] -> TaskEvalInfo
genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[],refreshSensitive=False} results
where
    addResult (ValueResult _ i1 _ _) i2
        # lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
        # refreshSensitive = i1.TaskEvalInfo.refreshSensitive || i2.TaskEvalInfo.refreshSensitive
        # removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
        = {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks,refreshSensitive=refreshSensitive}
    addResult _ i = i

readListId :: (SharedTaskList a) *IWorld -> (MaybeError TaskException TaskId,*IWorld) | TC a
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
removeTask :: !TaskId !(SharedTaskList a) -> Task () | TC a
removeTask removeId=:(TaskId instanceNo taskNo) slist = Task eval
where
    eval event evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //If we are removing from the top-level task list, just remove the instance
        | listId == TaskId 0 0
            # (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        //Mark the task as removed, and update the indices of the tasks afterwards
        # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
        # (mbError,iworld)      = modify (\xs -> ((),markAsRemoved removeId xs)) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
        | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
        //If it is a detached task, remove the detached instance, if it is embedded, pass notify the currently evaluating parallel
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            # (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        | otherwise
            //Pass removal information up
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[(listId,removeId)],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval event evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) state, iworld)
    eval event _ (TCDestroy _) iworld
        = (DestroyedResult,iworld)

	rep ResetEvent = ReplaceUI (ui UIEmpty)
	rep _          = NoChange

    //When a task is marked as removed, the index of the tasks after that are decreased
    markAsRemoved removeId [] = []
    markAsRemoved removeId [s=:{ParallelTaskState|taskId}:ss]
        | taskId == removeId = [{ParallelTaskState|s & change = Just RemoveParallelTask}
                               :[{ParallelTaskState|s` & index = index - 1} \\ s`=:{ParallelTaskState|index} <- ss]]
        | otherwise          = [s:markAsRemoved removeId ss]

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask replaceId=:(TaskId instanceNo taskNo) parTask slist = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //Replace the full instance task
        | listId == TaskId 0 0
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //If it is a detached task, replacee the detached instance, if it is embedded schedule the change in the parallel task state
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //Schedule the change in the parallel task state
        | otherwise
            # task                  = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
            # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
            # (mbError,iworld)      = modify (\ts -> ((),scheduleReplacement replaceId task ts)) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) iworld
            | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval event evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) state, iworld)
    eval _ _ (TCDestroy _) iworld
        = (DestroyedResult,iworld)

	rep ResetEvent = ReplaceUI (ui UIEmpty)
	rep _          = NoChange

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

attach :: !InstanceNo !Bool -> Task AttachmentStatus
attach instanceNo steal = Task eval
where
	eval event evalOpts (TCInit taskId ts) iworld=:{current={attachmentChain}}
		# (mbConstants,iworld)		= read (sdsFocus instanceNo taskInstanceConstants) iworld
		| mbConstants =: (Error _)   = (ExceptionResult (fromError mbConstants),iworld)
		# (mbProgress,iworld)		= read (sdsFocus instanceNo taskInstanceProgress) iworld
		| mbProgress =: (Error _)   = (ExceptionResult (fromError mbProgress),iworld)
		# (Ok {InstanceConstants|build}) = mbConstants
		# (Ok progress=:{InstanceProgress|instanceKey,value,attachedTo}) = mbProgress
		//Check if the task is already in use
		| (not (attachedTo =: [])) && (not steal)
			= eval event evalOpts (TCAttach taskId ts (ASInUse (hd attachedTo)) build instanceKey) iworld
		| otherwise
		//Take over the instance. We generate a new key, so the other instance will no longer have access
		# (newKey,iworld) = newInstanceKey iworld
        # progress      = {InstanceProgress|progress & instanceKey = newKey, attachedTo = [taskId:attachmentChain]}
		# (_,iworld)	= write progress (sdsFocus instanceNo taskInstanceProgress) iworld
		//Clear all input and output of that instance
		# (_,iworld)    = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceUIChanges) iworld 
		# (_,iworld)    = modify (\('DQ'.Queue a b) -> ((),'DQ'.Queue [(i,e) \\(i,e)<- a| i <> instanceNo][(i,e) \\(i,e)<- b| i <> instanceNo])) taskEvents iworld 
		= eval event evalOpts (TCAttach taskId ts (ASAttached (value =: Stable)) build newKey) iworld

	eval event evalOpts tree=:(TCAttach taskId ts prevStatus build instanceKey) iworld=:{options={appVersion},current={taskInstance}}
		//Load instance
		# (progress,iworld)	    = readRegister taskId (sdsFocus instanceNo taskInstanceProgress) iworld
		//Determine state of the instance
		# curStatus = case progress of
			(Ok progress=:{InstanceProgress|attachedTo=[attachedId:_],value})
			    | build <> appVersion   = ASIncompatible
				| value =: Exception    = ASExcepted
				| attachedId <> taskId  = ASInUse attachedId	
									 	= ASAttached (value =: Stable)
			_                           = ASDeleted
		//Determine UI change
		# change = determineUIChange event curStatus prevStatus instanceNo instanceKey
		# stable = (curStatus =: ASDeleted) || (curStatus =: ASExcepted)
		= (ValueResult (Value curStatus stable) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} change (TCAttach taskId ts curStatus build instanceKey), iworld)

	eval event evalOpts (TCDestroy (TCAttach taskId _ _ _ _)) iworld
		# (_,iworld)	    = modify (\p -> ((),release p)) (sdsFocus instanceNo taskInstanceProgress) iworld
        = (DestroyedResult,iworld)
	where
		release progress=:{InstanceProgress|attachedTo=[t:_]}
			| t == taskId = {InstanceProgress|progress & attachedTo=[]} //Only release if the instance is still attached to this 'attach' task
						  = progress
		release progress = progress

	determineUIChange event curStatus prevStatus instanceNo instanceKey
		| curStatus === prevStatus && not (event =: ResetEvent) = NoChange
		| curStatus =: (ASInUse _)    = ReplaceUI inuse
		| curStatus =: ASExcepted     = ReplaceUI exception
		| curStatus =: ASIncompatible = ReplaceUI incompatible
		| otherwise     		      = ReplaceUI viewport
	where
		inuse        = stringDisplay "This task is already in use"
		exception    = stringDisplay "An exception occurred in this task"
		incompatible = stringDisplay "This task can no longer be evaluated"
		viewport  =	(uia UIViewport ('DM'.unions [sizeAttr FlexSize FlexSize, instanceNoAttr instanceNo, instanceKeyAttr instanceKey]))

