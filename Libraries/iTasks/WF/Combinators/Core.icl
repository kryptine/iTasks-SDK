implementation module iTasks.WF.Combinators.Core

import iTasks.WF.Tasks.Core
import StdEnv

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition

import iTasks.Engine
import iTasks.Internal.EngineTasks
import iTasks.Internal.DynamicUtil
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskIO
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import iTasks.Internal.Util
import iTasks.Internal.AsyncSDS
import iTasks.Util.DeferredJSON

from iTasks.SDS.Combinators.Common import sdsFocus, sdsSplit, sdsTranslate, toReadOnly, mapRead, mapReadWriteError, mapSingle, removeMaybe, |*|
import iTasks.WF.Combinators.Common
from iTasks.Internal.SDS import write, read, readRegister, modify

import iTasks.WF.Tasks.System

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Queue as DQ

import Data.Maybe, Data.Either, Data.Error, Data.Func
import Text.GenJSON
from Data.Functor import <$>, class Functor(fmap)
from Data.Map import qualified instance Functor (Map k)

derive gEq TaskChange
derive gDefault TaskListFilter, TaskId

instance toString AttachException
where
	toString InstanceNotFound	= "Cannot find task instance to attach"
	toString InstanceEvalError	= "Error in attached task instance "

derive class iTask AttachException

transformError :: ((TaskValue a) -> MaybeError TaskException (TaskValue b)) !(Task a) -> Task b
transformError f task = Task (eval task)
where
	eval (Task task) event evalOpts iworld = case task event evalOpts iworld of
		(ValueResult val lastEvent rep task, iworld) = case f val of
			Error e = (ExceptionResult e, iworld)
			Ok v = (ValueResult v lastEvent rep (Task (eval task)), iworld)
		(ExceptionResult e, iworld)                  = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)                    = (DestroyedResult, iworld)

removeDupBy :: (a a -> Bool) [a] -> [a]
removeDupBy eq [x:xs] = [x:removeDupBy eq (filter (not o eq x) xs)]
removeDupBy _ [] = []

step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC, JSONEncode{|*|} a
step lhs lhsValFun conts = Task evalinit
where
	//Initial setup:
	//Destroyed before first evaluation
	//evalinit :: !Event !TaskEvalOpts !*IWorld -> *(TaskResult a, !*IWorld)
	evalinit DestroyEvent evalOpts iworld
		= (DestroyedResult,iworld)
	//Check for duplicates
	evalinit event evalOpts iworld
		# iworld = if (length (removeDupBy actionEq conts) == length conts)
			iworld
			(iShowErr ["Duplicate actions in step"] iworld)
		# (taskIda, iworld) = getNextTaskId iworld
		= evalleft lhs [] taskIda event evalOpts iworld
	where
		actionEq (OnAction (Action a) _) (OnAction (Action b) _) = a == b
		actionEq _ _ = False

	//Evaluating the lhs
	//Destroyed when executing the lhs
	//evalleft :: (Task a) [String] TaskId Event TaskEvalOpts !*IWorld -> *(TaskResult a, IWorld)
	evalleft (Task lhs) prevEnabledActions leftTaskId DestroyEvent evalOpts iworld
		= case lhs DestroyEvent {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld of
			(DestroyedResult, iworld)    = (DestroyedResult, iworld)
			(ExceptionResult e, iworld)  = (ExceptionResult e, iworld)
			(ValueResult _ _ _ _,iworld) = (ExceptionResult (exception "Failed destroying lhs in step"), iworld)
	//Execute lhs
	evalleft (Task lhs) prevEnabledActions leftTaskId event evalOpts=:{lastEval,taskId} iworld
		# mbAction = matchAction taskId event
		# (res, iworld) = lhs event {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld
		// Right  is a step
		# (mbCont, iworld) = case res of
			ValueResult val info rep lhs
				= case searchContValue val mbAction conts of
					//No match
					Nothing
						# info = {TaskEvalInfo|info & lastEvent = max lastEval info.TaskEvalInfo.lastEvent}
						# value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
						# actions = contActions taskId val conts
						# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
						# sl = wrapStepUI taskId evalOpts event actions prevEnabledActions val rep
						= (Left (ValueResult
							value
							info
							sl
							(Task (evalleft lhs curEnabledActions leftTaskId))
							)
						, iworld)
					//A match
					Just rewrite
						//Send a destroyevent to the lhs
						# (_, iworld) = (unTask lhs) DestroyEvent {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld
						= (Right (rewrite, info.TaskEvalInfo.lastEvent, info.TaskEvalInfo.removedTasks), iworld)
			ExceptionResult e
				= case searchContException e conts of
					//No match
					Nothing      = (Left (ExceptionResult e), iworld)
					//A match
					Just rewrite = (Right (rewrite, lastEval, []), iworld)
		= case mbCont of
			//No match, just pass through
			Left res = (res, iworld)
			//A match, continue with the matched rhs
			Right ((_, (Task rhs), _), lastEvent, removedTasks)
				//Execute the rhs with a reset event
				# (resb, iworld)        = rhs ResetEvent evalOpts iworld
				= case resb of
					ValueResult val info change=:(ReplaceUI _) (Task rhs)
						# info = {TaskEvalInfo|info & lastEvent = max lastEvent info.TaskEvalInfo.lastEvent, removedTasks = removedTasks ++ info.TaskEvalInfo.removedTasks}
						= (ValueResult
							val
							info
							change
							//Actually rewrite to the rhs
							(Task rhs)
						,iworld)
					ValueResult _ _ change _
						= (ExceptionResult (exception ("Reset event of task in step failed to produce replacement UI: ("+++ toString (toJSON change)+++")")), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)

	wrapStepUI taskId evalOpts event actions prevEnabled val change
		| actionUIs =: []
			= case (event,change) of
				(ResetEvent,ReplaceUI (UI type attributes items)) //Mark the ui as a step
					= ReplaceUI (UI type (addClassAttr "step" attributes) items)
				_
					= change
		| otherwise	//Wrap in a container
			= case (event,change) of
				(ResetEvent,ReplaceUI ui) //On reset generate a new step UI
					= ReplaceUI (uiac UIContainer (classAttr ["step-actions"]) [ui:actionUIs])
				_  //Otherwise create a compound change definition
					= ChangeUI [] [(0,ChangeChild change):actionChanges]
	where
		actionUIs = contActions taskId val conts
		actionChanges = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [1..]]
		where
			switch True name = if (isMember name prevEnabled) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
			switch False name = if (isMember name prevEnabled) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange

matchAction :: TaskId Event -> Maybe String
matchAction taskId (ActionEvent matchId action)
	| matchId == taskId = Just action
matchAction taskId _ = Nothing

isEnabled (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "enabled" attr)
actionId (UI _ attr _) = maybe "" (\(JSONString s) -> s) ('DM'.get "actionId" attr)

contActions :: TaskId (TaskValue a) [TaskCont a b]-> [UI]
contActions taskId val conts = [actionUI (isJust (taskbf val)) action\\ OnAction action taskbf <- conts]
where
	actionUI enabled action=:(Action actionId)
		= uia UIAction ('DM'.unions [enabledAttr enabled, taskIdAttr (toString taskId), actionIdAttr actionId])

searchContValue :: (TaskValue a) (Maybe String) [TaskCont a b] -> Maybe (!Int, !b, !DeferredJSON) | TC a & JSONEncode{|*|} a
searchContValue val mbAction conts
	= search val mbAction 0 Nothing conts
where
	search _ _ _ mbMatch []							= mbMatch		//No matching OnValue steps were found, return the potential match
	search val mbAction i mbMatch [OnValue f:cs]
		= case f val of
			Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
			Nothing		= search val mbAction (i + 1) mbMatch cs	//Keep search
	search val mbAction=:(Just actionEvent) i Nothing [OnAction (Action actionName) f:cs]
		| actionEvent == actionName
			= case f val of
				Just cont = search val mbAction (i + 1) (Just (i, cont, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
				Nothing = search val mbAction (i + 1) Nothing cs								//Keep searching
		= search val mbAction (i + 1) Nothing cs								//Keep searching
	search val mbAction i mbMatch [_:cs]			= search val mbAction (i + 1) mbMatch cs		//Keep searching

searchContException :: (Dynamic,String) [TaskCont a b] -> Maybe (Int, !b, !DeferredJSON)
searchContException (dyn,str) conts = search dyn str 0 Nothing conts
where
	search _ _ _ catchall []                        = catchall                                                        //Return the maybe catchall
	search dyn str i catchall [OnException f:cs]    = case (match f dyn) of
		Just (taskb,enca) = Just (i, taskb, enca)                                            //We have a match
		_                 = search dyn str (i + 1) catchall cs                            //Keep searching
	search dyn str i Nothing [OnAllExceptions f:cs] = search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
	search dyn str i mbcatchall [_:cs]              = search dyn str (i + 1) mbcatchall cs                            //Keep searching

	match :: (e -> b) Dynamic -> Maybe (b, DeferredJSON) | iTask e
	match f (e :: e^)    = Just (f e, DeferredJSON e)
	match _ _            = Nothing

// Parallel composition
parallel :: ![(ParallelTaskType,ParallelTask a)] [TaskCont [(Int,TaskValue a)] (ParallelTaskType,ParallelTask a)] -> Task [(Int,TaskValue a)] | iTask a
parallel initTasks conts = Task evalinit
where
	//Destroyed before initial execution
	evalinit DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	//Initialize the task list
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
	//Create the states for the initial tasks
		= case initParallelTasks evalOpts taskId initTasks iworld of
			(Ok (taskList,embeddedTasks),iworld)
				//Write the initial local task list (no need to set relevant columns, because no one is registered yet)
				# (e,iworld) = write taskList (sdsFocus (taskId, taskId, fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
				| isError e = (ExceptionResult (fromError e),iworld)
				//Write the local embedded tasks
				# (e,iworld) = write ('DM'.fromList embeddedTasks) (sdsFocus (taskId, taskId, fullTaskListFilter) taskInstanceParallelTaskListTasks) EmptyContext iworld
				| isError e = (ExceptionResult (fromError e),iworld)
				//Evaluate the parallel
				= eval (length embeddedTasks) [] event evalOpts iworld
			(Error err,iworld)
				= (ExceptionResult err, iworld)
	where
		initParallelTasks _ _ [] iworld = (Ok ([],[]),iworld)
		initParallelTasks evalOpts listId [(parType,parTask):parTasks] iworld
			# (mbStateMbTask, iworld) = initParallelTask evalOpts listId parType parTask iworld
			= case mbStateMbTask of
					Ok (state,mbTask)
						# (mbStateTasks, iworld) = initParallelTasks evalOpts listId parTasks iworld
						= case mbStateTasks of
							Ok (states,tasks)
								= (Ok ([state:states], maybe tasks (\task -> [(state.TaskMeta.taskId,task):tasks]) mbTask), iworld)
							err = (err, iworld)
					err = (liftError err, iworld)

	eval _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
		= destroyParallelTasks taskId iworld

	//Evaluate the task list
	eval prevNumBranches prevEnabledActions event evalOpts=:{TaskEvalOpts|taskId} iworld
		//Evaluate all branches of the parallel set
		= case evalParallelTasks event evalOpts conts [] [] 'DM'.newMap iworld of
			(Ok results, iworld)
				//Clean up the stored task list (remove entries marked as removed etc.)
				# (mbRes,iworld) = cleanupParallelTaskList taskId iworld
				//| mbRes =:(Error _) = (Error (fromError mbRes),iworld)
				//Construct the combined task result
				# value     = genParallelValue results
				# evalInfo  = genParallelEvalInfo results
				# actions   = contActions taskId value conts
				# rep       = genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
				# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
				# curNumBranches = length [()\\(ValueResult _ _ _ _)<-results]
				= (ValueResult value evalInfo rep (Task (eval curNumBranches curEnabledActions)), iworld)
			//Stopped because of an unhandled exception
			(Error e, iworld)
				//Clean up before returning the exception
				# (res,iworld) = destroyParallelTasks taskId iworld
				= (exceptionResult res e,iworld)
	where
		exceptionResult :: (TaskResult [(Int,TaskValue a)]) TaskException -> (TaskResult [(Int,TaskValue a)])
		exceptionResult DestroyedResult e = ExceptionResult e
		exceptionResult (ExceptionResult _) e = ExceptionResult e

		genParallelEvalInfo :: [TaskResult a] -> TaskEvalInfo
		genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[]} results
		where
			addResult (ValueResult _ i1 _ _) i2
				# lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
				# removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
				= {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks}
			addResult _ i = i

initParallelTask ::
	!TaskEvalOpts
	!TaskId
	!ParallelTaskType
	!(ParallelTask a)
	!*IWorld
	->
	(!MaybeError TaskException (TaskMeta, Maybe (Task a)), !*IWorld)
	| iTask a
initParallelTask evalOpts listId Embedded parTask iworld=:{options,clock,current={taskTime}}
	# (taskId,iworld) = getNextTaskId iworld
	# task            = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
	# meta =
		{ TaskMeta
		| taskId               = taskId
		, instanceType         = PersistentInstance
		, build                = options.EngineOptions.appVersion
		, createdAt            = clock
		, detachedFrom         = Just listId
		, nextTaskNo           = 0
		, nextTaskTime         = 0
		, status               = Right False
		, attachedTo           = []
		, connectedTo          = Nothing
		, instanceKey          = Nothing
		, firstEvent           = Just clock
		, lastEvent            = Just clock
		, lastIO               = Nothing
		, cookies              = 'DM'.newMap
		, taskAttributes       = 'DM'.newMap
		, managementAttributes = 'DM'.newMap
		, unsyncedAttributes   = 'DS'.newSet
		, unsyncedCookies      = []
		, change               = Nothing
		, initialized          = False
		}
	= (Ok (meta, Just task),iworld)
initParallelTask evalOpts listId (Detached evalDirect initManagementAttr) parTask iworld=:{options,clock,current={taskTime}}
	//We need to know the instance number in advance, so we can pass the correctly focused task list share
	//to the detached parallel task
	# (mbInstanceNo,iworld) = newInstanceNo iworld
	= case mbInstanceNo of
		Ok instanceNo
			# listShare = sdsTranslate "setTaskAndList" (\listFilter -> (listId, TaskId instanceNo 0, listFilter)) parallelTaskList
			= case createDetachedTaskInstance (parTask listShare) evalOpts instanceNo initManagementAttr listId evalDirect iworld of
				(Ok meta,iworld) = (Ok (meta, Nothing), iworld)
				(err,iworld)     = (liftError err, iworld)
		Error e = (Error e, iworld)

evalParallelTasks :: !Event !TaskEvalOpts
	[TaskCont [(TaskTime,TaskValue a)] (ParallelTaskType,ParallelTask a)]
	[(TaskId, TaskResult a)] [TaskMeta] (Map TaskId (TaskValue a)) !*IWorld
	->
	(MaybeError TaskException [TaskResult a],!*IWorld) | iTask a
evalParallelTasks event evalOpts=:{TaskEvalOpts|taskId=listId} conts completed [] values iworld
	//(re-)read the tasklist to check if it contains items we have not yet evaluated
	# filter = {TaskListFilter|fullTaskListFilter & notTaskId = Just (map fst completed)} //Explicitly exclude the tasks we already evaluated
	# (mbList,iworld)       = read (sdsFocus (listId,listId,filter) (taskInstanceParallelTaskList |*| taskInstanceParallelTaskListValues)) EmptyContext iworld
	| mbList =:(Error _)    = (Error (fromError mbList),iworld)
	# ((_,states),values)   = directResult (fromOk mbList)
	= case states of
		//We are done, unless we have continuations that extend the set
		[] = case searchContValue (genParallelValue (reverse (map snd completed))) (matchAction listId event) conts of
			Nothing //We have evaluated all branches and nothing is added anymore
                = (Ok $ reverse $ map snd completed, iworld)
			Just (_,(type,task),_) //One of the rules for extending the list triggered
				= case initParallelTask evalOpts listId type task iworld of
					(Ok (state,mbTask),iworld)
					  # taskId                    = state.TaskMeta.taskId
					  # (mbError,iworld)          = modify (\(_,states) -> states ++ [state])
							(sdsFocus (listId,taskId,fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
					  | mbError =:(Error _)       = (liftError mbError,iworld)
					  //Store the task function
					  # (mbError,iworld)          = (write (fromJust mbTask @? encodeTaskValue) (sdsFocus (listId,taskId) taskInstanceParallelTaskListTask) EmptyContext iworld)
					  | mbError =:(Error _)       = (liftError mbError,iworld)
					  = evalParallelTasks ResetEvent evalOpts conts completed [state] values iworld //Continue
					(err,iworld) = (liftError err, iworld)
		//There is more work to do:
		todo = evalParallelTasks event evalOpts conts completed todo values iworld

evalParallelTasks event evalOpts=:{TaskEvalOpts|taskId=listId} conts completed [t=:{TaskMeta|taskId=taskId=:(TaskId _ taskNo)}:todo] values iworld
	# lastValue = fromMaybe NoValue $ 'DM'.get taskId values
	= case evalParallelTask listId event evalOpts t lastValue iworld of
		(Error e, iworld) = (Error e,iworld)
		(Ok (ExceptionResult e), iworld) = (Error e,iworld) //Stop on exceptions
		(Ok result=:(ValueResult val {TaskEvalInfo|lastEvent,removedTasks} rep task), iworld)
			//Add the current result before checking for removals
			# completed = [(taskId, result):completed]
			//Check if in the branch tasks from this list were removed but that were already evaluated
			# removed = [t \\ (l,t=:(TaskId _ n)) <- removedTasks | l == listId && n <= taskNo]
			# (completed,iworld) = destroyRemoved listId removed completed iworld
			= evalParallelTasks event evalOpts conts completed todo values iworld
		(Ok result=:DestroyedResult, iworld)
			= evalParallelTasks event evalOpts conts [(taskId, result):completed] todo values iworld

evalParallelTask :: TaskId !Event !TaskEvalOpts TaskMeta (TaskValue a) !*IWorld
	-> *(MaybeError TaskException (TaskResult a), !*IWorld) | iTask a
evalParallelTask listId=:(TaskId listInstanceNo _) event evalOpts taskState=:{TaskMeta|taskId=TaskId instanceNo _} value iworld
	| instanceNo <> listInstanceNo = evalDetachedParallelTask listId event evalOpts taskState iworld
                                   = evalEmbeddedParallelTask listId event evalOpts taskState value iworld
where
	//Retrieve result of detached parallel task
	evalDetachedParallelTask :: !TaskId !Event !TaskEvalOpts !TaskMeta !*IWorld -> *(MaybeError TaskException (TaskResult a), *IWorld) | iTask a
	evalDetachedParallelTask listId event evalOpts localMeta=:{TaskMeta|taskId=taskId=:(TaskId instanceNo _),managementAttributes,unsyncedAttributes} iworld
		//If we have local management updates, first synchronize them to the detached task list entry
		# (mbError,iworld) = modify (syncManagementAttributes managementAttributes unsyncedAttributes)
			(sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| mbError =:(Error _) = (Error (fromError mbError),iworld)
		//Synchronize the meta-data and value
		= case readRegister listId (sdsFocus (instanceNo,False,False) taskInstance |*| sdsFocus instanceNo taskInstanceValue) iworld of
			(Error e,iworld) = (Error e,iworld)
			(Ok (ReadingDone (detachedMeta,encoded)),iworld)
				# value = decode encoded
				# evalInfo = {TaskEvalInfo|lastEvent=0,removedTasks=[]}
				# result = ValueResult value evalInfo NoChange nopTask
				//Synchronize the record in the local list with the entry in the global list
                # (mbError,iworld) = write detachedMeta (sdsFocus (listId,taskId,False) taskInstanceParallelTaskListItem) EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError),iworld)
				= (Ok (ValueResult value evalInfo NoChange nopTask), iworld)
	where
		decode (Value enc stable) = maybe NoValue (\dec -> Value dec stable) (fromDeferredJSON enc)
		decode NoValue = NoValue 

		syncManagementAttributes localAttr syncKeys meta=:{TaskMeta|managementAttributes}
			 = {TaskMeta|meta & managementAttributes = 'DM'.union syncAttr managementAttributes}
		where
			syncAttr = 'DM'.filterWithKey (\k v -> 'DS'.member k syncKeys) localAttr

	evalEmbeddedParallelTask :: !TaskId !Event !TaskEvalOpts !TaskMeta (TaskValue a) !*IWorld -> *(MaybeError TaskException (TaskResult a), *IWorld) | iTask a
	evalEmbeddedParallelTask listId event evalOpts meta=:{TaskMeta|taskId,createdAt,change,initialized} value iworld=:{current={taskTime}}
		//Check if we need to destroy the branch
		| change === Just RemoveTask
			# (result, iworld) = destroyEmbeddedParallelTask listId taskId iworld
			= case result of
				(Ok res) = (Ok res,iworld)
				(Error e) = (Error (exception (ExceptionList e)), iworld)
		//Lookup task evaluation function, and task evaluation state
		# thisTask = sdsFocus (listId,taskId) taskInstanceParallelTaskListTask
		# (mbTask,iworld) = read thisTask EmptyContext iworld
		| mbTask =:(Error _) = (Error (fromError mbTask),iworld)
		# (Task evala) = directResult (fromOk mbTask)
		//Evaluate new branches with a reset event, other with the event
		= case evala (if initialized event ResetEvent) {TaskEvalOpts|evalOpts&taskId=taskId} iworld of
			//If an exception occured, check if we can handle it at this level
			(ExceptionResult e, iworld)
				//TODO Check exception
				//If the exception can not be handled, don't continue evaluating just stop
				= (Ok (ExceptionResult e),iworld)
			(ValueResult val evalInfo=:{TaskEvalInfo|lastEvent,removedTasks} change task, iworld)
				# val = decodeTaskValue val
				//Isolate changes to implicit task attributes
				# taskAttributeUpdate = case change of
					ReplaceUI (UI _ attributes _) = const attributes
					ChangeUI changes _ = \a -> foldl (flip applyUIAttributeChange) a changes
					_ = id
				//Add unsynced changes to management attributes. We need to re-read the tasklist item because they may have
				//been modified by the other branches
				# (mbManagementMeta,iworld) = read (sdsFocus (listId,taskId,False) taskInstanceParallelTaskListItem) EmptyContext iworld
				| mbManagementMeta=:(Error _) = (Error (fromError mbManagementMeta),iworld)
				# change = addManagementAttributeChanges (directResult $ fromOk mbManagementMeta) change
				//Construct task result
				# result = ValueResult val evalInfo change (task @? decodeTaskValue)
				//Check if the value changed
				# valueChanged = val =!= value
				//Write the new reduct
				# (mbError, iworld) = write task thisTask EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError), iworld)
				//Write meta data
                # (mbError,iworld) = modify
						(\meta -> {TaskMeta|meta & status = valueStatus val,
							taskAttributes = taskAttributeUpdate meta.TaskMeta.taskAttributes, unsyncedAttributes = 'DS'.newSet, initialized = True})
                        (sdsFocus (listId,taskId,valueChanged) taskInstanceParallelTaskListItem)
						EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError),iworld)
				| not valueChanged 
					= (Ok result, iworld)
                //Write updated value
				# (mbError,iworld) = write val (sdsFocus (listId,taskId) taskInstanceParallelTaskListValue) EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError),iworld)
				= (Ok result, iworld)
	where
		(TaskId instanceNo taskNo)   = taskId

		valueStatus (Value _ True) = Right True
		valueStatus _ = Right False

		addManagementAttributeChanges {TaskMeta|unsyncedAttributes,managementAttributes} change 
			= mergeUIChanges change (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList managementAttributes | 'DS'.member k unsyncedAttributes] [])

cleanupParallelTaskList :: !TaskId !*IWorld -> *(MaybeError TaskException (), *IWorld)
cleanupParallelTaskList listId iworld
	//Remove all entries that are marked as removed from the list, they have been cleaned up by now
	# (res,iworld) = modify (filter (not o isRemoved) o snd)
		(sdsFocus (listId,listId,fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
	= (const () <$> res, iworld)
where
	isRemoved {TaskMeta|change=Just RemoveTask} = True
	isRemoved _ = False

destroyParallelTasks :: !TaskId !*IWorld -> *(TaskResult [(Int,TaskValue a)], *IWorld) | iTask a
destroyParallelTasks listId=:(TaskId instanceNo _) iworld
	// Unlink registrations for all detached tasks
	# iworld = clearTaskSDSRegistrations ('DS'.singleton listId) iworld
	= case read (sdsFocus (listId, listId, fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld of
		(Error e,iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone (_,taskStates)),iworld)
			// Destroy all child tasks (`result` is always `DestroyedResult` but passed to solve overloading
			# (result,exceptions,iworld) = foldl (destroyParallelTask listId) (DestroyedResult, [], iworld) taskStates

			// Remove the (shared) tasklist/value/reduct
			# (exceptions,iworld) = case write [] (sdsFocus (listId,listId,fullTaskListFilter,fullExtendedTaskListFilter) taskListMetaData) EmptyContext iworld of
				(Ok (WritingDone ),iworld) = (exceptions,iworld)
				(Error e,iworld) = ([e:exceptions],iworld)
			| not $ exceptions =: [] = (ExceptionResult (exception (ExceptionList exceptions)), iworld)
			# (exceptions,iworld) = case write 'DM'.newMap (sdsFocus (listId,listId,fullTaskListFilter,fullExtendedTaskListFilter) taskListDynamicValueData) EmptyContext iworld of
				(Ok (WritingDone ),iworld) = (exceptions,iworld)
				(Error e,iworld) = ([e:exceptions],iworld)
			| not $ exceptions =: [] = (ExceptionResult (exception (ExceptionList exceptions)), iworld)
			# (exceptions,iworld) = case write 'DM'.newMap (sdsFocus (listId,listId,fullTaskListFilter,fullExtendedTaskListFilter) taskListDynamicTaskData) EmptyContext iworld of
				(Ok (WritingDone ),iworld) = (exceptions,iworld)
				(Error e,iworld) = ([e:exceptions],iworld)
			| not $ exceptions =: [] = (ExceptionResult (exception (ExceptionList exceptions)), iworld)
			= (destroyResult result, iworld)
where
	destroyParallelTask listId=:(TaskId listInstance _) (_,exceptions,iworld) {TaskMeta|taskId=taskId=:(TaskId taskInstance _)}
		= case (if detached destroyDetachedParallelTask destroyEmbeddedParallelTask) listId taskId iworld of
			(Error e, iworld) = (DestroyedResult, e ++ exceptions,iworld)
			(Ok res, iworld) = (res, exceptions,iworld)
	where
		detached = taskInstance <> listInstance

	destroyResult :: (TaskResult a) -> (TaskResult [(Int,TaskValue a)])
	destroyResult DestroyedResult = DestroyedResult
	destroyResult (ExceptionResult e) = ExceptionResult e
	destroyResult (ValueResult _ _ _ _) = ExceptionResult (exception "Valueresult in a destroy?")

destroyEmbeddedParallelTask :: TaskId TaskId *IWorld -> *(MaybeError [TaskException] (TaskResult a), *IWorld) | iTask a
destroyEmbeddedParallelTask listId=:(TaskId instanceNo _) taskId iworld=:{current={taskTime}}
	# (errs,destroyResult,iworld) = case read (sdsFocus (listId,taskId) taskInstanceParallelTaskListTask) EmptyContext iworld of
		(Error e,iworld) = ([e], DestroyedResult,iworld)
		(Ok (ReadingDone (Task eval)),iworld)
			= case eval DestroyEvent {mkEvalOpts & noUI = True, taskId=taskId} iworld of
				(DestroyedResult,   iworld) = ([],  DestroyedResult, iworld)
				(ExceptionResult e, iworld) = ([e], DestroyedResult, iworld)
				(_,                 iworld) =
					([exception "destroyEmbeddedParallelTask: unexpected result"],DestroyedResult,iworld)
	// 2. Remove the task evaluation function
	# (errs,iworld) = case modify (\tasks -> 'DM'.del taskId tasks)
	                              (sdsFocus (listId,listId,defaultValue,defaultValue) taskListDynamicTaskData) EmptyContext iworld of
		(Error e,iworld) = ([e:errs],iworld)
		(Ok (ModifyingDone _),iworld) = (errs,iworld)
	= (Ok destroyResult, iworld)

destroyDetachedParallelTask :: TaskId TaskId *IWorld -> *(MaybeError [TaskException] (TaskResult a), *IWorld) | iTask a
destroyDetachedParallelTask listId=:(TaskId instanceNo _) taskId iworld
	//TODO: Detached parallel tasks should be marked that their parent no longer needs their result
	//      That way attach combinators can be programmed to notify the user or simply stop the task
	= (Ok DestroyedResult, iworld)

destroyRemoved :: TaskId [TaskId] [(TaskId, TaskResult a)] *IWorld -> ([(TaskId, TaskResult a)], *IWorld) | iTask a
destroyRemoved listId removed [] iworld = ([],iworld)
destroyRemoved listId removed [r=:(taskId, _):rs] iworld
	| isMember taskId removed
		= case destroyEmbeddedParallelTask listId taskId iworld of
			(Error e, iworld) = ([(taskId, ExceptionResult (exception (ExceptionList e))):rs], iworld)
			(Ok tr, iworld)
				# (rs,iworld) = destroyRemoved listId removed rs iworld
				= ([(taskId, tr):rs],iworld)
	# (rs,iworld) = destroyRemoved listId removed rs iworld
	= ([r:rs],iworld)

genParallelValue :: [TaskResult a] -> TaskValue [(TaskTime,TaskValue a)]
genParallelValue results = Value [(lastEvent,val) \\ ValueResult val {TaskEvalInfo|lastEvent} _ _ <- results] False

genParallelRep :: !TaskEvalOpts !Event [UI] [String] [TaskResult a] Int -> UIChange
genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
	= case event of
		ResetEvent
			= ReplaceUI (uiac UIContainer (classAttr [className]) ([def \\ ValueResult _ _ (ReplaceUI def) _ <- results] ++ actions))
		_
			# (idx,iChanges) = itemChanges 0 prevNumBranches results
			# aChanges       = actionChanges idx
			= ChangeUI [] (iChanges ++ aChanges)
where
	className = if (actions =: []) "parallel" "parallel-actions"

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
genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[]} results
where
    addResult (ValueResult _ i1 _ _) i2
        # lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
        # removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
        = {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks}
    addResult _ i = i

readListId :: (SharedTaskList a) *IWorld -> (MaybeError TaskException TaskId,*IWorld) | TC a
readListId slist iworld = case read (sdsFocus fullTaskListFilter slist) EmptyContext iworld of
	(Ok e,iworld)	= (Ok (fst (directResult e)), iworld)
	(Error e, iworld)	    = (Error e, iworld)

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval _ iworld=:{current={taskTime}}
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (mbListId,iworld)
		# listId = fromOk mbListId
		//Check if someone is trying to add an embedded task to the topLevel list
		| listId == TaskId 0 0 && parType =:(Embedded)
			= (Error (exception "Embedded tasks can not be added to the top-level task list"),iworld)
		# (mbStateMbTask, iworld) = initParallelTask mkEvalOpts listId parType parTask iworld
		= case mbStateMbTask of
			Ok (state,mbTask)
				# taskId = state.TaskMeta.taskId
				| listId == TaskId 0 0 //For the top-level list, we don't need to do anything else
					= (Ok taskId, iworld)
			  	//Update the task list
				# (mbError,iworld)    =  modify (\(_,states) -> states ++ [state]) (sdsFocus (listId,taskId,fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
				| mbError =:(Error _) = (liftError mbError,iworld)
				//If the task is an embedded one, we also need to store the task function
				| mbTask =:(Just _)
					# (mbError,iworld) = (write (fromJust mbTask @? encodeTaskValue) (sdsFocus (listId,taskId) taskInstanceParallelTaskListTask) EmptyContext iworld)
					| mbError =:(Error _) = (liftError mbError,iworld)
					= (Ok taskId, iworld)
				= (Ok taskId, iworld)
			err = (liftError err, iworld)
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task () | TC a
removeTask removeId=:(TaskId instanceNo taskNo) slist = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
		# listId = fromOk mbListId
		//If we are removing from the top-level task list, just remove the instance
		| listId == TaskId 0 0
			# (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
			= (ValueResult
				(Value () True)
				(mkTaskEvalInfo lastEval)
				(mkUIIfReset event (ui UIEmpty))
				(return ()), iworld)
		//Mark the task as removed, and update the indices of the tasks afterwards
		# (mbError,iworld)      = modify (markAsRemoved removeId o snd) (sdsFocus (listId,listId,fullTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
		| mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
		//If it is a detached task, remove the detached instance, if it is embedded, pass notify the currently evaluating parallel
		| taskNo == 0 //(if the taskNo equals zero the instance is embedded)
			# (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
			= (ValueResult (Value () True) {lastEvent=lastEval,removedTasks=[]} (mkUIIfReset event (ui UIEmpty)) (return ()), iworld)
		//Pass removal information up
		= (ValueResult (Value () True) {lastEvent=lastEval,removedTasks=[(listId,removeId)]} (mkUIIfReset event (ui UIEmpty)) (return ()), iworld)

	//When a task is marked as removed, the index of the tasks after that are decreased
	markAsRemoved removeId [] = []
	markAsRemoved removeId [s=:{TaskMeta|taskId}:ss]
		| taskId == removeId = [{TaskMeta|s & change = Just RemoveTask}:ss]
		| otherwise          = [s:markAsRemoved removeId ss]

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask replaceId=:(TaskId instanceNo taskNo) parTask slist = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
		# listId = fromOk mbListId
		//Replace the full instance task
		| listId == TaskId 0 0
			= case replaceTaskInstance instanceNo (parTask (sdsTranslate "setTopLevel" id topLevelTaskList)) iworld of
				(Ok (), iworld)
					= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (return ()), iworld)
				(Error e, iworld)
					= (ExceptionResult e,iworld)
		//If it is a detached task, replacee the detached instance, if it is embedded schedule the change in the parallel task state
		| taskNo == 0 //(if the taskNo equals zero the instance is embedded)
			= case replaceTaskInstance instanceNo (parTask (sdsTranslate "setTopLevel" id topLevelTaskList)) iworld of
				(Ok (), iworld)
					= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (return ()), iworld)
				(Error e, iworld)
					= (ExceptionResult e,iworld)
		//Schedule the change in the parallel task state
		# task                  = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
		# taskListFilter        = {fullTaskListFilter & onlyTaskId=Just [replaceId],includeValue=True,includeTaskAttributes=True,includeProgress=True}
		# (mbError,iworld)      = modify (scheduleReplacement replaceId task o snd) (sdsFocus (listId,listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
		| mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
		= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (return ()), iworld)

	scheduleReplacement replaceId task [item] = [{TaskMeta|item & change = Just (ReplaceTask (dynamic task :: Task a^))}]

attach :: !InstanceNo !Bool -> Task AttachmentStatus
attach instanceNo steal = Task evalinit
where
	evalinit DestroyEvent _ iworld = (DestroyedResult, iworld)
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld=:{current={attachmentChain}}
		# (mbMeta,iworld) = read (sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| mbMeta =: (Error _)  = (ExceptionResult (fromError mbMeta),iworld)
		# (Ok (ReadingDone meta=:{TaskMeta|build,instanceKey,status,attachedTo})) = mbMeta
		//Check if the task is already in use
		| (not (attachedTo =: [])) && (not steal)
			= eval (ASInUse (hd attachedTo)) build instanceKey event evalOpts  iworld
		//Take over the instance. We generate a new key, so the other instance will no longer have access
		# (newKey,iworld) = newInstanceKey iworld
        # meta = {TaskMeta|meta & instanceKey = Just newKey, attachedTo = [taskId:attachmentChain]}
		# (_,iworld)	= write meta (sdsFocus (instanceNo,False,True) taskInstance) EmptyContext iworld
		//Clear all input and output of that instance
		# (_,iworld)    = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld 
		# (_,iworld)    = modify (\('DQ'.Queue a b) -> 'DQ'.Queue [(i,e) \\(i,e)<- a| i <> instanceNo][(i,e) \\(i,e)<- b| i <> instanceNo]) taskEvents EmptyContext iworld 
		= eval (ASAttached (status =: (Right True))) build (Just newKey) event evalOpts iworld

	eval _ _ _ DestroyEvent evalOpts=:{TaskEvalOpts|taskId} iworld
		# iworld     = clearTaskSDSRegistrations ('DS'.singleton taskId) iworld
		# (_,iworld) = modify release (sdsFocus (instanceNo,False,True) taskInstance) EmptyContext iworld
        = (DestroyedResult, iworld)
	where
		release meta=:{TaskMeta|attachedTo=[t:_]}
			| t == taskId = {TaskMeta|meta & attachedTo=[]} //Only release if the instance is still attached to this 'attach' task
			              = meta
		release meta = meta

	eval prevStatus build instanceKey event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld=:{options={appVersion},current}
		//Load instance
		# tfilter = {fullTaskListFilter & onlyTaskId = Just [TaskId instanceNo 0], includeProgress = True, includeValue = True}
		# focus = (TaskId 0 0, TaskId current.TaskEvalState.taskInstance 0, tfilter, fullExtendedTaskListFilter)
		# (progress,iworld) = readRegister taskId (sdsFocus focus taskListMetaData) iworld
		//Determine state of the instance
		# curStatus = case progress of
			(Ok (ReadingDone (_,[progress=:{TaskMeta|attachedTo=[attachedId:_],status}])))
			    | build <> appVersion    = ASIncompatible
				| status =: (Left _)     = ASExcepted ("unable to read progress: " +++ fromLeft "" status)
				| attachedId <> taskId   = ASInUse attachedId	
				                         = ASAttached (status =: (Right True))
			_                            = ASDeleted
		//Determine UI change
		# change = determineUIChange event curStatus prevStatus instanceNo instanceKey
		# stable = (curStatus =: ASDeleted) || (curStatus =: ASExcepted _)
		= (ValueResult (Value curStatus stable)
			(mkTaskEvalInfo lastEval) change
			(Task (eval curStatus build instanceKey)), iworld)

	determineUIChange event curStatus prevStatus instanceNo instanceKey
		| curStatus === prevStatus && not (event =: ResetEvent) = NoChange
		| curStatus =: (ASInUse _)    = ReplaceUI inuse
		| curStatus =: (ASExcepted _)    = ReplaceUI exception
		| curStatus =: ASIncompatible || instanceKey =: Nothing = ReplaceUI incompatible
		| otherwise     		      = ReplaceUI viewport
	where
		inuse        = stringDisplay "This task is already in use"
		exception    = stringDisplay "An exception occurred in this task"
		incompatible = stringDisplay "This task can no longer be evaluated"
		viewport  =	(uia UIViewport ('DM'.unions [sizeAttr FlexSize FlexSize, instanceNoAttr instanceNo, instanceKeyAttr (fromJust instanceKey)]))

withCleanupHook :: (Task a) (Task b) -> Task b | iTask a & iTask b
withCleanupHook patch orig
	= appendTopLevelTask ('DM'.singleton "hidden" (JSONBool True)) False patch
	>>- \x->Task (eval x orig)
where
	eval tosignal (Task orig) DestroyEvent opts iw
		# (tr, iw) = orig DestroyEvent opts iw
		= (tr, queueRefresh tosignal iw)
	eval tosignal (Task orig) ev opts iw
		# (val, iw) = orig ev opts iw
		= (wrapTaskContinuation (eval tosignal) val, iw)
