implementation module iTasks.Internal.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdString
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.GenJSON
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.SDS, iTasks.Internal.AsyncSDS
import iTasks.Internal.TaskIO
import iTasks.Internal.Store, iTasks.Internal.Util
import iTasks.UI.Definition, iTasks.UI.Layout
import iTasks.Internal.SDSService
import iTasks.Internal.Util
import iTasks.Internal.EngineTasks

from iTasks.WF.Combinators.Core import :: SharedTaskList
import iTasks.WF.Derives
from iTasks.WF.Combinators.Core import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del
from Data.Queue import :: Queue (..)
from Data.Queue as DQ					import qualified newQueue, enqueue, dequeue, empty

import qualified iTasks.Internal.SDS as SDS
from iTasks.SDS.Combinators.Common      import sdsFocus, >*|, mapReadWrite, mapReadWriteError
from StdFunc import const, o

derive gEq TaskMeta, InstanceType, TaskChange

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
	{ TaskEvalOpts
	| noUI     = False
	, taskId   = TaskId 0 0
	, lastEval = 0
	}

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
	= (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)
processEvents max iworld
	| max <= 0 = (Ok (), iworld)
	| otherwise
		= case dequeueEvent iworld of
			(Error e, iworld) = (Error e, iworld)
			(Ok Nothing, iworld) = (Ok (), iworld)
			(Ok (Just (instanceNo,event)), iworld)
				= case evalTaskInstance instanceNo event iworld of
					(Ok taskValue,iworld)
						= processEvents (max - 1) iworld
					(Error msg,iworld=:{IWorld|world})
						= (Ok (),{IWorld|iworld & world = world})

evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue DeferredJSON),!*IWorld)
evalTaskInstance instanceNo event iworld
	# iworld            = mbResetUIState instanceNo event iworld
	# (res,iworld)      = evalTaskInstance` instanceNo event iworld
	= (res,iworld)
where
	evalTaskInstance` instanceNo event iworld=:{current}
	// Read the task reduct. If it does not exist, the task has been deleted.
	# (curReduct, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceTask) EmptyContext iworld
	| isError curReduct			= exitWithException instanceNo ((\(Error (e,msg)) -> msg) curReduct) iworld
	# curReduct=:(Task eval)    = directResult (fromOk curReduct)
	// Determine the task type (startup,session,local) 
	# (type,iworld)             = determineInstanceType instanceNo iworld
	// Determine the progress of the instance
	# (curProgress=:{TaskMeta|nextTaskTime,nextTaskNo,status,attachedTo},iworld) = determineInstanceProgress instanceNo iworld
	//Check exception
	| status =: (Left _) = let (Left message) = status in exitWithException instanceNo message iworld
	//Evaluate instance
    # (currentSession,currentAttachment) = case (type,attachedTo) of
        (SessionInstance,_)                       = (Just instanceNo,[])
        (_,[])                                    = (Nothing,[])
        (_,attachment=:[TaskId sessionNo _:_])    = (Just sessionNo,attachment)
	//Update current process id & eval stack in iworld
	# taskId = TaskId instanceNo 0
	# iworld =
		{iworld & current =
			{ taskInstance = instanceNo
			, sessionInstance = currentSession
			, attachmentChain = currentAttachment
			, taskTime = nextTaskTime
			, nextTaskNo = nextTaskNo
		}}
	//Apply task's eval function and take updated nextTaskId from iworld
	//the 'nextTaskNo' is possibly incremented during evaluation and we need to store it
	# (newResult,iworld=:{current=current=:{TaskEvalState|nextTaskNo}})
		= eval event {mkEvalOpts & lastEval=nextTaskTime, taskId=taskId} iworld
	# newTask = case newResult of
		(ValueResult _ _ _ newTask) = newTask
		_                           = Task eval
	# newValue = case newResult of
		ValueResult val _ _ _   = val
		ExceptionResult (e,str) = NoValue
		DestroyedResult         = NoValue
	# destroyed = newResult =: DestroyedResult
	//Reset necessary 'current' values in iworld
	# iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
	//Read unsynced cookies from meta-data //TODO: we should be able to this during updateTaskState, but SDS.modify can't return a value anymore
	# (mbErr,iworld) = if destroyed
		(Ok [],iworld)
		(readUnsyncedCookies instanceNo iworld)
	| mbErr=:(Error _)
		# (Error (_,description)) = mbErr
		= exitWithException instanceNo description iworld
	# syncCookies = fromOk mbErr
	//Write the updated state, or cleanup
	# (mbErr,iworld) = if destroyed
		(cleanupTaskState instanceNo iworld)
		(updateTaskState instanceNo newResult newTask newValue nextTaskNo nextTaskTime iworld)
	| mbErr=:(Error _)
		# (Error (_,description)) = mbErr
		= exitWithException instanceNo description iworld
	= case newResult of
		ValueResult value _ change _
			| destroyed = (Ok value,iworld)
			| isNothing currentSession = (Ok value, iworld)
			| otherwise = case (compactUIChange change, syncCookies) of
				//Only queue output if something interesting is changed
				(NoChange,[]) = (Ok value,iworld)
				(change,_) = (Ok value, queueOutput instanceNo [TOUIChange change:[TOSetCookie k v ttl \\ (k,v,ttl) <- reverse syncCookies]] iworld)
		ExceptionResult (e,description)
			# iworld = if (type =: StartupInstance)
				(iShowErr [description] {iworld & shutdown=Just 1})
				 iworld
			= exitWithException instanceNo description iworld
		DestroyedResult
			= (Ok NoValue, iworld)

	readUnsyncedCookies instanceNo iworld
		# (mbErr,iworld) = read (sdsFocus (instanceNo,False,True) taskInstance) EmptyContext iworld
		= (fmap ((\{TaskMeta|unsyncedCookies} -> unsyncedCookies) o directResult) mbErr, iworld)

	updateTaskState instanceNo newResult newTask newValue nextTaskNo nextTaskTime iworld=:{clock}
		//Store progress
		# (mbErr,iworld) = modify (updateProgress clock newResult nextTaskNo nextTaskTime) (sdsFocus (instanceNo,False,True) taskInstance) EmptyContext iworld
		| mbErr =: (Error _) = (liftError mbErr, iworld)
		//Store reduct
		# (mbErr,iworld) = write newTask (sdsFocus instanceNo taskInstanceTask) EmptyContext iworld
		| mbErr =: (Error _) = (liftError mbErr, iworld)
		//Store value
		# (mbErr,iworld) = write newValue (sdsFocus instanceNo taskInstanceValue) EmptyContext iworld
		| mbErr =: (Error _) = (liftError mbErr, iworld)
		= (Ok (),iworld)

	cleanupTaskState instanceNo iworld
		//Remove local shares
		# (mbErr,iworld) = write Nothing (sdsFocus instanceNo taskInstanceShares) EmptyContext iworld
		| mbErr =: (Error _) = (liftError mbErr, iworld)
		//Remove residual queued output
		# (mbErr,iworld) = modify (\output -> 'DM'.del instanceNo output) taskOutput EmptyContext iworld
		| mbErr =: (Error _) = (liftError mbErr, iworld)
		= (Ok (),iworld)

	exitWithException instanceNo description iworld
		# iworld = queueException instanceNo description iworld
		= (Error description, iworld)

	determineInstanceType instanceNo iworld
		# (meta, iworld) = 'SDS'.read (sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| isError meta = (SessionInstance,iworld)
		# {TaskMeta|instanceType} = directResult (fromOk meta)
		= (instanceType,iworld)

	determineInstanceProgress instanceNo iworld
		# (meta,iworld)      = 'SDS'.read (sdsFocus (instanceNo,False,False) taskInstance) EmptyContext iworld
		| isError meta       = ({defaultValue & nextTaskNo=1, nextTaskTime=1},iworld)
		= (directResult (fromOk meta),iworld)

	updateProgress now result nextTaskNo nextTaskTime meta
		# attachedTo = case meta.TaskMeta.attachedTo of //Release temporary attachment after first evaluation
			(Just (_,[]))   = Nothing
			attachment      = attachment
		# status = case result of
			(ExceptionResult (_,msg))             = Left msg
			(ValueResult (Value _ stable) _  _ _) = Right stable
			_                                     = Right False
		# taskAttributes = case result of
			(ValueResult _ _ change _) = foldr applyUIAttributeChange meta.TaskMeta.taskAttributes $ getAttributeChanges change
			_                          = meta.TaskMeta.taskAttributes
		= {TaskMeta| meta
			& status = status
			// The equivalent expression `Just (fromMaybe now meta.TaskMeta.firstEvent)` causes a memory leak!
			, firstEvent = maybe (Just now) Just meta.TaskMeta.firstEvent
			, lastEvent = Just now
			, nextTaskNo = nextTaskNo
			, nextTaskTime = nextTaskTime + 1
			, taskAttributes = taskAttributes
			, unsyncedCookies = []
			}
	where
		getAttributeChanges :: !UIChange -> [UIAttributeChange]
		getAttributeChanges NoChange = []
		getAttributeChanges (ChangeUI changes _) = changes
		getAttributeChanges (ReplaceUI (UI _ attrs _)) = [SetAttribute attr val \\ (attr,val) <- 'DM'.toList attrs]

	mbResetUIState instanceNo ResetEvent iworld
		# (_,iworld) = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld
		= iworld

	mbResetUIState _ _ iworld = iworld

currentInstanceShare :: SDSSource () InstanceNo ()
currentInstanceShare =: createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
