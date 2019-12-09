implementation module iTasks.Internal.EngineTasks

import Data.Error
import Data.Queue
import StdEnv
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskIO
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskState
import iTasks.Internal.Util
import iTasks.SDS.Combinators.Common
import iTasks.UI.Definition
import iTasks.WF.Definition
import Text

from Data.Map import newMap, member, del

derive gText TaskId, TaskListFilter
derive JSONEncode TaskId, TaskListFilter
derive gDefault TaskId, TaskListFilter

everyTick :: (*IWorld -> *(MaybeError TaskException (), *IWorld)) -> Task ()
everyTick f = Task eval
where
	eval DestroyEvent evalOpts iworld
		= (DestroyedResult, iworld)
	eval event {taskId,lastEval} iworld
		# (merr, iworld) = f iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		# (merr, iworld) = readRegister taskId tick iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		= (ValueResult
				NoValue
				(mkTaskEvalInfo lastEval)
				NoChange
				(Task eval)
			, iworld)
	
//When we run the built-in HTTP server we need to do active garbage collection of instances that were created for sessions
removeOutdatedSessions :: Task ()
removeOutdatedSessions = everyTick \iworld=:{IWorld|options} ->
	case read (sdsFocus (TaskId 0 0,TaskId 0 0, defaultValue, onlySessions) taskListMetaData) EmptyContext iworld of
		(Ok (ReadingDone (_,index)), iworld) = checkAll (removeIfOutdated options) index iworld
		(Error e, iworld)                = (Error e, iworld)
where
	onlySessions = {ExtendedTaskListFilter|defaultValue &includeSessions=True,includeDetached=False,includeStartup=False}

	checkAll f [] iworld = (Ok (),iworld)
	checkAll f [x:xs] iworld = case f x iworld of
		(Ok (),iworld) = checkAll f xs iworld
		(Error e,iworld) = (Error e,iworld)

    removeIfOutdated options {TaskMeta|taskId=TaskId instanceNo _,connectedTo,lastIO,build,createdAt} iworld=:{options={appVersion},clock=tNow}
		| if (lastIO =:(Just _))
			(tNow - fromJust lastIO > options.EngineOptions.sessionTime)
			((build <> appVersion) || ((tNow - createdAt) > options.EngineOptions.sessionTime))
				# (e,iworld) = deleteTaskInstance instanceNo iworld
				| e=:(Error _) = (e,iworld)
				# (e,iworld) = modify (\output -> del instanceNo output) taskOutput EmptyContext iworld
				| e=:(Error _) = (liftError e,iworld)
				= (Ok (),iworld)		
		| otherwise
			= (Ok (), iworld)

//When the event queue is empty, write deferred SDS's
flushWritesWhenIdle:: Task ()
flushWritesWhenIdle = everyTick \iworld->case read taskEvents EmptyContext iworld of
		(Error e,iworld)          = (Error e,iworld)
		(Ok (ReadingDone (Queue [] [])),iworld) = flushDeferredSDSWrites iworld
		(Ok _,iworld)             = (Ok (),iworld)

//When we don't run the built-in HTTP server we don't want to loop forever so we stop the loop
//once all non-system tasks are stable
stopOnStable :: Task ()
stopOnStable = everyTick \iworld->case read (sdsFocus selection taskListMetaData) EmptyContext iworld of
		(Ok (ReadingDone (_,index)), iworld)
			# iworld = if (isNothing iworld.shutdown && all isStable (filter (not o isSystem) index))
				{IWorld | iworld & shutdown=Just 0}
				iworld
			= (Ok (), iworld)
		(Ok _,iworld)
			= (Error (exception "Unexpeced SDS state"),iworld)
		(Error e, iworld)  = (Error e, iworld)
where
	selection = (TaskId 0 0, TaskId 0 0,{TaskListFilter|fullTaskListFilter & includeProgress=True}
		,{ExtendedTaskListFilter|fullExtendedTaskListFilter & includeStartup=True, includeSessions=False, includeDetached=False})

	isStable {TaskMeta|valuestatus} = valuestatus =: Stable
	isSystem {TaskMeta|taskAttributes} = member "system" taskAttributes

printStdErr :: v !*IWorld -> *IWorld | gText{|*|} v
printStdErr v iw=:{world}
	= {iw & world=snd (fclose (stderr <<< toSingleLineText v <<< "\n") world)}
