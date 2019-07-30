implementation module iTasks.WF.Combinators.SDS

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.SDS.Definition
from iTasks.SDS.Combinators.Common import sdsFocus

import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
from iTasks.Internal.SDS import write, read, readRegister

from Data.Func import mapSt
import StdMisc

import StdTuple, StdArray, StdList, StdString
import Text, Text.GenJSON
import Data.Maybe, Data.Error, Data.Functor
import System.Directory, System.File, System.FilePath, Data.Error, System.OSError
import qualified Data.Map as DM

withShared :: !b !((SimpleSDSLens b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task evalinit
where
	//Initialization
	evalinit DestroyEvent _ iworld = (DestroyedResult,iworld)

	evalinit event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
		# (taskIda, iworld) = getNextTaskId iworld
		# (e, iworld)       = write initial (sdsFocus taskId localShare) EmptyContext iworld
		| isError e
			= (ExceptionResult (fromError e),iworld)
		= eval taskIda (stask (sdsFocus taskId localShare)) event evalOpts iworld

	//Running
	eval innerTaskId (Task inner) DestroyEvent opts iworld
		= case inner DestroyEvent {TaskEvalOpts|opts&taskId=innerTaskId} iworld of
			(ValueResult _ _ _ _, iworld)
				= (ExceptionResult (exception "Failed to destroy withShared child"), iworld)
			e = e

	eval innerTaskId (Task inner) event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
		= case inner event {TaskEvalOpts|evalOpts&taskId=innerTaskId} iworld of
			(ValueResult val info rep newinner, iworld)
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult
					val
					info
					rep
					(Task (eval innerTaskId newinner))
				, iworld)
			e = e

withTaskId :: (Task a) -> Task (a, TaskId)
withTaskId (Task eval) = Task eval`
where
	eval` event evalOpts=:{TaskEvalOpts|taskId} iworld
		= case eval event evalOpts iworld of
			(ValueResult (Value x st) info rep newtask, iworld)
				= (ValueResult (Value (x, taskId) st) info rep (withTaskId newtask), iworld)
			(ExceptionResult te, iworld) = (ExceptionResult te, iworld)
			(DestroyedResult, iworld) = (DestroyedResult, iworld)

withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a
withTemporaryDirectory taskfun = Task undef/*eval
//TODO: port
where
	eval DestroyEvent evalOpts (TCShared taskId ts treea) iworld=:{options={appVersion,tempDirPath}} //First destroy inner task
		# tmpDir 		= tempDirPath </> (appVersion +++ "-" +++ toString taskId +++ "-tmpdir")
		# (Task evala)	= taskfun tmpDir
		# (resa,iworld)	= evala DestroyEvent evalOpts treea iworld
		# (merr, world) = recursiveDelete tmpDir iworld.world
		# iworld        = {iworld & world = world}
		| isError merr  = (ExceptionResult (exception (fromError merr)), iworld)
		= (resa,iworld)
	eval DestroyEvent _ _ iworld
		= (DestroyedResult,iworld)

	eval event evalOpts (TCInit taskId ts) iworld=:{options={appVersion,tempDirPath}}
		# tmpDir 			= tempDirPath </> (appVersion +++ "-" +++ toString taskId +++ "-tmpdir")
		# (taskIda,iworld=:{world})	= getNextTaskId iworld
		# (ok ,world)		= ensureDirectoryExists tmpDir world
		| isOk ok
			= eval event evalOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & world = world}
		| otherwise
			= (ExceptionResult (exception ("Could not create temporary directory: " +++ tmpDir)) , {iworld & world = world})

	eval event evalOpts (TCShared taskId ts treea) iworld=:{options={appVersion,tempDirPath},current={taskTime},world}
		# tmpDir 			        = tempDirPath </> (appVersion +++ "-" +++ toString taskId +++ "-tmpdir")
        # (mbCurdir,world)          = getCurrentDirectory world
        | isError mbCurdir          = (ExceptionResult (exception (fromError mbCurdir)), {IWorld|iworld & world = world})
        # (mbErr,world)             = setCurrentDirectory tmpDir world
        | isError mbErr             = (ExceptionResult (exception (fromError mbErr)), {IWorld|iworld & world = world})
		# ts						= case event of
			(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
			_						= ts
		# (Task evala)			= taskfun tmpDir
		# (resa,iworld=:{world})	= evala event evalOpts treea {IWorld|iworld & world = world}
        # (_,world)                 = setCurrentDirectory (fromOk mbCurdir) world
        | isError mbErr             = (ExceptionResult (exception (fromError mbErr)), {IWorld|iworld & world = world})
		= case resa of
			ValueResult value info rep ntreea
				# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
				= (ValueResult value info rep (TCShared taskId info.TaskEvalInfo.lastEvent ntreea),{IWorld|iworld & world = world})
			ExceptionResult e = (ExceptionResult e,{IWorld|iworld & world = world})

	eval _ _ _ iworld
		= (ExceptionResult (exception "Corrupt task state in withShared"), iworld)
*/

instance toString (OSErrorCode,String) where toString x = snd x
