implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.WF.Tasks.Core
import qualified iTasks.Internal.SDS as SDS
import StdString, Data.Func, Data.Error, StdBool
import qualified Data.Set as DS
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w
get sds = Task (eval sds)
where
	//Initial evaluation
	eval :: !(sds () a w) !Event !TaskEvalOpts !*IWorld -> *(TaskResult a, *IWorld) | iTask a & Readable sds & TC w
	eval _ DestroyEvent _ iworld = (DestroyedResult, iworld)

	eval sds event {TaskEvalOpts|taskId,ts} iworld
		= case 'SDS'.read sds ('SDS'.TaskContext taskId) iworld of
			// Remote read is queued, enter AwaitRead state and show loading UI.
			(Ok (Reading newsds), iworld)
				= (ValueResult
					NoValue
					(tei ts)
					(ReplaceUI (uia UIProgressBar (textAttr "Getting data")))
					(Task (eval newsds))
				, iworld)
			(Ok (ReadingDone val), iworld)
				= (ValueResult
					(Value val True)
					(tei ts)
					(rep event)
					(treturn val)
				, iworld)
			(Error e, iworld) = (ExceptionResult e, iworld)

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds
set val sds = Task (eval val sds)
where
	eval :: a (sds () r a) Event TaskEvalOpts *IWorld -> (TaskResult a, !*IWorld) | iTask a & TC r & Writeable sds
	eval _ _ DestroyEvent _ iworld = (DestroyedResult, iworld)

	eval val sds event {TaskEvalOpts|taskId, ts} iworld
	# evalInfo = {lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
	= case 'SDS'.write val sds ('SDS'.TaskContext taskId) iworld of
		(Ok (Writing sds), iworld)
			= (ValueResult
				NoValue
				evalInfo
				(ReplaceUI (uia UIProgressBar (textAttr "Writing data")))
				(Task (eval val sds))
			, iworld)
		(Ok WritingDone, iworld)
			= (ValueResult
				(Value val True)
				(tei ts)
				(rep event)
				(treturn val)
			, iworld)
		(Error e, iworld) = (ExceptionResult e, iworld)

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd fun sds = Task (eval fun sds)
where
	eval :: (r -> w) (sds () r w) Event TaskEvalOpts *IWorld -> (TaskResult w, !*IWorld) | iTask r & iTask w & Modifiable sds
	eval fun shared DestroyEvent opts iworld
		= (DestroyedResult, iworld)

	eval fun sds event {TaskEvalOpts|taskId,ts} iworld
		= case 'SDS'.modify fun sds ('SDS'.TaskContext taskId) iworld of
			(Ok (Modifying sds _), iworld)
				= (ValueResult
					NoValue
					(tei ts)
					(ReplaceUI (uia UIProgressBar (textAttr "Getting data")))
					(Task (eval fun sds))
				, iworld)
			(Ok (ModifyingDone val), iworld)
				= (ValueResult
					(Value val True)
					(tei ts)
					(rep event)
					(treturn val)
				, iworld)
			(Error e, iworld) = (ExceptionResult e, iworld)

watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds
watch origsds = Task (eval origsds NoValue origsds)
where
	eval :: (sds1 () r w) (TaskValue r) (sds2 () r w) Event TaskEvalOpts *IWorld
		-> (TaskResult r, !*IWorld) | iTask r & TC w & Readable, Registrable sds1 & Readable, Registrable sds2
	eval _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld
		= (DestroyedResult,iworld)

	eval origsds oldtv sds event {TaskEvalOpts|taskId,ts} iworld
		| not (isRefreshForTask event taskId)
			= (ValueResult oldtv (tei ts) NoChange (Task (eval origsds oldtv sds)), iworld)
		= case 'SDS'.readRegister taskId sds iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (ReadingDone val), iworld)
				= (ValueResult
					(Value val False)
					(tei ts)
					(rep event)
					//Reading is done, go back to the original sds
					(Task (eval origsds (Value val False) origsds))
				, iworld)
			(Ok (Reading sds), iworld)
				= (ValueResult
					oldtv
					(tei ts)
					(rep event)
					//Reading is not done, use sds reduct
					(Task (eval origsds oldtv sds))
				, iworld)

tei ts = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}

rep ResetEvent  = ReplaceUI (ui UIEmpty)
rep _ 			= NoChange

isRefreshForTask (RefreshEvent taskIds _) taskId = 'DS'.member taskId taskIds
isRefreshForTask ResetEvent _ = True
isRefreshForTask _ _ = False
