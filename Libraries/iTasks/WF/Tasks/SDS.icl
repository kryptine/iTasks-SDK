implementation module iTasks.WF.Tasks.SDS

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
watch shared = Task (wrapOldStyleTask (eval shared))
where
	eval :: (sds () r w) Event TaskEvalOpts TaskTree *IWorld
		-> (TaskResult` r, !*IWorld) | iTask r & TC w & Readable, Registrable sds
	eval _ DestroyEvent _ ttree iworld=:{sdsEvalStates}
		# taskId = fromOk $ taskIdFromTaskTree ttree
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ taskId) iworld
		# iworld = {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates}
		= (DestroyedResult`,iworld)

	eval shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e, iworld) = (ExceptionResult` e, iworld)
		(Ok (ReadingDone val), iworld)
			# tree = TCBasic taskId ts (DeferredJSON val) False
			= (ValueResult` (Value val False) (tei ts) (rep event) tree, iworld)
		(Ok (Reading sds), iworld)
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
			# result = ValueResult` NoValue (tei ts) (rep event) (TCAwait Read taskId ts tree)
			= (result, {iworld & sdsEvalStates = sdsEvalStates})

	eval shared event _ tree=:(TCBasic taskId ts val stable) iworld=:{sdsEvalStates}
	| not (isRefreshForTask event tree) = case fromDeferredJSON val of
		Nothing = (ExceptionResult` (exception "Corrupt task result"), iworld)
		Just v = (ValueResult` (Value v False) (tei ts) NoChange tree, iworld)
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e, iworld) = (ExceptionResult` e, iworld)
		(Ok (ReadingDone val), iworld)
			# tree = TCBasic taskId ts (DeferredJSON val) False
			= (ValueResult` (Value val False) (tei ts) (rep event) tree, iworld)
		(Ok (Reading sds), iworld) = case fromDeferredJSON val of
			Nothing = (ExceptionResult` (exception "Corrupt task result"), iworld)
			Just v
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
			# result = ValueResult` (Value v False) (tei ts) (rep event) (TCAwait Read taskId ts tree)
			= (result, {iworld & sdsEvalStates = sdsEvalStates})

	eval _ event=:(RefreshEvent taskIds reason) _ tree=:(TCAwait Read taskId ts subtree) iworld=:{sdsEvalStates}
	# oldValue = case subtree of
		(TCInit _ _) = NoValue
		(TCBasic _ _ val _) = case fromDeferredJSON val of
			Nothing = NoValue
			Just v = Value v False
	| not (isRefreshForTask event tree) = (ValueResult` oldValue (tei ts) NoChange tree, iworld)
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult` (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		Just val = case val iworld of
			(Error e, iworld) = (ExceptionResult` e, iworld)
			(Ok (res :: AsyncRead r^ w^), iworld) = case res of
				ReadingDone v
					# sdsEvalStates = 'DM'.del taskId sdsEvalStates
					# result = ValueResult` (Value v False) (tei ts) NoChange (TCBasic taskId ts (DeferredJSON v) False)
					= (result, {iworld & sdsEvalStates = sdsEvalStates})
				Reading sds
					# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
					# result = ValueResult` oldValue (tei ts) NoChange tree
					= (result, {iworld & sdsEvalStates = sdsEvalStates})

tei ts = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}

rep ResetEvent  = ReplaceUI (ui UIEmpty)
rep _ 			= NoChange

isRefreshForTask (RefreshEvent taskIds _) tree = 'DS'.member (fromOk (taskIdFromTaskTree tree)) taskIds
isRefreshForTask ResetEvent _ = True
isRefreshForTask _ _ = False
