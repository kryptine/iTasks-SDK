implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import qualified iTasks.Internal.SDS as SDS
import StdString, Data.Func, Data.Error
import qualified Data.Set as DS
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w
get shared = Task (eval shared)
where
	 // The first time get is evaluated
	 // We pass the SDS for typing reasons
	eval :: (sds () a w) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult a, !*IWorld) | TC w & TC a & Readable sds & iTask a
	eval shared event opts tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False}
	= case 'SDS'.read shared ('SDS'.TaskContext taskId) iworld of
		// Remote read is queued, enter AwaitRead state and show loading UI.
		(Ok (AsyncRead sds), iworld)
			# ui = ReplaceUI (uia UIProgressBar (textAttr "Getting data"))
			# newState = TCAwait Read taskId ts tree
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.read sds ('SDS'.TaskContext taskId))) sdsEvalStates
			= (ValueResult NoValue  evalInfo ui newState, {iworld & sdsEvalStates = sdsEvalStates})

		// Remote read not necessary, return result directly.
		(Ok (ReadResult val _), iworld) = (ValueResult (Value val True) evalInfo (ReplaceUI (ui UIEmpty)) (TCStable taskId ts (DeferredJSON val)), iworld)

		(Error e, iworld) 				= (ExceptionResult e, iworld)

	// The task is awaiting the result of a read operation 
	eval shared event opts tree=:(TCAwait Read taskId time subtree) iworld=:{IWorld|sdsEvalStates}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: ReadResult () a^ w^), iworld)
			# evalInfo = {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False}
			= case res of
				(ReadResult val _) = (ValueResult (Value val True) evalInfo (ReplaceUI (ui UIEmpty)) subtree, iworld)
				(AsyncRead sds) 
				# ui = NoChange
				# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.read sds ('SDS'.TaskContext taskId))) sdsEvalStates
				= (ValueResult NoValue evalInfo ui tree, {iworld & sdsEvalStates = sdsEvalStates})

	eval _ event opts s=:(TCStable taskId ts enc) iworld 
	= case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	eval _ event opts tree=:(TCDestroy _) w = (DestroyedResult, w)

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds
set val shared = Task (eval val shared)
where
	eval :: a (sds () r a) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult a, !*IWorld) | iTask a & TC r & Writeable sds
	eval _ _ event _ tree=:(TCDestroy _) iworld = (DestroyedResult, iworld)

	eval val shared event _ tree=:(TCAwait Write taskId ts st) iworld=:{sdsEvalStates}
	# evalInfo = {lastEvent=ts,removedTasks=[],refreshSensitive=False}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just f) = case f iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncWrite () r^ a^), iworld) = case res of
				Done = (ValueResult (Value val True) evalInfo (ReplaceUI (ui UIEmpty)) (TCStable taskId ts (DeferredJSON val)), iworld)
				(Writing sds) = (ValueResult NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.write val sds ('SDS'.TaskContext taskId))) sdsEvalStates})

	eval val shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	# evalInfo = {lastEvent=ts,removedTasks=[],refreshSensitive=False}
	= case 'SDS'.write val shared ('SDS'.TaskContext taskId) iworld of
		(Error e, iworld) 		= (ExceptionResult e, iworld)
		(Ok (Writing sds), iworld) 
			# ui = ReplaceUI (uia UIProgressBar (textAttr "Writing data"))
			# tree = TCAwait Write taskId ts (TCInit taskId ts)
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.write val sds ('SDS'.TaskContext taskId))) sdsEvalStates
			= (ValueResult NoValue evalInfo ui tree, {iworld & sdsEvalStates = sdsEvalStates})
		(Ok Done, iworld) 			= (ValueResult (Value val True) evalInfo (rep event) (TCStable taskId ts (DeferredJSON val)), iworld)

	eval val shared event _ s=:(TCStable taskId ts enc) iworld = case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd fun shared = Task (eval fun shared)
where
	eval :: (r -> w) (sds () r w) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult w, !*IWorld) | iTask r & iTask w & RWShared sds
	eval fun shared event _ tree=:(TCDestroy _) w = (DestroyedResult, w)
	eval fun shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates} 
	# evalInfo = {lastEvent=ts,removedTasks=[],refreshSensitive=False}
	= case 'SDS'.modify fun shared ('SDS'.TaskContext taskId) iworld of 
		(Error e, iworld) 						= (ExceptionResult e, iworld)
		(Ok (ModifyResult r w _), iworld)		= (ValueResult (Value w True) evalInfo  (rep event) (TCStable taskId ts (DeferredJSON w)), iworld)
		(Ok (AsyncModify sds _), iworld) 			
			# ui = ReplaceUI (uia UIProgressBar (textAttr "Getting data"))
			# tree = TCAwait Modify taskId ts (TCInit taskId ts)
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modify fun sds ('SDS'.TaskContext taskId))) sdsEvalStates
		= (ValueResult NoValue evalInfo ui tree, {iworld & sdsEvalStates = sdsEvalStates})

	eval fun shared event _ tree=:(TCAwait Modify taskId ts _) iworld=:{sdsEvalStates} =  case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: ModifyResult () r^ w^), iworld) = case res of
				(ModifyResult r w _) = (ValueResult (Value w True) {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) tree, iworld)
				(AsyncModify sds f) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modifySDS f sds () ('SDS'.TaskContext taskId))) sdsEvalStates})

	eval fun shared event _ s=:(TCStable taskId ts enc) iworld = case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds
watch shared = Task (eval shared)
where
	eval :: (sds () r w) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult r, !*IWorld) | iTask r & TC w & Readable, Registrable sds
	eval shared event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok (ReadResult val _)		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)

	eval shared event repAs ttree=:(TCDestroy _) iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree ttree) iworld
		= (DestroyedResult,iworld)

rep ResetEvent  = ReplaceUI (ui UIEmpty) 
rep _ 			= NoChange
