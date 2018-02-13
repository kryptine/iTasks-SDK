implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import qualified iTasks.Internal.SDS as SDS
import qualified iTasks.Internal.AsyncSDS as ASDS
import StdString, StdInt
import StdMisc
import Text.GenJSON

import Data.Maybe
import Data.Either
import qualified Data.Map as DM

from iTasks.Internal.Serialization    import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds
get shared = Task eval
where
	 // The first time get is evaluated
	eval event opts tree=:(TCInit taskId ts) w = case 'SDS'.read shared ('SDS'.TaskContext taskId) w of
		// Remote read is queued, enter AwaitRead state 
		(Ok ('SDS'.Queued connectionId), w) 		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Read taskId connectionId ts tree), w)

		// Remote read not necessary, return result directly.
		(Ok ('SDS'.Result val), w) 	= (ValueResult (Value val True) {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) tree, w)
		(Error e, w) 				= (ExceptionResult e, w)

	// The task is awaiting the result of a read operation 
	eval event opts s=:(TCAwait Read taskId connectionId time tree) w=:{IWorld| ioStates}  = case 'DM'.get taskId ioStates of
		Nothing 				= (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
		(Just (IOActive connectionMap)) 	= case 'DM'.get connectionId connectionMap of
			(Just (val :: Either [String] a^, _)) = case val of
				(Left _ ) 	= (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
				(Right val) 	= (ValueResult (Value val True) {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) tree, w)
			Nothing = (ExceptionResult (exception ("Expected a connection for id " +++ (toString connectionId))), w)

	eval event opts s=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	eval event opts tree=:(TCDestroy _) w = (DestroyedResult, w)

	eval _ _ _ _ = abort "get does not match!"

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writable sds
set val shared = Task eval
where
	eval event opts tree=:(TCDestroy _) w = (DestroyedResult, w)

	eval event opts tree=:(TCInit taskId ts) w = case 'SDS'.write val shared ('SDS'.TaskContext taskId) w of
		(Error e, w) 		= (ExceptionResult e, w)
		(Ok (), w) 			= (ValueResult (Value val True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSON val)), w)

	eval event opts s=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	eval _ _ _ _ = abort "set does not match!"

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & Readable, Writable sds
upd fun shared = Task eval
where
	eval event opts tree=:(TCDestroy _) w = (DestroyedResult, w)
	eval event opts tree=:(TCInit taskId ts) iworld = case 'SDS'.modify fun shared ('SDS'.TaskContext taskId) iworld of 
		(Error e, iworld) 						= (ExceptionResult e, iworld)
		(Ok ('SDS'.Result written), iworld)		= (ValueResult (Value written True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) (TCStable taskId ts (DeferredJSON written)), iworld)
		(Ok ('SDS'.Queued connectionId), iworld) = (ValueResult NoValue {lastEvent=ts,removedTasks=[],refreshSensitive=False} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Modify taskId connectionId ts (TCInit taskId ts)), iworld)

	eval event opts tree=:(TCAwait Modify taskId connectionId ts _) w=:{ioStates} = case 'ASDS'.getAsyncWriteValue shared taskId connectionId ioStates of
		Left error 			= (ExceptionResult (exception error), w)
		Right Nothing		= (ValueResult NoValue {lastEvent=ts,removedTasks=[],refreshSensitive=False} NoChange tree, w)
		Right (Just val) 	= (ValueResult (Value val True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (ReplaceUI (ui UIEmpty)) (TCStable taskId ts (DeferredJSON val)), w)

	eval event opts s=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],refreshSensitive=False} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	eval _ _ tree _ = abort ("upd does not match!" +++ toString (toJSON tree))

watch :: !(sds () r w) -> Task r | iTask r & Readable, Registrable sds
watch shared = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok ('SDS'.Result val)		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)

	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	eval _ _ _ _ = abort "watch does not match!"

rep ResetEvent  = ReplaceUI (ui UIEmpty) 
rep _ 			= NoChange

// TODO: Duplicate
fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json
