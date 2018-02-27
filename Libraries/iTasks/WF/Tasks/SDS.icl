implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import qualified iTasks.Internal.SDS as SDS
import StdString
import StdMisc

import Data.Maybe
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(ReadWriteShared a w) -> Task a | iTask a & TC w
get shared = Task eval
where
	 // The first time get is evaluated
	eval event opts (TCInit taskId ts) w = case 'SDS'.read shared 'SDS'.EmptyContext w of
		// Remote read is queued, enter AwaitRead state 
		(Ok 'SDS'.Queued, w) 		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} NoChange (TCAwaitRead taskId ts), w)

		// Remote read not necessary, return result directly.
		(Ok ('SDS'.Result val), w) 	= (ValueResult (Value val True) {TaskEvalInfo|lastEvent=ts,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) (TCInit taskId ts), w)
		(Error e, w) 				= (ExceptionResult e, w)

	// The task is awaiting the result of a read operation 
	eval event opts s=:(TCAwaitRead taskId time) w=:{IWorld| ioStates}  = case 'DM'.get taskId ioStates of
		Nothing 				= (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
		(Just (IOActive connectionMap)) 	= case 'DM'.toList connectionMap of
			[] 					= (ExceptionResult (exception "Expected at least one connection for this instance"), w) 
			[(id, (val :: a^, True))]	= (ValueResult (Value val True) {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) (TCInit taskId time), w)
			[(id, (val :: a^, False))]  = (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
			[x : xs] 			= (ExceptionResult (exception "Expected a single connection for reading a remote SDS"), w)

	eval event opts tree=:(TCDestroy _) w = (DestroyedResult, w)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & TC r
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (res,iworld)	='SDS'.write val shared iworld
		= case res of
			Ok _	= (Ok val, iworld)
			Error e	= (Error e, iworld)

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd fun shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (er, iworld)	= 'SDS'.read shared 'SDS'.EmptyContext iworld
		= case er of
			Error e		= (Error e, iworld)
			Ok ('SDS'.Result r)	
				# w				= fun r
				# (er, iworld)	=  'SDS'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error e, iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r & TC w
watch shared = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok ('SDS'.Result val)		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty) 
	rep _ 			= NoChange