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

import Data.Maybe
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = Task eval
where
	eval :: !(Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

	// The task is awaiting the result of a read operation 
	eval event opts s=:(TCAwaitRead id time readId) w=:{IWorld| ioStates}  = case  'DM'.get id ioStates of
		Nothing 				= (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
		(Just (IOActive connectionMap))) 	= case 'DM'.toList connectionMap of
			[] 					= (Error "Expected at least one connection for this instance", w) 
			[(d :: ^a, True)]	= (ValueResult (Value d True) {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} (ReplaceUI (ui UIEmpty)) (TCInit id time), w)
			[(d :: ^a, False)]  = (ValueResult NoValue {TaskEvalInfo|lastEvent=time,removedTasks=[], refreshSensitive=False} NoChange s, w)
			[x : xs] 			= (Error "Expected a single connection for reading a remote SDS", w)
		
 
 	// The firs time get is evaluated
	eval event opts state w = case read (Just (getTaskId state) shared w of
		// Remote read is queued, enter AwaitRead state 
		(Ok (Left id)), w) 		= (ValueResult NoValue) {TaskEvalInfo|lastEvent=getTime state,removedTasks=[], refreshSensitiveFalse} NoChange (TCAwaitRead (getTaskId state) time id)

		// Remote read not necessary, return result directly.
		(Ok (Right val), w) 	= (ValueResult (Value val True)) {TaskEvalInfo|lastEvent=getTime state,removedTasks=[], refreshSensitiveFalse} (ReplaceUI (ui UIEmpty)) (TCInit id time)
		(Error e, w) 			= (Error e, w)
	
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
		# (er, iworld)	= 'SDS'.read (Just taskId) shared iworld
		= case er of
			Error e		= (Error e, iworld)
			Ok (Just r)	
				# w				= fun r
				# (er, iworld)	=  'SDS'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error e, iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok (Just val)		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty) 
	rep _ 			= NoChange