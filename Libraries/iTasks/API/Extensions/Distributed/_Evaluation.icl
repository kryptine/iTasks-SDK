implementation module iTasks.API.Extensions.Distributed._Evaluation

import iTasks
import iTasks._Framework.TaskState
import iTasks._Framework.IWorld
from iTasks._Framework.SDS as SDS import qualified read, readRegister, write

from iTasks.UI.Definition import :: UIChange(..), :: UINodeType(..), :: UIChildChange(..), ui
from iTasks._Framework.Store import memoryStore, :: StoreName, :: StoreNamespace

evalRemoteTask :: (Task a) ((TaskValue a) -> Task ()) -> Task a | iTask a
evalRemoteTask task handleValue
	= get currentTaskInstanceNo
	>>= \taskid -> let share = taskValueShare taskid in 
		(customEval share task ||- whileUnchanged share (changeTask handleValue))
where
	changeTask :: ((TaskValue a) -> Task ()) (TaskValue a) -> Task a | iTask a
	changeTask handleValue value=:(Value v True)
		= handleValue value @! v
	changeTask handleValue value
		= handleValue value @? const NoValue

proxyTask :: (RWShared () (TaskValue a) (TaskValue a)) (*IWorld -> *IWorld) -> (Task a) | iTask a
proxyTask value_share onDestroy = Task eval
        where
        eval event evalOpts tree=:(TCInit taskId ts) iworld=:{IWorld|ioTasks={done,todo},ioStates,world}
                # (val,iworld)  = 'SDS'.readRegister taskId value_share iworld
                = case val of
                      Ok val            = (ValueResult val {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) tree, iworld)
                      Error e           = (ExceptionResult e,iworld)
        eval event repAs (TCDestroy _) iworld 
                # iworld = onDestroy iworld
                = (DestroyedResult,iworld)

        rep ResetEvent = ReplaceUI (ui UIEmpty)
        rep _          = NoChange

taskValueShare :: Int ->  RWShared () (TaskValue a) (TaskValue a) | iTask a
taskValueShare taskid = sdsFocus store_name (memoryStore store_name (Just NoValue))
where
	store_name = "taskValueShare_" +++ (toString taskid) 

customEval :: (RWShared () (TaskValue a) (TaskValue a)) (Task a) -> (Task a) | iTask a
customEval value_share (Task eval) = Task eval`
        where
        eval` event evalOpts state iworld
                = case eval event evalOpts state iworld of
                        v=:(ValueResult value info rep tree, iworld) -> storeValue v
                        (ExceptionResult te, iworld) -> (ExceptionResult te, iworld)
                        (DestroyedResult, iworld) -> (DestroyedResult, iworld)

        storeValue (ValueResult task_value info rep tree, iworld)
                # (res, iworld) = 'SDS'.write task_value value_share iworld
                = case res of
                        Ok _    = (ValueResult task_value info rep tree, iworld)
                        Error _ = (ValueResult task_value info rep tree, iworld)

