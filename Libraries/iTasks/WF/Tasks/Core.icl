implementation module iTasks.WF.Tasks.Core

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Prompt
import iTasks.SDS.Definition
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import qualified iTasks.Internal.SDS as SDS
import qualified iTasks.Internal.AsyncSDS as ASDS

import Data.Error, Data.Maybe, Data.Either
import Text.GenJSON
import StdString, StdBool, StdInt
import qualified Data.Set as DS
import qualified Data.Map as DM

import StdDebug

derive JSONEncode Event,Set

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (Ok (), {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= (Ok res, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|current={taskTime},world}
		# (res,world)	= fun world
		= case res of
			Error e
				# err = errf e		
				= (Error (dynamic err,toString err), {IWorld|iworld & world = world})	
			Ok v
				= (Ok v, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interact :: !d !EditMode !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
interact prompt mode shared handlers editor
=  Task (eval prompt mode shared handlers editor)
where
	eval :: !d !EditMode (sds () r w) (InteractionHandlers l r w v) (Editor v) Event TaskEvalOpts TaskTree *IWorld -> *(TaskResult (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
	eval _ _ _ _ _ event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	eval prompt mode shared handlers editor  (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Read taskId ts tree) iworld=:{sdsEvalStates, current={taskTime}} 
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) = case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: ReadResult () r^ w^), iworld) = case res of
				ReadResult r
					# (l, v) = handlers.onInit r
					# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
					= case editor.Editor.genUI [] v vst of
						(Error e, {VSt|iworld})		= (ExceptionResult (exception e), iworld)
						(Ok (ui, m), {VSt|iworld}) 	
							# change 	= ReplaceUI (uic UIInteract [toPrompt prompt, ui])
							# valid     = not (containsInvalidFields m)
					        # value     = if valid (Value (l,v) False) NoValue
					        # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
					        = (ValueResult value info change (TCInteract taskId ts (toJSON l) (toJSON v) m), iworld)
				AsyncRead sds = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
			(_, iworld) = (ExceptionResult (exception "Dynamic type mismatch"), iworld)		

    eval prompt mode shared handlers editor  (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Modify _ _ (TCInteract taskId ts encl encv m)) iworld=:{sdsEvalStates, current={taskTime}}
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: ModifyResult () r^ w^), iworld) = case res of
				// We already have the result from executing the modify function, it happened on this machine.
				ModifyResult _ _
					# value = (Value ((fromJust (fromJSON encl)), (fromJust (fromJSON encv))) False) 
					= (ValueResult value evalInfo NoChange (TCInteract taskId ts encl encv m), {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates })
				AsyncModify sds f 
				= (ValueResult NoValue evalInfo NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modifySDS f sds () (TaskContext taskId))) sdsEvalStates})
			(_, iworld)							= (ExceptionResult (exception "Dynamic type mismatch"), iworld)

    // Ignore all other events when waiting on an async operation.
    // TODO: handle Focus event with correct task time.
	eval _ _ _ _ _  _ _ t=:(TCAwait _ taskId ts tree) iworld 
		= (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)

	// Handle all other events normally
	eval prompt mode shared handlers editor  event evalOpts tree iworld=:{current={taskTime}, sdsEvalStates}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok ('SDS'.ReadResult r),iworld)
						# (l,v) = handlers.onInit r
						= case initMask taskId mode editor v iworld of
							(Ok m,iworld) = (Ok (Left (taskId,ts,l,v,m)),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Ok ('SDS'.AsyncRead sds), iworld) =(Ok (Right (taskId, ts, sds)),{iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv m)
				//Just decode the initially stored values
				= case (fromJSON encl, fromJSON encv) of
					(Just l,Just v) = (Ok (Left (taskId,ts,l,v,m)),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		| mbd =:(Ok (Right _)) = case mbd of 
			(Ok (Right (taskId, ts, sds))) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Read taskId taskTime tree), iworld)
		# (Left (taskId,ts,l,v,m)) = fromOk mbd
        # (mbRes, iworld) = case event of
            EditEvent eTaskId name edit | eTaskId == taskId
	            = applyEditEvent_ name edit taskId mode editor taskTime shared handlers.InteractionHandlers.onEdit l v m iworld
            ResetEvent
                # vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
                = case editor.Editor.genUI [] v vst of
			        (Ok (ui,m),{VSt|iworld}) = (Ok (Left (l,v,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime)),iworld)
			        (Error e,{VSt|iworld})   = (Error (exception e),iworld)
            RefreshEvent taskIds _ | 'DS'.member taskId taskIds
                = refreshView_ taskId mode editor shared handlers.InteractionHandlers.onRefresh l v m taskTime iworld
            FocusEvent fTaskId | fTaskId == taskId
	            = (Ok (Left (l,v,NoChange,m,taskTime)),iworld)
            _ = (Ok (Left (l,v,NoChange,m,ts)),iworld)
        = case mbRes of
		   Error e = (ExceptionResult e, iworld)
		   // An EditEvent can lead to an asynchronous update of a share. However, we do not 
		   // care about the result of this update so we do not show the loading bar. We do
		   // want to wait for the result of the modify (otherwise we send multiple requests which may interfere),
		   // so we transition to the TCAwait state
		   Ok (Right (type, sdsf, l, v, m, change))
			   	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
			   	# tree = TCAwait type taskId taskTime (TCInteract taskId taskTime (toJSON l) (toJSON v) m)
			   	= (ValueResult NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId sdsf iworld.sdsEvalStates})
		   Ok (Left (l,v,change,m,ts))
                //Construct the result
                # valid     = not (containsInvalidFields m)
                # value     = if valid (Value (l,v) False) NoValue
                # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
                = (ValueResult value info change (TCInteract taskId ts (toJSON l) (toJSON v) m), iworld)

initMask :: TaskId EditMode (Editor v) v !*IWorld -> (MaybeError TaskException EditMask, !*IWorld)
initMask taskId mode editor v iworld
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.genUI [] v vst of
		(Ok (_,mask),{VSt|iworld}) = (Ok mask, iworld)
		(Error e, {VSt|iworld}) = (Error (exception e), iworld)

applyEditEvent_ :: String JSONNode TaskId EditMode (Editor v) TaskTime (sds () r w) (v l v -> (l, v, Maybe (r -> w))) l v EditMask !*IWorld
                -> (!MaybeError TaskException (Either (!l, !v, !UIChange, !EditMask, !TaskTime) (!AsyncAction, !*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld), !l, !v, !EditMask, !UIChange)), !*IWorld) 
                | TC r & TC w & RWShared sds
applyEditEvent_ name edit taskId mode editor taskTime shared onEdit l ov m iworld
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.onEdit [] (s2dp name,edit) ov m vst of
        (Ok (change,m),v,{VSt|iworld})
	        # (l,v,mbf) = onEdit v l ov
	        # change    = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
            # valid     = not (containsInvalidFields m)
	        = case mbf of
		        Just f | valid = case 'SDS'.modify f shared ('SDS'.TaskContext taskId) iworld of
			        (Ok ('SDS'.ModifyResult _ _),iworld) = (Ok (Left (l,v,change,m,taskTime)),iworld)
			        (Ok ('SDS'.AsyncModify sds _), iworld) = (Ok (Right (Modify, dynamicResult ('SDS'.modify f sds ('SDS'.TaskContext taskId)), l, v, m, change)),iworld)
			        (Error e,iworld) = (Error e,iworld)
		        _ = (Ok (Left (l,v,change,m,taskTime)),iworld)
        (Error e,_,{VSt|iworld}) = (Error (exception e),iworld)

refreshView_ :: TaskId EditMode (Editor v) (sds () r w) (r l v -> (l, v, Maybe (r -> w))) l v EditMask TaskTime !*IWorld
             -> (!MaybeError TaskException (Either (!l, !v, !UIChange, !EditMask, !TaskTime) (!AsyncAction, !!*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld), !l, !v, !EditMask, !UIChange)), !*IWorld)
             | TC r & TC w & RWShared sds
refreshView_ taskId mode editor shared onRefresh l ov m taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok ('SDS'.AsyncRead sds), iworld) = (Ok (Right (Read, dynamicResult ('SDS'.readRegister taskId sds), l, ov, m, NoChange)), iworld)
		(Ok ('SDS'.ReadResult r),iworld)
			# (l,v,mbf) = onRefresh r l ov
			# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
			= case editor.Editor.onRefresh [] v ov m vst of
				(Ok (change,m),_,vst=:{VSt|iworld})
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					//Update the share if necessary
					= case mbf of
						Just f = case 'SDS'.modify f shared ('SDS'.TaskContext taskId) iworld of
							(Ok ('SDS'.ModifyResult _ _),iworld) = (Ok (Left (l,v,change,m,taskTime)), iworld)
							(Ok ('SDS'.AsyncModify sds _), iworld) = (Ok (Right (Modify, dynamicResult ('SDS'.modify f sds ('SDS'.TaskContext taskId)), l, v, m, change) ), iworld)
							(Error e,iworld) = (Error e,iworld)
						Nothing	= (Ok (Left (l,v,change,m,taskTime)), iworld)
				(Error e,_,vst=:{VSt|iworld}) = (Error (exception e),iworld)