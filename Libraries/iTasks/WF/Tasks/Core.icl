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

import StdDebug, StdMisc

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

interact :: !d !EditMode !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC w & RWShared sds
interact prompt mode shared {onInit,onEdit,onRefresh} editor
=  Task eval
where
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	eval (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Read taskId connectionId ts tree) iworld=:{ioStates, current={taskTime}} 
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	# data = 'ASDS'.getAsyncReadValue shared taskId connectionId ioStates
	// No data yet, keep showing the progress spinner
	| data=:(Right Nothing) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	# (l, v) = case data of (Right (Just d)) = onInit d;
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.genUI [] v vst of
		(Error e, {VSt|iworld})		= (ExceptionResult (exception e), iworld)
		(Ok (ui, m), {VSt|iworld}) 	
		# change =  ReplaceUI (uic UIInteract [toPrompt prompt, ui])
		# valid     = not (containsInvalidFields m)
        # value     = if valid (Value (l,v) False) NoValue
        # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
        = (ValueResult value info change (TCInteract taskId ts (toJSON l) (toJSON v) m), iworld)

    eval (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Modify _ connectionId _ (TCInteract taskId ts encl encv m)) iworld=:{ioStates, current={taskTime}}
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	# value = (Value ((fromJust (fromJSON encl)), (fromJust (fromJSON encv))) False)
	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
	= case 'ASDS'.getAsyncReadValue shared taskId connectionId ioStates of
		(Left exc) = (ExceptionResult (exception exc), iworld)
		// When we have gotten a response from the modify, we can start listening for other events again. 
		(Right (Just v)) = (ValueResult value evalInfo NoChange (TCInteract taskId ts encl encv m), iworld)
		// When we have not yet gotten a response, we wait for one
		(Right Nothing) = (ValueResult value evalInfo NoChange t, iworld)

	eval e=:(EditEvent editedTask name edit) evalOpts t=:(TCAwait Modify taskId connectionId ts tree) iworld 
	| not (editedTask == taskId) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld)
	| otherwise = case eval e evalOpts tree iworld of
		(ExceptionResult e, iworld) = (ExceptionResult e, iworld)
		(ValueResult val info change tree, iworld) = (ValueResult val info change (TCAwait Modify taskId connectionId ts tree), iworld)

    // Ignore all other events when waiting on an async operation.
	eval _ _ t=:(TCAwait _ taskId connectionId ts tree) iworld 
		= ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange t, iworld

	// Handle all other event normally
	eval event evalOpts tree iworld=:{current={taskTime}}
		| not (trace_tn "other event, other state") = undef
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
			| not (trace_tn "TCInit reading from share") = undef
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok ('SDS'.Result r),iworld)
						# (l,v) = onInit r
						= case initMask taskId mode editor v iworld of
							(Ok m,iworld) = (Ok (Left (taskId,ts,l,v,m)),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Ok ('SDS'.Queued connectionId), iworld) =(Ok (Right (taskId, ts, connectionId)),iworld)
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv m)
				//Just decode the initially stored values
				= case (fromJSON encl, fromJSON encv) of
					(Just l,Just v) = (Ok (Left (taskId,ts,l,v,m)),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		| mbd =:(Ok (Right _)) = case mbd of 
			(Ok (Right (taskId, ts, connectionId))) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Read taskId connectionId taskTime tree), iworld)
		# (Left (taskId,ts,l,v,m)) = fromOk mbd
        # (mbRes, iworld) = case event of
            EditEvent eTaskId name edit | eTaskId == taskId
	            | not (trace_tn "Edit event") = undef
	            = applyEditEvent_ name edit taskId mode editor taskTime shared onEdit l v m iworld
            ResetEvent
            	| not (trace_tn "Reset event") = undef
                # vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
                = case editor.Editor.genUI [] v vst of
			        (Ok (ui,m),{VSt|iworld}) = (Ok (Left (l,v,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime)),iworld)
			        (Error e,{VSt|iworld})   = (Error (exception e),iworld)
            RefreshEvent taskIds _ | 'DS'.member taskId taskIds
        		| not (trace_tn "Refresh event") = undef
                = refreshView_ taskId mode editor shared onRefresh l v m taskTime iworld
            FocusEvent fTaskId | fTaskId == taskId
	            | not (trace_tn "Refresh event") = undef
	            = (Ok (Left (l,v,NoChange,m,taskTime)),iworld)
            _ = (Ok (Left (l,v,NoChange,m,ts)),iworld)
        = case mbRes of
		   Error e = (ExceptionResult e, iworld)
		   // An EditEvent can lead to an asynchronous update of a share. However, we do not 
		   // care about the result of this update so we do not show the loading bar. We do
		   // want to wait for the result of the modify, we we transition to the TCAwait state
		   Ok (Right (type, connectionId, l, v, m, change))
			   # evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
			   # tree = TCAwait type taskId connectionId taskTime (TCInteract taskId taskTime (toJSON l) (toJSON v) m)
			   = (ValueResult NoValue evalInfo NoChange tree, iworld)
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
                -> (!MaybeError TaskException (Either (!l, !v, !UIChange, !EditMask, !TaskTime) (!AsyncAction, !Int, !l, !v, !EditMask, !UIChange)), !*IWorld) 
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
			        (Ok ('SDS'.Result _),iworld) = (Ok (Left (l,v,change,m,taskTime)),iworld)
			        (Ok ('SDS'.Queued connectionId), iworld) = (Ok (Right (Modify, connectionId, l, v, m, change)),iworld)
			        (Error e,iworld) = (Error e,iworld)
		        _ = (Ok (Left (l,v,change,m,taskTime)),iworld)
        (Error e,_,{VSt|iworld}) = (Error (exception e),iworld)

refreshView_ :: TaskId EditMode (Editor v) (sds () r w) (r l v -> (l, v, Maybe (r -> w))) l v EditMask TaskTime !*IWorld
             -> (!MaybeError TaskException (Either (!l, !v, !UIChange, !EditMask, !TaskTime) (!AsyncAction, !Int, !l, !v, !EditMask, !UIChange)), !*IWorld)
             | TC r & TC w & RWShared sds
refreshView_ taskId mode editor shared onRefresh l ov m taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok ('SDS'.Queued connectionId), iworld) = (Ok (Right (Read, connectionId, l, ov, m, NoChange) ), iworld)
		(Ok ('SDS'.Result r),iworld)
			# (l,v,mbf) = onRefresh r l ov
			# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
			= case editor.Editor.onRefresh [] v ov m vst of
				(Ok (change,m),_,vst=:{VSt|iworld})
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					//Update the share if necessary
					= case mbf of
						Just f = case 'SDS'.modify f shared ('SDS'.TaskContext taskId) iworld of
							(Ok ('SDS'.Result _),iworld) = (Ok (Left (l,v,change,m,taskTime)), iworld)
							(Ok ('SDS'.Queued connectionId), iworld) = (Ok (Right (Modify, connectionId, l, v, m, change) ), iworld)
							(Error e,iworld) = (Error e,iworld)
						Nothing	= (Ok (Left (l,v,change,m,taskTime)), iworld)
				(Error e,_,vst=:{VSt|iworld}) = (Error (exception e),iworld)