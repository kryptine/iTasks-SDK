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

import Data.Error, Data.Maybe, Data.Either
import Text.JSON
import StdString, StdBool, StdMisc
import qualified Data.Set as DS
import qualified Data.Map as DM
import iTasks.Internal.DynamicUtil

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

interact :: !d !EditMode !(SDS () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC w
interact prompt mode shared {onInit,onEdit,onRefresh} editor = Task eval
where
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	// TODO: Actively check for a value, or can we do something else?
	eval event evalOpts (TCAwaitRead taskId ts) iworld=:{ioStates, current={taskTime}} = case 'DM'.get taskId ioStates of
		Nothing = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} NoChange (TCAwaitRead taskId ts), iworld) 
		Just (IOActive connectionToValue) = evalAwaitResult ('DM'.toList connectionToValue)
		Just (IODestroyed connectionToValue) =  evalAwaitResult ('DM'.toList connectionToValue)
		Just (IOException exc) = (ExceptionResult (exception exc), iworld)
	where
		evalAwaitResult [] = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} NoChange (TCAwaitRead taskId ts), iworld) 
		evalAwaitResult [(_, (either :: (Either [String] r^), stability)):_] = case either of 
			(Right value)
				# (l, v) = onInit value 
				# (mask, iworld) = case initMask taskId mode editor v iworld of
					(Ok m,iworld) 		= (Ok (taskId,ts,l,v,m),iworld)
					(Error e,iworld) 	= (Error e,iworld)
				= case mask of
					(Ok (taskId,ts,l,v,m))
		        		# (mbRes, iworld) = case event of
				            EditEvent eTaskId name edit | eTaskId == taskId =
				                applyEditEvent_ name edit taskId mode editor taskTime shared onEdit l v m iworld
				            ResetEvent
				                # vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
				                = case editor.Editor.genUI [] v vst of
							        (Ok (ui,m),{VSt|iworld}) = (Ok (l,v,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime),iworld)
							        (Error e,{VSt|iworld})   = (Error (exception e),iworld)
				            RefreshEvent taskIds _ | 'DS'.member taskId taskIds
				                = refreshView_ taskId mode editor shared onRefresh l v m taskTime iworld
				            FocusEvent fTaskId | fTaskId == taskId = (Ok (l,v,NoChange,m,taskTime),iworld)
				            _ = (Ok (l,v,NoChange,m,ts),iworld)
		       			= case mbRes of
						   Error e = (ExceptionResult e, iworld)
						   Ok (l,v,change,m,ts)
				                //Construct the result
				                # valid     = not (containsInvalidFields m)
				                # value     = if valid (Value (l,v) False) NoValue
				                # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True}
				                = (ValueResult value info change (TCInteract taskId ts (toJSON l) (toJSON v) m), iworld)
            (Left value) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=False} NoChange (TCAwaitRead taskId ts), iworld) 

        evalAwaitResult [(_, (value , stability)):_] = abort ("Dynamic typecheck failed" +++ (toString (unpackType value)))

        evalAwaitResult _ = abort "NOT MATCHING JONGUH"

	eval event evalOpts tree iworld=:{current={taskTime}}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok ('SDS'.Result r),iworld)
						# (l,v) = onInit r
						= case initMask taskId mode editor v iworld of
							(Ok m,iworld) = (Ok (Left (taskId,ts,l,v,m)),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Ok 'SDS'.Queued, iworld) = (Ok (Right (taskId,ts)), iworld)
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv m)
				//Just decode the initially stored values
				= case (fromJSON encl, fromJSON encv) of
					(Just l,Just v) = (Ok (Left (taskId,ts,l,v,m)),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")), iworld)

		= case mbd of 
			(Error _) = (ExceptionResult (fromError mbd), iworld)
			(Ok (Right (id, ts))) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwaitRead id ts), iworld)
			(Ok (Left (taskId,ts,l,v,m)))
        		# (mbRes, iworld) = case event of
		            EditEvent eTaskId name edit | eTaskId == taskId =
		                applyEditEvent_ name edit taskId mode editor taskTime shared onEdit l v m iworld
		            ResetEvent
		                # vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
		                = case editor.Editor.genUI [] v vst of
					        (Ok (ui,m),{VSt|iworld}) = (Ok (l,v,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime),iworld)
					        (Error e,{VSt|iworld})   = (Error (exception e),iworld)
		            RefreshEvent taskIds _ | 'DS'.member taskId taskIds
		                = refreshView_ taskId mode editor shared onRefresh l v m taskTime iworld
		            FocusEvent fTaskId | fTaskId == taskId = (Ok (l,v,NoChange,m,taskTime),iworld)
		            _ = (Ok (l,v,NoChange,m,ts),iworld)
       			= case mbRes of
				   Error e = (ExceptionResult e, iworld)
				   Ok (l,v,change,m,ts)
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

applyEditEvent_ :: String JSONNode TaskId EditMode (Editor v) TaskTime (SDS () r w) (v l v -> (l, v, Maybe (r -> w))) l v EditMask !*IWorld
                -> (!MaybeError TaskException (!l, !v, !UIChange, !EditMask, !TaskTime), !*IWorld)
                | TC r & TC w & JSONDecode{|*|} r
applyEditEvent_ name edit taskId mode editor taskTime shared onEdit l ov m iworld
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.onEdit [] (s2dp name,edit) ov m vst of
        (Ok (change,m),v,{VSt|iworld})
	        # (l,v,mbf) = onEdit v l ov
	        # change    = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
            # valid     = not (containsInvalidFields m)
	        = case mbf of
		        Just f | valid = case 'SDS'.modify (\r -> ((),f r)) shared iworld of
			        (Ok (),iworld) = (Ok (l,v,change,m,taskTime),iworld)
			        (Error e,iworld) = (Error e,iworld)
		        _
			        = (Ok (l,v,change,m,taskTime),iworld)
        (Error e,_,{VSt|iworld}) = (Error (exception e),iworld)

refreshView_ :: TaskId EditMode (Editor v) (SDS () r w) (r l v -> (l, v, Maybe (r -> w))) l v EditMask TaskTime !*IWorld
             -> (!MaybeError TaskException (!l, !v, !UIChange, !EditMask, !TaskTime), !*IWorld)
             | TC r & TC w& JSONDecode{|*|} r
refreshView_ taskId mode editor shared onRefresh l ov m taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok ('SDS'.Queued), iworld) = 
		(Ok ('SDS'.Result r),iworld)
			# (l,v,mbf) = onRefresh r l ov
			# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
			= case editor.Editor.onRefresh [] v ov m vst of
				(Ok (change,m),_,vst=:{VSt|iworld})
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					//Update the share if necessary
					= case mbf of
						Just f = case 'SDS'.modify (\r -> ((),f r)) shared iworld of
							(Ok (),iworld) = (Ok (l,v,change,m,taskTime), iworld)
							(Error e,iworld) = (Error e,iworld)
						Nothing
							= (Ok (l,v,change,m,taskTime), iworld)
				(Error e,_,vst=:{VSt|iworld}) = (Error (exception e),iworld)

