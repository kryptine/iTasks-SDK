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

import Data.Error, Data.Maybe
import Text.JSON
import StdString

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

interact :: !d !EditMode !(RWShared () r w)
				(r -> (l, v))                       //On init
				(v l v -> (l, v, Maybe (r -> w))) 	//On edit
				(r l v -> (l, v, Maybe (r -> w)))  	//On refresh
				(Maybe (Editor v)) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC w
interact prompt mode shared initFun editFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	eval event evalOpts tree iworld=:{current={taskTime}}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok r,iworld)
						# (l,v) = initFun r
						= case initMask taskId mode mbEditor v iworld of
							(Ok m,iworld) = (Ok (taskId,ts,l,v,m),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv m)
				//Just decode the initially stored values
				= case (fromJSON encl, fromJSON encv) of
					(Just l,Just v) = (Ok (taskId,ts,l,v,m),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		# (taskId,ts,l,v,m) = fromOk mbd
        # (mbRes, iworld) = case event of
            EditEvent eTaskId name edit | eTaskId == taskId =
                applyEditEvent_ name edit taskId mode mbEditor taskTime shared editFun l v m iworld
            ResetEvent
                # editor = fromMaybe gEditor{|*|} mbEditor
                # vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
                = case editor.Editor.genUI [] v vst of
			        (Ok (ui,m),{VSt|iworld}) = (Ok (l,v,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),m,taskTime),iworld)
			        (Error e,{VSt|iworld})   = (Error (exception e),iworld)
            RefreshEvent _ = refreshView_ taskId mode mbEditor shared refreshFun l v m taskTime iworld
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

initMask :: TaskId EditMode (Maybe (Editor v)) v !*IWorld -> (MaybeError TaskException EditMask, !*IWorld) | gEditor{|*|} v
initMask taskId mode mbEditor v iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.genUI [] v vst of
		(Ok (_,mask),{VSt|iworld}) = (Ok mask, iworld)
		(Error e, {VSt|iworld}) = (Error (exception e), iworld)

applyEditEvent_ :: String JSONNode TaskId EditMode (Maybe (Editor v)) TaskTime (RWShared () r w) (v l v -> (l, v, Maybe (r -> w))) l v EditMask !*IWorld
                -> (!MaybeError TaskException (!l, !v, !UIChange, !EditMask, !TaskTime), !*IWorld)
                | gEditor{|*|} v & TC r & TC w
applyEditEvent_ name edit taskId mode mbEditor taskTime shared editFun l ov m iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
	# vst = {VSt| taskId = toString taskId, mode = mode, optional = False, selectedConsIndex = -1, iworld = iworld}
	= case editor.Editor.onEdit [] (s2dp name,edit) ov m vst of
        (Ok (change,m),v,{VSt|iworld})
	        # (l,v,mbf) = editFun v l ov
	        # change    = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
            # valid     = not (containsInvalidFields m)
	        = case mbf of
		        Just f | valid = case 'SDS'.modify (\r -> ((),f r)) shared iworld of
			        (Ok (),iworld) = (Ok (l,v,change,m,taskTime),iworld)
			        (Error e,iworld) = (Error e,iworld)
		        _
			        = (Ok (l,v,change,m,taskTime),iworld)
        (Error e,_,{VSt|iworld}) = (Error (exception e),iworld)

refreshView_ :: TaskId EditMode (Maybe (Editor v)) (RWShared () r w) (r l v -> (l, v, Maybe (r -> w))) l v EditMask TaskTime !*IWorld
             -> (!MaybeError TaskException (!l, !v, !UIChange, !EditMask, !TaskTime), !*IWorld)
             | gEditor{|*|} v & TC r & TC w
refreshView_ taskId mode mbEditor shared refreshFun l ov m taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok r,iworld)
			# (l,v,mbf) = refreshFun r l ov
			# editor = fromMaybe gEditor{|*|} mbEditor
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
