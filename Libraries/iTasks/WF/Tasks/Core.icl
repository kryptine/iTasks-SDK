implementation module iTasks.WF.Tasks.Core

import iTasks.WF.Derives
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

import Data.Error, Data.Maybe, Data.Func, Data.Either, Data.Tuple
import Text.GenJSON
import StdString, StdBool, StdInt, StdMisc, StdFunc
import qualified Data.Set as DS
import qualified Data.Map as DM

derive JSONEncode Event,Set
derive gText Event, Set

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = accWorld $ tuple () o fun

accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = accWorldError (appFst Ok o fun) \_->""

accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
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

interactRW :: !d !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v)
	| toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
interactRW prompt shared handlers editor
	= Task (evalInteractInit prompt shared shared handlers editor (interactModifyShareAsync shared))
where
	interactModifyShareAsync :: (sds () r w) TaskId (r -> w) !*IWorld ->
		(MaybeError TaskException (Maybe (!AsyncAction, !*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld))), !*IWorld)
		| TC r & TC w & RWShared sds
	interactModifyShareAsync shared taskId modifier iworld
		= case 'SDS'.modify modifier shared ('SDS'.TaskContext taskId) iworld of
			(Ok ('SDS'.ModifyingDone _),iworld)  = (Ok Nothing,iworld)
			(Ok ('SDS'.Modifying sds _), iworld) = (Ok (Just (Modify, dynamicResult ('SDS'.modify modifier sds ('SDS'.TaskContext taskId)))),iworld)
			(Error e,iworld)                     = (Error e,iworld)

interactR :: !d (sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
interactR prompt shared handlers editor
	= Task (evalInteractInit prompt shared shared handlers editor \_ _ iw -> (Ok Nothing,iw))

evalInteractInit :: !d (sds1 () r w) (sds2 () r w) (InteractionHandlers l r w v) (Editor v) (TaskId (r -> w) *IWorld -> (MaybeError TaskException (Maybe (!AsyncAction, !*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld))), !*IWorld)) Event TaskEvalOpts *IWorld
	-> *(TaskResult (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds1 & Registrable sds2
evalInteractInit _ _ _ _ _ _ DestroyEvent _ iworld
	= (DestroyedResult, iworld)
evalInteractInit prompt origsds sds handlers editor writefun event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	//Get initial value
	= case 'SDS'.readRegister taskId sds iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok ('SDS'.ReadingDone r), iworld)
			# (l, mode) = handlers.onInit r
			# v = case mode of
				Enter    = Nothing
				Update x = Just x
				View x   = Just x
			= case initEditorState taskId mode editor iworld of
				(Ok st, iworld)
					= evalInteract l v st (mode=:View _) prompt origsds origsds handlers editor writefun event evalOpts iworld
				(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok ('SDS'.Reading sds), iworld)
			= (ValueResult
				NoValue
				{TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
				(ReplaceUI (uia UIProgressBar (textAttr "Getting data")))
				(Task (evalInteractInit prompt origsds sds handlers editor writefun))
			, iworld)

initEditorState :: TaskId (EditMode v) (Editor v) !*IWorld -> (MaybeError TaskException EditState, !*IWorld)
initEditorState taskId mode editor iworld = withVSt taskId
	( \vst -> case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
		(Ok (_, st), vst) = (Ok st,               vst)
		(Error e,    vst) = (Error $ exception e, vst)
	) iworld

evalInteract :: !l !(Maybe v) !EditState Bool !d (sds1 () r w) (sds2 () r w) (InteractionHandlers l r w v) (Editor v) (TaskId (r -> w) *IWorld -> (MaybeError TaskException (Maybe (!AsyncAction, !*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld))), !*IWorld)) Event TaskEvalOpts *IWorld
	-> *(TaskResult (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds1 & Registrable sds2
evalInteract _ _ _ _ _ _ _ _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
	= (DestroyedResult, 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld)
evalInteract l v st mode prompt origsds sds handlers editor writefun event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	# (mbRes, iworld) = case event of
		EditEvent eTaskId name edit | eTaskId == taskId
			= applyEditEvent name edit l v iworld
		ResetEvent
			# resetMode = case (mode, v) of
				(True, Just v) = View v
				(True, _)      = abort "view mode without value"
				(_, Nothing)   = Enter
				(_, Just v)    = Update v
			= withVSt taskId
				( \vst -> case editor.Editor.genUI 'DM'.newMap [] resetMode vst of
					(Ok (ui,st),vst) = (Ok (Left (l,editor.Editor.valueFromState st,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),st)), vst)
					(Error e, vst)  = (Error (exception e), vst)
				)
				iworld
		RefreshEvent taskIds _ | 'DS'.member taskId taskIds
			= refreshView l v iworld
	= case mbRes of
		Error e = (ExceptionResult e, iworld)
		// An EditEvent can lead to an asynchronous update of a share. However, we do not
		// care about the result of this update so we do not show the loading bar. We do
		// want to wait for the result of the modify (otherwise we send multiple requests which may interfere),
		// so we transition to the TCAwait state
//		Ok (Right (type, sdsf, l, v, st, change))
//			# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
//			# tree = TCAwait type taskId taskTime (TCInteract taskId taskTime (DeferredJSON l) (DeferredJSON v) st viewMode)
//			= (ValueResult` NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId sdsf iworld.sdsEvalStates})
		Ok (Left (l,mbV,change,st))
			//Construct the result
			# v     = maybe v Just mbV // use previous view state of editor is in invalid state
			# value = maybe NoValue (\v -> Value (l, v) False) mbV
			# info  = {TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
			= (ValueResult
				value
				info
				change
				(Task (evalInteract l v st mode prompt origsds origsds handlers editor writefun))
			, iworld)
where
	applyEditEvent name edit l ov iworld
		# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) st) iworld
		= case res of
			Ok (change, st)
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
				= case editor.Editor.valueFromState st of
					Just v
						# (l, v, mbf) = handlers.InteractionHandlers.onEdit v l ov
						= case mbf of
							Just f = case writefun taskId f iworld of
								(Ok (Just (action, dynfun)), iworld) = (Ok (Right (action, dynfun, l, Just v, st, change)), iworld)
								(Ok Nothing,iworld) = (Ok (Left (l,Just v,change,st)),iworld)
								(Error e,iworld) = (Error e,iworld)
							_ = (Ok (Left (l,Just v,change,st)),iworld)
					_ = (Ok (Left (l,Nothing,change,st)),iworld)
			Error e = (Error (exception e), iworld)

	refreshView l ov iworld
		//Read the shared source and refresh the editor
		= case 'SDS'.readRegister taskId sds iworld of
			(Error e,iworld) = (Error e,iworld)
//			(Ok ('SDS'.Reading sds), iworld) = (Ok (Right (Read, dynamicResult ('SDS'.readRegister taskId sds), l, ov, st, NoChange)), iworld)
			(Ok ('SDS'.ReadingDone r),iworld)
				# (l,v,mbf) = handlers.InteractionHandlers.onRefresh r l ov
				# (res, iworld) = withVSt taskId (editor.Editor.onRefresh [] v st) iworld
				= case res of
					Ok (change,st)
						# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
						//Update the share if necessary
						= case mbf of
							Just f = case writefun taskId f iworld of
								(Ok (Just (action, dynfun)), iworld) = (Ok (Right (action, dynfun, l, Just v, st, change)), iworld)
								(Ok Nothing,iworld) = (Ok (Left (l,Just v,change,st)),iworld)
								(Error e,iworld) = (Error e,iworld)
							Nothing = (Ok (Left (l,Just v,change,st)), iworld)
					Error e = (Error (exception e), iworld)

//interactAwaitReadRefresh :: !d (sds () r w) (InteractionHandlers l r w v) (Editor v) Event TaskEvalOpts TaskTree *IWorld
//		-> *(TaskResult` (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
//interactAwaitReadRefresh prompt shared handlers editor (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Read taskId ts tree)
//	iworld=:{sdsEvalStates, current={taskTime}}
//	| not ('DS'.member taskId taskIds) = (ValueResult` NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)
//	= case 'DM'.get taskId sdsEvalStates of
//		Nothing = (ExceptionResult` (exception ("No SDS state found for task " +++ toString taskId)), iworld)
//		(Just val)
//		= case val iworld of
//			(Error e, iworld) = (ExceptionResult` e, iworld)
//			(Ok (res :: AsyncRead r^ w^), iworld) = case res of
//				ReadingDone r
//					# (l, mode) = handlers.onInit r
//					# mbV = case mode of
//							Enter    = Nothing
//							Update x = Just x
//							View x   = Just x
//					= withVSt taskId (\vst. case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
//						(Error e, vst)		= (ExceptionResult` (exception e), vst)
//						(Ok (ui, st), vst)
//							# change 	= ReplaceUI (uic UIInteract [toPrompt prompt, ui])
//					        # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
//                			# value 	= maybe NoValue (\v -> Value (l, v) False) mbV
//					        = (ValueResult` value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON mbV) st (mode =: View _)), vst)) iworld
//				Reading sds = (ValueResult` NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
//			(_, iworld) = (ExceptionResult` (exception "Dynamic type mismatch"), iworld)
//
//interactAwaitModifyRefresh :: !d (sds () r w) (InteractionHandlers l r w v) (Editor v) Event TaskEvalOpts TaskTree *IWorld
//		-> *(TaskResult` (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
//interactAwaitModifyRefresh prompt shared handlers editor (RefreshEvent taskIds reason) evalOpts
//	t=:(TCAwait Modify _ _ (TCInteract taskId ts encl encv st viewmode)) iworld=:{sdsEvalStates, current={taskTime}}
//	| not ('DS'.member taskId taskIds) = (ValueResult` NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)
//	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
//	= case 'DM'.get taskId sdsEvalStates of
//		Nothing 				= (ExceptionResult` (exception ("No SDS state found for task " +++ toString taskId)), iworld)
//		(Just val) 				= case val iworld of
//			(Error e, iworld) = (ExceptionResult` e, iworld)
//			(Ok (res :: AsyncModify r^ w^), iworld) = case res of
//				// We already have the result from executing the modify function, it happened on this machine.
//				ModifyingDone _
//					# value = (Value ((fromJust (fromDeferredJSON encl)), (fromJust (fromDeferredJSON encv))) False)
//					= (ValueResult` value evalInfo NoChange (TCInteract taskId ts encl encv st viewmode), {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates })
//				Modifying sds f
//				= (ValueResult` NoValue evalInfo NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modify f sds (TaskContext taskId))) sdsEvalStates})
//			(Ok (dyn), iworld)							= (ExceptionResult` (exception ("Dynamic type mismatch, type was " +++ toString (typeCodeOfDynamic dyn))), iworld)

uniqueMode :: (EditMode a) -> *(EditMode a)
uniqueMode mode = case mode of
	Enter    = Enter
	Update x = Update x
	View x   = View x
