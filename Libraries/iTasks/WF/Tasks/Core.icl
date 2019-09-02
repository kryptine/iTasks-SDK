implementation module iTasks.WF.Tasks.Core

import iTasks.SDS.Sources.Core
import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import qualified iTasks.Internal.SDS as SDS
import iTasks.Internal.AsyncSDS
import iTasks.Internal.Util

import Data.Error, Data.Maybe, Data.Func, Data.Either, Data.Tuple
import Text.GenJSON
import StdString, StdBool, StdInt, StdMisc, StdFunc
import qualified Data.Set as DS
import qualified Data.Map as DM

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (exception e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = accWorld $ tuple () o fun

accWorld :: !(*World -> *(a, *World)) -> Task a | iTask a
accWorld fun = accWorldError (appFst Ok o fun) \_->""

accWorldError :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= case res of
			Error e = (Error (exception (errf e)), {IWorld|iworld & world = world})
			Ok v    = (Ok v, {IWorld|iworld & world = world})

accWorldOSError :: !(*World -> (MaybeOSError a, *World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interactRW :: !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v)
	| iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
interactRW shared handlers editor
	= Task (readRegisterCompletely shared NoValue (\_->asyncSDSLoaderUI Read) (evalInteractInit shared handlers editor modifyCompletely))

interactR :: (sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v)
	| iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
interactR shared handlers editor
	= Task (readRegisterCompletely shared NoValue (\_->asyncSDSLoaderUI Read) (evalInteractInit shared handlers editor \_ _->modifyCompletely (\()->undef) nullShare))

//This initializes the editor state and continues with the actual interact task
evalInteractInit sds handlers editor writefun r event evalOpts=:{TaskEvalOpts|taskId} iworld
	//Get initial value
	# (l, mode) = handlers.onInit r
	# v = case mode of
		Enter    = Nothing
		Update x = Just x
		View x   = Just x
	= case initEditorState taskId mode editor iworld of
		(Ok st, iworld)
			= evalInteract l v st (mode=:View _) sds handlers editor writefun event evalOpts iworld
		(Error e, iworld) = (ExceptionResult e, iworld)

initEditorState :: TaskId (EditMode v) (Editor v) !*IWorld -> (MaybeError TaskException EditState, !*IWorld)
initEditorState taskId mode editor iworld = withVSt taskId
	( \vst -> case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
		(Ok (_, st), vst) = (Ok st,               vst)
		(Error e,    vst) = (Error $ exception e, vst)
	) iworld

evalInteract ::
	l
	(Maybe v)
	EditState
	Bool
	(sds () r w)
	(InteractionHandlers l r w v)
	(Editor v)
	(
		(r -> w)
		(sds () r w)
		(TaskValue (l,v))
		(Event -> UIChange)
		(w -> Event -> TaskEvalOpts -> *IWorld -> *(TaskResult (l,v),*IWorld))
		Event
		TaskEvalOpts
		*IWorld
		-> *(TaskResult (l,v),*IWorld))
	Event
	TaskEvalOpts
	*IWorld
	-> *(TaskResult (l,v),*IWorld)
	| iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
evalInteract _ _ _ _ _ _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
	= (DestroyedResult, 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld)
evalInteract l v st mode sds handlers editor writefun event=:(EditEvent eTaskId name edit) evalOpts=:{taskId,lastEval} iworld
	| eTaskId == taskId
		# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) st) iworld
		= case res of
			Ok (change, st)
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
				= case editor.Editor.valueFromState st of
					Just nv
						# (l, v, mbf) = handlers.InteractionHandlers.onEdit nv l v
						= case mbf of
							//We have an update function
							Just f = writefun f sds NoValue (\_->change)
								// We cannot just do this because this will loop endlessly:
								// (\_->evalInteract l (Just v) st mode sds handlers editor writefun)
								// Therefore we delay it by returning the continuation in a value instead of directly:
								(\w event {TaskEvalOpts|lastEval} iworld->
									(ValueResult
										(Value (l, v) False)
										(mkTaskEvalInfo lastEval)
										change
										(Task (evalInteract l (Just v) st mode sds handlers editor writefun))
									, iworld))
								event evalOpts iworld
							//There is no update function
							Nothing
								= (ValueResult
									(Value (l, v) False)
									(mkTaskEvalInfo lastEval)
									change
									(Task (evalInteract l (Just v) st mode sds handlers editor writefun))
								, iworld)
					Nothing
						= (ValueResult
							(maybe NoValue (\v->Value (l, v) False) v)
							(mkTaskEvalInfo lastEval)
							change
							(Task (evalInteract l v st mode sds handlers editor writefun))
						, iworld)
			Error e = (ExceptionResult (exception e), iworld)
evalInteract l v st mode sds handlers editor writefun ResetEvent evalOpts=:{taskId,lastEval} iworld
	# resetMode = case (mode, v) of
		(True, Just v) = View v
		(True, _)      = abort "view mode without value\n"
		(_, Nothing)   = Enter
		(_, Just v)    = Update v
	= case withVSt taskId (editor.Editor.genUI 'DM'.newMap [] resetMode) iworld of
		(Error e, iworld) = (ExceptionResult (exception e), iworld)
		(Ok (UI type attr items, st), iworld)
			# change = ReplaceUI (UI type (addClassAttr "interact" attr) items)
			# mbv = editor.Editor.valueFromState st
			# v = maybe v Just mbv
			= (ValueResult
				(maybe NoValue (\v->Value (l, v) False) v)
				(mkTaskEvalInfo lastEval)
				change
				(Task (evalInteract l v st mode sds handlers editor writefun))
			, iworld)
evalInteract l v st mode sds handlers editor writefun event=:(RefreshEvent taskIds _) evalOpts=:{taskId,lastEval} iworld
	| 'DS'.member taskId taskIds
		= readRegisterCompletely sds (maybe NoValue (\v->Value (l, v) False) v) (\e->case event of ResetEvent = asyncSDSLoaderUI Read; e = NoChange)
			(\r event evalOpts iworld
				# (l, v, mbf) = handlers.InteractionHandlers.onRefresh r l v
				= case withVSt taskId (editor.Editor.onRefresh [] v st) iworld of
					(Error e, iworld) = (ExceptionResult (exception e), iworld)
				    (Ok (change, st), iworld)
						# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
						= case mbf of
							Just f = writefun f sds NoValue (\_->change)
								(\_->evalInteract l (Just v) st mode sds handlers editor writefun)
								event evalOpts iworld
							Nothing
								= (ValueResult
									(Value (l, v) False)
									(mkTaskEvalInfo lastEval)
									change
									(Task (evalInteract l (Just v) st mode sds handlers editor writefun))
								, iworld)
			)
			event evalOpts iworld
evalInteract l v st mode sds handlers editor writefun event {lastEval} iworld
	//An event for a sibling?
	= (ValueResult
		(maybe NoValue (\v->Value (l, v) False) v)
		(mkTaskEvalInfo lastEval)
		NoChange
		(Task (evalInteract l v st mode sds handlers editor writefun))
	, iworld)

uniqueMode :: (EditMode a) -> *(EditMode a)
uniqueMode mode = case mode of
	Enter    = Enter
	Update x = Update x
	View x   = View x
