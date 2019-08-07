implementation module iTasks.WF.Tasks.Core

import iTasks.SDS.Sources.Core
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
import iTasks.Internal.AsyncSDS

import Data.Error, Data.Maybe, Data.Func, Data.Either, Data.Tuple
import Text.GenJSON
import StdString, StdBool, StdInt, StdMisc, StdFunc
import qualified Data.Set as DS
import qualified Data.Map as DM

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = accWorld $ tuple () o fun

accWorld :: !(*World -> *(a, *World)) -> Task a | iTask a
accWorld fun = accWorldError (appFst Ok o fun) \_->""

accWorldError :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | iTask a & TC, toString err
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

accWorldOSError :: !(*World -> (MaybeOSError a, *World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interactRW :: !d !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v)
	| toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
interactRW prompt shared handlers editor
	= Task (readCompletely shared NoValue (evalInteractInit prompt shared handlers editor modifyCompletely))

interactR :: !d (sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v)
	| toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
interactR prompt shared handlers editor
	= Task (readCompletely shared NoValue (evalInteractInit prompt shared handlers editor \_ _->modifyCompletely (\()->undef) nullShare))

//This initializes the editor state and continues with the actual interact task
evalInteractInit prompt sds handlers editor writefun r event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	//Get initial value
	# (l, mode) = handlers.onInit r
	# v = case mode of
		Enter    = Nothing
		Update x = Just x
		View x   = Just x
	= case initEditorState taskId mode editor iworld of
		(Ok st, iworld)
			= evalInteract l v st (mode=:View _) prompt sds handlers editor writefun event evalOpts iworld
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
	c
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
	| toPrompt c & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
evalInteract _ _ _ _ _ _ _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
	= (DestroyedResult, 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld)
evalInteract l v st mode prompt sds handlers editor writefun event=:(EditEvent eTaskId name edit) evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	| not (eTaskId == taskId) = (ExceptionResult (exception "Edit event not for our taskId"), iworld)
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
							(\_->evalInteract l (Just v) st mode prompt sds handlers editor writefun)
							event evalOpts iworld
						//There is no update function
						Nothing
							= (ValueResult
								(Value (l, v) False)
								{TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
								change
								(Task (evalInteract l (Just v) st mode prompt sds handlers editor writefun))
							, iworld)
				Nothing
					= (ValueResult
						(maybe NoValue (\v->Value (l, v) False) v)
						{TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
						change
						(Task (evalInteract l v st mode prompt sds handlers editor writefun))
					, iworld)
		Error e = (ExceptionResult (exception e), iworld)
evalInteract l v st mode prompt sds handlers editor writefun event=:ResetEvent evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	# resetMode = case (mode, v) of
		(True, Just v) = View v
		(True, _)      = abort "view mode without value"
		(_, Nothing)   = Enter
		(_, Just v)    = Update v
	= case withVSt taskId (editor.Editor.genUI 'DM'.newMap [] resetMode) iworld of
		(Error e, iworld) = (ExceptionResult (exception e), iworld)
		(Ok (ui, st), iworld)
			# v = editor.Editor.valueFromState st
			= (ValueResult
				(maybe NoValue (\v->Value (l, v) False) v)
				{TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
				(ReplaceUI (uic UIInteract [toPrompt prompt, ui]))
				(Task (evalInteract l v st mode prompt sds handlers editor writefun))
			, iworld)
evalInteract l v st mode prompt sds handlers editor writefun event=:(RefreshEvent taskIds _) evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	| not ('DS'.member taskId taskIds) = (ExceptionResult (exception "refresh not for this taskId"), iworld)
	= readCompletely sds (maybe NoValue (\v->Value (l, v) False) v)
		(\r event evalOpts iworld
			# (l, v, mbf) = handlers.InteractionHandlers.onRefresh r l v
			= case withVSt taskId (editor.Editor.onRefresh [] v st) iworld of
				(Error e, iworld) = (ExceptionResult (exception e), iworld)
			    (Ok (change, st), iworld)
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					= case mbf of
						Just f = writefun f sds NoValue (\_->change)
							(\_->evalInteract l (Just v) st mode prompt sds handlers editor writefun)
							event evalOpts iworld
						Nothing
							= (ValueResult
								(Value (l, v) False)
								{TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
								change
								(Task (evalInteract l (Just v) st mode prompt sds handlers editor writefun))
							, iworld)
		)
		event evalOpts iworld
evalInteract l v st mode prompt sds handlers editor writefun event evalOpts=:{TaskEvalOpts|taskId,ts} iworld
	//An event for a parent?
	= (ValueResult
		(maybe NoValue (\v->Value (l, v) False) v)
		{TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
		NoChange
		(Task (evalInteract l v st mode prompt sds handlers editor writefun))
	, iworld)

uniqueMode :: (EditMode a) -> *(EditMode a)
uniqueMode mode = case mode of
	Enter    = Enter
	Update x = Update x
	View x   = View x
