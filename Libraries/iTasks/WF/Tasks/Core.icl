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

return :: !a -> (Task a)
return a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | TC, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (exception e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = accWorld $ tuple () o fun

accWorld :: !(*World -> *(a, *World)) -> Task a
accWorld fun = accWorldError (appFst ok o fun) id
where
	ok :: !a -> MaybeError String a
	ok x = Ok x

accWorldError :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | TC e & toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= case res of
			Error e = (Error (dynamic e, toString (errf e)), {IWorld|iworld & world = world})
			Ok v    = (Ok v, {IWorld|iworld & world = world})

accWorldOSError :: !(*World -> (MaybeOSError a, *World)) -> Task a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interactRW :: !(sds () r w) (InteractionHandlers r w v) (Editor v) -> Task (r,v)
	| iTask r & iTask v & TC r & TC w & RWShared sds
interactRW shared handlers editor
	= Task (readRegisterCompletely shared NoValue (\event->mkUIIfReset event (asyncSDSLoaderUI Read)) (evalInteractInit shared handlers editor modifyCompletely))

interactR :: (sds () r w) (InteractionHandlers r w v) (Editor v) -> Task (r,v)
	| iTask r & iTask v & TC r & TC w & Registrable sds
interactR shared handlers editor
	= Task (readRegisterCompletely shared NoValue (\event->mkUIIfReset event (asyncSDSLoaderUI Read)) (evalInteractInit shared handlers editor \_ _->modifyCompletely (\()->undef) nullShare))

//This initializes the editor state and continues with the actual interact task
evalInteractInit sds handlers editor writefun r _ evalOpts iworld
	//Get initial value
	# mode = handlers.onInit r
	# v = case mode of
		Enter    = Nothing
		Update x = Just x
		View x   = Just x
	= evalInteractInitWithValue r v (mode =: View _) sds handlers editor writefun evalOpts iworld

evalInteractInitWithValue r v mode sds handlers editor writefun evalOpts=:{TaskEvalOpts|taskId, lastEval} iworld
	# resetMode = case (mode, v) of
		(True, Just v) = View v
		(True, _)      = abort "view mode without value\n"
		(_, Nothing)   = Enter
		(_, Just v)    = Update v
	= case withVSt taskId (editor.Editor.genUI 'DM'.newMap [] resetMode) iworld of
		(Error e, iworld) = (ExceptionResult (exception e), iworld)
		(Ok (UI type attr items, st), iworld)
			# change = ReplaceUI (UI type (addClassAttr "interact" attr) items)
			= (ValueResult
				(maybe NoValue (\v->Value (r,v) False) v)
				(mkTaskEvalInfo lastEval)
				change
				(Task (evalInteract r (Just st) mode sds handlers editor writefun))
			, iworld)

evalInteract ::
	r
	(Maybe EditState)
	Bool
	(sds () r w)
	(InteractionHandlers r w v)
	(Editor v)
	(
		(r -> w)
		(sds () r w)
		(TaskValue (r,v))
		(Event -> UIChange)
		(w -> Event -> TaskEvalOpts -> *IWorld -> *(TaskResult (r,v),*IWorld))
		Event
		TaskEvalOpts
		*IWorld
		-> *(TaskResult (r,v),*IWorld))
	Event
	TaskEvalOpts
	*IWorld
	-> *(TaskResult (r,v),*IWorld)
	| iTask r & iTask v & TC r & TC w & Registrable sds
evalInteract _ _ _ _ _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
	= (DestroyedResult, 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld)
evalInteract r mst mode sds handlers editor writefun event=:(EditEvent eTaskId name edit) evalOpts=:{taskId,lastEval} iworld
	| isNothing mst = (ExceptionResult (exception "corrupt editor state"), iworld)
	| eTaskId == taskId
		# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) (fromJust mst)) iworld
		= case res of
			Ok (change, st)
				= case editor.Editor.valueFromState st of
					Just nv
						# mbf = handlers.InteractionHandlers.onEdit nv
						= case mbf of
							//We have an update function
							Just f = writefun f sds NoValue (\_->change)
								// We cannot just do this because this will loop endlessly:
								// (\_->evalInteract (Just v) st mode sds handlers editor writefun)
								// Therefore we delay it by returning the continuation in a value instead of directly:
								(\w event {TaskEvalOpts|lastEval} iworld->
									(ValueResult
										(Value (r,nv) False)
										(mkTaskEvalInfo lastEval)
										change
										(Task (evalInteract r (Just st) mode sds handlers editor writefun))
									, iworld))
								event evalOpts iworld
							//There is no update function
							Nothing
								= (ValueResult
									(Value (r,nv) False)
									(mkTaskEvalInfo lastEval)
									change
									(Task (evalInteract r (Just st) mode sds handlers editor writefun))
								, iworld)
					Nothing
						= (ValueResult
							NoValue
							(mkTaskEvalInfo lastEval)
							change
							(Task (evalInteract r (Just st) mode sds handlers editor writefun))
						, iworld)
			Error e = (ExceptionResult (exception e), iworld)
evalInteract r mst mode sds handlers editor writefun ResetEvent evalOpts iworld
	# v = maybe Nothing editor.Editor.valueFromState mst
	= evalInteractInitWithValue r v mode sds handlers editor writefun evalOpts iworld
evalInteract r mst mode sds handlers editor writefun event=:(RefreshEvent taskIds) evalOpts=:{taskId,lastEval} iworld
	| isNothing mst = (ExceptionResult (exception "corrupt editor state"), iworld)
	# st = fromJust mst
	# v = editor.Editor.valueFromState st
	| 'DS'.member taskId taskIds
		= readRegisterCompletely sds (maybe NoValue (\v->Value (r,v) False) v) (\e->mkUIIfReset e (asyncSDSLoaderUI Read))
			(\r event evalOpts iworld
				# (mbV, mbf) = handlers.InteractionHandlers.onRefresh r v
				# mbChange = case mbV of
					Just v  = withVSt taskId (editor.Editor.onRefresh [] v st) iworld
					Nothing = (Ok (NoChange, st), iworld)
				= case mbChange of
					(Error e, iworld) = (ExceptionResult (exception e), iworld)
					(Ok (change, st), iworld)
						# v = editor.Editor.valueFromState st
						= case mbf of
							Just f = writefun f sds NoValue (\_->change)
								(\_->evalInteract r (Just st) mode sds handlers editor writefun)
								event evalOpts iworld
							Nothing
								= (ValueResult
									(maybe NoValue (\v -> Value (r,v) False) v)
									(mkTaskEvalInfo lastEval)
									change
									(Task (evalInteract r (Just st) mode sds handlers editor writefun))
								, iworld)
			)
			event evalOpts iworld
evalInteract r mst mode sds handlers editor writefun event {lastEval} iworld
	# v = maybe Nothing editor.Editor.valueFromState mst
	//An event for a sibling?
	= (ValueResult
		(maybe NoValue (\v->Value (r,v) False) v)
		(mkTaskEvalInfo lastEval)
		NoChange
		(Task (evalInteract r mst mode sds handlers editor writefun))
	, iworld)

uniqueMode :: (EditMode a) -> *(EditMode a)
uniqueMode mode = case mode of
	Enter    = Enter
	Update x = Update x
	View x   = View x
