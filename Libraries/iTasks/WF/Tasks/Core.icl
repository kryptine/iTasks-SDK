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
accWorld fun = accWorldError (appFst Ok o fun) \_->""

accWorldError :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= case res of
			Error e = (Error (exception (errf e)), {IWorld|iworld & world = world})
			Ok v    = (Ok v, {IWorld|iworld & world = world})

accWorldOSError :: !(*World -> (MaybeOSError a, *World)) -> Task a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interactRW :: (Editor r w) !(sds () (Maybe r) w) -> Task r | iTask r & TC r & TC w & RWShared sds
interactRW editor sds = Task
	(readRegisterCompletely sds NoValue
		(\event -> mkUIIfReset event (asyncSDSLoaderUI Read))
		(evalInteractInit sds editor modifyCompletely)
	)
interactR :: (Editor r w) (sds () (Maybe r) w) -> Task r | iTask r & TC r & TC w & Registrable sds
interactR editor sds = Task
	(readRegisterCompletely sds NoValue
		(\event->mkUIIfReset event (asyncSDSLoaderUI Read))
		(evalInteractInit sds editor (\_ _->modifyCompletely (\()->undef) nullShare))
	)

//This initializes the editor state and continues with the actual interact task
evalInteractInit sds editor writefun mbr event evalOpts=:{TaskEvalOpts|taskId} iworld
	= evalInteract mbr Nothing sds editor writefun ResetEvent evalOpts iworld

evalInteract ::
	(Maybe r)
	(Maybe EditState)
	(sds () (Maybe r) w)
	(Editor r w)
	(
		((Maybe r) -> w)
		(sds () (Maybe r) w)
		(TaskValue r)
		(Event -> UIChange)
		(w -> Event -> TaskEvalOpts -> *IWorld -> *(TaskResult r, *IWorld))
		Event
		TaskEvalOpts
		*IWorld
		-> *(TaskResult r,*IWorld))
	Event
	TaskEvalOpts
	*IWorld
	-> *(TaskResult r,*IWorld)
	| iTask r & TC r & TC w & Registrable sds
evalInteract _ _ _ _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
	= (DestroyedResult, 'SDS'.clearTaskSDSRegistrations ('DS'.singleton taskId) iworld)
evalInteract mbr Nothing sds editor writefun event=:(EditEvent eTaskId name edit) evalOpts=:{taskId,lastEval} iworld
	= (ExceptionResult (exception "corrupt editor state"), iworld)
evalInteract mbr (Just st) sds editor writefun event=:(EditEvent eTaskId name edit) evalOpts=:{taskId,lastEval} iworld | eTaskId == taskId
	# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) st) iworld
	= case res of
		Ok (change, st, mbw) = case mbw of
			//We have an update function
			Just w = writefun (const w) sds NoValue (\_->change)
				// We cannot just do this because this will loop endlessly:
				// Therefore we delay it by returning the continuation in a value instead of directly:
				(\w event {TaskEvalOpts|lastEval} iworld ->
					(ValueResult
						(maybe NoValue (\r -> Value r False) mbr)
						(mkTaskEvalInfo lastEval)
						change
						(Task (evalInteract mbr (Just st) sds editor writefun))
					, iworld))
				event evalOpts iworld
			//There is no update function
			Nothing
				= (ValueResult
					(maybe NoValue (\r -> Value r False) mbr)
					(mkTaskEvalInfo lastEval)
					change
					(Task (evalInteract mbr (Just st) sds editor writefun))
				, iworld)

		Error e = (ExceptionResult (exception e), iworld)

evalInteract mbr mst sds editor writefun ResetEvent evalOpts=:{taskId,lastEval} iworld
	= case withVSt taskId (editor.Editor.genUI 'DM'.newMap [] (maybe Enter (\r -> Update r) mbr)) iworld of
		(Error e, iworld) = (ExceptionResult (exception e), iworld)
		(Ok (UI type attr items, st), iworld)
			# change = ReplaceUI (UI type (addClassAttr "interact" attr) items)
			= (ValueResult
				(maybe NoValue (\r -> Value r False) mbr)
				(mkTaskEvalInfo lastEval)
				change
				(Task (evalInteract mbr (Just st) sds editor writefun))
			, iworld)

evalInteract mbr Nothing sds editor writefun event=:(RefreshEvent taskIds _) evalOpts=:{taskId,lastEval} iworld
	= (ExceptionResult (exception "corrupt editor state"), iworld)
evalInteract mbr (Just st) sds editor writefun event=:(RefreshEvent taskIds _) evalOpts=:{taskId,lastEval} iworld | 'DS'.member taskId taskIds
	= readRegisterCompletely sds (maybe NoValue (\r -> Value r False) mbr) (\e->mkUIIfReset e (asyncSDSLoaderUI Read))
		(\mbr event evalOpts iworld
			# mbChange = case mbr of
				Just r  = withVSt taskId (editor.Editor.onRefresh [] r st) iworld
				Nothing = (Ok (NoChange, st, Nothing), iworld)
			= case mbChange of
				(Ok (change, st, mbw), iworld)
					= case mbw of
						Just w = writefun (const w) sds NoValue (\_->change)
							(\_-> evalInteract mbr (Just st) sds editor writefun)
							event evalOpts iworld
						Nothing
							= (ValueResult
								(maybe NoValue (\r -> Value r False) mbr)
								(mkTaskEvalInfo lastEval)
								change
								(Task (evalInteract mbr (Just st) sds editor writefun))
							, iworld)
				(Error e, iworld) = (ExceptionResult (exception e), iworld)
		)
		event evalOpts iworld
evalInteract mbr mst sds editor writefun event {lastEval} iworld
	//An event for a sibling?
	= (ValueResult
		(maybe NoValue (\r->Value r False) mbr)
		(mkTaskEvalInfo lastEval)
		NoChange
		(Task (evalInteract mbr mst sds editor writefun))
	, iworld)

