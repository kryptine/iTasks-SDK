implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import iTaskClass, Task, TaskContext, TaskEval, TaskStore, TuningCombinators, TUIDefinition
from SharedCombinators		import :: Shared
from Shared					import qualified readShared, writeShared, isSharedChanged, updateShared, getSharedTimestamp
from Shared					import :: SharedGetTimestamp, :: SharedWrite, :: SharedRead, :: SharedId, :: ReadWriteShared(..), :: ReadOnlyShared(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..), :: Control(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from SystemData				import topLevelTasks
from Map					import qualified get

derive class iTask WorkOnProcessState

PARTS_STORE				:== "parts"
LOCAL_STORE				:== "local"
LAST_EDIT_STORE			:== "lastEdit"
EVENT_STORE				:== "event"
EDIT_CONFLICT_STORE		:== "editConflict"
EDIT_CONFLICT_WARNING	:== "An edit conflict occurred. The form was refreshed with the most recent value."

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask ("return", "Return a value") (\_ iworld -> (TaskStable a (NoRep,[]) TCEmpty, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask ("throw", "Throw an exception") (\_ iworld -> (TaskException (dynamic e) (toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask ("Read shared", "Reads a shared value") eval
where
	eval taskNr iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskStable val (NoRep,[]) TCEmpty
			Error e	= taskException (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask ("Write shared", "Writes a shared value") eval
where
	eval taskNr iworld
		# (res,iworld)	='Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskStable val (NoRep,[]) TCEmpty
			Error e	= taskException (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask ("Update shared", "Updates a shared value") eval
where
	eval taskNr iworld
		# (val,iworld)	= 'Shared'.updateShared shared fun iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		= (TaskStable (fromOk val) (NoRep,[]) TCEmpty, iworld)

interact :: !d !((Maybe l) r -> l) ![InteractionPart l r] !(Maybe l) !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r
interact desc initFun parts initLocal shared = mkTask desc init edit eval
where
	init taskNr iworld			//Create the initial views
		# (mbrvalue,iworld) 	= 'Shared'.readShared shared iworld
		| isError mbrvalue		= abort ("interact value fail " +++ fromError mbrvalue) //(TCEmpty, iworld)
		# rvalue				= fromOk mbrvalue
		# (rts,iworld)			= getShareTimestamp shared iworld
		# lvalue				= initFun initLocal rvalue
		= (TCInteract (toJSON lvalue) (initParts lvalue rvalue parts) rts Nothing, iworld)

	initParts l r parts = map (initPart l r) parts
	
	initPart l r (DisplayPart f)	= (toJSON (f l r),Untouched)
	initPart l r (FormPart f _ _)
		# (_,encv,maskv)	= initFormView (f l r)
		= (encv,maskv)
	
	initFormView BlankForm
		# v = defaultValue
		= (v,toJSON v,Untouched)
	initFormView (FilledForm v)
		= (v,toJSON v, defaultMask v)
	
	getShareTimestamp shared iworld
		# (mbts,iworld)			= 'Shared'.getSharedTimestamp shared iworld
		| isError mbts			= (Timestamp 0, iworld)
								= (fromOk mbts,iworld)
		
	//Rewrite lucky events to events that target this task
	edit taskNo (LuckyEvent e) context iworld = (edit taskNo (TaskEvent [] e) context iworld)
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit taskNo (TaskEvent [] (dps,editv)) context=:(TCInteract encl views rts _) iworld=:{IWorld|timestamp,latestEvent}		
		//Read latest versions of states
		# (mbrval,iworld) 			= 'Shared'.readShared shared iworld
		| isError mbrval			= (context, iworld)
		# r							= fromOk mbrval
		# l							= fromJust (fromJSON encl)
		//Split datapath into datapath & part index
		# (idx,dp)					= splitDataPath dps
		| idx >= length parts		= (context, iworld) 
		//Apply the event to the view
		# (l,view,iworld)			= updateView l r dp editv (parts !! idx) (views !! idx) iworld
		= (TCInteract (toJSON l) (updateAt idx view views) rts (Just (dps,editv)), iworld)
		
	edit _ _ context iworld = (context,iworld)
	
	splitDataPath dp
		= (hd dplist, dataPathFromList (reverse (tl dplist)))
	where
		dplist = reverse (dataPathList (s2dp dp))
	
	updateView l r dp editv (DisplayPart f) view iworld = (l,view,iworld)
	updateView l r dp editv (FormPart _ _ f) view=:(encv,maskv) iworld
		# (v,encv,maskv,iworld)	= applyEditEvent dp editv (fromJust (fromJSON encv)) encv maskv iworld	
		# (l,mbform) = f l r (Just v)
		= case mbform of
			Nothing 	= (l,(encv,maskv),iworld)
			Just form
				# (_,encv,maskv) = initFormView form
				= (l,(encv,maskv),iworld)
			
	applyEditEvent dp editv v encv maskv iworld
		| dataPathLevel dp == 0 //Replace entire value
			= case fromJSON editv of
				Just nv = (nv,editv,defaultMask nv,iworld)
				Nothing	= (v,encv,maskv,iworld)
		# (v,maskv,iworld)	= updateValueAndMask dp editv v maskv iworld
		= (v,toJSON v,maskv,iworld)
	
	//TODO: Update rts timestamp			
	eval taskNo props event tuiTaskNo repAs context=:(TCInteract encl views rts mbEdit) iworld=:{IWorld|timestamp}
		# (mbrvalue,iworld) 			= 'Shared'.readShared shared iworld
		| isError mbrvalue				= (sharedException mbrvalue, iworld)
		# rvalue						= fromOk mbrvalue	
		# (mbchanged,iworld)			= 'Shared'.isSharedChanged shared rts iworld
		| isError mbchanged				= (sharedException mbchanged, iworld)
		# changed						= fromOk mbchanged
		# (rts,iworld)					= if changed (getShareTimestamp shared iworld) (rts,iworld)
		# lvalue						= fromJust (fromJSON encl)
		# (lvalue,reps,views,iworld)	= evalParts 0 taskNo repAs (fmap (appFst s2dp) mbEdit) changed lvalue rvalue parts views iworld
		# (rep,actions) = case repAs of
			(RepAsTUI layout)	= appFst TUIRep (mergeTUI props layout [tui \\ TUIRep tui <- reps] [])
			_					= (ServiceRep (flatten [part \\ (ServiceRep part) <- reps]), [])
		
		# result						= Just (lvalue,rvalue)
		= (TaskInstable result (rep,actions) (TCInteract (toJSON lvalue) views rts Nothing), iworld)
	eval taskNo props event tuiTaskNo repAs context iworld
		= (taskException "Corrupt context in interact",iworld)


	evalParts idx taskNo repAs mbEvent changed l r [] [] iworld
		= (l,[],[],iworld)
	evalParts idx taskNo repAs mbEvent changed l r [p:ps] [v:vs] iworld	
		# (nl,rep,view,iworld)		= evalPart idx taskNo repAs mbEvent changed l r p v iworld
		# (nnl,reps,views,iworld)	= evalParts (idx + 1) taskNo repAs mbEvent changed nl r ps vs iworld
		= (nnl,[rep:reps],[view:views],iworld)
		
	evalPart idx taskNo repAs mbEvent changed l r part view=:(encv,maskv) iworld = case part of
		DisplayPart f
			//Simply visualize the view
			# (rep,iworld) 	= displayRep idx taskNo repAs f l r encv iworld
			= (l,rep,view,iworld)
		
		FormPart initf sharef viewf
			//Update the local value and possibly the view if the share has changed
			# v						= fromJust (fromJSON encv)
			# (l,v,encv,maskv)		= if changed (l,v,encv,maskv) (refreshForm sharef l r v encv maskv)
			//Create an editor for the view
			# (rep,iworld)			= editorRep idx taskNo repAs initf v encv maskv mbEvent iworld
			= (l,rep,(encv,maskv),iworld)
			
	displayRep idx taskNo (RepAsTUI _) f l r encv iworld
		# (editor,iworld) = visualizeAsDisplay (f l r) iworld
		= (mbToTUIRep editor,iworld)
	displayRep idx taskNo _ f l r encv iworld
		= (ServiceRep [(taskNrToString taskNo,idx,encv)],iworld)
	
	editorRep idx taskNo (RepAsTUI _) f v encv maskv mbEvent iworld
		# (editor,iworld) = visualizeAsEditor v (taskNrToString taskNo) idx (verifyForm v maskv) mbEvent iworld
		= (mbToTUIRep editor,iworld)
	editorRep idx taskNo _ f v encv maskv mbEvent iworld
		= (ServiceRep [(taskNrToString taskNo,idx,encv)],iworld)
	
	refreshForm f l r v encv maskv = case f l r (Just v) False of
		(l,Nothing)			= (l,v,encv,maskv)
		(l,Just form)
			# (v,encv,maskv)	= initFormView form
			= (l,v,encv,maskv)
		
	mergeTUI meta layout parts actions
		# ilayout	= case layout of
			(InteractionLayouter ilayout)	= ilayout
			_								= defaultInteractionLayout
		= ilayout	{ title = meta.TaskMeta.title
					, instruction = meta.TaskMeta.instruction
					, content = parts
					, actions = actions
					, type = meta.interactionType
					, localInteraction = meta.TaskMeta.localInteraction
					, warning = Nothing
					}
						
	mbToTUIRep Nothing		= NoRep
	mbToTUIRep (Just def)	= TUIRep def 
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

workOn :: !ProcessId -> Task WorkOnProcessState
workOn (SessionProcess sessionId)
	= abort "workOn applied to session process"
workOn processId
	= mkTask ("Work on","Work on another top-level instance.") init edit eval
where
	init taskNr iworld = (TCEmpty, iworld)
	
	edit taskNr event _ iworld
		//Load instance
		# (mbContext,iworld)	= loadTaskInstance processId iworld
		| isError mbContext		= (TCEmpty, iworld)
		//Apply event to instance
		# (mbContext,iworld)	= editInstance (Just event) (fromOk mbContext) iworld
		//Store instance
		| isError mbContext		= (TCEmpty, iworld)
		# iworld				= storeTaskInstance (fromOk mbContext) iworld
		= (TCEmpty, iworld)
		
	eval taskNr props event tuiTaskNr (RepAsTUI layout) _ iworld=:{evalStack}
		//Check for cycles
		| isMember processId evalStack
			=(taskException WorkOnDependencyCycle, iworld)
		//Load instance
		# (mbContext,iworld)		= loadTaskInstance processId iworld
		| isError mbContext	
			//If the instance can not be found, check if it was only just added by an
			//appendTask in the same session. If so, create a temporary result and trigger
			//reevaluation.
			# (found,iworld)	= checkIfAddedGlobally processId iworld
			| found
				= (TaskInstable Nothing (TUIRep (stringDisplay "Task finished"),[]) TCEmpty, {iworld & readShares = Nothing})
			| otherwise
				= (taskException WorkOnNotFound ,iworld)
		//Eval instance
		# target = case processId of
			(WorkflowProcess procNo)	= [procNo,changeNo (fromOk mbContext)]
			(EmbeddedProcess _ taskId)	= reverse (taskNrFromString taskId)
		# (mbResult,context,iworld)	= evalInstance target event True (fromOk mbContext) iworld
		= case mbResult of
			Error e				= (taskException WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (result,rep,actions,iworld) = case result of
					(TaskInstable _ (rep,actions) _)	= (WOActive, rep, actions, iworld)
					(TaskStable _ (rep,actions) _)		= (WOFinished, rep, actions, iworld)
					(TaskException _ err)				= (WOExcepted, TUIRep (stringDisplay ("Task excepted: " +++ err)), [], iworld)
				= case result of
					WOFinished	= (TaskStable WOFinished (rep,actions) TCEmpty, iworld)
					_			= (TaskInstable (Just result) (rep,actions) TCEmpty, iworld)
				
	changeNo (TaskContext _ _ _ _ n _) = n

	checkIfAddedGlobally (WorkflowProcess procNo) iworld=:{parallelControls,currentUser}
		= case 'Map'.get (toString topLevelTasks) parallelControls of
			Just (_,controls)
				= (isMember procNo [i \\ AppendTask i currentUser _ <- controls], iworld)
			_
				= (False,iworld)
	checkIfAddedGlobally _ iworld = (False,iworld)

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask ("Run world function", "Run a world function.") eval
where
	eval taskNr iworld=:{IWorld|world}
		= (TaskStable Void (NoRep,[]) TCEmpty, {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask ("Run world function", "Run a world function and get result.") eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world) = fun world
		= (TaskStable res (NoRep,[]) TCEmpty, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask ("Run a world function", "Run a world function with error handling.") eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world)	= fun world
		= case res of
			Error e		= (taskException (errf e), {IWorld|iworld & world = world})
			Ok v		= (TaskStable v (NoRep,[]) TCEmpty, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
