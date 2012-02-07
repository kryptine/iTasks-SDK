implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import iTaskClass, Task, TaskContext, TaskEval, TaskStore, TUIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, write, getVersion, readWrite, :: RWRes(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..), :: Control(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from SystemData				import topLevelTasks
from Map					import qualified get

derive class iTask WorkOnProcessState

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld -> (TaskStable a NoRep (TCEmpty taskId), iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (TaskException (dynamic e) (toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld
		# (val,iworld) = 'SharedDataSource'.read shared iworld
		# res = case val of
			Ok (val,_)	= TaskStable val NoRep (TCEmpty taskId)
			Error e		= taskException (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld
		# (res,iworld)	='SharedDataSource'.write val shared iworld
		# res = case res of
			Ok _	= TaskStable val NoRep (TCEmpty taskId)
			Error e	= taskException (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask eval
where
	eval taskId iworld
		# (val,iworld)	= 'SharedDataSource'.readWrite (\r _ -> let w = fun r in 'SharedDataSource'.Write w w) shared iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		= (TaskStable (fromOk val) NoRep (TCEmpty taskId), iworld)

interact :: !d !((Maybe l) r -> l) ![InteractionPart l r] !(Maybe l) !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r
interact desc initFun parts initLocal shared = mkTask init edit eval
where
	init taskId iworld			//Create the initial views
		# (mbrvalue,iworld) 	= 'SharedDataSource'.read shared iworld
		| isError mbrvalue		= abort (fromError mbrvalue)//(TCEmpty taskId, iworld)
		# (rvalue,_)			= fromOk mbrvalue
		# (version,iworld)		= getSharedVersion shared iworld
		# lvalue				= initFun initLocal rvalue
		= (TCInteract taskId (toJSON lvalue) (initParts lvalue rvalue parts) version, iworld)

	initParts l r parts = map (initPart l r) parts
	
	initPart l r (DisplayPart f)	= (toJSON (f l r),Untouched, False)
	initPart l r (FormPart f _ _)
		# (_,encv,maskv)	= initFormView (f l r)
		= (encv,maskv,False)
	
	initFormView BlankForm
		# v		= defaultValue
		= (v,toJSON v,Untouched)
	initFormView (FilledForm v)
		= (v,toJSON v, defaultMask v)
	
	getSharedVersion shared iworld
		# (mbv,iworld)			= 'SharedDataSource'.getVersion shared iworld
		| isError mbv			= (0, iworld)
								= (fromOk mbv,iworld)
		
	//Rewrite lucky events to events that target this task
	edit (LuckyEvent e) context=:(TCInteract taskId _ _ _) iworld	
			= (edit (TaskEvent taskId e) context iworld)
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit (TaskEvent targetId (dps,editv)) context=:(TCInteract taskId encl views version) iworld=:{IWorld|timestamp,latestEvent}		
		| targetId <> taskId
			= (context,iworld)
		//Read latest versions of states
		# (mbrval,iworld) 			= 'SharedDataSource'.read shared iworld
		| isError mbrval			= (context, iworld)
		# (r,_)						= fromOk mbrval
		# l							= fromJust (fromJSON encl)
		//Split datapath into datapath & part index
		# (idx,dp)					= splitDataPath dps
		| idx >= length parts		= (context, iworld) 
		//Apply the event to the view
		# (l,view,iworld)			= updateView l r dp editv (parts !! idx) (views !! idx) iworld
		= (TCInteract taskId (toJSON l) (updateAt idx view views) version, iworld)
		
	edit _ context iworld = (context,iworld)
	
	splitDataPath dp
		= (hd dplist, dataPathFromList (reverse (tl dplist)))
	where
		dplist = reverse (dataPathList (s2dp dp))
	
	updateView l r dp editv (DisplayPart f) view iworld = (l,view,iworld)
	updateView l r dp editv (FormPart _ _ f) view=:(encv,maskv,dirty) iworld
		# (v,encv,maskv,iworld)	= applyEditEvent dp editv (fromJust (fromJSON encv)) encv maskv iworld	
		# valid = isValidValue (verifyForm v maskv)
		# (l,mbform) = f l r (if valid (Just v) Nothing)
		= case mbform of
			Nothing 	= (l,(encv,maskv,True),iworld)
			Just form
				# (_,encv,maskv) = initFormView form
				= (l,(encv,maskv,True),iworld)
			
	applyEditEvent dp editv v encv maskv iworld
		| dataPathLevel dp == 0 //Replace entire value
			= case fromJSON editv of
				Just nv = (nv,editv,defaultMask nv,iworld)
				Nothing	= (v,encv,maskv,iworld)
		# (v,maskv,iworld)	= updateValueAndMask dp editv v maskv iworld
		= (v,toJSON v,maskv,iworld)
		
	eval eEvent cEvent repAs context=:(TCInteract taskId encl views rversion) iworld=:{IWorld|timestamp}
		# (mbrvalue,iworld) 				= 'SharedDataSource'.read shared iworld
		| isError mbrvalue					= (sharedException mbrvalue, iworld)
		# (rvalue,_)						= fromOk mbrvalue
		# (ver, iworld)						= 'SharedDataSource'.getVersion shared iworld
		| isError ver						= (sharedException ver, iworld)		
		# changed							= fromOk ver > rversion
		# (rversion,iworld)					= if changed (getSharedVersion shared iworld) (rversion,iworld)
		# lvalue							= fromJust (fromJSON encl)
		# mbEdit	= case eEvent of
			Just (TaskEvent t e)
				| t == taskId		= Just e
			_						= Nothing
		# (lvalue,reps,views,valid,iworld)	= evalParts 0 taskId repAs (fmap (appFst s2dp) mbEdit) changed lvalue rvalue parts views iworld
		# rep = case repAs of
			(RepAsTUI Nothing layout) 
				= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) [gui \\ (TUIRep gui) <- reps] [] (initAttributes desc))
			(RepAsTUI (Just target) layout)	//If there is a target set, we only produce a representation only if this task is the target
				| target == taskId
					= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) [gui \\ (TUIRep gui) <- reps] [] (initAttributes desc))
				| otherwise
					= NoRep
			_	
				# (parts,actions,attributes) = unzip3 [(part,actions,attributes) \\ (ServiceRep (part,actions,attributes)) <- reps]
				= ServiceRep (flatten parts,flatten actions, flatten attributes)
		
		# result							= if valid (Just (lvalue,rvalue)) Nothing 
		= (TaskInstable result rep (TCInteract taskId (toJSON lvalue) views rversion), iworld)
	eval eEvent cEvent repAs (TCEmpty _) iworld
		= (taskException "Failed to initialize interact",iworld)
	eval eEvent cEvent repAs context iworld
		= (taskException "Corrupt context in interact",iworld)

	evalParts idx taskId repAs mbEvent changed l r [] [] iworld
		= (l,[],[],True,iworld)
	evalParts idx taskId repAs mbEvent changed l r [p:ps] [v:vs] iworld	
		# (nl,rep,view,pvalid,iworld)	= evalPart idx taskId repAs mbEvent changed l r p v iworld
		# (nnl,reps,views,valid,iworld)	= evalParts (idx + 1) taskId repAs mbEvent changed nl r ps vs iworld
		= (nnl,[rep:reps],[view:views],pvalid && valid,iworld) //All parts have to be valid
		
	evalPart idx taskId repAs mbEvent changed l r part view=:(encv,maskv,dirty) iworld = case part of
		DisplayPart f
			//Simply visualize the view
			# (rep,iworld) 	= displayRep idx taskId repAs f l r encv iworld
			= (l,rep,view,True,iworld)
		
		FormPart initf sharef viewf
			//Update the local value and possibly the view if the share has changed
			# v							= fromJust (fromJSON encv)
			# vermask					= verifyForm v maskv
			# (l,v,encv,maskv,vermask,dirty)
				= if changed (refreshForm sharef l r v encv maskv vermask dirty) (l,v,encv,maskv,vermask,dirty) 
			//Create an editor for the view
			# (rep,iworld)				= editorRep idx taskId repAs initf v encv maskv vermask mbEvent iworld
			= (l,rep,(encv,maskv,dirty),isValidValue vermask,iworld)
			
	displayRep idx taskId (RepAsTUI _ _) f l r encv iworld
		# (editor,iworld) = visualizeAsDisplay (f l r) iworld
		= (TUIRep (editor,[],[]),iworld)
	displayRep idx taskId _ f l r encv iworld
		= (ServiceRep ([(toString taskId,idx,encv)],[],[]),iworld)
	
	editorRep idx taskId (RepAsTUI _ _) f v encv maskv vermask mbEvent iworld
		# (editor,iworld) = visualizeAsEditor v taskId idx vermask mbEvent iworld
		= (TUIRep (editor,[],[]),iworld)
	editorRep idx taskId _ f v encv maskv vermask mbEvent iworld
		= (ServiceRep ([(toString taskId,idx,encv)],[],[]),iworld)
	
	refreshForm f l r v encv maskv vermask dirty
		= case f l r (if (isValidValue vermask) (Just v) Nothing) dirty of
			(l,Nothing)			= (l,v,encv,maskv,vermask,dirty)
			(l,Just form)
				# (v,encv,maskv)	= initFormView form
				# vermask			= verifyForm v maskv
				= (l,v,encv,maskv,vermask,False)
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

workOn :: !TaskId -> Task WorkOnProcessState
workOn target=:(TaskId topNo taskNo)
	= mkTask init edit eval
where
	init taskId iworld
		= (TCEmpty taskId, iworld)
	
	edit event context iworld
		//Load instance
		# (mbContext,iworld)	= loadTaskInstance (Right topNo) iworld
		| isError mbContext		= (context, iworld)
		//Apply event to instance
		# (mbContext,iworld)	= editInstance (Just event) (fromOk mbContext) iworld
		//Store instance
		| isError mbContext		= (context, iworld)
		# iworld				= storeTaskInstance (fromOk mbContext) iworld
		= (context, iworld)
		
	eval eEvent cEvent (RepAsTUI _ layout) (TCEmpty taskId) iworld=:{evalStack}
		//Check for cycles
		| isMember taskId evalStack
			=(taskException WorkOnDependencyCycle, iworld)
		//Load instance
		# (mbContext,iworld)		= loadTaskInstance (Right topNo) iworld
		| isError mbContext	
			//If the instance can not be found, check if it was only just added by an
			//appendTask in the same session. If so, create a temporary result and trigger
			//reevaluation.
			# (found,iworld)	= checkIfAddedGlobally topNo iworld
			| found
				= (TaskInstable Nothing (TUIRep (Just (stringDisplay "Task finished"),[],[])) (TCEmpty taskId), {iworld & readShares = Nothing})
			| otherwise
				= (taskException WorkOnNotFound ,iworld)
		//Eval instance
		# target					= if (taskNo == 0) Nothing (Just (TaskId topNo taskNo))
		# (mbResult,context,iworld)	= evalInstance eEvent cEvent target True (fromOk mbContext) iworld
		= case mbResult of
			Error e				= (taskException WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (result,rep,iworld) = case result of
					(TaskInstable _ rep _)			= (WOActive, rep, iworld)
					(TaskStable _ rep _)			= (WOFinished, rep, iworld)
					(TaskException _ err)			= (WOExcepted, TUIRep (Just (stringDisplay ("Task excepted: " +++ err)), [], []), iworld)
				= case result of
					WOFinished	= (TaskStable WOFinished rep (TCEmpty taskId), iworld)
					_			= (TaskInstable (Just result) rep (TCEmpty taskId), iworld)
				
	checkIfAddedGlobally topNo iworld=:{parallelControls,currentUser}
		= case 'Map'.get ("taskList:" +++ toString TopLevelTaskList) parallelControls of
			Just (_,controls)
				= (isMember topNo [i \\ AppendTask i currentUser _ _ <- controls], iworld)
			_
				= (False,iworld)
	checkIfAddedGlobally _ iworld = (False,iworld)

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (TaskStable Void NoRep (TCEmpty taskId), {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= (TaskStable res NoRep (TCEmpty taskId), {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world)	= fun world
		= case res of
			Error e		= (taskException (errf e), {IWorld|iworld & world = world})
			Ok v		= (TaskStable v NoRep (TCEmpty taskId), {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
