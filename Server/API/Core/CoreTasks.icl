implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import iTaskClass, Task, TaskContext, TaskEval, TaskStore, TUIDefinition, LayoutCombinators
from SharedCombinators		import :: Shared
from Shared					import qualified readShared, writeShared, isSharedChanged, updateShared, getSharedVersion
from Shared					import :: SharedGetVersion, :: SharedWrite, :: SharedRead, :: SharedId, :: ReadWriteShared(..), :: ReadOnlyShared(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..), :: Control(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from SystemData				import topLevelTasks
from Map					import qualified get

derive class iTask WorkOnProcessState

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\_ iworld -> (TaskStable a NoRep TCEmpty, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\_ iworld -> (TaskException (dynamic e) (toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskNr iworld
		# (val,iworld) = 'Shared'.readShared shared iworld
		# res = case val of
			Ok val	= TaskStable val NoRep TCEmpty
			Error e	= taskException (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskNr iworld
		# (res,iworld)	='Shared'.writeShared shared val iworld
		# res = case res of
			Ok _	= TaskStable val NoRep TCEmpty
			Error e	= taskException (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask eval
where
	eval taskNr iworld
		# (val,iworld)	= 'Shared'.updateShared shared fun iworld
		| isError val	= (taskException (SharedException (fromError val)), iworld)
		= (TaskStable (fromOk val) NoRep TCEmpty, iworld)

interact :: !d !((Maybe l) r -> l) ![InteractionPart l r] !(Maybe l) !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r
interact desc initFun parts initLocal shared = mkTask init edit eval
where
	init taskNr iworld			//Create the initial views
		# (mbrvalue,iworld) 	= 'Shared'.readShared shared iworld
		| isError mbrvalue		= (TCEmpty, iworld)
		# rvalue				= fromOk mbrvalue
		# (version,iworld)		= getSharedVersion shared iworld
		# lvalue				= initFun initLocal rvalue
		= (TCInteract (toJSON lvalue) (initParts lvalue rvalue parts) version, iworld)

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
		# (mbv,iworld)			= 'Shared'.getSharedVersion shared iworld
		| isError mbv			= (0, iworld)
								= (fromOk mbv,iworld)
		
	//Rewrite lucky events to events that target this task
	edit taskNo (LuckyEvent e) context iworld = (edit taskNo (TaskEvent [] e) context iworld)
	//There is an edit event for this task (because the location part of the event is the empty list)
	edit taskNo (TaskEvent [] (dps,editv)) context=:(TCInteract encl views version) iworld=:{IWorld|timestamp,latestEvent}		
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
		= (TCInteract (toJSON l) (updateAt idx view views) version, iworld)
		
	edit _ _ context iworld = (context,iworld)
	
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
		
	eval taskNo eEvent cEvent tuiTaskNo repAs context=:(TCInteract encl views rversion) iworld=:{IWorld|timestamp}
		# (mbrvalue,iworld) 				= 'Shared'.readShared shared iworld
		| isError mbrvalue					= (sharedException mbrvalue, iworld)
		# rvalue							= fromOk mbrvalue	
		# (mbchanged,iworld)				= 'Shared'.isSharedChanged shared rversion iworld
		| isError mbchanged					= (sharedException mbchanged, iworld)
		# changed							= fromOk mbchanged
		# (rversion,iworld)					= if changed (getSharedVersion shared iworld) (rversion,iworld)
		# lvalue							= fromJust (fromJSON encl)
		# mbEdit	= case eEvent of
			Just (TaskEvent [] e)	= Just e
			_						= Nothing
		# (lvalue,reps,views,valid,iworld)	= evalParts 0 taskNo repAs (fmap (appFst s2dp) mbEdit) changed lvalue rvalue parts views iworld
		# rep = case repAs of
			(RepAsTUI layout)
				# layoutfun	= fromMaybe DEFAULT_LAYOUT layout
				= TUIRep (layoutfun [gui \\ (TUIRep gui) <- reps] [] (initAttributes desc))
			_	
				# (parts,actions) = unzip [(part,actions) \\ (ServiceRep (part,actions)) <- reps]
				= ServiceRep (flatten parts,flatten actions)
		
		# result							= if valid (Just (lvalue,rvalue)) Nothing 
		= (TaskInstable result rep (TCInteract (toJSON lvalue) views rversion), iworld)
	eval taskNo eEvent cEvent tuiTaskNo repAs TCEmpty iworld
		= (taskException "Failed to initialize interact",iworld)
	eval taskNo eEvent cEvent tuiTaskNo repAs context iworld
		= (taskException "Corrupt context in interact",iworld)

	evalParts idx taskNo repAs mbEvent changed l r [] [] iworld
		= (l,[],[],True,iworld)
	evalParts idx taskNo repAs mbEvent changed l r [p:ps] [v:vs] iworld	
		# (nl,rep,view,pvalid,iworld)	= evalPart idx taskNo repAs mbEvent changed l r p v iworld
		# (nnl,reps,views,valid,iworld)	= evalParts (idx + 1) taskNo repAs mbEvent changed nl r ps vs iworld
		= (nnl,[rep:reps],[view:views],pvalid && valid,iworld) //All parts have to be valid
		
	evalPart idx taskNo repAs mbEvent changed l r part view=:(encv,maskv,dirty) iworld = case part of
		DisplayPart f
			//Simply visualize the view
			# (rep,iworld) 	= displayRep idx taskNo repAs f l r encv iworld
			= (l,rep,view,True,iworld)
		
		FormPart initf sharef viewf
			//Update the local value and possibly the view if the share has changed
			# v							= fromJust (fromJSON encv)
			# vermask					= verifyForm v maskv
			# (l,v,encv,maskv,vermask,dirty)
				= if changed (refreshForm sharef l r v encv maskv vermask dirty) (l,v,encv,maskv,vermask,dirty) 
			//Create an editor for the view
			# (rep,iworld)				= editorRep idx taskNo repAs initf v encv maskv vermask mbEvent iworld
			= (l,rep,(encv,maskv,dirty),isValidValue vermask,iworld)
			
	displayRep idx taskNo (RepAsTUI _) f l r encv iworld
		# (editor,iworld) = visualizeAsDisplay (f l r) iworld
		= (TUIRep (editor,[],[]),iworld)
	displayRep idx taskNo _ f l r encv iworld
		= (ServiceRep ([(taskNrToString taskNo,idx,encv)],[]),iworld)
	
	editorRep idx taskNo (RepAsTUI _) f v encv maskv vermask mbEvent iworld
		# (editor,iworld) = visualizeAsEditor v (taskNrToString taskNo) idx vermask mbEvent iworld
		= (TUIRep (editor,[],[]),iworld)
	editorRep idx taskNo _ f v encv maskv vermask mbEvent iworld
		= (ServiceRep ([(taskNrToString taskNo,idx,encv)],[]),iworld)
	
	refreshForm f l r v encv maskv vermask dirty
		= case f l r (if (isValidValue vermask) (Just v) Nothing) dirty of
			(l,Nothing)			= (l,v,encv,maskv,vermask,dirty)
			(l,Just form)
				# (v,encv,maskv)	= initFormView form
				# vermask			= verifyForm v maskv
				= (l,v,encv,maskv,vermask,False)
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = taskException (SharedException (fromError err))

workOn :: !ProcessId -> Task WorkOnProcessState
workOn (SessionProcess sessionId)
	= abort "workOn applied to session process"
workOn processId
	= mkTask init edit eval
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
		
	eval taskNr eEvent cEvent tuiTaskNr (RepAsTUI layout) _ iworld=:{evalStack}
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
				= (TaskInstable Nothing (TUIRep (Just (stringDisplay "Task finished"),[],[])) TCEmpty, {iworld & readShares = Nothing})
			| otherwise
				= (taskException WorkOnNotFound ,iworld)
		//Eval instance
		# target = case processId of
			(WorkflowProcess procNo)	= [procNo,changeNo (fromOk mbContext)]
			(EmbeddedProcess _ taskId)	= reverse (taskNrFromString taskId)
		# (mbResult,context,iworld)	= evalInstance target eEvent cEvent True (fromOk mbContext) iworld
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
					WOFinished	= (TaskStable WOFinished rep TCEmpty, iworld)
					_			= (TaskInstable (Just result) rep TCEmpty, iworld)
				
	changeNo (TaskContext _ _ _ _ n _) = n

	checkIfAddedGlobally (WorkflowProcess procNo) iworld=:{parallelControls,currentUser}
		= case 'Map'.get (toString topLevelTasks) parallelControls of
			Just (_,controls)
				= (isMember procNo [i \\ AppendTask i currentUser _ <- controls], iworld)
			_
				= (False,iworld)
	checkIfAddedGlobally _ iworld = (False,iworld)

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask eval
where
	eval taskNr iworld=:{IWorld|world}
		= (TaskStable Void NoRep TCEmpty, {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world) = fun world
		= (TaskStable res NoRep TCEmpty, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskNr iworld=:{IWorld|world}
		# (res,world)	= fun world
		= case res of
			Error e		= (taskException (errf e), {IWorld|iworld & world = world})
			Ok v		= (TaskStable v NoRep TCEmpty, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
