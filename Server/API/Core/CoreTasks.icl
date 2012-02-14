implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import qualified StdList
import iTaskClass, Task, TaskContext, TaskEval, TaskStore, TUIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, write, getVersion, readWrite, :: RWRes(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..)
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

import StdDebug

interact :: !d !((Maybe l) r -> l) ![InteractionPart l r] !(Maybe l) !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r
interact desc initFun parts initLocal shared = mkTask init eval
where
	init taskId iworld					//Create the initial views
		# (mbrvalue,iworld) 			= 'SharedDataSource'.read shared iworld
		| isError mbrvalue				= (TCEmpty taskId, iworld)
		# (rvalue,version)				= fromOk mbrvalue
		# lvalue						= initFun initLocal rvalue
		= (TCInteract taskId (toJSON lvalue) (initParts lvalue rvalue parts) version, iworld)

	initParts l r parts = map (initPart l r) parts
	
	initPart l r (DisplayPart f)	= (toJSON (f l r),Untouched, False)
	initPart l r (FormPart f _ _)
		# (_,encv,maskv)	= initFormView (f l r)
		= (encv,maskv,False)
	
	initFormView BlankForm		= (v, toJSON v, Untouched) where v = defaultValue
	initFormView (FilledForm v)	= (v, toJSON v, defaultMask v)
	
	eval eEvent cEvent repAs context=:(TCInteract taskId encl views lastShareVersion) iworld=:{IWorld|timestamp}
		# (mbrvalue,iworld) 				= 'SharedDataSource'.read shared iworld
		| isError mbrvalue					= (sharedException mbrvalue, iworld)
		# (rvalue,currentShareVersion)		= (fromOk mbrvalue)
		# changed							= currentShareVersion > lastShareVersion
		# lvalue							= fromJust (fromJSON encl)
		# mbEdit	= case eEvent of
			Just (TaskEvent t e)
				| t == taskId		= Just e
			Just (LuckyEvent e)		= Just e						
			_						= Nothing
		# (lvalue,reps,views,valid,iworld)	= evalParts 0 taskId repAs (fmap (appFst s2dp) mbEdit) changed lvalue rvalue parts views iworld
		# rep = case repAs of
			(RepAsTUI Nothing layout) 
				= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) SingleTask [gui \\ (TUIRep gui) <- reps] [] (initAttributes desc))
			(RepAsTUI (Just target) layout)	//If there is a target set, we only produce a representation only if this task is the target
				| target == taskId
					= TUIRep ((fromMaybe DEFAULT_LAYOUT layout) SingleTask [gui \\ (TUIRep gui) <- reps] [] (initAttributes desc))
				| otherwise
					= NoRep
			_	
				# (parts,actions,attributes) = unzip3 [(part,actions,attributes) \\ (ServiceRep (part,actions,attributes)) <- reps]
				= ServiceRep (flatten parts,flatten actions, flatten attributes)
		
		# result	= if valid (Just (lvalue,rvalue)) Nothing 
		= (TaskUnstable result rep (TCInteract taskId (toJSON lvalue) views currentShareVersion), iworld)
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
			//If the edit event is for this part, update the view
			# (l,v,encv,maskv,vermask,dirty,iworld)
				= if (matchEditEvent idx mbEvent) 
						(applyEditEvent idx mbEvent viewf l r v encv maskv vermask dirty iworld)
						(l,v,encv,maskv,vermask,dirty,iworld)
			//If the share has changed, update the view
			# (l,v,encv,maskv,vermask,dirty)
				= if changed (refreshForm sharef l r v encv maskv vermask dirty) (l,v,encv,maskv,vermask,dirty) 
			//Create an editor for the view
			# (rep,iworld)				= editorRep idx taskId repAs initf v encv maskv vermask mbEvent iworld
			= (l,rep,(encv,maskv,dirty),isValidValue vermask,iworld)
			
	displayRep idx taskId (RepAsTUI _ _) f l r encv iworld
		# (editor,iworld) = visualizeAsDisplay (f l r) iworld
		= (TUIRep (ViewPart,editor,[],[]),iworld)
	displayRep idx taskId _ f l r encv iworld
		= (ServiceRep ([(toString taskId,idx,encv)],[],[]),iworld)
	
	editorRep idx taskId (RepAsTUI _ _) f v encv maskv vermask mbEvent iworld
		# (editor,iworld) = visualizeAsEditor v taskId idx vermask mbEvent iworld
		= (TUIRep (ViewPart,editor,[],[]),iworld)
	editorRep idx taskId _ f v encv maskv vermask mbEvent iworld
		= (ServiceRep ([(toString taskId,idx,encv)],[],[]),iworld)
	
	matchEditEvent idx Nothing = False
	matchEditEvent idx (Just (dp,_))
		= case reverse (dataPathList dp) of
			[idx:_]	= True
			_		= False 		
			
	applyEditEvent idx (Just (dp,editv)) viewf l r v encv maskv vermask dirty iworld
		//Remove part index from datapath
		# dp 	= dataPathFromList ('StdList'.init (dataPathList dp))
		//Update full value
		| dataPathLevel dp == 0
			= case fromJSON editv of
				Just nv
					# maskv = defaultMask nv
					# vermask = verifyForm nv maskv
					= (l,nv,editv,maskv,vermask,True,iworld)	//QUESTION: Should we also do a react here?
				Nothing
					= (l,v,encv,maskv,vermask,dirty,iworld)
		//Update partial value
		# (v,maskv,iworld)	= updateValueAndMask dp editv v maskv iworld
		# encv				= toJSON v
		# vermask			= verifyForm v maskv
		= reactToEditEvent viewf l r v encv maskv vermask dirty iworld
	
	reactToEditEvent viewf l r v encv maskv vermask dirty iworld
		= case viewf l r (if (isValidValue vermask) (Just v) Nothing) of
			(l,Nothing)
				= (l,v,encv,maskv,vermask,dirty,iworld)
			(l,Just form)
				# (v,encv,maskv)	= initFormView form
				# vermask 			= verifyForm v maskv
				= (l,v,encv,maskv,vermask,False,iworld)
		
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
	= mkTask init eval
where
	init taskId iworld
		= (TCEmpty taskId, iworld)

	eval eEvent cEvent repAs (TCEmpty taskId) iworld=:{evalStack}
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
				= (TaskUnstable Nothing (TUIRep (SingleTask, Just (stringDisplay "Task finished"),[],[])) (TCEmpty taskId), {iworld & readShares = Nothing})
			| otherwise
				= (taskException WorkOnNotFound ,iworld)
		//Eval instance
		# target					= if (taskNo == 0) Nothing (Just (TaskId topNo taskNo))
		# genGUI					= case repAs of (RepAsTUI _ _) = True ; _ = False
		# (mbResult,context,iworld)	= evalInstance eEvent cEvent target genGUI (fromOk mbContext) iworld
		= case mbResult of
			Error e				= (taskException WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (result,rep,iworld) = case result of
					(TaskUnstable _ rep _)			= (WOActive, rep, iworld)
					(TaskStable _ rep _)			= (WOFinished, rep, iworld)
					(TaskException _ err)			= (WOExcepted, TUIRep (SingleTask, Just (stringDisplay ("Task excepted: " +++ err)), [], []), iworld)
				= case result of
					WOFinished	= (TaskStable WOFinished rep (TCEmpty taskId), iworld)
					_			= (TaskUnstable (Just result) rep (TCEmpty taskId), iworld)

	//If a top instance has just been added, but has not been evaluated before it is still in the
	//queue of ParallelControls. If so, we don't throw an exception but return an unstable value
	//as we are still waiting for the instance to be evaluated
	checkIfAddedGlobally topNo iworld=:{parallelControls}
		= case 'Map'.get ("taskList:" +++ toString TopLevelTaskList) parallelControls of
			Just (_,controls)
				= (isMember topNo [i \\ AppendTask {ParallelItem|taskId=TaskId i 0} <- controls], iworld)
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
