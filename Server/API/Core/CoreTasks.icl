implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import qualified StdList
import iTaskClass, Task, TaskState, TaskEval, TaskStore, TUIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, write, readWrite, :: RWRes(..)
from StdFunc				import o, id
from IWorld					import :: IWorld(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from SystemData				import topLevelTasks
from Map					import qualified get

derive class iTask WorkOnProcessState

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld=:{taskTime} -> (ValueResult (Value a Stable) taskTime NoRep (TCEmpty taskId taskTime), iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (ExceptionResult (dynamic e) (toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (val,iworld) = 'SharedDataSource'.read shared iworld
		# res = case val of
			Ok (val,_)	= ValueResult (Value val Stable) taskTime NoRep (TCEmpty taskId taskTime)
			Error e		= exception (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (res,iworld)	='SharedDataSource'.write val shared iworld
		# res = case res of
			Ok _	= ValueResult (Value val Stable) taskTime NoRep (TCEmpty taskId taskTime)
			Error e	= exception (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (val,iworld)	= 'SharedDataSource'.readWrite (\r _ -> let w = fun r in 'SharedDataSource'.Write w w) shared iworld
		| isError val	= (exception (SharedException (fromError val)), iworld)
		= (ValueResult (Value (fromOk val) Stable) taskTime NoRep (TCEmpty taskId taskTime), iworld)

watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = mkTask eval
where
	eval eEvent cEvent repAs (TCInit taskId ts) iworld
		# (val,iworld)	= 'SharedDataSource'.read shared iworld
		# res = case val of
			Ok (val,_)	= ValueResult (Value val Unstable) ts NoRep (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)

interact :: !d ![InteractionPart l r] !l !(ReadOnlyShared r) -> Task (l,r) | descr d & iTask l & iTask r
interact desc parts initLocal shared = mkTask eval
where
	eval eEvent cEvent repAs state=:(TCInit taskId ts) iworld		//Create the initial views
		# (mbrvalue,iworld) 			= 'SharedDataSource'.read shared iworld
		| isError mbrvalue				= (exception "Could not read shared in interact", iworld)
		# (rvalue,_)					= fromOk mbrvalue
		= eval eEvent cEvent repAs (TCInteract taskId ts (toJSON initLocal) (toJSON rvalue) (initParts initLocal rvalue parts)) iworld
	where
		initParts l r parts = map (initPart l r) parts
		
		//initPart l r (DisplayPart f)	= (toJSON (f l r),Untouched, False)
		initPart l r (FormPart f _ _)
			# (_,encv,maskv)	= initFormView (f l r)
			= (encv,maskv,False)
		
	eval eEvent cEvent repAs state=:(TCInteract taskId ts encl encr views) iworld=:{IWorld|taskTime}
		# (mbrvalue,iworld) 				= 'SharedDataSource'.read shared iworld
		| isError mbrvalue					= (sharedException mbrvalue, iworld)
		# (rvalue,_)						= (fromOk mbrvalue)
		# changed							= rvalue =!= fromJust (fromJSON encr)
		# lvalue							= fromJust (fromJSON encl)

		# (is_lucky_event,lvalue,mbEdit,lastEvent)
		  = case eEvent of
			Just (TaskEvent t e)
				| t == taskId
					= (False,lvalue,Just e,taskTime)
			Just (LuckyEvent e=:(_,json_node))
				= case fromJSON json_node of
					Just lucky_value
						-> (True,lucky_value,Just e,taskTime)
					_
						-> (True,lvalue,Just e,taskTime)						
			_
				= (False,lvalue,Nothing,ts)

/*
		# (mbEdit,lastEvent)	= case eEvent of
			Just (TaskEvent t e)
				| t == taskId		= (Just e,taskTime)
			Just (LuckyEvent e)		= (Just e,taskTime)						
			_						= (Nothing,ts)

*/
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

		| is_lucky_event && valid
			# value	= Value (lvalue,rvalue) Stable
			= (ValueResult value lastEvent rep (TCInteract taskId  lastEvent (toJSON lvalue) (toJSON rvalue) views), iworld)
		
		# value	= if valid (Value (lvalue,rvalue) Unstable) NoValue 
		= (ValueResult value lastEvent rep (TCInteract taskId  lastEvent (toJSON lvalue) (toJSON rvalue) views), iworld)
	eval eEvent cEvent repAs state iworld
		= (exception "Corrupt context in interact",iworld)

	evalParts idx taskId repAs mbEvent changed l r [] [] iworld
		= (l,[],[],True,iworld)
	evalParts idx taskId repAs mbEvent changed l r [p:ps] [v:vs] iworld	
		# (nl,rep,view,pvalid,iworld)	= evalPart idx taskId repAs mbEvent changed l r p v iworld
		# (nnl,reps,views,valid,iworld)	= evalParts (idx + 1) taskId repAs mbEvent changed nl r ps vs iworld
		= (nnl,[rep:reps],[view:views],pvalid && valid,iworld) //All parts have to be valid

	evalPart idx taskId repAs mbEvent changed l r part=:(FormPart initf sharef viewf) view=:(encv,maskv,dirty) iworld
		//Update the local value and possibly the view if the share has changed
		# v							= fromJust (fromJSON encv)
		# vermask					= verifyForm v maskv
		//If the edit event is for this part, update the view
		# (l,v,encv,maskv,vermask,dirty,iworld)
			= if (matchEditEvent idx mbEvent) 
					(applyEditEvent /*idx*/ mbEvent viewf l r v encv maskv vermask dirty iworld)
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

	applyEditEvent /*idx*/ (Just (dp,editv)) viewf l r v encv maskv vermask dirty iworld	
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
	
	initFormView BlankForm		= (v, toJSON v, Untouched) where v = defaultValue
	initFormView (FilledForm v)	= (v, toJSON v, defaultMask v)
	
sharedException :: !(MaybeErrorString a) -> (TaskResult b)
sharedException err = exception (SharedException (fromError err))

workOn :: !TaskId -> Task WorkOnProcessState
workOn (TaskId topNo taskNo) = mkTask eval
where
	eval eEvent cEvent repAs (TCInit taskId ts) iworld=:{evalStack}
		//Check for cycles
		| isMember taskId evalStack
			=(exception WorkOnDependencyCycle, iworld)
		//Load instance
		# (mbContext,iworld)		= loadTaskInstance (Right topNo) iworld
		| isError mbContext	
			//If the instance can not be found, check if it was only just added by an
			//appendTask in the same session. If so, create a temporary result and trigger
			//reevaluation.
			# (found,iworld)	= checkIfAddedGlobally topNo iworld
			| found
				= (ValueResult NoValue ts (TUIRep (SingleTask, Just (stringDisplay "Task finished"),[],[])) (TCInit taskId ts), {iworld & readShares = Nothing})
			| otherwise
				= (exception WorkOnNotFound ,iworld)
		//Eval instance
		# target					= if (taskNo == 0) Nothing (Just (TaskId topNo taskNo))
		# genGUI					= case repAs of (RepAsTUI _ _) = True ; _ = False
		# (mbResult,context,iworld)	= evalInstance eEvent cEvent target genGUI (fromOk mbContext) iworld
		= case mbResult of
			Error e				= (exception WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (result,rep,iworld) = case result of
					(ValueResult (Value _ Stable) _ rep _)	= (WOFinished, rep, iworld)
					(ValueResult _ _ rep _)					= (WOActive, rep, iworld)
					(ExceptionResult _ err)					= (WOExcepted, TUIRep (SingleTask, Just (stringDisplay ("Task excepted: " +++ err)), [], []), iworld)
				= case result of
					WOFinished	= (ValueResult (Value result Stable) ts rep (TCInit taskId ts), iworld)
					_			= (ValueResult (Value result Unstable) ts rep (TCInit taskId ts), iworld)

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
	eval taskId iworld=:{IWorld|taskTime,world}
		= (ValueResult (Value Void Stable) taskTime NoRep (TCEmpty taskId taskTime), {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		# (res,world) = fun world
		= (ValueResult (Value res Stable) taskTime NoRep (TCEmpty taskId taskTime), {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		# (res,world)	= fun world
		= case res of
			Error e		= (exception (errf e), {IWorld|iworld & world = world})
			Ok v		= (ValueResult (Value v Stable) taskTime NoRep (TCEmpty taskId taskTime), {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
