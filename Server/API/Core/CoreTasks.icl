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

derive class iTask WorkOnStatus

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

NoRep :== TaskRep (SingleTask,Nothing,[],[]) []

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
watch shared = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		# (val,iworld)	= 'SharedDataSource'.read shared iworld
		# res = case val of
			Ok (val,_)	= ValueResult (Value val Unstable) ts NoRep (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)

interact :: !d !(ReadOnlyShared r) (r -> (l,v,UpdateMask)) (l r v UpdateMask Bool -> (l,v,UpdateMask)) -> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.read shared iworld
		= case mbr of
			Error _		= (exception "Could not read shared in interact", iworld)
			Ok (r,_)
				# (l,v,mask)	= initFun r
				= eval eEvent cEvent refresh repAs (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval eEvent cEvent refresh repAs (TCInteract taskId ts encl encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# event					= matchEvent taskId eEvent
		# (nv,nmask,nts,iworld) = if refresh (v,mask,ts,iworld) (applyEvent taskId taskTime v mask ts event iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.read shared iworld
		| isError mbr			= (exception "Could not read shared in interact", iworld)
		# (nr,_)				= (fromOk mbr)
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refreshFun l nr nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity event iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)
	
	matchEvent taskId1 (Just (LuckyEvent e))								= Just e	
	matchEvent taskId1 (Just (TaskEvent taskId2 e))	| taskId1 == taskId2	= Just e
	matchEvent taskId1 _													= Nothing
	
	isLucky (Just (LuckyEvent _))	= True	//HACK
	isLucky _						= False
	
	applyEvent taskId taskTime v mask ts event iworld = case event of
		Nothing	 = (v,mask,ts,iworld)
		Just (dps,encev)
			# dp = s2dp dps
			| dataPathLevel dp == 0
				= case fromJSON encev of
					Nothing	= (v,mask,ts,iworld)
					Just nv	= (nv,defaultMask nv,taskTime,iworld)
			| otherwise
				# (nv,nmask,iworld)	= updateValueAndMask dp encev v mask iworld
				= (nv,nmask,taskTime,iworld)
				
	visualizeView taskId repAs=:(TaskRepTarget target _ _) v validity event iworld
		| isNothing target || target == Just taskId
			# (editor,iworld) = visualizeAsEditor v validity taskId event iworld
			= (TaskRep ((repLayout repAs) SingleTask [(ViewPart, editor, [],[])] [] (initAttributes desc)) [(toString taskId,toJSON v)], iworld)
		| otherwise
			= (NoRep,iworld)
	visualizeView taskId _ v validity event iworld
		= (NoRep,iworld)

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId topNo taskNo) = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId ts) iworld=:{evalStack}
		//Check for cycles
		| isMember taskId evalStack
			=(exception WorkOnDependencyCycle, iworld)
		//Load instance
		# (mbInstance,iworld)		= loadTaskInstance topNo iworld
		| isError mbInstance	
			//If the instance can not be found, check if it was only just added by an
			//appendTask in the same session. If so, create a temporary result and trigger
			//reevaluation.
			# (found,iworld)	= checkIfAddedGlobally topNo iworld
			| found
				= (ValueResult NoValue ts (TaskRep (SingleTask, Just (stringDisplay "Task not yet initialized, please refresh."),[],[]) []) (TCInit taskId ts), {iworld & readShares = Nothing})
			| otherwise
				= (ValueResult (Value WODeleted Stable) ts (TaskRep (SingleTask, Just (stringDisplay "This task has been deleted."),[],[]) []) (TCInit taskId ts), iworld)
		//Eval instance
		# target					= if (taskNo == 0) Nothing (Just (TaskId topNo taskNo))
		# (mbResult,context,iworld)	= evalInstance eEvent cEvent refresh target (fromOk mbInstance) iworld
		= case mbResult of
			Error e				= (exception WorkOnEvalError, iworld)
			Ok result
				//Store context
				# iworld		= storeTaskInstance context iworld
				# (result,rep,iworld) = case result of
					(ValueResult (Value _ Stable) _ rep _)	= (WOFinished, rep, iworld)
					(ValueResult _ _ rep _)					= (WOActive, rep, iworld)
					(ExceptionResult _ err)					= (WOExcepted, TaskRep (SingleTask, Just (stringDisplay ("Task excepted: " +++ err)), [], []) [], iworld)
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
