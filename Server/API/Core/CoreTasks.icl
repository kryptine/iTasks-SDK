implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List_NG
import qualified StdList
import iTaskClass, Task, TaskState, TaskEval, TaskStore, TUIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, readRegister, write
from StdFunc				import o, id
from IWorld					import :: IWorld(..)
from iTasks					import dynamicJSONEncode, dynamicJSONDecode
from SystemData				import topLevelTasks
from Map					import qualified get

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

NoRep :== TaskRep (SingleTask,Nothing,[],[]) []

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld=:{taskTime} -> (ValueResult (Value a Stable) taskTime NoRep TCNop, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (ExceptionResult (dynamic e) (toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (val,iworld) = 'SharedDataSource'.read shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val Stable) taskTime NoRep TCNop
			Error e		= exception (SharedException e)
		= (res, iworld)

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (res,iworld)	='SharedDataSource'.write val shared iworld
		# res = case res of
			Ok _	= ValueResult (Value val Stable) taskTime NoRep TCNop
			Error e	= exception (SharedException e)
		= (res, iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (er, iworld)	= 'SharedDataSource'.read shared iworld
		| isError er	= (exception (SharedException (fromError er)), iworld)
		# w				= fun (fromOk er)
		# (er, iworld)	= 'SharedDataSource'.write w shared iworld
		= (ValueResult (Value w Stable) taskTime NoRep TCNop, iworld)

watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SharedDataSource'.readRegister instanceNo shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val Unstable) ts NoRep (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)

interact :: !d !(ReadOnlyShared r) (r -> (l,v,UpdateMask)) (l r v UpdateMask Bool -> (l,v,UpdateMask)) -> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error _		= (exception "Could not read shared in interact", iworld)
			Ok r
				# (l,v,mask)	= initFun r
				= eval eEvent cEvent refresh repAs (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval eEvent cEvent refresh repAs (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(applyEvent taskId taskTime v mask ts (matchEvent taskId eEvent) iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception "Could not read shared in interact", iworld)
		# nr					= (fromOk mbr)
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refreshFun l nr nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)
	
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)
	
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
				
	visualizeView taskId repAs v validity iworld
		# (editor,iworld) = visualizeAsEditor v validity taskId iworld
		= (TaskRep ((repLayout repAs) SingleTask [(ViewPart, editor, [],[])] [] (initAttributes desc)) [(toString taskId,toJSON v)], iworld)
	
appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		= (ValueResult (Value Void Stable) taskTime NoRep TCNop, {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		# (res,world) = fun world
		= (ValueResult (Value res Stable) taskTime NoRep TCNop, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		# (res,world)	= fun world
		= case res of
			Error e		= (exception (errf e), {IWorld|iworld & world = world})
			Ok v		= (ValueResult (Value v Stable) taskTime NoRep TCNop, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
