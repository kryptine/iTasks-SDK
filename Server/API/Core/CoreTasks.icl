implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List_NG
import qualified StdList
import iTaskClass, Task, TaskState, TaskEval, TaskStore, UIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, readRegister, write
from StdFunc				import o, id
from IWorld					import :: IWorld(..)
from SystemData				import topLevelTasks
from Map					import qualified get

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

NoRep :== TaskRep {UIDef|controls=[],actions=[],attributes=newMap} []

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
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

interactSharedChoice :: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t v l)
						-> Task (Maybe l) | descr d & Choice t & iTask r & iTask l & iTask (t v l)
interactSharedChoice desc shared initial_mask toView = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error _		= could_not_read_shared_in_interact_exception iworld
			Ok r
				# v = toView r initial_mask
				# (l,v,mask) = (initial_mask,v,defaultMask v)
				= eval eEvent cEvent refresh repAs (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld)
			= if refresh
				(v,mask,ts,iworld)
				(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= could_not_read_shared_in_interact_exception iworld
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv valid)
										(l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv valid
		# nl = if valid (getMbSelection nv) l
		# v = toView nr nl
		= (nl,v,defaultMask v)

interactSharedChoiceNoView :: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t l)
							  -> Task (Maybe l) | descr d & ChoiceNoView t & iTask r & iTask l & iTask (t l)
interactSharedChoiceNoView desc shared initial_mask toViewId = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error _		= could_not_read_shared_in_interact_exception iworld
			Ok r
				# v = toViewId r initial_mask
				# (l,v,mask) = (initial_mask,v,defaultMask v)
				= eval eEvent cEvent refresh repAs (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust( fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toViewId r l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld)
			= if refresh
				(v,mask,ts,iworld)
				(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= could_not_read_shared_in_interact_exception iworld
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv valid)
										(l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv valid
		# nl = if valid (getMbSelectionNoView nv) l
		# v = toViewId nr nl
		= (nl,v,defaultMask v)

interactSharedInformation :: !d !(ReadOnlyShared r) (r -> v) -> Task r | descr d & iTask r & iTask v
interactSharedInformation desc shared toView = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error _		= could_not_read_shared_in_interact_exception iworld
			Ok r
				# v = toView r
				# (l,v,mask) = (r,v,defaultMask v)
				= eval eEvent cEvent refresh repAs (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= could_not_read_shared_in_interact_exception iworld
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun nr) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun r
		# v = toView r
		= (r,v,defaultMask v) 

interactNullEnter :: !d !v (v->l) -> Task l | descr d & iTask v
interactNullEnter desc initFun fromf = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = initFun
		# mask = Untouched
		= eval eEvent cEvent refresh repAs (TCInteract1 taskId ts (toJSON v) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract1 taskId=:(TaskId instanceNo _) ts encv mask) iworld=:{taskTime}
		//Decode stored value
		# v = fromJust (fromJSON encv)
		  l = fromf v
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract1 taskId nts (toJSON nv) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok
			= (fromf v,v,m)
			= (l,v,m)

interactNullUpdate :: !d !(l -> v) (l v -> l) l -> Task l | descr d & iTask l & iTask v
interactNullUpdate desc tof fromf m = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = tof m
		  l = m
		  mask = defaultMask v
		= eval eEvent cEvent refresh repAs (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok 
			# nl = fromf l v
			  nv = tof nl
			= (nl,nv,defaultMask nv)
			= (l,v,m)

interactNullView :: !d (l->v) l -> Task l | descr d & iTask l & iTask v
interactNullView desc tof m = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# l = m
		  v = Display (tof l)
		  mask = defaultMask v
		= eval eEvent cEvent refresh repAs (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval eEvent cEvent refresh repAs (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = Display (tof l)
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		# nl = l
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

interact :: !d !(ReadOnlyShared r) (r -> (l,v,UpdateMask)) (l r v UpdateMask Bool -> (l,v,UpdateMask))
			-> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval eEvent cEvent refresh repAs (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error _		= could_not_read_shared_in_interact_exception iworld
			Ok r
				# (l,v,mask)	= initFun r
				= eval eEvent cEvent refresh repAs (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval eEvent cEvent refresh repAs (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = if refresh
			(v,mask,ts,iworld)
			(matchAndApplyEvent eEvent taskId taskTime v mask ts iworld)
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= could_not_read_shared_in_interact_exception iworld
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidValue (verifyForm nv nmask)
		# (nl,nv,nmask) 		= if changed (refreshFun l nr nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyForm nv nmask
		# (rep,iworld) 			= visualizeView taskId repAs nv validity desc iworld
		# value 				= if (isValidValue validity) (Value nl (if (isLucky eEvent) Stable Unstable)) NoValue
		= (ValueResult value nts rep (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval eEvent cEvent refresh repAs (TCDestroy _) iworld = (DestroyedResult,iworld)

isLucky (Just (LuckyEvent _ _))	= True	//HACK
isLucky _						= False

matchAndApplyEvent eEvent taskId taskTime v mask ts iworld
	= applyEvent taskId taskTime v mask ts (matchEvent taskId eEvent) iworld
where
	matchEvent taskId1 (Just (LuckyEvent _ e))								= Just e	
	matchEvent taskId1 (Just (TaskEvent taskId2 e))	| taskId1 == taskId2	= Just e
	matchEvent taskId1 _													= Nothing

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

visualizeView taskId repAs v validity desc iworld
	# (controls,iworld) = visualizeAsEditor v validity taskId iworld
	= (TaskRep ((repLayout repAs) (InteractLayout {UIDef|controls=[],actions=[],attributes=initAttributes desc} {UIDef|controls=controls,attributes=newMap,actions=[]})) [(toString taskId,toJSON v)], iworld)

could_not_read_shared_in_interact_exception iworld
	= (exception "Could not read shared in interact", iworld)

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
