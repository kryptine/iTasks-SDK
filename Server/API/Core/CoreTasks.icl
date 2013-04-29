implementation module CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Util, HtmlUtil, Time, Error, OSError, Map, Tuple, List
import qualified StdList
import iTaskClass, Task, TaskState, TaskEval, TaskStore, UIDefinition, LayoutCombinators, Shared
from SharedDataSource		import qualified read, readRegister, write, writeFilterMsg
from StdFunc				import o, id
from IWorld					import :: IWorld(..)
from SystemData				import topLevelTasks
from Map					import qualified get

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (val,iworld) = 'SharedDataSource'.read shared iworld
		= case val of
			Ok val		= (Ok val,iworld)
			Error e		= (Error (dynamic (SharedException e), e), iworld)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime,currentInstance}
		//# (res,iworld)	='SharedDataSource'.writeFilterMsg val ((<>) currentInstance) shared iworld
		# (res,iworld)	='SharedDataSource'.write val shared iworld
		= case res of
			Ok _	= (Ok val,iworld)
			Error e	= (Error (dynamic (SharedException e), e), iworld)

update :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
update fun shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime,currentInstance}
		# (er, iworld)	= 'SharedDataSource'.read shared iworld
		= case er of
			Error e		= (Error (dynamic (SharedException e), e), iworld)
			Ok r	
				# w				= fun r
				//# (er, iworld)	=  'SharedDataSource'.writeFilterMsg w ((<>) currentInstance) shared iworld
				# (er, iworld)	=  'SharedDataSource'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error (dynamic (SharedException e), e), iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SharedDataSource'.readRegister instanceNo shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val False) {TaskInfo|lastEvent=ts,refreshSensitive=True}
				(finalizeRep repOpts (TaskRep (UIControlSequence {UIControlSequence|attributes=newMap,controls=[],direction=Vertical}) [])) (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)


interact :: !d !(ReadOnlyShared r) (r -> (l,v,InteractionMask)) (l r v InteractionMask Bool -> (l,v,InteractionMask))
			-> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e, iworld)
			Ok r
				# (l,v,mask)	= initFun r
				= eval event repOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event repOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed (refreshFun l nr nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep)
			(TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	matchAndApplyEvent (EditEvent taskId name value) matchId taskTime v mask ts iworld
		| taskId == matchId
			| otherwise
				# (nv,nmask)	= updateValueAndMask (s2dp name) value (v,mask)
				= (nv,nmask,taskTime,iworld)
		| otherwise	= (v,mask,ts,iworld)
	matchAndApplyEvent (FocusEvent taskId) matchId taskTime v mask ts iworld
		= (v,mask, if (taskId == matchId) taskTime ts, iworld)
	matchAndApplyEvent _ matchId taskTime v mask ts iworld
		= (v,mask,ts,iworld)

	visualizeView taskId repOpts v validity desc valueAttr iworld
		# layout	= repLayout repOpts
		# (controls,iworld) = visualizeAsEditor v validity taskId layout iworld
		# uidef		= (afterLayout repOpts) (UIControlSequence (layout.Layout.interact (toPrompt desc) {UIControlSequence|attributes=put VALUE_ATTRIBUTE valueAttr newMap,controls=controls,direction=Vertical}))
		= (TaskRep uidef [(toString taskId,toJSON v)], iworld)

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (Ok Void, {IWorld|iworld & world = fun world})
		
accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= (Ok res, {IWorld|iworld & world = world})
	
accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|taskTime,world}
		# (res,world)	= fun world
		= case res of
			Error e
				# err = errf e		
				= (Error (dynamic err,toString err), {IWorld|iworld & world = world})	
			Ok v
				= (Ok v, {IWorld|iworld & world = world})
	
accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
