implementation module iTasks.API.Core.CoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc
import System.Time, Data.Error, System.OSError, Data.Tuple, Text.JSON
import qualified StdList
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.Framework.Generic, iTasks.Framework.Generic.Interaction, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.UIDefinition, iTasks.Framework.Shared
import iTasks.API.Core.LayoutCombinators

from Data.SharedDataSource			import qualified read, readRegister, write, writeFilterMsg
from StdFunc					import o, id
from iTasks.Framework.IWorld	import :: IWorld(..)
from iTasks.API.Core.SystemData	import topLevelTasks
from Data.Map						import qualified get
from Data.Map						import newMap, put


return :: !a -> (Task a) | iTask a
return a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		# (val,iworld) = 'Data.SharedDataSource'.read shared iworld
		= case val of
			Ok val		= (Ok val,iworld)
			Error e		= (Error (dynamic (SharedException e), e), iworld)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime,currentInstance}
		//# (res,iworld)	='Data.SharedDataSource'.writeFilterMsg val ((<>) currentInstance) shared iworld
		# (res,iworld)	='Data.SharedDataSource'.write val shared iworld
		= case res of
			Ok _	= (Ok val,iworld)
			Error e	= (Error (dynamic (SharedException e), e), iworld)

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd fun shared = mkInstantTask eval
where
	eval taskId iworld=:{taskTime,currentInstance}
		# (er, iworld)	= 'Data.SharedDataSource'.read shared iworld
		= case er of
			Error e		= (Error (dynamic (SharedException e), e), iworld)
			Ok r	
				# w				= fun r
				//# (er, iworld)	=  'Data.SharedDataSource'.writeFilterMsg w ((<>) currentInstance) shared iworld
				# (er, iworld)	=  'Data.SharedDataSource'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error (dynamic (SharedException e), e), iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val False) {TaskInfo|lastEvent=ts,refreshSensitive=True}
				(finalizeRep repOpts NoRep) (TCInit taskId ts)
			Error e		= exception (SharedException e)
		= (res,iworld)
	eval event repAs (TCDestroy _) iworld = (DestroyedResult,iworld)


interact :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask)))
			-> Task l | descr d & iTask l & iTask r & iTask v
interact desc shared initFun refreshFun = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event repOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event repOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if (rChanged || vChanged) (refreshFun l nr (nv,nmask) rChanged vChanged vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep)
			(TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	matchAndApplyEvent (EditEvent eventNo taskId name value) matchId taskTime v mask ts iworld
		| taskId == matchId
			| otherwise
				# ((nv,nmask),iworld)	= updateValueAndMask taskId (s2dp name) value (v,mask) iworld
				= (nv,nmask,taskTime,iworld)
		| otherwise	= (v,mask,ts,iworld)
	matchAndApplyEvent (FocusEvent eventNo taskId) matchId taskTime v mask ts iworld
		= (v,mask, if (taskId == matchId) taskTime ts, iworld)
	matchAndApplyEvent _ matchId taskTime v mask ts iworld
		= (v,mask,ts,iworld)

	visualizeView taskId repOpts value=:(v,vmask,vver) desc valueAttr iworld
		# layout	= repLayoutRules repOpts
		# (controls,iworld) = visualizeAsEditor value taskId layout iworld
		# uidef		= UIControlStack (layout.LayoutRules.accuInteract (toPrompt desc) {UIControlStack|attributes=put VALUE_ATTRIBUTE valueAttr newMap,controls=controls})
		= (TaskRep uidef [(toString taskId,toJSON v)], iworld)

tcpconnect :: !String !Int !(ReadOnlyShared r) (r -> (l,[TCPSend])) (l r [TCPReceive] Bool -> (l,[TCPSend])) -> Task l | iTask l & iTask r
tcpconnect host port shared initFun commFun = Task eval
where
	eval event repOpts tree=:(TCInit taskId ts) iworld
        = (ValueResult NoValue {TaskInfo|lastEvent=ts,refreshSensitive=True} NoRep tree,iworld)

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

shutDown :: Task Void 
shutDown = mkInstantTask (\taskId iworld -> (Ok Void, {IWorld|iworld & shutdown = True}))
