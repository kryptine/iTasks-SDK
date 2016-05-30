implementation module iTasks.API.Core.Optimized.Tasks

import StdList, StdBool, StdInt, StdTuple,StdMisc
import System.Time, Data.Error, System.OSError, Data.Tuple, Data.List, Text.JSON
import qualified StdList
import iTasks._Framework.Generic, iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskEval
import iTasks._Framework.TaskStore, iTasks.UI.Definition
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil
import iTasks._Framework.IWorld
import iTasks.UI.Layout, iTasks.UI.Editor
from iTasks._Framework.SDS as SDS import qualified read, readRegister, write
from StdFunc						import o, id
from iTasks.API.Core.SDSs		    import topLevelTasks
import qualified Data.Map as DM
from iTasks.API.Core.Tasks import matchAndApplyEvent_ , visualizeView_ 

interactExposed :: !p !EditMode !(ReadOnlyShared r) (r -> (l,(v,EditMask))) (l r (v,EditMask) Bool Bool Bool -> (l,(v,EditMask)))
						(Maybe (Editor v))
                        -> Task (l,v) | toPrompt p & iTask l & iTask r & iTask v
interactExposed prompt mode shared initFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event evalOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld

	eval event evalOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if (rChanged || vChanged) (refreshFun l nr (nv,nm) rChanged vChanged vValid) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld) 
				# value 				= if valid (Value (nl,nv) False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactLocalExposed :: !p !EditMode (l,(v,EditMask)) (l (v,EditMask) Bool -> (l,(v,EditMask))) (Maybe (Editor v))
                        -> Task (l,v) | toPrompt p & iTask l & iTask v
interactLocalExposed prompt mode initVal refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (l,(v,mask))	= initVal
		= eval event evalOpts (TCInteractLocal taskId ts (toJSON l) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractLocal taskId=:(TaskId instanceNo _) ts encl encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,v)					= (fromJust (fromJSON encl), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if vChanged (refreshFun l (nv,nm) vValid) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value (nl,nv) False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteractLocal taskId nts (toJSON nl) (toJSON nv) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactViewOnly :: !p !EditMode (ReadOnlyShared r) (r -> (v,EditMask)) (r (v,EditMask) Bool Bool Bool -> (v,EditMask)) (Maybe (Editor v))
                        -> Task v | toPrompt p & iTask r & iTask v
interactViewOnly prompt mode shared initFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (v,mask)	= initFun r
				= eval event evalOpts (TCInteractViewOnly taskId ts (toJSON r) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractViewOnly taskId=:(TaskId instanceNo _) ts encr encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# (r,v)					= (fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nv,nm) 				= if (rChanged || vChanged) (refreshFun nr (nv,nm) rChanged vChanged vValid) (nv,nm)
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nv False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteractViewOnly taskId nts (toJSON nr) (toJSON nv) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactLocalViewOnly :: !p !EditMode !(v,EditMask) ((v,EditMask) Bool -> (v,EditMask)) (Maybe (Editor v))
                        -> Task v | toPrompt p & iTask v
interactLocalViewOnly prompt mode initVal refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (v,mask)	= initVal
		= eval event evalOpts (TCInteractLocalViewOnly taskId ts (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractLocalViewOnly taskId=:(TaskId instanceNo _) ts encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# v						= fromJust (fromJSON encv)
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nv,nm) 				= if vChanged (refreshFun (nv,nm) vValid) (nv,nm)
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
		 	(Ok change,valid,iworld) 
				# value 				= if valid (Value nv False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteractLocalViewOnly taskId nts (toJSON nv) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactSharedInformation :: !p !EditMode !(ReadOnlyShared r) (r -> v) (Maybe (Editor v)) -> Task r | toPrompt p & iTask r & iTask v
interactSharedInformation prompt mode shared toView mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e,iworld)
			Ok r
				# v = toView r
				# (l,v,mask) = (r,v,newFieldMask)
				= eval event evalOpts (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld

	eval event evalOpts (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr m) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts mode mbEditor taskTime (v,m) ts prompt iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if rChanged (nr,(toView nr,nm)) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts mode mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nl False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactNullEnter :: !p !v (v->l) (Maybe (Editor v)) -> Task l | toPrompt p & iTask v & iTask l
interactNullEnter prompt initFun fromf mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = initFun
		= eval event evalOpts (TCInteract1 taskId ts (toJSON v) newFieldMask) iworld

	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encv m) iworld=:{current={taskTime}}
		//Decode stored values
		# v = fromJust (fromJSON encv)
		  l = fromf v
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts Enter mbEditor taskTime (v,m) ts prompt iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if vChanged (refreshFun l (nv,nm) vValid) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts Enter mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nl False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract1 taskId nts (toJSON nv) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refreshFun l (v,m) valid = if valid (fromf v,(v,m)) (l,(v,m))

interactNullUpdate :: !p !(l -> v) (l v -> l) (Maybe (Editor v)) l -> Task l | toPrompt p & iTask l & iTask v
interactNullUpdate prompt tof fromf mbEditor m = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = tof m
		  l = m
		  mask = newFieldMask 
		= eval event evalOpts (TCInteract1 taskId ts (toJSON l) mask) iworld

	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl m) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts Update mbEditor taskTime (v,m) ts prompt iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= not (containsInvalidFields nm)
		# (nl,(nv,nm)) 			= if vChanged (refreshFun l (nv,nm) vValid) (l,(nv,nm))
		//Update visualization v
		= case visualizeView_ taskId evalOpts Update mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nl False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract1 taskId nts (toJSON nl) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refreshFun l (v,m) ok
		| ok 
			# nl = fromf l v
			# nv = tof nl
			= (l,(nv,newFieldMask))	
		= (l,(v,m))

interactNullView :: !p (l->v) (Maybe (Editor v)) l -> Task l | toPrompt p & iTask l & iTask v
interactNullView prompt tof mbEditor m = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# l = m
		  v = Display (tof l)
		  mask = newFieldMask
		= eval event evalOpts (TCInteract1 taskId ts (toJSON l) mask) iworld

	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl m) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable	
		# ((nv,nm),nts,iworld)  = matchAndApplyEvent_ event taskId evalOpts View mbEditor taskTime (v,m) ts prompt iworld
		# nl = l
		//Update visualization v
		= case visualizeView_ taskId evalOpts View mbEditor event (v,m) (nv,nm) prompt iworld of
			(Ok change,valid,iworld)
				# value 				= if valid (Value nl False) NoValue
				# info 					= {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True}
				= (ValueResult value info change (TCInteract1 taskId nts (toJSON nl) nm), iworld)
			(Error e,_,iworld) = (ExceptionResult (exception e),iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)
