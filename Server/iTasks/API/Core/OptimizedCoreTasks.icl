implementation module iTasks.API.Core.OptimizedCoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc
import System.Time, Data.Error, System.OSError, Data.Tuple, Data.List, Text.JSON
import qualified StdList
import iTasks._Framework.Generic, iTasks._Framework.Generic.Interaction, iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskEval
import iTasks._Framework.TaskStore, iTasks.UI.Definition
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil
import iTasks._Framework.IWorld
import iTasks.UI.Layout, iTasks.UI.Editor
from iTasks._Framework.SDS as SDS import qualified read, readRegister, write
from StdFunc						import o, id
from iTasks.API.Core.SDSs		    import topLevelTasks
import qualified Data.Map as DM

interactExposed :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask)))
						(Maybe (Editor v))
                        -> Task (l,v) | descr d & iTask l & iTask r & iTask v
interactExposed desc shared initFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (l,(v,mask))	= initFun r
				= eval event evalOpts (TCInteract taskId ts (toJSON l) (toJSON r) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteract taskId=:(TaskId instanceNo _) ts encl encr encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,r,v)				= (fromJust (fromJSON encl), fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if (rChanged || vChanged) (refreshFun l nr (nv,nmask) rChanged vChanged vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value (nl,nv) False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True} rep
			(TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactLocalExposed :: !d (l,(v,InteractionMask)) (l (v,InteractionMask) Bool -> (l,(v,InteractionMask))) (Maybe (Editor v))
                        -> Task (l,v) | descr d & iTask l & iTask v
interactLocalExposed desc initVal refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (l,(v,mask))	= initVal
		= eval event evalOpts (TCInteractLocal taskId ts (toJSON l) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractLocal taskId=:(TaskId instanceNo _) ts encl encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# (l,v)				    = (fromJust (fromJSON encl), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable	
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if vChanged (refreshFun l (nv,nmask) vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value (nl,nv) False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=False} rep
			(TCInteractLocal taskId nts (toJSON nl) (toJSON nv) nmask), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactViewOnly :: !d !(ReadOnlyShared r) (r -> (v,InteractionMask)) (r (v,InteractionMask) Bool Bool Bool -> (v,InteractionMask)) (Maybe (Editor v))
                        -> Task v | descr d & iTask r & iTask v
interactViewOnly desc shared initFun refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e, iworld)
			Ok r
				# (v,mask)	= initFun r
				= eval event evalOpts (TCInteractViewOnly taskId ts (toJSON r) (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractViewOnly taskId=:(TaskId instanceNo _) ts encr encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# (r,v)				    = (fromJust (fromJSON encr), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# rChanged				= nr =!= r
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nv,nmask) 		    = if (rChanged || vChanged) (refreshFun nr (nv,nmask) rChanged vChanged vValid) (nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nv False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True} rep
			(TCInteractViewOnly taskId nts (toJSON nr) (toJSON nv) nmask), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)


interactLocalViewOnly :: !d (v,InteractionMask) ((v,InteractionMask) Bool -> (v,InteractionMask)) (Maybe (Editor v))
                        -> Task v | descr d & iTask v
interactLocalViewOnly desc initVal refreshFun mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (v,mask)	= initVal
		= eval event evalOpts (TCInteractLocalViewOnly taskId ts (toJSON v) mask) iworld
				
	eval event evalOpts (TCInteractLocalViewOnly taskId=:(TaskId instanceNo _) ts encv mask) iworld=:{current={taskTime}}
		//Decode stored values
		# v				        = fromJust (fromJSON encv)
		//Determine next v by applying edit event if applicable
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nv,nmask) 		    = if vChanged (refreshFun (nv,nmask) vValid) (nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nv False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=False} rep
			(TCInteractLocalViewOnly taskId nts (toJSON nv) nmask), iworld)

	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactSharedInformation :: !d !(ReadOnlyShared r) (r -> v) (Maybe (Editor v)) -> Task r | descr d & iTask r & iTask v
interactSharedInformation desc shared toView mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		= case mbr of
			Error e		= (ExceptionResult e,iworld)
			Ok r
				# v = toView r
				# (l,v,mask) = (r,v,Touched)
				= eval event evalOpts (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval event evalOpts (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'SDS'.readRegister taskId shared iworld
		| isError mbr			= (ExceptionResult (fromError mbr), iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun nr) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=True} rep (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun r
		# v = toView r
		= (r,v,Touched)

interactNullEnter :: !d !v (v->l) (Maybe (Editor v)) -> Task l | descr d & iTask v & iTask l
interactNullEnter desc initFun fromf mbEditor = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = initFun
		# mask = Untouched
		= eval event evalOpts (TCInteract1 taskId ts (toJSON v) mask) iworld
	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encv mask) iworld=:{current={taskTime}}
		//Decode stored value
		# v = fromJust (fromJSON encv)
		  l = fromf v
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=False} rep (TCInteract1 taskId nts (toJSON nv) nmask), iworld)
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok
			= (fromf v,v,m)
			= (l,v,m)

interactNullUpdate :: !d !(l -> v) (l v -> l) (Maybe (Editor v)) l -> Task l | descr d & iTask l & iTask v
interactNullUpdate desc tof fromf mbEditor m = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = tof m
		  l = m
		  mask = Touched
		= eval event evalOpts (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,change,iworld)
								= matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		# rep 					= TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=False} rep (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok 
			# nl = fromf l v
			# nv = tof nl
			= (l,nv,Touched)	
		= (l,v,m)

interactNullView :: !d (l->v) (Maybe (Editor v)) l -> Task l | descr d & iTask l & iTask v
interactNullView desc tof mbEditor m = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# l = m
		  v = Display (tof l)
		  mask = Touched
		= eval event evalOpts (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval event evalOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{current={taskTime}}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,change,iworld) = matchAndApplyEvent event taskId evalOpts mbEditor taskTime v mask ts desc iworld
		# nl = l
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (ui,iworld) 			= visualizeView taskId evalOpts mbEditor (nv,nmask,nver) desc iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		# rep = TaskRep ui change
		= (ValueResult value {TaskEvalInfo|lastEvent=nts,removedTasks=[],refreshSensitive=False} rep (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval event evalOpts (TCDestroy _) iworld = (DestroyedResult,iworld)


matchAndApplyEvent ResetEvent matchId evalOpts mbEditor taskTime v mask ts desc iworld
	//When we get a reset event, we recompute the entire UI
	# (ui,iworld) = visualizeView matchId evalOpts mbEditor (v,mask,verifyMaskedValue (v,mask)) desc iworld
	= (v,mask,ts,ReplaceUI ui,iworld)
matchAndApplyEvent (EditEvent taskId name value) matchId evalOpts mbEditor taskTime v mask ts desc iworld
	| taskId == matchId
		| otherwise
			# ((nv,nmask),change,iworld) = updateValueAndMask taskId (s2dp name) mbEditor value (v,mask) iworld
			= (nv,nmask,taskTime,change,iworld)
	| otherwise	= (v,mask,ts,NoChange,iworld)
matchAndApplyEvent (FocusEvent taskId) matchId evalOpts mbEditor taskTime v mask ts desc iworld
	= (v,mask, if (taskId == matchId) taskTime ts, NoChange, iworld)
matchAndApplyEvent _ matchId evalOpts mbEditor taskTime v mask ts desc iworld
	= (v,mask,ts,NoChange,iworld)

visualizeView taskId evalOpts mbEditor value=:(v,vmask,vver) desc iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
	# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, iworld = iworld}
	# (editUI,vst=:{VSt|iworld})	= editor.Editor.genUI [] v vmask vver vst
	# promptUI  = toPrompt desc
	# ui 		= UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap} [promptUI,editUI]
	# ui		= if evalOpts.autoLayout (autoAccuInteract.ContentLayout.layout ui) ui
	= (ui,iworld)

updateValueAndMask taskId path mbEditor update (old,omask) iworld
	# editor = fromMaybe gEditor{|*|} mbEditor
    # (new,nmask,ust=:{USt|iworld}) = editor.Editor.appDiff path update old omask {USt|taskId=toString taskId,editorId=editorId path,iworld=iworld}
	//Compare for changes
	# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, iworld = iworld}
	# (changes,vst=:{VSt|iworld}) 	= editor.Editor.genDiff [] old new vst
    = ((new,nmask),changes,iworld)


