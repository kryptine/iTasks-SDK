implementation module iTasks.API.Core.OptimizedCoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc
import System.Time, Data.Error, System.OSError, Data.Map, Data.Tuple, Data.List, Text.JSON
import qualified StdList
import iTasks.Framework.Generic, iTasks.Framework.Generic.Interaction, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval
import iTasks.Framework.TaskStore, iTasks.Framework.UIDefinition, iTasks.Framework.Shared
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.API.Core.LayoutCombinators 
from Data.SharedDataSource			import qualified read, readRegister, write, writeFilterMsg
from StdFunc						import o, id
from iTasks.Framework.IWorld		import :: IWorld(..)
from iTasks.API.Core.SystemData		import topLevelTasks
from Data.Map						import qualified get

interactExposed :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask)))
                        -> Task (l,v) | descr d & iTask l & iTask r & iTask v
interactExposed desc shared initFun refreshFun = Task eval
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
		# value 				= if (isValid nver) (Value (nl,nv) False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep)
			(TCInteract taskId nts (toJSON nl) (toJSON nr) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactLocalExposed :: !d (l,(v,InteractionMask)) (l (v,InteractionMask) Bool -> (l,(v,InteractionMask)))
                        -> Task (l,v) | descr d & iTask l & iTask v
interactLocalExposed desc initVal refreshFun = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (l,(v,mask))	= initVal
		= eval event repOpts (TCInteractLocal taskId ts (toJSON l) (toJSON v) mask) iworld
				
	eval event repOpts (TCInteractLocal taskId=:(TaskId instanceNo _) ts encl encv mask) iworld=:{taskTime}
		//Decode stored values
		# (l,v)				    = (fromJust (fromJSON encl), fromJust (fromJSON encv))
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nl,(nv,nmask)) 		= if vChanged (refreshFun l (nv,nmask) vValid) (l,(nv,nmask))
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value (nl,nv) False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep)
			(TCInteractLocal taskId nts (toJSON nl) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

interactViewOnly :: !d !(ReadOnlyShared r) (r -> (v,InteractionMask)) (r (v,InteractionMask) Bool Bool Bool -> (v,InteractionMask))
                        -> Task v | descr d & iTask r & iTask v
interactViewOnly desc shared initFun refreshFun = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e, iworld)
			Ok r
				# (v,mask)	= initFun r
				= eval event repOpts (TCInteractViewOnly taskId ts (toJSON r) (toJSON v) mask) iworld
				
	eval event repOpts (TCInteractViewOnly taskId=:(TaskId instanceNo _) ts encr encv mask) iworld=:{taskTime}
		//Decode stored values
		# (r,v)				    = (fromJust (fromJSON encr), fromJust (fromJSON encv))
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
		# (nv,nmask) 		    = if (rChanged || vChanged) (refreshFun nr (nv,nmask) rChanged vChanged vValid) (nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nv) iworld
		# value 				= if (isValid nver) (Value nv False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep)
			(TCInteractViewOnly taskId nts (toJSON nr) (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)


interactLocalViewOnly :: !d (v,InteractionMask) ((v,InteractionMask) Bool -> (v,InteractionMask))
                        -> Task v | descr d & iTask v
interactLocalViewOnly desc initVal refreshFun = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (v,mask)	= initVal
		= eval event repOpts (TCInteractLocalViewOnly taskId ts (toJSON v) mask) iworld
				
	eval event repOpts (TCInteractLocalViewOnly taskId=:(TaskId instanceNo _) ts encv mask) iworld=:{taskTime}
		//Decode stored values
		# v				        = fromJust (fromJSON encv)
		//Determine next v by applying edit event if applicable
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Apply refresh function if r or v changed
		# vChanged				= nts =!= ts
		# vValid				= isValid (verifyMaskedValue (nv,nmask))
		# (nv,nmask) 		    = if vChanged (refreshFun (nv,nmask) vValid) (nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nv) iworld
		# value 				= if (isValid nver) (Value nv False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep)
			(TCInteractLocalViewOnly taskId nts (toJSON nv) nmask), iworld)

	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

//John's versions
/*
interactSharedChoice	:: !d !(ReadOnlyShared r) (Maybe s) (l -> s) (r (Maybe s) -> t v l)
							-> Task (Maybe s) | descr d & Choice t & iTask r & iTask l & iTask (t v l) & iTask s
interactSharedChoice desc shared initial_mask targetFun toView = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e,iworld)
			Ok r
				# v = toView r initial_mask
				# (l,v,mask) = (initial_mask,v,Touched)
				= eval event repOpts (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval event repOpts (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv nmask valid)
										(l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep) (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv nmask valid
		# nl = if valid (getMbSelection targetFun nv) l
		# v = toView nr nl
		| v === nv	= (nl,nv,nmask)	//If the view value is the same, we can keep the mask info
					= (nl,v,Touched)
*/
/*
interactSharedChoiceNoView	:: !d !(ReadOnlyShared r) (Maybe s) (l -> s) (r (Maybe s) -> t l)
								-> Task (Maybe s) | descr d & ChoiceNoView t & iTask r & iTask l & iTask (t l) & iTask s
interactSharedChoiceNoView desc shared initial_mask targetFun toViewId = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e,iworld)
			Ok r
				# v = toViewId r initial_mask
				# (l,v,mask) = (initial_mask,v,Touched)
				= eval event repOpts (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval event repOpts (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust( fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toViewId r l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld)	= matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv nmask valid)
										(l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep) (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv nmask valid
		# nl = if valid (getMbSelectionNoView targetFun nv) l
		# v = toViewId nr nl
		| v === nv	= (nl,nv,nmask)	//If the view value is the same, we can keep the mask info
					= (nl,v,Touched)
*/
interactSharedInformation :: !d !(ReadOnlyShared r) (r -> v) -> Task r | descr d & iTask r & iTask v
interactSharedInformation desc shared toView = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		= case mbr of
			Error e		= (exception e,iworld)
			Ok r
				# v = toView r
				# (l,v,mask) = (r,v,Touched)
				= eval event repOpts (TCInteract2 taskId ts (toJSON l) (toJSON r) mask) iworld
	eval event repOpts (TCInteract2 taskId=:(TaskId instanceNo _) ts encl encr mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  r = fromJust (fromJSON encr)
		  v = toView r
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Load next r from shared value
		# (mbr,iworld) 			= 'Data.SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr), iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun nr) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep) (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun r
		# v = toView r
		= (r,v,Touched) 

interactNullEnter :: !d !v (v->l) -> Task l | descr d & iTask v & iTask l
interactNullEnter desc initFun fromf = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = initFun
		# mask = Untouched
		= eval event repOpts (TCInteract1 taskId ts (toJSON v) mask) iworld
	eval event repOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encv mask) iworld=:{taskTime}
		//Decode stored value
		# v = fromJust (fromJSON encv)
		  l = fromf v
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep) (TCInteract1 taskId nts (toJSON nv) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok
			= (fromf v,v,m)
			= (l,v,m)

interactNullUpdate :: !d !(l -> v) (l v -> l) l -> Task l | descr d & iTask l & iTask v
interactNullUpdate desc tof fromf m = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# v = tof m
		  l = m
		  mask = Touched
		= eval event repOpts (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval event repOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = tof l
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		//Apply refresh function if v changed
		# changed				= nts =!= ts
		# valid					= isValid (verifyMaskedValue (nv,nmask))
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,nmask)
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep) (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l v m ok
		| ok 
			# nl = fromf l v
			# nv = tof nl
			= (l,nv,Touched)	
		= (l,v,m)

interactNullView :: !d (l->v) l -> Task l | descr d & iTask l & iTask v
interactNullView desc tof m = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# l = m
		  v = Display (tof l)
		  mask = Touched
		= eval event repOpts (TCInteract1 taskId ts (toJSON l) mask) iworld
	eval event repOpts (TCInteract1 taskId=:(TaskId instanceNo _) ts encl mask) iworld=:{taskTime}
		//Decode stored values
		# l	= fromJust (fromJSON encl)
		  v = Display (tof l)
		//Determine next v by applying edit event if applicable 	
		# (nv,nmask,nts,iworld) = matchAndApplyEvent event taskId taskTime v mask ts iworld
		# nl = l
		//Make visualization
		# nver					= verifyMaskedValue (nv,nmask)
		# (rep,iworld) 			= visualizeView taskId repOpts (nv,nmask,nver) desc (visualizeAsLabel nl) iworld
		# value 				= if (isValid nver) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep) (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

matchAndApplyEvent (EditEvent _ taskId name value) matchId taskTime v mask ts iworld
	| taskId == matchId
		| otherwise
			# (nv,nmask)	= updateValueAndMask (s2dp name) value (v,mask)
			= (nv,nmask,taskTime,iworld)
	| otherwise	= (v,mask,ts,iworld)
matchAndApplyEvent (FocusEvent _ taskId) matchId taskTime v mask ts iworld
	= (v,mask, if (taskId == matchId) taskTime ts, iworld)
matchAndApplyEvent _ matchId taskTime v mask ts iworld
	= (v,mask,ts,iworld)

visualizeView taskId repOpts (v,mask,ver) desc valueAttr iworld
	# layout	= repLayoutRules repOpts
	# (controls,iworld) = visualizeAsEditor (v,mask,ver) taskId layout iworld
	# uidef		= UIControlStack (layout.LayoutRules.accuInteract (toPrompt desc) {UIControlStack|attributes=put VALUE_ATTRIBUTE valueAttr newMap,controls=controls})
    # tp = {TaskPart | taskId = toString taskId, tag = Nothing, repKind = EditorRep (toJSON v)}
	= (TaskRep uidef [tp], iworld)
