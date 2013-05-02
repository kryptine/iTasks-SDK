implementation module OptimizedCoreTasks

import StdList, StdBool, StdInt, StdTuple,StdMisc, Time, Error, OSError, Map, Tuple, List
import qualified StdList
import iTasks.Framework.iTaskClass, iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval
import iTasks.Framework.TaskStore, iTasks.Framework.UIDefinition, iTasks.Framework.Shared
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import LayoutCombinators 
from SharedDataSource		import qualified read, readRegister, write, writeFilterMsg
from StdFunc				import o, id
from iTasks.Framework.IWorld	import :: IWorld(..)
from SystemData				import topLevelTasks
from Map					import qualified get

interactSharedChoice :: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t v l)
						-> Task (Maybe l) | descr d & Choice t & iTask r & iTask l & iTask (t v l)
interactSharedChoice desc shared initial_mask toView = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
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
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv nmask valid)
										(l,nv,nmask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep) (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv nmask valid
		# nl = if valid (getMbSelection nv) l
		# v = toView nr nl
		| v === nv	= (nl,nv,nmask)	//If the view value is the same, we can keep the mask info
					= (nl,v,Touched)

interactSharedChoiceNoView :: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t l)
							  -> Task (Maybe l) | descr d & ChoiceNoView t & iTask r & iTask l & iTask (t l)
interactSharedChoiceNoView desc shared initial_mask toViewId = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
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
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr),iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed
										(refresh_fun l nr nv nmask valid)
										(l,nv,nmask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=True} (finalizeRep repOpts rep) (TCInteract2 taskId nts (toJSON nl) (toJSON nr) nmask), iworld)
	eval event repOpts (TCDestroy _) iworld = (DestroyedResult,iworld)

	refresh_fun l nr nv nmask valid
		# nl = if valid (getMbSelectionNoView nv) l
		# v = toViewId nr nl
		| v === nv	= (nl,nv,nmask)	//If the view value is the same, we can keep the mask info
					= (nl,v,Touched)

interactSharedInformation :: !d !(ReadOnlyShared r) (r -> v) -> Task r | descr d & iTask r & iTask v
interactSharedInformation desc shared toView = Task eval
where
	eval event repOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
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
		# (mbr,iworld) 			= 'SharedDataSource'.readRegister instanceNo shared iworld
		| isError mbr			= (exception (fromError mbr), iworld)
		# nr					= fromOk mbr
		//Apply refresh function if r or v changed
		# changed				= (nts =!= ts) || (nr =!= r) 
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun nr) (l,nv,mask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
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
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
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
		# valid					= isValidMask (verifyMaskedValue nv nmask)
		# (nl,nv,nmask) 		= if changed (refresh_fun l nv nmask valid) (l,nv,mask)
		//Make visualization
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
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
		# validity				= verifyMaskedValue nv nmask
		# (rep,iworld) 			= visualizeView taskId repOpts nv validity desc (visualizeAsText AsLabel nl) iworld
		# value 				= if (isValidMask validity) (Value nl False) NoValue
		= (ValueResult value {TaskInfo|lastEvent=nts,refreshSensitive=False} (finalizeRep repOpts rep) (TCInteract1 taskId nts (toJSON nl) nmask), iworld)
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
