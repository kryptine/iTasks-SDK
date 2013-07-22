implementation module iTasks.API.Common.InteractionTasks

from StdFunc import id, const, o, flip
from iTasks.API.Core.SystemData import null
from Data.Tuple import appSnd
from Data.List import isMemberGen, instance Functor []
from System.Time import :: Timestamp(..)
from Data.Map import qualified get, put

import StdBool, StdList, StdMisc, StdTuple, Data.Functor
import iTasks.API.Core.CoreTasks, iTasks.API.Core.OptimizedCoreTasks, iTasks.API.Core.CoreCombinators
import iTasks.API.Common.CommonCombinators, iTasks.API.Core.LayoutCombinators, iTasks.API.Core.SystemData
import iTasks.Framework.Generic.Interaction

enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m
enterInformation d [EnterWith fromf:_]
/*
	= interact d null
//		(\r -> (defaultValue,defaultValue,Untouched))
		(\r -> let v = defaultValue in (fromf v,v,Untouched))
		(\l r v m ok -> if ok (fromf v,v,m) (l,v,m))
*/
	= interactNullEnter d defaultValue fromf
enterInformation d _ = enterInformation d [EnterWith id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInformation d [UpdateWith tof fromf:_] m
	= interact d null
		(\r -> let v = tof m in (m,(v,Touched)))
		(\l r (v,m) rCh vCh vOk -> if vOk (let nl = fromf l v in (let nv = tof nl in (nl,(nv,m)))) (l,(v,m)))

//TODO: THIS OPTIMIZATION IS WRONG!
//	= interactNullUpdate d tof fromf m
updateInformation d _ m = updateInformation d [UpdateWith (\l -> l) (\_ v -> v)] m

//Same as update information, but with an initial untouched mask (used in enterChoice)
updateInitialInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInitialInformation d [UpdateWith tof fromf:_] m
	= interact d null
		(\r -> let v = tof m in (m,(v,Untouched)))
		(\l r (v,m) rCh vCh vOk -> if vOk (let nl = fromf l v in (let nv = tof nl in (nl,(nv,m)))) (l,(v,m)))
		
updateInitialInformation d _ m = updateInitialInformation d [UpdateWith (\l -> l) (\_ v -> v)] m


viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m
viewInformation d [ViewWith tof:_] m
/*
	= interact d null
		(\r -> let v = Display (tof m) in (m,v,defaultMask v))
		(\l r v m ok -> (l,v,m))
*/
	= interactNullView d tof m
viewInformation d _ m = viewInformation d [ViewWith id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d [UpdateWith tof fromf:_] shared
	= interact d (toReadOnly shared)
				(\r -> let v = tof r in (fromf r v,(v,Touched)))
				(\l r (v,m) rCh vCh vOk -> if vOk
					(if rCh //If the share changed, refresh the view
						(let nv = tof r in (fromf r nv,(nv,Touched)))
						(fromf r v,(v,m))
					)
					(l,(v,m))
				)
				@> (mapval,shared)

updateSharedInformation d _ shared			
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	= case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))
			= interact d (toReadOnly shared)
				(\r -> let v = rtow r in (rtow r,(v,Touched)))
				(\l r (v,m) rCh vCh vOk -> if vOk (if (rtow r =!= l) (let nv = rtow r in (nv,(nv,Touched))) (v,(v,m))) (l,(v,m)))
				@> (mapval,shared)
		_
			= interact d (toReadOnly shared)
				(\r -> let v = (Display r,defaultValue) in (defaultValue,(v,CompoundMask [Touched,Untouched])))
				(\l r ((_,v),(CompoundMask [_,m])) rCh vCh vOk -> let nl = if vOk v l in (let nv = (Display r,nl) in (nl,(nv,CompoundMask [Touched,m]))))
				@> (mapval,shared)	

mapval (Value w _) _	= Just w
mapval _ _				= Nothing

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | descr d & iTask r
viewSharedInformation d [ViewWith tof:_] shared
/*
	= interact d (toReadOnly shared)
		(\r -> let v = Display (tof r) in (r,v,defaultMask v))
		(\l r v m ok -> let v = Display (tof r) in (r,v,defaultMask v)) 
*/
	= interactSharedInformation d (toReadOnly shared) (\r -> Display (tof r))
viewSharedInformation d _ shared = viewSharedInformation d [ViewWith id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | descr d & iTask r & iTask m
updateInformationWithShared d [UpdateWith tof fromf:_] shared m
	= interact d (toReadOnly shared)
		(\r -> let v = tof (r,m) in (m,(v,Touched)))
		(\l r (v,msk) rCh vCh vOk -> let nl = if vOk (fromf (r,l) v) l in (let v = tof (r,nl) in (nl,(v,Touched))))
updateInformationWithShared d _ shared m
	= interact d (toReadOnly shared)
		(\r -> let v = (Display r,m) in (m,(v,CompoundMask [Touched,Untouched])))
		(\l r ((_,v),(CompoundMask [_,msk])) rCh vCh vOk -> let nl = if vOk v l in (let nv = (Display r,nl) in (nl,(nv,CompoundMask [Touched,msk]))))

enterChoice :: !d ![ChoiceOption o] ![o] -> Task o | descr d & iTask o
enterChoice d options container
	= updateInitialInformation d (choiceToUpdate options) (container,Nothing) @? res
where
	res (Value (_,Just x) s)	= Value x s
	res _						= NoValue

updateChoice :: !d ![ChoiceOption o] ![o] o -> Task o | descr d & iTask o
updateChoice d options container sel
	= updateInformation d (choiceToUpdate options) (container,Just sel) @? res
where
	res (Value (_,Just x) s)	= Value x s
	res _						= NoValue

removeMaybeFromValue (Value (Just x) s)		= Value x s
removeMaybeFromValue _						= NoValue

enterSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared [o] w)
					 -> Task o | descr d & iTask o & iTask w
enterSharedChoice d [] shared
	= updateInformationWithSharedChoiceNoView d shared Nothing @? removeMaybeFromValue
enterSharedChoice d options shared
	= updateInformationWithSharedChoice d options shared Nothing @? removeMaybeFromValue

updateSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) o
					  -> Task o | descr d & iTask o & iTask w
updateSharedChoice d [] shared sel
	= updateInformationWithSharedChoiceNoView d shared (Just sel) @? removeMaybeFromValue
updateSharedChoice d options shared sel
	= updateInformationWithSharedChoice d options shared (Just sel) @? removeMaybeFromValue

updateInformationWithSharedChoiceNoView :: d (ReadWriteShared [c] a) (Maybe c) -> Task (Maybe c) | descr d & iTask c
updateInformationWithSharedChoiceNoView d shared m
	= interactSharedChoiceNoView d (toReadOnly shared) m toViewId
  where
	toViewId :: [a] (Maybe a) -> DynamicChoiceNoView a | gEq{|*|},gEditMeta{|*|},gVisualizeText{|*|} a
	toViewId container mbSel
		# choice = initChoiceNoView AutoChoice container
		= maybe choice (\sel -> selectOptionNoView sel choice) mbSel

updateInformationWithSharedChoice :: !d ![ChoiceOption c] !(ReadWriteShared [c] a) (Maybe c) -> Task (Maybe c) | descr d & iTask c
updateInformationWithSharedChoice d [ChooseWith type view:_] shared m
	= interactSharedChoice d (toReadOnly shared) m (toView type view)
  where
	toView :: (ChoiceType a) (a -> b) [a] (Maybe a) -> DynamicChoice b a | gEq{|*|},gEditMeta{|*|},gVisualizeText{|*|} a
	toView type view container mbSel
		# choice = initChoice type container view
        = maybe choice (\sel -> selectOption sel choice) mbSel

enterMultipleChoice :: !d ![MultiChoiceOption o] ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options container
	= updateInformation d (multiChoiceToUpdate options) (container,[]) @ snd

updateMultipleChoice :: !d ![MultiChoiceOption o] ![o] [o] -> Task [o] | descr d & iTask o
updateMultipleChoice d options container sel
	= updateInformation d (multiChoiceToUpdate options) (container,sel) @ snd

enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d options shared
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared []

updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel 
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared sel

wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | descr d & iTask r
wait desc pred shared
	=	viewSharedInformation desc [ViewWith (const "Waiting for information update")] shared
	>>* [WhenValid pred return]
	
waitForTime :: !Time -> Task Time
waitForTime time =
	viewSharedInformation ("Wait for time", ("Wait until " +++ toString time)) [] currentTime >>* [WhenValid (\now -> time < now) return]

waitForDate :: !Date -> Task Date
waitForDate date =
	viewSharedInformation ("Wait for date", ("Wait until " +++ toString date)) [] currentDate >>* [WhenValid (\now -> date < now) return]
	
waitForDateTime :: !DateTime -> Task DateTime
waitForDateTime datetime =
	viewSharedInformation ("Wait for date and time", ("Wait until " +++ toString datetime)) [] currentDateTime >>* [WhenValid (\now -> datetime < now) return]

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>= \now -> waitForTime (now + time)

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions
	=	viewInformation Void [] Void
	>>* [AnyTime action (\_ -> return val) \\ (action,val) <- actions]

choiceToUpdate :: [ChoiceOption o] -> [UpdateOption ([o], Maybe o) ([o], Maybe o)] | iTask o
choiceToUpdate [ChooseWith type view:_] = [UpdateWith (toView type view) fromView]
where
	toView :: (ChoiceType a) (a -> b) ([a],Maybe a) -> DynamicChoice b a | gEq{|*|},gEditMeta{|*|},gVisualizeText{|*|} a
	toView type view (container,mbSel)
		= let choice = initChoice type container view in
			maybe choice (\sel -> selectOption sel choice) mbSel
	
	fromView :: (.a,.b) (c d e) -> (.a,Maybe e) | Choice c
	fromView (options,_) choice = (options,getMbSelection choice)

choiceToUpdate _ = [UpdateWith toViewId fromViewId]
where
	toViewId (container,mbSel)
		= let choice = initChoiceNoView AutoChoice container in
			maybe choice (\sel -> selectOptionNoView sel choice) mbSel

	fromViewId (container,_) choice = (container,getMbSelectionNoView choice)

autoChoiceType l = case headers l undef of
    [] = ChooseFromComboBox
    _  = ChooseFromGrid
where
    headers :: [a] a -> [String] | gEditMeta{|*|} a
    headers _ a = [fromMaybe "" label \\{EditMeta|label} <- gEditMeta{|*|} a]

initChoice AutoChoice 				 options view	= initChoice (autoChoiceType options) options view
initChoice ChooseFromComboBox		 options view	= DCCombo (ComboChoice [(view o,o) \\ o <- options] Nothing)
initChoice ChooseFromRadioButtons	 options view	= DCRadio (RadioChoice [(view o,o) \\ o <- options] Nothing)
initChoice (ChooseFromTree groupFun) options view	= DCTree (TreeChoice [fmap (\o -> (view o,o)) t \\ t <- groupFun options] Nothing)
initChoice ChooseFromGrid			 options view	= DCGrid (GridChoice [(view o,o) \\ o <- options] Nothing)

initChoiceNoView AutoChoice                 options = initChoiceNoView (autoChoiceType options) options
initChoiceNoView ChooseFromComboBox		    options = DCComboNoView	(ComboChoiceNoView options Nothing)
initChoiceNoView ChooseFromRadioButtons	    options = DCRadioNoView	(RadioChoiceNoView options Nothing)
initChoiceNoView (ChooseFromTree groupFun)	options = DCTreeNoView	(TreeChoiceNoView (groupFun options) Nothing)
initChoiceNoView ChooseFromGrid			    options = DCGridNoView	(GridChoiceNoView options Nothing)

multiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption ([o], [o]) ([o],[o])] | iTask o
multiChoiceToUpdate [ChooseMultipleWith type view:_] = [UpdateWith (toView type) fromView]
where
	toView type (options,sel)	= selectOptions sel (initChoice type options)

	initChoice AutoMultiChoice options      = initChoice ChooseFromCheckBoxes options
	initChoice ChooseFromCheckBoxes options = CheckMultiChoice [(view o,o) \\ o <- options] []
	
	fromView (options,_) choice = (options,getSelections choice)
	
multiChoiceToUpdate _ = multiChoiceToUpdate [ChooseMultipleWith AutoMultiChoice id]

sharedMultiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption ([o], [o]) [o]] | iTask o
sharedMultiChoiceToUpdate options = case multiChoiceToUpdate options of
	[UpdateWith fromf tof]	= [UpdateWith fromf (\m v -> snd (tof m v))]
	_						= []

viewTitle :: !a -> Task a | iTask a 
viewTitle a = viewInformation (Title title) [ViewWith view] a <<@ InContainer <<@ AfterLayout (tweakAttr titleFromValue)
where
	title	= visualizeAsLabel a
	view a	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text title]]

viewSharedTitle :: !(ReadWriteShared r w) -> Task r | iTask r
viewSharedTitle s = viewSharedInformation Void [ViewWith view] s <<@ InContainer <<@ AfterLayout (tweakAttr titleFromValue)
where
	view r	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text (visualizeAsLabel r)]]	

titleFromValue :: UIAttributes -> UIAttributes
titleFromValue attr = case 'Data.Map'.get VALUE_ATTRIBUTE attr of
	Just v	= 'Data.Map'.put TITLE_ATTRIBUTE v attr
	_		= attr

