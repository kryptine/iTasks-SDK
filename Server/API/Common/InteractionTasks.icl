implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Tuple import appSnd
from List import isMemberGen, instance Functor []
from Shared import makeReadOnlyShared
from Time import :: Timestamp(..)

import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, CoreCombinators, CommonCombinators, LayoutCombinators, SystemData
							
enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m
enterInformation d [EnterWith fromf]
	= interact d  null
		(\r -> (defaultValue,defaultValue,Untouched))
		(\l r v m ok -> if ok (fromf v,v,m) (l,v,m))
enterInformation d _ = enterInformation d [EnterWith id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInformation d [UpdateWith tof fromf] m
	= interact d null
		(\r -> let v = tof m in (m,v,defaultMask v))
		(\l r v m ok -> if ok (let nl = fromf l v in (let nv = tof nl in (nl,nv,defaultMask nv))) (l,v,m))
updateInformation d _ m = updateInformation d [UpdateWith (\l -> l) (\_ v -> v)] m

viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m
viewInformation d [ViewWith tof] m
	= interact d null
		(\r -> let v = (Display m) in (m,v,defaultMask v))
		(\l r v m ok -> (l,v,m))
viewInformation d _ m = viewInformation d [ViewWith id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d [UpdateWith tof fromf] shared
	= interact d (toReadOnly shared)
				(\r -> let v = tof r in (fromf r v,v,defaultMask v))
				(\l r v m ok -> if ok (let nl = fromf r v in (let nv = tof r in (nl,nv,defaultMask nv))) (l,v,m))
		@> (mapval,shared)
updateSharedInformation d _ shared			
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	= case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))
			= interact d (toReadOnly shared)
				(\r -> let v = rtow r in (rtow r,v,defaultMask v))
				(\l r v m ok -> if ok (let nl = v in (let nv = rtow r in (nl,nv,defaultMask nv))) (l,v,m))
				@> (mapval,shared)
		_
			= interact d (toReadOnly shared)
				(\r -> let v = (Display r,defaultValue) in (defaultValue,v,Touched [defaultMask (Display r),Untouched]))
				(\l r (_,v) (Touched [_,m]) ok -> let nl = if ok v l in (let nv = (Display r,nl) in (nl,nv,Touched [defaultMask (Display r),m])))
				@> (mapval,shared)	

mapval (Value w _) _	= Just w
mapval _ _				= Nothing

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | descr d & iTask r
viewSharedInformation d [ViewWith tof] shared
	= interact d (toReadOnly shared)
		(\r -> let v = Display (tof r) in (r,v,defaultMask v))
		(\l r v m ok -> let v = Display (tof r) in (r,v,defaultMask v)) 
viewSharedInformation d _ shared = viewSharedInformation d [ViewWith id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | descr d & iTask r & iTask m
updateInformationWithShared d [UpdateWith tof fromf] shared m
	= interact d (toReadOnly shared)
		(\r -> let v = tof (r,m) in (m,v,defaultMask v))
		(\l r v msk ok -> let nl = if ok (fromf (r,l) v) l in (let v = tof (r,nl) in (nl,v,defaultMask v)))
updateInformationWithShared d _ shared m
	= interact d (toReadOnly shared)
		(\r -> let v = (Display r,m) in (m,v,Touched [defaultMask (Display r),Untouched]))
		(\l r (_,v) (Touched [_,msk]) ok -> let nl = if ok v l in (let nv = (Display r,nl) in (nl,nv,Touched [defaultMask (Display r),msk])))

enterChoice :: !d ![ChoiceOption o] !(container o) -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
enterChoice d options container
	= updateInformation d (choiceToUpdate options) (container,Nothing) @? res
where
	res (Value (_,Just x) s)	= Value x s
	res _						= NoValue

updateChoice :: !d ![ChoiceOption o] !(container o) o -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
updateChoice d options container sel
	= updateInformation d (choiceToUpdate options) (container,Just sel) @? res
where
	res (Value (_,Just x) s)	= Value x s
	res _						= NoValue
	
enterSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared (container o) w) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedChoice d options shared
	= updateInformationWithShared d (sharedChoiceToUpdate options) shared Nothing @? res
where
	res (Value (Just x) s)		= Value x s
	res _						= NoValue

updateSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared (container o) w) o -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedChoice d options shared sel
	= updateInformationWithShared d (sharedChoiceToUpdate options) shared (Just sel) @? res
where
	res (Value (Just x) s)		= Value x s
	res _						= NoValue
	
enterMultipleChoice :: !d ![MultiChoiceOption o] !(container o) -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
enterMultipleChoice d options container
	= updateInformation d (multiChoiceToUpdate options) (container,[]) @ snd

updateMultipleChoice :: !d ![MultiChoiceOption o] !(container o) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
updateMultipleChoice d options container sel
	= updateInformation d (multiChoiceToUpdate options) (container,sel) @ snd

enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared (container o) w) -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedMultipleChoice d options shared
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared []

updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared (container o) w) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
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
	=	SetLayout hideLayout 
	@>> viewInformation "Choose an action" [] Void
	>>* [AnyTime action (\_ -> return val) \\ (action,val) <- actions]
	
instance OptionContainer []
where
	toOptionList l				= l
	toOptionTree l				= Tree (map Leaf l)
	suggestedChoiceType	l
		| not (isEmpty (snd (headers l)))	= ChooseFromGrid
		| length l > 7						= ChooseFromComboBox
		| otherwise							= ChooseFromRadioButtons
	where
		// unify type of list elements with type to determine headers for
		headers :: [a] -> (a,![String]) | gHeaders{|*|} a
		headers _ = gHeaders{|*|}
	suggestedMultiChoiceType _	= ChooseFromCheckBoxes
	
instance OptionContainer Tree
where
	toOptionList (Tree nodes) = flatten (map toOptionList` nodes)
	where
		toOptionList` node = case node of
			Leaf option			= [option]
			Node option nodes	= [option:flatten (map toOptionList` nodes)]
	toOptionTree t = t
	suggestedChoiceType _		= ChooseFromTree
	suggestedMultiChoiceType _	= ChooseFromCheckBoxes

choiceToUpdate :: [ChoiceOption o] -> [UpdateOption (container o, Maybe o) (container o, Maybe o)] | OptionContainer container & iTask o
choiceToUpdate [ChooseWith type view] = [UpdateWith (toView type) fromView]
where
	toView type (container,mbSel)
		= let choice = initChoice type container in
			maybe choice (\sel -> selectOption sel choice) mbSel
		
	initChoice AutoChoice container				= initChoice (suggestedChoiceType container) container
	initChoice ChooseFromComboBox container		= DCCombo	(ComboChoice [(view o,o) \\ o <- toOptionList container] Nothing)
	initChoice ChooseFromRadioButtons container	= DCRadio	(RadioChoice [(view o,o) \\ o <- toOptionList container] Nothing)
	initChoice ChooseFromTree container			= DCTree	(TreeChoice (fmap (\o -> (view o,o)) (toOptionTree container)) Nothing)
	initChoice ChooseFromGrid container			= DCGrid	(GridChoice [(view o,o) \\ o <- toOptionList container] Nothing)
	
	fromView (container,_) choice = (container,getMbSelection choice)
	
choiceToUpdate _ = choiceToUpdate [ChooseWith AutoChoice id]

sharedChoiceToUpdate :: [ChoiceOption o] -> [UpdateOption (container o, Maybe o) (Maybe o)] | OptionContainer container & iTask o
sharedChoiceToUpdate options = case choiceToUpdate options of
	[UpdateWith fromf tof]	= [UpdateWith fromf (\m v -> snd (tof m v))]
	_						= []

multiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption (container o, [o]) (container o,[o])] | OptionContainer container & iTask o
multiChoiceToUpdate [ChooseMultipleWith type view] = [UpdateWith (toView type) fromView]
where
	toView type (container,sel)	= selectOptions sel (initChoice type container)

	initChoice AutoMultiChoice container		= initChoice (suggestedMultiChoiceType container) container
	initChoice ChooseFromCheckBoxes container	= CheckMultiChoice [(view o,o) \\ o <- toOptionList container] []
	
	fromView (container,_) choice = (container,getSelections choice)
	
multiChoiceToUpdate _ = multiChoiceToUpdate [ChooseMultipleWith AutoMultiChoice id]

sharedMultiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption (container o, [o]) [o]] | OptionContainer container & iTask o
sharedMultiChoiceToUpdate options = case multiChoiceToUpdate options of
	[UpdateWith fromf tof]	= [UpdateWith fromf (\m v -> snd (tof m v))]
	_						= []
