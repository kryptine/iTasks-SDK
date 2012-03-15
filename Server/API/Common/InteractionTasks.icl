implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Tuple import appSnd
from List import isMemberGen, instance Functor []
from Shared import makeReadOnlyShared
from Time import :: Timestamp(..)

import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, CoreCombinators, CommonCombinators, LayoutCombinators, SystemData

DisplayPart f = FormPart (\l r -> FilledForm (Display (f l r)))
							(\l r _ _ -> (l,Just (FilledForm (Display (f l r)))))
							(\l _ _ -> (l,Nothing))
							
enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m
enterInformation d opts
	= interact d (parts opts) defaultValue null @ fst
where
	parts [EnterWith fromf]	= [FormPart (\_ _ -> BlankForm) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (maybe l fromf mbv, Nothing))]
	parts _					= [FormPart (\_ _ -> BlankForm) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (fromMaybe l mbv,Nothing))]

updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInformation d opts m
	= interact d (parts opts) m null @ fst	
where	
	parts [UpdateWith tof fromf]	= [FormPart (\l _ -> FilledForm (tof l)) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (maybe l (fromf l) mbv, Nothing))]
	parts _							= [FormPart (\l _ -> FilledForm l) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (fromMaybe l mbv,Nothing))]
	
viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m
viewInformation d opts m
	= interact d (parts opts) m null @ fst
where
	parts [ViewWith tof]	= [DisplayPart (\l _ -> tof l)]
	parts _					= [DisplayPart (\l _ -> m)]
	
updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d views shared
	= (interact d (parts views) defaultValue (toReadOnly shared) @ fst) @> (mapval, shared)
where
	/*
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	init = case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))			= (\_ r -> rtow r)
		_								= (\_ _ -> defaultValue)
	*/
	parts [UpdateWith tof fromf]
		= [FormPart
			(\w r -> FilledForm (tof r))
			(\w r _ _ -> (w, Just (FilledForm (tof r))))
			(\w r mbv -> (maybe w (fromf r) mbv, Nothing))
			]
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	parts _	= case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))
			//We can use a filled form if r == w
			=	[FormPart
				(\w r -> FilledForm (rtow r))
				(\w r _ _ -> (rtow r, Just (FilledForm (rtow r))))
				(\w r mbv -> (maybe w id mbv, Nothing)) 
				]
		_
			//Split up in display and enter
			=	[DisplayPart (\_ r -> r)
				,FormPart
				(\w r -> BlankForm)
				(\w r _ _ -> (w,Nothing))
				(\w r mbv -> (maybe w id mbv, Nothing))
				]
	
	mapval (Value w _) _	= Just w
	mapval _ _				= Nothing	

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | descr d & iTask r
viewSharedInformation d options shared
	= interact d (parts options) Void (toReadOnly shared) @ snd
where
	parts []		= [DisplayPart (\_ r -> r)]
	parts options	= [DisplayPart (\_ r -> tof r) \\ ViewWith tof <- options]

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | descr d & iTask r & iTask m
updateInformationWithShared d options shared init
	= interact d (parts options) init (toReadOnly shared) @ fst
where
	parts [UpdateWith tof fromf]
		= [FormPart
			(\m r -> FilledForm (tof (r,m)))
			(\m r _ _ -> (m, Just (FilledForm (tof (r,m)))))
			(\m r mbv -> (maybe m (fromf (r,m)) mbv, Nothing))
			]
	parts _	
		= [DisplayPart (\_ r -> r)
		  ,FormPart
		  	(\l _ -> FilledForm l)
		  	(\l _ _ _ -> (l,Nothing))
		  	(\l _ mbv -> (fromMaybe l mbv,Nothing))
		  	]

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
	=	viewSharedInformation desc [ViewWith (const Void)] shared
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
	@>> interact "Choose an action" [] Void null
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
multiChoiceToUpdate [MultiChooseWith type view] = [UpdateWith (toView type) fromView]
where
	toView type (container,sel)	= selectOptions sel (initChoice type container)

	initChoice AutoMultiChoice container		= initChoice (suggestedMultiChoiceType container) container
	initChoice ChooseFromCheckBoxes container	= CheckMultiChoice [(view o,o) \\ o <- toOptionList container] []
	
	fromView (container,_) choice = (container,getSelections choice)
	
multiChoiceToUpdate _ = multiChoiceToUpdate [MultiChooseWith AutoMultiChoice id]

sharedMultiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption (container o, [o]) [o]] | OptionContainer container & iTask o
sharedMultiChoiceToUpdate options = case multiChoiceToUpdate options of
	[UpdateWith fromf tof]	= [UpdateWith fromf (\m v -> snd (tof m v))]
	_						= []
