implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Tuple import appSnd
from List import isMemberGen, instance Functor []
from Shared import makeReadOnlyShared
from Time import :: Timestamp(..)
from SharedDataSource import constShare
import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, CoreCombinators, CommonCombinators, LayoutCombinators, SystemData

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
	
newIndexes :: ![a] ![a] ![Int] -> [Int] | gEq{|*|} a
newIndexes oldOpts newOpts sel = newIndexes` curChoices []
where
	newIndexes` [] acc = acc
	newIndexes` [(choice,nrOfOccurrence):choices] acc = case findOption choice nrOfOccurrence of
		Nothing	= newIndexes` choices acc
		Just i	= newIndexes` choices [i:acc]
	where
		findOption choice nr
			| isEmpty choiceIndexes			= Nothing
			| length choiceIndexes <= nr	= Just (choiceIndexes !! (length choiceIndexes - 1))
			| otherwise						= Just (choiceIndexes !! nr)
		where
			choiceIndexes = [i \\ opt <- newOpts & i <- [0..] | choice === opt]
	curChoices = [(choice,nrOfOccurrence choice i oldOpts) \\ choice <- oldOpts & i <- [0..] | isMember i sel]
	where
		nrOfOccurrence choice choiceIndex opts = nrOfOccurrence` 0 0 opts
		where
			nrOfOccurrence` _ nr [] = nr
			nrOfOccurrence` i nr [opt:oldOpts]
				| choice === opt
					| i == choiceIndex	= nr
					| otherwise			= nrOfOccurrence` (inc i) (inc nr) oldOpts
				| otherwise				= nrOfOccurrence` (inc i) nr oldOpts

enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m
enterInformation d opts
	= interact d init (parts opts) Nothing voidNull @ fst
where
	init _ _ = defaultValue
	
	parts [EnterWith fromf]	= [FormPart (\_ _ -> BlankForm) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (maybe l fromf mbv, Nothing))]
	parts _					= [FormPart (\_ _ -> BlankForm) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (fromMaybe l mbv,Nothing))]

updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInformation d opts m
	= interact d init (parts opts) Nothing voidNull @ fst	
where
	init _ _ = m
	
	parts [UpdateWith tof fromf]	= [FormPart (\l _ -> FilledForm (tof l)) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (maybe l (fromf l) mbv, Nothing))]
	parts _							= [FormPart (\l _ -> FilledForm l) (\l _ _ _ -> (l,Nothing)) (\l _ mbv -> (fromMaybe l mbv,Nothing))]
	
viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m
viewInformation d opts m
	= interact d init (parts opts) Nothing voidNull @ fst
where
	init _ _ = m
	
	parts [ViewWith tof]	= [DisplayPart (\l _ -> tof l)]
	parts _					= [DisplayPart (\l _ -> m)]
	
updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d views shared
	= (interact d init (parts views) Nothing (toReadOnly shared) @ fst) @> (mapval, shared)
where
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	init = case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))			= (\_ r -> rtow r)
		_								= (\_ _ -> defaultValue)
	
	parts [UpdateWith tof fromf]
		= [FormPart
			(\w r -> FilledForm (tof r))
			(\w r _ _ -> (w, Just (FilledForm (tof r))))
			(\w r mbv -> (maybe w (fromf r) mbv, Nothing))
			]
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
viewSharedInformation d views shared
	= interact d init (parts views) Nothing (toReadOnly shared) @ snd
where
	init _ _ = Void

	parts []	= [DisplayPart (\_ r -> r)]
	parts views = [DisplayPart (\_ r -> tof r) \\ ViewWith tof <- views]

enterChoice :: !d ![ChoiceOption o] !(container o) -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
enterChoice d views choiceOpts
	=	modifyChoice d views (constShare choiceOpts) Nothing

updateChoice :: !d ![ChoiceOption o] !(container o) o -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
updateChoice d views choiceOpts initC
	=	modifyChoice d views (constShare choiceOpts) (Just initC)
	
enterSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared (container o) w) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedChoice d views shared
	=	modifyChoice d views shared Nothing

updateSharedChoice :: !d ![ChoiceOption o] !(ReadWriteShared (container o) w) o -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedChoice d views shared initC
	=	modifyChoice d views shared (Just initC)

modifyChoice :: !d ![ChoiceOption o] !(ReadWriteShared (container o) w) (Maybe o) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
modifyChoice d views shared mbInitSel = 
	transform result (modifyInformation d  (\_ _ -> mbInitSel) (toChoiceOptions (addDefault views)) (toReadOnly shared) Nothing)
where
	toChoiceOptions views = map toChoiceOption views
	where
		toChoiceOption (ChooseWith type viewF) = case type of
			//TEMPORARY TRICK. WE WOULD LIKE TO USE THE ACTUAL OPTIONS INSTEAD OF DEFAULT VALUE
			//NOT POSSIBLE ATM
			AutoChoice					= toChoiceOption (ChooseWith (determineAutoChoice shared defaultValue) viewF)
			ChooseFromRadioButtons		= choiceView` mkRadioChoice
			ChooseFromComboBox			= choiceView` mkComboChoice
			ChooseFromGrid				= choiceView` mkGridChoice
			ChooseFromTree				= choiceView` mkTreeChoice
		where
			choiceView` mkF				= UpdateView (GetCombined (toChoice mkF viewF)) fromChoice
			
			determineAutoChoice :: (ReadWriteShared (container o) w) (container o) -> ChoiceType | OptionContainer container & iTask o & iTask (container o)
			determineAutoChoice _ options = suggestedChoiceType options
	 
		toChoice mkF viewF mbSel options
			= mkF (fmap (\o -> (viewF o, o)) options) mbSel
		
		fromChoice choice _ _		= getMbSelection choice
	
	addDefault [] = [ChooseWith AutoChoice id]
	addDefault views = views
	
	result (Value (Just sel,_) s)	= Value sel s
	result _						= NoValue
	
mkRadioChoice :: !(container (!v,!o)) !(Maybe o) -> RadioChoice v o | OptionContainer container & gEq{|*|} o
mkRadioChoice options mbSel = mkChoice` (RadioChoice (toOptionList options)) mbSel

mkComboChoice :: !(container (!v,!o)) !(Maybe o) -> ComboChoice v o | OptionContainer container & gEq{|*|} o
mkComboChoice options mbSel = mkChoice` (ComboChoice (toOptionList options)) mbSel

mkTreeChoice :: !(container (!v,!o)) !(Maybe o) -> TreeChoice v o | OptionContainer container & gEq{|*|} o
mkTreeChoice options mbSel = mkChoice` (TreeChoice (toOptionTree options)) mbSel

mkGridChoice :: !(container (!v,!o)) !(Maybe o) -> GridChoice v o | OptionContainer container & gEq{|*|} o
mkGridChoice options mbSel = mkChoice` (GridChoice (toOptionList options)) mbSel

mkChoice` :: !((Maybe Int) -> choice v o) !(Maybe o) -> choice v o | Choice choice & gEq{|*|} o
mkChoice` choice mbSel
	# choice = choice Nothing
	= case mbSel of
		Just sel	= selectOption sel choice
		_			= choice

mkCheckMultiChoice :: !(container (!v,!o)) ![o] -> CheckMultiChoice v o | OptionContainer container & gEq{|*|} o
mkCheckMultiChoice options sels = selectOptions sels (CheckMultiChoice (toOptionList options) [])

enterMultipleChoice :: !d ![MultiChoiceOption o] !(container o) -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
enterMultipleChoice d views choiceOpts
	=	modifyMultipleChoice d views (constShare choiceOpts) []

updateMultipleChoice :: !d ![MultiChoiceOption o] !(container o) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
updateMultipleChoice d views choiceOpts initC
	=	modifyMultipleChoice d views (constShare choiceOpts) initC

enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared (container o) w) -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedMultipleChoice d views shared
	=	modifyMultipleChoice d views shared []

updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared (container o) w) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedMultipleChoice d views shared sel 
	=	modifyMultipleChoice d views shared sel

modifyMultipleChoice d views shared initSels =
	(modifyInformation d (\_ _ -> initSels) (toChoiceOptions (addDefault views))  (toReadOnly shared) Nothing @ fst)
where
	toChoiceOptions views = map toChoiceOption views
	where
		toChoiceOption (MultiChooseWith type viewF )	= choiceView type viewF
			
		choiceView type viewF = case type of
			AutoMultiChoice				= choiceView (determineAutoChoice shared defaultValue) viewF
			ChooseFromCheckBoxes		= choiceView` mkCheckMultiChoice
		where
			choiceView` mkF				= UpdateView (GetCombined (toChoice mkF viewF)) fromChoice
			
			determineAutoChoice :: (ReadWriteShared (container o) w) (container o) -> MultiChoiceType | OptionContainer container & iTask o & iTask (container o)
			determineAutoChoice _ options = suggestedMultiChoiceType options

	choiceView mkF viewF				= UpdateView (GetCombined (toChoice mkF viewF), fromChoice)
	toChoice mkF viewF mbSel options	= mkF (fmap (\o -> (viewF o, o)) options) mbSel
	fromChoice choice _ _				= getSelections choice
	
	addDefault []		= [MultiChooseWith AutoMultiChoice id]
	addDefault views	= views
/**
* Defines a view on the data model of interaction tasks. 
*/
:: ViewOn l r	= E.v:	About			!v								& iTask v	//* additional information independent from the data model the interaction task works on
				//Convenient simple views
				| E.v:	DisplayLocal	!(l -> v)						& iTask v
				| E.v:	EnterLocal		!(v -> l)						& iTask v
				| E.v:	UpdateLocal		!(l -> v) (v l -> l)			& iTask v
				| E.v:	DisplayShared	!(r -> v)						& iTask v
				| E.v:	UpdateShared	!(r -> v) (v l -> l)			& iTask v
				//More fine grained views
				| E.v:	DisplayView		!(GetFun l r v)					& iTask v	//* a view to show the data model
				| E.v:	EnterView						!(SetFun l r v)	& iTask v	//* a view to put information into the data model
				| E.v:	UpdateView		!(GetFun l r v) !(SetFun l r v)	& iTask v	//* a view to update the data model
/**	
* Defines how to get a view from the data model.
*/
:: GetFun l r v	= GetLocal			!(l		-> v)	//* a get function on the local part of the data model
				| GetShared			!(r		-> v)	//* a get function on the shared part of the data model
				| GetCombined		!(l r	-> v)	//* a get function on both parts of the data model

:: SetFun l r v :== v l r -> l						//* a set function that updates the local part of the data model

:: LocalViewOn a	:== ViewOn a Void
:: SharedViewOn a	:== ViewOn Void a

modifyInformation :: !d !((Maybe l) r -> l) ![ViewOn l r] !(ReadOnlyShared r) (Maybe l) -> Task (l,r) | descr d & iTask l & iTask r
modifyInformation d initl views shared mbl = interact d initl [part v \\ v <- views] mbl shared
where
	part (About a)						= DisplayPart (\_ _ -> a)
	part (DisplayLocal getfun)			= DisplayPart (viewLocal getfun)
	part (EnterLocal setfun)			= FormPart blankInit ignoreShareUpdate (whenValidViewUpdate (\v _ _ -> setfun v))
	part (UpdateLocal getfun setfun)	= undef
	part (DisplayShared getfun)			= DisplayPart (viewShared getfun)
	part (UpdateShared getfun setfun)	= undef

	part (EnterView setfun)				= FormPart blankInit ignoreShareUpdate (whenValidViewUpdate setfun)
	part (UpdateView getfun setfun)		= FormPart (filledInit getfun) (onChangedShareUpdate getfun) (whenValidViewUpdate setfun)
	part (DisplayView getfun)			= DisplayPart (viewVal getfun)

	blankInit :: FormInitFun l r v
	blankInit = \l r -> BlankForm
	
	filledInit :: (GetFun l r v) -> FormInitFun l r v
	filledInit getfun = \l r -> FilledForm (viewVal getfun l r) 
	
	//Ignore changes in the share completely
	ignoreShareUpdate :: FormShareUpdateFun l r v
	ignoreShareUpdate = \mbl r mbv dirty -> (mbl,Nothing) //Don't change the result, don't change the view
	
	//Refresh the view if the share has changed
	onChangedShareUpdate :: (GetFun l r v) -> FormShareUpdateFun l r v
	onChangedShareUpdate getfun = \l r mbv _ -> (l, Just (FilledForm (viewVal getfun l r)))
	
	//Refresh the view if it hasn't been touched by the user yet
	whenCleanShareUpdate :: (GetFun l r v) -> FormShareUpdateFun l r v
	whenCleanShareUpdate getfun = \l r mbv dirty -> (l, if dirty Nothing (Just (FilledForm (viewVal getfun l r))))
	
	viewLocal :: (l -> v) l r -> v | iTask l
	viewLocal f l r = f l
	
	viewShared :: (r -> v) l r -> v
	viewShared f l r = f r
	
	viewVal :: (GetFun l r v) l r -> v
	viewVal (GetLocal f) l _ 	= f l
	viewVal (GetShared f) _ r	= f r
	viewVal (GetCombined f) l r	= f l r
	
	whenValidViewUpdate :: (SetFun l r v) -> FormViewUpdateFun l r v
	whenValidViewUpdate f = \l r mbv -> case mbv of
		Just v	= (f v l r, Nothing)
		Nothing	= (l, Nothing)
		
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
	@>> interact "Choose an action" (\_ _ -> Void) [] Nothing voidNull
	>>* [AnyTime action (\_ -> return val) \\ (action,val) <- actions]
	
voidNull :: Shared Void
voidNull = null
