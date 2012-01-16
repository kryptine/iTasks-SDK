implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Tuple import appSnd
from List import isMemberGen, instance Functor []
from Shared import makeReadOnlyShared, :: SharedId
from SharedCombinators import toReadOnlyShared
from Time import :: Timestamp(..)
import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, CoreCombinators, CommonCombinators, LayoutCombinators, SystemData

enterInformation :: !d ![LocalViewOn m] -> Task m | descr d & iTask m
enterInformation d views
	=	modifyInformation d (\_ _ -> defaultValue) filteredViews  voidNull Nothing @ fst
where
	filteredViews	= filterViews filterInputViews defaultViews views
	defaultViews	= [EnterView (\l _ _ -> l)]
	
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m
updateInformation d views m
	=	modifyInformation d (\_ _ -> m) filteredViews voidNull Nothing @ fst
where
	filteredViews	= filterViews noFilter defaultViews views
	defaultViews	= [UpdateView (GetLocal id) (\l _ _ -> l)]
	
viewInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m
viewInformation d views m
	=	modifyInformation d (\(Just m) _ -> m) filteredViews  voidNull (Just m) @ fst
where
	filteredViews	= filterViews filterOutputViews defaultViews views
	defaultViews	= [DisplayView (GetLocal id)]
		
updateSharedInformation :: !d ![ViewOn w r] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d views shared
	=	(modifyInformation d initLocal filteredViews (toReadOnlyShared shared) Nothing @ fst) @> (\mbw _ -> mbw, shared)
where
	filteredViews						= filterViews noFilter defaultViews views
	defaultViews						= [defaultView]
	
	//Trick: use dynamics to test if r == w
	(initLocal,defaultView) = case dynamic id :: A.a: (a -> a) of	
		(rtow :: (r^ -> w^))			= (\_ r -> (rtow r), UpdateView	(GetShared id) (\r _ _ -> rtow r))
		_								= (\_ _ -> defaultValue, EnterView 	(\w _ _ -> w))
	
viewSharedInformation :: !d ![SharedViewOn r] !(ReadWriteShared r w) -> Task r | descr d & iTask r
viewSharedInformation d views shared
	=	modifyInformation d (\_ _ -> Void) filteredViews (toReadOnlyShared shared) (Just Void) @ snd
where
	filteredViews	= filterViews filterOutputViews defaultViews views
	defaultViews	= [DisplayView (GetShared id)]
	

filterViews filterF defaultViews views = addDefault (catMaybes (map filterF views))
where	
	addDefault views
		| any (\v -> case v of (About _) = False; _ = True) views	= views
		| otherwise													= views ++ defaultViews
		
filterInputViews view = case view of
	About a						= Just (About a)
	EnterView e					= Just (EnterView e)
	UpdateView _ e				= Just (EnterView e)
	DisplayView (GetShared get)	= Just (DisplayView (GetShared get))
	_							= Nothing
		
filterOutputViews view = case view of
	About a				= Just (About a)
	DisplayView s		= Just (DisplayView s)
	UpdateView s _		= Just (DisplayView s)
	_					= Nothing
		
noFilter = Just

enterChoice :: !d ![ChoiceView ChoiceType o] !(container o) -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
enterChoice d views choiceOpts
	=	modifyChoice d views (constShared choiceOpts) Nothing

updateChoice :: !d ![ChoiceView ChoiceType o] !(container o) o -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
updateChoice d views choiceOpts initC
	=	modifyChoice d views (constShared choiceOpts) (Just initC)
	
enterSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedChoice d views shared
	=	modifyChoice d views shared Nothing

updateSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) o -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedChoice d views shared initC
	=	modifyChoice d views shared (Just initC)

modifyChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) (Maybe o) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
modifyChoice d views shared mbInitSel = 
	transform justValid (modifyInformation d  (\_ _ -> mbInitSel) (toChoiceViews (addDefault views)) (toReadOnlyShared shared) Nothing)
where
	toChoiceViews views = map toChoiceView views
	where
		toChoiceView view = case view of
			ChoiceContext v			= About v
			ChoiceView (type,viewF)	= choiceView type viewF
		
		choiceView type viewF = case type of
			//TEMPORARY TRICK. WE WOULD LIKE TO USE THE ACTUAL OPTIONS INSTEAD OF DEFAULT VALUE
			//NOT POSSIBLE ATM
			AutoChoiceView				= choiceView (determineAutoChoice shared defaultValue) viewF
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
	
	addDefault views
		| any (\v -> case v of (ChoiceContext _) = False; _ = True) views	= views
		| otherwise															= views ++ [ChoiceView (AutoChoiceView, id)]

	justValid (Just (Just sel,_))	= Just sel
	justValid _						= Nothing

enterMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
enterMultipleChoice d views choiceOpts
	=	modifyMultipleChoice d views (constShared choiceOpts) []

updateMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
updateMultipleChoice d views choiceOpts initC
	=	modifyMultipleChoice d views (constShared choiceOpts) initC

enterSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedMultipleChoice d views shared
	=	modifyMultipleChoice d views shared []

updateSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedMultipleChoice d views shared sel 
	=	modifyMultipleChoice d views shared sel

modifyMultipleChoice d views shared initSels =
	(modifyInformation d (\_ _ -> initSels) (toChoiceViews (addDefault views))  (toReadOnlyShared shared) Nothing @ fst)
where
	toChoiceViews views = map toChoiceView views
	where
		toChoiceView view = case view of
			ChoiceContext v			= About v
			ChoiceView (type,viewF)	= choiceView type viewF
		
		choiceView type viewF = case type of
			AutoMultiChoiceView			= choiceView (determineAutoChoice shared defaultValue) viewF
			ChooseFromCheckBoxes		= choiceView` mkCheckMultiChoice
		where
			choiceView` mkF				= UpdateView (GetCombined (toChoice mkF viewF)) fromChoice
			
			determineAutoChoice :: (ReadWriteShared (container o) w) (container o) -> MultiChoiceType | OptionContainer container & iTask o & iTask (container o)
			determineAutoChoice _ options = suggestedMultiChoiceType options

	choiceView mkF viewF				= UpdateView (GetCombined (toChoice mkF viewF), fromChoice)
	toChoice mkF viewF mbSel options	= mkF (fmap (\o -> (viewF o, o)) options) mbSel
	fromChoice choice _ _				= getSelections choice
	
	addDefault views
		| any (\v -> case v of (ChoiceContext _) = False; _ = True) views	= views
		| otherwise															= views ++ [ChoiceView (AutoMultiChoiceView, id)]

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
	part (UpdateView getfun setfun)		= FormPart (filledInit getfun) (whenCleanShareUpdate getfun) (whenValidViewUpdate setfun)
	part (DisplayView getfun)			= DisplayPart (viewVal getfun)

	blankInit :: FormInitFun l r v
	blankInit = \l r -> BlankForm
	
	filledInit :: (GetFun l r v) -> FormInitFun l r v
	filledInit getfun = \l r -> FilledForm (viewVal getfun l r) 
	
	//Ignore changes in the share completely
	ignoreShareUpdate :: FormShareUpdateFun l r v
	ignoreShareUpdate = \mbl r mbv dirty -> (mbl,Nothing) //Don't change the result, don't change the view
	
	//Refresh the view if it hasn't been touched by the user yet
	whenCleanShareUpdate :: (GetFun l r v) -> FormShareUpdateFun l r v
	whenCleanShareUpdate getfun = \l r mbv dirty ->(l, if dirty Nothing (Just (FilledForm (viewVal getfun l r))))
	
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
		
wait :: d (r -> Bool) (ReadWriteShared r w) -> Task r | descr d & iTask r
wait desc pred shared
	=	viewSharedInformation desc [DisplayView (GetLocal id)] shared
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

constShared a = makeReadOnlyShared ("const_" +++ toString (toJSON a)) (\world -> (a,world)) (\world -> (0,world))
