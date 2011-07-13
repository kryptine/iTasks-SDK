implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Util import appSnd, isMemberGen, instance Functor []
from Shared import makeReadOnlyShared, :: SharedId
from Time import :: Timestamp(..)
import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData, CommonCombinators

enterInformation :: !d ![LocalViewOn m] -> Task m | descr d & iTask m
enterInformation d options` = InputTask @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull defaultValue)
where
	options _	= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [EnterView (PutbackLocal (\l _ _ -> l))]
	
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m
updateInformation d options` m = UpdateTask @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull m)
where
	options _	= filterOptions noFilter defaultOpts options`
	defaultOpts	= [UpdateView (GetLocal id, PutbackLocal (\l _ _ -> l))]
	
showInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m
showInformation d options` m = OutputTask PassiveOutput @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull m)
where
	options	 _	= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [ShowView (GetLocal id)]
	
mapToLocalState = mapActionTaskModelValue (\(_,l) -> l)

enterSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) -> Task (r,l) | descr d & iTask l & iTask r & iTask w
enterSharedInformation d options` shared = InputTask @>> updateSharedInformation` d options shared defaultValue
where
	options _	= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [EnterView (PutbackLocal (\l _ _ -> l)), EnterView (PutbackShared (\w _ _ -> w))]
	
updateSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
updateSharedInformation d options` shared initLocal = UpdateTask @>> updateSharedInformation` d options shared initLocal
where
	options _							= filterOptions noFilter defaultOpts options`
	defaultOpts							= [defaultLocalView,defaultSharedView]
	defaultLocalView					= UpdateView (GetLocal id, PutbackLocal (\l _ _ -> l))
	defaultSharedView = case dynamic id :: A.a: (a -> a) of
		(putback :: (r^ -> w^))			= UpdateView	(GetShared id,PutbackShared (\r _ _ -> (putback r)))
		_								= ShowView		(GetShared id)

showSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) !l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
showSharedInformation d options` shared local = OutputTask PassiveOutput @>> updateSharedInformation` d options shared local
where
	options _	= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [ShowView (GetLocal id), ShowView (GetShared id)]
	
filterOptions filterF defaultOpts options = addDefault (catMaybes (map filterF options))
where	
	addDefault options
		| any (\o -> case o of (About _) = False; _ = True) options	= options
		| otherwise													= options ++ defaultOpts
		
filterInputOptions option = case option of
		About a						= Just (About a)
		EnterView e					= Just (EnterView e)
		UpdateView (_,e)			= Just (EnterView e)
		ShowView (GetShared get)	= Just (ShowView (GetShared get))
		_							= Nothing
		
filterOutputOptions option = case option of
		About a				= Just (About a)
		ShowView s			= Just (ShowView s)
		UpdateView (s,_)	= Just (ShowView s)
		_					= Nothing
		
noFilter = Just

enterChoice :: !d ![ChoiceView ChoiceType o] !(container o) -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
enterChoice d views choiceOpts = InputTask @>> LocalInteractionTask @>>
	choice` d views (constShared choiceOpts) Nothing

updateChoice :: !d ![ChoiceView ChoiceType o] !(container o) o -> Task o | descr d & OptionContainer container & iTask o & iTask (container o)
updateChoice d views choiceOpts initC = UpdateTask @>> LocalInteractionTask @>>
	choice` d views (constShared choiceOpts) (Just initC)
	
enterSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedChoice d views shared = InputTask @>> choice` d views shared Nothing

updateSharedChoice :: !d ![ChoiceView ChoiceType o] !(ReadWriteShared (container o) w) o -> Task o | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedChoice d views shared initC = UpdateTask @>> choice` d views shared (Just initC)

choice` d views shared mbInitSel = mapActionTask transF (updateSharedInformation` d (toChoiceViews (addDefault views)) shared Nothing)
where
	transF {modelValue=v=:(options,mbSelection)}
		= let mbSel = maybe mbInitSel Just mbSelection in {modelValue = fromMaybe defaultValue mbSel, localValid = isJust mbSel && isMemberGen (fromJust mbSel) (toOptionList options)}
	
	toChoiceViews views options = map toChoiceView views
	where
		toChoiceView view = case view of
			ChoiceContext v			= About v
			ChoiceView (type,viewF)	= choiceView type viewF
		
		choiceView type viewF = case type of
			AutoChoiceView				= choiceView (suggestedChoiceType options) viewF
			ChooseFromRadioButtons		= choiceView` mkRadioChoice
			ChooseFromComboBox			= choiceView` mkComboChoice
			//ChooseFromTable
			ChooseFromTree				= choiceView` mkTreeChoice
		where
			choiceView` mkF				= UpdateView (GetLocalAndShared (toChoice mkF viewF), PutbackLocal fromChoice)
			
		toChoice mkF get mbSel options	= mkF (fmap (\o -> (get o, o)) options) (maybe mbInitSel Just mbSel)
		fromChoice choice _ _			= getMbSelection choice
	
	addDefault options
		| any (\o -> case o of (ChoiceContext _) = False; _ = True) options	= options
		| otherwise															= options ++ [ChoiceView (AutoChoiceView, id)]

enterMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
enterMultipleChoice d views choiceOpts = InputTask @>> LocalInteractionTask @>>
	multipleChoice` d views (constShared choiceOpts) []

updateMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(container o) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask (container o)
updateMultipleChoice d views choiceOpts initC = UpdateTask @>> LocalInteractionTask @>>
	multipleChoice` d views (constShared choiceOpts) initC

enterSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
enterSharedMultipleChoice d views shared = InputTask @>> multipleChoice` d views shared []

updateSharedMultipleChoice :: !d ![ChoiceView MultiChoiceType o] !(ReadWriteShared (container o) w) [o] -> Task [o] | descr d & OptionContainer container & iTask o & iTask w & iTask (container o)
updateSharedMultipleChoice d views shared sel = UpdateTask @>> multipleChoice` d views shared sel

multipleChoice` d views shared initSels = mapActionTask transF (updateSharedInformation` d (toChoiceViews (addDefault views)) shared Nothing)
where
	transF {modelValue=v=:(options,mbSelections)} = {modelValue = filter (\sel -> isMemberGen sel (toOptionList options)) (fromMaybe initSels mbSelections), localValid = True}
	
	toChoiceViews views options = map toChoiceView views
	where
		toChoiceView view = case view of
			ChoiceContext v			= About v
			ChoiceView (type,viewF)	= choiceView type viewF
		
		choiceView type viewF = case type of
			AutoMultiChoiceView			= choiceView (suggestedMultiChoiceType options) viewF
			ChooseFromCheckBoxes		= choiceView` mkCheckMultiChoice
		where
			choiceView` mkF				= UpdateView (GetLocalAndShared (toChoice mkF viewF), PutbackLocal fromChoice)
	
	choiceView mkF viewF			= UpdateView (GetLocalAndShared (toChoice mkF viewF), PutbackLocal fromChoice)
	toChoice mkF get mbSel options	= mkF (fmap (\o -> (get o, o)) options) (fromMaybe initSels mbSel)
	fromChoice choice _ _			= Just (getSelections choice)
	
	addDefault options
		| any (\o -> case o of (ChoiceContext _) = False; _ = True) options	= options
		| otherwise															= options ++ [ChoiceView (AutoMultiChoiceView, id)]

updateSharedInformation` :: !d !(r -> [ViewOn l r w]) !(ReadWriteShared r w) l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
updateSharedInformation` d viewF shared initLocal = mapActionTaskModelValue (\((l,_,_),r) -> (r,l)) (interact d interaction (initLocal,False,False) shared)
where
	interaction (l,updateLocalViews,updateSharedViews) r changed = map mkPart (viewF r)
	where
		mkPart part = case part of
			About a							= DisplayPart a
			EnterView putback				= FormPart	(Unchanged Blank)
														(mkPutback putback l r)
			ShowView get = case get of
				GetLocal get				= DisplayPart (get l)
				GetShared get				= DisplayPart (get r)
				GetLocalAndShared get		= DisplayPart (get l r)
			UpdateView (get,putback)
				# updateViewGet = case get of
					GetLocal get			= let form = FormValue (get l) in if updateLocalViews										form (Unchanged form)
					GetShared get			= let form = FormValue (get r) in if (updateSharedViews || changed)							form (Unchanged form)
					GetLocalAndShared get	= let form = FormValue (get l r) in if (updateLocalViews || updateSharedViews || changed)	form (Unchanged form)
				= FormPart
					updateViewGet
					(mkPutback putback l r)
									
		mkPutback putback l r mbV = case mbV of
			Nothing = ((l,False,False),Nothing)
			Just v = case putback of
				Putback putback
					# (mbL,mbW)			= putback v l r
					# updateSharedViews	= isJust mbW
					= (maybe (l,False,updateSharedViews) (\l -> (l,True,updateSharedViews)) mbL,mbW)
				PutbackLocal putback	= ((putback v l r, True, False), Nothing)
				PutbackShared putback	= ((l, False, True), Just (putback v l r))

waitForTime :: !Time -> Task Time
waitForTime time =
	(showSharedInformation ("Wait for time", ("Wait until " +++ toString time)) [] currentTime Void >? \(now,_) -> time < now) >>= transform fst

waitForDate :: !Date -> Task Date
waitForDate date =
	(showSharedInformation ("Wait for date", ("Wait until " +++ toString date)) [] currentDate Void >? \(now,_) -> date < now) >>= transform fst

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>= \now -> waitForTime (now + time)

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions = interactLocal chooseActionDescr (const []) Void >>+ \_ -> UserActions (map (appSnd Just) actions)

chooseActionDyn :: !(r -> InteractionTerminators a) !(ReadWriteShared r w) -> Task a | iTask a & iTask r & iTask w
chooseActionDyn termF shared = interact chooseActionDescr (\_ _ _ -> []) Void shared >>+ \{modelValue=v=:(_,r)} -> termF r

chooseActionDescr = "Choose an action"

interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l
interactLocal d partFunc l = LocalInteractionTask @>> mapActionTask (\st=:{modelValue=v=:(l,_)} -> {st & modelValue = l}) (interact d interaction l voidNull)
where
	interaction l _ _ = map toSharedRes (partFunc l)

	toSharedRes (FormPart formView putback)	= FormPart formView (\mbV -> (putback mbV,Nothing))
	toSharedRes (UpdatePart label l)				= UpdatePart label (l,Nothing)
	toSharedRes (DisplayPart v)						= DisplayPart v
	
voidNull :: Shared Void
voidNull = null

constShared a = makeReadOnlyShared ("const_" +++ toString (toJSON a)) (\world -> (a,world)) (\world -> (Timestamp 0,world))