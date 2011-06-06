implementation module InteractionTasks

from StdFunc import id, const, o
from SystemData import null
from Util import voidNothing, appSnd, tuple, getItems, isMemberGen
import StdBool, StdList, StdMisc
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData, CommonCombinators

enterInformation :: !d ![LocalInteractionOption m] -> Task m | descr d & iTask m
enterInformation d options = InputTask @>> interactLocalAction d interaction Nothing toInfoSt
where
	interaction mbV						= mkParts False (toInfoSt mbV) mkPutback (filterOptions filterInputOptions (Putback const) options)
	toInfoSt mbV						= {modelValue = fromMaybe defaultValue mbV, localValid = isJust mbV}
	mkPutback putback modelValue mbV	= fmap (\v -> putback v modelValue) mbV
	
updateInformation :: !d ![LocalInteractionOption m] m -> Task m | descr d & iTask m
updateInformation d options initM = UpdateTask @>> interactLocalAction d interaction (verifyValue initM,initM) toInfoSt
where
	interaction st=:(valid,_)			= mkParts valid (toInfoSt st) mkPutback (filterOptions noFilter (View (id,const)) options)
	toInfoSt (valid,m)					= {localValid = valid, modelValue = m}
	mkPutback putback modelValue mbV	= (isJust mbV,maybe modelValue (\v -> putback v modelValue) mbV)
	
showInformation :: !d ![LocalInteractionOption m] !m -> Task m | descr d & iTask m
showInformation d options m = OutputTask PassiveOutput @>> interactLocalAction d interaction m toInfoSt
where
	interaction m	= mkParts False (toInfoSt m) undef (filterOptions filterOutputOptions (Get id) options)
	toInfoSt _		= {localValid = True, modelValue = m}

enterSharedInformation :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
enterSharedInformation d options shared = InputTask @>> interactAction d interaction False shared toInfoSt
where
	interaction valid r changed			= mkParts changed (toInfoSt valid r) mkPutback (filterOptions filterInputOptions (Putback const) options)
	toInfoSt valid r					= {localValid = valid, modelValue = r}
	mkPutback putback modelValue mbV	= (isJust mbV,fmap (\v -> putback v modelValue) mbV)
	
updateSharedInformation :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
updateSharedInformation d options shared = UpdateTask @>> interactAction d interaction True shared toInfoSt
where
	interaction valid r changed			= mkParts changed (toInfoSt valid r) mkPutback (filterOptions noFilter defaultOpt options)
	toInfoSt valid r					= {localValid = valid, modelValue = r}
	mkPutback putback modelValue mbV	= (isJust mbV,fmap (\v -> putback v modelValue) mbV)
	defaultOpt = Get id

monitor :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
monitor d options shared = OutputTask PassiveOutput @>> interactAction d interaction Void shared toInfoSt
where
	interaction _ r changed	= mkParts False (toInfoSt Void r) undef (filterOptions filterOutputOptions (Get id) options)
	toInfoSt _ r			= {localValid = True, modelValue = r}

enterChoice :: !d ![LocalInteractionOption o] ![o] -> Task o | descr d & iTask o
enterChoice d options choiceOpts = localChoice InputTask d options choiceOpts Nothing

updateChoice :: !d ![LocalInteractionOption o] ![o] o -> Task o | descr d & iTask o
updateChoice d options choiceOpts initC = localChoice UpdateTask d options choiceOpts (Just initC)

localChoice type d options choiceOpts mbSel = type @>> interactLocalAction d interaction (fmap fromViewOption (getMbChoice initChoice)) toInfoSt
where
	interaction _		= addAbouts options [UpdateView (Unchanged (FormValue initChoice)) (fmap (fromViewOption o getChoice))]
	toInfoSt mbChoice	= {modelValue = fromMaybe defaultValue mbChoice, localValid = isJust mbChoice}

	initChoice = maybe (choice viewOptions) (choiceSel viewOptions) (fmap toViewOption mbSel)
	viewOptions = map toViewOption choiceOpts
	toViewOption o = (o,Hidden o)
	fromViewOption (_,Hidden o) = o

enterSharedChoice :: !d ![InteractionOption o w] !(Shared [o] w) -> Task o | descr d & iTask o & iTask w
enterSharedChoice d options shared = sharedChoice InputTask d options shared Nothing

updateSharedChoice :: !d ![InteractionOption o w] !(Shared [o] w) o -> Task o | descr d & iTask o & iTask w
updateSharedChoice d options shared initC = sharedChoice UpdateTask d options shared (Just initC)

sharedChoice type d options shared mbSel = type @>> interactAction d interaction Nothing shared toInfoSt
where
	interaction mbLocal choiceOpts changed = addAbouts options [UpdateView toView fromView]
	where
		toView
			| changed	= FormValue (maybe (initChoice choiceOpts) (setOptions (viewOptions choiceOpts)) mbLocal)
			| otherwise	= Unchanged (FormValue (initChoice choiceOpts))
		fromView c	= (c, Nothing)
	
	toInfoSt mbLocal _ = {modelValue = maybe defaultValue (\c -> maybe defaultValue fromViewOption (getMbChoice c)) mbLocal, localValid = isJust mbLocal}
	
	initChoice choiceOpts = maybe (choice (viewOptions choiceOpts)) (\sel -> choiceSel (viewOptions choiceOpts) (toViewOption sel)) mbSel
	viewOptions choiceOpts = map toViewOption choiceOpts
	toViewOption o = (o,Hidden o)
	fromViewOption (_,Hidden o) = o

enterMultipleChoice :: !d ![LocalInteractionOption o] ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options choiceOpts = localMultipleChoice InputTask d options choiceOpts []

updateMultipleChoice :: !d ![LocalInteractionOption o] ![o] [o] -> Task [o] | descr d & iTask o
updateMultipleChoice d options shared initC = localMultipleChoice UpdateTask d options shared initC

localMultipleChoice type d options choiceOpts sel = type @>> interactLocalAction d interaction (fromViewOptions (getChoices initMultipleChoice)) toInfoSt
where
	interaction _		= addAbouts options [UpdateView (Unchanged (FormValue initMultipleChoice)) (\mbC -> maybe [] (fromViewOptions o getChoices) mbC)]
	toInfoSt choices	= {modelValue = choices, localValid = True}

	initMultipleChoice = multipleChoiceSel viewOptions (toViewOptions sel)
	viewOptions = toViewOptions choiceOpts
	toViewOptions l = map (\o -> (o,Hidden o)) l
	fromViewOptions l = map (\(_,Hidden o) -> o) l

enterSharedMultipleChoice :: !d ![InteractionOption o w] !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d options shared = sharedMultipleChoice InputTask d options shared []

updateSharedMultipleChoice :: !d ![InteractionOption o w] !(Shared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel = sharedMultipleChoice UpdateTask d options shared sel

sharedMultipleChoice type d options shared sel = type @>> interactAction d interaction Nothing shared toInfoSt
where
	interaction mbLocal choiceOpts changed = addAbouts options [UpdateView toView fromView]
	where
		toView
			| changed	= FormValue (maybe (initMultipleChoice choiceOpts) (setOptionsM (toViewOptions choiceOpts)) mbLocal)
			| otherwise	= Unchanged (FormValue (initMultipleChoice choiceOpts))
		fromView mc	= (mc, Nothing)
	
	toInfoSt mbLocal choiceOpts = {modelValue = fromViewOptions (maybe [] getChoices mbLocal), localValid = True}
	
	initMultipleChoice choiceOpts = multipleChoiceSel (toViewOptions choiceOpts) (toViewOptions sel)
	toViewOptions l = map (\o -> (o,Hidden o)) l
	fromViewOptions l = map (\(_,Hidden o) -> o) l

addAbouts options parts = [DisplayView a \\ About a <- options] ++ parts

filterOptions filterF defaultOpt options = addDefault (catMaybes (map filterF options))
where	
	addDefault options
		| any (\o -> case o of (About _) = False; _ = True) options	= options
		| otherwise													= options ++ [defaultOpt]
		
filterInputOptions option = case option of
		(View (_,putback))	= Just (Putback putback)
		(Get _)				= Nothing
		o					= Just o
		
filterOutputOptions option = case option of
		(View (get,_))		= Just (Get get)
		(Putback _)			= Nothing
		o					= Just o
		
noFilter = Just

mkParts update st mkPutback options = map (mkPart st mkPutback) options
where
	mkPart :: !(InformationState a) (A.b:(b -> a -> c) a (Maybe b) -> d) !(InteractionOption a c) -> (InteractionPart d)
	mkPart {modelValue,localValid} mkPutback part = case part of
		(About a)				= DisplayView a
		(Get get)				= DisplayView (get modelValue)
		(Putback putback)		= UpdateView
									(Unchanged Blank)
									(mkPutback putback modelValue)
		(View (get,putback))	= UpdateView
									(if update (FormValue (get modelValue)) (Unchanged (FormValue (get modelValue))))
									(mkPutback putback modelValue)

waitForTime :: !Time -> Task Time
waitForTime time =
	monitor ("Wait for time", ("Wait until " +++ toString time)) [] currentTime >? \now -> time < now

waitForDate :: !Date -> Task Date
waitForDate date =
	monitor ("Wait for date", ("Wait until " +++ toString date)) [] currentDate >? \now -> date < now

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>= \now -> waitForTime (now + time)

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions = interactLocal chooseActionDescr (const []) Void >>+ \_ -> UserActions (map (appSnd Just) actions)

chooseActionDyn :: !(Shared r w) !(r -> [(!Action,Maybe a)]) -> Task a | iTask a & iTask r & iTask w
chooseActionDyn shared actionsF = interact chooseActionDescr (\_ _ _ -> []) Void shared >>+ \{modelValue=v=:(_,r)} -> UserActions (actionsF r)

chooseActionDescr = "Choose an action"

interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l
interactLocal d partFunc l = LocalInteractionTask @>> mapActionTask (\st=:{modelValue=v=:(l,_)} -> {st & modelValue = l}) (interact d interaction l nullShared`)
where
	interaction l _ _ = map toSharedRes (partFunc l)

	toSharedRes (UpdateView formView putback)	= UpdateView formView (\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = null

interactLocalAction :: !d !(l -> [InteractionPart l]) l !(l -> InformationState a) -> Task a | descr d & iTask l & iTask a
interactLocalAction d partFunc l toInfoSt = mapActionTask (\{modelValue} -> toInfoSt modelValue) (interactLocal d partFunc l)
	
interactAction :: !d !(l r Bool -> [InteractionPart (!l,!Maybe w)]) l !(Shared r w) !(l r -> InformationState a) -> Task a | descr d & iTask l & iTask r & iTask w & iTask a
interactAction d partFunc l shared toInfoSt = mapActionTask (\{modelValue=v=:(l,r)} -> toInfoSt l r) (interact d partFunc l shared)