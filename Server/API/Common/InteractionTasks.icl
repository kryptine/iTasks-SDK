implementation module InteractionTasks

from StdFunc import id, const, o
from SystemData import null
from Util import voidNothing, appSnd, tuple, getItems, isMemberGen
import StdBool, StdList, StdMisc
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData, CommonCombinators

enterInformation :: !d ![LocalViewOn m] -> Task m | descr d & iTask m
enterInformation d options = InputTask @>> interactLocal` d interaction Nothing toInfoSt
where
	interaction mbV						= mkParts False (toInfoSt mbV) mkPutback (filterOptions filterInputOptions (Putback const) options)
	toInfoSt mbV						= {modelValue = fromMaybe defaultValue mbV, localValid = isJust mbV}
	mkPutback putback modelValue mbV	= fmap (\v -> putback v modelValue) mbV
	
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m
updateInformation d options initM = UpdateTask @>> interactLocal` d interaction (verifyValue initM,initM) toInfoSt
where
	interaction st=:(valid,_)			= mkParts valid (toInfoSt st) mkPutback (filterOptions noFilter (View (id,const)) options)
	toInfoSt (valid,m)					= {localValid = valid, modelValue = m}
	mkPutback putback modelValue mbV	= (isJust mbV,maybe modelValue (\v -> putback v modelValue) mbV)
	
showInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m
showInformation d options m = OutputTask PassiveOutput @>> interactLocal` d interaction m toInfoSt
where
	interaction m						= mkParts False (toInfoSt m) undef (filterOptions filterOutputOptions (Get id) options)
	toInfoSt _							= {localValid = True, modelValue = m}

enterSharedInformation :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
enterSharedInformation d options shared = InputTask @>> interact` d updL interaction False shared toInfoSt
where
	updL l r c							= l
	interaction valid r changed			= mkParts changed (toInfoSt valid r) mkPutback (filterOptions filterInputOptions (Putback const) options)
	toInfoSt valid r					= {localValid = valid, modelValue = r}
	mkPutback putback modelValue mbV	= (isJust mbV,fmap (\v -> putback v modelValue) mbV)
	
updateSharedInformation :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
updateSharedInformation d options` shared = UpdateTask @>> interact` d updL interaction True shared toInfoSt
where
	options								= filterOptions noFilter defaultOpt options`
	updL valid r changed				= if changed (verifyValue r) valid
	interaction valid r changed			= mkParts changed (toInfoSt valid r) mkPutback options
	toInfoSt valid r					= {localValid = valid, modelValue = r}
	mkPutback putback modelValue mbV	= (isJust mbV,fmap (\v -> putback v modelValue) mbV)
	defaultOpt = Get id

monitor :: !d ![ViewOn r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w
monitor d options shared = OutputTask PassiveOutput @>> interact` d updL interaction Void shared toInfoSt
where
	updL _ _ _							= Void
	interaction _ r changed				= mkParts False (toInfoSt Void r) undef (filterOptions filterOutputOptions (Get id) options)
	toInfoSt _ r						= {localValid = True, modelValue = r}

enterChoice :: !d ![LocalViewOn o] ![o] -> Task o | descr d & iTask o
enterChoice d options choiceOpts = localChoice InputTask d options choiceOpts Nothing

updateChoice :: !d ![LocalViewOn o] ![o] o -> Task o | descr d & iTask o
updateChoice d options choiceOpts initC = localChoice UpdateTask d options choiceOpts (Just initC)

localChoice type d options choiceOpts mbSel = type @>> interactLocal` d interaction (fmap fromViewOption (getMbChoice initChoice)) toInfoSt
where
	interaction _		= addAbouts options [UpdateView (Unchanged (FormValue initChoice)) (fmap (fromViewOption o getChoice))]
	toInfoSt mbChoice	= {modelValue = fromMaybe defaultValue mbChoice, localValid = isJust mbChoice}

	initChoice = maybe (choice viewOptions) (choiceSel viewOptions) (fmap toViewOption mbSel)
	viewOptions = map toViewOption choiceOpts
	toViewOption o = (o,Hidden o)
	fromViewOption (_,Hidden o) = o

enterSharedChoice :: !d ![ViewOn o w] !(Shared [o] w) -> Task o | descr d & iTask o & iTask w
enterSharedChoice d options shared = sharedChoice InputTask d options shared Nothing

updateSharedChoice :: !d ![ViewOn o w] !(Shared [o] w) o -> Task o | descr d & iTask o & iTask w
updateSharedChoice d options shared initC = sharedChoice UpdateTask d options shared (Just initC)

sharedChoice type d options shared mbSel = type @>> interact` d updL interaction Nothing shared toInfoSt
where
	updL mbLocal choiceOpts changed
		| changed	= fmap (setOptions (viewOptions choiceOpts)) mbLocal
		| otherwise	= mbLocal
	interaction mbLocal choiceOpts changed = addAbouts options [UpdateView toView fromView]
	where
		toView = FormValue (fromMaybe (initChoice choiceOpts) mbLocal)
		fromView c	= (c, Nothing)
	
	toInfoSt mbLocal _ = {modelValue = maybe defaultValue (\c -> maybe defaultValue fromViewOption (getMbChoice c)) mbLocal, localValid = maybe False (\c -> isJust (getMbChoice c)) mbLocal}
	
	initChoice choiceOpts = maybe (choice (viewOptions choiceOpts)) (\sel -> choiceSel (viewOptions choiceOpts) (toViewOption sel)) mbSel
	viewOptions choiceOpts = map toViewOption choiceOpts
	toViewOption o = (o,Hidden o)
	fromViewOption (_,Hidden o) = o

enterMultipleChoice :: !d ![LocalViewOn o] ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options choiceOpts = localMultipleChoice InputTask d options choiceOpts []

updateMultipleChoice :: !d ![LocalViewOn o] ![o] [o] -> Task [o] | descr d & iTask o
updateMultipleChoice d options shared initC = localMultipleChoice UpdateTask d options shared initC

localMultipleChoice type d options choiceOpts sel = type @>> interactLocal` d interaction (fromViewOptions (getChoices initMultipleChoice)) toInfoSt
where
	interaction _		= addAbouts options [UpdateView (Unchanged (FormValue initMultipleChoice)) (\mbC -> maybe [] (fromViewOptions o getChoices) mbC)]
	toInfoSt choices	= {modelValue = choices, localValid = True}

	initMultipleChoice = multipleChoiceSel viewOptions (toViewOptions sel)
	viewOptions = toViewOptions choiceOpts
	toViewOptions l = map (\o -> (o,Hidden o)) l
	fromViewOptions l = map (\(_,Hidden o) -> o) l

enterSharedMultipleChoice :: !d ![ViewOn o w] !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d options shared = sharedMultipleChoice InputTask d options shared []

updateSharedMultipleChoice :: !d ![ViewOn o w] !(Shared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel = sharedMultipleChoice UpdateTask d options shared sel

sharedMultipleChoice type d options shared sel = type @>> interact` d updL interaction Nothing shared toInfoSt
where
	updL mbLocal choiceOpts changed
		| changed	= fmap (setOptionsM (toViewOptions choiceOpts)) mbLocal
		| otherwise	= mbLocal
	interaction mbLocal choiceOpts _ = addAbouts options [UpdateView toView fromView]
	where
		toView = FormValue (fromMaybe (initMultipleChoice choiceOpts) mbLocal)
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
	mkPart :: !(InformationState a) (A.b:(b -> a -> c) a (Maybe b) -> d) !(ViewOn a c) -> (InteractionPart d)
	mkPart {modelValue,localValid} mkPutback part = case part of
		(About a)				= DisplayView a
		(Get get)				= DisplayView (get modelValue)
		(Putback putback)		= UpdateView
									(Unchanged Blank)
									(mkPutback putback modelValue)
		(View (get,putback))	= UpdateView
									(if update (FormValue (get modelValue)) (Unchanged (FormValue (get modelValue))))
									(mkPutback putback modelValue)
									
verifyViews options r = and [verifyValue (get r) \\ View (get,_) <- options]

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
chooseActionDyn shared actionsF = interact chooseActionDescr (\l _ _ -> ([],l)) Void shared >>+ \{modelValue=v=:(_,r)} -> UserActions (actionsF r)

chooseActionDescr = "Choose an action"

interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l
interactLocal d partFunc l = LocalInteractionTask @>> mapActionTask (\st=:{modelValue=v=:(l,_)} -> {st & modelValue = l}) (interact d interaction l nullShared`)
where
	interaction l _ _ = (map toSharedRes (partFunc l),l)

	toSharedRes (UpdateView formView putback)	= UpdateView formView (\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = null

interactLocal` :: !d !(l -> [InteractionPart l]) l !(l -> InformationState a) -> Task a | descr d & iTask l & iTask a
interactLocal` d partFunc l toInfoSt = mapActionTask (\{modelValue} -> toInfoSt modelValue) (interactLocal d partFunc l)
	
interact` :: !d !(l r Bool -> l) !(l r Bool -> [InteractionPart (!l,!Maybe w)]) l !(Shared r w) !(l r -> InformationState a) -> Task a | descr d & iTask l & iTask r & iTask w & iTask a
interact` d updL partFunc l shared toInfoSt = mapActionTask (\{modelValue=v=:(l,r)} -> toInfoSt l r) (interact d partFunc` l shared)
where
	partFunc` l r c
		# l = updL l r c
		= (partFunc l r c,l)