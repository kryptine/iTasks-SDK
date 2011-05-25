implementation module InteractionTasks

from StdFunc import id, const, o
from SystemData import null
from Util import voidNothing, appSnd, tuple, getItems
import StdBool, StdList
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData

instance editorState Maybe a
where
	editorState mbV = mbV
	
instance editorState InformationState a
where
	editorState {modelValue,localValid} = if localValid (Just modelValue) Nothing

interactLocal :: !d !(l -> [InteractionPart l]) !l !(l -> InteractionTerminators a) -> Task a | descr d & iTask l & iTask a
interactLocal d partFunc l termFunc = LocalInteractionTask @>> interact d (\l _ _ -> map toSharedRes (partFunc l)) l nullShared` (\l _ _ -> termFunc l)
where
	toSharedRes (UpdateView (formView,putback))	= UpdateView (formView,\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = null
	
//Local input
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation d = enterInformation` d okActionLocalEnter voidNothing

enterInformationAbout :: !d !about -> Task a | descr d & iTask a & iTask about
enterInformationAbout d about = enterInformation` d okActionLocalEnter (Just about)
	
enterInformationA :: !d !(ActionFunc (Maybe v) a) -> Task a | descr d & iTask a & iTask v
enterInformationA d actions = enterInformation` d actions voidNothing
	
enterInformationAboutA :: !d !about !(ActionFunc (Maybe v) a) -> Task a | descr d  & iTask a & iTask v & iTask about
enterInformationAboutA d about actions = enterInformation` d actions (Just about)

enterInformation` d actions mbAbout = InputTask @>>
	interactLocal d (const (addAbout mbAbout [UpdateView (Unchanged Blank,id)])) Nothing (\mbV -> UserActions (actions mbV))

//Shared input
enterSharedInformation :: !d !(v r -> w) !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w
enterSharedInformation d view shared = enterSharedInformation` d view okActionEditor shared voidNothing

enterSharedInformationAbout :: !d !(v r -> w) !about !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w & iTask about
enterSharedInformationAbout d view about shared = enterSharedInformation` d view okActionEditor shared (Just about)

enterSharedInformationA :: !d !(v r -> w) !(Shared r w) !(ActionFunc (InformationState r)  a) -> Task a | descr d & iTask a & iTask r & iTask v & iTask w
enterSharedInformationA d view shared actions = enterSharedInformation` d view actions shared voidNothing
	
enterSharedInformationAboutA :: !d !(v r -> w) !about !(Shared r w) !(ActionFunc (InformationState r)  a) -> Task a | descr d & iTask a & iTask r & iTask v & iTask w & iTask about
enterSharedInformationAboutA d view about shared actions = enterSharedInformation` d view actions shared (Just about)

enterSharedInformation` d view actions shared mbAbout = InputTask @>>
	interact d (\_ r _ -> addAbout mbAbout [UpdateView (Unchanged Blank,\mbV -> (isJust mbV,fmap (\v -> view v r) mbV))]) False shared (\valid r _ -> UserActions (actions {localValid = valid, modelValue = r}))

//Local choice tasks
enterChoice :: !d ![o] -> Task o | descr d & iTask o
enterChoice _ [] = throw EmptyOptionList
enterChoice d options = localChoice d id okActionLocalEnter options voidNothing Nothing

enterChoiceAbout :: !d !about ![o] -> Task o | descr d & iTask o & iTask about
enterChoiceAbout _ _ [] = throw EmptyOptionList
enterChoiceAbout d about options = localChoice d id okActionLocalEnter options (Just about) Nothing

enterChoiceA :: !d !(o -> v) ![o] !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v
enterChoiceA d view options actions = localChoice d view actions options voidNothing Nothing

enterChoiceAboutA :: !d !(o -> v) !about ![o] !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v & iTask about
enterChoiceAboutA d view about options actions = localChoice d view actions options (Just about) Nothing

//Shared choice tasks
enterSharedChoice :: !d !(Shared [o] w) -> Task o | descr d & iTask o & iTask w
enterSharedChoice d shared = sharedChoice d id okActionLocalEnter shared voidNothing Nothing

enterSharedChoiceAbout :: !d !about !(Shared [o] w) -> Task o | descr d & iTask o & iTask w & iTask about
enterSharedChoiceAbout d about shared = sharedChoice d id okActionLocalEnter shared (Just about) Nothing

enterSharedChoiceA :: !d !(o -> v) !(Shared [o] w) !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask w & iTask v
enterSharedChoiceA d view shared actions = sharedChoice d view actions shared voidNothing Nothing

enterSharedChoiceAboutA :: !d !(o -> v) !about !(Shared [o] w) !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask w & iTask v & iTask about
enterSharedChoiceAboutA d view about shared actions = sharedChoice d view actions shared (Just about) Nothing

//Local multiple choice tasks
enterMultipleChoice :: !d ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options = localMultipleChoice d id okActionMultipleChoice options voidNothing Nothing

enterMultipleChoiceAbout :: !d !about ![o] -> Task [o] | descr d & iTask o & iTask about
enterMultipleChoiceAbout d about options = localMultipleChoice d id okActionMultipleChoice options (Just about) Nothing

enterMultipleChoiceA :: !d !(o -> v) ![o] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v
enterMultipleChoiceA d view options actions = localMultipleChoice d view actions options voidNothing Nothing

enterMultipleChoiceAboutA :: !d !(o -> v) !about ![o] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask about
enterMultipleChoiceAboutA d view about options actions = localMultipleChoice d view actions options (Just about) Nothing

//Shared multiple choice tasks
enterSharedMultipleChoice :: !d !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d shared = sharedMultipleChoice d id okActionMultipleChoice shared voidNothing Nothing

enterSharedMultipleChoiceAbout :: !d !about !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w & iTask about
enterSharedMultipleChoiceAbout d about shared = sharedMultipleChoice d id okActionMultipleChoice shared (Just about) Nothing

enterSharedMultipleChoiceA :: !d !(o -> v) !(Shared [o] w) !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceA d view shared actions = sharedMultipleChoice d view actions shared voidNothing Nothing

enterSharedMultipleChoiceAboutA	:: !d !(o -> v) !about !(Shared [o] w) !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask w & iTask about
enterSharedMultipleChoiceAboutA d view about shared actions = sharedMultipleChoice d view actions shared (Just about) Nothing
		
//Local update
updateInformation :: !d !m -> Task m | descr d & iTask m
updateInformation d a = updateInformation` d idView okActionEditor a voidNothing

updateInformationAbout :: !d !about !m -> Task m | descr d & iTask m & iTask about
updateInformationAbout d about a = updateInformation` d idView okActionEditor a (Just about)

updateInformationA :: !d !(SymmetricView m v) !m !(ActionFunc (InformationState m) a) -> Task a | descr d & iTask a & iTask m & iTask v
updateInformationA d view a actions = updateInformation` d view actions a voidNothing

updateInformationAboutA :: !d !(SymmetricView m v) !about !m !(ActionFunc (InformationState m) a) -> Task a | descr d & iTask a & iTask m & iTask v & iTask about
updateInformationAboutA d view about a actions = updateInformation` d view actions a (Just about)

updateInformation` d (get,putback) actions a mbAbout
	= UpdateTask @>> interactLocal
		d
		(\(valid,a) -> addAbout mbAbout [UpdateView (if valid (FormValue (get a)) (Unchanged (FormValue (get a))),\mbV -> (isJust mbV,maybe a (\v -> putback v a) mbV))])
		(verifyValue a,a)
		(\(valid,m) -> UserActions (actions {localValid = valid, modelValue = m}))

//Shared update
updateSharedInformation :: !d !(View r v w) !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w
updateSharedInformation d view shared = updateSharedInformation` d view okActionEditor shared voidNothing

updateSharedInformationAbout :: !d !(View r v w) !about !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w & iTask about
updateSharedInformationAbout d view about shared = updateSharedInformation` d view okActionEditor shared (Just about)

updateSharedInformationA :: !d !(View r v w) !(Shared r w) !(ActionFunc (InformationState r)  a) -> Task a | descr d & iTask a & iTask r & iTask v & iTask w
updateSharedInformationA d view shared actions = updateSharedInformation` d view actions shared voidNothing

updateSharedInformationAboutA :: !d !(View r v w) !about !(Shared r w) !(ActionFunc (InformationState r)  a) -> Task a | descr d & iTask a & iTask r & iTask v & iTask w & iTask about
updateSharedInformationAboutA d view about shared actions = updateSharedInformation` d view actions shared (Just about)

updateSharedInformation` d (get,putback) actions shared mbAbout
	=  UpdateTask @>> interact
		d
		(\valid r changed -> addAbout mbAbout [UpdateView (if (valid || changed) (FormValue (get r)) (Unchanged (FormValue (get r))),\mbV -> (isJust mbV,fmap (\v -> putback v r) mbV))])
		True
		shared
		(\valid r _ -> UserActions (actions {localValid = valid, modelValue = r}))

updateChoice :: !d ![o] !Int -> Task o | descr d & iTask o
updateChoice _ [] _ = throw EmptyOptionList
updateChoice d options sel = localChoice d id okActionLocalEnter options voidNothing (Just sel)

updateChoiceAbout :: !d !about ![o] !Int -> Task o | descr d & iTask o & iTask about
updateChoiceAbout _ _ [] _ = throw EmptyOptionList
updateChoiceAbout d about options sel = localChoice d id okActionLocalEnter options (Just about) (Just sel)

updateChoiceA :: !d !(o -> v) ![o] !Int !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v
updateChoiceA d view options sel actions = localChoice d view actions options voidNothing (Just sel)

updateChoiceAboutA :: !d !(o -> v) !about ![o] !Int !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v & iTask about
updateChoiceAboutA d view about options sel actions = localChoice d view actions options (Just about) (Just sel)

localChoice d view actions options mbAbout mbSel	= InputTask @>>
	interactLocal d (\_ -> addAbout mbAbout [UpdateView (choiceFormView (map view options) mbSel,fmap getChoiceIndex)]) Nothing (\mbIdx -> UserActions (actions (fmap ((!!) options) mbIdx)))
where
	choiceFormView options mbSel = Unchanged (FormValue (maybe (choice options) (choiceSel options) mbSel))

updateSharedChoice :: !d !(Shared [o] w) !Int -> Task o | descr d & iTask o & iTask w
updateSharedChoice d shared sel = sharedChoice d id okActionLocalEnter shared voidNothing (Just sel)

updateSharedChoiceAbout :: !d !about !(Shared [o] w) !Int -> Task o | descr d & iTask o & iTask w & iTask about
updateSharedChoiceAbout d about shared sel = sharedChoice d id okActionLocalEnter shared (Just about) (Just sel)

updateSharedChoiceA :: !d !(o -> v) !(Shared [o] w) !Int !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v & iTask w
updateSharedChoiceA d view shared sel actions = sharedChoice d view actions shared voidNothing (Just sel)

updateSharedChoiceAboutA	:: !d !(o -> v) !about !(Shared [o] w) !Int !(ActionFunc (Maybe o)  a) -> Task a | descr d & iTask a & iTask v & iTask w & iTask about
updateSharedChoiceAboutA d view about shared sel actions = sharedChoice d view actions shared (Just about) (Just sel)

sharedChoice description view actions shared mbAbout mbSel = interact description interaction Nothing shared termination
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (if changed initChoice (Unchanged initChoice),fromView)]
	where
		initChoice		= FormValue (maybe (choice (map view model)) (choiceSel (map view model)) mbSel)
		fromView mbC 	= (fmap getChoiceIndex mbC, Nothing)
	
	termination mbSel options _ = UserActions (actions (fmap ((!!) options) mbSel))

updateMultipleChoice :: !d ![o] ![Int] -> Task [o] | descr d & iTask o
updateMultipleChoice d options sel = localMultipleChoice d id okActionMultipleChoice options voidNothing (Just sel)

updateMultipleChoiceAbout :: !d !about	![o] ![Int] -> Task [o] | descr d & iTask o & iTask about
updateMultipleChoiceAbout d about options sel = localMultipleChoice d id okActionMultipleChoice options (Just about) (Just sel)

updateMultipleChoiceA :: !d !(o -> v) ![o] ![Int] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v
updateMultipleChoiceA d view options sel actions = localMultipleChoice d view actions options voidNothing (Just sel)

updateMultipleChoiceAboutA :: !d !(o -> v) !about ![o] ![Int] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask about
updateMultipleChoiceAboutA d view about options sel actions = localMultipleChoice d view actions options (Just about) (Just sel)

localMultipleChoice d view actions options mbAbout mbSel
	= InputTask @>> interactLocal
		d
		(\_ -> addAbout mbAbout [UpdateView (multipleChoiceFormView (map view options) mbSel,\mbC -> maybe [] getChoiceIndexes mbC)])
		[]
		(\idxs -> UserActions (actions (getItems options idxs)))
where
	multipleChoiceFormView options mbSel = Unchanged (FormValue (maybe (multipleChoice options) (multipleChoiceSel options) mbSel))

updateSharedMultipleChoice :: !d !(Shared [o] w) ![Int] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d shared sel = sharedMultipleChoice d id okActionMultipleChoice shared voidNothing (Just sel)

updateSharedMultipleChoiceAbout :: !d !about !(Shared [o] w) ![Int] -> Task [o] | descr d & iTask o & iTask w & iTask about
updateSharedMultipleChoiceAbout d about shared sel = sharedMultipleChoice d id okActionMultipleChoice shared (Just about) (Just sel)

updateSharedMultipleChoiceA :: !d !(o -> v) !(Shared [o] w) ![Int] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask w
updateSharedMultipleChoiceA d view shared sel actions = sharedMultipleChoice d view actions shared voidNothing (Just sel)

updateSharedMultipleChoiceAboutA :: !d !(o -> v) !about !(Shared [o] w) ![Int] !(ActionFunc [o]  a) -> Task a | descr d & iTask a & iTask v & iTask w & iTask about
updateSharedMultipleChoiceAboutA d view about shared sel actions = sharedMultipleChoice d view actions shared (Just about) (Just sel)

sharedMultipleChoice description view actions shared mbAbout mbSel
	= interact description interaction (fromMaybe [] mbSel) shared termination
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (toView,fromView)]
	where
		toView				= FormValue (multipleChoiceSel (map view model) (if changed [] local))
		fromView (Just mc)	= (getChoiceIndexes mc, Nothing)
		fromView Nothing	= (local, Nothing)
	
	termination local model _
		# choices = [a \\ a <- model & i <- [0..] |isMember i local] //Inefficient :(
		= UserActions (actions choices)

//Local output
showMessage :: !d !a -> Task a | descr d & iTask a
showMessage d a = showMessage` d [(ActionOk,a)] voidNothing

showMessageAbout :: !d !about -> Task about | descr d & iTask about
showMessageAbout d about = showMessage` d [(ActionOk,about)] (Just about)

showMessageA :: !d ![(!Action,!a)] -> Task a | descr d & iTask a
showMessageA d actions = showMessage` d actions voidNothing

showMessageAboutA :: !d !(about -> v) !about ![(!Action,!a)] -> Task a | descr d & iTask a & iTask v
showMessageAboutA d view about actions = showMessage` d actions (Just (view about))

showMessage` d actions mbAbout = OutputTask PassiveOutput @>>
	interactLocal d (const (addAbout mbAbout [])) Void (const (UserActions (map (appSnd Just) actions)))

showInstruction :: !String !instruction	!a -> Task a | html instruction & iTask a
showInstruction title instr a = showInstruction` (title,instr) a voidNothing

showInstructionAbout :: !String !instruction !about -> Task about | html instruction & iTask about
showInstructionAbout title instr about = showInstruction` (title,instr) about (Just about)

showInstruction` d a mbAbout = OutputTask ActiveOutput @>>
	interactLocal d (const (addAbout mbAbout [])) Void (const (okAction (Just a)))

// Monitor (shared output) tasks

import StdMisc

monitor :: !d !(r -> v) !(r -> Bool) !Bool !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w
monitor d view pred autoContinue shared = monitor` d (Just view) pred autoContinue id voidNothing shared

monitorAbout :: !d !(r -> v) !(r -> Bool) !Bool !about !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w & iTask about
monitorAbout d view pred autoContinue about shared = monitor` d (Just view) pred autoContinue id (Just about) shared

monitorA :: !d !(r -> v) !(Shared r w) !(r -> InteractionTerminators a) -> Task a | descr d & iTask a & iTask v & iTask w
monitorA d view shared terms = monitorA` d view terms voidNothing shared

monitorAboutA :: !d !(r -> v) !about !(Shared r w) !(r -> InteractionTerminators a) -> Task a | descr d & iTask a & iTask v & iTask w & iTask about
monitorAboutA d view about shared terms = monitorA` d view terms (Just about) shared

wait :: !d !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
wait d shared = monitor` d noView isJust True fromJust voidNothing shared

waitAbout :: !d !about !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about
waitAbout d about shared = monitor` d noView isJust True fromJust (Just about) shared

waitUntil :: !d !(r -> Bool) !(Shared r w) -> Task r | descr d & iTask r & iTask w
waitUntil d pred shared = monitor` d noView pred True id voidNothing shared

waitUntilAbout :: !d !(r -> Bool) !about !(Shared r w) -> Task r | descr d & iTask r & iTask w & iTask about
waitUntilAbout d pred about shared = monitor` d noView pred True id (Just about) shared

monitor` d mbView pred autoContinue transF mbAbout shared
	= OutputTask PassiveOutput @>> interact d (maybe (\_ _ _ -> []) (\view _ m _ -> addAbout mbAbout [DisplayView (view m)]) mbView) Void shared termFunc
where
	termFunc _ m _
		| autoContinue
			| continue
				= StopInteraction (transF m)
			| otherwise
				= UserActions []
		| otherwise
			= UserActions [(ActionContinue,if continue (Just (transF m)) Nothing)]
	where
		continue = pred m
		
monitorA` d view terms mbAbout shared
	= OutputTask PassiveOutput @>> interact
		d
		(\_ r _ -> addAbout mbAbout [DisplayView (view r)])
		Void
		shared
		(\_ r _ -> terms r)

waitForTime :: !Time -> Task Time
waitForTime time =
		waitUntil ("Wait for time", ("Wait until " +++ toString time)) pred currentTime
where	
	pred now = time < now

waitForDate :: !Date -> Task Date
waitForDate date =
		waitUntil ("Wait for date", ("Wait until " +++ toString date)) pred currentDate
where
	pred now = date < now

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>= \now -> waitForTime (now + time)

noView :: Maybe (a -> Void)
noView = Nothing

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions = interactLocal chooseActionDescr (const []) Void (const (UserActions (map (appSnd Just) actions)))

chooseActionDyn :: !(Shared r w) !(ActionFunc r a) -> Task a | iTask a & iTask w
chooseActionDyn shared actionsF = interact chooseActionDescr (\_ _ _ -> []) Void shared (\_ r _ -> UserActions (actionsF r))

chooseActionDescr = "Choose an action"

okActionLocalEnter mbV					= [(ActionOk,mbV)]
okActionEditor {localValid,modelValue}	= [(ActionOk,if localValid (Just modelValue) Nothing)]
okActionMultipleChoice sel				= [(ActionOk,Just sel)]

noActions :: ActionFunc a Void
noActions = const []

noActionsMsg :: [(!Action,!Maybe Void)]
noActionsMsg = []