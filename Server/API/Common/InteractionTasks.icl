implementation module InteractionTasks

from StdFunc import id, const, o
from Shared import nullShared
import StdBool, StdList, Util
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData, SharedTasks
import StdMisc

interactLocal :: !d !(l -> [InteractionPart l]) !(l -> InteractionTerminators a) !l -> Task a | descr d & iTask l & iTask a
interactLocal d partFunc termFunc l = LocalInteractionTask @>> interact d (\l _ _ -> map toSharedRes (partFunc l)) (\l _ _ -> termFunc l) l nullShared`
where
	toSharedRes (UpdateView (formView,putback))	= UpdateView (formView,\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = nullShared
	
//Local input
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation d = enterInformation` d voidNothing
	
enterInformationA :: !d !(v -> a) ![PredAction (Verified a)] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v
enterInformationA d view actions = enterInformationA` d view actions voidNothing
		
enterInformationAbout :: !d !about -> Task a | descr d & iTask a & iTask about
enterInformationAbout d about = enterInformation` d (Just about)
	
enterInformationAboutA :: !d !(v -> a) ![PredAction (Verified a)] !about -> Task (!Action,!Maybe a) | descr d  & iTask a & iTask about & iTask v
enterInformationAboutA d view actions about = enterInformationA` d view actions (Just about)

enterInformation` d mbAbout					= InputTask @>> interactLocal d (const (addAbout mbAbout [UpdateView (Unchanged Blank,id)])) okAction Nothing
enterInformationA` d view actions mbAbout	= InputTask @>> interactLocal d (const (addAbout mbAbout [UpdateView (Unchanged Blank,fmap view)])) (fromPredActionsLocal mb2Ver tuple actions) Nothing

//Shared input
enterSharedInformationA :: !d !(v -> w) ![PredAction (!Valid,!r)] !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
enterSharedInformationA d view actions shared = enterSharedInformationA` d view actions shared voidNothing
	
enterSharedInformationAboutA :: !d !(v -> w) ![PredAction (!Valid,!r)] !about !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask about & iTask v & iTask w
enterSharedInformationAboutA d view actions about shared = enterSharedInformationA` d view actions shared (Just about)

enterSharedInformationA` d view actions shared mbAbout	= InputTask @>> interact d (\_ r _ -> addAbout mbAbout [UpdateView (Unchanged Blank,\mbV -> (isJust mbV,fmap view mbV))]) (fromPredActions (\a r _ -> (a,r)) (\a _ r _ -> (a,r)) actions) False shared

//Confirmation tasks
requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation d = requestConfirmation` d voidNothing
						
requestConfirmationAbout :: !d !about -> Task Bool | descr d & iTask about
requestConfirmationAbout d about = requestConfirmation` d (Just about)

requestConfirmation` d mbAbout = InputTask @>> interactLocal d (const (addAbout mbAbout [])) (const (UserActions [(ActionNo,Just False),(ActionYes,Just True)])) Void

//Local choice tasks
enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice _ [] = throw EmptyOptionList
enterChoice d options = enterChoice` d options voidNothing

enterChoiceA :: !d !(a -> v) ![PredAction (Verified a)] ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v
enterChoiceA _ _ _ [] = throw EmptyOptionList
enterChoiceA d view actions options = enterChoiceA` d view actions options voidNothing

enterChoiceAbout :: !d !about ![a] -> Task a | descr d & iTask a & iTask about
enterChoiceAbout _ _ [] = throw EmptyOptionList
enterChoiceAbout d about options = enterChoice` d options (Just about)

enterChoiceAboutA :: !d !(a -> v) ![PredAction (Verified a)] !about ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask about & iTask v
enterChoiceAboutA _ _ _ _ [] = throw EmptyOptionList
enterChoiceAboutA d view actions about options = enterChoiceA` d view actions options (Just about)

enterChoice` d options mbAbout					= InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (choiceFormView options,fmap getChoice)]) okAction Nothing
enterChoiceA` d view actions options mbAbout	= InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (choiceFormView (map view options),fmap getChoiceIndex)]) (fromPredActionsLocal (mb2Ver o fmap ((!!) options)) (\a mbIdx -> (a,fmap ((!!) options) mbIdx)) actions) Nothing
choiceFormView options = Unchanged (FormValue (choice options))

//Shared choice tasks
enterSharedChoiceA :: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask v & iTask w
enterSharedChoiceA d view actions shared = enterSharedChoiceA` d view actions shared voidNothing

enterSharedChoiceAboutA :: !d !(a -> v) ![PredAction (Verified a)] !about !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v & iTask w
enterSharedChoiceAboutA d view actions about shared = enterSharedChoiceA` d view actions shared (Just about)

enterSharedChoiceA` description view actions shared mbAbout
	= interact description interaction termination Nothing shared
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (if changed initChoice (Unchanged initChoice),fromView)]
	where
		initChoice		= FormValue (choice (map view model))
		fromView mbC 	= (fmap getChoiceIndex mbC, Nothing)
	
	termination = fromPredActions (\l model _ -> mb2Ver (fmap ((!!) model) l)) (\action mbIdx model _ -> (action,fmap ((!!) model) mbIdx)) actions

//Local multiple choice tasks
enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice d options = enterMultipleChoice` d options voidNothing

enterMultipleChoiceA :: !d !(a -> v) ![PredAction [a]] ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask v
enterMultipleChoiceA d view actions options = enterMultipleChoiceA` d view actions options voidNothing

enterMultipleChoiceAbout :: !d !about ![a] -> Task [a] | descr d & iTask a & iTask about
enterMultipleChoiceAbout d about options = enterMultipleChoice` d options (Just about)

enterMultipleChoiceAboutA :: !d !(a -> v) ![PredAction [a]] !about ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask about & iTask v
enterMultipleChoiceAboutA d view actions about options = enterMultipleChoiceA` d view actions options (Just about)

enterMultipleChoice` d options mbAbout = InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (multipleChoiceFormView options,maybe [] getChoices)]) (\choice -> okAction (Just choice)) []
enterMultipleChoiceA` d view actions options mbAbout
	= InputTask @>> interactLocal
		d
		(\_ -> addAbout mbAbout [UpdateView (multipleChoiceFormView (map view options),\mbC -> maybe [] getChoiceIndexes mbC)])
		(fromPredActionsLocal (getIndexes options) (\a idxs -> (a,getIndexes options idxs)) actions)
		[]
multipleChoiceFormView options = Unchanged (FormValue (multipleChoice options))

//Shared multiple choice tasks
enterSharedMultipleChoiceA	:: !d !(a -> v) ![PredAction [a]] !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceA description view actions shared = enterSharedMultipleChoiceA` description view actions shared voidNothing

enterSharedMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]] !about !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v & iTask w
enterSharedMultipleChoiceAboutA description view actions about shared = enterSharedMultipleChoiceA` description view actions shared (Just about)

enterSharedMultipleChoiceA` description view actions shared mbAbout
	= interact description interaction termination [] shared
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (toView,fromView)]
	where
		toView				= FormValue (multipleChoiceSel (map view model) (if changed [] local))
		fromView (Just mc)	= (getChoiceIndexes mc, Nothing)
		fromView Nothing	= (local, Nothing)
	
	termination local model _
		# choices = [a \\ a <- model & i <- [0..] |isMember i local] //Inefficient :(
		= UserActions [(action, if (pred choices) (Just (action,choices)) Nothing) \\ (action,pred) <- actions]
		
//Local update
updateInformation :: !d !a -> Task a | descr d & iTask a
updateInformation d a = updateInformation` d a voidNothing

updateInformationA :: !d !(SymmetricView a v) ![PredAction (Verified a)] !a -> Task (!Action, !Maybe a) | descr d & iTask a & iTask v
updateInformationA d view actions a = updateInformationA` d view actions a voidNothing

updateInformationAbout :: !d !about !a -> Task a | descr d & iTask a & iTask about
updateInformationAbout d about a = updateInformation` d a (Just about)

updateInformationAboutA :: !d !(SymmetricView a v) ![PredAction (Verified a)] !about !a -> Task (!Action, !Maybe a) | descr d & iTask a & iTask about & iTask v
updateInformationAboutA d view actions about a = updateInformationA` d view actions a (Just about)

updateInformation` d a mbAbout
	= UpdateTask @>> interactLocal
		d
		(\(valid,a) -> addAbout mbAbout [UpdateView (if valid (FormValue a) (Unchanged (FormValue a)),\mbA -> (isJust mbA,fromMaybe a mbA))])
		(\(valid,a) -> okAction (if valid (Just a) Nothing))
		(verifyValue a,a)

updateInformationA` d (get,putback) actions a mbAbout
	= UpdateTask @>> interactLocal
		d
		(\(valid,a) -> addAbout mbAbout [UpdateView (if valid (FormValue (get a)) (Unchanged (FormValue (get a))),\mbV -> (isJust mbV,maybe a (\v -> putback v a) mbV))])
		(fromPredActionsLocal (\(valid,a) -> if valid (Valid a) Invalid) (\action (valid,a) -> (action,if valid (Just a) Nothing)) actions)
		(verifyValue a,a)

//Shared update
updateSharedInformationA :: !d !(View r v w) ![PredAction (Valid,r)] !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
updateSharedInformationA d view actions shared = updateSharedInformationA` d view actions shared voidNothing

updateSharedInformationAboutA :: !d !(View r v w) ![PredAction (Valid,r)] !about !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w & iTask about
updateSharedInformationAboutA d view actions about shared = updateSharedInformationA` d view actions shared (Just about)

updateSharedInformationA` d (get,putback) actions shared mbAbout
	=  UpdateTask @>> interact
		d
		(\valid r changed -> addAbout mbAbout [UpdateView (if (valid || changed) (FormValue (get r)) (Unchanged (FormValue (get r))),\mbV -> (isJust mbV,fmap (\v -> putback v r) mbV))])
		(fromPredActions (\valid r changed -> ((valid || changed) && verifyValue (get r),r)) (\action _ r _ -> (action,r)) actions)
		True
		shared

/*
* Ask the user to select one item from a list of options with already one option pre-selected
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
* @param Int				The index of the item which should be pre-selected
*
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (updateSharedChoiceA only)
*/
//updateChoice				:: !d							![a]			!Int -> Task a					| descr d & iTask a	
//updateChoiceA 				:: !d !(a -> v) ![PredAction (Verified a)]	![a]			!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v 
//updateChoiceAbout			:: !d 							!about ![a]				!Int -> Task a					| descr d & iTask a	& iTask about
//updateChoiceAboutA			:: !d !(a -> v) ![PredAction (Verified a)] !about ![a]				!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v


/*
* Ask the user to select one item from a list of options with already one option pre-selected, given some context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
* @param Int				The index of the item which should be pre-selected
*
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (updateSharedChoiceAboutA only)
*/
//updateSharedChoiceA 		:: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v
//updateSharedChoiceAboutA	:: !d !(a -> v) ![PredAction (Verified a)] !about !(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select a number of items from a list of options with already some options pre-selected
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
* @param [Int]				The index of the items which should be pre-selected
*
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (updateSharedMultipleChoiceA only)
*/
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice _ _ _ = undef

//updateMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]	![a]			![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask v
//updateMultipleChoiceAbout		 :: !d 								!about ![a] 			![Int] -> Task [a]				| descr d & iTask a	& iTask about
//updateMultipleChoiceAboutA		 :: !d !(a -> v) ![PredAction [a]]	!about ![a] 			![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select a number of items from a list of options with already some options pre-selected, given additional context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
* @param [Int]				The index of the items which should be pre-selected
*
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (updateSharedMultipleChoiceAboutA only)
*/
//updateSharedMultipleChoiceA :: !d !(a -> v) ![PredAction [a]]	!(Shared [a] w)	![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask v
//updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![PredAction [a]]	!about !(Shared [a] w)	![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v

//Local output
showMessage :: !d !a -> Task a | descr d & iTask a
showMessage d a = showMessage` d a noView

showMessageA :: !d ![Action] !a -> Task (!Action,!a) | descr d & iTask a
showMessageA d actions a = showMessageA` d actions a noView

showMessageAbout :: !d !about -> Task about | descr d & iTask about
showMessageAbout d about = showMessage` d about (Just id)

showMessageAboutA :: !d !(about -> v) ![Action]	!about -> Task (!Action,!about) | descr d & iTask about & iTask v
showMessageAboutA d aboutView actions about = showMessageA` d actions about (Just aboutView)

showMessage` d a mbAboutView = OutputTask PassiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (okAction (Just a))) Void
showMessageA` d actions a mbAboutView
	= OutputTask PassiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (UserActions (map (\action -> (action,Just (action,a))) actions))) Void
msgAboutView a mbAboutView _ = maybe [] (\view -> [DisplayView (view a)]) mbAboutView

showInstruction :: !String !instruction	!a -> Task a | html instruction & iTask a
showInstruction title instr a = showInstruction` (title,instr) a noView

showInstructionAbout :: !String !instruction !about -> Task about | html instruction & iTask about
showInstructionAbout title instr about = showInstruction` (title,instr) about (Just id)

showInstruction` d a mbAboutView = OutputTask ActiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (okAction (Just a))) Void

// Monitor (shared output) tasks
monitor :: !d !(r -> v) !(r -> Bool) !Bool !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w
monitor d view pred autoContinue shared = monitor` d (Just view) pred autoContinue id voidNothing shared

monitorA :: !d !(r -> v) !(r -> Bool) ![PredAction r] !(Shared r w) -> Task (!Maybe Action,!r) | descr d & iTask r & iTask v & iTask w
monitorA d view pred actions shared = monitorA` d view pred actions voidNothing shared

monitorAbout :: !d !(r -> v) !(r -> Bool) !Bool !about !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w & iTask about
monitorAbout d view pred autoContinue about shared = monitor` d (Just view) pred autoContinue id (Just about) shared

monitorAboutA :: !d !(r -> v) !(r -> Bool) ![PredAction r] !about !(Shared r w) -> Task (!Maybe Action,!r) | descr d & iTask r & iTask v & iTask w & iTask about
monitorAboutA d view pred actions about shared = monitorA` d view pred actions (Just about) shared

wait :: !d !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
wait d shared = monitor` d noView isJust True fromJust voidNothing shared

waitAbout :: !d !about !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about
waitAbout d about shared = monitor` d noView isJust True fromJust (Just about) shared

waitUntil :: !d !(r -> Bool) !(Shared r w) -> Task r | descr d & iTask r & iTask w
waitUntil d pred shared = monitor` d noView pred True id voidNothing shared

waitUntilAbout :: !d !(r -> Bool) !about !(Shared r w) -> Task r | descr d & iTask r & iTask w & iTask about
waitUntilAbout d pred about shared = monitor` d noView pred True id (Just about) shared

monitor` d mbView pred autoContinue transF mbAbout shared
	= OutputTask PassiveOutput @>> interact d (maybe (\_ _ _ -> []) (\view _ m _ -> addAbout mbAbout [DisplayView (view m)]) mbView) termFunc Void shared
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
		
monitorA` d view pred actions mbAbout shared
	= OutputTask PassiveOutput @>> interact
		d
		(\_ r _ -> addAbout mbAbout [DisplayView (view r)])
		(\_ r _ -> if (pred r) (StopInteraction (Nothing,r)) ((fromPredActionsLocal id (\action r -> (Just action,r)) actions) r))
		Void
		shared

waitForTime :: !Time -> Task Time
waitForTime time =
		waitUntil ("Wait for time", ("Wait until " +++ toString time)) pred sharedCurrentTime
where	
	pred now = time < now

waitForDate :: !Date -> Task Date
waitForDate date =
		waitUntil ("Wait for date", ("Wait until " +++ toString date)) pred sharedCurrentDate
where
	pred now = date < now

waitForTimer :: !Time -> Task Time
waitForTimer time = readShared sharedCurrentTime >>= \now -> waitForTime (now + time)

noView :: Maybe (a -> Void)
noView = Nothing

chooseAction :: !d !(r -> [(!Action,!Maybe a)]) !(Shared r w) -> Task a | descr d & iTask a & iTask w
chooseAction d actionsF shared = interact d (\_ _ _ -> []) (\_ r _ -> UserActions (actionsF r)) Void shared

chooseActionConst :: !d ![(!Action,a)] -> Task a | descr d & iTask a
chooseActionConst d actions = interactLocal d (const []) (const (UserActions (map (appSnd Just) actions))) Void