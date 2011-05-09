implementation module InputTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Shared, Util, TSt, ExceptionCombinators, InteractionTasks, CoreCombinators, CommonCombinators
from StdFunc 		import id, const, o
from SharedTasks	import sharedStore, :: SharedStoreId
from SharedTasks	import qualified readShared, writeShared

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
enterSharedInformationA :: !d !(v -> w) ![PredAction (Valid,r)] !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
enterSharedInformationA d view actions shared = enterSharedInformationA` d view actions shared voidNothing
	
enterSharedInformationAboutA :: !d !(v -> w) ![PredAction (Valid,r)] !about !(Shared r w) -> Task (!Action,!r) | descr d  & iTask r & iTask about & iTask v & iTask w
enterSharedInformationAboutA d view actions about shared = enterSharedInformationA` d view actions shared (Just about)

enterSharedInformationA` d view actions shared mbAbout	= InputTask @>> interact d (\_ r _ -> addAbout mbAbout [UpdateView (Unchanged Blank,\mbV -> (isJust mbV,fmap view mbV))]) (fromPredActions (\a r _ -> (a,r)) (\a _ r _ -> (a,r)) actions) False shared

//Confirmation tasks
requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation d = requestConfirmation` d voidNothing
						
requestConfirmationAbout :: !d !about -> Task Bool | descr d & iTask about
requestConfirmationAbout d about = requestConfirmation` d (Just about)

requestConfirmation` d mbAbout = interactLocal d (const (addAbout mbAbout [])) (const (UserActions [(ActionNo,Just False),(ActionYes,Just True)])) Void

//Local choice tasks
enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice d options = enterChoice` d options voidNothing

enterChoiceA :: !d !(a -> v) ![PredAction (Verified a)] ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v
enterChoiceA d view actions options = enterChoiceA` d view actions options voidNothing

enterChoiceAbout :: !d !about ![a] -> Task a | descr d & iTask a & iTask about
enterChoiceAbout d about options = enterChoice` d options (Just about)

enterChoiceAboutA :: !d !(a -> v) ![PredAction (Verified a)] !about ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask about & iTask v
enterChoiceAboutA d view actions about options = enterChoiceA` d view actions options (Just about)

enterChoice` d options mbAbout					= InputTask @>> interactLocal d (\mbC -> addAbout mbAbout [UpdateView (choiceFormView mbC options,fmap getChoice)]) okAction Nothing
enterChoiceA` d view actions options mbAbout	= InputTask @>> interactLocal d (\mbC -> addAbout mbAbout [UpdateView (choiceFormView mbC (map view options),fmap getChoiceIndex)]) (fromPredActionsLocal (mb2Ver o fmap ((!!) options)) (\a mbIdx -> (a,fmap ((!!) options) mbIdx)) actions) Nothing
choiceFormView mbC options = if (isNothing mbC) (FormValue (choice options)) (Unchanged Blank)

//Local multiple choice tasks
enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice d options = enterMultipleChoice` d options voidNothing

enterMultipleChoiceA :: !d !(a -> v) ![PredAction [a]] ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask v
enterMultipleChoiceA d view actions options = enterMultipleChoiceA` d view actions options voidNothing

enterMultipleChoiceAbout :: !d !about ![a] -> Task [a] | descr d & iTask a & iTask about
enterMultipleChoiceAbout d about options = enterMultipleChoice` d options (Just about)

enterMultipleChoiceAboutA :: !d !(a -> v) ![PredAction [a]] !about ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask about & iTask v
enterMultipleChoiceAboutA d view actions about options = enterMultipleChoiceA` d view actions options (Just about)

enterMultipleChoice` d options mbAbout					= InputTask @>> interactLocal d (\mbC -> addAbout mbAbout [UpdateView (multipleChoiceFormView mbC options,fmap getChoices)]) okAction Nothing
enterMultipleChoiceA` d view actions options mbAbout
	= InputTask @>> interactLocal
		d
		(\mbC -> addAbout mbAbout [UpdateView (multipleChoiceFormView mbC (map view options),\mbC -> Just (maybe [] getChoiceIndexes mbC))])
		(fromPredActionsLocal (maybe [] (getIndexes options)) (\a mbIdx -> (a,maybe [] (getIndexes options) mbIdx)) actions)
		Nothing
multipleChoiceFormView mbC options = if (isNothing mbC) (FormValue (multipleChoice options)) (Unchanged Blank)

/*makeChoiceTask :: !d !(Maybe about) !(a -> v) ![a] !(Maybe Int) -> TaskFunctions a | descr d & iTask a & iTask v & iTask about
makeChoiceTask description _ _ [] _
	= (id,\tst -> (choiceException description,tst))
makeChoiceTask _ mbContext view opts mbSel
	# initChoice	= maybe (choice opts) (choiceSel opts) mbSel
	# taskFuncs		= makeInformationTask mbContext (mapOptions view,\v a -> setChoiceIndex (getChoiceIndex v) a) (LocalUpdate initChoice)
	= mapTaskFunctions getChoice taskFuncs*/
		


enterSharedChoiceA :: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
enterSharedChoiceA description view actions shared = undef
		
enterSharedMultipleChoiceA	:: !d !(a -> v) ![PredAction [a]] !(Shared [a] w) -> Task (!Action, [a])	| descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceA description view actions shared 
	= interact description interaction termination [] shared
where
	interaction local model changed
		= [UpdateView (toView,fromView)]
	where
		toView				= FormValue (multipleChoiceSel (map view model) (if changed [] local))
		fromView (Just mc)	= (getChoiceIndexes mc, Nothing)
		fromView Nothing	= (local, Nothing)
	
	termination local model _
		# choices = [a \\ a <- model & i <- [0..] |isMember i local] //Inefficient :(
		= UserActions [(action, if (pred choices) (Just (action,choices)) Nothing) \\ (action,pred) <- actions]
		
/*

enterSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v
enterSharedChoiceAboutA description view actions about shared
	= sharedChoiceTask description (Just about) view actions shared Nothing

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options Nothing)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!Action, [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options Nothing)

enterSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask v
enterSharedMultipleChoiceA description view actions shared
	= sharedMultipleChoiceTask description noAboutInfo view actions shared Nothing
		
enterMultipleChoiceAbout :: !d !about ![a] -> Task [a] | descr d & iTask a & iTask about
enterMultipleChoiceAbout description about options
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options Nothing)
		
enterMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about ![a] -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v
enterMultipleChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options Nothing)

enterSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v
enterSharedMultipleChoiceAboutA description view actions about shared
	= sharedMultipleChoiceTask description (Just about) view actions shared Nothing
		
updateMultipleChoiceAbout :: !d !about ![a] ![Int] -> Task [a] | descr d & iTask a & iTask about
updateMultipleChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options (Just sel))

updateMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about ![a] ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v
updateMultipleChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options (Just sel))

updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about !(Shared [a] w) ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v
updateSharedMultipleChoiceAboutA description view actions about shared sel
	=  sharedMultipleChoiceTask description (Just about) view actions shared (Just sel)

noAboutInfo :: Maybe Void
noAboutInfo = Nothing



makeChoiceTaskA :: !d !(Maybe about) !(a -> v) ![TaskAction a] ![a] !(Maybe Int) -> TaskFunctions (!Action,!Maybe a) | descr d & iTask a & iTask v & iTask about
makeChoiceTaskA description _ _ _ [] _
	= (id,\tst -> (choiceException description,tst))
makeChoiceTaskA _ mbContext view actions opts mbSel
	# initChoice	= maybe (choice opts) (choiceSel opts) mbSel
	# (result,tst)	= makeInformationTaskA mbContext (mapOptions view,\v a -> setChoiceIndex (getChoiceIndex v) a) (mapTaskActionPredicates getChoice actions) (LocalUpdate initChoice)
	= mapTaskFunctions (appSnd (fmap getChoice)) (result,tst)

sharedChoiceTask :: !d !(Maybe about) !(a -> v) ![TaskAction a] !(Shared [a] w) !(Maybe Int) -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v & iTask about
sharedChoiceTask description mbContext view actions sharedOpts mbSel =
				'SharedTasks'.readShared sharedOpts
	>>=		 	transform initChoice
	>>=			choiceStore
	>>= \store.	if (isJust mbContext) (updateSharedInformationAboutA description (toView,const) (mapTaskActionPredicates getChoiceFromModel actions) (fromJust mbContext) (store >+| sharedOpts)) (updateSharedInformationA description (toView,const) (mapTaskActionPredicates getChoiceFromModel actions) (store >+| sharedOpts))
	>>=			transform (appSnd (fmap getChoiceFromModel))
where
	initChoice opts
		# viewOpts = map view opts
		= case mbSel of
			Just sel	= choiceSel viewOpts sel
			Nothing		= choice viewOpts
	toView (choice,opts) = setOptions (map view opts) choice
	getChoiceFromModel (choice,opts) = opts !! getChoiceIndex choice
	
choiceException description = taskException "Cannot choose from empty option list"

makeMultipleChoiceTask :: !(Maybe about) !(a -> v) ![a] !(Maybe [Int]) -> TaskFunctions [a] | iTask a & iTask v & iTask about
makeMultipleChoiceTask mbContext view opts mbSel
	# initChoice	= maybe (multipleChoice opts) (multipleChoiceSel opts) mbSel
	# taskFuncs		= makeInformationTask mbContext (mapOptionsM view,\v a -> setChoiceIndexes (getChoiceIndexes v) a) (LocalUpdate initChoice)
	= mapTaskFunctions getChoices taskFuncs

makeMultipleChoiceTaskA :: !(Maybe about) !(a -> v) ![TaskAction [a]] ![a] !(Maybe [Int]) -> TaskFunctions (!Action,![a]) | iTask a & iTask v & iTask about
makeMultipleChoiceTaskA mbContext view actions opts mbSel
	# initChoice	= maybe (multipleChoice opts) (multipleChoiceSel opts) mbSel
	# taskFuncs		= makeInformationTaskA mbContext (mapOptionsM view,\v a -> setChoiceIndexes (getChoiceIndexes v) a) (mapTaskActionPredicates getChoices actions) (LocalUpdate initChoice)
	= mapTaskFunctions (appSnd (getChoices o fromJust)) taskFuncs
	
sharedMultipleChoiceTask :: !d !(Maybe about) !(a -> v) ![TaskAction [a]] !(Shared [a] w) !(Maybe [Int]) -> Task (!Action,![a]) | descr d & iTask a & iTask v & iTask about
sharedMultipleChoiceTask description mbContext view actions sharedOpts mbSel =
				'SharedTasks'.readShared sharedOpts
	>>=		 	transform initChoice
	>>=			choiceStore
	>>= \store.	if (isJust mbContext) (updateSharedInformationAboutA description (toView,const) (mapTaskActionPredicates getChoicesFromModel actions) (fromJust mbContext) (store >+| sharedOpts)) (updateSharedInformationA description (toView,const) (mapTaskActionPredicates getChoicesFromModel actions) (store >+| sharedOpts))
	>>=			transform (appSnd (getChoicesFromModel o fromJust))
where
	initChoice opts
		# viewOpts = map view opts
		= case mbSel of
			Just sel	= multipleChoiceSel viewOpts sel
			Nothing		= multipleChoice viewOpts
	toView (choice,opts) = setOptionsM (map view opts) choice
	getChoicesFromModel (choice,opts) = [opt \\ opt <- opts & i <- [0..] | isMember i (getChoiceIndexes choice)]

choiceStore init = mkInstantTask "Creates store for shared choice" (\tst=:{taskNr} -> (TaskFinished (sharedStore (iTaskId taskNr "choice") init),tst))



//Changes all predicates on values of type a to predicates on values of type about										
mapTaskActionPredicates :: !(about -> a) ![TaskAction a] -> [TaskAction about]
mapTaskActionPredicates vMap actions = map changePrecicate actions
where
	changePrecicate (action,pred) = (action,newPred pred)
	newPred pred v = case v of
		Invalid	= pred Invalid
		Valid about	= pred (Valid (vMap about))

undefGet :: !a -> abort
undefGet _ = abort "undefined view-get function"

sharedException :: !String -> TaskResult a
sharedException e = taskException (SharedException e)*/