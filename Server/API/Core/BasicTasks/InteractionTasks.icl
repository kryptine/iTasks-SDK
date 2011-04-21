implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Shared, Util, TSt, ExceptionCombinators, InteractiveTasks, CoreCombinators, CommonCombinators
from StdFunc 		import id, const, o
from SharedTasks	import sharedStore, :: SharedStoreId
from SharedTasks	import qualified readShared, writeShared

//Input tasks
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation description
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo (undefGet,snd idView) Enter)
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo (undefGet,\v _ -> view v) actions Enter)
		
enterInformationAbout :: !d !about -> Task a | descr d & iTask a & iTask about
enterInformationAbout description about
	= mkInteractiveTask description Information (makeInformationTask (Just about) (undefGet,snd idView) Enter)
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !about -> Task (!Action, Maybe a) | descr d  & iTask a & iTask about & iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) (undefGet,\v _ -> view v) actions Enter)

updateInformation :: !d !a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo idView (LocalUpdate initial))

updateInformationA :: !d !(SymmetricView a v) ![TaskAction a] !a -> Task (!Action,  !Maybe a) | descr d & iTask a & iTask v
updateInformationA description view actions initial
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo view actions (LocalUpdate initial))

updateSharedInformationA :: !d !(View i v o) ![TaskAction i] !(Shared i o) -> Task (!Action, !Maybe i) | descr d & iTask i & iTask v & iTask o
updateSharedInformationA description view actions shared
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo view actions (SharedUpdate shared))

updateInformationAbout :: !d !about !a -> Task a | descr d & iTask a & iTask about
updateInformationAbout description about initial
	= mkInteractiveTask description Information (makeInformationTask (Just about) idView (LocalUpdate initial))

updateInformationAboutA :: !d !(SymmetricView a v) ![TaskAction a] !about !a -> Task (!Action,  !Maybe a) | descr d & iTask a & iTask about & iTask v
updateInformationAboutA description view actions about initial
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) view actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(View i v o) ![TaskAction i] !about !(Shared i o) -> Task (!Action, !Maybe i) | descr d & iTask i & iTask v & iTask o & iTask about
updateSharedInformationAboutA description view actions about shared
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) view actions (SharedUpdate shared))

makeInformationTask :: !(Maybe about) !(View i v o) !(InteractiveTaskMode i o) -> TaskFunctions i | iTask i & iTask v & iTask o & iTask about
makeInformationTask mbContext view informationTaskMode
	= mapTaskFunctions (fromJust o snd) (makeInformationTaskA mbContext view [(ActionOk,ifvalid)] informationTaskMode)

makeInformationTaskA :: !(Maybe about) !(View i v o) ![TaskAction i] !(InteractiveTaskMode i o) -> TaskFunctions (!Action,!Maybe i) | iTask i & iTask v & iTask o & iTask about
makeInformationTaskA mbContext view actions informationTaskMode
	= makeInformationTaskAV mbContext view actions informationTaskMode
	
makeInformationTaskAV :: !(Maybe about) !(View i v o) ![TaskAction i] !(InteractiveTaskMode i o) -> TaskFunctions (!Action,!Maybe i) | iTask i & iTask v & iTask o & iTask about
makeInformationTaskAV mbContext view actions interactionTaskMode
	= makeInteractiveTask (fmap AboutValue mbContext) id view actions Nothing interactionTaskMode

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description options
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options Nothing)
		
enterChoiceA :: !d !(a -> v) ![TaskAction a] ![a] -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
enterChoiceA description view actions options
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options Nothing)

enterSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
enterSharedChoiceA description view actions shared
	= sharedChoiceTask description noAboutInfo view actions shared Nothing
				
updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a
updateChoice description options sel
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options (Just sel))

updateChoiceA :: !d !(a -> v) ![TaskAction a] ![a] !Int -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
updateChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options (Just sel))

updateSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a] w) !Int -> Task (!Action, Maybe a) | descr d & iTask a & iTask v
updateSharedChoiceA description view actions shared sel
	= sharedChoiceTask description noAboutInfo view actions shared (Just sel)
		
enterChoiceAbout :: !d !about ![a] -> Task a | descr d & iTask a & iTask about
enterChoiceAbout description about options
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options Nothing)
		
enterChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about ![a] -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v
enterChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options Nothing)

enterSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v
enterSharedChoiceAboutA description view actions about shared
	= sharedChoiceTask description (Just about) view actions shared Nothing
		
updateChoiceAbout :: !d !about ![a] !Int -> Task a | descr d & iTask a & iTask about
updateChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options (Just sel))

updateChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about ![a] !Int -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v
updateChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options (Just sel))

updateSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about !(Shared [a] w) !Int -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v
updateSharedChoiceAboutA description view actions about shared sel
	= sharedChoiceTask description (Just about) view actions shared (Just sel)

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options Nothing)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!Action, [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options Nothing)

enterSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask v
enterSharedMultipleChoiceA description view actions shared
	= sharedMultipleChoiceTask description noAboutInfo view actions shared Nothing
		
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options (Just sel))

updateMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask v
updateMultipleChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options (Just sel))

updateSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a] w) ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask v
updateSharedMultipleChoiceA description view actions shared sel
	= sharedMultipleChoiceTask description noAboutInfo view actions shared (Just sel)
		
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

makeChoiceTask :: !d !(Maybe about) !(a -> v) ![a] !(Maybe Int) -> TaskFunctions a | descr d & iTask a & iTask v & iTask about
makeChoiceTask description _ _ [] _
	= (id,\tst -> (choiceException description,tst))
makeChoiceTask _ mbContext view opts mbSel
	# initChoice	= maybe (choice opts) (choiceSel opts) mbSel
	# taskFuncs		= makeInformationTask mbContext (mapOptions view,\v a -> setChoiceIndex (getChoiceIndex v) a) (LocalUpdate initChoice)
	= mapTaskFunctions getChoice taskFuncs

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

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description Message (makeMessageTask (NoAboutMsg value))

showMessageA :: !d ![TaskAction a] a -> Task (!Action, a) | descr d & iTask a
showMessageA description actions value
	= mkInteractiveTask description Message (makeMessageTaskA (NoAboutMsg value) id actions)

showMessageAbout :: !d !about -> Task about | descr d & iTask about
showMessageAbout description about
	= mkInteractiveTask description Message (makeMessageTask (AboutValueMsg about))

showMessageAboutA :: !d !(about -> v) ![TaskAction about] !about -> Task (!Action, about) | descr d & iTask about & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description Message (makeMessageTaskA (AboutValueMsg about) view actions)

showMessageSharedA :: !d !(i -> v) ![TaskAction i] !(Shared i o) -> Task (!Action, i) | descr d & iTask i & iTask v
showMessageSharedA description view actions shared
	= mkInteractiveTask description Message (makeMessageTaskA (sharedAboutMsg shared) view actions)
	
showStickyMessage :: !d a -> Task a | descr d & iTask a
showStickyMessage description value
	= mkInteractiveTask description Message (makeMessageTaskSticky (NoAboutMsg value) id)

showStickyMessageAbout :: !d !about -> Task about | descr d & iTask about
showStickyMessageAbout description about
	= mkInteractiveTask description Message (makeMessageTaskSticky (AboutValueMsg about) id)

showStickyMessageShared :: !d !(i -> v) !(Shared i o) -> Task i | descr d & iTask i & iTask v
showStickyMessageShared description view shared
	= mkInteractiveTask description Message (makeMessageTaskSticky (sharedAboutMsg shared) view)

requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation description
	= mkInteractiveTask description Information (mapTaskFunctions mapConfirmationResult (makeMessageTaskA (NoAboutMsg Void) id confirmationButtons))
						
requestConfirmationAbout :: !d !about -> Task Bool | descr d & iTask about
requestConfirmationAbout description about
	= mkInteractiveTask description Information (mapTaskFunctions mapConfirmationResult (makeMessageTaskA (AboutValueMsg about) id confirmationButtons))

confirmationButtons = [(ActionNo, always),(ActionYes, always)]
		
mapConfirmationResult (action,_) = case action of
		ActionYes	= True
		_			= False

:: AboutMsg a	= NoAboutMsg !a						// don't show value, only return as result
				| AboutValueMsg !a					// show about value
				| SharedAboutMsg !(Shared a Void)	// show shared about value

sharedAboutMsg :: !(Shared r w) -> AboutMsg r
sharedAboutMsg shared = SharedAboutMsg (toReadOnlyShared shared)

makeMessageTask :: !(AboutMsg a) -> TaskFunctions a | iTask a
makeMessageTask about
	= mapTaskFunctions snd (makeMessageTaskA about id [(ActionOk,always)])
	
makeMessageTaskSticky :: !(AboutMsg a) !(a -> v) -> TaskFunctions a | iTask a & iTask v
makeMessageTaskSticky about view
	= mapTaskFunctions snd (makeMessageTaskA about view [])

makeMessageTaskA :: !(AboutMsg a) !(a -> v) ![TaskAction a] -> TaskFunctions (!Action,!a) | iTask a & iTask v
makeMessageTaskA about view actions = appSnd ((o) mapResult) (makeInteractiveTask mbAbout view (const (Hidden Void),\_ _ -> Void) actions Nothing mode)
where
	mapResult (res,tst) = case res of
		TaskFinished (event,_)
			# (msgResult,tst) = case about of
				NoAboutMsg v		= (Ok v,tst)
				AboutValueMsg v		= (Ok v,tst)
				SharedAboutMsg ref	= accIWorldTSt (readShared ref) tst
			= case msgResult of
				Ok msgResult		= (TaskFinished (event,msgResult),tst)
				Error e				= (sharedException e,tst)
		TaskBusy					= (TaskBusy,tst)
		TaskException e str			= (TaskException e str,tst)
	
	mbAbout = case about of
		NoAboutMsg _		= Nothing
		AboutValueMsg v		= Just (AboutValue v)
		SharedAboutMsg ref	= Just (SharedAbout ref)
	mode = case about of
		NoAboutMsg v		= LocalUpdateMode Void (const v)
		AboutValueMsg v		= LocalUpdateMode Void (const v)
		SharedAboutMsg ref	= SharedUpdate ref

showInstruction :: !String !instruction a -> Task a | html instruction & iTask a
showInstruction subject instruction value
	= mkInteractiveTask (subject,instruction) Instruction (makeInstructionTask noAboutInfo value)

showInstructionAbout :: !String !instruction !about -> Task about | html instruction & iTask about
showInstructionAbout subject instruction context
	= mkInteractiveTask (subject,instruction) Instruction (makeInstructionTask (Just context) context)

makeInstructionTask :: !(Maybe about) !a -> TaskFunctions a | iTask a & iTask about
makeInstructionTask context value
	= mapTaskFunctions (const value) (makeInteractiveTask (fmap AboutValue context) id idView [(ActionOk,always)] Nothing (LocalUpdate (Hidden Void)))

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
sharedException e = taskException (SharedException e)
