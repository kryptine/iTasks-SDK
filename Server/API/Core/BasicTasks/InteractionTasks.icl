implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Shared, Util, TSt, ExceptionCombinators, InteractiveTasks
from StdFunc import id, const, o
from TaskPanel		import :: InteractiveTaskType(..)

//Input tasks
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation description
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo (undefGet,snd idView) Enter)
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo (undefGet,\v _ -> view v) actions Enter)
		
enterInformationAbout :: !d !about -> Task a | descr d & iTask a & iTask about
enterInformationAbout description about
	= mkInteractiveTask description Information (makeInformationTask (Just about) (undefGet,snd idView) Enter)
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !about -> Task (!ActionEvent, Maybe a) | descr d  & iTask a & iTask about & iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) (undefGet,\v _ -> view v) actions Enter)

updateInformation :: !d !a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo idView (LocalUpdate initial))

updateInformationA :: !d !(SymmetricView a v) ![TaskAction a] !a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask v
updateInformationA description view actions initial
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo view actions (LocalUpdate initial))

updateSharedInformationA :: !d !(View i v o) ![TaskAction i] !(Shared i o) -> Task (!ActionEvent, !Maybe i) | descr d & iTask i & iTask v & iTask o
updateSharedInformationA description view actions shared
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo view actions (SharedUpdate shared))

updateInformationAbout :: !d !about !a -> Task a | descr d & iTask a & iTask about
updateInformationAbout description about initial
	= mkInteractiveTask description Information (makeInformationTask (Just about) idView (LocalUpdate initial))

updateInformationAboutA :: !d !(SymmetricView a v) ![TaskAction a] !about !a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask about & iTask v
updateInformationAboutA description view actions about initial
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) view actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(View i v o) ![TaskAction i] !about !(Shared i o) -> Task (!ActionEvent, !Maybe i) | descr d & iTask i & iTask v & iTask o & iTask about
updateSharedInformationAboutA description view actions about shared
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) view actions (SharedUpdate shared))

makeInformationTask :: !(Maybe about) !(View i v o) !(InteractionTaskMode i o) !*TSt -> (!TaskResult i,!*TSt) | iTask i & iTask v & iTask o & iTask about
makeInformationTask mbContext view informationTaskMode tst
	# (result,tst) = makeInformationTaskA mbContext view [(ActionOk,ifvalid)] informationTaskMode tst
	= (mapTaskResult (fromJust o snd) result,tst)

makeInformationTaskA :: !(Maybe about) !(View i v o) ![TaskAction i] !(InteractionTaskMode i o) !*TSt -> (!TaskResult (!ActionEvent,!Maybe i),!*TSt) | iTask i & iTask v & iTask o & iTask about
makeInformationTaskA mbContext view actions informationTaskMode tst
	= makeInformationTaskAV mbContext view actions informationTaskMode tst
	
makeInformationTaskAV :: !(Maybe about) !(View i v o) ![TaskAction i] !(InteractionTaskMode i o) !*TSt -> (!TaskResult (!ActionEvent,!Maybe i),!*TSt) | iTask i & iTask v & iTask o & iTask about
makeInformationTaskAV mbContext view actions interactionTaskMode tst
	= makeInteractiveTask (fmap AboutValue mbContext) id view actions noAutoActionEvents interactionTaskMode tst

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description options
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options Nothing)
		
enterChoiceA :: !d !(a -> v) ![TaskAction a] ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterChoiceA description view actions options
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options Nothing)

//enterSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a] w) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
//enterSharedChoiceA description view actions shared
//	= mkInteractiveTask description Information (makeSharedChoiceTask description noAboutInfo view actions shared Nothing)
				
updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a
updateChoice description options sel
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options (Just sel))

updateChoiceA :: !d !(a -> v) ![TaskAction a] ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
updateChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options (Just sel))

//updateSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a] w) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
//updateSharedChoiceA description view actions shared sel
//	= mkInteractiveTask description Information (makeSharedChoiceTask description noAboutInfo view actions shared (Just sel))
		
enterChoiceAbout :: !d !about ![a] -> Task a | descr d & iTask a & iTask about
enterChoiceAbout description about options
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options Nothing)
		
enterChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask about & iTask v
enterChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options Nothing)

//enterSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a] w) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
//enterSharedChoiceAboutA description view actions about shared
//	= mkInteractiveTask description Information (makeSharedChoiceTask description (Just about) view actions shared Nothing)
		
updateChoiceAbout :: !d !about ![a] !Int -> Task a | descr d & iTask a & iTask about
updateChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options (Just sel))

updateChoiceAboutA :: !d !(a -> v) ![TaskAction a] !about ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask about & iTask v
updateChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options (Just sel))

//updateSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a] w) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
//updateSharedChoiceAboutA description view actions about shared sel
//	= mkInteractiveTask description Information (makeSharedChoiceTask description (Just about) view actions shared (Just sel))

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options Nothing)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options Nothing)

//enterSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a] w) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
//enterSharedMultipleChoiceA description view actions shared
//	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask noAboutInfo view actions shared Nothing)
		
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options (Just sel))

updateMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
updateMultipleChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options (Just sel))

//updateSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a] w) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
//updateSharedMultipleChoiceA description view actions shared sel
//	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask noAboutInfo view actions shared (Just sel))
		
enterMultipleChoiceAbout :: !d !about ![a] -> Task [a] | descr d & iTask a & iTask about
enterMultipleChoiceAbout description about options
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options Nothing)
		
enterMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask about & iTask v
enterMultipleChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options Nothing)

//enterSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a] w) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
//enterSharedMultipleChoiceAboutA description view actions about shared
//	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask (Just about) view actions shared Nothing)
		
updateMultipleChoiceAbout :: !d !about ![a] ![Int] -> Task [a] | descr d & iTask a & iTask about
updateMultipleChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options (Just sel))

updateMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !about ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask about & iTask v
updateMultipleChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options (Just sel))

//updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a] w) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
//updateSharedMultipleChoiceAboutA description view actions about shared sel
//	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask (Just about) view actions shared (Just sel))

noAboutInfo :: Maybe Void
noAboutInfo = Nothing

makeChoiceTask :: !d !(Maybe about) !(a -> v) ![a] !(Maybe Int) !*TSt -> (!TaskResult a,!*TSt) | descr d & iTask a & iTask v & iTask about
makeChoiceTask description _ _ [] _ tst
	= choiceException description tst
makeChoiceTask _ mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTask mbContext (mapOptions view,\v a -> setChoiceIndex (getChoiceIndex v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoice result,tst)

makeChoiceTaskA :: !d !(Maybe about) !(a -> v) ![TaskAction a] ![a] !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | descr d & iTask a & iTask v & iTask about
makeChoiceTaskA description _ _ _ [] _ tst
	= choiceException description tst
makeChoiceTaskA _ mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTaskA mbContext (mapOptions view,\v a -> setChoiceIndex (getChoiceIndex v) a) (mapTaskActionPredicates getChoice actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (appSnd (fmap getChoice)) result,tst)

/*makeSharedChoiceTask :: !d !(Maybe about) !(a -> v) ![TaskAction a] !(Shared [a] w) !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent, !Maybe a),!*TSt) | descr d & iTask a & iTask v & iTask about
makeSharedChoiceTask description mbContext view actions shared mbSel tst
	# (opts,tst) = accIWorldTSt (readShared shared) tst
	| isEmpty opts
		= choiceException description tst
	| otherwise
		# viewOpts		= map view opts
		# initChoice = case mbSel of
			Just sel	= choiceSel viewOpts sel
			Nothing		= choice viewOpts
		# (result,tst)	= makeInformationTaskAV mbContext ((\opts choice -> app2 (id,not) (setOptions (map view opts) choice)),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoiceFromModel actions) (SharedUpdate shared) tst
		= (mapTaskResult (app2 (id,fmap getChoiceFromModel)) result,tst)
where
	getChoiceFromModel (opts,choice) = opts !! getChoiceIndex choice*/

choiceException description = applyTask (throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list"))
	
makeMultipleChoiceTask :: !(Maybe about) !(a -> v) ![a] !(Maybe [Int]) !*TSt -> (!TaskResult [a],!*TSt) | iTask a & iTask v & iTask about
makeMultipleChoiceTask mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTask mbContext (mapOptionsM view,\v a -> setChoiceIndexes (getChoiceIndexes v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoices result,tst)

makeMultipleChoiceTaskA :: !(Maybe about) !(a -> v) ![TaskAction [a]] ![a] !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent,!Maybe [a]),!*TSt) | iTask a & iTask v & iTask about
makeMultipleChoiceTaskA mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTaskA mbContext (mapOptionsM view,\v a -> setChoiceIndexes (getChoiceIndexes v) a) (mapTaskActionPredicates getChoices actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (appSnd (fmap getChoices)) result,tst)
	
/*makeSharedMultipleChoiceTask :: !(Maybe about) !(a -> v) ![TaskAction [a]] !(Shared [a] w) !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent, !Maybe [a]),!*TSt) | iTask a & iTask v & iTask about
makeSharedMultipleChoiceTask mbContext view actions shared mbSel tst =
	# (opts,tst)	= accIWorldTSt (readShared shared) tst
	# viewOpts		= map view opts
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel viewOpts sel
		Nothing		= multipleChoice viewOpts
	# (result,tst)	= makeInformationTaskAV mbContext (\opts choice -> (setOptionsM (map view opts) choice,False),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoicesFromModel actions) (SharedUpdate shared) tst
	= (mapTaskResult (app2 (id,fmap getChoicesFromModel)) result,tst)
where
	getChoicesFromModel (opts,choice) = [opt \\ opt <- opts & i <- [0..] | isMember i (getChoiceIndexes choice)]*/

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description Message (makeMessageTask (NoAboutMsg value))

showMessageA :: !d ![TaskAction a] a -> Task (!ActionEvent, a) | descr d & iTask a
showMessageA description actions value
	= mkInteractiveTask description Message (makeMessageTaskA (NoAboutMsg value) id actions)

showMessageAbout :: !d !about -> Task about | descr d & iTask about
showMessageAbout description about
	= mkInteractiveTask description Message (makeMessageTask (AboutValueMsg about))

showMessageAboutA :: !d !(about -> v) ![TaskAction about] !about -> Task (!ActionEvent, about) | descr d & iTask about & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description Message (makeMessageTaskA (AboutValueMsg about) view actions)

showMessageSharedA :: !d !(i -> v) ![TaskAction i] !(Shared i o) -> Task (!ActionEvent, i) | descr d & iTask i & iTask v
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
requestConfirmation description = mkInteractiveTask description Information requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = makeMessageTaskA (NoAboutMsg Void) id [(ActionNo, always),(ActionYes, always)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !d !about -> Task Bool | descr d & iTask about
requestConfirmationAbout description about = mkInteractiveTask description Information requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = makeMessageTaskA (AboutValueMsg about) id [(ActionNo, always),(ActionYes, ifvalid)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)

:: AboutMsg a	= NoAboutMsg !a						// don't show value, only return as result
				| AboutValueMsg !a					// show about value
				| SharedAboutMsg !(Shared a Void)	// show shared about value

sharedAboutMsg :: !(Shared r w) -> AboutMsg r
sharedAboutMsg shared = SharedAboutMsg (toReadOnlyShared shared)

makeMessageTask :: !(AboutMsg a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
makeMessageTask about tst
	# (result,tst) = makeMessageTaskA about id [(ActionOk,always)] tst
	= (mapTaskResult snd result,tst)
	
makeMessageTaskSticky :: !(AboutMsg a) !(a -> v) !*TSt -> (!TaskResult a,!*TSt) | iTask a & iTask v
makeMessageTaskSticky about view tst
	# (result,tst) = makeMessageTaskA about view [] tst
	= (mapTaskResult snd result,tst)

makeMessageTaskA :: !(AboutMsg a) !(a -> v) ![TaskAction a] !*TSt -> (!TaskResult (!ActionEvent, !a),!*TSt) | iTask a & iTask v
makeMessageTaskA about view actions tst
	# (result,tst) = makeInteractiveTask mbAbout view (Hidden,\_ _ -> Void) actions noAutoActionEvents mode tst
	# (msgResult,tst) = case about of
		NoAboutMsg v		= (v,tst)
		AboutValueMsg v		= (v,tst)
		SharedAboutMsg ref	= appFst fromOk (accIWorldTSt (readShared ref) tst)
	= (mapTaskResult (appSnd (const msgResult)) result,tst)
where
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

makeInstructionTask :: !(Maybe about) !a !*TSt -> *(!TaskResult a,!*TSt) | iTask a & iTask about
makeInstructionTask context value tst
	# (result,tst) = makeInteractiveTask (fmap AboutValue context) id idView [(ActionOk,always)] noAutoActionEvents (LocalUpdate Void) tst
	= (mapTaskResult (const value) result,tst)

//Changes all predicates on values of type a to predicates on values of type b										
mapTaskActionPredicates :: !(b -> a) ![TaskAction a] -> [TaskAction b]
mapTaskActionPredicates vMap actions = map changePrecicate actions
where
	changePrecicate (action,pred) = (action,newPred pred)
	newPred pred v = case v of
		Invalid	= pred Invalid
		Valid b	= pred (Valid (vMap b))

undefGet = abort "undefined bimap-get function"
