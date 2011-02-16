implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Shared, Util, Functor, HTML, Text, HTTP, TSt, Store, ExceptionCombinators
from StdFunc import id, const, o
from HtmlUtil import paramValue
from TaskPanel import :: InteractiveTaskType(..)

derive JSONEncode UpdateMask, VerifyMask, ErrorMessage
derive JSONDecode UpdateMask, VerifyMask, ErrorMessage
derive bimap Maybe,(,)

always :: (Verified a) -> Bool
always _ = True

ifvalid :: (Verified a) -> Bool
ifvalid (Valid _) 	= True
ifvalid _			= False 

ifinvalid :: (Verified a) -> Bool
ifinvalid Invalid	= True
ifinvalid _			= False

//Input tasks
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation description
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo undefGet (snd idBimap) Enter)
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo undefGet (\v _ -> view v) actions Enter)
		
enterInformationAbout :: !d !b -> Task a | descr d  & iTask a & iTask b
enterInformationAbout description about
	= mkInteractiveTask description Information (makeInformationTask (Just about) undefGet (snd idBimap) Enter)
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !b -> Task (!ActionEvent, Maybe a) | descr d  & iTask a & iTask b & iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) undefGet (\v _ -> view v) actions Enter)

updateInformation :: !d a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description Information (makeInformationTask noAboutInfo (toSymmetricBimapGet (fst idBimap)) (snd idBimap) (LocalUpdate initial))

updateInformationA :: !d !(IBimap a v) ![TaskAction a] a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask v
updateInformationA description (bimapGet,bimapPutback) actions initial
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo (toSymmetricBimapGet bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationA :: !d !(IBimap a v) ![TaskAction a] !(Shared a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask v
updateSharedInformationA description (bimapGet,bimapPutback) actions shared
	= mkInteractiveTask description Information (makeInformationTaskA noAboutInfo (toSymmetricBimapGet bimapGet) bimapPutback actions (SharedUpdate shared))

updateInformationAbout :: !d !b a -> Task a | descr d & iTask a & iTask b
updateInformationAbout description about initial
	= mkInteractiveTask description Information (makeInformationTask (Just about) (toSymmetricBimapGet (fst idBimap)) (snd idBimap) (LocalUpdate initial))

updateInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask b & iTask v
updateInformationAboutA description (bimapGet,bimapPutback) actions about initial
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) (toSymmetricBimapGet bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b !(Shared a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask b & iTask v
updateSharedInformationAboutA description (bimapGet,bimapPutback) actions about shared
	= mkInteractiveTask description Information (makeInformationTaskA (Just about) (toSymmetricBimapGet bimapGet) bimapPutback actions (SharedUpdate shared))

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description options
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options Nothing)
		
enterChoiceA :: !d !(a -> v) ![TaskAction a] ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterChoiceA description view actions options
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options Nothing)

enterSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a]) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterSharedChoiceA description view actions shared
	= mkInteractiveTask description Information (makeSharedChoiceTask description noAboutInfo view actions shared Nothing)
				
updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a
updateChoice description options sel
	= mkInteractiveTask description Information (makeChoiceTask description noAboutInfo id options (Just sel))

updateChoiceA :: !d !(a -> v) ![TaskAction a] ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
updateChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description noAboutInfo view actions options (Just sel))

updateSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a]) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
updateSharedChoiceA description view actions shared sel
	= mkInteractiveTask description Information (makeSharedChoiceTask description noAboutInfo view actions shared (Just sel))
		
enterChoiceAbout :: !d !b ![a] -> Task a | descr d & iTask a & iTask b
enterChoiceAbout description about options
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options Nothing)
		
enterChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
enterChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options Nothing)

enterSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a]) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
enterSharedChoiceAboutA description view actions about shared
	= mkInteractiveTask description Information (makeSharedChoiceTask description (Just about) view actions shared Nothing)
		
updateChoiceAbout :: !d !b ![a] !Int -> Task a | descr d & iTask a & iTask b
updateChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeChoiceTask description (Just about) id options (Just sel))

updateChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
updateChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeChoiceTaskA description (Just about) view actions options (Just sel))

updateSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a]) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
updateSharedChoiceAboutA description view actions about shared sel
	= mkInteractiveTask description Information (makeSharedChoiceTask description (Just about) view actions shared (Just sel))

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options Nothing)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options Nothing)

enterSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a]) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
enterSharedMultipleChoiceA description view actions shared
	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask noAboutInfo view actions shared Nothing)
		
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask noAboutInfo id options (Just sel))

updateMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
updateMultipleChoiceA description view actions options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA noAboutInfo view actions options (Just sel))

updateSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a]) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
updateSharedMultipleChoiceA description view actions shared sel
	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask noAboutInfo view actions shared (Just sel))
		
enterMultipleChoiceAbout :: !d !b ![a] -> Task [a] | descr d & iTask a & iTask b
enterMultipleChoiceAbout description about options
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options Nothing)
		
enterMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
enterMultipleChoiceAboutA description view actions about options
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options Nothing)

enterSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a]) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
enterSharedMultipleChoiceAboutA description view actions about shared
	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask (Just about) view actions shared Nothing)
		
updateMultipleChoiceAbout :: !d !b ![a] ![Int] -> Task [a] | descr d & iTask a & iTask b
updateMultipleChoiceAbout description about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTask (Just about) id options (Just sel))

updateMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
updateMultipleChoiceAboutA description view actions about options sel
	= mkInteractiveTask description Information (makeMultipleChoiceTaskA (Just about) view actions options (Just sel))

updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a]) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
updateSharedMultipleChoiceAboutA description view actions about shared sel
	= mkInteractiveTask description Information (makeSharedMultipleChoiceTask (Just about) view actions shared (Just sel))

noAboutInfo :: Maybe Void
noAboutInfo = Nothing

makeInformationTask :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) !(InteractionTaskMode a) !*TSt -> (!TaskResult a,!*TSt) | iTask a & iTask v & iTask about
makeInformationTask mbContext bimapGet bimapPutback informationTaskMode tst
	# (result,tst) = makeInformationTaskA mbContext bimapGet bimapPutback [(ActionOk,ifvalid)] informationTaskMode tst
	= (mapTaskResult (fromJust o snd) result,tst)

makeInformationTaskA :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) ![TaskAction a] !(InteractionTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | iTask a & iTask v & iTask about
makeInformationTaskA mbContext bimapGet bimapPutback actions informationTaskMode tst
	# (result,tst) = makeInformationTaskAV mbContext bimapGet bimapPutback (mapTaskActionPredicates fst actions) informationTaskMode tst
	= (mapTaskResult (appSnd (fmap fst)) result,tst)

makeChoiceTask :: !d !(Maybe about) !(a -> v) ![a] !(Maybe Int) !*TSt -> (!TaskResult a,!*TSt) | descr d & iTask a & iTask v & iTask about
makeChoiceTask description _ _ [] _ tst
	= choiceException description tst
makeChoiceTask _ mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTask mbContext (toSymmetricBimapGet (mapOptions view)) (\v a -> setChoiceIndex (getChoiceIndex v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoice result,tst)

makeChoiceTaskA :: !d !(Maybe about) !(a -> v) ![TaskAction a] ![a] !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | descr d & iTask a & iTask v & iTask about
makeChoiceTaskA description _ _ _ [] _ tst
	= choiceException description tst
makeChoiceTaskA _ mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTaskA mbContext (toSymmetricBimapGet (mapOptions view)) (\v a -> setChoiceIndex (getChoiceIndex v) a) (mapTaskActionPredicates getChoice actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (appSnd (fmap getChoice)) result,tst)

makeSharedChoiceTask :: !d !(Maybe about) !(a -> v) ![TaskAction a] !(Shared [a]) !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent, !Maybe a),!*TSt) | descr d & iTask a & iTask v & iTask about
makeSharedChoiceTask description mbContext view actions shared mbSel tst
	# (opts,tst) = accIWorldTSt (readModel shared) tst
	| isEmpty opts
		= choiceException description tst
	| otherwise
		# viewOpts		= map view opts
		# initChoice = case mbSel of
			Just sel	= choiceSel viewOpts sel
			Nothing		= choice viewOpts
		# (result,tst)	= makeInformationTaskAV mbContext ((\opts choice -> appSnd not (setOptions (map view opts) choice)),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoiceFromModel actions) (SharedUpdate shared) tst
		= (mapTaskResult (appSnd (fmap getChoiceFromModel)) result,tst)
where
	getChoiceFromModel (opts,choice) = opts !! getChoiceIndex choice

choiceException description = applyTask (throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list"))
	
makeMultipleChoiceTask :: !(Maybe about) !(a -> v) ![a] !(Maybe [Int]) !*TSt -> (!TaskResult [a],!*TSt) | iTask a & iTask v & iTask about
makeMultipleChoiceTask mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTask mbContext (toSymmetricBimapGet (mapOptionsM view)) (\v a -> setChoiceIndexes (getChoiceIndexes v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoices result,tst)

makeMultipleChoiceTaskA :: !(Maybe about) !(a -> v) ![TaskAction [a]] ![a] !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent,!Maybe [a]),!*TSt) | iTask a & iTask v & iTask about
makeMultipleChoiceTaskA mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTaskA mbContext (toSymmetricBimapGet (mapOptionsM view)) (\v a -> setChoiceIndexes (getChoiceIndexes v) a) (mapTaskActionPredicates getChoices actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (appSnd (fmap getChoices)) result,tst)
	
makeSharedMultipleChoiceTask :: !(Maybe about) !(a -> v) ![TaskAction [a]] !(Shared [a]) !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent, !Maybe [a]),!*TSt) | iTask a & iTask v & iTask about
makeSharedMultipleChoiceTask mbContext view actions shared mbSel tst
	# (opts,tst)	= accIWorldTSt (readModel shared) tst
	# viewOpts		= map view opts
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel viewOpts sel
		Nothing		= multipleChoice viewOpts
	# (result,tst)	= makeInformationTaskAV mbContext (\opts choice -> (setOptionsM (map view opts) choice,False),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoicesFromModel actions) (SharedUpdate shared) tst
	= (mapTaskResult (appSnd (fmap getChoicesFromModel)) result,tst)
where
	getChoicesFromModel (opts,choice) = [opt \\ opt <- opts & i <- [0..] | isMember i (getChoiceIndexes choice)]

makeInformationTaskAV :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) ![TaskAction (a,v)] !(InteractionTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe (a,v)),!*TSt) | iTask a & iTask v & iTask about
makeInformationTaskAV mbContext bimapGet bimapPutback actions interactionTaskMode tst
	= makeInteractionTask (fmap aboutValue mbContext) id bimapGet bimapPutback actions interactionTaskMode tst

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description Message (makeMessageTask (noAboutMsg value))

showMessageA :: !d ![TaskAction a] a -> Task (!ActionEvent, a) | descr d & iTask a
showMessageA description actions value
	= mkInteractiveTask description Message (makeMessageTaskA (noAboutMsg value) id actions)

showMessageAbout :: !d !a -> Task a | descr d & iTask a
showMessageAbout description about
	= mkInteractiveTask description Message (makeMessageTask (aboutValueMsg about))

showMessageAboutA :: !d !(a -> v) ![TaskAction a] !a -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description Message (makeMessageTaskA (aboutValueMsg about) view actions)

showMessageShared :: !d !(a -> v) ![TaskAction a] !(Shared a) -> Task (!ActionEvent, a) | descr d & iTask a & iTask v & toReadOnlyShared Shared a
showMessageShared description view actions shared
	= mkInteractiveTask description Message (makeMessageTaskA (SharedAboutMsg shared) view actions)
	
showStickyMessage :: !d a -> Task a | descr d & iTask a
showStickyMessage description value
	= mkInteractiveTask description Message (makeMessageTaskSticky (noAboutMsg value) id)

showStickyMessageAbout :: !d !a -> Task a | descr d & iTask a
showStickyMessageAbout description about
	= mkInteractiveTask description Message (makeMessageTaskSticky (aboutValueMsg about) id)

showStickyMessageShared :: !d !(a -> v) !(Shared a) -> Task a | descr d & iTask a & iTask v & toReadOnlyShared Shared a
showStickyMessageShared description view shared
	= mkInteractiveTask description Message (makeMessageTaskSticky (SharedAboutMsg shared) view)

requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation description = mkInteractiveTask description Information requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = makeMessageTaskA (noAboutMsg Void) id [(ActionNo, always),(ActionYes, always)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !d !a -> Task Bool | descr d & iTask a
requestConfirmationAbout description about = mkInteractiveTask description Information requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = makeMessageTaskA (aboutValueMsg about) id [(ActionNo, always),(ActionYes, ifvalid)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)

:: AboutMsg shared a	= NoAboutMsg !a					// don't show value, only return as result
						| AboutValueMsg !a				// show about value
						| SharedAboutMsg !(shared a)	// show shared about value
					
noAboutMsg :: !a -> AboutMsg Shared a
noAboutMsg v = NoAboutMsg v

aboutValueMsg :: !a -> AboutMsg Shared a
aboutValueMsg v = AboutValueMsg v

makeMessageTask :: !(AboutMsg Shared a) !*TSt -> (!TaskResult a,!*TSt) | iTask a & toReadOnlyShared Shared a
makeMessageTask about tst
	# (result,tst) = makeMessageTaskA about id [(ActionOk,always)] tst
	= (mapTaskResult snd result,tst)
	
makeMessageTaskSticky :: !(AboutMsg Shared a) !(a -> v) !*TSt -> (!TaskResult a,!*TSt) | iTask a & iTask v & toReadOnlyShared Shared a
makeMessageTaskSticky about view tst
	# (result,tst) = makeMessageTaskA about view [] tst
	= (mapTaskResult snd result,tst)

makeMessageTaskA :: !(AboutMsg Shared a) !(a -> v) ![TaskAction a] !*TSt -> (!TaskResult (!ActionEvent, !a),!*TSt) | iTask a & iTask v & toReadOnlyShared Shared a
makeMessageTaskA about view actions tst
	# (result,tst) = makeInteractionTask mbAbout view (toSymmetricBimapGet bimapGet) bimapPutback (mapTaskActionPredicates fst actions) mode tst
	# (msgResult,tst) = case about of
		NoAboutMsg v		= (v,tst)
		AboutValueMsg v		= (v,tst)
		SharedAboutMsg ref	= accIWorldTSt (readModel ref) tst
	= (mapTaskResult (appSnd (const msgResult)) result,tst)
where
	mbAbout = case about of
		NoAboutMsg _		= Nothing
		AboutValueMsg v		= Just (AboutValue v)
		SharedAboutMsg ref	= Just (SharedAbout ref)
	mode = case about of
		NoAboutMsg v		= LocalUpdate v
		AboutValueMsg v		= LocalUpdate v
		SharedAboutMsg ref	= SharedUpdate ref
		
	bimapGet h					= Hidden h
	bimapPutback (Hidden h)	_	= h

showInstruction :: !String !instruction a -> Task a | html instruction & iTask a
showInstruction subject instruction value
	= mkInteractiveTask (subject,instruction) Instruction (makeInstructionTask noAboutInfo value)

showInstructionAbout :: !String !instruction a -> Task a | html instruction & iTask a
showInstructionAbout subject instruction context
	= mkInteractiveTask (subject,instruction) Instruction (makeInstructionTask (Just context) context)

makeInstructionTask :: !(Maybe about) !a !*TSt -> *(!TaskResult a,!*TSt) | iTask a & iTask about
makeInstructionTask context value tst
	# (result,tst) = makeInteractionTask (fmap mkAbout context) id (toSymmetricBimapGet (fst idBimap)) (snd idBimap) [(ActionOk,always)] (LocalUpdate Void) tst
	= (mapTaskResult (const value) result,tst)
where
	mkAbout :: !about -> About Shared about
	mkAbout a = AboutValue a
			
:: InteractionTaskMode a = Enter | LocalUpdate !a | SharedUpdate !(Shared a)
:: About shared a	= AboutValue !a
					| SharedAbout !(shared a)
					
noAbout :: Maybe (About Shared a)
noAbout = Nothing

aboutValue :: !a -> About Shared a
aboutValue v = AboutValue v

makeInteractionTask :: !(Maybe (About shared about)) !(about -> aboutV) ((a v -> (v,Bool)),v) !(v a -> a) ![TaskAction (a,v)] !(InteractionTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe (a,v)),!*TSt) | iTask a & iTask v & iTask about & iTask aboutV & toReadOnlyShared shared about
makeInteractionTask mbAbout aboutView (bimapGet,initView) bimapPutback actions informationTaskMode tst=:{taskNr, newTask, treeType}
	# tst						= if newTask (appIWorldTSt initTask tst) tst
	# (ovalue,tst)				= accIWorldTSt readValue tst
	# (oumask,tst)				= accIWorldTSt readUMask tst
	# (ovmask,tst)				= accIWorldTSt (readVMask ovalue oumask) tst
	# old						= (ovalue,oumask,ovmask)
	# (events,tst)				= getEvents tst
	# (localTimestamp,tst)		= accIWorldTSt getLocalTimestamp tst
	# (mbClientTimestamp,tst)	= clientTimestamp tst
	# (refresh,outdatedClient) = case mbClientTimestamp of
		Nothing
			// refresh if client did not sent timestamp
			= (True,False)
		Just clientTimestamp
			// refresh if client timestamp is older than local timestamp of the task or task is new
			# outdated = clientTimestamp < localTimestamp
			= (outdated || newTask,outdated)
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			// check for value event
			# (new=:(nvalue,_,vmask),tst) = case (valueEvent events,outdatedClient) of
				(Nothing,_)		// no value events
					= (old,tst)
				(_,True)		// ignore update events of outdated clients
					= (old,tst)
				(Just nvalue,_)
					// update view value
					# umask			= defaultMask nvalue
					# (vmask,tst)	= accIWorldTSt (verifyValue nvalue umask) tst
					# new			= (nvalue,umask,vmask)
					# tst			= appIWorldTSt (setStores new) tst
					// don't update model in enter mode
					# tst = case enterMode of
						False
							# ((oldModelValue,_),tst)	= accIWorldTSt readModelValue tst
							# newModelValue				= bimapPutback nvalue oldModelValue
							# tst						= appIWorldTSt (writeShared shared newModelValue) tst
							= tst
						True
							= tst
					= (new,tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					= handleActionEvent nvalue (isValidValue vmask) event tst
				Nothing
					// JSON representation is built after all possible changes of the model are done
					# tst = setJSONFunc (buildJSONValue new localTimestamp) tst
					= (TaskBusy,tst)
		UITree
			// check for edit events
			# edits = editEvents events
			# (rebuild,new=:(nvalue,numask,nvmask),errors,tst) = case (edits,outdatedClient) of
				(_,True)	// ignore update events of outdated clients & give error msg
					= (True,old,[(p,ErrorMessage "The client is outdated. The form was refreshed with the most recent value.") \\ (p,_) <- edits],tst)
				([],_)		// no edit events
					= (True,old,[],tst)
				_			// update edited view value
					# (nvalue,numask,tst)	= applyUpdates edits ovalue oumask tst
					# (nvmask,tst)			= accIWorldTSt (verifyValue nvalue numask) tst
					# new					= (nvalue,numask,nvmask)
					# tst					= appIWorldTSt (setStores new) tst
					# (conflict,tst)		= accIWorldTSt (isSharedChanged shared localTimestamp) tst
					| not enterMode
						| not conflict
							| isValidValue nvmask
								# ((oldModelValue,_),tst)	= accIWorldTSt readModelValue tst
								# newModelValue				= bimapPutback nvalue oldModelValue
								# tst						= appIWorldTSt (writeShared shared newModelValue) tst
								// rebuild value from model after also other possible changes are done
								= (True,new,[],tst)
							| otherwise
								// for updated invalid editors views are not rebuilt, updates are based on current value
								= (False,new,[],tst)
						| otherwise
							// task causes an edit conflict
							// don't update model, rebuild view based on current value of model and set errors
							= (True,old,[(p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.") \\ (p,_) <- edits],tst)
					| otherwise
						// in enter mode views not rebuilt, updates are based on current value
						= (False,new,[],tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					= handleActionEvent nvalue (isValidValue nvmask) event tst
				Nothing
					// UI is built after all possible changes of the model are done
					# tst = setTUIFunc (buildUI (ovalue,ovmask) new rebuild refresh localTimestamp errors (map fst edits)) tst
					= (TaskBusy,tst)
where
	// for local mode use auto generated store name, for shared mode use given store
	shared = case informationTaskMode of
		SharedUpdate shared	= shared
		_					= mkSharedReference ("iTask_" +++ taskNrToString taskNr +++ "-model")
	
	//Iinitialises the task the first time it is ran
	initTask :: !*IWorld -> *IWorld
	initTask iworld
		// auto generate model store if in local mode
		# iworld = case informationTaskMode of
			LocalUpdate initial
				= writeShared shared initial iworld
			_
				= iworld
		// determine initial view value based on model if not in enter mode
		| not enterMode
			# ((modelValue,modelTimestamp),iworld)	= readModelValue iworld
			# (nvalue,blank)						= bimapGet modelValue initView
			# numask								= if blank Blanked (defaultMask nvalue)
			# (nvmask,iworld)						= verifyValue nvalue numask iworld
			# iworld								= setStores (nvalue,numask,nvmask) iworld
			= iworld
		| otherwise
			= iworld
	
	handleActionEvent viewValue valid event tst
		# ((modelValue,_),tst) = accIWorldTSt readModelValue tst
		// delete auto generated model store
		# tst = case informationTaskMode of
			SharedUpdate _	= tst
			_				= appIWorldTSt (deleteShared shared) tst
		= (TaskFinished (event,if valid (Just (modelValue,viewValue)) Nothing),tst)
	
	/**
	* Builds the user interface for an information task AFTER the entire task tree is built.
	* All changes to shared models have to be done before.
	*
	* @param The view value before the current request.
	* @param The view value possibly updated by events.
	* @param Determines if a new view value is build using the current model and the bimap get function.
	* @param Determines if a new UI definition is computed or the existing one is updated.
	* @param The timestamp of the local value before the current request.
	* @param Error messages added to the rebuilt value.
	* @param Datapaths of values updates by an event
	* @param IWorld
	*
	* @return A tree node containing the computed UI definition/updates.
	*/
	buildUI old new rebuild refresh localTimestamp errors updatedPaths iworld
		# ((modelValue,modelTimestamp), iworld)	= readModelValue iworld
		// check for changed model value
		# (modelChanged,iworld)					= isSharedChanged shared localTimestamp iworld
		// determine new view value if model is changed, rebuild is requested & not in enter mode
		# ((rvalue,_,rvmask),iworld) = case modelChanged && rebuild && not enterMode of
			True								= updateViewValue bimapGet new modelValue modelTimestamp errors iworld
			False								= (app3 (id,id,setInvalid errors) new,iworld)
		# evalActions							= evaluateConditions actions (isValidValue rvmask) (modelValue,rvalue)
		# editorId								= "tf-" +++ taskNrToString taskNr
		# iworld								= storeErrors errors iworld
		| refresh	// refresh UI, send new def instead of updates
			# form 								= visualizeAsEditor editorId rvalue rvmask
			# (about,iworld) = case mbAbout of
				Nothing							= (Nothing,iworld)
				Just (AboutValue a)				= (Just (visualizeAsHtmlDisplay (aboutView a)),iworld)
				Just (SharedAbout ref)			= appFst (Just o visualizeAsHtmlDisplay o aboutView) (readModel ref iworld)
			= (Definition (taskPanel (taskNrToString taskNr) about (Just form)) evalActions,iworld)
		| otherwise	// update UI
			// get stored old errors
			# (oldErrors,iworld)				= getErrors taskNr iworld
			# old								= appSnd (setInvalid oldErrors) old
			# updates							= determineEditorUpdates editorId old (rvalue,rvmask) updatedPaths
			= (Updates updates evalActions,iworld)

	buildJSONValue new=:(nvalue,_,_) localTimestamp iworld
		# ((modelValue,modelTimestamp),iworld)	= readModelValue iworld
		// check for changed model value
		# (modelChanged,iworld)					= isSharedChanged shared localTimestamp iworld
		// determine new view value if model is changed & not in enter mode
		# (rvalue,iworld) = case modelChanged && not enterMode of
			True								= appFst fst3 (updateViewValue bimapGet new modelValue modelTimestamp [] iworld)
			False								= (nvalue,iworld)
		= (toJSON rvalue,iworld)
					
	// determines a new view value from model
	updateViewValue :: !(a v -> (!v,!Bool)) (!v,!UpdateMask,!VerifyMask) !a !Timestamp ![(!DataPath,!ErrorMessage)] !*IWorld -> (!(v,UpdateMask,VerifyMask),!*IWorld) | iTask a & iTask v
	updateViewValue bimapGet view=:(viewValue,_,_) modelValue modelTimestamp errors iworld
		# (nvalue,blank) = bimapGet modelValue viewValue
		// only calculate new view value if 'get (put v m) <> v' or if mask is blanked
		| viewValue =!= nvalue || blank
			# numask			= if blank Blanked (defaultMask nvalue)
			# (nvmask,iworld)	= verifyValue nvalue numask iworld
			# nvmask			= setInvalid errors nvmask
			# new				= (nvalue,numask,nvmask)
			# iworld			= setStores new iworld
			= (new,iworld)
		| otherwise
			# iworld			= setTaskStoreFor taskNr "value" viewValue iworld // set value to update timestamp
			= (app3 (id,id,setInvalid errors) view,iworld)
					
	readValue iworld
		# (mbvalue,iworld)	= getTaskStoreFor taskNr "value" iworld
		= case mbvalue of
			Just v
				= (v,iworld)
			Nothing
				# (v,iworld)	= defaultValue iworld
				// store default value because store is used to determine local timestamp next time
				# iworld		= setTaskStoreFor taskNr "value" v iworld
				= (v,iworld)
							
	readUMask iworld
		# (mbmask,iworld) = getTaskStoreFor taskNr "umask" iworld
		= case mbmask of
			Just m	= (m,iworld)
			Nothing	= (Untouched,iworld)
			
	readVMask value umask iworld
		# (mbmask,iworld) = getTaskStoreFor taskNr "vmask" iworld
		= case mbmask of
			Just m
				= (m,iworld)
			Nothing
				# (vmask,iworld)	= verifyValue value umask iworld
				# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
				= (vmask,iworld)
				
	setStores(value,umask,vmask) iworld
		# iworld			= setTaskStoreFor taskNr "value" value iworld
		# iworld			= setTaskStoreFor taskNr "umask" umask iworld
		# iworld			= setTaskStoreFor taskNr "vmask" vmask iworld
		= iworld
			
	getLocalTimestamp iworld=:{IWorld|timestamp}
		# (mbTimestamp,iworld) = getTaskStoreTimestampFor taskNr "value" iworld
		= case mbTimestamp of
			Just timestamp	= (timestamp,iworld)
			Nothing			= (timestamp,iworld)
	
	readModelValue iworld
		| enterMode // don't read model in enter mode, but compute from view
			# (view,iworld)				= readValue iworld
			# (localTimestamp,iworld)	= getLocalTimestamp iworld
			= ((bimapPutback view undef,localTimestamp),iworld)
		| otherwise
			# (mbValue,iworld) = readSharedAndTimestamp shared iworld
			= case mbValue of
				Just v	= (v, iworld)
				Nothing	= abort "readModelValue: shared model deleted!"
	
	// Gets errors if stored (otherwise return empty error list)
	getErrors taskNr iworld
		# (mbErrors,iworld) = getTaskStoreFor taskNr "errors" iworld
		= case mbErrors of
			Just errors	= (map (appFst dataPathFromList) errors,iworld)
			Nothing		= ([],iworld)
			
	// Store errors if necessary
	storeErrors errors iworld
		// Only store error if store already exists or error are not empty
		# (mbErrors,iworld) = checkErrorStore iworld
		# store = case mbErrors of
			Just _	= True
			Nothing	= not (isEmpty errors)
		| store
			= setTaskStoreFor taskNr "errors" (map (appFst dataPathList) errors) iworld
		| otherwise
			= iworld
	where
		checkErrorStore :: !*IWorld -> (!Maybe [([Int],ErrorMessage)],!*IWorld)
		checkErrorStore tst = getTaskStoreFor taskNr "errors" tst
							
	applyUpdates [] val umask tst = (val,umask,tst)
	applyUpdates [(p,v):us] val umask tst=:{TSt|iworld}
		# (val,umask,iworld) = updateValueAndMask p v val umask iworld
		= applyUpdates us val umask {TSt|tst & iworld = iworld}
		
	clientTimestamp :: !*TSt -> (!Maybe Timestamp,!*TSt)
	clientTimestamp tst=:{request}
		# ts = paramValue "timestamp" request
		| ts <> ""	= (Just (Timestamp (toInt ts)),tst)
		| otherwise	= (Nothing,tst)
		
	enterMode = case informationTaskMode of
		Enter	= True
		_		= False
	
//Applies given function to the result if task is finished
mapTaskResult :: !(a -> b) !(TaskResult a) -> TaskResult b
mapTaskResult f (TaskFinished x)	= TaskFinished (f x) 
mapTaskResult f (TaskBusy)			= TaskBusy
mapTaskResult f (TaskException e)	= TaskException e

//Edit events of which the name is a datapath
editEvents :: [(String,JSONNode)] -> [(DataPath,String)]
editEvents events = [(s2dp name,value) \\ (name,JSONString value) <- events | isdps name]

//Check if there is an action event among the events 
actionEvent :: ![(!String,!JSONNode)] ![TaskAction a] -> Maybe ActionEvent
actionEvent events actions	
	= case [value \\ (name,value) <- events | name == "action"] of
		[JSONString key]							= addData "" (mbAction key)
		[JSONArray [JSONString key,JSONString data]]= addData data (mbAction key)
		_											= Nothing
where
	mbAction key = case [action \\ (action,pred) <- actions | actionName action == key] of
		[action]	= Just action
		_			= Nothing
		
	addData data (Just action)	= Just (action,data)
	addData data Nothing		= Nothing
	
//Check if there is a value event among the events
valueEvent :: ![(!String,!JSONNode)] -> Maybe a | JSONDecode{|*|} a
valueEvent events
	= case [value \\ (name,value) <- events | name == "value"] of
		[value]	= fromJSON value
		_		= Nothing

//Evaluate action's conditions
evaluateConditions :: ![(!Action, (Verified a) -> Bool)] !Bool !a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,evaluateCondition cond valid value) \\ (action,cond) <- actions]
where
	evaluateCondition :: !((Verified a) -> Bool) !Bool !a -> Bool
	evaluateCondition pred valid value = pred (if valid (Valid value) Invalid)

//Build TUI definition for task with given context/form	
taskPanel :: String (Maybe HtmlTag) (Maybe [TUIDef]) -> [TUIDef]
taskPanel taskid mbContext mbForm =
	(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
	(case mbForm of Just form = form; Nothing = [])
where			
	taskContextPanel :: !String !HtmlTag -> TUIDef
	taskContextPanel panelid context = TUIHtmlContainer
										{ TUIHtmlContainer
										| id = panelid
										, html = toString context
										, fieldLabel = Nothing
										}

//Changes all predicates on values of type a to predicates on values of type b										
mapTaskActionPredicates :: !(b -> a) ![TaskAction a] -> [TaskAction b]
mapTaskActionPredicates vMap actions = map changePrecicate actions
where
	changePrecicate (action,pred) = (action,newPred pred)
	newPred pred v = case v of
		Invalid	= pred Invalid
		Valid b	= pred (Valid (vMap b))

readModel :: !(shared a) !*IWorld -> (!a,!*IWorld) | JSONDecode{|*|}, TC a & toReadOnlyShared shared a
readModel shared iworld
	# (mbVal,iworld) = readShared shared iworld
	= case mbVal of
		Just val	= (val,iworld)
		Nothing		= abort "readModel: shared model deleted!"
		
//Convert an asymmetic to an symmetic bimap-get function
toSymmetricBimapGet :: !(a -> v) -> ((a v -> (v,Bool)),v)
toSymmetricBimapGet get = ((\a _ -> (get a,False)),undefInitViewV)

undefGet = (abort "undefined bimap-get function",undefInitViewV)
undefInitViewV = abort "undefined initial view value"

