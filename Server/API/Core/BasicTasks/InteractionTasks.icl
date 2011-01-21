implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Shared, Util, Html, Text, Http, TSt, Store, ExceptionCombinators
from StdFunc import id, const, o
from HtmlUtil import paramValue

derive JSONEncode UpdateMask, ErrorMessage
derive JSONDecode UpdateMask, ErrorMessage
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
	= mkInteractiveTask description (makeInformationTask noAbout undefGet (snd idBimap) Enter)
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description (makeInformationTaskA noAbout undefGet (\v _ -> view v) actions Enter)
		
enterInformationAbout :: !d !b -> Task a | descr d  & iTask a & iTask b
enterInformationAbout description about
	= mkInteractiveTask description (makeInformationTask (Just about) undefGet (snd idBimap) Enter)
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !b -> Task (!ActionEvent, Maybe a) | descr d  & iTask a & iTask b & iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description (makeInformationTaskA (Just about) undefGet (\v _ -> view v) actions Enter)

undefGet = (abort "undefined bimap-get function",abort "undefined initial view value")

updateInformation :: !d a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description (makeInformationTask noAbout (toExtendedBimapGet (fst idBimap)) (snd idBimap) (LocalUpdate initial))

updateInformationA :: !d !(IBimap a v) ![TaskAction a] a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask v
updateInformationA description (bimapGet,bimapPutback) actions initial
	= mkInteractiveTask description (makeInformationTaskA noAbout (toExtendedBimapGet bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationA :: !d !(IBimap a v) ![TaskAction a] !(Shared a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask v
updateSharedInformationA description (bimapGet,bimapPutback) actions shared
	= mkInteractiveTask description (makeInformationTaskA noAbout (toExtendedBimapGet bimapGet) bimapPutback actions (SharedUpdate shared))

updateInformationAbout :: !d !b a -> Task a | descr d & iTask a & iTask b
updateInformationAbout description about initial
	= mkInteractiveTask description (makeInformationTask (Just about) (toExtendedBimapGet (fst idBimap)) (snd idBimap) (LocalUpdate initial))

updateInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask b & iTask v
updateInformationAboutA description (bimapGet,bimapPutback) actions about initial
	= mkInteractiveTask description (makeInformationTaskA (Just about) (toExtendedBimapGet bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b !(Shared a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask b & iTask v
updateSharedInformationAboutA description (bimapGet,bimapPutback) actions about shared
	= mkInteractiveTask description (makeInformationTaskA (Just about) (toExtendedBimapGet bimapGet) bimapPutback actions (SharedUpdate shared))

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description options
	= mkInteractiveTask description (makeChoiceTask description noAbout id options Nothing)
		
enterChoiceA :: !d !(a -> v) ![TaskAction a] ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterChoiceA description view actions options
	= mkInteractiveTask description (makeChoiceTaskA description noAbout view actions options Nothing)

enterSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a]) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v & gEq{|*|} v
enterSharedChoiceA description view actions shared
	= mkInteractiveTask description (makeSharedChoiceTask description noAbout view actions shared Nothing)
				
updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a
updateChoice description options sel
	= mkInteractiveTask description (makeChoiceTask description noAbout id options (Just sel))

updateChoiceA :: !d !(a -> v) ![TaskAction a] ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
updateChoiceA description view actions options sel
	= mkInteractiveTask description (makeChoiceTaskA description noAbout view actions options (Just sel))

updateSharedChoiceA :: !d !(a -> v) ![TaskAction a] !(Shared [a]) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v & gEq{|*|} v
updateSharedChoiceA description view actions shared sel
	= mkInteractiveTask description (makeSharedChoiceTask description noAbout view actions shared (Just sel))
		
enterChoiceAbout :: !d !b ![a] -> Task a | descr d & iTask a & iTask b
enterChoiceAbout description about options
	= mkInteractiveTask description (makeChoiceTask description (Just about) id options Nothing)
		
enterChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
enterChoiceAboutA description view actions about options
	= mkInteractiveTask description (makeChoiceTaskA description (Just about) view actions options Nothing)

enterSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a]) -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v & gEq{|*|} v
enterSharedChoiceAboutA description view actions about shared
	= mkInteractiveTask description (makeSharedChoiceTask description (Just about) view actions shared Nothing)
		
updateChoiceAbout :: !d !b ![a] !Int -> Task a | descr d & iTask a & iTask b
updateChoiceAbout description about options sel
	= mkInteractiveTask description (makeChoiceTask description (Just about) id options (Just sel))

updateChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
updateChoiceAboutA description view actions about options sel
	= mkInteractiveTask description (makeChoiceTaskA description (Just about) view actions options (Just sel))

updateSharedChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b !(Shared [a]) !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v & gEq{|*|} v
updateSharedChoiceAboutA description view actions about shared sel
	= mkInteractiveTask description (makeSharedChoiceTask description (Just about) view actions shared (Just sel))

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options
	= mkInteractiveTask description (makeMultipleChoiceTask noAbout id options Nothing)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options
	= mkInteractiveTask description (makeMultipleChoiceTaskA noAbout view actions options Nothing)

enterSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a]) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v & gEq{|*|} v
enterSharedMultipleChoiceA description view actions shared
	= mkInteractiveTask description (makeSharedMultipleChoiceTask noAbout view actions shared Nothing)
		
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options sel
	= mkInteractiveTask description (makeMultipleChoiceTask noAbout id options (Just sel))

updateMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
updateMultipleChoiceA description view actions options sel
	= mkInteractiveTask description (makeMultipleChoiceTaskA noAbout view actions options (Just sel))

updateSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] !(Shared [a]) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v & gEq{|*|} v
updateSharedMultipleChoiceA description view actions shared sel
	= mkInteractiveTask description (makeSharedMultipleChoiceTask noAbout view actions shared (Just sel))
		
enterMultipleChoiceAbout :: !d !b ![a] -> Task [a] | descr d & iTask a & iTask b
enterMultipleChoiceAbout description about options
	= mkInteractiveTask description (makeMultipleChoiceTask (Just about) id options Nothing)
		
enterMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
enterMultipleChoiceAboutA description view actions about options
	= mkInteractiveTask description (makeMultipleChoiceTaskA (Just about) view actions options Nothing)

enterSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a]) -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v & gEq{|*|} v
enterSharedMultipleChoiceAboutA description view actions about shared
	= mkInteractiveTask description (makeSharedMultipleChoiceTask (Just about) view actions shared Nothing)
		
updateMultipleChoiceAbout :: !d !b ![a] ![Int] -> Task [a] | descr d & iTask a & iTask b
updateMultipleChoiceAbout description about options sel
	= mkInteractiveTask description (makeMultipleChoiceTask (Just about) id options (Just sel))

updateMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
updateMultipleChoiceAboutA description view actions about options sel
	= mkInteractiveTask description (makeMultipleChoiceTaskA (Just about) view actions options (Just sel))

updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b !(Shared [a]) ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v & gEq{|*|} v
updateSharedMultipleChoiceAboutA description view actions about shared sel
	= mkInteractiveTask description (makeSharedMultipleChoiceTask (Just about) view actions shared (Just sel))

noAbout :: Maybe Void
noAbout = Nothing

:: InformationTaskMode a = Enter | LocalUpdate !a | SharedUpdate !(Shared a)

makeInformationTask :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) !(InformationTaskMode a) !*TSt -> (!TaskResult a,!*TSt) | iTask a & iTask v & gVisualize{|*|} about
makeInformationTask mbContext bimapGet bimapPutback informationTaskMode tst
	# (result,tst) = makeInformationTaskA mbContext bimapGet bimapPutback [(ActionOk,ifvalid)] informationTaskMode tst
	= (mapTaskResult (fromJust o snd) result,tst)

makeInformationTaskA :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) ![TaskAction a] !(InformationTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | iTask a & iTask v & gVisualize{|*|} about
makeInformationTaskA mbContext bimapGet bimapPutback actions informationTaskMode tst
	# (result,tst) = makeInformationTaskAV mbContext bimapGet bimapPutback (mapTaskActionPredicates fst actions) informationTaskMode tst
	= (mapTaskResult (app2 (id,mapMaybe fst)) result,tst)

makeChoiceTask :: !d !(Maybe about) !(a -> v) ![a] !(Maybe Int) !*TSt -> (!TaskResult a,!*TSt) | descr d & iTask a & iTask v & gVisualize{|*|} about
makeChoiceTask description _ _ [] _ tst
	= choiceException description tst
makeChoiceTask _ mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTask mbContext (toExtendedBimapGet (mapOptions view)) (\v a -> setChoiceIndex (getChoiceIndex v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoice result,tst)

makeChoiceTaskA :: !d !(Maybe about) !(a -> v) ![TaskAction a] ![a] !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | descr d & iTask a & iTask v & gVisualize{|*|} about
makeChoiceTaskA description _ _ _ [] _ tst
	= choiceException description tst
makeChoiceTaskA _ mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= choiceSel opts sel
		Nothing		= choice opts
	# (result,tst)	= makeInformationTaskA mbContext (toExtendedBimapGet (mapOptions view)) (\v a -> setChoiceIndex (getChoiceIndex v) a) (mapTaskActionPredicates getChoice actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (app2 (id,mapMaybe getChoice)) result,tst)

makeSharedChoiceTask :: !d !(Maybe about) !(a -> v) ![TaskAction a] !(Shared [a]) !(Maybe Int) !*TSt -> (!TaskResult (!ActionEvent, !Maybe a),!*TSt) | descr d & iTask a & iTask v & gEq{|*|} v & gVisualize{|*|} about
makeSharedChoiceTask description mbContext view actions shared mbSel tst
	# (opts,tst) = readModel shared tst
	| isEmpty opts
		= choiceException description tst
	| otherwise
		# viewOpts		= map view opts
		# initChoice = case mbSel of
			Just sel	= choiceSel viewOpts sel
			Nothing		= choice viewOpts
		# (result,tst)	= makeInformationTaskAV mbContext ((\opts choice -> app2 (id,not) (setOptions (map view opts) choice)),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoiceFromModel actions) (SharedUpdate shared) tst
		= (mapTaskResult (app2 (id,mapMaybe getChoiceFromModel)) result,tst)
where
	getChoiceFromModel (opts,choice) = opts !! getChoiceIndex choice

choiceException description = applyTask (throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list"))
	
makeMultipleChoiceTask :: !(Maybe about) !(a -> v) ![a] !(Maybe [Int]) !*TSt -> (!TaskResult [a],!*TSt) | iTask a & iTask v & gVisualize{|*|} about
makeMultipleChoiceTask mbContext view opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTask mbContext (toExtendedBimapGet (mapOptionsM view)) (\v a -> setChoiceIndexes (getChoiceIndexes v) a) (LocalUpdate initChoice) tst
	= (mapTaskResult getChoices result,tst)

makeMultipleChoiceTaskA :: !(Maybe about) !(a -> v) ![TaskAction [a]] ![a] !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent,!Maybe [a]),!*TSt) | iTask a & iTask v & gVisualize{|*|} about
makeMultipleChoiceTaskA mbContext view actions opts mbSel tst
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel opts sel
		Nothing		= multipleChoice opts
	# (result,tst)	= makeInformationTaskA mbContext (toExtendedBimapGet (mapOptionsM view)) (\v a -> setChoiceIndexes (getChoiceIndexes v) a) (mapTaskActionPredicates getChoices actions) (LocalUpdate initChoice) tst
	= (mapTaskResult (app2 (id,mapMaybe getChoices)) result,tst)
	
makeSharedMultipleChoiceTask :: !(Maybe about) !(a -> v) ![TaskAction [a]] !(Shared [a]) !(Maybe [Int]) !*TSt -> (!TaskResult (!ActionEvent, !Maybe [a]),!*TSt) | iTask a & iTask v & gVisualize{|*|} about & gEq{|*|} v
makeSharedMultipleChoiceTask mbContext view actions shared mbSel tst
	# (opts,tst)	= readModel shared tst
	# viewOpts		= map view opts
	# initChoice = case mbSel of
		Just sel	= multipleChoiceSel viewOpts sel
		Nothing		= multipleChoice viewOpts
	# (result,tst)	= makeInformationTaskAV mbContext (\opts choice -> (setOptionsM (map view opts) choice,False),initChoice) (\_ a -> a) (mapTaskActionPredicates getChoicesFromModel actions) (SharedUpdate shared) tst
	= (mapTaskResult (app2 (id,mapMaybe getChoicesFromModel)) result,tst)
where
	getChoicesFromModel (opts,choice) = [opt \\ opt <- opts & i <- [0..] | isMember i (getChoiceIndexes choice)]

makeInformationTaskAV :: !(Maybe about) ((a v -> (v,Bool)),v) !(v a -> a) ![TaskAction (a,v)] !(InformationTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe (a,v)),!*TSt) | iTask a & iTask v & gVisualize{|*|} about
makeInformationTaskAV mbContext (bimapGet,initView) bimapPutback actions informationTaskMode tst=:{taskNr, newTask, treeType}
	# tst						= if newTask (initTask tst) tst
	# (ovalue,tst)				= readValue tst
	# (oumask,tst)				= readMask tst
	# (ovmask,tst)				= accIWorldTSt (verifyValue ovalue oumask) tst
	# old						= (ovalue,oumask,ovmask)
	# (events,tst)				= getEvents tst
	# (localTimestamp,tst)		= getLocalTimestamp tst
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
			# (nvalue,numask,tst) = case (valueEvent events,outdatedClient) of
				(Nothing,_)		// no value events
					= (ovalue,oumask,tst)
				(_,True)		// ignore update events of outdated clients
					= (ovalue,oumask,tst)
				(Just nvalue,_)
					// update view value
					# (nmask,tst)	= accIWorldTSt (defaultMask nvalue) tst
					# tst			= setTaskStore "value" nvalue tst
					# tst			= setTaskStore "mask" nmask tst
					// don't update model in enter mode
					# tst = case enterMode of
						False
							# ((oldModelValue,_),tst)	= readModelValue tst
							# newModelValue				= bimapPutback nvalue oldModelValue
							# tst						= appIWorldTSt (writeShared shared newModelValue) tst
							= tst
						True
							= tst
					= (nvalue,nmask,tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					# (nvmask,tst) = accIWorldTSt (verifyValue nvalue numask) tst
					= handleActionEvent nvalue (isValidValue nvmask) event tst
				Nothing
					// JSON representation is built after all possible changes of the model are done
					# tst = setJSONFunc (buildJSONValue nvalue localTimestamp) tst
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
					# tst					= setTaskStore "value" nvalue tst
					# tst					= setTaskStore "mask" numask tst
					# (nvmask,tst)			= accIWorldTSt (verifyValue nvalue numask) tst
					= case isValidValue nvmask && not enterMode of
						True	// if view is valid (and not in enter mode) also try to update model
							// check if the task causes an editing conflict
							# (conflict,tst) = accIWorldTSt (isSharedChanged shared localTimestamp) tst
							= case conflict of
								False	// no conflict, update model
									# ((oldModelValue,_),tst)	= readModelValue tst
									# newModelValue				= bimapPutback nvalue oldModelValue
									# tst						= appIWorldTSt (writeShared shared newModelValue) tst
									// rebuild value from model after also other possible changes are done
									= (True,(nvalue,numask,nvmask),[],tst)
								True
									// don't update model, rebuild view based on current value of model and set errors
									= (True,old,[(p,ErrorMessage "An edit conflict occurred. The field was reset to the most recent value.") \\ (p,_) <- edits],tst)
						False	// edited invalid views (or if in enter mode) are not rebuilt, updates are based on current value
							= (False,(nvalue,numask,nvmask),[],tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					= handleActionEvent nvalue (isValidValue nvmask) event tst
				Nothing
					// UI is built after all possible changes of the model are done
					# tst = setTUIFunc (buildUI old new rebuild refresh localTimestamp errors) tst
					= (TaskBusy,tst)
where
	// for local mode use auto generated store name, for shared mode use given store
	shared = case informationTaskMode of
		SharedUpdate shared	= shared
		_					= mkSharedReference ("iTask_" +++ taskNrToString taskNr +++ "-model")
	
	//Iinitialises the task the first time it is run
	initTask tst
		// auto generate model store if in local mode
		# tst = case informationTaskMode of
			LocalUpdate initial
				= appIWorldTSt (writeShared shared initial) tst
			_
				= tst
		// determine initial view value based on model if not in enter mode
		| not enterMode	
			# ((modelValue,modelTimestamp),tst)	= readModelValue tst
			= snd (updateViewValue bimapGet initView modelValue modelTimestamp [] tst)
		| otherwise
			= tst
	
	handleActionEvent viewValue valid event tst
		# ((modelValue,_), tst)	= readModelValue tst
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
	* @param TSt
	*
	* @return A tree node containing the computed UI definition/updates.
	*/
	buildUI old new=:(nvalue,numask,nvmask) rebuild refresh localTimestamp errors tst
		# ((modelValue,modelTimestamp), tst)	= readModelValue tst
		// check for changed model value
		# (modelChanged,tst)					= accIWorldTSt (isSharedChanged shared localTimestamp) tst
		// determine new view value if model is changed, rebuild is requested & not in enter mode
		# (rebuilded,tst) = case modelChanged && rebuild && not enterMode of
			True								= updateViewValue bimapGet nvalue modelValue modelTimestamp errors tst
			False								= (app3 (id,id,setInvalid errors) new,tst)
		# (rvalue,rumask,rvmask)				= rebuilded
		# evalActions							= evaluateConditions actions (isValidValue rvmask) (modelValue,rvalue)
		# editorId								= "tf-" +++ taskNrToString taskNr
		| refresh	// refresh UI, send new def instead of updates
			# form 								= visualizeAsEditor editorId rvalue rumask rvmask
			# tst								= storeErrors errors tst
			= (Definition (taskPanel (taskNrToString taskNr) (mapMaybe visualizeAsHtmlDisplay mbContext) (Just form)) evalActions,tst)
		| otherwise	// update UI
			// get stored old errors
			# (oldErrors,tst)					= getErrors taskNr tst
			# old								= app3 (id,id,setInvalid oldErrors) old
			# updates							= determineEditorUpdates editorId old rebuilded
			# tst	 							= storeErrors errors tst
			= (Updates updates evalActions,tst)
	
	buildJSONValue nvalue localTimestamp tst
		# ((modelValue,modelTimestamp), tst)	= readModelValue tst
		// check for changed model value
		# (modelChanged,tst)					= accIWorldTSt (isSharedChanged shared localTimestamp) tst
		// determine new view value if model is changed & not in enter mode
		# (rvalue,tst) = case modelChanged && not enterMode of
			True								= app2 (fst3,id) (updateViewValue bimapGet nvalue modelValue modelTimestamp [] tst)
			False								= (nvalue,tst)
		= (toJSON rvalue,tst)
					
	// determines a new view value from model
	updateViewValue :: !(a v -> (v,Bool)) v !a !Timestamp ![(DataPath,ErrorMessage)] !*TSt -> (!(v,UpdateMask,VerifyMask),!*TSt) | iTask a & iTask v
	updateViewValue bimapGet viewValue modelValue modelTimestamp errors tst
		# (nvalue,blank)	= bimapGet modelValue viewValue
		# (numask,tst) = case blank of
			False			= accIWorldTSt (defaultMask nvalue) tst
			True			= (Blanked True,tst)
		# (nvmask,tst)		= accIWorldTSt (verifyValue nvalue numask) tst
		# nvmask			= setInvalid errors nvmask
		# tst				= setTaskStoreFor taskNr "value" nvalue tst
		# tst				= setTaskStoreFor taskNr "mask" numask tst
		= ((nvalue,numask,nvmask),tst)
					
	readValue tst
		# (mbvalue,tst)	= getTaskStoreFor taskNr "value" tst
		= case mbvalue of
			Just v
				= (v,tst)
			Nothing
				# (v,tst)	= accIWorldTSt defaultValue tst
				// store default value because store is used to determine local timestamp next time
				# tst		= setTaskStoreFor taskNr "value" v tst
				= (v,tst)
							
	readMask tst
		# (mbmask,tst)	= getTaskStoreFor taskNr "mask" tst
		= case mbmask of
			Just m	= (m,tst)
			Nothing	= (Untouched,tst)
			
	getLocalTimestamp tst=:{TSt|iworld=iworld=:{IWorld|timestamp}}
		# (mbTimestamp,tst) = getTaskStoreTimestampFor taskNr "value" tst
		= case mbTimestamp of
			Just timestamp	= (timestamp,tst)
			Nothing			= (timestamp,tst)
	
	readModelValue tst
		| enterMode // don't read model in enter mode, but compute from view
			# (view,tst)			= readValue tst
			# (localTimestamp,tst)	= getLocalTimestamp tst
			= ((bimapPutback view undef,localTimestamp),tst)
		| otherwise
			# (mbValue,tst) = accIWorldTSt (readSharedAndTimestamp shared) tst
			= case mbValue of
				Just v	= (v, tst)
				Nothing	= abort "readModelValue: shared model deleted!"
	
	// Gets errors if stored (otherwise return empty error list)
	getErrors taskNr tst
		# (mbErrors,tst) = getTaskStoreFor taskNr "errors" tst
		= case mbErrors of
			Just errors	= (map (app2 (dataPathFromList,id)) errors,tst)
			Nothing		= ([],tst)
			
	// Store errors if necessary
	storeErrors errors tst
		// Only store error if store already exists or error are not empty
		# (mbErrors,tst) = checkErrorStore tst
		# store = case mbErrors of
			Just _	= True
			Nothing	= not (isEmpty errors)
		| store
			= setTaskStoreFor taskNr "errors" (map (app2 (dataPathList,id)) errors) tst
		| otherwise
			= tst
	where
		checkErrorStore :: !*TSt -> (!Maybe [([Int],ErrorMessage)],!*TSt)
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

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description (makeMessageTask (msgNoAbout value))

showMessageA :: !d ![TaskAction a] a -> Task (!ActionEvent, a) | descr d & iTask a
showMessageA description actions value
	= mkInteractiveTask description (makeMessageTaskA (msgNoAbout value) id actions)

showMessageAbout :: !d !a -> Task a | descr d & iTask a
showMessageAbout description about
	= mkInteractiveTask description (makeMessageTask (aboutValue about))

showMessageAboutA :: !d !(a -> v) ![TaskAction a] !a -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description (makeMessageTaskA (aboutValue about) view actions)

showMessageShared :: !d !(a -> v) ![TaskAction a] !(shared a) -> Task (!ActionEvent, a) | descr d & iTask a & iTask v & toReadOnlyShared shared a
showMessageShared description view actions shared
	= mkInteractiveTask description (makeMessageTaskA (SharedAbout shared) view actions)
	
showStickyMessage :: !d a -> Task a | descr d & iTask a
showStickyMessage description value
	= mkInteractiveTask description (makeMessageTaskSticky (msgNoAbout value) id)

showStickyMessageAbout :: !d !a -> Task a | descr d & iTask a
showStickyMessageAbout description about
	= mkInteractiveTask description (makeMessageTaskSticky (aboutValue about) id)

showStickyMessageShared :: !d !(a -> v) !(shared a) -> Task a | descr d & iTask a & iTask v & toReadOnlyShared shared a
showStickyMessageShared description view shared
	= mkInteractiveTask description (makeMessageTaskSticky (SharedAbout shared) view)

requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation description = mkInteractiveTask description requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = makeMessageTaskA (msgNoAbout Void) id [(ActionNo, always),(ActionYes, always)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !d !a -> Task Bool | descr d & iTask a
requestConfirmationAbout description about = mkInteractiveTask description requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = makeMessageTaskA (aboutValue about) id [(ActionNo, always),(ActionYes, ifvalid)] tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)

:: About shared a	= NoAbout !a				// don't show value, only return as result
					| AboutValue !a				// show about value
					| SharedAbout !(shared a)	// show shared about value
					
msgNoAbout :: !a -> About Shared a
msgNoAbout v = NoAbout v

aboutValue :: !a -> About Shared a
aboutValue v = AboutValue v

makeMessageTask :: !(About shared a) !*TSt -> (!TaskResult a,!*TSt) | iTask a & toReadOnlyShared shared a
makeMessageTask about tst
	# (result,tst) = makeMessageTaskA about id [(ActionOk, ifvalid)] tst
	= (mapTaskResult snd result,tst)
	
makeMessageTaskSticky :: !(About shared a) !(a -> v) !*TSt -> (!TaskResult a,!*TSt) | iTask a & iTask v & toReadOnlyShared shared a
makeMessageTaskSticky about view tst
	# (result,tst) = makeMessageTaskA about view [] tst
	= (mapTaskResult snd result,tst)

makeMessageTaskA :: !(About shared a) !(a -> v) ![TaskAction a] !*TSt -> (!TaskResult (!ActionEvent, !a),!*TSt) | iTask a & iTask v & toReadOnlyShared shared a
makeMessageTaskA about view actions tst=:{taskNr,treeType}
	# (events,tst) = getEvents tst
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			= case actionEvent events actions of
				Just actionEvent
					# (value,tst) = getValue tst
					= (TaskFinished (actionEvent,value), tst)
				Nothing
					# tst = setJSONFunc buildJSONValue tst
					= (TaskBusy,tst)
		UITree
			= case actionEvent events actions of
				Just actionEvent
					# (value,tst) = getValue tst
					= (TaskFinished (actionEvent,value), tst)
				Nothing
					# tst = setTUIFunc buildMsg tst
					= (TaskBusy, tst)
where
	buildMsg tst
		# (mbAbout,tst) = case about of
			NoAbout _			= (Nothing,tst)
			AboutValue v		= (Just v,tst)
			SharedAbout shared	= app2 (Just,id) (readModel shared tst)
		# context = case mbAbout of
			Just about	= Just (visualizeAsHtmlDisplay (view about))
			Nothing		= Nothing
		# (value,tst)	= getValue tst
		# evalActions	= evaluateConditions actions True value
		= (Message (taskPanel (taskNrToString taskNr) context Nothing) evalActions,tst)

	buildJSONValue tst
		# (value,tst) = getValue tst
		= (toJSON (view value),tst)

	getValue tst = case about of
		NoAbout v			= (v,tst)
		AboutValue v		= (v,tst)
		SharedAbout shared	= readModel shared tst

showInstruction :: !String !instruction a -> Task a | html instruction & iTask a
showInstruction subject instruction value
	= mkInstructionTask (subject,instruction) (makeInstructionTask Nothing value)

showInstructionAbout :: !String !instruction a -> Task a | html instruction & iTask a
showInstructionAbout subject instruction context
	= mkInstructionTask (subject,instruction) (makeInstructionTask (Just (visualizeAsHtmlDisplay context)) context)

makeInstructionTask :: (Maybe [HtmlTag]) a *TSt -> *(!TaskResult a,!*TSt) | iTask a
makeInstructionTask context value tst
	# (events, tst) = getEvents tst
	| isEmpty events
		# tst	= setInstruction context tst
		= (TaskBusy,tst)
	| otherwise
		= (TaskFinished value,tst)
	
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
taskPanel :: String (Maybe [HtmlTag]) (Maybe [TUIDef]) -> [TUIDef]
taskPanel taskid mbContext mbForm =
	(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
	(case mbForm of Just form = form; Nothing = [])
where			
	taskContextPanel :: !String ![HtmlTag] -> TUIDef
	taskContextPanel panelid context = TUIHtmlContainer
										{ TUIHtmlContainer
										| id = panelid
										, html = toString (html context)
										}

//Changes all predicates on values of type a to predicates on values of type b										
mapTaskActionPredicates :: !(b -> a) ![TaskAction a] -> [TaskAction b]
mapTaskActionPredicates vMap actions = map changePrecicate actions
where
	changePrecicate (action,pred) = (action,newPred pred)
	newPred pred v = case v of
		Invalid	= pred Invalid
		Valid b	= pred (Valid (vMap b))

readModel :: !(shared a) !*TSt -> (!a,!*TSt) | JSONDecode{|*|}, TC a & toReadOnlyShared shared a
readModel shared tst
	# (mbVal,tst) = accIWorldTSt (readShared shared) tst
	= case mbVal of
		Just val	= (val,tst)
		Nothing		= abort "readModel: shared model deleted!"
		
//Convert a simple to an extended bimap-get function
toExtendedBimapGet :: !(a -> v) -> ((a v -> (v,Bool)),v)
toExtendedBimapGet get = ((\a _ -> (get a,False)),abort "undefined initial view value")
