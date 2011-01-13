implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Html, Text, Http, TSt, Store, DocumentDB, ExceptionCombinators
from StdFunc import id, const, o
from CoreCombinators import >>=, >>|, return
from HtmlUtil import paramValue

derive gVisualize	Action
derive gUpdate		Action
derive gEq			Action
derive gVerify		Action
derive JSONEncode	Action
derive JSONDecode	Action

derive bimap (,), Maybe

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (Action name0 _) (Action name1 _) = name0 == name1
	(==) a b = gEq{|*|} a b

always :: (Verified a) -> Bool
always _ = True

ifvalid :: (Verified a) -> Bool
ifvalid (Valid _) 	= True
ifvalid _			= False 

ifinvalid :: (Verified a) -> Bool
ifinvalid Invalid	= True
ifinvalid _			= False

class html a 
where
	html :: a -> HtmlTag
	
instance html String
where
	html s = Text s

instance html HtmlTag
where
	html h = h
	
instance html [HtmlTag]
where
	html [h]	= h
	html h		= SpanTag [] h

instance html Note
where
	html (Note msg) = Text msg
		
instance html (Maybe a) | html a
where
	html Nothing	= SpanTag [] []
	html (Just h)	= html h
	
instance ActionName Action
where
	actionName (Action name _)		= name
	actionName ActionOk				= "ok"
	actionName ActionCancel			= "cancel"
	actionName ActionYes			= "yes"
	actionName ActionNo				= "no"
	actionName ActionNext			= "next"
	actionName ActionPrevious		= "previous"
	actionName ActionFinish			= "finish"
	actionName ActionNew			= "new"
	actionName ActionOpen			= "open"
	actionName ActionSave			= "save"
	actionName ActionSaveAs			= "save-as"
	actionName ActionClose			= "close"
	actionName ActionQuit			= "quit"
	actionName ActionHelp			= "help"
	actionName ActionAbout			= "about"
	actionName ActionFind			= "find"
	actionName ActionEdit			= "edit"
	actionName ActionDelete			= "delete"
	
instance ActionName ActionName	
where
	actionName name = name

actionIcon :: !Action -> String
actionIcon action = "icon-" +++ (actionName action) 

actionLabel :: !Action -> String
actionLabel (Action _ label)		= label
actionLabel (ActionSaveAs)			= "Save as"
actionLabel action					= upperCaseFirst (actionName action)

instance MenuAction Action
where
	menuAction action = (actionName action, "", "")
	
instance MenuAction ActionName
where
	menuAction name = (name, "", "")
	
instance MenuAction (actionName, ActionLabel, ActionData) | ActionName actionName
where
	menuAction (name, label, data) = (actionName name, label, data)

idBimap :: (IBimap a a)
idBimap = (id, const)

//Input tasks
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation description
	= mkInteractiveTask description (ignoreActionInfo (makeInformationTask Nothing (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] LocalEnter))
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description (makeInformationTask Nothing Nothing (\v _ -> view v) actions LocalEnter)
		
enterInformationAbout :: !d !b -> Task a | descr d  & iTask a & iTask b
enterInformationAbout description about
	= mkInteractiveTask description (ignoreActionInfo (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] LocalEnter))
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !b -> Task (!ActionEvent, Maybe a) | descr d  & iTask a & iTask b& iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) Nothing (\v _ -> view v) actions LocalEnter)

updateInformation :: !d a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description (ignoreActionInfo (makeInformationTask Nothing (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] (LocalUpdate initial)))

updateInformationA :: !d !(IBimap a v) ![TaskAction a] a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask v
updateInformationA description (bimapGet,bimapPutback) actions initial
	= mkInteractiveTask description (makeInformationTask Nothing (Just bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationA :: !d !(IBimap a v) ![TaskAction a] !(DBId a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask v
updateSharedInformationA description (bimapGet,bimapPutback) actions dbid
	= mkInteractiveTask description (makeInformationTask Nothing (Just bimapGet) bimapPutback actions (SharedUpdate dbid))

updateInformationAbout :: !d !b a -> Task a | descr d & iTask a & iTask b
updateInformationAbout description about initial
	= mkInteractiveTask description (ignoreActionInfo (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] (LocalUpdate initial)))

updateInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b a -> Task (!ActionEvent,  !Maybe a) | descr d & iTask a & iTask b & iTask v
updateInformationAboutA description (bimapGet,bimapPutback) actions about initial
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b !(DBId a) -> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask b & iTask v
updateSharedInformationAboutA description (bimapGet,bimapPutback) actions about dbid
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just bimapGet) bimapPutback actions (SharedUpdate dbid))

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description []		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoice description options	= mkInteractiveTask description enterChoice`
where
	enterChoice` tst
		# (result,tst) = makeInformationTask Nothing (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (choice options)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoice choice) result,tst)
		
enterChoiceA :: !d !(a -> v) ![TaskAction a] ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
enterChoiceA description view actions []		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceA description view actions options	= mkInteractiveTask description enterChoiceA`
where
	enterChoiceA` tst
		# (result,tst) = makeInformationTask Nothing (Just (mapOptions view)) (\v a -> setSelection (getSelection v) a) (mapTaskActionPredicates actions getChoice) (LocalUpdate (choice options)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoice) choice)) result,tst)
		
updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a
updateChoice description [] sel			= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoice description options sel	= mkInteractiveTask description updateChoice`
where
	updateChoice` tst
		# (result,tst) = makeInformationTask Nothing (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (choiceSel options sel)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoice choice) result,tst)

updateChoiceA :: !d !(a -> v) ![TaskAction a] ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask v
updateChoiceA description view actions [] sel		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceA description view actions options sel	= mkInteractiveTask description updateChoiceA`
where
	updateChoiceA` tst
		# (result,tst) = makeInformationTask Nothing (Just (mapOptions view)) (\v a -> setSelection (getSelection v) a) (mapTaskActionPredicates actions getChoice) (LocalUpdate (choiceSel options sel)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoice) choice)) result,tst)
		
enterChoiceAbout :: !d !b ![a] -> Task a | descr d & iTask a & iTask b
enterChoiceAbout description about []		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceAbout description about options	= mkInteractiveTask description enterChoiceAbout`
where
	enterChoiceAbout` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (choice options)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoice choice) result,tst)
		
enterChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
enterChoiceAboutA description view actions about []			= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceAboutA description view actions about options	= mkInteractiveTask description enterChoiceAboutA`
where
	enterChoiceAboutA` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (mapOptions view)) (\v a -> setSelection (getSelection v) a) (mapTaskActionPredicates actions getChoice) (LocalUpdate (choice options)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoice) choice)) result,tst)
		
updateChoiceAbout :: !d !b ![a] !Int -> Task a | descr d & iTask a & iTask b
updateChoiceAbout description about [] sel		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceAbout description about options sel	= mkInteractiveTask description updateChoiceAbout`
where
	updateChoiceAbout` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (choiceSel options sel)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoice choice) result,tst)

updateChoiceAboutA :: !d !(a -> v) ![TaskAction a] !b ![a] !Int -> Task (!ActionEvent, Maybe a) | descr d & iTask a & iTask b & iTask v
updateChoiceAboutA description view actions about [] sel		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceAboutA description view actions about options sel	= mkInteractiveTask description updateChoiceAboutA`
where
	updateChoiceAboutA` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (mapOptions view)) (\v a -> setSelection (getSelection v) a) (mapTaskActionPredicates actions getChoice) (LocalUpdate (choiceSel options sel)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoice) choice)) result,tst)

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options = mkInteractiveTask description enterMultipleChoice`
where
	enterMultipleChoice` tst
		# (result,tst) = makeInformationTask Nothing (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (multipleChoice options)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoices choice) result,tst)
		
enterMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
enterMultipleChoiceA description view actions options = mkInteractiveTask description enterMultipleChoiceA`
where
	enterMultipleChoiceA` tst
		# (result,tst) = makeInformationTask Nothing (Just (mapOptionsM view)) (\v a -> setSelectionM (getSelectionM v) a) (mapTaskActionPredicates actions getChoices) (LocalUpdate (multipleChoice options)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoices) choice)) result,tst)
		
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options sel = mkInteractiveTask description updateMultipleChoice`
where
	updateMultipleChoice` tst
		# (result,tst) = makeInformationTask Nothing (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (multipleChoiceSel options sel)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoices choice) result,tst)

updateMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]] ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask v
updateMultipleChoiceA description view actions options sel = mkInteractiveTask description updateMultipleChoiceA`
where
	updateMultipleChoiceA` tst
		# (result,tst) = makeInformationTask Nothing (Just (mapOptionsM view)) (\v a -> setSelectionM (getSelectionM v) a) (mapTaskActionPredicates actions getChoices) (LocalUpdate (multipleChoiceSel options sel)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoices) choice)) result,tst)
		
enterMultipleChoiceAbout :: !d !b ![a] -> Task [a] | descr d & iTask a & iTask b
enterMultipleChoiceAbout description about options = mkInteractiveTask description enterMultipleChoiceAbout`
where
	enterMultipleChoiceAbout` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (multipleChoice options)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoices choice) result,tst)
		
enterMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
enterMultipleChoiceAboutA description view actions about options = mkInteractiveTask description enterMultipleChoiceAboutA`
where
	enterMultipleChoiceAboutA` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (mapOptionsM view)) (\v a -> setSelectionM (getSelectionM v) a) (mapTaskActionPredicates actions getChoices) (LocalUpdate (multipleChoice options)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoices) choice)) result,tst)
		
updateMultipleChoiceAbout :: !d !b ![a] ![Int] -> Task [a] | descr d & iTask a & iTask b
updateMultipleChoiceAbout description about options sel = mkInteractiveTask description updateMultipleChoiceAbout`
where
	updateMultipleChoiceAbout` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just id) const [(ActionOk,ifvalid)] (LocalUpdate (multipleChoiceSel options sel)) tst
		= (mapTaskResult (\(_,Just choice) -> getChoices choice) result,tst)

updateMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]] !b ![a] ![Int] -> Task (!ActionEvent, Maybe [a]) | descr d & iTask a & iTask b & iTask v
updateMultipleChoiceAboutA description view actions about options sel = mkInteractiveTask description updateMultipleChoiceAboutA`
where
	updateMultipleChoiceAboutA` tst
		# (result,tst) = makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (mapOptionsM view)) (\v a -> setSelectionM (getSelectionM v) a) (mapTaskActionPredicates actions getChoices) (LocalUpdate (multipleChoiceSel options sel)) tst
		= (mapTaskResult (\(event,choice) -> (event,(mapMaybe getChoices) choice)) result,tst)

:: InformationTaskMode a = LocalEnter | LocalUpdate !a | SharedUpdate !(DBId a)

makeInformationTask :: !(Maybe [HtmlTag]) !(Maybe (a -> v)) !(v a -> a) ![TaskAction a] !(InformationTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!Maybe a),!*TSt) | iTask a & iTask v
makeInformationTask mbContext mbBimapGet bimapPutback actions informationTaskMode tst=:{taskNr, newTask, treeType}
	# tst = case newTask of
		True // the first time the task is executed build view value from model
			# tst = case informationTaskMode of
				SharedUpdate _
					= tst
				_ // auto generate model store if in local mode
					# (initial,tst) = case informationTaskMode of
						LocalEnter			= accIWorldTSt defaultValue tst
						LocalUpdate initial	= (initial,tst)
					# tst 					= appIWorldTSt (storeValue dbid initial) tst
					= tst
			# ((modelValue,modelTimestamp),tst)	= readModelValue tst
			# tst = case mbBimapGet of
				Just bimapGet // get view value from model
					= snd (updateViewValue bimapGet modelValue modelTimestamp tst)
				Nothing // no bimapGet is given, so use default value
					# (nvalue,tst)		= accIWorldTSt defaultValue tst
					# tst				= setTaskStore "value" nvalue tst
					# tst				= appIWorldTSt (storeValue dbid (bimapPutback nvalue modelValue)) tst
					= tst
			// set mask to untouched in enter mode
			# tst = case informationTaskMode of
				LocalEnter	= setTaskStoreFor taskNr "mask" Untouched tst
				_			= tst
			= tst
		False
			= tst
	# (Just localTimestamp,tst) = getTaskStoreTimestamp "value" tst
	# (mbClientTimestamp,tst)	= clientTimestamp tst
	# (refresh,outdatedClient) = case mbClientTimestamp of
		Nothing
			= (True,False)						// refresh if client did not sent timestamp
		Just clientTimestamp
			# outdated = clientTimestamp < localTimestamp
			= (outdated || newTask,outdated)	// refresh if client timestamp is older than local timestamp of the task
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			= (TaskBusy,tst)
		UITree
			# taskId		= taskNrToString taskNr
			# editorId		= "tf-" +++ taskNrToString taskNr
			# (ovalue,tst)	= readValue tst
			# (oumask,tst)	= readMask tst
			# (ovmask,tst)	= accIWorldTSt (verifyValue ovalue oumask) tst
			# (events,tst)	= getEvents tst
			# edits			= editEvents events
			// check for edit events
			# (rebuild,mbNew,tst) = case (edits,outdatedClient) of
				([],_)		// no edit events
					= (True,Nothing,tst)
				(_,True)	// don't perform update events of outdated client
					= (True,Nothing,tst)
				_			// update edited view value
					# (nvalue,numask,tst)	= applyUpdates edits ovalue oumask tst
					# tst					= setTaskStore "value" nvalue tst
					# tst					= setTaskStore "mask" numask tst
					# (nvmask,tst)			= accIWorldTSt (verifyValue nvalue numask) tst
					= case isValidValue nvmask of
						True
							// if view is valid also update model
							# ((oldModelValue,_), tst)	= readModelValue tst
							# modelValue				= bimapPutback nvalue oldModelValue
							# tst						= appIWorldTSt (storeValue dbid modelValue) tst
							// if there is no bimapGet view is not rebuilt, updates are based on current value
							= (isJust mbBimapGet,Just (nvalue,numask,nvmask),tst)
						False
							// edited invalid views are not rebuilt, updates are based on current value
							= (False,Just (nvalue,numask,nvmask), tst)
			// check for action event
			# mbActionEvent	= actionEvent events actions
			= case mbActionEvent of
				Just event
					# ((modelValue,_), tst)	= readModelValue tst
					// delete auto generated model store
					# tst = case informationTaskMode of
						SharedUpdate _
							= tst
						_
							# tst = appIWorldTSt (deleteValues dbid) tst
							= tst
					# valid = case mbNew of
						Just (_,_,nvmask)	= isValidValue nvmask
						Nothing				= isValidValue ovmask
					= (TaskFinished (event,if valid (Just modelValue) Nothing),tst)
				Nothing
					// UI is built after all possible changes of the model are done
					# tst = setTUIFunc (buildUI taskId editorId (ovalue,oumask,ovmask) (if rebuild Nothing mbNew) refresh localTimestamp) tst
					= (TaskBusy,tst)
where
	// for local mode use auto generated store name, for shared mode use given store
	dbid = case informationTaskMode of
		SharedUpdate dbid	= toString dbid
		_					= "DB_" +++ taskNrToString taskNr

	buildUI taskId editorId old mbNew refresh localTimestamp tst
		# ((modelValue,modelTimestamp), tst)	= readModelValue tst
		= case mbNew of
			Just new=:(nvalue,numask,nvmask) // this case is only needed if no bimap-get func is given, the model can't effect the local view then
				// update with diff of old and new
				# updates						= determineEditorUpdates editorId old new
				# evalActions					= evaluateConditions actions (isValidValue nvmask) modelValue
				= (Updates updates evalActions,tst)
			Nothing
				// check for changed model value
				# (modelChanged,tst)			= accIWorldTSt (isValueChanged dbid localTimestamp) tst
				// determine new view value if model has changed
				# (new,tst) = case (modelChanged,mbBimapGet) of
					(True,Just bimapGet)		= updateViewValue bimapGet modelValue modelTimestamp tst
					_							= (old,tst)
				# evalActions					= evaluateConditions actions (isValidValue (thd3 new)) modelValue
				| refresh
					# (nvalue,numask,nvmask)	= new
					# form 						= visualizeAsEditor editorId nvalue numask nvmask
					= (Definition (taskPanel taskId mbContext (Just form)) evalActions,tst)
				| modelChanged	
					# updates					= determineEditorUpdates editorId old new
					= (Updates updates evalActions,tst)
				| otherwise
					= (Updates [] evalActions,tst)
					
	// determines a new view value from model			
	updateViewValue bimapGet modelValue modelTimestamp tst
		# nvalue			= bimapGet modelValue
		# (numask,tst)		= accIWorldTSt (defaultMask nvalue) tst
		# (nvmask,tst)		= accIWorldTSt (verifyValue nvalue numask) tst
		# tst				= setTaskStoreFor taskNr "value" nvalue tst
		# tst				= setTaskStoreFor taskNr "mask" numask tst
		= ((nvalue,numask,nvmask),tst)
					
	readValue tst
		# (mbvalue,tst)	= getTaskStore "value" tst
		= case mbvalue of
			Just v		= (v,tst)
			Nothing		= abort "readValue: no local value stored"
							
	readMask tst
		# (mbmask,tst)	= getTaskStore "mask" tst
		= case mbmask of
			Just m = (m,tst)
			Nothing = abort "readValue: no local value stored"
				
	readModelValue tst
		# (mbValue,tst) = accIWorldTSt (loadValueAndTimestamp dbid) tst
		= case mbValue of
			Just v	= (v, tst)
			Nothing	= abort "readModelValue: shared model deleted!"
							
	applyUpdates [] val umask tst = (val,umask,tst)
	applyUpdates [(p,v):us] val umask tst=:{TSt|iworld}
		# (val,umask,iworld) = updateValueAndMask p v val umask iworld
		= applyUpdates us val umask {TSt|tst & iworld = iworld}
		
	clientTimestamp :: !*TSt -> (!Maybe Timestamp,!*TSt)
	clientTimestamp tst=:{request}
		# ts = paramValue "timestamp" request
		| ts <> ""	= (Just (Timestamp (toInt ts)),tst)
		| otherwise	= (Nothing,tst)

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description (ignoreActionMsg (makeMessageTask (NoAbout value) noView [(ActionOk, ifvalid)]))

showMessageA :: !d ![TaskAction a] a -> Task (!ActionEvent, a) | descr d & iTask a 
showMessageA description actions value
	= mkInteractiveTask description (makeMessageTask (NoAbout value) noView actions)

showMessageAbout :: !d !a -> Task a | descr d & iTask a
showMessageAbout description about
	= mkInteractiveTask description (ignoreActionMsg (makeMessageTask (AboutValue about) noView [(ActionOk, ifvalid)]))

showMessageAboutA :: !d !(a -> v) ![TaskAction a] !a -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description (makeMessageTask (AboutValue about) (Just view) actions)

showMessageShared :: !d !(a -> v) ![TaskAction a] !(DBId a) -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageShared description view actions dbid
	= mkInteractiveTask description (makeMessageTask (SharedAbout dbid) (Just view) actions)
	
showStickyMessage :: !d a -> Task a | descr d & iTask a
showStickyMessage description value
	= mkInteractiveTask description (ignoreActionMsg (makeMessageTask (NoAbout value) noView []))

showStickyMessageAbout :: !d !a -> Task a | descr d & iTask a
showStickyMessageAbout description about
	= mkInteractiveTask description (ignoreActionMsg (makeMessageTask (AboutValue about) noView []))

showStickyMessageShared :: !d !(a -> v) !(DBId a) -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showStickyMessageShared description view dbid
	= mkInteractiveTask description (makeMessageTask (SharedAbout dbid) (Just view) [])

requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation description = mkInteractiveTask description requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = (makeMessageTask (NoAbout Void) noView [(ActionNo, always),(ActionYes, always)]) tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !d !a -> Task Bool | descr d & iTask a
requestConfirmationAbout description about = mkInteractiveTask description requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = (makeMessageTask (AboutValue about) noView [(ActionNo, always),(ActionYes, ifvalid)]) tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)

:: About a	= NoAbout !a			// don't show value, only return as result
			| AboutValue !a			// show about value
			| SharedAbout !(DBId a)	// show shared about value

// give some type to unused view; otherwise compiler can't solve internal overloading
noView :: Maybe (b -> Void)
noView = Nothing

makeMessageTask :: !(About a) !(Maybe (a -> v)) ![TaskAction a] *TSt -> (!TaskResult (!ActionEvent, !a),!*TSt) | iTask a & iTask v
makeMessageTask about mbView actions tst=:{taskNr,treeType}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# (events,tst) = getEvents tst
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# (value,tst) = getValue tst
			# tst = setJSONValue (toJSON value) tst
			= case actionEvent events actions of
				Just actionEvent	= (TaskFinished (actionEvent,value), tst)
				Nothing				= (TaskBusy,tst)
		UITree
			| isEmpty events
				# (mbAbout,tst) = case about of
					NoAbout _			= (Nothing,tst)
					AboutValue v		= (Just v,tst)
					SharedAbout dbid	= app2 (Just,id) (readSharedValue dbid tst)
				# context = case mbAbout of
					Just about = case mbView of
						Just view	= Just (visualizeAsHtmlDisplay (view about))
						Nothing		= Just (visualizeAsHtmlDisplay about)
					Nothing			= Nothing
				# (value,tst)	= getValue tst
				# evalActions	= evaluateConditions actions True value
				# tst			= setTUIMessage (taskPanel taskId context Nothing) evalActions tst
				= (TaskBusy, tst)
			| otherwise
				# tst = setTUIUpdates [] [] tst
				= case actionEvent events actions of
					Just actionEvent
						# (value,tst) = getValue tst
						= (TaskFinished (actionEvent,value), tst)
					Nothing
						= (TaskBusy, tst)
where
	getValue tst = case about of
		NoAbout v			= (v,tst)
		AboutValue v		= (v,tst)
		SharedAbout dbid	= readSharedValue dbid tst

	readSharedValue dbid tst
		# (mbValue,tst) = accIWorldTSt (loadValue (toString dbid)) tst
		= case mbValue of
			Just v	= (v, tst)
			Nothing	= abort "showMessageShared: shared model deleted!"

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

//Throw away the chosen action part of the result & assume that editor was valid and returned a value
ignoreActionInfo :: (*TSt -> (!TaskResult (!ActionEvent,!Maybe a),*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
ignoreActionInfo f = \tst -> let (res,tst`) = f tst in (mapTaskResult (fromJust o snd) res,tst`)

//Throw away the chosen action part of the result
ignoreActionMsg :: (*TSt -> (!TaskResult (!ActionEvent,!a),*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
ignoreActionMsg f = \tst -> let (res,tst`) = f tst in (mapTaskResult snd res,tst`)

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
mapTaskActionPredicates :: [TaskAction a] (b -> a) -> [TaskAction b]
mapTaskActionPredicates actions vMap = map changePrecicate actions
where
	changePrecicate (action,pred) = (action,newPred pred)
	newPred pred v = case v of
		Invalid	= pred Invalid
		Valid b	= pred (Valid (vMap b))