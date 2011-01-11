implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Html, Text, Http, TSt, Store, DocumentDB, ExceptionCombinators
from StdFunc import id, const
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
	= mkInteractiveTask description (ignoreActionA (makeInformationTask Nothing (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] LocalEnter))
	
enterInformationA :: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
enterInformationA description view actions
	= mkInteractiveTask description (makeInformationTask Nothing Nothing (\v _ -> view v) [(ActionOk,ifvalid)] LocalEnter)
		
enterInformationAbout :: !d !b -> Task a | descr d  & iTask a & iTask b
enterInformationAbout description about
	= mkInteractiveTask description (ignoreActionA (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] LocalEnter))
	
enterInformationAboutA :: !d !(v -> a) ![TaskAction a] !b -> Task (!ActionEvent, a) | descr d  & iTask a & iTask b& iTask v
enterInformationAboutA description view actions about
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) Nothing (\v _ -> view v) actions LocalEnter)

updateInformation :: !d a -> Task a | descr d & iTask a
updateInformation description initial
	= mkInteractiveTask description (ignoreActionA (makeInformationTask Nothing (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] (LocalUpdate initial)))

updateInformationA :: !d !(IBimap a v) ![TaskAction a] a -> Task (!ActionEvent,  !a) | descr d & iTask a & iTask v
updateInformationA description (bimapGet,bimapPutback) actions initial
	= mkInteractiveTask description (makeInformationTask Nothing (Just bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationA :: !d !(IBimap a v) ![TaskAction a] !(DBId a) -> Task (!ActionEvent, !a)	| descr d & iTask a & iTask v
updateSharedInformationA description (bimapGet,bimapPutback) actions dbid
	= mkInteractiveTask description (makeInformationTask Nothing (Just bimapGet) bimapPutback actions (SharedUpdate dbid))

updateInformationAbout :: !d !b a -> Task a | descr d & iTask a & iTask b
updateInformationAbout description about initial
	= mkInteractiveTask description (ignoreActionA (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just (fst idBimap)) (snd idBimap) [(ActionOk,ifvalid)] (LocalUpdate initial)))

updateInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b a -> Task (!ActionEvent,  !a) | descr d & iTask a & iTask b & iTask v
updateInformationAboutA description (bimapGet,bimapPutback) actions about initial
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just bimapGet) bimapPutback actions (LocalUpdate initial))

updateSharedInformationAboutA :: !d !(IBimap a v) ![TaskAction a] !b !(DBId a) -> Task (!ActionEvent, !a)	| descr d & iTask a & iTask b & iTask v
updateSharedInformationAboutA description (bimapGet,bimapPutback) actions about dbid
	= mkInteractiveTask description (makeInformationTask (Just (visualizeAsHtmlDisplay about)) (Just bimapGet) bimapPutback actions (SharedUpdate dbid))

:: InformationTaskMode a = LocalEnter | LocalUpdate !a | SharedUpdate !(DBId a)

makeInformationTask :: !(Maybe [HtmlTag]) !(Maybe (a -> v)) !(v a -> a) ![TaskAction a] !(InformationTaskMode a) !*TSt -> (!TaskResult (!ActionEvent,!a),!*TSt) | iTask a & iTask v
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
			# taskId	= taskNrToString taskNr
			# editorId	= "tf-" +++ taskNrToString taskNr
			# (ovalue,tst)			= readValue tst
			# (oumask,tst)			= readMask tst
			# ovmask				= verifyValue ovalue oumask
			# (events,tst)			= getEvents tst
			# edits					= editEvents events
			// check for edit events
			# (mbNew,tst) = case (edits,outdatedClient) of
				([],_)		// no edit events
					= (Nothing,tst)
				(_,True)	// don't perform update events of outdated client
					= (Nothing,tst)
				_			// update edited view value
					# (nvalue,numask,tst)	= applyUpdates edits ovalue oumask tst
					# tst					= setTaskStore "value" nvalue tst
					# tst					= setTaskStore "mask" numask tst
					# nvmask				= verifyValue nvalue numask
					= case isValidValue nvmask of
						True
							// if view is valid also update model
							# ((oldModelValue,_), tst)	= readModelValue tst
							# modelValue				= bimapPutback nvalue oldModelValue
							# tst						= appIWorldTSt (storeValue dbid modelValue) tst
							// if there is no bimapGet view is not rebuilt, updates are based on current value
							= (if (isJust mbBimapGet) Nothing (Just (nvalue,numask,nvmask)),tst)
						False
							// edited invalid views are not rebuilt, updates are based on current value
							= (Just (nvalue,numask,nvmask), tst)
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
					= (TaskFinished (event,modelValue),tst)
				Nothing
					// UI is built after all possible changes of the model are done
					# tst = setTUIFunc (buildUI taskId editorId (ovalue,oumask,ovmask) mbNew refresh localTimestamp) tst
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
		# nvmask			= verifyValue nvalue numask
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

enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice description []		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoice description options	= mkInteractiveTask description (ignoreActionA (makeChoiceTask options -1 Nothing [(ActionOk, ifvalid)]))

enterChoiceA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction a] ![a] -> Task (!ActionEvent, a) | descr d & iTask a// & iTask v
enterChoiceA description actions []			= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceA description actions options	= mkInteractiveTask description (makeChoiceTask options -1 Nothing actions)

updateChoice :: !d ![a] !Int -> Task a | descr d & iTask a	
updateChoice description [] index		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoice description options index	= mkInteractiveTask description (ignoreActionA (makeChoiceTask options index Nothing [(ActionOk, ifvalid)]))

updateChoiceA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction a] ![a] !Int -> Task (!ActionEvent, a) | descr d & iTask a// & iTask v
updateChoiceA description actions [] index		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceA description actions options index	= mkInteractiveTask description (makeChoiceTask options index Nothing actions)

enterChoiceAbout :: !d !b ![a] -> Task a | descr d & iTask a & iTask b
enterChoiceAbout description about []			= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceAbout description about options		= mkInteractiveTask description (ignoreActionA (makeChoiceTask options -1 (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

enterChoiceAboutA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction a] !b ![a] -> Task (!ActionEvent, a) | descr d & iTask a & iTask b// & iTask v
enterChoiceAboutA description actions about []		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
enterChoiceAboutA description actions about options	= mkInteractiveTask description (makeChoiceTask options -1 (Just (visualizeAsHtmlDisplay about)) actions)

updateChoiceAbout :: !d !b ![a] !Int -> Task a | descr d & iTask a & iTask b
updateChoiceAbout description about [] index		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceAbout description about options index	= mkInteractiveTask description (ignoreActionA (makeChoiceTask options index (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

updateChoiceAboutA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction a] !b ![a] !Int -> Task (!ActionEvent, a) | descr d & iTask a & iTask b// & iTask v
updateChoiceAboutA description actions about [] index		= throw ((toDescr description).TaskDescription.title +++ ": cannot choose from empty option list")
updateChoiceAboutA description actions about options index	= mkInteractiveTask description (makeChoiceTask options index (Just (visualizeAsHtmlDisplay about)) actions)

makeChoiceTask :: ![a] !Int (Maybe [HtmlTag]) ![TaskAction a] !*TSt -> (!TaskResult (!ActionEvent,!a),!*TSt) | iTask a
makeChoiceTask options initsel context actions tst=:{taskNr,newTask,treeType}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# valid			= selection >= 0 && selection < length options	//Do we have a valid index
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# tst = setJSONValue (if valid (toJSON (options !! selection)) JSONNull) tst
			= (TaskBusy,tst)
		UITree
			# (anyEvent,tst)	= (False,tst)//anyEvents tst
			| newTask || not anyEvent
				# form			= [TUIChoiceControl {TUIChoiceControl
													| name = selectionId
													, id   = selectionId
													, fieldLabel = Nothing
													, optional = False
													, allowMultiple = False
													, options = [toString (SpanTag [ClassAttr "task-choice"] (visualizeAsHtmlLabel option)) \\ option <- options]
													, selection = [selection]
													}]
				# evalActions	= evaluateConditions actions valid (if valid (options !! selection) (hd options))
				# tst			= setTUIDef (taskPanel taskId context (Just form)) evalActions tst
				= (TaskBusy, tst)
			| otherwise
				//Check for user updates
				# (events,tst) = getEvents tst
				| isEmpty events
					// no change for this task
					# tst = setTUIUpdates [] [] tst
					= (TaskBusy,tst)
				| otherwise
					= case actionEvent events actions of
						// One of the buttons was pressed
						Just actionEvent	= (TaskFinished (actionEvent, if valid (options !! selection) (hd options)),tst)
						// The selection was updated
						Nothing
							// The selection was updated
							# upd = parseUpdate selectionId events
							# index = if(isEmpty upd) -1 (hd upd)
							| index <> -1
								# valid			= index >= 0 && index < length options	//Recompute validity
								# tst			= setTaskStore "selection" index tst
								# evalActions 	= evaluateConditions actions valid (if valid (options !! index) (hd options))
								# tst			= setTUIUpdates [] evalActions tst
								= (TaskBusy, tst)	
							// Fallback case (shouldn't really happen)
							| otherwise
								# tst = setTUIUpdates [] [] tst
								= (TaskBusy, tst)
where
	parseUpdate :: !String ![(String,JSONNode)] -> [Int]
	parseUpdate	selectionId events
		= case [(name,value) \\ (name,value) <- events | name == selectionId] of
			[(name,value)]	= case fromJSON value of
								Just l	= l
								Nothing	= []
			_				= []

enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice description options = mkInteractiveTask description (ignoreActionA (makeMultipleChoiceTask options [] Nothing [(ActionOk, ifvalid)]))

enterMultipleChoiceA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction [a]] ![a] -> Task (!ActionEvent, [a]) | descr d & iTask a// & iTask v
enterMultipleChoiceA description actions options = mkInteractiveTask description (makeMultipleChoiceTask options [] Nothing actions)

updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice description options indices = mkInteractiveTask description (ignoreActionA (makeMultipleChoiceTask options indices Nothing [(ActionOk, ifvalid)]))

updateMultipleChoiceA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction [a]] ![a] ![Int] -> Task (!ActionEvent, [a]) | descr d & iTask a// & iTask v
updateMultipleChoiceA description actions options indices = mkInteractiveTask description (makeMultipleChoiceTask options indices Nothing actions)

enterMultipleChoiceAbout :: !d !b ![a] -> Task [a] | descr d & iTask a & iTask b
enterMultipleChoiceAbout description about options = mkInteractiveTask description (ignoreActionA (makeMultipleChoiceTask options [] (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

enterMultipleChoiceAboutA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction [a]] !b ![a] -> Task (!ActionEvent, [a]) | descr d & iTask a & iTask b// & iTask v
enterMultipleChoiceAboutA description actions about options = mkInteractiveTask description (makeMultipleChoiceTask options [] (Just (visualizeAsHtmlDisplay about)) actions)

updateMultipleChoiceAbout :: !d !b ![a] ![Int] -> Task [a] | descr d & iTask a & iTask b
updateMultipleChoiceAbout description about options indices = mkInteractiveTask description (ignoreActionA (makeMultipleChoiceTask options indices (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

updateMultipleChoiceAboutA :: !d /*!(a -> v, v a -> a)*/ ![TaskAction [a]] !b ![a] ![Int] -> Task (!ActionEvent, [a]) | descr d & iTask a & iTask b// & iTask v
updateMultipleChoiceAboutA description actions about options indices = mkInteractiveTask description (makeMultipleChoiceTask options indices (Just (visualizeAsHtmlDisplay about)) actions)

makeMultipleChoiceTask :: ![a] ![Int] !(Maybe [HtmlTag]) ![TaskAction [a]] !*TSt -> (!TaskResult (!ActionEvent,![a]),!*TSt) | iTask a
makeMultipleChoiceTask options initsel context actions tst=:{taskNr,newTask,treeType}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# tst = setJSONValue (toJSON (select selection options)) tst
			= (TaskBusy,tst)
		UITree
			# (anyEvent,tst)= (False,tst)//anyEvents tst
			// finish the task in case of an empty options list. As no options are selectable, the result is -of course- an empty list.
			| isEmpty options
			= (TaskFinished ((ActionOk,""),[]),tst)
			| newTask || not anyEvent
				// generate TUI definition
				# checks		= [isMember i selection \\ i <- [0..(length options) - 1]]
				# form			= [TUIChoiceControl { TUIChoiceControl
													| name = "selection"
													, id = editorId +++ "-selection"
													, fieldLabel = Nothing
													, allowMultiple = True
													, optional = False
													, options = [toString (SpanTag [ClassAttr "task-choice"] (visualizeAsHtmlLabel option)) \\ option <- options]
													, selection = selection
													}]
				# evalActions	= evaluateConditions actions True (select selection options)
				# tst			= setTUIDef (taskPanel taskId context (Just form)) evalActions tst
				= (TaskBusy, tst)
			| otherwise
				//Check for events
				# (events,tst) = getEvents tst
				| isEmpty events
					// no change for this task
					# tst = setTUIUpdates [] [] tst
					= (TaskBusy,tst)
				| otherwise
					// One of the buttons was pressed
					= case actionEvent events actions of
						Just actionEvent	= (TaskFinished (actionEvent, select selection options),tst)
						Nothing
							// Perhaps the selection was changed
							# mbSel			= parseSelection events
							# selection		= case mbSel of Nothing = selection; Just sel = sel
							# tst			= setTaskStore "selection" (sort selection) tst
							# evalActions	= evaluateConditions actions True (select selection options)
							# tst			= setTUIUpdates [] evalActions tst
							= (TaskBusy, tst)
where
	parseSelection :: [(String,JSONNode)] -> Maybe [Int]
	parseSelection events =
		case [value \\ (name,value) <- events | name == "selection"] of
			[value] = fromJSON value
			_		= Nothing

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]

showMessage :: !d a -> Task a | descr d & iTask a
showMessage description value
	= mkInteractiveTask description (ignoreActionA (makeMessageTask (NoAbout value) noView [(ActionOk, ifvalid)]))

showMessageA :: !d ![TaskAction a] a -> Task (!ActionEvent, a) | descr d & iTask a 
showMessageA description actions value
	= mkInteractiveTask description (makeMessageTask (NoAbout value) noView actions)

showMessageAbout :: !d !a -> Task a | descr d & iTask a
showMessageAbout description about
	= mkInteractiveTask description (ignoreActionA (makeMessageTask (AboutValue about) noView [(ActionOk, ifvalid)]))

showMessageAboutA :: !d !(a -> v) ![TaskAction a] !a -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageAboutA description view actions about
	= mkInteractiveTask description (makeMessageTask (AboutValue about) (Just view) actions)

showMessageShared :: !d !(a -> v) ![TaskAction a] !(DBId a) -> Task (!ActionEvent, a) | descr d & iTask a & iTask v
showMessageShared description view actions dbid
	= mkInteractiveTask description (makeMessageTask (SharedAbout dbid) (Just view) actions)
	
showStickyMessage :: !d a -> Task a | descr d & iTask a
showStickyMessage description value
	= mkInteractiveTask description (ignoreActionA (makeMessageTask (NoAbout value) noView []))

showStickyMessageAbout :: !d !a -> Task a | descr d & iTask a
showStickyMessageAbout description about
	= mkInteractiveTask description (ignoreActionA (makeMessageTask (AboutValue about) noView []))

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

//Throw away the chosen action part of the result
ignoreActionA :: (*TSt -> (!TaskResult (!ActionEvent,!a),*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
ignoreActionA f = \tst -> let (res,tst`) = f tst in (mapTaskResult snd res,tst`)

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