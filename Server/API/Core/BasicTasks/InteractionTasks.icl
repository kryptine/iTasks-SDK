implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Html, Text, Http, TSt, Store, DocumentDB, ExceptionCombinators
from StdFunc import id
from CoreCombinators import >>=, >>|, return

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

class html a 
where
	html :: a -> HtmlTag
	
instance html String
where
	html s = Text s
	
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

//Input tasks
enterInformation :: !String !description -> Task a | html description & iTask a
enterInformation subject description = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeInformationTask Nothing Nothing [(ActionOk, ifvalid)] False))

enterInformationA :: !String !description ![TaskAction a] -> Task (!ActionEvent,!a) | html description & iTask a
enterInformationA subject description actions = mkInteractiveTask subject (toString (html description)) (makeInformationTask Nothing Nothing actions True)

updateInformation :: !String !description a -> Task a | html description & iTask a
updateInformation subject description initial = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeInformationTask (Just initial) Nothing [(ActionOk, ifvalid)] False)) 

updateInformationA :: !String !description ![TaskAction a] a -> Task (!ActionEvent,!a) | html description & iTask a
updateInformationA subject description actions initial = mkInteractiveTask subject (toString (html description)) (makeInformationTask (Just initial) Nothing actions True)

enterInformationAbout :: !String !description b -> Task a | html description & iTask a & iTask b
enterInformationAbout subject description about = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeInformationTask Nothing (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)] False))

enterInformationAboutA :: !String !description ![TaskAction a] b -> Task (!ActionEvent,!a) | html description & iTask a & iTask b
enterInformationAboutA subject description actions about = mkInteractiveTask subject (toString (html description)) (makeInformationTask Nothing (Just (visualizeAsHtmlDisplay about)) actions True)
	
updateInformationAbout :: !String !description b a -> Task a | html description & iTask a & iTask b 
updateInformationAbout subject description about initial = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeInformationTask (Just initial) (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)] False))

updateInformationAboutA	:: !String !description ![TaskAction a] b a -> Task (!ActionEvent,!a) | html description & iTask a & iTask b
updateInformationAboutA subject description actions about initial = mkInteractiveTask subject (toString (html description)) (makeInformationTask (Just initial) (Just (visualizeAsHtmlDisplay about)) actions True)

makeInformationTask :: (Maybe a) (Maybe [HtmlTag]) ![TaskAction a] !Bool !*TSt -> (!TaskResult (!ActionEvent,!a),!*TSt) | iTask a
makeInformationTask initial context actions actionStored tst=:{taskNr, newTask, treeType}
	# taskId			= taskNrToString taskNr
	# editorId			= "tf-" +++ taskNrToString taskNr
	# (ovalue,tst)		= readValue initial tst
	# (oumask,tst)		= readMask initial tst
	# (events,tst)		= getEvents tst
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# (nvalue,tst) = case valueEvent events of //Check action
				Just nvalue
					# (nmask,tst)	= accIWorldTSt (defaultMask nvalue) tst
					# tst			= setTaskStore "value" nvalue tst	
					# tst			= setTaskStore "mask" nmask tst
					= (nvalue,tst)
				Nothing
					= (ovalue,tst)
			# tst = setJSONValue (toJSON nvalue) tst
			= case actionEvent events actions of
				Just actionEvent	= (TaskFinished (actionEvent,nvalue),tst)
				Nothing				= (TaskBusy,tst)
		UITree
			# (anyEvent,tst)	= anyEvents tst
			# edits				= editEvents events
			| newTask || (isEmpty events && not anyEvent)
				// generate TUI definition
				# ovmask		= verifyValue ovalue oumask			
				# (form,valid) 	= visualizeAsEditor editorId Nothing oumask ovmask ovalue
				# evalActions	= evaluateConditions actions valid ovalue
				# tst			= setTUIDef (taskPanel taskId context (Just form)) evalActions tst
				= (TaskBusy,tst)
			| otherwise
				//Check for events
				| isEmpty events
					// no change for this task
					# ovmask		= verifyValue ovalue oumask
					# (_,valid) 	= visualizeAsEditor editorId Nothing oumask ovmask ovalue
					# evalActions	= evaluateConditions actions valid ovalue
					# tst			= setTUIUpdates [] evalActions tst
					= (TaskBusy,tst)
				| otherwise		
					# (nvalue,numask,tst)	= applyUpdates edits ovalue oumask tst
					# actionEvent			= actionEvent events actions
					| isJust actionEvent
						= (TaskFinished (fromJust actionEvent,nvalue),tst)
					| otherwise
						# tst				= setTaskStore "value" nvalue tst
						# tst				= setTaskStore "mask" numask tst
						# nvmask			= verifyValue nvalue numask
						# (updates,valid)	= determineEditorUpdates editorId Nothing (map fst edits) numask nvmask ovalue nvalue
						# evalActions		= evaluateConditions actions valid ovalue
						# tst				= setTUIUpdates updates evalActions tst
						= (TaskBusy, tst)
where
	readValue initial tst
		# (mbvalue,tst)	= getTaskStore "value" tst
		= case mbvalue of
			Just v		= (v,tst)
			Nothing		= case initial of
						Just i	= (i,tst)
						Nothing	
							# tst=:{TSt|iworld}			= tst
							# (def,iworld)				= defaultValue iworld
							= (def,{TSt|tst & iworld = iworld})
							
	readMask initial tst
		# (mbmask,tst)	= getTaskStore "mask" tst
		= case mbmask of
			Just m = (m,tst)
			Nothing = case initial of
				Just v 
					# tst=:{TSt|iworld}= tst
					# (mask,iworld)			= defaultMask v iworld
					# tst					= setTaskStore "mask" mask
												{TSt|tst & iworld = iworld} // <- store the initial mask
					= (mask,tst)
				Nothing	= ((Untouched True []),tst) 


	applyUpdates [] val umask tst = (val,umask,tst)
	applyUpdates [(p,v):us] val umask tst=:{TSt|iworld}
		# (val,umask,iworld) = updateValueAndMask p v val umask iworld
		= applyUpdates us val umask {TSt|tst & iworld = iworld}

enterChoice :: !String !description [a] -> Task a | html description & iTask a
enterChoice subject description  []		= throw (subject +++ ": cannot choose from empty option list")
enterChoice subject description options	= mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeChoiceTask options -1 Nothing [(ActionOk, ifvalid)]))

enterChoiceA :: !String !description ![TaskAction a] [a] -> Task (!ActionEvent,!a) | html description & iTask a
enterChoiceA subject description actions []		= throw (subject +++ ": cannot choose from empty option list")
enterChoiceA subject description actions options	= mkInteractiveTask subject (toString (html description)) (makeChoiceTask options -1 Nothing actions)

updateChoice :: !String !description [a] Int -> Task a | html description & iTask a 
updateChoice subject description [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoice subject description options index = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeChoiceTask options index Nothing [(ActionOk, ifvalid)]))

updateChoiceA :: !String !description ![TaskAction a] [a] Int -> Task (!ActionEvent,!a) | html description & iTask a 
updateChoiceA subject description actions [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoiceA subject description actions options index	= mkInteractiveTask subject (toString (html description)) (makeChoiceTask options index Nothing actions)

enterChoiceAbout :: !String !description b [a] -> Task a | html description & iTask a & iTask b
enterChoiceAbout subject description about []			= throw (subject +++ ": cannot choose from empty option list")
enterChoiceAbout subject description about options		= mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeChoiceTask options -1 (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

enterChoiceAboutA :: !String !description ![TaskAction a] b [a] -> Task (!ActionEvent,!a) | html description & iTask a & iTask b
enterChoiceAboutA subject description actions about []		= throw (subject +++ ": cannot choose from empty option list")
enterChoiceAboutA subject description actions about options	= mkInteractiveTask subject (toString (html description)) (makeChoiceTask options -1 (Just (visualizeAsHtmlDisplay about)) actions)

updateChoiceAbout :: !String !description b [a] Int -> Task a | html description & iTask a & iTask b
updateChoiceAbout subject description about [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoiceAbout subject description about options index  = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeChoiceTask options index (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

updateChoiceAboutA :: !String !description ![TaskAction a] b [a] Int-> Task (!ActionEvent,!a) | html description & iTask a & iTask b
updateChoiceAboutA subject description actions about [] index			= throw (subject +++ ": cannot choose from empty option list")
updateChoiceAboutA subject description actions about options index		= mkInteractiveTask subject (toString (html description)) (makeChoiceTask options index (Just (visualizeAsHtmlDisplay about)) actions)

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
			# (anyEvent,tst)	= anyEvents tst
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

enterMultipleChoice :: !String !description [a] -> Task [a] | html description & iTask a
enterMultipleChoice subject description options = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeMultipleChoiceTask options [] Nothing [(ActionOk, ifvalid)]))

enterMultipleChoiceA :: !String !description ![TaskAction [a]] [a] -> Task (!ActionEvent,![a]) | html description & iTask a
enterMultipleChoiceA subject description actions options = mkInteractiveTask subject (toString (html description)) (makeMultipleChoiceTask options [] Nothing actions)

updateMultipleChoice :: !String !description [a] [Int] -> Task [a] | html description & iTask a
updateMultipleChoice subject description options indices = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeMultipleChoiceTask options indices Nothing [(ActionOk, ifvalid)]))

updateMultipleChoiceA :: !String !description ![TaskAction [a]] [a] [Int] -> Task (!ActionEvent,![a]) | html description & iTask a
updateMultipleChoiceA subject description actions options indices = mkInteractiveTask subject (toString (html description)) (makeMultipleChoiceTask options indices Nothing actions)

enterMultipleChoiceAbout :: !String !description b [a] -> Task [a] | html description & iTask a & iTask b
enterMultipleChoiceAbout subject description about options = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeMultipleChoiceTask options [] (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

enterMultipleChoiceAboutA :: !String !description ![TaskAction [a]] b [a] -> Task (!ActionEvent,![a]) | html description & iTask a & iTask b
enterMultipleChoiceAboutA subject description actions about options = mkInteractiveTask subject (toString (html description)) (makeMultipleChoiceTask options [] (Just (visualizeAsHtmlDisplay about)) actions)

updateMultipleChoiceAbout :: !String !description b [a] [Int] -> Task [a] | html description & iTask a & iTask b
updateMultipleChoiceAbout subject description about options indices = mkInteractiveTask subject (toString (html description)) (ignoreActionA (makeMultipleChoiceTask options indices (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)]))

updateMultipleChoiceAboutA :: !String !description ![TaskAction [a]] b [a] [Int] -> Task (!ActionEvent,![a]) | html description & iTask a & iTask b
updateMultipleChoiceAboutA subject description actions about options indices = mkInteractiveTask subject (toString (html description)) (makeMultipleChoiceTask options indices (Just (visualizeAsHtmlDisplay about)) actions)

makeMultipleChoiceTask :: [a] [Int] (Maybe [HtmlTag]) ![TaskAction [a]] !*TSt -> (!TaskResult (!ActionEvent,![a]),!*TSt) | iTask a
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
			# (anyEvent,tst)= anyEvents tst
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
	
//Output tasks
showMessage	:: !String !message a -> Task a	| html message & iTask a
showMessage subject message value = mkInteractiveTask subject (toString (html message)) (ignoreActionA (makeMessageTask Nothing [(ActionOk, ifvalid)] value))

showMessageA :: !String !message ![TaskAction a] a -> Task (!ActionEvent, !a) | html message & iTask a
showMessageA subject message actions value = mkInteractiveTask subject (toString (html message)) (makeMessageTask Nothing actions value)

showMessageAbout :: !String !message a -> Task a | html message & iTask a
showMessageAbout subject message about = mkInteractiveTask subject (toString (html message)) (ignoreActionA (makeMessageTask (Just (visualizeAsHtmlDisplay about)) [(ActionOk, ifvalid)] about))

showMessageAboutA :: !String !message ![TaskAction a] a -> Task (!ActionEvent, !a) | html message & iTask a
showMessageAboutA subject message actions about = mkInteractiveTask subject (toString (html message)) (makeMessageTask (Just (visualizeAsHtmlDisplay about)) actions about)
	
showStickyMessage :: !String !message a -> Task a | html message & iTask a
showStickyMessage subject message value = mkInteractiveTask subject (toString (html message)) (ignoreActionA (makeMessageTask Nothing [] value))

showStickyMessageAbout :: !String !message a -> Task a | html message & iTask a
showStickyMessageAbout subject message about = mkInteractiveTask subject (toString (html message)) (ignoreActionA (makeMessageTask (Just (visualizeAsHtmlDisplay about)) [] about))

requestConfirmation	:: !String !description -> Task Bool | html description
requestConfirmation subject description = mkInteractiveTask subject (toString (html description)) requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = (makeMessageTask Nothing [(ActionNo, always),(ActionYes, always)] False) tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !String !description a -> Task Bool | html description & iTask a
requestConfirmationAbout subject description about = mkInteractiveTask subject (toString (html description)) requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = (makeMessageTask (Just (visualizeAsHtmlDisplay about)) [(ActionNo, always),(ActionYes, ifvalid)] False) tst
		= (mapTaskResult (\a -> case a of ((ActionYes,_),_) = True; _ = False) result, tst)

makeMessageTask :: (Maybe [HtmlTag]) ![TaskAction a] a *TSt -> (!TaskResult (!ActionEvent, !a),!*TSt) | iTask a
makeMessageTask context actions value tst=:{taskNr,treeType}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# (events,tst) = getEvents tst
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# tst = setJSONValue (toJSON value) tst
			= case actionEvent events actions of
				Just actionEvent	= (TaskFinished (actionEvent,value), tst)
				Nothing				= (TaskBusy,tst)
		UITree
			| isEmpty events
				# evalActions	= evaluateConditions actions True value
				# tst			= setTUIMessage (taskPanel taskId context Nothing) evalActions tst
				= (TaskBusy, tst)
			| otherwise
				# tst			= setTUIUpdates [] [] tst
				= case actionEvent events actions of
					Just actionEvent
						= (TaskFinished (actionEvent,value), tst)
					Nothing
						# tst	= setTUIUpdates [] [] tst
						= (TaskBusy, tst)

showInstruction :: !String !instruction a -> Task a | html instruction & iTask a
showInstruction subject instruction value = mkInstructionTask subject (toString (html instruction)) (makeInstructionTask Nothing value)

showInstructionAbout :: !String !instruction a -> Task a | html instruction & iTask a
showInstructionAbout subject instruction context = mkInstructionTask subject (toString (html instruction)) (makeInstructionTask (Just (visualizeAsHtmlDisplay context)) context)

makeInstructionTask :: (Maybe [HtmlTag]) a *TSt -> *(!TaskResult a,!*TSt) | iTask a
makeInstructionTask context value tst
	# (events, tst) = getEvents tst
	| isEmpty events
		# tst	= setInstruction context tst
		= (TaskBusy,tst)
	| otherwise
		= (TaskFinished value,tst)
			
//Shared value tasks
:: View s = E.a: Listener (Listener` s a) | E.a: Editor (Editor` s a)
:: Listener` s a =	{ visualize :: !s -> [HtmlTag] }
:: Editor` s a =	{ getNewValue :: !ViewNr [(DataPath,String)] s s *TSt -> *(s,*TSt)
					, determineUpdates :: !TaskNr ViewNr s [(String,String)] *TSt -> *(([TUIUpdate],Bool),*TSt)
					, visualize :: !TaskNr ViewNr s *TSt -> *(([TUIDef],Bool),*TSt)
					}
:: ViewNr :== Int

editor :: !(Editor s a) -> View s | iTask a & iTask s & SharedVariable s
editor {editorFrom, editorTo} = Editor {getNewValue = getNewValue, determineUpdates = determineUpdates, visualize = visualize}
where
	// determine new shared value based on events for this view
	getNewValue n updates old cur tst
		# oEditV				= editorFrom old
		# tst					= setTaskStore (addStorePrefix n "value") oEditV tst
		# myUpdates				= filter (\upd -> dataPathHasSubEditorIdx (fst upd) n) updates
		| isEmpty myUpdates
			= (cur,tst)
		| otherwise
			# (nEditV,tst)		= applyUpdates myUpdates oEditV tst
			= (mergeValues old cur (editorTo nEditV old), tst)
	where
		applyUpdates [] val tst = (val,tst)
		applyUpdates [(p,v):us] val tst=:{TSt|iworld}
			# (val,iworld)		= updateValue p v val iworld
			= applyUpdates us val {TSt|tst & iworld = iworld}
	
	// determine TUI updates for view
	determineUpdates taskNr n new postValues tst
		# (Just oEditV,tst)			= getTaskStoreFor taskNr (addStorePrefix n "value") tst
		# nEditV					= editorFrom new
		# tst=:{TSt|iworld}			= tst
		# (umask,iworld)			= defaultMask nEditV iworld
		# (vmask)					= verifyValue nEditV umask
		# (events,tst)				= getEvents {TSt|tst & iworld = iworld}
		# updpaths					= events2Paths postValues
		= (determineEditorUpdates (editorId taskNr n) (Just n) updpaths umask vmask oEditV nEditV,tst)
	
	// generate TUI definition for view
	visualize taskNr n stateV tst=:{TSt|iworld}
		# editV					= editorFrom stateV
		# (umask,iworld)		= defaultMask editV iworld
		# (vmask)				= verifyValue editV umask
		= (visualizeAsEditor (editorId taskNr n) (Just n) umask vmask editV,{TSt|tst & iworld = iworld})
				
listener :: !(Listener s a) -> View s | iTask a & iTask s
listener {listenerFrom} = Listener {Listener`|visualize = visualize}
where
	visualize v = visualizeAsHtmlDisplay (listenerFrom v)

idEditor	:: View s	| iTask s & SharedVariable s
idEditor = editor {editorFrom = id, editorTo = (\a _ -> a)}

idListener	:: View s	| iTask s
idListener = listener {listenerFrom = id}

updateShared :: !String description ![TaskAction s] !(DBId s) ![View s] -> Task (!ActionEvent, !s) | html description & iTask s & SharedVariable s
updateShared subject description actions sharedId views = mkInteractiveTask subject (toString (html description)) (makeSharedTask description actions sharedId views False)

updateSharedLocal :: !String description ![TaskAction s] !s ![View s] -> Task (!ActionEvent, !s) | html description & iTask s & SharedVariable s
updateSharedLocal subject description actions initial views =
				createDB initial
	>>= \sid.	mkInteractiveTask subject (toString (html description)) (makeSharedTask description actions sid views False)
	>>= \res.	deleteDB sid
	>>|			return res

makeSharedTask :: description ![TaskAction s] !(DBId s) ![View s] !Bool !*TSt -> (!TaskResult (!ActionEvent,!s),!*TSt) | html description & iTask s & SharedVariable s
makeSharedTask description actions (DBId sharedId) views actionStored tst=:{taskNr, newTask}
	# (mbcvalue,tst) = readShared sharedId tst
	= case mbcvalue of
		Nothing
			= (TaskException (dynamic "updateShared: shared variable is deleted"), tst)
		Just cvalue
			# (anyEvent,tst)			= anyEvents tst
			| newTask || not anyEvent
				// generate TUI definition for new tasks or if there are no updates (refresh entire task)
				# tst = setTUIFunc createDefs tst
				= (TaskBusy, tst)
			| otherwise
				# (events,tst)		= getEvents tst
				# edits				= editEvents events
				// determine new shared value by accumulating updates of all views
				# (nvalue,_,tst)	= foldl (updateSharedForView edits) (cvalue,0,tst) views
				# tst=:{TSt|iworld=iworld=:{IWorld|store}}
									= tst
				# store				= storeValue sharedId nvalue store
				# tst				= {TSt|tst & iworld = {IWorld|iworld & store = store}}
				// check if action is triggered
				= case actionEvent events actions of
					Just actionEvent
						= (TaskFinished (actionEvent,fromJust mbcvalue),tst)
					Nothing
						// updates are calculated after tree is build, shared value maybe changed by other tasks
						# tst = setTUIFunc (createUpdates events) tst
						= (TaskBusy, tst)
where
	// generate TUI definitions for all views
	createDefs :: !*TSt -> *(InteractiveTask,*TSt)
	createDefs tst
		# (Just svalue,tst)		= readShared sharedId tst
		# (form,valid,_,tst)	= foldl (createDef svalue) ([],True,0,tst) views
		# evalActions			= evaluateConditions actions valid svalue
		= (Definition (taskPanel taskId Nothing (Just form)) evalActions,tst)
	where
		createDef :: !a !*([TUIDef],Bool,ViewNr,*TSt) !(View a) -> *([TUIDef],Bool,ViewNr,*TSt) | iTask a
		createDef svalue (def,valid,n,tst) (Editor editor)
			# tst					= setTaskStoreFor taskNr (addStorePrefix n "value") svalue tst
			# ((ndef,nvalid),tst)	= editor.Editor`.visualize taskNr n svalue tst
			= (def ++ ndef,valid && nvalid,n + 1,tst)
		createDef svalue (def,valid,n,tst) (Listener listener) = (def ++ [listenerPanel svalue listener n],valid,n + 1,tst)
	
	// create TUI updates for all views
	createUpdates :: ![(String,JSONNode)] !*TSt -> *(InteractiveTask,*TSt)
	createUpdates postValues tst
		# (mbcvalue,tst)	= readShared sharedId tst
		# cvalue			= fromJust mbcvalue
		# (upd,valid,_,tst)	= foldl (detUpd cvalue postValues) ([],True,0,tst) views
		# evalActions		= evaluateConditions actions valid cvalue
		= (Updates upd evalActions,tst)
	where
		detUpd :: !a ![(String,JSONNode)] !*([TUIUpdate],Bool,ViewNr,*TSt) !(View a) -> *([TUIUpdate],Bool,ViewNr,*TSt) | iTask a
		detUpd nvalue events (upd,valid,n,tst) (Editor editor)
			# ((nupd,nvalid),tst)	= editor.determineUpdates taskNr n nvalue [(name,value) \\ (name,JSONString value) <- events] tst
			# tst					= setTaskStoreFor taskNr (addStorePrefix n "value") nvalue tst
			= (upd ++ nupd,	valid && nvalid, inc n, tst)
		detUpd nvalue _ (upd,valid,n,tst) (Listener listener) 
			= ([TUIReplace (editorId taskNr n) (listenerPanel nvalue listener n):upd],valid,n + 1,tst)
	
	// update value of shared according to events for one view
	updateSharedForView :: ![(DataPath,String)] !*(a,ViewNr,*TSt) !(View a) -> *(a,ViewNr,*TSt) | iTask a	
	updateSharedForView updates (cvalue,n,tst) (Editor editor)
		# (ovalue,tst)			= readLocalValue n tst
		# (nvalue,tst)			= editor.getNewValue n updates ovalue cvalue tst
		= (nvalue,n + 1,tst)
	updateSharedForView _ (cvalue,n,tst) (Listener _) = (cvalue,n + 1,tst)
	
	listenerPanel :: !a !(Listener` a b) !Int -> TUIDef
	listenerPanel value listener n = TUIHtmlContainer
										{ TUIHtmlContainer
										| id = (editorId taskNr n)
										, html = toString (html (listener.Listener`.visualize value))
										}
	
	readLocalValue :: !a !*TSt -> *(b,*TSt) | iTask b & toString a	
	readLocalValue n tst
		# (mbvalue,tst)	= getTaskStore (addStorePrefix n "value") tst
		= case mbvalue of
			Just v		= (v,tst)
			Nothing		= abort "cannot get local value"
	
	readShared :: !String !*TSt -> *((Maybe a),*TSt) | JSONDecode{|*|}, TC a		
	readShared sid tst=:{TSt|iworld = iworld =:{IWorld|store,world}}
		# (mbvalue,store,world) = loadValue sid store world
		# tst = {TSt|tst & iworld = {IWorld| iworld & store = store, world = world}}
		= case mbvalue of
			Just v		= (Just v,tst)
			Nothing		= (Nothing,tst)
			
	taskId			= taskNrToString taskNr
	baseEditorId	= "tf-" +++ taskId
			
addStorePrefix n key	= (toString n) +++ "_" +++ key
editorId taskNr n		= "tf-" +++ (taskNrToString taskNr) +++ "_" +++ (toString n)
		
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
		
//Edit events of which the name is a datapath
editEvents :: [(String,JSONNode)] -> [(DataPath,String)]
editEvents events = [(s2dp name,value) \\ (name,JSONString value) <- events| isdps name]

		
always :: (Verified a) -> Bool
always _ = True

ifvalid :: (Verified a) -> Bool
ifvalid (Valid _) 	= True
ifvalid _			= False 

ifinvalid :: (Verified a) -> Bool
ifinvalid Invalid	= True
ifinvalid _			= False

evaluateConditions :: ![(!Action, (Verified a) -> Bool)] !Bool !a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,evaluateCondition cond valid value) \\ (action,cond) <- actions]

evaluateCondition :: !((Verified a) -> Bool) !Bool !a -> Bool
evaluateCondition pred valid value = pred (if valid (Valid value) Invalid)
	
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
			
//Throw away the chosen action part of the result
ignoreActionA :: (*TSt -> (!TaskResult (!ActionEvent,!a),*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
ignoreActionA f = \tst -> let (res,tst`) = f tst in (mapTaskResult snd res,tst`)
			
mapTaskResult :: !(a -> b) !(TaskResult a) -> TaskResult b
mapTaskResult f (TaskFinished x)	= TaskFinished (f x) 
mapTaskResult f (TaskBusy)			= TaskBusy
mapTaskResult f (TaskException e)	= TaskException e
