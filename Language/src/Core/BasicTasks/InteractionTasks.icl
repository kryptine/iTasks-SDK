implementation module InteractionTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc
import Types, Html, Text, Http, TSt, Store, DocumentDB, ExceptionCombinators
from StdFunc import id
from ProcessDB import :: Action(..), getActionIcon
from CoreCombinators import >>=, >>|, return

derive bimap (,)

class html a 
where
	html :: a -> [HtmlTag]
	
instance html String
where
	html s = [Text s]
	
instance html [HtmlTag]
where
	html h = h

//Input tasks
enterInformation :: !String !description -> Task a | html description & iTask a
enterInformation subject description = mkInteractiveTask subject (ignoreActionA (makeInformationTask description Nothing Nothing [ButtonAction (ActionOk, IfValid)] False))

enterInformationA :: !String !description ![TaskAction a] -> Task (!Action,!a) | html description & iTask a
enterInformationA subject description actions = mkInteractiveTask subject (makeInformationTask description Nothing Nothing actions True)

updateInformation :: !String !description a -> Task a | html description & iTask a
updateInformation subject description initial = mkInteractiveTask subject (ignoreActionA (makeInformationTask description (Just initial) Nothing [ButtonAction (ActionOk, IfValid)] False)) 

updateInformationA :: !String !description ![TaskAction a] a -> Task (!Action,!a) | html description & iTask a
updateInformationA subject description actions initial = mkInteractiveTask subject (makeInformationTask description (Just initial) Nothing actions True)

enterInformationAbout :: !String !description b -> Task a | html description & iTask a & iTask b
enterInformationAbout subject description about = mkInteractiveTask subject (ignoreActionA (makeInformationTask description Nothing (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)] False))

enterInformationAboutA :: !String !description ![TaskAction a] b -> Task (!Action,!a) | html description & iTask a & iTask b
enterInformationAboutA subject description actions about = mkInteractiveTask subject (makeInformationTask description Nothing (Just (visualizeAsHtmlDisplay about)) actions True)
	
updateInformationAbout :: !String !description b a -> Task a | html description & iTask a & iTask b 
updateInformationAbout subject description about initial = mkInteractiveTask subject (ignoreActionA (makeInformationTask description (Just initial) (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)] False))

updateInformationAboutA	:: !String !description ![TaskAction a] b a -> Task (!Action,!a) | html description & iTask a & iTask b
updateInformationAboutA subject description actions about initial = mkInteractiveTask subject (makeInformationTask description (Just initial) (Just (visualizeAsHtmlDisplay about)) actions True)

makeInformationTask :: description (Maybe a) (Maybe [HtmlTag]) ![TaskAction a] !Bool !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html description & iTask a
makeInformationTask description initial context actions actionStored tst=:{taskNr, newTask, treeType}
	# taskId			= taskNrToString taskNr
	# editorId			= "tf-" +++ taskNrToString taskNr
	# (ovalue,tst)		= readValue initial tst
	# (omask,tst)		= readMask initial tst
	# buttonActions		= getButtonActions actions
	= case treeType of
		SpineTree
			= (TaskBusy,tst)
		JSONTree
			# tst = setJSONValue (toJSON ovalue) tst
			= (TaskBusy,tst)
		UITree
			# (anyEvent,tst)	= anyEvents tst
			| newTask || not anyEvent
				// generate TUI definition
				# (form,valid) 	= visualizeAsEditor editorId Nothing omask ovalue
				# menuActions	= evaluateConditions (getMenuActions actions) valid ovalue
				# buttonActions	= evaluateConditions buttonActions valid ovalue
				# tst			= setTUIDef (taskPanel taskId (html description) context (Just form) (makeButtons editorId buttonActions)) (html description) menuActions tst
				= (TaskBusy,tst)
			| otherwise
				//Check for events
				# (events,tst) = getEvents tst
				| isEmpty events
					// no change for this task
					# tst = setTUIUpdates [] [] tst
					= (TaskBusy,tst)
				| otherwise
					# (nvalue,nmask,tst) = applyUpdates [(s2dp key,value) \\ (key,value) <- events | isdps key] ovalue omask tst
					# (action,tst) = getAction events (map fst buttonActions) tst
					| isJust action
						= (TaskFinished (fromJust action,nvalue),tst)
					| otherwise
						# tst				= setTaskStore "value" nvalue tst
						# tst				= setTaskStore "mask" nmask tst
						# updpaths			= events2Paths events
						# (updates,valid)	= determineEditorUpdates editorId Nothing updpaths omask nmask ovalue nvalue
						# menuActions		= evaluateConditions (getMenuActions actions) valid nvalue
						# buttonActions		= evaluateConditions buttonActions valid nvalue
						# tst				= setTUIUpdates (enables editorId buttonActions ++ updates) menuActions tst
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
				Nothing	= ([],tst) 


	applyUpdates [] val mask tst = (val,mask,tst)
	applyUpdates [(p,v):us] val mask tst=:{TSt|iworld}
		# (val,mask,iworld) = updateValueAndMask p v val mask iworld
		= applyUpdates us val mask {TSt|tst & iworld = iworld}

enterChoice :: !String !description [a] -> Task a | html description & iTask a
enterChoice subject description  []		= throw (subject +++ ": cannot choose from empty option list")
enterChoice subject description options	= mkInteractiveTask subject (ignoreActionA (makeChoiceTask description options -1 Nothing [ButtonAction (ActionOk, IfValid)]))

enterChoiceA :: !String !description ![TaskAction a] [a] -> Task (!Action,!a) | html description & iTask a
enterChoiceA subject description actions []		= throw (subject +++ ": cannot choose from empty option list")
enterChoiceA subject description actions options	= mkInteractiveTask subject (makeChoiceTask description options -1 Nothing actions)

updateChoice :: !String !description [a] Int -> Task a | html description & iTask a 
updateChoice subject description [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoice subject description options index = mkInteractiveTask subject (ignoreActionA (makeChoiceTask description options index Nothing [ButtonAction (ActionOk, IfValid)]))

updateChoiceA :: !String !description ![TaskAction a] [a] Int -> Task (!Action,!a) | html description & iTask a 
updateChoiceA subject description actions [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoiceA subject description actions options index	= mkInteractiveTask subject (makeChoiceTask description options index Nothing actions)

enterChoiceAbout :: !String !description b [a] -> Task a | html description & iTask a & iTask b
enterChoiceAbout subject description about []		= throw (subject +++ ": cannot choose from empty option list")
enterChoiceAbout subject description about options = mkInteractiveTask subject (ignoreActionA (makeChoiceTask description options -1 (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

enterChoiceAboutA :: !String !description ![TaskAction a] b [a] -> Task (!Action,!a) | html description & iTask a & iTask b
enterChoiceAboutA subject description actions about []		= throw (subject +++ ": cannot choose from empty option list")
enterChoiceAboutA subject description actions about options	= mkInteractiveTask subject (makeChoiceTask description options -1 (Just (visualizeAsHtmlDisplay about)) actions)

updateChoiceAbout :: !String !description b [a] Int -> Task a | html description & iTask a & iTask b
updateChoiceAbout subject description about [] index		= throw (subject +++ ": cannot choose from empty option list")
updateChoiceAbout subject description about options index  = mkInteractiveTask subject (ignoreActionA (makeChoiceTask description options index (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

updateChoiceAboutA :: !String !description ![TaskAction a] b [a] Int-> Task (!Action,!a) | html description & iTask a & iTask b
updateChoiceAboutA subject description actions about [] index			= throw (subject +++ ": cannot choose from empty option list")
updateChoiceAboutA subject description actions about options index		= mkInteractiveTask subject (makeChoiceTask description options index (Just (visualizeAsHtmlDisplay about)) actions)

makeChoiceTask :: !description ![a] !Int (Maybe [HtmlTag]) ![TaskAction a] !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html description & iTask a
makeChoiceTask description options initsel context actions tst=:{taskNr, newTask}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# valid			= selection >= 0 && selection < length options	//Do we have a valid index
	# buttonActions = getButtonActions actions
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
		# menuActions	= evaluateConditions (getMenuActions actions) valid (if valid (options !! selection) (hd options))
		# buttonActions	= evaluateConditions buttonActions valid (if valid (options !! selection) (hd options))
		# tst			= setTUIDef (taskPanel taskId (html description) context (Just form) (makeButtons editorId buttonActions)) (html description) menuActions tst
		= (TaskBusy, tst)
	| otherwise
		//Check for user updates
		# (events,tst) = getEvents tst
		| isEmpty events
			// no change for this task
			# tst = setTUIUpdates [] [] tst
			= (TaskBusy,tst)
		| otherwise
			# (action,tst) = getAction events (map fst buttonActions) tst
			= case action of
				// One of the buttons was pressed
				Just action	= (TaskFinished (action, if valid (options !! selection) (hd options)),tst)
				// The selection was updated
				Nothing
					// The selection was updated
					# upd = parseUpdate selectionId events
					# index = if(isEmpty upd) -1 (hd upd)
					| index <> -1
						# valid			= index >= 0 && index < length options	//Recompute validity
						# tst			= setTaskStore "selection" index tst
						# menuActions	= evaluateConditions (getMenuActions actions) valid (if valid (options !! index) (hd options))
						# buttonActions = evaluateConditions buttonActions valid (if valid (options !! index) (hd options))
						# tst			= setTUIUpdates (enables editorId buttonActions) menuActions tst
						= (TaskBusy, tst)	
					// Fallback case (shouldn't really happen)
					| otherwise
						# tst = setTUIUpdates [] [] tst
						= (TaskBusy, tst)
where
	parseUpdate :: !String ![(String,String)] -> [Int]
	parseUpdate	selectionId events
		# mbList = fromJSON(fromString (http_getValue selectionId events "[]"))
		= case mbList of Nothing = []; Just list = list

enterMultipleChoice :: !String !description [a] -> Task [a] | html description & iTask a
enterMultipleChoice subject description options = mkInteractiveTask subject (ignoreActionA (makeMultipleChoiceTask description options [] Nothing [ButtonAction (ActionOk, IfValid)]))

enterMultipleChoiceA :: !String !description ![TaskAction [a]] [a] -> Task (!Action,![a]) | html description & iTask a
enterMultipleChoiceA subject description actions options = mkInteractiveTask subject (makeMultipleChoiceTask description options [] Nothing actions)

updateMultipleChoice :: !String !description [a] [Int] -> Task [a] | html description & iTask a
updateMultipleChoice subject description options indices = mkInteractiveTask subject (ignoreActionA (makeMultipleChoiceTask description options indices Nothing [ButtonAction (ActionOk, IfValid)]))

updateMultipleChoiceA :: !String !description ![TaskAction [a]] [a] [Int] -> Task (!Action,![a]) | html description & iTask a
updateMultipleChoiceA subject description actions options indices = mkInteractiveTask subject (makeMultipleChoiceTask description options indices Nothing actions)

enterMultipleChoiceAbout :: !String !description b [a] -> Task [a] | html description & iTask a & iTask b
enterMultipleChoiceAbout subject description about options = mkInteractiveTask subject (ignoreActionA (makeMultipleChoiceTask description options [] (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

enterMultipleChoiceAboutA :: !String !description ![TaskAction [a]] b [a] -> Task (!Action,![a]) | html description & iTask a & iTask b
enterMultipleChoiceAboutA subject description actions about options = mkInteractiveTask subject (makeMultipleChoiceTask description options [] (Just (visualizeAsHtmlDisplay about)) actions)

updateMultipleChoiceAbout :: !String !description b [a] [Int] -> Task [a] | html description & iTask a & iTask b
updateMultipleChoiceAbout subject description about options indices = mkInteractiveTask subject (ignoreActionA (makeMultipleChoiceTask description options indices (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

updateMultipleChoiceAboutA :: !String !description ![TaskAction [a]] b [a] [Int] -> Task (!Action,![a]) | html description & iTask a & iTask b
updateMultipleChoiceAboutA subject description actions about options indices = mkInteractiveTask subject (makeMultipleChoiceTask description options indices (Just (visualizeAsHtmlDisplay about)) actions)

makeMultipleChoiceTask :: description [a] [Int] (Maybe [HtmlTag]) ![TaskAction [a]] !*TSt -> (!TaskResult (!Action,![a]),!*TSt) | html description & iTask a
makeMultipleChoiceTask description options initsel context actions tst=:{taskNr, newTask}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# buttonActions	= evaluateConditions (getButtonActions actions) True (select selection options)
	# (anyEvent,tst)= anyEvents tst
	// finish the task in case of an empty options list. As no options are selectable, the result is -of course- an empty list.
	| isEmpty options
	= (TaskFinished (ActionOk,[]),tst)
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
		# menuActions	= evaluateConditions (getMenuActions actions) True (select selection options)
		# tst			= setTUIDef (taskPanel taskId (html description) context (Just form) (makeButtons editorId buttonActions)) (html description) menuActions tst
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
			# (action,tst) = getAction events (map fst buttonActions) tst
			= case action of
				Just action	= (TaskFinished (action, select selection options),tst)
				Nothing
					// Perhaps the selection was changed
					# mbSel		= parseSelection events
					# selection	= case mbSel of Nothing = selection; Just sel = map toInt sel
					# tst		= setTaskStore "selection" (sort selection) tst
					# tst		= setTUIUpdates [] (evaluateConditions (getMenuActions actions) True (select selection options)) tst
					= (TaskBusy, tst)
where
	parseSelection :: [(String,String)] -> Maybe [Int]
	parseSelection events = fromJSON (fromString (http_getValue "selection" events "[]"))	

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]
	
//Output tasks
showMessage	:: !String !message a -> Task a	| html message & iTask a
showMessage subject message value = mkInteractiveTask subject (ignoreActionA (makeMessageTask message Nothing [ButtonAction (ActionOk, IfValid)] value))

showMessageA :: !String !message ![TaskAction a] a -> Task (!Action, !a) | html message & iTask a
showMessageA subject message actions value = mkInteractiveTask subject (makeMessageTask message Nothing actions value)

showMessageAbout :: !String !message a -> Task a | html message & iTask a
showMessageAbout subject message about = mkInteractiveTask subject (ignoreActionA (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)] about))

showMessageAboutA :: !String !message ![TaskAction a] a -> Task (!Action, !a) | html message & iTask a
showMessageAboutA subject message actions about = mkInteractiveTask subject (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) actions about)
	
showStickyMessage :: !String !message a -> Task a | html message & iTask a
showStickyMessage subject message value = mkInteractiveTask subject (ignoreActionA (makeMessageTask message Nothing [] value))

showStickyMessageAbout :: !String !message a -> Task a | html message & iTask a
showStickyMessageAbout subject message about = mkInteractiveTask subject (ignoreActionA (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [] about))

requestConfirmation	:: !String !description -> Task Bool | html description
requestConfirmation subject description = mkInteractiveTask subject requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = (makeMessageTask description Nothing [ButtonAction (ActionNo, Always),ButtonAction (ActionYes, Always)] False) tst
		= (mapTaskResult (\a -> case a of (ActionYes,_) = True; _ = False) result, tst)
								
requestConfirmationAbout :: !String !description a -> Task Bool | html description & iTask a
requestConfirmationAbout subject description about = mkInteractiveTask subject requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = (makeMessageTask description (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionNo, Always),ButtonAction (ActionYes, IfValid)] False) tst
		= (mapTaskResult (\a -> case a of (ActionYes,_) = True; _ = False) result, tst)

makeMessageTask :: message (Maybe [HtmlTag]) ![TaskAction a] a *TSt -> (!TaskResult (!Action, !a),!*TSt) | html message & iTask a
makeMessageTask message context actions value tst=:{taskNr}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# buttonActions	= getButtonActions actions
	# (events,tst) = getEvents tst
	| isEmpty events
		# menuActions	= evaluateConditions (getMenuActions actions) True value
		# buttonActions	= evaluateConditions buttonActions True value
		# tst			= setTUIMessage (taskPanel taskId (html message) context Nothing (makeButtons editorId buttonActions)) (html message) menuActions tst
		= (TaskBusy, tst)
	| otherwise
		# (action,tst) = getAction events (map fst buttonActions) tst
		= case action of
			Just action	=	(TaskFinished (action,value), tst)
			Nothing =		(TaskBusy, tst)

showInstruction :: !String !instruction a -> Task a | html instruction & iTask a
showInstruction subject instruction value = mkInstructionTask subject (makeInstructionTask instruction Nothing value)

showInstructionAbout :: !String !instruction a -> Task a | html instruction & iTask a
showInstructionAbout subject instruction context = mkInstructionTask subject (makeInstructionTask instruction (Just (visualizeAsHtmlDisplay context)) context)

makeInstructionTask :: !instruction (Maybe [HtmlTag]) a *TSt -> *(!TaskResult a,!*TSt) | html instruction & iTask a
makeInstructionTask instruction context value tst
	# (events, tst) = getEvents tst
	| isEmpty events
		# tst	= setInstruction (html instruction) context tst
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
		# (mask,iworld)				= defaultMask nEditV iworld
		# (events,tst)				= getEvents {TSt|tst & iworld = iworld}
		# updpaths					= events2Paths postValues
		= (determineEditorUpdates (editorId taskNr n) (Just n) updpaths mask mask oEditV nEditV,tst)
	
	// generate TUI definition for view
	visualize taskNr n stateV tst=:{TSt|iworld}
		# editV					= editorFrom stateV
		# (mask,iworld)			= defaultMask editV iworld
		= (visualizeAsEditor (editorId taskNr n) (Just n) mask editV,{TSt|tst & iworld = iworld})
				
listener :: !(Listener s a) -> View s | iTask a & iTask s & SharedVariable s
listener {listenerFrom} = Listener {Listener`|visualize = visualize}
where
	visualize v = visualizeAsHtmlDisplay (listenerFrom v)

idEditor	:: View s	| iTask s & SharedVariable s
idEditor = editor {editorFrom = id, editorTo = (\a _ -> a)}

idListener	:: View s	| iTask s & SharedVariable s
idListener = listener {listenerFrom = id}

updateShared :: question ![TaskAction s] !(DBid s) ![View s] -> Task (!Action, !s) | html question & iTask s & SharedVariable s
updateShared question actions sharedId views = mkInteractiveTask "updateShared" (makeSharedTask question actions sharedId views False)

updateSharedLocal :: question ![TaskAction s] !s ![View s] -> Task (!Action, !s) | html question & iTask s & SharedVariable s
updateSharedLocal question actions initial views =
				createDB initial
	>>= \sid.	mkInteractiveTask "updateShared" (makeSharedTask question actions sid views False)
	>>= \res.	deleteDB sid
	>>|			return res

makeSharedTask :: question ![TaskAction s] !(DBid s) ![View s] !Bool !*TSt -> (!TaskResult (!Action,!s),!*TSt) | html question & iTask s & SharedVariable s
makeSharedTask question actions sharedId views actionStored tst=:{taskNr, newTask}
	# (mbcvalue,tst) = readShared sharedId tst
	= case mbcvalue of
		Nothing
			= (TaskException (dynamic "updateShared: shared variable is deleted"), tst)
		Just cvalue
			# (anyEvent,tst)			= anyEvents tst
			| newTask || not anyEvent
				// generate TUI definition for new tasks or if there are no updates (refresh entire task)
				# tst = setTUIFunc createDefs (html question) tst
				= (TaskBusy, tst)
			| otherwise
				# (events,tst)		= getEvents tst
				# dpEvents			= [(s2dp key,value) \\ (key,value) <- events | isdps key]
				// determine new shared value by accumulating updates of all views
				# (nvalue,_,tst)	= foldl (updateSharedForView dpEvents) (cvalue,0,tst) views
				# tst=:{TSt|iworld=iworld=:{IWorld|store}}
									= tst
				# store				= storeValue sharedId nvalue store
				# tst				= {TSt|tst & iworld = {IWorld|iworld & store = store}}
				// check if action is triggered
				# (mbAction,tst)	= getAction events (map fst buttonActions) tst
				= case mbAction of
					Just action
						= (TaskFinished (action,fromJust mbcvalue),tst)
					Nothing
						// updates are calculated after tree is build, shared value maybe changed by other tasks
						# tst = setTUIFunc (createUpdates events) (html question) tst
						= (TaskBusy, tst)
where
	// generate TUI definitions for all views
	createDefs :: !*TSt -> *(InteractiveTask,*TSt)
	createDefs tst
		# (Just svalue,tst)		= readShared sharedId tst
		# (form,valid,_,tst)	= foldl (createDef svalue) ([],True,0,tst) views
		# menuActions			= evaluateConditions menuActions valid svalue
		# buttonActions			= evaluateConditions buttonActions valid svalue
		= (Definition (taskPanel taskId (html question) Nothing (Just form) (makeButtons baseEditorId buttonActions)) menuActions,tst)
	where
		createDef :: !a !*([TUIDef],Bool,ViewNr,*TSt) !(View a) -> *([TUIDef],Bool,ViewNr,*TSt) | iTask a
		createDef svalue (def,valid,n,tst) (Editor editor)
			# tst					= setTaskStoreFor taskNr (addStorePrefix n "value") svalue tst
			# ((ndef,nvalid),tst)	= editor.Editor`.visualize taskNr n svalue tst
			= (def ++ ndef,valid && nvalid,n + 1,tst)
		createDef svalue (def,valid,n,tst) (Listener listener) = (def ++ [listenerPanel svalue listener n],valid,n + 1,tst)
	
	// create TUI updates for all views
	createUpdates :: ![(String,String)] !*TSt -> *(InteractiveTask,*TSt)
	createUpdates postValues tst
		# (mbcvalue,tst)	= readShared sharedId tst
		# cvalue			= fromJust mbcvalue
		# (upd,valid,_,tst)	= foldl (detUpd cvalue postValues) ([],True,0,tst) views
		# menuActions		= evaluateConditions menuActions valid cvalue
		# buttonActions		= evaluateConditions buttonActions valid cvalue
		= (Updates (enables baseEditorId buttonActions ++ upd) menuActions,tst)
	where
		detUpd :: !a ![(String,String)] !*([TUIUpdate],Bool,ViewNr,*TSt) !(View a) -> *([TUIUpdate],Bool,ViewNr,*TSt) | iTask a
		detUpd nvalue postValues (upd,valid,n,tst) (Editor editor)
			# ((nupd,nvalid),tst)	= editor.determineUpdates taskNr n nvalue postValues tst
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
	listenerPanel value listener n = TUIHtmlPanel	{ TUIHtmlPanel
													| id = (editorId taskNr n)
													, html = toString (DivTag [] (html (listener.Listener`.visualize value)))
													, border = True, bodyCssClass = "task-context"
													, fieldLabel = Nothing
													, hideLabel = True
													, unstyled=True}
	
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
	menuActions		= getMenuActions actions
	buttonActions	= getButtonActions actions
			
addStorePrefix n key	= (toString n) +++ "_" +++ key
editorId taskNr n		= "tf-" +++ (taskNrToString taskNr) +++ "_" +++ (toString n)

//TODO remove encapsulating TUIPanel -> Form elements should be placed directly in their containers				
taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> ([TUIDef],[TUIButton])
taskPanel taskid description mbContext mbForm buttons
	= (items,taskButtons buttons) 
	//(TUIPanel {TUIPanel| layout = "auto", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "basic-task", fieldLabel = Nothing, renderingHint = 0, unstyled=True}, taskButtons buttons)
where
	items = //[taskDescriptionPanel ("description-"+++taskid) description] ++ //extracted from form
			(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
			//(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			(case mbForm of Just form = form; Nothing = [])
			
	//taskDescriptionPanel :: !String ![HtmlTag] -> TUIDef
	//taskDescriptionPanel panelid description = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] description), border = False, bodyCssClass = "task-description", fieldLabel = Nothing, hideLabel = True, unstyled=True} 
	
	taskContextPanel :: !String ![HtmlTag] -> TUIDef
	taskContextPanel panelid context = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] (html context)), border = False, bodyCssClass = "task-context", fieldLabel = Nothing, hideLabel = True, unstyled=True} 
	
	//taskFormPanel :: [TUIDef] -> TUIDef
	//taskFormPanel items = TUIPanel {TUIPanel| layout = "", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "task-form", fieldLabel = Nothing, renderingHint = 0, unstyled=True}
	
	taskButtons	:: [(Action,String,String,String,Bool)] -> [TUIButton]
	taskButtons buttons = [toTUIButton button id name value enable \\ (button,id,name,value,enable) <- buttons]

	toTUIButton :: !Action !String !String !String !Bool -> TUIButton
	toTUIButton action id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = actionText, iconCls = getActionIcon action}
	where
		actionText =	case action of
							ActionLabel text	= text
							ActionIcon text _	= text
							ActionParam text _	= text
							action				#str = (toString (toJSON action))
												| startsWith "\"Action" str	= subString 7 ((textSize str)-8) str
												| otherwise					= str

//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
makeButtons :: !String ![(Action, Bool)] -> [(!Action,!String,!String,!String,!Bool)]	
makeButtons editorId actions
	= [(b,editorId +++ "-action-" +++ toString i, "action",toString i, p) \\ (b, p) <- actions & i <- [0..] ]

//Generate the TUIUpdates for the buttons that are active when valid
enables :: !String ![(Action, Bool)] -> [TUIUpdate]	
enables editorId actions
	= [TUISetEnabled (editorId +++ "-action-" +++ toString i) p \\ (_,p) <- actions & i <- [0..]]

//Get button or menu action given by updates.
getAction :: [(String, String)] ![Action] !*TSt -> (!Maybe Action, !*TSt)
getAction events buttonActions tst
		# index = toInt (http_getValue "action" events "-1")
		| index <> -1
			= (Just (buttonActions !! index),tst)
		| otherwise
			= case fromJSON (fromString (http_getValue "menu" events "")) of
				Nothing	= case fromJSON (fromString (http_getValue "menuAndGroup" events "")) of
					Nothing	= (fromJSON (fromString (http_getValue "hotkey" events "")) ,tst)
					res		= (res, tst)
				res			= (res ,tst)
			
getButtonActions :: ![TaskAction a] -> [(!Action, ActionCondition a)]
getButtonActions actions = map getAction (filter isButtonAction actions)
where
	isButtonAction (ButtonAction _)			= True
	isButtonAction (ButtonAndMenuAction _)	= True
	isButtonAction _						= False
	getAction (ButtonAction a)			= a
	getAction (ButtonAndMenuAction a)	= a
	
getMenuActions :: ![TaskAction a] -> [(!Action, ActionCondition a)]
getMenuActions actions = map getAction (filter isMenuAction actions)
where
	isMenuAction (MenuAction _)				= True
	isMenuAction (ButtonAndMenuAction _)	= True
	isMenuAction (MenuParamAction _)		= True
	isMenuAction _							= False
	getAction (MenuAction a)				= a
	getAction (ButtonAndMenuAction a)		= a
	getAction (MenuParamAction (s,c))		= (ActionParam s "?", c)

evaluateConditions :: ![(!Action, ActionCondition a)] !Bool !a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,evaluateCondition cond valid value) \\ (action,cond) <- actions]

evaluateCondition :: !(ActionCondition a) !Bool !a -> Bool
evaluateCondition Always _ _		= True
evaluateCondition IfValid valid _ 	= valid
evaluateCondition (Predicate p) valid value
	= case valid of
		False	= p Invalid
		True	= p (Valid value)
			
//Throw away the chosen action part of the result
ignoreActionA :: (*TSt -> (!TaskResult (!Action,!a),*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
ignoreActionA f = \tst -> let (res,tst`) = f tst in (mapTaskResult snd res,tst`)
			
mapTaskResult :: !(a -> b) !(TaskResult a) -> TaskResult b
mapTaskResult f (TaskFinished x)	= TaskFinished (f x) 
mapTaskResult f (TaskBusy)			= TaskBusy
mapTaskResult f (TaskException e)	= TaskException e
