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
enterInformation :: question -> Task a | html question & iTask a
enterInformation question = mkInteractiveTask "enterInformation" (ignoreActionA (makeInformationTask question Nothing Nothing [ButtonAction (ActionOk, IfValid)] False))

enterInformationA :: question ![TaskAction a] -> Task (!Action,!a) | html question & iTask a
enterInformationA question actions = mkInteractiveTask "enterInformationA" (makeInformationTask question Nothing Nothing actions True)

updateInformation :: question a -> Task a | html question & iTask a
updateInformation question initial = mkInteractiveTask "updateInformation" (ignoreActionA (makeInformationTask question (Just initial) Nothing [ButtonAction (ActionOk, IfValid)] False)) 

updateInformationA :: question ![TaskAction a] a -> Task (!Action,!a) | html question & iTask a
updateInformationA question actions initial = mkInteractiveTask "updateInformationA" (makeInformationTask question (Just initial) Nothing actions True)

enterInformationAbout :: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = mkInteractiveTask "enterInformationAbout" (ignoreActionA (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)] False))

enterInformationAboutA :: question ![TaskAction a] b -> Task (!Action,!a) | html question & iTask a & iTask b
enterInformationAboutA question actions about = mkInteractiveTask "enterInformationAboutA" (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) actions True)
	
updateInformationAbout :: question b a -> Task a | html question & iTask a & iTask b 
updateInformationAbout question about initial = mkInteractiveTask "updateInformationAbout" (ignoreActionA (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)] False))

updateInformationAboutA	:: question ![TaskAction a] b a -> Task (!Action,!a) | html question & iTask a & iTask b
updateInformationAboutA question actions about initial = mkInteractiveTask "updateInformationAboutA" (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) actions True)

makeInformationTask :: question (Maybe a) (Maybe [HtmlTag]) ![TaskAction a] !Bool !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html question & iTask a
makeInformationTask question initial context actions actionStored tst=:{taskNr, newTask}
	# taskId			= taskNrToString taskNr
	# editorId			= "tf-" +++ taskNrToString taskNr
	# (ovalue,tst)		= readValue initial tst
	# (omask,tst)		= readMask initial tst
	# buttonActions		= getButtonActions actions
	# (anyUpd,tst)		= anyUpdates tst
	| newTask || not anyUpd
		// generate TUI definition
		# (form,valid) 	= visualizeAsEditor editorId Nothing omask ovalue
		# menuActions	= evaluateConditions (getMenuActions actions) valid ovalue
		# buttonActions	= evaluateConditions buttonActions valid ovalue
		# hotkeyActions	= evaluateHotkeyConditions (getHotkeyActions actions) valid ovalue
		# tst			= setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) (html question) menuActions hotkeyActions tst
		= (TaskBusy,tst)
	| otherwise
		//Check for user updates
		# (updates,tst) = getUserUpdates tst
		| isEmpty updates
			// no change for this task
			# tst = setTUIUpdates [] [] [] tst
			= (TaskBusy,tst)
		| otherwise
			# (nvalue,nmask,lmask,tst) = applyUpdates [(s2dp key,value) \\ (key,value) <- updates | isdps key] ovalue omask [] tst
			# (action,tst) = getAction updates (map fst buttonActions) tst
			| isJust action = (TaskFinished (fromJust action,nvalue),tst)
			| otherwise
				# tst				= setTaskStore "value" nvalue tst
				# tst				= setTaskStore "mask" nmask tst
				# (updates,valid)	= determineEditorUpdates editorId Nothing omask nmask lmask ovalue nvalue
				# menuActions		= evaluateConditions (getMenuActions actions) valid nvalue
				# buttonActions		= evaluateConditions buttonActions valid nvalue
				# hotkeyActions		= evaluateHotkeyConditions (getHotkeyActions actions) valid nvalue
				# tst				= setTUIUpdates (enables editorId buttonActions ++ updates) menuActions hotkeyActions tst
				= (TaskBusy, tst)
where
	readValue initial tst
		# (mbvalue,tst)	= getTaskStore "value" tst
		= case mbvalue of
			Just v		= (v,tst)
			Nothing		= case initial of
						Just i	= (i,tst)
						Nothing	= accWorldTSt defaultValue tst
	readMask initial tst
		# (mbmask,tst)	= getTaskStore "mask" tst
		= case mbmask of
			Just m = (m,tst)
			Nothing = case initial of
				Just v 
					# (mask,tst)	= accWorldTSt (defaultMask v) tst
					# tst			= setTaskStore "mask" mask tst // <- store the initial mask
					= (mask,tst)
				Nothing	= ([],tst) 


	applyUpdates [] val mask lmask tst = (val,mask,lmask,tst)
	applyUpdates [(p,v):us] val mask lmask tst=:{TSt|world}
		# (val,mask,lmask,world) = updateValueAndMask p v val mask lmask world
		= applyUpdates us val mask lmask {TSt|tst & world = world}

enterChoice :: question [a] -> Task a | html question & iTask a
enterChoice question []			= throw "enterChoice: cannot choose from empty option list"
enterChoice question options	= mkInteractiveTask "enterChoice" (ignoreActionA (makeChoiceTask question options -1 Nothing [ButtonAction (ActionOk, IfValid)]))

enterChoiceA :: question ![TaskAction a] [a] -> Task (!Action,!a) | html question & iTask a
enterChoiceA question actions []		= throw "enterChoice: cannot choose from empty option list"
enterChoiceA question actions options	= mkInteractiveTask "enterChoice" (makeChoiceTask question options -1 Nothing actions)

updateChoice :: question [a] Int -> Task a | html question & iTask a 
updateChoice question [] index		= throw "updateChoice: cannot choose from empty option list"
updateChoice question options index = mkInteractiveTask "updateChoice" (ignoreActionA (makeChoiceTask question options index Nothing [ButtonAction (ActionOk, IfValid)]))

updateChoiceA :: question ![TaskAction a] [a] Int -> Task (!Action,!a) | html question & iTask a 
updateChoiceA question actions [] index		= throw "updateChoice: cannot choose from empty option list"
updateChoiceA question actions options index	= mkInteractiveTask "updateChoice" (makeChoiceTask question options index Nothing actions)

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b
enterChoiceAbout question about []		= throw "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAbout question about options = mkInteractiveTask "enterChoiceAbout" (ignoreActionA (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

enterChoiceAboutA :: question ![TaskAction a] b [a] -> Task (!Action,!a) | html question & iTask a & iTask b
enterChoiceAboutA question actions about []		= throw "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAboutA question actions about options	= mkInteractiveTask "enterChoiceAbout" (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) actions)

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b
updateChoiceAbout question about [] index		= throw "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAbout question about options index  = mkInteractiveTask "updateChoiceAbout" (ignoreActionA (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

updateChoiceAboutA :: question ![TaskAction a] b [a] Int-> Task (!Action,!a) | html question & iTask a & iTask b
updateChoiceAboutA question actions about [] index			= throw "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAboutA question actions about options index		= mkInteractiveTask "updateChoiceAbout" (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) actions)

makeChoiceTask :: !question ![a] !Int (Maybe [HtmlTag]) ![TaskAction a] !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html question & iTask a
makeChoiceTask question options initsel context actions tst=:{taskNr, newTask}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# valid			= selection >= 0 && selection < length options	//Do we have a valid index
	# buttonActions = getButtonActions actions
	# (anyUpd,tst)	= anyUpdates tst
	| newTask || not anyUpd
		// generate TUI definition
		# radios = [TUIRadio {TUIRadio	| name = selectionId
										, id = ""
										, value = toString i
										, boxLabel = Just (toString (SpanTag [ClassAttr "task-choice"] (visualizeAsHtmlLabel option)))
										, fieldLabel = Nothing
										, hideLabel	= True
										, checked = (i == selection)
										} \\ option <- options & i <- [0..] ]
	
		# form 	= [TUIRadioGroup {TUIRadioGroup	| name = selectionId
												, id = selectionId
												, fieldLabel = Nothing
												, hideLabel = True
												, columns = 1
												, items = radios
												}]
		# menuActions	= evaluateConditions (getMenuActions actions) valid (if valid (options !! selection) (hd options))
		# buttonActions	= evaluateConditions buttonActions valid (if valid (options !! selection) (hd options))
		# hotkeyActions	= evaluateHotkeyConditions (getHotkeyActions actions) valid (if valid (options !! selection) (hd options))
		# tst			= setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) (html question) menuActions hotkeyActions tst
		= (TaskBusy, tst)
	| otherwise
		//Check for user updates
		# (updates,tst) = getUserUpdates tst
		| isEmpty updates
			// no change for this task
			# tst = setTUIUpdates [] [] [] tst
			= (TaskBusy,tst)
		| otherwise
			# (action,tst) = getAction updates (map fst buttonActions) tst
			= case action of
				// One of the buttons was pressed
				Just action	= (TaskFinished (action, if valid (options !! selection) (hd options)),tst)
				// The selection was updated
				Nothing
					// The selection was updated
					# index = toInt (http_getValue selectionId updates "-1")
					| index <> -1
						# valid			= index >= 0 && index < length options	//Recompute validity
						# tst			= setTaskStore "selection" index tst
						# menuActions	= evaluateConditions (getMenuActions actions) valid (if valid (options !! selection) (hd options))
						# buttonActions = evaluateConditions buttonActions valid (if valid (options !! selection) (hd options))
						# hotkeyActions	= evaluateHotkeyConditions (getHotkeyActions actions) valid (if valid (options !! selection) (hd options))
						# tst			= setTUIUpdates (enables editorId buttonActions) menuActions hotkeyActions tst
						= (TaskBusy, tst)	
					// Fallback case (shouldn't really happen)
					| otherwise
						# tst = setTUIUpdates [] [] [] tst
						= (TaskBusy, tst)

enterMultipleChoice :: question [a] -> Task [a] | html question & iTask a
enterMultipleChoice question options = mkInteractiveTask "enterMultipleChoice" (ignoreActionA (makeMultipleChoiceTask question options [] Nothing [ButtonAction (ActionOk, IfValid)]))

enterMultipleChoiceA :: question ![TaskAction [a]] [a] -> Task (!Action,![a]) | html question & iTask a
enterMultipleChoiceA question actions options = mkInteractiveTask "enterMultipleChoiceA" (makeMultipleChoiceTask question options [] Nothing actions)

updateMultipleChoice :: question [a] [Int] -> Task [a] | html question & iTask a
updateMultipleChoice question options indices = mkInteractiveTask "updateMultipleChoice" (ignoreActionA (makeMultipleChoiceTask question options indices Nothing [ButtonAction (ActionOk, IfValid)]))

updateMultipleChoiceA :: question ![TaskAction [a]] [a] [Int] -> Task (!Action,![a]) | html question & iTask a
updateMultipleChoiceA question actions options indices = mkInteractiveTask "updateMultipleChoiceA" (makeMultipleChoiceTask question options indices Nothing actions)

enterMultipleChoiceAbout :: question b [a] -> Task [a] | html question & iTask a & iTask b
enterMultipleChoiceAbout question about options = mkInteractiveTask "enterMultipleChoiceAbout" (ignoreActionA (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

enterMultipleChoiceAboutA :: question ![TaskAction [a]] b [a] -> Task (!Action,![a]) | html question & iTask a & iTask b
enterMultipleChoiceAboutA question actions about options = mkInteractiveTask "enterMultipleChoiceAboutA" (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)) actions)

updateMultipleChoiceAbout :: question b [a] [Int] -> Task [a] | html question & iTask a & iTask b
updateMultipleChoiceAbout question about options indices = mkInteractiveTask "updateMultipleChoiceAbout" (ignoreActionA (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

updateMultipleChoiceAboutA :: question ![TaskAction [a]] b [a] [Int] -> Task (!Action,![a]) | html question & iTask a & iTask b
updateMultipleChoiceAboutA question actions about options indices = mkInteractiveTask "updateMultipleChoiceAboutA" (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)) actions)

makeMultipleChoiceTask :: question [a] [Int] (Maybe [HtmlTag]) ![TaskAction [a]] !*TSt -> (!TaskResult (!Action,![a]),!*TSt) | html question & iTask a
makeMultipleChoiceTask question options initsel context actions tst=:{taskNr, newTask}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# buttonActions	= evaluateConditions (getButtonActions actions) True (select selection options)
	# (anyUpd,tst)	= anyUpdates tst
	| newTask || not anyUpd
		// generate TUI definition
		# checks	= [isMember i selection \\ i <- [0..(length options) - 1]]
		# cboxes	= [TUICheckBox 
					  {TUICheckBox
					  | name = "sel-" +++ toString i
					  , id = editorId +++ "-cb-" +++ toString i
					  , value = toString i
					  , fieldLabel = Nothing
					  , hideLabel = True
					  , boxLabel = Just (visualizeAsTextLabel o)
					  , checked = c} \\ o <- options & i <- [0..] & c <- checks ]
		# form			= [ TUICheckBoxGroup {TUICheckBoxGroup |name = "selection", id = editorId +++ "-selection", fieldLabel = Nothing, hideLabel = True, columns = 1, items = cboxes}]
		# menuActions	= evaluateConditions (getMenuActions actions) True (select selection options)
		# hotkeyActions = evaluateHotkeyConditions (getHotkeyActions actions) True (select selection options)
		# tst			= setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) (html question) menuActions hotkeyActions tst
		= (TaskBusy, tst)
	| otherwise
		//Check for user updates
		# (updates,tst) = getUserUpdates tst
		| isEmpty updates
			// no change for this task
			# tst = setTUIUpdates [] [] [] tst
			= (TaskBusy,tst)
		| otherwise
			// One of the buttons was pressed
			# (action,tst) = getAction updates (map fst buttonActions) tst
			= case action of
				Just action	= (TaskFinished (action, select selection options),tst)
				Nothing
					// Perhaps the selection was changed
					# mbSel		= parseSelection updates 
					# selection	= case mbSel of Nothing = selection; Just sel = map toInt sel
					# tst		= setTaskStore "selection" (sort selection) tst
					# hotkeyActions = evaluateHotkeyConditions (getHotkeyActions actions) True (select selection options)
					# tst		= setTUIUpdates [] (evaluateConditions (getMenuActions actions) True (select selection options)) hotkeyActions tst
					= (TaskBusy, tst)
where
	parseSelection :: [(String,String)] -> Maybe [String]
	parseSelection updates = fromJSON (http_getValue "selection" updates "[]")	

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]

//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkInteractiveTask "showMessage" (ignoreActionV (makeMessageTask message Nothing [ButtonAction (ActionOk, IfValid)]))

showMessageA :: message ![TaskAction Void] -> Task Action | html message
showMessageA message actions = mkInteractiveTask "showMessageA" (makeMessageTask message Nothing actions)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkInteractiveTask "showMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

showMessageAboutA :: message ![TaskAction Void] a -> Task Action | html message & iTask a
showMessageAboutA message actions about = mkInteractiveTask "showMessageAboutA" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) actions)
	
showStickyMessage :: message -> Task Void | html message
showStickyMessage message = mkInteractiveTask "showStickyMessage" (ignoreActionV (makeMessageTask message Nothing []))

showStickyMessageAbout :: message a -> Task Void | html message & iTask a
showStickyMessageAbout message about = mkInteractiveTask "showStickyMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) []))

requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkInteractiveTask "requestConfirmation" requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = (makeMessageTask question Nothing [ButtonAction (ActionNo, Always),ButtonAction (ActionYes, Always)]) tst
		= (mapTaskResult (\a -> case a of ActionYes = True; _ = False) result, tst)
								
requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = mkInteractiveTask "requestConfirmationAbout" requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = (makeMessageTask question (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionNo, Always),ButtonAction (ActionYes, IfValid)]) tst
		= (mapTaskResult (\a -> case a of ActionYes = True; _ = False) result, tst)

makeMessageTask :: message (Maybe [HtmlTag]) ![TaskAction Void] *TSt -> (!TaskResult Action,!*TSt) | html message
makeMessageTask message context actions tst=:{taskNr}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# buttonActions	= getButtonActions actions
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# menuActions	= evaluateConditions (getMenuActions actions) True Void
		# buttonActions	= evaluateConditions buttonActions True Void
		# hotkeyActions = evaluateHotkeyConditions (getHotkeyActions actions) True Void
		# tst			= setTUIMessage (taskPanel taskId (html message) context Nothing (makeButtons editorId buttonActions)) (html message) menuActions hotkeyActions tst
		= (TaskBusy, tst)
	| otherwise
		# (action,tst) = getAction updates (map fst buttonActions) tst
		= case action of
			Just action	=	(TaskFinished action, tst)
			Nothing =		(TaskBusy, tst)

showInstruction :: !String !instruction -> Task Void | html instruction
showInstruction title instruction = mkInstructionTask title (makeInstructionTask instruction Nothing)

showInstructionAbout :: !String !instruction b -> Task Void | html instruction & iTask b
showInstructionAbout title instruction context = mkInstructionTask title (makeInstructionTask instruction (Just (visualizeAsHtmlDisplay context)))

makeInstructionTask :: !instruction (Maybe [HtmlTag]) *TSt -> *(!TaskResult Void,!*TSt) | html instruction
makeInstructionTask instruction context tst
	# (updates, tst) = getUserUpdates tst
	| isEmpty updates
		= case tst.tree of
			(TTInstructionTask ti _ _)	= (TaskBusy ,{tst & tree = TTInstructionTask ti (html instruction) context})
			_							= (TaskException (dynamic "Illegal node in makeInstructionTask"), tst)
	| otherwise
		= (TaskFinished Void,tst)
			
//Shared value tasks
:: View s = E.a: Listener (Listener` s a) | E.a: Editor (Editor` s a)
:: Listener` s a =	{ visualize :: s -> [HtmlTag] }
:: Editor` s a =	{ getNewValue :: Int [(!DataPath,!String)] s s *TSt -> *(!s,!*TSt)
					, determineUpdates :: !TaskNr Int s *TSt -> *((![TUIUpdate],!Bool),!*TSt)
					, visualize :: !TaskNr Int s *TSt -> *((![TUIDef],!Bool),!*TSt)
					}

editor :: !(Editor s a) -> View s | iTask a & iTask s & SharedVariable s
editor {editorFrom, editorTo} = Editor {getNewValue = getNewValue, determineUpdates = determineUpdates, visualize = visualize}
where
	getNewValue n updates old cur tst
		# oEditV				= editorFrom old
		# myUpdates				= filter (\upd -> dataPathHasSubEditorIdx (fst upd) n) updates
		| isEmpty myUpdates
			# tst				= setTaskStore (addStorePrefix n "value") oEditV tst
			= (cur,tst)
		| otherwise
			//first apply basic value updates to get value represented by user interface on client
			# basicValueUpdates	= filter (\upd -> not (dataPathHasConsFlag (fst upd))) myUpdates
			# (oEditV,tst)		= applyUpdates basicValueUpdates oEditV tst
			# tst				= setTaskStore (addStorePrefix n "value") oEditV tst
			//then apply constructor updates
			# consUpdates		= filter (\upd -> dataPathHasConsFlag (fst upd)) myUpdates
			# (nEditV,tst)		= applyUpdates consUpdates oEditV tst
			= (mergeValues old cur (editorTo nEditV old), tst)
		
	determineUpdates taskNr n new tst
		# (mbOEditV,tst)	= getTaskStoreFor taskNr (addStorePrefix n "value") tst
		# oEditV			= fromJust mbOEditV
		# nEditV			= editorFrom new
		# (mask,tst)		= accWorldTSt (defaultMask nEditV) tst
		= (determineEditorUpdates (editorId taskNr n) (Just n) mask mask [] oEditV nEditV,tst)
	
	visualize taskNr n stateV tst
		# editV			= editorFrom stateV
		# (mask,tst)	= accWorldTSt (defaultMask editV) tst
		= (visualizeAsEditor (editorId taskNr n) (Just n) mask editV,tst)
				
	applyUpdates [] val tst = (val,tst)
	applyUpdates [(p,v):us] val tst=:{TSt|world}
		# (val,world) = updateValue p v val world
		= applyUpdates us val {TSt|tst & world = world}
		
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
	# (mbcvalue,tst)		= readShared sharedId tst
	= case mbcvalue of
		Nothing
			= (TaskException (dynamic "updateShared: shared variable is deleted"), tst)
		Just cvalue
			# (anyUpd,tst)			= anyUpdates tst
			| newTask || not anyUpd
				// generate TUI definition
				# tst = setTUIFunc createDefs (html question) tst
				= (TaskBusy, tst)
			| otherwise
				# (updates,tst)		= getUserUpdates tst
				# dpUpdates			= [(s2dp key,value) \\ (key,value) <- updates | isdps key]
				# (nvalue,_,tst)	= foldl (updateV dpUpdates) (cvalue,0,tst) views
				# nvalue			= gMakeSharedCopy{|*|} nvalue sharedId
				# tst				= {tst & dataStore = storeValue sharedId nvalue tst.dataStore}
				# (mbAction,tst)	= getAction updates (map fst buttonActions) tst
				= case mbAction of
					Just action
						# (result,tst)	= gMakeLocalCopy{|*|} (fromJust mbcvalue) tst
						= (TaskFinished (action,result),tst)
					Nothing
						# tst = setTUIFunc createUpdates (html question) tst
						= (TaskBusy, tst)
where
	createDefs tst
		# (Just svalue,tst)		= readShared sharedId tst
		# (form,valid,_,tst)	= foldl (createDef svalue) ([],True,0,tst) views
		# menuActions			= evaluateConditions menuActions valid svalue
		# buttonActions			= evaluateConditions buttonActions valid svalue
		# hotkeyActions			= evaluateHotkeyConditions (getHotkeyActions actions) valid svalue
		= (Definition (taskPanel taskId (html question) Nothing (Just form) (makeButtons baseEditorId buttonActions)) menuActions hotkeyActions,tst)
	
	createDef svalue (def,valid,n,tst) (Editor editor)
		# tst					= setTaskStoreFor taskNr (addStorePrefix n "value") svalue tst
		# ((ndef,nvalid),tst)	= editor.Editor`.visualize taskNr n svalue tst
		= (def ++ ndef,valid && nvalid,n + 1,tst)
		
	createDef svalue (def,valid,n,tst) (Listener listener) = (def ++ [listenerPanel svalue listener n],valid,n + 1,tst)
	
	createUpdates tst
		# (mbcvalue,tst)	= readShared sharedId tst
		# cvalue			= fromJust mbcvalue
		# (upd,valid,_,tst)	= foldl (detUpd cvalue) ([],True,0,tst) views
		# menuActions		= evaluateConditions menuActions valid cvalue
		# buttonActions		= evaluateConditions buttonActions valid cvalue
		# hotkeyActions		= evaluateHotkeyConditions (getHotkeyActions actions) valid cvalue
		= (Updates (enables baseEditorId buttonActions ++ upd) menuActions hotkeyActions,tst)
		
	updateV updates (cvalue,n,tst) (Editor editor)
		# (ovalue,tst)			= readValue n tst
		# (nvalue,tst)			= editor.getNewValue n updates ovalue cvalue tst
		= (nvalue,n + 1,tst)
	updateV _ (cvalue,n,tst) (Listener _) = (cvalue,n + 1,tst)
	
	detUpd nvalue (upd,valid,n,tst) (Editor editor)
		# ((nupd,nvalid),tst)	= editor.determineUpdates taskNr n nvalue tst
		# tst					= setTaskStoreFor taskNr (addStorePrefix n "value") nvalue tst
		= (upd ++ nupd,	valid && nvalid, n + 1, tst)
	detUpd nvalue (upd,valid,n,tst) (Listener listener) = ([TUIReplace (editorId taskNr n) (listenerPanel nvalue listener n):upd],valid,n + 1,tst)
	
	listenerPanel value listener n = TUIHtmlPanel {TUIHtmlPanel| id = (editorId taskNr n), html = toString (DivTag [] (html (listener.Listener`.visualize value))), border = True, bodyCssClass = "task-context", fieldLabel = Nothing, hideLabel = True, unstyled=True}
	
	taskId			= taskNrToString taskNr
	baseEditorId	= "tf-" +++ taskId
	menuActions		= getMenuActions actions
	buttonActions	= getButtonActions actions
		
	readValue n tst
		# (mbvalue,tst)	= getTaskStore (addStorePrefix n "value") tst
		= case mbvalue of
			Just v		= (v,tst)
			Nothing		= abort "cannot get local value"
			
	readShared sid tst=:{dataStore,world}
		# (mbvalue,dstore,world) = loadValue sid dataStore world
		# tst = {tst & dataStore = dstore, world = world}
		= case mbvalue of
			Just v		= (Just (gMakeSharedCopy{|*|} v sid),tst)
			Nothing		= (Nothing,tst)
			
addStorePrefix n key	= (toString n) +++ "_" +++ key
editorId taskNr n		= "tf-" +++ (taskNrToString taskNr) +++ "_" +++ (toString n)

//TODO remove encapsulating TUIPanel -> Form elements should be placed directly in their containers				
taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> (TUIDef,[TUIButton])
taskPanel taskid description mbContext mbForm buttons
	= (TUIPanel {TUIPanel| layout = "", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "basic-task", fieldLabel = Nothing, renderingHint = 0, unstyled=True}, taskButtons buttons)
where
	items = //[taskDescriptionPanel ("description-"+++taskid) description] ++ //extracted from form
			(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
			(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			
	//taskDescriptionPanel :: !String ![HtmlTag] -> TUIDef
	//taskDescriptionPanel panelid description = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] description), border = False, bodyCssClass = "task-description", fieldLabel = Nothing, hideLabel = True, unstyled=True} 
	
	taskContextPanel :: !String ![HtmlTag] -> TUIDef
	taskContextPanel panelid context = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] (html context)), border = False, bodyCssClass = "task-context", fieldLabel = Nothing, hideLabel = True, unstyled=True} 
	
	taskFormPanel :: [TUIDef] -> TUIDef
	taskFormPanel items = TUIPanel {TUIPanel| layout = "form", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "task-form", fieldLabel = Nothing, renderingHint = 0, unstyled=True}
	
	taskButtons	:: [(Action,String,String,String,Bool)] -> [TUIButton]
	taskButtons buttons = [toTUIButton button id name value enable \\ (button,id,name,value,enable) <- buttons]

	toTUIButton :: !Action !String !String !String !Bool -> TUIButton
	toTUIButton action id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = actionText, iconCls = getActionIcon action}
	where
		actionText =	case action of
							ActionLabel text	= text
							ActionIcon text _	= text
							ActionParam text _	= text
							action				#str = printToString action
												| startsWith "Action" str	= subString 6 ((textSize str)-1) str
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
getAction updates buttonActions tst
		# index = toInt (http_getValue "action" updates "-1")
		| index <> -1
			= (Just (buttonActions !! index),tst)
		| otherwise
			= case parseString (http_getValue "menu" updates "") of
				Nothing	= case parseString (http_getValue "menuAndGroup" updates "") of
					Nothing	= (parseString (http_getValue "hotkey" updates "") ,tst)
					res		= (res, tst)
				res			= (res ,tst)
			
getButtonActions :: ![TaskAction a] -> [ActionWithCond a]
getButtonActions actions = map getAction (filter isButtonAction actions)
where
	isButtonAction (ButtonAction _)			= True
	isButtonAction (ButtonAndMenuAction _)	= True
	isButtonAction _						= False
	getAction (ButtonAction a)			= a
	getAction (ButtonAndMenuAction a)	= a
	
getMenuActions :: ![TaskAction a] -> [ActionWithCond a]
getMenuActions actions = map getAction (filter isMenuAction actions)
where
	isMenuAction (MenuAction _)				= True
	isMenuAction (ButtonAndMenuAction _)	= True
	isMenuAction (MenuParamAction _)		= True
	isMenuAction (MenuActionWithHotkey _ _)	= True
	isMenuAction _							= False
	getAction (MenuAction a)				= a
	getAction (ButtonAndMenuAction a)		= a
	getAction (MenuParamAction (s,c))		= (ActionParam s "?", c)
	getAction (MenuActionWithHotkey a _)	= a
	
getHotkeyActions :: ![TaskAction a] -> [(ActionWithCond a, Hotkey)]
getHotkeyActions actions = map getAction (filter isHotkeyAction actions)
where
	isHotkeyAction (HotkeyAction _ _)			= True
	isHotkeyAction (MenuActionWithHotkey _ _)	= True
	isHotkeyAction _							= False
	getAction (HotkeyAction a hk)				= (a, hk)
	getAction (MenuActionWithHotkey a hk)		= (a, hk)

evaluateConditions :: ![ActionWithCond a] !Bool !a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,evaluateCondition cond valid value) \\ (action,cond) <- actions]
			
evaluateHotkeyConditions :: ![(ActionWithCond a, !Hotkey)] !Bool !a -> [(Action, Hotkey)]
evaluateHotkeyConditions actions valid value = [(action, hk) \\ ((action,cond), hk) <- actions | evaluateCondition cond valid value]

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
			
ignoreActionV :: (*TSt -> (!TaskResult Action,!*TSt)) -> (*TSt -> (!TaskResult Void,!*TSt))
ignoreActionV f = \tst -> let (res,tst`) = f tst in (mapTaskResult (\_ -> Void) res,tst`)

mapTaskResult :: !(a -> b) !(TaskResult a) -> TaskResult b
mapTaskResult f (TaskFinished x)	= TaskFinished (f x) 
mapTaskResult f (TaskBusy)			= TaskBusy
mapTaskResult f (TaskException e)	= TaskException e

notifyUser :: message UserName -> Task Void | html message
notifyUser message username = mkInstantTask "notifyUser" (\tst -> (TaskFinished Void,tst))

notifyGroup :: message Role -> Task Void | html message
notifyGroup message role = mkInstantTask "notifyGroup" (\tst -> (TaskFinished Void,tst))