implementation module InteractionTasks

import	StdList, StdOrdList, StdTuple, StdBool, StdMisc, Text
from	StdFunc import id, const
import	TSt, ProcessDB

import	GenVisualize, GenUpdate, Util, Http	

from iTasks import class iTask(..)

class html a 
where
	html :: a -> [HtmlTag]
	
instance html String
where
	html s = [Text s]
	
instance html [HtmlTag]
where
	html h = h

always	:: ActionPredicate a | iTask a
always = (\_ _ -> True)

ifValid	:: ActionPredicate a | iTask a
ifValid = (\_ valid -> valid)

//Input tasks
enterInformation :: question -> Task a | html question & iTask a
enterInformation question = mkInteractiveTask "enterInformation" (ignoreActionA (makeInformationTask question Nothing Nothing [] [ActionOk] [] False))

enterInformationA :: question [Action] [Action] [(Action,ActionPredicate a)] -> Task (!Action,!a) | html question & iTask a
enterInformationA question aAlways aValid aAccepted = mkInteractiveTask "enterInformationA" (makeInformationTask question Nothing Nothing aAlways aValid aAccepted True)

updateInformation :: question a -> Task a | html question & iTask a
updateInformation question initial = mkInteractiveTask "updateInformation" (ignoreActionA (makeInformationTask question (Just initial) Nothing [] [ActionOk] [] False)) 

updateInformationA :: question [Action] [Action] [(Action,ActionPredicate a)] a -> Task (!Action,!a) | html question & iTask a
updateInformationA question aAlways aValid aAccepted initial = mkInteractiveTask "updateInformationA" (makeInformationTask question (Just initial) Nothing aAlways aValid aAccepted True)

enterInformationAbout :: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = mkInteractiveTask "enterInformationAbout" (ignoreActionA (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] [] False))

enterInformationAboutA :: question [Action] [Action] [(Action,ActionPredicate a)] b -> Task (!Action,!a) | html question & iTask a & iTask b
enterInformationAboutA question aAlways aValid aAccepted about = mkInteractiveTask "enterInformationAboutA" (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) aAlways aValid aAccepted True)
	
updateInformationAbout :: question b a -> Task a | html question & iTask a & iTask b 
updateInformationAbout question about initial = mkInteractiveTask "updateInformationAbout" (ignoreActionA (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] [] False))

updateInformationAboutA	:: question [Action] [Action] [(Action,ActionPredicate a)] b a -> Task (!Action,!a) | html question & iTask a & iTask b
updateInformationAboutA question aAlways aValid aAccepted about initial = mkInteractiveTask "updateInformationAboutA" (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) aAlways aValid aAccepted True)

makeInformationTask :: question (Maybe a) (Maybe [HtmlTag]) [Action] [Action] [(Action,ActionPredicate a)] !Bool !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html question & iTask a
makeInformationTask question initial context aAlways aValid aAccepted actionStored tst=:{taskNr}
	# taskId			= taskNrToString taskNr
	# editorId			= "tf-" +++ taskNrToString taskNr
	# (ovalue,tst)		= readValue initial tst
	# (omask,tst)		= readMask initial tst
	//Check for user updates
	# (updates,tst) = getUserUpdates tst	
	| isEmpty updates
		# (form,valid) 	= visualizeAsEditor editorId omask ovalue
		# tst			= setAccActions [(action,pred ovalue valid) \\ (action,pred) <- aAccepted] tst
		# tst			= setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId aAlways aValid valid)) tst
		= (TaskBusy,tst)
	| otherwise
		# (nvalue,nmask,lmask,tst) = applyUpdates updates ovalue omask [] tst
		# (action,tst) = getAction updates aAlways aValid tst
		| isJust action = (TaskFinished (fromJust action,nvalue),tst)
		| otherwise
			# tst				= setTaskStore "value" nvalue tst
			# tst				= setTaskStore "mask" nmask tst
			# (updates,valid)	= determineEditorUpdates editorId omask nmask lmask ovalue nvalue
			# tst				= setAccActions [(action,pred nvalue valid) \\ (action,pred) <- aAccepted] tst
			# tst				= setTUIUpdates (enables editorId aAlways aValid valid ++ updates) tst
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
enterChoice question []			= abort "enterChoice: cannot choose from empty option list"
enterChoice question options	= mkInteractiveTask "enterChoice" (ignoreActionA (makeChoiceTask question options -1 Nothing [] [ActionOk] []))

enterChoiceA :: question [Action] [Action] [(Action,ActionPredicate a)] [a] -> Task (!Action,!a) | html question & iTask a
enterChoiceA question aAlways aValid aAccepted []		= abort "enterChoice: cannot choose from empty option list"
enterChoiceA question aAlways aValid aAccepted options	= mkInteractiveTask "enterChoice" (makeChoiceTask question options -1 Nothing aAlways aValid aAccepted)

updateChoice :: question [a] Int -> Task a | html question & iTask a 
updateChoice question [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoice question options index = mkInteractiveTask "updateChoice" (ignoreActionA (makeChoiceTask question options index Nothing [] [ActionOk] []))

updateChoiceA :: question [Action] [Action] [(Action,ActionPredicate a)] [a] Int -> Task (!Action,!a) | html question & iTask a 
updateChoiceA question aAlways aValid aAccepted [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoiceA question aAlways aValid aAccepted options index	= mkInteractiveTask "updateChoice" (makeChoiceTask question options index Nothing aAlways aValid aAccepted)

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b
enterChoiceAbout question about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAbout question about options = mkInteractiveTask "enterChoiceAbout" (ignoreActionA (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] []))

enterChoiceAboutA :: question [Action] [Action] [(Action,ActionPredicate a)] b [a] -> Task (!Action,!a) | html question & iTask a & iTask b
enterChoiceAboutA question aAlways aValid aAccepted about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAboutA question aAlways aValid aAccepted about options	= mkInteractiveTask "enterChoiceAbout" (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) aAlways aValid aAccepted)

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b
updateChoiceAbout question about [] index		= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAbout question about options index  = mkInteractiveTask "updateChoiceAbout" (ignoreActionA (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] []))

updateChoiceAboutA :: question [Action] [Action] [(Action,ActionPredicate a)] b [a] Int-> Task (!Action,!a) | html question & iTask a & iTask b
updateChoiceAboutA question aAlways aValid aAccepted about [] index			= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAboutA question aAlways aValid aAccepted about options index	= mkInteractiveTask "updateChoiceAbout" (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) aAlways aValid aAccepted)

makeChoiceTask :: !question ![a] !Int (Maybe [HtmlTag]) ![Action] ![Action] ![(Action,ActionPredicate a)] !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html question & iTask a
makeChoiceTask question options initsel context aAlways aValid aAccepted tst=:{taskNr}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# valid			= selection >= 0 && selection < length options	//Do we have a valid index
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# radios = [TUIRadio {TUIRadio	| name = selectionId
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
		# tst = setAccActions [(action,pred (if valid (options !! selection) (hd options)) valid) \\ (action,pred) <- aAccepted] tst
		# tst = setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId aAlways aValid valid)) tst
		= (TaskBusy, tst)
	| otherwise
		# (action,tst) = getAction updates aAlways aValid tst
		= case action of
			// One of the buttons was pressed
			Just action	= (TaskFinished (action, if valid (options !! selection) (hd options)),tst)
			// The selection was updated
			Nothing
				// The selection was updated
				# index = toInt (http_getValue selectionId updates "-1")
				| index <> -1
					# valid		= index >= 0 && index < length options	//Recompute validity
					# tst		= setTaskStore "selection" index tst
					# tst		= setAccActions [(action,pred (if valid (options !! selection) (hd options)) valid) \\ (action,pred) <- aAccepted] tst
					# tst		= setTUIUpdates (enables editorId aAlways aValid valid) tst
					= (TaskBusy, tst)	
				// Fallback case (shouldn't really happen)
				| otherwise
					# tst		= setTUIUpdates [] tst
					= (TaskBusy, tst)

enterMultipleChoice :: question [a] -> Task [a] | html question & iTask a
enterMultipleChoice question options = mkInteractiveTask "enterMultipleChoice" (ignoreActionA (makeMultipleChoiceTask question options [] Nothing [ActionOk] []))

enterMultipleChoiceA :: question [Action] [(Action,ActionPredicate [a])] [a] -> Task (!Action,![a]) | html question & iTask a
enterMultipleChoiceA question aAlways aAccepted options = mkInteractiveTask "enterMultipleChoiceA" (makeMultipleChoiceTask question options [] Nothing aAlways aAccepted)

updateMultipleChoice :: question [a] [Int] -> Task [a] | html question & iTask a
updateMultipleChoice question options indices = mkInteractiveTask "updateMultipleChoice" (ignoreActionA (makeMultipleChoiceTask question options indices Nothing [ActionOk] []))

updateMultipleChoiceA :: question [Action] [(Action,ActionPredicate [a])] [a] [Int] -> Task (!Action,![a]) | html question & iTask a
updateMultipleChoiceA question aAlways aAccepted options indices = mkInteractiveTask "updateMultipleChoiceA" (makeMultipleChoiceTask question options indices Nothing aAlways aAccepted)

enterMultipleChoiceAbout :: question b [a] -> Task [a] | html question & iTask a & iTask b
enterMultipleChoiceAbout question about options = mkInteractiveTask "enterMultipleChoiceAbout" (ignoreActionA (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)) [ActionOk] []))

enterMultipleChoiceAboutA :: question [Action] [(Action,ActionPredicate [a])] b [a] -> Task (!Action,![a]) | html question & iTask a & iTask b
enterMultipleChoiceAboutA question aAlways aAccepted about options = mkInteractiveTask "enterMultipleChoiceAboutA" (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)) aAlways aAccepted)

updateMultipleChoiceAbout :: question b [a] [Int] -> Task [a] | html question & iTask a & iTask b
updateMultipleChoiceAbout question about options indices = mkInteractiveTask "updateMultipleChoiceAbout" (ignoreActionA (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)) [ActionOk] []))

updateMultipleChoiceAboutA :: question [Action] [(Action,ActionPredicate [a])] b [a] [Int] -> Task (!Action,![a]) | html question & iTask a & iTask b
updateMultipleChoiceAboutA question aAlways aAccepted about options indices = mkInteractiveTask "updateMultipleChoiceAboutA" (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)) aAlways aAccepted)

makeMultipleChoiceTask :: question [a] [Int] (Maybe [HtmlTag]) [Action] [(Action,ActionPredicate [a])] !*TSt -> (!TaskResult (!Action,![a]),!*TSt) | html question & iTask a
makeMultipleChoiceTask question options initsel context aAlways aAccepted tst=:{taskNr}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
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
		# form = [ TUICheckBoxGroup {TUICheckBoxGroup |name = "selection", id = editorId +++ "-selection", fieldLabel = Nothing, hideLabel = True, columns = 1, items = cboxes}]
		# tst = setAccActions [(action,pred (select selection options) True) \\ (action,pred) <- aAccepted] tst
		# tst = setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId aAlways [] True)) tst
		= (TaskBusy, tst)
	| otherwise
		// One of the buttons was pressed
		# (action,tst) = getAction updates aAlways [] tst
		= case action of
			Just action	= (TaskFinished (action, select selection options),tst)
			Nothing
				// Perhaps the selection was changed
				# mbSel		= parseSelection updates 
				# selection	= case mbSel of Nothing = selection; Just sel = map toInt sel
				# tst		= setTaskStore "selection" (sort selection) tst
				# tst		= setAccActions [(action,pred (select selection options) True) \\ (action,pred) <- aAccepted] tst
				# tst		= setTUIUpdates [] tst
				= (TaskBusy, tst)
where
	parseSelection :: [(String,String)] -> Maybe [String]
	parseSelection updates = fromJSON (http_getValue "selection" updates "[]")	

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]

//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkInteractiveTask "showMessage" (ignoreActionV (makeMessageTask message Nothing [ActionOk] []))

showMessageA :: message [Action] [(Action,ActionPredicate Void)] -> Task Action | html message
showMessageA message aAlways aAccepted = mkInteractiveTask "showMessageA" (makeMessageTask message Nothing aAlways aAccepted)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkInteractiveTask "showMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [ActionOk] []))

showMessageAboutA :: message [Action] [(Action,ActionPredicate Void)] a -> Task Action | html message & iTask a
showMessageAboutA message aAlways aAccepted about = mkInteractiveTask "showMessageAboutA" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) aAlways aAccepted)
	
showStickyMessage :: message -> Task Void | html message
showStickyMessage message = mkInteractiveTask "showStickyMessage" (ignoreActionV (makeMessageTask message Nothing [] []))

showStickyMessageAbout :: message a -> Task Void | html message & iTask a
showStickyMessageAbout message about = mkInteractiveTask "showStickyMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [] []))

requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkInteractiveTask "requestConfirmation" requestConfirmation`
where
	requestConfirmation` tst 
		# (result,tst) = (makeMessageTask question Nothing [ActionNo,ActionYes] []) tst
		= (mapTaskResult (\a -> case a of ActionYes = True; _ = False) result, tst)
								
requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = mkInteractiveTask "requestConfirmationAbout" requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (result,tst) = (makeMessageTask question (Just (visualizeAsHtmlDisplay about)) [ActionNo,ActionYes] []) tst
		= (mapTaskResult (\a -> case a of ActionYes = True; _ = False) result, tst)

makeMessageTask :: message (Maybe [HtmlTag]) [Action] [(Action,ActionPredicate Void)] *TSt -> (!TaskResult Action,!*TSt) | html message
makeMessageTask message context aAlways aAccepted tst=:{taskNr}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# tst = setAccActions [(action,pred Void True) \\ (action,pred) <- aAccepted] tst
		# tst = setTUIDef (taskPanel taskId (html message) context Nothing (makeButtons editorId aAlways [] True)) tst
		= (TaskBusy, tst)
	| otherwise
		# (action,tst) = getAction updates aAlways [] tst
		= case action of
			Just action	=	(TaskFinished action, tst)
			Nothing =		(TaskBusy, tst)
	
taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> TUIDef
taskPanel taskid description mbContext mbForm buttons
	= TUIPanel {TUIPanel| layout = "", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Just (taskButtons buttons), bodyCssClass = "basic-task", fieldLabel = Nothing, renderingHint = 0, unstyled=False}
where
	items = [taskDescriptionPanel ("description-"+++taskid) description] ++
			(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
			(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			
	taskDescriptionPanel :: !String ![HtmlTag] -> TUIDef
	taskDescriptionPanel panelid description = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] description), border = False, bodyCssClass = "task-description", fieldLabel = Nothing, hideLabel = True} 
	
	taskContextPanel :: !String ![HtmlTag] -> TUIDef
	taskContextPanel panelid context = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] (html context)), border = False, bodyCssClass = "task-context", fieldLabel = Nothing, hideLabel = True} 
	
	taskFormPanel :: [TUIDef] -> TUIDef
	taskFormPanel items = TUIPanel {TUIPanel| layout = "form", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "task-form", fieldLabel = Nothing, renderingHint = 0, unstyled=False}
	
	taskButtons	:: [(Action,String,String,String,Bool)] -> [TUIDef]
	taskButtons buttons = [TUIButton (toTUIButton button id name value enable) \\ (button,id,name,value,enable) <- buttons]

	toTUIButton :: !Action !String !String !String !Bool -> TUIButton
	toTUIButton action id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = actionText, iconCls = actionIcon}
	where
		actionText =	case action of
							ActionLabel text	= text
							ActionIcon text _	= text
							ActionParam text _	= text
							action				#str = printToString action
												| startsWith "Action" str	= subString 6 ((textSize str)-1) str
												| otherwise					= str
		actionIcon =	case action of
							ActionIcon _ icon	= icon
							ActionOk			= "icon-ok"
							ActionCancel		= "icon-cancel"
							ActionYes			= "icon-yes"
							ActionNo			= "icon-no"
							ActionNext			= "icon-next"
							ActionPrevious		= "icon-previous"
							ActionFinish		= "icon-finish"
							_					= ""

//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
makeButtons :: !String ![Action] ![Action] !Bool -> [(!Action,!String,!String,!String,!Bool)]	
makeButtons editorId aAlways aValid valid
	= [(b,editorId +++ "-action-" +++ toString i, "action",toString i, True) \\ b <- aAlways & i <- [0..] ]
	  ++
	  [(b,editorId +++ "-action-" +++ toString i, "action",toString i, valid) \\ b <- aValid & i <- [(length aAlways)..] ] //Continue counting

//Generate the TUIUpdates for the buttons that are active when valid
enables :: !String ![Action] ![Action] !Bool -> [TUIUpdate]	
enables editorId aAlways aValid valid
	= [TUISetEnabled (editorId +++ "-action-" +++ toString i) valid \\ b <- aValid & i <- [(length aAlways)..]]

getAction :: [(String, String)] [Action] [Action] !*TSt -> (!Maybe Action, !*TSt)
getAction updates aAlways aValid tst
		# index = toInt (http_getValue "action" updates "-1")
		| index <> -1
			= (Just ((aAlways ++ aValid) !! index),tst)
		| otherwise
			= (parseString (http_getValue "menu" updates ""),tst)

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
