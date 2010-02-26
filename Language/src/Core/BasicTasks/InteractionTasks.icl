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
makeInformationTask question initial context actions actionStored tst=:{taskNr}
	# taskId			= taskNrToString taskNr
	# editorId			= "tf-" +++ taskNrToString taskNr
	# (ovalue,tst)		= readValue initial tst
	# (omask,tst)		= readMask initial tst
	# buttonActions		= getButtonActions actions
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# (form,valid) 	= visualizeAsEditor editorId omask ovalue
		# tst			= setAccActions (evaluateConditions (getMenuActions actions) valid ovalue) tst
		# buttonActions	= evaluateConditions buttonActions valid ovalue
		# tst			= setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) tst
		= (TaskBusy,tst)
	| otherwise
		# (nvalue,nmask,lmask,tst) = applyUpdates updates ovalue omask [] tst
		# (action,tst) = getAction updates (map fst buttonActions) tst
		| isJust action = (TaskFinished (fromJust action,nvalue),tst)
		| otherwise
			# tst				= setTaskStore "value" nvalue tst
			# tst				= setTaskStore "mask" nmask tst
			# (updates,valid)	= determineEditorUpdates editorId omask nmask lmask ovalue nvalue
			# tst				= setAccActions (evaluateConditions (getMenuActions actions) valid nvalue) tst
			# buttonActions		= evaluateConditions buttonActions valid nvalue
			# tst				= setTUIUpdates (enables editorId buttonActions ++ updates) tst
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
enterChoice question options	= mkInteractiveTask "enterChoice" (ignoreActionA (makeChoiceTask question options -1 Nothing [ButtonAction (ActionOk, IfValid)]))

enterChoiceA :: question ![TaskAction a] [a] -> Task (!Action,!a) | html question & iTask a
enterChoiceA question actions []		= abort "enterChoice: cannot choose from empty option list"
enterChoiceA question actions options	= mkInteractiveTask "enterChoice" (makeChoiceTask question options -1 Nothing actions)

updateChoice :: question [a] Int -> Task a | html question & iTask a 
updateChoice question [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoice question options index = mkInteractiveTask "updateChoice" (ignoreActionA (makeChoiceTask question options index Nothing [ButtonAction (ActionOk, IfValid)]))

updateChoiceA :: question ![TaskAction a] [a] Int -> Task (!Action,!a) | html question & iTask a 
updateChoiceA question actions [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoiceA question actions options index	= mkInteractiveTask "updateChoice" (makeChoiceTask question options index Nothing actions)

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b
enterChoiceAbout question about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAbout question about options = mkInteractiveTask "enterChoiceAbout" (ignoreActionA (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

enterChoiceAboutA :: question ![TaskAction a] b [a] -> Task (!Action,!a) | html question & iTask a & iTask b
enterChoiceAboutA question actions about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAboutA question actions about options	= mkInteractiveTask "enterChoiceAbout" (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)) actions)

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b
updateChoiceAbout question about [] index		= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAbout question about options index  = mkInteractiveTask "updateChoiceAbout" (ignoreActionA (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) [ButtonAction (ActionOk, IfValid)]))

updateChoiceAboutA :: question ![TaskAction a] b [a] Int-> Task (!Action,!a) | html question & iTask a & iTask b
updateChoiceAboutA question actions about [] index			= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAboutA question actions about options index		= mkInteractiveTask "updateChoiceAbout" (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)) actions)

makeChoiceTask :: !question ![a] !Int (Maybe [HtmlTag]) ![TaskAction a] !*TSt -> (!TaskResult (!Action,!a),!*TSt) | html question & iTask a
makeChoiceTask question options initsel context actions tst=:{taskNr}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# valid			= selection >= 0 && selection < length options	//Do we have a valid index
	# buttonActions = getButtonActions actions
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
		# tst = setAccActions (evaluateConditions (getMenuActions actions) valid (if valid (options !! selection) (hd options))) tst
		# buttonActions = evaluateConditions buttonActions valid (if valid (options !! selection) (hd options))
		# tst = setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) tst
		= (TaskBusy, tst)
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
					# tst			= setAccActions (evaluateConditions (getMenuActions actions) valid (if valid (options !! selection) (hd options))) tst
					# buttonActions = evaluateConditions buttonActions valid (if valid (options !! selection) (hd options))
					# tst			= setTUIUpdates (enables editorId buttonActions) tst
					= (TaskBusy, tst)	
				// Fallback case (shouldn't really happen)
				| otherwise
					# tst		= setTUIUpdates [] tst
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
makeMultipleChoiceTask question options initsel context actions tst=:{taskNr}
	# taskId		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskId
	# selectionId	= editorId +++ "-sel"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = initsel ; Just sel = sel
	# buttonActions	= evaluateConditions (getButtonActions actions) True (select selection options)
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
		# tst = setAccActions (evaluateConditions (getMenuActions actions) True (select selection options)) tst
		# tst = setTUIDef (taskPanel taskId (html question) context (Just form) (makeButtons editorId buttonActions)) tst
		= (TaskBusy, tst)
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
				# tst 		= setAccActions (evaluateConditions (getMenuActions actions) True (select selection options)) tst
				# tst		= setTUIUpdates [] tst
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
		# tst = setAccActions (evaluateConditions (getMenuActions actions) True Void) tst
		# buttonActions = evaluateConditions buttonActions True Void
		# tst = setTUIDef (taskPanel taskId (html message) context Nothing (makeButtons editorId buttonActions)) tst
		= (TaskBusy, tst)
	| otherwise
		# (action,tst) = getAction updates (map fst buttonActions) tst
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
			= (parseString (http_getValue "menu" updates ""),tst)
			
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
	isMenuAction _							= False
	getAction (MenuAction a)			= a
	getAction (ButtonAndMenuAction a)	= a
	getAction (MenuParamAction (s,c))	= (ActionParam s "?", c)

evaluateConditions :: ![ActionWithCond a] Bool a -> [(Action, Bool)]
evaluateConditions actions valid value = [(action,evaluateCondition cond valid value) \\ (action,cond) <- actions]
where
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
