implementation module InteractionTasks

import	StdList, StdOrdList, StdTuple, StdBool, StdMisc
from	StdFunc import id, const
import	TSt

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

derive gVisualize	Action
derive gUpdate		Action
derive gPrint		Action
derive gParse		Action

derive bimap Maybe, (,)

//Input tasks
enterInformation :: question -> Task a | html question & iTask a
enterInformation question = mkInteractiveTask "enterInformation" (ignoreActionA (makeInformationTask question Nothing Nothing [] [ActionOk] False))

enterInformationA :: question [Action] [Action] -> Task (!Action,!a) | html question & iTask a
enterInformationA question aAlways aValid = mkInteractiveTask "enterInformationA" (makeInformationTask question Nothing Nothing aAlways aValid True)

updateInformation :: question a -> Task a | html question & iTask a
updateInformation question initial = mkInteractiveTask "updateInformation" (ignoreActionA (makeInformationTask question (Just initial) Nothing [] [ActionOk] False)) 

updateInformationA :: question a [Action] [Action] -> Task (!Action,!a) | html question & iTask a
updateInformationA question initial aAlways aValid = mkInteractiveTask "updateInformationA" (makeInformationTask question (Just initial) Nothing aAlways aValid True)

enterInformationAbout :: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = mkInteractiveTask "enterInformationAbout" (ignoreActionA (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] False))

enterInformationAboutA :: question b [Action] [Action] -> Task (!Action,!a) | html question & iTask a & iTask b
enterInformationAboutA question about aAlways aValid = mkInteractiveTask "enterInformationAboutA" (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about)) aAlways aValid True)
	
updateInformationAbout :: question b a -> Task a | html question & iTask a & iTask b 
updateInformationAbout question about initial = mkInteractiveTask "updateInformationAbout" (ignoreActionA (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) [] [ActionOk] False))

updateInformationAboutA	:: question b a [Action] [Action] -> Task (!Action,!a) | html question & iTask a & iTask b
updateInformationAboutA question about initial aAlways aValid = mkInteractiveTask "updateInformationAboutA" (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)) aAlways aValid True)

makeInformationTask :: question (Maybe a) (Maybe [HtmlTag]) [Action] [Action] !Bool !*TSt -> (!(!Action,!a),!*TSt) | html question & iTask a
makeInformationTask question initial context aAlways aValid actionStored tst=:{taskNr}
	# editorId		= "tf-" +++ taskNrToString taskNr
	//Read current value
	# (oldaction,oldval,tst)
					= readTaskValue actionStored initial tst

	# (mbmask,tst)	= getTaskStore "mask" tst
	# (omask,tst)	= case mbmask of
						Just m = (m,tst)
						Nothing = case initial of
							Just v 
								# (mask,tst)	= accWorldTSt (defaultMask v) tst
								# tst			= setTaskStore "mask" mask tst // <- store the initial mask
								= (mask,tst)
							Nothing	= ([],tst) 
	//Check for user updates
	# (updates,tst) = getUserUpdates tst	
	| isEmpty updates
		# (form,valid) 	= visualizeAsEditor editorId omask oldval
		# tst			= setTUIDef (taskPanel taskid (html question) context (Just form) (buttons editorId aAlways aValid valid)) tst
		= ((oldaction,oldval),{tst & activated = False})
	| otherwise
		# (newval,nmask,lmask,tst) = applyUpdates updates oldval omask [] tst
		# index = toInt (http_getValue "action" updates "-1")
		| index <> -1
			= ((selAction index aAlways aValid,newval),{tst & activated = True})
		| otherwise
			# tst				= setTaskStore "mask" nmask tst
			# (updates,valid)	= determineEditorUpdates editorId omask nmask lmask oldval newval
			# tst				= setTUIUpdates (enables editorId aAlways aValid valid ++ updates) tst
			= ((oldaction,newval), {tst & activated = False})
where
	taskid = taskNrToString taskNr
	applyUpdates [] val mask lmask tst = (val,mask,lmask,tst)
	applyUpdates [(p,v):us] val mask lmask tst=:{TSt|world}
		# (val,mask,lmask,world) = updateValueAndMask p v val mask lmask world
		= applyUpdates us val mask lmask {TSt|tst & world = world}
	
	//Because the current value of a task is stored by applyTask
	//and loaded in this function, we need to differentiate between tasks
	//in which this function is used directly (of type (Action,a)) and those in which
	//the ignoreActionA is used to create a task of type a.
	readTaskValue True initial tst	
		# (mbtv,tst)	= getTaskValue tst
		= case mbtv of
			Just (a,v)	= (a,v,tst)
			Nothing = case initial of
				Just i	= (ActionCancel,i,tst)
				Nothing	
					# (v,tst)	= accWorldTSt defaultValue tst
					= (ActionCancel,v,tst)
	readTaskValue False initial tst
		# (mbtv,tst)	= getTaskValue tst
		= case mbtv of
			Just v		= (ActionCancel,v,tst)
			Nothing		= case initial of
				Just i	= (ActionCancel,i,tst)
				Nothing
					# (v,tst)	= accWorldTSt defaultValue tst
					= (ActionCancel,v,tst)
	
enterChoice :: question [a] -> Task a | html question & iTask a
enterChoice question []			= abort "enterChoice: cannot choose from empty option list"
enterChoice question options	= mkInteractiveTask "enterChoice" (makeChoiceTask question options -1 Nothing)

updateChoice :: question [a] Int -> Task a | html question & iTask a 
updateChoice question [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoice question options index = mkInteractiveTask "updateChoice" (makeChoiceTask question options index Nothing)

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b
enterChoiceAbout question about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAbout question about options = mkInteractiveTask "enterChoiceAbout" (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)))

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b
updateChoiceAbout question about [] index		= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAbout question about options index  = mkInteractiveTask "updateChoiceAbout" (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)))

makeChoiceTask :: question [a] Int (Maybe [HtmlTag]) !*TSt -> (!a,!*TSt) | html question & iTask a
makeChoiceTask question options index context tst=:{taskNr}
	# taskid	= taskNrToString taskNr
	# editorid	= "tf-" +++ taskid
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# form = [TUIButton {TUIButton	| name = "button-" +++ toString i
											, id	= editorid +++ "-" +++ toString i
											, value = toString i
											, disabled = False
											, text = visualizeAsTextLabel option +++ if (i == index) " (current)" ""
											, iconCls = ""} \\ option <- options & i <- [0..] ]
		# tst = setTUIDef (taskPanel taskid (html question) context (Just form) []) tst
		= (hd options, {tst & activated = False})
	| otherwise
		= (options !! (toInt (snd (hd updates))), tst) 

enterMultipleChoice :: question [a] -> Task [a] | html question & iTask a
enterMultipleChoice question options = mkInteractiveTask "enterMultipleChoice" (makeMultipleChoiceTask question options [] Nothing)

updateMultipleChoice :: question [a] [Int] -> Task [a] | html question & iTask a
updateMultipleChoice question options indices = mkInteractiveTask "updateMultipleChoice" (makeMultipleChoiceTask question options indices Nothing)

enterMultipleChoiceAbout :: question b [a] -> Task [a] | html question & iTask a & iTask b
enterMultipleChoiceAbout question about options = mkInteractiveTask "enterMultipleChoiceAbout" (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)))

updateMultipleChoiceAbout :: question b [a] [Int] -> Task [a] | html question & iTask a & iTask b
updateMultipleChoiceAbout question about options indices = mkInteractiveTask "updateMultipleChoiceAbout" (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)))

makeMultipleChoiceTask :: question [a] [Int] (Maybe [HtmlTag]) !*TSt -> (![a],!*TSt) | html question & iTask a
makeMultipleChoiceTask question options inselection context tst=:{taskNr}
	# taskid		= taskNrToString taskNr
	# editorId		= "tf-" +++ taskid
	# doneId		= editorId +++ "-done"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = inselection; Just sel = sel
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
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
		# form = [ TUICheckBoxGroup {TUICheckBoxGroup |name = "selection", id = editorId +++ "-selection", fieldLabel = Nothing, hideLabel = True, columns = 3, items = cboxes}]
		# tst = setTUIDef (taskPanel taskid (html question) context (Just form) [(ActionOk, doneId,"done","done",True)]) tst
		= ([],{tst & activated = False})
	| otherwise
		# done = (http_getValue "done" updates "") == "done"
		| done
			= (select selection options,{tst & activated = True})
		| otherwise
			# mbSel		= parseSelection updates 
			# selection	= case mbSel of Nothing = selection; Just sel = map toInt sel
			# tst		= setTaskStore "selection" (sort selection) tst
			# tst		= setTUIUpdates [] tst
			= ([],{tst & activated = False})
where
	parseSelection :: [(String,String)] -> Maybe [String]
	parseSelection updates = fromJSON (http_getValue "selection" updates "[]")	

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]

//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkInteractiveTask "showMessage" (ignoreActionV (makeMessageTask message Nothing [ActionOk]))

showMessageA :: message [Action] -> Task Action | html message
showMessageA message aAlways = mkInteractiveTask "showMessageA" (makeMessageTask message Nothing aAlways)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkInteractiveTask "showMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) [ActionOk]))

showMessageAboutA :: message a [Action] -> Task Action | html message & iTask a
showMessageAboutA message about aAlways = mkInteractiveTask "showMessageAboutA" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) aAlways)
	
showStickyMessage :: message -> Task Void | html message
showStickyMessage message = mkInteractiveTask "showStickyMessage" (ignoreActionV (makeMessageTask message Nothing []))

showStickyMessageAbout :: message a -> Task Void | html message & iTask a
showStickyMessageAbout message about = mkInteractiveTask "showStickyMessageAbout" (ignoreActionV (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) []))

requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkInteractiveTask "requestConfirmation" requestConfirmation`
where
	requestConfirmation` tst 
		# (action,tst) = (makeMessageTask question Nothing [ActionNo,ActionYes]) tst
		= case action of
			ActionYes	= (True,tst)
			_			= (False,tst)
	
requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = mkInteractiveTask "requestConfirmationAbout" requestConfirmationAbout`
where
	requestConfirmationAbout` tst
		# (action,tst) = (makeMessageTask question (Just (visualizeAsHtmlDisplay about)) [ActionNo,ActionYes]) tst
		= case action of
			ActionYes	= (True,tst)
			_			= (False,tst)

makeMessageTask :: message (Maybe [HtmlTag]) [Action] *TSt -> (!Action,!*TSt) | html message
makeMessageTask message context aAlways tst=:{taskNr}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# tst = setTUIDef (taskPanel taskId (html message) context Nothing (buttons editorId aAlways [] True)) tst
		= (ActionCancel,{tst & activated = False})
	| otherwise
		= (selAction (toInt (http_getValue "action" updates "0")) aAlways [],{tst & activated = True})
	
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
	toTUIButton (ActionLabel text)		id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = text, iconCls = ""}
	toTUIButton (ActionIcon text icon)	id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = text, iconCls = icon}
	toTUIButton (ActionOk)				id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Ok", iconCls = "icon-ok"}
	toTUIButton (ActionCancel)			id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Cancel", iconCls = "icon-cancel"}
	toTUIButton (ActionYes)				id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Yes", iconCls = "icon-yes"}
	toTUIButton (ActionNo)				id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "No", iconCls = "icon-no"}
	toTUIButton (ActionNext)			id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Next", iconCls = "icon-next"}
	toTUIButton (ActionPrevious)		id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Previous", iconCls = "icon-previous"}
	toTUIButton (ActionFinish)			id name value enable = {TUIButton| name = name, id = id, value = value, disabled = not enable, text = "Finish", iconCls = "icon-finish"}

//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
buttons :: !String ![Action] ![Action] !Bool -> [(!Action,!String,!String,!String,!Bool)]	
buttons editorId aAlways aValid valid
	= [(b,editorId +++ "-action-" +++ toString i, "action",toString i, True) \\ b <- aAlways & i <- [0..] ]
	  ++
	  [(b,editorId +++ "-action-" +++ toString i, "action",toString i, valid) \\ b <- aValid & i <- [(length aAlways)..] ] //Continue counting

//Generate the TUIUpdates for the buttons that are active when valid
enables :: !String ![Action] ![Action] !Bool -> [TUIUpdate]	
enables editorId aAlways aValid valid
	= [TUISetEnabled (editorId +++ "-action-" +++ toString i) valid \\ b <- aValid & i <- [(length aAlways)..]]

selAction :: !Int ![Action] ![Action] -> Action
selAction index aAlways aValid = (aAlways ++ aValid) !! index

//Throw away the chosen action part of the result
ignoreActionA :: (*TSt -> ((!Action,!a),*TSt)) -> (*TSt -> (!a,!*TSt))
ignoreActionA f = \tst -> let ((_,a),tst`) = f tst in (a,tst`)

ignoreActionV :: (*TSt -> (!Action,!*TSt)) -> (*TSt -> (!Void,!*TSt))
ignoreActionV f = \tst -> let (_,tst`) = f tst in (Void,tst`)

notifyUser :: message UserId -> Task Void | html message
notifyUser message uid = mkInstantTask "notifyUser" (\tst -> (Void,tst))

notifyGroup :: message Role -> Task Void | html message
notifyGroup message role = mkInstantTask "notifyGroup" (\tst -> (Void,tst))
