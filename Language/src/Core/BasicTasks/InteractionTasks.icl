implementation module InteractionTasks

import	StdList, StdOrdList, StdTuple, StdBool, StdMisc, GenBimap
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

//Input tasks
enterInformation :: question -> Task a | html question & iTask a
enterInformation question = mkInteractiveTask "enterInformation" (makeInformationTask question Nothing Nothing) 

updateInformation :: question a -> Task a | html question & iTask a
updateInformation question initial = mkInteractiveTask "updateInformation" (makeInformationTask question (Just initial) Nothing) 

enterInformationAbout :: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = mkInteractiveTask "enterInformationAbout" (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about))) 

updateInformationAbout :: question b a -> Task a | html question & iTask a & iTask b 
updateInformationAbout question about initial = mkInteractiveTask "updateInformationAbout" (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)))

makeInformationTask :: question (Maybe a) (Maybe [HtmlTag])  !*TSt -> (!a,!*TSt) | html question & iTask a
makeInformationTask question initial context tst=:{taskNr}
	# editorId		= "tf-" +++ taskNrToString taskNr
	# doneId		= editorId +++ "-done"
	//Read current value
	# (mbtv,tst)	= getTaskValue tst
	# (oldval,tst)	= case mbtv of
						Just v	= (v,tst)
						Nothing = case initial of
							Just v	= (v,tst)
							Nothing
								= accWorldTSt defaultValue tst	
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
	| length updates == 0
		# (form,valid) 	= visualizeAsEditor editorId omask oldval
		# tst			= setTUIDef (taskPanel taskid (html question) context (Just form) [(doneId,"done","done","Ok","icon-ok",valid)]) tst
		= (oldval,{tst & activated = False})
	| otherwise
		# (newval,nmask,lmask,tst) = applyUpdates updates oldval omask [] tst
		# done = (http_getValue "done" updates "") == "done"
		| done
			= (newval,{tst & activated = True})
		| otherwise
			# tst				= setTaskStore "mask" nmask tst
			# (updates,valid)	= determineEditorUpdates editorId omask nmask lmask oldval newval
			# tst				= setTUIUpdates [TUISetEnabled doneId valid:updates] tst
			= (newval, {tst & activated = False})
where
	taskid = taskNrToString taskNr
	applyUpdates [] val mask lmask tst = (val,mask,lmask,tst)
	applyUpdates [(p,v):us] val mask lmask tst=:{TSt|world}
		# (val,mask,lmask,world) = updateValueAndMask p v val mask lmask world
		= applyUpdates us val mask lmask {TSt|tst & world = world}

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
	| length updates == 0
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
		# tst = setTUIDef (taskPanel taskid (html question) context (Just form) [(doneId,"done","done","Ok","icon-ok",True)]) tst
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

requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkInteractiveTask "requestConfirmation" (makeConfirmationTask question Nothing)

requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = mkInteractiveTask "requestConfirmationAbout" (makeConfirmationTask question (Just (visualizeAsHtmlDisplay about)))

makeConfirmationTask :: question (Maybe [HtmlTag]) *TSt -> (Bool,*TSt) | html question
makeConfirmationTask question context tst=:{taskNr}
	//Check for user updates
	# taskid	= taskNrToString taskNr
	# editorid	= "tf-" +++ taskid
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
		# tst = setTUIDef (taskPanel taskid (html question) context Nothing [(editorid +++ "-no","answer-no","no","No","icon-no",True),(editorid +++ "-yes","answer-yes","yes","Yes","icon-yes",True)]) tst
		= (False,{tst & activated = False})
	| otherwise
		= (snd (hd updates) == "yes", tst)

//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkInteractiveTask "showMessage"  (makeMessageTask message Nothing False)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkInteractiveTask "showMessageAbout" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) False)

showStickyMessage :: message -> Task Void | html message
showStickyMessage message = mkInteractiveTask "showStickyMessage" (makeMessageTask message Nothing True)

showStickyMessageAbout :: message a -> Task Void | html message & iTask a
showStickyMessageAbout message about = mkInteractiveTask "showStickyMessageAbout" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) True)

makeMessageTask :: message (Maybe [HtmlTag]) Bool *TSt -> (Void, *TSt) | html message
makeMessageTask message context sticky tst=:{taskNr}
	# taskid	= taskNrToString taskNr
	# editorid	= "tf-" +++ taskid
	# (updates,tst) = getUserUpdates tst
	| length updates == 0 || sticky
		# tst = setTUIDef (taskPanel taskid (html message) context Nothing (if sticky [] [(editorid +++ "-done","done","done","Ok","icon-ok",True)])) tst
		= (Void,{tst & activated = False})
	| otherwise
		= (Void, tst)


taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(String,String,String,String,String,Bool)] -> TUIDef
taskPanel taskid description mbContext mbForm buttons
	= TUIPanel {TUIPanel| layout = "", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Just (taskButtons buttons), bodyCssClass = "basic-task", fieldLabel = Nothing, renderingHint = 0, unstyled=False}
where
	items = [taskDescriptionPanel ("description-"+++taskid) description] ++
			(case mbContext of Just context = [taskContextPanel ("context-"+++taskid) context]; Nothing = []) ++
			(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			
	taskDescriptionPanel :: !String ![HtmlTag] -> TUIDef
	//taskDescriptionPanel description = TUIHtmlPanel {TUIHtmlPanel| html = toString (SpanTag [] description), border = False, bodyCssClass = "task-description"} 
	taskDescriptionPanel panelid description = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] description), border = False, bodyCssClass = "task-description", fieldLabel = Nothing, hideLabel = True} 
	
	taskContextPanel :: !String ![HtmlTag] -> TUIDef
	//taskContextPanel context = TUIHtmlPanel {TUIHtmlPanel| html = toString (SpanTag [] (html context)), border = False, bodyCssClass = "task-context"} 
	taskContextPanel panelid context = TUIHtmlPanel {TUIHtmlPanel| id = panelid, html = toString (DivTag [] (html context)), border = False, bodyCssClass = "task-context", fieldLabel = Nothing, hideLabel = True} 
	
	taskFormPanel :: [TUIDef] -> TUIDef
	taskFormPanel items = TUIPanel {TUIPanel| layout = "form", autoHeight = True, autoWidth = True, border = False, items = items, buttons = Nothing, bodyCssClass = "task-form", fieldLabel = Nothing, renderingHint = 0, unstyled=False}
	
	taskButtons	:: [(String,String,String,String,String,Bool)] -> [TUIDef]
	taskButtons buttons = [TUIButton {TUIButton| name = name, id = id, value = value, disabled = not enabled, text = text, iconCls = icon} \\ (id,name,value,text,icon,enabled) <- buttons]

notifyUser :: message UserId -> Task Void | html message
notifyUser message uid = mkInstantTask "notifyUser" (\tst -> (Void,tst))

notifyGroup :: message Role -> Task Void | html message
notifyGroup message role = mkInstantTask "notifyGroup" (\tst -> (Void,tst))
