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
enterInformation question = mkExtJSTask "enterInformation" (makeInformationTask question Nothing Nothing) 

updateInformation :: question a -> Task a | html question & iTask a
updateInformation question initial = mkExtJSTask "updateInformation" (makeInformationTask question (Just initial) Nothing) 

enterInformationAbout :: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = mkExtJSTask "enterInformationAbout" (makeInformationTask question Nothing (Just (visualizeAsHtmlDisplay about))) 

updateInformationAbout :: question b a -> Task a | html question & iTask a & iTask b 
updateInformationAbout question about initial = mkExtJSTask "updateInformationAbout" (makeInformationTask question (Just initial) (Just (visualizeAsHtmlDisplay about)))

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
							Just v	= accWorldTSt (defaultMask initial) tst
							Nothing	= ([],tst)
	//Check for user updates
	# (updates,tst) = getUserUpdates tst	
	| length updates == 0
		# (form,valid) 	= visualizeAsEditor editorId omask oldval
		# tst			= setExtJSDef (taskPanel (html question) context (Just form) [(doneId,"done","done","Ok","icon-ok",valid)]) tst
		= (oldval,{tst & activated = False})
	| otherwise
		# (newval,nmask,tst) = applyUpdates updates oldval omask tst
		# done = (http_getValue "done" updates "") == "done"
		| done
			= (newval,{tst & activated = True})
		| otherwise
			# tst				= setTaskStore "mask" nmask tst
			# (updates,valid)	= determineEditorUpdates editorId omask nmask oldval newval
			# tst				= setExtJSUpdates [ExtJSSetEnabled doneId valid:updates] tst
			= (newval, {tst & activated = False})
where
	applyUpdates [] val mask tst = (val,mask,tst)
	applyUpdates [(p,v):us] val mask tst=:{TSt|world}
		# (val,mask,world) = updateValueAndMask p v val mask world
		= applyUpdates us val mask {TSt|tst & world = world}

enterChoice :: question [a] -> Task a | html question & iTask a
enterChoice question []			= abort "enterChoice: cannot choose from empty option list"
enterChoice question options	= mkExtJSTask "enterChoice" (makeChoiceTask question options -1 Nothing)

updateChoice :: question [a] Int -> Task a | html question & iTask a 
updateChoice question [] index		= abort "updateChoice: cannot choose from empty option list"
updateChoice question options index = mkExtJSTask "updateChoice" (makeChoiceTask question options index Nothing)

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b
enterChoiceAbout question about []		= abort "enterChoiceAbout: cannot choose from empty option list"
enterChoiceAbout question about options = mkExtJSTask "enterChoiceAbout" (makeChoiceTask question options -1 (Just (visualizeAsHtmlDisplay about)))

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b
updateChoiceAbout question about [] index		= abort "updateChoiceAbout: cannot choose from empty option list"
updateChoiceAbout question about options index  = mkExtJSTask "updateChoiceAbout" (makeChoiceTask question options index (Just (visualizeAsHtmlDisplay about)))

makeChoiceTask :: question [a] Int (Maybe [HtmlTag]) !*TSt -> (!a,!*TSt) | html question & iTask a
makeChoiceTask question options index context tst=:{taskNr}
	# editorid	= "tf-" +++ taskNrToString taskNr
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
		# form = [ExtJSButton {ExtJSButton	| name = "button-" +++ toString i
											, id	= editorid +++ "-" +++ toString i
											, value = toString i
											, disabled = False
											, text = visualizeAsTextLabel option +++ if (i == index) " (current)" ""
											, iconCls = ""} \\ option <- options & i <- [0..] ]
		# tst = setExtJSDef (taskPanel (html question) context (Just form) []) tst
		= (hd options, {tst & activated = False})
	| otherwise
		= (options !! (toInt (snd (hd updates))), tst) 

enterMultipleChoice :: question [a] -> Task [a] | html question & iTask a
enterMultipleChoice question options = mkExtJSTask "enterMultipleChoice" (makeMultipleChoiceTask question options [] Nothing)

updateMultipleChoice :: question [a] [Int] -> Task [a] | html question & iTask a
updateMultipleChoice question options indices = mkExtJSTask "updateMultipleChoice" (makeMultipleChoiceTask question options indices Nothing)

enterMultipleChoiceAbout :: question b [a] -> Task [a] | html question & iTask a & iTask b
enterMultipleChoiceAbout question about options = mkExtJSTask "enterMultipleChoiceAbout" (makeMultipleChoiceTask question options [] (Just (visualizeAsHtmlDisplay about)))

updateMultipleChoiceAbout :: question b [a] [Int] -> Task [a] | html question & iTask a & iTask b
updateMultipleChoiceAbout question about options indices = mkExtJSTask "updateMultipleChoiceAbout" (makeMultipleChoiceTask question options indices (Just (visualizeAsHtmlDisplay about)))

makeMultipleChoiceTask :: question [a] [Int] (Maybe [HtmlTag]) !*TSt -> (![a],!*TSt) | html question & iTask a
makeMultipleChoiceTask question options inselection context tst=:{taskNr}
	# editorId		= "tf-" +++ taskNrToString taskNr
	# doneId		= editorId +++ "-done"
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = inselection; Just sel = sel
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
		# checks	= [isMember i selection \\ i <- [0..(length options) - 1]]
		# cboxes	= [ExtJSCheckBox 
					  {ExtJSCheckBox
					  | name = "sel-" +++ toString i
					  , id = editorId +++ "-cb-" +++ toString i
					  , value = toString i
					  , fieldLabel = Nothing
					  , hideLabel = True
					  , boxLabel = Just (visualizeAsTextLabel o)
					  , checked = c} \\ o <- options & i <- [0..] & c <- checks ]
		# form = [ ExtJSCheckBoxGroup {ExtJSCheckBoxGroup |name = "selection", id = editorId +++ "-selection", fieldLabel = Nothing, hideLabel = True, columns = 3, items = cboxes}]
		# tst = setExtJSDef (taskPanel (html question) context (Just form) [(doneId,"done","done","Ok","icon-ok",True)]) tst
		= ([],{tst & activated = False})
	| otherwise
		# done = (http_getValue "done" updates "") == "done"
		| done
			= (select selection options,{tst & activated = True})
		| otherwise
			# mbSel		= parseSelection updates 
			# selection	= case mbSel of Nothing = selection; Just sel = map toInt sel
			# tst		= setTaskStore "selection" (sort selection) tst
			# tst		= setExtJSUpdates [] tst
			= ([],{tst & activated = False})
where
	parseSelection :: [(String,String)] -> Maybe [String]
	parseSelection updates = fromJSON (http_getValue "selection" updates "[]")	

	select :: [Int] [a] -> [a]
	select indices options = [options !! index \\ index <- indices]

requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkExtJSTask "requestConfirmation" (makeConfirmationTask question Nothing)

requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = mkExtJSTask "requestConfirmationAbout" (makeConfirmationTask question (Just (visualizeAsHtmlDisplay about)))

makeConfirmationTask :: question (Maybe [HtmlTag]) *TSt -> (Bool,*TSt) | html question
makeConfirmationTask question context tst=:{taskNr}
	//Check for user updates
	# editorid	= "tf-" +++ taskNrToString taskNr
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
		# tst = setExtJSDef (taskPanel (html question) context Nothing [(editorid +++ "-no","answer-no","no","No","icon-no",True),(editorid +++ "-yes","answer-yes","yes","Yes","icon-yes",True)]) tst
		= (False,{tst & activated = False})
	| otherwise
		= (snd (hd updates) == "yes", tst)

//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkExtJSTask "showMessage"  (makeMessageTask message Nothing False)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkExtJSTask "showMessageAbout" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) False)

showStickyMessage :: message -> Task Void | html message
showStickyMessage message = mkExtJSTask "showStickyMessage" (makeMessageTask message Nothing True)

showStickyMessageAbout :: message a -> Task Void | html message & iTask a
showStickyMessageAbout message about = mkExtJSTask "showStickyMessageAbout" (makeMessageTask message (Just (visualizeAsHtmlDisplay about)) True)

makeMessageTask :: message (Maybe [HtmlTag]) Bool *TSt -> (Void, *TSt) | html message
makeMessageTask message context sticky tst=:{taskNr}
	# editorid	= "tf-" +++ taskNrToString taskNr
	# (updates,tst) = getUserUpdates tst
	| length updates == 0 || sticky
		# tst = setExtJSDef (taskPanel (html message) context Nothing (if sticky [] [(editorid +++ "-done","done","done","Ok","icon-ok",True)])) tst
		= (Void,{tst & activated = False})
	| otherwise
		= (Void, tst)


taskPanel :: [HtmlTag] (Maybe [HtmlTag]) (Maybe [ExtJSDef]) [(String,String,String,String,String,Bool)] -> ExtJSDef
taskPanel description mbContext mbForm buttons
	= ExtJSPanel {ExtJSPanel| layout = "", border = False, items = items, buttons = taskButtons buttons, bodyCssClass = "basic-task", fieldLabel = Nothing}
where
	items = [taskDescriptionPanel description] ++
			(case mbContext of Just context = [taskContextPanel context]; Nothing = []) ++
			(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			
	taskDescriptionPanel :: [HtmlTag] -> ExtJSDef
	taskDescriptionPanel description = ExtJSHtmlPanel {ExtJSHtmlPanel| html = toString (SpanTag [] description), border = False, bodyCssClass = "task-description"} 
	
	taskContextPanel :: [HtmlTag] -> ExtJSDef
	taskContextPanel context = ExtJSHtmlPanel {ExtJSHtmlPanel| html = toString (SpanTag [] (html context)), border = False, bodyCssClass = "task-context"} 
	
	taskFormPanel :: [ExtJSDef] -> ExtJSDef
	taskFormPanel items = ExtJSPanel {ExtJSPanel| layout = "form", border = False, items = items, buttons = [], bodyCssClass = "task-form", fieldLabel = Nothing}
	
	taskButtons	:: [(String,String,String,String,String,Bool)] -> [ExtJSDef]
	taskButtons buttons = [ExtJSButton {ExtJSButton| name = name, id = id, value = value, disabled = not enabled, text = text, iconCls = icon} \\ (id,name,value,text,icon,enabled) <- buttons]

notifyUser :: message UserId -> Task Void | html message
notifyUser message uid = mkInstantTask "notifyUser" (\tst -> (Void,tst))

notifyGroup :: message Role -> Task Void | html message
notifyGroup message role = mkInstantTask "notifyGroup" (\tst -> (Void,tst))
