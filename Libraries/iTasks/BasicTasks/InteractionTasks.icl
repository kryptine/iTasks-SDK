implementation module InteractionTasks

import	StdList, StdOrdList, StdTuple, StdMisc, GenBimap
from	StdFunc import id, const
import	TSt

import	GUICore, Util, Http	
import	CoreCombinators

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
enterInformation question = updateInformation question defaultValue

updateInformation :: question a -> Task a | html question & iTask a //With default value
updateInformation question initial = mkExtJSTask "updateInformation" updateInformation`
where
	updateInformation` tst=:{taskNr}
		# editorid	= "tf-" +++ taskNrToString taskNr
		//Read current value
		# (mbtv,tst) = getTaskValue tst
		# oldval = case mbtv of Nothing = initial; Just v = v;
		//Check for user updates
		# (updates,tst) = getUserUpdates tst	
		//Update GUI
		| length updates == 0
			# form = visualizeAsEditor editorid oldval
			# tst = setExtJSDef (taskPanel (html question) Nothing (Just form) [("done","done","Ok","icon-ok")]) tst
			= (oldval,{tst & activated = False})
		| otherwise
			# newval = foldr (\(p,v) -> updateValue p v) oldval updates
			# done = (http_getValue "done" updates "") == "done"
			| done
				= (newval,{tst & activated = True})
			| otherwise
				# updates	= determineEditorUpdates editorid oldval newval
				# tst		= setExtJSUpdates updates tst
				= (newval, {tst & activated = False})


enterInformationAbout	:: question b -> Task a	| html question & iTask a & iTask b
enterInformationAbout question about = updateInformationAbout question about defaultValue

updateInformationAbout :: question b a -> Task a	| html question & iTask a & iTask b 
updateInformationAbout question about initial = mkExtJSTask "updateInformationAbout" updateInformationAbout`
where
	updateInformationAbout` tst=:{taskNr}
		# editorid	= "tf-" +++ taskNrToString taskNr
		//Read current value
		# (mbtv,tst) = getTaskValue tst
		# oldval = case mbtv of Nothing = initial; Just v = v;
		//Check for user updates
		# (updates,tst) = getUserUpdates tst	
		//Update GUI
		| length updates == 0
			# context = visualizeAsHtmlDisplay about
			# form = visualizeAsEditor editorid oldval
			# tst = setExtJSDef (taskPanel (html question) (Just context) (Just form) [("done","done","Ok","icon-ok")]) tst
			= (oldval,{tst & activated = False})
		| otherwise
			# newval = foldr (\(p,v) -> updateValue p v) oldval updates
			# done = (http_getValue "done" updates "") == "done"
			| done
				= (newval,{tst & activated = True})
			| otherwise
				# updates	= determineEditorUpdates editorid oldval newval
				# tst		= setExtJSUpdates updates tst
				= (newval, {tst & activated = False})


requestConfirmation	:: question -> Task Bool | html question
requestConfirmation question = mkExtJSTask "requestConfirmation" requestConfirmation`
where
	requestConfirmation` tst
		//Check for user updates
		# (updates,tst) = getUserUpdates tst
		| length updates == 0
			# tst = setExtJSDef (taskPanel (html question) Nothing Nothing [("answer-no","no","No","icon-no"),("answer-yes","yes","Yes","icon-yes")]) tst
			= (False,{tst & activated = False})
		| otherwise
			= (snd (hd updates) == "yes", tst)

requestConfirmationAbout :: question a -> Task Bool | html question & iTask a
requestConfirmationAbout question about = requestConfirmation question


enterChoice :: question [a] -> Task a | html question & iTask a
enterChoice question [] = abort "requestChoice: cannot choose from empty option list"
enterChoice question options = mkExtJSTask "enterChoice" enterChoice`
where
	enterChoice` tst
		//Check for user updates
		# (updates,tst) = getUserUpdates tst
		| length updates == 0
			# form = [ExtJSButton {ExtJSButton	| name = "button-" +++ toString i
												, value = toString i
												, text = visualizeAsTextLabel option
												, iconCls = ""} \\ option <- options & i <- [0..] ]
			# tst = setExtJSDef (taskPanel (html question) Nothing (Just form) []) tst
			= (hd options, {tst & activated = False})
		| otherwise
			= (options !! (toInt (snd (hd updates))), tst) 


updateChoice :: question [a] Int -> Task a | html question & iTask a //TODO
updateChoice question options index = abort "TODO: updateChoice"

enterChoiceAbout :: question b [a] -> Task a | html question & iTask a & iTask b //TODO
enterChoiceAbout question about options = abort "TODO: enterChoiceAbout"

updateChoiceAbout :: question b [a] Int -> Task a | html question & iTask a & iTask b //TODO
updateChoiceAbout question about options index = abort "TODO: updateChoiceAbout"

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
	# editorid	= "tf-" +++ taskNrToString taskNr
	# (mbSel,tst)	= getTaskStore "selection" tst
	# selection		= case mbSel of Nothing = inselection; Just sel = sel
	//Check for user updates
	# (updates,tst) = getUserUpdates tst
	| length updates == 0
		# checks	= [isMember i selection \\ i <- [0..(length options) - 1]]
		# cboxes	= [ExtJSCheckBox 
					  {ExtJSCheckBox
					  | name = "sel-" +++ toString i
					  , id = editorid +++ "-cb-" +++ toString i
					  , value = toString i
					  , fieldLabel = Nothing
					  , boxLabel = Just (visualizeAsTextLabel o)
					  , checked = c} \\ o <- options & i <- [0..] & c <- checks ]
		# form = [ ExtJSCheckBoxGroup {ExtJSCheckBoxGroup |name = "selection", id = editorid +++ "-selection", fieldLabel = Nothing, columns = 3, items = cboxes}]
		# tst = setExtJSDef (taskPanel (html question) context (Just form) [("done","done","Ok","icon-ok")]) tst
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


//Output tasks
showMessage	:: message -> Task Void	| html message
showMessage message = mkExtJSTask "showMessage" showMessage`
where
	showMessage` tst
		# (updates,tst) = getUserUpdates tst
		| length updates == 0
			# tst = setExtJSDef (taskPanel (html message) Nothing Nothing [("done","done","Ok","icon-ok")]) tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void, tst)

showMessageAbout :: message a -> Task Void | html message & iTask a
showMessageAbout message about = mkExtJSTask "showMessageAbout" showMessageAbout`
where
	showMessageAbout` tst
		# (updates,tst) = getUserUpdates tst
		| length updates == 0
			# context = visualizeAsHtmlDisplay about
			# tst = setExtJSDef (taskPanel (html message) (Just context) Nothing [("done","done","Ok","icon-ok")]) tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void, tst)

taskPanel :: [HtmlTag] (Maybe [HtmlTag]) (Maybe [ExtJSDef]) [(String,String,String,String)] -> ExtJSDef
taskPanel description mbContext mbForm buttons
	= ExtJSPanel {ExtJSPanel| layout = "", border = False, items = items, buttons = taskButtons buttons, bodyCssClass = "basic-task"}
where
	items = [taskDescriptionPanel description] ++
			(case mbContext of Just context = [taskContextPanel context]; Nothing = []) ++
			(case mbForm of Just form = [taskFormPanel form]; Nothing = [])
			
	taskDescriptionPanel :: [HtmlTag] -> ExtJSDef
	taskDescriptionPanel description = ExtJSHtmlPanel {ExtJSHtmlPanel| html = toString (SpanTag [] description), border = False, bodyCssClass = "task-description"} 
	
	taskContextPanel :: [HtmlTag] -> ExtJSDef
	taskContextPanel context = ExtJSHtmlPanel {ExtJSHtmlPanel| html = toString (SpanTag [] (html context)), border = False, bodyCssClass = "task-context"} 
	
	taskFormPanel :: [ExtJSDef] -> ExtJSDef
	taskFormPanel items = ExtJSPanel {ExtJSPanel| layout = "form", border = False, items = items, buttons = [], bodyCssClass = "task-form"}
	
	taskButtons	:: [(String,String,String,String)] -> [ExtJSDef]
	taskButtons buttons = [ExtJSButton {ExtJSButton| name = name, value = value, text = text, iconCls = icon} \\ (name,value,text,icon) <- buttons]

notifyUser :: message UserId -> Task Void | html message
notifyUser message uid = return Void

notifyGroup :: message Role -> Task Void | html message
notifyGroup message role = return Void
