implementation module InteractionTasks

import	StdList, StdTuple, StdMisc, GenBimap
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
requestInformation :: question -> Task a | html question & iTask a
requestInformation question = requestInformationWD question defaultValue

requestInformationWD :: question a -> Task a | html question & iTask a //With default value
requestInformationWD question initial = mkExtJSTask "requestInformationWD" requestInformationWD`
where
	requestInformationWD` tst=:{taskNr}
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


requestInformationAbout	:: question b -> Task a	| html question & iTask a & iTask b
requestInformationAbout question about = requestInformationAboutWD question about defaultValue

requestInformationAboutWD :: question b a -> Task a	| html question & iTask a & iTask b //With default value
requestInformationAboutWD question about initial = mkExtJSTask "requestInformationAbout" requestInformationAboutWD`
where
	requestInformationAboutWD` tst=:{taskNr}
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
	
requestChoice :: question [a] -> Task a | html question & iTask a
requestChoice question [] = abort "requestChoice: cannot choose from empty option list"
requestChoice question options = mkExtJSTask "requestChoice" requestChoice`
where
	requestChoice` tst
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

requestMultipleChoice :: question [a] -> Task [a] | html question & iTask a
requestMultipleChoice question options
	= abort "TODO: requestMultipleChoice"

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
