implementation module GUITasks

import	StdList, StdTuple, StdMisc, GenBimap
from	StdFunc import id, const
import	TSt
import	CoreCombinators, CommonCombinators, TuningCombinators, PromptingCombinators
import	GUICore, Util, Http	

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
















selectWithButtons :: ![String] -> Task Int
selectWithButtons labels = abort "TODO: selectWithButtons" //mkBasicTask "selectWithButtons" (selectWithButtons` labels)	
/*
where
	selectWithButtons` [] tst		= (0,tst)				
	selectWithButtons` labels tst=:{taskNr,options}									// choose one subtask out of the list
		# taskId						= iTaskId taskNr "ChoSt"
		# (chosen,tst)					= accHStTSt (mkStoreForm  (Init, storageFormId options taskId -1) id) tst
		| chosen.Form.value == -1		// no choice made yet
			# buttonId					= iTaskId taskNr "ChoBut"
			# allButtons				= [[(HtmlButton txt False,\_ -> n)  \\ txt <- labels & n <- [0..]]]
			# (choice,tst)				= accHStTSt (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
			# (chosen,tst)				= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) choice.Form.value ) tst
			| chosen.Form.value == -1
				# tst = setOutput choice.form tst
				# tst = setInputs choice.inputs tst
				= (0,{tst & activated = False})
			| otherwise
				= (chosen.Form.value,{tst & activated = True})
		= (chosen.Form.value,{tst & activated = True})
*/
selectWithPulldown :: ![String] !Int -> Task Int
selectWithPulldown labels initial = abort "TODO: selectWithPulldown" /* mkBasicTask "selectWithPulldown" (selectWithPulldown` labels initial)
where	
	selectWithPulldown` [] _ tst			= (0,tst)
	selectWithPulldown` labels initial tst=:{taskNr,options}
		# taskId							= iTaskId taskNr "ChoStPdm"
		# (chosen,tst)						= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
		| chosen.Form.value == -1			// no choice made yet	
			# pulldownId					= iTaskId taskNr "ChoPdm"
			# buttonId						= iTaskId taskNr "ChoBut"
			# (choice,tst)					= accHStTSt (mkEditForm (Init, pageFormId options pulldownId (mkSelect labels initial))) tst
			# (done,tst)					= accHStTSt (mkEditForm (Init, pageFormId options buttonId mkButton )) tst
			| fromButton done.Form.value
				# chosenId						= fromSelect choice.Form.value
				# (chosen,tst)					= accHStTSt (mkStoreForm (Init,storageFormId options taskId -1) (\_ -> chosenId)) tst
				= (chosen.Form.value,{tst & activated = True})
			| otherwise
				# tst = setOutput (choice.form ++ done.form) tst
				# tst = setInputs (choice.inputs ++ done.inputs) tst
				= (0,{tst & activated = False})
		= (chosen.Form.value,{tst & activated = True})

	mkButton				= HtmlButton "Ok" False
	mkSelect labels cur 	= HtmlSelect [(label,toString i) \\ label <- labels & i <- [0..] ] (toString cur)
	
	fromButton (HtmlButton _ val) = val
	fromSelect (HtmlSelect _ val) = toInt val
*/
selectWithRadiogroup :: ![[HtmlTag]] !Int -> Task Int
selectWithRadiogroup labels initial = abort "TODO: selectWithRadiogroup" /* mkBasicTask "selectWithRadiogroup" (selectWithRadiogroup` labels initial)
where
	selectWithRadiogroup` [] _ tst = (0,tst)
	selectWithRadiogroup` labels initial tst=:{taskNr,options}
		# valueId		= iTaskId taskNr "val"
		# (value,tst)	= accHStTSt (mkStoreForm (Init,storageFormId options valueId -1) id) tst
		| value.Form.value == -1
			# (radio,tst)	= accHStTSt (mkEditForm (Init,pageFormId options (iTaskId taskNr "radiogroup") (initRadio labels initial))) tst
			# (button,tst)	= accHStTSt (mkEditForm (Init,pageFormId options (iTaskId taskNr "button") initButton)) tst
			| toBool button.Form.value
				# (value,tst)	= accHStTSt (mkStoreForm (Init,storageFormId options valueId -1) (\_ -> toInt radio.Form.value)) tst
				= (value.Form.value, {tst & activated = True})
			| otherwise
				# tst = setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] radio.form, DivTag [ClassAttr "it-editor-buttons"] button.form]] tst
				# tst = setInputs (radio.inputs ++ button.inputs) tst
				= (toInt radio.Form.value, {tst & activated = False})
		= (value.Form.value,{tst & activated = True})

	initButton				= HtmlButton "Ok" False
	initRadio labels cur	= HtmlRadiogroup labels cur
*/
selectWithCheckboxes :: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]
selectWithCheckboxes choices = abort "TODO: selectWithCheckBoxes" /* mkBasicTask "selectWithCheckboxes" (selectWithCheckboxes` choices)
where
	selectWithCheckboxes` [] tst		= ([],tst)
	selectWithCheckboxes` choices tst=:{taskNr,options}				// choose one subtask out of the list
		# seltaskId				= iTaskId taskNr "MtpChSel"
		# donetaskId			= iTaskId taskNr "MtpChSt"
		# buttonId				= iTaskId taskNr "MtpChBut"
		# (cboxes,tst)			= accHStTSt (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
		# (fun,nblist)			= cboxes.Form.value
		# nsettings				= fun nblist
		# (cboxes,tst)			= accHStTSt (ListFuncCheckBox (Set , cFormId options seltaskId (setCheckboxes nsettings))) tst
		# (done,tst)			= accHStTSt (mkStoreForm      (Init, storageFormId options donetaskId False) id) tst
		| done.Form.value
			= ([i \\ True <- snd cboxes.Form.value & i <- [0..]],{tst & activated = True})
		# (button,tst)			= accHStTSt (mkEditForm 	  (Init, pageFormId options buttonId mkButton )) tst
		| fromButton button.Form.value
			# (_,tst)			= accHStTSt (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
			= ([i \\ True <- snd cboxes.Form.value & i <- [0..]],{tst & activated = True})
		| otherwise
			# tst = setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] cboxes.form, DivTag [ClassAttr "it-editor-buttons"] button.form]] tst
			# tst = setInputs (cboxes.inputs ++ button.inputs) tst
			= ([],{tst & activated = False})
	
	initCheckboxes  = 
		[(HtmlCheckbox html set,  \b bs _ -> setfun b bs) \\ (html,set,setfun) <- choices ] 

	setCheckboxes boollist = 
		[(HtmlCheckbox html set,  \b bs _ -> setfun b bs) \\ (html,_,setfun) <- choices & i <- [0..] & set <- boollist]

	mkButton						= HtmlButton "Done" False
	fromButton (HtmlButton _ val) 	= val

*/
button :: !String !a -> Task a | iTask a
button s a = abort "TODO: button" ///*selectWithButtons [s] >>| */ return a

ok :: Task Void
ok = button "Ok" Void

yes	:: Task Bool
yes = button "Yes" True

no :: Task Bool
no = button "No" False

