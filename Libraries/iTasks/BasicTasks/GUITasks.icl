implementation module GUITasks

import	StdList, StdTuple, StdMisc, GenBimap
from	StdFunc import id, const
import	TSt
import	CoreCombinators, CommonCombinators, TuningCombinators, PromptingCombinators
import	GUICore, GUIWidgets, Util, Http	

from iTasks import class iTask(..)

class vizHtml a 
where
	vizHtml :: a -> [HtmlTag]
	
instance vizHtml String
where
	vizHtml s = [Text s]
	
instance vizHtml [HtmlTag]
where
	vizHtml h = h

//Input tasks
requestInformation :: question -> Task a | vizHtml question & iTask a
requestInformation question = requestInformationWD question createDefault

requestInformationWD :: question a -> Task a | vizHtml question & iTask a //With default value
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
			# tst = setExtJSDef (panel description (form (visualizeAsEditor editorid oldval))) tst
			= (oldval,{tst & activated = False})
		| otherwise
			# newval = foldr (\(p,v) -> updateValue p v) oldval updates
			# done = (http_getValue "ok" updates "") == "ok"
			| done
				= (newval,{tst & activated = True})
			| otherwise
				# updates = determineEditorUpdates editorid oldval newval
				# tst = setExtJSUpdates updates tst
				= (newval,{tst & activated = False})
			
	
	panel d f	= ExtJSPanel {ExtJSPanel| layout = "", border = False, items = [d,f], buttons = [okbutton]}
	description = ExtJSHtmlPanel {ExtJSHtmlPanel| html = toString (SpanTag [] (vizHtml question)), border = False, bodyCssClass = "task-description"} 
	form items	= ExtJSPanel {ExtJSPanel| layout = "form", border = False, items = items, buttons = []}
	okbutton	= ExtJSButton {ExtJSButton| name = "ok", text = "Ok", value = "ok", iconCls = "icon-ok"}

/*
requestInformationAbout	:: question b -> Task a	| vizHtml question & iData a & iTask b & iData b
requestInformationAbout question about = requestInformationAboutWD question about createDefault

requestInformationAboutWD :: question b a -> Task a	| vizHtml question & iData a & iTask b & iData b //With default value
requestInformationAboutWD question about default
	= requestInformationWD question default -||- (requestInformationWD "" about >>| return question)
*/
requestConfirmation	:: question -> Task Bool | vizHtml question
requestConfirmation question
	= return True//vizHtml question ?>> (yes -||- no)
	
requestChoice :: question [a] -> Task a | vizHtml question & iTask a
requestChoice question options
	= abort "TODO requestChoice" // vizHtml question ?>> selectWithRadiogroup [[toHtml o] \\ o <- options] 0 >>= \i -> return (options !! i)

requestMultipleChoice :: question [a] -> Task [a] | vizHtml question & iTask a
requestMultipleChoice question options
	= abort "TODO: requestMultipleChoice"
	
	/*
	= 				vizHtml question
	?>> 			selectWithCheckboxes [([toHtml o], False, (\_ x -> x) ) \\ o <- options]
	>>= \indexes ->	return [options !! i \\ i <- indexes]
	*/
	
//Output tasks
showMessage	:: message -> Task Void	| vizHtml message
showMessage message = return Void //vizHtml message ?>> button "Ok" Void

showMessageAbout :: message a -> Task Void | vizHtml message & iTask a
showMessageAbout message about = return Void// vizHtml message ?>> (displayValue about -||- button "Ok" Void <<@ TTVertical)

//notifyUser				:: message UserId -> Task Void	| vizHtml message
//notifyGroup				:: message Role -> Task Void	| vizHtml message

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

