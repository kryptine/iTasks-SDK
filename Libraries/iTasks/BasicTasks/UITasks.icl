implementation module UITasks

import StdList, StdTuple, GenBimap
from StdFunc import id
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import TSt
import BasicCombinators, CommonCombinators, TuningCombinators, PromptingCombinators
import Util

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkBasicTask "editTask" (editTask` prompt a)
where
	editTask` prompt a tst=:{taskNr,hst}
	# taskId			= iTaskId taskNr "EdFin"
	# editId			= iTaskId taskNr "EdVal"
	# buttonId			= iTaskId taskNr "EdBut"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) id hst  		// determine if the task has been done previously
	| taskdone.Form.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.TSt.options editId a <@ Display) hst)			// yes, read out current value, make editor passive
		= (editor.Form.value,{tst & activated = True, hst = hst})									// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.TSt.options editId a) hst						// no, read out current value from active editor
	# (finbut,hst)  	= mySimpleButton tst.TSt.options buttonId prompt (\_ -> True) hst				// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) finbut.Form.value hst 	// remember task status for next time
	| taskdone.Form.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
	# tst				= {tst & hst = hst}
	# tst				= setOutput [DivTag [ClassAttr "it-editor"] ((if (isEmpty editor.form) [] [DivTag [ClassAttr "it-editor-content"] editor.form]) ++ [DivTag [ClassAttr "it-editor-buttons"] finbut.form])] tst
	# tst				= setInputs (editor.inputs ++ finbut.inputs) tst
	= (editor.Form.value,{tst & activated = taskdone.Form.value})

editTaskPred :: !a !(a -> (Bool, [HtmlTag]))-> (Task a) | iData a 
editTaskPred  a pred = mkBasicTask "editTask" (editTaskPred` a)
where
	editTaskPred` a tst=:{taskNr,hst}
	# taskId			= iTaskId taskNr "EdFin"
	# editId			= iTaskId taskNr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) id hst  	// remember if the task has been done
	| taskdone.Form.value																		// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.TSt.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		# tst			= {tst & hst = hst}
		# tst			= setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] editor.form]] tst
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value,{tst & activated = True})											// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.TSt.options editId a ) hst					// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.Form.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) (\_ -> True) hst 	// remember task status for next time
			= editTaskPred` a {tst & hst = hst}													// task is now completed, handle as previously
		# tst			= {tst & hst = hst}
		# tst			= setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-message"] (snd (pred editor.Form.value)), DivTag [ClassAttr "it-editor-content"] editor.form]] tst
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value,{tst & activated = taskdone.Form.value})
	# tst			= {tst & hst = hst}
	# tst			= setOutput editor.form tst
	# tst			= setInputs editor.inputs tst
	= (editor.Form.value,{tst & activated = taskdone.Form.value})

mySimpleButton :: !Options !String !String !(a -> a) !*HSt -> (Form (a -> a),!*HSt)
mySimpleButton options id label fun hst	
	= FuncBut (Init, (nFormId id (HtmlButton label False,fun)) <@ if (options.tasklife == LSClient) LSClient LSPage) hst

displayHtml	:: ![HtmlTag] -> Task a	| iData a
displayHtml [] = mkBasicTask "displayHtml" (\tst -> (createDefault,{tst & activated = False}))
displayHtml html = mkBasicTask "displayHtml" displayTask`
where
	displayTask` tst
		# tst = setOutput [DivTag [ClassAttr "it-display"] html] tst
		= (createDefault, {tst & activated = False})

displayValue :: !a -> Task b | iData a & iData b 
displayValue a = displayHtml [toHtml a ]

viewTask :: !String !a  -> Task a	| iData a
viewTask prompt a = mkBasicTask "viewTask" (viewTask` prompt a)
where
	viewTask` prompt a tst=:{taskNr,hst}
		# taskId			= iTaskId taskNr "ViewFin"
		# editId			= iTaskId taskNr "ViewVal"
		# buttonId			= iTaskId taskNr "ViewBut"
		# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) id hst  				// determine if the task has been done previously
		| taskdone.Form.value
			= (a,{tst & hst = hst, activated = True})															// test if task has completed
		# (editor,hst) 		= mkEditForm  (Init,cFormId tst.TSt.options editId a <@ Display) hst					// no, read out current value from active editor
		# (finbut,hst)  	= mySimpleButton tst.TSt.options buttonId prompt (\_ -> True) hst						// add button for marking task as done
		# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.TSt.options taskId False) finbut.Form.value hst 	// remember task status for next time
		| taskdone.Form.value
			= (a,{tst & hst = hst, activated = True})															// task is now completed, handle as previously
		# tst				= {tst & hst = hst}
		# tst				= setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] editor.form, DivTag [ClassAttr "it-editor-buttons"] finbut.form]] tst
		# tst				= setInputs (editor.inputs ++ finbut.inputs) tst
		= (editor.Form.value,{tst & activated = taskdone.Form.value})


selectWithButtons :: ![String] -> Task Int
selectWithButtons labels = mkBasicTask "selectWithButtons" (selectWithButtons` labels)	
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

selectWithPulldown :: ![String] !Int -> Task Int
selectWithPulldown labels initial =  mkBasicTask "selectWithPulldown" (selectWithPulldown` labels initial)
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

selectWithRadiogroup :: ![[HtmlTag]] !Int -> Task Int
selectWithRadiogroup labels initial = mkBasicTask "selectWithRadiogroup" (selectWithRadiogroup` labels initial)
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

selectWithCheckboxes :: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]
selectWithCheckboxes choices = mkBasicTask "selectWithCheckboxes" (selectWithCheckboxes` choices)
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


button :: !String !a -> Task a | iData a
button s a = selectWithButtons [s] >>| return a

ok :: Task Void
ok = button "Ok" Void

yes	:: Task Bool
yes = button "Yes" True

no :: Task Bool
no = button "No" False


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
requestInformation :: question -> Task a | vizHtml question & iData a
requestInformation question = requestInformationWD question createDefault

requestInformationWD :: question a -> Task a | vizHtml question & iData a 			//With default value
requestInformationWD question default
	= vizHtml question ?>> editTask "Ok" default

requestInformationAbout	:: question b -> Task a	| vizHtml question & iData a & iData b
requestInformationAbout question about = requestInformationAboutWD question about createDefault

requestInformationAboutWD :: question b a -> Task a	| vizHtml question & iData a & iData b	//With default value
requestInformationAboutWD question about default
	= vizHtml question ?>> (displayValue about -||- editTask "Ok" default <<@ TTVertical)

requestConfirmation	:: question -> Task Bool | vizHtml question
requestConfirmation question
	= vizHtml question ?>> (yes -||- no)
	
requestChoice :: question [a] -> Task a | vizHtml question & iData a
requestChoice question options
	= vizHtml question ?>> selectWithRadiogroup [[toHtml o] \\ o <- options] 0 >>= \i -> return (options !! i)

//requestMultipleChoice		:: question [a] -> Task [a]		| vizHtml question & iData a

//Output tasks
showMessage	:: message -> Task Void	| vizHtml message
showMessage message = vizHtml message ?>> button "Ok" Void

showMessageAbout :: message a -> Task Void | vizHtml message & iData a
showMessageAbout message about = vizHtml message ?>> (displayValue about -||- button "Ok" Void <<@ TTVertical)

//notifyUser				:: message UserId -> Task Void	| vizHtml message
//notifyGroup				:: message Role -> Task Void	| vizHtml message