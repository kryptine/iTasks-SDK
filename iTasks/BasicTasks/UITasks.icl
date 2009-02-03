implementation module UITasks

import StdList, StdTuple, StdFunc, GenBimap
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import TuningCombinators
import InternaliTasksCommon

derive gForm []
derive gUpd []

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkBasicTask "editTask" (Task (editTask` prompt a))
where
	editTask` prompt a tst=:{taskNr,userId,hst}
	# taskId			= iTaskId userId taskNr "EdFin"
	# editId			= iTaskId userId taskNr "EdVal"
	# buttonId			= iTaskId userId taskNr "EdBut"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  		// determine if the task has been done previously
	| taskdone.Form.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)			// yes, read out current value, make editor passive
		= (editor.Form.value,{tst & activated = True, hst = hst})									// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a) hst						// no, read out current value from active editor
	# (finbut,hst)  	= mySimpleButton tst.options buttonId prompt (\_ -> True) hst				// add button for marking task as done
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) finbut.Form.value hst 	// remember task status for next time
	| taskdone.Form.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
	# tst				= {tst & hst = hst}
	# tst				= setOutput [DivTag [ClassAttr "it-editor"] ((if (isEmpty editor.form) [] [DivTag [ClassAttr "it-editor-content"] editor.form]) ++ [DivTag [ClassAttr "it-editor-buttons"] finbut.form])] tst
	# tst				= setInputs (editor.inputs ++ finbut.inputs) tst
	= (editor.Form.value,{tst & activated = taskdone.Form.value})

editTaskPred :: !a !(a -> (Bool, [HtmlTag]))-> (Task a) | iData a 
editTaskPred  a pred = mkBasicTask "editTask" (Task (editTaskPred` a))
where
	editTaskPred` a tst=:{taskNr,userId,hst}
	# taskId			= iTaskId userId taskNr "EdFin"
	# editId			= iTaskId userId taskNr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
	| taskdone.Form.value																		// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		# tst			= {tst & hst = hst}
		# tst			= setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] editor.form]] tst
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value,{tst & activated = True})											// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a ) hst					// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.Form.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) (\_ -> True) hst 	// remember task status for next time
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

displayHtml	:: ![HtmlTag] -> Task a	| iCreateAndPrint a
displayHtml html = mkBasicTask "displayHtml" (Task displayTask`)
where
	displayTask` tst
		# tst = setOutput [DivTag [ClassAttr "it-display"] html] tst
		= (createDefault, {tst & activated = False})

displayValue :: !a -> Task b | iData a & iCreateAndPrint b 
displayValue a = displayHtml [toHtml a ]

viewTask :: !String !a  -> Task a	| iData a
viewTask prompt a = mkBasicTask "viewTask" (Task (viewTask` prompt a))
where
	viewTask` prompt a tst=:{taskNr,userId,hst}
		# taskId			= iTaskId userId taskNr "ViewFin"
		# editId			= iTaskId userId taskNr "ViewVal"
		# buttonId			= iTaskId userId taskNr "ViewBut"
		# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  				// determine if the task has been done previously
		| taskdone.Form.value
			= (a,{tst & hst = hst, activated = True})															// test if task has completed
		# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst					// no, read out current value from active editor
		# (finbut,hst)  	= mySimpleButton tst.options buttonId prompt (\_ -> True) hst						// add button for marking task as done
		# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) finbut.Form.value hst 	// remember task status for next time
		| taskdone.Form.value
			= (a,{tst & hst = hst, activated = True})															// task is now completed, handle as previously
		# tst				= {tst & hst = hst}
		# tst				= setOutput [DivTag [ClassAttr "it-editor"] [DivTag [ClassAttr "it-editor-content"] editor.form, DivTag [ClassAttr "it-editor-buttons"] finbut.form]] tst
		# tst				= setInputs (editor.inputs ++ finbut.inputs) tst
		= (editor.Form.value,{tst & activated = taskdone.Form.value})


selectWithButtons :: ![String] !Bool -> Task Int
selectWithButtons labels horizontal = mkBasicTask "selectWithButtons" (Task (selectWithButtons` labels horizontal))	
where
	selectWithButtons` [] _ tst		= (0,tst)				
	selectWithButtons` labels horizontal tst=:{taskNr,userId,options}									// choose one subtask out of the list
		# taskId						= iTaskId userId taskNr "ChoSt"
		# (chosen,tst)					= accHStTSt (mkStoreForm  (Init, storageFormId options taskId -1) id) tst
		| chosen.Form.value == -1		// no choice made yet
			# buttonId					= iTaskId userId taskNr "ChoBut"
			# allButtons				= if horizontal 
												[[(HtmlButton txt False,\_ -> n)  \\ txt <- labels & n <- [0..]]]
												[[(HtmlButton txt False,\_ -> n)] \\ txt <- labels & n <- [0..]]
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
selectWithPulldown labels initial =  mkBasicTask "selectWithPulldown" (Task (selectWithPulldown` labels initial))
where	
	selectWithPulldown` [] _ tst			= (0,tst)
	selectWithPulldown` labels initial tst=:{taskNr,userId,options}
		# taskId							= iTaskId userId taskNr ("ChoStPdm")
		# (chosen,tst)						= accHStTSt (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
		| chosen.Form.value == -1			// no choice made yet	
			# pulldownId					= iTaskId userId taskNr "ChoPdm"
			# buttonId						= iTaskId userId taskNr "ChoBut"
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

selectWithCheckboxes :: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]
selectWithCheckboxes choices = mkBasicTask "selectWithCheckboxes" (Task (selectWithCheckboxes` choices))
where
	selectWithCheckboxes` [] tst		= ([],tst)
	selectWithCheckboxes` choices tst=:{taskNr,userId,options}									// choose one subtask out of the list
		# seltaskId				= iTaskId userId taskNr "MtpChSel"
		# donetaskId			= iTaskId userId taskNr "MtpChSt"
		# buttonId				= iTaskId userId taskNr "MtpChBut"
		# (cboxes,tst)			= accHStTSt (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
		# (fun,nblist)			= cboxes.Form.value
		# nsettings				= fun nblist
		# (cboxes,tst)			= accHStTSt (ListFuncCheckBox (Set , cFormId options seltaskId (setCheckboxes nsettings))) tst
		# (done,tst)			= accHStTSt (mkStoreForm      (Init, storageFormId options donetaskId False) id) tst
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
