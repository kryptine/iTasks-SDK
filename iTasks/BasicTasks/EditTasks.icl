implementation module EditTasks

import StdList, StdTuple, StdFunc
import iDataSettings, iDataForms, iDataWidgets, iDataFormlib, iDataTrivial
import TuningCombinators
import InternaliTasksCommon

editTaskLabel :: !String !String !a -> (Task a) | iData a 
editTaskLabel tracename prompt task = Task (\tst =:{options} -> accTaskTSt (mkBasicTask tracename ((Task (editTask` prompt task) <<@ (nPage options)) <<@ Edit)) tst)
where
	nPage options = if (options.tasklife == LSClient) LSClient LSPage 

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkBasicTask "editTask" (Task (editTask` prompt a))

editTask` prompt a tst=:{taskNr,html,hst,userId}
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
	# tst				= setOutput (editor.form ++ finbut.form) tst
	# tst				= setInputs (editor.inputs ++ finbut.inputs) tst
	= (editor.Form.value,{tst & activated = taskdone.Form.value, html = html +|+ BT (editor.form ++ finbut.form) (editor.inputs ++ finbut.inputs)})

editTaskPred :: !a !(a -> (Bool, [HtmlTag]))-> (Task a) | iData a 
editTaskPred  a pred = mkBasicTask "editTask" (Task (editTaskPred` a))
where
	editTaskPred` a tst=:{taskNr,html,hst,userId}
	# taskId			= iTaskId userId taskNr "EdFin"
	# editId			= iTaskId userId taskNr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
	| taskdone.Form.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		# tst			= {tst & hst = hst}
		# tst			= setOutput editor.form tst
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value,{tst & activated = True, html = html +|+ BT editor.form editor.inputs})	// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a ) hst					// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.Form.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) (\_ -> True) hst 	// remember task status for next time
			= editTaskPred` a {tst & hst = hst, html = html}									// task is now completed, handle as previously
		# tst			= {tst & hst = hst}
		# tst			= setOutput (editor.form ++ snd (pred editor.Form.value)) tst
		# tst			= setInputs editor.inputs tst
		= (editor.Form.value,{tst & activated = taskdone.Form.value, html = html +|+ BT (editor.form ++ snd (pred editor.Form.value)) editor.inputs})
	# tst			= {tst & hst = hst}
	# tst			= setOutput editor.form tst
	# tst			= setInputs editor.inputs tst
	= (editor.Form.value,{tst & activated = taskdone.Form.value, html = html +|+ BT editor.form editor.inputs})

mySimpleButton :: !Options !String !String     !(a -> a) 				!*HSt -> (Form (a -> a),!*HSt)
mySimpleButton options id label fun hst	
							= FuncBut (Init, (nFormId id (iTaskButton label,fun)) <@ if (options.tasklife == LSClient) LSClient LSPage) hst

iTaskButton :: String -> HtmlButton
iTaskButton label = HtmlButton label False

