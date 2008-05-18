implementation module iTasksEditors

// *********************************************************************************************************************************
// Basic iTasks Editors
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdTuple, StdFunc
import iTasksHandler, InternaliTasksCommon, iTasksBasicCombinators
import iDataSettings, iDataHandler, iDataTrivial, iDataButtons, iDataFormlib, iDataStylelib

editTaskLabel :: !String !String !a -> (Task a) | iData a 
editTaskLabel tracename prompt task = \tst =:{options} -> mkTask tracename ((editTask` prompt task <<@ (nPage options)) <<@ Edit) tst
where
	nPage options = if (options.tasklife == Client) Client Page 

editTask :: !String !a -> (Task a) | iData a 
editTask prompt a = mkTask "editTask" (editTask` prompt a)

editTask` prompt a tst=:{tasknr,html,hst,userId}
# taskId			= iTaskId userId tasknr "EdFin"
# editId			= iTaskId userId tasknr "EdVal"
# buttonId			= iTaskId userId tasknr "EdBut"
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
| taskdone.value																			// test if task has completed
	# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
	= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a) hst					// no, read out current value from active editor
# (finbut,hst)  	= mySimpleButton tst.options buttonId prompt (\_ -> True) hst			// add button for marking task as done
# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) finbut.value hst 	// remember task status for next time
| taskdone.value	= editTask` prompt a {tst & hst = hst}									// task is now completed, handle as previously
= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ finbut.form), hst = hst})

editTaskPred :: !a !(a -> (Bool, HtmlCode))-> (Task a) | iData a 
editTaskPred  a pred = mkTask "editTask" (editTaskPred` a)
where
	editTaskPred` a tst=:{tasknr,html,hst,userId}
	# taskId			= iTaskId userId tasknr "EdFin"
	# editId			= iTaskId userId tasknr "EdVal"
	# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) id hst  	// remember if the task has been done
	| taskdone.value																			// test if task has completed
		# (editor,hst) 	= (mkEditForm  (Init,cFormId tst.options editId a <@ Display) hst)		// yes, read out current value, make editor passive
		= (editor.value,{tst & activated = True, html = html +|+ BT editor.form, hst = hst})	// return result task
	# (editor,hst) 		= mkEditForm  (Init,cFormId tst.options editId a <@ Submit) hst			// no, read out current value from active editor
	| editor.changed
		| fst (pred editor.value)
			# (taskdone,hst) 	= mkStoreForm (Init,storageFormId tst.options taskId False) (\_ -> True) hst 	// remember task status for next time
			= editTaskPred` a {tst & hst = hst, html = html}									// task is now completed, handle as previously
		= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT (editor.form ++ snd (pred editor.value)), hst = hst})
	= (editor.value,{tst & activated = taskdone.value, html = html +|+ BT editor.form, hst = hst})

mySimpleButton :: !Options !String !String     !(a -> a) 				!*HSt -> (Form (a -> a),!*HSt)
mySimpleButton options id label fun hst	
							= FuncBut (Init, (nFormId id (iTaskButton label,fun)) <@ if (options.tasklife == Client) Client Page) hst

iTaskButton :: String -> Button
iTaskButton label = LButton defpixel label

