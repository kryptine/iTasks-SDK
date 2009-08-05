implementation module TimeAndDateTasks

import StdFunc, StdMisc
import GUIWidgets
import TSt, Util

// Timer Tasks ending when timed out

waitForTimeTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = abort "TODO: waitForTimeTask" /*mkBasicTask "waitForTimeTask" waitForTimeTask`
where
	waitForTimeTask` tst=:{taskNr,hst}
	# taskId				= iTaskId taskNr "Time_"
	# (stime,hst) 			= mkStoreForm (Init,storageFormId tst.TSt.options taskId time) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < stime.Form.value= (stime.Form.value,{tst & activated = False,hst = hst})
	= (currtime - stime.Form.value,{tst & hst = hst})
*/
waitForDateTask:: !HtmlDate	-> (Task HtmlDate)
waitForDateTask date = abort "TODO: waitForDateTask" /* mkBasicTask "waitForDateTask" waitForDateTask`
where
	waitForDateTask` tst=:{taskNr,hst}
	# taskId				= iTaskId taskNr "Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,storageFormId tst.TSt.options taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})
*/