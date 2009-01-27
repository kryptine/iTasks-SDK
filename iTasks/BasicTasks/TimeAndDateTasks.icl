implementation module TimeAndDateTasks

import StdFunc
import iDataFormlib, iDataTrivial
import TSt
import InternaliTasksCommon

// Timer Tasks ending when timed out

waitForTimeTask:: !HtmlTime	-> (Task HtmlTime)
waitForTimeTask time = mkTask "waitForTimeTask" (Task waitForTimeTask`)
where
	waitForTimeTask` tst=:{taskNr,userId,hst}
	# taskId				= iTaskId userId taskNr "Time_"
	# (stime,hst) 			= mkStoreForm (Init,storageFormId tst.options taskId time) id hst  			// remember time
	# ((currtime,_),hst)	= getTimeAndDate hst
	| currtime < stime.Form.value= (stime.Form.value,{tst & activated = False,hst = hst})
	= (currtime - stime.Form.value,{tst & hst = hst})

waitForDateTask:: !HtmlDate	-> (Task HtmlDate)
waitForDateTask date = mkTask "waitForDateTask" (Task waitForDateTask`)
where
	waitForDateTask` tst=:{taskNr,userId,hst}
	# taskId				= iTaskId userId taskNr "Date_"
	# (taskdone,hst) 		= mkStoreForm (Init,storageFormId tst.options taskId (False,date)) id hst  			// remember date
	# ((_,currdate),hst) 	= getTimeAndDate hst
	| currdate < date		= (date,{tst & activated = False, hst = hst})
	= (date,{tst & hst = hst})
