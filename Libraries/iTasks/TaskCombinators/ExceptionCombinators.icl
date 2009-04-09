implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id
import TSt, Engine, Util
import iDataFormlib
import StdDebug

(<\/>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
(<\/>) normaltask alternativeTask = mkSequenceTask "<v>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst,changeRequests}
		# storeId			= iTaskId (tl taskNr) "catchChangeDemand"
		# (store,hst) 		= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) id hst
		# (changed,change)	= store.Form.value
		| changed
			= accTaskTSt (alternativeTask change) {tst & hst = hst}				// demand was catched in the past, call alternative task with pushed information
		| otherwise
			= case findChange changeRequests {tst & hst = hst} of
				(Nothing,accu,tst)	= accTaskTSt normaltask tst					// no change requested, perform normal task			 
				(Just change,accu,tst)								
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) (\_ -> (True,change)) tst.hst // remember that we catched
					= accTaskTSt (alternativeTask change) {tst & activated = True, changeRequests = accu, hst = hst} // call alternative task with updated request predicate
	where	
		findChange [] tst
			= (Nothing,[],tst)
		findChange [pchd=:(taskId,RC pred,chd):chds] tst
			# (b,mbNextPred,tst) 	= pred tst			// test predicate, also returning the updated predicate
			| b 					= case chd of		// predicate holds, but pushed information should type match as well
										(ch :: e^)	= (Just ch, if (isNothing mbNextPred) chds [(taskId,fromJust mbNextPred,chd):chds], tst)
										_			= (Nothing, [pchd:chds], tst)
			# (mc,chds,tst) = findChange chds tst
			= (mc,[pchd:chds],tst)
					

pushChangeRequest :: !ChangeCondition !e !(Task a) -> Task a | iData a & TC e	
pushChangeRequest pred e task = mkSequenceTask "change" raise`
where
	raise` tst=:{taskNr,options,hst,changeRequests = orgRequests}
		# storeId									= iTaskId (tl taskNr) "pushChangeDemand"
		# myTaskId									= taskNrToString (tl taskNr)
		# (store,hst) 								= mkStoreForm (Init,storageFormId options storeId (True,pred)) id hst	// store for change request
		# (b,spred)									= store.Form.value
		# newRequests								= if b [(myTaskId,spred,dynamic e):orgRequests] orgRequests
		# (a,tst=:{changeRequests = newRequests,hst})	= accTaskTSt task {tst & changeRequests = newRequests, hst = hst} 		// push request down the task tree
		# mbDemand									= find myTaskId newRequests
		# new_bspred								= if (isNothing mbDemand) (False,pred) (True,fromJust mbDemand)			// remember updated request for future events
		# (store,hst) 								= mkStoreForm (Init,storageFormId options storeId (True,pred)) (\_ -> new_bspred) hst
		= (a, {tst & changeRequests = orgRequests, hst = hst})																// recover original request list
	where
		find myTaskId []							= trace_n "Good" Nothing
		find myTaskId [(taskId,RC pred,chd):chds]
		| myTaskId == taskId						= trace_n "Bad" (Just (RC pred))
		| otherwise									= find myTaskId chds

(<^>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
(<^>) normaltask exceptiontask = mkSequenceTask "<^>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst}
		# storeId			= iTaskId (tl taskNr) "catchException"
		# (store,hst) 		= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) id hst
		# (caught,exception)= store.Form.value
		| caught
			= accTaskTSt (exceptiontask exception) {tst & hst = hst}
		| otherwise
			# (a, tst =:{exceptions,hst})	= accTaskTSt normaltask {tst & hst = hst}
			# (mbEx,otherExceptions)		= findException exceptions
			= case mbEx of
				Just ex
					# hst		= deleteIData (iTaskId (tl taskNr) "") hst
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) (\_ -> (True,ex)) hst
					# tst		= resetSequence {tst & hst = hst}
					# (a,tst=:{exceptions=newExceptions})
								= accTaskTSt (exceptiontask ex) {tst & exceptions = [], activated = True}
					= (a, {tst & exceptions = newExceptions ++ otherExceptions})
				Nothing
					= (a, {tst & hst = hst})
	
	findException []
		= (Nothing, [])
	findException [ex:exs]
		= case ex of
			(ex :: e^)
				= (Just ex, exs)
			_
				# (mbEx, exs) = findException exs
				= (mbEx, [ex:exs])

raise :: !e -> Task a | iData a & TC e	
raise e = mkBasicTask "raiseException" raise`
where
	raise` tst
		# tst	= setOutput [H1Tag [] [Text "Error, an uncaught exception was raised"]] tst
		= (createDefault, {tst & exceptions = [(dynamic e):tst.exceptions], activated = False})