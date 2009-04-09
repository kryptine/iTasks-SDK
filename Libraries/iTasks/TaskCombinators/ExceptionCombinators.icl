implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id
import TSt, Engine, Util
import iDataFormlib
import StdDebug
import LiftingCombinators

import GenBimap
derive gUpd 	Time
derive gForm 	Time
derive gPrint	Time
derive gParse	Time


(<\/>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
(<\/>) normaltask alternativeTask = mkSequenceTask "<v>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst,changeRequests}
		# storeId						= iTaskId (tl taskNr) "catchChangeRequest"
		# (store,hst) 					= mkStoreForm (Init,storageFormId options storeId (False,0,createDefault)) id hst
		# (changed,timestamp,change)	= store.Form.value
		| changed
			= accTaskTSt (alternativeTask change) {tst & hst = hst}				// demand was catched in the past, call alternative task with pushed information
		| otherwise
			= case findChange changeRequests timestamp {tst & hst = hst} of
				(Nothing,timestamp,accu,tst)	
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,0,createDefault)) (\_ -> (False,timestamp,change)) tst.hst // remember timestamp
					= accTaskTSt normaltask {tst & changeRequests = accu, hst = hst}	// no change requested, perform normal task			 
				(Just change,timestamp,accu,tst)								
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,0,createDefault)) (\_ -> (True,timestamp,change)) tst.hst // remember that we catched
					= accTaskTSt (alternativeTask change) {tst & activated = True, changeRequests = accu, hst = hst} // call alternative task with updated request predicate
	where	
		findChange [] timestamp tst
			= (Nothing,timestamp,[],tst)
		findChange [pchd=:(taskId,CC pred,timeStampChange,chd =:(ch :: e^)):chds] timestamp tst		// pushed dynamic should match
			| timeStampChange > timestamp
				# (changeResult,tst) 		= pred tst											// predicate should match as well
				| changeResult.changePred 	= ( if changeResult.makeChange (Just ch) Nothing	// determine if alternative task has to be taken here
											  , timeStampChange									// remember new timestamp
											  , if (isNothing changeResult.newCondition)		// update predicate if there is one, kick it out otherwise 
													chds 
													[(taskId,fromJust changeResult.newCondition,timeStampChange,chd):chds]
											  , tst)
				# (mc,timestamp,chds,tst) = findChange chds timestamp tst
				= (mc,timestamp,[pchd:chds],tst)
			# (mc,timestamp,chds,tst) = findChange chds timestamp tst
			= (mc,timestamp,[pchd:chds],tst)
					

pushChangeRequest :: !ChangeCondition !e !(Task a) -> Task a | iData a & TC e	
pushChangeRequest pred e task = mkSequenceTask "pushChangeRequest" raise`
where
	raise` tst=:{taskNr,options,hst,changeRequests = orgRequests}
		# storeId									= iTaskId (tl taskNr) "pushChangeRequest"
		# myTaskId									= taskNrToString (tl taskNr)
		# (Time curTime,tst)						= accTaskTSt (appWorld "getTimeForPush" time) tst
		# (store,hst) 								= mkStoreForm (Init,storageFormId options storeId (True,curTime,pred)) id tst.hst			// store for change request
		# (notFinished,curTime,spred)				= store.Form.value
		# newRequests								= if notFinished [(myTaskId,spred,curTime,dynamic e):orgRequests] orgRequests			// determine if previous request is still active
		# (a,tst=:{changeRequests = newRequests,hst})	
													= accTaskTSt task {tst & changeRequests = newRequests, hst = hst} 						// push request down the task tree
		# mbDemand									= find myTaskId newRequests
		# new_bspred								= if (isNothing mbDemand) (False,curTime,pred) (True,curTime,fromJust mbDemand)			// remember updated request for future events
		# (store,hst) 								= mkStoreForm (Init,storageFormId options storeId (True,curTime,pred)) (\_ -> new_bspred) hst
		= (a, {tst & changeRequests = orgRequests, hst = hst})																				// recover original request list
	where
		find myTaskId []							= Nothing
		find myTaskId [(taskId,CC pred,time,chd):chds]
		| myTaskId == taskId						= Just (CC pred)
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