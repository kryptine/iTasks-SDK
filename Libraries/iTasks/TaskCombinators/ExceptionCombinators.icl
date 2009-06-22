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

// TODO: change demands still have to be sorted to prevent that some changes are skipped MJP !!!!!!!!!!!!!

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
				| changeResult.isApplicable 	= ( if changeResult.applyChange (Just ch) Nothing	// determine if alternative task has to be taken here
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

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
try normalTask handlerTask = mkSequenceTask "try" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst}
		# storeId		= iTaskId (tl taskNr) "exception"
		# (store,hst) 	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) id hst
		# (caught,e)	= store.Form.value
		| caught
			= accTaskTSt (handlerTask e) {tst & hst = hst}
		| otherwise
			# (a, tst =:{exception,hst})	= accTaskTSt normalTask {tst & hst = hst}
			= case exception of
				Just (ex :: e^)
					# hst		= deleteIData (iTaskId (tl taskNr) "") hst 														//Garbage collect
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) (\_ -> (True,ex)) hst 	//Store the exception
					= accTaskTSt (handlerTask ex) (resetSequence {tst & exception = Nothing, activated = True, hst = hst})		//Run the handler
					
				_	= (a, {tst & hst = hst})	//Don't handle the exception
						
throw :: !e -> Task a | iData a & TC e	
throw e = mkBasicTask "throw" throw`
where
	throw` tst
		# tst	= setOutput [H1Tag [] [Text "Error, an uncaught exception was thrown"]] tst
		= (createDefault, {tst & exception = Just (dynamic e), activated = False})
