implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import	StdList, StdArray, StdTuple
from	StdFunc import id
import	TSt, Engine, Util
import	iDataFormlib
import	LiftingCombinators

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
