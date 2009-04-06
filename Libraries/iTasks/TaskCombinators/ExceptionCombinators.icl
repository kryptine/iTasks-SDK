implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id
import TSt, Engine, Util
import iDataFormlib



(<\/>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
(<\/>) normaltask exceptiontask = mkSequenceTask "<v>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst,changeDemands}
		# storeId			= iTaskId (tl taskNr) "changeDemand"
		# (store,hst) 		= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) id hst
		# (changed,change)	= store.Form.value
		| changed
			= accTaskTSt (exceptiontask change) {tst & hst = hst}
		| otherwise
			= case findChange changeDemands {tst & hst = hst} of
				(Nothing,accu,tst)	= accTaskTSt normaltask tst			 
				(Just change,accu,tst)
					# hst		= deleteIData (iTaskId (tl taskNr) "") tst.hst
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) (\_ -> (True,change)) hst
					# tst		= resetSequence {tst & hst = hst}
					= accTaskTSt (exceptiontask change) {tst & activated = True, changeDemands = [] /*accu*/}
	where	
		findChange [] tst
			= (Nothing,[],tst)
		findChange [pchd=:(RC pred,chd):chds] tst
			# (b,mbNextPred,tst) = pred tst
			| b
				= case chd of
					(ch :: e^)
						= (Just ch,if (isNothing mbNextPred) chds [(fromJust mbNextPred,chd):chds],tst)
					_	= (Nothing,[pchd:chds],tst)
			# (mc,chds,tst) = findChange chds tst
			= (mc,[pchd:chds],tst)
					

raiseChange :: !RaiseCondition !e !(Task a) -> Task a | iData a & TC e	
raiseChange pred e task = mkSequenceTask "change" raise`
where
	raise` tst=:{changeDemands}
		= accTaskTSt task {tst & changeDemands = [(pred,dynamic e):changeDemands]} 


(<^>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e
(<^>) normaltask exceptiontask = mkSequenceTask "<^>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst}
		# storeId			= iTaskId (tl taskNr) "exception"
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
raise e = mkBasicTask "raise" raise`
where
	raise` tst
		# tst	= setOutput [H1Tag [] [Text "Error, an uncaught exception was raised"]] tst
		= (createDefault, {tst & exceptions = [(dynamic e):tst.exceptions], activated = False})