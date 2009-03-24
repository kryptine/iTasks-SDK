implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id
import TSt, Engine, Util
import iDataFormlib

(<^>) infixl  1  :: !(Task a) !(e (Task a) -> Task a) 	-> Task a 	| iData a & iData e
(<^>) normaltask exceptiontask = mkSequenceTask "<^>" exceptionTask
where
	exceptionTask tst=:{taskNr,options,hst}
		# storeId			= iTaskId (tl taskNr) "exception"
		# (store,hst) 		= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) id hst
		# (caught,exception)= store.Form.value
		| caught
			= accTaskTSt (exceptiontask exception normaltask) {tst & hst = hst}
		| otherwise
			# (a, tst =:{exceptions,hst})	= accTaskTSt normaltask {tst & hst = hst}
			# (mbEx,otherExceptions)		= findException exceptions
			= case mbEx of
				Just ex
					# hst		= deleteIData (iTaskId (tl taskNr) "") hst
					# (_,hst)	= mkStoreForm (Init,storageFormId options storeId (False,createDefault)) (\_ -> (True,ex)) hst
					# tst		= resetSequence {tst & hst = hst}
					# (a,tst=:{exceptions=newExceptions})
								= accTaskTSt (exceptiontask ex normaltask) {tst & exceptions = [], activated = True}
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