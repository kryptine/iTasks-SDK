implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import	StdList, StdArray, StdTuple
import	TSt, Store, Util

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e
try normalTask handlerTask = mkSequenceTask "try" exceptionTask
where
	exceptionTask tst=:{taskNr,options,store,world}
		# key				= iTaskId (tl taskNr) "exception"
		# (mbEx,store,world)= loadValue key store world
		= case mbEx of
			Just ex
				= applyTask (handlerTask ex) {tst & store = store, world = world}
			Nothing				
				# (a, tst =:{exception})	= applyTask normalTask {tst & store = store, world = world}
				= case exception of
					Just (ex :: e^)
						# tst=:{TSt|store}	= deleteTaskStates (tl taskNr) tst 														//Garbage collect
						# store				= storeValueAs SFDynamic key ex store													//Store the exception
						= applyTask (handlerTask ex) (resetSequence {tst & exception = Nothing, activated = True, store = store})	//Run the handler
						
					_	= (a, tst)	//Don't handle the exception
						
throw :: !e -> Task a | iTask a & TC e	
throw e = mkMonitorTask "throw" throw`
where
	throw` tst
		# tst	= setStatus [H1Tag [] [Text "Error, an uncaught exception was thrown"]] tst
		= (defaultValue, {tst & exception = Just (dynamic e), activated = False})
