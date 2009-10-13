implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import	StdList, StdArray, StdTuple
import	TSt, Store, Util

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e
try normalTask handlerTask = mkSequenceTask "try" exceptionTask
where
	exceptionTask tst=:{taskNr,options,dataStore,world}
		# key				= iTaskId (tl taskNr) "exception"
		# (mbEx,dstore,world)= loadValue key dataStore world
		= case mbEx of
			Just ex
				= applyTask (handlerTask ex) {TSt|tst & dataStore = dstore, world = world}
			Nothing				
				# (a, tst =:{exception})	= applyTask normalTask {TSt|tst & dataStore = dstore, world = world}
				= case exception of
					Just (ex :: e^)
						# tst=:{TSt|dataStore}	= deleteTaskStates (tl taskNr) tst 														//Garbage collect
						# dstore				= storeValueAs SFDynamic key ex dataStore													//Store the exception
						= applyTask (handlerTask ex) (resetSequence {tst & exception = Nothing, activated = True, dataStore = dstore})	//Run the handler
						
					_	= (a, tst)	//Don't handle the exception
						
throw :: !e -> Task a | iTask a & TC e	
throw e = mkMonitorTask "throw" throw`
where
	throw` tst
		# tst	= setStatus [H1Tag [] [Text "Error, an uncaught exception was thrown"]] tst
		= accWorldTSt defaultValue {tst & exception = Just (dynamic e), activated = False}
