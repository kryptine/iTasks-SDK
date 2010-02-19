implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import	StdList, StdArray, StdTuple
import	TSt, Store, Util

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e
try normalTask handlerTask = mkSequenceTask "try" exceptionTask
where
	exceptionTask tst=:{taskNr,dataStore,world}
		# key						= iTaskId (tl taskNr) "exception"
		# (mbEx,dataStore,world)	= loadValue key dataStore world
		= case mbEx of
			Just ex	
				= applyTask (handlerTask ex) {TSt|tst & dataStore = dataStore, world = world}
			Nothing			
				# (result, tst)	= applyTask normalTask {TSt|tst & dataStore = dataStore, world = world}
				= case result of
					//Handle exception if it matches
					TaskException (ex :: e^)
						# tst=:{TSt|dataStore}	= deleteTaskStates taskNr tst 						//Garbage collect
						# dataStore				= storeValueAs SFDynamic key ex dataStore			//Store the exception
						= applyTask (handlerTask ex) (resetSequence {tst & dataStore = dataStore})	//Run the handler
					//Just pass through the result
					_
						= (result,tst) 
									
throw :: !e -> Task a | iTask a & TC e	
throw e = mkMonitorTask "throw" throw`
where
	throw` tst
		# tst		= setStatus [H1Tag [] [Text "Error, an uncaught exception was thrown"]] tst
		= (TaskException (dynamic e),tst) 