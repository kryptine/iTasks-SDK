implementation module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/
import	StdList, StdArray, StdTuple
import	TSt, Store, Util
import 	ProcessDB

try :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e
try normalTask handlerTask = mkSequenceTask (taskSubject normalTask) (taskDescription normalTask) exceptionTask
where
	exceptionTask tst=:{taskNr,iworld=iworld=:{IWorld|store,world}}
		# key						= iTaskId (tl taskNr) "exception"
		# (mbEx,store,world)	= loadValue key store world
		= case mbEx of
			Just ex	
				= applyTask (handlerTask ex) {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}
			Nothing			
				# (result, tst)	= applyTask normalTask {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}
				= case result of
					//Handle exception if it matches
					TaskException (ex :: e^)
						# tst					= deleteTaskStates taskNr tst 					//Garbage collect
						# tst					= deleteSubProcesses (taskNrToString taskNr) tst
						# tst=:{TSt|iworld=iworld=:{IWorld|store}}
												= tst
						# store					= storeValueAs SFDynamic key ex store			//Store the exception
						# tst					= {TSt| tst & iworld = {IWorld|iworld & store = store}}
						= applyTask (handlerTask ex) (resetSequence tst)						//Run the handler
					//Just pass through the result
					_
						= (result,tst) 
									
throw :: !e -> Task a | iTask a & TC e	
throw e = mkMonitorTask "Throw an exception" "Throw an exception" throw`
where
	throw` tst
		# tst		= setStatus [H1Tag [] [Text "Error, an uncaught exception was thrown"]] tst
		= (TaskException (dynamic e),tst) 