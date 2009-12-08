definition module Types
/**
* This module provides types for all the globally shared concepts within
* the iTasks framework.
*/
from TSt 			import :: TSt
from Html 			import :: HtmlTag
from CommonDomain	import :: Note
from StdString		import class toString

import GenPrint, GenParse, GenVisualize, GenUpdate

:: UserId			:== Int					// A user id of an iTask user must be a unique integer value
:: User 			=
	{ userId		:: !Int
	, userName		:: !String
	, password		:: !String
	, displayName	:: !String
	, roles			:: ![Role]
	}

:: Role				:== String				// A role is identified by a string

:: SessionId		:== String
:: Session			=
	{ sessionId	::	!String
	, user		::	!User
	, timestamp	::	!Int
	}

:: ProcessId		:== String
:: ProcessRef a		= ProcessRef !ProcessId


:: DynamicId		:== Int
:: TaskId			:== String	
:: TaskNr			:== [Int]				// task nr i.j is administrated as [j,i]


// The task monad
:: Task a 			= Task !TaskDescription !(Maybe TaskNr) !(*TSt -> *(!a,!*TSt))
:: TaskDescription	=
	{ title			:: !String
	, description	:: !Note
	}
	
:: TaskPriority		= HighPriority			// tasks can have three levels of priority
					| NormalPriority
					| LowPriority
					
:: EvaluationOption	= OnClient 				// Evaluate a task on the client whenever possible
					| OnServer				// Always evaluate a task on the server
					
derive gPrint		Session
derive gParse		Session
derive gVisualize	Session
derive gUpdate		Session			

instance toString TaskPriority