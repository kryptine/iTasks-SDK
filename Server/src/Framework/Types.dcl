definition module Types
/**
* This module provides types for all the globally shared concepts within
* the iTasks framework.
*/

from TSt 			import :: TSt
from Html 			import :: HtmlTag
from CommonDomain	import :: Note
from StdString		import class toString

import GenPrint, GenParse, GenVisualize, GenUpdate, JSON

derive gPrint		Session, Document, Hidden, Static
derive gParse		Session, Document, Hidden, Static
derive gVisualize	Session
derive gUpdate		Session

derive JSONEncode Document
derive JSONDecode Document

instance toString TaskPriority

:: UserName			:== String
:: DisplayName		:== String			
:: User 			=
	{ userName		:: !UserName
	, password		:: !String
	, displayName	:: !DisplayName
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


// Field behaviour extensions
:: Static a = Static a						// Variable is always rendered as a HTML-fragment
:: Hidden a = Hidden a						// Variable is never rendered

fromStatic :: !(Static .a) -> .a
toStatic :: !.a -> (Static .a)

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> (Hidden .a)

// Document
:: DocumentData :== String

:: Document = 
	{ fileName 		:: String
	, size	   		:: Int
	, mimeType 		:: String
	, taskId		:: String
	, index			:: Int	
	}

emptyDoc :: Document