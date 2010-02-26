definition module Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

from TSt 			import :: TSt
from TaskTree		import :: TaskProperties
from Html 			import :: HtmlTag
from CommonDomain	import :: Note
from StdString		import class toString

from iTasks			import class iTask
 
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
:: Role				:== String

:: User 			=
	{ userName		:: !UserName
	, password		:: !String
	, displayName	:: !DisplayName
	, roles			:: ![Role]
	}

:: SessionId		:== String
:: Session			=
	{ sessionId	::	!String
	, user		::	!User
	, timestamp	::	!Int
	}

:: ProcessId		:== String
:: ProcessRef a		= ProcessRef !ProcessId

// Tasks
:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]
:: TaskId			:== String		// String serialization of TaskNr values
:: MenuId			:== Int

:: Task a 			= Task !TaskDescription !(Maybe TaskNr) !(*TSt -> *(!TaskResult a,!*TSt))
:: TaskResult a		= TaskBusy
					| TaskFinished !a
					| TaskException !Dynamic

:: TaskDescription	=
	{ title			:: !String
	, description	:: !Note
	}

:: TaskThread a		=
	{ originalTask	:: !Task a
	, currentTask	:: !Task a
	}	

:: TaskPriority		= HighPriority				// tasks can have three levels of priority
					| NormalPriority
					| LowPriority


:: Container a c	= Container a & iTask c		// container for context restrictions
					
// Changes

// A change function which may be used to change tasks at runtime
:: Change a :== (TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !ChangeLabel

//A label for identifying changes externally
:: ChangeLabel	:== String
//A labeled new change
:: ChangeInjection :== (!ChangeLifeTime,!Dynamic)

// Field behaviour extensions
:: Static a = Static a						// Variable is always rendered as a HTML-fragment
:: Hidden a = Hidden a						// Variable is never rendered

fromStatic :: !(Static .a) -> .a
toStatic :: !.a -> (Static .a)

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> (Hidden .a)

// Documents
:: DocumentData :== String

:: Document = 
	{ fileName 		:: String
	, size	   		:: Int
	, mimeType 		:: String
	, taskId		:: String
	, index			:: Int	
	}

emptyDoc :: Document