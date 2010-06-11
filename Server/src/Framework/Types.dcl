definition module Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

from TSt 			import :: TSt
from TaskTree		import :: TaskProperties, :: GroupedBehaviour(..), :: GroupActionsBehaviour(..)
from Html 			import :: HtmlTag
from CommonDomain	import :: Note, :: Password
from Time			import :: Timestamp
from StdString		import class toString
from iTasks			import class iTask
 
import GenPrint, GenParse, GenVisualize, GenUpdate, JSON, StoreTasks

derive gPrint			User, UserDetails, Session, Task, Document, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gParse			User, UserDetails, Session, Task, Document, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gVisualize		User, UserDetails, Session, Task
derive gUpdate			User, UserDetails, Session, Task
derive gError			User, UserDetails, Session, Task, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gHint			User, UserDetails, Session, Task, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint

derive gMerge			User, Session, VisualizationHint
derive gMakeLocalCopy	User, Session, VisualizationHint
derive gMakeSharedCopy	User, Session, VisualizationHint
		
derive JSONEncode Document
derive JSONDecode Document

derive gEq Document

instance toString User
instance toString TaskPriority
instance toString Password

instance == User
instance == Password

instance < User
		
:: User
	= AnyUser						// Any not further specified person
	| RootUser						// The system super user
	| RegisteredUser !UserDetails	// A registered person of whom we know details
	| NamedUser !String				// A person identified by a username
	| SessionUser !SessionId		// A person that is only identified by a session
	
:: UserDetails			=
	{ userName		:: !UserId
	, password		:: !Password
	, displayName	:: !String
	, roles			:: ![Role]
	}

:: UserId			:== String
:: Role				:== String

:: Password			= Password !String

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

:: Task a 			= Task !ManagerProperties !GroupedBehaviour !GroupActionsBehaviour !(Maybe TaskNr) !(*TSt -> *(!TaskResult a,!*TSt))
:: TaskResult a		= TaskBusy
					| TaskFinished !a
					| TaskException !Dynamic

:: TaskThread a		=
	{ originalTask		:: !Task a
	, currentTask		:: !Task a
	}	

:: TaskPriority		= HighPriority				// tasks can have three levels of priority
					| NormalPriority
					| LowPriority

:: TaskProperties =
	{ systemProps		:: SystemProperties
	, managerProps		:: ManagerProperties
	, workerProps		:: WorkerProperties
	}

:: SystemProperties =
	{ processId			:: ProcessId				// Process table identification
	, manager			:: User						// Who is managing this task
	, issuedAt			:: Timestamp				// When was the task created
	, firstEvent		:: Maybe Timestamp			// When was the first work done on this task
	, latestEvent		:: Maybe Timestamp			// When was the latest event on this task	
	, latestExtEvent	:: Maybe Timestamp			// When was the latest event from an external source (e.g. Rpc Daemon)
	, subTaskWorkers	:: [(ProcessId, User)] 		// Users who have temporary access to the process because they work on a subprocess in an open parralel.
	, deleteWhenDone	:: Bool						// Delete the process after completion
	}

:: ManagerProperties =
	{ worker			:: User						// Who has to do the task? 
	, subject			:: String 					// The subject of the task
	, priority			:: TaskPriority				// What is the current priority of this task?
	, deadline			:: Maybe Timestamp			// When is the task due?
	}
					
:: WorkerProperties =
	{ progress		:: TaskProgress		// Indication of the worker's progress
	}

:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task


:: Container a c	= Container a & iTask c		// container for context restrictions
					
// Changes

// A dynamic that contains a change
:: ChangeDyn	:== Dynamic

// A change function which may be used to change tasks at runtime
:: Change a :== (TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe ChangeDyn))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !ChangeLabel

//A label for identifying changes externally
:: ChangeLabel	:== String

//A labeled new change
:: ChangeInjection :== (!ChangeLifeTime,!ChangeDyn)


// Field behaviour extensions
:: VisualizationHint a = VHEditable a
					   | VHHtmlDisplay a
					   | VHHidden a
:: Editable a = Editable a					// Variable is always rendered within a form as editor field
:: HtmlDisplay a = HtmlDisplay a			// Variable is always rendered within a form as a HTML-fragment
:: Hidden a = Hidden a						// Variable is never rendered

fromVisualizationHint :: !(VisualizationHint .a) -> .a
toVisualizationHint :: !.a -> (VisualizationHint .a)

fromEditable :: !(Editable .a) -> .a
toEditable :: !.a -> (Editable .a)

fromHtmlDisplay :: !(HtmlDisplay .a) -> .a
toHtmlDisplay :: !.a -> (HtmlDisplay .a)

fromHidden :: !(Hidden .a) -> .a
toHidden :: !.a -> (Hidden .a)

// Documents
:: Document =	{ type		:: !DocumentType
				, content	:: !DocumentContent
				}
:: DocumentType		= Local | Shared !String
:: DocumentContent	= EmptyDocument | DocumentContent !DocumentInfo
:: DocumentInfo = 
	{ fileName 		:: !String
	, size	   		:: !Int
	, mimeType 		:: !String
	, dataLocation	:: !DocumentDataLocation
	, index			:: !Int	
	}
:: SharedDocumentVersion :== Int
:: DocumentDataLocation = LocalLocation !TaskId | SharedLocation !String !SharedDocumentVersion
:: DocumentData :== String

emptyDoc 	 		:: Document
isEmptyDoc 			:: !Document -> Bool
/*
* Gives the unique username of a user
*
* @param The user
* @return The user's username
*/
userName 			:: !User -> String
/*
* Gives the display name of a user
*
* @param The user
* @return The user's display name
*/
displayName			:: !User -> String
/**
* Extracts the task label of a task
*
* @param The task
* @return The task's label
*/
taskLabel			:: !(Task a)				-> String
/**
* Extracts the initial worker of a task
*
* @param The task
* @param The task's initial worker
*/
taskUser			:: !(Task a)				-> User
/*
* Extracts the initial properties of a task
*
* @param The task
* @return The task's initial properties
*/
taskProperties		:: !(Task a)				-> ManagerProperties