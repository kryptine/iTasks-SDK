definition module Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

from TSt 			import :: TSt
from TaskTree		import :: TaskProperties, :: GroupedBehaviour(..), :: GroupActionsBehaviour(..)
from Html 			import :: HtmlTag
from Time			import :: Timestamp
from StdString		import class toString
from iTasks			import class iTask
from Config			import :: Config

import GenVisualize, GenUpdate, JSON, StoreTasks

derive gVisualize		EmailAddress, Session
derive gUpdate			EmailAddress, Session 
derive gVerify			EmailAddress, Session

derive gMerge			FormButton, User, Session, VisualizationHint, Note, Password, Date, Time, DateTime

derive JSONEncode		EmailAddress, Currency, FormButton, User, UserDetails, Session, Task, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Note, Password, Date, Time, DateTime
derive JSONDecode		EmailAddress, Currency, FormButton, User, UserDetails, Session, Task, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Note, Password, Date, Time, DateTime
		
instance toString User
instance toString TaskPriority
instance toString TaskStatus

instance toString Note
instance toString Password
instance toString Date
instance toString Time
instance toString DateTime
instance toString Currency

instance fromString Date
instance fromString Time

instance == User
instance == TaskStatus
instance == Document
instance == Note
instance == Password

instance < Time
instance < Date
instance < User
instance < Currency

instance + Time		//Naive fieldwise addition
instance + Date		//Naive fieldwise addition
instance + Currency 

instance - Time		//Naive fieldwise subtraction
instance - Date		//Naive fieldwise subtraction
instance - Currency

instance toInt Currency
instance zero Currency

// Strings with special meanings
:: EmailAddress	= EmailAddress String
:: URL			= URL String
:: PhoneNr		= PhoneNr String
:: Password		= Password !String

// Form buttons
:: FormButton 		= 
	{ label			:: String
	, icon			:: String
	, state			:: ButtonState
	}
	
:: ButtonState		= NotPressed | Pressed

// Money
:: Currency		// Type of currency and amount in cents. ISO4217 currency codes are used
	= EUR Int
	| GBP Int
	| USD Int
	| JPY Int
	
// Users	
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
	, roles			:: !Maybe [Role]
	}

:: UserId			:== String
:: Role				:== String

// Session
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

:: Task a 			= Task !ManagerProperties !GroupedProperties !(Maybe TaskNr) !(*TSt -> *(!TaskResult a,!*TSt))
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
	{ systemProperties	:: SystemProperties
	, managerProperties	:: ManagerProperties
	, workerProperties	:: WorkerProperties
	}

:: SystemProperties =
	{ taskId			:: !TaskId					// Process table identification
	, parent			:: !Maybe TaskId			// The (direct) parent process
	, status			:: !TaskStatus				// Is a maintask active,suspended,finished or excepted
	, manager			:: !User					// Who is managing this task
	, issuedAt			:: !Timestamp				// When was the task created
	, firstEvent		:: !Maybe Timestamp			// When was the first work done on this task
	, latestEvent		:: !Maybe Timestamp			// When was the latest event on this task	
	, latestExtEvent	:: !Maybe Timestamp			// When was the latest event from an external source (e.g. Rpc Daemon)
	, subTaskWorkers	:: ![(ProcessId, User)] 	// Users who have temporary access to the process because they work on a subprocess in an open parallel.
	, deleteWhenDone	:: !Bool					// Delete the process after completion
	}

:: ManagerProperties =
	{ worker			:: !User					// Who has to do the task? 
	, subject			:: !String 					// The subject of the task
	, description		:: !String					// Description of the task (html)
	, context			:: !Maybe String			// Optional context information for doing the task (html)
	, priority			:: !TaskPriority			// What is the current priority of this task?
	, deadline			:: !Maybe Timestamp			// When is the task due?
	, tags				:: ![String]				// A list of tags
	}
	
:: WorkerProperties =
	{ progress			:: !TaskProgress			// Indication of the worker's progress
	}

:: TaskStatus =	Active			// A process is active and can be further evaluated
			 |	Suspended		// A process is (temporarily) suspended and will not be evaluated until it is activated 
			 |	Finished		// A process terminated normally
			 |	Excepted		// A process terminated with an exception
			 |	Deleted			// A process is deleted (never set, but returned when process can not be found)

initManagerProperties :: ManagerProperties
	
:: GroupedProperties =
	{ groupedBehaviour		:: !GroupedBehaviour
	, groupActionsBehaviour	:: !GroupActionsBehaviour
	}

initGroupedProperties :: GroupedProperties
					
:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task


:: Container a c	= Container a & iTask c		// container for context restrictions

:: TaskEvent	:== (!TaskId,!String,!String)	// taskid, name, value			

:: *IWorld		=	{ application	:: !String											// The name of the application	
					, store			:: !Store											// The generic data store
					, config		:: !Config											// The server configuration
					, world			:: !*World											// The outside world
					}

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

// Data types for which the framework provides special visualizations and support
// Plain text notes

:: Note			= Note String

:: Date	=
	{ day	:: Int
	, mon	:: Int
	, year	:: Int
	}
:: Time =
	{ hour	:: Int
	, min	:: Int
	, sec	:: Int
	}

:: DateTime = DateTime Date Time

// Documents
:: Document =
	{ documentId	:: !DocumentId				//A unique identifier of the document
	, name			:: !String					//The filename of a document
	, mime			:: !String					//The mime type of the document
	, size			:: !Int						//The filesize in bytes
	}

:: DocumentId :== String

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
/* 
* Gives the roles of the passed user
*
* @param The user
* @return The roles currently assigned to this user
*/
getRoles			:: !User -> [Role]
/**
* Extracts the subject of a task
*
* @param The task
* @return The task's subject
*/
taskSubject			:: !(Task a)				-> String

/**
* Extracts the description of a task
*/
taskDescription		:: !(Task a)				-> String

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