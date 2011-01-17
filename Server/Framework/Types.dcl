definition module Types
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/

from TSt 				import :: TSt
from TaskTree			import :: TaskProperties, :: GroupedBehaviour(..), :: GroupActionsBehaviour(..)
from Html 				import :: HtmlTag
from Time				import :: Timestamp
from StdString			import class toString
from iTasks				import class iTask
from Config				import :: Config
from InteractionTasks	import :: Menus, :: Menu, class html

import GenVisualize, GenUpdate, JSON, StoreTasks

derive gVisualize		EmailAddress, Session
derive gUpdate			EmailAddress, Session 
derive gVerify			EmailAddress, Session

derive JSONEncode		EmailAddress, Currency, FormButton, User, UserDetails, Session, Task, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Note, Password, Date, Time, DateTime, Choice, MultipleChoice
derive JSONDecode		EmailAddress, Currency, FormButton, User, UserDetails, Session, Task, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Note, Password, Date, Time, DateTime, Choice, MultipleChoice

derive JSONEncode		(->)
derive JSONDecode		(->)
		
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
instance fromString DateTime

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
instance + DateTime	//Naive fieldwise addition
instance + Currency 

instance - Time		//Naive fieldwise subtraction
instance - Date		//Naive fieldwise subtraction
instance - DateTime	//Naive fieldwise subtraction
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
	, emailAddress	:: !EmailAddress
	, roles			:: !Maybe [Role]
	}

:: UserId			:== String
:: Role				:== String

// Session
:: SessionId		:== String
:: Session			=
	{ sessionId	::	!String
	, user		::	!User
	, timestamp	::	!Timestamp
	}

:: ProcessId		:== String
:: ProcessRef a		= ProcessRef !ProcessId

// Tasks
:: TaskNr			:== [Int]		// task nr i.j is administrated as [j,i]
:: TaskId			:== String		// String serialization of TaskNr values
:: MenuId			:== Int

:: Task a =
	{ taskProperties	:: !ManagerProperties					// the task's manager properties
	, groupedProperties	:: !GroupedProperties					// properties about how the tasks behaves inside of a group
	, mbTaskNr			:: !(Maybe TaskNr)						// the task's identifier
	, mbMenuGenFunc		:: !(Maybe MenuGenFunc)					// a function generating a menu structure
	, taskFunc			:: !(*TSt -> *(!TaskResult a,!*TSt))	// a function on TSt implementing the task
	}

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
	, taskDescription	:: !TaskDescription			// Description of the task
	, context			:: !Maybe String			// Optional context information for doing the task (html)
	, priority			:: !TaskPriority			// What is the current priority of this task?
	, deadline			:: !Maybe DateTime			// When is the task due?
	, tags				:: ![String]				// A list of tags
	}

:: TaskDescription	=
	{ title				:: !String					// The task's title
	, description		:: !HtmlTag					// A longer description of the task
	}
		
class descr d
where
	toDescr :: d -> TaskDescription

instance descr String
instance descr (String, descr) | html descr
instance descr TaskDescription

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

:: MenuGenFunc :== *IWorld -> *(Menus, *IWorld)
					
:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task


:: Container a c	= Container a & iTask c		// container for context restrictions

:: TaskEvent	:== (!TaskId,!String,!JSONNode)	// taskid, name, value	

:: *IWorld		=	{ application	:: !String		// The name of the application	
					, store			:: !Store		// The generic data store
					, config		:: !Config		// The server configuration
					, world			:: !*World		// The outside world
					, timestamp		:: !Timestamp	// The timestamp of the current request
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

// Represents the choice of one element from a list
:: Choice			a = Choice			![a] !Int
// Represents the choice of a number of items from a list
:: MultipleChoice	a = MultipleChoice	![a] ![Int]

// Generates a choice with given options
choice 				:: ![a]								-> Choice a
// Generates a choice with given options and preselected item
choiceSel			:: ![a] !Int						-> Choice a
// Gets the currently chosen item
getChoice			:: !(Choice a)						-> a
// Gets the index of the currently chosen item
getChoiceIndex		:: !(Choice a)						-> Int
// Sets the index of the currently chosen item
setChoiceIndex		:: !Int !(Choice a)					-> Choice a
// Transforms the choice's options
mapOptions			:: !(a -> b) !(Choice a)			-> Choice b
// Sets the choice's options, tries to keep the selection as intact as possible
// Returns a flag indicating if the choice still has the same value
setOptions			:: ![a] !(Choice a)					-> (Choice a,Bool) | gEq{|*|} a

// Generates a multiple choice with given options
multipleChoice		:: ![a] 							-> MultipleChoice a
// Generates a multiple choice with given options and preselected items
multipleChoiceSel	:: ![a] ![Int]						-> MultipleChoice a
// Get the currently chosen items
getChoices			:: !(MultipleChoice a)				-> [a]
// Gets the indexes of the currently chosen items
getChoiceIndexes	:: !(MultipleChoice a)				-> [Int]
// Sets the indexes of the currently chosen items
setChoiceIndexes	:: ![Int] !(MultipleChoice a)		-> MultipleChoice a
// Transforms the multiple choice's options
mapOptionsM			:: !(a -> b) !(MultipleChoice a)	-> MultipleChoice b
// Sets the multiple choice's options, tries to keep the selection as intact as possible
setOptionsM			:: ![a] !(MultipleChoice a)			-> MultipleChoice a | gEq{|*|} a

// Plain text notes
:: Note = Note String

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
taskTitle			:: !(Task a)				-> String

/**
* Extracts the description of a task
*/
taskDescription		:: !(Task a)				-> HtmlTag

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