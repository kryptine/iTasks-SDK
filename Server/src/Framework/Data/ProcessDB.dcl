definition module ProcessDB
/**
* This module provides an abstract process database
* 
*/
import StdMaybe
import Types, TSt
from TaskTree	import :: TaskProperties, :: TaskParallelType
from Time		import :: Timestamp

/**
* Our local process type
*/
:: Process 		= {	processId		:: !ProcessId					// The process identification				  
				  , status			:: !ProcessStatus				// The status of the process (updated after each run)
				  , parent			:: !ProcessId					// The (direct) parent process
				  , properties		:: !TaskProperties				// The properties of the main task node of this process
				  , changeCount		:: !Int							// The number of task changes that have been applied
				  , mutable			:: !Bool						// May the process be evaluated further (required for duplication of processes by changes)
				  , menus			:: !(Maybe [Menu])
				  , inParallelType	:: !(Maybe TaskParallelType)	// The type of parallel, if the process is part of one
				  }				

:: ProcessStatus =	Active			// A process is active and can be further evaluated
				 |	Suspended		// A process is (temporarily) suspended and will not be evaluated until it is activated 
				 |	Finished		// A process terminated normally
				 |	Excepted		// A process terminated with an exception
				 |	Deleted			// A process is marked for deletion and may be garbage collected
				 
:: Action
	= ActionLabel !String
	| ActionParam !String !String
	| ActionIcon !String !String
	| ActionOk
	| ActionCancel
	| ActionYes
	| ActionNo
	| ActionNext
	| ActionPrevious
	| ActionFinish
	| ActionNew
	| ActionOpen
	| ActionSaveAs
	| ActionSave
	| ActionQuit
	| ActionClose
	| ActionHelp
	| ActionShowAbout
	| ActionFind
	
:: Hotkey =	{ keys	:: !String
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}
	
getActionIcon :: !Action -> String

:: Menu 		= Menu !String ![MenuItem]
:: MenuItem 	= SubMenu !String ![MenuItem] 
				| MenuItem !String !Action 
				| MenuSeparator 
				| MenuName !String !MenuItem

class ProcessDB st
where
	createProcess			:: !Process												!*st -> (!ProcessId,	!*st)
	deleteProcess			:: !ProcessId											!*st -> (!Bool,			!*st)
	getProcess				:: !ProcessId											!*st -> (!Maybe Process,!*st)
	getProcessForUser		:: !UserName !ProcessId									!*st -> (!Maybe Process,!*st)
	getProcesses 			:: ![ProcessStatus] 									!*st -> (![Process], 	!*st)
	getProcessesById		:: ![ProcessId]											!*st -> (![Process],	!*st)
	getProcessesForUser		:: !UserName ![ProcessStatus]							!*st -> (![Process],	!*st)
	getTempProcessesForUser :: !UserName ![ProcessStatus]							!*st -> (![Process],	!*st)
	
	setProcessOwner			:: !UserName !ProcessId									!*st -> (!Bool,			!*st)
	setProcessStatus		:: !ProcessStatus !ProcessId							!*st -> (!Bool,			!*st)

	updateProcess			:: !ProcessId (Process -> Process)						!*st -> (!Bool,			!*st)
	updateProcessProperties	:: !ProcessId (TaskProperties -> TaskProperties)		!*st -> (!Bool,			!*st)

	removeFinishedProcesses :: 														!*st -> (!Bool, 		!*st)

	setImmutable			:: !ProcessId											!*st -> *st
	copySubProcesses		:: !ProcessId !ProcessId								!*st -> *st
	deleteSubProcesses		:: !ProcessId											!*st -> *st



instance ProcessDB TSt
instance toString ProcessStatus
instance == ProcessStatus
instance == Action

derive gPrint Action
derive gVisualize Action
derive gUpdate Action
derive gParse Action
derive gError Action
derive gHint Action