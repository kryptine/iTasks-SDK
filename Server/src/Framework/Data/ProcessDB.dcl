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
:: Process 		= {	taskId			:: !TaskId						// The process identification				  
				  //Public process meta data
				  , properties		:: !TaskProperties				// The properties of the main task node of this process
				  //System internal information
				  , changeCount		:: !Int							// The number of task changes that have been applied
				  , mutable			:: !Bool						// May the process be evaluated further (required for duplication of processes by changes)
				  , menus			:: !(Maybe [Menu])
				  , inParallelType	:: !(Maybe TaskParallelType)	// The type of parallel, if the process is part of one
				  }				


				 
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
	
getActionIcon :: !Action -> String

:: Menu 		= Menu !String ![MenuItem]
:: MenuItem 	= SubMenu !String ![MenuItem] 
				| MenuItem !String !Action !(Maybe Hotkey)
				| MenuSeparator 
				| MenuName !String !MenuItem
				
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}
			
:: Key = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

class ProcessDB st
where
	createProcess			:: !Process												!*st -> (!ProcessId,		!*st)
	deleteProcess			:: !TaskId												!*st -> (!Bool,				!*st)
	getProcess				:: !TaskId												!*st -> (!Maybe Process,	!*st)
	getProcessForUser		:: !User !TaskId										!*st -> (!Maybe Process,	!*st)
	getProcessForManager 	:: !User !TaskId 										!*st -> (!Maybe Process, 	!*st)
	getProcesses 			:: ![TaskStatus] 										!*st -> (![Process], 		!*st)
	getProcessesById		:: ![TaskId]											!*st -> (![Process],		!*st)
	getProcessesForUser		:: !User ![TaskStatus]									!*st -> (![Process],		!*st)
	
	setProcessOwner			:: !User !TaskId										!*st -> (!Bool,				!*st)
	setProcessStatus		:: !TaskStatus !TaskId									!*st -> (!Bool,				!*st)
	
	updateProcess			:: !TaskId (Process -> Process)							!*st -> (!Bool,				!*st)
	updateProcessProperties	:: !TaskId (TaskProperties -> TaskProperties)			!*st -> (!Bool,				!*st)
	
	removeFinishedProcesses :: 														!*st -> (!Bool, 			!*st)
	
	setImmutable			:: !TaskId												!*st -> *st
	copySubProcesses		:: !TaskId !TaskId										!*st -> *st
	deleteSubProcesses		:: !TaskId												!*st -> *st

instance ProcessDB IWorld
instance ProcessDB TSt


instance == Action

derive gPrint Action
derive gVisualize Action
derive gUpdate Action
derive gParse Action
derive gError Action
derive gHint Action