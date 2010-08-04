definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import StdMaybe, Either
import Types
import Html, Time
import RPC

from   ProcessDB		import :: Action, :: Menu, :: MenuItem
from   JSON 			import :: JSONNode
from   TUIDefinition	import :: TUIDef, :: TUIUpdate

// give definition/updates or determine it after entire tree is build, needed for updateShared, ...
:: InteractiveTask	= Definition ([TUIDef],[TUIButton]) [(Action,Bool)]															//Definition for rendering a user interface
					| Updates [TUIUpdate] [(Action,Bool)]																		//Update an already rendered user interface
					| Message ([TUIDef],[TUIButton]) [(Action,Bool)]															//Just show a message
					| Func (*TSt -> *(!InteractiveTask, !*TSt))																	//Function for delayed generation of an interface definition.
																																//These functions are evaluated after the full tree has been built.
					
:: TaskTree			= TTMainTask		TaskInfo TaskProperties (Maybe [Menu]) !(Maybe TaskParallelType) TaskTree				//A task that is treated as a main chunk of work
					| TTInteractiveTask	TaskInfo InteractiveTask																//A task that can be worked on through a gui 
					| TTMonitorTask		TaskInfo [HtmlTag]																		//A task that upon evaluation monitors a condition and may give status output
					| TTInstructionTask TaskInfo [HtmlTag] (Maybe [HtmlTag])													//A task which displays an (offline) instruction to the user
					| TTRpcTask			TaskInfo RPCExecute																		//A task that represents an rpc invocation
					| TTSequenceTask	TaskInfo [TaskTree]																		//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskParallelInfo [TaskTree]													//A task that is composed of a number of parallel executed subprocesses  
					| TTGroupedTask		TaskInfo [TaskTree] ![(Action, (Either Bool (*TSt -> *(!Bool,!*TSt))))] !(Maybe String)	//A task that is composed of a number of grouped subtasks
					| TTFinishedTask	TaskInfo [HtmlTag]																		//A completed task
							
:: TaskInfo	=		{ taskId				:: TaskId											//Task number in string format
					, taskLabel				:: String											//Descriptive label of the task
					, taskDescription		:: String
					, tags					:: [String]
					, groupedBehaviour		:: GroupedBehaviour
					, groupActionsBehaviour	:: GroupActionsBehaviour
					}

:: TaskParallelInfo =
	{ type			:: TaskParallelType //Indicating the scope of the parallel. 
	, description	:: String			//Description of the behavior of this parallel. This is also displayed in the overview panel in the interface
	}

:: TaskParallelType = Open 				//Everybody to whom a subtask is assigned can see the full status of this parallel, including the results of others
					| Closed			//Only the manager can see the overview. For assigned users, it just looks like an ordinary task.
	
:: GroupedBehaviour = GBFixed 			//The editor is fixed in the window, user can undock editor (making it floating)
					| GBFloating		//The editor is shown in a floating window, user can dock editor (making it fixed)
					| GBAlwaysFixed		//Same as Fixed, but user cannot undock
					| GBAlwaysFloating	//Same as Floating, but user cannot dock
					| GBModal			//The editor is shown in a modal dialog

// Determines if group-actions are added to actions of interactive task
:: GroupActionsBehaviour	= IncludeGroupActions
							| ExcludeGroupActions