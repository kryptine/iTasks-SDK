definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import Maybe, Either, HTML, Time, Types
from JSON 			import :: JSONNode
from TUIDefinition	import :: TUIDef, :: TUIUpdate

:: SpineTree					:== TaskTree Void Void Void
:: UITree						:== TaskTree TTContainerType InteractiveTask HtmlTag
:: JSONTree						:== TaskTree Void JSONNode JSONNode
:: NonNormalizedTree			:== TaskTree TaskContainerType TTNNInteractiveTask TTNNFinished

:: SpineTreeContainer			:== TaskTreeContainer Void Void Void
:: UITreeContainer				:== TaskTreeContainer TTContainerType InteractiveTask HtmlTag
:: JSONTreeContainer			:== TaskTreeContainer Void JSONNode JSONNode
:: NonNormalizedTreeContainer	:== TaskTreeContainer TaskContainerType TTNNInteractiveTask TTNNFinished

:: TTNNInteractiveTask			:== (*IWorld -> *(!InteractiveTask,![(Action,Bool)],!*IWorld),*IWorld -> *(!JSONNode,!*IWorld))
:: TTNNFinished					:== (HtmlTag,JSONNode)

//A container used for subtrees representing top level tasks or subtasks or parallel (including the type, the actual tree & a flag indicating if the task is a control task)
:: TaskTreeContainer containerType interactiveOutput finishedOutput = TTContainer !containerType !.(TaskTree containerType interactiveOutput finishedOutput) !Bool

:: TaskTree containerType interactiveOutput finishedOutput
	//A task that is composed of a number of parallel executed main tasks (a division of big chunks of work)
	= TTParallelTask	!TaskInfo !.[.TaskTreeContainer containerType interactiveOutput finishedOutput]								
	//A task that can be worked on through a gui
	| TTInteractiveTask	!TaskInfo !InteractiveTaskType interactiveOutput								
	//A completed task (the flag indicates if the result is shown to the user)
	| TTFinishedTask	!TaskInfo finishedOutput !Bool

// similar to TaskContainerType but with calculated menus
:: TTContainerType	= TTDetached ![TUIDef]				// task detached as separate process
					| TTWindow !WindowTitle ![TUIDef]	// task shwon in a window (with own menu)
					| TTDialog !WindowTitle				// task shwon as dialogue (without own menu)
					| TTInBody							// task shown in the body of the parallel container
					| TTHidden							// task not shown to the user

:: TaskInfo	=	{ taskId				:: !TaskId											//Task number in string format
				, subject				:: !String											//Short subject of the task
				, description			:: !String											//Description of the task (html)
				, tags					:: ![String]
				, formWidth				:: !Maybe FormWidth
				}

// definition/updates for interactive tasks
:: InteractiveTask	= Definition	!TUIDef			![TUIDef]	//Definition for rendering a user interface & buttons
					| Updates		![TUIUpdate]	![TUIDef]	//Update an already rendered user interface & buttons

toSpineTreeContainer	:: !NonNormalizedTreeContainer			-> SpineTreeContainer
toUITreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!UITreeContainer,!*IWorld)
toJSONTreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!JSONTreeContainer,!*IWorld)
