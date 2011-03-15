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
:: UITree						:== TaskTree InteractiveTask HtmlTag MenuDefinition
:: JSONTree						:== TaskTree JSONNode JSONNode MenuDefinition
:: NonNormalizedTree			:== TaskTree TTNNInteractiveTask TTNNFinished MenuGenFunc

:: TTNNInteractiveTask			:== (*IWorld -> *(!InteractiveTask,!*IWorld),*IWorld -> *(!JSONNode,!*IWorld))
:: TTNNFinished					:== (HtmlTag,JSONNode)

:: TaskTree interactiveOutput finishedOutput menus
	//NODE CONSTRUCTORS
	
	//A task that is composed of a number of sequentially executed subtasks
	= TTSequenceTask	!.(TaskInfo menus) !.[TaskTree interactiveOutput finishedOutput menus]
	//A task that is composed of a number of parallel executed main tasks (a division of big chunks of work)
	| TTParallelTask	!.(TaskInfo menus) !.[TaskTree interactiveOutput finishedOutput menus]
	
	//LEAF CONSTRUCTORS
											
	//A task that can be worked on through a gui
	| TTInteractiveTask	!.(TaskInfo menus) !InteractiveTaskType interactiveOutput								
	//A completed task
	| TTFinishedTask	!.(TaskInfo menus) finishedOutput !Bool


:: NonNormalizedTaskInfo	:== TaskInfo MenuGenFunc
:: NormalizedTaskInfo		:== TaskInfo MenuDefinition

:: TaskInfo	menus =		{ taskId				:: !TaskId											//Task number in string format
						, subject				:: !String											//Short subject of the task
						, description			:: !String											//Description of the task (html)
						, tags					:: ![String]
						, containerType			:: !TaskContainerType
						, menus					:: !.(Maybe menus)
						, formWidth				:: !Maybe FormWidth
						}

// give definition/updates or determine it after entire tree is build, needed for updateShared, ...
:: InteractiveTask	= Definition	!TUIDef			![(Action,Bool)]					//Definition for rendering a user interface
					| Updates		![TUIUpdate]	![(Action,Bool)]					//Update an already rendered user interface

toSpineTree	:: !NonNormalizedTree			-> SpineTree
toUITree	:: !NonNormalizedTree !*IWorld	-> (!UITree,!*IWorld)
toJSONTree	:: !NonNormalizedTree !*IWorld	-> (!JSONTree,!*IWorld)
