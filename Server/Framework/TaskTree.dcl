definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import Maybe, Either, HTML, Time, Types
from JSON 			import :: JSONNode
from TUIDefinition	import :: TUIDef, :: InteractiveLayoutMerger, :: ParallelLayoutMerger, :: ResultLayoutMerger, :: LayoutMerger, :: TUIInteractive, :: TUIParallel, :: TUIResult

:: SpineTreeContainer					:== TaskTreeContainer Void Void Void Void
:: UITreeContainer						:== TaskTreeContainer [TUIDef] TTContainerType (![TUIDef],!Maybe TUIDef,![TUIDef]) HtmlTag
:: JSONTreeContainer					:== TaskTreeContainer Void Void JSONNode JSONNode
:: NonNormalizedTreeContainer			:== TaskTreeContainer ActionMenu TaskContainerType TTNNInteractiveTask TTNNFinished

:: SpineParallelTreeContainer			:== ParallelTaskTreeContainer Void Void Void
:: UIParallelTreeContainer				:== ParallelTaskTreeContainer TTContainerType (![TUIDef],!Maybe TUIDef,![TUIDef]) HtmlTag
:: JSONParallelTreeContainer			:== ParallelTaskTreeContainer Void JSONNode JSONNode
:: NonNormalizedParallelTreeContainer	:== ParallelTaskTreeContainer TaskContainerType TTNNInteractiveTask TTNNFinished

:: SpineTree							:== TaskTree Void Void Void
:: UITree								:== TaskTree TTContainerType (![TUIDef],!Maybe TUIDef,![TUIDef]) HtmlTag
:: JSONTree								:== TaskTree Void JSONNode JSONNode
:: NonNormalizedTree					:== TaskTree TaskContainerType TTNNInteractiveTask TTNNFinished

:: TTNNInteractiveTask					:== (*IWorld -> *(!(![TUIDef],!Maybe TUIDef),![(Action,Bool)],!*IWorld),*IWorld -> *(!JSONNode,!*IWorld))
:: TTNNFinished							:== (HtmlTag,JSONNode)

//A container used for tree representing top level tasks (including the menu, the actual tree & a flag indicating if the task is a control task)
:: TaskTreeContainer menu containerType interactiveOutput finishedOutput = TTContainer !menu !.(TaskTree containerType interactiveOutput finishedOutput) !Bool
//A container used for subtrees representing subtasks or parallel (including the type, the actual tree, an index determining the order of appearance & a flag indicating if the task is a control task)
:: ParallelTaskTreeContainer containerType interactiveOutput finishedOutput = TTParallelContainer !Int !containerType !.(TaskTree containerType interactiveOutput finishedOutput) !Bool

:: TaskTree containerType interactiveOutput finishedOutput
	//A task that is composed of a number of parallel executed main tasks (a division of big chunks of work)
	= TTParallelTask	!TaskInfo !.[.ParallelTaskTreeContainer containerType interactiveOutput finishedOutput]								
	//A task that can be worked on through a gui
	| TTInteractiveTask	!TaskInfo !InteractiveTaskType interactiveOutput								
	//A completed task (the flag indicates if the result is shown to the user)
	| TTFinishedTask	TaskInfo finishedOutput !Bool

// similar to TaskContainerType but without tasks not shown (detached & hidden) and with calculated menus
:: TTContainerType	= TTWindow !WindowTitle ![TUIDef]	// task shown in a window (with own menu)
					| TTDialog !WindowTitle				// task shown as dialogue (without own menu)
					| TTInBody							// task shown in the body of the parallel container

:: TaskInfo	=	{ taskId				:: !TaskId											//Task number in string format
				, subject				:: !String											//Short subject of the task
				, description			:: !String											//Description of the task (html)
				, interactiveLayout		:: !TIInteractiveLayoutMerger
				, parallelLayout		:: !TIParallelLayoutMerger
				, resultLayout			:: !TIResultLayoutMerger
				}
				
 // special types for layout mergers, needed to be able to use generic map
:: TIInteractiveLayoutMerger	= TIInteractiveLayoutMerger	!InteractiveLayoutMerger
:: TIParallelLayoutMerger		= TIParallelLayoutMerger	!ParallelLayoutMerger
:: TIResultLayoutMerger			= TIResultLayoutMerger		!ResultLayoutMerger

toSpineTreeContainer	:: !NonNormalizedTreeContainer			-> SpineTreeContainer
toUITreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!UITreeContainer,!*IWorld)
toJSONTreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!JSONTreeContainer,!*IWorld)
