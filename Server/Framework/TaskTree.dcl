definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import Maybe, Either, HTML, Time, Types
from JSON 			import :: JSONNode
from TUIDefinition	import :: TUIDef, :: InteractionLayoutMerger, :: ParallelLayoutMerger, :: ResultLayoutMerger, :: LayoutMerger, :: TUIInteraction, :: TUIParallel, :: TUIResult

:: SpineTreeContainer					:== TaskTreeContainer Void Void Void Void
:: UITreeContainer						:== TaskTreeContainer [TUIDef] TTContainerType (![TUIDef],![TUIDef]) HtmlTag
:: JSONTreeContainer					:== TaskTreeContainer Void Void JSONNode JSONNode
:: NonNormalizedTreeContainer			:== TaskTreeContainer ActionMenu TaskContainerType TTNNInteractionTask TTNNFinished

:: SpineParallelTreeContainer			:== ParallelTaskTreeContainer Void Void Void
:: UIParallelTreeContainer				:== ParallelTaskTreeContainer TTContainerType (![TUIDef],![TUIDef]) HtmlTag
:: JSONParallelTreeContainer			:== ParallelTaskTreeContainer Void JSONNode JSONNode
:: NonNormalizedParallelTreeContainer	:== ParallelTaskTreeContainer TaskContainerType TTNNInteractionTask TTNNFinished

:: SpineTree							:== TaskTree Void Void Void
:: UITree								:== TaskTree TTContainerType (![TUIDef],![TUIDef]) HtmlTag
:: JSONTree								:== TaskTree Void JSONNode JSONNode
:: NonNormalizedTree					:== TaskTree TaskContainerType TTNNInteractionTask TTNNFinished

:: TTNNInteractionTask					:== (*IWorld -> *(![TUIDef],![(Action,Bool)],!*IWorld),*IWorld -> *(!JSONNode,!*IWorld))
:: TTNNFinished							:== (HtmlTag,JSONNode)

//A container used for tree representing top level tasks (including the menu & the actual tree)
:: TaskTreeContainer menu containerType interactionOutput finishedOutput = TTContainer !menu !.(TaskTree containerType interactionOutput finishedOutput)
//A container used for subtrees representing subtasks or parallel (including the type, the actual tree & an index determining the order of appearance)
:: ParallelTaskTreeContainer containerType interactionOutput finishedOutput = TTParallelContainer !Int !containerType !.(TaskTree containerType interactionOutput finishedOutput)

:: TaskTree containerType interactionOutput finishedOutput
	//A task that is composed of a number of parallel executed main tasks (a division of big chunks of work)
	= TTParallelTask	!TaskInfo !.[.ParallelTaskTreeContainer containerType interactionOutput finishedOutput]								
	//A task that can be worked on through a gui
	| TTInteractionTask	!TaskInfo interactionOutput
	//A completed task (the flag indicates if the result is shown to the user)
	| TTFinishedTask	TaskInfo finishedOutput !Bool

// similar to TaskContainerType but without tasks not shown (detached & hidden) and with calculated menus
:: TTContainerType	= TTWindow !WindowTitle ![TUIDef]	// task shown in a window (with own menu)
					| TTDialog !WindowTitle				// task shown as dialogue (without own menu)
					| TTInBody							// task shown in the body of the parallel container

:: TaskInfo	=	{ taskId				:: !TaskId											//Task number in string format
				, title					:: !String											//Short title of the task
				, description			:: !String											//Description of the task (html)
				, type					:: !Maybe InteractionTaskType
				, isControlTask			:: !Bool
				, localInteraction		:: !Bool
				, interactionLayout		:: !TIInteractionLayoutMerger
				, parallelLayout		:: !TIParallelLayoutMerger
				, resultLayout			:: !TIResultLayoutMerger
				}
				
 // special types for layout mergers, needed to be able to use generic map
:: TIInteractionLayoutMerger	= TIInteractionLayoutMerger	!InteractionLayoutMerger
:: TIParallelLayoutMerger		= TIParallelLayoutMerger	!ParallelLayoutMerger
:: TIResultLayoutMerger			= TIResultLayoutMerger		!ResultLayoutMerger

toSpineTreeContainer	:: !NonNormalizedTreeContainer			-> SpineTreeContainer
toUITreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!UITreeContainer,!*IWorld)
toJSONTreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!JSONTreeContainer,!*IWorld)
