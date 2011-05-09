definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Time	import :: Timestamp
import Task

:: Title				= E.s: Title !s			& toString s
:: Description			= E.s: Description !s	& html s
:: Tag					= E.s: Tag !s			& toString s
:: Tags					= E.s: Tags ![s]		& toString s
:: ControlTask			= ControlTask
:: LocalInteractionTask	= LocalInteractionTask

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	Title				//Set initial title
instance tune	Description			//Set initial subject
instance tune	Tag					//Append Tag
instance tune	Tags				//Append List of Tags
instance tune	InteractionTaskType
instance tune	ControlTask
instance tune	LocalInteractionTask
instance tune	InteractionLayoutMerger
instance tune	ParallelLayoutMerger
instance tune	ResultLayoutMerger
