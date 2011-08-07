definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Time	import :: Timestamp
import Task, SystemTypes

:: Description			= E.s: Description !s	& toString s
:: Tag					= E.s: Tag !s			& toString s
:: Tags					= E.s: Tags ![s]		& toString s
:: LocalInteractionTask	= LocalInteractionTask
:: ControlTask			= ControlTask

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	Description			//Set initial description
instance tune	Tag					//Append Tag
instance tune	Tags				//Append List of Tags
instance tune	InteractionTaskType
instance tune	LocalInteractionTask
instance tune	ControlTask
instance tune	InteractionLayouter
instance tune	ParallelLayouter
