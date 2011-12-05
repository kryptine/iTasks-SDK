definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Time	import :: Timestamp
import Task, SystemTypes

:: Description			= E.s: Description !s				& toString s
:: Attribute			= E.s: Attribute !String !s			& toString s
:: Attributes			= E.s: Attributes ![(!String,!s)]	& toString s
:: LocalInteractionTask	= LocalInteractionTask
:: Icon					= Icon !String
:: Hide					= Hide
:: Window				= Window

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	Description			//Set initial description
instance tune	Icon				//Set icon
instance tune	Attribute			//Set attribute
instance tune	Attributes			//Set multiple attributes at once
instance tune	Hide
instance tune	Window
instance tune	InteractionTaskType
instance tune	LocalInteractionTask
instance tune	InteractionLayouter
instance tune	ParallelLayouter

instance tune	LayoutTweak
instance tune	TUITweak
instance tune	ActionTweak