definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Types		import :: Task, :: ManagerProperties, :: User, :: TaskPriority
from Time		import :: Timestamp
from TaskTree	import :: GroupedBehaviour, :: GroupActionsBehaviour

:: Subject s		= Subject !s		& toString s
:: Description s	= Description !s	& toString s
:: Tag s			= Tag !s			& toString s
:: Tags s			= Tags ![s]			& toString s

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	ManagerProperties		//Set initial properties
instance tune	User					//Set initial worker
instance tune	(Subject s)				//Set initial subject
instance tune	(Description s)			//Set initial subject
instance tune	TaskPriority			//Set initial priority
instance tune	Timestamp				//Set initial deadline
instance tune	(Tag s)					//Append Tag
instance tune	(Tags s)				//Append List of Tags	
instance tune	GroupedBehaviour		//Set grouped behaviour
instance tune	GroupActionsBehaviour	//Set group actions behaviour