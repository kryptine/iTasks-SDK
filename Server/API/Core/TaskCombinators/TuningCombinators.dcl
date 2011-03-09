definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Time	import :: Timestamp
import Task

:: Title			= E.s: Title !s			& toString s
:: Description		= E.s: Description !s	& html s
:: Tag				= E.s: Tag !s			& toString s
:: Tags				= E.s: Tags ![s]		& toString s
:: MenuAnnotation	= 	NoMenus
					| 	StaticMenus !MenuDefinition
					| E.s w:
						DynamicMenus !(Shared s w) !(s -> MenuDefinition) & iTask s

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	ManagerProperties		//Set initial properties
instance tune	User					//Set initial worker
instance tune	Title					//Set initial title
instance tune	Description				//Set initial subject
instance tune	TaskPriority			//Set initial priority
instance tune	DateTime				//Set initial deadline
instance tune	Tag						//Append Tag
instance tune	Tags					//Append List of Tags
instance tune	MenuAnnotation			//Set menu structure for this task and all children
instance tune	MenuDefinition			//Abbreviation for StaticMenus
instance tune	FormWidth				//Set form width for this task and all children
