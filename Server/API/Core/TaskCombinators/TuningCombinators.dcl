definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Time	import :: Timestamp
from Shared	import class toReadOnlyShared
import Task

:: Title			= E.s: Title !s			& toString s
:: Description		= E.s: Description !s	& html s
:: Tag				= E.s: Tag !s			& toString s
:: Tags				= E.s: Tags ![s]		& toString s
:: MenuAnnotation	= 	NoMenus
					| 	StaticMenus !Menus
					| E.sharedReadOnly s:
						DynamicMenus !(sharedReadOnly s) !(s -> Menus) & iTask s & toReadOnlyShared sharedReadOnly s

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
instance tune	GroupedBehaviour		//Set grouped behaviour
instance tune	GroupActionsBehaviour	//Set group actions behaviour
instance tune	MenuAnnotation			//Set menu structure for this task and all children
instance tune	Menus					//Abbreviation for StaticMenus
instance tune	FormWidth				//Set form width for this task and all children
