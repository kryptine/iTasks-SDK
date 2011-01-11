definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Types				import :: Task, :: ManagerProperties, :: User, :: TaskPriority, :: DateTime
from Time				import :: Timestamp
from TaskTree			import :: GroupedBehaviour, :: GroupActionsBehaviour
from StoreTasks			import :: DBId
from InteractionTasks	import :: Menus, :: Menu(..), :: MenuItem, :: MenuLabel, class html
from iTasks				import class iTask
import GenVisualize, GenUpdate

:: Title			= E.s: Title !s			& toString s
:: Description		= E.s: Description !s	& html s
:: Tag				= E.s: Tag !s			& toString s
:: Tags				= E.s: Tags ![s]		& toString s
:: MenuAnnotation	= 		NoMenus
					| 		StaticMenus		!Menus
					| E.s:	DynamicMenus	!(DBId s) !(s -> Menus) & iTask s

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
instance tune	MenuAnnotation			//Set menu structure for this tasks and all children
instance tune	Menus					//Abbreviation for StaticMenus