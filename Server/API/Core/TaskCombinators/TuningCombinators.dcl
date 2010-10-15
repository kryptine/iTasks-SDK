definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Types				import :: Task, :: ManagerProperties, :: User, :: TaskPriority, :: DateTime
from Time				import :: Timestamp
from TaskTree			import :: GroupedBehaviour, :: GroupActionsBehaviour
from StoreTasks			import :: DBId
from InteractionTasks	import :: Menus, :: Menu(..), :: MenuItem
from iTasks				import class iTask
import GenVisualize, GenUpdate

:: Subject s		= Subject !s		& toString s
:: Description s	= Description !s	& toString s
:: Tag s			= Tag !s			& toString s
:: Tags s			= Tags ![s]			& toString s
:: MenuAnnotation s	= NoMenus
					| StaticMenus	!Menus
					| DynamicMenus	!(DBId s) !(s -> Menus) & iTask s

//Annotation combinator
class tune b :: !b !(Task a) -> Task a
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b

instance tune	ManagerProperties		//Set initial properties
instance tune	User					//Set initial worker
instance tune	(Subject s)				//Set initial subject
instance tune	(Description s)			//Set initial subject
instance tune	TaskPriority			//Set initial priority
instance tune	DateTime				//Set initial deadline
instance tune	(Tag s)					//Append Tag
instance tune	(Tags s)				//Append List of Tags	
instance tune	GroupedBehaviour		//Set grouped behaviour
instance tune	GroupActionsBehaviour	//Set group actions behaviour
instance tune	(MenuAnnotation s)		//Set menu structure for this tasks and all children
instance tune	Menus					//Abbreviation for StaticMenus