definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Types	import :: Task, :: ManagerProperties, :: UserName, :: TaskPriority
from Time	import :: Timestamp
from TaskTree import :: GroupedBehaviour, :: GroupActionsBehaviour

//Annotation combinator
class   (<<@) infixl 2 b :: !(Task a) !b 	-> Task a 
class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a

instance <<@	ManagerProperties		//Set initial properties
instance @>>	ManagerProperties
instance <<@	UserName				//Set initial worker
instance @>>	UserName
instance <<@	String					//Set initial subject
instance @>>	String
instance <<@	TaskPriority			//Set initial priority
instance @>>	TaskPriority
instance <<@	Timestamp				//Set initial deadline	
instance @>>	Timestamp

instance <<@	GroupedBehaviour		//Set grouped behaviour
instance @>>	GroupedBehaviour
instance <<@	GroupActionsBehaviour
instance @>>	GroupActionsBehaviour