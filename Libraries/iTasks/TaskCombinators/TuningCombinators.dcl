definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from TSt	import :: Task
from Types	import :: TaskCombination
from FormId	import :: Lifespan, :: StorageFormat, :: Mode

//Annotation combinator
class   (<<@) infixl 2 b :: !(Task a) !b 	-> Task a 	 

/**
* Change where the state of forms is to be stored.
*/
instance <<@	Lifespan			// default: Session
/**
* Change how states should be serialized.
*/
instance <<@	StorageFormat		// default: PlainString
/**
* Change the default mode for forms
*/
instance <<@	Mode				// default: Edit
/**
* Change the combination of the next underlying parallel task
*/
instance <<@	TaskCombination		// default: TTVertical