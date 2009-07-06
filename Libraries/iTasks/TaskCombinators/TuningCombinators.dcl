definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from TSt	import :: Task
from Types	import :: TaskCombination
from FormId	import :: Lifespan, :: StorageFormat, :: Mode
import iDataForms

//Annotation combinator
class   (<<@) infixl 2 b :: !(Task a) !b 	-> Task a 		| iData a

/**
* Change the label of a task
*/
instance <<@	String

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