definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from TSt	import :: Task
from Types	import :: TaskCombination

//Annotation combinator
class   (<<@) infixl 2 b :: !(Task a) !b 	-> Task a 

/**
* Change the label of a task
*/
instance <<@	String
/**
* Change the combination of the next underlying parallel task
*/
instance <<@	TaskCombination		// default: TTVertical