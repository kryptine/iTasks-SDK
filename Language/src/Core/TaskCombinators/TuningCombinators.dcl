definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from Types	import :: Task, :: TaskDescription

//Annotation combinator
class   (<<@) infixl 2 b :: !(Task a) !b 	-> Task a 
class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a
/**
* Change the label of a task
*/
instance <<@	String
instance @>>	String

instance <<@	TaskDescription
instance @>>	TaskDescription