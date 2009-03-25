definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
from TSt	import :: Task
from FormId	import :: Lifespan, :: StorageFormat, :: Mode
from Types	import :: GarbageCollect

class   (<<@) infixl 3 b :: !(Task a) !b 	-> Task a 	 
instance <<@	  Lifespan			// default: Session
				, StorageFormat		// default: PlainString
				, Mode				// default: Edit
				, GarbageCollect	// default: Collect
