definition module TuningCombinators
/**
* This module provides combinators for fine-tuning of workflows.
*/
import Types, TSt

class (<<@) infixl 3 b :: !(Task a) !b 	-> Task a 	 
instance <<@	  Lifespan			// default: Session
				, StorageFormat		// default: PlainString
				, Mode				// default: Edit
				, GarbageCollect	// default: Collect
