implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> (Task a) | iData a
instance <<@  Lifespan
where   (<<@) (Task name mbCxt tf) lifespan			= Task name mbCxt setTaskLifespan
		where
			setTaskLifespan tst=:{TSt|options}
			= IF_Ajax 
				(IF_ClientServer																// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == LSClient && (lifespan == LSTxtFile || lifespan == LSDataFile))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> tf {TSt|tst & options.tasklife = lifespan}))				// assign option on client
						(\tst -> tf {TSt|tst & options.tasklife = lifespan})tst					// assign option on server
					)
					(tf {TSt|tst & options.tasklife = lifespan})								// assign option on server
				)
				(tf {TSt|tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) (Task name mbCxt tf) storageformat 	= Task name mbCxt (\tst -> tf {TSt|tst & options.taskstorage = storageformat})
instance <<@  Mode
where   (<<@) (Task name mbCxt tf) mode 			= Task name mbCxt (\tst -> tf {TSt|tst & options.taskmode = mode})
instance <<@  TaskCombination
where	(<<@) (Task name mbCxt tf) combination		= Task name mbCxt (\tst -> tf (setNextCombination combination tst))