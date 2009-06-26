implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= Task Nothing setTaskLifespan
		where
			setTaskLifespan tst=:{TSt|options}
			
			= IF_Ajax 
				(IF_ClientServer																		// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == LSClient && (lifespan == LSTxtFile || lifespan == LSDataFile))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> applyTask task {TSt|tst & options.tasklife = lifespan}))				// assign option on client
						(\tst -> applyTask task {TSt|tst & options.tasklife = lifespan})tst				// assign option on server
					)
					(applyTask task {TSt|tst & options.tasklife = lifespan})								// assign option on server
				)
				(applyTask task {TSt|tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= Task Nothing (\tst -> applyTask task {TSt|tst & options.taskstorage = storageformat})
instance <<@  Mode
where   (<<@) task mode 			= Task Nothing (\tst -> applyTask task {TSt|tst & options.taskmode = mode})
instance <<@  TaskCombination
where	(<<@) task combination		= Task Nothing (\tst -> applyTask task (setNextCombination combination tst))