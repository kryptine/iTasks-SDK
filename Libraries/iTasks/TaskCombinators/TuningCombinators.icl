implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= Task setTaskLifespan
		where
			setTaskLifespan tst=:{options}
			
			= IF_Ajax 
				(IF_ClientServer																		// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == LSClient && (lifespan == LSTxtFile || lifespan == LSDataFile))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> accTaskTSt task {tst & options.tasklife = lifespan}))				// assign option on client
						(\tst -> accTaskTSt task {tst & options.tasklife = lifespan})tst				// assign option on server
					)
					(accTaskTSt task {tst & options.tasklife = lifespan})								// assign option on server
				)
				(accTaskTSt task {tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= Task (\tst -> accTaskTSt task {tst & options.taskstorage = storageformat})
instance <<@  Mode
where   (<<@) task mode 			= Task (\tst -> accTaskTSt task {tst & options.taskmode = mode})
instance <<@  TaskCombination
where	(<<@) task combination		= Task (\tst -> accTaskTSt task (setNextCombination combination tst))