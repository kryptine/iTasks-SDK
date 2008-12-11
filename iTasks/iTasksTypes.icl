implementation module iTasksTypes

derive gForm 	Void, []
derive gUpd 	Void, []
derive gParse 	Void
derive gPrint	Void
derive gerda 	Void
derive read 	Void
derive write 	Void


import StdEnv
import InternaliTasksCommon, InternaliTasksThreadHandling, BasicCombinators
import GenBimap


// ******************************************************************************************************
// Overloaded Functions on Tasks
// ******************************************************************************************************

class 	(<<@) infixl 3 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= Task setTaskLifespan
		where
			setTaskLifespan tst=:{options}
			
			= IF_Ajax 
				(IF_ClientServer															// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == LSClient && (lifespan == LSTxtFile || lifespan == LSDataFile || lifespan == LSDatabase))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> appTaskTSt task {tst & options.tasklife = lifespan}))						// assign option on client
						(\tst -> appTaskTSt task {tst & options.tasklife = lifespan})tst							// assign option on server
					)
					(appTaskTSt task {tst & options.tasklife = lifespan})								// assign option on server
				)
				(appTaskTSt task {tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= Task (\tst -> appTaskTSt task {tst & options.taskstorage = storageformat})
instance <<@  Mode
where   (<<@) task mode 			= Task (\tst -> appTaskTSt task {tst & options.taskmode = mode})
instance <<@  GarbageCollect
where   (<<@) task gc 				= Task (\tst -> appTaskTSt task {tst & options.gc = gc})

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  EvaluationOption
where   (@>>) UseAjax task			= Task (\tst -> IF_Ajax 
												(appTaskTSt (mkTaskThread UseAjax task) tst)
												(appTaskTSt (newTask "Ajax Thread Disabled" task) tst))
		(@>>) OnClient  task 		= Task (\tst -> IF_Ajax 
												(appTaskTSt (mkTaskThread OnClient task) tst)
												(appTaskTSt (newTask "Client Thread Disabled" task) tst))


