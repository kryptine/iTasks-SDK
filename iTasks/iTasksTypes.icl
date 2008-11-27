implementation module iTasksTypes

derive gForm 	Void, []
derive gUpd 	Void, []
derive gParse 	Void
derive gPrint	Void
derive gerda 	Void
derive read 	Void
derive write 	Void


import StdEnv
import InternaliTasksCommon, InternaliTasksThreadHandling, iTasksBasicCombinators
import GenBimap


// ******************************************************************************************************
// Overloaded Functions on Tasks
// ******************************************************************************************************

class 	(<<@) infixl 3 b ::  !(Task a) !b  -> (Task a)
instance <<@  Lifespan
where   (<<@) task lifespan			= setTaskLifespan
		where
			setTaskLifespan tst=:{options}
			
			= IF_Ajax 
				(IF_ClientServer															// we running both client and server
					(IF_ClientTasks												
						(if (options.tasklife == LSClient && (lifespan == LSTxtFile || lifespan == LSDataFile || lifespan == LSDatabase))
							(abort "Cannot make persistent storage on Client\n")
							(\tst -> task {tst & options.tasklife = lifespan}))						// assign option on client
						(\tst -> task {tst & options.tasklife = lifespan})tst							// assign option on server
					)
					(task {tst & options.tasklife = lifespan})								// assign option on server
				)
				(task {tst & options.tasklife = lifespan}) 									// assign option on server

instance <<@  StorageFormat
where   (<<@) task storageformat 	= \tst -> task {tst & options.taskstorage = storageformat}
instance <<@  Mode
where   (<<@) task mode 			= \tst -> task {tst & options.taskmode = mode}
instance <<@  GarbageCollect
where   (<<@) task gc 				= \tst -> task {tst & options.gc = gc}

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  EvaluationOption
where   (@>>) UseAjax task			= \tst -> IF_Ajax 
												(mkTaskThread UseAjax task tst)
												(newTask "Ajax Thread Disabled" task tst) 
		(@>>) OnClient  task 		= \tst -> IF_Ajax 
												(mkTaskThread OnClient task tst)
												(newTask "Client Thread Disabled" task tst) 


