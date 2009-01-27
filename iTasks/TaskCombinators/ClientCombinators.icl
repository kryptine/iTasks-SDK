implementation module ClientCombinators

import TSt, BasicCombinators, InternaliTasksThreadHandling

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  EvaluationOption
where   (@>>) UseAjax task			= Task (\tst -> IF_Ajax 
												(accTaskTSt (mkTaskThread UseAjax task) tst)
												(accTaskTSt (newTask "Ajax Thread Disabled" task) tst))
		(@>>) OnClient  task 		= Task (\tst -> IF_Ajax 
												(accTaskTSt (mkTaskThread OnClient task) tst)
												(accTaskTSt (newTask "Client Thread Disabled" task) tst))