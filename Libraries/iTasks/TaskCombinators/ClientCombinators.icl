implementation module ClientCombinators

import TSt, BasicCombinators, CommonCombinators

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a
instance @>>  EvaluationOption
where
		(@>>) OnClient  task 		= task /*Task (\tst -> IF_Ajax 
												(accTaskTSt (mkTaskThread OnClient task) tst)
												(accTaskTSt (newTask "Client Thread Disabled" task) tst))
										*/