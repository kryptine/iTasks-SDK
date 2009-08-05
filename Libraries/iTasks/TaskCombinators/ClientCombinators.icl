implementation module ClientCombinators

import TSt

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iTask a
instance @>>  EvaluationOption
where
		(@>>) OnClient  task 		= task 