implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> Task a

instance <<@ String
where	(<<@) (Task _ mbCxt tf) name			= Task name mbCxt tf
instance <<@  TaskCombination
where	(<<@) (Task name mbCxt tf) combination	= Task name mbCxt (\tst -> tf (setNextCombination combination tst))

class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a
instance @>> String
where	(@>>) name (Task _ mbCxt tf)			= Task name mbCxt tf
instance @>> TaskCombination
where	(@>>) combination (Task name mbCxt tf)	= Task name mbCxt (\tst -> tf (setNextCombination combination tst))