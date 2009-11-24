implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> Task a

instance <<@ String
where	(<<@) (Task desc mbCxt tf) s	= Task {TaskDescription| desc & title = s} mbCxt tf

class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a
instance @>> String
where	(@>>) s (Task desc mbCxt tf)	= Task {TaskDescription| desc & title = s} mbCxt tf