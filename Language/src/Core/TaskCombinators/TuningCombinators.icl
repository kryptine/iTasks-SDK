implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> Task a

instance <<@ String
where	(<<@) (Task desc mbCxt tf) s	= Task {TaskDescription| desc & title = s} mbCxt tf
instance <<@ TaskDescription
where	(<<@) (Task _ mbCxt tf) td		= Task td mbCxt tf	

class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a
instance @>> String
where	(@>>) s (Task desc mbCxt tf)	= Task {TaskDescription| desc & title = s} mbCxt tf
instance @>> TaskDescription
where	(@>>) td (Task _ mbCxt tf)		= Task td mbCxt tf