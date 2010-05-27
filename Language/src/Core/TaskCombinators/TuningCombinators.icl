implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> Task a

instance <<@ ManagerProperties
where	(<<@) (Task _ gb mbTn tf) props	= Task props gb mbTn tf
instance <<@ UserName
where	(<<@) (Task props gb mbTn tf) u	= Task {ManagerProperties|props & worker = u} gb mbTn tf
instance <<@ String
where	(<<@) (Task props gb mbTn tf) s	= Task {ManagerProperties|props & subject = s} gb mbTn tf
instance <<@ TaskPriority
where	(<<@) (Task props gb mbTn tf) p	= Task {ManagerProperties|props & priority = p} gb mbTn tf
instance <<@ Timestamp
where	(<<@) (Task props gb mbTn tf) d	= Task {ManagerProperties|props & deadline = Just d} gb mbTn tf

instance <<@ GroupedBehaviour
where	(<<@) (Task props _ mbTn tf) gb	= Task props gb mbTn tf


class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a

instance @>> ManagerProperties
where	(@>>) props (Task _ gb mbTn tf)	= Task props gb mbTn tf
instance @>> UserName
where	(@>>) u (Task props gb mbTn tf)	= Task {ManagerProperties|props & worker = u} gb mbTn tf
instance @>> String
where	(@>>) s (Task props gb mbTn tf)	= Task {ManagerProperties|props & subject = s} gb mbTn tf
instance @>> TaskPriority
where	(@>>) p (Task props gb mbTn tf)	= Task {ManagerProperties|props & priority = p} gb mbTn tf
instance @>> Timestamp
where	(@>>) d (Task props gb mbTn tf)	= Task {ManagerProperties|props & deadline = Just d} gb mbTn tf

instance @>> GroupedBehaviour
where	(@>>) gb (Task props _ mbTn tf) = Task props gb mbTn tf
