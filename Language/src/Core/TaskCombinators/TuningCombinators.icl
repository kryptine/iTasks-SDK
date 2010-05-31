implementation module TuningCombinators

import StdEnv
import Types, TSt

class 	(<<@) infixl 2 b ::  !(Task a) !b  -> Task a

instance <<@ ManagerProperties
where	(<<@) (Task _ gb ga mbTn tf) props	= Task props gb ga mbTn tf
instance <<@ UserName
where	(<<@) (Task props gb ga mbTn tf) u	= Task {ManagerProperties|props & worker = u} gb ga mbTn tf
instance <<@ String
where	(<<@) (Task props gb ga mbTn tf) s	= Task {ManagerProperties|props & subject = s} gb ga mbTn tf
instance <<@ TaskPriority
where	(<<@) (Task props gb ga mbTn tf) p	= Task {ManagerProperties|props & priority = p} gb ga mbTn tf
instance <<@ Timestamp
where	(<<@) (Task props gb ga mbTn tf) d	= Task {ManagerProperties|props & deadline = Just d} gb ga mbTn tf

instance <<@ GroupedBehaviour
where	(<<@) (Task props _ ga mbTn tf) gb	= Task props gb ga mbTn tf
instance <<@ GroupActionsBehaviour
where	(<<@) (Task props gb _ mbTn tf) ga	= Task props gb ga mbTn tf


class 	(@>>) infixr 2 b ::  !b !(Task a)   -> Task a

instance @>> ManagerProperties
where	(@>>) props (Task _ gb ga mbTn tf)	= Task props gb ga mbTn tf
instance @>> UserName
where	(@>>) u (Task props gb ga mbTn tf)	= Task {ManagerProperties|props & worker = u} gb ga mbTn tf
instance @>> String
where	(@>>) s (Task props gb ga mbTn tf)	= Task {ManagerProperties|props & subject = s} gb ga mbTn tf
instance @>> TaskPriority
where	(@>>) p (Task props gb ga mbTn tf)	= Task {ManagerProperties|props & priority = p} gb ga mbTn tf
instance @>> Timestamp
where	(@>>) d (Task props gb ga mbTn tf)	= Task {ManagerProperties|props & deadline = Just d} gb ga mbTn tf

instance @>> GroupedBehaviour
where	(@>>) gb (Task props _ ga mbTn tf) = Task props gb ga mbTn tf
instance @>> GroupActionsBehaviour
where	(@>>) ga (Task props gb _ mbTn tf) = Task props gb ga mbTn tf
