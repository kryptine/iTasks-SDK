implementation module TuningCombinators

import Types, StdList
from Time		import :: Timestamp, :: Tm(..), mkTime
from TaskTree	import :: GroupedBehaviour, :: GroupActionsBehaviour

class tune b :: !b !(Task a) -> Task a
instance tune ManagerProperties
where tune props (Task _ gprops mbTn tf)				= Task props gprops mbTn tf
instance tune User
where tune u (Task props gprops mbTn tf)				= Task {ManagerProperties|props & worker = u} gprops mbTn tf
instance tune (Subject s)
where tune (Subject s) (Task props gprops mbTn tf)		= Task {ManagerProperties|props & subject = toString s} gprops mbTn tf
instance tune (Description s)
where tune (Description s) (Task props gprops mbTn tf)	= Task {ManagerProperties|props & description = toString s} gprops mbTn tf
instance tune TaskPriority
where tune p (Task props gprops mbTn tf)				= Task {ManagerProperties|props & priority = p} gprops mbTn tf
instance tune Timestamp
where tune d (Task props gprops mbTn tf)				= Task {ManagerProperties|props & deadline = Just d} gprops mbTn tf
instance tune DateTime
where tune d (Task props gprops mbTn tf)				= Task {ManagerProperties|props & deadline = Just (dt2ts d)} gprops mbTn tf
instance tune (Tag s)
where tune (Tag t) (Task props gprops mbTn tf)			= Task {ManagerProperties|props & tags = [toString t : props.tags]} gprops mbTn tf
instance tune (Tags s)
where tune (Tags ts) (Task props gprops mbTn tf)		= Task {ManagerProperties|props & tags = (map toString ts) ++ props.tags} gprops mbTn tf
instance tune GroupedBehaviour
where tune gb (Task props gprops mbTn tf)				= Task props {gprops & groupedBehaviour = gb} mbTn tf
instance tune GroupActionsBehaviour
where tune ga (Task props gprops mbTn tf)				= Task props {gprops & groupActionsBehaviour = ga} mbTn tf

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t


dt2ts :: DateTime -> Timestamp
dt2ts (DateTime date time) = mkTime tm
where
	tm = {Tm
		 | sec = time.Time.sec
	     , min = time.Time.min
	     , hour = time.Time.hour
	     , mday = date.Date.day
	     , mon = date.Date.mon - 1
	     , year = date.Date.year - 1900
	     , wday = -1
	     , yday = -1
	     , isdst = True
	     }