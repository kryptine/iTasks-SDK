implementation module TuningCombinators

import Types, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune Title
where tune (Title s) task		= let p = taskProperties task in {Task|task & properties = {p & taskDescription = {TaskDescription|p.taskDescription & title = toString s}}}
instance tune Description
where tune (Description s) task	= let p = taskProperties task in {Task|task & properties = {p & taskDescription = {TaskDescription|p.taskDescription & description = toString (html s)}}}
instance tune Tag
where tune (Tag t) task			= let p = taskProperties task in {Task|task & properties = {p & tags = [toString t : p.tags]}}
instance tune Tags
where tune (Tags ts) task		= let p = taskProperties task in {Task|task & properties = {p & tags = (map toString ts) ++ p.tags}}
instance tune InteractiveLayoutMerger
where tune l task				= {task & mbInteractiveLayout = Just l}
instance tune ParallelLayoutMerger
where tune l task				= {task & mbParallelLayout = Just l}
instance tune ResultLayoutMerger
where tune l task				= {task & mbResultLayout = Just l}

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t
