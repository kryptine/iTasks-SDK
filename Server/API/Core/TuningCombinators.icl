implementation module TuningCombinators

import SystemTypes, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune Description
where tune (Description d) task	= updateTaskProperties (\p -> {p & taskDescription = toDescr d}) task
instance tune Tag
where tune (Tag t) task			= updateTaskProperties (\p -> {p & tags = [toString t : p.tags]}) task
instance tune Tags
where tune (Tags ts) task		= updateTaskProperties (\p -> {p & tags = (map toString ts) ++ p.tags}) task
instance tune InteractionTaskType
where tune t task				= updateTaskProperties (\p -> {p & interactionType = Just t}) task
instance tune LocalInteractionTask
where tune _ task				= updateTaskProperties (\p -> {TaskProperties|p & localInteraction = True}) task
instance tune ControlTask
where tune _ task				= updateTaskProperties (\p -> {TaskProperties|p & controlTask = True}) task
instance tune InteractionLayouter
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\termF -> changeLayout (actionF termF))}
	where	
		changeLayout funcs = {funcs & evalTaskFun = \taskNr props event tuiTaskNr _ pmerge context iworld -> funcs.evalTaskFun taskNr props event tuiTaskNr l pmerge context iworld}
instance tune ParallelLayouter
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\termF -> changeLayout (actionF termF))}
	where	
		changeLayout funcs = {funcs & evalTaskFun = \taskNr props event tuiTaskNr imerge _ context iworld -> funcs.evalTaskFun taskNr props event tuiTaskNr imerge l context iworld}

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

updateTaskProperties updF task = let p = taskProperties task in {Task|task & properties = updF p}