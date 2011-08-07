implementation module TuningCombinators

import SystemTypes, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune Description
where tune (Description t) task	= updateTaskMeta (\p -> {TaskMeta|p & title = toString t}) task
instance tune Tag
where tune (Tag t) task			= updateTaskMeta (\p -> {TaskMeta|p & tags = [toString t : p.tags]}) task
instance tune Tags
where tune (Tags ts) task		= updateTaskMeta (\p -> {TaskMeta|p & tags = (map toString ts) ++ p.tags}) task
instance tune InteractionTaskType
where tune t task				= updateTaskMeta (\p -> {TaskMeta|p & interactionType = Just t}) task
instance tune LocalInteractionTask
where tune _ task				= updateTaskMeta (\p -> {TaskMeta|p & localInteraction = True}) task
instance tune ControlTask
where tune _ task				= updateTaskMeta (\p -> {TaskMeta|p & controlTask = True}) task
instance tune InteractionLayouter
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\termF -> changeLayout (actionF termF))}
	where	
		changeLayout funcs = {funcs & evalFun = \taskNr props event tuiTaskNr _ pmerge context iworld -> funcs.evalFun taskNr props event tuiTaskNr l pmerge context iworld}
instance tune ParallelLayouter
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\termF -> changeLayout (actionF termF))}
	where	
		changeLayout funcs = {funcs & evalFun = \taskNr props event tuiTaskNr imerge _ context iworld -> funcs.evalFun taskNr props event tuiTaskNr imerge l context iworld}

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

updateTaskMeta updF task = let p = taskProperties task in {Task|task & properties = updF p}