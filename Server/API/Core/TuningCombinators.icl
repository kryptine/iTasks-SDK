implementation module TuningCombinators

import Types, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune Title
where tune (Title s) task		= updateTaskProperties (\p -> {p & taskDescription = {TaskDescription|p.taskDescription & title = toString s}}) task
instance tune Description
where tune (Description s) task	= updateTaskProperties (\p -> {p & taskDescription = {TaskDescription|p.taskDescription & description = toString (html s)}}) task
instance tune Tag
where tune (Tag t) task			= updateTaskProperties (\p -> {p & tags = [toString t : p.tags]}) task
instance tune Tags
where tune (Tags ts) task		= updateTaskProperties (\p -> {p & tags = (map toString ts) ++ p.tags}) task
instance tune InteractionTaskType
where tune t task				= updateTaskProperties (\p -> {p & interactionType = Just t}) task
instance tune LocalInteractionTask
where tune _ task				= updateTaskProperties (\p -> {TaskProperties|p & localInteraction = True}) task
instance tune InteractionLayoutMerger
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\dict termF -> changeLayout (actionF dict termF))}
	where	
		changeLayout funcs = {funcs & evalTaskFun = \taskNr event tuiTaskNr _ pmerge context iworld -> funcs.evalTaskFun taskNr event tuiTaskNr l pmerge context iworld}
instance tune ParallelLayoutMerger
where
	tune l task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (changeLayout funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\dict termF -> changeLayout (actionF dict termF))}
	where	
		changeLayout funcs = {funcs & evalTaskFun = \taskNr event tuiTaskNr imerge _ context iworld -> funcs.evalTaskFun taskNr event tuiTaskNr imerge l context iworld}
instance tune ResultLayoutMerger
where tune l task				= task

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

updateTaskProperties updF task = let p = taskProperties task in {Task|task & properties = updF p}