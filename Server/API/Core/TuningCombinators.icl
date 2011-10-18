implementation module TuningCombinators

import SystemTypes, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune Description
where tune (Description t) task	= updateTaskMeta (\m -> {TaskMeta|m & title = toString t}) task
instance tune Icon
where tune (Icon i) task		= updateTaskMeta (\m -> {TaskMeta|m & icon = Just i}) task
instance tune Tag
where tune (Tag t) task			= updateTaskMeta (\m -> {TaskMeta|m & tags = [toString t : m.tags]}) task
instance tune Tags
where tune (Tags ts) task		= updateTaskMeta (\m -> {TaskMeta|m & tags = (map toString ts) ++ m.tags}) task
instance tune Hide
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & hide = True}) task
instance tune Window
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & window = True}) task
instance tune InteractionTaskType
where tune t task				= updateTaskMeta (\m -> {TaskMeta|m & interactionType = Just t}) task
instance tune LocalInteractionTask
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & localInteraction = True}) task
instance tune ControlTask
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & controlTask = True}) task
instance tune InteractionLayouter
where tune l task				= {Task|task & layout = Just (Left l)}
instance tune ParallelLayouter
where tune l task				= {Task|task & layout = Just (Right l)}
instance tune LayoutTweak
where
	tune tweak task=:{Task|type} = case type of
		NormalTask funcs	= {Task|task & type = NormalTask (applyTweak funcs)}
		ActionTask actionF	= {Task|task & type = ActionTask (\termF -> applyTweak (actionF termF))}
	where
		applyTweak funcs = {funcs & evalFun = eval}
		where
			eval taskNr props event tuiTaskNr ilayout playout context iworld
				# (res,iworld) = funcs.evalFun taskNr props event tuiTaskNr ilayout playout context iworld
				= case res of
					(TaskBusy (Just tui) actions context)
						# (tui,actions) = tweak (tui,actions)
						= (TaskBusy (Just tui) actions context,iworld)
					_	
						= (res,iworld)
		
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

updateTaskMeta updF task = let p = taskProperties task in {Task|task & properties = updF p}