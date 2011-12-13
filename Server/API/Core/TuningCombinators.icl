implementation module TuningCombinators

import SystemTypes, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime
from Util import kvSet

class tune b :: !b !(Task a) -> Task a
instance tune Description
where tune (Description t) task	= updateTaskMeta (\m -> {TaskMeta|m & title = toString t}) task
instance tune Icon
where tune (Icon i) task		= updateTaskMeta (\m -> {TaskMeta|m & icon = Just i}) task
instance tune Attribute
where tune (Attribute k v) task	= updateTaskMeta (\m -> {TaskMeta|m & attributes = kvSet k (toString v) m.attributes}) task
instance tune Attributes
where tune (Attributes as) task	= updateTaskMeta (\m -> {TaskMeta|m & attributes = foldr (\(k,v) as -> kvSet k (toString v) as) m.attributes as}) task
instance tune Hide
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & hide = True}) task
instance tune Window
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & window = True}) task
instance tune InteractionTaskType
where tune t task				= updateTaskMeta (\m -> {TaskMeta|m & interactionType = Just t}) task
instance tune LocalInteractionTask
where tune _ task				= updateTaskMeta (\m -> {TaskMeta|m & localInteraction = True}) task
instance tune InteractionLayouter
where tune l task				= {Task|task & layout = InteractionLayouter l}
instance tune StepLayouter
where tune l task				= {Task|task & layout = StepLayouter l}
instance tune ParallelLayouter
where tune l task				= {Task|task & layout = ParallelLayouter l}
instance tune LayoutTweak
where
	tune tweak task=:{Task|def} = {Task|task & def = applyTweak def}
	where
		applyTweak funcs = {funcs & evalFun = eval}
		where
			eval taskNr props event tuiTaskNr repInput context iworld
				# (res,iworld) = funcs.evalFun taskNr props event tuiTaskNr repInput context iworld
				= case res of
					(TaskInstable mbv (TUIRep tui, actions) context)
						# (tui,actions) = tweak (tui,actions)
						= (TaskInstable mbv (TUIRep tui, actions) context,iworld)
					_	
						= (res,iworld)
instance tune TUITweak
where tune tweak task = tune (tuiTweakConv tweak) task 

tuiTweakConv :: (TUIDef -> TUIDef) -> LayoutTweak
tuiTweakConv f = \(t,a) -> (f t,a)

instance tune ActionTweak
where tune tweak task = tune (actionTweakConv tweak) task

actionTweakConv :: ([TaskAction] -> [TaskAction]) -> LayoutTweak
actionTweakConv f = (\(t,a) -> (t,f a))

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

updateTaskMeta updF task=:{Task|meta} = {Task|task & meta = updF meta}