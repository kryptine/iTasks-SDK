implementation module TuningCombinators

import Types, StdList, StdMisc, Shared, HTML, Task
from Time import :: Timestamp, :: Tm(..), mkTime

class tune b :: !b !(Task a) -> Task a
instance tune ManagerProperties
where tune props task								= {task & taskProperties = props}
instance tune User
where tune u task=:{taskProperties}					= {task & taskProperties = {taskProperties & worker = u}}
instance tune Title
where tune (Title s) task=:{taskProperties}			= {task & taskProperties = {taskProperties & taskDescription = {TaskDescription|taskProperties.taskDescription & title = toString s}}}
instance tune Description
where tune (Description s) task=:{taskProperties}	= {task & taskProperties = {taskProperties & taskDescription = {TaskDescription|taskProperties.taskDescription & description = toString (html s)}}}
instance tune TaskPriority
where tune p task=:{taskProperties}					= {task & taskProperties = {taskProperties & priority = p}}
instance tune DateTime
where tune d task=:{taskProperties}					= {task & taskProperties = {taskProperties & deadline = Just d}}
instance tune Tag
where tune (Tag t) task=:{taskProperties}			= {task & taskProperties = {taskProperties & tags = [toString t : taskProperties.tags]}}
instance tune Tags
where tune (Tags ts) task=:{taskProperties}			= {task & taskProperties = {taskProperties & tags = (map toString ts) ++ taskProperties.tags}}
instance tune MenuAnnotation
where
	tune ma task									= {task & mbMenuGenFunc = (Just menuGenFunc)}
	where
		menuGenFunc = case ma of
			NoMenus						= \iworld -> ([], iworld)
			StaticMenus menus			= \iworld -> (menus, iworld)
			DynamicMenus shared genF	= dynamicMenus
			where
				dynamicMenus iworld
					# (v,iworld) = readShared shared iworld
					= case v of
						Ok v	= (genF v,iworld)
						Error _	= ([],iworld) // empty menus on error
					
instance tune MenuDefinition
where tune menus task = tune (StaticMenus menus) task
instance tune FormWidth
where tune fw task=:{taskProperties}				= {task & formWidth = Just fw}

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t
