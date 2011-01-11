implementation module TuningCombinators

import Types, StdList, StdMisc
from Time				import :: Timestamp, :: Tm(..), mkTime
from TaskTree			import :: GroupedBehaviour, :: GroupActionsBehaviour
from InteractionTasks	import class html(..)

class tune b :: !b !(Task a) -> Task a
instance tune ManagerProperties
where tune props task								= {task & taskProperties = props}
instance tune User
where tune u task=:{taskProperties}					= {task & taskProperties = {taskProperties & worker = u}}
instance tune Title
where tune (Title s) task=:{taskProperties}			= {task & taskProperties = {taskProperties & taskDescription = {TaskDescription|taskProperties.taskDescription & title = toString s}}}
instance tune Description
where tune (Description s) task=:{taskProperties}	= {task & taskProperties = {taskProperties & taskDescription = {TaskDescription|taskProperties.taskDescription & description = html s}}}
instance tune TaskPriority
where tune p task=:{taskProperties}					= {task & taskProperties = {taskProperties & priority = p}}
instance tune DateTime
where tune d task=:{taskProperties}					= {task & taskProperties = {taskProperties & deadline = Just d}}
instance tune Tag
where tune (Tag t) task=:{taskProperties}			= {task & taskProperties = {taskProperties & tags = [toString t : taskProperties.tags]}}
instance tune Tags
where tune (Tags ts) task=:{taskProperties}			= {task & taskProperties = {taskProperties & tags = (map toString ts) ++ taskProperties.tags}}
instance tune GroupedBehaviour
where tune gb task=:{groupedProperties}				= {task & groupedProperties = {groupedProperties & groupedBehaviour = gb}}
instance tune GroupActionsBehaviour
where tune ga task=:{groupedProperties}				= {task & groupedProperties = {groupedProperties & groupActionsBehaviour = ga}}
instance tune MenuAnnotation
where
	tune ma task									= {task & mbMenuGenFunc = (Just menuGenFunc)}
	where
		menuGenFunc = case ma of
			NoMenus							= \iworld -> ([], iworld)
			StaticMenus menus				= \iworld -> (menus, iworld)
			DynamicMenus (DBId refStr) genF	= dynamicMenus
			where
				dynamicMenus iworld
					# (mbV,iworld) = loadValue refStr iworld
					= case mbV of
						Just v	= (genF v,iworld)
						Nothing	= abort "Cannot dynamically generate menus! Stored value deleted!"
instance tune Menus
where tune menus task = tune (StaticMenus menus) task

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t
