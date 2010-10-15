implementation module TuningCombinators

import Types, StdList, StdMisc
from Time		import :: Timestamp, :: Tm(..), mkTime
from TaskTree	import :: GroupedBehaviour, :: GroupActionsBehaviour

class tune b :: !b !(Task a) -> Task a
instance tune ManagerProperties
where tune props (Task _ gprops mbTn mbMGF tf)					= Task props gprops mbTn mbMGF tf
instance tune User
where tune u (Task props gprops mbTn mbMGF tf)					= Task {ManagerProperties|props & worker = u} gprops mbTn mbMGF tf
instance tune (Subject s)
where tune (Subject s) (Task props gprops mbTn mbMGF tf)		= Task {ManagerProperties|props & subject = toString s} gprops mbTn mbMGF tf
instance tune (Description s)
where tune (Description s) (Task props gprops mbTn mbMGF tf)	= Task {ManagerProperties|props & description = toString s} gprops mbTn mbMGF tf
instance tune TaskPriority
where tune p (Task props gprops mbTn mbMGF tf)					= Task {ManagerProperties|props & priority = p} gprops mbTn mbMGF tf
instance tune DateTime
where tune d (Task props gprops mbTn mbMGF tf)					= Task {ManagerProperties|props & deadline = Just d} gprops mbTn mbMGF tf
instance tune (Tag s)
where tune (Tag t) (Task props gprops mbTn mbMGF tf)			= Task {ManagerProperties|props & tags = [toString t : props.tags]} gprops mbTn mbMGF tf
instance tune (Tags s)
where tune (Tags ts) (Task props gprops mbTn mbMGF tf)			= Task {ManagerProperties|props & tags = (map toString ts) ++ props.tags} gprops mbTn mbMGF tf
instance tune GroupedBehaviour
where tune gb (Task props gprops mbTn mbMGF tf)					= Task props {gprops & groupedBehaviour = gb} mbTn mbMGF tf
instance tune GroupActionsBehaviour
where tune ga (Task props gprops mbTn mbMGF tf)					= Task props {gprops & groupActionsBehaviour = ga} mbTn mbMGF tf
instance tune (MenuAnnotation s)
where
	tune ma (Task props gprops mbTn _ tf)						= Task props gprops mbTn (Just menuGenFunc) tf
	where
		menuGenFunc = case ma of
			NoMenus							= \iworld -> ([], iworld)
			StaticMenus menus				= \iworld -> (menus, iworld)
			DynamicMenus (DBId refStr) genF	= dynamicMenus
			where
				dynamicMenus iworld=:{world, store}
					# (mbV, store, world) = loadValue refStr store world
					= case mbV of
						Just v	= (genF v, {iworld & store = store, world = world})
						Nothing	= abort "Cannot dynamically generate menus! Stored value deleted!"
instance tune Menus
where tune menus task = tune (StaticMenus menus) task

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t
