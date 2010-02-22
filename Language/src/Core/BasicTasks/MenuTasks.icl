implementation module MenuTasks

from ProcessDB	import qualified class ProcessDB(..)
from ProcessDB	import qualified instance ProcessDB TSt
from ProcessDB	import ::Menu(..), ::MenuItem(..), ::Process{..}
from TSt		import ::Task
from Void		import :: Void
import TSt, CoreCombinators, StdMisc
import StdList

derive gParse Menu, MenuItem
derive gPrint Menu, MenuItem
derive gVisualize Menu, MenuItem
derive gUpdate Menu, MenuItem

derive bimap (,), Maybe

getMenus :: Task (Maybe [Menu])
getMenus
	= mkInstantTask "getMenus" getMenus`
where
	getMenus` tst
		#(pid, tst) = getCurrentProcess tst
		#(p, tst) = ProcessDB@getProcess pid tst
		= case p of
			Just p	= (TaskFinished p.menus, tst)
			Nothing	= abort "Cannot get current process!"

setMenus :: ![Menu] -> Task Void
setMenus menus
	= mkInstantTask "setMenus" (setMenus` (Just menus))
	
		
removeMenus :: Task Void
removeMenus = mkInstantTask "removeMenus" (setMenus` Nothing)

setMenus` :: !(Maybe [Menu]) !*TSt -> (!TaskResult Void,!*TSt) 		
setMenus` menus tst
		#(pid, tst) = getCurrentProcess tst
		#(_, tst) = ProcessDB@updateProcess pid (\p -> {p & menus = menus}) tst
		= (TaskFinished Void, tst)

setMenuItem :: !String !MenuItem -> Task Void
setMenuItem updName newItem =
				getMenus
	>>= \menus.	case menus of
					Nothing		= return Void
					Just menus	= setMenus (updateMenus menus)
where
	updateMenus menus = map updateMenu menus
	updateMenu (Menu label items) = Menu label (map updateItem items)
	updateItem (SubMenu label items) = SubMenu label (map updateItem items)
	updateItem (MenuName name item)
		| name == updName	= MenuName name newItem
		| otherwise			= MenuName name (updateItem item)
	updateItem item = item
	
getMenuItem :: !String -> Task (Maybe MenuItem)
getMenuItem findName =
				getMenus
	>>= \menus.	case menus of
					Nothing		= return Nothing
					Just menus	= return (searchMenus menus)
where
	searchMenus [Menu _ items:menus] = case searchItems items of
		Nothing	= searchMenus menus
		res		= res
	searchMenus [] = Nothing
	searchItems [item:items] = case searchItem item of
		Nothing	= searchItems items
		res		= res
	searchItems [] = Nothing
	searchItem (SubMenu _ items) = searchItems items
	searchItem (MenuName name item)
		| name == findName	= Just item
		| otherwise			= Nothing
	searchItem _ = Nothing