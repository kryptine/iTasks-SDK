definition module MenuTasks

from ProcessDB	import ::Menu, ::MenuItem
from TSt		import ::Task
from Void		import :: Void
import StdMaybe, GenPrint, GenParse, GenVisualize, GenUpdate

derive gParse Menu, MenuItem
derive gPrint Menu, MenuItem
derive gVisualize Menu, MenuItem
derive gUpdate Menu, MenuItem

getMenus :: Task (Maybe [Menu])
setMenus :: ![Menu] -> Task Void
removeMenus :: Task Void
setMenuItem :: !String !MenuItem -> Task Void
getMenuItem :: !String -> Task (Maybe MenuItem)