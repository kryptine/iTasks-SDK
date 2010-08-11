definition module MenuTasks

from ProcessDB	import ::Menu, ::MenuItem, ::Hotkey{..}, ::Key(..)
from TSt		import ::Task
from Void		import :: Void
import StdMaybe, GenVisualize, GenUpdate

derive class iTask Menu, MenuItem, Hotkey

getMenus :: Task (Maybe [Menu])
setMenus :: ![Menu] -> Task Void
removeMenus :: Task Void
setMenuItem :: !String !MenuItem -> Task Void
getMenuItem :: !String -> Task (Maybe MenuItem)