definition module MenuTasks
/*
* This module provides tasks to handle Menus
*/
from ProcessDB	import ::Menu, ::MenuItem, ::Hotkey{..}, ::Key(..)
from TSt		import ::Task
from Void		import :: Void
import StdMaybe, GenVisualize, GenUpdate

derive class iTask Menu, MenuItem, Hotkey

/*
* Access the menus set for the current process.
*
* @return (Maybe [Menu])		The current set of menus, if any.
*/
getMenus :: Task (Maybe [Menu])

/*
* Set the menus for the current process.
*
* @param [Menu]					The new set of menus
*
* @return Void
*/
setMenus :: ![Menu] -> Task Void

/*
* Removes all menus from the current process
*
* @return Void
*/
removeMenus :: Task Void

/*
* Overwrite a specific item in the current menus
*
* @param String					The label of the item which should be replaced
* @param MenuItem				The replacement item
*
* @return Void
*/
setMenuItem :: !String !MenuItem -> Task Void

/*
* Retrieves a specific item from the current menus
*
* @param String					The label of the item which should be retrieved
*
* @return (Maybe MenuItem)		The menu-item, if it exists
*/
getMenuItem :: !String -> Task (Maybe MenuItem)