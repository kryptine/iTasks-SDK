definition module iTasks.WF.Combinators.Tune

import iTasks.WF.Definition
from iTasks.UI.Layout import :: Layout
from Text.JSON import :: JSONNode

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
*/
class tune b    :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

//*  Fine tune evaluation behaviour
:: LazyRefresh = LazyRefresh 
instance tune	LazyRefresh

//* Apply a layout to a task
:: ApplyLayout	= ApplyLayout Layout
instance tune	ApplyLayout 

:: ApplyAttribute a = ApplyAttribute String a

class toAttribute a where toAttribute :: a -> JSONNode
instance toAttribute String

instance tune (ApplyAttribute a) | toAttribute a
