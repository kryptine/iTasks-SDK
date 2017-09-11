definition module iTasks.UI.Editor.Containers
/**
* This module provides a set of editors for builtin controls
* of the client-side UI framework.
*
* To keep everything well-typed there are lots of boiler-plate versions to create the containers
*/
from iTasks.UI.Definition import :: UIAttributes
from iTasks.UI.Editor import :: Editor 
from Data.Map import :: Map
from Text.JSON import :: JSONNode

//# UIContainer
container  :: Editor ()
containerl :: (Editor a) -> Editor [a]
container1 :: (Editor a) -> Editor a
container2 :: (Editor a) (Editor b) -> Editor (a,b)
container3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
container4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
container5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIPanel
panel  :: Editor ()
panell :: (Editor a) -> Editor [a]
panel1 :: (Editor a) -> Editor a
panel2 :: (Editor a) (Editor b) -> Editor (a,b)
panel3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
panel4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
panel5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UITabSet
tabset  :: Editor ()
tabsetl :: (Editor a) -> Editor [a]
tabset1 :: (Editor a) -> Editor a
tabset2 :: (Editor a) (Editor b) -> Editor (a,b)
tabset3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
tabset4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
tabset5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIWindow
window  :: Editor ()
windowl :: (Editor a) -> Editor [a]
window1 :: (Editor a) -> Editor a
window2 :: (Editor a) (Editor b) -> Editor (a,b)
window3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
window4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
window5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIMenu
menu  :: Editor ()
menul :: (Editor a) -> Editor [a]
menu1 :: (Editor a) -> Editor a
menu2 :: (Editor a) (Editor b) -> Editor (a,b)
menu3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
menu4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
menu5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIToolBar
toolbar  :: Editor ()
toolbarl :: (Editor a) -> Editor [a]
toolbar1 :: (Editor a) -> Editor a
toolbar2 :: (Editor a) (Editor b) -> Editor (a,b)
toolbar3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
toolbar4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
toolbar5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIButtonBar
buttonbar  :: Editor ()
buttonbarl :: (Editor a) -> Editor [a]
buttonbar1 :: (Editor a) -> Editor a
buttonbar2 :: (Editor a) (Editor b) -> Editor (a,b)
buttonbar3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
buttonbar4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
buttonbar5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIList
list  :: Editor ()
listl :: (Editor a) -> Editor [a]
list1 :: (Editor a) -> Editor a
list2 :: (Editor a) (Editor b) -> Editor (a,b)
list3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
list4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
list5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIListItem
listitem  :: Editor ()
listiteml :: (Editor a) -> Editor [a]
listitem1 :: (Editor a) -> Editor a
listitem2 :: (Editor a) (Editor b) -> Editor (a,b)
listitem3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
listitem4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
listitem5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIDebug
debug  :: Editor ()
debugl :: (Editor a) -> Editor [a]
debug1 :: (Editor a) -> Editor a
debug2 :: (Editor a) (Editor b) -> Editor (a,b)
debug3 :: (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
debug4 :: (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
debug5 :: (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

