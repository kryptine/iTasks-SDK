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
container  :: UIAttributes -> Editor ()
containerl :: UIAttributes (Editor a) -> Editor [a]
container1 :: UIAttributes (Editor a) -> Editor a
container2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
container3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
container4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
container5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIPanel
panel  :: UIAttributes -> Editor ()
panell :: UIAttributes (Editor a) -> Editor [a]
panel1 :: UIAttributes (Editor a) -> Editor a
panel2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
panel3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
panel4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
panel5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UITabSet
tabset  :: UIAttributes -> Editor ()
tabsetl :: UIAttributes (Editor a) -> Editor [a]
tabset1 :: UIAttributes (Editor a) -> Editor a
tabset2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
tabset3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
tabset4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
tabset5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIWindow
window  :: UIAttributes -> Editor ()
windowl :: UIAttributes (Editor a) -> Editor [a]
window1 :: UIAttributes (Editor a) -> Editor a
window2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
window3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
window4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
window5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIMenu
menu  :: UIAttributes -> Editor ()
menul :: UIAttributes (Editor a) -> Editor [a]
menu1 :: UIAttributes (Editor a) -> Editor a
menu2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
menu3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
menu4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
menu5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIToolBar
toolbar  :: UIAttributes -> Editor ()
toolbarl :: UIAttributes (Editor a) -> Editor [a]
toolbar1 :: UIAttributes (Editor a) -> Editor a
toolbar2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
toolbar3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
toolbar4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
toolbar5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIButtonBar
buttonbar  :: UIAttributes -> Editor ()
buttonbarl :: UIAttributes (Editor a) -> Editor [a]
buttonbar1 :: UIAttributes (Editor a) -> Editor a
buttonbar2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
buttonbar3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
buttonbar4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
buttonbar5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIList
list  :: UIAttributes -> Editor ()
listl :: UIAttributes (Editor a) -> Editor [a]
list1 :: UIAttributes (Editor a) -> Editor a
list2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
list3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
list4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
list5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIListItem
listitem  :: UIAttributes -> Editor ()
listiteml :: UIAttributes (Editor a) -> Editor [a]
listitem1 :: UIAttributes (Editor a) -> Editor a
listitem2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
listitem3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
listitem4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
listitem5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

//# UIDebug
debug  :: UIAttributes -> Editor ()
debugl :: UIAttributes (Editor a) -> Editor [a]
debug1 :: UIAttributes (Editor a) -> Editor a
debug2 :: UIAttributes (Editor a) (Editor b) -> Editor (a,b)
debug3 :: UIAttributes (Editor a) (Editor b) (Editor c) -> Editor (a,b,c)
debug4 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) -> Editor (a,b,c,d)
debug5 :: UIAttributes (Editor a) (Editor b) (Editor c) (Editor d) (Editor e) -> Editor (a,b,c,d,e)

