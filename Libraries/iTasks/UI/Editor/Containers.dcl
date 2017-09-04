definition module iTasks.UI.Editor.Containers
/**
* Editor combinators for the builtin containers
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

