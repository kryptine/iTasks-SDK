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
from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode

//# UIContainer
container  :: Editor () ()
containerl :: !(Editor a w) -> Editor [a] [w]
containerL :: ![Editor a w] -> Editor [a] [w]
container1 :: !(Editor a w) -> Editor a w
container2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
container3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
container4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
container5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
containerc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIPanel
panel  :: Editor () ()
panell :: !(Editor a w) -> Editor [a] [w]
panelL :: ![Editor a w] -> Editor [a] [w]
panel1 :: !(Editor a w) -> Editor a w
panel2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
panel3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
panel4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
panel5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
panelc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UITabSet
tabset  :: Editor () ()
tabsetl :: !(Editor a w) -> Editor [a] [w]
tabsetL :: ![Editor a w] -> Editor [a] [w]
tabset1 :: !(Editor a w) -> Editor a w
tabset2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
tabset3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
tabset4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
tabset5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
tabsetc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) w

//# UIWindow
window  :: Editor () ()
windowl :: !(Editor a w) -> Editor [a] [w]
windowL :: ![Editor a w] -> Editor [a] [w]
window1 :: !(Editor a w) -> Editor a w
window2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
window3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
window4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
window5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
windowc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) w

//# UIMenu
menu  :: Editor () ()
menul :: !(Editor a w) -> Editor [a] [w]
menuL :: ![Editor a w] -> Editor [a] [w]
menu1 :: !(Editor a w) -> Editor a w
menu2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
menu3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
menu4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
menu5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
menuc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIToolBar
toolbar  :: Editor () ()
toolbarl :: !(Editor a w) -> Editor [a] [w]
toolbarL :: ![Editor a w] -> Editor [a] [w]
toolbar1 :: !(Editor a w) -> Editor a w
toolbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
toolbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
toolbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
toolbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
toolbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIButtonBar
buttonbar  :: Editor () ()
buttonbarl :: !(Editor a w) -> Editor [a] [w]
buttonbarL :: ![Editor a w] -> Editor [a] [w]
buttonbar1 :: !(Editor a w) -> Editor a w
buttonbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
buttonbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
buttonbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
buttonbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
buttonbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIList
list  :: Editor () ()
listl :: !(Editor a w) -> Editor [a] [w]
listL :: ![Editor a w] -> Editor [a] [w]
list1 :: !(Editor a w) -> Editor a w
list2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
list3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
list4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
list5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
listc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIListItem
listitem  :: Editor () ()
listiteml :: !(Editor a w) -> Editor [a] [w]
listitemL :: ![Editor a w] -> Editor [a] [w]
listitem1 :: !(Editor a w) -> Editor a w
listitem2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
listitem3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
listitem4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
listitem5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
listitemc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w

//# UIDebug
debug  :: Editor () ()
debugl :: !(Editor a w) -> Editor [a] [w]
debugL :: ![Editor a w] -> Editor [a] [w]
debug1 :: !(Editor a w) -> Editor a w
debug2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (wa,wb)
debug3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (wa,wb,wc)
debug4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (wa,wb,wc,wd)
debug5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (wa,wb,wc,wd,we)
debugc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) w
