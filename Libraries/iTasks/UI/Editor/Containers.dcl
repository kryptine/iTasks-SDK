definition module iTasks.UI.Editor.Containers
/**
* This module provides a set of editors for builtin controls
* of the client-side UI framework.
*
* To keep everything well-typed there are lots of boiler-plate versions to create the containers
*/
from iTasks.UI.Definition import :: UIAttributes
from iTasks.UI.Editor import :: Editor 
from Data.Either import :: Either
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode

//# UIContainer
container  :: Editor () ()
containerl :: !(Editor a w) -> Editor [a] [(Int,w)]
containerL :: ![Editor a w] -> Editor [a] [(Int,w)]
container1 :: !(Editor a w) -> Editor a w
container2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
container3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
container4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
container5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
containerc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIPanel
panel  :: Editor () ()
panell :: !(Editor a w) -> Editor [a] [(Int,w)]
panelL :: ![Editor a w] -> Editor [a] [(Int,w)]
panel1 :: !(Editor a w) -> Editor a w
panel2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
panel3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
panel4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
panel5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
panelc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UITabSet
tabset  :: Editor () ()
tabsetl :: !(Editor a w) -> Editor [a] [(Int,w)]
tabsetL :: ![Editor a w] -> Editor [a] [(Int,w)]
tabset1 :: !(Editor a w) -> Editor a w
tabset2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
tabset3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
tabset4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
tabset5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
tabsetc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)

//# UIWindow
window  :: Editor () ()
windowl :: !(Editor a w) -> Editor [a] [(Int,w)]
windowL :: ![Editor a w] -> Editor [a] [(Int,w)]
window1 :: !(Editor a w) -> Editor a w
window2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
window3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
window4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
window5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
windowc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)

//# UIMenu
menu  :: Editor () ()
menul :: !(Editor a w) -> Editor [a] [(Int,w)]
menuL :: ![Editor a w] -> Editor [a] [(Int,w)]
menu1 :: !(Editor a w) -> Editor a w
menu2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
menu3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
menu4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
menu5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
menuc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIToolBar
toolbar  :: Editor () ()
toolbarl :: !(Editor a w) -> Editor [a] [(Int,w)]
toolbarL :: ![Editor a w] -> Editor [a] [(Int,w)]
toolbar1 :: !(Editor a w) -> Editor a w
toolbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
toolbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
toolbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
toolbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
toolbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIButtonBar
buttonbar  :: Editor () ()
buttonbarl :: !(Editor a w) -> Editor [a] [(Int,w)]
buttonbarL :: ![Editor a w] -> Editor [a] [(Int,w)]
buttonbar1 :: !(Editor a w) -> Editor a w
buttonbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
buttonbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
buttonbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
buttonbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
buttonbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIList
list  :: Editor () ()
listl :: !(Editor a w) -> Editor [a] [(Int,w)]
listL :: ![Editor a w] -> Editor [a] [(Int,w)]
list1 :: !(Editor a w) -> Editor a w
list2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
list3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
list4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
list5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
listc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIListItem
listitem  :: Editor () ()
listiteml :: !(Editor a w) -> Editor [a] [(Int,w)]
listitemL :: ![Editor a w] -> Editor [a] [(Int,w)]
listitem1 :: !(Editor a w) -> Editor a w
listitem2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
listitem3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
listitem4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
listitem5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
listitemc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)

//# UIDebug
debug  :: Editor () ()
debugl :: !(Editor a w) -> Editor [a] [(Int,w)]
debugL :: ![Editor a w] -> Editor [a] [(Int,w)]
debug1 :: !(Editor a w) -> Editor a w
debug2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa, Maybe wb)
debug3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
debug4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
debug5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
debugc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)
