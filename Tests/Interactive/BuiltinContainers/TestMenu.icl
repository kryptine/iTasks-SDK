module TestMenu
import iTasks
import qualified Data.Map as DM

test :: Task String
test = enterInformation [EnterUsing id editor]
	 //Remove prompt 
     <<@ ApplyLayout (foldl1 sequenceLayouts [removeSubUIs (SelectByPath [0]),unwrapUI,setUIAttributes (textAttr "Sub menu")])
where
    editor = menu2 (button <<@ 'DM'.unions[textAttr "Button a",iconClsAttr "icon-ok"]) (button <<@ (textAttr "Button b")) 
test =   viewInformation () [] "This is the content of the container"
     <<@ ApplyLayout (wrapUI UIMenu)
     <<@ ApplyLayout (setUIAttributes (textAttr "Open menu"))

Start world = doTasks test world
