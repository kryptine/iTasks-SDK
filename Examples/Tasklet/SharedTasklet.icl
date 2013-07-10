module SharedTasklet

import iTasks, iTasks.Framework.ClientSupport.Tasklet
import Text.StringAppender, graph_to_sapl_string
import sapldebug

//-------------------------------------------------------------------------

:: Position :== Int

movinTasklet :: Tasklet Position Position 
movinTasklet = 
	{ generatorFunc		= generateGUI
	, resultFunc		= \pos -> Value pos False
	, tweakUI  			= id //setTitle "Moving object"
	}

generateGUI :: !TaskInstanceId !TaskId (Maybe Position) !*IWorld -> *(!TaskletGUI Position, !Position, !*IWorld)
generateGUI iid taskId Nothing iworld  = generateGUI iid taskId (Just 0) iworld
generateGUI _ _ (Just x) iworld  

	# gui = { TaskletHTML
			| width  		= ExactSize 800
			, height 		= ExactSize 600
			, html   		= HtmlDef ("<div id=\"object\" style=\""+++style+++"\">Airplane</div>")
			, eventHandlers = [ HtmlEvent "tasklet" "init" onInit
							  , HtmlEvent "tasklet" "update" onInit]			
			}
			
	= (TaskletHTML gui, x, iworld)
where
    style = "position:absolute; left:"+++toString x+++"px;top:8em;width:5em;line-height:3em;background:#99ccff;border:1px solid #003366;white-space:nowrap;padding:0.5em;"
    
	onInit x _ _ d
		# (d, str) = setDomAttr d "object" "style.left" (toString x+++"px")
		= (d, x)
 
toPrj pos = {Scale|min=0,cur=pos,max=800}
fromPrj _ {Scale|cur} = cur

taskletSlider :: Task Position
taskletSlider
	= mkInstanceId >>= \iid -> 
		withShared 0 (\pos ->
			  updateSharedInformation "Adapt position" [UpdateWith toPrj fromPrj] pos
			  -||-
	  		  mkTaskWithShared (iid, movinTasklet) pos updateFun)
where
	updateFun :: Position Position -> Position
	updateFun sharedval st = sharedval

tasklet :: Task Position
tasklet
	= mkInstanceId >>= \iid -> 
		withShared 0 (\pos ->
			  updateSharedInformation "Adapt position" [] pos
			  -||-
	  		  mkTaskWithShared (iid, movinTasklet) pos updateFun)
where
	updateFun :: Position Position -> Position
	updateFun sharedval st = sharedval

//Start :: *World -> *World
//Start world = startEngine taskletSlider world

Start = acos 10.2




