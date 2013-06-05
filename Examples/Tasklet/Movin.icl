module Movin

import iTasks, Tasklet
import Text.StringAppender, graph_to_sapl_string
import sapldebug

//-------------------------------------------------------------------------

:: Position :== Int

movinTasklet :: Tasklet Position Void 
movinTasklet = 
	{ generatorFunc		= generateGUI
	, resultFunc		= const (Value Void False)
	, tweakUI  			= id //setTitle "Moving object"
	}

generateGUI :: !TaskInstanceId !TaskId (Maybe Position) !*IWorld -> *(!TaskletGUI Position, !Position, !*IWorld)
generateGUI iid taskId Nothing iworld  = generateGUI iid taskId (Just 0) iworld
generateGUI _ _ (Just x) iworld  

	# gui = { TaskletHTML
			| width  		= ExactSize 800
			, height 		= ExactSize 600
			, html   		= HtmlDef ("<div id=\"object\" style=\""+++style+++"\">Airplane</div>")
			, eventHandlers = [HtmlEvent "tasklet" "init" onInit]
			}
			
	= (TaskletHTML gui, x, iworld)
where
    style = "position:absolute; left:"+++toString x+++"px;top:8em;width:5em;line-height:3em;background:#99ccff;border:1px solid #003366;white-space:nowrap;padding:0.5em;"
    
	onInit x _ _ d
		# (d, str) = setDomAttr d "object" "style.left" (toString x)
		= (d, x)
 
tasklet :: Task Void
tasklet
	= mkInstanceId >>= \iid ->
	  mkTask (iid, movinTasklet) id >>|
	  forever (waitForTime delay >>| mkTask (iid, movinTasklet) moveLeft) 
where
	delay = {Time | hour = 0, min = 0, sec = 3}

	moveLeft :: Position -> Position
	moveLeft x = x + 10
							     
Start :: *World -> *World
Start world = startEngine tasklet world
