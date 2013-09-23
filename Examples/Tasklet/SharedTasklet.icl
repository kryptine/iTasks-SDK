module SharedTasklet

import iTasks, iTasks.API.Core.Client.Tasklet

//-------------------------------------------------------------------------

:: Position :== Int

movinTasklet :: Tasklet Position Position 
movinTasklet = 
	{ generatorFunc		= generateGUI
	, resultFunc		= \pos -> Value pos False
	, tweakUI  			= id //setTitle "Moving object"
	}

generateGUI :: !TaskId (Maybe Position) !*IWorld -> *(!TaskletGUI Position, !Position, !*IWorld)
generateGUI taskId Nothing iworld  = generateGUI taskId (Just 0) iworld
generateGUI _ (Just x) iworld  

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
    
	onInit x _ _ world
		# world = setDomAttr "object" "style.left" (toJSVal (toString x+++"px")) world
		= (x, world)
 
toPrj pos = {Scale|min=0,cur=pos,max=800}
fromPrj _ {Scale|cur} = cur

taskletSlider :: Task Position
taskletSlider
	= withShared 0 (\pos ->
			  updateSharedInformation "Adapt position" [UpdateWith toPrj fromPrj] pos
			  -||-
	  		  mkTaskWithShared movinTasklet pos updateFun)
where
	updateFun :: Position Position -> Position
	updateFun sharedval st = sharedval

tasklet :: Task Position
tasklet
	= withShared 0 (\pos ->
			  updateSharedInformation "Adapt position" [] pos
			  -||-
	  		  mkTaskWithShared movinTasklet pos updateFun)
where
	updateFun :: Position Position -> Position
	updateFun sharedval st = sharedval

Start :: *World -> *World
Start world = startEngine taskletSlider world





