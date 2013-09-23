module Movin

import iTasks, iTasks.API.Core.Client.Tasklet

//-------------------------------------------------------------------------

:: Position :== Int

movinTasklet :: Tasklet Position Void 
movinTasklet = 
	{ generatorFunc		= generateGUI
	, resultFunc		= const (Value Void False)
	, tweakUI  			= id
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

:: Cmd = SetPosX Position

derive class iTask Cmd

tasklet :: Task Void
tasklet
	= withShared (SetPosX 0) (\pos ->
	  		  mkTaskWithShared movinTasklet pos updateFun
			  -||  
			  forever (wait 10 >>- update (moveForward 40) pos ))
where
	updateFun :: Cmd Position -> Position
	updateFun (SetPosX x) st = x

	moveForward :: Position Cmd -> Cmd
	moveForward px (SetPosX x) = SetPosX (x+px)

//UTIL
(>>-) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>-) taska taskb = step taska (const Nothing) [OnValue (ifStable (const taskb))]

//Wait for (at least) n seconds
wait :: Int -> Task Void
wait n = get currentTime >>= \start -> watch currentTime >>* [OnValue (\(Value now _) -> if (now > addSeconds n start) (Just (return Void)) Nothing)]
where
	//ONLY CORRECT FOR n < 60
	addSeconds n t = t + {Time|hour=0,min=0,sec=n}
	     
Start :: *World -> *World
Start world = startEngine tasklet world
