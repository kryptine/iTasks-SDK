module Square

import iTasks, iTasks.Framework.Generic, iTasks.Framework.ClientSupport.Tasklet

//-------------------------------------------------------------------------

:: Color :== String

:: Square = { highlighted :: Bool
			, finished    :: Maybe Bool
			}

:: Cmd = NoCmd | DeHighlight | Highlight | Destroy

derive class iTask Cmd

createSquare :: Color Int (Shared Cmd) -> Task Bool
createSquare color number shared 
	= mkTaskWithShared squareTasklet shared updateFun
where
	squareTasklet :: Tasklet Square Bool 
	squareTasklet = 
		{ generatorFunc		= generateGUI
		, resultFunc		= resultFun
		, tweakUI  			= id
		}

	resultFun {finished=Just clicked} = Value clicked True
	resultFun _ = Value False False

	generateGUI :: !TaskInstanceId !TaskId (Maybe Square) !*IWorld -> *(!TaskletGUI Square, !Square, !*IWorld)
	generateGUI iid _ _ iworld  

		# gui = { TaskletHTML
				| width  		= ExactSize 40
				, height 		= ExactSize 40
				, html   		= HtmlDef ("<div id=\""+++oid+++"\" style=\""+++style+++"\">"+++toString number+++"</div>")
				, eventHandlers = [ HtmlEvent "tasklet" "init" onInit
								  , HtmlEvent "tasklet" "update" onInit
								  , HtmlEvent oid "click" onClick]			
				}
			
		= (TaskletHTML gui, {highlighted = False, finished = Nothing}, iworld)
	where
		oid = "object"+++iid
	
  	  	style = "position:absolute; left:0px;top:0px;width:40px;height:40px;rem;background:"+++color+++";"
    
		onInit st=:{highlighted=True} _ _ d
			# (d, str) = setDomAttr d oid "style.border" "2px solid black"			
			= (d, st)
		onInit st _ _ d
			# (d, str) = setDomAttr d oid "style.border" ("2px solid "+++color)
			= (d, st)		
		
		onClick st=:{highlighted=True} _ _ d
			= (d, {st & finished = Just True})
		onClick st _ _ d
			= (d, st)	
		
	updateFun :: Cmd Square -> Square
	updateFun NoCmd st = st
	updateFun Destroy st = {st & finished = Just False}		
	updateFun Highlight st = {st & highlighted = True}
	updateFun DeHighlight st = {st & highlighted = False}

//UTIL
(>>-) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>-) taska taskb = step taska [WhenStable (const taskb)]

//Wait for (at least) n seconds
wait :: Int -> Task Void
wait n = get currentTime >>= \start -> watch currentTime >>* [OnValue (\(Value now _) -> if (now > addSeconds n start) (Just (return Void)) Nothing)]
where
	//ONLY CORRECT FOR n < 60
	addSeconds n t = t + {Time|hour=0,min=0,sec=n}

tasklet num = withShared NoCmd (\shared -> 
		//viewSharedInformation "" [] shared
		createSquare "red" num shared
	    -||
	    forever (wait 2 >>- update blink shared))
	    >>- viewInformation "" [] "Clicked"  
where
	blink Highlight = DeHighlight
	blink _ = Highlight	

	destroy _ = Destroy

Start :: *World -> *World
Start world = startEngine (tasklet 1 -&&- (wait 1 >>- tasklet 2)) world
