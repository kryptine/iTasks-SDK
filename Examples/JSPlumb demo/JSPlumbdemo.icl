module JSPlumbdemo

import iTasks, Tasklet
import Text.StringAppender, graph_to_sapl_string
import sapldebug

Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "JSPlumb tasklet" "Simple JSPlumb tasklet" plumbtasklet]

plumbtasklet :: Task Void
plumbtasklet
	= 		mkInstanceId >>= \iid ->
	 		mkTask (iid, jsPlumbTasklet)
							 
targetOptions = {anchor 				= "TopCenter"
                ,maxConnections 		= -1
                ,isSource 				= False
                ,isTarget 				= True
                ,endpoint 				= [toHtmlObject "Dot",toHtmlObject {radius = 5}]
                ,paintStyle 			= {fillStyle = "#66CC00"}
                ,setDragAllowedWhenFull = True
                }

sourceOptions = {anchor 				= "BottomCenter"
                ,maxConnections 		= -1
                ,isSource 				= True
                ,isTarget 				= False
                ,endpoint 				= [toHtmlObject "Dot",toHtmlObject {radius = 5}]
                ,paintStyle 			= {fillStyle =  "#EEDD00"}
                ,setDragAllowedWhenFull = True
                }

//derive  JSONEncode  EndPointOptions, FillStyle
               
:: EndPointOptions =   {anchor    				:: String
          			   ,maxConnections  		:: Int
          			   ,isSource  				:: Bool
          			   ,isTarget  				:: Bool
          			   ,endpoint        		:: [HtmlObject]
          			   ,paintStyle      		:: FillStyle
          			   ,setDragAllowedWhenFull  :: Bool
        			   }

:: Radius = {radius :: Int}
:: FillStyle = {fillStyle :: String}

:: PlumbState = {plumb :: Maybe HtmlObject}

jsPlumbTasklet ::  Tasklet PlumbState Void
jsPlumbTasklet = 
	{ generatorFunc		= jsPlumbGUI
	, resultFunc		= \_ -> Value Void False
	, tweakUI  			= setTitle "JSPlumb Tasklet"
	}
where
	jsPlumbGUI iid taskId Nothing iworld 
		= jsPlumbGUI iid taskId (Just {plumb = Nothing}) iworld

	jsPlumbGUI iid _ (Just st) iworld

		# canvas = DivTag [IdAttr "plumb_canvas", StyleAttr "width:100%; height:100%"] []

		# gui = { TaskletHTML
				| width  		= ExactSize 600
				, height 		= ExactSize 600
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "destroy" onDestroy
				                  ,HtmlEvent "tasklet" "afterlayout" onAfterLayout]
				}
			
		= (TaskletHTML gui, st, iworld)
				
	where

	    onScriptLoad st _ _ d
		    # (d, _) = setDomAttr d "plumb_canvas" "innerHTML"
		    				("<div class='node' id='block1' style='position: absolute; top: 50px; left: 50px; border: 1px solid black;'>Block 1</div>" +++
		    				 "<div class='node' id='block2' style='position: absolute; top: 100px; left: 100px; border: 1px solid black;'>Block 2</div>")
		    
		    # (d, plumb)    = jsPlumb d
			# (d, plumb, _) = runObjectMethod d plumb "ready" [createEventHandler onReady iid]
			= (d, {st & plumb = Just plumb})

		onInit st iid e d
			# (d, plumb) = jsPlumb d
			| isUndefined plumb 
			= (loadPlumbAPI iid e d, st)
			= onScriptLoad st iid e d
		
		onReady st iid e d
			# (d, plumb) = jsPlumb d
			# (d, plumb) = addEndpoint plumb "block1" targetOptions d
			# (d, plumb) = addEndpoint plumb "block1" sourceOptions d
			# (d, plumb) = addEndpoint plumb "block2" targetOptions d
			# (d, plumb) = addEndpoint plumb "block2" sourceOptions d
			# (d, plumb) = draggable plumb "block1" d
			# (d, plumb) = draggable plumb "block2" d
			= (d, plumb)
		
		loadPlumbAPI _ _ d = d

		nullEventHandler st _ _ d = (d, st)

		onDestroy st=:{plumb = Just plumb} _ _ d
			// clear generated stuff
			# (d, _) = setDomAttr d "plumb_canvas" "innerHTML" ""
		
			= (d, {st & plumb = Nothing})

		onDestroy st _ _ d
			= (d, st)

		onAfterLayout st _ _ d
			= (d, st)

jsPlumb :: *HtmlDocument -> *(*HtmlDocument,HtmlObject)
jsPlumb d = findObject d "jsPlumb"

addEndpoint :: HtmlObject a b *HtmlDocument -> *(*HtmlDocument,HtmlObject)
addEndpoint plumb target opts d
	# (d, p, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject target,toHtmlObject opts]
	= (d, p)

draggable :: HtmlObject a *HtmlDocument -> *(*HtmlDocument,HtmlObject)
draggable plumb target d
	# (d, p, _) = runObjectMethod d plumb "draggable" [toHtmlObject target]
	= (d, p)

//-------------------------------------------------------------------------

ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing

ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
