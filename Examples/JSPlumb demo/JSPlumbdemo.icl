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
                ,endPoint 				= toHtmlObject [toHtmlObject "Dot",toHtmlObject {radius = 1}]
                ,paintStyle 			= {fillStyle =  "red"}//"#66CC00"}
                ,setDragAllowedWhenFull = True
                }

sourceOptions = {anchor 				= "BottomCenter"
                ,maxConnections 		= -1
                ,isSource 				= True
                ,isTarget 				= False
                ,endPoint 				= toHtmlObject [toHtmlObject "Dot",toHtmlObject {radius = 5}]
                ,paintStyle 			= {fillStyle =  "#EEDD00"}
                ,setDragAllowedWhenFull = True
                }

//derive  JSONEncode  EndPointOptions, FillStyle
               
:: EndPointOptions =   {anchor    				:: String
          			   ,maxConnections  		:: Int
          			   ,isSource  				:: Bool
          			   ,isTarget  				:: Bool
          			   ,endPoint        		:: HtmlObject
          			   ,paintStyle      		:: FillStyle
          			   ,setDragAllowedWhenFull  :: Bool
        			   }
        			   
:: Radius = {radius :: Int}
:: FillStyle = {fillStyle :: String}

:: PlumbState = {plumb :: Maybe HtmlObject}

jsPlumbTasklet ::  Tasklet PlumbState Void
jsPlumbTasklet = 
	{ generatorFunc		= jsPlumbGUI
	, resultFunc		= \_ = Value Void False
	, tweakUI  			= setTitle "JSPlumb Tasklet"
	}
where
	jsPlumbGUI iid taskId Nothing iworld 
		= jsPlumbGUI iid taskId (Just {plumb = Nothing}) iworld

	jsPlumbGUI iid _ (Just st) iworld

		# canvas = DivTag [IdAttr "plumb_place_holder", StyleAttr "width:100%; height:100%"] []

		# gui = { TaskletHTML
				| width  		= ExactSize 300
				, height 		= ExactSize 300
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "destroy" onDestroy
				                  ,HtmlEvent "tasklet" "afterlayout" onAfterLayout]
				}
			
		= (TaskletHTML gui, st, iworld)
				
	where

	    onScriptLoad st _ _ d
		    # (d, _) = setDomAttr d "plumb_place_holder" "innerHTML"
		    				("<div id=\"plumb_canvas\" style=\"width:100%; height:100%\">" +++
		    					"<div class='node' id='block1' style='position: absolute; top: 50px; left: 50px; border: 1px solid black;'>Block 1</div>" +++
		    					"<div class='node' id='block2' style='position: absolute; top: 100px; left: 100px; border: 1px solid black;'>Block 2</div>" +++
		    				"</div>")
		    # (d, mapdiv) = getDomElement d "plumb_canvas"
	        
		    # (d, jsPlumb)    = findObject d "jsPlumb"
			# (d, jsPlumb, _) = runObjectMethod d jsPlumb "ready" [createEventHandler onReady iid]

	
		    //# (d, mapevent) = findObject d "google.maps.event" 
			//# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "dragend", onChange]
			//# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "maptypeid_changed", onChange]
			//# (d, mapevent, _) = runObjectMethod d mapevent "addListener" [map, toHtmlObject "zoom_changed", onChange]
			= (d, {st & plumb = Just jsPlumb})
		//where
			//onChange = createEventHandler updatePerspective iid

		// Google maps API doesn't like to be loaded twice	
		onInit st iid e d
			# (d, jsPlumb) = findObject d "jsPlumb"
			| isUndefined jsPlumb 
			= (loadPlumbAPI iid e d, st)
			= onScriptLoad st iid e d
		
		onReady st iid e d
			# (d, plumb) = findObject d "jsPlumb"
			# (d, plumb, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject "block1",toHtmlObject targetOptions]
			# (d, plumb, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject "block1",toHtmlObject sourceOptions]
			# (d, plumb, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject "block2",toHtmlObject targetOptions]
			# (d, plumb, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject "block2",toHtmlObject sourceOptions]						
			# (d, plumb, _) = runObjectMethod d plumb "draggable" [toHtmlObject "block1"]
			# (d, plumb, _) = runObjectMethod d plumb "draggable" [toHtmlObject "block2"]
			= (d, plumb)
		
		loadPlumbAPI iid e d	
			# (d, window)  = findObject d "window"	
			//# (d, _, _)    = setObjectAttr d window "gmapscallback" (createEventHandler onScriptLoad iid)
	
			= d

		nullEventHandler st _ _ d = (d, st)

		onDestroy st=:{plumb = Just plumb} _ _ d
		    # (d, mapevent) = findObject d "jsPlumb" 
			# (d, mapevent, _) = runObjectMethod d mapevent "clearInstanceListeners" [plumb]
		
			// clear generated stuff
			# (d, _) = setDomAttr d "plumb_place_holder" "innerHTML" ""
		
			= (d, {st & plumb = Nothing})

		onDestroy st _ _ d
			= (d, st)

		// http://stackoverflow.com/questions/1746608/google-maps-not-rendering-completely-on-page
		onAfterLayout st=:{plumb = Just plumb} _ _ d
		    //# (d, mapevent) = findObject d "google.maps.event" 
			//# (d, mapevent, _) = runObjectMethod d mapevent "trigger" [plumb, toHtmlObject "resize"]
		
			= (d, st)

		onAfterLayout st _ _ d
			= (d, st)

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
    
