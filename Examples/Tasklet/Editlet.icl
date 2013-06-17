module Editlet

import iTasks
import iTasks.Framework.ClientInterface

:: TimeDelta = SetSec !Int | SetMin !Int | SetHour !Int
derive class iTask TimeDelta

buienLet :: Editlet String Void
buienLet = {Editlet|value="Buienradar",html=const (RawText html), handlers = [], genDiff = \_ _ -> Nothing, appDiff = \_ v -> v}
where
	html = "<a href=\"http://www.buienradar.nl\" target=\"_blank\"><img border=\"0\" src=\"http://m.buienradar.nl/\"></a>"

:: StringDelta = {newString :: String}
derive class iTask StringDelta

stringlet :: Editlet String [String]
stringlet = {Editlet|value = "Hello world",html = \id -> TextareaTag [IdAttr id] []
			,handlers = [ComponentEvent "editlet" "init" onUpdate
						,ComponentEvent "editlet" "update" onUpdate
						,ComponentEvent "editlet" "keyup" onChange]
			,genDiff = \o n -> if (o == n) Nothing (Just [n,n])
			,appDiff = \n _ -> hd n
			}
where
	onUpdate :: ComponentId HtmlObject String *HtmlWindow -> *(String, !*HtmlWindow)
	onUpdate id event val win
		# (_,win) 	= setDomAttr id "value" val win
		= (val,win)
	
	onChange :: ComponentId HtmlObject String *HtmlWindow -> *(String, !*HtmlWindow)
	onChange id event val win = getDomAttr id "value" win

timeEditlet :: Time -> Editlet Time [TimeDelta]
timeEditlet t =	{Editlet
				|value		= t
				,html		= \id -> RawText ("<div style=\"font-size: 24pt;\" id=\"" +++ id +++ "\"></div>")
				,handlers	= [ComponentEvent "editlet" "init" onUpdate, ComponentEvent "editlet" "update" onUpdate]
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	onUpdate :: ComponentId HtmlObject Time *HtmlWindow -> *(Time, !*HtmlWindow)
	onUpdate id obj val win
		# (_,win) 	= setDomAttr id "innerHTML" (toString val) win
		# (_,win)	= setDomAttr id "style.color" (colors !! (val.Time.sec rem (length colors))) win
		= (val,win)
		
	colors = ["#f0f","#00f","#f00","#30f","#ff0","#66f"]
	
	genDiff :: Time Time -> Maybe [TimeDelta]
	genDiff t1 t2 = case (  (if (t1.Time.sec == t2.Time.sec) [] [SetSec t2.Time.sec])
						 ++ (if (t1.Time.min == t2.Time.min) [] [SetMin t2.Time.min])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [SetHour t2.Time.hour])
						 ) of [] = Nothing ; delta = Just delta

	appDiff :: [TimeDelta] Time -> Time
	appDiff [] t = t
	appDiff [SetSec s:d] t = appDiff d {Time|t & sec = s}
	appDiff [SetMin m:d] t = appDiff d {Time|t & min = m}
	appDiff [SetHour h:d] t = appDiff d {Time|t & hour = h}	

test2 = updateInformation "Test" [] (timeEditlet (fromString "13:00:00"))

test3 = viewSharedInformation "Clock2" [] (mapRead (\t -> repeatn 2 (timeEditlet t)) currentTime)
		//||- viewInformation (Title "Buienradar") [] buienLet
		//<<@ AfterLayout (uiDefSetDirection Horizontal)
		
//test = viewSharedInformation "Clock" [ViewWith timeEditlet] currentTime

test = updateInformation "String" [] stringlet @ (\e -> e.Editlet.value) >&> viewSharedInformation "DEBUG" []

Start :: *World -> *World
Start world = startEngine test3 world