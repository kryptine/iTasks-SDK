module Editlet

import iTasks
import Tasklet

:: TimeDelta = SetSec !Int | SetMin !Int | SetHour !Int
derive class iTask TimeDelta

timeEditlet :: Time -> Editlet Time [TimeDelta]
timeEditlet t =	{Editlet
				|value		= t
				,html		= HtmlDef ("<div style=\"font-size: 24pt;\" id=\"ugly-test\"></div>")
				,handlers	= [HtmlEvent "editlet" "init" onUpdate,HtmlEvent "editlet" "update" onUpdate]
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	onUpdate :: Time TaskInstanceId HtmlObject *HtmlDocument -> *(!*HtmlDocument, Time)
	onUpdate val iid obj doc
		# (doc,_) 	= setDomAttr doc "ugly-test" "innerHTML" (toString val)
		# (doc,_)	= setDomAttr doc "ugly-test" "style.color" (colors !! (val.Time.sec rem (length colors)))
		= (doc,val)
		
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

test = viewSharedInformation "Clock" [ViewWith timeEditlet] currentTime

Start :: *World -> *World
Start world = startEngine test world