implementation module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks
import iTasks.UI.Editor

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
derive gEditMeta AnalogClock
derive gVerify AnalogClock
derive gEq AnalogClock
derive gDefault AnalogClock
derive gText AnalogClock

gEditor{|AnalogClock|} = {render=render,edit=edit}
where
	render dp (AnalogClock t) mask ver meta vst
    	= gEditor{|*|}.render dp (analogClockEditlet t) mask ver meta vst
	
	edit dp e (AnalogClock t) mask ust
		# (editlet,mask,ust) = gEditor{|*|}.edit dp e (analogClockEditlet t) mask ust
		= (AnalogClock editlet.currVal,mask,ust) 

//SVG Based analog clock editlet
analogClockEditlet :: Time -> Editlet Time [(Int,Int)] ()
analogClockEditlet t 
    = {Editlet
      |currVal      = t
      ,defValSrv    = gDefault{|*|}
      ,genUI        = genUI
      , initClient = \_ _ world -> ((), world)
      ,appDiffClt   = appDiffClt
      ,genDiffSrv   = genTimeDiff
      ,appDiffSrv   = appTimeDiff
      }
where
	genUI cid world
		  =({ ComponentHTML
            | html 			= svgClock cid
		  	, width 		= ExactSize 100
		  	, height 		= ExactSize 100
		  	},world)
	
    svgClock cid = SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          (face ++
                          [hand (cid+++"-hour-hand") 30 "#999"
                          ,hand (cid+++"-min-hand") 45 "#666"
                          ,hand (cid+++"-sec-hand") 40 "#000"])
    face = [RectElt [WidthAttr "100px",HeightAttr "100px",StyleAttr "fill:#ccc;stroke: #000;stroke-width: 3px"] [XAttr ("0",PX),YAttr ("0",PX)]
           :[RectElt [WidthAttr "10px",HeightAttr "2px",StyleAttr "fill: #ddd;"]
                     [XAttr ("90",PX),YAttr ("50",PX),TransformAttr [RotateTransform (toString (30*i)) (Just ("50","50"))]] \\ i <- [0..11]
            ]]

    hand id len color
        = RectElt [WidthAttr (toString len +++"px"),HeightAttr "2px",IdAttr id,StyleAttr ("fill: "+++color)]
                  [XAttr ("50",PX),YAttr ("50",PX)]
    
    appDiffClt mkEventHandler cid [] () world = ((),world)
    appDiffClt mkEventHandler cid [(0,s):upd] () world = appDiffClt mkEventHandler cid upd () (updateHand (cid+++"-sec-hand", 6 * s) world)
    appDiffClt mkEventHandler cid [(1,m):upd] () world = appDiffClt mkEventHandler cid upd () (updateHand (cid+++"-min-hand", 6 * m) world)
    appDiffClt mkEventHandler cid [(2,h):upd] () world = appDiffClt mkEventHandler cid upd () (updateHand (cid+++"-hour-hand", 30 * h) world)
        
    updateHand (id,degrees) world
        # (hand,world) = .? (getElementById id) world
        # (_,world)    = callObjectMethod "setAttribute" [toJSArg "transform",toJSArg ("rotate("+++toString (degrees - 90)+++" 50 50)")] hand world
        = world

genTimeDiff :: Time Time -> Maybe [(Int,Int)]
genTimeDiff t1 t2 = case (  (if (t1.Time.sec == t2.Time.sec) [] [(0,t2.Time.sec)])
						 ++ (if (t1.Time.min == t2.Time.min) [] [(1,t2.Time.min)])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [(2,t2.Time.hour)])
						 ) of [] = Nothing ; delta = Just delta

appTimeDiff :: [(Int,Int)] Time -> Time
appTimeDiff [] t = t
appTimeDiff [(0,s):d] t = appTimeDiff d {Time|t & sec = s}
appTimeDiff [(1,m):d] t = appTimeDiff d {Time|t & min = m}
appTimeDiff [(2,h):d] t = appTimeDiff d {Time|t & hour = h}	

