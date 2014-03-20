implementation module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks
import iTasks.API.Core.Client.Editlet

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
derive gEditMeta AnalogClock
derive gVerify AnalogClock
derive gEq AnalogClock
derive gDefault AnalogClock
derive gVisualizeText AnalogClock

gEditor{|AnalogClock|} dp vv=:(AnalogClock t,mask,ver) meta vst
    = gEditor{|*|} dp (analogClockEditlet t,mask,ver) meta vst

gUpdate{|AnalogClock|} dp upd (AnalogClock t,mask) iworld
    # ((Editlet t _ _,mask),iworld) = gUpdate{|*|} dp upd (analogClockEditlet t,mask) iworld
    = ((AnalogClock t,mask),iworld)

//SVG Based analog clock editlet
analogClockEditlet :: Time -> Editlet Time [(Int,Int)]
analogClockEditlet t
    = toEditlet (EditletSimpl t {EditletSimplDef| genUI = genUI, updateUI = updateUI, genDiff = genTimeDiff, appDiff= appTimeDiff})
where
	genUI cid world
		  =({ html 			= svgClock cid
		  	, eventHandlers = []
		  	, width 		= ExactSize 100
		  	, height 		= ExactSize 100
		  	},world)

	updateUI cid _ val=:{Time|sec,min,hour}world
        //Update hands
        # world = foldr updateHand world [(cid+++"-sec-hand",6*sec),(cid+++"-min-hand",6*min),(cid+++"-hour-hand",30*hour)]
		= (val,world)

    updateHand (id,degrees) world
        # (hand,world) = getDomElement id world
        # (_,world)    = callObjectMethod "setAttribute" [toJSArg "transform",toJSArg ("rotate("+++toString (degrees - 90)+++" 50 50)")] hand world
        = world
	
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

