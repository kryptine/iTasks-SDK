implementation module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor
import qualified Data.Map as DM

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
derive gEditMeta AnalogClock
derive gVerify AnalogClock
derive gEq AnalogClock
derive gDefault AnalogClock
derive gText AnalogClock

gEditor{|AnalogClock|} = fromEditlet analogClockEditlet

//SVG Based analog clock editlet
analogClockEditlet :: Editlet AnalogClock [(Int,Int)] ()
analogClockEditlet
    = {Editlet
      |genUI        = genUI
      ,initClient = \_ _ _ world -> ((), world)
      ,appDiffClt   = appDiffClt
      ,genDiffSrv   = genTimeDiff
      ,appDiffSrv   = appTimeDiff
      }
where
	genUI cid val world
		= (setSize (ExactSize 100) (ExactSize 100) (uia UIViewHtml ('DM'.fromList [("value",JSONString (toString (svgClock cid)))])), world)

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

genTimeDiff :: AnalogClock AnalogClock -> Maybe [(Int,Int)]
genTimeDiff (AnalogClock t1) (AnalogClock t2) = case (  (if (t1.Time.sec == t2.Time.sec) [] [(0,t2.Time.sec)])
						 ++ (if (t1.Time.min == t2.Time.min) [] [(1,t2.Time.min)])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [(2,t2.Time.hour)])
						 ) of [] = Nothing ; delta = Just delta

appTimeDiff :: [(Int,Int)] AnalogClock -> AnalogClock
appTimeDiff [] t = t
appTimeDiff [(0,s):d] (AnalogClock t) = appTimeDiff d (AnalogClock {Time|t & sec = s})
appTimeDiff [(1,m):d] (AnalogClock t) = appTimeDiff d (AnalogClock {Time|t & min = m})
appTimeDiff [(2,h):d] (AnalogClock t) = appTimeDiff d (AnalogClock {Time|t & hour = h})

