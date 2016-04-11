implementation module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor
import qualified Data.Map as DM

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
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
      ,saplInit   = saplInit 
      ,initClient = \_ _ _ world -> ((), world)
      ,appDiffClt   = appDiffClt
      ,genDiffSrv   = genTimeDiff
      ,appDiffSrv   = appTimeDiff
      }
where
	genUI cid val world
		= (setSize (ExactSize 100) (ExactSize 100) (uia UIViewHtml ('DM'.fromList [("value",JSONString (toString (svgClock cid)))])), world)

	saplInit me world
		//Register listener for ui diffs from the server
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
		# world = jsSetObjectAttr "onAttributeChange" jsOnAttributeChange me world
		= world

	onAttributeChange me args world
		| jsArgToString (args !! 0) == "diff"
			# changes = toJSVal (args !! 1)
			//ONLY FIRST CHANGE FOR NOW
			# (change,world) = jsGetObjectEl 0 changes world
			# (field,world) = jsGetObjectEl 0 change world
			# (value,world) = jsGetObjectEl 1 change world
			//Update the hand
			# (taskId,world) = jsGetObjectAttr "taskId" me world 
			# (editorId,world) = jsGetObjectAttr "editorId" me world 
			# cid = "editlet-" +++ (jsValToString taskId) +++ "-" +++ (jsValToString editorId)
			# field = jsValToInt field
			# world = updateHand (handId cid field, degrees field (jsValToInt value)) world
			= (jsNull,world)
		| otherwise
			= (jsNull,jsTrace "Unknown attribute change" world)

	handId cid 0 = cid+++"-sec-hand"
	handId cid 1 = cid+++"-min-hand"
	handId cid 2 = cid+++"-hour-hand"

	degrees 0 v = 6 * v
	degrees 1 v = 6 * v
	degrees 2 v = 30 * v

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
        = jsTrace hand world

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

