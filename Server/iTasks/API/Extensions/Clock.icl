implementation module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor
import qualified Data.Map as DM, Data.Tuple

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
      ,saplInit     = saplInit 
      ,initClient   = \_ _ _ world -> ((), world)
      ,appDiffClt   = \_ _ _ _ world -> ((),world)
      ,genDiffSrv   = genTimeDiff
      ,appDiffSrv   = appTimeDiff
      }
where
	genUI (AnalogClock {Time|hour,min,sec}) world
		= (setSize (ExactSize 100) (ExactSize 100) (uia UIViewHtml ('DM'.fromList [("value",JSONString (toString (svgClock hour min sec)))])), world)
	where
		svgClock hour min sec 
			= SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          (face ++
                          [hand 45 (degrees 0 sec) "#000"
                          ,hand 50 (degrees 1 min) "#666"
                          ,hand 40 (degrees 2 hour) "#999"])

    face = [RectElt [WidthAttr "100px",HeightAttr "100px",StyleAttr "fill:#ccc;stroke: #000;stroke-width: 3px"] [XAttr ("0",PX),YAttr ("0",PX)]
           :[RectElt [WidthAttr "10px",HeightAttr "2px",StyleAttr "fill: #ddd;"]
                     [XAttr ("90",PX),YAttr ("50",PX),TransformAttr [RotateTransform (toString (30*i)) (Just ("50","50"))]] \\ i <- [0..11]
            ]]

    hand len angle color
        = RectElt [WidthAttr (toString len +++"px"),HeightAttr "2px",StyleAttr ("fill: "+++color)]
                  [XAttr ("50",PX),YAttr ("50",PX),TransformAttr [RotateTransform (toString (angle - 90)) (Just ("50","50"))]]

	saplInit me world
		//Register listener for ui diffs from the server
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
		# world = (me .# "onAttributeChange" .= jsOnAttributeChange) world
		= world

	onAttributeChange me args world
		| jsArgToString (args !! 0) == "diff"
			# (changes, world) = fromJSArray (toJSVal (args !! 1)) id world
			# world = foldl (updateHand me) world changes
			= (jsNull,world)
		| otherwise
			= (jsNull,jsTrace "Unknown attribute change" world)

    updateHand me world change
		# (which,world) = appFst jsValToInt (.? (change .# 0) world)
		# (value,world) = appFst jsValToInt (.? (change .# 1) world)
		# (svgEl,world)  = .? (me .# "domEl" .# "children" .# 0) world
		# (handEl,world) = .? (svgEl .# "children" .# (13 + which)) world //The first 13 svg elements are the clock face and markers
        # (_,world)      = (handEl .# "setAttribute" .$ [toJSArg "transform",toJSArg ("rotate("+++toString (degrees which value - 90)+++" 50 50)")]) world
        = world

	degrees 0 v = 6 * v
	degrees 1 v = 6 * v
	degrees 2 v = 30 * v

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

