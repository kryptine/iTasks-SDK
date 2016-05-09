implementation module iTasks.API.Extensions.Dashboard
import iTasks
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.JS.Interface
import qualified Data.Map as DM

derive JSONEncode ControlLight
derive JSONDecode ControlLight
derive gVerify ControlLight
derive gEq ControlLight
derive gDefault ControlLight
derive gText ControlLight

gEditor{|ControlLight|} = fromEditlet controlLightEditlet

//SVG Based fake control light
controlLightEditlet :: Editlet ControlLight
controlLightEditlet
    = {Editlet
      |genUI  = genUI
      ,initUI = initUI
      ,updUI  = \_ a _ b _ vst -> (Ok (if (a===b) NoChange (ChangeUI [SetAttribute "value" (JSONString (color b))] [])),vst)
      ,onEdit = \_ _ a m ust -> (a,m,ust)
      }
where
	genUI dp val mask world
		# attr = 'DM'.unions [sizeAttr (ExactSize 20) (ExactSize 20),valueAttr (JSONString (toString (svgLight (color val))))]
		= (Ok (uia UIViewHtml attr), world)

    initUI me world 
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
		# world = (me .# "onAttributeChange" .= jsOnAttributeChange) world
		= world

	onAttributeChange me args world
		| jsArgToString (args !! 0) == "diff"
			# (color, world) = fromJSArray (toJSVal (args !! 1)) id world
			# (svgEl,world)    = .? (me .# "domEl" .# "children" .# 0) world
			# (lightEl,world)  = .? (svgEl .# "children" .# 1) world
			# (_,world)        = (lightEl .# "setAttribute" .$ ("fill",color)) world //Just update the color
			= (jsNull,world)
		| otherwise
			= (jsNull,jsTrace "Unknown attribute change" world)

    color LightOnGreen  = "green"
    color LightOnRed    = "red"
    color LightOnOrange = "orange"
    color _             = "#333"

    svgLight val = SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          [defs,light val,glass,flare]

    defs  = DefsElt [] [] [glassgr,flaregr]
    where
    	glassgr = RadialGradientElt [IdAttr "glass-gradient"] []
				   [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "100%",StopColorAttr "white",StopOpacityAttr "0"]]
    	flaregr = LinearGradientElt [IdAttr "flare-gradient"] [X1Attr ("0",PX),X2Attr ("0",PX),Y1Attr ("0",PX),Y2Attr ("1",PX)] 
                   [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "90%",StopColorAttr "white",StopOpacityAttr "0"]]

    light val = CircleElt [] [CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX),FillAttr (PaintColor (SVGColorText val) Nothing)]
    glass = CircleElt [StyleAttr "stroke: #000;stroke-width: 8px"] [FillAttr (PaintFuncIRI (IRI ("#glass-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX)]
    flare = EllipseElt [] [FillAttr (PaintFuncIRI (IRI ("#flare-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("45",PX),RxAttr ("35",PX),RyAttr ("30",PX)]
