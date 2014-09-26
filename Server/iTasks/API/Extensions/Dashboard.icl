implementation module iTasks.API.Extensions.Dashboard
import iTasks
import iTasks.API.Core.Client.Editlet

derive JSONEncode ControlLight
derive JSONDecode ControlLight
derive gEditMeta ControlLight
derive gVerify ControlLight
derive gEq ControlLight
derive gDefault ControlLight
derive gText ControlLight

gEditor{|ControlLight|} dp vv=:(v,mask,ver) meta vst
    = gEditor{|*|} dp (controlLightEditlet v,mask,ver) meta vst

gUpdate{|ControlLight|} dp upd (v,mask) iworld
    # ((Editlet v _ _,mask),iworld) = gUpdate{|*|} dp upd (controlLightEditlet v,mask) iworld
    = ((v,mask),iworld)

//SVG Based analog clock editlet
controlLightEditlet :: ControlLight -> Editlet ControlLight ControlLight
controlLightEditlet t
    = toEditlet (EditletSimpl t {EditletSimplDef| genUI = genUI, updateUI = updateUI
                                , genDiff = \a b -> if (a===b) Nothing (Just b)
                                , appDiff= \a _ -> a
                                })
where
	genUI cid world
		  =({ html 			= svgLight cid
		  	, eventHandlers = []
		  	, width 		= ExactSize 20
		  	, height 		= ExactSize 20
		  	},world)

	updateUI cid _ val world
        # (light,world) = getDomElement (lightId cid) world
        # (_,world)     = callObjectMethod "setAttribute" [toJSArg "fill",toJSArg (color val)] light world
		= (val,world)

    color LightOnGreen  = "green"
    color LightOnRed    = "red"
    color LightOnOrange = "orange"
    color _             = "#333"

    lightId cid = cid +++ "-light"
    svgLight cid = SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          [defs cid,light cid,glass cid,flare cid]

    defs cid     = DefsElt [] [] [glassgr cid,flaregr cid]
    glassgr cid  = RadialGradientElt [IdAttr (cid +++ "-glass-gradient")] [] [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "100%",StopColorAttr "white",StopOpacityAttr "0"]]
    flaregr cid  = LinearGradientElt [IdAttr (cid +++ "-flare-gradient")] [X1Attr ("0",PX),X2Attr ("0",PX),Y1Attr ("0",PX),Y2Attr ("1",PX)] [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "90%",StopColorAttr "white",StopOpacityAttr "0"]]
    light cid = CircleElt [IdAttr (lightId cid)] [CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX)]
    glass cid = CircleElt [StyleAttr "stroke: #000;stroke-width: 8px"] [FillAttr (PaintFuncIRI (IRI ("#"+++cid+++"-glass-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX)]
    flare cid = EllipseElt [] [FillAttr (PaintFuncIRI (IRI ("#"+++cid+++ "-flare-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("45",PX),RxAttr ("35",PX),RyAttr ("30",PX)]


