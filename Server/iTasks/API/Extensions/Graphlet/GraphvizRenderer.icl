implementation module iTasks.API.Extensions.Graphlet.GraphvizRenderer

import iTasks.API.Extensions.Graphlet.Graphlet
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.Graphlib

derive class iTask GraphvizShape

graphvizRenderer :: GraphletRenderer GraphvizShape GraphvizEdge
graphvizRenderer =
  { GraphletRenderer
  | drawNodeCallback      = drawNode
  , drawEdgeLabelCallback = drawEdgeLabel
  , styleSheets           = ["/graphviz.css"]
  }

// TODO Get rid of copy/paste programming
drawNode :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
drawNode shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  //where
  //drawNode` :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
  //drawNode` _ graph u root world
    //# (d3, world)     = append "g" root world
    //# (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    //# (ev, world)     = getNodeValue graph (toJSVal u) world
    //# (rect, world)   = append "rect" d3 world
    //# (lblSvg, world) = append "g" d3 world
    //# (d3, world)     = append "text" lblSvg world
    //# (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "<box>" Nothing) d3 world
    //# (rnd, world)    = firstNode root world
    //# (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    //# (bbh, world)    = jsGetObjectAttr "height" bbox world
    //# (bbw, world)    = jsGetObjectAttr "width" bbox world
    //# (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    //# (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    //# (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 //, ("ry", toJSVal 5)
                                 //, ("x", toJSVal (0 - (bbw / 2)))
                                 //, ("y", toJSVal (0 - (bbh / 2)))
                                 //, ("width", toJSVal bbw)
                                 //, ("height", toJSVal bbh)
                                 //] rect world
    //= world

  where
  drawNode` :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` (GSBoxShape mstr) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<box>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSEllipse mstr)  graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<ellipse>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSOval mstr)     graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<oval>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSCircle mstr)   graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<circle>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` GSPoint           graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText "<point>" d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSTriangle mstr) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<triangle>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSPlainText str) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText str d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSDiamond mstr)  graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<diamond>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSSquare mstr)   graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "label") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # (d3, world)     = append "tspan" d3 world
    # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    # (d3, world)     = setText (fromMaybe "<square>" mstr) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                                 , ("ry", toJSVal 5)
                                 , ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` _                 _     _ _    world = world

drawEdgeLabel :: GraphvizEdge GLGraph (Int, Int) D3 *JSWorld -> *JSWorld
drawEdgeLabel Nothing     _     _ _    world = world
drawEdgeLabel (Just str)  graph e root world
  # (d3, world)     = append "g" root world
  # (d3, world)     = setAttr "class" (toJSVal "edge-label") d3 world
  # (ev, world)     = getEdgeValue graph (toJSVal e) world
  # (rect, world)   = append "rect" d3 world
  # (lblSvg, world) = append "g" d3 world
  # (d3, world)     = append "text" lblSvg world
  # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
  # (d3, world)     = append "tspan" d3 world
  # (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
  # (d3, world)     = setText str d3 world
  # (rnd, world)    = firstNode root world
  # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
  # (bbh, world)    = jsGetObjectAttr "height" bbox world
  # (bbw, world)    = jsGetObjectAttr "width" bbox world
  # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
  # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
  # (_, world)      = setAttrs [ ("rx", toJSVal 5)
                               , ("ry", toJSVal 5)
                               , ("x", toJSVal (0 - (bbw / 2)))
                               , ("y", toJSVal (0 - (bbh / 2)))
                               , ("width", toJSVal bbw)
                               , ("height", toJSVal bbh)
                               ] rect world
  = world

