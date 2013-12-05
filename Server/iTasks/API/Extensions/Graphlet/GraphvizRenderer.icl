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

breakText :: [String] D3 *JSWorld -> *JSWorld
breakText xs d3 world = foldl f world xs
  where
  f world str
    # (tspan, world) = append "tspan" d3 world
    # (tspan, world) = setAttr "x" (toJSVal "0") tspan world
    # (tspan, world) = setAttr "dy" (toJSVal "1em") tspan world
    # (tspan, world) = setText str tspan world
    = world

// TODO Get rid of copy/paste programming
drawNode :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
drawNode shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  where
  drawNode` :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` (GSBox mstrs) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "box") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "<box>" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSEllipse mstrs)  graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "ellipse") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "ellipse" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "<ellipse>" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("cx", toJSVal 0)
                                 , ("cy", toJSVal 0)
                                 , ("rx", toJSVal bbw)
                                 , ("ry", toJSVal bbh)
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSCircle mstrs)   graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "circle") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "circle" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "<circle>" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` GSPoint           graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "point") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "circle" d3 world
    # (lblSvg, world) = append "g" d3 world
    //# (d3, world)     = append "text" lblSvg world
    //# (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    //# world           = maybe world (\strs -> breakText strs d3 world) mstrs
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText "<point>" d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSTriangle mstrs) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "triangle") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "<triangle>" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSPlainText strs) graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "plaintext") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = breakText strs d3 world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText str d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbh / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbh)
                                 ] rect world
    = world
  drawNode` (GSDiamond mstrs)  graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "diamond") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (rect, world)   = setAttr "transform" (toJSVal ("rotate(45)")) rect world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # bbh             = if (bbh < 10) 10 bbh
    # bbw             = if (bbw < 10) 10 bbw
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbw / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbw)
                                 ] rect world
    = world
  drawNode` (GSSquare mstrs)   graph u root world
    # (d3, world)     = append "g" root world
    # (d3, world)     = setAttr "class" (toJSVal "square") d3 world
    # (ev, world)     = getNodeValue graph (toJSVal u) world
    # (rect, world)   = append "rect" d3 world
    # (lblSvg, world) = append "g" d3 world
    # (d3, world)     = append "text" lblSvg world
    # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
    # world           = case mstrs of
                          Just strs -> breakText strs d3 world
                          _         -> world
    //# (d3, world)     = append "tspan" d3 world
    //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
    //# (d3, world)     = setText (fromMaybe "" mstrs) d3 world
    # (rnd, world)    = firstNode root world
    # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
    # (bbh, world)    = jsGetObjectAttr "height" bbox world
    # (bbw, world)    = jsGetObjectAttr "width" bbox world
    # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
    # (lblSvg, world) = setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) lblSvg world
    # (_, world)      = setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                 , ("y", toJSVal (0 - (bbw / 2)))
                                 , ("width", toJSVal bbw)
                                 , ("height", toJSVal bbw)
                                 ] rect world
    = world
  drawNode` GSNone graph u root world
    # (_, world)     = append "g" root world
    = world
  drawNode` _                 _     _ _    world = world

drawEdgeLabel :: GraphvizEdge GLGraph (Int, Int) D3 *JSWorld -> *JSWorld
drawEdgeLabel Nothing      _     _ _    world = world
drawEdgeLabel (Just strs)  graph e root world
  # (d3, world)     = append "g" root world
  # (d3, world)     = setAttr "class" (toJSVal "edge-label") d3 world
  # (ev, world)     = getEdgeValue graph (toJSVal e) world
  # (rect, world)   = append "rect" d3 world
  # (lblSvg, world) = append "g" d3 world
  # (d3, world)     = append "text" lblSvg world
  # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
  # world           = breakText strs d3 world
  //# (d3, world)     = append "tspan" d3 world
  //# (d3, world)     = setAttr "dy" (toJSVal "1em") d3 world
  //# (d3, world)     = setText str d3 world
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

