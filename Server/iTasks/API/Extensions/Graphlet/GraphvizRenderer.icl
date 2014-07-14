implementation module iTasks.API.Extensions.Graphlet.GraphvizRenderer

import StdOverloaded
from Data.Graph import :: NodeIndex, :: EdgeIndex
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


:: NodeShape = Rect | Ellipse | Circle

instance toString NodeShape where
  toString Rect    = "rect"
  toString Ellipse = "ellipse"
  toString Circle  = "circle"

:: ClassName :== String

:: Transformation :== Int Int D3 *JSWorld -> *(D3, *JSWorld)

mkNode :: NodeShape ClassName (Maybe [String]) Transformation Transformation D3 *JSWorld -> *JSWorld
mkNode shape clnm mstrs sizeLbl sizeShape root world
  # (d3, world)     = append "g" root world
  # (d3, world)     = setAttr "class" (toJSVal clnm) d3 world
  # (rect, world)   = append (toString shape) d3 world
  # (lblSvg, world) = append "g" d3 world
  # (d3, world)     = append "text" lblSvg world
  # (d3, world)     = setAttr "text-anchor" (toJSVal "left") d3 world
  # world           = case mstrs of
                        Just strs -> breakText strs d3 world
                        _         -> world
  # (rnd, world)    = firstNode root world
  # (bbox, world)   = callObjectMethod "getBBox" [] rnd world
  # (bbh, world)    = jsGetObjectAttr "height" bbox world
  # (bbw, world)    = jsGetObjectAttr "width" bbox world
  # (bbh, bbw)      = (jsValToInt bbh, jsValToInt bbw)
  # (_, world)      = sizeLbl bbh bbw lblSvg world
  # (_, world)      = sizeShape bbh bbw rect world
  = world

defaultLabelTransform :: Transformation
defaultLabelTransform = \bbh bbw d3 world -> setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) d3 world

defaultShapeTransform :: Transformation
defaultShapeTransform = \bbh bbw d3 world -> setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                                      , ("y", toJSVal (0 - (bbh / 2)))
                                                      , ("width", toJSVal bbw)
                                                      , ("height", toJSVal bbh)
                                                      ] d3 world

drawNode :: GraphvizShape GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  where
  drawNode` :: GraphvizShape GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` (GSBox mstrs)      _ _ root world
    = mkNode Rect "box" mstrs
        (\bbh bbw d3 world -> setAttr "transform" (toJSVal ("translate(" +++ toString (((0 - bbw) / 2) + 3) +++ "," +++ toString (((0 - bbh) / 2) + 3) +++ ")")) d3 world)
        (\bbh bbw d3 world -> setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                       , ("y", toJSVal (0 - (bbh / 2)))
                                       , ("width", toJSVal (bbw + 6))
                                       , ("height", toJSVal (bbh + 6))
                                       ] d3 world
                                       ) root world
  drawNode` (GSEllipse mstrs)  _ _ root world
    = mkNode Ellipse "ellipse" mstrs defaultLabelTransform
        (\bbh bbw d3 world -> setAttrs [ ("cx", toJSVal 0)
                                       , ("cy", toJSVal 0)
                                       , ("rx", toJSVal (toReal bbw / 1.25))
                                       , ("ry", toJSVal (toReal bbh / 1.25))
                                       , ("width", toJSVal bbw)
                                       , ("height", toJSVal bbh)
                                       ] d3 world
                                       ) root world
  drawNode` (GSCircle mstrs)   _ _ root world = mkNode Circle "circle" mstrs defaultLabelTransform defaultShapeTransform root world
  drawNode` GSPoint            _ _ root world = mkNode Circle "point" Nothing defaultLabelTransform defaultShapeTransform root world
  drawNode` (GSTriangle mstrs) _ _ root world = mkNode Rect "triangle" mstrs defaultLabelTransform defaultShapeTransform root world
  drawNode` (GSPlainText strs) _ _ root world = mkNode Rect "plaintext" (Just strs) defaultLabelTransform defaultShapeTransform root world
  drawNode` (GSDiamond mstrs)  _ _ root world = mkNode Rect "diamond" mstrs (\bbh bbw -> defaultLabelTransform (tfSz bbh) (tfSz bbw)) tfSp root world
    where
    tfSz n = if (n < 10) 10 n
    tfSp bbh bbw d3 world
      # (root, world) = setAttr "transform" (toJSVal ("rotate(45)")) d3 world
      = defaultShapeTransform (tfSz bbh) (tfSz bbw) root world
  drawNode` (GSSquare mstrs)   _ _ root world = mkNode Rect "square" mstrs defaultLabelTransform defaultShapeTransform root world
  drawNode` GSNone graph u root world
    # (_, world)     = append "g" root world
    = world
  drawNode` _                  _ _ _    world = world

drawEdgeLabel :: GraphvizEdge GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
drawEdgeLabel mstrs _ _ root world
  = mkNode Rect "edge-label" mstrs defaultLabelTransform
      (\bbh bbw d3 world -> setAttrs [ ("rx", toJSVal 5)
                                     , ("ry", toJSVal 5)
                                     , ("x", toJSVal (0 - (bbw / 2)))
                                     , ("y", toJSVal (0 - (bbh / 2)))
                                     , ("width", toJSVal bbw)
                                     , ("height", toJSVal bbh)
                                     ] d3 world
                                     ) root world

