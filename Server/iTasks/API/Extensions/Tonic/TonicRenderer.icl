implementation module iTasks.API.Extensions.Tonic.TonicRenderer

import StdOverloaded
import Data.Functor
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Extensions.Graphlet.Graphlet
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.Graphlib

tonicRenderer :: GraphletRenderer GNode GEdge
tonicRenderer =
  { GraphletRenderer
  | drawNodeCallback      = drawNode
  , drawEdgeLabelCallback = drawEdgeLabel
  , styleSheets           = ["/tonic.css"]
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

:: NodeShape = Rect | Ellipse | Circle

instance toString NodeShape where
  toString Rect    = "rect"
  toString Ellipse = "ellipse"
  toString Circle  = "circle"

:: ClassName :== String

:: Transformation :== Int Int D3 *JSWorld -> *(D3, *JSWorld)

defaultLabelTransform :: Transformation
defaultLabelTransform = \bbh bbw d3 world -> setAttr "transform" (toJSVal ("translate(" +++ toString ((0 - bbw) / 2) +++ "," +++ toString ((0 - bbh) / 2) +++ ")")) d3 world

defaultShapeTransform :: Transformation
defaultShapeTransform = \bbh bbw d3 world -> setAttrs [ ("x", toJSVal (0 - (bbw / 2)))
                                                      , ("y", toJSVal (0 - (bbh / 2)))
                                                      , ("width", toJSVal bbw)
                                                      , ("height", toJSVal bbh)
                                                      ] d3 world

toCenteredTspan strs = map f strs
  where
  f str = TspanTag [XAttr "2.5em", DYAttr "1em"] [Text str]

drawNode :: GNode GLGraph Int D3 *JSWorld -> *JSWorld
drawNode shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape.nodeType graph u root` world
  where
  drawNode` :: GNodeType GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` (GAssign expr)          _ _ root world = world
  drawNode` (GDecision _ expr)      _ _ root world = world
  drawNode` GInit                   _ _ root world
    # tag = SvgTag [ ViewBoxAttr "0 0 2 2"
                   , HeightAttr "1.25em"
                   , WidthAttr "1.25em"]
                   [ PolygonTag [ WidthAttr "1em"
                                , HeightAttr "1em"
                                , StyleAttr "fill: #000"
                                , PointsAttr "0,0 2,1 0,2 0,0"
                                ]]
    = snd (appendHtml tag root world)
  drawNode` (GLet gl)               _ _ root world = world
  drawNode` (GListComprehension gl) _ _ root world = world
  drawNode` GParallelSplit          _ _ root world
    # tag = GTag [] [ CircleTag [ CXAttr "2.5em"
                                , CYAttr "2.5em"
                                , RAttr "2.5em"
                                , ClassAttr "parallelsplit"
                                , StrokeDashArrayAttr "5,5"
                                ]
                    , TextTag [ TextAnchorAttr "middle", YAttr "1em"]
                              (toCenteredTspan ["Start", "parallel", "tasks"])
                    ]
    = snd (appendHtml tag root world)
  drawNode` (GParallelJoin jt)      _ _ root world
    # tag = GTag [] [ CircleTag [ CXAttr "2.5em"
                                , CYAttr "2.5em"
                                , RAttr "2.5em"
                                , ClassAttr "paralleljoin"
                                , StrokeDashArrayAttr "5,5"
                                ]
                    , TextTag [ TextAnchorAttr "middle", YAttr "1em"]
                              (case jt of
                                 DisFirstBin  -> toCenteredTspan ["First", "finished", "tasks"]
                                 DisFirstList -> toCenteredTspan ["First", "finished", "tasks"]
                                 DisLeft      -> toCenteredTspan ["Left", "task", "result"]
                                 DisRight     -> toCenteredTspan ["Right", "task", "result"]
                                 ConAll       -> toCenteredTspan ["All", "task", "results"]
                                 ConPair      -> toCenteredTspan ["Paired", "task", "results"]
                              )
                    ]
    = snd (appendHtml tag root world)
  drawNode` (GReturn expr)          _ _ root world = world
  drawNode` GStep                   _ _ root world = world
  drawNode` GStop                   _ _ root world
    # tag = RectTag [WidthAttr "1em", HeightAttr "1em", StyleAttr "fill: #000"]
    = snd (appendHtml tag root world)
  drawNode` (GTaskApp tid exprs)    _ _ root world = world
  drawNode` _                       _ _ _    world = world

drawEdgeLabel :: GEdge GLGraph (Int, Int) D3 *JSWorld -> *JSWorld
drawEdgeLabel {edge_pattern} _ _ root world
  = mkNode Rect "edge-label" (fmap (\x -> [x]) edge_pattern) defaultLabelTransform
      (\bbh bbw d3 world -> setAttrs [ ("rx", toJSVal 5)
                                     , ("ry", toJSVal 5)
                                     , ("x", toJSVal (0 - (bbw / 2)))
                                     , ("y", toJSVal (0 - (bbh / 2)))
                                     , ("width", toJSVal bbw)
                                     , ("height", toJSVal bbh)
                                     ] d3 world
                                     ) root world

