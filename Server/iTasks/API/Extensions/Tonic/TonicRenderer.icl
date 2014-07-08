implementation module iTasks.API.Extensions.Tonic.TonicRenderer

import StdOverloaded, StdArray, StdReal
from StdClass import max
from StdMisc import abort
import Data.Functor, Data.List
from Crypto.Hash.MD5 import md5
import iTasks.Framework.Tonic
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Extensions.Tonic.Toniclet
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.Graphlib
import qualified Data.Map as DM

tonicRenderer :: TonicletRenderer
tonicRenderer =
  { TonicletRenderer
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
  # (bbh, bbw)      = (jsValToReal bbh, jsValToReal bbw)
  # (_, world)      = sizeLbl bbh bbw lblSvg world
  # (_, world)      = sizeShape bbh bbw rect world
  = world

:: NodeShape = Rect | Ellipse | Circle

instance toString NodeShape where
  toString Rect    = "rect"
  toString Ellipse = "ellipse"
  toString Circle  = "circle"

:: ClassName :== String

:: Transformation :== Real Real D3 *JSWorld -> *(D3, *JSWorld)

defaultLabelTransform :: Transformation
defaultLabelTransform = \bbh bbw d3 world -> setAttr "transform" (toJSVal ("translate(" +++ toString ((0.0 - bbw) / 2.0) +++ "," +++ toString ((0.0 - bbh) / 2.0) +++ ")")) d3 world

defaultShapeTransform :: Transformation
defaultShapeTransform = \bbh bbw d3 world -> setAttrs [ ("x", toJSVal (0.0 - (bbw / 2.0)))
                                                      , ("y", toJSVal (0.0 - (bbh / 2.0)))
                                                      , ("width", toJSVal bbw)
                                                      , ("height", toJSVal bbh)
                                                      ] d3 world

mkCenteredTspan strs root world = foldl f world strs
  where
  f world str
    # (tsp, world) = append "tspan" root world
    # (tsp, world) = setAttrs [ ("x", toJSVal "0")
                              , ("dy", toJSVal "1em")
                              ] tsp world
    # (_, world)   = setText str tsp world
    = world

mkTspans strs root world = foldl f world strs
  where
  f world str
    # (tsp, world) = append "tspan" root world
    # (tsp, world) = setAttrs [ ("x", toJSVal "0")
                              , ("dy", toJSVal "1em")
                              ] tsp world
    # (_, world)   = setText str tsp world
    = world

mkLines xs root world = foldr f world xs
  where
  f (x1, y1, x2, y2) world
    # (line, world) = append "line" root world
    # (line, world) = setAttrs [ ("x1", toJSVal x1)
                               , ("y1", toJSVal y1)
                               , ("x2", toJSVal x2)
                               , ("y2", toJSVal y2)
                               ] line world
    = world

addPath xs path world
  # pts           = foldl (\x xs -> x +++ xs) "" xs
  # (path, world) = setAttr "d" (toJSVal pts) path world
  = (path, world)

ppGExpression e = e 

ppNodeContents (VarOrExpr expr) = expr
ppNodeContents ArbitraryOrUnknownExpr = "?"
ppNodeContents (Subgraph _) = "TODO PP Subgraph"

mkCSSClasses :: Bool String -> String
mkCSSClasses isActive cls = cls +++ if isActive " activeNode" ""

drawArbitrary g world
  # (app, world)      = append "rect" g world
  # (tg, world)       = append "g" g world
  # (task, world)     = append "text" tg world
  # (task, world)     = setText "?" task world
  # (app, world)      = setAttrs [ ("x", toJSVal (-12.5))
                                 , ("y", toJSVal (-12.5))
                                 , ("rx", toJSVal "5")
                                 , ("ry", toJSVal "5")
                                 , ("width", toJSVal 25)
                                 , ("height", toJSVal 25)
                                 ] app world
  = world

drawVar g expr world
  # (app, world)        = append "rect" g world
  # (tg, world)         = append "g" g world
  # (var, world)        = append "text" tg world
  # (var, world)        = setText expr var world
  # (line, world)       = append "line" g world
  # ((bbh, bbw), world) = getBBox g world
  # (var, world)        = setAttrs [ ("x", toJSVal (0.0 - (bbw / 2.0)))
                                   , ("y", toJSVal (bbh / 4.0))
                                   ] var world
  # (app, world)        = setAttrs [ ("x", toJSVal (0.0 - (bbw / 2.0)))
                                   , ("y", toJSVal (0.0 - (bbh / 2.0)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal bbw)
                                   , ("height", toJSVal bbh)
                                   ] app world
  = world

drawNode :: Bool GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode active shape graph u root world
  = drawNode_ active shape graph u root world

drawNode_ :: Bool GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode_ active shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  where
  drawNode` :: GNode GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` {nodeType=GVar expr} _ nid root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal "tonic-var") g world
    = drawVar g expr world
  drawNode` {nodeType=GArbitraryExpression} _ nid root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal "tonic-arbitrary") g world
    = drawArbitrary g world
  drawNode` {nodeType=GAssign expr (Subgraph sub)} _ _ root world // TODO Draw subgraph
    # (g, world)    = append "g" root world
    # (g, world)    = setAttr "class" (toJSVal "tonic-assign") g world
    # (head, world) = append "circle" g world
    # (head, world) = setAttrs [ ("cx", toJSVal "1em")
                               , ("cy", toJSVal "1em")
                               , ("r", toJSVal "1em")
                               ] g world
    # world         = mkLines [ ("0em", "2.5em", "2em", "2.5em")
                              , ("1em", "2em",   "1em", "3.5em")
                              , ("1em", "3.5em", "0em", "5em")
                              , ("1em", "3.5em", "2em", "5em")
                              ] g world
    # (text, world) = append "text" g world
    # (text, world) = setAttrs [ ("text-anchor", toJSVal "middle")
                               , ("x", toJSVal "1em")
                               , ("y", toJSVal "6em")
                               ] text world
    # (_, world)    = setText expr text world
    = world
  drawNode` {nodeType=GDecision _ expr} _ nid root world
    # (g, world)          = append "g" root world
    # (g, world)          = setAttr "class" (toJSVal "tonic-decision") g world
    # (path, world)       = append "path" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # (_, world)          = setText expr text world
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "y" (toJSVal (bbh / 4.0)) text world
    # (path, world)       = addPath [ "M" +++ "-" +++ toString bbh +++ " 0"
                                    , "L" +++ toString (bbw / 2.0) +++ " " +++ toString (bbh * 2.0)
                                    , "L" +++ toString (bbw + bbh) +++ " 0"
                                    , "L" +++ toString (bbw / 2.0) +++ " " +++ " -" +++ toString (bbh * 2.0)
                                    , "Z"] path world
    # (g, world)          = setAttr "transform" (toJSVal ("translate(" +++ toString (0.0 - (bbw / 2.0)) +++ ",0)")) g world
    = world
  drawNode` {nodeType=GInit} _ _ root world
    # (g, world)    = append "g" root world
    # (g, world)    = setAttr "class" (toJSVal "tonic-init") g world
    # (svg, world)  = append "svg" g world
    # (svg, world)  = setAttrs [ ("viewBox", toJSVal "0 0 2 2")
                               , ("height", toJSVal "1.25em")
                               , ("width", toJSVal "1.25em")
                               , ("x", toJSVal "-0.6125em")
                               , ("y", toJSVal "-0.6125em")
                               ] svg world
    # (poly, world) = append "polygon" svg world
    # (poly, world) = setAttrs [ ("width", toJSVal "1em")
                               , ("height", toJSVal "1em")
                               , ("class", toJSVal "init-poly")
                               , ("points", toJSVal "0,0 2,1 0,2 0,0")
                               ] poly world
    = world
  drawNode` {nodeType=GLet gl}               _ _ root world
    # (g, world)          = append "g" root world
    # (g, world)          = setAttr "class" (toJSVal "tonic-let") g world
    # (rect, world)       = append "rect" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # world               = mkTspans (foldr (\(v, e) xs -> [v +++ " = " +++ e : xs]) [] gl.glet_binds) text world
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "transform" (toJSVal ("translate(" +++ toString (0.0 - (bbw / 2.0)) +++ "," +++ toString (0.0 - (bbh / 2.0)) +++ ")")) text world
    # (rect, world)       = setAttrs [ ("x", toJSVal (0.0 - ((bbw + (bbh / 2.0)) / 2.0)))
                                     , ("y", toJSVal (0.0 - (bbh / 2.0)))
                                     , ("width", toJSVal (bbw + (bbh / 2.0)))
                                     , ("height", toJSVal bbh)
                                     ] rect world
    = world

  drawNode` {nodeType=GReturn expr} _ nid root world
    # (g, world)          = append "g" root world
    # (g, world)          = setAttr "class" (toJSVal "tonic-return") g world
    # (rect, world)       = append "ellipse" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # (_, world)          = setText (ppNodeContents expr) text world // TODO Draw entire subgraph
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "transform" (toJSVal ("translate(" +++ toString (0.0 - (bbw / 2.0)) +++ "," +++ toString (bbh / 4.0) +++ ")")) text world
    # (rect, world)       = setAttrs [ ("rx", toJSVal ((bbw / 2.0) + bbh))
                                     , ("ry", toJSVal bbh)
                                     ] rect world
    = world
  drawNode` {nodeType=GTransform expr} _ nid root world
    # (g, world)          = append "g" root world
    # (g, world)          = setAttr "class" (toJSVal "tonic-transform") g world
    # (rect, world)       = append "rect" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # (_, world)          = setText expr text world
    # ((bbh, bbw), world) = getBBox root world
    # ((th, tw), world)   = getBBox text world
    # (text, world)       = setAttr "transform" (toJSVal ("translate(" +++ toString (0.0 - (bbw / 2.0)) +++ "," +++ toString (bbh / 4.0) +++ ")")) text world
    # (rect, world)       = setAttrs [ ("x", toJSVal (0.0 - (tw / 2.0)))
                                     , ("y", toJSVal (0.0 - (th / 2.0)))
                                     , ("rx", toJSVal "5")
                                     , ("ry", toJSVal "5")
                                     , ("width", toJSVal tw)
                                     , ("height", toJSVal th)
                                     ] rect world
    = world
  drawNode` {nodeType=(GStep ndcs)} _ _ root world
    # (g, world)    = append "g" root world
    # (g, world)    = setAttr "class" (toJSVal "tonic-step") g world
    = foldr (drawStep g) world ndcs
    where
    drawStep g (VarOrExpr str) world = drawVar g str world
    drawStep g ArbitraryOrUnknownExpr world = drawArbitrary g world
    drawStep g (Subgraph _) world = world
    drawStep g (StepElem sel) world = drawStepElem g sel world
      where
      drawStepElem g (StepOnValue cont) world = drawCont cont world
      drawStepElem g (StepOnButton str cont) world
        # (btnGrp, world)  = append "g" g world
        # (btnRect, world) = append "rect" btnGrp world
        # (txt, world)     = append "text" g world
        # (_, world)       = setText str txt world
        = drawCont cont world
      drawStepElem g (StepOnException cont) world = drawCont cont world
      drawCont {stepContFilter, stepContLbl, stepContNode} world
        # world = drawFilter stepContFilter world
        # world = case stepContLbl of
                    Just lbl
                      # (txt, world) = append "text" g world
                      # (_, world)   = setText lbl txt world
                      = world
                    _ = world
        = world // TODO draw stepcondnode
      drawFilter StepAlways world = world
      drawFilter StepNever world = world
      drawFilter StepHasValue world = world
      drawFilter StepIfStable world = world
      drawFilter StepIfUnstable world = world
      drawFilter StepIfValue world = world
      drawFilter (StepCond str) world = world
  drawNode` {nodeType=GStop} _ _ root world
    # (g, world)    = append "g" root world
    # (g, world)    = setAttr "class" (toJSVal "tonic-stop") g world
    # (stop, world) = append "rect" g world
    # (_, world)    = setAttrs [ ("x", toJSVal "-0.5em")
                               , ("y", toJSVal "-0.5em")
                               , ("width", toJSVal "1em")
                               , ("height", toJSVal "1em") ] stop world
    = world
    // TODO Instead of coloring the backgroun, draw coloured squares next to
    // the task application and display the corresponding user name on mouse over
  drawNode` {nodeType=GTaskApp tid exprs}    _ _ root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal ( mkCSSClasses active "tonic-taskapplication")) g world
    # (app, world)      = append "rect" g world
    # (tg, world)       = append "g" g world
    # (task, world)     = append "text" tg world
    # (task, world)     = setText tid task world
    # ((nh, nw), world) = getBBox task world
    # (args, world)     = append "text" tg world
    # (args, world)     = setAttrs [
                                    ("y", toJSVal "0.35em")
                                   ] args world
    # world             = mkTspans (map ppGExpression exprs) args world
    # ((ah, aw), world) = getBBox args world
    # ((th, tw), world) = getBBox tg world
    # (line, world)     = append "line" g world
    # (app, world)      = setAttrs [ ("x", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y", toJSVal (0.0 - (th / 2.0)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal tw)
                                   , ("height", toJSVal th)
                                   ] app world
    # (task, world)     = setAttrs [ ("text-anchor", toJSVal "middle")
                                   , ("y", toJSVal (0.0 - (th / 2.0) + 12.0)) // TODO Remove hardcoding of 12
                                   ] task world
    # (line, world)     = setAttrs [ ("x1", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y1", toJSVal (0.0 - (th / 2.0) + nh))
                                   , ("x2", toJSVal (tw / 2.0))
                                   , ("y2", toJSVal (0.0 - (th / 2.0) + nh))
                                   ] line world
    # (args, world)     = setAttrs [ ("text-anchor", toJSVal "left")
                                   , ("y", toJSVal (0.0 - (th / 4.0)))
                                   ] args world
    # (args`, world)    = selectAllChildElems args "tspan" world
    # (args`, world)    = setAttr "x" (toJSVal (0.0 - (tw / 2.0))) args` world
    = world
  drawNode` {nodeType=GParallel parType contents}    _ _ root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal "tonic-parallel") g world
    # (app, world)      = append "rect" g world
    # (tg, world)       = append "g" g world
    # (task, world)     = append "text" tg world
    # (task, world)     = setText (ppParType parType) task world
    # ((nh, nw), world) = getBBox task world
    # (args, world)     = append "text" tg world
    # (args, world)     = setAttrs [
                                    ("y", toJSVal "0.35em")
                                   ] args world
    //# world             = mkTspans (map ppGExpression exprs) args world // TODO Render the rest
    # ((ah, aw), world) = getBBox args world
    # ((th, tw), world) = getBBox tg world
    # (line, world)     = append "line" g world
    # (app, world)      = setAttrs [ ("x", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y", toJSVal (0.0 - (th / 2.0)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal tw)
                                   , ("height", toJSVal th)
                                   ] app world
    # (task, world)     = setAttrs [ ("text-anchor", toJSVal "middle")
                                   , ("y", toJSVal (0.0 - (th / 2.0) + 12.0)) // TODO Remove hardcoding of 12
                                   ] task world
    # (line, world)     = setAttrs [ ("x1", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y1", toJSVal (0.0 - (th / 2.0) + nh))
                                   , ("x2", toJSVal (tw / 2.0))
                                   , ("y2", toJSVal (0.0 - (th / 2.0) + nh))
                                   ] line world
    # (args, world)     = setAttrs [ ("text-anchor", toJSVal "left")
                                   , ("y", toJSVal (0.0 - (th / 4.0)))
                                   ] args world
    # (args`, world)    = selectAllChildElems args "tspan" world
    # (args`, world)    = setAttr "x" (toJSVal (0.0 - (tw / 2.0))) args` world
    = world
  drawNode` _                       _ _ _    world = world

ppParType DisFirstBin  = "Parallel: first finished task"
ppParType DisFirstList = "Parallel: first finished task"
ppParType DisLeft      = "Parallel: left result"
ppParType DisRight     = "Parallel: right result"
ppParType ConAll       = "Parallel: all task results"
ppParType ConPair      = "Parallel: both task results"

getBBox root world
  # (elem, world) = firstNode root world
  # (bbox, world) = (elem .# "getBBox" .$ Void) world // callObjectMethod "getBBox" [] elem world
  # (jbbh, world) = .? (bbox .# "height") world
  # (jbbw, world) = .? (bbox .# "width") world
  = ((jsValToReal jbbh, jsValToReal jbbw), world)

drawEdgeLabel :: GEdge GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
drawEdgeLabel {edge_pattern} _ (fromIdx, toIdx) root world
  = drawEdgeLabel` edge_pattern (fromIdx, toIdx) root world

drawEdgeLabel` :: (Maybe String) EdgeIndex D3 *JSWorld -> *JSWorld
drawEdgeLabel` edge_pattern (fromIdx, toIdx) root world
  # (grp, world)     = append "g" root world
  # (grp, world)     = setAttr "class" (toJSVal "edge-label") grp world
  # (rect, world)    = append (toString Rect) grp world
  # (lblSvg, world)  = append "g" grp world
  # (txt, world)     = append "text" lblSvg world
  # (txt, world)     = setAttr "text-anchor" (toJSVal "left") txt world
  # world            = case fmap (\x -> [x]) edge_pattern of
                         Just strs -> breakText strs txt world
                         _         -> world
  # (rnd, world)     = firstNode root world
  # (bbox, world)    = (rnd .# "getBBox" .$ Void) world // callObjectMethod "getBBox" [] rnd world
  # (bbh, world)     = .? (bbox .# "height") world
  # (bbw, world)     = .? (bbox .# "width") world // jsGetObjectAttr "width" bbox world
  # (bbh, bbw)       = (jsValToReal bbh, jsValToReal bbw)
  # (_, world)       = defaultLabelTransform bbh bbw lblSvg world
  # (_, world)       = setAttrs [ ("rx", toJSVal 5)
                                , ("ry", toJSVal 5)
                                , ("x", toJSVal (0.0 - (bbw / 2.0)))
                                , ("y", toJSVal (0.0 - (bbh / 2.0)))
                                , ("width", toJSVal bbw)
                                , ("height", toJSVal bbh)
                                ] rect world
  = world
