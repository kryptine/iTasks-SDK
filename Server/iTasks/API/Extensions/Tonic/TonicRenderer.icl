implementation module iTasks.API.Extensions.Tonic.TonicRenderer

import StdOverloaded
from StdClass import max
from StdMisc import abort
import Data.Functor
import iTasks.Framework.Tonic
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Extensions.Tonic.Toniclet
import iTasks.API.Extensions.Graphlet.D3
import iTasks.API.Extensions.Graphlet.Graphlib

derive class iTask TonicState, TonicDiff

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

ppGExpression GUndefinedExpression    = "undefined"
ppGExpression (GGraphExpression _)    = "<complex subgraph; consider refactoring>"
ppGExpression (GCleanExpression expr) = expr

isActiveNode :: (Maybe TonicInfo) TonicState *JSWorld -> *(Bool, *JSWorld)
isActiveNode (Just renderingNode) (TonicState xs=:[_:_]) world //[{traceType, tuneInfo}:_]) world
  # tuneInfo = getNonBindTrace xs // TODO Ugh... ugly :(
  # world = if tuneInfo.isBind (jsTrace ("Is Bind" +++
                          toString tuneInfo.moduleName  +++
                          toString tuneInfo.taskName    +++
                          toString tuneInfo.entryUniqId +++
                          toString tuneInfo.exitUniqId
                         ) world)
                        (jsTrace ("Comparing top of trace stack with node being rendered:\n" +++
                     toString tuneInfo.moduleName  +++ " == " +++ toString renderingNode.tonicModuleName  +++ "\n" +++
                     toString tuneInfo.taskName    +++ " == " +++ toString renderingNode.tonicTaskName    +++ "\n" +++
                     toString tuneInfo.entryUniqId +++ " >= " +++ toString renderingNode.tonicEntryUniqId +++ "\n" +++
                     toString tuneInfo.exitUniqId  +++ " <= " +++ toString renderingNode.tonicExitUniqId
                    ) world)
  = ( not tuneInfo.isBind &&
      renderingNode.tonicModuleName  == tuneInfo.moduleName  &&
      renderingNode.tonicTaskName    == tuneInfo.taskName    &&
      renderingNode.tonicEntryUniqId >= tuneInfo.entryUniqId &&
      renderingNode.tonicExitUniqId  <= tuneInfo.exitUniqId
    , world)
  where
  getNonBindTrace []     = abort "getNonBindTrace: should not happen"
  getNonBindTrace [{tuneInfo}:xs]
    | tuneInfo.isBind = getNonBindTrace xs
    | otherwise       = tuneInfo
isActiveNode _ _ world = (False, world)

mkCSSClasses :: Bool String -> String
mkCSSClasses isActive cls = cls +++ if isActive " activeNode" ""

drawNode :: TonicState GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode ts shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  where
  drawNode` :: GNode GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` {nodeType=GAssign expr, nodeTonicInfo} _ _ root world
    # (g, world)    = append "g" root world
    # (ia, world)   = isActiveNode nodeTonicInfo ts world
    # (g, world)    = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-assign")) g world
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
  drawNode` {nodeType=GDecision _ expr, nodeTonicInfo} _ nid root world
    # (g, world)          = append "g" root world
    # (ia, world)         = isActiveNode nodeTonicInfo ts world
    # (g, world)          = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-decision")) g world
    # (path, world)       = append "path" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # (_, world)          = setText expr text world
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "y" (toJSVal (bbh / 4)) text world
    # (path, world)       = addPath [ "M" +++ "-" +++ toString bbh +++ " 0"
                                    , "L" +++ toString (bbw / 2) +++ " " +++ toString (bbh * 2)
                                    , "L" +++ toString (bbw + bbh) +++ " 0"
                                    , "L" +++ toString (bbw / 2) +++ " " +++ " -" +++ toString (bbh * 2)
                                    , "Z"] path world
    # (g, world)          = setAttr "transform" (toJSVal ("translate(" +++ toString (0 - (bbw / 2)) +++ ",0)")) g world
    = world
  drawNode` {nodeType=GInit, nodeTonicInfo} _ _ root world
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
  drawNode` {nodeType=GLet gl, nodeTonicInfo}               _ _ root world
    # (g, world)          = append "g" root world
    # (ia, world)         = isActiveNode nodeTonicInfo ts world
    # (g, world)          = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-let")) g world
    # (rect, world)       = append "rect" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # world               = mkTspans (foldr (\(v, e) xs -> [v +++ " = " +++ e : xs]) [] gl.glet_binds) text world
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "transform" (toJSVal ("translate(" +++ toString (0 - (bbw / 2)) +++ "," +++ toString (0 - (bbh / 2)) +++ ")")) text world
    # (rect, world)       = setAttrs [ ("x", toJSVal (0 - ((bbw + (bbh / 2)) / 2)))
                                     , ("y", toJSVal (0 - (bbh / 2)))
                                     , ("width", toJSVal (bbw + (bbh / 2)))
                                     , ("height", toJSVal bbh)
                                     ] rect world
    = world
  drawNode` {nodeType=GListComprehension gl, nodeTonicInfo} _ _ root world
    # (g, world)        = append "g" root world
    # (ia, world)       = isActiveNode nodeTonicInfo ts world
    # (g, world)        = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-listcomprehension")) g world
    # (app, world)      = append "rect" g world
    # (tg, world)       = append "g" g world
    # (task, world)     = append "text" tg world
    # (args, world)     = append "text" tg world
    # (args, world)     = setAttrs [
                                     ("y", toJSVal "2em")
                                   , ("text-anchor", toJSVal "middle")
                                   ] args world
    # world             = mkTspans [ppGExpression gl.output] args world
    # world             = let xs = [ "for each <TODO>" // +++ gl.selector
                                   , "in <TODO> "] ++ // +++ ppGExpression gl.input] ++
                                   (case gl.guard of
                                      Just grd -> [grd]
                                      _        -> [])
                          in mkTspans xs task world
    # ((nh, nw), world) = getBBox task world
    # ((th, tw), world) = getBBox tg world
    # (line, world)     = append "line" g world
    # (app, world)      = setAttrs [ ("x", toJSVal (0 - (tw / 2)))
                                   , ("y", toJSVal (0 - (th / 2)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal tw)
                                   , ("height", toJSVal th)
                                   ] app world
    # (task, world)     = setAttrs [ ("text-anchor", toJSVal "middle")
                                   , ("y", toJSVal (0 - (th / 4)))
                                   ] task world
    # (line, world)     = setAttrs [ ("x1", toJSVal (0 - (tw / 2)))
                                   , ("y1", toJSVal (0 - (th / 2) + nh))
                                   , ("x2", toJSVal (tw / 2))
                                   , ("y2", toJSVal (0 - (th / 2) + nh))
                                   ] line world
    = world
  drawNode` {nodeType=GParallelSplit, nodeTonicInfo}          _ _ root world
    # (g, world)    = append "g" root world
    # (ia, world)   = isActiveNode nodeTonicInfo ts world
    # (g, world)    = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-parallelsplit")) g world
    # (poly, world) = append "circle" g world
    # (poly, world) = setAttrs [ ("r", toJSVal "2.5em")
                               , ("stroke-dasharray", toJSVal "5,5")
                               ] poly world
    # (text, world) = append "text" g world
    # (text, world) = setAttrs [ ("text-anchor", toJSVal "middle")
                               , ("y", toJSVal "-1.75em")
                               ] text world
    = mkCenteredTspan ["Start", "parallel", "tasks"] text world
  drawNode` {nodeType=GParallelJoin jt, nodeTonicInfo}      _ _ root world
    # (g, world)    = append "g" root world
    # (ia, world)   = isActiveNode nodeTonicInfo ts world
    # (g, world)    = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-paralleljoin")) g world
    # (poly, world) = append "circle" g world
    # (poly, world) = setAttrs [ ("r", toJSVal "2.5em")
                               , ("stroke-dasharray", toJSVal "5,5")
                               ] poly world
    # (text, world) = append "text" g world
    # (text, world) = setAttrs [ ("text-anchor", toJSVal "middle")
                               , ("y", toJSVal "-1.75em")
                               ] text world
    = mkCenteredTspan (case jt of
                         DisFirstBin  -> ["First", "task", "result"]
                         DisFirstList -> ["First", "task", "result"]
                         DisLeft      -> ["Left", "task", "result"]
                         DisRight     -> ["Right", "task", "result"]
                         ConAll       -> ["All", "task", "results"]
                         ConPair      -> ["Paired", "task", "results"]
                      ) text world
  drawNode` {nodeType=GReturn expr, nodeTonicInfo} _ nid root world
    # (g, world)          = append "g" root world
    # (ia, world)         = isActiveNode nodeTonicInfo ts world
    # (g, world)          = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-return")) g world
    # (rect, world)       = append "ellipse" g world
    # (g``, world)        = append "g" g world
    # (text, world)       = append "text" g`` world
    # (_, world)          = setText (ppGExpression expr) text world
    # ((bbh, bbw), world) = getBBox root world
    # (text, world)       = setAttr "transform" (toJSVal ("translate(" +++ toString (0 - (bbw / 2)) +++ "," +++ toString (bbh / 4) +++ ")")) text world
    # (rect, world)       = setAttrs [ ("rx", toJSVal ((bbw / 2) + bbh))
                                     , ("ry", toJSVal bbh)
                                     ] rect world
    = world
  drawNode` {nodeType=GStep, nodeTonicInfo}                   _ _ root world
    = world
  drawNode` {nodeType=GStop, nodeTonicInfo} _ _ root world
    # (g, world)    = append "g" root world
    # (g, world)    = setAttr "class" (toJSVal "tonic-stop") g world
    # (stop, world) = append "rect" g world
    # (_, world)    = setAttrs [ ("x", toJSVal "-0.5em")
                               , ("y", toJSVal "-0.5em")
                               , ("width", toJSVal "1em")
                               , ("height", toJSVal "1em") ] stop world
    = world
  drawNode` {nodeType=GTaskApp tid exprs, nodeTonicInfo}    _ _ root world
    # (g, world)        = append "g" root world
    # (ia, world)       = isActiveNode nodeTonicInfo ts world
    # (g, world)        = setAttr "class" (toJSVal (mkCSSClasses ia "tonic-taskapplication")) g world
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
    # (app, world)      = setAttrs [ ("x", toJSVal (0 - (tw / 2)))
                                   , ("y", toJSVal (0 - (th / 2)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal tw)
                                   , ("height", toJSVal th)
                                   ] app world
    # (task, world)     = setAttrs [ ("text-anchor", toJSVal "middle")
                                   , ("y", toJSVal (0 - (th / 2) + 12)) // TODO Remove hardcoding of 12
                                   ] task world
    # (line, world)     = setAttrs [ ("x1", toJSVal (0 - (tw / 2)))
                                   , ("y1", toJSVal (0 - (th / 2) + nh))
                                   , ("x2", toJSVal (tw / 2))
                                   , ("y2", toJSVal (0 - (th / 2) + nh))
                                   ] line world
    # (args, world)     = setAttrs [ ("text-anchor", toJSVal "left")
                                   , ("y", toJSVal (0 - (th / 4)))
                                   ] args world
    # (args`, world)    = selectAllChildElems args "tspan" world
    # (args`, world)    = setAttr "x" (toJSVal (0 - (tw / 2))) args` world
    = world
  drawNode` _                       _ _ _    world = world

getBBox root world
  # (elem, world) = firstNode root world
  # (bbox, world) = callObjectMethod "getBBox" [] elem world
  # (jbbh, world) = jsGetObjectAttr "height" bbox world
  # (jbbw, world) = jsGetObjectAttr "width" bbox world
  = ((jsValToInt jbbh, jsValToInt jbbw), world)

drawEdgeLabel :: TonicState GEdge GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
drawEdgeLabel (TonicState []) {edge_pattern} _ (fromIdx, toIdx) root world = drawEdgeLabel` edge_pattern root world
drawEdgeLabel (TonicState [{tuneInfo={entryUniqId,exitUniqId,valAsStr}}:_]) {edge_pattern} _ (fromIdx, toIdx) root world
  | entryUniqId == fromIdx && exitUniqId == toIdx = drawEdgeLabel` (fmap (\x -> x +++ (maybe "" (\ms -> " [" +++ ms +++ "]") valAsStr)) edge_pattern) root world
  | otherwise                                     = drawEdgeLabel` edge_pattern root world

drawEdgeLabel` :: (Maybe String) D3 *JSWorld -> *JSWorld
drawEdgeLabel` pat root world
  = mkNode Rect "edge-label" (fmap (\x -> [x]) pat) defaultLabelTransform
      (\bbh bbw d3 world -> setAttrs [ ("rx", toJSVal 5)
                                     , ("ry", toJSVal 5)
                                     , ("x", toJSVal (0 - (bbw / 2)))
                                     , ("y", toJSVal (0 - (bbh / 2)))
                                     , ("width", toJSVal bbw)
                                     , ("height", toJSVal bbh)
                                     ] d3 world
                                     ) root world
