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

derive class iTask TonicState, RenderMode

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

//usersForNode :: (Maybe TonicInfo) TonicState -> [User]
//usersForNode (Just renderingNode) (TonicState traces=:[_:_])
  //# traces = takeCurrentTraces traces
  //= nub [ trace.traceUser \\ trace <- traces
        //| not trace.tuneInfo.tu_isBind &&
          //renderingNode.tonicModuleName  == trace.tuneInfo.moduleName  &&
          //renderingNode.tonicTaskName    == trace.tuneInfo.taskName    &&
          //renderingNode.tonicEntryUniqId >= trace.tuneInfo.entryUniqId &&
          //renderingNode.tonicExitUniqId  <= trace.tuneInfo.exitUniqId
        //]
//usersForNode _ _ = []

userNamesAsString [] = ""
userNamesAsString [x:xs] = toString x +++ ", " +++ userNamesAsString xs

userColour :: User -> String
userColour user = userColour` user % (0, 6)
  where
  userColour` (AnonymousUser sid)         = md5 sid
  userColour` SystemUser                  = "ccffcc"
  userColour` (AuthenticatedUser uid _ _) = md5 uid

tracesForUserInstance :: User InstanceNo UserTraceMap -> [TonicTrace]
tracesForUserInstance user instanceNo userInstanceTraceMap =
  case 'DM'.get user userInstanceTraceMap of
    Just instanceTraceMap ->
      case 'DM'.get instanceNo instanceTraceMap of
        Just traces -> traces
        _           -> []
    _ -> []

activeUserTracesMap :: UserTraceMap [InstanceNo] -> Map User [TonicTrace]
activeUserTracesMap utmap activeInstanceNos = 'DM'.foldrWithKey f 'DM'.newMap utmap
  where
  f user instanceTraceMap userTraceMap = 'DM'.put user (flatten [traces \\ Just traces <- ['DM'.get ino instanceTraceMap \\ ino <- activeInstanceNos]]) userTraceMap

activeUserTraces :: UserTraceMap [InstanceNo] -> [TonicTrace]
activeUserTraces utmap activeInstanceNos = 'DM'.foldrWithKey f [] utmap
  where
  f user instanceTraceMap traces = flatten [traces \\ Just traces <- ['DM'.get ino instanceTraceMap \\ ino <- activeInstanceNos]] ++ traces

colours :: {String}
colours =
  { "#730000"
  , "#d96c6c"
  , "#e53d00"
  , "#33170d"
  , "#e6bbac"
  , "#995426"
  , "#ff8800"
  , "#593a16"
  , "#ffe1bf"
  , "#b27700"
  , "#99804d"
  , "#ccbe00"
  , "#caf279"
  , "#44592d"
  , "#cbe6ac"
  , "#338000"
  , "#41f200"
  , "#00e67a"
  , "#86b3a4"
  , "#005947"
  , "#33ccad"
  , "#009ba6"
  , "#1a3133"
  , "#00ccff"
  , "#206080"
  , "#a3bfd9"
  , "#6c98d9"
  , "#001b66"
  , "#334166"
  , "#8080ff"
  , "#3600cc"
  , "#d0bfff"
  , "#c339e6"
  , "#300d33"
  , "#80407b"
  , "#cc99b4"
  , "#735665"
  , "#590024"
  , "#f23d85"
  , "#ff0044"
  }

pickColour :: UserTraceMap User -> String
pickColour utm user = f ('DM'.toList utm) 0
  where
  sz = size colours
  f [] _ = colours.[0]
  f [(u, _):xs] n
    | u === user = colours.[n]
    | otherwise  = if (n < sz)
                     (f xs (n + 1))
                     (f xs 0)

isActiveNode :: (Maybe TonicInfo) TonicState *JSWorld -> *(Bool, *JSWorld)
isActiveNode (Just renderingNode) {traces, renderMode=SingleUser user instanceNo} world
  | 'DM'.empty traces = (False, world)
  | otherwise
      = case tracesForUserInstance user instanceNo traces of
          [] -> (False, world)
          xs -> (isActiveNode` xs, world)
  where
  isActiveNode` [] = False
  isActiveNode` traces=:[_:_]
    # tuneInfo = getNonBindTrace traces
    = not tuneInfo.tu_isBind &&
      renderingNode.tonicModuleName  == tuneInfo.tu_moduleName  &&
      renderingNode.tonicTaskName    == tuneInfo.tu_taskName    &&
      renderingNode.tonicNodeId      == tuneInfo.tu_nodeId
isActiveNode (Just renderingNode) {traces, renderMode=MultiUser instanceNos} world
  | 'DM'.empty traces = (False, world)
  | otherwise
      = case activeUserTraces traces instanceNos of
          [] -> (False, world)
          xs -> (isActiveNode` xs, world)
  where
  isActiveNode` [] = False
  isActiveNode` traces=:[_:_]
    # tuneInfo = getNonBindTrace traces
    = not tuneInfo.tu_isBind &&
      renderingNode.tonicModuleName  == tuneInfo.tu_moduleName  &&
      renderingNode.tonicTaskName    == tuneInfo.tu_taskName    &&
      renderingNode.tonicNodeId      == tuneInfo.tu_nodeId
isActiveNode _ _ world = (False, world)

getNonBindTrace []     = abort "getNonBindTrace: should not happen"
getNonBindTrace [{tr_tuneInfo}:xs]
  | tr_tuneInfo.tu_isBind = getNonBindTrace xs
  | otherwise             = tr_tuneInfo

mkCSSClasses :: Bool String -> String
mkCSSClasses isActive cls = cls +++ if isActive " activeNode" ""

drawNode :: (Maybe TonicState) GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode (Just {traces, renderMode=SingleUser user instanceNo}) shape graph u root world
  # singleUserMap = 'DM'.singleton user (tracesForUserInstance user instanceNo traces)
  = drawNode_ traces singleUserMap shape graph u root world
drawNode (Just {traces, renderMode=MultiUser instanceNos}) shape graph u root world
  = drawNode_ traces (activeUserTracesMap traces instanceNos) shape graph u root world
drawNode _ shape graph u root world
  = drawNode_ 'DM'.newMap 'DM'.newMap shape graph u root world

//drawNode_ :: TonicState GNode GLGraph NodeIndex D3 *JSWorld -> *JSWorld
drawNode_ allTraces userTracesMap shape graph u root world
  # (root`, world) = append "g" root world
  = drawNode` shape graph u root` world
  where
  drawNode` :: GNode GLGraph Int D3 *JSWorld -> *JSWorld
  drawNode` {nodeType=GAssign expr, nodeTonicInfo} _ _ root world
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
  drawNode` {nodeType=GDecision _ expr, nodeTonicInfo} _ nid root world
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
  drawNode` {nodeType=GListComprehension gl, nodeTonicInfo} _ _ root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal "tonic-listcomprehension") g world
    # (app, world)      = append "rect" g world
    # (tg, world)       = append "g" g world
    # (task, world)     = append "text" tg world
    # (args, world)     = append "text" tg world
    # (args, world)     = setAttrs [ ("y", toJSVal "2em")
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
    # (app, world)      = setAttrs [ ("x", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y", toJSVal (0.0 - (th / 2.0)))
                                   , ("rx", toJSVal "5")
                                   , ("ry", toJSVal "5")
                                   , ("width", toJSVal tw)
                                   , ("height", toJSVal th)
                                   ] app world
    # (task, world)     = setAttrs [ ("text-anchor", toJSVal "middle")
                                   , ("y", toJSVal (0.0 - (th / 4.0)))
                                   ] task world
    # (line, world)     = setAttrs [ ("x1", toJSVal (0.0 - (tw / 2.0)))
                                   , ("y1", toJSVal (0.0 - (th / 2.0) + nh))
                                   , ("x2", toJSVal (tw / 2.0))
                                   , ("y2", toJSVal (0.0 - (th / 2.0) + nh))
                                   ] line world
    = world

  drawNode` {nodeType=GReturn expr, nodeTonicInfo} _ nid root world
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
  drawNode` {nodeType=(GStep _), nodeTonicInfo}                   _ _ root world
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
    // TODO Instead of coloring the backgroun, draw coloured squares next to
    // the task application and display the corresponding user name on mouse over
  drawNode` {nodeType=GTaskApp tid exprs, nodeTonicInfo}    _ _ root world
    # (g, world)        = append "g" root world
    # (g, world)        = setAttr "class" (toJSVal "tonic-taskapplication") g world
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
  drawNode` _                       _ _ _    world = world

getBBox root world
  # (elem, world) = firstNode root world
  # (bbox, world) = (elem .# "getBBox" .$ Void) world // callObjectMethod "getBBox" [] elem world
  # (jbbh, world) = .? (bbox .# "height") world
  # (jbbw, world) = .? (bbox .# "width") world
  = ((jsValToReal jbbh, jsValToReal jbbw), world)

drawEdgeLabel :: (Maybe TonicState) GEdge GLGraph EdgeIndex D3 *JSWorld -> *JSWorld
drawEdgeLabel (Just {traces, renderMode=SingleUser user instanceNo}) {edge_pattern} _ (fromIdx, toIdx) root world
  # singleUserMap = 'DM'.singleton user (tracesForUserInstance user instanceNo traces)
  = drawEdgeLabel` traces singleUserMap edge_pattern (fromIdx, toIdx) root world
drawEdgeLabel (Just {traces, renderMode=MultiUser instanceNos}) {edge_pattern} _ (fromIdx, toIdx) root world
  = drawEdgeLabel` traces (activeUserTracesMap traces instanceNos) edge_pattern (fromIdx, toIdx) root world
drawEdgeLabel _ {edge_pattern} _ (fromIdx, toIdx) root world
  = drawEdgeLabel` 'DM'.newMap 'DM'.newMap edge_pattern (fromIdx, toIdx) root world

drawEdgeLabel` allTraces userTracesMap edge_pattern (fromIdx, toIdx) root world
  # (grp, world)          = append "g" root world
  # (grp, world)          = setAttr "class" (toJSVal "edge-label") grp world
  # (rect, world)         = append (toString Rect) grp world
  # (lblSvg, world)       = append "g" grp world
  # (txt, world)          = append "text" lblSvg world
  # (txt, world)          = setAttr "text-anchor" (toJSVal "left") txt world
  # world                 = case fmap (\x -> [x]) edge_pattern of
                              Just strs -> breakText strs txt world
                              _         -> world
  # (rnd, world)          = firstNode root world
  # (bbox, world)         = (rnd .# "getBBox" .$ Void) world // callObjectMethod "getBBox" [] rnd world
  # (bbh, world)          = .? (bbox .# "height") world
  # (bbw, world)          = .? (bbox .# "width") world // jsGetObjectAttr "width" bbox world
  # (bbh, bbw)            = (jsValToReal bbh, jsValToReal bbw)
  # (_, world)            = defaultLabelTransform bbh bbw lblSvg world
  # (_, world)            = setAttrs [ ("rx", toJSVal 5)
                                     , ("ry", toJSVal 5)
                                     , ("x", toJSVal (0.0 - (bbw / 2.0)))
                                     , ("y", toJSVal (0.0 - (bbh / 2.0)))
                                     , ("width", toJSVal bbw)
                                     , ("height", toJSVal bbh)
                                     ] rect world
  # (patsGrp, world)      = append "g" grp world
  # (txt, world)          = firstNode txt world
  # (bbox, world)         = (txt .# "getBBox" .$ Void) world // callObjectMethod "getBBox" [] txt world
  # (bbw, world)          = .? (bbox .# "width") world // jsGetObjectAttr "width" bbox world
  # ((patsGrp, _), world) = 'DM'.foldrWithKey f ((patsGrp, (jsValToReal bbw / bbh) + 0.25), world) userTracesMap
  = world
  where
  f user traces ((patsGrp, n), world)
    # mTuneInfo = edgeInTraces fromIdx toIdx user traces
    = case mTuneInfo of
        Just {tu_nodeId, tu_valAsStr}
          # (box, world)   = append (toString Rect) patsGrp world
          # (box, world)   = setAttr "x" (toJSVal (toString n +++ "em")) box world
          # (box, world)   = setAttr "y" (toJSVal "-0.5em") box world
          # (box, world)   = setAttr "style" (toJSVal ("fill:" +++ pickColour allTraces user)) box world
          # (box, world)   = setAttr "height" (toJSVal "1em") box world
          # (box, world)   = setAttr "width" (toJSVal "1em") box world
          # (title, world) = append "title" box world
          # (_, world)     = setText (maybe "" (\ms -> toString user +++ ": " +++ ms) tu_valAsStr) title world
          = ((patsGrp, n + 1.2), world)
        _ = ((patsGrp, n), world)

edgeInTraces _ _ _ [] = Nothing
edgeInTraces fromIdx toIdx user [{tr_tuneInfo=ti=:{tu_nodeId, tu_valAsStr}, tr_traceUser}:xs]
  | tr_traceUser == user && tu_nodeId == toIdx = Just ti
  | otherwise                                  = edgeInTraces fromIdx toIdx user xs

