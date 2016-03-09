implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import Graphics.Scalable.Internal
import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Types
from StdOrdList import minList, maxList
import StdOverloaded
import StdArray
import StdMisc
import Data.Array
import Data.List
import Data.Func
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Set as DS
from StdFunc import `bind`, flip
import Text
from Data.IntMap.Strict import :: IntMap, instance Functor IntMap
import qualified Data.IntMap.Strict as DIS
import Data.Matrix

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr, Angle

:: *GenSVGStVal s =
  { uniqueIdCounter :: !Int
  , genStates       :: !*SpanEnvs
  }

:: DropTarget = DropTarget

:: SVGClSt s v =
  { svgNumClicks    :: !Int
  , svgClickTimeout :: !Maybe (JSVal Int)
  , svgClSt         :: !v
  , svgClSrvSt      :: !s
  , svgMousePos     :: !MousePos
  , svgDropCallback :: !Maybe ((Maybe (Set ImageTag)) Real Real v -> v)
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe (JSObj DropTarget)
  }

mainSvgId :: !ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

mkMouseDragDown :: !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                   !ComponentId ((Maybe (Set ImageTag)) Real Real v -> v)
                   !String !(JSObj o) String {JSObj JSEvent} !(SVGClSt s v)
                   !*JSWorld
                -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
mkMouseDragDown resolve state2image updSrv updClient cid sttf elemId _ _ evts=:{[0] = evt} clval world
  #! (svgContainer, world)  = .? (getElementById (mainSvgId cid)) world
  #! (svgRoot, world)       = .? (svgContainer .# "firstChild") world
  #! (targetElement, world) = (svgRoot .# "getElementById" .$ elemId) world
  #! (_, world)             = (targetElement .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world

  #! (boundingRect, world)  = (targetElement .# "getBoundingClientRect" .$ ()) world
  #! (left, world)          = .? (boundingRect .# "left") world
  #! (top, world)           = .? (boundingRect .# "top") world
  #! (p, world)             = (svgRoot `createSVGPoint` ()) world
  #! world                  = (p .# "x" .= left) world
  #! world                  = (p .# "y" .= top) world
  #! (m, world)             = (svgRoot `getScreenCTM` ()) world
  #! (inv, world)           = (m `inverse` ()) world
  #! (p, world)             = (p `matrixTransform` inv) world
  #! (px, world)            = .? (p .# "x") world
  #! (py, world)            = .? (p .# "y") world
  #! (e, f)                 = (jsValToReal px, jsValToReal py)
  = ({clval & svgDropCallback = Just sttf, svgMousePos = MouseDown
            , svgDragTarget = Just targetElement, svgGrabPointX = clval.svgTrueCoordsX - e, svgGrabPointY = clval.svgTrueCoordsY - f}, NoDiff, world)

mkMouseDragMove :: !ComponentId (JSObj o) String !{JSObj JSEvent} !(SVGClSt s v)
                   !*JSWorld
                -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
mkMouseDragMove cid _ _ evts=:{[0] = evt} clval=:{svgMousePos = MouseDown, svgDragTarget = Just dragTarget} world
  // Append the dragTarget to the root of the SVG element for two reasons:
  //   1. To allow it to be dragged over all other elements
  //   2. To not be bothered by the offsets of one or more groups it might initially be in
  #! (svgContainer, world) = .? (getElementById (mainSvgId cid)) world
  #! (svgRoot, world)      = .? (svgContainer .# "firstChild") world
  #! (_, world)             = (svgRoot `appendChild` dragTarget) world

  #! (newTrueCoordsX, newTrueCoordsY, world) = getNewTrueCoords cid evt world
  #! newX       = newTrueCoordsX - clval.svgGrabPointX
  #! newY       = newTrueCoordsY - clval.svgGrabPointY
  #! (_, world) = (dragTarget `setAttribute` ("transform", "translate(" +++ toString newX +++ "," +++ toString newY +++ ")")) world
  = ({clval & svgTrueCoordsX = newTrueCoordsX
            , svgTrueCoordsY = newTrueCoordsY}, NoDiff, world)
mkMouseDragMove cid _ _ evts=:{[0] = evt} clval world
  #! (newTrueCoordsX, newTrueCoordsY, world) = getNewTrueCoords cid evt world
  = ({clval & svgTrueCoordsX = newTrueCoordsX
            , svgTrueCoordsY = newTrueCoordsY}, NoDiff, world)

getNewTrueCoords :: !ComponentId !(JSObj JSEvent) !*JSWorld
                 -> *(!Real, !Real, !*JSWorld)
getNewTrueCoords cid evt world
  #! (svgContainer, world) = .? (getElementById (mainSvgId cid)) world
  #! (svgRoot, world)      = .? (svgContainer .# "firstChild") world
  #! (newScale, world)     = .? (svgRoot .# "currentScale") world
  #! newScale              = jsValToReal newScale
  #! (translation, world)  = .? (svgRoot .# "currentTranslate") world
  #! (translationX, world) = .? (translation .# "x") world
  #! (translationY, world) = .? (translation .# "y") world
  #! (translationX, translationY) = (jsValToReal translationX, jsValToReal translationY)
  #! (clientX, world)      = .? (evt .# "clientX") world
  #! (clientY, world)      = .? (evt .# "clientY") world
  #! (clientX, clientY)    = (jsValToReal clientX, jsValToReal clientY)
  #! newTrueCoordsX        = (clientX - translationX) / newScale
  #! newTrueCoordsY        = (clientY - translationY) / newScale
  = (newTrueCoordsX, newTrueCoordsY, world)

mkMouseDragUp :: !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                 !ComponentId !(JSObj o) !(Map String (Set ImageTag)) String
                 {JSObj JSEvent} !(SVGClSt s v) !*JSWorld
              -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
mkMouseDragUp resolve state2image updSrv updClient cid _ idMap _ evts=:{[0] = evt} clval=:{svgClSt, svgClSrvSt, svgDragTarget = Just dragTarget} world
  #! (_, world)         = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (evtTarget, world) = .? (evt .# "target") world
  #! (parentId, world)  = firstIdentifiableParentId evtTarget world
  #! (diff, srvSt`)     = case clval.svgDropCallback of
                            Just sttf
                              #! xdiff  = clval.svgTrueCoordsX - clval.svgGrabPointX
                              #! ydiff  = clval.svgTrueCoordsY - clval.svgGrabPointY
                              #! clSt`  = sttf ('DM'.get parentId idMap) xdiff ydiff svgClSt
                              #! srvSt` = updSrv svgClSrvSt clSt`
                              = (Diff (SetState srvSt`) (doResolve resolve), srvSt`)
                            _ = (NoDiff, svgClSrvSt)
  = ({clval & svgClSrvSt = srvSt`, svgMousePos = MouseUp, svgDragTarget = Nothing}, diff, world)
mkMouseDragUp _ _ _ _ _ _ _ _ _ clval=:{svgDragTarget = Nothing} world
  = ({clval & svgMousePos = MouseUp}, NoDiff, world)

firstIdentifiableParentId :: !(JSObj a) *JSWorld -> *(String, *JSWorld)
firstIdentifiableParentId elem world
  #! (idval, world) = .? (elem .# "id") world
  | jsIsNull idval
      #! (parent, world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  #! idval = jsValToString idval
  | idval == ""
      #! (parent, world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  | otherwise = (idval, world)

registerDraggables :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                      !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                      !ComponentId !(JSObj svg) !(Map String (ImageAttr v))
                      !(Map String (Set ImageTag)) !*JSWorld
                    -> *JSWorld | iTask s & iTask v
registerDraggables mkEventHandler resolve state2Image updSrv updClient cid svg draggables idMap world
  #! (svgContainer, world) = .? (getElementById (mainSvgId cid)) world
  #! (svgRoot, world)      = .? (svgContainer .# "firstChild") world
  #! idMap                 = 'DM'.foldrWithKey (\k v m -> 'DM'.put (replaceSubString editletId cid k) v m) 'DM'.newMap idMap
  #! cbUp                  = mkEventHandler (mkMouseDragUp   resolve state2Image updSrv updClient cid svgRoot idMap) cid
  #! cbMove                = mkEventHandler (mkMouseDragMove cid svgRoot) cid
  #! (_, world)            = (svgRoot `addEventListener` ("mouseup",   cbUp,   True)) world
  #! (_, world)            = (svgRoot `addEventListener` ("mousemove", cbMove, True)) world
  = 'DM'.foldrWithKey (registerDraggable mkEventHandler resolve state2Image updSrv updClient cid svg) world draggables
  where
  registerDraggable :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                       !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                       !ComponentId !(JSObj svg) !String !(ImageAttr v)
                       !*JSWorld
                     -> *JSWorld | iTask s & iTask v
  registerDraggable _ resolve state2image updSrv updClient cid svg _ (ImageDraggableAttr {draggable = Nothing}) world = world
  registerDraggable mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageDraggableAttr {draggable = Just sttf}) world
    #! elemId        = replaceSubString editletId cid elemId
    #! (elem, world) = (svg .# "getElementById" .$ elemId) world
    #! cbDown        = mkEventHandler (mkMouseDragDown resolve state2image updSrv updClient cid sttf elemId elem) cid
    #! (_, world)    = (elem `addEventListener` ("mousedown", cbDown, True)) world
    = world

registerSVGEvents :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                     !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                     !ComponentId !(JSObj svg) !(Map String (ImageAttr v))
                     !*JSWorld
                   -> *JSWorld | iTask s & iTask v
registerSVGEvents mkEventHandler resolve state2Image updSrv updClient cid svg onclicks world
  = 'DM'.foldrWithKey (registerEvent mkEventHandler resolve state2Image updSrv updClient cid svg) world onclicks
  where
  registerEvent :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                   !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                   !ComponentId !(JSObj svg) !ComponentId !(ImageAttr v) !*JSWorld
                 -> *JSWorld | iTask s & iTask v
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnClickAttr     {local, onclick})     world = registerNClick   mkEventHandler resolve state2image updSrv updClient cid svg elemId             onclick     local world
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnMouseDownAttr {local, onmousedown}) world = actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId "mousedown" onmousedown local world
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnMouseUpAttr   {local, onmouseup})   world = actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId "mouseup"   onmouseup   local world
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnMouseOverAttr {local, onmouseover}) world = actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId "mouseover" onmouseover local world
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnMouseMoveAttr {local, onmousemove}) world = actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId "mousemove" onmousemove local world
  registerEvent mkEventHandler resolve state2image updSrv updClient cid svg elemId (ImageOnMouseOutAttr  {local, onmouseout})  world = actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId "mouseout"  onmouseout  local world

actuallyRegister :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                    !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                    !ComponentId !(JSObj svg) !String !String !(v -> v) !Bool
                    *JSWorld
                 -> *JSWorld | iTask s & iTask v
actuallyRegister mkEventHandler resolve state2image updSrv updClient cid svg elemId evt sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! cb            = mkEventHandler (mkCB mkEventHandler resolve state2image updSrv updClient cid sttf local) cid
  #! (_, world)    = (elem `addEventListener` (evt, cb, True)) world
  = world

mkCB :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
        !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v) !ComponentId
        !(v -> v) !Bool String {JSObj JSEvent} !(SVGClSt s v) !*JSWorld
     -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
mkCB mkEventHandler resolve state2image updSrv updClient cid sttf local _ _ clval=:{svgClSt, svgClSrvSt} world
  #! svgClSt`       = sttf svgClSt
  #! srvSt`         = updSrv svgClSrvSt svgClSt`
  #! diff           = SetState srvSt`
  #! clval          = {clval & svgClSt = svgClSt`, svgClSrvSt = srvSt`}
  #! (clval, world) = if local
                        (appClientDiff resolve state2image updSrv updClient mkEventHandler cid diff clval world)
                        (clval, world)
  = ( clval
    , if local NoDiff (Diff diff (doResolve resolve))
    , world)

doResolve :: !(Conflict s -> Maybe s) !Conflict !(SVGClSt s v) !*JSWorld
          -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
doResolve resolve c s=:{svgClSrvSt} w
  = case resolve c svgClSrvSt of
      Just s` -> (s, Diff (SetState s`) (doResolve resolve), w)
      _       -> (s, NoDiff, w)
doResolve _ _ s w = (s, NoDiff, w)

registerNClick :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                  !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                  !ComponentId !(JSObj svg) !String !(Int v -> v) !Bool
                  *JSWorld
               -> *JSWorld | iTask s & iTask v
registerNClick mkEventHandler resolve state2image updSrv updClient cid svg elemId sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! cb            = mkEventHandler (mkNClickCB elemId mkEventHandler resolve state2image updSrv updClient sttf local) cid
  #! (_, world)    = (elem `addEventListener` ("click", cb, False)) world
  = world

mkNClickCB :: !String !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
              !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v) !(Int v -> v)
              !Bool !String !{JSObj JSEvent} !(SVGClSt s v) !*JSWorld
           -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
mkNClickCB elemID mkEventHandler resolve state2image updSrv updClient sttf local cid args clval=:{svgClickTimeout} world
  #! world            = if (size args > 0) (snd ((args.[0] .# "stopPropagation" .$ ()) world)) world
  #! world            = case svgClickTimeout of
                          Just to -> snd (("clearTimeout" .$ to) world)
                          _       -> world
  #! cb               = mkEventHandler (handleNClick mkEventHandler resolve state2image updSrv updClient sttf local) cid
  #! (timeOut, world) = ("setTimeout" .$ (cb, 225)) world
  = ({clval & svgClickTimeout = Just timeOut, svgNumClicks = clval.svgNumClicks + 1}, NoDiff, world)

handleNClick :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                !(Int v -> v) !Bool !String !{JSObj JSEvent} !(SVGClSt s v)
                !*JSWorld
             -> *(!SVGClSt s v, !ComponentDiff (SVGDiff s) (SVGClSt s v), !*JSWorld) | iTask s & iTask v
handleNClick mkEventHandler resolve state2image updSrv updClient sttf local cid args clval=:{svgClSt, svgClSrvSt, svgNumClicks} world
  #! svgClSt`       = sttf svgNumClicks svgClSt
  #! srvSt`         = updSrv svgClSrvSt svgClSt`
  #! clval          = {clval & svgClSt = svgClSt`, svgClSrvSt = srvSt`, svgNumClicks = 0}
  #! diff           = SetState srvSt`
  #! cdiff          = if local NoDiff (Diff diff (\_ s w -> (s, NoDiff, w)))
  #! (clval, world) = appClientDiff resolve state2image updSrv updClient mkEventHandler cid diff clval world
  = (clval, cdiff, world)

imageUpdate :: !(s -> v) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v) !(Conflict s -> Maybe s)
               !(s s -> s`)
            -> UpdateOption s s` | iTask s & iTask v
imageUpdate initClSt toImage updSrv updClient resolve fromViewState
  = UpdateWith (\s -> svgRenderer resolve s initClSt toImage updSrv updClient) (\s e -> fromViewState s e.Editlet.currVal)

derive class iTask ActionState

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s))
         -> Maybe b
doAction astos stotaskb = ifAction (const True) (const id) astos stotaskb

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b)
            !(TaskValue (ActionState a s))
         -> Maybe b
ifAction pred astos stotaskb (Value {ActionState|state=s,action=Just a} _)
  | pred a    = Just (stotaskb a {ActionState|state = astos a s, action = Nothing})
  | otherwise = Nothing
ifAction _ _ _ _ = Nothing

svgns =: "http://www.w3.org/2000/svg"

:: MousePos = MouseUp | MouseDown

instance == MousePos where
  (==) MouseDown MouseDown = True
  (==) MouseUp   MouseUp   = True
  (==) _         _         = False

defaultClSt :: s v -> SVGClSt s v
defaultClSt srvSt clSt
  = { svgNumClicks    = 0
    , svgClickTimeout = Nothing
    , svgClSt         = clSt
    , svgClSrvSt      = srvSt
    , svgMousePos     = MouseUp
    , svgDropCallback = Nothing
    , svgDragTarget   = Nothing
    , svgTrueCoordsX  = 0.0
    , svgTrueCoordsY  = 0.0
    , svgGrabPointX   = 0.0
    , svgGrabPointY   = 0.0
    }

:: SVGDiff s
  = SetState s

derive class iTask Set, DropTarget, MousePos, ImageTag
derive class iTask SVGDiff, SVGClSt

svgRenderer :: !(Conflict s -> Maybe s) !s !(s -> v) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
            -> Editlet s (SVGDiff s) (SVGClSt s v) | iTask s & iTask v
svgRenderer resolve origState initClSt state2Image updSrv updClient
  = { currVal    = origState
    , defValSrv  = origState
    , genUI      = genUI
    , initClient = initClient resolve state2Image updSrv origState initClSt updClient
    , appDiffClt = appClientDiff resolve state2Image updSrv updClient
    , genDiffSrv = genServerDiff
    , appDiffSrv = appServerDiff
    }
  where
  genUI = \cid world ->
    ({ ComponentHTML
     | width  = FlexSize
     , height = FlexSize
     , html   = DivTag [IdAttr (mainSvgId cid), StyleAttr "overflow: auto;"] []
     }
     , world
    )

  initClient :: !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !s !(s -> v) !(s v -> v)
                !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f) !ComponentId !*JSWorld
             -> *(!SVGClSt s v, !*JSWorld) | iTask s & iTask v
  initClient resolve state2Image updSrv origState initClSt updClient mkEventHandler cid world = appClientDiff resolve state2Image updSrv updClient mkEventHandler cid (SetState origState) (defaultClSt origState (initClSt origState)) world

  genServerDiff :: !s !s -> Maybe (SVGDiff s) | iTask s
  genServerDiff oldSrvSt newSrvSt
    | oldSrvSt === newSrvSt = Nothing
    | otherwise             = Just (SetState newSrvSt)

  appServerDiff :: !(SVGDiff s) s -> s | iTask s
  appServerDiff (SetState st) _ = st

imageFromState :: !(Image v) !(Map FontDef (Map String Real)) -> *(!Image v, !*SpanEnvs) | iTask v
imageFromState img env
  #! spanEnvs  = { spanEnvImageTagPostTrans  = 'DIS'.newMap
                 , spanEnvImageSpanPostTrans = 'DIS'.newMap
                 , spanEnvGridTag            = 'DIS'.newMap
                 , spanEnvGridSpan           = 'DIS'.newMap
                 , spanEnvFonts              = env
                 }
  #! (img, st) = desugarAndTag img { desugarAndTagCounter  = 0
                                   , desugarAndTagSpanEnvs = spanEnvs}
  = (img, st.desugarAndTagSpanEnvs)

appClientDiff :: !(Conflict s -> Maybe s) !(s v *TagSource -> Image v) !(s v -> s) !(s v -> v)
                 !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s v)) ComponentId -> JSFun f)
                 !ComponentId !(SVGDiff s) !(SVGClSt s v) !*JSWorld
              -> *(!SVGClSt s v, !*JSWorld) | iTask s & iTask v
appClientDiff resolve state2Image updSrv updClient mkEventHandler cid (SetState s) clst world
  #! svgClSt`             = updClient s clst.svgClSt
  #! clst                 = {clst & svgClSt = svgClSt`, svgClSrvSt = s}
  #! image                = state2Image s svgClSt` [(ImageTagUser no cid, ImageTagUser no cid) \\ no <- [0..]]
  #! fontMap              = gatherFonts image
  #! (realFontMap, world) = if ('DM'.null fontMap) ('DM'.newMap, world) (calcTextLengths fontMap world)
  #! (img, spanEnvs)      = imageFromState image realFontMap
  #! fixVal               = fixEnvs {FixSpansSt | fixSpansDidChange = False, fixSpansSpanEnvs = spanEnvs}
  #! (syn, clval)         = genSVG img { uniqueIdCounter = 0, genStates = fixVal.fixSpansSpanEnvs }
  #! (imXSp, imYSp)       = syn.genSVGSyn_imageSpanReal
  #! (imXSp, imYSp)       = (toString (to2dec imXSp), toString (to2dec imYSp))
  #! svgStr               = browserFriendlySVGEltToString (SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns]
                                             [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp]
                                             syn.genSVGSyn_svgElts)
  #! svgStr               = replaceSubString editletId cid svgStr
  #! (parser, world)      = new "DOMParser" () world
  #! (doc, world)         = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
  #! (newSVG, world)      = .? (doc .# "firstChild") world
  #! svgDiv               = getElementById (mainSvgId cid)
  #! (currSVG, world)     = .? (svgDiv .# "firstChild") world
  #! (_, world)           = if (jsIsNull currSVG)
                              ((svgDiv `appendChild` newSVG) world)
                              ((svgDiv .# "replaceChild" .$ (newSVG, currSVG)) world)
  #! world                = registerSVGEvents mkEventHandler resolve state2Image updSrv updClient cid newSVG syn.genSVGSyn_events world
  #! world                = registerDraggables mkEventHandler resolve state2Image updSrv updClient cid newSVG syn.genSVGSyn_draggable syn.genSVGSyn_idMap world
  = (clst, world)

(`getElementsByClassName`) obj args :== obj .# "getElementsByClassName" .$ args
(`addEventListener`)       obj args :== obj .# "addEventListener"       .$ args
(`setAttribute`)           obj args :== obj .# "setAttribute"           .$ args
(`setAttributeNS`)         obj args :== obj .# "setAttributeNS"         .$ args
(`createElementNS`)        obj args :== obj .# "createElementNS"        .$ args
(`appendChild`)            obj args :== obj .# "appendChild"            .$ args
(`removeChild`)            obj args :== obj .# "removeChild"            .$ args
(`getComputedTextLength`)  obj args :== obj .# "getComputedTextLength"  .$ args
(`getAttribute`)           obj args :== obj .# "getAttribute"           .$ args
(`createSVGPoint`)         obj args :== obj .# "createSVGPoint"         .$ args
(`getScreenCTM`)           obj args :== obj .# "getScreenCTM"           .$ args
(`getCTM`)                 obj args :== obj .# "getCTM"                 .$ args
(`inverse`)                obj args :== obj .# "inverse"                .$ args
(`matrixTransform`)        obj args :== obj .# "matrixTransform"        .$ args

calcTextLengths :: !(Map FontDef (Set String)) !*JSWorld -> *(!Map FontDef (Map String Real), !*JSWorld)
calcTextLengths fontdefs world
  #! (svg, world)  = (jsDocument `createElementNS` (svgns, "svg")) world
  #! (body, world) = .? (jsDocument .# "body") world
  #! (_, world)    = (body `appendChild` svg) world
  #! (elem, world) = (jsDocument `createElementNS` (svgns, "text")) world
  #! (_, world)    = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
  #! (_, world)    = (svg `appendChild` elem) world
  #! (res, world)  = 'DM'.foldrWithKey (f elem) ('DM'.newMap, world) fontdefs
  #! (_, world)    = (svg `removeChild` elem) world
  #! (_, world)    = (body `removeChild` svg) world
  = (res, world)
  where
  f :: !(JSVal (JSObject a)) !FontDef !(Set String) !*(!Map FontDef (Map String Real), !*JSWorld) -> *(!Map FontDef (Map String Real), !*JSWorld)
  f elem fontdef strs (acc, world)
    #! fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                     , ("font-size",    toString fontdef.fontysize)
                     , ("font-stretch", fontdef.fontstretch)
                     , ("font-style",   fontdef.fontstyle)
                     , ("font-variant", fontdef.fontvariant)
                     , ("font-weight",  fontdef.fontweight)
                     , ("alignment-baseline", "auto")
                     , ("dominant-baseline", "auto")
                     , ("x", "-10000")
                     , ("y", "-10000")
                     ]
    #! world       = strictFoldl (\world args -> snd ((elem `setAttribute` args) world)) world fontAttrs
    #! (ws, world) = 'DS'.fold (g elem) ('DM'.newMap, world) strs
    = ('DM'.put fontdef ws acc, world)
  g :: !(JSVal (JSObject a)) !String !*(!Map String Real, !*JSWorld) -> *(!Map String Real, !*JSWorld)
  g elem str (acc, world)
    #! world        = (elem .# "textContent" .= str) world
    #! (ctl, world) = (elem `getComputedTextLength` ()) world
    = ('DM'.put str (jsValToReal ctl) acc, world)

:: *FixSpansSt =
  { fixSpansDidChange :: !Bool
  , fixSpansSpanEnvs  :: !*SpanEnvs
  }

:: DesugarAndTagSt a :== State *DesugarAndTagStVal a

:: *DesugarAndTagStVal =
  { desugarAndTagCounter  :: !Int
  , desugarAndTagSpanEnvs :: !*SpanEnvs
  }

class nextNo a :: !*a -> *(!Int, !*a)

instance nextNo (GenSVGStVal s) where
  nextNo st = (st.uniqueIdCounter, {st & uniqueIdCounter = st.uniqueIdCounter + 1})

instance nextNo DesugarAndTagStVal where
  nextNo st = (st.desugarAndTagCounter, {st & desugarAndTagCounter = st.desugarAndTagCounter + 1})

:: State s a :== s -> *(!a, !s)

:: ErrorMessage :== String

:: DesugarAndTagSyn s =
  { desugarAndTagSyn_ImageContent        :: !ImageContent s
  , desugarAndTagSyn_TotalSpan_PreTrans  :: !ImageSpan
  , desugarAndTagSyn_TotalSpan_PostTrans :: !ImageSpan
  , desugarAndTagSyn_OffsetCorrection    :: !ImageOffset
  , desugarAndTagSyn_IsBasic             :: !Bool
  }

cacheImageSpanPostTrans :: !Int !(Set ImageTag) !ImageSpan !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheImageSpanPostTrans n imTas sp st
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put n sp spanEnvs.spanEnvImageSpanPostTrans}
  #! env      = 'DS'.fold (putImgTag n) spanEnvs.spanEnvImageTagPostTrans imTas
  #! spanEnvs = {spanEnvs & spanEnvImageTagPostTrans = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}

cacheGridSpans :: !Int !(Set ImageTag) ![Span] ![Span] !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheGridSpans n imTas xsps ysps st
  #! xsps`    = 'DIS'.fromList (strictTRZip2 [0..] xsps)
  #! ysps`    = 'DIS'.fromList (strictTRZip2 [0..] ysps)
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvGridSpan = 'DIS'.put n (xsps`, ysps`) spanEnvs.spanEnvGridSpan}
  #! env      = 'DS'.fold (putImgTag n) spanEnvs.spanEnvGridTag imTas
  #! spanEnvs = {spanEnvs & spanEnvGridTag = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}

putImgTag :: !Int !ImageTag !(IntMap Int) -> IntMap Int
putImgTag n t env = 'DIS'.put (numTag t) n env

point2Vec :: !(!Span, !Span) -> Vector Span
point2Vec (x, y) = {x, y, px 1.0}

appTF :: !(Matrix Span) !(!Span, !Span) -> (!Span, !Span)
appTF m p
  #! m = mulMatrixVec m (point2Vec p)
  = (m.[0].[0], m.[1].[0])

translateTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
translateTF sx sy p
  = appTF { {px 1.0, px 0.0, sx}
          , {px 0.0, px 1.0, sy}
          , {px 0.0, px 0.0, px 1.0}
          } p

scaleTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
scaleTF sx sy p
  = appTF { {sx,     px 0.0, px 0.0}
          , {px 0.0, sy,     px 0.0}
          , {px 0.0, px 0.0, px 1.0}
          } p

rotateTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
rotateTF a p
  #! a = toRad a
  = appTF { {px (cos a), px (0.0 - sin a), px 0.0}
          , {px (sin a), px (cos a),       px 0.0}
          , {px 0.0,     px 0.0,           px 1.0}
          } p

skewXTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewXTF a p
  = appTF { {px 1.0, px (tan (toRad a)), px 0.0}
          , {px 0.0, px 1.0,             px 0.0}
          , {px 0.0, px 0.0,             px 1.0}
          } p

skewYTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewYTF a p
  = appTF { {px 1.0,             px 0.0, px 0.0}
          , {px (tan (toRad a)), px 1.0, px 0.0}
          , {px 0.0,             px 0.0, px 1.0}
          } p

revFstsSnds :: ![(!a, !b)] -> (![a], ![b])
revFstsSnds xs = strictFoldl (\(xs, ys) (x, y) -> ([x:xs], [y:ys])) ([], []) xs

applyTransforms :: ![ImageTransform] !ImageSpan -> (!ImageSpan, !ImageOffset)
applyTransforms ts (xsp, ysp)
  #! origPoints     = [(zero, zero), (xsp, zero), (zero, ysp), (xsp, ysp)]
  #! newPoints      = strictFoldr f origPoints ts
  #! (allXs, allYs) = revFstsSnds newPoints
  #! minX           = minSpan allXs
  #! maxX           = maxSpan allXs
  #! minY           = minSpan allYs
  #! maxY           = maxSpan allYs
  = ((maxX - minX, maxY - minY), (zero - minX, zero - minY))
  where
  f :: !ImageTransform ![(!Span, !Span)] -> [(!Span, !Span)]
  f (RotateImage th) coords
    #! (allXs, allYs) = revFstsSnds coords
    #! minX           = minSpan allXs
    #! maxX           = maxSpan allXs
    #! minY           = minSpan allYs
    #! maxY           = maxSpan allYs
    #! cx             = (maxX - minX) /. 2.0
    #! cy             = (maxY - minY) /. 2.0
    #! translated     = strictTRMap (translateTF (zero - cx) (zero - cy)) coords
    #! rotated        = strictTRMap (rotateTF th) translated
    = strictTRMap (translateTF cx cy) rotated
  f (SkewXImage th)      coords = strictTRMap (skewXTF th) coords
  f (SkewYImage th)      coords = strictTRMap (skewYTF th) coords
  f (FitImage xsp` ysp`) coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          = strictTRMap (scaleTF (xsp` / (brX - tlX)) (ysp` / (brY - tlX))) coords
  f (FitXImage xsp`)     coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          #! factor  = xsp` / (brX - tlX)
          = strictTRMap (scaleTF factor factor) coords
  f (FitYImage ysp`)     coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          #! factor  = ysp` / (brY - tlY)
          = strictTRMap (scaleTF factor factor) coords
  f (ScaleImage xsp` ysp`) coords
    = strictTRMap (scaleTF (px xsp`) (px ysp`)) coords
  f (ScaleXImage xsp`)     coords
    = strictTRMap (scaleTF (px xsp`) (px 1.0)) coords
  f (ScaleYImage ysp`)     coords
    = strictTRMap (scaleTF (px 1.0) (px ysp`)) coords
  f _ coords = coords


gatherFonts :: !(Image s) -> Map FontDef (Set String)
gatherFonts {content, mask, attribs, transform}
  = gatherFontsUnions [ gatherFontsImageContent content
                      , gatherFontsMask mask
                      , gatherFontsAttribs attribs
                      , gatherFontsTransforms transform
                      ]
  where
  gatherFontsImageContent :: !(ImageContent m) -> Map FontDef (Set String)
  gatherFontsImageContent (Basic _ (l, r)) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
  gatherFontsImageContent (Line {lineSpan = (l, r), lineContent, markers}) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r, gatherFontsMarkers markers, gatherFontsLineContent lineContent]
    where
    gatherFontsMarkers :: !(Maybe (Markers m)) -> Map FontDef (Set String)
    gatherFontsMarkers (Just {markerStart, markerMid, markerEnd}) = gatherFontsUnions (strictTRMapRev gatherFonts (maybeToList markerStart ++ maybeToList markerMid ++ maybeToList markerEnd))
    gatherFontsMarkers _ = 'DM'.newMap
    gatherFontsLineContent :: !LineContent -> Map FontDef (Set String)
    gatherFontsLineContent (PolygonImage ios)  = gatherFontsUnions (gatherFontsPairs ios)
    gatherFontsLineContent (PolylineImage ios) = gatherFontsUnions (gatherFontsPairs ios)
    gatherFontsLineContent _                   = 'DM'.newMap
  gatherFontsImageContent (Composite {host, compose}) = gatherFontsUnions [gatherFontsCompose compose : strictTRMapRev gatherFonts (maybeToList host)]
    where
    gatherFontsCompose :: !(Compose m) -> Map FontDef (Set String)
    gatherFontsCompose (AsGrid    _ offss _ imgss) = gatherFontsUnions (strictTRMapRev gatherFonts (flattenTR imgss) ++ (gatherFontsPairs (flattenTR offss)))
    gatherFontsCompose (AsCollage   offs    imgs)  = gatherFontsUnions (strictTRMapRev gatherFonts imgs ++ gatherFontsPairs offs)
    gatherFontsCompose (AsOverlay   offs  _ imgs)  = gatherFontsUnions (strictTRMapRev gatherFonts imgs ++ gatherFontsPairs offs)
  gatherFontsMask :: !(Maybe (Image m)) -> Map FontDef (Set String)
  gatherFontsMask (Just img) = gatherFonts img
  gatherFontsMask _          = 'DM'.newMap
  gatherFontsAttribs :: !(Set (ImageAttr m )) -> Map FontDef (Set String)
  gatherFontsAttribs attribs = gatherFontsUnions (strictTRMapRev gatherFontsAttrib ('DS'.toList attribs))
    where
    gatherFontsAttrib :: !(ImageAttr m) -> Map FontDef (Set String)
    gatherFontsAttrib (ImageStrokeWidthAttr {strokewidth}) = gatherFontsSpan strokewidth
    gatherFontsAttrib (ImageXRadiusAttr     {xradius})     = gatherFontsSpan xradius
    gatherFontsAttrib (ImageYRadiusAttr     {yradius})     = gatherFontsSpan yradius
    gatherFontsAttrib _                                    = 'DM'.newMap
  gatherFontsTransforms :: ![ImageTransform] -> Map FontDef (Set String)
  gatherFontsTransforms transforms = gatherFontsUnions (strictTRMapRev gatherFontsTransform transforms)
    where
    gatherFontsTransform :: !ImageTransform -> Map FontDef (Set String)
    gatherFontsTransform (FitImage l r) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
    gatherFontsTransform (FitXImage l)  = gatherFontsSpan l
    gatherFontsTransform (FitYImage l)  = gatherFontsSpan l
    gatherFontsTransform _              = 'DM'.newMap

gatherFontsPairs :: ![(!Span, !Span)] -> [Map FontDef (Set String)]
gatherFontsPairs pairs = strictFoldl f [] pairs
  where
  f :: ![Map FontDef (Set String)] !(!Span, !Span) -> [Map FontDef (Set String)]
  f acc (x, y) = [gatherFontsSpan x : gatherFontsSpan y : acc]

gatherFontsSpan :: !Span -> Map FontDef (Set String)
gatherFontsSpan (AddSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (SubSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (MulSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (DivSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (AbsSpan x)                     = gatherFontsSpan x
gatherFontsSpan (MinSpan xs)                    = gatherFontsUnions (strictTRMapRev gatherFontsSpan xs)
gatherFontsSpan (MaxSpan xs)                    = gatherFontsUnions (strictTRMapRev gatherFontsSpan xs)
gatherFontsSpan (LookupSpan (TextXSpan fd str)) = 'DM'.singleton fd ('DS'.singleton str)
gatherFontsSpan _                               = 'DM'.newMap

const2 :: !a b c -> a
const2 x _ _ = x

gatherFontsUnions :: ![Map FontDef (Set String)] -> Map FontDef (Set String)
gatherFontsUnions m = 'DM'.unionsWith 'DS'.union m

desugarAndTagMaybeImage :: !(Maybe (Image s)) !*DesugarAndTagStVal -> (!Maybe (Image s), !*DesugarAndTagStVal) | iTask s
desugarAndTagMaybeImage (Just img) st
  #! (img, st) = desugarAndTag img st
  = (Just img, st)
desugarAndTagMaybeImage n st = (n, st)

desugarAndTag :: !(Image s) !*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal) | iTask s
desugarAndTag {content, mask, attribs, transform, tags} st
  #! (mask, st)      = desugarAndTagMaybeImage mask st
  #! (no, st)        = nextNo st
  #! newTag          = ImageTagSystem no
  #! (syn, st)       = desugarAndTagImageContent content transform tags st
  #! tags            = 'DS'.insert newTag tags
  #! st              = cacheImageSpanPostTrans no tags syn.desugarAndTagSyn_TotalSpan_PostTrans st
  #! img             = { Image
                       | content             = syn.desugarAndTagSyn_ImageContent
                       , mask                = mask
                       , attribs             = attribs
                       , transform           = transform
                       , tags                = tags
                       , uniqId              = no
                       , totalSpanPreTrans   = syn.desugarAndTagSyn_TotalSpan_PreTrans  // TODO Get rid of these fields in favor of cached spans
                       , totalSpanPostTrans  = (LookupSpan (ImageXSpan newTag), LookupSpan (ImageYSpan newTag)) // TODO Get rid of these fields in favor of cached spans
                       , transformCorrection = syn.desugarAndTagSyn_OffsetCorrection    // TODO Get rid of these fields in favor of cached spans
                       }
  = (img, st)
  where
  desugarAndTagImageContent :: !(ImageContent s) ![ImageTransform] !(Set ImageTag) !*DesugarAndTagStVal
                            -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
  desugarAndTagImageContent (Basic bi imSp) transform tags st
    #! (imSp`, imOff) = applyTransforms transform imSp
    = ({ desugarAndTagSyn_ImageContent        = Basic bi imSp
       , desugarAndTagSyn_TotalSpan_PreTrans  = imSp
       , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
       , desugarAndTagSyn_OffsetCorrection    = imOff
       , desugarAndTagSyn_IsBasic             = True }, st)
  desugarAndTagImageContent (Line {lineSpan, lineContent, markers}) transform tags st
    #! (markers, st)  = desugarAndTagMarkers markers st
    #! (imSp`, imOff) = applyTransforms transform lineSpan
    = ({ desugarAndTagSyn_ImageContent        = Line { LineImage
                                                     | lineSpan    = lineSpan
                                                     , markers     = markers
                                                     , lineContent = lineContent }
       , desugarAndTagSyn_TotalSpan_PreTrans  = lineSpan
       , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
       , desugarAndTagSyn_OffsetCorrection    = imOff
       , desugarAndTagSyn_IsBasic             = False }, st)
    where
    desugarAndTagMarkers :: !(Maybe (Markers s)) !*DesugarAndTagStVal
                         -> *(!(Maybe (Markers s)), !*DesugarAndTagStVal) | iTask s
    desugarAndTagMarkers (Just {markerStart, markerMid, markerEnd}) st
      #! (markerStart, st) = desugarAndTagMaybeImage markerStart st
      #! (markerMid, st)   = desugarAndTagMaybeImage markerMid st
      #! (markerEnd, st)   = desugarAndTagMaybeImage markerEnd st
      = (Just {markerStart = markerStart, markerMid = markerMid, markerEnd = markerEnd}, st)
    desugarAndTagMarkers n st = (n, st)
  desugarAndTagImageContent (Composite {host, compose}) transform tags st
    #! (host, st)                   = desugarAndTagMaybeImage host st
    #! ((compose, composeSpan), st) = desugarAndTagCompose compose host transform tags st
    #! (host, span)                 = case host of
                                       Just hostImg
                                          -> (host, hostImg.totalSpanPostTrans)
                                       _  -> (Nothing, composeSpan)
    #! (span`, corr)                = applyTransforms transform span
    = ({ desugarAndTagSyn_ImageContent        = Composite { CompositeImage
                                                          | host    = host
                                                          , compose = compose
                                                          }
       , desugarAndTagSyn_TotalSpan_PreTrans  = span
       , desugarAndTagSyn_TotalSpan_PostTrans = span`
       , desugarAndTagSyn_OffsetCorrection    = corr
       , desugarAndTagSyn_IsBasic             = False }, st)
    where
    desugarAndTagCompose :: !(Compose s) !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag) !*DesugarAndTagStVal
                         -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal) | iTask s
    desugarAndTagCompose (AsGrid (numcols, numrows) offsetss iass imgss) host transform tags st
      #! (imgss, st) = strictTRMapSt (strictTRMapSt desugarAndTag) imgss st
      #! (tag, st)   = nextNo st
      #! sysTags     = ImageTagSystem tag
      #! colIndices  = [0 .. numcols - 1]
      #! rowIndices  = [0 .. numrows - 1]
      #! gridSpan    = maybe ( strictFoldl (\acc n -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) colIndices
                             , strictFoldl (\acc n -> LookupSpan (RowYSpan sysTags n)    + acc) (px 0.0) rowIndices
                             )
                             (\x -> x.totalSpanPostTrans) host
      #! spanss      = strictTRMap (strictTRMap (\x -> x.totalSpanPostTrans)) imgss
      #! st          = cacheGridSpans tag ('DS'.insert sysTags tags)
                                      (strictTRMap (maxSpan o strictTRMap fst) (transpose spanss))
                                      (strictTRMap (maxSpan o strictTRMap snd) spanss) st
      #! offsets`    = calculateGridOffsets (strictTRMap (\n -> LookupSpan (ColumnXSpan sysTags n)) colIndices)
                                            (strictTRMap (\n -> LookupSpan (RowYSpan sysTags n))    rowIndices) iass imgss offsetss
      #! offsets`    = reverseTR (flattenTR offsets`)
      = (( AsCollage offsets` (flattenTR imgss)
         , gridSpan), st)
      where
      calculateGridOffsets :: ![Span] ![Span] ![[ImageAlign]] ![[Image s]] ![[(!Span, !Span)]] -> [[(!Span, !Span)]]
      calculateGridOffsets cellXSpans cellYSpans alignss imagess offsetss
        = fst (strictFoldl (mkRows cellXSpans) ([], zero) (strictTRZip4 alignss imagess cellYSpans offsetss))
        where
        mkRows :: ![Span] !(![[(!Span, !Span)]], !Span) !(![(!XAlign, !YAlign)], ![Image s], !Span, ![(!Span, !Span)])
               -> (![[(!Span, !Span)]], !Span)
        mkRows cellXSpans (acc, yoff) (aligns, imgs, cellYSpan, offsets)
          = ( [fst (strictFoldl (mkCols cellYSpan yoff) ([], zero) (strictTRZip4 aligns imgs cellXSpans offsets)) : acc]
            , yoff + cellYSpan)
        mkCols :: !Span !Span !(![(!Span, !Span)], !Span) !(!(!XAlign, !YAlign), !Image s, !Span, !(!Span, !Span))
               -> (![(!Span, !Span)], !Span)
        mkCols cellYSpan yoff (acc, xoff) (align, img=:{totalSpanPostTrans, transformCorrection = (tfXCorr, tfYCorr)}, cellXSpan, (manXOff, manYOff))
          #! (alignXOff, alignYOff) = calcAlignOffset cellXSpan cellYSpan totalSpanPostTrans align
          = ([( xoff + alignXOff + manXOff + tfXCorr
              , yoff + alignYOff + manYOff + tfYCorr) : acc], xoff + cellXSpan)
    desugarAndTagCompose (AsCollage offsets imgs) host transform tags st
      #! (imgs, st)    = strictTRMapSt desugarAndTag imgs st
      = (( AsCollage offsets imgs
         , maybe (calculateComposedSpan (strictTRMap (\x -> x.totalSpanPostTrans) imgs) offsets) (\x -> x.totalSpanPostTrans) host), st)
    desugarAndTagCompose (AsOverlay offsets ias imgs) host transform tags st
      #! (imgs, st)     = strictTRMapSt desugarAndTag imgs st
      #! spans          = strictTRMap (\x -> x.totalSpanPostTrans) imgs
      #! (  maxXSpan
          , maxYSpan)   = maybe (maxSpan (strictTRMap fst spans), maxSpan (strictTRMap snd spans))
                                (\x -> x.totalSpanPreTrans) host
      #! alignOffsets   = strictTRZipWith (calcAlignOffset maxXSpan maxYSpan) spans ias
      #! placingOffsets = strictTRZipWith3 addOffset alignOffsets offsets imgs
      = ( ( AsCollage placingOffsets imgs
          , maybe (calculateComposedSpan spans placingOffsets) (\x -> x.totalSpanPostTrans) host)
        , st)
      where
      addOffset :: !(!Span, !Span) !(!Span, !Span) !(Image s) -> (!Span, !Span) | iTask s
      addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)

:: *SpanEnvs =
  { spanEnvImageTagPostTrans  :: !IntMap Int
  , spanEnvImageSpanPostTrans :: !IntMap ImageSpan
  , spanEnvGridTag            :: !IntMap Int
  , spanEnvGridSpan           :: !IntMap (!IntMap Span, !IntMap Span)
  , spanEnvFonts              :: !Map FontDef (Map String Real)
  }

fixEnvs :: !*FixSpansSt -> *FixSpansSt
fixEnvs st
  = fixEnvs` True {FixSpansSt | st & fixSpansDidChange = False}
  where
  fixEnvs` :: !Bool !*FixSpansSt -> *FixSpansSt
  fixEnvs` False st=:{fixSpansDidChange = False} = st
  fixEnvs` _ st
    #! st = fixImageSpansPostTrans st
    #! st = fixGridSpans           st
    = fixEnvs` False st
  fixImageSpansPostTrans :: !*FixSpansSt -> *FixSpansSt
  fixImageSpansPostTrans st
    #! fixSpansSpanEnvs          = st.fixSpansSpanEnvs
    #! spanEnvImageSpanPostTrans = fixSpansSpanEnvs.spanEnvImageSpanPostTrans
    #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
    #! st                        = {st & fixSpansSpanEnvs = fixSpansSpanEnvs}
    = 'DIS'.foldrWithKey f st spanEnvImageSpanPostTrans
    where
    f :: !Int !(!Span, !Span) !*FixSpansSt -> *FixSpansSt
    f k (PxSpan _, PxSpan _) st = {FixSpansSt | st & fixSpansDidChange = False}
    f k (w=:(PxSpan _), h) st
      #! (h`, st`) = fixSpans h {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st` & fixSpansDidChange = st.fixSpansDidChange}
    f k (w, h=:(PxSpan _)) st
      #! (w`, st`) = fixSpans w {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st` & fixSpansDidChange = st.fixSpansDidChange}
    f k (w, h) st
      #! (w`, st1) = fixSpans w {st & fixSpansDidChange = False}
      #! (h`, st2) = fixSpans h {st1 & fixSpansDidChange = False}
      | st1.fixSpansDidChange || st2.fixSpansDidChange
        #! fixSpansSpanEnvs          = st2.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st2 & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st2 & fixSpansDidChange = st.fixSpansDidChange}
  fixGridSpans :: !*FixSpansSt -> *FixSpansSt
  fixGridSpans st=:{fixSpansSpanEnvs, fixSpansDidChange = origDidChange}
    #! fixSpansSpanEnvs = st.fixSpansSpanEnvs
    #! spanEnvGridSpan  = fixSpansSpanEnvs.spanEnvGridSpan
    #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
    #! st               = {st & fixSpansSpanEnvs = fixSpansSpanEnvs}
    #! st               = 'DIS'.foldrWithKey f {st & fixSpansDidChange = False} spanEnvGridSpan
    = {st & fixSpansDidChange = st.fixSpansDidChange || origDidChange}
    where
    f :: !Int !(!IntMap Span, !IntMap Span) !*FixSpansSt -> *FixSpansSt
    f k (xsps, ysps) st=:{fixSpansDidChange = origDidChange}
      #! (xsps`, st1) = 'DIS'.mapSt fixWithState xsps {st & fixSpansDidChange = False}
      #! (ysps`, st2) = 'DIS'.mapSt fixWithState ysps {st1 & fixSpansDidChange = False}
      | st1.fixSpansDidChange || st2.fixSpansDidChange
        #! fixSpansSpanEnvs = st2.fixSpansSpanEnvs
        #! spanEnvGridSpan  = 'DIS'.put k (xsps`, ysps`) fixSpansSpanEnvs.spanEnvGridSpan
        #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
        = {st2 & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st2 & fixSpansDidChange = origDidChange}

fixWithState :: !Span !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixWithState v st
  #! (v, st`) = fixSpans v {st & fixSpansDidChange = False}
  = (v, {st` & fixSpansDidChange = st`.fixSpansDidChange || st.fixSpansDidChange})

isPxSpan :: !Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

fixSpans :: !Span !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixSpans sp=:(PxSpan _) st = (sp, {st & fixSpansDidChange = False})
fixSpans osp=:(AddSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` + y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(SubSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` - y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(MulSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` * y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(DivSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` / y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(AbsSpan x) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  | otherwise
  = case x` of
      PxSpan x` | x` < 0.0 = (PxSpan (abs x`), {st & fixSpansDidChange = True})
      sp                   = (sp, st1)
fixSpans osp=:(MinSpan xs) st
  #! (xs, st1) = strictTRMapSt fixWithState xs {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  = case minSpan xs of
      sp=:(PxSpan _) -> (sp, {st1 & fixSpansDidChange = True})
      sp             -> (sp, st1)
fixSpans osp=:(MaxSpan xs) st
  #! (xs, st1) = strictTRMapSt fixWithState xs {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  = case maxSpan xs of
      sp=:(PxSpan _) -> (sp, {st1 & fixSpansDidChange = True})
      sp             -> (sp, st1)
fixSpans (LookupSpan lu) st = fixLookupSpans lu st

fixLookupSpans :: !LookupSpan !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixLookupSpans (TextXSpan fd str) st
  #! sw = case 'DM'.get fd st.fixSpansSpanEnvs.spanEnvFonts of
            Just fs -> case 'DM'.get str fs of
                         Just sw -> sw
                         _       -> 0.0
            _       -> 0.0
  = (PxSpan sw, {st & fixSpansDidChange = False})
fixLookupSpans osp=:(ImageXSpan t) st
  #! ses                      = st.fixSpansSpanEnvs
  #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
  #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
        #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (xsp=:(PxSpan _), _)
              = (xsp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
            Just _
              = (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
fixLookupSpans osp=:(ImageYSpan t) st
  #! ses                      = st.fixSpansSpanEnvs
  #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
  #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
        #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (_, ysp=:(PxSpan _))
              = (ysp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
            Just _
              = (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
fixLookupSpans osp=:(ColumnXSpan t n) st
  #! ses            = st.fixSpansSpanEnvs
  #! spanEnvGridTag = ses.spanEnvGridTag
  #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = ses.spanEnvGridSpan
        #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (xs, _)
              = case 'DIS'.get n xs of
                  Just xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                  _                    -> (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
fixLookupSpans osp=:(RowYSpan t n) st
  #! ses            = st.fixSpansSpanEnvs
  #! spanEnvGridTag = ses.spanEnvGridTag
  #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = ses.spanEnvGridSpan
        #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (_, xs)
              = case 'DIS'.get n xs of
                  Just xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                  _                    -> (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})

numTag (ImageTagUser n _) = n
numTag (ImageTagSystem n) = n + 8096

:: ImageSpanReal :== (!Real, !Real)

:: ImageOffsetReal :== (!Real, !Real)

:: GenSVGSyn s =
  { genSVGSyn_svgElts       :: ![SVGElt]
  , genSVGSyn_imageSpanReal :: !ImageSpanReal
  , genSVGSyn_events        :: !Map String (ImageAttr s)
  , genSVGSyn_draggable     :: !Map String (ImageAttr s)
  , genSVGSyn_idMap         :: !Map String (Set ImageTag)
  }

mkGenSVGSyn =: { genSVGSyn_svgElts       = []
               , genSVGSyn_imageSpanReal = (0.0, 0.0)
               , genSVGSyn_events        = 'DM'.newMap
               , genSVGSyn_draggable     = 'DM'.newMap
               , genSVGSyn_idMap         = 'DM'.newMap
               }

editletId =: "__INTERNAL_editletId_PLACEHOLDER__"

mkMaskId :: !String !Int -> String
mkMaskId editletId uniqId = "maskId-" +++ editletId +++ toString uniqId

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkOnClickId :: !String !Int -> String
mkOnClickId editletId uniqId = "onClickId-" +++ editletId +++ toString uniqId

mkOnMouseDownId :: !String !Int -> String
mkOnMouseDownId editletId uniqId = "onMouseDownId-" +++ editletId +++ toString uniqId

mkOnMouseUpId :: !String !Int -> String
mkOnMouseUpId editletId uniqId = "onMouseUpId-" +++ editletId +++ toString uniqId

mkOnMouseOverId :: !String !Int -> String
mkOnMouseOverId editletId uniqId = "onMouseOverId-" +++ editletId +++ toString uniqId

mkOnMouseMoveId :: !String !Int -> String
mkOnMouseMoveId editletId uniqId = "onMouseMoveId-" +++ editletId +++ toString uniqId

mkOnMouseOutId :: !String !Int -> String
mkOnMouseOutId editletId uniqId = "onMouseOutId-" +++ editletId +++ toString uniqId

getSvgAttrs :: ![Maybe SVGAttr] -> [SVGAttr]
getSvgAttrs as = [a \\ Just a <- as]

mkUrl :: !String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (toString (to2dec imXSp)), HeightAttr (toString (to2dec imYSp))]

to2dec :: !Real -> Real
to2dec n = toReal (toInt (n * 100.0)) / 100.0

mkUniqId :: !String !Int -> String
mkUniqId editletId uniqId = "uniqId-" +++ editletId +++ toString uniqId

imageMaskId :: !*a -> *(!String, !*a) | nextNo a
imageMaskId clval
  #! (uid, clval) = nextNo clval
  = (mkMaskId editletId uid, clval)

mkElt :: !String !(Maybe (GenSVGSyn .a)) !(GenSVGSyn .b) -> [SVGElt]
mkElt _      Nothing     syn = syn.genSVGSyn_svgElts
mkElt maskId (Just mask) syn
  = [ DefsElt [] [] [MaskElt [IdAttr maskId] [] mask.genSVGSyn_svgElts]
    : syn.genSVGSyn_svgElts]

genSVGMaybeImage :: !(Maybe (Image s)) !*(GenSVGStVal s) -> (!Maybe (GenSVGSyn s), !*GenSVGStVal s) | iTask s
genSVGMaybeImage (Just img) st
  #! (syn, st) = genSVG img st
  = (Just syn, st)
genSVGMaybeImage _ st = (Nothing, st)

splitAttribs :: ![(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s))]
             -> (![Maybe SVGAttr], ![Map String (ImageAttr s)], ![Map String (ImageAttr s)])
splitAttribs [] = ([], [], [])
splitAttribs [(x, y, z) : rest]
  #! (xs, ys, zs) = splitAttribs rest
  = ([x:xs], [y:ys], [z:zs])

genSVG :: !(Image s) !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
genSVG {content, mask, attribs, transform, tags, uniqId, totalSpanPreTrans = (txsp, tysp), totalSpanPostTrans = (txsp`, tysp`)} st
  #! (attribs, st)         = strictTRMapSt (genSVGImageAttr uniqId) ('DS'.toList attribs) st
  #! (txsp, st)            = evalSpan txsp st
  #! (tysp, st)            = evalSpan tysp st
  #! (txsp`, st)           = evalSpan txsp` st
  #! (tysp`, st)           = evalSpan tysp` st
  #! (maskId, st)          = imageMaskId st
  #! (imAts`, evts, drags) = splitAttribs attribs
  #! interactive           = strictFoldl (\acc (_, m1, m2) -> acc || not ('DM'.null m1) || not ('DM'.null m2)) False attribs
  #! (syn, st)             = genSVGImageContent content interactive (txsp, tysp) (maybe imAts` (const [Just (MaskAttr (mkUrl maskId)) : imAts`]) mask) transform tags uniqId st
  #! (mask, st)            = genSVGMaybeImage mask st
  = ({ genSVGSyn_svgElts       = mkElt maskId mask syn
     , genSVGSyn_imageSpanReal = (txsp`, tysp`)
     , genSVGSyn_events        = 'DM'.unions [syn.genSVGSyn_events : evts]
     , genSVGSyn_draggable     = 'DM'.unions [syn.genSVGSyn_draggable : drags]
     , genSVGSyn_idMap         = 'DM'.put (mkUniqId editletId uniqId) tags syn.genSVGSyn_idMap
     }, st)
  where
  genSVGImageAttr :: !Int !(ImageAttr s) !*(GenSVGStVal s)
                  -> *(!(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*(GenSVGStVal s)) | iTask s
  genSVGImageAttr _ (ImageStrokeAttr { stroke }) st
    = ((Just (StrokeAttr (PaintColor stroke Nothing)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageStrokeWidthAttr { strokewidth }) st
    #! (w, st) = evalSpan strokewidth st
    = ((Just (StrokeWidthAttr (StrokeWidthLength (toString w, PX))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageXRadiusAttr { xradius }) st
    #! (r, st) = evalSpan xradius st
    = ((Just (RxAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageYRadiusAttr { yradius }) st
    #! (r, st) = evalSpan yradius st
    = ((Just (RyAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageStrokeOpacityAttr { opacity }) st
    = ((Just (StrokeOpacityAttr (toString opacity)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageFillOpacityAttr { opacity }) st
    = ((Just (FillOpacityAttr (FillOpacity (toString opacity))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageFillAttr { fill }) st
    = ((Just (FillAttr (PaintColor fill Nothing)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr _ (ImageDashAttr { dash }) st
    = ((Just (StrokeDashArrayAttr (DashArray (strictTRMap toString dash))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr uniqId attr=:(ImageOnClickAttr _) st
    = mkEvent mkOnClickId uniqId attr st
  genSVGImageAttr uniqId attr=:(ImageOnMouseDownAttr _) st
    = mkEvent mkOnMouseDownId uniqId attr st
  genSVGImageAttr uniqId attr=:(ImageOnMouseUpAttr _) st
    = mkEvent mkOnMouseUpId uniqId attr st
  genSVGImageAttr uniqId attr=:(ImageOnMouseOverAttr _) st
    = mkEvent mkOnMouseOverId uniqId attr st
  genSVGImageAttr uniqId attr=:(ImageOnMouseMoveAttr _) st
    = mkEvent mkOnMouseMoveId uniqId attr st
  genSVGImageAttr uniqId attr=:(ImageOnMouseOutAttr _) st
    = mkEvent mkOnMouseOutId uniqId attr st
  genSVGImageAttr uniqId (ImageDraggableAttr { draggable = Nothing }) st
    = ((Nothing, 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr uniqId attr=:(ImageDraggableAttr _) st
    #! ocId = mkUniqId editletId uniqId
    = ((Nothing, 'DM'.newMap, 'DM'.singleton ocId attr), st)
  genSVGImageAttr _ _ st
    = ((Nothing, 'DM'.newMap, 'DM'.newMap), st)

  mkEvent :: !(String Int -> String) !Int !(ImageAttr s) !*(GenSVGStVal s)
          -> *(!(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*(GenSVGStVal s)) | iTask s
  mkEvent mkIdFun uniqId attr st
    #! ocId = mkIdFun editletId uniqId
    = ((Nothing, 'DM'.singleton ocId attr, 'DM'.newMap), st)

  genSVGImageContent :: !(ImageContent s) !Bool !ImageSpanReal ![Maybe SVGAttr] ![ImageTransform] !(Set ImageTag) Int !*(GenSVGStVal s)
                     -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
  genSVGImageContent (Basic baIm (xsp, ysp)) interactive _ attribs transform _ uniqId st
    #! isText          = case baIm of
                           TextImage _ _ -> True
                           _             -> False
    #! (xsp, st)       = evalSpan xsp st
    #! (ysp, st)       = evalSpan ysp st
    #! imSp            = (xsp, ysp)
    #! (transform, st) = strictTRMapSt (genSVGTransform isText imSp) transform st
    = genSVGBasicImage interactive attribs (flattenTR transform) baIm imSp st
    where
    genSVGBasicImage :: !Bool ![Maybe SVGAttr] ![SVGTransform] !BasicImage !ImageSpanReal !*(GenSVGStVal s)
                     -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    genSVGBasicImage interactive attribs transform EmptyImage imSp st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup hattrs (mkAttrs attribs transform) [] }, st)
    genSVGBasicImage interactive attribs transform (TextImage fd str) imSp st
      // TODO Currently we manually translate text by fontysize pixels to
      // compensate for the "auto" baseline. The result look OK, but a bit off
      // compare to the old approach where we forced the origin to be the
      // top-left corner (which didn't work with zooming)
      // We need to offset by the font's descent height, but that's not easy to
      // calculate currently (there are no JS APIs for that yet). Current
      // heuristic: we assume that the ex-height is half of the font height. We
      // assume that the descent height is half of the ex-height. Therefore, we
      // multiply by 0.75
      #! hattrs = [XmlspaceAttr "preserve"]
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [TextElt hattrs (addAttr (TransformAttr [TranslateTransform (toString 0.0) (toString (fd.fontysize * 0.75))]) (mkAttrs attribs transform ++ fontAttrs fd.fontysize)) str] }
         , st)
      where
      fontAttrs :: !Real -> [SVGAttr]
      fontAttrs fsz = [ AlignmentBaselineAttr "auto"
                      , DominantBaselineAttr "auto"
                      , FontFamilyAttr fd.fontfamily
                      , FontSizeAttr (toString fsz)
                      , FontStyleAttr fd.fontstyle
                      , FontStretchAttr fd.fontstretch
                      , FontVariantAttr fd.fontvariant
                      , FontWeightAttr fd.fontweight
                      , TextRenderingAttr "geometricPrecision"
                      ]
    genSVGBasicImage interactive attribs transform RectImage imSp st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [RectElt hattrs (mkAttrs attribs transform)] }, st)
    genSVGBasicImage interactive attribs transform (RawImage svgStr) imSp st
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [RawElt svgStr] }, st)

    genSVGBasicImage interactive attribs transform CircleImage (imXSp`, _) st
      #! r = imXSp` / 2.0
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [CircleElt (if interactive [IdAttr (mkUniqId editletId uniqId)] [])
                                               [ RAttr (toString (to2dec r), PX), CxAttr (toString (to2dec r), PX)
                                               , CyAttr (toString (to2dec r), PX) : (mkAttrs attribs transform) ]] }, st)
    genSVGBasicImage interactive attribs transform EllipseImage (imXSp, imYSp) st
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [EllipseElt (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs attribs transform ++
                                               [ RxAttr (toString (to2dec (imXSp / 2.0)), PX), RyAttr (toString (to2dec (imYSp / 2.0)), PX)
                                               , CxAttr (toString (to2dec (imXSp / 2.0)), PX), CyAttr (toString (to2dec (imYSp / 2.0)), PX)])] }, st)
  genSVGImageContent (Line {lineSpan = (xsp, ysp), markers, lineContent}) interactive totalSpanPreTrans attribs transform tags uniqId st
    #! (xsp, st)       = evalSpan xsp st
    #! (ysp, st)       = evalSpan ysp st
    #! (transform, st) = strictTRMapSt (genSVGTransform False (xsp, ysp)) transform st
    #! (markers, st)   = genSVGMarkers markers st
    = genSVGLineContent (xsp, ysp) markers (flattenTR transform) lineContent st
    where
    genSVGMarkers :: !(Maybe (Markers s)) !*(GenSVGStVal s)
                  -> *(!Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s)), !*GenSVGStVal s) | iTask s
    genSVGMarkers (Just { markerStart, markerMid, markerEnd }) st
      #! (markerStart, st) = genSVGMaybeImage markerStart st
      #! (markerMid, st)   = genSVGMaybeImage markerMid st
      #! (markerEnd, st)   = genSVGMaybeImage markerEnd st
      = (Just (markerStart, markerMid, markerEnd), st)
    genSVGMarkers _ st = (Nothing, st)

    genSVGLineContent :: !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s))) ![SVGTransform] !LineContent !*(GenSVGStVal s)
                      -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    genSVGLineContent sp=:(xspan, yspan) markers transform (SimpleLineImage sl) st
      #! (y1, y2) = case sl of
                      Slash     -> (toString (to2dec yspan), "0.0")
                      Backslash -> ("0.0", toString (to2dec yspan))
      = mkLine LineElt [X1Attr ("0.0", PX), X2Attr (toString (to2dec xspan), PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : mkAttrs attribs transform] sp markers st
    genSVGLineContent sp=:(xspan, yspan) markers transform (PolygonImage points) st
      #! (offsets, st) = evalListOfSpanPair points st
      = mkLine PolygonElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs attribs transform] sp markers st
    mkPolylineImage sp=:(xspan, yspan) markers transform (PolylineImage points) st
      #! (offsets, st) = evalListOfSpanPair points st
      = mkLine PolylineElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs attribs transform] sp markers st

    mkLine :: !([HtmlAttr] [SVGAttr] -> SVGElt) ![SVGAttr] !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) !*(GenSVGStVal s)
           -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) clval
      #! (uid1, clval) = nextNo clval
      #! (uid2, clval) = nextNo clval
      #! (uid3, clval) = nextNo clval
      #! markersAndIds = [  (m, i, s, d, p)
                         \\ Just (m, i, s, d, p) <- [ mkMarkerAndId mmStart (mkMarkerId editletId uid1) MarkerStartAttr
                                                    , mkMarkerAndId mmMid   (mkMarkerId editletId uid2) MarkerMidAttr
                                                    , mkMarkerAndId mmEnd   (mkMarkerId editletId uid3) MarkerEndAttr ]]
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts   = [ constr [] (atts ++ strictTRMap (\(_, x, _, _, _) -> x) markersAndIds)
                                 , DefsElt [] [] (strictTRMap (\(x, _, _, _, _) -> x) markersAndIds)]
         , genSVGSyn_events    = 'DM'.unions (strictTRMap (\(_, _, x, _, _) -> x) markersAndIds)
         , genSVGSyn_draggable = 'DM'.unions (strictTRMap (\(_, _, _, x, _) -> x) markersAndIds)
         , genSVGSyn_idMap     = 'DM'.unions (strictTRMap (\(_, _, _, _, x) -> x) markersAndIds)
         }, clval) // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId :: !(Maybe (GenSVGSyn s)) !String !(String -> SVGAttr) -> Maybe (!SVGElt, !SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s), !Map String (Set ImageTag)) | iTask s
      mkMarkerAndId (Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal = (w, h), genSVGSyn_events, genSVGSyn_draggable, genSVGSyn_idMap}) mid posAttr
        #! wStr = toString (to2dec w)
        #! hStr = toString (to2dec h)
        = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto"
                                        , ViewBoxAttr "0" "0" wStr hStr
                                        , RefXAttr (wStr, PX)
                                        , RefYAttr (toString (to2dec (h / 2.0)), PX)
                                        , MarkerHeightAttr (hStr, PX)
                                        , MarkerWidthAttr (wStr, PX)
                                        ] genSVGSyn_svgElts
               , posAttr (mkUrl mid)
               , genSVGSyn_events
               , genSVGSyn_draggable
               , genSVGSyn_idMap)
      mkMarkerAndId _ _ _ = Nothing
    mkLine constr atts spans _ st = ({ mkGenSVGSyn & genSVGSyn_svgElts = [constr [] atts]}, st)

  genSVGImageContent (Composite {host, compose}) interactive totalSpanPreTrans attribs transform tags uniqId st
    #! (host, st)    = genSVGMaybeImage host st
    #! (compose, st) = genSVGCompose compose host totalSpanPreTrans attribs transform tags st
    #! (cpId, st)    = getCpId st
    #! (elts, spans, onclicks, draggables, idMap)
         = case host of
             Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal, genSVGSyn_events, genSVGSyn_draggable, genSVGSyn_idMap}
               = (genSVGSyn_svgElts ++ compose.genSVGSyn_svgElts, genSVGSyn_imageSpanReal, 'DM'.union genSVGSyn_events compose.genSVGSyn_events, 'DM'.union genSVGSyn_draggable compose.genSVGSyn_draggable, 'DM'.union genSVGSyn_idMap compose.genSVGSyn_idMap)
             _ = (compose.genSVGSyn_svgElts, compose.genSVGSyn_imageSpanReal, compose.genSVGSyn_events, compose.genSVGSyn_draggable, compose.genSVGSyn_idMap)
    #! (transform, st) = strictTRMapSt (genSVGTransform False spans) transform st
    = ({ genSVGSyn_imageSpanReal = (0.0, 0.0)
       , genSVGSyn_svgElts       = mkGroup (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs attribs (flattenTR transform)) elts
       , genSVGSyn_events        = onclicks
       , genSVGSyn_draggable     = draggables
       , genSVGSyn_idMap         = idMap
       }, st)
    where
    getCpId :: !*(GenSVGStVal s) -> (!String, !*GenSVGStVal s) | iTask s
    getCpId clval
      #! (n, clval) = nextNo clval
      = (mkClipPathId editletId n, clval)
    genSVGCompose :: !(Compose s) !(Maybe (GenSVGSyn s)) !ImageSpanReal ![Maybe SVGAttr] ![ImageTransform] !(Set ImageTag) !*(GenSVGStVal s)
                  -> (!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    genSVGCompose (AsCollage offsets imgs) host totalSpanPreTrans attribs transform tags st
      #! (offsets, st) = evalListOfSpanPair offsets st
      #! (imgsSps, st) = strictTRMapSt genSVG imgs st
      = ({ genSVGSyn_svgElts       = flattenTR (strictTRZipWith mkTranslateGroup offsets (strictTRMap (\x -> x.genSVGSyn_svgElts) imgsSps))
         , genSVGSyn_events        = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_events) imgsSps)
         , genSVGSyn_draggable     = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_draggable) imgsSps)
         , genSVGSyn_idMap         = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_idMap) imgsSps)
         , genSVGSyn_imageSpanReal = totalSpanPreTrans }, st) // Setting genSVGSyn_imageSpanReal is required here. It needs to be totalSpanPreTrans, because transforms will be calculated just after this.
    // These aren't used. They're translated to collages in fixSpans. We
    // provide them here only because we must if we don't want the evaluation
    // to crash.
    genSVGCompose compose host totalSpanPreTrans attribs transform tags st
      = (mkGenSVGSyn, st)

  genSVGTransform :: !Bool !ImageSpanReal !ImageTransform !*(GenSVGStVal s)
                  -> (![SVGTransform], !*GenSVGStVal s) | iTask s
  genSVGTransform isText (xsp, ysp) (RotateImage imAn) st
    // FIXME: We currently devide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
    #! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
    = ([RotateTransform (toString (to2dec (toDeg imAn))) (Just (toString (to2dec (xsp / 2.0)), toString (to2dec yoff)))], st)
  genSVGTransform _ _ (SkewXImage imAn) st
    = ([SkewXTransform (toString (toDeg imAn))], st)
  genSVGTransform _ _ (SkewYImage imAn) st
    = ([SkewYTransform (toString (toDeg imAn))], st)
  genSVGTransform isText (xsp, ysp) (FitImage sp1 sp2) st
    #! (sp1, st) = evalSpan sp1 st
    #! (sp2, st) = evalSpan sp2 st
    #! factorx   = to2dec (sp1 / xsp)
    #! factory   = to2dec (sp2 / ysp)
    #! attrs     = [ScaleTransform (toString factorx) (toString factory)]
    #! attrs     = case isText of
                     True
                       = [TranslateTransform "0" (toString ysp) : attrs]
                     _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (FitXImage sp) st
    #! (sp, st) = evalSpan sp st
    #! factor   = to2dec (sp / xsp)
    #! scale    = if (xsp > 0.0) (toString factor) "1.0"
    #! attrs    = [ScaleTransform scale scale]
    #! attrs    = case isText of
                    True
                      #! yoff = to2dec (ysp * 0.7 * factor)
                      = [TranslateTransform "0" (toString yoff) : attrs]
                    _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (FitYImage sp) st
    #! (sp, st) = evalSpan sp st
    #! factor   = to2dec (sp / ysp)
    #! scale    = if (ysp > 0.0) (toString factor) "1.0"
    #! attrs    = [ScaleTransform scale scale]
    #! attrs    = case isText of
                    True
                      = [TranslateTransform "0" (toString ysp) : attrs]
                    _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleImage sp1 sp2) st
    #! attrs = [ScaleTransform (toString sp1) (toString sp2)]
    #! attrs = case isText of
                 True
                   = [TranslateTransform "0" (toString ysp) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleXImage sp) st
    #! attrs = [ScaleTransform (toString sp) "1.0"]
    #! attrs = case isText of
                 True
                   #! yoff = to2dec (ysp * 0.7 * sp)
                   = [TranslateTransform "0" (toString yoff) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleYImage sp) st
    #! attrs = [ScaleTransform "1.0" (toString sp)]
    #! attrs = case isText of
                 True
                   = [TranslateTransform "0" (toString ysp) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) FlipXImage st
    = ([TranslateTransform (toString xsp) "0", ScaleTransform "-1" "1"], st)
  genSVGTransform isText (xsp, ysp) FlipYImage st
    #! ysp = if isText ((~ ysp) * 0.7) ysp
    = ([TranslateTransform "0" (toString ysp), ScaleTransform "1" "-1"], st)

stTrace :: !a !*(!cl, !*JSWorld) -> *(cl, *JSWorld)
stTrace x (clval, world)
  #! world = jsTrace x world
  = (clval, world)

instance + (Real, Real) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _      _      []                  = []
mkGroup []     []     xs                  = xs
mkGroup hattrs []     [GElt [] sattrs xs] = [GElt hattrs sattrs xs]
mkGroup []     sattrs [GElt hattrs [] xs] = [GElt hattrs sattrs xs]
mkGroup []     [tfattr=:(TransformAttr [TranslateTransform x y])] xs = map f xs
  where
  f :: !SVGElt -> SVGElt
  f (GElt        hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = GElt       hattrs (addAttr (dualTransformTranslate x y x` y`) attrs) elts
  f (GElt        hattrs attrs elts)                                              = GElt       hattrs (addAttr tfattr attrs) elts
  f (TextElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = TextElt    hattrs (addAttr (dualTransformTranslate x y x` y`) attrs) elts
  f (TextElt     hattrs attrs elts)                                              = TextElt    hattrs (addAttr tfattr attrs) elts
  f (EllipseElt  hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = EllipseElt hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (EllipseElt  hattrs attrs)                                                   = EllipseElt hattrs (addAttr tfattr attrs)
  f (RectElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = RectElt    hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (RectElt     hattrs attrs)                                                   = RectElt    hattrs (addAttr tfattr attrs)
  f (CircleElt   hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = CircleElt  hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (CircleElt   hattrs attrs)                                                   = CircleElt  hattrs (addAttr tfattr attrs)
  f (LineElt _ [X1Attr (x1, PX), X2Attr (x2, PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : attrs]) = LineElt [] [X1Attr (lineAdd x1 x, PX), X2Attr (lineAdd x2 x, PX), Y1Attr (lineAdd y1 y, PX), Y2Attr (lineAdd y2 y, PX) : attrs]
  f elt                                                                                      = GElt    [] [tfattr] [elt]
  lineAdd :: !String !SVGNumber -> String
  lineAdd strVal n = toString (to2dec (toReal strVal + toReal n))
mkGroup has    sas elts = [GElt has sas elts]

dualTransformTranslate :: !a !a !a !a -> SVGAttr | toReal a
dualTransformTranslate x y x` y` = TransformAttr [TranslateTransform (toString (to2dec (toReal x + toReal x`))) (toString (to2dec (toReal y + toReal y`)))]

addAttr :: !SVGAttr ![SVGAttr] -> [SVGAttr]
addAttr (TransformAttr tfs) attrs = addTransforms tfs attrs []
  where
  addTransforms :: ![SVGTransform] ![SVGAttr] ![SVGAttr] -> [SVGAttr]
  addTransforms tfs []                            acc = reverseTR acc ++ [TransformAttr tfs]
  addTransforms tfs [TransformAttr tfs` : attrs`] acc = reverseTR acc ++ [TransformAttr (tfs ++ tfs`) : attrs`]
  addTransforms tfs [attr:attrs]                  acc = addTransforms tfs attrs [attr:acc]
addAttr attr attrs = [attr:attrs]

addAttrs :: ![SVGAttr] ![SVGAttr] -> [SVGAttr]
addAttrs newAttrs oldAttrs = strictFoldr addAttr oldAttrs newAttrs

evalOffsets :: ![(!State .st a, !State .st a)] !.st -> *(![(!a, !a)], !.st)
evalOffsets offsets st = strictTRMapSt f offsets st
  where
  f :: !(!.a -> (!b, !.c), !.c -> *(!d, !.e)) !.a -> (!(!b, !d), !.e)
  f (sp1, sp2) st
    #! (sp1, st) = sp1 st
    #! (sp2, st) = sp2 st
    = ((sp1, sp2), st)

evalMaybe :: !(Maybe (State .s a)) !.s -> *(!Maybe a, !.s)
evalMaybe (Just x) st
  #! (x, st) = x st
  = (Just x, st)
evalMaybe _ st = (Nothing, st)

ret :: !a !.s -> (!a, !.s)
ret x st = (x, st)

mkAttrs :: ![Maybe SVGAttr] ![SVGTransform] -> [SVGAttr]
mkAttrs imAts [] = getSvgAttrs imAts
mkAttrs imAts xs = addAttr (TransformAttr xs) (getSvgAttrs imAts)

calcAlignOffset :: !Span !Span !(!Span, !Span) !ImageAlign -> (!Span, !Span)
calcAlignOffset maxxsp maxysp (imXSp, imYSp) (xal, yal) = (mkXAl maxxsp imXSp xal, mkYAl maxysp imYSp yal)
  where
  mkXAl :: !Span !Span !XAlign -> Span
  mkXAl maxxsp imXSp AtLeft    = zero
  mkXAl maxxsp imXSp AtMiddleX = (maxxsp /. 2.0) - (imXSp /. 2.0)
  mkXAl maxxsp imXSp AtRight   = maxxsp - imXSp

  mkYAl :: !Span !Span !YAlign -> Span
  mkYAl maxysp imYSp AtTop     = zero
  mkYAl maxysp imYSp AtMiddleY = (maxysp /. 2.0) - (imYSp /. 2.0)
  mkYAl maxysp imYSp AtBottom  = maxysp - imYSp

calculateComposedSpan :: ![(!Span, !Span)] ![(!Span, !Span)] -> (!Span, !Span)
calculateComposedSpan spans offs
  = strictFoldl f (zero, zero) (strictTRZip2Rev offs spans)
  where
  f :: !(!Span, !Span) !(!(!Span, !Span), !(!Span, !Span)) -> (!Span, !Span)
  f (maxX, maxY) ((xoff, yoff), (imXSp, imYSp)) = (maxSpan [maxX, xoff + imXSp], maxSpan [maxY, yoff + imYSp])

mkTranslateGroup :: !ImageOffsetReal ![SVGElt] -> [SVGElt]
mkTranslateGroup (xoff, yoff) contents
  = mkGroup [] (mkTransformTranslateAttr (to2dec xoff, to2dec yoff)) contents

mkTransformTranslateAttr :: !(!Real, !Real) -> [SVGAttr]
mkTransformTranslateAttr (0.0,   0.0)   = []
mkTransformTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

evalListOfSpanPair :: ![(!Span, !Span)] !*(GenSVGStVal s) -> *(![(!Real, !Real)], !*(GenSVGStVal s)) | iTask s
evalListOfSpanPair xs st = mapSt evalSpanPair xs st

evalSpanPair :: !(!Span, !Span) !*(GenSVGStVal s) -> *(!(!Real, !Real), !*(GenSVGStVal s)) | iTask s
evalSpanPair (xsp, ysp) st
    #! (xsp, st) = evalSpan xsp st
    #! (ysp, st) = evalSpan ysp st
    = ((xsp, ysp), st)

evalSpan :: !Span !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
evalSpan (PxSpan r)    st = (r, st)
evalSpan (AddSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l + r, st)
evalSpan (SubSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l - r, st)
evalSpan (MulSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l * r, st)
evalSpan (DivSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l / r, st)
evalSpan (AbsSpan x)   st
  #! (x, st) = evalSpan x st
  = (abs x, st)
evalSpan (MinSpan xs)  st
  #! (xs, st) = mapSt evalSpan xs st
  = (minList xs, st)
evalSpan (MaxSpan xs)  st
  #! (xs, st) = mapSt evalSpan xs st
  = (maxList xs, st)
evalSpan (LookupSpan lu) st = evalLookupSpans lu st

evalLookupSpans :: !LookupSpan !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
evalLookupSpans (TextXSpan fd str) st
  #! sw = case 'DM'.get fd st.genStates.spanEnvFonts of
            Just fs -> case 'DM'.get str fs of
                         Just sw -> sw
                         _       -> 0.0
            _       -> 0.0
  = (sw, st)
evalLookupSpans (ImageXSpan t)     st
  #! genStates                = st.genStates
  #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
  #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = genStates.spanEnvImageSpanPostTrans
        #! genStates                 = {genStates & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (PxSpan x, _)
              = (x, {st & genStates = genStates})
            Just (xsp, _)
              = evalSpan xsp {st & genStates = genStates}
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (ImageYSpan t)     st
  #! genStates                = st.genStates
  #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
  #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = genStates.spanEnvImageSpanPostTrans
        #! genStates                 = {genStates & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (_, PxSpan x)
              = (x, {st & genStates = genStates})
            Just (_, ysp)
              = evalSpan ysp {st & genStates = genStates}
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (ColumnXSpan t colIdx)  st
  #! genStates      = st.genStates
  #! spanEnvGridTag = genStates.spanEnvGridTag
  #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = genStates.spanEnvGridSpan
        #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (xs, _)
              = case 'DIS'.get colIdx xs of
                  Just (PxSpan x) -> (x, {st & genStates = genStates})
                  Just sp         -> evalSpan sp {st & genStates = genStates}
                  _               -> (0.0, {st & genStates = genStates})
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (RowYSpan t rowIdx)     st
  #! genStates      = st.genStates
  #! spanEnvGridTag = genStates.spanEnvGridTag
  #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = genStates.spanEnvGridSpan
        #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (_, xs)
              = case 'DIS'.get rowIdx xs of
                  Just (PxSpan x) -> (x, {st & genStates = genStates})
                  Just sp         -> evalSpan sp {st & genStates = genStates}
                  _               -> (0.0, {st & genStates = genStates})
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
