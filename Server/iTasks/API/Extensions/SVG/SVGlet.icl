implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import Graphics.Scalable.Internal
import iTasks
import iTasks.API.Core.Client.Editlet
from StdOrdList import minList, maxList
import StdOverloaded
import StdArray
import StdMisc
import Data.List
import Data.Func
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Set as DS
from StdFunc import `bind`, flip
import Text
from Data.IntMap.Strict import :: IntMap, instance Functor IntMap
import qualified Data.IntMap.Strict as DIS
import Data.Matrix

:: *GenSVGStVal s =
  { uniqueIdCounter :: !Int
  , genStates       :: !*SpanEnvs
  }

:: DropTarget = DropTarget

:: SVGClSt s =
  { svgNumClicks    :: !Int
  , svgClickTimeout :: !Maybe (JSVal Int)
  , svgClSt         :: !Maybe s
  , svgMousePos     :: !MousePos
  , svgDropCallback :: !Maybe ((Maybe (Set ImageTag)) Real Real s -> s)
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe (JSObj DropTarget)
  }

mainSvgId :: !ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

mkMouseDragDown :: !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                   !ComponentId ((Maybe (Set ImageTag)) Real Real s -> s)
                   !String !(JSObj o) String {JSObj JSEvent} !(SVGClSt s)
                   !*JSWorld
                -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
mkMouseDragDown resolve state2image cid sttf elemId _ _ evts=:{[0] = evt} clval world
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

mkMouseDragMove :: !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                   !ComponentId !(JSObj o) String {JSObj JSEvent} !(SVGClSt s)
                   !*JSWorld
                -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
mkMouseDragMove _ _ cid _ _ evts=:{[0] = evt} clval=:{svgMousePos = MouseDown, svgDragTarget = Just dragTarget} world
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
mkMouseDragMove _ _ cid   _   _ evts=:{[0] = evt} clval world
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

mkMouseDragUp :: !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                 !ComponentId !(JSObj o) !(Map String (Set ImageTag)) String
                 {JSObj JSEvent} !(SVGClSt s) !*JSWorld
              -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
mkMouseDragUp resolve state2image cid _ idMap _ evts=:{[0] = evt} clval=:{svgDragTarget = Just dragTarget} world
  #! (_, world)         = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (evtTarget, world) = .? (evt .# "target") world
  #! (parentId, world)  = firstIdentifiableParentId evtTarget world
  #! diff               = case (clval.svgDropCallback, clval.svgClSt) of
                            (Just sttf, Just clSt) -> Diff (SetState (sttf ('DM'.get parentId idMap) (clval.svgTrueCoordsX - clval.svgGrabPointX) (clval.svgTrueCoordsY - clval.svgGrabPointY) clSt)) (doResolve resolve)
                            _         -> NoDiff
  = ({clval & svgMousePos = MouseUp, svgDragTarget = Nothing}, diff, world)
mkMouseDragUp _ _ _ _ _ _ _ clval=:{svgDragTarget = Nothing} world
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

registerDraggables :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                      !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                      !ComponentId !(JSObj svg) !(Map String (ImageAttr s))
                      !(Map String (Set ImageTag)) !*JSWorld
                   -> *JSWorld | iTask s
registerDraggables mkEventHandler resolve state2Image cid svg draggables idMap world
  #! (svgContainer, world) = .? (getElementById (mainSvgId cid)) world
  #! (svgRoot, world)      = .? (svgContainer .# "firstChild") world
  #! idMap                 = 'DM'.foldrWithKey (\k v m -> 'DM'.put (replaceSubString editletId cid k) v m) 'DM'.newMap idMap
  #! cbUp                  = mkEventHandler (mkMouseDragUp   resolve state2Image cid svgRoot idMap) cid
  #! cbMove                = mkEventHandler (mkMouseDragMove resolve state2Image cid svgRoot) cid
  #! (_, world)            = (svgRoot `addEventListener` ("mouseup",   cbUp,   True)) world
  #! (_, world)            = (svgRoot `addEventListener` ("mousemove", cbMove, True)) world
  = 'DM'.foldrWithKey (registerDraggable mkEventHandler resolve state2Image cid svg) world draggables
  where
  registerDraggable :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                       !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                       !ComponentId !(JSObj svg) !String !(ImageAttr s)
                       !*JSWorld
                    -> *JSWorld | iTask s
  registerDraggable _ resolve state2image cid svg _      (ImageDraggableAttr {draggable = Nothing}) world = world
  registerDraggable mkEventHandler resolve state2image cid svg elemId (ImageDraggableAttr {draggable = Just sttf}) world
    #! elemId        = replaceSubString editletId cid elemId
    #! (elem, world) = (svg .# "getElementById" .$ elemId) world
    #! cbDown        = mkEventHandler (mkMouseDragDown resolve state2image cid sttf elemId elem) cid
    #! (_, world)    = (elem `addEventListener` ("mousedown", cbDown, True)) world
    = world

registerSVGEvents :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                     !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                     !ComponentId !(JSObj svg) !(Map String (ImageAttr s))
                     !*JSWorld
                  -> *JSWorld | iTask s
registerSVGEvents mkEventHandler resolve state2Image cid svg onclicks world
  = 'DM'.foldrWithKey (registerEvent mkEventHandler resolve state2Image cid svg) world onclicks
  where
  registerEvent :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                   !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                   !ComponentId !(JSObj svg) !String !(ImageAttr s) !*JSWorld
                -> *JSWorld | iTask s
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnClickAttr     {local, onclick})     world = registerNClick   mkEventHandler resolve state2image cid svg elemId             onclick     local world
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnMouseDownAttr {local, onmousedown}) world = actuallyRegister mkEventHandler resolve state2image cid svg elemId "mousedown" onmousedown local world
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnMouseUpAttr   {local, onmouseup})   world = actuallyRegister mkEventHandler resolve state2image cid svg elemId "mouseup"   onmouseup   local world
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnMouseOverAttr {local, onmouseover}) world = actuallyRegister mkEventHandler resolve state2image cid svg elemId "mouseover" onmouseover local world
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnMouseMoveAttr {local, onmousemove}) world = actuallyRegister mkEventHandler resolve state2image cid svg elemId "mousemove" onmousemove local world
  registerEvent mkEventHandler resolve state2image cid svg elemId (ImageOnMouseOutAttr  {local, onmouseout})  world = actuallyRegister mkEventHandler resolve state2image cid svg elemId "mouseout"  onmouseout  local world

actuallyRegister :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                    !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                    !ComponentId !(JSObj svg) !String !String !(s -> s) !Bool
                    *JSWorld
                 -> *JSWorld | iTask s
actuallyRegister mkEventHandler resolve state2image cid svg elemId evt sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! cb            = mkEventHandler (mkCB mkEventHandler resolve state2image cid sttf local) cid
  #! (_, world)    = (elem `addEventListener` (evt, cb, True)) world
  = world

mkCB :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
        !(Conflict s -> Maybe s) !(s *TagSource -> Image s) !ComponentId
        !(s -> s) !Bool String {JSObj JSEvent} !(SVGClSt s) !*JSWorld
     -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
mkCB mkEventHandler resolve state2image cid sttf local _ _ clval=:{svgClSt = Just svgClSt} world
  #! st` = sttf svgClSt
  #! (clval, world) = if local
                        (appClientDiff resolve state2image mkEventHandler cid (SetState st`) clval world)
                        (clval, world)
  = ( clval
    , if local NoDiff (Diff (SetState st`) (doResolve resolve))
    , world)
mkCB mkEventHandler resolve state2image cid sttf local _ _ clval world
  = (clval, NoDiff, world)

doResolve :: !(Conflict s -> Maybe s) !Conflict !(SVGClSt s) !*JSWorld
          -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld)
doResolve resolve c s=:{svgClSt = Just svgClSt} w
  = case resolve c svgClSt of
      Just s` -> (s, Diff (SetState s`) (doResolve resolve), w)
      _       -> (s, NoDiff, w)
doResolve _ _ s w = (s, NoDiff, w)

registerNClick :: !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                  !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                  !ComponentId !(JSObj svg) !String !(Int s -> s) !Bool
                  *JSWorld
               -> *JSWorld | iTask s
registerNClick mkEventHandler resolve state2image cid svg elemId sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! cb            = mkEventHandler (mkNClickCB elemId mkEventHandler sttf local) cid
  #! (_, world)    = (elem `addEventListener` ("click", cb, False)) world
  = world

mkNClickCB :: !String !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
              !(Int s -> s) !Bool !String !{JSObj JSEvent} !(SVGClSt s)
              !*JSWorld
           -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
mkNClickCB elemID mkEventHandler sttf local cid args clval=:{svgClSt = Just svgClSt, svgClickTimeout} world
  #! world = if (size args > 0) (snd ((args.[0] .# "stopPropagation" .$ ()) world)) world
  #! world = case svgClickTimeout of
               Just to -> snd (("clearTimeout" .$ to) world)
               _       -> world
  #! cb = mkEventHandler (handleNClick sttf local) cid
  #! (timeOut, world) = ("setTimeout" .$ (cb, 225)) world
  = ({clval & svgClickTimeout = Just timeOut, svgNumClicks = clval.svgNumClicks + 1}, NoDiff, world)
mkNClickCB elemID mkEventHandler sttf local cid args clval world
  = (clval, NoDiff, world)

handleNClick :: !(Int s -> s) !Bool !String !{JSObj JSEvent} !(SVGClSt s)
                !*JSWorld
             -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
handleNClick sttf local _ args clval=:{svgClSt = Just svgClSt, svgNumClicks} world
  #! st` = sttf svgNumClicks svgClSt
  = ( {clval & svgNumClicks = 0}
    , if local NoDiff (Diff (SetState st`) (\_ s w -> (s, NoDiff, w)))
    , world)
handleNClick sttf local _ args clval world = (clval, NoDiff, world)


imageView :: !(s *TagSource -> Image s) !(Conflict s -> Maybe s)
          -> ViewOption s | iTask s
imageView toImage resolve = ViewWith (\s -> svgRenderer resolve s toImage)

imageUpdate :: !(s -> v) !(v *TagSource -> Image v) !(Conflict v -> Maybe v)
               !(s v -> s`)
            -> UpdateOption s s` |  iTask v
imageUpdate toViewState toImage resolve fromViewState
  = UpdateWith (\s -> svgRenderer resolve (toViewState s) toImage) (\s e -> fromViewState s e.Editlet.currVal.svgSrvSt)

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

:: SVGSrvSt s =
  { svgSrvSt :: !s
  }

defaultSrvSt :: !s -> SVGSrvSt s
defaultSrvSt s = { svgSrvSt = s
                 }

:: MousePos = MouseUp | MouseDown

instance == MousePos where
  (==) MouseDown MouseDown = True
  (==) MouseUp   MouseUp   = True
  (==) _         _         = False

defaultClSt :: SVGClSt s
defaultClSt = { svgNumClicks    = 0
              , svgClickTimeout = Nothing
              , svgClSt         = Nothing
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
derive class iTask SVGDiff, SVGSrvSt, SVGClSt

svgRenderer :: !(Conflict s -> Maybe s) !s !(s *TagSource -> Image s)
            -> Editlet (SVGSrvSt s) (SVGDiff s) (SVGClSt s) | iTask s
svgRenderer resolve origState state2Image
  #! dst = defaultSrvSt origState
  = { currVal    = dst
    , defValSrv  = dst
    , genUI      = genUI
    , initClient = \_ _ world -> (defaultClSt, world)
    , appDiffClt = appClientDiff resolve state2Image
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

  genServerDiff :: !(SVGSrvSt s) !(SVGSrvSt s) -> Maybe (SVGDiff s) | iTask s
  genServerDiff oldSrvSt newSrvSt
    | oldSrvSt.svgSrvSt === newSrvSt.svgSrvSt = Just (SetState newSrvSt.svgSrvSt)
    | otherwise                               = Nothing

  appServerDiff :: !(SVGDiff s) !(SVGSrvSt s) -> SVGSrvSt s | iTask s
  appServerDiff (SetState st) srvSt = {srvSt & svgSrvSt = st}

imageFromState :: !(Image s) !(Map FontDef (Map String Real)) -> *(!Image s, !*SpanEnvs) | iTask s
imageFromState img env
  #! spanEnvs  = { spanEnvImageTagPreTrans   = 'DM'.newMap
                 , spanEnvImageTagPostTrans  = 'DM'.newMap
                 , spanEnvImageSpanPostTrans = 'DIS'.newMap
                 //, spanEnvImageSpanPostTrans` = createArray 1024 (PxSpan 0.0, PxSpan 0.0)
                 , spanEnvGridTag   = 'DM'.newMap
                 , spanEnvGridSpan  = 'DIS'.newMap
                 //, spanEnvGridSpan` = createArray 1024 (createArray 0 (PxSpan 0.0), createArray 0 (PxSpan 0.0))
                 , spanEnvFonts     = env
                 }
  #! (img, st) = desugarAndTag img { desugarAndTagCounter  = 0
                                   , desugarAndTagSpanEnvs = spanEnvs}
  = (img, st.desugarAndTagSpanEnvs)

appClientDiff :: !(Conflict s -> Maybe s) !(s *TagSource -> Image s)
                 !((EditletEventHandlerFunc (SVGDiff s) (SVGClSt s)) ComponentId -> JSFun f)
                 !String !(SVGDiff s) !(SVGClSt s) !*JSWorld
              -> *(!SVGClSt s, !*JSWorld) | iTask s
appClientDiff resolve state2Image mkEventHandler cid (SetState s) clst world
  | clst.svgClSt === Just s = (clst, world)
  #! image                = state2Image s [(ImageTagUser no cid,ImageTagUser no cid) \\ no <- [0..]]
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
  #! world                = registerSVGEvents mkEventHandler resolve state2Image cid newSVG syn.genSVGSyn_events world
  #! world                = registerDraggables mkEventHandler resolve state2Image cid newSVG syn.genSVGSyn_draggable syn.genSVGSyn_idMap world
  = ({clst & svgClSt = Just s}, world)

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
    #! world       = foldr (\args world -> snd ((elem `setAttribute` args) world)) world fontAttrs
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
  }

sequence ms :== strictTRMapSt id ms

cacheImageSpanPostTrans :: !Int !(Set ImageTag) !ImageSpan !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheImageSpanPostTrans n imTas sp st
  #! spanEnvs = st.desugarAndTagSpanEnvs
  //#! spanEnvImageSpanPostTrans` = spanEnvs.spanEnvImageSpanPostTrans`
  //#! spanEnvImageSpanPostTrans` = {spanEnvImageSpanPostTrans` & [n] = sp}
  #! spanEnvs = {spanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put n sp spanEnvs.spanEnvImageSpanPostTrans}
  //#! spanEnvs = {spanEnvs & spanEnvImageSpanPostTrans` = spanEnvImageSpanPostTrans`}
  #! env      = 'DS'.fold (f n) spanEnvs.spanEnvImageTagPostTrans imTas
  #! spanEnvs = {spanEnvs & spanEnvImageTagPostTrans = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}
  where
  f :: !Int !ImageTag !(Map ImageTag Int) -> Map ImageTag Int
  f n t env = 'DM'.put t n env

cacheGridSpans :: !Int !(Set ImageTag) ![Span] ![Span] !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheGridSpans n imTas xsps ysps st
  #! xsps` = {x \\ x <- xsps}
  #! ysps` = {y \\ y <- ysps}
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvGridSpan = 'DIS'.put n (xsps`, ysps`) spanEnvs.spanEnvGridSpan}
  #! env      = 'DS'.fold (f n) spanEnvs.spanEnvGridTag imTas
  #! spanEnvs = {spanEnvs & spanEnvGridTag = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}
  where
  f :: !Int !ImageTag !(Map ImageTag Int) -> Map ImageTag Int
  f n t env = 'DM'.put t n env


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
  #! newPoints      = foldr f origPoints ts
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
  f _ coords = coords

gatherFonts :: !(Image s) -> Map FontDef (Set String)
gatherFonts img = imageCata gatherFontsAllAlgs img
  where
  gatherFontsAllAlgs :: Algebras s (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsAllAlgs =
    { imageAlgs          = gatherFontsImageAlgs
    , imageContentAlgs   = gatherFontsImageContentAlgs
    , imageAttrAlgs      = gatherFontsImageAttrAlgs
    , imageTransformAlgs = gatherFontsImageTransformAlgs
    , imageSpanAlgs      = gatherFontsImageSpanAlgs
    , basicImageAlgs     = gatherFontsBasicImageAlgs
    , lineImageAlgs      = gatherFontsLineImageAlgs
    , markersAlgs        = gatherFontsMarkersAlgs
    , lineContentAlgs    = gatherFontsLineContentAlgs
    , compositeImageAlgs = gatherFontsCompositeImageAlgs
    , composeAlgs        = gatherFontsComposeAlgs
    , spanAlgs           = gatherFontsSpanAlgs
    , lookupSpanAlgs     = gatherFontsLookupSpanAlgs
    }
  gatherFontsImageAlgs :: ImageAlg (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: !(Map FontDef (Set String)) !(Maybe (Map FontDef (Set String))) ![Map FontDef (Set String)] ![Map FontDef (Set String)] a b c d c -> Map FontDef (Set String)
    mkImage imCo mask imAts imTrs _ _ _ _ _ = gatherFontsUnions [imCo : maybeToList mask ++ imAts ++ imTrs]
  gatherFontsImageContentAlgs :: ImageContentAlg (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsImageContentAlgs =
    { imageContentBasicAlg     = binUnion
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
  gatherFontsImageAttrAlgs :: ImageAttrAlg s (Map FontDef (Set String))
  gatherFontsImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = const 'DM'.newMap
    , imageAttrStrokeWidthAttrAlg   = \{strokewidth} -> gatherFontsSpan strokewidth
    , imageAttrXRadiusAttrAlg       = \{xradius} -> gatherFontsSpan xradius
    , imageAttrYRadiusAttrAlg       = \{yradius} -> gatherFontsSpan yradius
    , imageAttrStrokeOpacityAttrAlg = const 'DM'.newMap
    , imageAttrFillAttrAlg          = const 'DM'.newMap
    , imageAttrFillOpacityAttrAlg   = const 'DM'.newMap
    , imageAttrOnClickAttrAlg       = const 'DM'.newMap
    , imageAttrOnMouseDownAttrAlg   = const 'DM'.newMap
    , imageAttrOnMouseUpAttrAlg     = const 'DM'.newMap
    , imageAttrOnMouseOverAttrAlg   = const 'DM'.newMap
    , imageAttrOnMouseMoveAttrAlg   = const 'DM'.newMap
    , imageAttrOnMouseOutAttrAlg    = const 'DM'.newMap
    , imageAttrDraggableAttrAlg     = const 'DM'.newMap
    , imageAttrDashAttrAlg          = const 'DM'.newMap
    }
  gatherFontsImageTransformAlgs :: ImageTransformAlg (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsImageTransformAlgs =
    { imageTransformRotateImageAlg = const 'DM'.newMap
    , imageTransformSkewXImageAlg  = const 'DM'.newMap
    , imageTransformSkewYImageAlg  = const 'DM'.newMap
    , imageTransformFitImageAlg    = binUnion
    , imageTransformFitXImageAlg   = id
    , imageTransformFitYImageAlg   = id
    , imageTransformFlipXImageAlg  = 'DM'.newMap
    , imageTransformFlipYImageAlg  = 'DM'.newMap
    }
  gatherFontsImageSpanAlgs :: ImageSpanAlg (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsImageSpanAlgs =
    { imageSpanAlg = binUnion
    }
  gatherFontsBasicImageAlgs :: BasicImageAlg (Map FontDef (Set String))
  gatherFontsBasicImageAlgs =
    { basicImageTextImageAlg    = mkTextXSpan
    , basicImageEmptyImageAlg   = 'DM'.newMap
    , basicImageCircleImageAlg  = 'DM'.newMap
    , basicImageRectImageAlg    = 'DM'.newMap
    , basicImageEllipseImageAlg = 'DM'.newMap
    }
    where
    mkTextXSpan :: !FontDef !String -> Map FontDef (Set String)
    mkTextXSpan fd str = 'DM'.put fd ('DS'.singleton str) 'DM'.newMap
  gatherFontsLineImageAlgs :: LineImageAlg (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage :: !(Map FontDef (Set String)) !(Maybe (Map FontDef (Set String))) !(Map FontDef (Set String)) -> Map FontDef (Set String)
    mkLineImage sp ms liCo = gatherFontsUnions [liCo:sp:maybeToList ms]
  gatherFontsMarkersAlgs :: MarkersAlg (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (Map FontDef (Set String))) !(Maybe (Map FontDef (Set String))) !(Maybe (Map FontDef (Set String))) -> Map FontDef (Set String)
    mkMarkers m1 m2 m3 = gatherFontsUnions (concatMap maybeToList [m1, m2, m3])
  gatherFontsLineContentAlgs :: LineContentAlg (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsLineContentAlgs =
    { lineContentSimpleLineImageAlg = const 'DM'.newMap
    , lineContentPolygonImageAlg    = gatherFontsUnions o (strictFoldl (\acc (x, y) -> [x:y:acc]) [])
    , lineContentPolylineImageAlg   = gatherFontsUnions o (strictFoldl (\acc (x, y) -> [x:y:acc]) [])
    }
  gatherFontsCompositeImageAlgs :: CompositeImageAlg (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: !(Maybe (Map FontDef (Set String))) !(Map FontDef (Set String)) -> Map FontDef (Set String)
    mkCompositeImage host compose = gatherFontsUnions [compose : maybeToList host]
  gatherFontsComposeAlgs :: ComposeAlg (Map FontDef (Set String)) (Map FontDef (Set String)) (Map FontDef (Set String))
  gatherFontsComposeAlgs =
    { composeAsGridAlg    = \_ offss _ imgss -> gatherFontsUnions (flattenTR imgss ++ (strictFoldl (\acc (x, y) -> [x:y:acc]) [] (flattenTR offss)))
    , composeAsCollageAlg = \  offs    imgs  -> gatherFontsUnions (imgs ++ strictFoldl (\acc (x, y) -> [x:y:acc]) [] offs)
    , composeAsOverlayAlg = \  offs  _ imgs  -> gatherFontsUnions (imgs ++ strictFoldl (\acc (x, y) -> [x:y:acc]) [] offs)
    }
gatherFontsSpanAlgs :: SpanAlg (Map FontDef (Set String)) (Map FontDef (Set String))
gatherFontsSpanAlgs =:
  { spanPxSpanAlg     = const 'DM'.newMap
  , spanLookupSpanAlg = id
  , spanAddSpanAlg    = binUnion
  , spanSubSpanAlg    = binUnion
  , spanMulSpanAlg    = flip (const id)
  , spanDivSpanAlg    = flip (const id)
  , spanAbsSpanAlg    = id
  , spanMinSpanAlg    = gatherFontsUnions
  , spanMaxSpanAlg    = gatherFontsUnions
  }
gatherFontsLookupSpanAlgs :: LookupSpanAlg (Map FontDef (Set String))
gatherFontsLookupSpanAlgs =:
  { lookupSpanColumnXSpanAlg  = const2 'DM'.newMap
  , lookupSpanRowYSpanAlg     = const2 'DM'.newMap
  , lookupSpanImageXSpanAlg   = const 'DM'.newMap
  , lookupSpanImageYSpanAlg   = const 'DM'.newMap
  , lookupSpanTextXSpanAlg    = mkTextXSpan
  }
  where
  mkTextXSpan :: !FontDef !String -> Map FontDef (Set String)
  mkTextXSpan fd str = 'DM'.put fd ('DS'.singleton str) 'DM'.newMap

binUnion :: !(Map FontDef (Set String)) !(Map FontDef (Set String)) -> Map FontDef (Set String)
binUnion x y = gatherFontsUnions [x, y]

const2 :: !a b c -> a
const2 x _ _ = x

gatherFontsUnions :: ![Map FontDef (Set String)] -> Map FontDef (Set String)
gatherFontsUnions m = 'DM'.unionsWith 'DS'.union m

gatherFontsSpan :: !Span -> Map FontDef (Set String)
gatherFontsSpan sp = spanCata gatherFontsSpanAlgs gatherFontsLookupSpanAlgs sp


desugarAndTag :: !(Image s) !*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal) | iTask s
desugarAndTag img st = imageCata desugarAndTagAllAlgs img st
  where
  desugarAndTagAllAlgs :: Algebras s
                     ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal))
                     (DesugarAndTagSt (ImageAttr s))
                     (DesugarAndTagSt ImageTransform)
                     (DesugarAndTagSt (Image s))
                     ((!Span, !Span) [ImageTransform] *DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal))
                     (DesugarAndTagSt (!Span, !Span))
                     ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal))
                     (DesugarAndTagSt (Image s))
                     ((Maybe (Image s)) [ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *((!Compose s, !(!Span, !Span)), !*DesugarAndTagStVal))
                     (*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal))
                     (*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal))
                     (DesugarAndTagSt (Markers s))
                     ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal))
                     (*DesugarAndTagStVal -> *(!LineContent, !*DesugarAndTagStVal)) | iTask s
  desugarAndTagAllAlgs =
    { imageAlgs          = desugarAndTagImageAlgs
    , imageContentAlgs   = desugarAndTagImageContentAlgs
    , imageAttrAlgs      = desugarAndTagImageAttrAlgs
    , imageTransformAlgs = desugarAndTagImageTransformAlgs
    , imageSpanAlgs      = desugarAndTagImageSpanAlgs
    , basicImageAlgs     = desugarAndTagBasicImageAlgs
    , lineImageAlgs      = desugarAndTagLineImageAlgs
    , markersAlgs        = desugarAndTagMarkersAlgs
    , lineContentAlgs    = desugarAndTagLineContentAlgs
    , compositeImageAlgs = desugarAndTagCompositeImageAlgs
    , composeAlgs        = desugarAndTagComposeAlgs
    , spanAlgs           = desugarAndTagSpanAlgs
    , lookupSpanAlgs     = desugarAndTagLookupSpanAlgs
    }
  desugarAndTagImageAlgs :: ImageAlg ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                                (DesugarAndTagSt (ImageAttr s))
                                (DesugarAndTagSt ImageTransform)
                                (DesugarAndTagSt Span)
                                (DesugarAndTagSt (Image s)) | iTask s
  desugarAndTagImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: !([ImageTransform] (Set ImageTag) -> (DesugarAndTagSt (DesugarAndTagSyn s)))
               !(Maybe (DesugarAndTagSt (Image s))) ![DesugarAndTagSt (ImageAttr s)] ![DesugarAndTagSt ImageTransform]
               !(Set ImageTag)
               Int
               (!*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal), !*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal))
               (!*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal), !*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal))
               (!*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal), !*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal))
               !*DesugarAndTagStVal
            -> *(!Image s, !*DesugarAndTagStVal) | iTask s
    mkImage imCo mask imAts imTrs imTas _ _ _ _ st
      #! (mask, st)   = evalMaybe mask st
      #! (imAts, st)  = sequence imAts st
      #! (imTrs, st)  = sequence imTrs st
      #! (syn, st)    = imCo imTrs imTas st
      #! (prexsp,  preysp)  = syn.desugarAndTagSyn_TotalSpan_PreTrans
      #! (postxsp, postysp) = syn.desugarAndTagSyn_TotalSpan_PostTrans
      #! (no, st)     = nextNo st
      #! imTas        = 'DS'.insert (ImageTagSystem no) imTas
      #! st           = cacheImageSpanPostTrans no imTas (postxsp, postysp) st
      #! img          = { Image
                        | content             = syn.desugarAndTagSyn_ImageContent
                        , mask                = mask
                        , attribs             = 'DS'.fromList imAts
                        , transform           = imTrs
                        , tags                = imTas
                        , uniqId              = no
                        , totalSpanPreTrans   = (prexsp, preysp)
                        , totalSpanPostTrans  = (postxsp, postysp)
                        , transformCorrection = syn.desugarAndTagSyn_OffsetCorrection
                        }
      = (img, st)

  desugarAndTagImageContentAlgs :: ImageContentAlg (ImageSpan [ImageTransform] *DesugarAndTagStVal -> *(DesugarAndTagSyn s, *DesugarAndTagStVal))
                                                   (*DesugarAndTagStVal -> *(ImageSpan, *DesugarAndTagStVal))
                                                   ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(DesugarAndTagSyn s, *DesugarAndTagStVal))
                                                   ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(DesugarAndTagSyn s, *DesugarAndTagStVal))
                                                   ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(DesugarAndTagSyn s, *DesugarAndTagStVal)) | iTask s
  desugarAndTagImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpan [ImageTransform] *DesugarAndTagStVal -> *(DesugarAndTagSyn s, *DesugarAndTagStVal))
               !(*DesugarAndTagStVal -> *(ImageSpan, *DesugarAndTagStVal)) ![ImageTransform] !(Set ImageTag)
               !*DesugarAndTagStVal
            -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkBasic baIm imSp imTrs imTas st
      #! (imSp, st) = imSp st
      = baIm imSp imTrs st
  desugarAndTagImageAttrAlgs :: ImageAttrAlg s (DesugarAndTagSt (ImageAttr s)) | iTask s
  desugarAndTagImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = ret o ImageStrokeAttr
    , imageAttrStrokeWidthAttrAlg   = ret o ImageStrokeWidthAttr
    , imageAttrXRadiusAttrAlg       = ret o ImageXRadiusAttr
    , imageAttrYRadiusAttrAlg       = ret o ImageYRadiusAttr
    , imageAttrStrokeOpacityAttrAlg = ret o ImageStrokeOpacityAttr
    , imageAttrFillAttrAlg          = ret o ImageFillAttr
    , imageAttrFillOpacityAttrAlg   = ret o ImageFillOpacityAttr
    , imageAttrOnClickAttrAlg       = ret o ImageOnClickAttr
    , imageAttrOnMouseDownAttrAlg   = ret o ImageOnMouseDownAttr
    , imageAttrOnMouseUpAttrAlg     = ret o ImageOnMouseUpAttr
    , imageAttrOnMouseOverAttrAlg   = ret o ImageOnMouseOverAttr
    , imageAttrOnMouseMoveAttrAlg   = ret o ImageOnMouseMoveAttr
    , imageAttrOnMouseOutAttrAlg    = ret o ImageOnMouseOutAttr
    , imageAttrDraggableAttrAlg     = ret o ImageDraggableAttr
    , imageAttrDashAttrAlg          = ret o ImageDashAttr
    }
  desugarAndTagImageTransformAlgs :: ImageTransformAlg (DesugarAndTagSt Span) (DesugarAndTagSt ImageTransform)
  desugarAndTagImageTransformAlgs =
    { imageTransformRotateImageAlg = ret o RotateImage
    , imageTransformSkewXImageAlg  = ret o SkewXImage
    , imageTransformSkewYImageAlg  = ret o SkewYImage
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitDim FitXImage
    , imageTransformFitYImageAlg   = mkFitDim FitYImage
    , imageTransformFlipXImageAlg  = ret FlipXImage
    , imageTransformFlipYImageAlg  = ret FlipYImage
    }
    where
    mkFitImage :: !(DesugarAndTagSt Span) !(DesugarAndTagSt Span) !*DesugarAndTagStVal
               -> *(!ImageTransform, !*DesugarAndTagStVal)
    mkFitImage sp1 sp2 st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      = (FitImage sp1 sp2, st)
    mkFitDim :: !(Span -> ImageTransform) !(DesugarAndTagSt Span) !*DesugarAndTagStVal
             -> *(!ImageTransform, !*DesugarAndTagStVal)
    mkFitDim ctr sp st
      #! (sp, st) = sp st
      = (ctr sp, st)
  desugarAndTagImageSpanAlgs :: ImageSpanAlg (*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal)) (*DesugarAndTagStVal -> *(ImageSpan, *DesugarAndTagStVal))
  desugarAndTagImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: !(DesugarAndTagSt Span) !(DesugarAndTagSt Span) !*DesugarAndTagStVal
           -> *(!ImageSpan, !*DesugarAndTagStVal)
    mkSpan xspan yspan st
      #! (xspan, st) = xspan st
      #! (yspan, st) = yspan st
      = ((xspan, yspan), st)
  desugarAndTagBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s)) | iTask s
  desugarAndTagBasicImageAlgs =
    { basicImageEmptyImageAlg   = mkEmptyImage
    , basicImageTextImageAlg    = mkTextImage
    , basicImageCircleImageAlg  = mkCircleImage
    , basicImageRectImageAlg    = mkRectImage
    , basicImageEllipseImageAlg = mkEllipseImage
    }
    where
    mkEmptyImage :: !ImageSpan ![ImageTransform] !*DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkEmptyImage imSp imTrs st = mkSpan EmptyImage imSp imTrs st

    mkRectImage :: !ImageSpan ![ImageTransform] !*DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkRectImage imSp imTrs st = mkSpan RectImage imSp imTrs st

    mkTextImage :: !FontDef !String !ImageSpan ![ImageTransform] !*DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkTextImage fd str imSp imTrs st = mkSpan (TextImage fd str) imSp imTrs st

    mkCircleImage :: !ImageSpan ![ImageTransform] !*DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkCircleImage imSp imTrs st = mkSpan CircleImage imSp imTrs st

    mkEllipseImage :: !ImageSpan ![ImageTransform] !*DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkEllipseImage imSp imTrs st = mkSpan EllipseImage imSp imTrs st

    mkSpan :: !BasicImage !ImageSpan ![ImageTransform]
              !*DesugarAndTagStVal
           -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkSpan ctor imSp imTrs st
      #! (imSp`, imOff) = applyTransforms imTrs imSp
      = ({ desugarAndTagSyn_ImageContent        = Basic ctor imSp
         , desugarAndTagSyn_TotalSpan_PreTrans  = imSp
         , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
         , desugarAndTagSyn_OffsetCorrection    = imOff }, st)

  desugarAndTagLineImageAlgs :: LineImageAlg (DesugarAndTagSt ImageSpan)
                                        (DesugarAndTagSt (Markers s))
                                        (DesugarAndTagSt LineContent)
                                        ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s)) | iTask s
  desugarAndTagLineImageAlgs =
    { lineImageLineImageAlg = mkLine
    }
    where
    mkLine :: !(DesugarAndTagSt ImageSpan) !(Maybe (DesugarAndTagSt (Markers s))) !(DesugarAndTagSt LineContent)
              ![ImageTransform] !(Set ImageTag)
              !*DesugarAndTagStVal
           -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkLine imSp mmarkers liCo imTrs imTas st
      #! (imSp, st)     = imSp st
      #! (mmarkers, st) = evalMaybe mmarkers st
      #! (liCo, st)     = liCo st
      #! (imSp`, imOff) = applyTransforms imTrs imSp
      = ({ desugarAndTagSyn_ImageContent        = Line { LineImage
                                                  | lineSpan    = imSp
                                                  , markers     = mmarkers
                                                  , lineContent = liCo }
         , desugarAndTagSyn_TotalSpan_PreTrans  = imSp
         , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
         , desugarAndTagSyn_OffsetCorrection    = imOff }, st)
  desugarAndTagMarkersAlgs :: MarkersAlg (DesugarAndTagSt (Image s)) (DesugarAndTagSt (Markers s)) | iTask s
  desugarAndTagMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal)))
                 !(Maybe (*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal)))
                 !(Maybe (*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal)))
                 !*DesugarAndTagStVal
              -> *(!Markers s, !*DesugarAndTagStVal) | iTask s
    mkMarkers mStart mMid mEnd st
      #! (mStart, st) = evalMaybe mStart st
      #! (mMid, st)   = evalMaybe mMid st
      #! (mEnd, st)   = evalMaybe mEnd st
      = ({ markerStart = mStart
         , markerMid   = mMid
         , markerEnd   = mEnd }, st)
  desugarAndTagLineContentAlgs :: LineContentAlg (*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)) (*DesugarAndTagStVal -> *(!LineContent, !*DesugarAndTagStVal))
  desugarAndTagLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkSimpleLine
    , lineContentPolygonImageAlg    = mkPolygon
    , lineContentPolylineImageAlg   = mkPolyline
    }
    where
    mkSimpleLine :: !Slash !*DesugarAndTagStVal -> *(!LineContent, !*DesugarAndTagStVal)
    mkSimpleLine sl st = (SimpleLineImage sl, st)

    mkPolygon :: ![(!*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal), !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal))] !*DesugarAndTagStVal -> *(!LineContent, !*DesugarAndTagStVal)
    mkPolygon coords st
      #! (coords, st) = evalOffsets coords st
      = (PolygonImage coords, st)

    mkPolyline :: ![(!*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal), !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal))] !*DesugarAndTagStVal -> *(!LineContent, !*DesugarAndTagStVal)
    mkPolyline coords st
      #! (coords, st) = evalOffsets coords st
      = (PolylineImage coords, st)
  desugarAndTagCompositeImageAlgs :: CompositeImageAlg (DesugarAndTagSt Span)
                                                  (DesugarAndTagSt (Image s))
                                                  ((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !ImageSpan))
                                                  ([ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal)) | iTask s
  desugarAndTagCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: !(Maybe (DesugarAndTagSt (Image s)))
                        !((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !ImageSpan))
                        ![ImageTransform] !(Set ImageTag)
                        !*DesugarAndTagStVal
                     -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal) | iTask s
    mkCompositeImage host compose imTrs imTas st
      #! (host, st)    = evalMaybe host st
      #! ((compose, composeSpan), st) = compose host imTrs imTas st
      #! (host, span)  = case host of
                          Just hostImg
                             -> (Just hostImg, hostImg.totalSpanPostTrans)
                          _  -> (Nothing, composeSpan)
      #! (span`, corr) = applyTransforms imTrs span
      = ({ desugarAndTagSyn_ImageContent        = Composite { CompositeImage
                                                            | host    = host
                                                            , compose = compose
                                                            }
         , desugarAndTagSyn_TotalSpan_PreTrans  = span
         , desugarAndTagSyn_TotalSpan_PostTrans = span`
         , desugarAndTagSyn_OffsetCorrection    = corr }, st)
  desugarAndTagComposeAlgs :: ComposeAlg (DesugarAndTagSt Span) (DesugarAndTagSt (Image s))
                                    ((Maybe (Image s)) [ImageTransform] (Set ImageTag) *DesugarAndTagStVal -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal)) | iTask s
  desugarAndTagComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![[(DesugarAndTagSt Span, DesugarAndTagSt Span)]]
              ![[ImageAlign]] ![[DesugarAndTagSt (Image s)]]
              !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
              !*DesugarAndTagStVal
           -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal) | iTask s
    mkGrid (numcols, numrows) offsetss iass imgss mbhost imTrs imTas st
      #! (offsetss, st) = strictTRMapSt evalOffsets offsetss st
      #! (imgss, st)    = strictTRMapSt sequence imgss st
      #! (tag, st)      = nextNo st
      #! sysTags        = ImageTagSystem tag
      #! colIndices     = [0 .. numcols - 1]
      #! rowIndices     = [0 .. numrows - 1]
      #! gridSpan       = maybe ( strictFoldl (\acc n -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) colIndices
                                , strictFoldl (\acc n -> LookupSpan (RowYSpan sysTags n)    + acc) (px 0.0) rowIndices
                                )
                                (\x -> x.totalSpanPostTrans) mbhost
      #! spanss         = strictTRMap (strictTRMap (\x -> x.totalSpanPostTrans)) imgss
      #! st             = cacheGridSpans tag ('DS'.insert sysTags imTas)
                                         (strictTRMap (maxSpan o strictTRMap fst) (transpose spanss))
                                         (strictTRMap (maxSpan o strictTRMap snd) spanss) st
      #! offsets`       = calculateGridOffsets (strictTRMap (\n -> LookupSpan (ColumnXSpan sysTags n)) colIndices)
                                               (strictTRMap (\n -> LookupSpan (RowYSpan sysTags n))    rowIndices) iass imgss offsetss
      #! offsets`       = reverseTR (flattenTR offsets`)
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

    mkCollage :: ![(DesugarAndTagSt Span, DesugarAndTagSt Span)]
                 ![DesugarAndTagSt (Image s)] !(Maybe (Image s))
                 ![ImageTransform] !(Set ImageTag)
                 !*DesugarAndTagStVal
              -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal) | iTask s
    mkCollage offsets imgs mbhost imTrs imTas st
      #! (offsets, st) = evalOffsets offsets st
      #! (imgs, st)    = sequence imgs st
      = (( AsCollage offsets imgs
         , maybe (calculateComposedSpan (strictTRMap (\x -> x.totalSpanPostTrans) imgs) offsets) (\x -> x.totalSpanPostTrans) mbhost), st)

    mkOverlay :: ![(DesugarAndTagSt Span, DesugarAndTagSt Span)]
                 ![ImageAlign] ![DesugarAndTagSt (Image s)]
                 !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
                 !*DesugarAndTagStVal
              -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal) | iTask s
    mkOverlay offsets ias imgs mbhost imTrs imTas st
      #! (offsets, st)  = evalOffsets offsets st
      #! (imgs, st)     = sequence imgs st
      #! spans          = strictTRMap (\x -> x.totalSpanPostTrans) imgs
      #! (  maxXSpan
          , maxYSpan)   = maybe (maxSpan (strictTRMap fst spans), maxSpan (strictTRMap snd spans))
                                (\x -> x.totalSpanPreTrans) mbhost
      #! alignOffsets   = strictTRZipWith (calcAlignOffset maxXSpan maxYSpan) spans ias
      #! placingOffsets = strictTRZipWith3 addOffset alignOffsets offsets imgs
      = ( ( AsCollage placingOffsets imgs
          , maybe (calculateComposedSpan spans placingOffsets) (\x -> x.totalSpanPostTrans) mbhost)
        , st)
      where
      addOffset :: !(!Span, !Span) !(!Span, !Span) !(Image s) -> (!Span, !Span) | iTask s
      addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)

  desugarAndTagSpanAlgs :: SpanAlg (*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal)) (*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal))
  desugarAndTagSpanAlgs =
    { spanPxSpanAlg     = mkPxSpan
    , spanLookupSpanAlg = ($)
    , spanAddSpanAlg    = mkBin (+)
    , spanSubSpanAlg    = mkBin (-)
    , spanMulSpanAlg    = mkBin (*)
    , spanDivSpanAlg    = mkBin (/)
    , spanAbsSpanAlg    = mkAbs
    , spanMinSpanAlg    = mkList minSpan
    , spanMaxSpanAlg    = mkList maxSpan
    }
    where
    mkPxSpan :: !Real !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    mkPxSpan r st = (PxSpan r, st)

  desugarAndTagLookupSpanAlgs :: LookupSpanAlg (*DesugarAndTagStVal -> *(Span, *DesugarAndTagStVal))
  desugarAndTagLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg = desugarAndTagMkImageGridColSpan
    , lookupSpanRowYSpanAlg    = desugarAndTagMkImageGridRowSpan
    , lookupSpanImageXSpanAlg  = desugarAndTagMkImageXSpan
    , lookupSpanImageYSpanAlg  = desugarAndTagMkImageYSpan
    , lookupSpanTextXSpanAlg   = desugarAndTagMkTextLU
    }
    where
    desugarAndTagMkTextLU :: !FontDef !String !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    desugarAndTagMkTextLU fd str st
        #! sw = case 'DM'.get fd st.desugarAndTagSpanEnvs.spanEnvFonts of
                  Just fs -> case 'DM'.get str fs of
                               Just sw -> sw
                               _       -> 0.0
                  _       -> 0.0
        = (PxSpan sw, st)

    desugarAndTagMkImageXSpan :: !ImageTag !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    desugarAndTagMkImageXSpan t st = (LookupSpan (ImageXSpan t), st)

    desugarAndTagMkImageYSpan :: !ImageTag !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    desugarAndTagMkImageYSpan t st = (LookupSpan (ImageYSpan t), st)

    desugarAndTagMkImageGridColSpan :: !ImageTag !Int !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    desugarAndTagMkImageGridColSpan t n st = (LookupSpan (ColumnXSpan t n), st)

    desugarAndTagMkImageGridRowSpan :: !ImageTag !Int !*DesugarAndTagStVal -> *(!Span, !*DesugarAndTagStVal)
    desugarAndTagMkImageGridRowSpan t n st = (LookupSpan (RowYSpan t n), st)

import StdDebug

:: *SpanEnvs =
  { spanEnvImageTagPreTrans   :: !Map ImageTag Int
  , spanEnvImageTagPostTrans  :: !Map ImageTag Int
  , spanEnvImageSpanPostTrans :: !IntMap ImageSpan
  //, spanEnvImageSpanPostTrans` :: !*{!ImageSpan}
  , spanEnvGridTag            :: !Map ImageTag Int
  , spanEnvGridSpan           :: !IntMap (!{!Span}, !{!Span})
  //, spanEnvGridSpan`          :: !*{!(!*{!Span}, !*{!Span})}
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
      #! fixSpansDidChange  = st.fixSpansDidChange
      #! (h`, st`)          = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs}
      | otherwise
        = {FixSpansSt | st` & fixSpansDidChange = fixSpansDidChange}
    f k (w, h=:(PxSpan _)) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs}
      | otherwise
        = {FixSpansSt | st` & fixSpansDidChange = st.fixSpansDidChange}
    f k (w, h) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      #! (h`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h st`
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs}
      | otherwise
        = {FixSpansSt | st` & fixSpansDidChange = st.fixSpansDidChange}
  fixGridSpans :: !*FixSpansSt -> *FixSpansSt
  fixGridSpans st=:{fixSpansSpanEnvs}
    #! fixSpansSpanEnvs = st.fixSpansSpanEnvs
    #! spanEnvGridSpan  = fixSpansSpanEnvs.spanEnvGridSpan
    #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
    #! st               = {st & fixSpansSpanEnvs = fixSpansSpanEnvs}
    = 'DIS'.foldrWithKey f st spanEnvGridSpan
    where
    f :: !Int !(!{!Span}, !{!Span}) !*FixSpansSt -> *FixSpansSt
    f k (xsps, ysps) st=:{fixSpansDidChange = origDidChange}
      #! (xsps`, _, st) = foldrArr g ({x \\ x <-: xsps}, size xsps - 1, {st & fixSpansDidChange = False}) xsps
      #! (ysps`, _, st) = foldrArr g ({y \\ y <-: ysps}, size ysps - 1, st) ysps
      | st.fixSpansDidChange
        #! fixSpansSpanEnvs = st.fixSpansSpanEnvs
        #! spanEnvGridSpan  = 'DIS'.put k (xsps`, ysps`) fixSpansSpanEnvs.spanEnvGridSpan
        #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
        = {st & fixSpansSpanEnvs  = fixSpansSpanEnvs}
      | otherwise = {st & fixSpansDidChange = origDidChange}
    g :: !Span !*(!*{!Span}, !Int, !*FixSpansSt) -> *(!*{!Span}, !Int, !*FixSpansSt)
    g v (acc, n, st)
      #! (v, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs v {st & fixSpansDidChange = False}
      = if st`.fixSpansDidChange
          ({acc & [n] = v}, n - 1, st`)
          (acc, n - 1, {st` & fixSpansDidChange = st.fixSpansDidChange} )

mapArrSt :: !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
mapArrSt f arr st
  #! (sz, arr) = usize arr
  = mapArrSt` sz 0 f arr st
  where
  mapArrSt` :: !Int !Int !(.a *st -> *(!.a, !*st)) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
  mapArrSt` sz idx f arr st
    | idx == sz = (arr, st)
    | otherwise
        #! (e, arr) = arr![idx]
        #! (e, st)  = f e st
        #! arr      = {arr & [idx] = e}
        = mapArrSt` sz (idx + 1) f arr st

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
foldrArr f b arr
  #! (arrSz, arr) = usize arr
  = foldrArr` arrSz 0 f b arr
  where
  foldrArr` :: !Int !Int !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
  foldrArr` arrSz idx f b arr
    | idx == arrSz = b
    | otherwise
        #! (e, arr) = arr![idx]
        = f e (foldrArr` arrSz (idx + 1) f b arr)

mkBin` :: !(a b -> Span) !(*FixSpansSt -> *(!a, !*FixSpansSt))
          !(*FixSpansSt -> *(!b, !*FixSpansSt)) !*FixSpansSt
       -> *(!Span, !*FixSpansSt)
mkBin` op x y st
  #! (x, st) = x st
  #! (y, st) = y st
  = case op x y of
      sp=:(PxSpan _) -> (sp, {st & fixSpansDidChange = True})
      sp             -> (sp, st)

mkAbs` :: !(*FixSpansSt -> *(!Span, !*FixSpansSt)) !*FixSpansSt -> *(!Span, !*FixSpansSt)
mkAbs` x st
  #! (x, st) = x st
  = case abs x of
      sp=:(PxSpan _) -> (sp, {st & fixSpansDidChange = True})
      sp             -> (sp, st)

mkList` :: !([a] -> Span) ![*FixSpansSt -> *(!a, !*FixSpansSt)] !*FixSpansSt
        -> *(!Span, !*FixSpansSt)
mkList` f xs st
  #! (xs, st) = sequence xs st
  = case f xs of
      sp=:(PxSpan _) -> (sp, {st & fixSpansDidChange = True})
      sp             -> (sp, st)

fixSpansSpanAlgs :: SpanAlg (*FixSpansSt -> *(Span, *FixSpansSt)) (*FixSpansSt -> *(Span, *FixSpansSt))
fixSpansSpanAlgs =
  { spanPxSpanAlg     = mkPxSpan
  , spanLookupSpanAlg = ($)
  , spanAddSpanAlg    = mkBin` (+)
  , spanSubSpanAlg    = mkBin` (-)
  , spanMulSpanAlg    = mkBin` (*)
  , spanDivSpanAlg    = mkBin` (/)
  , spanAbsSpanAlg    = mkAbs`
  , spanMinSpanAlg    = mkList` minSpan
  , spanMaxSpanAlg    = mkList` maxSpan
  }
  where
  mkPxSpan :: !Real !*FixSpansSt -> *(!Span, !*FixSpansSt)
  mkPxSpan r st = (PxSpan r, {st & fixSpansDidChange = False})

fixSpansLookupSpanAlgs :: LookupSpanAlg (*FixSpansSt -> *(Span, *FixSpansSt))
fixSpansLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg = fixSpansMkImageGridColSpan
  , lookupSpanRowYSpanAlg    = fixSpansMkImageGridRowSpan
  , lookupSpanImageXSpanAlg  = fixSpansMkImageXSpan
  , lookupSpanImageYSpanAlg  = fixSpansMkImageYSpan
  , lookupSpanTextXSpanAlg   = fixSpansMkTextLU
  }
  where
  fixSpansMkTextLU :: !FontDef !String !*FixSpansSt -> *(!Span, !*FixSpansSt)
  fixSpansMkTextLU fd str st = (PxSpan 0.0, {st & fixSpansDidChange = True})

  fixSpansMkImageXSpan :: !ImageTag !*FixSpansSt -> *(!Span, !*FixSpansSt)
  fixSpansMkImageXSpan t st
    #! ses                      = st.fixSpansSpanEnvs
    #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
    #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
    = case 'DM'.get t spanEnvImageTagPostTrans of
        Just n
          #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
          #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
          = case 'DIS'.get n spanEnvImageSpanPostTrans of
              Just (xsp=:(PxSpan _), _)
                = (xsp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
              Just _
                = (LookupSpan (ImageXSpan t), {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
              _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
        _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})

  fixSpansMkImageYSpan :: !ImageTag !*FixSpansSt -> *(!Span, !*FixSpansSt)
  fixSpansMkImageYSpan t st
    #! ses                      = st.fixSpansSpanEnvs
    #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
    #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
    = case 'DM'.get t spanEnvImageTagPostTrans of
        Just n
          #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
          #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
          = case 'DIS'.get n spanEnvImageSpanPostTrans of
              Just (_, ysp=:(PxSpan _))
                = (ysp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
              Just _
                = (LookupSpan (ImageYSpan t), {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
              _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
        _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})

  fixSpansMkImageGridColSpan :: !ImageTag !Int !*FixSpansSt -> *(!Span, !*FixSpansSt)
  fixSpansMkImageGridColSpan t n st
    #! ses            = st.fixSpansSpanEnvs
    #! spanEnvGridTag = ses.spanEnvGridTag
    #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
    = case 'DM'.get t spanEnvGridTag of
        Just cacheIdx
          #! spanEnvGridSpan = ses.spanEnvGridSpan
          #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
          = case 'DIS'.get cacheIdx spanEnvGridSpan of
              Just (xs, _)
                = case xs.[n] of
                    xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                    _               -> (LookupSpan (ColumnXSpan t n), {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
              _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
        _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})

  fixSpansMkImageGridRowSpan :: !ImageTag !Int !*FixSpansSt -> *(!Span, !*FixSpansSt)
  fixSpansMkImageGridRowSpan t n st
    #! ses            = st.fixSpansSpanEnvs
    #! spanEnvGridTag = ses.spanEnvGridTag
    #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
    = case 'DM'.get t spanEnvGridTag of
        Just cacheIdx
          #! spanEnvGridSpan = ses.spanEnvGridSpan
          #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
          = case 'DIS'.get cacheIdx spanEnvGridSpan of
              Just (_, xs)
                = case xs.[n] of
                    xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                    _               -> (LookupSpan (RowYSpan t n), {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
              _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
        _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})

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

genSVG :: !(Image s) !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
genSVG img st = imageCata genSVGAllAlgs img st
  where
  genSVGAllAlgs =
    { imageAlgs          = genSVGImageAlgs
    , imageContentAlgs   = genSVGImageContentAlgs
    , imageAttrAlgs      = genSVGImageAttrAlgs
    , imageTransformAlgs = genSVGImageTransformAlgs
    , imageSpanAlgs      = genSVGImageSpanAlgs
    , basicImageAlgs     = genSVGBasicImageAlgs
    , lineImageAlgs      = genSVGLineImageAlgs
    , markersAlgs        = genSVGMarkersAlgs
    , lineContentAlgs    = genSVGLineContentAlgs
    , compositeImageAlgs = genSVGCompositeImageAlgs
    , composeAlgs        = genSVGComposeAlgs
    , spanAlgs           = genSVGSpanAlgs
    , lookupSpanAlgs     = genSVGLookupSpanAlgs
    }
  genSVGImageAlgs :: ImageAlg (Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> (!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) Int *(GenSVGStVal s) -> *(!GenSVGSyn s, *GenSVGStVal s))
                              (Int *(GenSVGStVal s) -> *(!(!Maybe SVGAttr, Map String (ImageAttr s), Map String (ImageAttr s)), !*(GenSVGStVal s)))
                              (ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*(GenSVGStVal s)))
                              (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                              (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)) | iTask s
  genSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage :: !(Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> (!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) Int *(GenSVGStVal s) -> *(!GenSVGSyn s, *GenSVGStVal s))
               !(Maybe (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)))
               !([Int *(GenSVGStVal s) -> *(!(!Maybe SVGAttr, Map String (ImageAttr s), Map String (ImageAttr s)), !*(GenSVGStVal s))])
               ![ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*(GenSVGStVal s))]
               !(Set ImageTag)
               !Int
               !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), *(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
               !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), *(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
               !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), *(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
               !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkImage imCo mask imAts imTrs imTas uniqId (txsp, tysp) (txsp`, tysp`) _ st
      #! (imAts, st)  = strictTRMapSt (\f -> f uniqId) imAts st
      #! (txsp, st)   = txsp st
      #! (tysp, st)   = tysp st
      #! (txsp`, st)  = txsp` st
      #! (tysp`, st)  = tysp` st
      #! (maskId, st) = imageMaskId st
      #! imAts`       = strictTRMap (\(x, _, _) -> x) imAts
      #! interactive  = strictFoldl (||) False (strictTRMap (\(_, m1, m2) -> not ('DM'.null m1) || not ('DM'.null m2)) imAts)
      #! (syn, st)    = imCo interactive (txsp, tysp) (maybe imAts` (const [Just (MaskAttr (mkUrl maskId)) : imAts`]) mask) imTrs imTas uniqId st
      #! (mask, st)   = evalMaybe mask st
      = ({ genSVGSyn_svgElts       = mkElt maskId mask syn
         , genSVGSyn_imageSpanReal = (txsp`, tysp`)
         , genSVGSyn_events        = 'DM'.unions [syn.genSVGSyn_events : strictTRMap (\(_, x, _) -> x) imAts]
         , genSVGSyn_draggable     = 'DM'.unions [syn.genSVGSyn_draggable : strictTRMap (\(_, _, x) -> x) imAts]
         , genSVGSyn_idMap         = 'DM'.put (mkUniqId editletId uniqId) imTas syn.genSVGSyn_idMap
         }, st)

    imageMaskId :: !*a -> *(!String, !*a) | nextNo a
    imageMaskId clval
      #! (uid, clval) = nextNo clval
      #! maskId       = mkMaskId editletId uid
      = (maskId, clval)

    mkElt :: !String !(Maybe (GenSVGSyn .a)) !(GenSVGSyn .b) -> [SVGElt]
    mkElt _      Nothing     syn = syn.genSVGSyn_svgElts
    mkElt maskId (Just mask) syn
      = [ DefsElt [] [] [MaskElt [IdAttr maskId] [] mask.genSVGSyn_svgElts]
        : syn.genSVGSyn_svgElts]

  genSVGImageContentAlgs :: ImageContentAlg (Int Bool ImageSpanReal [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] *(GenSVGStVal s) -> *(!(!GenSVGSyn s, !Bool) , !*GenSVGStVal s))
                                           (*(GenSVGStVal s) -> *(!ImageSpanReal, !*GenSVGStVal s))
                                           (Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*(GenSVGStVal s))] (Set ImageTag) Int *(GenSVGStVal s) -> *(GenSVGSyn s, *GenSVGStVal s))
                                           (Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*(GenSVGStVal s))] (Set ImageTag) Int *(GenSVGStVal s) -> *(GenSVGSyn s, *GenSVGStVal s))
                                           (Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*(GenSVGStVal s))] (Set ImageTag) Int *(GenSVGStVal s) -> *(GenSVGSyn s, *GenSVGStVal s)) | iTask s
  genSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(Int Bool ImageSpanReal [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] *(GenSVGStVal s) -> *(!(!GenSVGSyn s, !Bool) , !*GenSVGStVal s))
               !(*(GenSVGStVal s) -> *(!ImageSpanReal, !*GenSVGStVal s))
               !Bool
               ImageSpanReal // Not used
               ![Maybe SVGAttr]
               ![ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)]
               (Set ImageTag) // Not used
               !Int
               !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkBasic baIm imSp interactive _ imAts imTrs _ uniqId st
      #! (imSp, st)        = imSp st
      #! ((_, isText), st) = baIm uniqId interactive imSp [] [] st
      #! (imTrs, st)       = sequence (strictTRMap (\f -> f imSp isText) imTrs) st
      #! ((syn, _), st)    = baIm uniqId interactive imSp imAts imTrs st
      = (syn, st)
  genSVGImageAttrAlgs :: ImageAttrAlg s (Int *(GenSVGStVal s) -> *(!(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*(GenSVGStVal s))) | iTask s
  genSVGImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr _ s -> ((Just (StrokeAttr (PaintColor attr.stroke Nothing)), 'DM'.newMap, 'DM'.newMap), s)
    , imageAttrStrokeWidthAttrAlg   = mkStrokeWidth
    , imageAttrXRadiusAttrAlg       = mkXRadius
    , imageAttrYRadiusAttrAlg       = mkYRadius
    , imageAttrStrokeOpacityAttrAlg = \attr _ s -> ((Just (StrokeOpacityAttr (toString attr.opacity)), 'DM'.newMap, 'DM'.newMap), s)
    , imageAttrFillAttrAlg          = \attr _ s -> ((Just (FillAttr (PaintColor attr.fill Nothing)), 'DM'.newMap, 'DM'.newMap), s)
    , imageAttrFillOpacityAttrAlg   = \attr _ s -> ((Just (FillOpacityAttr (FillOpacity (toString attr.opacity))), 'DM'.newMap, 'DM'.newMap), s)
    , imageAttrOnClickAttrAlg       = mkOnClick
    , imageAttrOnMouseDownAttrAlg   = mkOnMouseDown
    , imageAttrOnMouseUpAttrAlg     = mkOnMouseUp
    , imageAttrOnMouseOverAttrAlg   = mkOnMouseOver
    , imageAttrOnMouseMoveAttrAlg   = mkOnMouseMove
    , imageAttrOnMouseOutAttrAlg    = mkOnMouseOut
    , imageAttrDraggableAttrAlg     = mkDraggable
    , imageAttrDashAttrAlg          = \attr _ s -> ((Just (StrokeDashArrayAttr (DashArray (strictTRMap toString attr.dash))), 'DM'.newMap, 'DM'.newMap), s)
    }
    where
    mkStrokeWidth :: !(StrokeWidthAttr s) Int !*(GenSVGStVal s)
                  -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkStrokeWidth {strokewidth} _ st
      #! (w, st) = evalSpan strokewidth st
      = ((Just (StrokeWidthAttr (StrokeWidthLength (toString w, PX))), 'DM'.newMap, 'DM'.newMap), st)

    mkXRadius :: !(XRadiusAttr s) Int !*(GenSVGStVal s)
              -> *((!Maybe SVGAttr, Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkXRadius attr _ st
      #! (r, st) = evalSpan attr.xradius st
      = ((Just (RxAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)

    mkYRadius :: !(YRadiusAttr s) Int !*(GenSVGStVal s)
              -> *((!Maybe SVGAttr, Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkYRadius attr _ st
      #! (r, st) = evalSpan attr.yradius st
      = ((Just (RyAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)

    mkOnClick :: !(OnClickAttr s) !Int !*(GenSVGStVal s)
              -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnClick attr uniqId clval = mkEvent mkOnClickId uniqId (ImageOnClickAttr attr) clval

    mkOnMouseDown :: !(OnMouseDownAttr s) !Int !*(GenSVGStVal s)
                  -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnMouseDown attr uniqId clval = mkEvent mkOnMouseDownId uniqId (ImageOnMouseDownAttr attr) clval

    mkOnMouseUp :: !(OnMouseUpAttr s) !Int !*(GenSVGStVal s)
                -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnMouseUp attr uniqId clval = mkEvent mkOnMouseUpId uniqId (ImageOnMouseUpAttr attr) clval

    mkOnMouseOver :: !(OnMouseOverAttr s) !Int !*(GenSVGStVal s)
                  -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnMouseOver attr uniqId clval = mkEvent mkOnMouseOverId uniqId (ImageOnMouseOverAttr attr) clval

    mkOnMouseMove :: !(OnMouseMoveAttr s) !Int !*(GenSVGStVal s)
                  -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnMouseMove attr uniqId clval = mkEvent mkOnMouseMoveId uniqId (ImageOnMouseMoveAttr attr) clval

    mkOnMouseOut :: !(OnMouseOutAttr s) !Int !*(GenSVGStVal s)
                 -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkOnMouseOut attr uniqId clval = mkEvent mkOnMouseOutId uniqId (ImageOnMouseOutAttr attr) clval

    mkDraggable :: !(DraggableAttr s) !Int !*(GenSVGStVal s)
                -> *((!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*GenSVGStVal s) | iTask s
    mkDraggable {draggable = Nothing} _ clval = ((Nothing, 'DM'.newMap, 'DM'.newMap), clval)
    mkDraggable attr uniqId clval
      #! ocId = mkUniqId editletId uniqId
      = ((Nothing, 'DM'.newMap, 'DM'.singleton ocId (ImageDraggableAttr attr)), clval)

    mkEvent mkIdFun uniqId attr clval
      #! ocId = mkUniqId editletId uniqId
      = ((Nothing, 'DM'.singleton ocId attr, 'DM'.newMap), clval)

  genSVGImageTransformAlgs :: ImageTransformAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) ((!Real, !Real) Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)) | iTask s
  genSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = mkRotateTransform
    , imageTransformSkewXImageAlg  = mkSkewX
    , imageTransformSkewYImageAlg  = mkSkewY
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitXImage
    , imageTransformFitYImageAlg   = mkFitYImage
    , imageTransformFlipXImageAlg  = mkFlipXImage
    , imageTransformFlipYImageAlg  = mkFlipYImage
    }
    where
    mkRotateTransform :: !Angle !(!Real, !Real) !Bool !*(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkRotateTransform imAn (xsp, ysp) isText s
      // FIXME: We currently devide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
      #! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
      = (([RotateTransform (toString (to2dec (toDeg imAn))) (Just (toString (to2dec (xsp / 2.0)), toString (to2dec yoff)))], RotateImage imAn), s)

    mkSkewX :: !Angle
               !(!Real, Real /* Not used */)
               Bool // Not used
               !*(GenSVGStVal s)
            -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkSkewX imAn (_, ysp) isText s = (([SkewXTransform (toString (toDeg imAn))], SkewXImage imAn), s)

    mkSkewY :: !Angle
               !(Real /* Not used */, !Real)
               !Bool
               !*(GenSVGStVal s)
            -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkSkewY imAn (_, ysp) isText s
      = (([SkewYTransform (toString (toDeg imAn))], SkewYImage imAn), s)

    mkFitImage :: !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                  !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                  !(!Real, !Real)
                  !Bool
                  !*(GenSVGStVal s)
               -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkFitImage sp1 sp2 (xsp, ysp) isText st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      #! factorx  = to2dec (sp1 / xsp)
      #! factory  = to2dec (sp2 / ysp)
      #! attrs    = [ScaleTransform (toString factorx) (toString factory)]
      #! attrs    = case isText of
                      True
                        = [TranslateTransform "0" (toString ysp) : attrs]
                      _ = attrs
      = ((attrs, FitImage (px sp1) (px sp2)), st)

    mkFitXImage :: !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                   !(!Real, !Real)
                   !Bool
                   !*(GenSVGStVal s)
                -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkFitXImage sp (xsp, ysp) isText st
      #! (sp, st) = sp st
      #! factor   = to2dec (sp / xsp)
      #! scale    = if (xsp > 0.0) (toString factor) "1.0"
      #! attrs    = [ScaleTransform scale scale]
      #! attrs    = case isText of
                      True
                        #! yoff = to2dec (ysp * 0.7 * factor)
                        = [TranslateTransform "0" (toString yoff) : attrs]
                      _ = attrs
      = ((attrs, FitXImage (px sp)), st)

    mkFitYImage :: !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                   !(Real /* Not used */, !Real)
                   !Bool
                   !*(GenSVGStVal s)
                -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkFitYImage sp (_, ysp) isText st
      #! (sp, st) = sp st
      #! factor   = to2dec (sp / ysp)
      #! scale    = if (ysp > 0.0) (toString factor) "1.0"
      #! attrs    = [ScaleTransform scale scale]
      #! attrs    = case isText of
                      True
                        = [TranslateTransform "0" (toString ysp) : attrs]
                      _ = attrs
      = ((attrs, FitYImage (px sp)), st)

    mkFlipXImage :: !(!Real, Real /* Not used */) 
                    Bool // Not used
                    !*(GenSVGStVal s)
                 -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkFlipXImage (xsp, _) _ st
      = (([TranslateTransform (toString xsp) "0", ScaleTransform "-1" "1"], FlipXImage), st)

    mkFlipYImage :: !(Real /* Not used */, !Real)
                    Bool // Not used
                    !*(GenSVGStVal s)
                 -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)
    mkFlipYImage (_, ysp) isText st
      #! ysp = if isText ((~ ysp) * 0.7) ysp
      = (([TranslateTransform "0" (toString ysp), ScaleTransform "1" "-1"], FlipYImage), st)
  genSVGImageSpanAlgs :: ImageSpanAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) (*(GenSVGStVal s) -> *(!ImageSpanReal, !*GenSVGStVal s)) | iTask s
  genSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan :: !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) !(*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) !*(GenSVGStVal s) -> *(!ImageSpanReal, !*GenSVGStVal s) | iTask s
    mkImageSpan sp1 sp2 st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      = ((sp1, sp2), st)
  genSVGBasicImageAlgs :: BasicImageAlg (Int Bool ImageSpanReal [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] *(GenSVGStVal s) -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s)) | iTask s
  genSVGBasicImageAlgs =
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage :: !Int !Bool !ImageSpanReal ![Maybe SVGAttr]
                    ![(![SVGTransform], !ImageTransform)]
                    !*(GenSVGStVal s)
                 -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s) | iTask s
    mkEmptyImage uniqId interactive imSp imAts imTrs st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup hattrs (mkAttrs imAts imTrs) [] }, False), st)

    mkTextImage :: !FontDef !String !Int !Bool !ImageSpanReal
                   ![Maybe SVGAttr]
                   ![(![SVGTransform], !ImageTransform)]
                   !*(GenSVGStVal s)
                -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s) | iTask s
    mkTextImage fd str uniqId interactive imSp imAts imTrs st
    // TODO Currently we manually translate text by fontysize pixels to compensate for the "auto" baseline. The result look OK, but a bit off compare to the old approach where we forced the origin to be the top-left corner (which didn't work with zooming)
    // We need to offset by the font's descent height, but that's not easy to calculate currently (there are no JS APIs for that yet). Current heuristic: we assume that the ex-height is half of the font height. We assume that the descent height is half of the ex-height. Therefore, we multiply by 0.75
      #! hattrs = [XmlspaceAttr "preserve"]
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = [TextElt hattrs (addAttr (TransformAttr [TranslateTransform (toString 0.0) (toString (fd.fontysize * 0.75))]) (mkAttrs imAts imTrs ++ fontAttrs fd.fontysize)) str] }
         , True), st)
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
    mkRectImage :: !Int !Bool !ImageSpanReal ![Maybe SVGAttr]
                   ![(![SVGTransform], !ImageTransform)]
                   !*(GenSVGStVal s)
                -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s) | iTask s
    mkRectImage uniqId interactive imSp imAts imTrs st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = [RectElt hattrs (mkAttrs imAts imTrs)] }, False), st)

    mkCircleImage :: !Int !Bool !ImageSpanReal ![Maybe SVGAttr]
                     ![(![SVGTransform], !ImageTransform)]
                     !*(GenSVGStVal s)
                   -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s) | iTask s
    mkCircleImage uniqId interactive imSp=:(imXSp`, _) imAts imTrs st
      #! r = imXSp` / 2.0
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = [CircleElt (if interactive [IdAttr (mkUniqId editletId uniqId)] [])
                                                [ RAttr (toString (to2dec r), PX), CxAttr (toString (to2dec r), PX)
                                                , CyAttr (toString (to2dec r), PX) : (mkAttrs imAts imTrs) ]] }, False), st)

    mkEllipseImage :: !Int !Bool !ImageSpanReal ![Maybe SVGAttr]
                      ![(![SVGTransform], !ImageTransform)]
                      !*(GenSVGStVal s)
                   -> *(!(!GenSVGSyn s, !Bool), !*GenSVGStVal s) | iTask s
    mkEllipseImage uniqId interactive imSp=:(imXSp, imYSp) imAts imTrs st
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = [EllipseElt (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs imAts imTrs ++
                                                [ RxAttr (toString (to2dec (imXSp / 2.0)), PX), RyAttr (toString (to2dec (imYSp / 2.0)), PX)
                                                , CxAttr (toString (to2dec (imXSp / 2.0)), PX), CyAttr (toString (to2dec (imYSp / 2.0)), PX)])] }, False), st)

  genSVGLineImageAlgs :: LineImageAlg (*(GenSVGStVal s) -> *(!(!Real, !Real), !*GenSVGStVal s))
                                      (*(GenSVGStVal s) -> *(!b, !*GenSVGStVal s))
                                      ((!Real, !Real) (Maybe b) [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                                      (Bool c [Maybe SVGAttr] [(!Real, !Real) Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) Int *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
  genSVGLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage :: !(*(GenSVGStVal s) -> *(!ImageSpanReal, !*GenSVGStVal s))
                   !(Maybe (*(GenSVGStVal s) -> *(b, !*GenSVGStVal s)))
                   !(ImageSpanReal (Maybe b) [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                   Bool // Not used
                   c // Not used
                   ![Maybe SVGAttr]
                   ![ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)]
                   !(Set ImageTag)
                   Int // Not used
                   !*(GenSVGStVal s)
                -> *(!GenSVGSyn s, !*GenSVGStVal s)
    mkLineImage lineSpan mmarkers lineContent _ _ imAts imTrs imTas _ st
      #! (lineSpan, st) = lineSpan st
      #! (mmarkers, st) = evalMaybe mmarkers st
      #! (imTrs, st)    = sequence (strictTRMap (\f -> f lineSpan False) imTrs) st
      = lineContent lineSpan mmarkers imAts imTrs imTas st

  genSVGMarkersAlgs :: MarkersAlg (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                                  (*(GenSVGStVal s) -> *(!(!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)), !*GenSVGStVal s)) | iTask s
  genSVGMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (*(GenSVGStVal s) -> *(!b, !*GenSVGStVal s)))
                 !(Maybe (*(GenSVGStVal s) -> *(!c, !*GenSVGStVal s)))
                 !(Maybe (*(GenSVGStVal s) -> *(!d, !*GenSVGStVal s)))
                 !*(GenSVGStVal s) -> *(!(!Maybe b, !Maybe c, !Maybe d), !*GenSVGStVal s) | iTask s
    mkMarkers m1 m2 m3 st
      #! (m1, st) = evalMaybe m1 st
      #! (m2, st) = evalMaybe m2 st
      #! (m3, st) = evalMaybe m3 st
      = ((m1, m2, m3), st)
  genSVGLineContentAlgs :: LineContentAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                                          (ImageSpanReal (Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) [Maybe SVGAttr] [(![SVGTransform], !ImageTransform)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)) | iTask s
  genSVGLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkLineImage
    , lineContentPolygonImageAlg    = mkPolygonImage
    , lineContentPolylineImageAlg   = mkPolylineImage
    }
    where
    mkLineImage :: !Slash !ImageSpanReal !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                   ![Maybe SVGAttr] ![(![SVGTransform], !ImageTransform)] !(Set ImageTag) !*(GenSVGStVal s)
                -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkLineImage sl sp=:(xspan, yspan) mmarkers imAts imTrs imTas st
      #! (y1, y2) = case sl of
                      Slash     -> (toString (to2dec yspan), "0.0")
                      Backslash -> ("0.0", toString (to2dec yspan))
      = mkLine LineElt [X1Attr ("0.0", PX), X2Attr (toString (to2dec xspan), PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : mkAttrs imAts imTrs] sp mmarkers st
    mkPolygonImage :: ![(!*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))] !ImageSpanReal
                      !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                      ![Maybe SVGAttr] ![(![SVGTransform], !ImageTransform)]
                      !(Set ImageTag) !*(GenSVGStVal s)
                   -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkPolygonImage points sp mmarkers imAts imTrs imTas st
      #! (offsets, st) = evalOffsets points st
      = mkLine PolygonElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs imAts imTrs] sp mmarkers st
    mkPolylineImage :: ![(!*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))] !ImageSpanReal
                       !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                       ![Maybe SVGAttr] ![(![SVGTransform], !ImageTransform)]
                       !(Set ImageTag) !*(GenSVGStVal s)
                    -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkPolylineImage points sp mmarkers imAts imTrs imTas st
      #! (offsets, st) = evalOffsets points st
      = mkLine PolylineElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs imAts imTrs] sp mmarkers st

    mkLine :: !([HtmlAttr] [SVGAttr] -> SVGElt) ![SVGAttr] !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) clval
      #! (uid1, clval) = nextNo clval
      #! (uid2, clval) = nextNo clval
      #! (uid3, clval) = nextNo clval
      #! markersAndIds = [(m, i, s, d, p) \\ Just (m, i, s, d, p) <- [ mkMarkerAndId mmStart (mkMarkerId editletId uid1) MarkerStartAttr
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

  genSVGCompositeImageAlgs :: CompositeImageAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                                                (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                                                ((Maybe (GenSVGSyn s)) ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                                                (Bool ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) Int *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)) | iTask s
  genSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: !(Maybe (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)))
                        !((Maybe (GenSVGSyn s)) ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                        !Bool
                        !ImageSpanReal
                        ![Maybe SVGAttr]
                        ![ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)]
                        !(Set ImageTag) !Int !*(GenSVGStVal s)
                     -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkCompositeImage host compose interactive totalSpanPreTrans imAts imTrs imTas uniqId st
      #! (host, st)    = evalMaybe host st
      #! (compose, st) = compose host totalSpanPreTrans imAts imTrs imTas st
      #! (cpId, st)    = getCpId st
      #! (elts, spans, onclicks, draggables, idMap)
           = case host of
               Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal, genSVGSyn_events, genSVGSyn_draggable, genSVGSyn_idMap}
                 = (genSVGSyn_svgElts ++ compose.genSVGSyn_svgElts, genSVGSyn_imageSpanReal, 'DM'.union genSVGSyn_events compose.genSVGSyn_events, 'DM'.union genSVGSyn_draggable compose.genSVGSyn_draggable, 'DM'.union genSVGSyn_idMap compose.genSVGSyn_idMap)
               _ = (compose.genSVGSyn_svgElts, compose.genSVGSyn_imageSpanReal, compose.genSVGSyn_events, compose.genSVGSyn_draggable, compose.genSVGSyn_idMap)
      #! (imTrs, st) = sequence (strictTRMap (\f -> f spans False) imTrs) st
      = ({ genSVGSyn_imageSpanReal = (0.0, 0.0)
         , genSVGSyn_svgElts       = mkGroup (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs imAts imTrs) elts
         , genSVGSyn_events        = onclicks
         , genSVGSyn_draggable     = draggables
         , genSVGSyn_idMap         = idMap
         }, st)
    getCpId :: !*(GenSVGStVal s) -> (!String, !*GenSVGStVal s) | iTask s
    getCpId clval
      #! (n, clval) = nextNo clval
      = (mkClipPathId editletId n, clval)

  genSVGComposeAlgs :: ComposeAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))
                                  (*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s))
                                  ((Maybe (GenSVGSyn s)) ImageSpanReal [Maybe SVGAttr] [ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)] (Set ImageTag) *(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)) | iTask s
  genSVGComposeAlgs =
    { composeAsGridAlg    = \_ _ _ _ _ _ _ _ _ -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsOverlayAlg = \_ _ _ _ _ _ _ _   -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsCollageAlg = mkCollage
    }
    where
    mkCollage :: ![(!*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s), !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s))]
                 ![*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)]
                 (Maybe (GenSVGSyn s)) // Not used
                 !ImageSpanReal ![Maybe SVGAttr]
                 ![ImageSpanReal Bool *(GenSVGStVal s) -> *(!(![SVGTransform], !ImageTransform), !*GenSVGStVal s)]
                 !(Set ImageTag) !*(GenSVGStVal s)
              -> *(!GenSVGSyn s, !*GenSVGStVal s) | iTask s
    mkCollage offsets imgs _ totalSpanPreTrans imAts imTrs imTas st
      #! (offsets, st) = evalOffsets offsets st
      #! (imgsSps, st) = sequence imgs st
      = ({ genSVGSyn_svgElts       = flattenTR (strictTRZipWith mkTranslateGroup offsets (strictTRMap (\x -> x.genSVGSyn_svgElts) imgsSps))
         , genSVGSyn_events        = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_events) imgsSps)
         , genSVGSyn_draggable     = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_draggable) imgsSps)
         , genSVGSyn_idMap         = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_idMap) imgsSps)
         , genSVGSyn_imageSpanReal = totalSpanPreTrans }, st) // Setting genSVGSyn_imageSpanReal is required here. It needs to be totalSpanPreTrans, because transforms will be calculated just after this.

  genSVGSpanAlgs :: SpanAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) | iTask s
  genSVGSpanAlgs = evalSpanSpanAlgs

  genSVGLookupSpanAlgs :: LookupSpanAlg (*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)) | iTask s
  genSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

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
//addAttr (AlignmentBaselineAttr   !String
//addAttr (BaseProfileAttr         !String
//addAttr (ContentScriptTypeAttr   !String
//addAttr (ClipPathAttr            !String
//addAttr (CxAttr                  !SVGCoordinate
//addAttr (CyAttr                  !SVGCoordinate
//addAttr (DominantBaselineAttr    !String
//addAttr (ExternalResourcesRequiredAttr !Bool
//addAttr (FillAttr                !SVGPaint
//addAttr (FillOpacityAttr         !SVGFillOpacity
//addAttr (FillRuleAttr            !SVGFillRule
//addAttr (FontFamilyAttr          !String
//addAttr (FontSizeAttr            !String
//addAttr (FontStyleAttr           !String
//addAttr (FontStretchAttr         !String
//addAttr (FontVariantAttr         !String
//addAttr (FontWeightAttr          !String
//addAttr (LengthAdjustAttr        !SVGLengthAdjust
//addAttr (MarkerStartAttr         !String
//addAttr (MarkerMidAttr           !String
//addAttr (MarkerEndAttr           !String
//addAttr (MarkerHeightAttr        !SVGLength
//addAttr (MarkerWidthAttr         !SVGLength
//addAttr (MaskAttr                !String
//addAttr (OffsetAttr              !String
//addAttr (OrientAttr              !String
//addAttr (PointsAttr              ![(String, String)]
//addAttr (PreserveAspectRatioAttr !(Maybe SVGDefer) !(Maybe SVGAlign) !(Maybe SVGMeetOrSlice)
//addAttr (RAttr                   !SVGLength
//addAttr (RefXAttr                !SVGLength
//addAttr (RefYAttr                !SVGLength
//addAttr (RxAttr                  !SVGLength
//addAttr (RyAttr                  !SVGLength
//addAttr (StopColorAttr           !String
//addAttr (StopOpacityAttr         !String
//addAttr (StrokeAttr              !SVGPaint
//addAttr (StrokeDashArrayAttr     !SVGStrokeDashArray
//addAttr (StrokeDashOffsetAttr    !SVGStrokeDashOffset
//addAttr (StrokeLineCapAttr       !SVGLineCap
//addAttr (StrokeLineJoinAttr      !SVGLineJoin
//addAttr (StrokeMiterLimitAttr    !SVGStrokeMiterLimit
//addAttr (StrokeOpacityAttr       !String
//addAttr (StrokeWidthAttr         !SVGStrokeWidth
//addAttr (TextAnchorAttr          !String
//addAttr (TextLengthAttr          !SVGLength
//addAttr (TextRenderingAttr       !String
addAttr (TransformAttr tfs) attrs = addTransforms tfs attrs []
  where
  addTransforms :: ![SVGTransform] ![SVGAttr] ![SVGAttr] -> [SVGAttr]
  addTransforms tfs []                            acc = reverseTR acc ++ [TransformAttr tfs]
  addTransforms tfs [TransformAttr tfs` : attrs`] acc = reverseTR acc ++ [TransformAttr (tfs ++ tfs`) : attrs`]
  addTransforms tfs [attr:attrs]                  acc = addTransforms tfs attrs [attr:acc]
//addAttr (VersionAttr             !String
//addAttr (ViewBoxAttr             !SVGNumber !SVGNumber !SVGNumber !SVGNumber
//addAttr (XAttr                   !SVGCoordinate
//addAttr (X1Attr                  !SVGLength
//addAttr (X2Attr                  !SVGLength
//addAttr (XLinkHRefAttr           !String
//addAttr (YAttr                   !SVGCoordinate
//addAttr (Y1Attr                  !SVGLength
//addAttr (Y2Attr                  !SVGLength
//addAttr (ZoomAndPanAttr          !SVGZoomAndPan
addAttr attr attrs = [attr:attrs]

addAttrs :: ![SVGAttr] ![SVGAttr] -> [SVGAttr]
addAttrs newAttrs oldAttrs = foldr addAttr oldAttrs newAttrs

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

flattenTR :: ![[a]] -> [a]
flattenTR xss = reverseTR (flattenTRAcc xss [])

flattenTRAcc :: ![[a]] [a] -> [a]
flattenTRAcc [] acc = acc
flattenTRAcc [xs:xss] acc
  #! r = reverseTR xs ++ acc
  = flattenTRAcc xss r

mkAttrs :: ![Maybe SVGAttr] ![(![SVGTransform], !ImageTransform)] -> [SVGAttr]
mkAttrs imAts [] = getSvgAttrs imAts
mkAttrs imAts xs = addAttr (TransformAttr (flattenTR (strictTRMap fst xs))) (getSvgAttrs imAts)

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

evalSpan :: !Span !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
evalSpan sp st = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs sp st

evalSpanSpanAlgs :: SpanAlg (*(GenSVGStVal s) -> *(!Real, !*(GenSVGStVal s))) (*(GenSVGStVal s) -> *(!Real, !*(GenSVGStVal s)))
evalSpanSpanAlgs =:
  { spanPxSpanAlg     = ret
  , spanLookupSpanAlg = ($)
  , spanAddSpanAlg    = mkBin (+)
  , spanSubSpanAlg    = mkBin (-)
  , spanMulSpanAlg    = mkBin (*)
  , spanDivSpanAlg    = mkBin (/)
  , spanAbsSpanAlg    = mkAbs
  , spanMinSpanAlg    = mkList minList
  , spanMaxSpanAlg    = mkList maxList
  }

evalSpanLookupSpanAlgs :: LookupSpanAlg (*(GenSVGStVal s) -> *(!Real, !*(GenSVGStVal s))) | iTask s
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg = evalSpanMkImageGridColSpan
  , lookupSpanRowYSpanAlg    = evalSpanMkImageGridRowSpan
  , lookupSpanImageXSpanAlg  = evalSpanMkImageXSpan
  , lookupSpanImageYSpanAlg  = evalSpanMkImageYSpan
  , lookupSpanTextXSpanAlg   = evalSpanMkTextLU
  }
  where
  evalSpanMkTextLU :: !FontDef !String !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
  evalSpanMkTextLU fd str st = (0.0, st)

  evalSpanMkImageXSpan :: !ImageTag !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
  evalSpanMkImageXSpan t st
    #! genStates                = st.genStates
    #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
    #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
    = case 'DM'.get t spanEnvImageTagPostTrans of
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

  evalSpanMkImageYSpan :: !ImageTag !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
  evalSpanMkImageYSpan t st
    #! genStates                = st.genStates
    #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
    #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
    = case 'DM'.get t spanEnvImageTagPostTrans of
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

  evalSpanMkImageGridColSpan :: !ImageTag !Int !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
  evalSpanMkImageGridColSpan t colIdx st
    #! genStates      = st.genStates
    #! spanEnvGridTag = genStates.spanEnvGridTag
    #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
    = case 'DM'.get t spanEnvGridTag of
        Just cacheIdx
          #! spanEnvGridSpan = genStates.spanEnvGridSpan
          #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
          = case 'DIS'.get cacheIdx spanEnvGridSpan of
              Just (xs, _)
                = case xs.[colIdx] of
                    PxSpan x -> (x, {st & genStates = genStates})
                    sp       -> evalSpan sp {st & genStates = genStates}
              _ = (0.0, {st & genStates = genStates})
        _ = (0.0, {st & genStates = genStates})

  evalSpanMkImageGridRowSpan :: !ImageTag !Int !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s) | iTask s
  evalSpanMkImageGridRowSpan t rowIdx st
    #! genStates      = st.genStates
    #! spanEnvGridTag = genStates.spanEnvGridTag
    #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
    = case 'DM'.get t spanEnvGridTag of
        Just cacheIdx
          #! spanEnvGridSpan = genStates.spanEnvGridSpan
          #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
          = case 'DIS'.get cacheIdx spanEnvGridSpan of
              Just (_, xs)
                = case xs.[rowIdx] of
                    PxSpan x -> (x, {st & genStates = genStates})
                    sp       -> evalSpan sp {st & genStates = genStates}
              _ = (0.0, {st & genStates = genStates})
        _ = (0.0, {st & genStates = genStates})

mkAbs :: !(*a -> *(!b, !*a)) !*a -> *(!b, !*a) | abs b
mkAbs x st
  #! (x, st) = x st
  = (abs x, st)

mkBin :: !(a b -> c) !(*s -> *(!a, !*s)) !(*s -> *(!b, !*s)) !*s -> *(!c, !*s)
mkBin op x y st
  #! (x, st) = x st
  #! (y, st) = y st
  = (op x y, st)

mkList :: !([a] -> b) ![*c -> *(!a, !*c)] !*c -> *(!b, !*c)
mkList f xs st
  #! (xs, st) = sequence xs st
  = (f xs, st)

:: Algebras m imCo imAt imTr im baIm imSp coIm ho co sp loSp ma liIm liCo =
  { imageAlgs          :: !ImageAlg imCo imAt imTr sp im
  , imageContentAlgs   :: !ImageContentAlg baIm imSp liIm coIm imCo
  , imageAttrAlgs      :: !ImageAttrAlg m imAt
  , imageTransformAlgs :: !ImageTransformAlg sp imTr
  , imageSpanAlgs      :: !ImageSpanAlg sp imSp
  , basicImageAlgs     :: !BasicImageAlg baIm
  , lineImageAlgs      :: !LineImageAlg imSp ma liCo liIm
  , markersAlgs        :: !MarkersAlg im ma
  , lineContentAlgs    :: !LineContentAlg sp liCo
  , compositeImageAlgs :: !CompositeImageAlg sp ho co coIm
  , composeAlgs        :: !ComposeAlg sp im co
  , spanAlgs           :: !SpanAlg loSp sp
  , lookupSpanAlgs     :: !LookupSpanAlg loSp
  }

:: ImageAlg imCo imAt imTr sp im =
  { imageAlg :: !imCo (Maybe im) [imAt] [imTr] (Set ImageTag) Int (!sp, !sp) (!sp, !sp) (!sp, !sp) -> im
  }

:: ImageContentAlg baIm imSp liIm coIm imCo =
  { imageContentBasicAlg     :: !baIm imSp -> imCo
  , imageContentLineAlg      :: !liIm      -> imCo
  , imageContentCompositeAlg :: !coIm      -> imCo
  }

:: ImageAttrAlg m imAt =
  { imageAttrImageStrokeAttrAlg   :: !(StrokeAttr m)      -> imAt
  , imageAttrStrokeWidthAttrAlg   :: !(StrokeWidthAttr m) -> imAt
  , imageAttrXRadiusAttrAlg       :: !(XRadiusAttr m)     -> imAt
  , imageAttrYRadiusAttrAlg       :: !(YRadiusAttr m)     -> imAt
  , imageAttrStrokeOpacityAttrAlg :: !(OpacityAttr m)     -> imAt
  , imageAttrFillAttrAlg          :: !(FillAttr m)        -> imAt
  , imageAttrFillOpacityAttrAlg   :: !(OpacityAttr m)     -> imAt
  , imageAttrOnClickAttrAlg       :: !(OnClickAttr m)     -> imAt
  , imageAttrOnMouseDownAttrAlg   :: !(OnMouseDownAttr m) -> imAt
  , imageAttrOnMouseUpAttrAlg     :: !(OnMouseUpAttr m)   -> imAt
  , imageAttrOnMouseOverAttrAlg   :: !(OnMouseOverAttr m) -> imAt
  , imageAttrOnMouseMoveAttrAlg   :: !(OnMouseMoveAttr m) -> imAt
  , imageAttrOnMouseOutAttrAlg    :: !(OnMouseOutAttr m)  -> imAt
  , imageAttrDraggableAttrAlg     :: !(DraggableAttr m) -> imAt
  , imageAttrDashAttrAlg          :: !(DashAttr m)        -> imAt
  }

:: ImageTransformAlg sp imTr =
  { imageTransformRotateImageAlg :: !Angle -> imTr
  , imageTransformSkewXImageAlg  :: !Angle -> imTr
  , imageTransformSkewYImageAlg  :: !Angle -> imTr
  , imageTransformFitImageAlg    :: !sp sp -> imTr
  , imageTransformFitXImageAlg   :: !sp    -> imTr
  , imageTransformFitYImageAlg   :: !sp    -> imTr
  , imageTransformFlipXImageAlg  ::           imTr
  , imageTransformFlipYImageAlg  ::           imTr
  }

:: ImageSpanAlg sp imSp =
  { imageSpanAlg :: !sp sp -> imSp
  }

:: BasicImageAlg baIm =
  { basicImageEmptyImageAlg   :: !                  baIm
  , basicImageTextImageAlg    :: !FontDef String -> baIm
  , basicImageCircleImageAlg  :: !                  baIm
  , basicImageRectImageAlg    :: !                  baIm
  , basicImageEllipseImageAlg :: !                  baIm
  }

:: LineImageAlg imSp ma liCo liIm =
  { lineImageLineImageAlg :: !imSp (Maybe ma) liCo -> liIm
  }

:: LineContentAlg sp liCo =
  { lineContentSimpleLineImageAlg :: !Slash        -> liCo
  , lineContentPolygonImageAlg    :: ![(!sp, !sp)] -> liCo
  , lineContentPolylineImageAlg   :: ![(!sp, !sp)] -> liCo
  }

:: MarkersAlg im ma =
  { markersMarkersAlg :: !(Maybe im) (Maybe im) (Maybe im) -> ma
  }

:: CompositeImageAlg sp ho co coIm =
  { compositeImageAlg :: !(Maybe ho) co -> coIm
  }

:: ComposeAlg sp im co =
  { composeAsGridAlg    :: !(!Int, !Int) [[(!sp, !sp)]] [[ImageAlign]] [[im]] -> co
  , composeAsCollageAlg :: !             [(!sp, !sp)]                  [im]   -> co
  , composeAsOverlayAlg :: !             [(!sp, !sp)]   [ImageAlign]   [im]   -> co
  }

:: SpanAlg loSp sp =
  { spanPxSpanAlg     :: !Real  -> sp
  , spanLookupSpanAlg :: !loSp  -> sp
  , spanAddSpanAlg    :: !sp sp -> sp
  , spanSubSpanAlg    :: !sp sp -> sp
  , spanMulSpanAlg    :: !sp sp -> sp
  , spanDivSpanAlg    :: !sp sp -> sp
  , spanAbsSpanAlg    :: !sp    -> sp
  , spanMinSpanAlg    :: ![sp]  -> sp
  , spanMaxSpanAlg    :: ![sp]  -> sp
  }

:: LookupSpanAlg loSp =
  { lookupSpanColumnXSpanAlg :: !ImageTag Int -> loSp
  , lookupSpanImageXSpanAlg  :: !ImageTag     -> loSp
  , lookupSpanImageYSpanAlg  :: !ImageTag     -> loSp
  , lookupSpanRowYSpanAlg    :: !ImageTag Int -> loSp
  , lookupSpanTextXSpanAlg   :: !FontDef String     -> loSp
  }

cataOffsets :: !(SpanAlg a b) !(LookupSpanAlg a) ![(!Span, !Span)] -> [(!b, !b)]
cataOffsets spanAlgs lookupSpanAlgs xs = strictTRMap (f spanAlgs lookupSpanAlgs) xs
  where
  f :: !(SpanAlg a b) !(LookupSpanAlg a) !(!Span, !Span) -> (!b, !b)
  f spanAlgs lookupSpanAlgs (l, r)
    #! synl = spanCata spanAlgs lookupSpanAlgs l
    #! synr = spanCata spanAlgs lookupSpanAlgs r
    = (synl, synr)

imageCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(Image m) -> im
imageCata allAlgs { Image | content, mask, attribs, transform, tags, uniqId, totalSpanPreTrans = (txsp, tysp), totalSpanPostTrans = (txsp`, tysp`), transformCorrection = (tfXCorr, tfYCorr) }
  #! synContent    = imageContentCata allAlgs content
  #! synMask       = fmap (imageCata allAlgs) mask
  #! synsAttribs   = strictTRMap (imageAttrCata allAlgs.imageAttrAlgs) ('DS'.toList attribs)
  #! synsTransform = strictTRMap (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  #! synTXsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp
  #! synTYsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp
  #! synTXsp`      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp`
  #! synTYsp`      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp`
  #! synXCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfXCorr
  #! synYCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfYCorr
  = allAlgs.imageAlgs.imageAlg synContent synMask synsAttribs synsTransform tags uniqId (synTXsp, synTYsp) (synTXsp`, synTYsp`) (synXCorr, synYCorr)

imageContentCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(ImageContent m) -> imCo
imageContentCata allAlgs (Basic bi is)
  #! synBasicImage = basicImageCata allAlgs.basicImageAlgs bi
  #! synImageSpan  = span2TupleCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs is
  = allAlgs.imageContentAlgs.imageContentBasicAlg synBasicImage synImageSpan
imageContentCata allAlgs (Line li)
  #! synLineImage = lineImageCata allAlgs li
  = allAlgs.imageContentAlgs.imageContentLineAlg synLineImage
imageContentCata allAlgs (Composite ci)
  #! synCompositeImage = compositeImageCata allAlgs ci
  = allAlgs.imageContentAlgs.imageContentCompositeAlg synCompositeImage

imageAttrCata :: !(ImageAttrAlg m imAt) !(ImageAttr m) -> imAt
imageAttrCata imageAttrAlgs (ImageStrokeAttr sa)         = imageAttrAlgs.imageAttrImageStrokeAttrAlg sa
imageAttrCata imageAttrAlgs (ImageStrokeWidthAttr swa)   = imageAttrAlgs.imageAttrStrokeWidthAttrAlg swa
imageAttrCata imageAttrAlgs (ImageXRadiusAttr r)         = imageAttrAlgs.imageAttrXRadiusAttrAlg r
imageAttrCata imageAttrAlgs (ImageYRadiusAttr r)         = imageAttrAlgs.imageAttrYRadiusAttrAlg r
imageAttrCata imageAttrAlgs (ImageStrokeOpacityAttr swa) = imageAttrAlgs.imageAttrStrokeOpacityAttrAlg swa
imageAttrCata imageAttrAlgs (ImageFillAttr fa)           = imageAttrAlgs.imageAttrFillAttrAlg fa
imageAttrCata imageAttrAlgs (ImageFillOpacityAttr swa)   = imageAttrAlgs.imageAttrFillOpacityAttrAlg swa
imageAttrCata imageAttrAlgs (ImageOnClickAttr cl)        = imageAttrAlgs.imageAttrOnClickAttrAlg cl
imageAttrCata imageAttrAlgs (ImageOnMouseDownAttr cl)    = imageAttrAlgs.imageAttrOnMouseDownAttrAlg cl
imageAttrCata imageAttrAlgs (ImageOnMouseUpAttr cl)      = imageAttrAlgs.imageAttrOnMouseUpAttrAlg cl
imageAttrCata imageAttrAlgs (ImageOnMouseOverAttr cl)    = imageAttrAlgs.imageAttrOnMouseOverAttrAlg cl
imageAttrCata imageAttrAlgs (ImageOnMouseMoveAttr cl)    = imageAttrAlgs.imageAttrOnMouseMoveAttrAlg cl
imageAttrCata imageAttrAlgs (ImageOnMouseOutAttr cl)     = imageAttrAlgs.imageAttrOnMouseOutAttrAlg cl
imageAttrCata imageAttrAlgs (ImageDraggableAttr cl)      = imageAttrAlgs.imageAttrDraggableAttrAlg cl
imageAttrCata imageAttrAlgs (ImageDashAttr d)            = imageAttrAlgs.imageAttrDashAttrAlg d

imageTransformCata :: !(ImageTransformAlg sp imTr) !(SpanAlg loSp sp) !(LookupSpanAlg loSp) !ImageTransform -> imTr
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (RotateImage ia)
  = imageTransformAlgs.imageTransformRotateImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (SkewXImage ia)
  = imageTransformAlgs.imageTransformSkewXImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (SkewYImage ia)
  = imageTransformAlgs.imageTransformSkewYImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitImage sp1 sp2)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = imageTransformAlgs.imageTransformFitImageAlg synSpan1 synSpan2
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitXImage sp)
  #! synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = imageTransformAlgs.imageTransformFitXImageAlg synSpan
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitYImage sp)
  #! synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = imageTransformAlgs.imageTransformFitYImageAlg synSpan
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs FlipXImage
  = imageTransformAlgs.imageTransformFlipXImageAlg
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs FlipYImage
  = imageTransformAlgs.imageTransformFlipYImageAlg

basicImageCata :: !(BasicImageAlg baIm) !BasicImage -> baIm
basicImageCata basicImageAlgs EmptyImage         = basicImageAlgs.basicImageEmptyImageAlg
basicImageCata basicImageAlgs (TextImage fd str) = basicImageAlgs.basicImageTextImageAlg fd str
basicImageCata basicImageAlgs CircleImage        = basicImageAlgs.basicImageCircleImageAlg
basicImageCata basicImageAlgs RectImage          = basicImageAlgs.basicImageRectImageAlg
basicImageCata basicImageAlgs EllipseImage       = basicImageAlgs.basicImageEllipseImageAlg

lineImageCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(LineImage m) -> liIm
lineImageCata allAlgs { LineImage | lineSpan, markers, lineContent }
  #! synImageSpan   = span2TupleCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs lineSpan
  #! synMarkers     = fmap (markersCata allAlgs) markers
  #! synLineContent = lineContentCata allAlgs.lineContentAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs lineContent
  = allAlgs.lineImageAlgs.lineImageLineImageAlg synImageSpan synMarkers synLineContent

markersCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(Markers m) -> ma
markersCata allAlgs { Markers | markerStart, markerMid, markerEnd }
  #! synStart = fmap (imageCata allAlgs) markerStart
  #! synMid   = fmap (imageCata allAlgs) markerMid
  #! synEnd   = fmap (imageCata allAlgs) markerEnd
  = allAlgs.markersAlgs.markersMarkersAlg synStart synMid synEnd

lineContentCata :: !(LineContentAlg sp liCo) !(SpanAlg loSp sp) !(LookupSpanAlg loSp) !LineContent -> liCo
lineContentCata lineContentAlgs _ _ (SimpleLineImage sl)
  = lineContentAlgs.lineContentSimpleLineImageAlg sl
lineContentCata lineContentAlgs spanAlgs lookupSpanAlgs (PolygonImage offsets)
  #! synsImageOffset = cataOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolygonImageAlg synsImageOffset
lineContentCata lineContentAlgs spanAlgs lookupSpanAlgs (PolylineImage offsets)
  #! synsImageOffset = cataOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolylineImageAlg synsImageOffset

span2TupleCata :: !(ImageSpanAlg sp imSp) !(SpanAlg loSp sp) !(LookupSpanAlg loSp) !(Span, Span) -> imSp
span2TupleCata imageSpanAlgs spanAlgs lookupSpanAlgs (xspan, yspan)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs xspan
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs yspan
  = imageSpanAlgs.imageSpanAlg synSpan1 synSpan2

compositeImageCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(CompositeImage m) -> coIm
compositeImageCata allAlgs { CompositeImage | host, compose }
  #! synHost    = fmap (imageCata allAlgs) host
  #! synCompose = composeCata allAlgs compose
  = allAlgs.compositeImageAlgs.compositeImageAlg synHost synCompose

composeCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(Compose m) -> co
composeCata allAlgs (AsGrid n offsetss ias imgss)
  #! synsImageOffsetss = strictTRMap (cataOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs) offsetss
  #! synsContent       = strictTRMap (strictTRMap (imageCata allAlgs)) imgss
  = allAlgs.composeAlgs.composeAsGridAlg n synsImageOffsetss ias synsContent
composeCata allAlgs (AsCollage offsets imgs)
  #! synsImageOffsets = cataOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  #! synsContent      = strictTRMap (imageCata allAlgs) imgs
  = allAlgs.composeAlgs.composeAsCollageAlg synsImageOffsets synsContent
composeCata allAlgs (AsOverlay offsets ias imgs)
  #! synsImageOffsets = cataOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  #! synsContent      = strictTRMap (imageCata allAlgs) imgs
  = allAlgs.composeAlgs.composeAsOverlayAlg synsImageOffsets ias synsContent

spanCata :: !(SpanAlg loSp sp) !(LookupSpanAlg loSp) !Span -> sp
spanCata spanAlgs lookupSpanAlgs (PxSpan rl)
  = spanAlgs.spanPxSpanAlg rl
spanCata spanAlgs lookupSpanAlgs (LookupSpan lu)
  #! synLookup = lookupCata lookupSpanAlgs lu
  = spanAlgs.spanLookupSpanAlg synLookup
spanCata spanAlgs lookupSpanAlgs (AddSpan sp1 sp2)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanAddSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (SubSpan sp1 sp2)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanSubSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (MulSpan sp1 sp2)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanMulSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (DivSpan sp1 sp2)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanDivSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (AbsSpan sp)
  #! synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = spanAlgs.spanAbsSpanAlg synSpan
spanCata spanAlgs lookupSpanAlgs (MinSpan sps)
  #! synsSpans = strictTRMap (spanCata spanAlgs lookupSpanAlgs) sps
  = spanAlgs.spanMinSpanAlg synsSpans
spanCata spanAlgs lookupSpanAlgs (MaxSpan sps)
  #! synsSpans = strictTRMap (spanCata spanAlgs lookupSpanAlgs) sps
  = spanAlgs.spanMaxSpanAlg synsSpans

lookupCata :: !(LookupSpanAlg loSp) !LookupSpan -> loSp
lookupCata lookupSpanAlgs (ColumnXSpan imts n)
  = lookupSpanAlgs.lookupSpanColumnXSpanAlg imts n
lookupCata lookupSpanAlgs (RowYSpan imts n)
  = lookupSpanAlgs.lookupSpanRowYSpanAlg imts n
lookupCata lookupSpanAlgs (ImageXSpan imts)
  = lookupSpanAlgs.lookupSpanImageXSpanAlg imts
lookupCata lookupSpanAlgs (ImageYSpan imts)
  = lookupSpanAlgs.lookupSpanImageYSpanAlg imts
lookupCata lookupSpanAlgs (TextXSpan fd str)
  = lookupSpanAlgs.lookupSpanTextXSpanAlg fd str
