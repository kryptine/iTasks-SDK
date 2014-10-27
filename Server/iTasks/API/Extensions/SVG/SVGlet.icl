implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
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
from StdFunc import `bind`
import Text

derive class iTask FontDef, Set

:: GenSVGStVal s =
  { uniqueIdCounter :: Int
  }

:: CachedSpan =
  { cachedGridSpans :: Maybe ({Span}, {Span})
  , cachedImageSpan :: Maybe ImageSpan
  }

mainSvgId :: !ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

addOnclicks :: ComponentId (JSObj svg) (Map String (s -> s)) *JSWorld -> *JSWorld | iTask s
addOnclicks cid svg onclicks world
  = 'DM'.foldrWithKey f world onclicks
  where
  f :: String (s -> s) *JSWorld -> *JSWorld | iTask s
  f elemCls sttf world
    # elemCls           = replaceSubString editletId cid elemCls
    # (elems, world)    = (svg `getElementsByClassName` elemCls) world
    # (numElems, world) = .? (elems .# "length") world
    | jsValToInt (numElems) < 1 = world
    # (elem, world)     = .? (elems .# "0") world
    # cb                = createEditletEventHandler (mkCB sttf) cid
    # (_, world)        = (elem `addEventListener` ("click", cb, True)) world
    = world
  mkCB :: (s -> s) String {JSObj JSEvent} (SVGClSt s) *JSWorld -> *(SVGClSt s, *JSWorld) | iTask s
  mkCB sttf _ _ clval=:{svgClSt} world
    # newSt = sttf svgClSt
    = ({clval & svgClSt = newSt, svgClIsDefault = False, svgClHasStUpd = not (newSt === svgClSt)}, world)
  mkCB sttf _ _ clval world
    = ({clval & svgClIsDefault = False}, world)

imageView :: !(s -> Image s) -> ViewOption s | iTask s
imageView toImage = ViewWith (\s -> svgRenderer s toImage)

imageViewUpdate :: !(s -> v) !(v -> Image v)  !(s v -> s`) -> UpdateOption s s` |  iTask v
imageViewUpdate toViewState toImage fromViewState
  = UpdateWith (\s -> svgRenderer (toViewState s) toImage) (\s e -> fromViewState s e.Editlet.currVal.svgSrvSt)

derive class iTask ActionState

ifAction :: !(a -> Bool) !(a s -> s) !((ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b
ifAction pred astos stotaskb (Value {ActionState|state=s,action=Just a} _)
  | pred a    = Just (stotaskb {ActionState|state = astos a s, action = Nothing})
  | otherwise = Nothing
ifAction _ _ _ _ = Nothing

svgns :== "http://www.w3.org/2000/svg"

:: SVGSrvSt s =
  { svgSrvIsDefault  :: Bool
  , svgSrvHasStUpd   :: Bool
  , svgSrvHasFontUpd :: Bool
  , svgSrvFontRender :: Bool
  , svgSrvSt         :: s
  , svgSrvStrings    :: Map FontDef (Set String)
  , svgSrvTextWidths :: Map FontDef (Map String Real)
  }

defaultSrvSt :: s -> SVGSrvSt s
defaultSrvSt s = { svgSrvIsDefault  = True
                 , svgSrvHasStUpd   = False
                 , svgSrvHasFontUpd = False
                 , svgSrvFontRender = False
                 , svgSrvSt         = s
                 , svgSrvStrings    = 'DM'.newMap
                 , svgSrvTextWidths = 'DM'.newMap
                 }

:: SVGClSt s =
  { svgClIsDefault  :: Bool
  , svgClHasStUpd   :: Bool
  , svgClHasFontUpd :: Bool
  , svgClSt         :: s
  , svgClStrings    :: Map FontDef (Set String)
  , svgClTextWidths :: Map FontDef (Map String Real)
  , svgClCallbacks  :: Map String (s -> s)
  , svgClImageStr   :: Maybe String
  }

defaultClSt :: s -> SVGClSt s
defaultClSt s = { svgClIsDefault  = True
                , svgClHasStUpd   = False
                , svgClHasFontUpd = False
                , svgClSt         = s
                , svgClStrings    = 'DM'.newMap
                , svgClTextWidths = 'DM'.newMap
                , svgClCallbacks  = 'DM'.newMap
                , svgClImageStr   = Nothing
                }

:: SVGDiff s
  = SetState s
  | SetImage       String (Map String (s -> s))
  | SetFontStringsMap     (Map FontDef (Set String))
  | SetFontStringWidthMap (Map FontDef (Map String Real))
  | SetHasStUpd
  | SetHasNoStUpd
  | SetHasFontUpd
  | SetHasNoFontUpd
  | SetHasNoFontRender

derive class iTask SVGDiff, SVGSrvSt

import StdDebug

// TODO : Immediately render if no fonts are detected
svgRenderer :: !s !(s -> Image s) -> Editlet (SVGSrvSt s) [SVGDiff s] | iTask s
svgRenderer origState state2Image
  = { currVal   = {defaultSrvSt origState & svgSrvIsDefault = False}
    , genUI     = genUI
    , serverDef = server
    , clientDef = client
    }
  where
  server
    = { EditletDef
      | performIO = serverIO
      , defVal    = defaultSrvSt origState
      , genDiff   = genServerDiff
      , appDiff   = appServerDiff
      }
  client
    = { EditletDef
      | performIO = updateUI
      , defVal    = defaultClSt origState
      , genDiff   = genClientDiff
      , appDiff   = appClientDiff
      }
  genUI cid world
    = ({ ComponentHTML
       | width         = FlexSize
       , height        = FlexSize
       , html          = DivTag [IdAttr (mainSvgId cid)] []
       , eventHandlers = []
       }
       , world
      )

  serverIO _ _ s=:{svgSrvHasFontUpd = True} w
    = ({s & svgSrvFontRender = True}, trace_n "Server performIO svgSrvHasFontUpd" w)

  serverIO _ _ s w
    = (s, trace_n "Server performIO fallthrough" w)

  updateUI cid (Just [SetFontStringsMap fontMap : ds]) clst world
    # world = jsTrace "updateUI Just [SetFontStringsMap fontMap : ds]" world
    # (realFontMap, world) = calcTextLengths fontMap world
    = updateUI cid (Just ds) {clst & svgClTextWidths = realFontMap, svgClIsDefault = False, svgClHasFontUpd = True} world

  updateUI cid (Just [SetImage svgStr onclicks:ds]) clst world
    # world = jsTrace "updateUI Just [SetImage svgStr onclicks:ds]" world
    # svgStr           = replaceSubString editletId cid svgStr
    # world = jsTrace svgStr world
    # (parser, world)  = new "DOMParser" () world
    # (doc, world)     = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
    # (newSVG, world)  = .? (doc .# "firstChild") world
    # (svgDiv, world)  = getDomElement (mainSvgId cid) world
    # (currSVG, world) = .? (svgDiv .# "firstChild") world
    # (_, world)       = if (jsIsNull currSVG)
                           ((svgDiv `appendChild` newSVG) world)
                           ((svgDiv .# "replaceChild" .$ (newSVG, currSVG)) world)
    # world           = addOnclicks cid newSVG onclicks world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False} world

  updateUI cid (Just [SetFontStringWidthMap mp:ds]) clst world
    # world = jsTrace "updateUI Just [SetFontStringWidthMap _:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClTextWidths = mp} world

  updateUI cid (Just [SetHasFontUpd:ds]) clst world
    # world = jsTrace "updateUI Just [SetHasFontUpd:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClHasFontUpd = True} world

  updateUI cid (Just [SetHasNoFontUpd:ds]) clst world
    # world = jsTrace "updateUI Just [SetHasNoFontUpd:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClHasFontUpd = False} world

  updateUI cid (Just [SetState s:ds]) clst world
    # world = jsTrace "updateUI Just [SetState s:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClSt = s} world

  updateUI cid (Just [SetHasStUpd:ds]) clst world
    # world = jsTrace "updateUI Just [SetHasStUpd:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClHasStUpd = True} world

  updateUI cid (Just [SetHasNoStUpd:ds]) clst world
    # world = jsTrace "updateUI Just [SetHasNoStUpd:ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False, svgClHasStUpd = False} world

  updateUI cid (Just [_ : ds]) clst world
    # world = jsTrace "updateUI Just [_ : ds]" world
    = updateUI cid (Just ds) {clst & svgClIsDefault = False} world

  updateUI _ _ clval world
    # world = jsTrace "updateUI fallthrough" world
    = ({clval & svgClIsDefault = False}, world)

  // In this function we assume that all spans have already been reduced to
  // pixels. If there still are unresolved lookups, they will be defaulted to
  // 0px.
  renderSVG :: (Image s) -> (String, Map String (s -> s)) | iTask s
  renderSVG img
    # (syn, clval)   = genSVG img { uniqueIdCounter = 0 }
    # (imXSp, imYSp) = syn.genSVGSyn_imageSpanReal
    # (imXSp, imYSp) = (toString (toInt imXSp), toString (toInt imYSp))
    # svgStr         = toString (SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns]
                                        [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp]
                                        syn.genSVGSyn_svgElts)
    = (svgStr, syn.genSVGSyn_onclicks)

  fixImageSpans img env
    = fst (fixSpans img { fixSpansTaggedSpanEnv = 'DM'.newMap
                        , fixSpansDidChange     = False
                        , fixSpansCounter       = 0
                        , fixSpansFonts         = env})

  diffFromImgState srvSt
    # image   = state2Image srvSt.svgSrvSt
    # fontMap = gatherFonts image
    | 'DM'.null fontMap      = mkSetImageDiff image 'DM'.newMap
    | srvSt.svgSrvHasFontUpd = mkSetImageDiff image srvSt.svgSrvTextWidths
    | otherwise              = [SetFontStringsMap fontMap]
    where
    mkSetImageDiff image mp
      # (svgStr, onClicks) = renderSVG (fixImageSpans image mp)
      = trace_n svgStr [SetImage svgStr onClicks, SetHasNoFontUpd]

  genServerDiff oldSrvSt newSrvSt
    | newSrvSt.svgSrvFontRender = trace_n ("\ngenServerDiff newSrvSt.svgSrvFontRender\n\toldSt: " +++ toString (toJSON oldSrvSt) +++ "\n\tnewSt: " +++ toString (toJSON newSrvSt)) Just [SetHasNoFontRender : diffFromImgState newSrvSt]
    | oldSrvSt.svgSrvIsDefault  = trace_n ("\ngenServerDiff oldSrvSt.svgSrvIsDefault \n\toldSt: " +++ toString (toJSON oldSrvSt) +++ "\n\tnewSt: " +++ toString (toJSON newSrvSt)) Just [SetState newSrvSt.svgSrvSt : diffFromImgState newSrvSt]
    | newSrvSt.svgSrvHasFontUpd = trace_n ("\ngenServerDiff newSrvSt.svgSrvHasFontUpd\n\toldSt: " +++ toString (toJSON oldSrvSt) +++ "\n\tnewSt: " +++ toString (toJSON newSrvSt)) Just (diffFromImgState newSrvSt)
    | newSrvSt.svgSrvHasStUpd   = trace_n ("\ngenServerDiff newSrvSt.svgSrvHasStUpd  \n\toldSt: " +++ toString (toJSON oldSrvSt) +++ "\n\tnewSt: " +++ toString (toJSON newSrvSt)) Just [SetHasNoStUpd : diffFromImgState newSrvSt]
    | otherwise                 = trace_n ("\ngenServerDiff otherwise                \n\toldSt: " +++ toString (toJSON oldSrvSt) +++ "\n\tnewSt: " +++ toString (toJSON newSrvSt)) Nothing

  appServerDiff ds srvSt = appServerDiff` ds {srvSt & svgSrvIsDefault = False}
    where
    appServerDiff` [SetState st : ds]               srvSt
      | st === srvSt.svgSrvSt = trace_n "appServerDiff` [SetState st : ds] st === srvSt.svgSrvSt" appServerDiff` ds {srvSt & svgSrvHasStUpd = False}
      | otherwise             = trace_n "appServerDiff` [SetState st : ds] otherwise" appServerDiff` ds {srvSt & svgSrvHasStUpd = True, svgSrvSt = st}
    appServerDiff` [SetFontStringWidthMap swm : ds] srvSt = trace_n "appServerDiff` [SetFontStringWidthMap swm : ds]" appServerDiff` ds {srvSt & svgSrvHasFontUpd = True, svgSrvTextWidths = swm}
    appServerDiff` [SetHasFontUpd : ds]             srvSt = trace_n "appServerDiff` [SetHasFontUpd : ds]" appServerDiff` ds {srvSt & svgSrvHasFontUpd = True}
    appServerDiff` [SetHasNoFontUpd : ds]           srvSt = trace_n "appServerDiff` [SetHasNoFontUpd : ds]" appServerDiff` ds {srvSt & svgSrvHasFontUpd = False}
    appServerDiff` [SetHasStUpd : ds]               srvSt = trace_n "appServerDiff` [SetHasStUpd : ds]" appServerDiff` ds {srvSt & svgSrvHasStUpd = True}
    appServerDiff` [SetHasNoStUpd : ds]             srvSt = trace_n "appServerDiff` [SetHasNoStUpd : ds]" appServerDiff` ds {srvSt & svgSrvHasStUpd = False}
    appServerDiff` [SetHasNoFontRender : ds]        srvSt = trace_n "appServerDiff` [SetHasNoFontRender : ds]" appServerDiff` ds {srvSt & svgSrvFontRender = False}
    appServerDiff` [_ : ds]                         srvSt = trace_n "appServerDiff` [_ : ds]" appServerDiff` ds srvSt
    appServerDiff` _                                srvSt = trace_n "appServerDiff` _" srvSt

  genClientDiff oldClSt newClSt
    # ds1 = if (oldClSt.svgClSt === newClSt.svgClSt) [] [SetState newClSt.svgClSt]
    # ds2 = if (oldClSt.svgClTextWidths === newClSt.svgClTextWidths) [] [SetFontStringWidthMap newClSt.svgClTextWidths]
    # ds3 = if newClSt.svgClHasStUpd [SetHasStUpd] []
    # ds4 = if newClSt.svgClHasFontUpd [SetHasFontUpd] []
    = case ds1 ++ ds2 ++ ds3 ++ ds4 of
        [] -> trace_n "genClientDiff []" Nothing
        xs -> trace_n ("genClientDiff xs: " +++ toString (toJSON xs)) Just xs

  appClientDiff ds clSt = appClientDiff` ds {clSt & svgClIsDefault = False}
    where
    appClientDiff` [SetState st:ds]              clSt = appClientDiff ds {clSt & svgClSt = st}
    appClientDiff` [SetFontStringsMap ss:ds]     clSt = appClientDiff ds {clSt & svgClStrings = ss}
    appClientDiff` [SetImage str cbs:ds]         clSt = appClientDiff ds {clSt & svgClImageStr = Just str, svgClCallbacks = cbs }
    appClientDiff` [SetFontStringWidthMap mp:ds] clSt = appClientDiff ds {clSt & svgClTextWidths = mp }
    appClientDiff` [SetHasStUpd:ds]              clSt = appClientDiff ds {clSt & svgClHasStUpd = True}
    appClientDiff` [SetHasNoStUpd:ds]            clSt = appClientDiff ds {clSt & svgClHasStUpd = False}
    appClientDiff` [_:ds]                        clSt = appClientDiff ds clSt
    appClientDiff` _                             clSt = clSt

(`getElementsByClassName`) obj args :== obj .# "getElementsByClassName" .$ args
(`addEventListener`)       obj args :== obj .# "addEventListener"       .$ args
(`setAttribute`)           obj args :== obj .# "setAttribute"           .$ args
(`createElementNS`)        obj args :== obj .# "createElementNS"        .$ args
(`appendChild`)            obj args :== obj .# "appendChild"            .$ args
(`removeChild`)            obj args :== obj .# "removeChild"            .$ args
(`getComputedTextLength`)  obj args :== obj .# "getComputedTextLength"  .$ args

calcTextLengths :: !(Map FontDef (Set String)) *JSWorld -> *(Map FontDef (Map String Real), *JSWorld)
calcTextLengths fontdefs world
  # (svg, world)  = (jsDocument `createElementNS` (svgns, "svg")) world
  # (body, world) = .? (jsDocument .# "body") world
  # (_, world)    = (body `appendChild` svg) world
  # (elem, world) = (jsDocument `createElementNS` (svgns, "text")) world
  # (_, world)    = (svg `appendChild` elem) world
  # (res, world)  = 'DM'.foldrWithKey (f elem) ('DM'.newMap, world) fontdefs
  # (_, world)    = (svg `removeChild` elem) world
  # (_, world)    = (body `removeChild` svg) world
  = (res, world)
  where
  f elem fontdef strs (acc, world)
    # fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                    , ("font-size",    toString fontdef.fontysize)
                    , ("font-stretch", fontdef.fontstretch)
                    , ("font-style",   fontdef.fontstyle)
                    , ("font-variant", fontdef.fontvariant)
                    , ("font-weight",  fontdef.fontweight)
                    , ("x", "-10000")
                    , ("y", "-10000") ]
    # world       = foldr (\args world -> snd ((elem `setAttribute` args) world)) world fontAttrs
    # (ws, world) = 'DS'.fold (g elem) ('DM'.newMap, world) strs
    = ('DM'.put fontdef ws acc, world)
  g elem str (acc, world)
    # world        = (elem .# "textContent" .= str) world
    # (ctl, world) = (elem `getComputedTextLength` ()) world
    = ('DM'.put str (jsValToReal ctl) acc, world)

:: GenSVGSt m a :== State (GenSVGStVal m) a

:: FixSpansSt a :== State FixSpansStVal a

:: FixSpansStVal =
  { fixSpansTaggedSpanEnv :: Map (Set ImageTag) CachedSpan
  , fixSpansCounter       :: Int
  , fixSpansDidChange     :: Bool
  , fixSpansFonts         :: Map FontDef (Map String Real)
  }

class nextNo a :: a -> (Int, a)

instance nextNo (GenSVGStVal s) where
  nextNo st = (st.uniqueIdCounter, {st & uniqueIdCounter = st.uniqueIdCounter + 1})

instance nextNo FixSpansStVal where
  nextNo st = (st.fixSpansCounter, {st & fixSpansCounter = st.fixSpansCounter + 1})

:: State s a :== s -> *(a, s)

:: ErrorMessage :== String

:: FixSpansSyn s =
  { fixSpansSyn_ImageContent     :: ImageContent s
  , fixSpansSyn_TotalSpan        :: ImageSpan
  , fixSpansSyn_OffsetCorrection :: ImageOffset
  }

runM :: !(State s a) s -> (a, s)
runM m st = m st

sequence :: ![.st -> .(a, .st)] -> (.st -> .([a], .st))
sequence ms = mapSt id ms

addCachedSpan :: !(CachedSpan -> CachedSpan) !(Set ImageTag) !FixSpansStVal -> FixSpansStVal
addCachedSpan f imTas st=:{fixSpansTaggedSpanEnv}
  | 'DS'.null imTas = st
  | otherwise
    # r = fromMaybe { cachedGridSpans = Nothing, cachedImageSpan = Nothing } ('DM'.get imTas fixSpansTaggedSpanEnv)
    = { st & fixSpansTaggedSpanEnv = 'DM'.put imTas (f r) fixSpansTaggedSpanEnv }

cacheImageSpan :: !(Set ImageTag) !ImageSpan !FixSpansStVal -> FixSpansStVal
cacheImageSpan imTas sp st = addCachedSpan (\r -> {r & cachedImageSpan = Just sp}) imTas st

cacheGridSpans :: !(Set ImageTag) ![Span] ![Span] !FixSpansStVal -> FixSpansStVal
cacheGridSpans imTas xsps ysps st = addCachedSpan (\r -> {r & cachedGridSpans = Just ({x \\ x <- xsps}, {y \\ y <- ysps})}) imTas st

applyTransforms :: ![ImageTransform] !ImageSpan -> (ImageSpan, ImageOffset)
applyTransforms ts sp = foldr f (sp, (px 0.0, px 0.0)) ts
  where
  f (RotateImage th) (accSp, accOff)
    # (imSp, offs) = rotatedImageSpan th accSp
    = (imSp, accOff + offs)
  f (SkewXImage th) (accSp=:(_, ysp), (xoff, yoff))
    # (xsp, offs) = skewXImageWidth th accSp
    = ((xsp, ysp), (xoff + offs, yoff))
  f (SkewYImage th) (accSp=:(xsp, _), (xoff, yoff))
    # (ysp, offs) = skewYImageHeight th accSp
    = ((xsp, ysp), (xoff, yoff + offs))
  f (FitImage xsp ysp) (_, accOff)
    = ((xsp, ysp), accOff)
  f (FitXImage sp) ((xsp, ysp), accOff)
    = ((sp, (sp / xsp) * ysp), accOff)
  f (FitYImage sp) ((xsp, ysp), accOff)
    = (((sp / ysp) * xsp, sp), accOff)

instance /    Span where / (PxSpan 0.0) _             = PxSpan 0.0
                         / _            (PxSpan 0.0)  = PxSpan 0.0 // Division by zero should be undefined, but that would be impractical
                         / l            (PxSpan 1.0)  = l // Identity
                         / l            r             = DivSpan l r

instance *    Span where * (PxSpan 0.0)           _                      = PxSpan 0.0
                         * _                      (PxSpan 0.0)           = PxSpan 0.0
                         * (PxSpan 1.0)           r                      = r // Identity
                         * l                      (PxSpan 1.0)           = l // Identity
                         * (PxSpan a)             (PxSpan b)             = PxSpan (a * b)
                         * (PxSpan a)             (MulSpan (PxSpan b) c) = MulSpan (PxSpan (a * b)) c // Associativity
                         * (PxSpan a)             (MulSpan b (PxSpan c)) = MulSpan (PxSpan (a * c)) b // Associativity + commutativity
                         * (MulSpan a (PxSpan b)) (PxSpan c)             = MulSpan a (PxSpan (b * c)) // Associativity
                         * (MulSpan (PxSpan a) b) (PxSpan c)             = MulSpan b (PxSpan (a * c)) // Associativity + commutativity
                         * l                      r                      = MulSpan l r

// Rotates a rectangle by a given angle. Currently, this function is rather
// naive. It rotates the rectangle (usually the bounding box of a shape) around
// its centre point. It returns the span of the entire rotated image, i.e., the
// new bounding box of the rotated image. If you rotate a square by, e.g. 45
// degrees, then the resulting bounding box will be larger than the original
// square bounding box. If you rotate the square again by 45 degrees, you would
// expect that the bounding box after the second rotation is as big as the
// original square again. However, with this particular function, the
// resulting bounding box is bigger still, because the new bounding box was
// rotated.
//
// @param th | Angle th      angle          The angle of rotation
// @param (a, a) | IsSpan a  (xspan, yspan) The original x and y spans of the
//                                          non-rotated image
// @return ((a, a), (a, a)) | IsSpan a      The span of the rotated image and
//                                          the offset from between the old and
//                                          new top-left corner of the bounding
//                                          box
rotatedImageSpan :: !th !(a, a) -> ((a, a), (a, a)) | Angle th & IsSpan a
rotatedImageSpan angle (xspan, yspan)
  = ( (abs (maxAllX - minAllX), abs (maxAllY - minAllY))
    , (zero - minAllX, zero - minAllY))
  where
  cx        = xspan /. 2.0
  cy        = yspan /. 2.0
  allPoints = [ mkTransform zero  zero
              , mkTransform xspan zero
              , mkTransform zero  yspan
              , mkTransform xspan yspan ]
  allX      = map fst allPoints
  maxAllX   = maxOf allX
  minAllX   = minOf allX
  allY      = map snd allPoints
  maxAllY   = maxOf allY
  minAllY   = minOf allY
  angle`    = toReal (toRad angle)
  mkTransform x y = ( cx + (x - cx) *. cos angle` + (y - cy) *. sin angle`
                    , cy - (x - cx) *. sin angle` + (y - cy) *. cos angle`)

// Skew an image by a given angle. This function is naive as well, for the same
// reasons as the rotation function.
// TODO : We need to calculate the difference between the original and skewed
// top-left coordinate here as well, because we need it in grid layouts
//
// @param (th | Angle th)     angle          The skew angle
// @param ((a, a) | IsSpan a) (xspan, yspan) The original x and y spans of the
//                                           non-skewed image
// @return ((a, a) | IsSpan a) The new width of the skewed image and possible offset
skewXImageWidth :: !th !(a, a) -> (a, a) | Angle th & IsSpan a
skewXImageWidth angle (xspan, yspan) = (newXSpan, mkOffset)
  where
  rAngle   = toReal (toRad angle)
  newXSpan = xspan + (abs (yspan *. tan rAngle))
  spanDiff = newXSpan - xspan
  mkOffset
    | rAngle <= 0.0 = zero - spanDiff
    | otherwise     = spanDiff

// Skew an image by a given angle. This function is naive as well, for the same
// reasons as the rotation function.
// TODO : We need to calculate the difference between the original and skewed
// top-left coordinate here as well, because we need it in grid layouts
//
// @param (th | Angle th)     angle          The skew angle
// @param ((a, a) | IsSpan a) (xspan, yspan) The original x and y spans of the
//                                           non-skewed image
// @return ((a, a) | IsSpan a) The new height of the skewed image and possible offset
skewYImageHeight :: !th !(a, a) -> (a, a) | Angle th & IsSpan a
skewYImageHeight angle (xspan, yspan) = (newYSpan, mkOffset)
  where
  rAngle   = toReal (toRad angle)
  newYSpan = yspan + (abs (xspan *. tan rAngle))
  spanDiff = newYSpan - xspan
  mkOffset
    | rAngle <= 0.0 = zero - spanDiff
    | otherwise     = spanDiff


gatherFonts :: !(Image s) -> Map FontDef (Set String)
gatherFonts img = imageCata gatherFontsAllAlgs img
  where
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
  gatherFontsImageAlgs =
    { imageAlg = \imCo mask imAts imTrs _ _ (m1, m2, m3, m4) _ -> 'DM'.unionsWith 'DS'.union [imCo : m1 : m2 : m3 : m4 : maybeToList mask ++ imAts ++ imTrs]
    }
  gatherFontsImageContentAlgs =
    { imageContentBasicAlg     = id
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
  gatherFontsImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \_ -> 'DM'.newMap
    , imageAttrStrokeWidthAttrAlg   = \{strokewidth} -> gatherFontsspan strokewidth
    , imageAttrXRadiusAttrAlg       = \{xradius} -> gatherFontsspan xradius
    , imageAttrYRadiusAttrAlg       = \{yradius} -> gatherFontsspan yradius
    , imageAttrStrokeOpacityAttrAlg = \_ -> 'DM'.newMap
    , imageAttrFillAttrAlg          = \_ -> 'DM'.newMap
    , imageAttrFillOpacityAttrAlg   = \_ -> 'DM'.newMap
    , imageAttrOnClickAttrAlg       = \_ -> 'DM'.newMap
    , imageAttrDashAttr             = \_ -> 'DM'.newMap
    }
  gatherFontsImageTransformAlgs =
    { imageTransformRotateImageAlg = \_   -> 'DM'.newMap
    , imageTransformSkewXImageAlg  = \_   -> 'DM'.newMap
    , imageTransformSkewYImageAlg  = \_   -> 'DM'.newMap
    , imageTransformFitImageAlg    = \x y -> 'DM'.unionsWith 'DS'.union [x, y]
    , imageTransformFitXImageAlg   = id
    , imageTransformFitYImageAlg   = id
    }
  gatherFontsImageSpanAlgs =
    { imageSpanAlg = \x y -> 'DM'.unionsWith 'DS'.union [x, y]
    }
  gatherFontsBasicImageAlgs =
    { basicImageTextImageAlg    = \_ _ sp -> sp
    , basicImageEmptyImageAlg   = id
    , basicImageCircleImageAlg  = id
    , basicImageRectImageAlg    = id
    , basicImageEllipseImageAlg = id
    }

  gatherFontsLineImageAlgs =
    { lineImageLineImageAlg = \sp ms liCo -> 'DM'.unionsWith 'DS'.union [liCo:sp:maybeToList ms]
    }
  gatherFontsMarkersAlgs =
    { markersMarkersAlg = \m1 m2 m3 -> 'DM'.unionsWith 'DS'.union (concatMap maybeToList [m1, m2, m3])
    }
  gatherFontsLineContentAlgs =
    { lineContentSimpleLineImageAlg = \_      -> 'DM'.newMap
    , lineContentPolygonImageAlg    = \coords -> 'DM'.unionsWith 'DS'.union (map fst coords ++ map snd coords) // TODO refactor
    , lineContentPolylineImageAlg   = \coords -> 'DM'.unionsWith 'DS'.union (map fst coords ++ map snd coords) // TODO refactor
    }
  gatherFontsCompositeImageAlgs =
    { compositeImageAlg = \offsets host compose -> 'DM'.unionsWith 'DS'.union [compose : map fst offsets ++ map snd offsets ++ maybeToList host] // TODO refactor
    }
  gatherFontsComposeAlgs =
    { composeAsGridAlg    = \_ _ imgss -> 'DM'.unionsWith 'DS'.union (flatten imgss)
    , composeAsCollageAlg = \    imgs  -> 'DM'.unionsWith 'DS'.union imgs
    , composeAsOverlayAlg = \  _ imgs  -> 'DM'.unionsWith 'DS'.union imgs
    }
gatherFontsSpanAlgs =
  { spanPxSpanAlg     = \_   -> 'DM'.newMap
  , spanLookupSpanAlg = id
  , spanAddSpanAlg    = \x y -> 'DM'.unionsWith 'DS'.union [x, y]
  , spanSubSpanAlg    = \x y -> 'DM'.unionsWith 'DS'.union [x, y]
  , spanMulSpanAlg    = \x _ -> x
  , spanDivSpanAlg    = \x _ -> x
  , spanAbsSpanAlg    = id
  , spanMinSpanAlg    = \xs  -> 'DM'.unionsWith 'DS'.union xs
  , spanMaxSpanAlg    = \xs  -> 'DM'.unionsWith 'DS'.union xs
  }
gatherFontsLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \_ _ -> 'DM'.newMap
  , lookupSpanRowYSpanAlg     = \_ _ -> 'DM'.newMap
  , lookupSpanImageXSpanAlg   = \_   -> 'DM'.newMap
  , lookupSpanImageYSpanAlg   = \_   -> 'DM'.newMap
  , lookupSpanTextXSpanAlg    = \fd str -> 'DM'.put fd ('DS'.singleton str) 'DM'.newMap
  }

gatherFontsspan :: Span -> Map FontDef (Set String)
gatherFontsspan sp = spanCata gatherFontsSpanAlgs gatherFontsLookupSpanAlgs sp

fixSpans :: !(Image s) -> FixSpansSt (Image s) | iTask s
fixSpans img = go
  where
  go st
    # (img`, st)      = imageCata fixSpansAllAlgs img st
    | st.fixSpansDidChange = fixSpans img` {st & fixSpansDidChange = False}
    | otherwise       = ret img` st
  fixSpansAllAlgs =
    { imageAlgs          = fixSpansImageAlgs
    , imageContentAlgs   = fixSpansImageContentAlgs
    , imageAttrAlgs      = fixSpansImageAttrAlgs
    , imageTransformAlgs = fixSpansImageTransformAlgs
    , imageSpanAlgs      = fixSpansImageSpanAlgs
    , basicImageAlgs     = fixSpansBasicImageAlgs
    , lineImageAlgs      = fixSpansLineImageAlgs
    , markersAlgs        = fixSpansMarkersAlgs
    , lineContentAlgs    = fixSpansLineContentAlgs
    , compositeImageAlgs = fixSpansCompositeImageAlgs
    , composeAlgs        = fixSpansComposeAlgs
    , spanAlgs           = fixSpansSpanAlgs
    , lookupSpanAlgs     = fixSpansLookupSpanAlgs
    }
  fixSpansImageAlgs :: ImageAlg ([ImageTransform] (Set ImageTag) -> (FixSpansSt (FixSpansSyn s)))
                                (FixSpansSt (ImageAttr s))
                                (FixSpansSt ImageTransform)
                                (FixSpansSt Span)
                                (FixSpansSt (Image s)) | iTask s
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: !([ImageTransform] (Set ImageTag) -> (FixSpansSt (FixSpansSyn s)))
               !(Maybe (FixSpansSt (Image s))) ![FixSpansSt (ImageAttr s)] ![FixSpansSt ImageTransform]
               !(Set ImageTag) !(FixSpansSt Span, FixSpansSt Span)
               !(FixSpansSt Span, FixSpansSt Span, FixSpansSt Span, FixSpansSt Span)
               !(FixSpansSt Span, FixSpansSt Span)
            -> FixSpansSt (Image s) | iTask s
    mkImage imCo mask imAts imTrs imTas _ (m1, m2, m3, m4) _ = go
      where
      go st
        # (mask, st)       = evalMaybe mask st
        # (imAts, st)      = sequence imAts st
        # (imTrs, st)      = sequence imTrs st
        # (fixSpansSyn, st) = imCo imTrs imTas st
        # (m1, st)         = m1 st
        # (m2, st)         = m2 st
        # (m3, st)         = m3 st
        # (m4, st)         = m4 st
        # (xsp, ysp)       = fixSpansSyn.fixSpansSyn_TotalSpan
        # (xsp, ysp)       = (xsp + m2 + m4, ysp + m1 + m3)
        # st               = cacheImageSpan imTas (xsp, ysp) st
        # (no, st)         = nextNo st
        = ret (tag [ImageTagSystem no]
              { Image
              | content             = fixSpansSyn.fixSpansSyn_ImageContent
              , mask                = mask
              , attribs             = 'DS'.fromList imAts
              , transform           = imTrs
              , tags                = imTas
              , totalSpan           = (xsp, ysp)
              , margin              = (m1, m2, m3, m4)
              , transformCorrection = fixSpansSyn.fixSpansSyn_OffsetCorrection
              })
              st

  fixSpansImageContentAlgs :: ImageContentAlg (ImageSpan [ImageTransform] -> FixSpansSt (FixSpansSyn s))
                                              (FixSpansSt ImageSpan)
                                              ([ImageTransform] (Set ImageTag) -> FixSpansSt (FixSpansSyn s))
                                              ([ImageTransform] (Set ImageTag) -> FixSpansSt (FixSpansSyn s))
                                              ([ImageTransform] (Set ImageTag) -> FixSpansSt (FixSpansSyn s)) | iTask s
  fixSpansImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpan [ImageTransform] -> FixSpansSt (FixSpansSyn s))
               !(FixSpansSt ImageSpan) ![ImageTransform] !(Set ImageTag)
            -> FixSpansSt (FixSpansSyn s) | iTask s
    mkBasic baIm imSp imTrs imTas
      =        imSp `b`
      \imSp -> baIm imSp imTrs
  fixSpansImageAttrAlgs :: ImageAttrAlg s (FixSpansSt (ImageAttr s)) | iTask s
  fixSpansImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = ret o ImageStrokeAttr
    , imageAttrStrokeWidthAttrAlg   = ret o ImageStrokeWidthAttr
    , imageAttrXRadiusAttrAlg       = ret o ImageXRadiusAttr
    , imageAttrYRadiusAttrAlg       = ret o ImageYRadiusAttr
    , imageAttrStrokeOpacityAttrAlg = ret o ImageStrokeOpacityAttr
    , imageAttrFillAttrAlg          = ret o ImageFillAttr
    , imageAttrFillOpacityAttrAlg   = ret o ImageFillOpacityAttr
    , imageAttrOnClickAttrAlg       = ret o ImageOnClickAttr
    , imageAttrDashAttr             = ret o ImageDashAttr
    }
  fixSpansImageTransformAlgs :: ImageTransformAlg Deg (FixSpansSt Span) (FixSpansSt ImageTransform)
  fixSpansImageTransformAlgs =
    { imageTransformRotateImageAlg = ret o RotateImage
    , imageTransformSkewXImageAlg  = ret o SkewXImage
    , imageTransformSkewYImageAlg  = ret o SkewYImage
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitDim FitXImage
    , imageTransformFitYImageAlg   = mkFitDim FitYImage
    }
    where
    mkFitImage :: !(FixSpansSt Span) !(FixSpansSt Span) -> FixSpansSt ImageTransform
    mkFitImage sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (FitImage sp1 sp2)
    mkFitDim :: !(Span -> ImageTransform) !(FixSpansSt Span) -> FixSpansSt ImageTransform
    mkFitDim ctr sp
      =      sp `b`
      \sp -> ret (ctr sp)
  fixSpansImageSpanAlgs :: ImageSpanAlg (FixSpansSt Span) (FixSpansSt ImageSpan)
  fixSpansImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: !(FixSpansSt Span) !(FixSpansSt Span) -> FixSpansSt ImageSpan
    mkSpan xspan yspan
      =         xspan `b`
      \xspan -> yspan `b`
      \yspan -> ret (xspan, yspan)
  fixSpansBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageTransform] -> FixSpansSt (FixSpansSyn s)) | iTask s
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg   = mkEmptyImage
    , basicImageTextImageAlg    = mkTextImage
    , basicImageCircleImageAlg  = mkCircleImage
    , basicImageRectImageAlg    = mkRectImage
    , basicImageEllipseImageAlg = mkEllipseImage
    }
    where
    mkEmptyImage       imSp imTrs = mkSpan EmptyImage         imSp imTrs
    mkRectImage        imSp imTrs = mkSpan RectImage          imSp imTrs
    mkTextImage fd str imSp imTrs = mkSpan (TextImage fd str) imSp imTrs
    mkCircleImage      imSp imTrs = mkSpan CircleImage        imSp imTrs
    mkEllipseImage     imSp imTrs = mkSpan EllipseImage       imSp imTrs

    mkSpan :: !BasicImage !ImageSpan ![ImageTransform]
           -> FixSpansSt (FixSpansSyn s) | iTask s
    mkSpan ctor imSp imTrs
      # (imSp`, imOff) = applyTransforms imTrs imSp
      = ret { fixSpansSyn_ImageContent     = Basic ctor imSp
            , fixSpansSyn_TotalSpan        = imSp`
            , fixSpansSyn_OffsetCorrection = imOff }

  fixSpansLineImageAlgs :: LineImageAlg (FixSpansSt ImageSpan)
                                        (FixSpansSt (Markers s))
                                        (FixSpansSt LineContent)
                                        ([ImageTransform] (Set ImageTag) -> FixSpansSt (FixSpansSyn s)) | iTask s
  fixSpansLineImageAlgs =
    { lineImageLineImageAlg = mkLine
    }
    where
    mkLine :: !(FixSpansSt ImageSpan) !(Maybe (FixSpansSt (Markers s))) !(FixSpansSt LineContent)
              ![ImageTransform] !(Set ImageTag)
           -> FixSpansSt (FixSpansSyn s) | iTask s
    mkLine imSp mmarkers liCo imTrs imTas
      =            imSp `b`
      \imSp     -> evalMaybe mmarkers `b`
      \mmarkers -> liCo `b`
      \liCo     -> let (imSp`, imOff) = applyTransforms imTrs imSp
                   in  ret { fixSpansSyn_ImageContent     = Line { LineImage
                                                                 | lineSpan    = imSp
                                                                 , markers     = mmarkers
                                                                 , lineContent = liCo }
                           , fixSpansSyn_TotalSpan        = imSp`
                           , fixSpansSyn_OffsetCorrection = imOff }
  fixSpansMarkersAlgs :: MarkersAlg (FixSpansSt (Image s)) (FixSpansSt (Markers s)) | iTask s
  fixSpansMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (FixSpansSt (Image s)))
                 !(Maybe (FixSpansSt (Image s)))
                 !(Maybe (FixSpansSt (Image s)))
              -> FixSpansSt (Markers s) | iTask s
    mkMarkers mStart mMid mEnd
      =          evalMaybe mStart `b`
      \mStart -> evalMaybe mMid `b`
      \mMid   -> evalMaybe mEnd `b`
      \mEnd   -> ret { markerStart = mStart
                     , markerMid   = mMid
                     , markerEnd   = mEnd }
  fixSpansLineContentAlgs :: LineContentAlg (FixSpansSt Span) (FixSpansSt LineContent)
  fixSpansLineContentAlgs =
    { lineContentSimpleLineImageAlg = \sl     -> ret (SimpleLineImage sl)
    , lineContentPolygonImageAlg    = \coords -> evalOffsets coords `b`
                                      \coords -> ret (PolygonImage coords)
    , lineContentPolylineImageAlg   = \coords -> evalOffsets coords `b`
                                      \coords -> ret (PolylineImage coords)
    }
  fixSpansCompositeImageAlgs :: CompositeImageAlg (FixSpansSt Span)
                                                  (FixSpansSt (Image s))
                                                  ([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> FixSpansSt (Compose s, ImageSpan, [ImageOffset]))
                                                  ([ImageTransform] (Set ImageTag) -> FixSpansSt (FixSpansSyn s)) | iTask s
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: ![(FixSpansSt Span, FixSpansSt Span)]
                        !(Maybe (FixSpansSt (Image s)))
                        !([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> FixSpansSt (Compose s, ImageSpan, [ImageOffset]))
                        ![ImageTransform] !(Set ImageTag)
                     -> FixSpansSt (FixSpansSyn s) | iTask s
    mkCompositeImage offsets host compose imTrs imTas = go
      where
      go st
        # (offsets, st) = evalOffsets offsets st
        # (host, st)    = evalMaybe host st
        # ((compose, composeSpan, offsets), st) = compose offsets host imTrs imTas st
        # (host, span) = case host of
                           Just hostImg
                              -> (Just hostImg, hostImg.totalSpan)
                           _  -> (Nothing, composeSpan)
        # (span, corr)  = applyTransforms imTrs span
        = ret { fixSpansSyn_ImageContent     = Composite { CompositeImage
                                                         | offsets = offsets
                                                         , host    = host
                                                         , compose = compose
                                                         }
              , fixSpansSyn_TotalSpan        = span
              , fixSpansSyn_OffsetCorrection = corr } st
  fixSpansComposeAlgs :: ComposeAlg (FixSpansSt (Image s))
                                    ([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> FixSpansSt (Compose s, ImageSpan, [ImageOffset])) | iTask s
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![ImageAlign] ![[FixSpansSt (Image s)]]
              ![ImageOffset] !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
           -> FixSpansSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
    mkGrid (numcols, numrows) ias imgss offsets mbhost imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr seqImgsGrid ([], st) imgss
        # imgss       = reverse (map reverse imgss) // TODO This is more or less a hack... why do we need this?
        # (tag, st)   = nextNo st
        # sysTags     = 'DS'.singleton (ImageTagSystem tag)
        # gridSpan    = maybe ( foldr (\n acc -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) [0..numcols - 1]
                              , foldr (\n acc -> LookupSpan (RowYSpan sysTags n) + acc) (px 0.0) [0..numrows - 1]
                              )
                              (\x -> x.totalSpan) mbhost
        # offsets     = flatten (calculateGridOffsets (map (\n -> LookupSpan (ColumnXSpan sysTags n)) [0..numcols - 1])
                                                      (map (\n -> LookupSpan (RowYSpan sysTags n)) [0..numrows - 1]) imgss offsets)
        # spanss      = map (map (\x -> x.totalSpan)) imgss
        # st          = cacheGridSpans ('DS'.union sysTags imTas) (map (maxSpan o map fst) (transpose spanss)) (map (maxSpan o map snd) spanss) st
        = ret ( AsCollage (flatten imgss)
              , gridSpan
              , offsets) { st & fixSpansDidChange = True }
      calculateGridOffsets cellXSpans cellYSpans imgss offsets
        = let (x, _, _, _) = foldr (mkRows cellXSpans) ([], px 0.0, ias, offsets) (zip2 imgss cellYSpans)
          in  x
        where
        mkRows cellXSpans (imgs, cellYSpan) (acc, yoff, aligns, offsets)
          # imgsLength = length imgs
          = ( [fst (foldr (mkCols cellYSpan yoff) ([], px 0.0) (zip4 imgs cellXSpans (take imgsLength aligns) (take imgsLength offsets))) : acc]
            , yoff + cellYSpan, drop imgsLength aligns, drop imgsLength offsets)
        mkCols cellYSpan yoff (img=:{totalSpan, transformCorrection = (tfXCorr, tfYCorr)}, cellXSpan, align, (manXOff, manYOff)) (acc, xoff)
          # (alignXOff, alignYOff) = calcAlignOffset cellXSpan cellYSpan totalSpan align
          = ([( alignXOff + xoff + manXOff + tfXCorr
              , alignYOff + yoff + manYOff + tfYCorr):acc], xoff + cellXSpan)

    mkCollage :: ![FixSpansSt (Image s)] ![ImageOffset] !(Maybe (Image s))
                 ![ImageTransform] !(Set ImageTag)
              -> FixSpansSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
    mkCollage imgs offsets mbhost imTrs imTas
      =        sequence imgs `b`
      \imgs -> ret ( AsCollage imgs
                   , maybe (calculateComposedSpan (map (\x -> x.totalSpan) imgs) offsets) (\x -> x.totalSpan) mbhost
                   , offsets)
    mkOverlay :: ![ImageAlign] ![FixSpansSt (Image s)] ![ImageOffset]
                 !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
              -> FixSpansSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
    mkOverlay ias imgs offsets mbhost imTrs imTas = go
      where
      go st
        # (imgs, st) = sequence imgs st
        # spans      = map (\x -> x.totalSpan) imgs
        # (maxXSpan, maxYSpan) = maybe (maxSpan (map fst spans), maxSpan (map snd spans))
                                       (\x -> x.totalSpan) mbhost
        # offsets    = zipWith3 addOffset (zipWith (calcAlignOffset maxXSpan maxYSpan) spans ias)
                                          offsets
                                          imgs
        = ret ( AsCollage imgs
              , maybe (calculateComposedSpan spans offsets) (\x -> x.totalSpan) mbhost
              , offsets ) { st & fixSpansDidChange = True }
        where
        addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)

  fixSpansSpanAlgs :: SpanAlg (FixSpansSt Span) (FixSpansSt Span)
  fixSpansSpanAlgs =
    { spanPxSpanAlg     = \r   -> ret (PxSpan r)
    , spanLookupSpanAlg = ($)
    , spanAddSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x + y)
    , spanSubSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x - y)
    , spanMulSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x * y)
    , spanDivSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x / y)
    , spanAbsSpanAlg    = \x   -> x `b` \x ->             ret (abs x)
    , spanMinSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (minSpan xs)
    , spanMaxSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (maxSpan xs)
    }

  fixSpansLookupSpanAlgs :: LookupSpanAlg (FixSpansSt Span)
  fixSpansLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg = mkImageGridSpan (\xss n -> (fst xss).[n]) ColumnXSpan
    , lookupSpanRowYSpanAlg    = mkImageGridSpan (\xss n -> (snd xss).[n]) RowYSpan
    , lookupSpanImageXSpanAlg  = mkImageSpan fst ImageXSpan
    , lookupSpanImageYSpanAlg  = mkImageSpan snd ImageYSpan
    , lookupSpanTextXSpanAlg   = mkTextLU
    }
    where
    mkTextLU fd str st
      # sw = case 'DM'.get fd st.fixSpansFonts of
               Just fs -> case 'DM'.get str fs of
                            Just sw -> sw
                            _       -> 0.0
               _       -> 0.0
      = ret (PxSpan sw) st

    lookupTags :: !(Set ImageTag) -> FixSpansSt (Maybe CachedSpan)
    lookupTags ts
      | 'DS'.null ts = ret Nothing
      | otherwise    = go
      where
      go st
        = case 'DM'.elems ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) st.fixSpansTaggedSpanEnv) of
            [x:_] -> (Just x, {st & fixSpansDidChange = True})
            _     -> (Nothing, st)

    mkImageSpan :: !(ImageSpan -> Span) !((Set ImageTag) -> LookupSpan) !(Set ImageTag)
                -> FixSpansSt Span
    mkImageSpan f c ts
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just {cachedImageSpan = Just xs} -> f xs
                      _                                -> LookupSpan (c ts))

    mkImageGridSpan :: !(({Span}, {Span}) Int -> Span) !((Set ImageTag) Int -> LookupSpan)
                       !(Set ImageTag) Int
                    -> FixSpansSt Span
    mkImageGridSpan f c ts n
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just csp=:{cachedGridSpans = Just xss} -> f xss n
                      _                                      -> LookupSpan (c ts n))

seqImgsGrid imgs (acc, st) :== (sequence imgs `b` \imgs -> ret [imgs:acc]) st

:: ImageSpanReal :== (Real, Real)

:: ImageOffsetReal :== (Real, Real)

:: GenSVGSyn s =
  { genSVGSyn_svgElts       :: [SVGElt]
  , genSVGSyn_imageSpanReal :: ImageSpanReal
  , genSVGSyn_onclicks      :: Map String (s -> s)
  }

mkGenSVGSyn = { genSVGSyn_svgElts       = []
              , genSVGSyn_imageSpanReal = (0.0, 0.0)
              , genSVGSyn_onclicks      = 'DM'.newMap
              }


editletId = "__INTERNAL_editletId_PLACEHOLDER__"

mkMaskId :: !String !Int -> String
mkMaskId editletId uniqId = "maskId-" +++ editletId +++ toString uniqId

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkOnClickId :: !String !Int -> String
mkOnClickId editletId uniqId = "onClickId-" +++ editletId +++ toString uniqId

getSvgAttrs :: ![(Maybe HtmlAttr, Maybe SVGAttr)] -> [SVGAttr]
getSvgAttrs as = [a \\ (_, Just a) <- as]

getHtmlAttrs :: ![(Maybe HtmlAttr, Maybe SVGAttr)] -> [HtmlAttr]
getHtmlAttrs as = [a \\ (Just a, _) <- as]

mkUrl :: String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))]

to2dec :: Real -> Real
to2dec n = toReal (toInt (n * 100.0)) / 100.0

genSVG :: !(Image s) -> GenSVGSt s (GenSVGSyn s) | iTask s
genSVG img = imageCata genSVGAllAlgs img
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
  genSVGImageAlgs :: ImageAlg (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                             (GenSVGSt s ((Maybe HtmlAttr, Maybe SVGAttr), Map String (s -> s)))
                             (ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform))
                             (GenSVGSt s Real)
                             (GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage :: !(ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
               !(Maybe (GenSVGSt s (GenSVGSyn s))) ![GenSVGSt s ((Maybe HtmlAttr, Maybe SVGAttr), Map String (s -> s))]
               ![ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)]
               !(Set ImageTag) !(GenSVGSt s Real, GenSVGSt s Real)
               !(GenSVGSt s Real, GenSVGSt s Real, GenSVGSt s Real, GenSVGSt s Real)
               !(GenSVGSt s Real, GenSVGSt s Real)
            -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkImage imCo mask imAts imTrs imTas (txsp, tysp) (m1, m2, _, _) _
      =          sequence imAts `b`
      \imAts  -> txsp `b`
      \txsp   -> tysp `b`
      \tysp   -> m1 `b`
      \m1     -> m2 `b`
      \m2     -> withSt imageMaskId `b`
      \maskId -> let imAts` = map fst imAts
                 in imCo (txsp, tysp) (maybe imAts` (const [(Nothing, Just (MaskAttr (mkUrl maskId))) : imAts`]) mask) imTrs imTas `b`
      \syn    -> evalMaybe mask `b`
      \mask   -> ret { mkGenSVGSyn
                     & genSVGSyn_svgElts       = mkGroup [] (mkTransformTranslateAttr (to2dec m1, to2dec m2)) (mkElt maskId mask syn)
                     , genSVGSyn_imageSpanReal = (txsp, tysp)
                     , genSVGSyn_onclicks      = 'DM'.unions [syn.genSVGSyn_onclicks : map snd imAts]
                     }
    imageMaskId clval
      # (uid, clval) = nextNo clval
      # maskId       = mkMaskId editletId uid
      = (maskId, clval)

    mkElt _      Nothing     syn = syn.genSVGSyn_svgElts
    mkElt maskId (Just mask) syn
      = [ DefsElt [] [] [MaskElt [IdAttr maskId] [] mask.genSVGSyn_svgElts]
        : syn.genSVGSyn_svgElts]

  genSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [(SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                           (GenSVGSt s ImageSpanReal)
                                           (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                           (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                           (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [(SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
               !(GenSVGSt s ImageSpanReal) !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)]
               ![ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
            -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkBasic baIm imSp totalSpan imAts imTrs imTas
      =         imSp `b`
      \imSp  -> sequence (map (\f -> f imSp) imTrs) `b`
      \imTrs -> baIm imSp imAts imTrs imTas
  genSVGImageAttrAlgs :: ImageAttrAlg s (GenSVGSt s ((Maybe HtmlAttr, Maybe SVGAttr), Map String (s -> s))) | iTask s
  genSVGImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr -> ret ((Nothing, Just (StrokeAttr (PaintColor attr.stroke Nothing))), 'DM'.newMap)
    , imageAttrStrokeWidthAttrAlg   = mkStrokeWidth
    , imageAttrXRadiusAttrAlg       = \attr -> evalSpan attr.xradius `b` \r -> ret ((Nothing, Just (RxAttr (toString r, PX))), 'DM'.newMap)
    , imageAttrYRadiusAttrAlg       = \attr -> evalSpan attr.yradius `b` \r -> ret ((Nothing, Just (RyAttr (toString r, PX))), 'DM'.newMap)
    , imageAttrStrokeOpacityAttrAlg = \attr -> ret ((Nothing, Just (StrokeOpacityAttr (toString attr.opacity))), 'DM'.newMap)
    , imageAttrFillAttrAlg          = \attr -> ret ((Nothing, Just (FillAttr (PaintColor attr.fill Nothing))), 'DM'.newMap)
    , imageAttrFillOpacityAttrAlg   = \attr -> ret ((Nothing, Just (FillOpacityAttr (FillOpacity (toString attr.opacity)))), 'DM'.newMap)
    , imageAttrOnClickAttrAlg       = mkOnClick
    , imageAttrDashAttr             = \attr -> ret ((Nothing, Just (StrokeDashArrayAttr (DashArray (map toString attr.dash)))), 'DM'.newMap)
    }
    where
    mkStrokeWidth :: !(StrokeWidthAttr s) -> GenSVGSt s ((Maybe HtmlAttr, Maybe SVGAttr), Map String (s -> s)) | iTask s
    mkStrokeWidth {strokewidth}
      =     evalSpan strokewidth `b`
      \w -> ret ((Nothing, Just (StrokeWidthAttr (StrokeWidthLength (toString w, PX)))), 'DM'.newMap)
    mkOnClick :: !(OnClickAttr s) -> GenSVGSt s ((Maybe HtmlAttr, Maybe SVGAttr), Map String (s -> s)) | iTask s
    mkOnClick {onclick} = go
      where
      go clval
        # (uniqId, clval) = nextNo clval
        # ocId            = mkOnClickId editletId uniqId
        = ret ((Just (ClassAttr ocId), Nothing), 'DM'.singleton ocId onclick) clval
  genSVGImageTransformAlgs :: ImageTransformAlg Deg (GenSVGSt s Real) (ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)) | iTask s
  genSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    (xsp, ysp) -> ret (RotateTransform (toString (toReal imAn)) (Just (toString (xsp / 2.0), toString (ysp / 2.0))), RotateImage imAn)
    , imageTransformSkewXImageAlg  = \imAn    (xsp, _)   -> ret (SkewXTransform (toString (toReal imAn)), SkewXImage imAn)
    , imageTransformSkewYImageAlg  = \imAn    (_, ysp)   -> ret (SkewYTransform (toString (toReal imAn)), SkewYImage imAn)
    , imageTransformFitImageAlg    = \sp1 sp2 (xsp, ysp) -> sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (ScaleTransform (toString (sp1 / xsp)) (toString (sp2 / ysp)), FitImage (px sp1) (px sp2))
    , imageTransformFitXImageAlg   = \sp      (xsp, _)   -> sp  `b` \sp  -> let scale
                                                                                  | xsp > 0.0 = toString (sp / xsp)
                                                                                  | otherwise = "1.0"
                                                                            in  ret (ScaleTransform scale scale, FitXImage (px sp))
    , imageTransformFitYImageAlg   = \sp      (_, ysp)   -> sp  `b` \sp  -> let scale
                                                                                  | ysp > 0.0 = toString (sp / ysp)
                                                                                  | otherwise = "1.0"
                                                                            in  ret (ScaleTransform scale scale, FitYImage (px sp))
    }
  genSVGImageSpanAlgs :: ImageSpanAlg (GenSVGSt s Real) (GenSVGSt s ImageSpanReal) | iTask s
  genSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan :: !(GenSVGSt s Real) !(GenSVGSt s Real) -> GenSVGSt s ImageSpanReal
    mkImageSpan sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (sp1, sp2)
  genSVGBasicImageAlgs :: BasicImageAlg (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [(SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGBasicImageAlgs =
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage :: !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                 -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkEmptyImage imSp imAts imTrs imTas
      = ret { mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (getHtmlAttrs imAts) [] (mkGroup (mkWH imSp) (getSvgAttrs (mkAttrs imAts imTrs)) []) }
    mkTextImage :: !FontDef !String !ImageSpanReal  ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkTextImage fd str imSp imAts imTrs imTas
      = ret { mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (getHtmlAttrs imAts) [] [TextElt [] (getSvgAttrs (mkAttrs imAts imTrs) ++ fontAttrs fd.fontysize) str] }
      where
      fontAttrs fsz = [ AlignmentBaselineAttr "text-before-edge"
                      , DominantBaselineAttr "text-before-edge"
                      , FontFamilyAttr fd.fontfamily
                      , FontSizeAttr (toString fsz)
                      , FontStyleAttr fd.fontstyle
                      , FontStretchAttr fd.fontstretch
                      , FontVariantAttr fd.fontvariant
                      , FontWeightAttr fd.fontweight
                      ]
    mkRectImage :: !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkRectImage imSp imAts imTrs imTas
      = ret { mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (getHtmlAttrs imAts) [] [RectElt (mkWH imSp) (getSvgAttrs (mkAttrs imAts imTrs))] }
    mkCircleImage :: !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                  -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkCircleImage imSp=:(imXSp`, _) imAts imTrs imTas
      # r = imXSp` / 2.0
      = ret { mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (getHtmlAttrs imAts) [] [CircleElt []
                                          [ RAttr (toString (to2dec r), PX), CxAttr (toString (to2dec r), PX)
                                          , CyAttr (toString (to2dec r), PX) : (getSvgAttrs (mkAttrs imAts imTrs)) ]] }
    mkEllipseImage :: !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                   -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkEllipseImage imSp=:(imXSp, imYSp) imAts imTrs imTas
      = ret { mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (getHtmlAttrs imAts) [] [EllipseElt [] (getSvgAttrs (mkAttrs imAts imTrs) ++
                                          [ RxAttr (toString (to2dec (imXSp / 2.0)), PX), RyAttr (toString (to2dec (imYSp / 2.0)), PX)
                                          , CxAttr (toString (to2dec (imXSp / 2.0)), PX), CyAttr (toString (to2dec (imYSp / 2.0)), PX)])] }

  // TODO Type signature
  genSVGLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage lineSpan mmarkers lineContent totalSpan imAts imTrs imTas
      =            lineSpan `b`
      \lineSpan -> evalMaybe mmarkers `b`
      \mmarkers -> sequence (map (\f -> f lineSpan) imTrs) `b`
      \imTrs    -> lineContent lineSpan mmarkers imAts imTrs imTas

  genSVGMarkersAlgs :: MarkersAlg (GenSVGSt s (GenSVGSyn s)) (GenSVGSt s (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s))) | iTask s
  genSVGMarkersAlgs =
    { markersMarkersAlg = \m1 m2 m3 -> evalMaybe m1 `b`
                          \m1 ->       evalMaybe m2 `b`
                          \m2 ->       evalMaybe m3 `b`
                          \m3 ->       ret (m1, m2, m3)
    }
  genSVGLineContentAlgs :: LineContentAlg (GenSVGSt s Real)
                                         (ImageSpanReal (Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s))) [(Maybe HtmlAttr, Maybe SVGAttr)] [(SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkLineImage
    , lineContentPolygonImageAlg    = mkPolygonImage
    , lineContentPolylineImageAlg   = mkPolylineImage
    }
    where
    mkLineImage :: !Slash !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s)))
                   ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkLineImage sl sp mmarkers imAts imTrs imTas
      = mkLine LineElt (getSvgAttrs (mkAttrs imAts imTrs) ++ mkLineAttrs sl sp) sp mmarkers
      where
      mkLineAttrs slash (xspan, yspan)
        # (y1, y2) = case slash of
                       Slash     -> (toString (to2dec yspan), "0.0")
                       Backslash -> ("0.0", toString (to2dec yspan))
        = [ X1Attr ("0.0", PX), X2Attr (toString (to2dec xspan), PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    mkPolygonImage :: ![(GenSVGSt s Real, GenSVGSt s Real)] !ImageSpanReal
                      !(Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s)))
                      ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)]
                      !(Set ImageTag)
                   -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkPolygonImage points sp mmarkers imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> mkLine PolygonElt [PointsAttr (map (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers
    mkPolylineImage :: ![(GenSVGSt s Real, GenSVGSt s Real)] !ImageSpanReal
                       !(Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s)))
                       ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)]
                       !(Set ImageTag)
                    -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkPolylineImage points sp mmarkers imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> mkLine PolylineElt [PointsAttr (map (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers

    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) clval
      # (uid1, clval) = nextNo clval
      # (uid2, clval) = nextNo clval
      # (uid3, clval) = nextNo clval
      # markersAndIds = [(m, i, s) \\ Just (m, i, s) <- [ mkMarkerAndId mmStart (mkMarkerId editletId uid1) MarkerStartAttr
                                                        , mkMarkerAndId mmMid   (mkMarkerId editletId uid2) MarkerMidAttr
                                                        , mkMarkerAndId mmEnd   (mkMarkerId editletId uid3) MarkerEndAttr ]]
      = ret { mkGenSVGSyn
            & genSVGSyn_svgElts  = [ constr [] (map (\(_, x, _) -> x) markersAndIds ++ atts)
                                   , DefsElt [] [] (map (\(x, _, _) -> x) markersAndIds)]
            , genSVGSyn_onclicks = 'DM'.unions (map (\(_, _, x) -> x) markersAndIds)
            }
            clval // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId :: (Maybe (GenSVGSyn s)) String (String -> SVGAttr) -> Maybe (SVGElt, SVGAttr, Map String (s -> s)) | iTask s
      mkMarkerAndId (Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal = (w, h), genSVGSyn_onclicks}) mid posAttr
        = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto" // TODO Do something with offset?
                                        , ViewBoxAttr "0" "0" (toString (toInt w)) (toString (toInt h))
                                        , RefXAttr (toString (toInt w), PX)
                                        , RefYAttr (toString (toInt (h / 2.0)), PX)
                                        , MarkerHeightAttr (toString (toInt h), PX)
                                        , MarkerWidthAttr (toString (toInt w), PX)
                                        ] genSVGSyn_svgElts
               , posAttr (mkUrl mid)
               , genSVGSyn_onclicks)
      mkMarkerAndId _ _ _ = Nothing
    mkLine constr atts spans _ st = ret { mkGenSVGSyn & genSVGSyn_svgElts = [constr [] atts]} st

  genSVGCompositeImageAlgs :: CompositeImageAlg (GenSVGSt s Real)
                                               (GenSVGSt s (GenSVGSyn s))
                                               ([ImageOffsetReal] (Maybe (GenSVGSyn s)) ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                               (ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose totalSpan imAts imTrs imTas
      =           evalOffsets offsets `b`
      \offsets -> evalMaybe host `b`
      \host    -> compose offsets host totalSpan imAts imTrs imTas `b`
      \compose -> withSt getCpId `b`
      \cpId    -> let (elts, spans, onclicks) = case host of
                                                  Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal, genSVGSyn_onclicks}
                                                    = (genSVGSyn_svgElts ++ compose.genSVGSyn_svgElts, genSVGSyn_imageSpanReal, 'DM'.union genSVGSyn_onclicks compose.genSVGSyn_onclicks)
                                                  _ = (compose.genSVGSyn_svgElts, compose.genSVGSyn_imageSpanReal, compose.genSVGSyn_onclicks)
                  in  sequence (map (\f -> f spans) imTrs) `b`
      \imTrs   -> let attrs = mkAttrs imAts imTrs
                  in  ret { mkGenSVGSyn
                          & genSVGSyn_svgElts  = mkGroup (getHtmlAttrs attrs) [] (mkGroup [] (getSvgAttrs attrs) elts)
                          , genSVGSyn_onclicks = onclicks
                          }
    getCpId clval
      # (n, clval) = nextNo clval
      = (mkClipPathId editletId n, clval)

  genSVGComposeAlgs :: ComposeAlg (GenSVGSt s (GenSVGSyn s))
                                 ([ImageOffsetReal] (Maybe (GenSVGSyn s)) ImageSpanReal [(Maybe HtmlAttr, Maybe SVGAttr)] [ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGComposeAlgs =
    { composeAsGridAlg    = \_ _ _ _ _ _ _ _ _ -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsOverlayAlg = \_ _ _ _ _ _ _ _   -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsCollageAlg = mkCollage
    }
    where
    mkCollage :: ![GenSVGSt s (GenSVGSyn s)] ![ImageOffsetReal] !(Maybe (GenSVGSyn s))
                 !ImageSpanReal ![(Maybe HtmlAttr, Maybe SVGAttr)]
                 ![ImageSpanReal -> GenSVGSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
              -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkCollage imgs offsets mbhost totalSpan imAts imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> ret { mkGenSVGSyn
                      & genSVGSyn_svgElts  = flatten (zipWith mkTranslateGroup offsets (map (\x -> x.genSVGSyn_svgElts) imgsSps))
                      , genSVGSyn_onclicks = 'DM'.unions (map (\x -> x.genSVGSyn_onclicks) imgsSps) }

  genSVGSpanAlgs :: SpanAlg (GenSVGSt s Real) (GenSVGSt s Real) | iTask s
  genSVGSpanAlgs = evalSpanSpanAlgs

  genSVGLookupSpanAlgs :: LookupSpanAlg (GenSVGSt s Real) | iTask s
  genSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

stTrace x (clval, world)
  # world = jsTrace x world
  = (clval, world)

instance + (Real, Real) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

xor x y :== not (x && y) && (x || y)

//withSt :: (*(St a) -> *(b, *(St a))) *(St a) -> *(b, *(St a))
withSt f st = f st

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _   _   []   = []
mkGroup []  []  xs   = xs
mkGroup has sas elts = [GElt has sas elts]

evalOffsets :: ![(State .st a, State .st a)] -> State .st [(a, a)]
evalOffsets offsets = \st -> foldr f ([], st) offsets
  where
  f (sp1, sp2) (xs, st)
    # (sp1, st) = sp1 st
    # (sp2, st) = sp2 st
    = ([(sp1, sp2):xs], st)

evalMaybe :: !(Maybe (State .s a)) -> State .s (Maybe a)
evalMaybe (Just x) = x `b` \x -> ret (Just x)
evalMaybe _        = ret Nothing

ret x :== \st -> (x, st)

(`b`) f g :== f `bind` g

mkAttrs :: ![(Maybe HtmlAttr, Maybe SVGAttr)] ![(SVGTransform, ImageTransform)] -> [(Maybe HtmlAttr, Maybe SVGAttr)]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [(Nothing, Just (TransformAttr (map fst xs))):imAts]

calcAlignOffset :: !a !a !(a, a) !ImageAlign -> (a, a) | IsSpan a
calcAlignOffset maxxsp maxysp (imXSp, imYSp) (xal, yal) = (mkXAl xal, mkYAl yal)
  where
  mkXAl AtLeft    = zero
  mkXAl AtMiddleX = (maxxsp /. 2.0) - (imXSp /. 2.0)
  mkXAl AtRight   = maxxsp - imXSp
  mkYAl AtTop     = zero
  mkYAl AtMiddleY = (maxysp /. 2.0) - (imYSp /. 2.0)
  mkYAl AtBottom  = maxysp - imYSp

calculateComposedSpan :: ![(a, a)] ![(a, a)] -> (a, a) | IsSpan a
calculateComposedSpan spans offs
  = foldr f (zero, zero) (zip2 offs spans)
  where
  f ((xoff, yoff), (imXSp, imYSp)) (maxX, maxY)
    # maxX = maxOf [maxX, xoff + imXSp]
    # maxY = maxOf [maxY, yoff + imYSp]
    = (maxX, maxY)

mkTranslateGroup :: !ImageOffsetReal ![SVGElt] -> [SVGElt]
mkTranslateGroup (xoff, yoff) contents
  = mkGroup [] (mkTransformTranslateAttr (to2dec xoff, to2dec yoff)) contents

mkTransformTranslateAttr :: (Real, Real) -> [SVGAttr]
mkTransformTranslateAttr (0.0,   0.0)   = []
mkTransformTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

// TODO Detect divergence
evalSpan :: !Span -> GenSVGSt s Real | iTask s
evalSpan sp = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs sp

evalSpanSpanAlgs :: SpanAlg (GenSVGSt s Real) (GenSVGSt s Real) | iTask s
evalSpanSpanAlgs =
  { spanPxSpanAlg     = \r   -> ret r
  , spanLookupSpanAlg = \_   -> ret 0.0
  , spanAddSpanAlg    = \x y -> mkBin (+) x y
  , spanSubSpanAlg    = \x y -> mkBin (-) x y
  , spanMulSpanAlg    = \x y -> mkBin (*) x y
  , spanDivSpanAlg    = \x y -> mkBin (/) x y
  , spanAbsSpanAlg    = \x   -> mkAbs x
  , spanMinSpanAlg    = \xs  -> mkList minList xs
  , spanMaxSpanAlg    = \xs  -> mkList maxList xs
  }

evalSpanLookupSpanAlgs :: LookupSpanAlg (GenSVGSt s Real) | iTask s
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \_ _ -> ret 0.0
  , lookupSpanRowYSpanAlg     = \_ _ -> ret 0.0
  , lookupSpanImageXSpanAlg   = \_   -> ret 0.0
  , lookupSpanImageYSpanAlg   = \_   -> ret 0.0
  , lookupSpanTextXSpanAlg    = \_ _ -> ret 0.0
  }

mkAbs :: !(GenSVGSt s Real) -> GenSVGSt s Real | iTask s
mkAbs x = x `b` \x -> ret (abs x)

mkBin :: !(Real Real -> Real) !(GenSVGSt s Real) !(GenSVGSt s Real) -> GenSVGSt s Real | iTask s
mkBin op x y = x `b` \x -> y `b` \y -> ret (op x y)

mkList :: !([Real] -> Real) ![GenSVGSt s Real] -> GenSVGSt s Real | iTask s
mkList f xs = sequence xs `b` \xs -> ret (f xs)

:: Algebras m imCo imAt imTr im baIm imSp coIm imAn ho co sp loSp ma liIm liCo =
  { imageAlgs          :: ImageAlg imCo imAt imTr sp im
  , imageContentAlgs   :: ImageContentAlg baIm imSp liIm coIm imCo
  , imageAttrAlgs      :: ImageAttrAlg m imAt
  , imageTransformAlgs :: ImageTransformAlg imAn sp imTr
  , imageSpanAlgs      :: ImageSpanAlg sp imSp
  , basicImageAlgs     :: BasicImageAlg baIm
  , lineImageAlgs      :: LineImageAlg imSp ma liCo liIm
  , markersAlgs        :: MarkersAlg im ma
  , lineContentAlgs    :: LineContentAlg sp liCo
  , compositeImageAlgs :: CompositeImageAlg sp ho co coIm
  , composeAlgs        :: ComposeAlg im co
  , spanAlgs           :: SpanAlg loSp sp
  , lookupSpanAlgs     :: LookupSpanAlg loSp
  }

:: ImageAlg imCo imAt imTr sp im =
  { imageAlg :: imCo (Maybe im) [imAt] [imTr] (Set ImageTag) (sp, sp) (sp, sp, sp, sp) (sp, sp) -> im
  }

:: ImageContentAlg baIm imSp liIm coIm imCo =
  { imageContentBasicAlg     :: baIm imSp -> imCo
  , imageContentLineAlg      :: liIm      -> imCo
  , imageContentCompositeAlg :: coIm      -> imCo
  }

:: ImageAttrAlg m imAt =
  { imageAttrImageStrokeAttrAlg   :: (StrokeAttr m)      -> imAt
  , imageAttrStrokeWidthAttrAlg   :: (StrokeWidthAttr m) -> imAt
  , imageAttrXRadiusAttrAlg       :: (XRadiusAttr m)     -> imAt
  , imageAttrYRadiusAttrAlg       :: (YRadiusAttr m)     -> imAt
  , imageAttrStrokeOpacityAttrAlg :: (OpacityAttr m)     -> imAt
  , imageAttrFillAttrAlg          :: (FillAttr m)        -> imAt
  , imageAttrFillOpacityAttrAlg   :: (OpacityAttr m)     -> imAt
  , imageAttrOnClickAttrAlg       :: (OnClickAttr m)     -> imAt
  , imageAttrDashAttr             :: (DashAttr m)        -> imAt
  }

:: ImageTransformAlg imAn sp imTr =
  { imageTransformRotateImageAlg :: imAn  -> imTr
  , imageTransformSkewXImageAlg  :: imAn  -> imTr
  , imageTransformSkewYImageAlg  :: imAn  -> imTr
  , imageTransformFitImageAlg    :: sp sp -> imTr
  , imageTransformFitXImageAlg   :: sp    -> imTr
  , imageTransformFitYImageAlg   :: sp    -> imTr
  }

:: ImageSpanAlg sp imSp =
  { imageSpanAlg :: sp sp -> imSp
  }

:: BasicImageAlg baIm =
  { basicImageEmptyImageAlg   ::                   baIm
  , basicImageTextImageAlg    :: FontDef String -> baIm
  , basicImageCircleImageAlg  ::                   baIm
  , basicImageRectImageAlg    ::                   baIm
  , basicImageEllipseImageAlg ::                   baIm
  }

:: LineImageAlg imSp ma liCo liIm =
  { lineImageLineImageAlg :: imSp (Maybe ma) liCo -> liIm
  }

:: LineContentAlg sp liCo =
  { lineContentSimpleLineImageAlg :: Slash      -> liCo
  , lineContentPolygonImageAlg    :: [(sp, sp)] -> liCo
  , lineContentPolylineImageAlg   :: [(sp, sp)] -> liCo
  }

:: MarkersAlg im ma =
  { markersMarkersAlg :: (Maybe im) (Maybe im) (Maybe im) -> ma
  }

:: CompositeImageAlg sp ho co coIm =
  { compositeImageAlg :: [(sp, sp)] (Maybe ho) co -> coIm
  }

:: ComposeAlg im co =
  { composeAsGridAlg    :: (Int, Int) [ImageAlign] [[im]] -> co
  , composeAsCollageAlg ::                         [im]   -> co
  , composeAsOverlayAlg ::            [ImageAlign] [im]   -> co
  }

:: SpanAlg loSp sp =
  { spanPxSpanAlg     :: Real  -> sp
  , spanLookupSpanAlg :: loSp  -> sp
  , spanAddSpanAlg    :: sp sp -> sp
  , spanSubSpanAlg    :: sp sp -> sp
  , spanMulSpanAlg    :: sp sp -> sp
  , spanDivSpanAlg    :: sp sp -> sp
  , spanAbsSpanAlg    :: sp    -> sp
  , spanMinSpanAlg    :: [sp]  -> sp
  , spanMaxSpanAlg    :: [sp]  -> sp
  }

:: LookupSpanAlg loSp =
  { lookupSpanColumnXSpanAlg  :: (Set ImageTag) Int -> loSp
  , lookupSpanImageXSpanAlg   :: (Set ImageTag)     -> loSp
  , lookupSpanImageYSpanAlg   :: (Set ImageTag)     -> loSp
  , lookupSpanRowYSpanAlg     :: (Set ImageTag) Int -> loSp
  , lookupSpanTextXSpanAlg    :: FontDef String     -> loSp
  }

foldrCata cata xs :== foldr (\x xs -> [cata x:xs]) [] xs

foldSetCata cata xs :== 'DS'.fold (\x rs -> 'DS'.insert (cata x) rs) 'DS'.newSet xs

foldrOffsets spanAlgs lookupSpanAlgs xs
  :== let f (l, r) xs
            # synr = spanCata spanAlgs lookupSpanAlgs l
            # synl = spanCata spanAlgs lookupSpanAlgs r
            = [(synr, synl):xs]
      in  foldr f [] xs

imageCata allAlgs { Image | content, mask, attribs, transform, tags, totalSpan = (txsp, tysp), margin = (m1, m2, m3, m4), transformCorrection = (tfXCorr, tfYCorr) }
  # synContent    = imageContentCata allAlgs content
  # synMask       = fmap (imageCata allAlgs) mask
  # synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) ('DS'.toList attribs)
  # synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  # synTXsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp
  # synTYsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp
  # synm1         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m1
  # synm2         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m2
  # synm3         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m3
  # synm4         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m4
  # synXCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfXCorr
  # synYCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfYCorr
  = allAlgs.imageAlgs.imageAlg synContent synMask synsAttribs synsTransform tags (synTXsp, synTYsp) (synm1, synm2, synm3, synm4) (synXCorr, synYCorr)

imageContentCata allAlgs (Basic bi is)
  # synBasicImage = basicImageCata allAlgs.basicImageAlgs bi
  # synImageSpan  = span2TupleCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs is
  = allAlgs.imageContentAlgs.imageContentBasicAlg synBasicImage synImageSpan
imageContentCata allAlgs (Line li)
  # synLineImage = lineImageCata allAlgs li
  = allAlgs.imageContentAlgs.imageContentLineAlg synLineImage
imageContentCata allAlgs (Composite ci)
  # synCompositeImage = compositeImageCata allAlgs ci
  = allAlgs.imageContentAlgs.imageContentCompositeAlg synCompositeImage

imageAttrCata imageAttrAlgs (ImageStrokeAttr sa)         = imageAttrAlgs.imageAttrImageStrokeAttrAlg sa
imageAttrCata imageAttrAlgs (ImageStrokeWidthAttr swa)   = imageAttrAlgs.imageAttrStrokeWidthAttrAlg swa
imageAttrCata imageAttrAlgs (ImageXRadiusAttr r)         = imageAttrAlgs.imageAttrXRadiusAttrAlg r
imageAttrCata imageAttrAlgs (ImageYRadiusAttr r)         = imageAttrAlgs.imageAttrYRadiusAttrAlg r
imageAttrCata imageAttrAlgs (ImageStrokeOpacityAttr swa) = imageAttrAlgs.imageAttrStrokeOpacityAttrAlg swa
imageAttrCata imageAttrAlgs (ImageFillAttr fa)           = imageAttrAlgs.imageAttrFillAttrAlg fa
imageAttrCata imageAttrAlgs (ImageFillOpacityAttr swa)   = imageAttrAlgs.imageAttrFillOpacityAttrAlg swa
imageAttrCata imageAttrAlgs (ImageOnClickAttr cl)        = imageAttrAlgs.imageAttrOnClickAttrAlg cl
imageAttrCata imageAttrAlgs (ImageDashAttr d)            = imageAttrAlgs.imageAttrDashAttr d

imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (RotateImage ia)
  = imageTransformAlgs.imageTransformRotateImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (SkewXImage ia)
  = imageTransformAlgs.imageTransformSkewXImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (SkewYImage ia)
  = imageTransformAlgs.imageTransformSkewYImageAlg ia
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitImage sp1 sp2)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = imageTransformAlgs.imageTransformFitImageAlg synSpan1 synSpan2
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitXImage sp)
  # synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = imageTransformAlgs.imageTransformFitXImageAlg synSpan
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs (FitYImage sp)
  # synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = imageTransformAlgs.imageTransformFitYImageAlg synSpan

basicImageCata basicImageAlgs EmptyImage         = basicImageAlgs.basicImageEmptyImageAlg
basicImageCata basicImageAlgs (TextImage fd str) = basicImageAlgs.basicImageTextImageAlg fd str
basicImageCata basicImageAlgs CircleImage        = basicImageAlgs.basicImageCircleImageAlg
basicImageCata basicImageAlgs RectImage          = basicImageAlgs.basicImageRectImageAlg
basicImageCata basicImageAlgs EllipseImage       = basicImageAlgs.basicImageEllipseImageAlg

lineImageCata allAlgs { LineImage | lineSpan, markers, lineContent }
  # synImageSpan   = span2TupleCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs lineSpan
  # synMarkers     = fmap (markersCata allAlgs) markers
  # synLineContent = lineContentCata allAlgs.lineContentAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs lineContent
  = allAlgs.lineImageAlgs.lineImageLineImageAlg synImageSpan synMarkers synLineContent

markersCata allAlgs { Markers | markerStart, markerMid, markerEnd }
  # synStart = fmap (imageCata allAlgs) markerStart
  # synMid   = fmap (imageCata allAlgs) markerMid
  # synEnd   = fmap (imageCata allAlgs) markerEnd
  = allAlgs.markersAlgs.markersMarkersAlg synStart synMid synEnd

lineContentCata lineContentAlgs _ _ (SimpleLineImage sl)
  = lineContentAlgs.lineContentSimpleLineImageAlg sl
lineContentCata lineContentAlgs spanAlgs lookupSpanAlgs (PolygonImage offsets)
  # synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolygonImageAlg synsImageOffset
lineContentCata lineContentAlgs spanAlgs lookupSpanAlgs (PolylineImage offsets)
  # synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolylineImageAlg synsImageOffset

span2TupleCata imageSpanAlgs spanAlgs lookupSpanAlgs (xspan, yspan)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs xspan
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs yspan
  = imageSpanAlgs.imageSpanAlg synSpan1 synSpan2

compositeImageCata allAlgs { CompositeImage | offsets, host, compose }
  # synsImageOffset = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  # synHost         = fmap (imageCata allAlgs) host
  # synCompose      = composeCata allAlgs compose
  = allAlgs.compositeImageAlgs.compositeImageAlg synsImageOffset synHost synCompose

composeCata allAlgs (AsGrid n ias imgss)
  # synsContent = foldr (\xs xss -> [foldrCata (imageCata allAlgs) xs:xss]) [] imgss
  = allAlgs.composeAlgs.composeAsGridAlg n ias synsContent
composeCata allAlgs (AsCollage imgs)
  # synsContent = foldrCata (imageCata allAlgs) imgs
  = allAlgs.composeAlgs.composeAsCollageAlg synsContent
composeCata allAlgs (AsOverlay ias imgs)
  # synsContent = foldrCata (imageCata allAlgs) imgs
  = allAlgs.composeAlgs.composeAsOverlayAlg ias synsContent

spanCata spanAlgs lookupSpanAlgs (PxSpan rl)
  = spanAlgs.spanPxSpanAlg rl
spanCata spanAlgs lookupSpanAlgs (LookupSpan lu)
  # synLookup = lookupCata lookupSpanAlgs lu
  = spanAlgs.spanLookupSpanAlg synLookup
spanCata spanAlgs lookupSpanAlgs (AddSpan sp1 sp2)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanAddSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (SubSpan sp1 sp2)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanSubSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (MulSpan sp1 sp2)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanMulSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (DivSpan sp1 sp2)
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs sp1
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs sp2
  = spanAlgs.spanDivSpanAlg synSpan1 synSpan2
spanCata spanAlgs lookupSpanAlgs (AbsSpan sp)
  # synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = spanAlgs.spanAbsSpanAlg synSpan
spanCata spanAlgs lookupSpanAlgs (MinSpan sps)
  # synsSpans = foldrCata (spanCata spanAlgs lookupSpanAlgs) sps
  = spanAlgs.spanMinSpanAlg synsSpans
spanCata spanAlgs lookupSpanAlgs (MaxSpan sps)
  # synsSpans = foldrCata (spanCata spanAlgs lookupSpanAlgs) sps
  = spanAlgs.spanMaxSpanAlg synsSpans

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
