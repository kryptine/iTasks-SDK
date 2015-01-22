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
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS

derive class iTask FontDef, Set

:: GenSVGStVal s =
  { uniqueIdCounter :: !Int
  , genStates       :: !SpanEnvs
  }

mainSvgId :: !ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

registerSVGEvents :: !ComponentId !(JSObj svg) !(Map String (ImageAttr s)) !*JSWorld -> *JSWorld | iTask s
registerSVGEvents cid svg onclicks world
  = 'DM'.foldrWithKey (registerEvent cid svg) world onclicks
  where
  registerEvent :: !ComponentId !(JSObj svg) !String !(ImageAttr s) !*JSWorld -> *JSWorld | iTask s
  registerEvent cid svg elemCls (ImageOnClickAttr     {onclick})     world = actuallyRegister cid svg elemCls "click"     onclick     world
  registerEvent cid svg elemCls (ImageOnMouseDownAttr {onmousedown}) world = actuallyRegister cid svg elemCls "mousedown" onmousedown world
  registerEvent cid svg elemCls (ImageOnMouseUpAttr   {onmouseup})   world = actuallyRegister cid svg elemCls "mouseup"   onmouseup   world
  registerEvent cid svg elemCls (ImageOnMouseOverAttr {onmouseover}) world = actuallyRegister cid svg elemCls "mouseover" onmouseover world
  registerEvent cid svg elemCls (ImageOnMouseMoveAttr {onmousemove}) world = actuallyRegister cid svg elemCls "mousemove" onmousemove world
  registerEvent cid svg elemCls (ImageOnMouseOutAttr  {onmouseout})  world = actuallyRegister cid svg elemCls "mouseout"  onmouseout  world

  actuallyRegister cid svg elemCls evt sttf world
    #! elemCls           = replaceSubString editletId cid elemCls
    #! (elems, world)    = (svg `getElementsByClassName` elemCls) world
    #! (numElems, world) = .? (elems .# "length") world
    | jsValToInt (numElems) < 1 = world
    #! (elem, world)     = .? (elems .# "0") world
    #! cb                = createEditletEventHandler (mkCB sttf) cid
    #! (_, world)        = (elem `addEventListener` (evt, cb, True)) world
    = world
  mkCB :: !(s -> s) !String !{JSObj JSEvent} !(SVGClSt s) !*JSWorld -> *(!SVGClSt s, !ComponentDiff (SVGDiff s) (SVGClSt s), !*JSWorld) | iTask s
  mkCB sttf _ _ clval=:{svgClSt} world
    #! st` = sttf svgClSt
    = ({clval & svgClSt = st`, svgClIsDefault = False}, Diff (SetState st`) (\_ s w -> (s, NoDiff, w)), world)
  mkCB sttf _ _ clval world
    = ({clval & svgClIsDefault = False}, NoDiff, world)

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

svgns =: "http://www.w3.org/2000/svg"

:: SVGSrvSt s =
  { svgSrvIsDefault  :: !Bool
  , svgSrvSt         :: !s
  }

defaultSrvSt :: !s -> SVGSrvSt s
defaultSrvSt s = { svgSrvIsDefault  = True
                 , svgSrvSt         = s
                 }

:: SVGClSt s =
  { svgClIsDefault  :: !Bool
  , svgClSt         :: !s
  }

defaultClSt :: !s -> SVGClSt s
defaultClSt s = { svgClIsDefault  = True
                , svgClSt         = s
                }

:: SVGDiff s
  = SetState s

derive class iTask SVGDiff, SVGSrvSt

svgRenderer :: !s !(s -> Image s) -> Editlet (SVGSrvSt s) (SVGDiff s) | iTask s
svgRenderer origState state2Image
  #! dst = defaultSrvSt origState
  = { currVal    = {dst & svgSrvIsDefault = False}
    , defValSrv  = dst
    , defValClt  = defaultClSt origState
    , genUI      = genUI
    , appDiffClt = appClientDiff state2Image
    , genDiffSrv = genServerDiff
    , appDiffSrv = appServerDiff
    }
  where
  genUI :: GenUI (SVGDiff s) (SVGClSt s)
  genUI = \cid world
    = ({ ComponentHTML
       | width         = FlexSize
       , height        = FlexSize
       , html          = DivTag [IdAttr (mainSvgId cid)] []
       , eventHandlers = []
       }
       , world
      )

  appClientDiff :: !(s -> Image s) !String !(SVGDiff s) !(SVGClSt s) !*JSWorld -> *(!SVGClSt s, !*JSWorld) | iTask s
  appClientDiff state2Image cid (SetState s) clst world
    #! image            = state2Image s
    #! fontMap          = gatherFonts image
    #! (realFontMap, world) = if ('DM'.null fontMap) ('DM'.newMap, world) (calcTextLengths fontMap world)
    #! (img, spanEnvs)  = imageFromState image realFontMap
    #! fixVal           = fixEnvs {FixSpansStVal | fixSpansDidChange = False, fixSpansSpanEnvs = spanEnvs}
    #! (syn, clval)     = genSVG img { uniqueIdCounter = 0, genStates = fixVal.fixSpansSpanEnvs }
    #! (imXSp, imYSp)   = syn.genSVGSyn_imageSpanReal
    #! (imXSp, imYSp)   = (toString (toInt imXSp), toString (toInt imYSp))
    #! svgStr           = browserFriendlySVGEltToString (SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns]
                                           [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp]
                                           syn.genSVGSyn_svgElts)
    #! svgStr           = replaceSubString editletId cid svgStr
    #! (parser, world)  = new "DOMParser" () world
    #! (doc, world)     = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
    #! (newSVG, world)  = .? (doc .# "firstChild") world
    #! svgDiv           = getElementById (mainSvgId cid)
    #! (currSVG, world) = .? (svgDiv .# "firstChild") world
    #! (_, world)       = if (jsIsNull currSVG)
                            ((svgDiv `appendChild` newSVG) world)
                            ((svgDiv .# "replaceChild" .$ (newSVG, currSVG)) world)
    #! world            = registerSVGEvents cid newSVG syn.genSVGSyn_events world
    = ({clst & svgClIsDefault = False, svgClSt = s}, world)

  imageFromState :: !(Image s) !(Map FontDef (Map String Real)) -> (!Image s, !SpanEnvs) | iTask s
  imageFromState img env
    #! spanEnvs  = { spanEnvImageTagPreTrans   = 'DM'.newMap
                   , spanEnvImageSpanPreTrans  = 'DIS'.newMap
                   , spanEnvImageTagPostTrans  = 'DM'.newMap
                   , spanEnvImageSpanPostTrans = 'DIS'.newMap
                   , spanEnvGridTag  = 'DM'.newMap
                   , spanEnvGridSpan = 'DIS'.newMap
                   , spanEnvFonts    = env
                   }
    #! (img, st) = desugarAndTag img { desugarAndTagCounter  = 0
                                     , desugarAndTagSpanEnvs = spanEnvs}
    = (img, st.desugarAndTagSpanEnvs)

  genServerDiff :: !(SVGSrvSt s) !(SVGSrvSt s) -> Maybe (SVGDiff s)
  genServerDiff oldSrvSt newSrvSt = Just (SetState newSrvSt.svgSrvSt)

  appServerDiff :: !(SVGDiff s) !(SVGSrvSt s) -> SVGSrvSt s
  appServerDiff (SetState st) srvSt = {srvSt & svgSrvIsDefault = False, svgSrvSt = st}

(`getElementsByClassName`) obj args :== obj .# "getElementsByClassName" .$ args
(`addEventListener`)       obj args :== obj .# "addEventListener"       .$ args
(`setAttribute`)           obj args :== obj .# "setAttribute"           .$ args
(`setAttributeNS`)         obj args :== obj .# "setAttributeNS"         .$ args
(`createElementNS`)        obj args :== obj .# "createElementNS"        .$ args
(`appendChild`)            obj args :== obj .# "appendChild"            .$ args
(`removeChild`)            obj args :== obj .# "removeChild"            .$ args
(`getComputedTextLength`)  obj args :== obj .# "getComputedTextLength"  .$ args

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

:: GenSVGSt m a :== State (GenSVGStVal m) a

:: FixSpansSt a :== State FixSpansStVal a

:: SpanEnvs =
  { spanEnvImageTagPreTrans   :: !Map ImageTag Int
  , spanEnvImageSpanPreTrans  :: !IntMap ImageSpan
  , spanEnvImageTagPostTrans  :: !Map ImageTag Int
  , spanEnvImageSpanPostTrans :: !IntMap ImageSpan
  , spanEnvGridTag            :: !Map ImageTag Int
  , spanEnvGridSpan           :: !IntMap (!{!Span}, !{!Span})
  , spanEnvFonts              :: !Map FontDef (Map String Real)
  }

:: FixSpansStVal =
  { fixSpansDidChange :: !Bool
  , fixSpansSpanEnvs  :: !SpanEnvs
  }

:: DesugarAndTagSt a :== State DesugarAndTagStVal a

:: DesugarAndTagStVal =
  { desugarAndTagCounter  :: !Int
  , desugarAndTagSpanEnvs :: !SpanEnvs
  }

class nextNo a :: !a -> (!Int, !a)

instance nextNo (GenSVGStVal s) where
  nextNo st = (st.uniqueIdCounter, {st & uniqueIdCounter = st.uniqueIdCounter + 1})

instance nextNo DesugarAndTagStVal where
  nextNo st = (st.desugarAndTagCounter, {st & desugarAndTagCounter = st.desugarAndTagCounter + 1})

:: State s a :== s -> *(!a, !s)

:: ErrorMessage :== String

:: FixSpansSyn s =
  { fixSpansSyn_ImageContent        :: !ImageContent s
  , fixSpansSyn_TotalSpan_PreTrans  :: !ImageSpan
  , fixSpansSyn_TotalSpan_PostTrans :: !ImageSpan
  , fixSpansSyn_OffsetCorrection    :: !ImageOffset
  }

:: DesugarAndTagSyn s =
  { desugarAndTagSyn_ImageContent        :: !ImageContent s
  , desugarAndTagSyn_TotalSpan_PreTrans  :: !ImageSpan
  , desugarAndTagSyn_TotalSpan_PostTrans :: !ImageSpan
  , desugarAndTagSyn_OffsetCorrection    :: !ImageOffset
  }

sequence ms :== mapSt id ms

cacheImageSpanPreTrans :: !Int !(Set ImageTag) !ImageSpan !DesugarAndTagStVal -> DesugarAndTagStVal
cacheImageSpanPreTrans n imTas sp st
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvImageSpanPreTrans = 'DIS'.put n sp st.desugarAndTagSpanEnvs.spanEnvImageSpanPreTrans}
  #! env      = 'DS'.fold (f n) spanEnvs.spanEnvImageTagPreTrans imTas
  #! spanEnvs = {spanEnvs & spanEnvImageTagPreTrans = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}
  where
  f :: !Int !ImageTag !(Map ImageTag Int) -> Map ImageTag Int
  f n t env = 'DM'.put t n env

cacheImageSpanPostTrans :: !Int !(Set ImageTag) !ImageSpan !DesugarAndTagStVal -> DesugarAndTagStVal
cacheImageSpanPostTrans n imTas sp st
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put n sp st.desugarAndTagSpanEnvs.spanEnvImageSpanPostTrans}
  #! env      = 'DS'.fold (f n) spanEnvs.spanEnvImageTagPostTrans imTas
  #! spanEnvs = {spanEnvs & spanEnvImageTagPostTrans = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}
  where
  f :: !Int !ImageTag !(Map ImageTag Int) -> Map ImageTag Int
  f n t env = 'DM'.put t n env

cacheGridSpans :: !Int !(Set ImageTag) ![Span] ![Span] !DesugarAndTagStVal -> DesugarAndTagStVal
cacheGridSpans n imTas xsps ysps st
  #! xsps` = {x \\ x <- xsps}
  #! ysps` = {y \\ y <- ysps}
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvGridSpan = 'DIS'.put n (xsps`, ysps`) st.desugarAndTagSpanEnvs.spanEnvGridSpan}
  #! env      = 'DS'.fold (f n) spanEnvs.spanEnvGridTag imTas
  #! spanEnvs = {spanEnvs & spanEnvGridTag = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}
  where
  f :: !Int !ImageTag !(Map ImageTag Int) -> Map ImageTag Int
  f n t env = 'DM'.put t n env

applyTransforms :: ![ImageTransform] !ImageSpan -> (!ImageSpan, !ImageOffset)
applyTransforms ts sp = foldr f (sp, (px 0.0, px 0.0)) ts
  where
  f :: !ImageTransform !(!(!Span, !Span), !(!Span, !Span)) -> (!(!Span, !Span), !(!Span, !Span))
  f (RotateImage th) (accSp, accOff)
    #! (imSp, offs) = rotatedImageSpan th accSp
    = (imSp, accOff + offs)
  f (SkewXImage th) (accSp=:(_, ysp), (xoff, yoff))
    #! (xsp, offs) = skewXImageWidth th accSp
    = ((xsp, ysp), (xoff + offs, yoff))
  f (SkewYImage th) (accSp=:(xsp, _), (xoff, yoff))
    #! (ysp, offs) = skewYImageHeight th accSp
    = ((xsp, ysp), (xoff, yoff + offs))
  f (FitImage xsp ysp) (_, accOff)
    = ((xsp, ysp), accOff)
  f (FitXImage sp) ((xsp, ysp), accOff)
    = ((sp, (sp / xsp) * ysp), accOff)
  f (FitYImage sp) ((xsp, ysp), accOff)
    = (((sp / ysp) * xsp, sp), accOff)
  f FlipXImage (spans, accOff)
    = (spans, accOff)
  f FlipYImage (spans, accOff)
    = (spans, accOff)

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
rotatedImageSpan :: !Angle !(!Span, !Span) -> (!(!Span, !Span), !(!Span, !Span))
rotatedImageSpan angle (xspan, yspan)
  #! cx        = xspan /. 2.0
  #! cy        = yspan /. 2.0
  #! angle`    = toRad angle
  #! allPoints = [ mkTransform angle` cx cy zero  zero
                 , mkTransform angle` cx cy xspan zero
                 , mkTransform angle` cx cy zero  yspan
                 , mkTransform angle` cx cy xspan yspan ]
  #! allX      = strictTRMap fst allPoints
  #! allY      = strictTRMap snd allPoints
  #! minAllX   = minSpan allX
  #! minAllY   = minSpan allY
  = ( (abs (maxSpan allX - minAllX), abs (maxSpan allY - minAllY))
    , (zero - minAllX, zero - minAllY))
  where
  mkTransform :: !Real !Span !Span !Span !Span -> (!Span, !Span)
  mkTransform angle` cx cy x y = ( cx + (x - cx) *. cos angle` + (y - cy) *. sin angle`
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
skewXImageWidth :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewXImageWidth angle (xspan, yspan)
  #! rAngle   = toRad angle
  #! newXSpan = xspan + (abs (yspan *. tan rAngle))
  #! spanDiff = newXSpan - xspan
  #! mkOffset = if (rAngle <= 0.0) (zero - spanDiff) spanDiff
  = (newXSpan, mkOffset)

// Skew an image by a given angle. This function is naive as well, for the same
// reasons as the rotation function.
// TODO : We need to calculate the difference between the original and skewed
// top-left coordinate here as well, because we need it in grid layouts
//
// @param (th | Angle th)     angle          The skew angle
// @param ((a, a) | IsSpan a) (xspan, yspan) The original x and y spans of the
//                                           non-skewed image
// @return ((a, a) | IsSpan a) The new height of the skewed image and possible offset
skewYImageHeight :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewYImageHeight angle (xspan, yspan)
  #! rAngle   = toRad angle
  #! newYSpan = yspan + (abs (xspan *. tan rAngle))
  #! spanDiff = newYSpan - xspan
  #! mkOffset = if (rAngle <= 0.0) (zero - spanDiff) spanDiff
  = (newYSpan, mkOffset)

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
    mkImage :: !(Map FontDef (Set String)) !(Maybe (Map FontDef (Set String))) ![Map FontDef (Set String)] ![Map FontDef (Set String)] a b c !(!Map FontDef (Set String), !Map FontDef (Set String), !Map FontDef (Set String), !Map FontDef (Set String)) c -> Map FontDef (Set String)
    mkImage imCo mask imAts imTrs _ _ _ (m1, m2, m3, m4) _ = gatherFontsUnions [imCo : m1 : m2 : m3 : m4 : maybeToList mask ++ imAts ++ imTrs]
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
    , imageAttrDashAttr             = const 'DM'.newMap
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
    , lineContentPolygonImageAlg    = gatherFontsUnions o (foldr (\(x, y) acc -> [x:y:acc]) [])
    , lineContentPolylineImageAlg   = gatherFontsUnions o (foldr (\(x, y) acc -> [x:y:acc]) [])
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
    { composeAsGridAlg    = \_ offss _ imgss -> gatherFontsUnions (flatten imgss ++ (foldr (\(x, y) acc -> [x:y:acc]) [] (flatten offss)))
    , composeAsCollageAlg = \  offs    imgs  -> gatherFontsUnions (imgs ++ foldr (\(x, y) acc -> [x:y:acc]) [] offs)
    , composeAsOverlayAlg = \  offs  _ imgs  -> gatherFontsUnions (imgs ++ foldr (\(x, y) acc -> [x:y:acc]) [] offs)
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


desugarAndTag :: !(Image s) -> DesugarAndTagSt (Image s) | iTask s
desugarAndTag img = go
  where
  go st = imageCata desugarAndTagAllAlgs img st
  desugarAndTagAllAlgs :: Algebras s
                     ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                     (DesugarAndTagSt (ImageAttr s))
                     (DesugarAndTagSt ImageTransform)
                     (DesugarAndTagSt (Image s))
                     ((!Span, !Span) [ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s))
                     (DesugarAndTagSt (!Span, !Span))
                     ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                     (DesugarAndTagSt (Image s))
                     ((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !(!Span, !Span)))
                     (DesugarAndTagSt Span)
                     (DesugarAndTagSt Span)
                     (DesugarAndTagSt (Markers s))
                     ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                     (DesugarAndTagSt LineContent) | iTask s
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
               !(!DesugarAndTagSt Span, !DesugarAndTagSt Span)
               !(!DesugarAndTagSt Span, !DesugarAndTagSt Span)
               !(!DesugarAndTagSt Span, !DesugarAndTagSt Span, !DesugarAndTagSt Span, !DesugarAndTagSt Span)
               !(!DesugarAndTagSt Span, !DesugarAndTagSt Span)
               !DesugarAndTagStVal
            -> .(!Image s, !DesugarAndTagStVal) | iTask s
    mkImage imCo mask imAts imTrs imTas _ _ (m1, m2, m3, m4) _ st
      #! (mask, st)   = evalMaybe mask st
      #! (imAts, st)  = sequence imAts st
      #! (imTrs, st)  = sequence imTrs st
      #! (syn, st)    = imCo imTrs imTas st
      #! (m1, st)     = m1 st
      #! (m2, st)     = m2 st
      #! (m3, st)     = m3 st
      #! (m4, st)     = m4 st
      #! (xsp`, ysp`) = syn.desugarAndTagSyn_TotalSpan_PostTrans
      #! xsp`         = xsp` + m2 + m4
      #! ysp`         = ysp` + m1 + m3
      #! (no, st)     = nextNo st
      #! imTas        = 'DS'.insert (ImageTagSystem no) imTas
      #! st           = cacheImageSpanPreTrans  no imTas syn.desugarAndTagSyn_TotalSpan_PreTrans st
      #! st           = cacheImageSpanPostTrans no imTas (xsp`, ysp`) st
      #! img          = { Image
                        | content             = syn.desugarAndTagSyn_ImageContent
                        , mask                = mask
                        , attribs             = 'DS'.fromList imAts
                        , transform           = imTrs
                        , tags                = imTas
                        , totalSpanPreTrans   = syn.desugarAndTagSyn_TotalSpan_PreTrans
                        , totalSpanPostTrans  = (xsp`, ysp`)
                        , margin              = (m1, m2, m3, m4)
                        , transformCorrection = syn.desugarAndTagSyn_OffsetCorrection
                        }
      = (img, st)

  desugarAndTagImageContentAlgs :: ImageContentAlg (ImageSpan [ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s))
                                                   (DesugarAndTagSt ImageSpan)
                                                   ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                                                   ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s))
                                                   ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s)) | iTask s
  desugarAndTagImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpan [ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s))
               !(DesugarAndTagSt ImageSpan) ![ImageTransform] !(Set ImageTag)
               !DesugarAndTagStVal
            -> .(!DesugarAndTagSyn s, !DesugarAndTagStVal) | iTask s
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
    , imageAttrDashAttr             = ret o ImageDashAttr
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
    mkFitImage :: !(DesugarAndTagSt Span) !(DesugarAndTagSt Span) !DesugarAndTagStVal
               -> .(!ImageTransform, !DesugarAndTagStVal)
    mkFitImage sp1 sp2 st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      = (FitImage sp1 sp2, st)
    mkFitDim :: !(Span -> ImageTransform) !(DesugarAndTagSt Span) !DesugarAndTagStVal
             -> .(!ImageTransform, !DesugarAndTagStVal)
    mkFitDim ctr sp st
      #! (sp, st) = sp st
      = (ctr sp, st)
  desugarAndTagImageSpanAlgs :: ImageSpanAlg (DesugarAndTagSt Span) (DesugarAndTagSt ImageSpan)
  desugarAndTagImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: !(DesugarAndTagSt Span) !(DesugarAndTagSt Span) !DesugarAndTagStVal
           -> .(!ImageSpan, !DesugarAndTagStVal)
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
    mkEmptyImage :: !ImageSpan ![ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s) | iTask s
    mkEmptyImage imSp imTrs = mkSpan EmptyImage imSp imTrs

    mkRectImage :: !ImageSpan ![ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s) | iTask s
    mkRectImage imSp imTrs = mkSpan RectImage imSp imTrs

    mkTextImage :: !FontDef !String !ImageSpan ![ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s) | iTask s
    mkTextImage fd str imSp imTrs = mkSpan (TextImage fd str) imSp imTrs

    mkCircleImage :: !ImageSpan ![ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s) | iTask s
    mkCircleImage imSp imTrs = mkSpan CircleImage imSp imTrs

    mkEllipseImage :: !ImageSpan ![ImageTransform] -> DesugarAndTagSt (DesugarAndTagSyn s) | iTask s
    mkEllipseImage imSp imTrs = mkSpan EllipseImage imSp imTrs

    mkSpan :: !BasicImage !ImageSpan ![ImageTransform]
              !DesugarAndTagStVal
           -> .(!DesugarAndTagSyn s, !DesugarAndTagStVal) | iTask s
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
              !DesugarAndTagStVal
           -> .(!DesugarAndTagSyn s, !DesugarAndTagStVal) | iTask s
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
    mkMarkers :: !(Maybe (DesugarAndTagSt (Image s)))
                 !(Maybe (DesugarAndTagSt (Image s)))
                 !(Maybe (DesugarAndTagSt (Image s)))
                 !DesugarAndTagStVal
              -> .(!Markers s, !DesugarAndTagStVal) | iTask s
    mkMarkers mStart mMid mEnd st
      #! (mStart, st) = evalMaybe mStart st
      #! (mMid, st)   = evalMaybe mMid st
      #! (mEnd, st)   = evalMaybe mEnd st
      = ({ markerStart = mStart
         , markerMid   = mMid
         , markerEnd   = mEnd }, st)
  desugarAndTagLineContentAlgs :: LineContentAlg (DesugarAndTagSt Span) (DesugarAndTagSt LineContent)
  desugarAndTagLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkSimpleLine
    , lineContentPolygonImageAlg    = mkPolygon
    , lineContentPolylineImageAlg   = mkPolyline
    }
    where
    mkSimpleLine :: !Slash !DesugarAndTagStVal -> .(!LineContent, !DesugarAndTagStVal)
    mkSimpleLine sl st = (SimpleLineImage sl, st)

    mkPolygon :: ![(!State DesugarAndTagStVal Span, !State DesugarAndTagStVal Span)] !DesugarAndTagStVal -> .(!LineContent, !DesugarAndTagStVal)
    mkPolygon coords st
      #! (coords, st) = evalOffsets coords st
      = (PolygonImage coords, st)

    mkPolyline :: ![(!State DesugarAndTagStVal Span, !State DesugarAndTagStVal Span)] !DesugarAndTagStVal -> .(!LineContent, !DesugarAndTagStVal)
    mkPolyline coords st
      #! (coords, st) = evalOffsets coords st
      = (PolylineImage coords, st)
  desugarAndTagCompositeImageAlgs :: CompositeImageAlg (DesugarAndTagSt Span)
                                                  (DesugarAndTagSt (Image s))
                                                  ((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !ImageSpan))
                                                  ([ImageTransform] (Set ImageTag) -> DesugarAndTagSt (DesugarAndTagSyn s)) | iTask s
  desugarAndTagCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: !(Maybe (DesugarAndTagSt (Image s)))
                        !((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !ImageSpan))
                        ![ImageTransform] !(Set ImageTag)
                        !DesugarAndTagStVal
                    -> .(!DesugarAndTagSyn s, !DesugarAndTagStVal) | iTask s
    mkCompositeImage host compose imTrs imTas st
      #! (host, st)    = evalMaybe host st
      #! ((compose, composeSpan), st) = compose host imTrs imTas st
      #! (host, span)  = case host of
                          Just hostImg
                             -> (Just hostImg, hostImg.totalSpanPostTrans)
                          _  -> (Nothing, composeSpan)
      #! (span`, corr) = applyTransforms imTrs span
      = ({ desugarAndTagSyn_ImageContent     = Composite { CompositeImage
                                                    | host    = host
                                                    , compose = compose
                                                    }
         , desugarAndTagSyn_TotalSpan_PreTrans  = span
         , desugarAndTagSyn_TotalSpan_PostTrans = span`
         , desugarAndTagSyn_OffsetCorrection    = corr }, st)
  desugarAndTagComposeAlgs :: ComposeAlg (DesugarAndTagSt Span) (DesugarAndTagSt (Image s))
                                    ((Maybe (Image s)) [ImageTransform] (Set ImageTag) -> DesugarAndTagSt (!Compose s, !ImageSpan)) | iTask s
  desugarAndTagComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![[(DesugarAndTagSt Span, DesugarAndTagSt Span)]]
              ![[ImageAlign]] ![[DesugarAndTagSt (Image s)]]
              !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
              !DesugarAndTagStVal
           -> .(!(!Compose s, !ImageSpan), !DesugarAndTagStVal) | iTask s
    mkGrid (numcols, numrows) offsetss iass imgss mbhost imTrs imTas st
      #! (offsetss, st) = mapSt evalOffsets offsetss st
      #! (imgss, st)    = foldr seqImgsGrid ([], st) imgss
      #! imgss          = strictTRMapRev reverseTR imgss // TODO This is more or less a hack... why do we need this?
      #! offsetss       = strictTRMapRev reverseTR offsetss // TODO This is more or less a hack... why do we need this?
      #! iass           = strictTRMapRev reverseTR iass // TODO This is more or less a hack... why do we need this?
      #! (tag, st)      = nextNo st
      #! sysTags        = ImageTagSystem tag
      #! colIndices     = [0 .. numcols - 1]
      #! rowIndices     = [0 .. numrows - 1]
      #! gridSpan       = maybe ( foldr (\n acc -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) colIndices
                                , foldr (\n acc -> LookupSpan (RowYSpan sysTags n)    + acc) (px 0.0) rowIndices
                                )
                                (\x -> x.totalSpanPostTrans) mbhost
      #! spanss         = strictTRMap (strictTRMap (\x -> x.totalSpanPostTrans)) imgss
      #! st             = cacheGridSpans tag ('DS'.insert sysTags imTas) (strictTRMap (maxSpan o strictTRMap fst) (transpose spanss)) (strictTRMap (maxSpan o strictTRMap snd) spanss) st
      #! offsets`       = flatten (calculateGridOffsets (strictTRMap (\n -> LookupSpan (ColumnXSpan sysTags n)) colIndices)
                                                        (strictTRMap (\n -> LookupSpan (RowYSpan sysTags n))    rowIndices) iass imgss offsetss)
      = (( AsCollage offsets` (flatten imgss)
         , gridSpan), st)
      where
      seqImgsGrid :: ![a -> .(!b, !a)] !(![[b]], !a) -> (![[b]], !a)
      seqImgsGrid imgs (acc, st)
        #! (imgs, st) = sequence imgs st
        = ([imgs : acc], st)

      calculateGridOffsets :: ![Span] ![Span] ![[ImageAlign]] ![[Image s]] ![[(!Span, !Span)]] -> [[(!Span, !Span)]]
      calculateGridOffsets cellXSpans cellYSpans alignss imagess offsetss
        = fst (foldr (mkRows cellXSpans) ([], px 0.0) (zip4 alignss imagess cellYSpans offsetss))
        where
        mkRows :: ![Span] !(![(!XAlign, !YAlign)], ![Image s], !Span, ![(!Span, !Span)]) !(![[(!Span, !Span)]], !Span)
               -> (![[(!Span, !Span)]], !Span)
        mkRows cellXSpans (aligns, imgs, cellYSpan, offsets) (acc, yoff)
          = ( [fst (foldr (mkCols cellYSpan yoff) ([], px 0.0) (zip4 aligns imgs cellXSpans offsets)) : acc]
            , yoff + cellYSpan)
        mkCols :: !Span !Span !(!(!XAlign, !YAlign), !Image s, !Span, !(!Span, !Span)) !(![(!Span, !Span)], !Span)
               -> (![(!Span, !Span)], !Span)
        mkCols cellYSpan yoff (align, img=:{totalSpanPostTrans, transformCorrection = (tfXCorr, tfYCorr)}, cellXSpan, (manXOff, manYOff)) (acc, xoff)
          #! (alignXOff, alignYOff) = calcAlignOffset cellXSpan cellYSpan totalSpanPostTrans align
          = ([( alignXOff + xoff + manXOff + tfXCorr
              , alignYOff + yoff + manYOff + tfYCorr) : acc], xoff + cellXSpan)

    mkCollage :: ![(DesugarAndTagSt Span, DesugarAndTagSt Span)]
                 ![DesugarAndTagSt (Image s)] !(Maybe (Image s))
                 ![ImageTransform] !(Set ImageTag)
                 !DesugarAndTagStVal
              -> .(!(!Compose s, !ImageSpan), !DesugarAndTagStVal) | iTask s
    mkCollage offsets imgs mbhost imTrs imTas st
      #! (offsets, st) = evalOffsets offsets st
      #! (imgs, st) = sequence imgs st
      = (( AsCollage offsets imgs
         , maybe (calculateComposedSpan (strictTRMap (\x -> x.totalSpanPostTrans) imgs) offsets) (\x -> x.totalSpanPostTrans) mbhost), st)

    mkOverlay :: ![(DesugarAndTagSt Span, DesugarAndTagSt Span)]
                 ![ImageAlign] ![DesugarAndTagSt (Image s)]
                 !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
                 !DesugarAndTagStVal
              -> .(!(!Compose s, !ImageSpan), !DesugarAndTagStVal) | iTask s
    mkOverlay offsets ias imgs mbhost imTrs imTas st
      #! (offsets, st)  = evalOffsets offsets st
      #! (imgs, st)     = sequence imgs st
      #! spans          = strictTRMap (\x -> x.totalSpanPostTrans) imgs
      #! (  maxXSpan
          , maxYSpan)   = maybe (maxSpan (strictTRMap fst spans), maxSpan (strictTRMap snd spans))
                                (\x -> x.totalSpanPreTrans) mbhost
      #! alignOffsets   = zipWith (calcAlignOffset maxXSpan maxYSpan) spans ias
      #! placingOffsets = zipWith3 addOffset alignOffsets offsets imgs
      = ( ( AsCollage placingOffsets imgs
          , maybe (calculateComposedSpan spans alignOffsets) (\x -> x.totalSpanPostTrans) mbhost)
        , st)
      where
      addOffset :: !(!Span, !Span) !(!Span, !Span) !(Image s) -> (!Span, !Span) | iTask s
      addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)

  desugarAndTagSpanAlgs :: SpanAlg (DesugarAndTagSt Span) (DesugarAndTagSt Span)
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
    mkPxSpan :: !Real !DesugarAndTagStVal -> .(!Span, !DesugarAndTagStVal)
    mkPxSpan r st = (PxSpan r, st)

  desugarAndTagLookupSpanAlgs :: LookupSpanAlg (DesugarAndTagSt Span)
  desugarAndTagLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg = mkImageGridColSpan
    , lookupSpanRowYSpanAlg    = mkImageGridRowSpan
    , lookupSpanImageXSpanAlg  = mkImageXSpan
    , lookupSpanImageYSpanAlg  = mkImageYSpan
    , lookupSpanTextXSpanAlg   = mkTextLU
    }
    where
    mkTextLU :: !FontDef !String !DesugarAndTagStVal -> *(!Span, !DesugarAndTagStVal)
    mkTextLU fd str st
        #! sw = case 'DM'.get fd st.desugarAndTagSpanEnvs.spanEnvFonts of
                  Just fs -> case 'DM'.get str fs of
                               Just sw -> sw
                               _       -> 0.0
                  _       -> 0.0
        = (PxSpan sw, st)

    mkImageXSpan :: !ImageTag !DesugarAndTagStVal -> *(!Span, !DesugarAndTagStVal)
    mkImageXSpan t st = (LookupSpan (ImageXSpan t), st)

    mkImageYSpan :: !ImageTag !DesugarAndTagStVal -> *(!Span, !DesugarAndTagStVal)
    mkImageYSpan t st = (LookupSpan (ImageYSpan t), st)

    mkImageGridColSpan :: !ImageTag !Int !DesugarAndTagStVal -> *(!Span, !DesugarAndTagStVal)
    mkImageGridColSpan t n st = (LookupSpan (ColumnXSpan t n), st)

    mkImageGridRowSpan :: !ImageTag !Int !DesugarAndTagStVal -> *(!Span, !DesugarAndTagStVal)
    mkImageGridRowSpan t n st = (LookupSpan (RowYSpan t n), st)

fixEnvs :: !FixSpansStVal -> FixSpansStVal
fixEnvs st = fixEnvs` {st & fixSpansDidChange = False} True
  where
  fixEnvs` :: !FixSpansStVal !Bool -> FixSpansStVal
  fixEnvs` st False = st
  fixEnvs` st _
    #! st = fixImageSpansPreTrans  st
    #! st = fixImageSpansPostTrans st
    #! st = fixGridSpans           st
    = fixEnvs` st st.fixSpansDidChange
  fixImageSpansPreTrans :: !FixSpansStVal -> FixSpansStVal
  fixImageSpansPreTrans st = 'DIS'.foldrWithKey f st st.fixSpansSpanEnvs.spanEnvImageSpanPreTrans
    where
    f :: !Int !(!Span, !Span) !FixSpansStVal -> FixSpansStVal
    f k (PxSpan _, PxSpan _) st = st
    f k (w=:(PxSpan _), h) st
      #! (h`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h {st & fixSpansDidChange = False}
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPreTrans = 'DIS'.put k (w, h`) st`.fixSpansSpanEnvs.spanEnvImageSpanPreTrans}}
          st`
    f k (w, h=:(PxSpan _)) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPreTrans = 'DIS'.put k (w`, h) st`.fixSpansSpanEnvs.spanEnvImageSpanPreTrans}}
          st`
    f k (w, h) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      #! (h`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h st`
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPreTrans = 'DIS'.put k (w`, h`) st`.fixSpansSpanEnvs.spanEnvImageSpanPreTrans}}
          st`
  fixImageSpansPostTrans :: !FixSpansStVal -> FixSpansStVal
  fixImageSpansPostTrans st = 'DIS'.foldrWithKey f st st.fixSpansSpanEnvs.spanEnvImageSpanPostTrans
    where
    f :: !Int !(!Span, !Span) !FixSpansStVal -> FixSpansStVal
    f k (PxSpan _, PxSpan _) st = st
    f k (w=:(PxSpan _), h) st
      #! (h`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h {st & fixSpansDidChange = False}
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put k (w, h`) st`.fixSpansSpanEnvs.spanEnvImageSpanPostTrans}}
          st`
    f k (w, h=:(PxSpan _)) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h) st`.fixSpansSpanEnvs.spanEnvImageSpanPostTrans}}
          st`
    f k (w, h) st
      #! (w`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs w {st & fixSpansDidChange = False}
      #! (h`, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs h st`
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs = {st`.fixSpansSpanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h`) st`.fixSpansSpanEnvs.spanEnvImageSpanPostTrans}}
          st`
  fixGridSpans :: !FixSpansStVal -> FixSpansStVal
  fixGridSpans st = 'DIS'.foldrWithKey f st st.fixSpansSpanEnvs.spanEnvGridSpan
    where
    f :: !Int !(!{!Span}, !{!Span}) !FixSpansStVal -> FixSpansStVal
    f k (xsps, ysps) st
      #! (xsps`, _, st`) = foldrArr g ({x \\ x <-: xsps}, size xsps - 1, st)  xsps
      #! (ysps`, _, st`) = foldrArr g ({y \\ y <-: ysps}, size ysps - 1, st`) ysps
      = if st`.fixSpansDidChange
          {st` & fixSpansSpanEnvs  = {st`.fixSpansSpanEnvs & spanEnvGridSpan = 'DIS'.put k (xsps`, ysps`) st`.fixSpansSpanEnvs.spanEnvGridSpan}}
          st`
    g :: !Span !*(!*{!Span}, !Int, !FixSpansStVal) -> u:(!v:{!Span}, !Int, !FixSpansStVal), [u <= v]
    g v (acc, n, st)
      #! (v, st`) = spanCata fixSpansSpanAlgs fixSpansLookupSpanAlgs v {st & fixSpansDidChange = False}
      = ({acc & [n] = v}, n - 1, st`)

foldrArr :: !(a -> .b -> .b) !.b !.(arr a) -> .b | Array arr a
foldrArr f b arr
  #! arrSz = size arr
  = foldrArr` arrSz 0 f b arr
  where
  foldrArr` :: !Int !Int !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
  foldrArr` arrSz n f b arr
    | n < arrSz  = f (select arr n) (foldrArr` arrSz (n + 1) f b arr)
    | otherwise  = b

fixSpansSpanAlgs :: SpanAlg (FixSpansSt Span) (FixSpansSt Span)
fixSpansSpanAlgs =
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
  mkPxSpan :: !Real !FixSpansStVal -> .(!Span, !FixSpansStVal)
  mkPxSpan r st = (PxSpan r, st)

fixSpansLookupSpanAlgs :: LookupSpanAlg (FixSpansSt Span)
fixSpansLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg = mkImageGridColSpan
  , lookupSpanRowYSpanAlg    = mkImageGridRowSpan
  , lookupSpanImageXSpanAlg  = mkImageXSpan
  , lookupSpanImageYSpanAlg  = mkImageYSpan
  , lookupSpanTextXSpanAlg   = mkTextLU
  }
  where
  mkTextLU :: !FontDef !String !FixSpansStVal -> *(!Span, !FixSpansStVal)
  mkTextLU fd str st = (PxSpan 0.0, st)

  mkImageXSpan :: !ImageTag !FixSpansStVal -> *(!Span, !FixSpansStVal)
  mkImageXSpan t st
    = case 'DM'.get t st.fixSpansSpanEnvs.spanEnvImageTagPostTrans of
        Just n
          = case 'DIS'.get n st.fixSpansSpanEnvs.spanEnvImageSpanPostTrans of
              Just (xsp=:(PxSpan _), _)
                = (xsp, {st & fixSpansDidChange = True})
              Just _
                = (LookupSpan (ImageXSpan t), st)
              _ = (PxSpan 0.0, st)
        _ = (PxSpan 0.0, st)

  mkImageYSpan :: !ImageTag !FixSpansStVal -> *(!Span, !FixSpansStVal)
  mkImageYSpan t st
    = case 'DM'.get t st.fixSpansSpanEnvs.spanEnvImageTagPostTrans of
        Just n
          = case 'DIS'.get n st.fixSpansSpanEnvs.spanEnvImageSpanPostTrans of
              Just (_, ysp=:(PxSpan _))
                = (ysp, {st & fixSpansDidChange = True})
              Just _
                = (LookupSpan (ImageYSpan t), st)
              _ = (PxSpan 0.0, st)
        _ = (PxSpan 0.0, st)

  mkImageGridColSpan :: !ImageTag !Int !FixSpansStVal -> *(!Span, !FixSpansStVal)
  mkImageGridColSpan t n st
    = case 'DM'.get t st.fixSpansSpanEnvs.spanEnvGridTag of
        Just cacheIdx
          = case 'DIS'.get cacheIdx st.fixSpansSpanEnvs.spanEnvGridSpan of
              Just (xs, _)
                = case xs.[n] of
                    xsn=:(PxSpan _) -> (xsn, {st & fixSpansDidChange = True})
                    _               -> (LookupSpan (ColumnXSpan t n), st)
              _ = (PxSpan 0.0, st)
        _ = (PxSpan 0.0, st)

  mkImageGridRowSpan :: !ImageTag !Int !FixSpansStVal -> *(!Span, !FixSpansStVal)
  mkImageGridRowSpan t n st
    = case 'DM'.get t st.fixSpansSpanEnvs.spanEnvGridTag of
        Just cacheIdx
          = case 'DIS'.get cacheIdx st.fixSpansSpanEnvs.spanEnvGridSpan of
              Just (_, xs)
                = case xs.[n] of
                    xsn=:(PxSpan _) -> (xsn, {st & fixSpansDidChange = True})
                    _               -> (LookupSpan (RowYSpan t n), st)
              _ = (PxSpan 0.0, st)
        _ = (PxSpan 0.0, st)

:: ImageSpanReal :== (!Real, !Real)

:: ImageOffsetReal :== (!Real, !Real)

:: GenSVGSyn s =
  { genSVGSyn_svgElts       :: ![SVGElt]
  , genSVGSyn_imageSpanReal :: !ImageSpanReal
  , genSVGSyn_events        :: !Map String (ImageAttr s)
  }

mkGenSVGSyn =: { genSVGSyn_svgElts       = []
               , genSVGSyn_imageSpanReal = (0.0, 0.0)
               , genSVGSyn_events      = 'DM'.newMap
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

getSvgAttrs :: ![(![String], !Maybe SVGAttr)] -> [SVGAttr]
getSvgAttrs as = [a \\ (_, Just a) <- as]

mkClassAttr :: ![(![String], !Maybe SVGAttr)] -> [HtmlAttr]
mkClassAttr xs =
  case concatMap fst xs of
    []  -> []
    xs` -> [ClassAttr (foldr (\x xs -> x +++ " " +++ xs) "" xs`)]

mkUrl :: !String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))]

to2dec :: !Real -> Real
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
  genSVGImageAlgs :: ImageAlg (ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                             (GenSVGSt s ((![String], !Maybe SVGAttr), Map String (ImageAttr s)))
                             (ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform))
                             (GenSVGSt s Real)
                             (GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage :: !(ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
               !(Maybe (GenSVGSt s (GenSVGSyn s)))
               ![GenSVGSt s ((![String], !Maybe SVGAttr), Map String (ImageAttr s))]
               ![ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)]
               !(Set ImageTag)
               !(!GenSVGSt s Real, !GenSVGSt s Real)
               !(!GenSVGSt s Real, !GenSVGSt s Real)
               !(!GenSVGSt s Real, !GenSVGSt s Real, !GenSVGSt s Real, !GenSVGSt s Real)
               !(!GenSVGSt s Real, !GenSVGSt s Real)
               !(GenSVGStVal s) -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkImage imCo mask imAts imTrs imTas (txsp, tysp) (txsp`, tysp`) (m1, m2, _, _) _ st
      #! (imAts, st)  = sequence imAts st
      #! (txsp, st)   = txsp st
      #! (tysp, st)   = tysp st
      #! (txsp`, st)  = txsp` st
      #! (tysp`, st)  = tysp` st
      #! (m1, st)     = m1 st
      #! (m2, st)     = m2 st
      #! (maskId, st) = imageMaskId st
      #! imAts`       = strictTRMap fst imAts
      #! (syn, st)    = imCo (txsp, tysp) (maybe imAts` (const [([], Just (MaskAttr (mkUrl maskId))) : imAts`]) mask) imTrs imTas st
      #! (mask, st)   = evalMaybe mask st
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts       = mkGroup [] (mkTransformTranslateAttr (to2dec m1, to2dec m2)) (mkElt maskId mask syn)
         , genSVGSyn_imageSpanReal = (txsp`, tysp`)
         , genSVGSyn_events        = 'DM'.unions [syn.genSVGSyn_events : strictTRMap snd imAts]
         }, st)

    imageMaskId :: !a -> (!String, !a) | nextNo a
    imageMaskId clval
      #! (uid, clval) = nextNo clval
      #! maskId       = mkMaskId editletId uid
      = (maskId, clval)

    mkElt :: !String !(Maybe (GenSVGSyn .a)) !(GenSVGSyn .b) -> [SVGElt]
    mkElt _      Nothing     syn = syn.genSVGSyn_svgElts
    mkElt maskId (Just mask) syn
      = [ DefsElt [] [] [MaskElt [IdAttr maskId] [] mask.genSVGSyn_svgElts]
        : syn.genSVGSyn_svgElts]

  genSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] -> GenSVGSt s (!GenSVGSyn s, !Bool))
                                           (GenSVGSt s ImageSpanReal)
                                           (ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                           (ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                           (ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpanReal [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] -> GenSVGSt s (!GenSVGSyn s, !Bool))
               !(GenSVGSt s ImageSpanReal) !ImageSpanReal ![(![String], !Maybe SVGAttr)]
               ![ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] !(Set ImageTag)
               !(GenSVGStVal s) -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkBasic baIm imSp totalSpanPreTrans imAts imTrs _ st
      #! (imSp, st)        = imSp st
      #! ((_, isText), st) = baIm imSp [] [] st
      #! (imTrs, st)       = sequence (strictTRMap (\f -> f imSp isText) imTrs) st
      #! ((syn, _), st)    = baIm imSp imAts imTrs st
      = (syn, st)
  genSVGImageAttrAlgs :: ImageAttrAlg s (GenSVGSt s (!(![String], !Maybe SVGAttr), !Map String (ImageAttr s))) | iTask s
  genSVGImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr s -> ((([], Just (StrokeAttr (PaintColor attr.stroke Nothing))), 'DM'.newMap), s)
    , imageAttrStrokeWidthAttrAlg   = mkStrokeWidth
    , imageAttrXRadiusAttrAlg       = mkXRadius
    , imageAttrYRadiusAttrAlg       = mkYRadius
    , imageAttrStrokeOpacityAttrAlg = \attr s -> ((([], Just (StrokeOpacityAttr (toString attr.opacity))), 'DM'.newMap), s)
    , imageAttrFillAttrAlg          = \attr s -> ((([], Just (FillAttr (PaintColor attr.fill Nothing))), 'DM'.newMap), s)
    , imageAttrFillOpacityAttrAlg   = \attr s -> ((([], Just (FillOpacityAttr (FillOpacity (toString attr.opacity)))), 'DM'.newMap), s)
    , imageAttrOnClickAttrAlg       = mkOnClick
    , imageAttrOnMouseDownAttrAlg   = mkOnMouseDown
    , imageAttrOnMouseUpAttrAlg     = mkOnMouseUp
    , imageAttrOnMouseOverAttrAlg   = mkOnMouseOver
    , imageAttrOnMouseMoveAttrAlg   = mkOnMouseMove
    , imageAttrOnMouseOutAttrAlg    = mkOnMouseOut
    , imageAttrDashAttr             = \attr s -> ((([], Just (StrokeDashArrayAttr (DashArray (strictTRMap toString attr.dash)))), 'DM'.newMap), s)
    }
    where
    mkStrokeWidth :: !(StrokeWidthAttr s) !(GenSVGStVal s)
                  -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkStrokeWidth {strokewidth} st
      #! (w, st) = evalSpan strokewidth st
      = ((([], Just (StrokeWidthAttr (StrokeWidthLength (toString w, PX)))), 'DM'.newMap), st)

    mkXRadius :: !(XRadiusAttr s) !(GenSVGStVal s) -> .(!((![String], !Maybe SVGAttr), Map String (ImageAttr s)), !GenSVGStVal s) | iTask s
    mkXRadius attr st
      #! (r, st) = evalSpan attr.xradius st
      = ((([], Just (RxAttr (toString r, PX))), 'DM'.newMap), st)

    mkYRadius :: !(YRadiusAttr s) !(GenSVGStVal s) -> .(!((![String], !Maybe SVGAttr), Map String (ImageAttr s)), !GenSVGStVal s) | iTask s
    mkYRadius attr st
      #! (r, st) = evalSpan attr.yradius st
      = ((([], Just (RyAttr (toString r, PX))), 'DM'.newMap), st)

    mkOnClick :: !(OnClickAttr s) !(GenSVGStVal s)
              -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnClick attr clval = mkEvent mkOnClickId (ImageOnClickAttr attr) clval

    mkOnMouseDown :: !(OnMouseDownAttr s) !(GenSVGStVal s)
                  -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnMouseDown attr clval = mkEvent mkOnMouseDownId (ImageOnMouseDownAttr attr) clval

    mkOnMouseUp :: !(OnMouseUpAttr s) !(GenSVGStVal s)
                -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnMouseUp attr clval = mkEvent mkOnMouseUpId (ImageOnMouseUpAttr attr) clval

    mkOnMouseOver :: !(OnMouseOverAttr s) !(GenSVGStVal s)
                  -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnMouseOver attr clval = mkEvent mkOnMouseOverId (ImageOnMouseOverAttr attr) clval

    mkOnMouseMove :: !(OnMouseMoveAttr s) !(GenSVGStVal s)
                  -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnMouseMove attr clval = mkEvent mkOnMouseMoveId (ImageOnMouseMoveAttr attr) clval

    mkOnMouseOut :: !(OnMouseOutAttr s) !(GenSVGStVal s)
                 -> .((!(![String], !Maybe SVGAttr), !Map String (ImageAttr s)) , GenSVGStVal s) | iTask s
    mkOnMouseOut attr clval = mkEvent mkOnMouseOutId (ImageOnMouseOutAttr attr) clval

    mkEvent mkIdFun attr clval
      #! (uniqId, clval) = nextNo clval
      #! ocId            = mkIdFun editletId uniqId
      = ((([ocId], Nothing), 'DM'.singleton ocId attr), clval)
  genSVGImageTransformAlgs :: ImageTransformAlg (GenSVGSt s Real) (ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)) | iTask s
  genSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = mkRotateTransform
    , imageTransformSkewXImageAlg  = \imAn _ _ s -> (([SkewXTransform (toString (toDeg imAn))], SkewXImage imAn), s)
    , imageTransformSkewYImageAlg  = \imAn _ _ s -> (([SkewYTransform (toString (toDeg imAn))], SkewYImage imAn), s)
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitXImage
    , imageTransformFitYImageAlg   = mkFitYImage
    , imageTransformFlipXImageAlg  = mkFlipXImage
    , imageTransformFlipYImageAlg  = mkFlipYImage
    }
    where
    mkRotateTransform :: !Angle !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkRotateTransform imAn (xsp, ysp) isText s
      // FIXME: We currently devide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
      #! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
      = (([RotateTransform (toString (to2dec (toDeg imAn))) (Just (toString (to2dec (xsp / 2.0)), toString (to2dec yoff)))], RotateImage imAn), s)

    mkFitImage :: !((GenSVGStVal s) -> (!Real, !GenSVGStVal s)) !((GenSVGStVal s) -> (!Real, !GenSVGStVal s)) !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkFitImage sp1 sp2 (xsp, ysp) _ st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      = (([ScaleTransform (toString (to2dec (sp1 / xsp))) (toString (to2dec (sp2 / ysp)))], FitImage (px sp1) (px sp2)), st)

    mkFitXImage :: !((GenSVGStVal s) -> (!Real, !GenSVGStVal s)) !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkFitXImage sp (xsp, _) _ st
      #! (sp, st) = sp st
      #! scale    = if (xsp > 0.0) (toString (to2dec (sp / xsp))) "1.0"
      = (([ScaleTransform scale scale], FitXImage (px sp)), st)

    mkFitYImage :: !((GenSVGStVal s) -> (!Real, !GenSVGStVal s)) !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkFitYImage sp (_, ysp) _ st
      #! (sp, st) = sp st
      #! scale    = if (ysp > 0.0) (toString (to2dec (sp / ysp))) "1.0"
      = (([ScaleTransform scale scale], FitYImage (px sp)), st)

    mkFlipXImage :: !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkFlipXImage (xsp, _) _ st
      = (([TranslateTransform (toString xsp) "0", ScaleTransform "-1" "1"], FlipXImage), st)

    mkFlipYImage :: !(!Real, !Real) !Bool !(GenSVGStVal s) -> .(!(![SVGTransform], !ImageTransform), !GenSVGStVal s)
    mkFlipYImage (_, ysp) _ st
      = (([TranslateTransform "0" (toString ysp), ScaleTransform "1" "-1"], FlipYImage), st)
  genSVGImageSpanAlgs :: ImageSpanAlg (GenSVGSt s Real) (GenSVGSt s ImageSpanReal) | iTask s
  genSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan :: !(GenSVGSt s Real) !(GenSVGSt s Real) !(GenSVGStVal s) -> .(!ImageSpanReal, GenSVGStVal s) | iTask s
    mkImageSpan sp1 sp2 st
      #! (sp1, st) = sp1 st
      #! (sp2, st) = sp2 st
      = ((sp1, sp2), st)
  genSVGBasicImageAlgs :: BasicImageAlg (ImageSpanReal [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] -> GenSVGSt s (!GenSVGSyn s, !Bool)) | iTask s
  genSVGBasicImageAlgs =
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage :: !ImageSpanReal ![(![String], !Maybe SVGAttr)]
                    ![(![SVGTransform], !ImageTransform)]
                    !(GenSVGStVal s)
                 -> .(!(!GenSVGSyn s, !Bool), GenSVGStVal s) | iTask s
    mkEmptyImage imSp imAts imTrs st
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (mkClassAttr imAts) [] (mkGroup (mkWH imSp) (getSvgAttrs (mkAttrs imAts imTrs)) []) }, False), st)
    mkTextImage :: !FontDef !String !ImageSpanReal
                   ![(![String], !Maybe SVGAttr)]
                   ![(![SVGTransform], !ImageTransform)]
                   !(GenSVGStVal s)
                -> .(!(!GenSVGSyn s, !Bool), GenSVGStVal s) | iTask s
    mkTextImage fd str imSp imAts imTrs st
    // TODO Currently we manually translate text by fontysize pixels to compensate for the "auto" baseline. The result look OK, but a bit off compare to the old approach where we forced the origin to be the top-left corner (which didn't work with zooming)
    // We need to offset by the font's descent height, but that's not easy to calculate currently (there are no JS APIs for that yet). Current heuristic: we assume that the ex-height is half of the font height. We assume that the descent height is half of the ex-height. Therefore, we multiply by 0.75
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (mkClassAttr imAts) [TransformAttr [TranslateTransform "0" (toString (fd.fontysize * 0.75))]]
                                                                          [TextElt [XmlspaceAttr "preserve"] (getSvgAttrs (mkAttrs imAts imTrs) ++ fontAttrs fd.fontysize) str] }
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
    mkRectImage :: !ImageSpanReal ![(![String], !Maybe SVGAttr)]
                   ![(![SVGTransform], !ImageTransform)]
                   !(GenSVGStVal s)
                -> .(!(!GenSVGSyn s, !Bool), GenSVGStVal s) | iTask s
    mkRectImage imSp imAts imTrs st
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (mkClassAttr imAts) [] [RectElt (mkWH imSp) (getSvgAttrs (mkAttrs imAts imTrs))] }, False), st)
    mkCircleImage :: !ImageSpanReal ![(![String], !Maybe SVGAttr)]
                     ![(![SVGTransform], !ImageTransform)]
                     !(GenSVGStVal s)
                   -> .(!(!GenSVGSyn s, !Bool), GenSVGStVal s) | iTask s
    mkCircleImage imSp=:(imXSp`, _) imAts imTrs st
      #! r = imXSp` / 2.0
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (mkClassAttr imAts) [] [CircleElt []
                                          [ RAttr (toString (to2dec r), PX), CxAttr (toString (to2dec r), PX)
                                          , CyAttr (toString (to2dec r), PX) : (getSvgAttrs (mkAttrs imAts imTrs)) ]] }, False), st)
    mkEllipseImage :: !ImageSpanReal ![(![String], !Maybe SVGAttr)]
                      ![(![SVGTransform], !ImageTransform)]
                      !(GenSVGStVal s)
                   -> .(!(!GenSVGSyn s, !Bool), GenSVGStVal s) | iTask s
    mkEllipseImage imSp=:(imXSp, imYSp) imAts imTrs st
      = (({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup (mkClassAttr imAts) [] [EllipseElt [] (getSvgAttrs (mkAttrs imAts imTrs) ++
                                          [ RxAttr (toString (to2dec (imXSp / 2.0)), PX), RyAttr (toString (to2dec (imYSp / 2.0)), PX)
                                          , CxAttr (toString (to2dec (imXSp / 2.0)), PX), CyAttr (toString (to2dec (imYSp / 2.0)), PX)])] }, False), st)

  genSVGLineImageAlgs :: LineImageAlg (GenSVGSt s (!Real, !Real))
                                      (GenSVGSt s b)
                                      ((!Real, !Real) (Maybe b) [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                      (c [(![String], !Maybe SVGAttr)] [(!Real, !Real) Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
  genSVGLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage :: !(GenSVGSt s ImageSpanReal)
                   !(Maybe (GenSVGSt s b))
                   !(ImageSpanReal (Maybe b) [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                   c
                   ![([String], Maybe SVGAttr)]
                   ![ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)]
                   !(Set ImageTag)
                   !(GenSVGStVal s)
                -> .(!GenSVGSyn s, !GenSVGStVal s)
    mkLineImage lineSpan mmarkers lineContent _ imAts imTrs imTas st
      #! (lineSpan, st) = lineSpan st
      #! (mmarkers, st) = evalMaybe mmarkers st
      #! (imTrs, st)    = sequence (strictTRMap (\f -> f lineSpan False) imTrs) st
      = lineContent lineSpan mmarkers imAts imTrs imTas st

  genSVGMarkersAlgs :: MarkersAlg (GenSVGSt s (GenSVGSyn s)) (GenSVGSt s (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) | iTask s
  genSVGMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (GenSVGSt s b)) !(Maybe (GenSVGSt s c)) !(Maybe (GenSVGSt s d)) !(GenSVGStVal s) -> .(!(!Maybe b, !Maybe c, !Maybe d), !GenSVGStVal s) | iTask s
    mkMarkers m1 m2 m3 st
      #! (m1, st) = evalMaybe m1 st
      #! (m2, st) = evalMaybe m2 st
      #! (m3, st) = evalMaybe m3 st
      = ((m1, m2, m3), st)
  genSVGLineContentAlgs :: LineContentAlg (GenSVGSt s Real)
                                         (ImageSpanReal (Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) [(![String], !Maybe SVGAttr)] [(![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkLineImage
    , lineContentPolygonImageAlg    = mkPolygonImage
    , lineContentPolylineImageAlg   = mkPolylineImage
    }
    where
    mkLineImage :: !Slash !ImageSpanReal !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                   ![(![String], !Maybe SVGAttr)] ![(![SVGTransform], !ImageTransform)] !(Set ImageTag)
                -> GenSVGSt s (GenSVGSyn s) | iTask s
    mkLineImage sl sp mmarkers imAts imTrs imTas
      = mkLine LineElt (getSvgAttrs (mkAttrs imAts imTrs) ++ mkLineAttrs sl sp) sp mmarkers
      where
      mkLineAttrs :: !Slash !(!Real, !Real) -> [SVGAttr]
      mkLineAttrs slash (xspan, yspan)
        #! (y1, y2) = case slash of
                        Slash     -> (toString (to2dec yspan), "0.0")
                        Backslash -> ("0.0", toString (to2dec yspan))
        = [ X1Attr ("0.0", PX), X2Attr (toString (to2dec xspan), PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    mkPolygonImage :: ![(!GenSVGSt s Real, !GenSVGSt s Real)] !ImageSpanReal
                      !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                      ![(![String], !Maybe SVGAttr)] ![(![SVGTransform], !ImageTransform)]
                      !(Set ImageTag) !(GenSVGStVal s)
                   -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkPolygonImage points sp mmarkers imAts imTrs imTas st
      #! (offsets, st) = evalOffsets points st
      = mkLine PolygonElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers st
    mkPolylineImage :: ![(!GenSVGSt s Real, !GenSVGSt s Real)] !ImageSpanReal
                       !(Maybe (!Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s)))
                       ![(![String], !Maybe SVGAttr)] ![(![SVGTransform], !ImageTransform)]
                       !(Set ImageTag) !(GenSVGStVal s)
                    -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkPolylineImage points sp mmarkers imAts imTrs imTas st
      #! (offsets, st) = evalOffsets points st
      = mkLine PolylineElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers st

    mkLine :: !([HtmlAttr] [SVGAttr] -> SVGElt) ![SVGAttr] !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) !(GenSVGStVal s) -> .(!GenSVGSyn s, !GenSVGStVal s) | iTask s
    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) clval
      #! (uid1, clval) = nextNo clval
      #! (uid2, clval) = nextNo clval
      #! (uid3, clval) = nextNo clval
      #! markersAndIds = [(m, i, s) \\ Just (m, i, s) <- [ mkMarkerAndId mmStart (mkMarkerId editletId uid1) MarkerStartAttr
                                                         , mkMarkerAndId mmMid   (mkMarkerId editletId uid2) MarkerMidAttr
                                                         , mkMarkerAndId mmEnd   (mkMarkerId editletId uid3) MarkerEndAttr ]]
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts  = [ constr [] (strictTRMap (\(_, x, _) -> x) markersAndIds ++ atts)
                                , DefsElt [] [] (strictTRMap (\(x, _, _) -> x) markersAndIds)]
         , genSVGSyn_events = 'DM'.unions (strictTRMap (\(_, _, x) -> x) markersAndIds)
         }, clval) // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId :: !(Maybe (GenSVGSyn s)) !String !(String -> SVGAttr) -> Maybe (!SVGElt, !SVGAttr, !Map String (ImageAttr s)) | iTask s
      mkMarkerAndId (Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal = (w, h), genSVGSyn_events}) mid posAttr
        = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto"
                                        , ViewBoxAttr "0" "0" (toString (toInt w)) (toString (toInt h))
                                        , RefXAttr (toString (toInt w), PX)
                                        , RefYAttr (toString (toInt (h / 2.0)), PX)
                                        , MarkerHeightAttr (toString (toInt h), PX)
                                        , MarkerWidthAttr (toString (toInt w), PX)
                                        ] genSVGSyn_svgElts
               , posAttr (mkUrl mid)
               , genSVGSyn_events)
      mkMarkerAndId _ _ _ = Nothing
    mkLine constr atts spans _ st = ({ mkGenSVGSyn & genSVGSyn_svgElts = [constr [] atts]}, st)

  genSVGCompositeImageAlgs :: CompositeImageAlg (GenSVGSt s Real)
                                               (GenSVGSt s (GenSVGSyn s))
                                               ((Maybe (GenSVGSyn s)) ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                                               (ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: !(Maybe (GenSVGSt s (GenSVGSyn s)))
                        !((Maybe (GenSVGSyn s)) ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s))
                        !ImageSpanReal
                        ![(![String], !Maybe SVGAttr)]
                        ![ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)]
                        !(Set ImageTag) !(GenSVGStVal s)
                     -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkCompositeImage host compose totalSpanPreTrans imAts imTrs imTas st
      #! (host, st)    = evalMaybe host st
      #! (compose, st) = compose host totalSpanPreTrans imAts imTrs imTas st
      #! (cpId, st)    = getCpId st
      #! (elts, spans, onclicks) = case host of
                                     Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal, genSVGSyn_events}
                                       = (genSVGSyn_svgElts ++ compose.genSVGSyn_svgElts, genSVGSyn_imageSpanReal, 'DM'.union genSVGSyn_events compose.genSVGSyn_events)
                                     _ = (compose.genSVGSyn_svgElts, compose.genSVGSyn_imageSpanReal, compose.genSVGSyn_events)
      #! (imTrs, st) = sequence (strictTRMap (\f -> f spans False) imTrs) st
      #! attrs = mkAttrs imAts imTrs
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts = mkGroup (mkClassAttr attrs) [] (mkGroup [] (getSvgAttrs attrs) elts)
         , genSVGSyn_events  = onclicks
         }, st)
    getCpId :: !(GenSVGStVal s) -> (!String, !GenSVGStVal s) | iTask s
    getCpId clval
      #! (n, clval) = nextNo clval
      = (mkClipPathId editletId n, clval)

  genSVGComposeAlgs :: ComposeAlg (GenSVGSt s Real) (GenSVGSt s (GenSVGSyn s))
                                 ((Maybe (GenSVGSyn s)) ImageSpanReal [(![String], !Maybe SVGAttr)] [ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] (Set ImageTag) -> GenSVGSt s (GenSVGSyn s)) | iTask s
  genSVGComposeAlgs =
    { composeAsGridAlg    = \_ _ _ _ _ _ _ _ _ -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsOverlayAlg = \_ _ _ _ _ _ _ _   -> ret mkGenSVGSyn // These aren't used. They're translated to collages in fixSpans. We provide them here only because we must if we don't want the evaluation to crash.
    , composeAsCollageAlg = mkCollage
    }
    where
    mkCollage :: ![(!GenSVGSt s Real, !GenSVGSt s Real)]
                 ![GenSVGSt s (GenSVGSyn s)] !(Maybe (GenSVGSyn s))
                 !ImageSpanReal ![(![String], !Maybe SVGAttr)]
                 ![ImageSpanReal Bool -> GenSVGSt s (![SVGTransform], !ImageTransform)] !(Set ImageTag)
                 !(GenSVGStVal s)
              -> .(!GenSVGSyn s, GenSVGStVal s) | iTask s
    mkCollage offsets imgs _ totalSpanPreTrans imAts imTrs imTas st
      #! (offsets, st) = evalOffsets offsets st
      #! (imgsSps, st) = sequence imgs st
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts       = flatten (zipWith mkTranslateGroup offsets (strictTRMap (\x -> x.genSVGSyn_svgElts) imgsSps))
         , genSVGSyn_events        = 'DM'.unions (strictTRMap (\x -> x.genSVGSyn_events) imgsSps)
         , genSVGSyn_imageSpanReal = totalSpanPreTrans }, st) // Setting genSVGSyn_imageSpanReal is required here. It needs to be totalSpanPreTrans, because transforms will be calculated just after this.

  genSVGSpanAlgs :: SpanAlg (GenSVGSt s Real) (GenSVGSt s Real) | iTask s
  genSVGSpanAlgs = evalSpanSpanAlgs

  genSVGLookupSpanAlgs :: LookupSpanAlg (GenSVGSt s Real) | iTask s
  genSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

stTrace :: !a !*(!cl, !*JSWorld) -> *(cl, *JSWorld)
stTrace x (clval, world)
  #! world = jsTrace x world
  = (clval, world)

instance + (Real, Real) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _   _   []   = []
mkGroup []  []  xs   = xs
mkGroup []  [TransformAttr [TranslateTransform x y]] [GElt [] [TransformAttr [TranslateTransform x` y`]] elts] = [GElt [] [TransformAttr [TranslateTransform (toString (to2dec (toReal x + toReal x`))) (toString (to2dec (toReal y + toReal y`)))]] elts]
mkGroup has sas elts = [GElt has sas elts]

evalOffsets :: ![(!State .st a, !State .st a)] !.st -> .(![(!a, !a)], !.st)
evalOffsets offsets st = foldr f ([], st) offsets
  where
  f :: !(!.a -> (!b, !.c), !.c -> .(!d, !.e)) !(![(!b, !d)], !.a) -> (![(!b, !d)], !.e)
  f (sp1, sp2) (xs, st)
    #! (sp1, st) = sp1 st
    #! (sp2, st) = sp2 st
    = ([(sp1, sp2):xs], st)

evalMaybe :: !(Maybe (State .s a)) !.s -> .(!Maybe a, !.s)
evalMaybe (Just x) st
  #! (x, st) = x st
  = (Just x, st)
evalMaybe _ st = (Nothing, st)

ret :: !a !.s -> (!a, !.s)
ret x st = (x, st)

mkAttrs :: ![(![String], !Maybe SVGAttr)] ![(![SVGTransform], !ImageTransform)] -> [(![String], !Maybe SVGAttr)]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [([], Just (TransformAttr (flatten (strictTRMap fst xs)))):imAts]

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
  = foldr f (zero, zero) (zip2 offs spans)
  where
  f :: !(!(!Span, !Span), !(!Span, !Span)) !(!Span, !Span) -> (!Span, !Span)
  f ((xoff, yoff), (imXSp, imYSp)) (maxX, maxY) = (maxSpan [maxX, xoff + imXSp], maxSpan [maxY, yoff + imYSp])

mkTranslateGroup :: !ImageOffsetReal ![SVGElt] -> [SVGElt]
mkTranslateGroup (xoff, yoff) contents
  = mkGroup [] (mkTransformTranslateAttr (to2dec xoff, to2dec yoff)) contents

mkTransformTranslateAttr :: !(!Real, !Real) -> [SVGAttr]
mkTransformTranslateAttr (0.0,   0.0)   = []
mkTransformTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

evalSpan :: !Span -> GenSVGSt s Real | iTask s
evalSpan sp = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs sp

evalSpanSpanAlgs :: SpanAlg (GenSVGSt s Real) (GenSVGSt s Real)
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

evalSpanLookupSpanAlgs :: LookupSpanAlg (GenSVGSt s Real) | iTask s
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg = mkImageGridColSpan
  , lookupSpanRowYSpanAlg    = mkImageGridRowSpan
  , lookupSpanImageXSpanAlg  = mkImageXSpan
  , lookupSpanImageYSpanAlg  = mkImageYSpan
  , lookupSpanTextXSpanAlg   = mkTextLU
  }
  where
  mkTextLU :: !FontDef !String !(GenSVGStVal s) -> *(!Real, !(GenSVGStVal s)) | iTask s
  mkTextLU fd str st = (0.0, st)

  mkImageXSpan :: !ImageTag !(GenSVGStVal s) -> *(!Real, !(GenSVGStVal s)) | iTask s
  mkImageXSpan t st
    = case 'DM'.get t st.genStates.spanEnvImageTagPostTrans of
        Just n
          = case 'DIS'.get n st.genStates.spanEnvImageSpanPostTrans of
              Just (xsp, _)
                = evalSpan xsp st
              _ = (0.0, st)
        _ = (0.0, st)

  mkImageYSpan :: !ImageTag !(GenSVGStVal s) -> *(!Real, !(GenSVGStVal s)) | iTask s
  mkImageYSpan t st
    = case 'DM'.get t st.genStates.spanEnvImageTagPostTrans of
        Just n
          = case 'DIS'.get n st.genStates.spanEnvImageSpanPostTrans of
              Just (_, ysp)
                = evalSpan ysp st
              _ = (0.0, st)
        _ = (0.0, st)

  mkImageGridColSpan :: !ImageTag !Int !(GenSVGStVal s) -> *(!Real, !(GenSVGStVal s)) | iTask s
  mkImageGridColSpan t colIdx st
    = case 'DM'.get t st.genStates.spanEnvGridTag of
        Just cacheIdx
          = case 'DIS'.get cacheIdx st.genStates.spanEnvGridSpan of
              Just (xs, _)
                = evalSpan xs.[colIdx] st
              _ = (0.0, st)
        _ = (0.0, st)

  mkImageGridRowSpan :: !ImageTag !Int !(GenSVGStVal s) -> *(!Real, !(GenSVGStVal s)) | iTask s
  mkImageGridRowSpan t rowIdx st
    = case 'DM'.get t st.genStates.spanEnvGridTag of
        Just cacheIdx
          = case 'DIS'.get cacheIdx st.genStates.spanEnvGridSpan of
              Just (_, xs)
                = evalSpan xs.[rowIdx] st
              _ = (0.0, st)
        _ = (0.0, st)

mkAbs :: !(a -> (!b, !c)) !a -> .(!b, !c) | abs b
mkAbs x st
  #! (x, st) = x st
  = (abs x, st)

mkBin :: !(a b -> c) !(d -> .(!a, !d)) !(d -> .(!b, !d)) !d -> .(!c, !d)
mkBin op x y st
  #! (x, st) = x st
  #! (y, st) = y st
  = (op x y, st)

mkList :: !([a] -> b) ![c -> .(!a, !c)] !c -> .(!b, !c)
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
  { imageAlg :: !imCo (Maybe im) [imAt] [imTr] (Set ImageTag) (!sp, !sp) (!sp, !sp) (!sp, !sp, !sp, !sp) (!sp, !sp) -> im
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
  , imageAttrDashAttr             :: !(DashAttr m)        -> imAt
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
  { lookupSpanColumnXSpanAlg  :: !ImageTag Int -> loSp
  , lookupSpanImageXSpanAlg   :: !ImageTag     -> loSp
  , lookupSpanImageYSpanAlg   :: !ImageTag     -> loSp
  , lookupSpanRowYSpanAlg     :: !ImageTag Int -> loSp
  , lookupSpanTextXSpanAlg    :: !FontDef String     -> loSp
  }

foldrCata :: !(a -> b) ![a] -> [b]
foldrCata cata xs = foldr (f cata) [] xs
  where
  f :: !(a -> b) !a ![b] -> [b]
  f cata x xs = [cata x : xs]

//foldSetCata :: !(a -> b) !(Set a) -> Set b | < a & == a & < b & == b
//foldSetCata cata xs = 'DS'.fold (f cata) 'DS'.newSet xs
  //where
  //f :: !(a -> b) !a !(Set b) -> Set b | < a & == a & < b & == b
  //f cata x rs = 'DS'.insert (cata x) rs

foldrOffsets :: !(SpanAlg a b) !(LookupSpanAlg a) ![(!Span, !Span)] -> [(!b, !b)]
foldrOffsets spanAlgs lookupSpanAlgs xs = foldr (f spanAlgs lookupSpanAlgs) [] xs
  where
  f :: !(SpanAlg a b) !(LookupSpanAlg a) !(!Span, !Span) ![(!b, !b)] -> [(!b, !b)]
  f spanAlgs lookupSpanAlgs (l, r) xs
    #! synl = spanCata spanAlgs lookupSpanAlgs l
    #! synr = spanCata spanAlgs lookupSpanAlgs r
    = [(synl, synr):xs]

imageCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(Image m) -> im
imageCata allAlgs { Image | content, mask, attribs, transform, tags, totalSpanPreTrans = (txsp, tysp), totalSpanPostTrans = (txsp`, tysp`), margin = (m1, m2, m3, m4), transformCorrection = (tfXCorr, tfYCorr) }
  #! synContent    = imageContentCata allAlgs content
  #! synMask       = fmap (imageCata allAlgs) mask
  #! synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) ('DS'.toList attribs)
  #! synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  #! synTXsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp
  #! synTYsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp
  #! synTXsp`      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp`
  #! synTYsp`      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp`
  #! synm1         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m1
  #! synm2         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m2
  #! synm3         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m3
  #! synm4         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m4
  #! synXCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfXCorr
  #! synYCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfYCorr
  = allAlgs.imageAlgs.imageAlg synContent synMask synsAttribs synsTransform tags (synTXsp, synTYsp) (synTXsp`, synTYsp`) (synm1, synm2, synm3, synm4) (synXCorr, synYCorr)

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
imageAttrCata imageAttrAlgs (ImageDashAttr d)            = imageAttrAlgs.imageAttrDashAttr d

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
  #! synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolygonImageAlg synsImageOffset
lineContentCata lineContentAlgs spanAlgs lookupSpanAlgs (PolylineImage offsets)
  #! synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = lineContentAlgs.lineContentPolylineImageAlg synsImageOffset

span2TupleCata :: !(ImageSpanAlg sp imSp) !(SpanAlg loSp sp) !(LookupSpanAlg loSp) !(Span, Span) -> imSp
span2TupleCata imageSpanAlgs spanAlgs lookupSpanAlgs (xspan, yspan)
  #! synSpan1 = spanCata spanAlgs lookupSpanAlgs xspan
  #! synSpan2 = spanCata spanAlgs lookupSpanAlgs yspan
  = imageSpanAlgs.imageSpanAlg synSpan1 synSpan2

compositeImageCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(CompositeImage m) -> coIm
compositeImageCata allAlgs { CompositeImage | host, compose }
  #! synHost         = fmap (imageCata allAlgs) host
  #! synCompose      = composeCata allAlgs compose
  = allAlgs.compositeImageAlgs.compositeImageAlg synHost synCompose

composeCata :: !(Algebras m imCo imAt imTr im baIm imSp coIm im co sp loSp ma liIm liCo) !(Compose m) -> co
composeCata allAlgs (AsGrid n offsetss ias imgss)
  #! synsImageOffsetss = foldr (\xs xss -> [foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs xs:xss]) [] offsetss
  #! synsContent = foldr (\xs xss -> [foldrCata (imageCata allAlgs) xs:xss]) [] imgss
  = allAlgs.composeAlgs.composeAsGridAlg n synsImageOffsetss ias synsContent
composeCata allAlgs (AsCollage offsets imgs)
  #! synsImageOffsets = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  #! synsContent = foldrCata (imageCata allAlgs) imgs
  = allAlgs.composeAlgs.composeAsCollageAlg synsImageOffsets synsContent
composeCata allAlgs (AsOverlay offsets ias imgs)
  #! synsImageOffsets = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  #! synsContent = foldrCata (imageCata allAlgs) imgs
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
  #! synsSpans = foldrCata (spanCata spanAlgs lookupSpanAlgs) sps
  = spanAlgs.spanMinSpanAlg synsSpans
spanCata spanAlgs lookupSpanAlgs (MaxSpan sps)
  #! synsSpans = foldrCata (spanCata spanAlgs lookupSpanAlgs) sps
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
