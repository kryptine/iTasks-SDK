implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import iTasks
import iTasks.API.Core.Client.Editlet
from StdOrdList import minList, maxList
import StdOverloaded
import StdArray
import StdMisc
import GenLexOrd, GenEq
import Data.List
import Data.Func
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Set as DS
from StdFunc import `bind`

derive JSONEncode Image
derive JSONDecode Image
derive gEq        Image
derive gEditMeta  Image, SVGColor
derive gVerify    Image, SVGColor
derive gDefault   Image, SVGColor
derive gText      Image, SVGColor
derive gEditor    Image, SVGColor
derive gUpdate    Image, SVGColor

derive class iTask ImageTag, ImageTransform, Span, LookupSpan, ImageAttr,
  ImageContent, BasicImage, CompositeImage, Slash, FontDef, Compose,
  OpacityAttr, FillAttr, StrokeWidthAttr, StrokeAttr, OnClickAttr, XAlign,
  YAlign, Set, CachedSpan, Deg, Rad, LineImage, Markers,
  LineContent, XRadiusAttr, YRadiusAttr

derive gLexOrd FontDef, Span, LookupSpan, ImageTag, Set, CachedSpan, Deg, Rad,
  Maybe, LineContent, Slash

:: ClientState s =
  { textXSpanEnv    :: Map FontDef (Map String Real)
  , spanCache       :: Map (Set ImageTag) CachedSpan
  , uniqueIdCounter :: Int
  , editletId       :: String
  , onclicks        :: Map String (s -> s)
  , currState       :: s
  }

:: CachedSpan =
  { cachedGridSpans :: Maybe ([Span], [Span])
  , cachedImageSpan :: Maybe ImageSpan
  }

derive class iTask ClientState

viewImage :: !d !(Image ()) -> Task () | descr d
viewImage descr img = viewInformation descr [] (svgRenderer () (\_ -> img)) @! ()

mainSvgId :: !ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

addOnclicks :: ComponentId (JSObj svg) (Map String (s -> s)) *JSWorld -> *JSWorld | iTask s
addOnclicks cid svg onclicks world = 'DM'.foldrWithKey f world onclicks
  where
  f :: String (s -> s) *JSWorld -> *JSWorld | iTask s
  f elemCls sttf world
    # (elems, world)    = (svg `getElementsByClassName` elemCls) world
    # (numElems, world) = .? (elems .# "length") world
    | jsValToInt (numElems) < 1 = world
    # (elem, world)     = .? (elems .# "0") world
    # cb                = createEditletEventHandler (mkCB sttf) cid
    # (_, world)        = (elem `addEventListener` ("click", cb)) world
    = world
  mkCB :: (s -> s) String {JSObj JSEvent} (ClientState s) *JSWorld -> *(ClientState s, *JSWorld) | iTask s
  mkCB sttf _ _ clval world = ({clval & currState = sttf clval.currState}, world)

updateImageState :: !d  !(s -> Image s) !(s -> s) !s -> Task s | iTask s & descr d
updateImageState d toImage handleAction s  
    = updateInformation d [UpdateWith (\s -> svgRenderer s toImage) (\_ (Editlet s _ _) -> handleAction s)] s

updateSharedImageState :: !d  !(s -> Image s) !(s -> s) (Shared s) -> Task s | iTask s & descr d 
updateSharedImageState d toImage handleAction sharedState   
	= updateSharedInformation d [UpdateWith	(\s -> svgRenderer s toImage) (\_ (Editlet s _ _) -> handleAction s)] sharedState //@ (\(Editlet s` _ _) -> s`)

:: ActionState a s  = 	{ state		:: s
						, action	:: Maybe a
						}

derive class iTask ActionState

ifAction 				:: !(a -> Bool) !(a s -> s) !((ActionState a s) -> Task b) !(TaskValue (ActionState a s)) -> Maybe (Task b)
ifAction pred astos stotaskb (Value {ActionState|state=s,action=Just a} _) 
    | pred a 	= Just (stotaskb {ActionState|state = astos a s, action = Nothing})
    | otherwise = Nothing
ifAction _ _ _ _ = Nothing


appendSVG :: SVGElt (JSObj r) *JSWorld -> *(JSObj s, *JSWorld)
appendSVG (SVGElt            htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "svg"            htmlAttrs svgAttrs svgElts world
appendSVG (ClipPathElt       htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "clipPath"       htmlAttrs svgAttrs svgElts world
appendSVG (CircleElt         htmlAttrs svgAttrs        ) parent world = appendSVG` parent "circle"         htmlAttrs svgAttrs []      world
appendSVG (DefsElt           htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "defs"           htmlAttrs svgAttrs svgElts world
appendSVG (EllipseElt        htmlAttrs svgAttrs        ) parent world = appendSVG` parent "ellipse"        htmlAttrs svgAttrs []      world
appendSVG (GElt              htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "g"              htmlAttrs svgAttrs svgElts world
appendSVG (ImageElt          htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "image"          htmlAttrs svgAttrs svgElts world
appendSVG (LinearGradientElt htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "linearGradient" htmlAttrs svgAttrs svgElts world
appendSVG (LineElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "line"           htmlAttrs svgAttrs []      world
appendSVG (MarkerElt         htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "marker"         htmlAttrs svgAttrs svgElts world
appendSVG (PolygonElt        htmlAttrs svgAttrs        ) parent world = appendSVG` parent "polygon"        htmlAttrs svgAttrs []      world
appendSVG (PolylineElt       htmlAttrs svgAttrs        ) parent world = appendSVG` parent "polyline"       htmlAttrs svgAttrs []      world
appendSVG (RectElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "rect"           htmlAttrs svgAttrs []      world
appendSVG (RadialGradientElt htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "radialGradient" htmlAttrs svgAttrs svgElts world
appendSVG (StopElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "stop"           htmlAttrs svgAttrs []      world
appendSVG (TextElt           htmlAttrs svgAttrs str    ) parent world
  # (elem, world) = (jsDocument `createElementNS` (svgns, "text")) world
  # world         = setAttrs htmlAttrs elem world
  # world         = setAttrs svgAttrs elem world
  # world         = (elem .# "textContent" .= str) world
  = (elem, snd ((parent `appendChild` elem) world))

appendSVG` parent elemName htmlAttrs svgAttrs children world
  # (elem, world) = (jsDocument `createElementNS` (svgns, elemName)) world
  # world         = setAttrs htmlAttrs elem world
  # world         = setAttrs svgAttrs elem world
  # world         = foldl (\world child -> snd (appendSVG child elem world)) world children
  = (elem, snd ((parent `appendChild` elem) world))

svgns :== "http://www.w3.org/2000/svg"

setAttrs :: [a] (JSObj r) *JSWorld -> *JSWorld | toAttr a
setAttrs as obj world = foldr f world as
  where
  f attr world = snd ((obj `setAttribute` (toAttr attr)) world)

class toAttr a :: a -> (String, String)

// TODO This is rather hacky: rewrite serialization in Text.HTML to be able to
// support this as well
instance toAttr HtmlAttr where
  toAttr attr = (e1 % (1, size e1), e2 % (1, size e2))
    where
    lst = fromString (fst (serializeAttr attr "" 0))
    e1 = toString (takeWhile (\x -> x <> '=') lst)
    e2 = toString (filter (\x -> x <> '"') (dropWhile (\x -> x <> '=') lst))

// TODO This is rather hacky: rewrite serialization in Text.HTML to be able to
// support this as well
instance toAttr SVGAttr where
  toAttr attr = (e1 % (1, size e1), e2 % (1, size e2))
    where
    lst = fromString (fst (serializeSVGAttr attr "" 0))
    e1 = toString (takeWhile (\x -> x <> '=') lst)
    e2 = toString (filter (\x -> x <> '"') (dropWhile (\x -> x <> '=') lst))

svgRenderer :: !s !(s -> Image s) -> Editlet s (s, Image s, Map FontDef (Set String), Map (Set ImageTag) CachedSpan) | iTask s
svgRenderer origState state2Image = Editlet origState server client
  where
  server
    = { EditletServerDef
      | genUI   = genUI
      , defVal  = gDefault{|*|}
      , genDiff = genServerDiff
      , appDiff = appServerDiff
      }
  client
    = { EditletClientDef
      | updateUI = updateUI
      , defVal   = { textXSpanEnv    = 'DM'.newMap
                   , spanCache       = 'DM'.newMap
                   , onclicks        = 'DM'.newMap
                   , uniqueIdCounter = 0
                   , editletId       = ""
                   , currState       = gDefault{|*|}
                   }
      , genDiff  = genClientDiff
      , appDiff  = appClientDiff
      }
  genUI cid world
    = ({ ComponentHTML
       | width         = FlexSize
       , height        = FlexSize
       , html          = SvgTag [ IdAttr (mainSvgId cid)
                                , XmlnsAttr svgns
                                , XmlnsXlinkAttr "http://www.w3.org/1999/xlink"]
                                [VersionAttr "1.1"] []
       , eventHandlers = []
       }
       , world
      )

  updateUI cid (Just (_, img, fonts, spanCache)) clval world
    //# ((imgs, (imXSp, imYSp), _), (clval, world)) = toSVG img ({clval & editletId = cid}, world)
    # (clval, world) = calcTextLengths fonts (clval, world)
    # (syn, (clval, world)) = toSVG img ({clval & editletId = cid, spanCache = spanCache}, world)
    # (svg, world)   = getDomElement (mainSvgId cid) world
    # (imXSp, imYSp) = syn.clSyn_imageSpanReal
    # (_, world)     = (svg `setAttribute` ("height", toInt imYSp)) world
    # (_, world)     = (svg `setAttribute` ("width", toInt imXSp)) world
    # (_, world)     = (svg `setAttribute` ("viewBox", "0 0 " +++ toString (toInt imXSp) +++ " " +++ toString (toInt imYSp))) world
    # world          = (svg .# "innerHTML" .= "") world
    # (elem, world)  = appendSVG (GElt [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))] [] syn.clSyn_svgElts) svg world
    # world          = addOnclicks cid svg clval.onclicks world
    = (clval, world)

  updateUI _ _ clval world = (clval, world)

  genServerDiff _ y
    # (img, st) = fixSpans (state2Image y) {srvTaggedSpanEnv = 'DM'.newMap, didChange = False, srvCounter = 0, srvFonts = 'DM'.newMap}
    //= Just (y, overlay [(AtMiddleX, AtMiddleY)] [] [img] Nothing)
    = Just (y, img, st.srvFonts, st.srvTaggedSpanEnv)
    //= Nothing
  appServerDiff (st, _, _, _) _ = st

  genClientDiff x y
    | x.currState === y.currState = Nothing
    //| otherwise                   = Just (y.currState, state2Image y.currState, 'DM'.newMap)
    | otherwise                   = Just (y.currState, undef, undef, undef)
  appClientDiff (st, _, _, _) clval = {clval & currState = st}

(`getElementsByClassName`) obj args :== obj .# "getElementsByClassName" .$ args
(`addEventListener`)       obj args :== obj .# "addEventListener"       .$ args
(`setAttribute`)           obj args :== obj .# "setAttribute"           .$ args
(`createElementNS`)        obj args :== obj .# "createElementNS"        .$ args
(`appendChild`)            obj args :== obj .# "appendChild"            .$ args
(`removeChild`)            obj args :== obj .# "removeChild"            .$ args
(`getBBox`)                obj args :== obj .# "getBBox"                .$ args
(`getComputedTextLength`)  obj args :== obj .# "getComputedTextLength"  .$ args

getTextLength :: !FontDef !String !*(St m) -> *(Real, *(St m)) | iTask m
getTextLength fontdef str (clval, world)
  = case 'DM'.gGet fontdef clval.textXSpanEnv of
      Just strs = case 'DM'.get str strs of
                    Just tw -> (tw, (clval, world))
                    _       -> (0.0, (clval, world)) // TODO ?
      Nothing   = (0.0, (clval, world)) // TODO?
        //# (svg, world)   = (jsDocument `createElementNS` (svgns, "svg")) world
        //# (body, world)  = .? (jsDocument .# "body") world
        //# (_, world)     = (body `appendChild` svg) world
        //# (elem, world)  = (jsDocument `createElementNS` (svgns, "text")) world
        //# (fntSz, (clval, world)) =  evalSpan fontdef.fontyspan (clval, world)
        //# fontAttrs      = [ ("font-family",  fontdef.fontfamily)
                           //, ("font-size",    toString fntSz)
                           //, ("font-stretch", fontdef.fontstretch)
                           //, ("font-style",   fontdef.fontstyle)
                           //, ("font-variant", fontdef.fontvariant)
                           //, ("font-weight",  fontdef.fontweight)
                           //, ("x", "-10000")
                           //, ("y", "-10000") ]
        //# world          = foldr (\args world -> snd ((elem `setAttribute` args) world)) world fontAttrs
        //# world          = (elem .# "textContent" .= str) world
        //# (_, world)     = (svg `appendChild` elem) world
        //# (ctl, world)   = (elem `getComputedTextLength` ()) world
        //# (_, world)     = (svg `removeChild` elem) world
        //# (_, world)     = (body `removeChild` svg) world
        //# twidth         = jsValToReal ctl
        //= (twidth, ({clval & textXSpanEnv = 'DM'.gPut (fontdef, str) twidth clval.textXSpanEnv}, world))

calcTextLengths :: !(Map FontDef (Set String)) !*(St m) -> *(St m) | iTask m
calcTextLengths fontdefs (clval, world)
  # (svg, world)          = (jsDocument `createElementNS` (svgns, "svg")) world
  # (body, world)         = .? (jsDocument .# "body") world
  # (_, world)            = (body `appendChild` svg) world
  # (elem, world)         = (jsDocument `createElementNS` (svgns, "text")) world
  # (_, world)            = (svg `appendChild` elem) world
  # (res, (clval, world)) = 'DM'.foldrWithKey (f elem) ('DM'.newMap, (clval, world)) fontdefs
  # (_, world)            = (svg `removeChild` elem) world
  # (_, world)            = (body `removeChild` svg) world
  = ({clval & textXSpanEnv = res}, world)
  where
  f elem fontdef strs (acc, (clval, world))
    # (fntSz, (clval, world)) = evalSpan fontdef.fontyspan (clval, world)
    # fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                    , ("font-size",    toString fntSz)
                    , ("font-stretch", fontdef.fontstretch)
                    , ("font-style",   fontdef.fontstyle)
                    , ("font-variant", fontdef.fontvariant)
                    , ("font-weight",  fontdef.fontweight)
                    , ("x", "-10000")
                    , ("y", "-10000") ]
    # world       = foldr (\args world -> snd ((elem `setAttribute` args) world)) world fontAttrs
    # (ws, world) = 'DS'.fold (g elem) ('DM'.newMap, world) strs
    = ('DM'.gPut fontdef ws acc, (clval, world))
  g elem str (acc, world)
    # world        = (elem .# "textContent" .= str) world
    # (ctl, world) = (elem `getComputedTextLength` ()) world
    = ('DM'.put str (jsValToReal ctl) acc, world)

:: *St m :== *(ClientState m, *JSWorld)

:: ClSt m a :== State (St m) a

:: SrvSt a :== State ServerState a

:: ServerState =
  { srvTaggedSpanEnv :: Map (Set ImageTag) CachedSpan
  , didChange        :: Bool
  , srvCounter       :: Int
  , srvFonts         :: Map FontDef (Set String)
  }

class nextNo a :: a -> (Int, a)

instance nextNo (ClientState s) where
  nextNo st = (st.uniqueIdCounter, {st & uniqueIdCounter = st.uniqueIdCounter + 1})

instance nextNo ServerState where
  nextNo st = (st.srvCounter, {st & srvCounter = st.srvCounter + 1})

:: State s a :== s -> *(a, s)

:: ErrorMessage :== String

:: FixSpansContentSyn s =
  { contSynImageContent     :: ImageContent s
  , contSynTotalSpan        :: ImageSpan
  , contSynOffsetCorrection :: ImageOffset
  , contSynConnectors       :: [Connector]
  }

runM :: !(State s a) s -> (a, s)
runM m st = m st

sequence :: ![.st -> .(a, .st)] -> (.st -> .([a], .st))
sequence ms = mapSt id ms

addCachedSpan :: !(CachedSpan -> CachedSpan) !(Set ImageTag) !ServerState -> ServerState
addCachedSpan f imTas st=:{srvTaggedSpanEnv}
  | 'DS'.null imTas = st
  | otherwise
    # r = fromMaybe { cachedGridSpans = Nothing, cachedImageSpan = Nothing } ('DM'.get imTas srvTaggedSpanEnv)
    = { st & srvTaggedSpanEnv = 'DM'.put imTas (f r) srvTaggedSpanEnv }

cacheImageSpan :: !(Set ImageTag) !ImageSpan !ServerState -> ServerState
cacheImageSpan imTas sp st = addCachedSpan (\r -> {r & cachedImageSpan = Just sp}) imTas st

//cacheGridSpans :: !(Set ImageTag) !{Span} !{Span} !ServerState -> ServerState
cacheGridSpans imTas xsps ysps st = addCachedSpan (\r -> {r & cachedGridSpans = Just (xsps, ysps)}) imTas st

rectAnchors :: !ImageSpan -> [Connector]
rectAnchors (w, h) = [(zero, zero), (zero, h /. 2.0), (zero, h), (w /. 2.0, zero), (w /. 2.0, h), (w, zero), (w, h /. 2.0), (w, h)]

ellipseAnchors :: !ImageSpan -> [Connector]
ellipseAnchors (w, h) = [(rx, zero), (rx *. 2.0, ry), (rx, ry *. 2.0), (zero, ry)] ++ concatMap (\th -> [(rSin th ry, rCos th rx), (~(rSin th ry), rCos th rx), (rSin th ry, ~(rCos th rx)), (~(rSin th ry), ~(rCos th rx))]) [22.5, 45.0, 67.5]
  where
  rx        = w /. 2.0
  ry        = h /. 2.0
  rSin th r = r *. sin th
  rCos th r = r *. cos th
import StdDebug
// TODO : Detect divergence due to lookups and return an Either ErrorMessage (Image s), instead of just an Image s
fixSpans :: !(Image s) -> SrvSt (Image s) | iTask s
fixSpans img = go
  where
  go st
    # (img, st)    = imageCata fixSpansAllAlgs img st
    | st.didChange = trace_n "fix" fixSpans img {st & didChange = False}
    | otherwise    = ret img st
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
  fixSpansImageAlgs :: ImageAlg ([ImageTransform] (Set ImageTag) -> (SrvSt (FixSpansContentSyn s)))
                                (SrvSt (ImageAttr s))
                                (SrvSt ImageTransform)
                                (SrvSt Span)
                                (SrvSt (Image s)) | iTask s
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: !([ImageTransform] (Set ImageTag) -> (SrvSt (FixSpansContentSyn s)))
               ![SrvSt (ImageAttr s)]
               ![SrvSt ImageTransform]
               !(Set ImageTag) !(SrvSt Span, SrvSt Span)
               !(SrvSt Span, SrvSt Span, SrvSt Span, SrvSt Span)
               !(SrvSt Span, SrvSt Span) ![(SrvSt Span, SrvSt Span)]
            -> SrvSt (Image s) | iTask s
    mkImage imCo imAts imTrs imTas _ (m1, m2, m3, m4) _ _ = go
      where
      go st
        # (imAts, st)   = sequence imAts st
        # (imTrs, st)   = sequence imTrs st
        # (contSyn, st) = imCo imTrs imTas st
        # (m1, st)      = m1 st
        # (m2, st)      = m2 st
        # (m3, st)      = m3 st
        # (m4, st)      = m4 st
        # (xsp, ysp)    = contSyn.contSynTotalSpan
        # (xsp, ysp)    = (xsp + m2 + m4, ysp + m1 + m3)
        # st            = cacheImageSpan imTas (xsp, ysp) st
        # (no, st)      = nextNo st
        = ret (tag [ImageTagSystem no]
              { Image
              | content             = contSyn.contSynImageContent
              , attribs             = imAts
              , transform           = imTrs
              , tags                = imTas
              , totalSpan           = (xsp, ysp)
              , margin              = (m1, m2, m3, m4)
              , transformCorrection = contSyn.contSynOffsetCorrection
              , connectors          = contSyn.contSynConnectors
              })
              st

  fixSpansImageContentAlgs :: ImageContentAlg (ImageSpan [ImageTransform] -> SrvSt (FixSpansContentSyn s))
                                              (SrvSt ImageSpan)
                                              ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s))
                                              ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s))
                                              ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpan [ImageTransform] -> SrvSt (FixSpansContentSyn s))
               !(SrvSt ImageSpan) ![ImageTransform] !(Set ImageTag)
            -> SrvSt (FixSpansContentSyn s) | iTask s
    mkBasic baIm imSp imTrs imTas
      =        imSp `b`
      \imSp -> baIm imSp imTrs
  fixSpansImageAttrAlgs :: ImageAttrAlg s (SrvSt (ImageAttr s)) | iTask s
  fixSpansImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = ret o ImageStrokeAttr
    , imageAttrStrokeWidthAttrAlg   = ret o ImageStrokeWidthAttr
    , imageAttrXRadiusAttrAlg       = ret o ImageXRadiusAttr
    , imageAttrYRadiusAttrAlg       = ret o ImageYRadiusAttr
    , imageAttrStrokeOpacityAttrAlg = ret o ImageStrokeOpacityAttr
    , imageAttrFillAttrAlg          = ret o ImageFillAttr
    , imageAttrFillOpacityAttrAlg   = ret o ImageFillOpacityAttr
    , imageAttrOnClickAttrAlg       = ret o ImageOnClickAttr
    }
  fixSpansImageTransformAlgs :: ImageTransformAlg Deg (SrvSt Span) (SrvSt ImageTransform)
  fixSpansImageTransformAlgs =
    { imageTransformRotateImageAlg = ret o RotateImage
    , imageTransformSkewXImageAlg  = ret o SkewXImage
    , imageTransformSkewYImageAlg  = ret o SkewYImage
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitDim FitXImage
    , imageTransformFitYImageAlg   = mkFitDim FitXImage
    }
    where
    mkFitImage :: !(SrvSt Span) !(SrvSt Span) -> SrvSt ImageTransform
    mkFitImage sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (FitImage sp1 sp2)
    mkFitDim :: !(Span -> ImageTransform) !(SrvSt Span) -> SrvSt ImageTransform
    mkFitDim ctr sp
      =      sp `b`
      \sp -> ret (ctr sp)
  fixSpansImageSpanAlgs :: ImageSpanAlg (SrvSt Span) (SrvSt ImageSpan)
  fixSpansImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: !(SrvSt Span) !(SrvSt Span) -> SrvSt ImageSpan
    mkSpan xspan yspan
      =         xspan `b`
      \xspan -> yspan `b`
      \yspan -> ret (xspan, yspan)
  fixSpansBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageTransform] -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg   = mkEmptyImage
    , basicImageTextImageAlg    = mkTextImage
    , basicImageCircleImageAlg  = mkCircleImage
    , basicImageRectImageAlg    = mkRectImage
    , basicImageEllipseImageAlg = mkEllipseImage
    }
    where
    mkEmptyImage       imSp imTrs = mkSpan EmptyImage         imSp imTrs rectAnchors
    mkRectImage        imSp imTrs = mkSpan RectImage          imSp imTrs rectAnchors
    mkTextImage fd str imSp imTrs = mkSpan (TextImage fd str) imSp imTrs rectAnchors
    mkCircleImage      imSp imTrs = mkSpan CircleImage        imSp imTrs ellipseAnchors
    mkEllipseImage     imSp imTrs = mkSpan EllipseImage       imSp imTrs ellipseAnchors

    mkSpan :: !BasicImage !ImageSpan ![ImageTransform] !(ImageSpan -> [Connector]) -> SrvSt (FixSpansContentSyn s) | iTask s
    mkSpan ctor imSp imTrs f
      # (imSp`, imOff) = applyTransforms imTrs imSp
      = ret { contSynImageContent     = Basic ctor imSp
            , contSynTotalSpan        = imSp`
            , contSynOffsetCorrection = imOff
            , contSynConnectors       = f imSp }

  fixSpansLineImageAlgs :: LineImageAlg (SrvSt ImageSpan)
                                        (SrvSt (Markers s))
                                        (SrvSt LineContent)
                                        ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansLineImageAlgs =
    { lineImageLineImageAlg = mkLine
    }
    where
    mkLine :: !(SrvSt ImageSpan) !(Maybe (SrvSt (Markers s))) !(SrvSt LineContent)
              ![ImageTransform] !(Set ImageTag)
           -> SrvSt (FixSpansContentSyn s) | iTask s
    mkLine imSp mmarkers liCo imTrs imTas
      =            imSp `b`
      \imSp     -> evalMaybe mmarkers `b`
      \mmarkers -> liCo `b`
      \liCo     -> let (imSp`, imOff) = applyTransforms imTrs imSp
                   in  ret { contSynImageContent     = Line { LineImage
                                                            | lineSpan    = imSp
                                                            , markers     = mmarkers
                                                            , lineContent = liCo }
                           , contSynTotalSpan        = imSp`
                           , contSynOffsetCorrection = imOff
                           , contSynConnectors       = [] /* TODO Connectors? */ }
  fixSpansMarkersAlgs :: MarkersAlg (SrvSt (Image s)) (SrvSt (Markers s)) | iTask s
  fixSpansMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (SrvSt (Image s)))
                 !(Maybe (SrvSt (Image s)))
                 !(Maybe (SrvSt (Image s)))
              -> SrvSt (Markers s) | iTask s
    mkMarkers mStart mMid mEnd
      =          evalMaybe mStart `b`
      \mStart -> evalMaybe mMid `b`
      \mMid   -> evalMaybe mEnd `b`
      \mEnd   -> ret { markerStart = mStart
                     , markerMid   = mMid
                     , markerEnd   = mEnd }
  fixSpansLineContentAlgs :: LineContentAlg (SrvSt Span) (SrvSt LineContent)
  fixSpansLineContentAlgs =
    { lineContentSimpleLineImageAlg = \sl     -> ret (SimpleLineImage sl)
    , lineContentPolygonImageAlg    = \coords -> evalOffsets coords `b`
                                      \coords -> ret (PolygonImage coords)
    , lineContentPolylineImageAlg   = \coords -> evalOffsets coords `b`
                                      \coords -> ret (PolylineImage coords)
    }
  fixSpansCompositeImageAlgs :: CompositeImageAlg (SrvSt Span)
                                                  (SrvSt (Image s))
                                                  ([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan, [ImageOffset]))
                                                  ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: ![(SrvSt Span, SrvSt Span)]
                        !(Maybe (SrvSt (Image s)))
                        !([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan, [ImageOffset]))
                        !(Set (Set ImageTag, Set ImageTag))
                        ![ImageTransform] !(Set ImageTag)
                     -> SrvSt (FixSpansContentSyn s) | iTask s
    mkCompositeImage offsets host compose edges imTrs imTas = go
      where
      go st
        # (offsets, st) = evalOffsets offsets st
        # (host, st)    = evalMaybe host st
        # ((compose, composeSpan, offsets), st) = compose offsets host imTrs imTas st
        # (host, span, conns) = case host of
                                  Just hostImg
                                     -> (Just hostImg, hostImg.totalSpan, hostImg.connectors)
                                  _  -> (Nothing, composeSpan
                                        , rectAnchors composeSpan) // TODO Somehow this tends to break
        # (span, corr)  = applyTransforms imTrs span // TODO Transform the anchor points as well
        = ret { contSynImageContent     = Composite { CompositeImage
                                                    | offsets = offsets
                                                    , host    = host
                                                    , compose = compose
                                                    , edges   = edges }
              , contSynTotalSpan        = span
              , contSynOffsetCorrection = corr
              , contSynConnectors       = conns } st
  fixSpansComposeAlgs :: ComposeAlg (SrvSt (Image s))
                                    ([ImageOffset] (Maybe (Image s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan, [ImageOffset])) | iTask s
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![ImageAlign] ![[SrvSt (Image s)]]
              ![ImageOffset] !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag)
           -> SrvSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
    mkGrid (numcols, numrows) ias imgss offsets mbhost imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr seqImgsGrid ([], st) imgss
        # imgss       = reverse (map reverse imgss) // TODO This is more or less a hack... why do we need this?
        # spanss      = map (map (\x -> x.totalSpan)) imgss
        # maxXSpans   = map (maxSpan o map fst) (transpose spanss)
        # maxYSpans   = map (maxSpan o map snd) spanss
        # (tag, st)   = nextNo st
        # sysTags     = 'DS'.singleton (ImageTagSystem tag)
        # gridSpan    = maybe ( foldr (\n acc -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) [0..numcols - 1]
                              , foldr (\n acc -> LookupSpan (RowYSpan sysTags n) + acc) (px 0.0) [0..numrows - 1]
                              )
                              (\x -> x.totalSpan) mbhost
        # offsets     = flatten (calculateGridOffsets maxXSpans maxYSpans imgss offsets)
        //# xspansArr   = {xsp \\ xsp <- maxXSpans}
        //# yspansArr   = {ysp \\ ysp <- maxYSpans}
        # xspansArr   = maxXSpans
        # yspansArr   = maxYSpans
        //# offsets     = calculateGridOffsets xspansArr yspansArr imgss offsets
        # st          = cacheGridSpans ('DS'.union sysTags imTas) xspansArr yspansArr st
        = ret ( AsCollage (flatten imgss)
              , gridSpan
              , offsets) st
      //calculateGridOffsets xspansArr yspansArr imgss offsets
        //= foldr f ([], px 0.0, px 0.0) (zip4 (flatten imgss) offsets ias [(c, r) \\ c <- [0 .. size xspansArr - 1], r <- [0 .. size yspansArr - 1]])
        //where
        //f img=:{totalSpan, transformCorrection = (tfXCorr, tfYCorr)} (manXOff, manYOff) align (column, row) (acc, xoff, yoff)
        ////f img=:{totalSpan, transformCorrection = (tfXCorr, tfYCorr)} (manXOff, manYOff) align (column, row)
          //# (alignXOff, alignYOff) = calcAlignOffset xspansArr.[column] yspansArr.[row] totalSpan align
          ////= ( alignXOff + (LookupSpan (xoff + manXOff + tfXCorr
              ////alignYOff + yoff + manYOff + tfYCorr)
          //= ([( alignXOff + xoff + manXOff + tfXCorr
            //, alignYOff + yoff + manYOff + tfYCorr):acc], xoff + xspansArr.[column], yoff + yspansArr.[row])

        //= let (x, _, _, _) = foldr mkRows ([], px 0.0, ias, offsets, 0) imgss
          //in  x
        //where
        //mkRows cellXSpans (imgs, cellYSpan) (acc, yoff, aligns, offsets)
          //# imgsLength = length imgs
          //= ( [fst (foldr (mkCols cellYSpan yoff) ([], px 0.0) (zip4 imgs cellXSpans (take imgsLength aligns) (take imgsLength offsets))) : acc]
            //, yoff + cellYSpan, drop imgsLength aligns, drop imgsLength offsets)
        //mkCols cellYSpan yoff (img=:{totalSpan, transformCorrection = (tfXCorr, tfYCorr)}, cellXSpan, align, (manXOff, manYOff)) (acc, xoff)
          //# (alignXOff, alignYOff) = calcAlignOffset cellXSpan cellYSpan totalSpan align
          //= ([( alignXOff + xoff + manXOff + tfXCorr
              //, alignYOff + yoff + manYOff + tfYCorr):acc], xoff + cellXSpan)
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

    mkCollage :: ![SrvSt (Image s)] ![ImageOffset] !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag) -> SrvSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
    mkCollage imgs offsets mbhost imTrs imTas
      =        sequence imgs `b`
      \imgs -> ret ( AsCollage imgs
                   , maybe (calculateComposedSpan (map (\x -> x.totalSpan) imgs) offsets) (\x -> x.totalSpan) mbhost
                   , offsets)
    mkOverlay :: ![ImageAlign] ![SrvSt (Image s)] ![ImageOffset] !(Maybe (Image s)) ![ImageTransform] !(Set ImageTag) -> SrvSt (Compose s, ImageSpan, [ImageOffset]) | iTask s
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
              , offsets ) st
        where
        addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)
  fixSpansSpanAlgs :: SpanAlg (SrvSt Span) (SrvSt Span)
  fixSpansSpanAlgs =
    { spanPxSpanAlg     = \r   -> ret (PxSpan r)
    , spanLookupSpanAlg = ($)
    , spanAddSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x + y)
    , spanSubSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (x - y)
    , spanMulSpanAlg    = \x y -> x `b` \x ->             ret (x *. y)
    , spanDivSpanAlg    = \x y -> x `b` \x ->             ret (x /. y)
    , spanAbsSpanAlg    = \x   -> x `b` \x ->             ret (abs x)
    , spanMinSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (minSpan xs)
    , spanMaxSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (maxSpan xs)
    }
  fixSpansLookupSpanAlgs :: LookupSpanAlg (SrvSt Span)
  fixSpansLookupSpanAlgs =
    //{ lookupSpanColumnXSpanAlg  = mkImageGridSpan (\xss n -> maxSpan (map fst (transpose xss !! n))) ColumnXSpan
    { lookupSpanColumnXSpanAlg  = \ts n -> ret (trace_n ("ColumnXSpan " +++ toString n) LookupSpan (ColumnXSpan ts n))
    //, lookupSpanRowYSpanAlg     = mkImageGridSpan (\xss n -> maxSpan (map snd (xss !! n))) RowYSpan
    , lookupSpanRowYSpanAlg     = \ts n -> ret (trace_n ("RowYSpan " +++ toString n) LookupSpan (RowYSpan ts n))
    //, lookupSpanImageXSpanAlg   = mkImageSpan fst ImageXSpan
    , lookupSpanImageXSpanAlg   = \ts     -> ret (LookupSpan (ImageXSpan ts))
    //, lookupSpanImageYSpanAlg   = mkImageSpan snd ImageYSpan
    , lookupSpanImageYSpanAlg   = \ts     -> ret (LookupSpan (ImageYSpan ts))
    , lookupSpanDescentYSpanAlg = \fd     -> ret (LookupSpan (DescentYSpan fd))
    , lookupSpanExYSpanAlg      = \fd     -> ret (LookupSpan (ExYSpan fd))
    , lookupSpanTextXSpanAlg    = mkTextLU
    }
    where
    mkTextLU fd str st
      # strs = case 'DM'.gGet fd st.srvFonts of
                 Just fs -> fs
                 _       -> 'DS'.newSet
      = ret (LookupSpan (TextXSpan fd str)) { st & srvFonts = 'DM'.gPut fd ('DS'.insert str strs) st.srvFonts }

    //lookupTags :: !(Set ImageTag) -> SrvSt (Maybe CachedSpan)
    //lookupTags ts
      //| 'DS'.null ts = ret Nothing
      //| otherwise    = go
      //where
      //go srv
        //= case 'DM'.elems ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) srv.srvTaggedSpanEnv) of
            //[x:_] -> (Just x, {srv & didChange = True})
            //_     -> (Nothing, srv)

    //mkImageSpan :: !(ImageSpan -> Span) !((Set ImageTag) -> LookupSpan) !(Set ImageTag) -> SrvSt Span
    //mkImageSpan f c ts
      //=        lookupTags ts `b`
      //\luts -> ret (case luts of
                      //Just {cachedImageSpan=Just xs} -> f xs
                      //_                              -> LookupSpan (c ts))

    //mkImageGridSpan :: !([[ImageSpan]] Int -> Span) !((Set ImageTag) Int -> LookupSpan) !(Set ImageTag) Int -> SrvSt Span
    //mkImageGridSpan f c ts n
      //=        lookupTags ts `b`
      //\luts -> ret (case luts of
                      //Just csp=:{cachedGridSpans=Just xss} -> f xss n
                      //_                                    -> LookupSpan (c ts n))

seqImgsGrid imgs (acc, st) :== (sequence imgs `b` \imgs -> ret [imgs:acc]) st

:: ImageSpanReal :== (Real, Real)

:: ImageOffsetReal :== (Real, Real)

:: ToSVGSyn =
  { clSyn_svgElts       :: [SVGElt]
  , clSyn_imageSpanReal :: ImageSpanReal
  , clSyn_connectors    :: [ConnectorReal]
  }

:: ConnectorReal :== (Real, Real)

mkClSyn = { clSyn_svgElts         = []
          , clSyn_imageSpanReal   = (0.0, 0.0)
          , clSyn_connectors      = []
          }

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkOnClickId :: !String !Int -> String
mkOnClickId editletId uniqId = "onClickId-" +++ editletId +++ toString uniqId

getSvgAttrs :: ![Either HtmlAttr SVGAttr] -> [SVGAttr]
getSvgAttrs as = [a \\ Right a <- as]

getHtmlAttrs :: ![Either HtmlAttr SVGAttr] -> [HtmlAttr]
getHtmlAttrs as = [a \\ Left a <- as]

toSVG :: !(Image s) -> ClSt s ToSVGSyn | iTask s
toSVG img = imageCata toSVGAllAlgs img
  where
  toSVGAllAlgs =
    { imageAlgs          = toSVGImageAlgs
    , imageContentAlgs   = toSVGImageContentAlgs
    , imageAttrAlgs      = toSVGImageAttrAlgs
    , imageTransformAlgs = toSVGImageTransformAlgs
    , imageSpanAlgs      = toSVGImageSpanAlgs
    , basicImageAlgs     = toSVGBasicImageAlgs
    , lineImageAlgs      = toSVGLineImageAlgs
    , markersAlgs        = toSVGMarkersAlgs
    , lineContentAlgs    = toSVGLineContentAlgs
    , compositeImageAlgs = toSVGCompositeImageAlgs
    , composeAlgs        = toSVGComposeAlgs
    , spanAlgs           = toSVGSpanAlgs
    , lookupSpanAlgs     = toSVGLookupSpanAlgs
    }
  toSVGImageAlgs :: ImageAlg (ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                             (ClSt s (Either HtmlAttr SVGAttr))
                             (ImageSpanReal -> ClSt s (SVGTransform, ImageTransform))
                             (ClSt s Real)
                             (ClSt s ToSVGSyn) | iTask s
  toSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage :: !(ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
               ![ClSt s (Either HtmlAttr SVGAttr)]
               ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)]
               !(Set ImageTag) !(ClSt s Real, ClSt s Real)
               !(ClSt s Real, ClSt s Real, ClSt s Real, ClSt s Real)
               !(ClSt s Real, ClSt s Real) ![(ClSt s Real, ClSt s Real)]
            -> ClSt s ToSVGSyn | iTask s
    mkImage imCo imAts imTrs imTas (txsp, tysp) (m1, m2, _, _) _ conns
      =         sequence imAts `b`
      \imAts -> txsp `b`
      \txsp  -> tysp `b`
      \tysp  -> imCo (txsp, tysp) imAts imTrs imTas `b`
      \syn   -> m1 `b`
      \m1    -> m2 `b`
      \m2    -> evalOffsets conns `b`
      \conns -> ret { mkClSyn
                    & clSyn_svgElts       = mkGroup [] (mkTransformTranslateAttr (m1, m2)) syn.clSyn_svgElts
                    , clSyn_imageSpanReal = (txsp, tysp)
                    , clSyn_connectors    = conns}

  toSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ClSt s ImageSpanReal)
                                           (ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpanReal [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
               !(ClSt s ImageSpanReal) !ImageSpanReal ![Either HtmlAttr SVGAttr]
               ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
            -> ClSt s ToSVGSyn | iTask s
    mkBasic baIm imSp totalSpan imAts imTrs imTas
      =         imSp `b`
      \imSp  -> sequence (map (\f -> f imSp) imTrs) `b`
      \imTrs -> baIm imSp imAts imTrs imTas
  toSVGImageAttrAlgs :: ImageAttrAlg s (ClSt s (Either HtmlAttr SVGAttr)) | iTask s
  toSVGImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr -> ret (Right (StrokeAttr (PaintColor attr.stroke Nothing)))
    , imageAttrStrokeWidthAttrAlg   = \attr -> mkStrokeWidth attr
    , imageAttrXRadiusAttrAlg       = \attr -> evalSpan attr.xradius `b` \r -> ret (Right (RxAttr (toString r, PX)))
    , imageAttrYRadiusAttrAlg       = \attr -> evalSpan attr.yradius `b` \r -> ret (Right (RyAttr (toString r, PX)))
    , imageAttrStrokeOpacityAttrAlg = \attr -> ret (Right (StrokeOpacityAttr (toString attr.opacity)))
    , imageAttrFillAttrAlg          = \attr -> ret (Right (FillAttr (PaintColor attr.fill Nothing)))
    , imageAttrFillOpacityAttrAlg   = \attr -> ret (Right (FillOpacityAttr (FillOpacity (toString attr.opacity))))
    , imageAttrOnClickAttrAlg       = mkOnClick
    }
    where
    mkStrokeWidth :: !(StrokeWidthAttr s) -> ClSt s (Either HtmlAttr SVGAttr) | iTask s
    mkStrokeWidth {strokewidth}
      =     evalSpan strokewidth `b`
      \w -> ret (Right (StrokeWidthAttr (StrokeWidthLength (toString w, PX))))
    mkOnClick :: !(OnClickAttr s) -> ClSt s (Either HtmlAttr SVGAttr) | iTask s
    mkOnClick {onclick} = go
      where
      go (clval, world)
        # uniqId = clval.uniqueIdCounter
        # ocId   = mkOnClickId clval.editletId uniqId
        # st     = ({ clval
                    & uniqueIdCounter = uniqId + 1
                    , onclicks = 'DM'.put ocId onclick clval.onclicks}, world)
        = ret (Left (ClassAttr ocId)) st
  toSVGImageTransformAlgs :: ImageTransformAlg Deg (ClSt s Real) (ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)) | iTask s
  toSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    (xsp, ysp) -> ret (RotateTransform (toString (toReal imAn)) (Just (toString (xsp / 2.0), toString (ysp / 2.0))), RotateImage imAn)
    , imageTransformSkewXImageAlg  = \imAn    (xsp, _)   -> ret (SkewXTransform (toString (toReal imAn)), SkewXImage imAn)
    , imageTransformSkewYImageAlg  = \imAn    (_, ysp)   -> ret (SkewYTransform (toString (toReal imAn)), SkewYImage imAn)
    , imageTransformFitImageAlg    = \sp1 sp2 (xsp, ysp) -> sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (ScaleTransform (toString (sp1 / xsp)) (toString (sp2 / ysp)), FitImage (px sp1) (px sp2))
    , imageTransformFitXImageAlg   = \sp      (xsp, _)   -> sp  `b` \sp  -> ret (ScaleTransform (toString (sp / xsp)) "1.0", FitXImage (px sp))
    , imageTransformFitYImageAlg   = \sp      (_, ysp)   -> sp  `b` \sp  -> ret (ScaleTransform "1.0" (toString (sp / ysp)), FitYImage (px sp))
    }
  toSVGImageSpanAlgs :: ImageSpanAlg (ClSt s Real) (ClSt s ImageSpanReal) | iTask s
  toSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan :: !(ClSt s Real) !(ClSt s Real) -> ClSt s ImageSpanReal
    mkImageSpan sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (sp1, sp2)
  toSVGBasicImageAlgs :: BasicImageAlg (ImageSpanReal [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGBasicImageAlgs =
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                 -> ClSt s ToSVGSyn | iTask s
    mkEmptyImage imSp imAts imTrs imTas
      = ret { mkClSyn & clSyn_svgElts = mkGroup (mkWH imSp ++ getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs)) [] }
    mkTextImage :: !FontDef !String !ImageSpanReal  ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> ClSt s ToSVGSyn | iTask s
    mkTextImage fd str imSp imAts imTrs imTas
      =      evalSpan fd.fontyspan `b`
      \sp -> ret { mkClSyn & clSyn_svgElts = [TextElt (getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs) ++ fontAttrs sp) str] }
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
    mkRectImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> ClSt s ToSVGSyn | iTask s
    mkRectImage imSp imAts imTrs imTas
      = ret { mkClSyn & clSyn_svgElts = [RectElt (mkWH imSp ++ getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs))] }
    mkCircleImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                  -> ClSt s ToSVGSyn | iTask s
    mkCircleImage imSp=:(imXSp`, _) imAts imTrs imTas
      # r = imXSp` / 2.0
      = ret { mkClSyn & clSyn_svgElts = [CircleElt (getHtmlAttrs imAts)
                                          [ RAttr (toString r, PX), CxAttr (toString r, PX)
                                          , CyAttr (toString r, PX) : (getSvgAttrs (mkAttrs imAts imTrs)) ]] }
    mkEllipseImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                   -> ClSt s ToSVGSyn | iTask s
    mkEllipseImage imSp=:(imXSp, imYSp) imAts imTrs imTas
      = ret { mkClSyn & clSyn_svgElts = [EllipseElt (getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs) ++
                                          [ RxAttr (toString (imXSp / 2.0), PX), RyAttr (toString (imYSp / 2.0), PX)
                                          , CxAttr (toString (imXSp / 2.0), PX), CyAttr (toString (imYSp / 2.0), PX)])] }

    mkWH :: !ImageSpanReal -> [HtmlAttr]
    mkWH (imXSp, imYSp) = [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))]

  // TODO Type signature
  toSVGLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage lineSpan mmarkers lineContent totalSpan imAts imTrs imTas
      =            lineSpan `b`
      \lineSpan -> evalMaybe mmarkers `b`
      \mmarkers -> sequence (map (\f -> f lineSpan) imTrs) `b`
      \imTrs    -> lineContent lineSpan mmarkers imAts imTrs imTas

  toSVGMarkersAlgs :: MarkersAlg (ClSt s ToSVGSyn) (ClSt s (Maybe ToSVGSyn, Maybe ToSVGSyn, Maybe ToSVGSyn)) | iTask s
  toSVGMarkersAlgs =
    { markersMarkersAlg = \m1 m2 m3 -> evalMaybe m1 `b`
                          \m1 ->       evalMaybe m2 `b`
                          \m2 ->       evalMaybe m3 `b`
                          \m3 ->       ret (m1, m2, m3)
    }
  toSVGLineContentAlgs :: LineContentAlg (ClSt s Real)
                                         (ImageSpanReal (Maybe (Maybe ToSVGSyn, Maybe ToSVGSyn, Maybe ToSVGSyn)) [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGLineContentAlgs =
    { lineContentSimpleLineImageAlg = mkLineImage
    , lineContentPolygonImageAlg    = mkPolygonImage
    , lineContentPolylineImageAlg   = mkPolylineImage
    }
    where
    mkLineImage :: !Slash !ImageSpanReal !(Maybe (Maybe ToSVGSyn, Maybe ToSVGSyn, Maybe ToSVGSyn))
                   ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> ClSt s ToSVGSyn | iTask s
    mkLineImage sl sp mmarkers imAts imTrs imTas
      = mkLine LineElt (getSvgAttrs (mkAttrs imAts imTrs) ++ mkLineAttrs sl sp) sp mmarkers
      where
      mkLineAttrs slash (xspan, yspan)
        # (y1, y2) = case slash of
                       Slash     -> (toString yspan, "0.0")
                       Backslash -> ("0.0", toString yspan)
        = [ X1Attr ("0.0", PX), X2Attr (toString xspan, PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    mkPolygonImage :: ![(ClSt s Real, ClSt s Real)] !ImageSpanReal
                      !(Maybe (Maybe ToSVGSyn, Maybe ToSVGSyn, Maybe ToSVGSyn))
                      ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)]
                      !(Set ImageTag)
                   -> ClSt s ToSVGSyn | iTask s
    mkPolygonImage points sp mmarkers imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> mkLine PolygonElt [PointsAttr (map (\(x, y) -> (toString x, toString y)) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers
    mkPolylineImage :: ![(ClSt s Real, ClSt s Real)] !ImageSpanReal
                       !(Maybe (Maybe ToSVGSyn, Maybe ToSVGSyn, Maybe ToSVGSyn))
                       ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)]
                       !(Set ImageTag)
                    -> ClSt s ToSVGSyn | iTask s
    mkPolylineImage points sp mmarkers imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> mkLine PolylineElt [PointsAttr (map (\(x, y) -> (toString x, toString y)) offsets) : getSvgAttrs (mkAttrs imAts imTrs)] sp mmarkers

    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) (clval, world)
      # m1Id = mkMarkerId clval.editletId clval.uniqueIdCounter
      # m2Id = mkMarkerId clval.editletId (clval.uniqueIdCounter + 1)
      # m3Id = mkMarkerId clval.editletId (clval.uniqueIdCounter + 2)
      # markersAndIds = [(m, i) \\ Just (m, i) <- [ mkMarkerAndId mmStart m1Id MarkerStartAttr
                                                  , mkMarkerAndId mmMid m2Id MarkerMidAttr
                                                  , mkMarkerAndId mmEnd m3Id MarkerEndAttr ]]
      = ret {mkClSyn & clSyn_svgElts = [constr [] (map snd markersAndIds ++ atts), DefsElt [] [] (map fst markersAndIds)]}
            ({ clval & uniqueIdCounter = clval.uniqueIdCounter + 3 }, world) // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId (Just {clSyn_svgElts, clSyn_imageSpanReal = (w, h)}) mid posAttr
        = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto" // TODO Do something with offset?
                                        , ViewBoxAttr "0" "0" (toString (toInt w)) (toString (toInt h))
                                        , RefXAttr (toString (toInt w), PX)
                                        , RefYAttr (toString (toInt (h / 2.0)), PX)
                                        , MarkerHeightAttr (toString (toInt h), PX)
                                        , MarkerWidthAttr (toString (toInt w), PX)
                                        ] clSyn_svgElts
               , posAttr ("url(#" +++ mid +++ ")"))
      mkMarkerAndId _ _ _ = Nothing
    mkLine constr atts spans _ st = ret { mkClSyn & clSyn_svgElts = [constr [] atts]} st

  toSVGCompositeImageAlgs :: CompositeImageAlg (ClSt s Real)
                                               (ClSt s ToSVGSyn)
                                               ([ImageOffsetReal] (Maybe ToSVGSyn) (Set (Set ImageTag, Set ImageTag)) ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                               (ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose edges totalSpan imAts imTrs imTas
      =           evalOffsets offsets `b`
      \offsets -> evalMaybe host `b`
      \host    -> compose offsets host edges totalSpan imAts imTrs imTas `b`
      \compose -> let (elts, spans) = case host of
                                        Just {clSyn_svgElts, clSyn_imageSpanReal} -> (clSyn_svgElts ++ compose.clSyn_svgElts, clSyn_imageSpanReal)
                                        _                                         -> (compose.clSyn_svgElts, compose.clSyn_imageSpanReal)
                  in  sequence (map (\f -> f spans) imTrs) `b`
      \imTrs   -> ret { mkClSyn & clSyn_svgElts = mkGroup [] (getSvgAttrs (mkAttrs imAts imTrs)) elts}

  toSVGComposeAlgs :: ComposeAlg (ClSt s ToSVGSyn)
                                 ([ImageOffsetReal] (Maybe ToSVGSyn) (Set (Set ImageTag, Set ImageTag)) ImageSpanReal [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![ImageAlign] ![[ClSt s ToSVGSyn]] ![ImageOffsetReal]
              !(Maybe ToSVGSyn) !(Set (Set ImageTag, Set ImageTag)) !ImageSpanReal
              ![Either HtmlAttr SVGAttr]
              ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
           -> ClSt s ToSVGSyn | iTask s
    mkGrid _ aligns imgss offsets mbhost edges totalSpan=:(xsp, ysp) imAts imTrs imTas = ret mkClSyn

    mkCollage :: ![ClSt s ToSVGSyn] ![ImageOffsetReal] !(Maybe ToSVGSyn)
                 !(Set (Set ImageTag, Set ImageTag)) !ImageSpanReal ![Either HtmlAttr SVGAttr]
                 ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
              -> ClSt s ToSVGSyn | iTask s
    mkCollage imgs offsets mbhost edges totalSpan imAts imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> ret {mkClSyn & clSyn_svgElts = flatten (zipWith mkTranslateGroup offsets (map (\x -> x.clSyn_svgElts) imgsSps))}
    mkOverlay :: ![ImageAlign] ![ClSt s ToSVGSyn] ![ImageOffsetReal] !(Maybe ToSVGSyn)
                 !(Set (Set ImageTag, Set ImageTag)) !ImageSpanReal ![Either HtmlAttr SVGAttr]
                 ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
              -> ClSt s ToSVGSyn | iTask s
    mkOverlay aligns imgs offsets mbhost edges totalSpan imAts imTrs imTas = ret mkClSyn
      //where
      //go st
        //# (imgsSps, st) = sequence imgs st
        //# connectors    = flatten (map (\x -> map (\c -> x.clSyn_imageOffsetReal + c) x.clSyn_connectors) imgsSps)
        //# st = stTrace connectors st
        //# connpset      = [(cx, cy) \\ (cx, _) <- connectors, (_, cy) <- connectors]
        //# st = stTrace connpset st
        //# connedges     = [ ((x, y), (x`, y`)) \\ (x, y) <- connpset, (x`, y`) <- connpset
        //# connedges     = [ ((x, y), (x`, y`)) \\ (x, y) <- connectors, (x`, y`) <- connectors
                          //| xor (x == x`) (y == y`)] // && doesNotCrossImage (x, y) (x`, y`) imgsSps]
        //# st = stTrace connedges st
        //# connImgs      = [PolylineElt [StyleAttr "fill:none;stroke:black;stroke-width:3"] [PointsAttr [(toString x, toString y), (toString x`, toString y`)]]
                          //\\ ((x, y), (x`, y`)) <- connedges]
        //# st = stTrace (length connedges) st
        //# st = stTrace (length connImgs) st
        //= ret {mkClSyn & clSyn_svgElts = flatten (map (\x -> mkTranslateGroup x.clSyn_imageOffsetReal x.clSyn_svgElts) imgsSps)} st
        //where
        //doesNotCrossImage v1 v2 imgsSps = all (doesNotCrossImage` v1 v2) imgsSps
        //doesNotCrossImage` (x, y) (x`, y`) {clSyn_imageOffsetReal = (xoff, yoff), clSyn_imageSpanReal = (xsp, ysp)}
          //# xright  = xoff + xsp
          //# ybottom = yoff + ysp
          //= ((x <= xoff && x` <= xoff) || (x >= xright  && x` >= xright)) &&
            //((y <= yoff && y` <= yoff) || (y >= ybottom && y` >= ybottom))

  toSVGSpanAlgs :: SpanAlg (ClSt s Real) (ClSt s Real) | iTask s
  toSVGSpanAlgs = evalSpanSpanAlgs

  toSVGLookupSpanAlgs :: LookupSpanAlg (ClSt s Real) | iTask s
  toSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

stTrace x (clval, world)
  # world = jsTrace x world
  = (clval, world)

instance + (Real, Real) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

xor x y :== not (x && y) && (x || y)

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

(`b`) f g :== \st0 -> let (r, st1) = f st0 in g r st1

mkAttrs :: ![Either HtmlAttr SVGAttr] ![(SVGTransform, ImageTransform)] -> [Either HtmlAttr SVGAttr]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [Right (TransformAttr (map fst xs)):imAts]

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
  = mkGroup [] (getSvgAttrs (map Right (mkTransformTranslateAttr (xoff, yoff)))) contents

mkTransformTranslateAttr :: (Real, Real) -> [SVGAttr]
mkTransformTranslateAttr (0.0,   0.0)   = []
mkTransformTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

evalSpan :: !Span -> ClSt s Real | iTask s
evalSpan sp = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs sp

evalSpanSpanAlgs :: SpanAlg (ClSt s Real) (ClSt s Real) | iTask s
evalSpanSpanAlgs =
  { spanPxSpanAlg     = \r   -> ret r
  , spanLookupSpanAlg = id
  , spanAddSpanAlg    = \x y -> mkBin (+) x y
  , spanSubSpanAlg    = \x y -> mkBin (-) x y
  , spanMulSpanAlg    = \x y -> mkBin` (*) x y
  , spanDivSpanAlg    = \x y -> mkBin` (/) x y
  , spanAbsSpanAlg    = \x   -> mkAbs x
  , spanMinSpanAlg    = \xs  -> mkList minList xs
  , spanMaxSpanAlg    = \xs  -> mkList maxList xs
  }

evalSpanLookupSpanAlgs :: LookupSpanAlg (ClSt s Real) | iTask s
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = getColumnWidth
  , lookupSpanRowYSpanAlg     = getRowHeight
  , lookupSpanDescentYSpanAlg = \fd     st -> abort "DescentYSpanAlg" // TODO Will we even use this?
  , lookupSpanExYSpanAlg      = mkExYSpan
  , lookupSpanImageXSpanAlg   = getImageXSpan
  , lookupSpanImageYSpanAlg   = getImageYSpan
  , lookupSpanTextXSpanAlg    = getTextLength
  }
  where
  getColumnWidth ts n st=:({spanCache}, world)
    = case tagLookup ts spanCache of
        Just {cachedGridSpans = Just (cols, _)}
          //# st = stTrace n st
          //# st = stTrace cols st
          //# st = if (n < size cols) (stTrace cols.[n] st) (stTrace ("out of bounds " +++ toString n) st)
          //# st = if (n < length cols) (stTrace (cols !! n) st) (stTrace (toString n +++ " is out of bounds. length " +++ toString (length cols)) st)
          //= evalSpan cols.[0] st
          //= evalSpan cols.[n] st
          = if (n < length cols) (evalSpan (cols !! n) st) (0.0, st)
          //= if (n < size cols) (evalSpan cols.[n] st) (0.0, st)
        _ = (0.0, stTrace "getColumnWidth 1" st)
  getColumnWidth _ _ st = (0.0, stTrace "getColumnWidth 2" st) // TODO ?

  getRowHeight ts n st=:({spanCache}, world)
    = case tagLookup ts spanCache of
        Just {cachedGridSpans = Just (_, rows)}
          //# st = stTrace n st
          //# st = stTrace rows st
          //# st = if (n < size rows) (stTrace rows.[n] st) (stTrace ("out of bounds " +++ toString n) st)
          //# st = if (n < length rows) (stTrace (rows !! n) st) (stTrace (toString n +++ " is out of bounds. length " +++ toString (length rows)) st)
          //= evalSpan rows.[0] st
          //= evalSpan rows.[n] st
          //= (0.0, st)
          = if (n < length rows) (evalSpan (rows !! n) st) (0.0, st)
          //= if (n < size rows) (evalSpan rows.[n] st) (0.0, st)
        _ = (0.0, stTrace "getRowHeight 1" st)
  getRowHeight _ _ st = (0.0, stTrace "getRowHeight 2" st) // TODO ?

  getImageXSpan ts st=:({spanCache}, world)
    = case tagLookup ts spanCache of
        Just {cachedImageSpan = Just (xsp, _)}
          = evalSpan xsp st
        _ = (0.0, stTrace "getImageXSpan 1" st)
  getImageXSpan _ st = (0.0, stTrace "getImageXSpan 2" st) // TODO ?

  getImageYSpan ts st=:({spanCache}, world)
    = case tagLookup ts spanCache of
        Just {cachedImageSpan = Just (_, ysp)}
          = evalSpan ysp st
        _ = (0.0, stTrace "getImageYSpan 1" st)
  getImageYSpan _ st = (0.0, stTrace "getImageYSpan 2" st) // TODO ?

  mkExYSpan fd
    =       evalSpan fd.fontyspan `b`
    \fys -> ret (fys / 2.0) // TODO : Can we do better?

tagLookup ts mp = case 'DM'.elems ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) mp) of
                    [x:_] -> Just x
                    _     -> Nothing

mkAbs x = x `b` \x -> ret (abs x)

mkBin op x y = x `b` \x -> y `b` \y -> ret (op x y)

mkBin` op x y = x `b` \x -> ret (op x y)

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
  { imageAlg :: imCo [imAt] [imTr] (Set ImageTag) (sp, sp) (sp, sp, sp, sp) (sp, sp) [(sp, sp)] -> im
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
  { compositeImageAlg :: [(sp, sp)] (Maybe ho) co (Set (Set ImageTag, Set ImageTag)) -> coIm
  }

:: ComposeAlg im co =
  { composeAsGridAlg    :: (Int, Int) [ImageAlign] [[im]] -> co
  , composeAsCollageAlg ::                         [im]   -> co
  , composeAsOverlayAlg ::            [ImageAlign] [im]   -> co
  }

:: SpanAlg loSp sp =
  { spanPxSpanAlg     :: Real    -> sp
  , spanLookupSpanAlg :: loSp    -> sp
  , spanAddSpanAlg    :: sp sp   -> sp
  , spanSubSpanAlg    :: sp sp   -> sp
  , spanMulSpanAlg    :: sp Real -> sp
  , spanDivSpanAlg    :: sp Real -> sp
  , spanAbsSpanAlg    :: sp      -> sp
  , spanMinSpanAlg    :: [sp]    -> sp
  , spanMaxSpanAlg    :: [sp]    -> sp
  }

:: LookupSpanAlg loSp =
  { lookupSpanColumnXSpanAlg  :: (Set ImageTag) Int -> loSp
  , lookupSpanDescentYSpanAlg :: FontDef            -> loSp
  , lookupSpanExYSpanAlg      :: FontDef            -> loSp
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

imageCata allAlgs { Image | content, attribs, transform, tags, totalSpan = (txsp, tysp), margin = (m1, m2, m3, m4), transformCorrection = (tfXCorr, tfYCorr), connectors}
  # synContent    = imageContentCata allAlgs content
  # synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) attribs
  # synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  # synTXsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs txsp
  # synTYsp       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tysp
  # synm1         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m1
  # synm2         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m2
  # synm3         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m3
  # synm4         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m4
  # synXCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfXCorr
  # synYCorr      = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs tfYCorr
  # synsConnectors = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs connectors
  = allAlgs.imageAlgs.imageAlg synContent synsAttribs synsTransform tags (synTXsp, synTYsp) (synm1, synm2, synm3, synm4) (synXCorr, synYCorr) synsConnectors

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

compositeImageCata allAlgs { CompositeImage | offsets, host, compose, edges }
  # synsImageOffset = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs offsets
  # synHost         = fmap (imageCata allAlgs) host
  # synCompose      = composeCata allAlgs compose
  = allAlgs.compositeImageAlgs.compositeImageAlg synsImageOffset synHost synCompose edges

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
spanCata spanAlgs lookupSpanAlgs (MulSpan sp r)
  # synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = spanAlgs.spanMulSpanAlg synSpan r
spanCata spanAlgs lookupSpanAlgs (DivSpan sp r)
  # synSpan = spanCata spanAlgs lookupSpanAlgs sp
  = spanAlgs.spanDivSpanAlg synSpan r
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
lookupCata lookupSpanAlgs (DescentYSpan fd)
  = lookupSpanAlgs.lookupSpanDescentYSpanAlg fd
lookupCata lookupSpanAlgs (ExYSpan fd)
  = lookupSpanAlgs.lookupSpanExYSpanAlg fd
lookupCata lookupSpanAlgs (TextXSpan fd str)
  = lookupSpanAlgs.lookupSpanTextXSpanAlg fd str
