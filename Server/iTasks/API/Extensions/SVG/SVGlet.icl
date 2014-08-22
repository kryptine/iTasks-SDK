implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import iTasks
import iTasks.API.Core.Client.Editlet
from StdOrdList import minList, maxList
import StdArray
import StdMisc
import GenLexOrd
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
  { textXSpanEnv    :: Map (FontDef, String) Real
  , uniqueIdCounter :: Int
  , editletId       :: String
  , onclicks        :: Map String (s -> s)
  , currState       :: s
  }

:: CachedSpan =
  { cachedGridSpans :: Maybe [[ImageSpan]]
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

updateImageState :: !d !s !(s -> Image s) -> Task s | iTask s & descr d
updateImageState d s f = updateInformation d [] (svgRenderer s f) @ (\(Editlet s` _ _) -> s`)

svgRenderer :: !s !(s -> Image s) -> Editlet s (s, Image s) | iTask s
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

  updateUI cid (Just (_, img)) clval world
    //# ((imgs, (imXSp, imYSp), _), (clval, world)) = toSVG img ({clval & editletId = cid}, world)
    # (syn, (clval, world)) = toSVG img ({clval & editletId = cid}, world)
    # (svg, world)  = getDomElement (mainSvgId cid) world
    # (imXSp, imYSp) = syn.clSyn_imageSpanReal
    # (_, world)    = (svg `setAttribute` ("height", toInt imYSp)) world
    # (_, world)    = (svg `setAttribute` ("width", toInt imXSp)) world
    # (_, world)    = (svg `setAttribute` ("viewBox", "0 0 " +++ toString (toInt imXSp) +++ " " +++ toString (toInt imYSp))) world
    # world         = (svg .# "innerHTML" .= "") world
    # (elem, world) = appendSVG (GElt [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))] [] syn.clSyn_svgElts) svg world
    # world         = addOnclicks cid svg clval.onclicks world
    = (clval, world)

  updateUI _ _ clval world = (clval, world)

  genServerDiff _ y
    # ((img, _), _) = fixSpans (state2Image y) {srvTaggedSpanEnv = 'DM'.newMap, didChange = False}
    //= Just (y, overlay [(AtMiddleX, AtMiddleY)] [] [img] Nothing)
    = Just (y, img)
  appServerDiff (st, _) _ = st

  genClientDiff x y
    | x.currState === y.currState = Nothing
    | otherwise                   = Just (y.currState, state2Image y.currState)
  appClientDiff (st, _) clval = {clval & currState = st}

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
  = case 'DM'.gGet (fontdef, str) clval.textXSpanEnv of
      Just wh = (wh, (clval, world))
      Nothing
        # (svg, world)   = (jsDocument `createElementNS` (svgns, "svg")) world
        # (body, world)  = .? (jsDocument .# "body") world
        # (_, world)     = (body `appendChild` svg) world
        # (elem, world)  = (jsDocument `createElementNS` (svgns, "text")) world
        # (fntSz, (clval, world)) =  evalSpan fontdef.fontyspan (clval, world)
        # fontAttrs      = [ ("font-family",  fontdef.fontfamily)
                           , ("font-size",    toString fntSz)
                           , ("font-stretch", fontdef.fontstretch)
                           , ("font-style",   fontdef.fontstyle)
                           , ("font-variant", fontdef.fontvariant)
                           , ("font-weight",  fontdef.fontweight)
                           , ("x", "-10000")
                           , ("y", "-10000") ]
        # world          = foldr (\args world -> snd ((elem `setAttribute` args) world)) world fontAttrs
        # world          = (elem .# "textContent" .= str) world
        # (_, world)     = (svg `appendChild` elem) world
        # (ctl, world)   = (elem `getComputedTextLength` ()) world
        # (_, world)     = (svg `removeChild` elem) world
        # (_, world)     = (body `removeChild` svg) world
        # twidth         = jsValToReal ctl
        = (twidth, ({clval & textXSpanEnv = 'DM'.gPut (fontdef, str) twidth clval.textXSpanEnv}, world))

:: *St m :== *(ClientState m, *JSWorld)

:: ClSt m a :== State (St m) a

:: SrvSt a :== State ServerState a

:: ServerState =
  { srvTaggedSpanEnv :: Map (Set ImageTag) CachedSpan
  , didChange        :: Bool
  }

:: State s a :== s -> *(a, s)

:: ErrorMessage :== String

//:: FixSpansSyn s :== (Image s, ImageSpan, [Connector])
:: FixSpansSyn s =
  { srvSyn_image      :: Image s
  , srvSyn_imageSpan  :: ImageSpan
  , srvSyn_connectors :: [Connector]
  }

mkSrvSyn img = { srvSyn_image      = img
               , srvSyn_imageSpan  = (px 0.0, px 0.0)
               , srvSyn_connectors = []
               }

:: FixSpansContentSyn s :== (ImageContent s, ImageSpan, [Connector])

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

cacheGridSpans :: !(Set ImageTag) ![[ImageSpan]] !ServerState -> ServerState
cacheGridSpans imTas sp st = addCachedSpan (\r -> {r & cachedGridSpans = Just sp}) imTas st

rectAnchors :: !ImageSpan -> [Connector]
rectAnchors (w, h) = [(zero, zero), (zero, h /. 2.0), (zero, h), (w /. 2.0, zero), (w /. 2.0, h), (w, zero), (w, h /. 2.0), (w, h)]

ellipseAnchors :: !ImageSpan -> [Connector]
ellipseAnchors (w, h) = [(rx, zero), (rx *. 2.0, ry), (rx, ry *. 2.0), (zero, ry)] ++ concatMap (\th -> [(rSin th ry, rCos th rx), (~(rSin th ry), rCos th rx), (rSin th ry, ~(rCos th rx)), (~(rSin th ry), ~(rCos th rx))]) [22.5, 45.0, 67.5]
  where
  rx        = w /. 2.0
  ry        = h /. 2.0
  rSin th r = r *. sin th
  rCos th r = r *. cos th

// TODO : Detect divergence due to lookups and return an Either ErrorMessage (Image s), instead of just an Image s
fixSpans :: !(Image s) -> SrvSt (Image s, ImageSpan) | iTask s
fixSpans img = go
  where
  go st
    # (syn, st)    = imageCata fixSpansAllAlgs img st
    | st.didChange = fixSpans syn.srvSyn_image {st & didChange = False}
    | otherwise    = ret (syn.srvSyn_image, syn.srvSyn_imageSpan) st
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
                                (SrvSt (FixSpansSyn s)) | iTask s
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: !([ImageTransform] (Set ImageTag) -> (SrvSt (FixSpansContentSyn s)))
               ![SrvSt (ImageAttr s)]
               ![SrvSt ImageTransform]
               !(Set ImageTag)
               !(SrvSt Span, SrvSt Span, SrvSt Span, SrvSt Span)
               !(SrvSt Span, SrvSt Span) ![(SrvSt Span, SrvSt Span)]
            -> SrvSt (FixSpansSyn s) | iTask s
    mkImage imCo imAts imTrs imTas (m1, m2, m3, m4) offset connectors = go
      where
      go st
        # (imAts, st) = sequence imAts st
        # (imTrs, st) = sequence imTrs st
        # ((content, (xsp, ysp), connectors), st) = imCo imTrs imTas st
        # (m1, st)    = m1 st
        # (m2, st)    = m2 st
        # (m3, st)    = m3 st
        # (m4, st)    = m4 st
        # (xsp, ysp)  = (xsp + m2 + m4, ysp + m1 + m3)
        # st          = cacheImageSpan imTas (xsp, ysp) st
        = ret {mkSrvSyn { Image
                        | content    = content
                        , attribs    = imAts
                        , transform  = imTrs
                        , tags       = imTas
                        , margin     = (m1, m2, m3, m4)
                        , offset     = (zero, zero) // TODO !!! TODO !!! TODO
                        , connectors = connectors
                        }
              & srvSyn_imageSpan  = (xsp, ysp)
              , srvSyn_connectors = connectors} st

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
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage       imSp imTrs = mkSpan EmptyImage         imSp imTrs rectAnchors
    mkRectImage        imSp imTrs = mkSpan RectImage          imSp imTrs rectAnchors
    mkTextImage fd str imSp imTrs = mkSpan (TextImage fd str) imSp imTrs rectAnchors
    mkCircleImage      imSp imTrs = mkSpan CircleImage        imSp imTrs ellipseAnchors
    mkEllipseImage     imSp imTrs = mkSpan EllipseImage       imSp imTrs ellipseAnchors

    mkSpan :: !BasicImage !ImageSpan ![ImageTransform] !(ImageSpan -> [Connector]) -> SrvSt (FixSpansContentSyn s) | iTask s
    mkSpan val imSp imTrs f = ret (Basic val imSp, fst (applyTransforms imTrs imSp) /* TODO */, f imSp)

  fixSpansLineImageAlgs :: LineImageAlg (SrvSt ImageSpan)
                                        (SrvSt (Markers s, ImageSpan))
                                        (SrvSt LineContent)
                                        ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansLineImageAlgs =
    { lineImageLineImageAlg = mkLine
    }
    where
    mkLine :: !(SrvSt ImageSpan)
              !(Maybe (SrvSt (Markers s, ImageSpan)))
              !(SrvSt LineContent)
              ![ImageTransform] !(Set ImageTag)
           -> SrvSt (FixSpansContentSyn s) | iTask s
    mkLine imSp mmarkers liCo imTrs imTas
      =            imSp `b`
      \imSp     -> evalMaybe mmarkers `b`
      \mmarkers -> liCo `b`
      \liCo     -> ret (Line { LineImage
                             | lineSpan    = imSp
                             , markers     = fmap fst mmarkers
                             , lineContent = liCo
                             }
                       , fst (applyTransforms imTrs imSp) /* TODO */
                       , [] /* TODO ? */)
  fixSpansMarkersAlgs :: MarkersAlg (SrvSt (FixSpansSyn s)) (SrvSt (Markers s, ImageSpan)) | iTask s
  fixSpansMarkersAlgs =
    { markersMarkersAlg = mkMarkers
    }
    where
    mkMarkers :: !(Maybe (SrvSt (FixSpansSyn s)))
                 !(Maybe (SrvSt (FixSpansSyn s)))
                 !(Maybe (SrvSt (FixSpansSyn s)))
              -> SrvSt (Markers s, ImageSpan) | iTask s
    mkMarkers mStart mMid mEnd
      =          evalMaybe mStart `b`
      \mStart -> evalMaybe mMid `b`
      \mMid   -> evalMaybe mEnd `b`
      \mEnd   -> ret ({ markerStart = fmap (\x -> x.srvSyn_image) mStart
                      , markerMid   = fmap (\x -> x.srvSyn_image) mMid
                      , markerEnd   = fmap (\x -> x.srvSyn_image) mEnd
                      }
                     , (px 0.0, px 0.0))
  fixSpansLineContentAlgs :: LineContentAlg (SrvSt Span) (SrvSt LineContent)
  fixSpansLineContentAlgs =
    { lineContentSimpleLineImageAlg = \sl     -> ret (SimpleLineImage sl)
    , lineContentPolygonImageAlg    = \coords -> evalOffsets coords `b` \coords -> ret (PolygonImage coords)
    , lineContentPolylineImageAlg   = \coords -> evalOffsets coords `b` \coords -> ret (PolylineImage coords)
    }
  fixSpansCompositeImageAlgs :: CompositeImageAlg (SrvSt Span)
                                                  (SrvSt (FixSpansSyn s))
                                                  ([ImageOffset] (Maybe (FixSpansSyn s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan))
                                                  ([ImageTransform] (Set ImageTag) -> SrvSt (FixSpansContentSyn s)) | iTask s
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage :: ![(SrvSt Span, SrvSt Span)]
                        !(Maybe (SrvSt (FixSpansSyn s)))
                        !([ImageOffset] (Maybe (FixSpansSyn s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan))
                        !(Set (Set ImageTag, Set ImageTag))
                        ![ImageTransform] !(Set ImageTag)
                     -> SrvSt (FixSpansContentSyn s) | iTask s
    mkCompositeImage offsets host compose edges imTrs imTas = go
      where
      go st
        # (offsets, st) = evalOffsets offsets st
        # (host, st)    = evalMaybe host st
        # ((compose, composeSpan), st) = compose offsets host imTrs imTas st
        # (host, span, conns) = case host of
                                  Just {srvSyn_image = hostImg, srvSyn_imageSpan = hostSpan, srvSyn_connectors = connectors}
                                     -> (Just hostImg, hostSpan, connectors)
                                  _  -> (Nothing, composeSpan
                                        , [])
                                        //, rectAnchors composeSpan) // TODO somehow this breaks
        # (span, imgOffset)   = applyTransforms imTrs span // TODO Transform the anchor points as well
        = ret (Composite { CompositeImage
                         | offsets = offsets
                         , host    = host
                         , compose = compose
                         , edges   = edges }
                         , span
                         , conns
                         ) st
  fixSpansComposeAlgs :: ComposeAlg (SrvSt (FixSpansSyn s))
                                    ([ImageOffset] (Maybe (FixSpansSyn s)) [ImageTransform] (Set ImageTag) -> SrvSt (Compose s, ImageSpan)) | iTask s
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![ImageAlign] ![[SrvSt (FixSpansSyn s)]]
              ![ImageOffset] !(Maybe (FixSpansSyn s)) ![ImageTransform] !(Set ImageTag)
           -> SrvSt (Compose s, ImageSpan) | iTask s
    mkGrid dims ias imgss offsets mbhost imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr f ([], st) imgss
        # spanss      = map (map (\x -> x.srvSyn_imageSpan)) imgss
        # yspans      = map (maxSpan o map snd) spanss
        # xspans      = map (maxSpan o map fst) (transpose spanss)
        # infoffs     = offsets ++ repeatn (length xspans * length yspans) (px 0.0, px 0.0)
        # (xsp, ysp)  = maybe ( foldr (\(xsp, off) acc -> xsp + off + acc) (px 0.0) (zip2 xspans (map fst infoffs))
                              , foldr (\(ysp, off) acc -> ysp + off + acc) (px 0.0) (zip2 yspans (map snd infoffs))
                              )
                              (\x -> x.srvSyn_imageSpan) mbhost
        # st          = cacheGridSpans imTas ((map (\yspan -> map (\xspan -> (xspan, yspan)) xspans)) yspans) st
        = ret (AsGrid dims ias (map (map (\x -> x.srvSyn_image)) imgss), (xsp, ysp)) st
      f :: ![SrvSt (FixSpansSyn s)] !*([[(FixSpansSyn s)]], ServerState) -> *([[(FixSpansSyn s)]], ServerState) | iTask s
      f imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: ![SrvSt (FixSpansSyn s)] ![ImageOffset] !(Maybe (FixSpansSyn s)) ![ImageTransform] !(Set ImageTag) -> SrvSt (Compose s, ImageSpan) | iTask s
    mkCollage imgs offsets mbhost imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> ret ( AsCollage (map (\x -> x.srvSyn_image) imgsSps)
                      , maybe (calculateComposedSpan (map (\x -> x.srvSyn_imageSpan) imgsSps) offsets) (\x -> x.srvSyn_imageSpan) mbhost)
    mkOverlay :: ![ImageAlign] ![SrvSt (FixSpansSyn s)] ![ImageOffset] !(Maybe (FixSpansSyn s)) ![ImageTransform] !(Set ImageTag) -> SrvSt (Compose s, ImageSpan) | iTask s
    mkOverlay ias imgs offsets mbhost imTrs imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # spans         = map (\x -> x.srvSyn_imageSpan) imgsSps
        # maxXSpan      = maxSpan (map fst spans)
        # maxYSpan      = maxSpan (map snd spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\x -> x.srvSyn_imageSpan) mbhost
        = ret ( AsOverlay ias (map (\x -> x.srvSyn_image) imgsSps)
              , maybe (calculateComposedSpan spans offsets) (\x -> x.srvSyn_imageSpan) mbhost) st
  fixSpansSpanAlgs :: SpanAlg (SrvSt Span) (SrvSt Span)
  fixSpansSpanAlgs =
    { spanPxSpanAlg     = \r   -> ret (PxSpan r)
    , spanLookupSpanAlg = ($)
    , spanAddSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (AddSpan x y)
    , spanSubSpanAlg    = \x y -> x `b` \x -> y `b` \y -> ret (SubSpan x y)
    , spanMulSpanAlg    = \x y -> x `b` \x ->             ret (MulSpan x y)
    , spanDivSpanAlg    = \x y -> x `b` \x ->             ret (DivSpan x y)
    , spanAbsSpanAlg    = \x   -> x `b` \x ->             ret (AbsSpan x)
    , spanMinSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (MinSpan xs)
    , spanMaxSpanAlg    = \xs  -> sequence xs `b` \xs ->  ret (MaxSpan xs)
    }
  fixSpansLookupSpanAlgs :: LookupSpanAlg (SrvSt Span)
  fixSpansLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg  = mkImageGridSpan (\xss n -> maxSpan (map fst (transpose xss !! n))) ColumnXSpan
    , lookupSpanRowYSpanAlg     = mkImageGridSpan (\xss n -> maxSpan (map snd (xss !! n))) RowYSpan
    , lookupSpanImageXSpanAlg   = mkImageSpan fst ImageXSpan
    , lookupSpanImageYSpanAlg   = mkImageSpan snd ImageYSpan
    , lookupSpanDescentYSpanAlg = \fd     -> ret (LookupSpan (DescentYSpan fd))
    , lookupSpanExYSpanAlg      = \fd     -> ret (LookupSpan (ExYSpan fd))
    , lookupSpanTextXSpanAlg    = \fd str -> ret (LookupSpan (TextXSpan fd str))
    }
    where
    lookupTags :: !(Set ImageTag) -> SrvSt (Maybe CachedSpan)
    lookupTags ts
      | 'DS'.null ts = ret Nothing
      | otherwise    = go
      where
      go srv
        = case 'DM'.elems ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) srv.srvTaggedSpanEnv) of
            [x:_] -> (Just x, {srv & didChange = True})
            _     -> (Nothing, srv)

    mkImageSpan :: !(ImageSpan -> Span) !((Set ImageTag) -> LookupSpan) !(Set ImageTag) -> SrvSt Span
    mkImageSpan f c ts
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just {cachedImageSpan=Just xs} -> f xs
                      _                              -> LookupSpan (c ts))

    mkImageGridSpan :: !([[ImageSpan]] Int -> Span) !((Set ImageTag) Int -> LookupSpan) !(Set ImageTag) Int -> SrvSt Span
    mkImageGridSpan f c ts n
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just csp=:{cachedGridSpans=Just xss} -> f xss n
                      _                                    -> LookupSpan (c ts n))

:: ImageSpanReal :== (Real, Real)

:: ImageOffsetReal :== (Real, Real)

//:: ToSVGSyn :== ([SVGElt], ImageSpanReal, ImageOffsetReal)
:: ToSVGSyn =
  { clSyn_svgElts         :: [SVGElt]
  , clSyn_imageSpanReal   :: ImageSpanReal
  , clSyn_imageOffsetReal :: ImageOffsetReal
  , clSyn_connectors      :: [ConnectorReal]
  }

:: ConnectorReal :== (Real, Real)

mkClSyn = { clSyn_svgElts         = []
          , clSyn_imageSpanReal   = (0.0, 0.0)
          , clSyn_imageOffsetReal = (0.0, 0.0)
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

calcSpanOffset :: ImageSpanReal ImageSpanReal -> ImageOffsetReal
calcSpanOffset (xsp1, ysp1) (xsp2, ysp2) = (abs (xsp2 - xsp1) / 2.0, abs (ysp2 - ysp1) / 2.0)

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
  toSVGImageAlgs :: ImageAlg ([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                             (ClSt s (Either HtmlAttr SVGAttr))
                             (ImageSpanReal -> ClSt s (SVGTransform, ImageTransform))
                             (ClSt s Real)
                             (ClSt s ToSVGSyn) | iTask s
  toSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage :: !([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
               ![ClSt s (Either HtmlAttr SVGAttr)]
               ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)]
               !(Set ImageTag)
               !(ClSt s Real, ClSt s Real, ClSt s Real, ClSt s Real)
               !(ClSt s Real, ClSt s Real) ![(ClSt s Real, ClSt s Real)]
            -> ClSt s ToSVGSyn | iTask s
    mkImage imCo imAts imTrs imTas (m1, m2, m3, m4) offset connectors
      =         sequence imAts `b`
      \imAts -> finishMkImage imAts imTrs
      where
      finishMkImage imAts imTrs st
        # (syn=:{clSyn_imageSpanReal = (xsp, ysp)}, st) = imCo imAts imTrs imTas st
        # (m1, st) = m1 st
        # (m2, st) = m2 st
        # (m3, st) = m3 st
        # (m4, st) = m4 st
        # marginXSpan = xsp + m2 + m4
        # marginYSpan = ysp + m1 + m3
        # (connectors, st) = evalOffsets connectors st
        = ({mkClSyn & clSyn_svgElts         = mkGroup [] (mkTransformTranslateAttr (m1, m2)) syn.clSyn_svgElts
                    , clSyn_imageSpanReal   = (marginXSpan, marginYSpan)
                    , clSyn_imageOffsetReal = syn.clSyn_imageOffsetReal
                    , clSyn_connectors      = connectors}, st)

  toSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ClSt s ImageSpanReal)
                                           ([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           ([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           ([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentLineAlg      = id
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: !(ImageSpanReal [Either HtmlAttr SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
               !(ClSt s ImageSpanReal)
               ![Either HtmlAttr SVGAttr] ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag) ->
               ClSt s ToSVGSyn | iTask s
    mkBasic baIm imSp imAts imTrs imTas
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
        # st     = ({ clval & uniqueIdCounter = uniqId + 1
                            , onclicks = 'DM'.put ocId onclick clval.onclicks}, world)
        = ret (Left (ClassAttr ocId)) st
  toSVGImageTransformAlgs :: ImageTransformAlg Deg (ClSt s Real) (ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)) | iTask s
  toSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    (xsp, ysp) -> ret (RotateTransform (toString (toReal imAn)) (Just (toString (xsp / 2.0), toString (ysp / 2.0))), RotateImage imAn)
    , imageTransformSkewXImageAlg  = \imAn    (xsp, _)   -> ret (SkewXTransform (toString (toReal imAn)), SkewXImage imAn)
    , imageTransformSkewYImageAlg  = \imAn    (_, ysp)   -> ret (SkewYTransform (toString (toReal imAn)), SkewYImage imAn)
    , imageTransformFitImageAlg    = \sp1 sp2 (xsp, ysp) -> sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (ScaleTransform (toString (sp1 / xsp)) (toString (sp2 / ysp)), FitImage (px sp1) (px sp2))
    , imageTransformFitXImageAlg   = \sp      (xsp, _)   -> sp `b` \sp -> ret (ScaleTransform (toString (sp / xsp)) "1.0", FitXImage (px sp))
    , imageTransformFitYImageAlg   = \sp      (_, ysp)   -> sp `b` \sp -> ret (ScaleTransform "1.0" (toString (sp / ysp)), FitYImage (px sp))
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
    mkEmptyImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr]
                    ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                 -> ClSt s ToSVGSyn | iTask s
    mkEmptyImage imSp imAts imTrs imTas
      =                 mkWH imSp `b`
      \wh            -> applyRealTransforms (map snd imTrs) imSp `b`
      \(imSp`, offs) -> ret { mkClSyn & clSyn_svgElts         = mkGroup (wh ++ getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs)) []
                                      , clSyn_imageSpanReal   = imSp`
                                      , clSyn_imageOffsetReal = offs }
    mkTextImage :: !FontDef !String !ImageSpanReal ![Either HtmlAttr SVGAttr]
                   ![(SVGTransform, ImageTransform)] !(Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkTextImage fd str imSp imAts imTrs imTas
      =                 evalSpan fd.fontyspan `b`
      \sp            -> applyRealTransforms (map snd imTrs) imSp `b`
      \(imSp`, offs) -> ret { mkClSyn & clSyn_svgElts         = [TextElt (getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs) ++ fontAttrs sp) str]
                                      , clSyn_imageSpanReal   = imSp`
                                      , clSyn_imageOffsetReal = offs }
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
    mkRectImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr]
                   ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                -> ClSt s ToSVGSyn | iTask s
    mkRectImage imSp imAts imTrs imTas
      =                 mkWH imSp `b`
      \wh            -> applyRealTransforms (map snd imTrs) imSp `b`
      \(imSp`, offs) -> ret { mkClSyn & clSyn_svgElts         = [RectElt (wh ++ getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs))]
                                      , clSyn_imageSpanReal   = imSp`
                                      , clSyn_imageOffsetReal = offs }
    mkCircleImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr]
                     ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                  -> ClSt s ToSVGSyn | iTask s
    mkCircleImage imSp=:(imXSp`, _) imAts imTrs imTas
      =                          applyRealTransforms (map snd imTrs) imSp `b`
      \((imXSp, imYSp), offs) -> let r   = imXSp` / 2.0
                                 in  ret { mkClSyn & clSyn_svgElts         = [CircleElt (getHtmlAttrs imAts) [ RAttr (toString r, PX), CxAttr (toString r, PX)
                                                                                                           , CyAttr (toString r, PX) : (getSvgAttrs (mkAttrs imAts imTrs)) ]]
                                                   , clSyn_imageSpanReal   = (imXSp, imYSp)
                                                   , clSyn_imageOffsetReal = offs }
    mkEllipseImage :: !ImageSpanReal ![Either HtmlAttr SVGAttr]
                      ![(SVGTransform, ImageTransform)] !(Set ImageTag)
                   -> ClSt s ToSVGSyn | iTask s
    mkEllipseImage imSp=:(imXSp, imYSp) imAts imTrs imTas
      =                            applyRealTransforms (map snd imTrs) imSp `b`
      \((imXSp`, imYSp`), offs) -> ret { mkClSyn & clSyn_svgElts         = [EllipseElt (getHtmlAttrs imAts) (getSvgAttrs (mkAttrs imAts imTrs) ++
                                                                                     [ RxAttr (toString (imXSp / 2.0), PX), RyAttr (toString (imYSp / 2.0), PX)
                                                                                     , CxAttr (toString (imXSp / 2.0), PX), CyAttr (toString (imYSp / 2.0), PX)])]
                                                 , clSyn_imageSpanReal   = (imXSp`, imYSp`)
                                                 , clSyn_imageOffsetReal = offs }

    mkWH :: !ImageSpanReal -> ClSt s [HtmlAttr] | iTask s
    mkWH (imXSp, imYSp) = ret [WidthAttr (toString (toInt imXSp)), HeightAttr (toString (toInt imYSp))]

  // TODO Type signature
  toSVGLineImageAlgs =
    { lineImageLineImageAlg = mkLineImage
    }
    where
    mkLineImage lineSpan mmarkers lineContent imAts imTrs imTas
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
      # st   = ({ clval & uniqueIdCounter = clval.uniqueIdCounter + 3 }, world)
      # m1Id = mkMarkerId clval.editletId clval.uniqueIdCounter
      # m2Id = mkMarkerId clval.editletId (clval.uniqueIdCounter + 1)
      # m3Id = mkMarkerId clval.editletId (clval.uniqueIdCounter + 2)
      # markersAndIds = [(m, i) \\ Just (m, i) <- [mkMarkerAndId mmStart m1Id MarkerStartAttr, mkMarkerAndId mmMid m2Id MarkerMidAttr, mkMarkerAndId mmEnd m3Id MarkerEndAttr]]
      = ret {mkClSyn & clSyn_svgElts         = [constr [] (map snd markersAndIds ++ atts), DefsElt [] [] (map fst markersAndIds)]
                     , clSyn_imageSpanReal   = spans
                     , clSyn_imageOffsetReal = (0.0, 0.0)} st // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId (Just {clSyn_svgElts, clSyn_imageSpanReal = (w, h)}) mid posAttr = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto" // TODO Do something with offset?
                                                                                        , ViewBoxAttr "0" "0" (toString w) (toString h)
                                                                                        , RefXAttr (toString w, PX)
                                                                                        , RefYAttr (toString (h / 2.0), PX)
                                                                                        , MarkerHeightAttr (toString h, PX)
                                                                                        , MarkerWidthAttr (toString w, PX)
                                                                                        ] clSyn_svgElts
                                                            , posAttr ("url(#" +++ mid +++ ")"))
      mkMarkerAndId _                    _   _       = Nothing
    mkLine constr atts spans _ st
      = ret { mkClSyn & clSyn_svgElts         = [constr [] atts]
                      , clSyn_imageSpanReal   = spans
                      , clSyn_imageOffsetReal = (0.0, 0.0)
            } st

  toSVGCompositeImageAlgs :: CompositeImageAlg (ClSt s Real)
                                               (ClSt s ToSVGSyn)
                                               ([ImageOffsetReal] (Maybe ToSVGSyn) (Set (Set ImageTag, Set ImageTag)) [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                               ([Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose edges imAts imTrs imTas
      =           evalOffsets offsets `b`
      \offsets -> evalMaybe host `b`
      \host    -> compose offsets host edges imAts imTrs imTas `b`
      \compose -> finishMkImage host compose
      where
      finishMkImage (Just {clSyn_svgElts = hostImg, clSyn_imageSpanReal = hostSpans, clSyn_imageOffsetReal = hostOffs}) {clSyn_svgElts = compose} st // TODO Do something with hostOffs?
        # (imTrs, (clval, world)) = sequence (map (\f -> f hostSpans) imTrs) st
        # uniqId                  = clval.uniqueIdCounter
        # st                      = ({ clval & uniqueIdCounter = uniqId + 1 }, world)
        # clipPathId              = mkClipPathId clval.editletId uniqId
        # ((hostSpans, offs), st) = applyRealTransforms (map snd imTrs) hostSpans st
        # g                       = mkGroup [] (getSvgAttrs (mkAttrs imAts imTrs)) (compose ++ hostImg)
        = ret { mkClSyn & clSyn_svgElts         = g
                        , clSyn_imageSpanReal   = hostSpans
                        , clSyn_imageOffsetReal = offs} st
      finishMkImage _ {clSyn_svgElts = compose, clSyn_imageSpanReal = composeSpans, clSyn_imageOffsetReal = composeOffs} st // TODO Do something with composeOffs?
        # (imTrs, st)                = sequence (map (\f -> f composeSpans) imTrs) st
        # ((composeSpans, offs), st) = applyRealTransforms (map snd imTrs) composeSpans st
        = ret {mkClSyn & clSyn_svgElts         = mkGroup [] (getSvgAttrs (mkAttrs imAts imTrs)) compose
                       , clSyn_imageSpanReal   = composeSpans
                       , clSyn_imageOffsetReal = offs} st
  toSVGComposeAlgs :: ComposeAlg (ClSt s ToSVGSyn) ([ImageOffsetReal] (Maybe ToSVGSyn) (Set (Set ImageTag, Set ImageTag)) [Either HtmlAttr SVGAttr] [ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: !(Int, Int) ![ImageAlign] ![[ClSt s ToSVGSyn]] ![ImageOffsetReal]
              !(Maybe ToSVGSyn) !(Set (Set ImageTag, Set ImageTag)) ![Either HtmlAttr SVGAttr]
              ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag)
           -> ClSt s ToSVGSyn | iTask s
    mkGrid _ aligns imgss offsets mbhost edges imAts imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr seqImgsGrid ([], st) imgss
        # spanss      = map (map (\x -> x.clSyn_imageSpanReal)) imgss
        # yspans      = map (maxList o map snd) spanss
        # xspans      = map (maxList o map fst) (transpose spanss)
        # n           = length xspans * length yspans
        # infoffs     = offsets ++ repeatn n (0.0, 0.0)
        # (xsp, ysp)  = maybe ( foldr (\(xsp, off) acc -> xsp + off + acc) 0.0 (zip2 xspans (map fst infoffs))
                              , foldr (\(ysp, off) acc -> ysp + off + acc) 0.0 (zip2 yspans (map snd infoffs))
                              )
                              (\x -> x.clSyn_imageSpanReal) mbhost
        = ret { mkClSyn & clSyn_svgElts         = mkGridChildren n xspans yspans imgss infoffs
                        , clSyn_imageSpanReal   = maybe (xsp, ysp) (\x -> x.clSyn_imageSpanReal) mbhost
                        , clSyn_imageOffsetReal = (0.0, 0.0)} // TODO Correct offsets here
              st
      mkGridChildren n xspans yspans imgss infoffs
        = (flatten o (\(x, _, _, _) -> x)) (foldl (mkRows xspans) ([], 0.0, aligns ++ repeatn n (AtLeft, AtTop), infoffs) (zip2 imgss yspans))
        where
        mkRows xspans (acc, yoff, aligns, offsets) (imgs, yspan)
          # imgsLength = length imgs
          = ( [fst (foldl (mkCols yspan yoff) ([], 0.0) (zip4 imgs xspans (take imgsLength aligns) (take imgsLength offsets))) : acc]
            , yoff + yspan, drop imgsLength aligns, drop imgsLength offsets)
        mkCols yspan yoff (acc, xoff) ({clSyn_svgElts = img, clSyn_imageSpanReal = imgSpan, clSyn_imageOffsetReal = (imXOff, imYOff)}, xspan, align, (manxoff, manyoff))
          # (xoff`, yoff`) = calcOffset xspan yspan imgSpan align
          = ( mkGroup [WidthAttr (toString (toInt xspan)), HeightAttr (toString (toInt yspan))]
                      (mkTransformTranslateAttr (xoff` + xoff + manxoff + imXOff, yoff` + yoff + manyoff + imYOff)) img ++ acc
            , xoff + xspan)

      seqImgsGrid :: ![ClSt m ToSVGSyn] *([[ToSVGSyn]], *(St m)) -> *([[ToSVGSyn]], *(St m)) | iTask m
      seqImgsGrid imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: ![ClSt s ToSVGSyn] ![ImageOffsetReal] !(Maybe ToSVGSyn) !(Set (Set ImageTag, Set ImageTag)) ![Either HtmlAttr SVGAttr] ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkCollage imgs offsets mbhost edges imAts imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> ret { mkClSyn & clSyn_svgElts         = flatten (zipWith mkTranslateGroup offsets (map (\x -> x.clSyn_svgElts) imgsSps))
                                , clSyn_imageSpanReal   = maybe (calculateComposedSpan (map (\x -> x.clSyn_imageSpanReal) imgsSps) offsets) (\x -> x.clSyn_imageSpanReal) mbhost
                                , clSyn_imageOffsetReal = (0.0, 0.0)} // TODO Correct offsets here
    mkOverlay :: ![ImageAlign] ![ClSt s ToSVGSyn] ![ImageOffsetReal] !(Maybe ToSVGSyn) !(Set (Set ImageTag, Set ImageTag)) ![Either HtmlAttr SVGAttr] ![ImageSpanReal -> ClSt s (SVGTransform, ImageTransform)] !(Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkOverlay aligns imgs offsets mbhost edges imAts imTrs imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # images        = map (\x -> x.clSyn_svgElts) imgsSps
        # numImgs       = length images
        # spans         = map (\x -> x.clSyn_imageSpanReal) imgsSps
        # maxXSpan      = maxList (map fst spans)
        # maxYSpan      = maxList (map snd spans)
        # tfOffs        = map (\x -> x.clSyn_imageOffsetReal) imgsSps
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\x -> x.clSyn_imageSpanReal) mbhost
        # alignOffs     = zipWith3 addOffset (zipWith (calcOffset maxXSpan maxYSpan) spans (aligns ++ repeatn numImgs (AtLeft, AtTop)))
                                             (offsets ++ repeatn numImgs (0.0, 0.0))
                                             tfOffs
        # connectors    = map (\x -> x.clSyn_connectors) imgsSps
        # connpset      = [(cx, cy) \\ (cx, _) <- connectors, (_, cy) <- connectors]
        # connverts     = [ (x, y) \\ (x, y) <- connpset, (x`, y`) <- connpset
                          | xor (x == x`) (y == y`) && doesNotCrossImage (x, y) (x`, y`) imgsSps]
        = ret {mkClSyn & clSyn_svgElts         = reverse (flatten (zipWith mkTranslateGroup alignOffs images))
                       , clSyn_imageSpanReal   = maybe (calculateComposedSpan spans offsets) (\x -> x.clSyn_imageSpanReal) mbhost
                       , clSyn_imageOffsetReal = (0.0, 0.0)} // TODO Correct offsets here
              st
        where
        addOffset (x1, y1) (x2, y2) (xoff, yoff) = (x1 + x2 + xoff, y1 + y2 + yoff)
        doesNotCrossImage (x, y) (x`, y`) {clSyn_imageOffsetReal = (xoff, yoff), clSyn_imageSpanReal = (xsp, ysp)}
          # xright  = xoff + xsp
          # ybottom = yoff + ysp
          = ((x <= xoff && x` <= xoff) || (x >= xright && x` >= xright)) &&
            ((y <= yoff && y` <= yoff) || (y >= ybottom && y` >= ybottom))

  toSVGSpanAlgs :: SpanAlg (ClSt s Real) (ClSt s Real) | iTask s
  toSVGSpanAlgs = evalSpanSpanAlgs

  toSVGLookupSpanAlgs :: LookupSpanAlg (ClSt s Real) | iTask s
  toSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

xor x y :== not (x && y) && (x || y)

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _   _   []   = []
mkGroup []  []  xs   = xs
mkGroup has sas elts = [GElt has sas elts]

applyRealTransforms :: ![ImageTransform] !ImageSpanReal -> ClSt s (ImageSpanReal, ImageOffsetReal) | iTask s
applyRealTransforms ts (xspr, yspr) = go
  where
  go st
    # ((xsp, ysp), (xoff, yoff)) = applyTransforms ts (px xspr, px yspr)
    # (xsp, st)                  = evalSpan xsp st
    # (ysp, st)                  = evalSpan ysp st
    # (xoff, st)                 = evalSpan xoff st
    # (yoff, st)                 = evalSpan yoff st
    = ret ((xsp, ysp), (xoff, yoff)) st

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

calcOffset :: !a !a !(a, a) !ImageAlign -> (a, a) | IsSpan a
calcOffset maxxsp maxysp (imXSp, imYSp) (xal, yal) = (mkXAl xal, mkYAl yal)
  where
  mkXAl AtLeft    = zero
  mkXAl AtMiddleX = (maxxsp /. 2.0) - (imXSp /. 2.0)
  mkXAl AtRight   = maxxsp - imXSp
  mkYAl AtTop     = zero
  mkYAl AtMiddleY = (maxysp /. 2.0) - (imYSp /. 2.0)
  mkYAl AtBottom  = maxysp - imYSp

calculateComposedSpan :: ![(a, a)] ![(a, a)] -> (a, a) | IsSpan a
calculateComposedSpan spans offs
  = foldr f (zero, zero) (zip2 (offs ++ repeatn (length spans) (zero, zero)) spans)
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

// We use aborts here, because they _should_ have been rewritten away...
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \ts n   st -> abort "ColumnXSpanAlg "
  , lookupSpanDescentYSpanAlg = \fd     st -> abort "DescentYSpanAlg" // TODO Will we even use this?
  , lookupSpanExYSpanAlg      = mkExYSpan
  , lookupSpanImageXSpanAlg   = \ts     st -> abort "ImageXSpanAlg  "
  , lookupSpanImageYSpanAlg   = \ts     st -> abort "ImageYSpanAlg  "
  , lookupSpanRowYSpanAlg     = \ts n   st -> abort "RowYSpanAlg    "
  , lookupSpanTextXSpanAlg    = getTextLength
  }
  where
  mkExYSpan fd
    =       evalSpan fd.fontyspan `b`
    \fys -> ret (fys / 2.0) // TODO : Can we do better?

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
  { imageAlg :: imCo [imAt] [imTr] (Set ImageTag) (sp, sp, sp, sp) (sp, sp) [(sp, sp)] -> im
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

imageCata allAlgs { Image | content, attribs, transform, tags, margin = (m1, m2, m3, m4), offset = (xoff, yoff), connectors}
  # synContent    = imageContentCata allAlgs content
  # synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) attribs
  # synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  # synm1         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m1
  # synm2         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m2
  # synm3         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m3
  # synm4         = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs m4
  # synXOff       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs xoff
  # synYOff       = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs yoff
  # synsConnectors = foldrOffsets allAlgs.spanAlgs allAlgs.lookupSpanAlgs connectors
  = allAlgs.imageAlgs.imageAlg synContent synsAttribs synsTransform tags (synm1, synm2, synm3, synm4) (synXOff, synYOff) synsConnectors

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
  # world         = foldr (\child world -> snd (appendSVG child elem world)) world children
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
