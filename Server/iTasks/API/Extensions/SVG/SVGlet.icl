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

//gEditor{|Image|} fx gx dx hx jex jdx dp vv=:(image, mask, ver) meta vst
  //= gEditor{|* -> *|} fx gx dx hx jex jdx dp (svglet image, mask, ver) meta vst

//gUpdate{|Image|} gUpdx gDefx jEncx jDecx target upd (image,mask) iworld
  //# ((Editlet image` _ _, mask), iworld) = gUpdate{|* -> *|} gUpdx gDefx jEncx jDecx target upd (svglet image, mask) iworld
  //= ((image`, mask), iworld)

derive class iTask ImageTag, ImageTransform, Span, LookupSpan, ImageAttr,
  ImageContent, BasicImage, CompositeImage, Slash, FontDef, Compose,
  OpacityAttr, FillAttr, StrokeWidthAttr, StrokeAttr, OnClickAttr, XAlign,
  YAlign, Set, CachedSpan, SVGletDiff, Deg, Rad

derive gLexOrd FontDef, Span, LookupSpan, ImageTag, Set, CachedSpan, Deg, Rad, Maybe

:: ClientState s =
  { didInit         :: Bool
  , didDraw         :: Bool
  , textXSpanEnv    :: Map (FontDef, String) Real
  , textYSpanEnv    :: Map FontDef (Real, Real)
  , uniqueIdCounter :: Int
  , image           :: Maybe (Image s)
  , editletId       :: String
  }

:: CachedSpan =
  { cachedGridSpans :: Maybe [[ImageSpan]]
  , cachedImageSpan :: Maybe ImageSpan
  }

derive class iTask ClientState

mainSvgId :: ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

svglet :: (Image s) -> Editlet (Image s) [SVGletDiff s] | iTask s
svglet origImg = let ((img`, imgSpan`), _) = fixSpans origImg {srvTaggedSpanEnv = 'DM'.newMap}
                 in  Editlet img` server client
where
  server
    = { EditletServerDef
      | genUI    = genUI
      , defVal   = empty (px 0.0) (px 0.0)
      , genDiff  = genServerDiff
      , appDiff  = appServerDiff
      }
  client
    = { EditletClientDef
      | updateUI = updateUI
      , defVal   = { didInit         = False
                   , didDraw         = False
                   , textXSpanEnv    = 'DM'.newMap
                   , textYSpanEnv    = 'DM'.newMap
                   , uniqueIdCounter = 0
                   , image           = Nothing
                   , editletId       = ""
                   }
      , genDiff  = genClientDiff
      , appDiff  = appClientDiff
      }
  genUI cid world
    = ({ ComponentHTML
       | width         = FlexSize
       , height        = FlexSize
       , html          = SvgTag [IdAttr (mainSvgId cid), XmlnsAttr "http://www.w3.org/2000/svg"] [] []
       , eventHandlers = []
       }
       , world
      )

  updateUI cid diffs=:(Just [Redraw img:_]) clval=:{didInit = False} world
    //# world = jsTrace "updateUI False" world
    //# world = jsTrace diffs world
    //= ({clval & didInit = True, image = Just img}, world)

  //updateUI cid diffs clval=:{didInit = True, image = Just img} world
    //# world = jsTrace "updateUI True" world
    # ((img, (imXSp, imYSp)), (_, world))     = toSVG img ({clval & editletId = cid}, world)
    # (svg, world)          = getDomElement (mainSvgId cid) world
    # (_, world)            = (svg `setAttribute` ("height", imYSp)) world
    # (_, world)            = (svg `setAttribute` ("width", imXSp)) world
    # world                 = (svg .# "innerHTML" .= "") world
    # (elem, world)         = appendSVG img svg world
    = (clval, world)

  updateUI cid diffs clval world
    # world = jsTrace "updateUI fallthrough" world
    = (clval, world)

  genServerDiff x y
    | x === y   = Nothing
    | otherwise = Just [Redraw y]
  appServerDiff [Redraw x:_] _ = x

  genClientDiff x y
    | x.image === y.image = Nothing
    | otherwise           = fmap (\img -> [Redraw img]) y.image
  appClientDiff [Redraw x:_] clval = {clval & image = Just x}


(`setAttribute`)          obj args :== obj .# "setAttribute"          .$ args
(`createElementNS`)       obj args :== obj .# "createElementNS"       .$ args
(`appendChild`)           obj args :== obj .# "appendChild"           .$ args
(`removeChild`)           obj args :== obj .# "removeChild"           .$ args
(`getBBox`)               obj args :== obj .# "getBBox"               .$ args
(`getComputedTextLength`) obj args :== obj .# "getComputedTextLength" .$ args

getTextLength :: FontDef String *(St m) -> *(Real, *(St m)) | iTask m
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
  }

:: State s a :== s -> *(a, s)

:: ErrorMessage :== String

runM :: (State s a) s -> (a, s)
runM m st = m st

sequence :: [.st -> .(a, .st)] -> (.st -> .([a], .st))
sequence ms = \st -> mapSt id ms st

addCachedSpan :: (CachedSpan -> CachedSpan) (Set ImageTag) ServerState -> ServerState
addCachedSpan f imTas st=:{srvTaggedSpanEnv}
  | 'DS'.null imTas = st
  | otherwise
    # r = fromMaybe { cachedGridSpans = Nothing, cachedImageSpan = Nothing } ('DM'.get imTas srvTaggedSpanEnv)
    = { st & srvTaggedSpanEnv = 'DM'.put imTas (f r) srvTaggedSpanEnv }

cacheImageSpan :: (Set ImageTag) ImageSpan ServerState -> ServerState
cacheImageSpan imTas sp st = addCachedSpan (\r -> {r & cachedImageSpan = Just sp}) imTas st

cacheGridSpans :: (Set ImageTag) [[ImageSpan]] ServerState -> ServerState
cacheGridSpans imTas sp st = addCachedSpan (\r -> {r & cachedGridSpans = Just sp}) imTas st

// TODO : Detect divergence due to lookups and return an Either ErrorMessage (Image s), instead of just an Image s
fixSpans :: (Image s) -> SrvSt (Image s, ImageSpan) | iTask s
fixSpans img
  =              imageCata fixSpansAllAlgs img `b`
  \(img`, sp) -> if (img === img`)
                   (ret (img, sp))
                   (fixSpans img`)
  where
  fixSpansAllAlgs =
    { imageAlgs          = fixSpansImageAlgs
    , imageContentAlgs   = fixSpansImageContentAlgs
    , imageAttrAlgs      = fixSpansImageAttrAlgs
    , imageTransformAlgs = fixSpansImageTransformAlgs
    , imageSpanAlgs      = fixSpansImageSpanAlgs
    , basicImageAlgs     = fixSpansBasicImageAlgs
    , compositeImageAlgs = fixSpansCompositeImageAlgs
    , composeAlgs        = fixSpansComposeAlgs
    , spanAlgs           = fixSpansSpanAlgs
    , lookupSpanAlgs     = fixSpansLookupSpanAlgs
    }
  fixSpansImageAlgs :: ImageAlg ([ImageAttr s] [ImageTransform] (Set ImageTag) -> (SrvSt (ImageContent s, ImageSpan)))
                                (SrvSt (ImageAttr s))
                                (SrvSt ImageTransform)
                                (SrvSt (Image s, ImageSpan))
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: ([ImageAttr s] [ImageTransform] (Set ImageTag) -> (SrvSt (ImageContent s, ImageSpan)))
               [SrvSt (ImageAttr s)]
               [SrvSt ImageTransform]
               (Set ImageTag) -> SrvSt (Image s, ImageSpan)
    mkImage imCo imAts imTrs imTas = go
      where
      go st
        # (imAts, st)         = sequence imAts st
        # (imTrs, st)         = sequence imTrs st
        # ((content, sp), st) = imCo imAts imTrs imTas st
        # st                  = cacheImageSpan imTas sp st
        = ret ({ Image | content = content, attribs = imAts, transform = imTrs, tags = imTas}, sp) st
  fixSpansImageContentAlgs :: ImageContentAlg (ImageSpan [ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
                                              (SrvSt ImageSpan)
                                              ([ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
                                              ([ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
  fixSpansImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: (ImageSpan [ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
               (SrvSt ImageSpan)
               [ImageAttr s] [ImageTransform] (Set ImageTag) ->
               SrvSt (ImageContent s, ImageSpan)
    mkBasic baIm imSp imAts imTrs imTas
      =        imSp `b`
      \imSp -> baIm imSp imAts imTrs imTas
  fixSpansImageAttrAlgs :: ImageAttrAlg s (SrvSt (ImageAttr s))
  fixSpansImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = ret o ImageStrokeAttr
    , imageAttrStrokeWidthAttrAlg   = ret o ImageStrokeWidthAttr
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
    mkFitImage :: (SrvSt Span) (SrvSt Span) -> SrvSt ImageTransform
    mkFitImage sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (FitImage sp1 sp2)
    mkFitDim :: (Span -> ImageTransform) (SrvSt Span) -> SrvSt ImageTransform
    mkFitDim ctr sp
      =      sp `b`
      \sp -> ret (ctr sp)
  fixSpansImageSpanAlgs :: ImageSpanAlg (SrvSt Span) (SrvSt ImageSpan)
  fixSpansImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: (SrvSt Span) (SrvSt Span) -> SrvSt ImageSpan
    mkSpan xspan yspan
      =         xspan `b`
      \xspan -> yspan `b`
      \yspan -> ret (xspan, yspan)
  fixSpansBasicImageAlgs :: BasicImageAlg (SrvSt Span) (ImageSpan [ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg    = \       imSp _ imTrs imTas -> mkSpan EmptyImage            imSp imTrs imTas
    , basicImageTextImageAlg     = \fd str imSp _ imTrs imTas -> mkSpan (TextImage fd str)    imSp imTrs imTas
    , basicImageLineImageAlg     = \sl     imSp _ imTrs imTas -> mkSpan (LineImage sl)        imSp imTrs imTas
    , basicImagePolygonImageAlg  = \coords imSp _ imTrs imTas -> evalOffsets coords `b` \coords -> mkSpan (PolygonImage coords)  imSp imTrs imTas
    , basicImagePolylineImageAlg = \coords imSp _ imTrs imTas -> evalOffsets coords `b` \coords -> mkSpan (PolylineImage coords) imSp imTrs imTas
    , basicImageCircleImageAlg   = \       imSp _ imTrs imTas -> mkSpan CircleImage           imSp imTrs imTas
    , basicImageRectImageAlg     = \       imSp _ imTrs imTas -> mkSpan RectImage             imSp imTrs imTas
    , basicImageEllipseImageAlg  = \       imSp _ imTrs imTas -> mkSpan EllipseImage          imSp imTrs imTas
    }
    where
    mkSpan :: BasicImage ImageSpan [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan)
    mkSpan val imSp imTrs imTas = ret (Basic val imSp, applyTransforms imTrs imSp)
  fixSpansCompositeImageAlgs :: CompositeImageAlg (SrvSt Span)
                                                  (SrvSt (Image s, ImageSpan))
                                                  ([ImageOffset] (Maybe (Image s, ImageSpan)) (Set ImageTag) -> SrvSt (Compose s, ImageSpan))
                                                  ([ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose imAts imTrs imTas st
      # (offsets, st) = evalOffsets offsets st
      # (host, st)    = evalHost host st
      # ((compose, composeSpan), st) = compose offsets host imTas st
      = case host of
          Just (hostImg, hostSpan) -> ret (Composite {CompositeImage | offsets = offsets, host = Just hostImg, compose = compose}, applyTransforms imTrs hostSpan) st
          _                        -> ret (Composite {CompositeImage | offsets = offsets, host = Nothing, compose = compose}, applyTransforms imTrs composeSpan) st
  fixSpansComposeAlgs :: ComposeAlg (SrvSt (Image s, ImageSpan)) ([ImageOffset] (Maybe (Image s, ImageSpan)) (Set ImageTag) -> SrvSt (Compose s, ImageSpan))
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[SrvSt (Image s, ImageSpan)]] [ImageOffset] (Maybe (Image s, ImageSpan)) (Set ImageTag) -> SrvSt (Compose s, ImageSpan)
    mkGrid dims ias imgss offsets mbhost imTas = go
      where
      go st
        # (imgss, st) = foldr f ([], st) imgss
        # spanss      = map (map snd) imgss
        # yspans      = map (maxSpan o map snd) spanss
        # xspans      = map (maxSpan o map fst) (transpose spanss)
        # infoffs     = offsets ++ repeatn (length xspans * length yspans) (px 0.0, px 0.0)
        # (xsp, ysp)  = maybe ( foldr (\(xsp, off) acc -> xsp + off + acc) (px 0.0) (zip2 xspans (map fst infoffs))
                              , foldr (\(ysp, off) acc -> ysp + off + acc) (px 0.0) (zip2 yspans (map snd infoffs))
                              )
                              snd mbhost
        # st          = cacheGridSpans imTas ((map (\yspan -> map (\xspan -> (xspan, yspan)) xspans)) yspans) st
        = ret (AsGrid dims ias (map (map fst) imgss), (xsp, ysp)) st
      f :: [SrvSt (Image s, ImageSpan)] *([[(Image s, ImageSpan)]], ServerState) -> *([[(Image s, ImageSpan)]], ServerState)
      f imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: [SrvSt (Image s, ImageSpan)] [ImageOffset] (Maybe (Image s, ImageSpan)) (Set ImageTag) -> SrvSt (Compose s, ImageSpan)
    mkCollage imgs offsets mbhost imTas
      =           sequence imgs `b`
      \imgsSps -> ret ( AsCollage (map fst imgsSps)
                      , maybe (calculateComposedSpan (map snd imgsSps) offsets) snd mbhost)
    mkOverlay :: [ImageAlign] [SrvSt (Image s, ImageSpan)] [ImageOffset] (Maybe (Image s, ImageSpan)) (Set ImageTag) -> SrvSt (Compose s, ImageSpan)
    mkOverlay ias imgs offsets mbhost imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # spans         = map snd imgsSps
        # maxXSpan      = maxSpan (map fst spans)
        # maxYSpan      = maxSpan (map snd spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) snd mbhost
        = ret ( AsOverlay ias (map fst imgsSps)
              , maybe (calculateComposedSpan spans offsets) snd mbhost) st
  fixSpansSpanAlgs :: SpanAlg (SrvSt Span) (SrvSt Span)
  fixSpansSpanAlgs =
    { spanPxSpanAlg     = \r   -> ret (PxSpan r)
    , spanLookupSpanAlg = ($)
    , spanAddSpanAlg    = \x y -> reduceSpanBin (+) AddSpan x y
    , spanSubSpanAlg    = \x y -> reduceSpanBin (-) SubSpan x y
    , spanMulSpanAlg    = \x y -> reduceSpanNum (*) MulSpan x y
    , spanDivSpanAlg    = \x y -> reduceSpanNum (/) DivSpan x y
    , spanAbsSpanAlg    = \x   -> mkAbs x
    , spanMinSpanAlg    = \xs  -> sequence xs `b` \xs -> ret (minSpan xs)
    , spanMaxSpanAlg    = \xs  -> sequence xs `b` \xs -> ret (maxSpan xs)
    }
  fixSpansLookupSpanAlgs :: LookupSpanAlg (SrvSt Span) // TODO : If we look something up, store it somewhere and update Span AST accordingly
  fixSpansLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg  = mkImageGridSpan (\xss n -> maxSpan (map fst (transpose xss !! n))) ColumnXSpan
    , lookupSpanRowYSpanAlg     = mkImageGridSpan (\xss n -> maxSpan (map snd (xss !! n))) RowYSpan
    , lookupSpanImageXSpanAlg   = mkImageSpan fst ImageXSpan
    , lookupSpanImageYSpanAlg   = mkImageSpan snd ImageYSpan
    , lookupSpanDescentYSpanAlg = \fd -> ret (LookupSpan (DescentYSpan fd))
    , lookupSpanExYSpanAlg      = \fd -> ret (LookupSpan (ExYSpan fd))
    , lookupSpanTextXSpanAlg    = \fd str -> ret (LookupSpan (TextXSpan fd str))
    }
    where
    lookupTags :: (Set ImageTag) -> SrvSt (Maybe CachedSpan)
    lookupTags ts
      | 'DS'.null ts = ret Nothing
      | otherwise    = go
      where
      go srv
        = case 'DM'.elems ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) srv.srvTaggedSpanEnv) of
            [x:_] -> (Just x, srv)
            _     -> (Nothing, srv)

    mkImageSpan :: (ImageSpan -> Span) ((Set ImageTag) -> LookupSpan) (Set ImageTag) -> SrvSt Span
    mkImageSpan f c ts
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just {cachedImageSpan=Just xs} -> f xs
                      _                              -> LookupSpan (c ts))

    mkImageGridSpan :: ([[ImageSpan]] Int -> Span) ((Set ImageTag) Int -> LookupSpan) (Set ImageTag) Int -> SrvSt Span
    mkImageGridSpan f c ts n
      =        lookupTags ts `b`
      \luts -> ret (case luts of
                      Just csp=:{cachedGridSpans=Just xss} -> f xss n
                      _                                    -> LookupSpan (c ts n))

:: ImageSpanReal :== (Real, Real)

:: ImageOffsetReal :== (Real, Real)

:: ToSVGSyn :== (SVGElt, ImageSpanReal)

mkClipPathId :: String Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

toSVG :: (Image s) -> ClSt s ToSVGSyn | iTask s
toSVG img = \st -> imageCata toSVGAllAlgs img st
  where
  toSVGAllAlgs =
    { imageAlgs          = toSVGImageAlgs
    , imageContentAlgs   = toSVGImageContentAlgs
    , imageAttrAlgs      = toSVGImageAttrAlgs
    , imageTransformAlgs = toSVGImageTransformAlgs
    , imageSpanAlgs      = toSVGImageSpanAlgs
    , basicImageAlgs     = toSVGBasicImageAlgs
    , compositeImageAlgs = toSVGCompositeImageAlgs
    , composeAlgs        = toSVGComposeAlgs
    , spanAlgs           = toSVGSpanAlgs
    , lookupSpanAlgs     = toSVGLookupSpanAlgs
    }
  toSVGImageAlgs :: ImageAlg ([SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> (ClSt s ToSVGSyn))
                             (ClSt s SVGAttr)
                             (ClSt s (SVGTransform, ImageTransform))
                             (ClSt s ToSVGSyn) | iTask s
  toSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage imCo imAts imTrs imTas
      =         sequence imAts `b`
      \imAts -> sequence imTrs `b`
      \imTrs -> finishMkImage imAts imTrs
      where
      finishMkImage imAts imTrs st
        # ((img, sp), st) = imCo imAts imTrs imTas st
        # (img, st) = case compensateRotation imTrs sp of
                        Just (xoff, yoff)
                          = (GElt [] [TransformAttr [TranslateTransform (toString yoff) (toString xoff)]] [img], st) // TODO This is still a bit funky, with yoff and xoff switched etc... Also this might not be the desired behavior in every case. Perhaps this is only desired in grids
                        _ = (img, st)
        = ((img, sp), st)
      compensateRotation :: [(SVGTransform, ImageTransform)] ImageSpanReal -> Maybe ImageOffsetReal
      compensateRotation [(_, RotateImage angle):_] (xspr, yspr) = Just (snd (rotatedImageSpanAndOriginOffset angle (xspr, yspr))) // TODO Support multiple rotations
      compensateRotation [_:xs]                     sp = compensateRotation xs sp
      compensateRotation _                          _  = Nothing

  toSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ClSt s ImageSpanReal)
                                           ([SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           ([SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: (ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
               (ClSt s ImageSpanReal)
               [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) ->
               ClSt s ToSVGSyn | iTask s
    mkBasic baIm imSp imAts imTrs imTas
      =        imSp `b`
      \imSp -> baIm imSp imAts imTrs imTas
  toSVGImageAttrAlgs :: ImageAttrAlg s (ClSt s SVGAttr) | iTask s
  toSVGImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr -> ret (StrokeAttr (PaintColor attr.stroke Nothing))
    , imageAttrStrokeWidthAttrAlg   = \attr -> mkStrokeWidth attr
    , imageAttrStrokeOpacityAttrAlg = \attr -> ret (StrokeOpacityAttr (toString attr.opacity))
    , imageAttrFillAttrAlg          = \attr -> ret (FillAttr (PaintColor attr.fill Nothing))
    , imageAttrFillOpacityAttrAlg   = \attr -> ret (FillOpacityAttr (FillOpacity (toString attr.opacity)))
    , imageAttrOnClickAttrAlg       = \attr -> abort "imageAttrOnClickAttrAlg" // (("onclick", "TODO How?"), st) // TODO
    }
    where
    mkStrokeWidth :: (StrokeWidthAttr s) -> ClSt s SVGAttr | iTask s
    mkStrokeWidth attr
      =     evalSpan attr.strokewidth `b`
      \w -> ret (StrokeWidthAttr (StrokeWidthLength (toString w, PX)))
  toSVGImageTransformAlgs :: ImageTransformAlg Deg (ClSt s Real) (ClSt s (SVGTransform, ImageTransform)) | iTask s
  toSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    -> ret (RotateTransform (toString (toReal imAn)) Nothing, RotateImage imAn)
    , imageTransformSkewXImageAlg  = \imAn    -> ret (SkewXTransform (toString (toReal imAn)), SkewXImage imAn)
    , imageTransformSkewYImageAlg  = \imAn    -> ret (SkewYTransform (toString (toReal imAn)), SkewYImage imAn)
    , imageTransformFitImageAlg    = \sp1 sp2 -> sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (ScaleTransform (toString sp1) (toString sp2), FitImage (px sp1) (px sp2)) // TODO : These aren't really scales... for that we need to actual image dimensions
    , imageTransformFitXImageAlg   = \sp      -> sp `b` \sp -> ret (ScaleTransform (toString sp) "1.0", FitXImage (px sp))                             // TODO : These aren't really scales... for that we need to actual image dimensions
    , imageTransformFitYImageAlg   = \sp      -> sp `b` \sp -> ret (ScaleTransform "1.0" (toString sp), FitYImage (px sp))                             // TODO : These aren't really scales... for that we need to actual image dimensions
    }
  toSVGImageSpanAlgs :: ImageSpanAlg (ClSt s Real) (ClSt s ImageSpanReal) | iTask s
  toSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret (sp1, sp2)
  toSVGBasicImageAlgs :: BasicImageAlg (ClSt s Real) (ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGBasicImageAlgs =
    { basicImageEmptyImageAlg    = mkEmptyImage
    , basicImageTextImageAlg     = mkTextImage
    , basicImageLineImageAlg     = mkLineImage
    , basicImagePolygonImageAlg  = mkPolygonImage
    , basicImagePolylineImageAlg = mkPolylineImage
    , basicImageCircleImageAlg   = mkCircleImage
    , basicImageRectImageAlg     = mkRectImage
    , basicImageEllipseImageAlg  = mkEllipseImage
    }
    where
    mkEmptyImage :: ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkEmptyImage imSp imAts imTrs imTas
      =      mkWH imSp `b`
      \wh -> ret (GElt wh (mkAttrs imAts imTrs) [], imSp)
    mkTextImage :: FontDef String ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkTextImage fd str imSp imAts imTrs imTas
      =      evalSpan fd.fontyspan `b`
      \sp -> ret (TextElt [] (mkAttrs imAts imTrs ++ fontAttrs sp) str, imSp)
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
    mkLineImage :: Slash ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkLineImage sl (imXSp, imYSp) imAts imTrs imTas
      = ret (LineElt [] (mkAttrs imAts imTrs ++ mkLineAttrs sl (imXSp, imYSp)), (imXSp, imYSp))
      where
      mkLineAttrs slash (xspan, yspan)
        # (y1, y2) = case slash of
                       Slash     -> (toString yspan, "0.0")
                       Backslash -> ("0.0", toString yspan)
        = [ X1Attr ("0.0", PX), X2Attr (toString xspan, PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    mkPolygonImage :: [(ClSt s Real, ClSt s Real)] ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkPolygonImage points (imXSp, imYSp) imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> ret (PolygonElt [] [PointsAttr (map (\(x, y) -> (toString x, toString y)) offsets) : mkAttrs imAts imTrs], (imXSp, imYSp))
    mkPolylineImage :: [(ClSt s Real, ClSt s Real)] ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkPolylineImage points (imXSp, imYSp) imAts imTrs imTas
      =           evalOffsets points `b`
      \offsets -> ret (PolylineElt [] [PointsAttr (map (\(x, y) -> (toString x, toString y)) offsets) : mkAttrs imAts imTrs], (imXSp, imYSp))
    mkRectImage :: ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkRectImage imSp imAts imTrs imTas
      =      mkWH imSp `b`
      \wh -> ret (RectElt wh (mkAttrs imAts imTrs), imSp)
    mkCircleImage :: ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkCircleImage (imXSp, imYSp) imAts imTrs imTas
      = ret ( CircleElt [] [ RAttr (toString r, PX), CxAttr (toString r, PX)
            , CyAttr (toString r, PX) : (mkAttrs imAts imTrs) ], (imXSp, imYSp))
      where r = imXSp / 2.0
    mkEllipseImage :: ImageSpanReal [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkEllipseImage (imXSp, imYSp) imAts imTrs imTas
      = ret (EllipseElt [] (mkAttrs imAts imTrs ++ [ RxAttr (toString (imXSp / 2.0), PX), RyAttr (toString (imYSp / 2.0), PX)
                                                   , CxAttr (toString (imXSp / 2.0), PX), CyAttr (toString (imYSp / 2.0), PX)]), (imXSp, imYSp))

    mkWH :: ImageSpanReal -> ClSt s [HtmlAttr] | iTask s
    mkWH (imXSp, imYSp) = ret [WidthAttr (toString imXSp), HeightAttr (toString imYSp)]
  toSVGCompositeImageAlgs :: CompositeImageAlg (ClSt s Real)
                                               (ClSt s ToSVGSyn)
                                               ([ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn)
                                               ([SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    // TODO Calculate transformed image spans
    mkCompositeImage offsets host compose imAts imTrs imTas
      =           evalOffsets offsets `b`
      \offsets -> evalHost host `b`
      \host    -> compose offsets host imAts imTrs imTas `b`
      \compose -> finishMkImage host compose
      where
      finishMkImage (Just (hostImg, hostSpans)) (compose, _) (clval, world)
        # uniqId        = clval.uniqueIdCounter
        # st            = ({ clval & uniqueIdCounter = uniqId + 1 }, world)
        # clipPathId    = mkClipPathId clval.editletId uniqId
        # (hostSps, st) = applyRealTransforms (map snd imTrs) hostSpans st
        # g             = GElt [] (mkAttrs imAts imTrs) [
                            ClipPathElt [IdAttr clipPathId] [] [hostImg]
                          , GElt [StyleAttr ("clip-path: url(#" +++ clipPathId +++ ")")] [] [compose, hostImg]
                          ]
        = ret (g, hostSps) st
      finishMkImage _ (compose, composeSpans) st
        # (composeSpans, st) = applyRealTransforms (map snd imTrs) composeSpans st
        = ret (GElt [] (mkAttrs imAts imTrs) [compose], composeSpans) st
  toSVGComposeAlgs :: ComposeAlg (ClSt s ToSVGSyn) ([ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> (ClSt s ToSVGSyn)) | iTask s
  toSVGComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[ClSt s ToSVGSyn]] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkGrid dims aligns imgss offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr seqImgsGrid ([], st) imgss
        # spanss      = map (map snd) imgss
        # yspans      = map (maxList o map snd) spanss
        # xspans      = map (maxList o map fst) (transpose spanss)
        # infoffs     = offsets ++ repeatn (length xspans * length yspans) (0.0, 0.0)
        # (xsp, ysp)  = maybe ( foldr (\(xsp, off) acc -> xsp + off + acc) 0.0 (zip2 xspans (map fst infoffs))
                              , foldr (\(ysp, off) acc -> ysp + off + acc) 0.0 (zip2 yspans (map snd infoffs))
                              )
                              snd mbhost
        = ret ( GElt [] [] (mkGridChildren xspans yspans imgss infoffs)
              , maybe (xsp, ysp) snd mbhost) st
      mkGridChildren xspans yspans imgss infoffs
        = (flatten o (\(x, _, _, _) -> x)) (foldl (mkRows xspans) ([], 0.0, aligns ++ repeatn (length xspans * length yspans) (AtLeft, AtTop), infoffs) (zip2 imgss yspans))
        where
        mkRows xspans (acc, yoff, aligns, offsets) (imgs, yspan)
          # imgsLength = length imgs
          = ( [fst (foldl (mkCols yspan yoff) ([], 0.0) (zip4 imgs xspans (take imgsLength aligns) (take imgsLength offsets))) : acc]
            , yoff + yspan, drop imgsLength aligns, drop imgsLength offsets)
        mkCols yspan yoff (acc, xoff) ((img, imgSpan), xspan, align, (manxoff, manyoff))
          # (xoff`, yoff`) = calcOffset xspan yspan imgSpan align
          = ( [GElt [WidthAttr (toString xspan), HeightAttr (toString yspan)]
                    [TransformAttr [TranslateTransform (toString (xoff` + xoff + manxoff)) (toString (yoff` + yoff + manyoff))]] [img]:acc]
            , xoff + xspan)

      seqImgsGrid :: [ClSt m ToSVGSyn] *([[ToSVGSyn]], *(St m)) -> *([[ToSVGSyn]], *(St m)) | iTask m
      seqImgsGrid imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: [ClSt s ToSVGSyn] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkCollage imgs offsets mbhost imAts imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> zipWithSt mkTranslateGroup offsets imgsSps `b`
      \trimgs  -> ret ( GElt [] [] (map fst trimgs)
                      , maybe (calculateComposedSpan (map snd imgsSps) offsets) snd mbhost)
    mkOverlay :: [ImageAlign] [ClSt s ToSVGSyn] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [(SVGTransform, ImageTransform)] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkOverlay aligns imgs offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # images        = map fst imgsSps
        # numImgs       = length images
        # spans         = map snd imgsSps
        # maxXSpan      = maxList (map fst spans)
        # maxYSpan      = maxList (map snd spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) snd mbhost
        # alignOffs            = zipWith addOffset (zipWith (calcOffset maxXSpan maxYSpan) spans (aligns ++ repeatn numImgs (AtLeft, AtTop)))
                                                   (offsets ++ repeatn numImgs (0.0, 0.0))
        # (imgs, st)           = zipWithSt mkTranslateGroup alignOffs imgsSps st
        = ret ( GElt [] [] (reverse (map fst imgs))
              , maybe (calculateComposedSpan spans offsets) snd mbhost) st
        where
        addOffset (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  toSVGSpanAlgs :: SpanAlg (ClSt s Real) (ClSt s Real) | iTask s
  toSVGSpanAlgs = evalSpanSpanAlgs

  toSVGLookupSpanAlgs :: LookupSpanAlg (ClSt s Real) | iTask s
  toSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

t val (clval, jsworld)
  # jsworld = jsTrace val jsworld
  = (clval, jsworld)

applyRealTransforms :: [ImageTransform] ImageSpanReal -> ClSt s ImageSpanReal | iTask s
applyRealTransforms ts (xspr, yspr) = go
  where
  go st
    # (xsp, ysp) = applyTransforms ts (px xspr, px yspr)
    # (xsp, st)  = evalSpan xsp st
    # (ysp, st)  = evalSpan ysp st
    = ret (xsp, ysp) st

evalOffsets :: [(.st -> .(a, .st), .st -> .(a, .st))] .st -> .([(a, a)], .st)
evalOffsets offsets st = foldr f ([], st) offsets
  where
  f (sp1, sp2) (xs, st)
    # (sp1, st) = sp1 st
    # (sp2, st) = sp2 st
    = ([(sp1, sp2):xs], st)

evalHost :: (Maybe (State .s a)) -> State .s (Maybe a)
evalHost (Just x) = x `b` \x -> ret (Just x)
evalHost _        = ret Nothing

ret x :== \st -> (x, st)

(`b`) f g :== \st0 -> let (r,st1) = f st0 in g r st1

mkAttrs :: [SVGAttr] [(SVGTransform, ImageTransform)] -> [SVGAttr]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [TransformAttr (map fst xs):imAts]

calcOffset :: Real Real ImageSpanReal ImageAlign -> ImageOffsetReal
calcOffset mxsp mysp (imXSp, imYSp) (xal, yal) = (mkXAl xal, mkYAl yal)
  where
  mkXAl AtLeft    = 0.0
  mkXAl AtMiddleX = (mxsp / 2.0) - (imXSp / 2.0)
  mkXAl AtRight   = mxsp - imXSp
  mkYAl AtTop     = 0.0
  mkYAl AtMiddleY = (mysp / 2.0) - (imYSp / 2.0)
  mkYAl AtBottom  = mysp - imYSp

calculateComposedSpan :: [(a, a)] [(a, a)] -> (a, a) | span a
calculateComposedSpan spans offs
  = foldr f (zero, zero) (zip2 (offs ++ repeatn (length spans) (zero, zero)) spans)
  where
  f ((xoff, yoff), (imXSp, imYSp)) (maxX, maxY)
    # maxX = maxOf [maxX, xoff + imXSp]
    # maxY = maxOf [maxY, yoff + imYSp]
    = (maxX, maxY)

mkTranslateGroup :: ImageOffsetReal ToSVGSyn -> ClSt s ToSVGSyn | iTask s
mkTranslateGroup (xoff, yoff) (contents, imSp)
  = ret (GElt [] (mkTranslateAttr (xoff, yoff)) [contents], imSp)
  where
  mkTranslateAttr :: (Real, Real) -> [SVGAttr]
  mkTranslateAttr (0.0,   0.0)   = []
  mkTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

undef = undef

evalSpan :: Span -> ClSt s Real | iTask s
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

reduceSpanBin :: (Real Real -> Real) (Span Span -> Span) (.st -> .(Span, .st)) (.st -> .(Span, .st)) -> (.st -> .(Span, .st))
reduceSpanBin op cons x y
  =     x `b`
  \x -> y `b`
  \y -> ret (case (x, y) of
              (PxSpan x`, PxSpan y`) -> PxSpan (op x` y`)
              (x`, y`)               -> cons x` y`)

reduceSpanNum :: (Real Real -> Real) (Span Real -> Span) (.st -> .(Span, .st)) Real -> (.st -> .(Span, .st))
reduceSpanNum op cons x y
  =     x `b`
  \x -> ret (case x of
              PxSpan x` -> PxSpan (op x` y)
              x`        -> cons x` y)


:: Algebras m imCo imAt imTr im baIm imSp coIm imAn ho co sp loSp =
  { imageAlgs          :: ImageAlg imCo imAt imTr im
  , imageContentAlgs   :: ImageContentAlg baIm imSp coIm imCo
  , imageAttrAlgs      :: ImageAttrAlg m imAt
  , imageTransformAlgs :: ImageTransformAlg imAn sp imTr
  , imageSpanAlgs      :: ImageSpanAlg sp imSp
  , basicImageAlgs     :: BasicImageAlg sp baIm
  , compositeImageAlgs :: CompositeImageAlg sp ho co coIm
  , composeAlgs        :: ComposeAlg im co
  , spanAlgs           :: SpanAlg loSp sp
  , lookupSpanAlgs     :: LookupSpanAlg loSp
  }

:: ImageAlg imCo imAt imTr im =
  { imageAlg :: imCo [imAt] [imTr] (Set ImageTag) -> im
  }

:: ImageContentAlg baIm imSp coIm imCo =
  { imageContentBasicAlg     :: baIm imSp -> imCo
  , imageContentCompositeAlg :: coIm      -> imCo
  }

:: ImageAttrAlg m imAt =
  { imageAttrImageStrokeAttrAlg   :: (StrokeAttr m)      -> imAt
  , imageAttrStrokeWidthAttrAlg   :: (StrokeWidthAttr m) -> imAt
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

:: BasicImageAlg sp baIm =
  { basicImageEmptyImageAlg    ::                   baIm
  , basicImageTextImageAlg     :: FontDef String -> baIm
  , basicImageLineImageAlg     :: Slash          -> baIm
  , basicImageCircleImageAlg   ::                   baIm
  , basicImageRectImageAlg     ::                   baIm
  , basicImageEllipseImageAlg  ::                   baIm
  , basicImagePolygonImageAlg  :: [(sp, sp)]     -> baIm
  , basicImagePolylineImageAlg :: [(sp, sp)]     -> baIm
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

imageCata allAlgs { Image | content, attribs, transform, tags }
  # synContent    = imageContentCata allAlgs content
  # synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) attribs
  # synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  = allAlgs.imageAlgs.imageAlg synContent synsAttribs synsTransform tags

imageContentCata allAlgs (Basic bi is)
  # synBasicImage = basicImageCata allAlgs.basicImageAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs bi
  # synImageSpan  = imageSpanCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs is
  = allAlgs.imageContentAlgs.imageContentBasicAlg synBasicImage synImageSpan
imageContentCata allAlgs (Composite ci)
  # synCompositeImage = compositeImageCata allAlgs ci
  = allAlgs.imageContentAlgs.imageContentCompositeAlg synCompositeImage

imageAttrCata imageAttrAlgs (ImageStrokeAttr sa)         = imageAttrAlgs.imageAttrImageStrokeAttrAlg sa
imageAttrCata imageAttrAlgs (ImageStrokeWidthAttr swa)   = imageAttrAlgs.imageAttrStrokeWidthAttrAlg swa
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

basicImageCata basicImageAlgs _ _ EmptyImage          = basicImageAlgs.basicImageEmptyImageAlg
basicImageCata basicImageAlgs _ _ (TextImage fd str)  = basicImageAlgs.basicImageTextImageAlg fd str
basicImageCata basicImageAlgs _ _ (LineImage sl)      = basicImageAlgs.basicImageLineImageAlg sl
basicImageCata basicImageAlgs spanAlgs lookupSpanAlgs (PolygonImage offsets)
  # synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = basicImageAlgs.basicImagePolygonImageAlg synsImageOffset
basicImageCata basicImageAlgs spanAlgs lookupSpanAlgs (PolylineImage offsets)
  # synsImageOffset = foldrOffsets spanAlgs lookupSpanAlgs offsets
  = basicImageAlgs.basicImagePolylineImageAlg synsImageOffset
basicImageCata basicImageAlgs _ _ CircleImage         = basicImageAlgs.basicImageCircleImageAlg
basicImageCata basicImageAlgs _ _ RectImage           = basicImageAlgs.basicImageRectImageAlg
basicImageCata basicImageAlgs _ _ EllipseImage        = basicImageAlgs.basicImageEllipseImageAlg

imageSpanCata imageSpanAlgs spanAlgs lookupSpanAlgs (xspan, yspan)
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
appendSVG (SVGElt            htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "svg" htmlAttrs svgAttrs svgElts            world
appendSVG (ClipPathElt       htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "clipPath" htmlAttrs svgAttrs svgElts       world
appendSVG (CircleElt         htmlAttrs svgAttrs        ) parent world = appendSVG` parent "circle" htmlAttrs svgAttrs []              world
appendSVG (DefsElt           htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "defs" htmlAttrs svgAttrs svgElts           world
appendSVG (EllipseElt        htmlAttrs svgAttrs        ) parent world = appendSVG` parent "ellipse" htmlAttrs svgAttrs []             world
appendSVG (GElt              htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "g" htmlAttrs svgAttrs svgElts              world
appendSVG (ImageElt          htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "image" htmlAttrs svgAttrs svgElts          world
appendSVG (LinearGradientElt htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "linearGradient" htmlAttrs svgAttrs svgElts world
appendSVG (LineElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "line" htmlAttrs svgAttrs []                world
appendSVG (PolygonElt        htmlAttrs svgAttrs        ) parent world = appendSVG` parent "polygon" htmlAttrs svgAttrs []             world
appendSVG (PolylineElt       htmlAttrs svgAttrs        ) parent world = appendSVG` parent "polyline" htmlAttrs svgAttrs []             world
appendSVG (RectElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "rect" htmlAttrs svgAttrs []                world
appendSVG (RadialGradientElt htmlAttrs svgAttrs svgElts) parent world = appendSVG` parent "radialGradient" htmlAttrs svgAttrs svgElts world
appendSVG (StopElt           htmlAttrs svgAttrs        ) parent world = appendSVG` parent "stop" htmlAttrs svgAttrs []                world
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
