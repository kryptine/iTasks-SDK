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
  ImageContent, BasicImage, ImageSpan, CompositeImage, Slash, FontDef, Compose,
  OpacityAttr, FillAttr, StrokeWidthAttr, StrokeAttr, OnClickAttr, XAlign,
  YAlign, Set, CachedSpan, SVGletDiff

derive gLexOrd FontDef, Span, LookupSpan, ImageTag, Set, CachedSpan, ImageSpan

:: ClientState s =
  { didInit         :: Bool
  , didDraw         :: Bool
  , textXSpanEnv    :: Map (FontDef, String) Real
  , textYSpanEnv    :: Map FontDef (Real, Real)
  , uniqueIdCounter :: Int
  , image           :: Maybe (Image s)
  , editletId       :: String
  }

:: CachedSpan
  = CachedGridSpan [[ImageSpan]]
  | CachedImageSpan ImageSpan

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
    # ((img, imSp), (_, world))     = toSVG img ({clval & editletId = cid}, world)
    # (svg, world)          = getDomElement (mainSvgId cid) world
    # (_, world)            = (svg `setAttribute` ("height", imSp.yspr)) world
    # (_, world)            = (svg `setAttribute` ("width", imSp.xspr)) world
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

// TODO : Detect divergence due to lookups and return an Either ErrorMessage (Image s), instead of just an Image s
fixSpans :: (Image s) -> SrvSt (Image s, ImageSpan) | iTask s
fixSpans img = go
  where
  go st
    # ((img`, sp), st) = imageCata fixSpansAllAlgs img st
    | img === img` = ((img, sp), st)
    | otherwise    = fixSpans img` st
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
        # (imAts, st) = sequence imAts st
        # (imTrs, st) = sequence imTrs st
        # ((content, sp), st)  = imCo imAts imTrs imTas st
        # st = { st & srvTaggedSpanEnv = 'DM'.put imTas (CachedImageSpan sp) st.srvTaggedSpanEnv }
        = ret ({ Image | content = content, attribs = imAts, transform = imTrs, tags = imTas}, sp) st // TODO transforms can influence size as well...
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
    mkBasic baIm imSp imAts imTrs imTas = imSp `b` \imSp -> baIm imSp imAts imTrs imTas
  fixSpansImageAttrAlgs :: ImageAttrAlg s (SrvSt (ImageAttr s))
  fixSpansImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = ret o ImageStrokeAttr
    , imageAttrStrokeWidthAttrAlg   = ret o ImageStrokeWidthAttr
    , imageAttrStrokeOpacityAttrAlg = ret o ImageStrokeOpacityAttr
    , imageAttrFillAttrAlg          = ret o ImageFillAttr
    , imageAttrFillOpacityAttrAlg   = ret o ImageFillOpacityAttr
    , imageAttrOnClickAttrAlg       = ret o ImageOnClickAttr
    }
  fixSpansImageTransformAlgs :: ImageTransformAlg Real (SrvSt Span) (SrvSt ImageTransform)
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
    mkFitImage sp1 sp2 = sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (FitImage sp1 sp2)
    mkFitDim :: (Span -> ImageTransform) (SrvSt Span) -> SrvSt ImageTransform
    mkFitDim ctr sp = sp `b` \sp -> ret (ctr sp)
  fixSpansImageSpanAlgs :: ImageSpanAlg (SrvSt Span) (SrvSt ImageSpan)
  fixSpansImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: (SrvSt Span) (SrvSt Span) -> SrvSt ImageSpan
    mkSpan xspan yspan = xspan `b` \xspan -> yspan `b` \yspan -> ret { xspan = xspan, yspan = yspan }
  fixSpansBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg   = \       imSp _ imTrs imTas -> mkSpan EmptyImage         imSp imTrs imTas
    , basicImageTextImageAlg    = \fd str imSp _ imTrs imTas -> mkSpan (TextImage fd str) imSp imTrs imTas
    , basicImageLineImageAlg    = \sl     imSp _ imTrs imTas -> mkSpan (LineImage sl)     imSp imTrs imTas
    , basicImageCircleImageAlg  = \       imSp _ imTrs imTas -> mkSpan CircleImage        imSp imTrs imTas
    , basicImageRectImageAlg    = \       imSp _ imTrs imTas -> mkSpan RectImage          imSp imTrs imTas
    , basicImageEllipseImageAlg = \       imSp _ imTrs imTas -> mkSpan EllipseImage       imSp imTrs imTas
    }
    where
    mkSpan :: BasicImage ImageSpan [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan)
    mkSpan val imSp imTrs imTas = ret (Basic val imSp, applyTransforms imTrs imSp)
  fixSpansCompositeImageAlgs :: CompositeImageAlg (SrvSt Span)
                                                  (SrvSt (Image s, ImageSpan))
                                                  ([ImageOffset] (Maybe (Image s, ImageSpan)) -> SrvSt (Compose s, ImageSpan))
                                                  ([ImageAttr s] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent s, ImageSpan))
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose imAts imTrs imTas st
      # (offsets, st) = evalOffsets offsets st
      # (host, st)    = evalHost host st
      # ((compose, composeSpan), st) = compose offsets host st
      = case host of
          Just (hostImg, hostSpan) -> ret (Composite {CompositeImage | offsets = offsets, host = Just hostImg, compose = compose}, applyTransforms imTrs hostSpan) st
          _                        -> ret (Composite {CompositeImage | offsets = offsets, host = Nothing, compose = compose}, applyTransforms imTrs composeSpan) st
  fixSpansComposeAlgs :: ComposeAlg (SrvSt (Image s, ImageSpan)) ([ImageOffset] (Maybe (Image s, ImageSpan)) -> SrvSt (Compose s, ImageSpan))
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[SrvSt (Image s, ImageSpan)]] [ImageOffset] (Maybe (Image s, ImageSpan)) -> SrvSt (Compose s, ImageSpan)
    mkGrid dims ias imgss offsets mbhost = go
      where
      go st
        # (imgss, st) = foldr f ([], st) imgss
        # spanss = map (map snd) imgss
        # yspans = map (maxSpan o map (\x -> x.yspan)) spanss
        # xspans = map (maxSpan o map (\x -> x.xspan)) (transpose spanss)
        // TODO Add offsets
        # (xsp, ysp) = maybe (foldr (+) (px 0.0) xspans, foldr (+) (px 0.0) yspans) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
        = ret (AsGrid dims ias (map (map fst) imgss), { xspan = xsp, yspan = ysp }) st
      f :: [SrvSt (Image s, ImageSpan)] *([[(Image s, ImageSpan)]], ServerState) -> *([[(Image s, ImageSpan)]], ServerState)
      f imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: [SrvSt (Image s, ImageSpan)] [ImageOffset] (Maybe (Image s, ImageSpan)) -> SrvSt (Compose s, ImageSpan)
    mkCollage imgs offsets mbhost = sequence imgs `b`
                        \imgsSps -> ret ( AsCollage (map fst imgsSps)
                                        , maybe (calculateComposedSpan (map snd imgsSps) offsets) snd mbhost)
    mkOverlay :: [ImageAlign] [SrvSt (Image s, ImageSpan)] [ImageOffset] (Maybe (Image s, ImageSpan)) -> SrvSt (Compose s, ImageSpan)
    mkOverlay ias imgs offsets mbhost = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # spans         = map snd imgsSps
        # maxXSpan      = maxSpan (map (\x -> x.xspan) spans)
        # maxYSpan      = maxSpan (map (\x -> x.yspan) spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
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
    , spanMinSpanAlg    = \xs  -> reduceSpanList min MinSpan xs
    , spanMaxSpanAlg    = \xs  -> reduceSpanList max MaxSpan xs
    }
  fixSpansLookupSpanAlgs :: LookupSpanAlg (SrvSt Span) // TODO : If we look something up, store it somewhere and update Span AST accordingly
  fixSpansLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg  = mkImageGridSpan (\xss _ -> maxSpan (map (\x -> x.xspan) (map hd xss))) ColumnXSpan // TODO Will fail hard if it fails
    , lookupSpanRowYSpanAlg     = mkImageGridSpan (\xss n -> maxSpan (map (\x -> x.yspan) (xss !! n))) RowYSpan      // TODO Will fail hard if it fails
    , lookupSpanImageXSpanAlg   = mkImageSpan (\x -> x.xspan) ImageXSpan
    , lookupSpanImageYSpanAlg   = mkImageSpan (\x -> x.yspan) ImageYSpan
    , lookupSpanDescentYSpanAlg = \fd -> ret (LookupSpan (DescentYSpan fd))
    , lookupSpanExYSpanAlg      = \fd -> ret (LookupSpan (ExYSpan fd))
    , lookupSpanTextXSpanAlg    = \fd str -> ret (LookupSpan (TextXSpan fd str))
    }
    where
    lookupTags :: (Set ImageTag) -> SrvSt (Maybe CachedSpan)
    lookupTags ts = go
      where
      go srv
        = case 'DM'.toList ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) srv.srvTaggedSpanEnv) of
            [(_, x):_] -> (Just x, srv)
            _          -> (Nothing, srv)

    mkImageSpan :: (ImageSpan -> Span) ((Set ImageTag) -> LookupSpan) (Set ImageTag) -> SrvSt Span
    mkImageSpan f c ts = go
      where
      go st
        = case lookupTags ts st of
            (Just (CachedImageSpan x), st) -> (f x, st)
            (_                       , st) -> (LookupSpan (c ts), st)

    mkImageGridSpan :: ([[ImageSpan]] Int -> Span) ((Set ImageTag) Int -> LookupSpan) (Set ImageTag) Int -> SrvSt Span
    mkImageGridSpan f c ts n = go
      where
      go st
        = case lookupTags ts st of
            (Just (CachedGridSpan xss), st) -> (f xss n, st)
            (_                        , st) -> (LookupSpan (c ts n), st)

:: ImageSpanReal =
  { xspr :: Real
  , yspr :: Real
  }

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
  toSVGImageAlgs :: ImageAlg ([SVGAttr] [SVGTransform] (Set ImageTag) -> (ClSt s ToSVGSyn))
                             (ClSt s SVGAttr)
                             (ClSt s SVGTransform)
                             (ClSt s ToSVGSyn) | iTask s
  toSVGImageAlgs =
    { imageAlg = mkImage
    }
    where // TODO transforms can influence size as well...
    mkImage imCo imAts imTrs imTas
      =         sequence imAts `b`
      \imAts -> sequence imTrs `b`
      \imTrs -> imCo imAts imTrs imTas

  toSVGImageContentAlgs :: ImageContentAlg (ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           (ClSt s ImageSpanReal)
                                           ([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn)
                                           ([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: (ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn)
               (ClSt s ImageSpanReal)
               [SVGAttr] [SVGTransform] (Set ImageTag) ->
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
  toSVGImageTransformAlgs :: ImageTransformAlg Real (ClSt s Real) (ClSt s SVGTransform) | iTask s
  toSVGImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    -> ret (RotateTransform (toString imAn) Nothing)
    , imageTransformSkewXImageAlg  = \imAn    -> ret (SkewXTransform (toString imAn))
    , imageTransformSkewYImageAlg  = \imAn    -> ret (SkewYTransform (toString imAn))
    , imageTransformFitImageAlg    = \sp1 sp2 -> sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret (ScaleTransform (toString sp1) (toString sp2)) // TODO : These aren't really scales... for that we need to actual image dimensions
    , imageTransformFitXImageAlg   = \sp      -> sp `b` \sp -> ret (ScaleTransform (toString sp) "1.0")                             // TODO : These aren't really scales... for that we need to actual image dimensions
    , imageTransformFitYImageAlg   = \sp      -> sp `b` \sp -> ret (ScaleTransform "1.0" (toString sp))                             // TODO : These aren't really scales... for that we need to actual image dimensions
    }
  toSVGImageSpanAlgs :: ImageSpanAlg (ClSt s Real) (ClSt s ImageSpanReal) | iTask s
  toSVGImageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan sp1 sp2
      =       sp1 `b`
      \sp1 -> sp2 `b`
      \sp2 -> ret { ImageSpanReal | xspr = sp1, yspr = sp2}
  toSVGBasicImageAlgs :: BasicImageAlg (ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGBasicImageAlgs =
    { basicImageEmptyImageAlg   = mkEmptyImage
    , basicImageTextImageAlg    = mkTextImage
    , basicImageLineImageAlg    = mkLineImage
    , basicImageCircleImageAlg  = mkCircleImage
    , basicImageRectImageAlg    = mkRectImage
    , basicImageEllipseImageAlg = mkEllipseImage
    }
    where
    mkEmptyImage :: ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkEmptyImage imSp imAts imTrs imTas
      =      mkWH imSp `b`
      \wh -> ret (GElt wh (mkAttrs imAts imTrs) [], imSp)
    mkTextImage :: FontDef String ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkTextImage fd str imSp imAts imTrs imTas
      =      evalSpan fd.fontyspan `b`
      \sp -> ret (TextElt [] (mkAttrs imAts imTrs ++ fontAttrs sp) str, imSp)
      where
      fontAttrs fsz = [ FontFamilyAttr fd.fontfamily
                      , FontSizeAttr (toString fsz)
                      , FontStyleAttr fd.fontstyle
                      , FontStretchAttr fd.fontstretch
                      , FontVariantAttr fd.fontvariant
                      , FontWeightAttr fd.fontweight
                      ]
    mkLineImage :: Slash ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkLineImage sl imSp imAts imTrs imTas
      = ret (LineElt [] (mkAttrs imAts imTrs ++ mkLineAttrs sl (imSp.xspr, imSp.yspr)), imSp)
      where
      mkLineAttrs slash (xspan, yspan)
        # (y1, y2) = case slash of
                       Slash     -> (toString yspan, "0.0")
                       Backslash -> ("0.0", toString yspan)
        = [ X1Attr ("0.0", PX), X2Attr (toString xspan, PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    mkRectImage :: ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkRectImage imSp imAts imTrs imTas
      =      mkWH imSp `b`
      \wh -> ret (RectElt wh (mkAttrs imAts imTrs) , imSp)
    mkCircleImage :: ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkCircleImage imSp imAts imTrs imTas
      =  ret ( CircleElt [] [ RAttr (toString r, PX), CxAttr (toString r, PX)
             , CyAttr (toString r, PX) : (mkAttrs imAts imTrs) ], imSp)
      where r = imSp.xspr / 2.0
    mkEllipseImage :: ImageSpanReal [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkEllipseImage imSp imAts imTrs imTas
      = ret (EllipseElt [] (mkAttrs imAts imTrs ++ [ RxAttr (toString (imSp.xspr / 2.0), PX), RyAttr (toString (imSp.yspr / 2.0), PX)
                                                   , CxAttr (toString (imSp.xspr / 2.0), PX), CyAttr (toString (imSp.yspr / 2.0), PX)]), imSp)

    mkWH :: ImageSpanReal -> ClSt s [HtmlAttr] | iTask s
    mkWH imSp = ret [WidthAttr (toString imSp.xspr), HeightAttr (toString imSp.yspr)]
  toSVGCompositeImageAlgs :: CompositeImageAlg (ClSt s Real)
                                               (ClSt s ToSVGSyn)
                                               ([ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn)
                                               ([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn) | iTask s
  toSVGCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose imAts imTrs imTas st
      # (offsets, st) = evalOffsets offsets st
      # (host, st)    = evalHost host st
      # ((compose, composeSpan), (clval, world)) = compose offsets host imAts imTrs imTas st
      # uniqId = clval.uniqueIdCounter
      # st     = ({ clval & uniqueIdCounter = uniqId + 1 }, world)
      # clipPathId = mkClipPathId clval.editletId uniqId
      = case host of
          Just (hostImg, hostSpan) -> let g = GElt [] [] [
                                                ClipPathElt [IdAttr clipPathId] [] [hostImg]
                                              , GElt [StyleAttr ("clip-path: url(#" +++ clipPathId +++ ")")] [] [compose, hostImg]
                                              ]
                                      in  ret (g, hostSpan) st
          _                        -> ret (compose, composeSpan) st
      //# ((imgs, compSp), st) = addComposition ho co offs st
      //= ret (GElt [] imAts (map fst imgs), compSp) st
    //addComposition mbhost (AsGrid (cols, rows) aligns imgs) offs st
      //# (conts, st) = mapSt id imgs st
      //# (imgs, st)  = evalList imgs st
      //# maxXSpan    = maxSpan (map ((\x -> x.xspan) o snd) imgs)
      //# maxYSpan    = maxSpan (map ((\x -> x.yspan) o snd) imgs)
      //# (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
      //# (rowImgs, colImgs)   = partitionImages cols rows imgs
      //# alignOffs            = zipWith (+) (zipWith (calcOffset maxXSpan maxYSpan) imgs (aligns ++ repeat (AtLeft, AtTop)))
                                           //(offs ++ repeat (px 0.0, px 0.0))
      //# (imgs, st)           = zipWithSt mkTranslateGroup (alignOffs ++ [(px 0.0, px 0.0)]) (maybe imgs (\h -> imgs ++ [h]) mbhost) st
      //= ((imgs, undef), st)
      //where
      //partitionImages = undef
    //addComposition mbhost (AsOverlay aligns imgs) offs st
      //# (conts, st) = mapSt id imgs st
      //# (imgs, st)  = evalList imgs st
      //# maxXSpan    = maxSpan (map ((\x -> x.xspan) o snd) imgs)
      //# maxYSpan    = maxSpan (map ((\x -> x.yspan) o snd) imgs)
      //# (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
      //# sp                   = maybe (calculateComposedSpanReal imgs offs) snd mbhost
      //# alignOffs            = zipWith (+) (zipWith (calcOffset maxXSpan maxYSpan) imgs (aligns ++ repeat (AtLeft, AtTop)))
                                           //(offs ++ repeat (px 0.0, px 0.0))
      //# (imgs, st)           = zipWithSt mkTranslateGroup (alignOffs ++ [(px 0.0, px 0.0)]) (maybe imgs (\h -> imgs ++ [h]) mbhost) st
      //= ((imgs, sp), st)
    //addComposition mbhost (AsCollage imgs) offs st
      //# sp         = maybe (calculateComposedSpanReal imgs offs) snd mbhost
      //# (imgs, st) = zipWithSt mkTranslateGroup offs imgs st
      //= ((imgs, sp), st)
  toSVGComposeAlgs :: ComposeAlg (ClSt s ToSVGSyn) ([ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> (ClSt s ToSVGSyn)) | iTask s
  toSVGComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[ClSt s ToSVGSyn]] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkGrid dims aligns imgss offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr seqImgsGrid ([], st) imgss
        # imagess = map (map fst) imgss
        # spanss  = map (map snd) imgss
        # yspans  = map (maxList o map (\x -> x.yspr)) spanss
        # xspans  = map (maxList o map (\x -> x.xspr)) (transpose spanss)
        //// TODO Add offsets and host
        # (xsp, ysp) = maybe (foldr (+) 0.0 xspans, foldr (+) 0.0 yspans) (\(_, sp) -> (sp.xspr, sp.yspr)) mbhost
        = ret ( GElt [] (mkAttrs imAts imTrs) (mkGridChildren xspans yspans imagess)
              , { xspr = xsp, yspr = ysp }) st
      mkGridChildren xspans yspans imagess = (flatten o fst) (foldl (mkRows xspans) ([], 0.0) (zip2 imagess yspans))
        where
        mkRows xspans     (acc, yoff) (imgs, yspan) = ( [fst (foldl (mkCols yspan yoff) ([], 0.0) (zip2 imgs xspans)) : acc], yoff + yspan)
        mkCols yspan yoff (acc, xoff) (img, xspan)  = ( [GElt [WidthAttr (toString xspan), HeightAttr (toString yspan)]
                                                              [TransformAttr [TranslateTransform (toString xoff) (toString yoff)]] [img]:acc]
                                                      , xoff + xspan)
      seqImgsGrid :: [ClSt m ToSVGSyn] *([[ToSVGSyn]], *(St m)) -> *([[ToSVGSyn]], *(St m)) | iTask m
      seqImgsGrid imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: [ClSt s ToSVGSyn] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkCollage imgs offsets mbhost imAts imTrs imTas
      =           sequence imgs `b`
      \imgsSps -> zipWithSt mkTranslateGroup offsets imgsSps `b`
      \trimgs  -> ret ( GElt [] (mkAttrs imAts imTrs) (map fst trimgs) // TODO Add host
                      , maybe (calculateComposedSpanReal (map snd imgsSps) offsets) snd mbhost)
    mkOverlay :: [ImageAlign] [ClSt s ToSVGSyn] [ImageOffsetReal] (Maybe ToSVGSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt s ToSVGSyn | iTask s
    mkOverlay aligns imgs offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # images        = map fst imgsSps
        # spans         = map snd imgsSps
        # maxXSpan      = maxList (map (\x -> x.xspr) spans)
        # maxYSpan      = maxList (map (\x -> x.yspr) spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\(_, sp) -> (sp.xspr, sp.yspr)) mbhost
        # alignOffs            = zipWith addOffset (zipWith (calcOffset maxXSpan maxYSpan) spans (aligns ++ repeat (AtLeft, AtTop)))
                                                   (offsets ++ repeat (0.0, 0.0))
        # (imgs, st)           = zipWithSt mkTranslateGroup (maybe alignOffs (\_ -> [(0.0, 0.0):alignOffs]) mbhost) (maybe imgsSps (\h -> [h:imgsSps]) mbhost) st
        = ret ( GElt [] (mkAttrs imAts imTrs) (reverse (map fst imgs))
              , maybe (calculateComposedSpanReal spans offsets) snd mbhost) st
        where
        addOffset (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    //mkFontYSpan :: ((Span, Span) -> Span) (FontDef -> LookupSpan) FontDef -> SrvSt Span
    //mkFontYSpan sel c fd = go
      //where
      //go (clval, world)
        //= case 'DM'.gGet fd clval.textYSpanEnv of
            //Just x -> (sel x, (clval, world))
            //_      -> (LookupSpan (c fd), (clval, world))

    //mkImageSpan :: (ImageSpan -> Span) ((Set ImageTag) -> LookupSpan) (Set ImageTag) -> SrvSt Span
    //mkImageSpan f c ts = go
      //where
      //go st
        //= case lookupTags ts st of
            //(Just (CachedImageSpan x), st) -> (f x, st)
            //(_                       , st) -> (LookupSpan (c ts), st)

    //mkImageGridSpan :: ([[ImageSpan]] Int -> Span) ((Set ImageTag) Int -> LookupSpan) (Set ImageTag) Int -> SrvSt Span
    //mkImageGridSpan f c ts n = go
      //where
      //go st
        //= case lookupTags ts st of
            //(Just (CachedGridSpan xss), st) -> (f xss n, st)
            //(_                        , st) -> (LookupSpan (c ts n), st)

  toSVGSpanAlgs :: SpanAlg (ClSt s Real) (ClSt s Real) | iTask s
  toSVGSpanAlgs = evalSpanSpanAlgs

  toSVGLookupSpanAlgs :: LookupSpanAlg (ClSt s Real) | iTask s
  toSVGLookupSpanAlgs = evalSpanLookupSpanAlgs

evalList xs st :==
  let f x (xs, st)
        # (x, st) = x st
        = ([x:xs], st)
  in  foldr f ([], st) xs

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
(`b2`) f g :== \st0 -> let (r,st1) = f st0 in (let (_, st2) = g r st1 in st2)

mkAttrs :: [SVGAttr] [SVGTransform] -> [SVGAttr]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [TransformAttr xs:imAts]


calcOffset :: Real Real ImageSpanReal ImageAlign -> ImageOffsetReal
calcOffset mxsp mysp imSp (xal, yal) = (mkXAl xal, mkYAl yal)
  where
  mkXAl AtLeft    = 0.0
  mkXAl AtMiddleX = (mxsp / 2.0) - (imSp.xspr / 2.0)
  mkXAl AtRight   = mxsp - imSp.xspr
  mkYAl AtTop     = 0.0
  mkYAl AtMiddleY = (mysp / 2.0) - (imSp.yspr / 2.0)
  mkYAl AtBottom  = mysp - imSp.yspr

calculateComposedSpan :: [ImageSpan] [ImageOffset] -> ImageSpan
calculateComposedSpan spans offs
  = foldr f {xspan = px 0.0, yspan = px 0.0} (zip2 (offs ++ repeat (px 0.0, px 0.0)) spans)
  where
  f ((xoff, yoff), imSp) {xspan = maxX, yspan = maxY}
    # maxX = maxSpan [maxX, xoff + imSp.xspan]
    # maxY = maxSpan [maxY, yoff + imSp.yspan]
    = {xspan = maxX, yspan = maxY}

calculateComposedSpanReal :: [ImageSpanReal] [ImageOffsetReal] -> ImageSpanReal
calculateComposedSpanReal spans offs
  = foldr f {xspr = 0.0, yspr = 0.0} (zip2 (offs ++ repeat (0.0, 0.0)) spans)
  where
  f ((xoff, yoff), imSp) {xspr = maxX, yspr = maxY}
    # maxX = maxList [maxX, xoff + imSp.xspr]
    # maxY = maxList [maxY, yoff + imSp.yspr]
    = {xspr = maxX, yspr = maxY}

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
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \ts n   st -> ret 10.0 st // abort "ColumnXSpanAlg " // ret 10.0 st
  , lookupSpanDescentYSpanAlg = \fd     st -> ret 10.0 st // abort "DescentYSpanAlg" // ret 10.0 st // TODO Will we even use this?
  , lookupSpanExYSpanAlg      = \fd     st -> ret 10.0 st // abort "ExYSpanAlg     " // ret 10.0 st // TODO Shouldn't we simply use em instead?
  , lookupSpanImageXSpanAlg   = \ts     st -> ret 10.0 st // abort "ImageXSpanAlg  " // ret 10.0 st
  , lookupSpanImageYSpanAlg   = \ts     st -> ret 10.0 st // abort "ImageYSpanAlg  " // ret 10.0 st
  , lookupSpanRowYSpanAlg     = \ts n   st -> ret 10.0 st // abort "RowYSpanAlg    " // ret 10.0 st
  , lookupSpanTextXSpanAlg    = getTextLength
  }

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

reduceSpanList :: (Real Real -> Real) ([Span] -> Span) [.st -> .(Span, .st)] -> (.st -> .(Span, .st))
reduceSpanList op cons ss
  =      sequence ss `b`
  \ss -> ret (case reduceSpans ss of
               [PxSpan x] -> PxSpan x
               xs         -> cons xs)
  where
  reduceSpans [PxSpan x : PxSpan y : xs] = reduceSpans [PxSpan (op x y) : xs]
  reduceSpans [PxSpan x : y : xs]        = [y : reduceSpans [PxSpan x : xs]]
  reduceSpans [x : xs]                   = [x : reduceSpans xs]
  reduceSpans []                         = []


:: Algebras m imCo imAt imTr im baIm imSp coIm imAn imOf ho co sp loSp =
  { imageAlgs          :: ImageAlg imCo imAt imTr im
  , imageContentAlgs   :: ImageContentAlg baIm imSp coIm imCo
  , imageAttrAlgs      :: ImageAttrAlg m imAt
  , imageTransformAlgs :: ImageTransformAlg imAn sp imTr
  , imageSpanAlgs      :: ImageSpanAlg sp imSp
  , basicImageAlgs     :: BasicImageAlg baIm
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

:: BasicImageAlg baIm =
  { basicImageEmptyImageAlg   ::                   baIm
  , basicImageTextImageAlg    :: FontDef String -> baIm
  , basicImageLineImageAlg    :: Slash          -> baIm
  , basicImageCircleImageAlg  ::                   baIm
  , basicImageRectImageAlg    ::                   baIm
  , basicImageEllipseImageAlg ::                   baIm
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

imageCata allAlgs { Image | content, attribs, transform, tags }
  # synContent    = imageContentCata allAlgs content
  # synsAttribs   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) attribs
  # synsTransform = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs) transform
  = allAlgs.imageAlgs.imageAlg synContent synsAttribs synsTransform tags

imageContentCata allAlgs (Basic bi is)
  # synBasicImage = basicImageCata allAlgs.basicImageAlgs bi
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

basicImageCata basicImageAlgs EmptyImage         = basicImageAlgs.basicImageEmptyImageAlg
basicImageCata basicImageAlgs (TextImage fd str) = basicImageAlgs.basicImageTextImageAlg fd str
basicImageCata basicImageAlgs (LineImage sl)     = basicImageAlgs.basicImageLineImageAlg sl
basicImageCata basicImageAlgs CircleImage        = basicImageAlgs.basicImageCircleImageAlg
basicImageCata basicImageAlgs RectImage          = basicImageAlgs.basicImageRectImageAlg
basicImageCata basicImageAlgs EllipseImage       = basicImageAlgs.basicImageEllipseImageAlg

imageSpanCata imageSpanAlgs spanAlgs lookupSpanAlgs { ImageSpan | xspan, yspan }
  # synSpan1 = spanCata spanAlgs lookupSpanAlgs xspan
  # synSpan2 = spanCata spanAlgs lookupSpanAlgs yspan
  = imageSpanAlgs.imageSpanAlg synSpan1 synSpan2

compositeImageCata allAlgs { CompositeImage | offsets, host, compose }
  # synsImageOffset = let f (l, r) xs
                            # synr = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs l
                            # synl = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs r
                            = [(synr, synl):xs]
                      in  foldr f [] offsets
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
lookupCata lookupSpanAlgs (DescentYSpan fd)
  = lookupSpanAlgs.lookupSpanDescentYSpanAlg fd
lookupCata lookupSpanAlgs (ExYSpan fd)
  = lookupSpanAlgs.lookupSpanExYSpanAlg fd
lookupCata lookupSpanAlgs (ImageXSpan imts)
  = lookupSpanAlgs.lookupSpanImageXSpanAlg imts
lookupCata lookupSpanAlgs (ImageYSpan imts)
  = lookupSpanAlgs.lookupSpanImageYSpanAlg imts
lookupCata lookupSpanAlgs (RowYSpan imts n)
  = lookupSpanAlgs.lookupSpanRowYSpanAlg imts n
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
