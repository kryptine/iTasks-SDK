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

:: ClientState m =
  { didInit         :: Bool
  , didDraw         :: Bool
  , textXSpanEnv    :: Map (FontDef, String) Span
  , textYSpanEnv    :: Map FontDef (Span, Span)
  , uniqueIdCounter :: Int
  , image           :: Maybe (Image m)
  , editletId       :: String
  }

:: CachedSpan
  = CachedGridSpan [[ImageSpan]]
  | CachedImageSpan ImageSpan

derive class iTask ClientState

mainSvgId :: ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

svglet :: (Image m) -> Editlet (Image m) [SVGletDiff m] | iTask m
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
    # ((img, imSp), st)     = toSVGImage img ({clval & editletId = cid}, world)
    # (imh, st)             = evalSpan imSp.yspan st
    # (imw, (clval, world)) = evalSpan imSp.xspan st
    # (svg, world)          = getDomElement (mainSvgId cid) world
    # (_, world)            = (svg `setAttribute` ("height", imh)) world
    # (_, world)            = (svg `setAttribute` ("width", imw)) world
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

getTextLength :: FontDef String *(St m) -> *(Span, *(St m)) | iTask m
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
        # twidth         = px (jsValToReal ctl)
        = (twidth, ({clval & textXSpanEnv = 'DM'.gPut (fontdef, str) twidth clval.textXSpanEnv}, world))

:: *St m :== *(ClientState m, *JSWorld)

:: *ClSt m a :== *(St m) -> *(a, *(St m))

:: ServerState =
  { srvTaggedSpanEnv :: Map (Set ImageTag) CachedSpan
  }

:: SrvSt a :== State ServerState a // ServerState -> (a, ServerState)

:: State s a :== s -> (a, s)

:: ErrorMessage :== String

runM :: (u:a -> v:(b,u:a)) u:a -> w:(b,u:a), [w <= u,v <= w]
runM m st = m st

sequence :: [.st -> .(a, .st)] -> (.st -> .([a], .st))
sequence ms = \st -> mapSt id ms st

// TODO : Detect divergence due to lookups and return an Either ErrorMessage (Image m), instead of just an Image m
fixSpans :: (Image m) -> SrvSt (Image m, ImageSpan) | iTask m
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
  fixSpansImageAlgs :: ImageAlg ([ImageAttr m] [ImageTransform] (Set ImageTag) -> (SrvSt (ImageContent m, ImageSpan)))
                                (SrvSt (ImageAttr m))
                                (SrvSt ImageTransform)
                                (SrvSt (Image m, ImageSpan))
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: ([ImageAttr m] [ImageTransform] (Set ImageTag) -> (SrvSt (ImageContent m, ImageSpan)))
               [SrvSt (ImageAttr m)]
               [SrvSt ImageTransform]
               (Set ImageTag) -> SrvSt (Image m, ImageSpan)
    mkImage imCo imAts imTrs imTas = go
      where
      go st
        # (imAts, st) = sequence imAts st
        # (imTrs, st) = sequence imTrs st
        # ((content, sp), st)  = imCo imAts imTrs imTas st
        # st = { st & srvTaggedSpanEnv = 'DM'.put imTas (CachedImageSpan sp) st.srvTaggedSpanEnv }
        = ret ({ Image | content = content, attribs = imAts, transform = imTrs, tags = imTas}, sp) st // TODO transforms can influence size as well...
  fixSpansImageContentAlgs :: ImageContentAlg (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
                                              (SrvSt ImageSpan)
                                              ([ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
                                              ([ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
  fixSpansImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
               (SrvSt ImageSpan)
               [ImageAttr m] [ImageTransform] (Set ImageTag) ->
               SrvSt (ImageContent m, ImageSpan)
    mkBasic baIm imSp imAts imTrs imTas = imSp `b` \imSp -> baIm imSp imAts imTrs imTas
  fixSpansImageAttrAlgs :: ImageAttrAlg m (SrvSt (ImageAttr m))
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
  fixSpansBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg   = \       imSp _ imTrs imTas -> mkSpan EmptyImage         imSp imTrs imTas
    , basicImageTextImageAlg    = \fd str imSp _ imTrs imTas -> mkSpan (TextImage fd str) imSp imTrs imTas
    , basicImageLineImageAlg    = \sl     imSp _ imTrs imTas -> mkSpan (LineImage sl)     imSp imTrs imTas
    , basicImageCircleImageAlg  = \       imSp _ imTrs imTas -> mkSpan (CircleImage)      imSp imTrs imTas
    , basicImageRectImageAlg    = \       imSp _ imTrs imTas -> mkSpan (RectImage)        imSp imTrs imTas
    , basicImageEllipseImageAlg = \       imSp _ imTrs imTas -> mkSpan (EllipseImage)     imSp imTrs imTas
    }
    where
    mkSpan :: BasicImage ImageSpan [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan)
    mkSpan val imSp imTrs imTas = ret (Basic val imSp, applyTransforms imTrs imSp)
  fixSpansCompositeImageAlgs :: CompositeImageAlg (SrvSt Span)
                                                  (SrvSt (Image m, ImageSpan))
                                                  ([ImageOffset] (Maybe (Image m, ImageSpan)) -> SrvSt (Compose m, ImageSpan))
                                                  ([ImageAttr m] [ImageTransform] (Set ImageTag) -> SrvSt (ImageContent m, ImageSpan))
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
  fixSpansComposeAlgs :: ComposeAlg (SrvSt (Image m, ImageSpan)) ([ImageOffset] (Maybe (Image m, ImageSpan)) -> SrvSt (Compose m, ImageSpan))
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[SrvSt (Image m, ImageSpan)]] [ImageOffset] (Maybe (Image m, ImageSpan)) -> SrvSt (Compose m, ImageSpan)
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
      f :: [SrvSt (Image m, ImageSpan)] *([[(Image m, ImageSpan)]], ServerState) -> *([[(Image m, ImageSpan)]], ServerState)
      f imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    mkCollage :: [SrvSt (Image m, ImageSpan)] [ImageOffset] (Maybe (Image m, ImageSpan)) -> SrvSt (Compose m, ImageSpan)
    mkCollage imgs offsets mbhost = sequence imgs `b`
                        \imgsSps -> ret ( AsCollage (map fst imgsSps)
                                        , maybe (calculateComposedSpan (map snd imgsSps) offsets) snd mbhost)
    mkOverlay :: [ImageAlign] [SrvSt (Image m, ImageSpan)] [ImageOffset] (Maybe (Image m, ImageSpan)) -> SrvSt (Compose m, ImageSpan)
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

:: ToSVGImageSyn :== (SVGElt, ImageSpan)

mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

//toSVGImage :: (Image a) -> ClSt ToSVGImageSyn | iTask a
toSVGImage img = \st -> imageCata allAlgs img st
  where
  allAlgs =
    { imageAlgs          = imageAlgs
    , imageContentAlgs   = imageContentAlgs
    , imageAttrAlgs      = imageAttrAlgs
    , imageTransformAlgs = imageTransformAlgs
    , imageSpanAlgs      = imageSpanAlgs
    , basicImageAlgs     = basicImageAlgs
    , compositeImageAlgs = compositeImageAlgs
    , composeAlgs        = composeAlgs
    , spanAlgs           = reduceSpanSpanAlgs
    , lookupSpanAlgs     = reduceSpanLookupSpanAlgs
    }
  //imageAlgs :: ImageAlg ([SVGAttr] [SVGTransform] (Set ImageTag) -> (ClSt ToSVGImageSyn))
                        //(ClSt SVGAttr)
                        //(ClSt SVGTransform)
                        //(ClSt ToSVGImageSyn)
  imageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage imCo imAts imTrs imTas = go
      where
      go st
        # (imAts, st) = sequence imAts st
        # (imTrs, st) = sequence imTrs st
        # ((img, sp), (clval, world))  = imCo imAts imTrs imTas st
        = ret (img, sp) (clval, world) // TODO transforms can influence size as well...
      //# (imCo, st)  = imCo st
      //# (imAts, st) = mapSt id imAts st
      //# (imTrs, st) = mapSt id imTrs st
      //= ret (imCo (mkAttrs imAts imTrs)) st

  //imageContentAlgs :: ImageContentAlg (ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
                                      //(ClSt ImageSpan)
                                      //([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
                                      //([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
  imageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    //mkBasic :: (ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
               //(ClSt ImageSpan)
               //[SVGAttr] [SVGTransform] (Set ImageTag) ->
               //ClSt ToSVGImageSyn
    mkBasic baIm imSp imAts imTrs imTas = imSp `b` \imSp -> baIm imSp imAts imTrs imTas
  //imageAttrAlgs :: ImageAttrAlg m (ClSt SVGAttr)
  imageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr -> ret (StrokeAttr (PaintColor attr.stroke Nothing))
    , imageAttrStrokeWidthAttrAlg   = \attr -> mkStrokeWidth attr
    , imageAttrStrokeOpacityAttrAlg = \attr -> ret (StrokeOpacityAttr (toString attr.opacity))
    , imageAttrFillAttrAlg          = \attr -> ret (FillAttr (PaintColor attr.fill Nothing))
    , imageAttrFillOpacityAttrAlg   = \attr -> ret (FillOpacityAttr (FillOpacity (toString attr.opacity)))
    , imageAttrOnClickAttrAlg       = \attr -> abort "imageAttrOnClickAttrAlg" // (("onclick", "TODO How?"), st) // TODO
    }
    where
    //mkStrokeWidth :: (StrokeWidthAttr m) -> ClSt SVGAttr
    mkStrokeWidth attr = evalSpan attr.strokewidth `b` \sp -> ret (StrokeWidthAttr (StrokeWidthLength (toString sp, PX)))
  //imageTransformAlgs :: ImageTransformAlg Real (ClSt Span) (ClSt SVGTransform)
  imageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    -> ret (RotateTransform (toString imAn) Nothing)
    , imageTransformSkewXImageAlg  = \imAn    -> ret (SkewXTransform (toString imAn))
    , imageTransformSkewYImageAlg  = \imAn    -> ret (SkewYTransform (toString imAn))
    , imageTransformFitImageAlg    = \sp1 sp2 -> ret (ScaleTransform "scale" "1.0") // TODO
    , imageTransformFitXImageAlg   = \sp      -> ret (ScaleTransform "scale" "1.0") // TODO
    , imageTransformFitYImageAlg   = \sp      -> ret (ScaleTransform "scale" "1.0") // TODO
    }
  //imageSpanAlgs :: ImageSpanAlg (ClSt Span) (ClSt ImageSpan)
  imageSpanAlgs =
    { imageSpanAlg = mkImageSpan
    }
    where
    mkImageSpan sp1 sp2 = sp1 `b` \sp1 -> sp2 `b` \sp2 -> ret { ImageSpan | xspan = sp1, yspan = sp2}
  //basicImageAlgs :: BasicImageAlg (ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
  basicImageAlgs =
    { basicImageEmptyImageAlg   = mkEmptyImage
    , basicImageTextImageAlg    = mkTextImage
    , basicImageLineImageAlg    = mkLineImage
    , basicImageCircleImageAlg  = mkCircleImage
    , basicImageRectImageAlg    = mkRectImage
    , basicImageEllipseImageAlg = mkEllipseImage
    }
    where
    //mkEmptyImage :: ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkEmptyImage imSp imAts imTrs imTas = mkWH imSp `b` \wh -> ret (GElt wh (mkAttrs imAts imTrs) [], imSp)
    //mkTextImage :: FontDef String ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkTextImage fd str imSp imAts imTrs imTas = ret (TextElt [] (mkAttrs imAts imTrs) str, imSp)
    //mkLineImage :: Slash ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkLineImage sl imSp imAts imTrs imTas
      = evalSpan imSp.xspan `b` \xsp -> evalSpan imSp.yspan `b` \ysp -> ret (LineElt [] (mkAttrs imAts imTrs ++ mkLineAttrs sl (xsp, ysp)), imSp)
      where
      mkLineAttrs slash (xspan, yspan)
        # (y1, y2) = case slash of
                       Slash     -> (toString yspan, "0.0")
                       Backslash -> ("0.0", toString yspan)
        = [ X1Attr ("0.0", PX), X2Attr (toString xspan, PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    //mkRectImage :: ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkRectImage imSp imAts imTrs imTas = mkWH imSp `b` \wh -> ret (RectElt wh (mkAttrs imAts imTrs) , imSp)
    //mkCircleImage :: ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkCircleImage imSp imAts imTrs imTas
      = evalSpan imSp.xspan `b` \xsp -> let r = toString (xsp / 2.0)
                                        in  ret (CircleElt [] [ RAttr (r, PX), CxAttr (r, PX)
                                                , CyAttr (r, PX) : (mkAttrs imAts imTrs) ], imSp) // TODO Cx and Cy depend on positioning
    //mkEllipseImage :: ImageSpan [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkEllipseImage imSp imAts imTrs imTas
      = evalSpan imSp.xspan `b` \xsp -> evalSpan imSp.yspan `b` \ysp ->
        ret (EllipseElt [] (mkAttrs imAts imTrs ++ [ RxAttr (toString (xsp / 2.0), PX), RyAttr (toString (ysp / 2.0), PX)
                                                   , CxAttr (toString (xsp / 2.0), PX), CyAttr (toString (ysp / 2.0), PX)]), imSp)
    //mkWH :: ImageSpan -> ClSt [HtmlAttr]
    mkWH imSp = evalSpan imSp.xspan `b` \xsp -> evalSpan imSp.yspan `b` \ysp -> ret [WidthAttr (toString xsp), HeightAttr (toString ysp)]
  //compositeImageAlgs :: CompositeImageAlg (ClSt Span)
                                          //(ClSt ToSVGImageSyn)
                                          //([ImageOffset] (Maybe ToSVGImageSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
                                          //([SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn)
  compositeImageAlgs =
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
      //# sp                   = maybe (calculateComposedSpan imgs offs) snd mbhost
      //# alignOffs            = zipWith (+) (zipWith (calcOffset maxXSpan maxYSpan) imgs (aligns ++ repeat (AtLeft, AtTop)))
                                           //(offs ++ repeat (px 0.0, px 0.0))
      //# (imgs, st)           = zipWithSt mkTranslateGroup (alignOffs ++ [(px 0.0, px 0.0)]) (maybe imgs (\h -> imgs ++ [h]) mbhost) st
      //= ((imgs, sp), st)
    //addComposition mbhost (AsCollage imgs) offs st
      //# sp         = maybe (calculateComposedSpan imgs offs) snd mbhost
      //# (imgs, st) = zipWithSt mkTranslateGroup offs imgs st
      //= ((imgs, sp), st)
    //calculateComposedSpan imgs offs
      //= foldr f {xspan = px 0.0, yspan = px 0.0} (zip2 (offs ++ repeat (px 0.0, px 0.0)) imgs)
      //where
      //f ((xoff, yoff), (_, imSp)) {xspan = maxX, yspan = maxY}
        //# maxX = maxSpan [maxX, xoff + imSp.xspan]
        //# maxY = maxSpan [maxY, yoff + imSp.yspan]
        //= {xspan = maxX, yspan = maxY}
  //composeAlgs :: ComposeAlg (ClSt ToSVGImageSyn)
                            //(ClSt (Compose m))
  composeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    //mkGrid :: (Int, Int) [ImageAlign] [[ClSt ToSVGImageSyn]] [ImageOffset] (Maybe ToSVGImageSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkGrid dims aligns imgss offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgss, st) = foldr f ([], st) imgss
        # spanss = map (map snd) imgss
        # yspans = map (maxSpan o map (\x -> x.yspan)) spanss
        # xspans = map (maxSpan o map (\x -> x.xspan)) (transpose spanss)
        //// TODO Add offsets and host
        # (xsp, ysp) = maybe (foldr (+) (px 0.0) xspans, foldr (+) (px 0.0) yspans) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
        = ret ( GElt [] (mkAttrs imAts imTrs) [] // TODO
              , { xspan = xsp, yspan = ysp }) st
      //f :: [ClSt m ToSVGImageSyn] *([[ToSVGImageSyn]], *(St m)) -> *([[ToSVGImageSyn]], *(St m)) | iTask m
      f imgs (acc, st) = (sequence imgs `b` \imgs -> ret [imgs:acc]) st
    //mkCollage :: [ClSt ToSVGImageSyn] [ImageOffset] (Maybe ToSVGImageSyn) [SVGAttr] [SVGTransform] (Set ImageTag) -> ClSt ToSVGImageSyn
    mkCollage imgs offsets mbhost imAts imTrs imTas
      =             sequence imgs `b`
        \imgsSps -> zipWithSt mkTranslateGroup offsets imgsSps `b`
        \trimgs  -> ret ( GElt [] (mkAttrs imAts imTrs) (map fst trimgs)
                        , maybe (calculateComposedSpan (map snd imgsSps) offsets) snd mbhost)
    //mkOverlay :: [ImageAlign] [*(u:(ClientState a),*JSWorld) -> *(ToSVGImageSyn,*(v:(ClientState a),*JSWorld))] [(Span,Span)] (Maybe (SVGElt,ImageSpan)) [SVGAttr] [SVGTransform] b -> *(w:(ClientState a),*JSWorld) -> *(.(SVGElt,ImageSpan),*(ClientState a,*JSWorld)) | iTask a, [w v <= u]
    mkOverlay aligns imgs offsets mbhost imAts imTrs imTas = go
      where
      go st
        # (imgsSps, st) = sequence imgs st
        # images        = map fst imgsSps
        # spans         = map snd imgsSps
        # maxXSpan      = maxSpan (map (\x -> x.xspan) spans)
        # maxYSpan      = maxSpan (map (\x -> x.yspan) spans)
        # (maxXSpan, maxYSpan) = maybe (maxXSpan, maxYSpan) (\(_, sp) -> (sp.xspan, sp.yspan)) mbhost
        # alignOffs            = zipWith (+) (zipWith (calcOffset maxXSpan maxYSpan) spans (aligns ++ repeat (AtLeft, AtTop)))
                                             (offsets ++ repeat (px 0.0, px 0.0))
        # (imgs, st)           = zipWithSt mkTranslateGroup (alignOffs ++ [(px 0.0, px 0.0)]) (maybe imgsSps (\h -> imgsSps ++ [h]) mbhost) st
        = ret ( GElt [] (mkAttrs imAts imTrs) (map fst imgs)
              , maybe (calculateComposedSpan spans offsets) snd mbhost) st

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

evalList xs st :==
  let f x (xs, st)
        # (x, st) = x st
        = ([x:xs], st)
  in  foldr f ([], st) xs

//evalOffsets :: [(.st -> .(Span, .st), .st -> .(Span, .st))] .st -> .([(Span, Span)], .st)
evalOffsets offsets st = foldr f ([], st) offsets
  where
  f (sp1, sp2) (xs, st)
    # (sp1, st) = sp1 st
    # (sp2, st) = sp2 st
    = ([(sp1, sp2):xs], st)

//evalHost :: (Maybe (u:(Maybe a) -> v:(a,u:(Maybe a)))) -> w:(Maybe a) -> x:(Maybe a,u:(Maybe a)), [x w v <= u]
evalHost (Just x) = x `b` \x -> ret (Just x)
evalHost _        = ret Nothing

ret x :== \st -> (x, st)

(`b`) f g :== \st0 -> let (r,st1) = f st0 in g r st1
(`b2`) f g :== \st0 -> let (r,st1) = f st0 in (let (_, st2) = g r st1 in st2)

mkAttrs :: [SVGAttr] [SVGTransform] -> [SVGAttr]
mkAttrs imAts [] = imAts
mkAttrs imAts xs = [TransformAttr xs:imAts]


calcOffset :: Span Span ImageSpan ImageAlign -> ImageOffset
calcOffset mxsp mysp imSp (xal, yal) = (mkXAl xal, mkYAl yal)
  where
  mkXAl AtLeft    = px 0.0
  mkXAl AtMiddleX = (mxsp /. 2.0) - (imSp.xspan /. 2.0)
  mkXAl AtRight   = mxsp - imSp.xspan
  mkYAl AtTop     = px 0.0
  mkYAl AtMiddleY = (mysp /. 2.0) - (imSp.yspan /. 2.0)
  mkYAl AtBottom  = mysp - imSp.yspan

calculateComposedSpan :: [ImageSpan] [ImageOffset] -> ImageSpan
calculateComposedSpan spans offs
  = foldr f {xspan = px 0.0, yspan = px 0.0} (zip2 (offs ++ repeat (px 0.0, px 0.0)) spans)
  where
  f ((xoff, yoff), imSp) {xspan = maxX, yspan = maxY}
    # maxX = maxSpan [maxX, xoff + imSp.xspan]
    # maxY = maxSpan [maxY, yoff + imSp.yspan]
    = {xspan = maxX, yspan = maxY}

//mkTranslateGroup :: ImageOffset ToSVGImageSyn -> ClSt ToSVGImageSyn
mkTranslateGroup (xoff, yoff) (contents, imSp)
  = evalSpan xoff `b` \xsp -> evalSpan yoff `b` \ysp -> ret (GElt [] (mkTranslateAttr (xsp, ysp)) [contents], imSp)
  where
  mkTranslateAttr :: (Real, Real) -> [SVGAttr]
  mkTranslateAttr (0.0,   0.0)   = []
  mkTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

undef = undef

evalSpan :: Span *(St m) -> *(Real, *(St m)) | iTask m
evalSpan sp st = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs sp st

evalSpanSpanAlgs =
  { spanPxSpanAlg     = \r   -> ret r
  , spanLookupSpanAlg = id // TODO
  , spanAddSpanAlg    = \x y -> mkBin (+) x y
  , spanSubSpanAlg    = \x y -> mkBin (-) x y
  , spanMulSpanAlg    = \x y -> mkBin` (*) x y
  , spanDivSpanAlg    = \x y -> mkBin` (/) x y
  , spanAbsSpanAlg    = \x   -> mkAbs x
  , spanMinSpanAlg    = \xs  -> mkList minList xs
  , spanMaxSpanAlg    = \xs  -> mkList maxList xs
  }
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \ts n   st -> ret 100.0 st
  , lookupSpanDescentYSpanAlg = \fd     st -> ret 100.0 st // TODO Will we even use this?
  , lookupSpanExYSpanAlg      = \fd     st -> ret 100.0 st // TODO Shouldn't we simply use em instead?
  , lookupSpanImageXSpanAlg   = \ts     st -> ret 100.0 st
  , lookupSpanImageYSpanAlg   = \ts     st -> ret 100.0 st
  , lookupSpanRowYSpanAlg     = \ts n   st -> ret 100.0 st
  , lookupSpanTextXSpanAlg    = mkTextXSpan
  }
  where
  mkTextXSpan fd str st
    # (PxSpan n, st) = getTextLength fd str st
    = (n, st)

reduceSpanSpanAlgs =
  { spanPxSpanAlg     = \r   -> ret (PxSpan r)
  , spanLookupSpanAlg = \lu  -> reduceLU lu
  , spanAddSpanAlg    = \x y -> reduceSpanBin (+) AddSpan x y
  , spanSubSpanAlg    = \x y -> reduceSpanBin (-) SubSpan x y
  , spanMulSpanAlg    = \x y -> reduceSpanNum (*) MulSpan x y
  , spanDivSpanAlg    = \x y -> reduceSpanNum (/) DivSpan x y
  , spanAbsSpanAlg    = \x   -> mkAbs x
  , spanMinSpanAlg    = \xs  -> reduceSpanList min MinSpan xs
  , spanMaxSpanAlg    = \xs  -> reduceSpanList max MaxSpan xs
  }
  where
  reduceLU x st = x st

reduceSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \ts n   -> ret (PxSpan 100.0)
  , lookupSpanDescentYSpanAlg = \fd     -> ret (PxSpan 100.0) // TODO Will we even use this?
  , lookupSpanExYSpanAlg      = \fd     -> ret (PxSpan 100.0) // TODO Shouldn't we simply use em instead?
  , lookupSpanImageXSpanAlg   = \ts     -> ret (PxSpan 100.0)
  , lookupSpanImageYSpanAlg   = \ts     -> ret (PxSpan 100.0)
  , lookupSpanRowYSpanAlg     = \ts n   -> ret (PxSpan 100.0)
  , lookupSpanTextXSpanAlg    = \fd str -> getTextLength fd str
  }

mkAbs x st
  # (x, st) = x st
  = (abs x, st)

mkBin op x y st
  # (x, st) = x st
  # (y, st) = y st
  = (op x y, st)

mkBin` op x y st
  # (x, st) = x st
  = (op x y, st)

mkList f xs st
  # (xs, st) = mapSt id xs st
  = (f xs, st)

reduceSpanBin :: (Real Real -> Real) (Span Span -> Span) (.st -> .(Span, .st)) (.st -> .(Span, .st)) .st -> .(Span, .st)
reduceSpanBin op cons x y st
  # (x, st) = x st
  # (y, st) = y st
  = (case (x, y) of
       (PxSpan x`, PxSpan y`) -> PxSpan (op x` y`)
       (x`, y`)               -> cons x` y`, st)

reduceSpanNum :: (Real Real -> Real) (Span Real -> Span) (.st -> .(Span, .st)) Real .st -> .(Span, .st)
reduceSpanNum op cons x y st
  # (x, st) = x st
  = (case x of
       PxSpan x` -> PxSpan (op x` y)
       x`        -> cons x` y, st)

reduceSpanList :: (Real Real -> Real) ([Span] -> Span) [.st -> .(Span, .st)] .st -> .(Span, .st)
reduceSpanList op cons ss st
  # (ss, st) = sequence ss st
  = (case reduceSpans ss of
       [PxSpan x] -> PxSpan x
       xs         -> cons xs, st)
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
