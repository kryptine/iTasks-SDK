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
  YAlign, Set, CachedSpan

derive gLexOrd FontDef, Span, LookupSpan, ImageTag, Set, CachedSpan, ImageSpan

:: ClientState =
  { didInit       :: Bool
  , didDraw       :: Bool
  , textXSpanEnv  :: Map (FontDef, String) Span
  , textYSpanEnv  :: Map FontDef (Span, Span)
  , taggedSpanEnv :: Map (Set ImageTag) CachedSpan
  }

:: CachedSpan
  = CachedGridSpan [[ImageSpan]]
  | CachedImageSpan ImageSpan

derive class iTask ClientState

mainSvgId :: ComponentId -> ComponentId
mainSvgId cid = cid +++ "-svg"

svglet :: (Image m) -> Editlet (Image m) Int | iTask m
svglet img = Editlet img server client
where
  server
    = { EditletServerDef
      | genUI    = genUI
      , defVal   = empty (px 0.0) (px 0.0)
      , genDiff  = \x y -> Just 0
      , appDiff  = \_ x -> x
      }
  client
    = { EditletClientDef
      | updateUI = updateUI
      , defVal   = { didInit       = False
                   , didDraw       = False
                   , textXSpanEnv  = 'DM'.newMap
                   , textYSpanEnv  = 'DM'.newMap
                   , taggedSpanEnv = 'DM'.newMap
                   }
      , genDiff  =  \x y -> if (x === y) Nothing (Just 0)
      , appDiff  =  \_ x -> x
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

  updateUI cid diffs clval=:{didInit = False} world
    = ({clval & didInit = True}, world)

  updateUI cid diffs clval=:{didInit = True} world
    # (svgImgFn, st)        = undef // toSVGImage img (clval, world)
    # ((img, imsp), st)     = svgImgFn st
    # (imh, st)             = evalSpan imsp.yspan st
    # (imw, (clval, world)) = evalSpan imsp.xspan st
    # (svg, world)          = getDomElement (mainSvgId cid) world
    # (_, world)            = (svg `setAttribute` ("height", imh)) world
    # (_, world)            = (svg `setAttribute` ("width", imw)) world
    # (elem, world)         = appendSVG img svg world
    = (clval, world)


(`setAttribute`)          obj args :== obj .# "setAttribute"          .$ args
(`createElementNS`)       obj args :== obj .# "createElementNS"       .$ args
(`appendChild`)           obj args :== obj .# "appendChild"           .$ args
(`removeChild`)           obj args :== obj .# "removeChild"           .$ args
(`getBBox`)               obj args :== obj .# "getBBox"               .$ args
(`getComputedTextLength`) obj args :== obj .# "getComputedTextLength" .$ args

getTextLength :: FontDef String *St -> *(Span, *St)
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

:: *St :== *(ClientState, *JSWorld)

:: ErrorMessage :== String

fixSpans :: (Image m) *St -> *(Image m, *St) | iTask m
fixSpans img st
  # (img`, st) = imageCata fixSpansAllAlgs img st
  | img === img` = (img, st)
  | otherwise    = fixSpans img st
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
  fixSpansImageAlgs :: ImageAlg ([ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
                                (*St -> *(ImageAttr m, *St))
                                (*St -> *(ImageTransform, *St))
                                (*St -> *(Image m, *St))
  fixSpansImageAlgs =
    { imageAlg = mkImage
    }
    where
    mkImage :: ([ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
               [*St -> *(ImageAttr m, *St)]
               [*St -> *(ImageTransform, *St)]
               (Set ImageTag) *St -> *(Image m, *St)
    mkImage imCo imAts imTrs imTas st
      # (imAts, st) = mapSt id imAts st
      # (imTrs, st) = mapSt id imTrs st
      # (imCo, st)  = imCo imAts imTrs imTas st
      = ret { Image | content = imCo, attribs = imAts, transform = imTrs, tags = imTas} st // TODO transforms can influence size as well...
  fixSpansImageContentAlgs :: ImageContentAlg (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
                                              (*St -> *(ImageSpan, *St))
                                              ([ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
                                              ([ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
  fixSpansImageContentAlgs =
    { imageContentBasicAlg     = mkBasic
    , imageContentCompositeAlg = id
    }
    where
    mkBasic :: (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
               (*St -> (ImageSpan, *St))
               [ImageAttr m] [ImageTransform] (Set ImageTag)
               *St -> *(ImageContent m, *St)
    mkBasic baIm imSp imAts imTrs imTas st
      # (imSp, st) = imSp st
      = baIm imSp imAts imTrs imTas st
  fixSpansImageAttrAlgs :: ImageAttrAlg m (*St -> (ImageAttr m, *St))
  fixSpansImageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \x -> ret (ImageStrokeAttr x)
    , imageAttrStrokeWidthAttrAlg   = \x -> ret (ImageStrokeWidthAttr x)
    , imageAttrStrokeOpacityAttrAlg = \x -> ret (ImageStrokeOpacityAttr x)
    , imageAttrFillAttrAlg          = \x -> ret (ImageFillAttr x)
    , imageAttrFillOpacityAttrAlg   = \x -> ret (ImageFillOpacityAttr x)
    , imageAttrOnClickAttrAlg       = \x -> ret (ImageOnClickAttr x)
    }
  fixSpansImageTransformAlgs :: ImageTransformAlg Real (*St -> *(Span, *St)) (*St -> *(ImageTransform, *St))
  fixSpansImageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn -> ret (RotateImage imAn)
    , imageTransformSkewXImageAlg  = \imAn -> ret (SkewXImage  imAn)
    , imageTransformSkewYImageAlg  = \imAn -> ret (SkewYImage  imAn)
    , imageTransformFitImageAlg    = mkFitImage
    , imageTransformFitXImageAlg   = mkFitDim FitXImage
    , imageTransformFitYImageAlg   = mkFitDim FitXImage
    }
    where
    mkFitImage :: (*St -> *(Span,*St)) (*St -> *(Span,*St)) *St -> *(ImageTransform,*St)
    mkFitImage sp1 sp2 st
      # (sp1, st) = sp1 st
      # (sp2, st) = sp2 st
      = ret (FitImage sp1 sp2) st
    mkFitDim :: (Span -> ImageTransform) (*St -> *(Span, *St)) *St -> *(ImageTransform, *St)
    mkFitDim ctr sp st
      # (sp, st) = sp st
      = ret (ctr sp) st
  fixSpansImageSpanAlgs :: ImageSpanAlg (*St -> *(Span, *St)) (*St -> *(ImageSpan, *St))
  fixSpansImageSpanAlgs =
    { imageSpanAlg = mkSpan
    }
    where
    mkSpan :: (*St -> *(Span, *St)) (*St -> *(Span, *St)) *St -> *(ImageSpan, *St)
    mkSpan xspan yspan st
      # (xspan, st) = xspan st
      # (yspan, st) = yspan st
      = ret { xspan = xspan, yspan = yspan } st
  fixSpansBasicImageAlgs :: BasicImageAlg (ImageSpan [ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
  fixSpansBasicImageAlgs =
    { basicImageEmptyImageAlg   = \       imSp _ _ imTas -> mkSpan EmptyImage         imSp imTas
    , basicImageTextImageAlg    = \fd str imSp _ _ imTas -> mkSpan (TextImage fd str) imSp imTas
    , basicImageLineImageAlg    = \sl     imSp _ _ imTas -> mkSpan (LineImage sl)     imSp imTas
    , basicImageCircleImageAlg  = \       imSp _ _ imTas -> mkSpan (CircleImage)      imSp imTas
    , basicImageRectImageAlg    = \       imSp _ _ imTas -> mkSpan (RectImage)        imSp imTas
    , basicImageEllipseImageAlg = \       imSp _ _ imTas -> mkSpan (EllipseImage)     imSp imTas
    }
    where
    mkSpan :: BasicImage ImageSpan (Set ImageTag) *St -> *(ImageContent m, *St)
    mkSpan val imSp imTas (clval, world)
      # newEnv = 'DM'.put imTas (CachedImageSpan imSp) clval.taggedSpanEnv
      = ret (Basic val imSp) ({clval & taggedSpanEnv = newEnv}, world)
  fixSpansCompositeImageAlgs :: CompositeImageAlg (*St -> *(Span, *St))
                                                  (*St -> *(Image m, *St))
                                                  (*St -> *(Compose m, *St))
                                                  ([ImageAttr m] [ImageTransform] (Set ImageTag) *St -> *(ImageContent m, *St))
  fixSpansCompositeImageAlgs =
    { compositeImageAlg = mkCompositeImage
    }
    where
    mkCompositeImage offsets host compose imAts imTrs imTas st
      # (offsets, st)  = let f (sp1, sp2) (xs, st)
                               # (sp1, st) = sp1 st
                               # (sp2, st) = sp2 st
                               = ([(sp1, sp2):xs], st)
                         in foldr f ([], st) offsets
      # (host, st) = case host of
                       Just x
                         # (x, st) = x st
                         = (Just x, st)
                       _ = (Nothing, st)
      # (compose, st) = compose st
      = ret (Composite {CompositeImage | offsets = offsets, host = host, compose = compose}) st
  fixSpansComposeAlgs :: ComposeAlg (*St -> *(Image m, *St)) (*St -> *(Compose m, *St))
  fixSpansComposeAlgs =
    { composeAsGridAlg    = mkGrid
    , composeAsCollageAlg = mkCollage
    , composeAsOverlayAlg = mkOverlay
    }
    where
    mkGrid :: (Int, Int) [ImageAlign] [[St -> *(Image m, *St)]] *St -> *(Compose m, *St)
    mkGrid dims ias imgss st
      # (imgss, st) = foldr f ([], st) imgss
      = ret (AsGrid dims ias imgss) st
      where
      f imgs (acc, st)
        # (imgs, st) = mapSt id imgs st
        = ([imgs:acc], st)
    mkCollage :: [*St -> *(Image m, *St)] *St -> *(Compose m, *St)
    mkCollage imgs st
      # (imgs, st) = mapSt id imgs st
      = ret (AsCollage imgs) st
    mkOverlay :: [ImageAlign] [*St -> *(Image m, *St)] *St -> *(Compose m,*St)
    mkOverlay ias imgs st
      # (imgs, st) = mapSt id imgs st
      = ret (AsOverlay ias imgs) st
  fixSpansSpanAlgs :: SpanAlg (*St -> *(Span, *St)) (*St -> *(Span, *St))
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
  fixSpansLookupSpanAlgs :: LookupSpanAlg (*St -> *(Span,*St))
  fixSpansLookupSpanAlgs =
    { lookupSpanColumnXSpanAlg  = \ts n   -> ret (PxSpan 100.0) // TODO
    , lookupSpanRowYSpanAlg     = \ts n   -> ret (PxSpan 100.0) // TODO
    , lookupSpanImageXSpanAlg   = mkImageSpan (\x -> x.xspan) ImageXSpan
    , lookupSpanImageYSpanAlg   = mkImageSpan (\x -> x.yspan) ImageYSpan
    , lookupSpanDescentYSpanAlg = \fd     -> ret (PxSpan 100.0) // TODO Will we even use this?
    , lookupSpanExYSpanAlg      = \fd     -> ret (PxSpan 100.0) // TODO Shouldn't we simply use em instead?
    , lookupSpanTextXSpanAlg    = \fd str -> getTextLength fd str
    }
    where
    mkImageSpan :: (ImageSpan -> Span) ((Set ImageTag) -> LookupSpan) (Set ImageTag) *St -> *(Span, *St)
    mkImageSpan f c ts (clval, world)
      = case 'DM'.toList ('DM'.filterWithKey (\k _ -> 'DS'.isSubsetOf ts k) clval.taggedSpanEnv) of
          []         -> (LookupSpan (c ts), (clval, world))
          [(_, x):_] -> (getSpan x, (clval, world))
      where
      getSpan (CachedImageSpan x) = f x
      getSpan _                   = LookupSpan (c ts)

//:: ToSVGImageSyn :== (SVGElt, ImageSpan)

//toSVGImage :: (Image a) *St -> *((*St -> *(ToSVGImageSyn, *St)), *St)
//toSVGImage img st = imageCata allAlgs img st
  //where
  ////allAlgs :: Algebras m (*St -> *([SVGAttr] -> *St -> *(ToSVGImageSyn, *St), *St))
                      ////(*St -> *(SVGAttr, *St))
                      ////(*St -> *(SVGTransform, *St))
                      ////(*St -> *(*St -> *(ToSVGImageSyn, *St), *St))
                      ////(ImageSpan -> [SVGAttr] -> *St -> *(ToSVGImageSyn, *St))
                      ////(*St -> *(ImageSpan, *St))
                      ////([SVGAttr] -> *St -> *(ToSVGImageSyn, *St))
                      ////Real b
                      //////(*St -> *(Maybe ToSVGImageSyn, *St))
                      ////(*St -> *(*St -> *(Maybe ToSVGImageSyn, *St), *St))
                      ////(*St -> *(Compose, *St))
                      ////(*St -> *(Span, *St))
                      ////(*St -> *(Span, *St))
  //allAlgs =
    //{ imageAlgs          = imageAlgs
    //, imageContentAlgs   = imageContentAlgs
    //, imageAttrAlgs      = imageAttrAlgs
    //, imageTransformAlgs = imageTransformAlgs
    //, imageSpanAlgs      = imageSpanAlgs
    //, basicImageAlgs     = basicImageAlgs
    //, compositeImageAlgs = compositeImageAlgs
    //, composeAlgs        = composeAlgs
    //, spanAlgs           = reduceSpanSpanAlgs
    //, lookupSpanAlgs     = reduceSpanLookupSpanAlgs
    //}
  //imageAlgs :: ImageAlg (*St -> *(([SVGAttr] *St -> *(ToSVGImageSyn, *St)), *St))
                        //(*St -> *(SVGAttr, *St))
                        //(*St -> *(SVGTransform, *St))
                        //(*St -> *((*St -> *(ToSVGImageSyn, *St)), *St))
  //imageAlgs =
    //{ imageAlg = mkImage
    //}
    //where
    //mkImage imCo imAts imTrs imTas st
      //# (imCo, st)  = imCo st
      //# (imAts, st) = mapSt id imAts st
      //# (imTrs, st) = mapSt id imTrs st
      //= ret (imCo (mkAttrs imAts imTrs)) st
    //mkAttrs imAts [] = imAts
    //mkAttrs imAts xs = [TransformAttr xs:imAts]

  //imageContentAlgs :: ImageContentAlg (ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St))
                        //(*St -> *(ImageSpan, *St))
                        //([SVGAttr] *St -> *(ToSVGImageSyn, *St))
                        //(*St -> *([SVGAttr] *St -> *(ToSVGImageSyn, *St), *St))
  //imageContentAlgs =
    //{ imageContentBasicAlg     = mkBasic
    //, imageContentCompositeAlg = mkComposite
    //}
    //where
    //mkBasic :: (ImageSpan [SVGAttr] *St -> (ToSVGImageSyn, *St))
               //(*St -> *(ImageSpan, *St))
               //*St -> *([SVGAttr] *St -> (ToSVGImageSyn, *St), *St)
    //mkBasic baIm imSp st
      //# (imSp, st) = imSp st
      //= ret (\attrs -> baIm imSp attrs) st
    //mkComposite :: ([SVGAttr] *St -> (ToSVGImageSyn, *St))
                   //*St -> *([SVGAttr] *St -> (ToSVGImageSyn, *St), *St)
    //mkComposite coIm st
      //= ret (\attrs -> coIm attrs) st
  //imageAttrAlgs :: ImageAttrAlg m (*St -> *(SVGAttr, *St))
  //imageAttrAlgs =
    //{ imageAttrImageStrokeAttrAlg   = \attr -> ret (StrokeAttr (PaintColor attr.stroke Nothing))
    //, imageAttrStrokeWidthAttrAlg   = \attr -> mkStrokeWidth attr
    //, imageAttrStrokeOpacityAttrAlg = \attr -> ret (StrokeOpacityAttr (toString attr.opacity))
    //, imageAttrFillAttrAlg          = \attr -> ret (FillAttr (PaintColor attr.fill Nothing))
    //, imageAttrFillOpacityAttrAlg   = \attr -> ret (FillOpacityAttr (FillOpacity (toString attr.opacity)))
    //, imageAttrOnClickAttrAlg       = \attr -> abort "imageAttrOnClickAttrAlg" // (("onclick", "TODO How?"), st) // TODO
    //}
    //where
    //mkStrokeWidth :: (StrokeWidthAttr a) *St -> *(SVGAttr, *St)
    //mkStrokeWidth attr st
      //# (sp, st) = evalSpan attr.strokewidth st
      //= ret (StrokeWidthAttr (StrokeWidthLength (toString sp, PX))) st
  //imageTransformAlgs :: ImageTransformAlg Real (*St -> *(Span, *St)) (*St -> (SVGTransform, *St))
  //imageTransformAlgs =
    //{ imageTransformRotateImageAlg = \imAn    -> ret (RotateTransform (toString imAn) Nothing)
    //, imageTransformSkewXImageAlg  = \imAn    -> ret (SkewXTransform (toString imAn))
    //, imageTransformSkewYImageAlg  = \imAn    -> ret (SkewYTransform (toString imAn))
    //, imageTransformFitImageAlg    = \sp1 sp2 -> ret (ScaleTransform "scale" "1.0") // TODO
    //, imageTransformFitXImageAlg   = \sp      -> ret (ScaleTransform "scale" "1.0") // TODO
    //, imageTransformFitYImageAlg   = \sp      -> ret (ScaleTransform "scale" "1.0") // TODO
    //}
  //imageSpanAlgs :: ImageSpanAlg (*St -> *(Span, *St)) (*St -> *(ImageSpan, *St))
  //imageSpanAlgs =
    //{ imageSpanAlg = mkImageSpan
    //}
    //where
    //mkImageSpan sp1 sp2 st
      //# (sp1, st) = sp1 st
      //# (sp2, st) = sp2 st
      //= ret { ImageSpan | xspan = sp1, yspan = sp2} st
  //basicImageAlgs :: BasicImageAlg (ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St))
  //basicImageAlgs =
    //{ basicImageEmptyImageAlg   = mkEmptyImage
    //, basicImageTextImageAlg    = mkTextImage
    //, basicImageLineImageAlg    = mkLineImage
    //, basicImageCircleImageAlg  = mkCircleImage
    //, basicImageRectImageAlg    = mkRectImage
    //, basicImageEllipseImageAlg = mkEllipseImage
    //}
    //where
    //mkEmptyImage :: ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkEmptyImage       imSp imAts st
      //# (wh, st) = mkWH imSp st
      //= ret (GElt wh imAts [], imSp) st
    //mkTextImage :: FontDef String ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkTextImage fd str imSp imAts st
      //= ret (TextElt [] imAts str, imSp) st
    //mkLineImage :: Slash ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkLineImage sl  imSp imAts st
      //# (xsp, st) = evalSpan imSp.xspan st
      //# (ysp, st) = evalSpan imSp.yspan st
      //= ret (LineElt [] (imAts ++ mkLineAttrs sl (xsp, ysp)), imSp) st
      //where
      //mkLineAttrs slash (xspan, yspan)
        //# (y1, y2) = case slash of
                       //Slash     -> (toString yspan, "0.0")
                       //Backslash -> ("0.0", toString yspan)
        //= [ X1Attr ("0.0", PX), X2Attr (toString xspan, PX), Y1Attr (y1, PX), Y2Attr (y2, PX)]
    //mkRectImage :: ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkRectImage     imSp imAts st
      //# (wh, st) = mkWH imSp st
      //= ret (RectElt wh imAts, imSp) st
    //mkCircleImage :: ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkCircleImage   imSp imAts st
      //# (xsp, st) = evalSpan imSp.xspan st
      //# r         = toString (xsp / 2.0)
      //= ret (CircleElt [] [ RAttr (r, PX), CxAttr (r, PX)
                          //, CyAttr (r, PX) : imAts], imSp) st // TODO Cx and Cy depend on positioning
    //mkEllipseImage :: ImageSpan [SVGAttr] *St -> *(ToSVGImageSyn, *St)
    //mkEllipseImage imSp imAts st
      //# (xsp, st) = evalSpan imSp.xspan st
      //# (ysp, st) = evalSpan imSp.yspan st
      //= ret (EllipseElt [] (imAts ++ [ RxAttr (toString (xsp / 2.0), PX), RyAttr (toString (ysp / 2.0), PX)
                                     //, CxAttr (toString (xsp / 2.0), PX), CyAttr (toString (ysp / 2.0), PX)]), imSp) st
    //mkWH :: ImageSpan *St -> *([HtmlAttr], *St)
    //mkWH imSp st
      //# (xsp, st) = evalSpan imSp.xspan st
      //# (ysp, st) = evalSpan imSp.yspan st
      //= ret [WidthAttr (toString xsp), HeightAttr (toString ysp)] st
  ////compositeImageAlgs :: CompositeImageAlg (*St -> *(Span, *St))
                                          ////(*St -> *(*St -> *(ToSVGImageSyn, *St), *St))
                                          ////(*St -> *(Compose, *St))
                                          ////([SVGAttr] *St -> *(ToSVGImageSyn, *St))
  //compositeImageAlgs =
    //{ compositeImageAlg = mkCompositeImage
    //}
    //where
    //mkCompositeImage offs ho co imAts st
      //# (offs, st)  = let f (sp1, sp2) (xs, st)
                            //# (sp1, st) = sp1 st
                            //# (sp2, st) = sp2 st
                            //= ([(sp1, sp2):xs], st)
                      //in foldr f ([], st) offs
      //# (ho, st)    = case ho of
                        //Just x
                          //# (x, st) = x st
                          //# (x, st) = x st
                          //= (Just x, st)
                        //_ = (Nothing, st)
      //# (co, st)    = co st
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
    //calcOffset mxsp mysp (_, imSp) (xal, yal) = (mkXAl xal, mkYAl yal)
      //where
      //mkXAl AtLeft    = px 0.0
      //mkXAl AtMiddleX = (mxsp /. 2.0) - (imSp.xspan /. 2.0)
      //mkXAl AtRight   = mxsp - imSp.xspan
      //mkYAl AtTop     = px 0.0
      //mkYAl AtMiddleY = (mysp /. 2.0) - (imSp.yspan /. 2.0)
      //mkYAl AtBottom  = mysp - imSp.yspan
  ////composeAlgs :: ComposeAlg (*St -> *(*St -> *(ToSVGImageSyn, *St), *St))
                            ////(*St -> *(Compose, *St))
  //composeAlgs =
    //{ composeAsGridAlg    = \n ias -> ret (AsGrid n ias)
    //, composeAsCollageAlg =           ret AsCollage
    //, composeAsOverlayAlg = \ias   -> ret (AsOverlay ias)
    //}

evalList xs st :==
  let f x (xs, st)
        # (x, st) = x st
        = ([x:xs], st)
  in  foldr f ([], st) xs

ret x :== \st -> (x, st)

//mkTranslateGroup :: ImageOffset (SVGElt, ImageSpan) *St -> *((SVGElt, ImageSpan), *St)
//mkTranslateGroup (xoff, yoff) (contents, imSp) st
   //# (xsp, st) = evalSpan xoff st
   //# (ysp, st) = evalSpan yoff st
   //= ret (GElt [] (mkTranslateAttr (xsp, ysp)) [contents], imSp) st
  //where
  //mkTranslateAttr :: (Real, Real) -> [SVGAttr]
  //mkTranslateAttr (0.0,   0.0)   = []
  //mkTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

undef = undef

evalSpan :: Span *St -> *(Real, *St)
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

//reduceSpanSpanAlgs =
  //{ spanPxSpanAlg     = \r   -> ret (PxSpan r)
  //, spanLookupSpanAlg = \lu  -> reduceLU lu
  //, spanAddSpanAlg    = \x y -> reduceSpanBin (+) AddSpan x y
  //, spanSubSpanAlg    = \x y -> reduceSpanBin (-) SubSpan x y
  //, spanMulSpanAlg    = \x y -> reduceSpanNum (*) MulSpan x y
  //, spanDivSpanAlg    = \x y -> reduceSpanNum (/) DivSpan x y
  //, spanAbsSpanAlg    = \x   -> mkAbs x
  //, spanMinSpanAlg    = \xs  -> reduceSpanList min MinSpan xs
  //, spanMaxSpanAlg    = \xs  -> reduceSpanList max MaxSpan xs
  //}
  //where
  //reduceLU x st = x st

//reduceSpanLookupSpanAlgs =
  //{ lookupSpanColumnXSpanAlg  = \ts n   -> ret (PxSpan 100.0)
  //, lookupSpanDescentYSpanAlg = \fd     -> ret (PxSpan 100.0) // TODO Will we even use this?
  //, lookupSpanExYSpanAlg      = \fd     -> ret (PxSpan 100.0) // TODO Shouldn't we simply use em instead?
  //, lookupSpanImageXSpanAlg   = \ts     -> ret (PxSpan 100.0)
  //, lookupSpanImageYSpanAlg   = \ts     -> ret (PxSpan 100.0)
  //, lookupSpanRowYSpanAlg     = \ts n   -> ret (PxSpan 100.0)
  //, lookupSpanTextXSpanAlg    = \fd str -> doLU fd str
  //}
  //where
  //doLU fd str st
    //# (sp, st) = getTextLength fd str st
    //= (PxSpan sp, st)

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

reduceSpanBin :: (Real Real -> Real) (Span Span -> Span) (*St -> *(Span, *St)) (*St -> *(Span, *St)) *St -> *(Span, *St)
reduceSpanBin op cons x y st
  # (x, st) = x st
  # (y, st) = y st
  = (case (x, y) of
       (PxSpan x`, PxSpan y`) -> PxSpan (op x` y`)
       (x`, y`)               -> cons x` y`, st)

reduceSpanNum :: (Real Real -> Real) (Span Real -> Span) (*St -> *(Span, *St)) Real *St -> *(Span, *St)
reduceSpanNum op cons x y st
  # (x, st) = x st
  = (case x of
       PxSpan x` -> PxSpan (op x` y)
       x`        -> cons x` y, st)

reduceSpanList :: (Real Real -> Real) ([Span] -> Span) [*St -> *(Span, *St)] *St -> *(Span, *(ClientState, *JSWorld))
reduceSpanList op cons ss st
  # (ss, st) = mapSt id ss st
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
