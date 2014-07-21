implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import iTasks
import iTasks.API.Core.Client.Editlet
from StdOrdList import minList, maxList

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
  YAlign

:: ClientState =
  { didInit       :: Bool
  , didDraw       :: Bool
  , fontSpanCache :: Map (FontDef, String) Span
  }

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
                   , fontSpanCache = 'DM'.newMap }
      , genDiff  =  \x y -> if (x === y) Nothing (Just 0)
      , appDiff  =  \_ x -> x
      }
  genUI cid world
    # w = 1920 // TODO Calculate from img
    # h = 1080 // TODO Calculate from img
    # minx = 1920 // TODO Calculate from img
    # miny = 1080 // TODO Calculate from img
    = ({ ComponentHTML
       | width      = ExactSize w
       , height     = ExactSize h
       , html       = SvgTag [IdAttr (mainSvgId cid)]
                        [ViewBoxAttr (toString minx) (toString miny) (toString w) (toString h)] []
                        //[] []
       , eventHandlers = []
       }
       , world
      )

  updateUI cid diffs clval=:{didInit = False} world
    # world = jsTrace "updateUI didInit False" world
    = ({clval & didInit = True}, world)

  updateUI cid diffs clval=:{didInit = True} world
    # world = jsTrace "updateUI didInit True" world
    # (r, (clval, world)) = toSVGImage img (clval, world)
    # (p, world)          = new "DOMParser" () world
    # (pdoc, world)       = (p .# "parseFromString" .$ (toString r, "application/xml")) world
    # (docel, world)      = .? (pdoc .# "documentElement") world
    # (svg, world)        = getDomElement (mainSvgId cid) world
    # (_, world)          = (svg .# "appendChild" .$ docel) world
    = (clval, world)

toSVGImage img world = imageCata allAlgs img world
  where
  allAlgs =
    { imageAlgs          = imageAlgs
    , imageContentAlgs   = imageContentAlgs
    , imageAttrAlgs      = imageAttrAlgs
    , imageTransformAlgs = imageTransformAlgs
    , imageSpanAlgs      = imageSpanAlgs
    , imageTagAlgs       = imageTagAlgs
    , basicImageAlgs     = basicImageAlgs
    , composeImageAlgs   = composeImageAlgs
    , hostAlgs           = hostAlgs
    , composeAlgs        = composeAlgs
    , spanAlgs           = evalSpanSpanAlgs
    , lookupSpanAlgs     = evalSpanLookupSpanAlgs
    }
  imageAlgs =
    { imageAlg = \imCo imAts imTrs imTas st -> (imCo imAts imTrs imTas, st)
    }
  imageContentAlgs =
    { imageContentBasicAlg     = \baIm imSp st -> (baIm imSp, st)
    , imageContentCompositeAlg = \coIm      st -> (coIm, st)
    }
  imageAttrAlgs =
    { imageAttrImageStrokeAttrAlg   = \attr st -> (("stroke", toString attr.stroke), st)
    , imageAttrStrokeWidthAttrAlg   = mkStrokeWidth
    , imageAttrStrokeOpacityAttrAlg = \attr st -> (("stroke-opacity", toString attr.opacity), st)
    , imageAttrFillAttrAlg          = \attr st -> (("fill", toString attr.fill), st)
    , imageAttrFillOpacityAttrAlg   = \attr st -> (("fill-opacity", toString attr.opacity), st)
    , imageAttrOnClickAttrAlg       = \attr st -> (("onclick", "TODO How?"), st) // TODO
    }
    where
    mkStrokeWidth attr (clval, world)
      # (sp, world) = evalSpan attr.strokewidth world
      = (("stroke-width", toString sp), (clval, world))
  imageTransformAlgs =
    { imageTransformRotateImageAlg = \imAn    st -> (mkAttr "rotate" (toString imAn), st)
    , imageTransformSkewXImageAlg  = \imAn    st -> (mkAttr "skewX" (toString imAn), st)
    , imageTransformSkewYImageAlg  = \imAn    st -> (mkAttr "skewY" (toString imAn), st)
    , imageTransformFitImageAlg    = \sp1 sp2 st -> (undef, st) // TODO
    , imageTransformFitXImageAlg   = \sp      st -> (undef, st) // TODO
    , imageTransformFitYImageAlg   = \sp      st -> (undef, st) // TODO
    }
    where
    mkAttr attr val = attr +++ "(" +++ val +++ ")"
  imageSpanAlgs =
    { imageSpanAlg = \sp1 sp2 st -> ((sp1, sp2), st)
    }
  imageTagAlgs =
    { imageTagIntAlg    = \n    st -> (undef, st)
    , imageTagStringAlg = \str  st -> (undef, st)
    , imageTagSystemAlg = \n    st -> (undef, st)
    }
  basicImageAlgs =
    { basicImageEmptyImageAlg   = \       st -> (\wh             imAts imTrs imTas -> GElt (mkWH wh) [] [], st)
    , basicImageTextImageAlg    = \fd str st -> (\wh             imAts imTrs imTas -> TextElt [] [] str, st) // TODO
    , basicImageLineImageAlg    = \sl     st -> (\wh             imAts imTrs imTas -> LineElt [] (mkLineAttrs sl wh), st)
    , basicImageCircleImageAlg  = \       st -> (\(xspan, _)     imAts imTrs imTas -> CircleElt [] [RAttr (toString (xspan / 2.0), PX)], st)
    , basicImageRectImageAlg    = \       st -> (\wh             imAts imTrs imTas -> RectElt (mkWH wh) [], st)
    , basicImageEllipseImageAlg = \       st -> (\(xspan, yspan) imAts imTrs imTas -> EllipseElt [] [ RxAttr (toString (xspan / 2.0), PX), RyAttr (toString (yspan / 2.0), PX)
                                                                                                    , CxAttr (toString (xspan / 2.0), PX), CyAttr (toString (yspan / 2.0), PX)], st)
    }
    where
    mkWH (xspan, yspan) = [WidthAttr (toString xspan), HeightAttr (toString yspan)]
    mkLineAttrs slash (xspan, yspan)
      # (y1, y2) = case slash of
                     Slash     -> (yspan, 0.0)
                     Backslash -> (0.0, yspan)
      = [ X1Attr (toString 0.0, PX), X2Attr (toString yspan, PX)
        , Y1Attr (toString y1, PX), Y2Attr (toString y2, PX)]
  composeImageAlgs =
    { composeImageAlg = \sps ims ho co st -> (undef, st)
    }
  hostAlgs =
    { hostNothingAlg = \   st -> (undef, st)
    , hostJustAlg    = \im st -> (undef, st)
    }
  composeAlgs =
    { composeAsGridAlg    = \n ias st -> (undef, st)
    , composeAsCollageAlg = \      st -> (undef, st)
    , composeAsOverlayAlg = \ias   st -> (undef, st)
    }

undef = undef

evalSpan sp world = spanCata evalSpanSpanAlgs evalSpanLookupSpanAlgs evalSpanImageTagAlgs sp world

evalSpanSpanAlgs =
  { spanPxSpanAlg     = \r   st -> (r, st)
  , spanLookupSpanAlg = \lu  st -> (lu, st)
  , spanAddSpanAlg    = \x y st -> (x + y, st)
  , spanSubSpanAlg    = \x y st -> (x - y, st)
  , spanMulSpanAlg    = \x y st -> (x * y, st)
  , spanDivSpanAlg    = \x y st -> (x / y, st)
  , spanAbsSpanAlg    = \x   st -> (abs x, st)
  , spanMinSpanAlg    = \xs  st -> (minList xs, st)
  , spanMaxSpanAlg    = \xs  st -> (maxList xs, st)
  }
evalSpanLookupSpanAlgs =
  { lookupSpanColumnXSpanAlg  = \ts n   st -> (300.0, st)
  , lookupSpanDescentYSpanAlg = \fd     st -> (300.0, st)
  , lookupSpanExYSpanAlg      = \fd     st -> (300.0, st)
  , lookupSpanImageXSpanAlg   = \ts     st -> (300.0, st)
  , lookupSpanImageYSpanAlg   = \ts     st -> (300.0, st)
  , lookupSpanRowYSpanAlg     = \ts n   st -> (300.0, st)
  , lookupSpanTextXSpanAlg    = \fd str st -> (300.0, st)
  }
evalSpanImageTagAlgs =
  { imageTagIntAlg    = \_ st -> (0.0, st)
  , imageTagStringAlg = \_ st -> (0.0, st)
  , imageTagSystemAlg = \_ st -> (0.0, st)
  }


:: Algebras m imCo imAt imTr imTa im baIm imSp coIm imAn imOf ho hoIm co sp loSp st =
  { imageAlgs          :: ImageAlg imCo imAt imTr imTa im st
  , imageContentAlgs   :: ImageContentAlg baIm imSp coIm imCo st
  , imageAttrAlgs      :: ImageAttrAlg m imAt st
  , imageTransformAlgs :: ImageTransformAlg imAn sp imTr st
  , imageSpanAlgs      :: ImageSpanAlg sp imSp st
  , imageTagAlgs       :: ImageTagAlg imTa st
  , basicImageAlgs     :: BasicImageAlg baIm st
  , composeImageAlgs   :: ComposeImageAlg sp im ho co coIm st
  , hostAlgs           :: HostAlg im hoIm st
  , composeAlgs        :: ComposeAlg co st
  , spanAlgs           :: SpanAlg loSp sp st
  , lookupSpanAlgs     :: LookupSpanAlg imTa loSp st
  }

:: ImageAlg imCo imAt imTr imTa im st =
  { imageAlg :: imCo [imAt] [imTr] [imTa] st -> *(im, st)
  }

:: ImageContentAlg baIm imSp coIm imCo st =
  { imageContentBasicAlg     :: baIm imSp st -> *(imCo, st)
  , imageContentCompositeAlg :: coIm      st -> *(imCo, st)
  }

:: ImageAttrAlg m imAt st =
  { imageAttrImageStrokeAttrAlg   :: (StrokeAttr m)      st -> *(imAt, st)
  , imageAttrStrokeWidthAttrAlg   :: (StrokeWidthAttr m) st -> *(imAt, st)
  , imageAttrStrokeOpacityAttrAlg :: (OpacityAttr m)     st -> *(imAt, st)
  , imageAttrFillAttrAlg          :: (FillAttr m)        st -> *(imAt, st)
  , imageAttrFillOpacityAttrAlg   :: (OpacityAttr m)     st -> *(imAt, st)
  , imageAttrOnClickAttrAlg       :: (OnClickAttr m)     st -> *(imAt, st)
  }

:: ImageTransformAlg imAn sp imTr st =
  { imageTransformRotateImageAlg :: imAn  st -> *(imTr, st)
  , imageTransformSkewXImageAlg  :: imAn  st -> *(imTr, st)
  , imageTransformSkewYImageAlg  :: imAn  st -> *(imTr, st)
  , imageTransformFitImageAlg    :: sp sp st -> *(imTr, st)
  , imageTransformFitXImageAlg   :: sp    st -> *(imTr, st)
  , imageTransformFitYImageAlg   :: sp    st -> *(imTr, st)
  }

:: ImageSpanAlg sp imSp st =
  { imageSpanAlg :: sp sp st -> *(imSp, st)
  }

:: ImageTagAlg imTa st =
  { imageTagIntAlg    :: Int    st -> *(imTa, st)
  , imageTagStringAlg :: String st -> *(imTa, st)
  , imageTagSystemAlg :: Int    st -> *(imTa, st)
  }

:: BasicImageAlg baIm st =
  { basicImageEmptyImageAlg   ::                st -> *(baIm, st)
  , basicImageTextImageAlg    :: FontDef String st -> *(baIm, st)
  , basicImageLineImageAlg    :: Slash          st -> *(baIm, st)
  , basicImageCircleImageAlg  ::                st -> *(baIm, st)
  , basicImageRectImageAlg    ::                st -> *(baIm, st)
  , basicImageEllipseImageAlg ::                st -> *(baIm, st)
  }

:: ComposeImageAlg sp im ho co coIm st =
  { composeImageAlg :: [(sp, sp)] [im] ho co st -> *(coIm, st)
  }

:: HostAlg im hoIm st =
  { hostNothingAlg ::    st -> *(hoIm, st)
  , hostJustAlg    :: im st -> *(hoIm, st)
  }

:: ComposeAlg co st =
  { composeAsGridAlg    :: Int [ImageAlign] st -> *(co, st)
  , composeAsCollageAlg ::                  st -> *(co, st)
  , composeAsOverlayAlg :: [ImageAlign]     st -> *(co, st)
  }

:: SpanAlg loSp sp st =
  { spanPxSpanAlg     :: Real    st -> *(sp, st)
  , spanLookupSpanAlg :: loSp    st -> *(sp, st)
  , spanAddSpanAlg    :: sp sp   st -> *(sp, st)
  , spanSubSpanAlg    :: sp sp   st -> *(sp, st)
  , spanMulSpanAlg    :: sp Real st -> *(sp, st)
  , spanDivSpanAlg    :: sp Real st -> *(sp, st)
  , spanAbsSpanAlg    :: sp      st -> *(sp, st)
  , spanMinSpanAlg    :: [sp]    st -> *(sp, st)
  , spanMaxSpanAlg    :: [sp]    st -> *(sp, st)
  }

:: LookupSpanAlg imTa loSp st =
  { lookupSpanColumnXSpanAlg  :: [imTa] Int     st -> *(loSp, st)
  , lookupSpanDescentYSpanAlg :: FontDef        st -> *(loSp, st)
  , lookupSpanExYSpanAlg      :: FontDef        st -> *(loSp, st)
  , lookupSpanImageXSpanAlg   :: [imTa]         st -> *(loSp, st)
  , lookupSpanImageYSpanAlg   :: [imTa]         st -> *(loSp, st)
  , lookupSpanRowYSpanAlg     :: [imTa] Int     st -> *(loSp, st)
  , lookupSpanTextXSpanAlg    :: FontDef String st -> *(loSp, st)
  }

foldrCata cata xs st :==
  let f x (rs, st)
        # (r, st) = cata x st
        = ([r:rs], st)
  in  foldr f ([], st) xs

imageCata allAlgs { Image | content, attribs, transform, tags } st
  # (synContent, st)    = imageContentCata allAlgs content st
  # (synsAttribs, st)   = foldrCata (imageAttrCata allAlgs.imageAttrAlgs) attribs st
  # (synsTransform, st) = foldrCata (imageTransformCata allAlgs.imageTransformAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs allAlgs.imageTagAlgs) transform st
  # (synsTags, st)      = foldrCata (imageTagCata allAlgs.imageTagAlgs) tags st
  = allAlgs.imageAlgs.imageAlg synContent synsAttribs synsTransform synsTags st

imageContentCata allAlgs (Basic bi is) st
  # (synBasicImage, st) = basicImageCata allAlgs.basicImageAlgs bi st
  # (synImageSpan, st)  = imageSpanCata allAlgs.imageSpanAlgs allAlgs.spanAlgs allAlgs.lookupSpanAlgs allAlgs.imageTagAlgs is st
  = allAlgs.imageContentAlgs.imageContentBasicAlg synBasicImage synImageSpan st
imageContentCata allAlgs (Composite ci) st
  # (synCompositeImage, st) = compositeImageCata allAlgs ci st
  = allAlgs.imageContentAlgs.imageContentCompositeAlg synCompositeImage st

imageAttrCata imageAttrAlgs (ImageStrokeAttr sa)         st = imageAttrAlgs.imageAttrImageStrokeAttrAlg sa st
imageAttrCata imageAttrAlgs (ImageStrokeWidthAttr swa)   st = imageAttrAlgs.imageAttrStrokeWidthAttrAlg swa st
imageAttrCata imageAttrAlgs (ImageStrokeOpacityAttr swa) st = imageAttrAlgs.imageAttrStrokeOpacityAttrAlg swa st
imageAttrCata imageAttrAlgs (ImageFillAttr fa)           st = imageAttrAlgs.imageAttrFillAttrAlg fa st
imageAttrCata imageAttrAlgs (ImageFillOpacityAttr swa)   st = imageAttrAlgs.imageAttrFillOpacityAttrAlg swa st
imageAttrCata imageAttrAlgs (ImageOnClickAttr cl)        st = imageAttrAlgs.imageAttrOnClickAttrAlg cl st

imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (RotateImage ia) st
  = imageTransformAlgs.imageTransformRotateImageAlg ia st
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (SkewXImage ia) st
  = imageTransformAlgs.imageTransformSkewXImageAlg ia st
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (SkewYImage ia) st
  = imageTransformAlgs.imageTransformSkewYImageAlg ia st
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (FitImage sp1 sp2) st
  # (synSpan1, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp1 st
  # (synSpan2, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp2 st
  = imageTransformAlgs.imageTransformFitImageAlg synSpan1 synSpan2 st
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (FitXImage sp) st
  # (synSpan, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp st
  = imageTransformAlgs.imageTransformFitXImageAlg synSpan st
imageTransformCata imageTransformAlgs spanAlgs lookupSpanAlgs imageTagAlgs (FitYImage sp) st
  # (synSpan, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp st
  = imageTransformAlgs.imageTransformFitYImageAlg synSpan st

imageTagCata imageTagAlgs (ImageTagInt n)      st = imageTagAlgs.imageTagIntAlg n st
imageTagCata imageTagAlgs (ImageTagString str) st = imageTagAlgs.imageTagStringAlg str st
imageTagCata imageTagAlgs (ImageTagSystem n)   st = imageTagAlgs.imageTagSystemAlg n st

basicImageCata basicImageAlgs EmptyImage         st = basicImageAlgs.basicImageEmptyImageAlg st
basicImageCata basicImageAlgs (TextImage fd str) st = basicImageAlgs.basicImageTextImageAlg fd str st
basicImageCata basicImageAlgs (LineImage sl)     st = basicImageAlgs.basicImageLineImageAlg sl st
basicImageCata basicImageAlgs CircleImage        st = basicImageAlgs.basicImageCircleImageAlg st
basicImageCata basicImageAlgs RectImage          st = basicImageAlgs.basicImageRectImageAlg st
basicImageCata basicImageAlgs EllipseImage       st = basicImageAlgs.basicImageEllipseImageAlg st

imageSpanCata imageSpanAlgs spanAlgs lookupSpanAlgs imageTagAlgs { ImageSpan | xspan, yspan } st
  # (synSpan1, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs xspan st
  # (synSpan2, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs yspan st
  = imageSpanAlgs.imageSpanAlg synSpan1 synSpan2 st

compositeImageCata allAlgs { CompositeImage | offsets, content, host, compose } st
  # (synsImageOffset, st) = let f (l, r) (xs, st)
                                  # (synr, st) = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs allAlgs.imageTagAlgs l st
                                  # (synl, st) = spanCata allAlgs.spanAlgs allAlgs.lookupSpanAlgs allAlgs.imageTagAlgs r st
                                  = ([(synr, synl):xs], st)
                            in  foldr f ([], st) offsets
  # (synsContent, st)     = foldrCata (imageCata allAlgs) content st
  # (synHost, st)         = hostCata allAlgs host st
  # (synCompose, st)      = composeCata allAlgs.composeAlgs compose st
  = allAlgs.composeImageAlgs.composeImageAlg synsImageOffset synsContent synHost synCompose st

hostCata allAlgs Nothing st
  = allAlgs.hostAlgs.hostNothingAlg st
hostCata allAlgs (Just im) st
  # (synImage, st) = imageCata allAlgs im st
  = allAlgs.hostAlgs.hostJustAlg synImage st

composeCata composeAlgs (AsGrid n ias)  st = composeAlgs.composeAsGridAlg n ias st
composeCata composeAlgs AsCollage       st = composeAlgs.composeAsCollageAlg st
composeCata composeAlgs (AsOverlay ias) st = composeAlgs.composeAsOverlayAlg ias st

spanCata spanAlgs lookupSpanAlgs imageTagAlgs (PxSpan rl) st
  = spanAlgs.spanPxSpanAlg rl st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (LookupSpan lu) st
  # (synLookup, st) = lookupCata lookupSpanAlgs imageTagAlgs lu st
  = spanAlgs.spanLookupSpanAlg synLookup st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (AddSpan sp1 sp2) st
  # (synSpan1, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp1 st
  # (synSpan2, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp2 st
  = spanAlgs.spanAddSpanAlg synSpan1 synSpan2 st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (SubSpan sp1 sp2) st
  # (synSpan1, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp1 st
  # (synSpan2, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp2 st
  = spanAlgs.spanSubSpanAlg synSpan1 synSpan2 st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (MulSpan sp r) st
  # (synSpan, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp st
  = spanAlgs.spanMulSpanAlg synSpan r st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (DivSpan sp r) st
  # (synSpan, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp st
  = spanAlgs.spanDivSpanAlg synSpan r st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (AbsSpan sp) st
  # (synSpan, st) = spanCata spanAlgs lookupSpanAlgs imageTagAlgs sp st
  = spanAlgs.spanAbsSpanAlg synSpan st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (MinSpan sps) st
  # (synsSpans, st) = foldrCata (spanCata spanAlgs lookupSpanAlgs imageTagAlgs) sps st
  = spanAlgs.spanMinSpanAlg synsSpans st
spanCata spanAlgs lookupSpanAlgs imageTagAlgs (MaxSpan sps) st
  # (synsSpans, st) = foldrCata (spanCata spanAlgs lookupSpanAlgs imageTagAlgs) sps st
  = spanAlgs.spanMaxSpanAlg synsSpans st

lookupCata lookupSpanAlgs imageTagAlgs (ColumnXSpan imts n) st
  # (synsColumXSpans, st) = foldrCata (imageTagCata imageTagAlgs) imts st
  = lookupSpanAlgs.lookupSpanColumnXSpanAlg synsColumXSpans n st
lookupCata lookupSpanAlgs imageTagAlgs (DescentYSpan fd) st
  = lookupSpanAlgs.lookupSpanDescentYSpanAlg fd st
lookupCata lookupSpanAlgs imageTagAlgs (ExYSpan fd) st
  = lookupSpanAlgs.lookupSpanExYSpanAlg fd st
lookupCata lookupSpanAlgs imageTagAlgs (ImageXSpan imts) st
  # (synsImageXSpans, st) = foldrCata (imageTagCata imageTagAlgs) imts st
  = lookupSpanAlgs.lookupSpanImageXSpanAlg synsImageXSpans st
lookupCata lookupSpanAlgs imageTagAlgs (ImageYSpan imts) st
  # (synsImageYSpans, st) = foldrCata (imageTagCata imageTagAlgs) imts st
  = lookupSpanAlgs.lookupSpanImageYSpanAlg synsImageYSpans st
lookupCata lookupSpanAlgs imageTagAlgs (RowYSpan imts n) st
  # (synsRowYSpans, st) = foldrCata (imageTagCata imageTagAlgs) imts st
  = lookupSpanAlgs.lookupSpanRowYSpanAlg synsRowYSpans n st
lookupCata lookupSpanAlgs imageTagAlgs (TextXSpan fd str) st
  = lookupSpanAlgs.lookupSpanTextXSpanAlg fd str st
