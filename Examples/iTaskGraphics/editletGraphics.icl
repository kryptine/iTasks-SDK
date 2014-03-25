implementation module editletGraphics

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdArray
from   StdFunc import flip
import StdDebug
import GenPrint

svg_rects :: Task Void
svg_rects = (updateSharedInformation "Click the rects" [] share -&&- updateSharedInformation "What is clicked?" [UpdateWith view upd] share) @ const Void
where
	share = sharedStore "MyRects" mrs
	mrs   = MR [{ pos = (dx+dx/10,dx+dx/14), size = (200-2*dx,220-2*dx), frame = "black", framew = 1, fill = "limegreen", opacity = (toReal dx) / 100.0 }
               \\ dx <- [0,10..90]
               ] 0
	view (MR _ i)		= i
	upd (MR mrs _) i	= MR mrs i

::  MR = MR [ModelRect] Int

derive JSONEncode     MR
derive JSONDecode     MR
derive gEditMeta      MR
derive gVerify        MR
derive gEq            MR
derive gDefault       MR
derive gVisualizeText MR

gEditor{|MR|} dp vv=:(mr,mask,ver) meta vst
	= gEditor{|*|} dp (rectsEditlet mr,mask,ver) meta vst

gUpdate{|MR|} dp upd (mr,mask) iworld
    # ((Editlet mr` _ _, mask),iworld) = gUpdate{|*|} dp upd (rectsEditlet mr,mask) iworld
    = ((mr`,mask),iworld)

:: MouseEvent = {screenPos :: !(!Int,!Int), clientPos :: !(!Int,!Int), buttonSt :: !MouseButton}
:: MouseButton = LeftButton | MiddleButton | RightButton
/* printing using generic gPrint does not work.
//derive gPrint MouseEvent, MouseButton, (,)
instance toString MouseEvent  where toString me = printToString me
instance toString MouseButton where toString mb = printToString mb
*/
// old-fashioned printing...
instance toString (Maybe a) | toString a where toString Nothing = "Nothing"
                                               toString (Just a) = "(Just " <+++ toString a <+++ ")"
instance toString (a,b) | toString a & toString b where toString (a,b) = "(" <+++ toString a <+++ "," <+++ toString b <+++ ")"
instance toString MouseButton where toString LeftButton   = "LeftButton"
                                    toString MiddleButton = "MiddleButton"
                                    toString RightButton  = "RightButton"
instance toString MouseEvent  where toString {screenPos,clientPos,buttonSt}
                                        = "{ screenPos = " <+++ toString screenPos <+++ " , clientPos = " <+++ toString clientPos <+++ " , buttonSt = " <+++ toString buttonSt <+++ "}"

toMouseEvent :: !ComponentId !ComponentId !(JSVal JSEvent) !*JSWorld -> (!MouseEvent,!*JSWorld)
toMouseEvent svg_id elt_id event env
# (clientX,env)		= jsGetObjectAttr "clientX" event env
# (clientY,env)		= jsGetObjectAttr "clientY" event env
# (screenX,env)		= jsGetObjectAttr "screenX" event env
# (screenY,env)		= jsGetObjectAttr "screenY" event env
# (button, env)		= jsGetObjectAttr "button"  event env
# (svg,env)			= getDomElement svg_id env
# (pt, env)			= callObjectMethod "createSVGPoint" [] svg env
# env				= jsSetObjectAttr "x" clientX pt env
# env				= jsSetObjectAttr "y" clientY pt env
# (ctm,env)			= callObjectMethod "getScreenCTM" [] svg env
# (inv,env)			= callObjectMethod "inverse" [] ctm env
# (pt`,  env)		= callObjectMethod "matrixTransform" [toJSArg inv] pt env
# (x, env)			= jsGetObjectAttr "x" pt` env
# (y, env)			= jsGetObjectAttr "y" pt` env
= ({ screenPos = (jsValToInt screenX,jsValToInt screenY)
   , clientPos = (jsValToInt x,jsValToInt y)
   , buttonSt  = case jsValToInt button of
                   0 = LeftButton
                   1 = MiddleButton
                   2 = RightButton
   }
  ,env)


::  ModelRect = { pos :: !(!Int,!Int), size :: !(!Int,!Int), frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real }
derive class iTask ModelRect

THICK :== 4
THIN  :== 1
::  ClientSt = Initialize | Running

rectsEditlet :: MR -> Editlet MR Int
rectsEditlet mr=:(MR mrs i)	= Editlet mr server client
where
	server					= {EditletServerDef | genUI    = genUI
							                    , defVal   = gDefault{|*|}
							                    , genDiff  = genServerDiff
							                    , appDiff  = \i` (MR mrs _) -> consistent (MR mrs i`)
							  }
	client					= {EditletClientDef | updateUI = updUI
							                    , defVal   = ((Initialize,i),i)
							                    , genDiff  = genClientDiff
							                    , appDiff  = \i` ((cst,_),i) -> ((cst,i`),i)
							                    }
	(minx,miny,maxx,maxy)	= boundingbox mrs
	(w,h)					= (maxx-minx, maxy-miny)
	
	genUI cid env			= ({ComponentHTML | width      = ExactSize (maxx-minx)
							                  , height     = ExactSize (maxy-miny)
							                  , html       = DivTag [IdAttr (main_id cid)] 
							                                    [SvgTag [IdAttr (main_svg_id cid)] [ViewBoxAttr (toString minx)
							                                                                                    (toString miny)
							                                                                                    (toString w)
							                                                                                    (toString h)
							                                                                       ] 
							                                            [ (tosvg {r & framew = if (j==i) THICK THIN}) <@< (IdAttr (elt_id cid j))
							                                            \\ r <- mrs & j <- [0..]
							                                            ]
							                                    ]
							                  , eventHandlers = []
							   }
							  ,env
							  )
	
	genServerDiff (MR _ i) (MR _ i`)
		= diffUI i i`
	
	updUI cid mi ((Initialize,i`),i) env
	# env					= foldr add_click env [0..length mrs-1]
	= updUI cid mi ((Running,i`),i) env
	where
		add_click i env
		# (rect,env)		= getDomElement (elt_id cid i) env
		# (_,env)			= callObjectMethod "addEventListener" [toJSArg "click",toJSArg (createEditletEventHandler (select_rect i) cid)] rect env
		= env
	updUI cid mi ((running,_),i) env
	# env					= set_framew (elt_id cid i)  THIN  env
	# env					= set_framew (elt_id cid i`) THICK env
	= (((running,i`),i`),env)
	where
		i`					= fromMaybe i mi
	
	genClientDiff ((_,i),_) ((_,i`),_)
		= diffUI i i`
	
	diffUI i i`
	| i <> i`				= Just i`
	| otherwise				= Nothing

consistent :: !MR -> MR
consistent (MR mrs i)		= MR [{mr & framew = if (j==i) THICK THIN} \\ mr <- mrs & j <- [0..]] i

select_rect :: Int ComponentId {JSVal JSEvent} ((ClientSt,Int),Int) *JSWorld -> *(!(!(!ClientSt,!Int),!Int), !*JSWorld)
select_rect i` cid events=:{[0]=event} ((cst,_),i) env
# (mouse, env)				= toMouseEvent (main_svg_id cid) (elt_id cid i`) event env
# env						= jsTrace mouse env
= (((cst,i`),i`), set_framew (elt_id cid i`) THICK (set_framew (elt_id cid i) THIN env))

set_framew :: ComponentId Int *JSWorld -> *JSWorld
set_framew cid fw env
# (r,env)			= getDomElement cid env
# (_,env)			= callObjectMethod "setAttribute" [toJSArg "stroke-width",toJSArg (toString fw+++"px")] r env
= env

main_id :: ComponentId -> ComponentId
main_id cid = cid

main_svg_id :: ComponentId -> ComponentId
main_svg_id cid = cid +++ "-svg"

elt_id :: ComponentId Int -> ComponentId
elt_id cid nr = main_svg_id cid +++ "-" +++ toString nr

main_svg_mouse :: ComponentId *JSWorld -> (!(!Int,!Int),!*JSWorld)
main_svg_mouse cid env
# (main_svg,env)		= getDomElement (main_svg_id cid) env
# (clientX,env)			= jsGetObjectAttr "x" main_svg env
# (clientY,env)			= jsGetObjectAttr "y" main_svg env
= ((jsValToInt clientX,jsValToInt clientY),env)

boundingbox :: [ModelRect] -> (Int,Int,Int,Int)
boundingbox mrs
	= (minx-THICK/2, miny-THICK/2, maxx+THICK/2, maxy+THICK/2)
where
	(minx,miny,maxx,maxy)	= foldr (\{pos=(x,y),size=(w,h)} (minx,miny,maxx,maxy) -> (min minx x, min miny y, max maxx (x+w), max maxy (y+h))) 
							        (zero,zero,zero,zero) mrs

tosvg :: ModelRect -> SVGElt
tosvg r
	= RectElt [ WidthAttr       (toString          (fst r.ModelRect.size)+++"px")
              , HeightAttr      (toString          (snd r.ModelRect.size)+++"px")
              ]
              [ XAttr           (toString          (fst r.ModelRect.pos),PX)
              , YAttr           (toString          (snd r.ModelRect.pos),PX)
              , StrokeWidthAttr (StrokeWidthLength (toString r.ModelRect.framew,PX))
              , StrokeAttr      (PaintColor        (SVGColorText r.ModelRect.frame) Nothing)
              , FillAttr        (PaintColor        (SVGColorText r.ModelRect.fill)  Nothing)
              , FillOpacityAttr (FillOpacity       (toString r.ModelRect.opacity))
              ]

class (<@<) infixl a :: !SVGElt !a -> SVGElt
instance <@< HtmlAttr where <@< (SVGElt   html_attrs svg_attrs svg_elts) html_attr = SVGElt   [html_attr : html_attrs] svg_attrs svg_elts
                            <@< (GElt     html_attrs svg_attrs svg_elts) html_attr = GElt     [html_attr : html_attrs] svg_attrs svg_elts
                            <@< (ImageElt html_attrs svg_attrs svg_elts) html_attr = ImageElt [html_attr : html_attrs] svg_attrs svg_elts
                            <@< (RectElt  html_attrs svg_attrs)          html_attr = RectElt  [html_attr : html_attrs] svg_attrs
instance <@< SVGAttr  where <@< (SVGElt   html_attrs svg_attrs svg_elts)  svg_attr = SVGElt   html_attrs [svg_attr : svg_attrs] svg_elts
                            <@< (GElt     html_attrs svg_attrs svg_elts)  svg_attr = GElt     html_attrs [svg_attr : svg_attrs] svg_elts
                            <@< (ImageElt html_attrs svg_attrs svg_elts)  svg_attr = ImageElt html_attrs [svg_attr : svg_attrs] svg_elts
                            <@< (RectElt  html_attrs svg_attrs)           svg_attr = RectElt  html_attrs [svg_attr : svg_attrs]
