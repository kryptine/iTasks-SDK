implementation module editletGraphics

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdArray

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

toMouseEvent :: !ComponentId !ComponentId !(JSObj JSEvent) !*JSWorld -> (!MouseEvent,!*JSWorld)
toMouseEvent svg_id elt_id event env
# (clientX,env)		= .? (event .# "clientX") env
# (clientY,env)		= .? (event .# "clientY") env
# (screenX,env)		= .? (event .# "screenX") env
# (screenY,env)		= .? (event .# "screenY") env
# (button, env)		= .? (event .# "button")  env
# (svg,env)			= getDomElement svg_id env
# (pt, env)			= (svg .# "createSVGPoint" .$ Void) env
# env				= (pt .# "x" .= clientX) env
# env				= (pt .# "y" .= clientY) env
# (ctm,env)			= (svg .# "getScreenCTM" .$ Void) env
# (inv,env)			= (ctm .# "inverse" .$ Void) env
# (pt`,env)			= (pt .# "matrixTransform" .$ [toJSArg inv]) env
# (x, env)			= .? (pt` .# "x") env
# (y, env)			= .? (pt` .# "y") env
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
	client					= {EditletClientDef | updateUI = updUI mrs
							                    , defVal   = ((Initialize,i),i)
							                    , genDiff  = genClientDiff
							                    , appDiff  = \i` ((cst,_),i) -> ((cst,i`),i)
							                    }
	(minx,miny,maxx,maxy)	= boundingbox mrs
	(w,h)					= (maxx-minx, maxy-miny)
	
	genUI cid env			= ({ComponentHTML | width      = ExactSize (maxx-minx)
							                  , height     = ExactSize (maxy-miny)
							                  , html       = DivTag [IdAttr (main_id cid)] 
							                                    [SvgTag [IdAttr (main_svg_id cid)]
							                                            [ViewBoxAttr (toString minx) (toString miny) (toString w) (toString h)] []
							                                    ]
							                  , eventHandlers = []
							   }
							  ,env
							  )
	
	genServerDiff (MR _ i) (MR _ i`)
		= diffUI i i`
	
	updUI mrs cid mi ((Initialize,i`),i) env
	# env					= foldl add_rect env [(mr,i) \\ mr <- mrs & i <- [0..]]
	= updUI mrs cid mi ((Running,i`),i) env
	where
		add_rect env (mr,i)
		# (rect,env)		= (jsDocument .# "createElementNS" .$ [toJSArg "http://www.w3.org/2000/svg", toJSArg "rect"]) env
		# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "id",           toJSArg (elt_id cid i)]) env
		# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "width",        toJSArg (toString (fst mr.ModelRect.size)+++"px")]) env
		# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "height",       toJSArg (toString (snd mr.ModelRect.size)+++"px")]) env
		# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "x",            toJSArg (toString (fst mr.ModelRect.pos)+++"px")]) env
		# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "y",            toJSArg (toString (fst mr.ModelRect.pos)+++"px")]) env
		# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke-width", toJSArg (toString mr.ModelRect.framew+++"px")]) env
		# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke",       toJSArg mr.ModelRect.frame]) env
		# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill",         toJSArg mr.ModelRect.fill]) env
		# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill-opacity", toJSArg (toString mr.ModelRect.opacity)]) env
		# (_,env)			= (rect .# "addEventListener" .$ [toJSArg "click",toJSArg (createEditletEventHandler (select_rect i) cid)]) env
		# (svg,env)			= getDomElement (main_svg_id cid) env
		# (_,env)			= (svg .# "appendChild" .$ [toJSArg rect]) env
		= env
	updUI _ cid mi ((running,_),i) env
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

select_rect :: Int ComponentId {JSObj JSEvent} ((ClientSt,Int),Int) *JSWorld -> *(!(!(!ClientSt,!Int),!Int), !*JSWorld)
select_rect i` cid events=:{[0]=event} ((cst,_),i) env
# (mouse, env)				= toMouseEvent (main_svg_id cid) (elt_id cid i`) event env
# env						= jsTrace mouse env
= (((cst,i`),i`), set_framew (elt_id cid i`) THICK (set_framew (elt_id cid i) THIN env))

set_framew :: ComponentId Int *JSWorld -> *JSWorld
set_framew cid fw env
# (r,env)			= getDomElement cid env
# (_,env)			= (r .# "setAttribute" .$ [toJSArg "stroke-width",toJSArg (toString fw+++"px")]) env
= env

main_id :: ComponentId -> ComponentId
main_id cid = cid

main_svg_id :: ComponentId -> ComponentId
main_svg_id cid = cid +++ "-svg"

elt_id :: ComponentId Int -> ComponentId
elt_id cid nr = main_svg_id cid +++ "-" +++ toString nr

boundingbox :: [ModelRect] -> (Int,Int,Int,Int)
boundingbox mrs
	= (minx-THICK/2, miny-THICK/2, maxx+THICK/2, maxy+THICK/2)
where
	(minx,miny,maxx,maxy)	= foldr (\{pos=(x,y),size=(w,h)} (minx,miny,maxx,maxy) -> (min minx x, min miny y, max maxx (x+w), max maxy (y+h))) 
							        (zero,zero,zero,zero) mrs
