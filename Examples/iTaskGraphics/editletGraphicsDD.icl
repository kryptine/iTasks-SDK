implementation module editletGraphicsDD

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import StdArray

svg_image :: Task MR
svg_image = updateInformation "Click the rects" [] circles
where
	circles = MR [Circle (dx/10) {ModelCircle | pos = (dx+dx/10,dx+dx/14), r = 200-2*dx, frame = "black", framew = 1, fill = "limegreen", opacity = (toReal dx) / 100.0, draggable = True }
               \\ dx <- [0,10..90]
               ] 0
	rects   = MR [Rect (dx/10) {ModelRect | pos = (dx+dx/10,dx+dx/14), size = (200-2*dx,220-2*dx), frame = "black", framew = 1, fill = "limegreen", opacity = (toReal dx) / 100.0 }
               \\ dx <- [0,10..90]
               ] 0
	lines   = MR [Line (dx/10) {ModelLine | pos = (0,dx), end = (200,200-2*dx), edge = "black", linew = dx/10 }
               \\ dx <- [0,10..90]
               ] 0
	view (MR _ i)		= i
	upd (MR mrs _) i	= MR mrs i

::  MR = MR [ModelImage] Int

derive JSONEncode     MR
derive JSONDecode     MR
derive gEditMeta      MR
derive gVerify        MR
derive gEq            MR
derive gDefault       MR
derive gVisualizeText MR

gEditor{|MR|} dp vv=:(mr,mask,ver) meta vst
	= gEditor{|*|} dp (imageEditlet mr,mask,ver) meta vst

gUpdate{|MR|} dp upd (mr,mask) iworld
    # ((Editlet mr` _ _, mask),iworld) = gUpdate{|*|} dp upd (imageEditlet mr,mask) iworld
    = ((mr`,mask),iworld)

::  ModelImage  = Line Int ModelLine | Rect Int ModelRect | Circle Int ModelCircle
::  ModelLine   = { pos :: !(!Int,!Int), end :: !(!Int,!Int), linew :: !Int, edge :: !String }
::  ModelRect   = { pos :: !(!Int,!Int), size :: !(!Int,!Int), frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real }
::  ModelCircle = { pos :: !(!Int,!Int), r :: !Int, frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real, draggable :: !Bool }

derive class iTask ModelImage, ModelLine, ModelRect, ModelCircle

::  ClientSt = Initialize | Running

:: MousePos = MouseDown | MouseUp

:: MouseSt = E.a:
  { mPos :: MousePos
  , dragOffsetX :: Int
  , dragOffsetY :: Int
  , dragElem :: Maybe (JSObj a)
  }

defMouseSt = { mPos = MouseUp
             , dragOffsetX = 0
             , dragOffsetY = 0
             , dragElem    = Nothing
             }

imageEditlet :: MR -> Editlet MR Int
imageEditlet mr=:(MR mrs i)	= Editlet mr server client
where
	server					= {EditletServerDef | genUI    = genUI
							                    , defVal   = gDefault{|*|}
							                    , genDiff  = genServerDiff
							                    , appDiff  = \i` (MR mrs _) -> MR mrs i`
							  }
	client					= {EditletClientDef | updateUI = updUI mrs
							                    , defVal   = ((Initialize, defMouseSt,i),i)
							                    , genDiff  = genClientDiff
							                    , appDiff  = \i` ((cst,mouse,_),i) -> ((cst,mouse,i`),i)
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
	
	updUI mrs cid mi ((Initialize, mouse,i`),i) env
	# (svg,env)				= getDomElement (main_svg_id cid) env
	# env					= foldl (add_image svg cid) env mrs
	= updUI mrs cid mi ((Running, mouse,i`),i) env
	updUI _ cid mi ((running,mouse,_),i) env
	= (((running,mouse,i`),i`),env)
	where
		i`					= fromMaybe i mi
	
	genClientDiff ((_,_,i),_) ((_,_,i`),_)
		= diffUI i i`
	
	diffUI i i`
	| i <> i`				= Just i`
	| otherwise				= Nothing

add_image :: !(JSObj a) !ComponentId !*JSWorld !ModelImage -> *JSWorld
add_image svg cid env (Line i ml)
# (rect,env)		= (jsDocument .# "createElementNS" .$ [toJSArg "http://www.w3.org/2000/svg", toJSArg "line"]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "id",           toJSArg (elt_id cid i)]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "x1",           toJSArg (toString (fst ml.ModelLine.pos)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "y1",           toJSArg (toString (snd ml.ModelLine.pos)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "x2",           toJSArg (toString (fst ml.ModelLine.end)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "y2",           toJSArg (toString (snd ml.ModelLine.end)+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke-width", toJSArg (toString ml.ModelLine.linew+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke",       toJSArg ml.ModelLine.edge]) env
# (_,env)			= (rect .# "addEventListener" .$ [toJSArg "click",toJSArg (createEditletEventHandler (select_elt i) cid)]) env
# (_,env)			= (svg  .# "appendChild"  .$ [toJSArg rect]) env
= env
add_image svg cid env (Rect i mr)
# (rect,env)		= (jsDocument .# "createElementNS" .$ [toJSArg "http://www.w3.org/2000/svg", toJSArg "rect"]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "id",           toJSArg (elt_id cid i)]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "width",        toJSArg (toString (fst mr.ModelRect.size)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "height",       toJSArg (toString (snd mr.ModelRect.size)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "x",            toJSArg (toString (fst mr.ModelRect.pos)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "y",            toJSArg (toString (snd mr.ModelRect.pos)+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke-width", toJSArg (toString mr.ModelRect.framew+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke",       toJSArg mr.ModelRect.frame]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill",         toJSArg mr.ModelRect.fill]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill-opacity", toJSArg (toString mr.ModelRect.opacity)]) env
# (_,env)			= (rect .# "addEventListener" .$ [toJSArg "click",toJSArg (createEditletEventHandler (select_elt i) cid)]) env
# (_,env)			= (svg  .# "appendChild"  .$ [toJSArg rect]) env
= env
add_image svg cid env (Circle i mc)
# (rect,env)		= (jsDocument .# "createElementNS" .$ [toJSArg "http://www.w3.org/2000/svg", toJSArg "circle"]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "id",           toJSArg (elt_id cid i)]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "cx",           toJSArg (toString (fst mc.ModelCircle.pos)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "cy",           toJSArg (toString (snd mc.ModelCircle.pos)+++"px")]) env
# (_,env)           = (rect .# "setAttribute" .$ [toJSArg "r",            toJSArg (toString mc.ModelCircle.r+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke-width", toJSArg (toString mc.ModelCircle.framew+++"px")]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "stroke",       toJSArg mc.ModelCircle.frame]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill",         toJSArg mc.ModelCircle.fill]) env
# (_,env)			= (rect .# "setAttribute" .$ [toJSArg "fill-opacity", toJSArg (toString mc.ModelCircle.opacity)]) env
# (_,env)			= (rect .# "addEventListener" .$ [toJSArg "click",toJSArg (createEditletEventHandler (select_elt i) cid)]) env
# env			    = if mc.draggable (mkDraggable rect env) env
# (_,env)			= (svg  .# "appendChild"  .$ [toJSArg rect]) env
= env
  where
    mkDraggable rect env
      # (_, env) = (rect .# "addEventListener" .$ [toJSArg "mousedown",toJSArg (createEditletEventHandler (mousedragdown_elt i) cid)]) env
      # (_, env) = (rect .# "addEventListener" .$ [toJSArg "mouseup",toJSArg (createEditletEventHandler (mousedragup_elt i) cid)]) env
      # (_, env) = (rect .# "addEventListener" .$ [toJSArg "mousemove",toJSArg (createEditletEventHandler (mousedragmove_elt i) cid)]) env
      = env

mousedragmove_elt :: m ComponentId {JSObj JSEvent} ((ClientSt,MouseSt,m),m) *JSWorld -> *(!(!(!ClientSt, MouseSt, !m),!m), !*JSWorld)
mousedragmove_elt i` cid es ((cst,mst=:{mPos=MouseDown,dragOffsetX,dragOffsetY, dragElem=Just delem},_),i) env
  # evt        = es.[0]
  # (de, env)  = getDomElement (main_svg_id cid) env
  # (p, env)   = (de .# "createSVGPoint" .$ Void) env
  # (eCX, env) = .? (evt .# "clientX") env
  # (eCY, env) = .? (evt .# "clientY") env
  # env        = (p .# "x" .= eCX) env
  # env        = (p .# "y" .= eCY) env
  # (m, env)   = (de .# "getScreenCTM" .$ Void) env
  # (inv, env) = (m .# "inverse" .$ Void) env
  # (p, env)   = (p .# "matrixTransform" .$ toJSArg inv) env
  # (px, env)  = .? (p .# "x") env
  # (py, env)  = .? (p .# "y") env
  # (px, py)   = (jsValToInt px - dragOffsetX, jsValToInt py - dragOffsetY)
  # env        = if (jsIsNull delem) env (f px py env)
  = (((cst, mst,i`),i`),env)
  where
    f px py env
      # (_, env) = (delem .# "setAttribute" .$ ("dragx", px)) env
      # (_, env) = (delem .# "setAttribute" .$ ("dragy", py)) env
      # (_, env) = (delem .# "setAttribute" .$ ("transform", "translate(" +++ toString px +++ "," +++ toString py +++ ")")) env
      = env
mousedragmove_elt i` cid es st env = (st, env)

mousedragdown_elt :: m ComponentId {JSObj JSEvent} ((ClientSt,MouseSt,m),m) *JSWorld -> *(!(!(!ClientSt, MouseSt, !m),!m), !*JSWorld)
mousedragdown_elt i` cid es ((cst,mst,_),i) env
# evt = es.[0]
# (target, env) = .? (evt .# "currentTarget") env
# mst = {mst & dragElem = Just target}
| jsIsNull target = (((cst, mst,i`),i`),env)
| otherwise
    # (de, env)    = getDomElement (main_svg_id cid) env
    # (p, env)     = (de .# "createSVGPoint" .$ Void) env
    # (eCX, env)   = .? (evt .# "clientX") env
    # (eCY, env)   = .? (evt .# "clientY") env
    # env          = (p .# "x" .= eCX) env
    # env          = (p .# "y" .= eCY) env
    # (m, env)     = (de .# "getScreenCTM" .$ Void) env
    # (inv, env)   = (m .# "inverse" .$ Void) env
    # (p, env)     = (p .# "matrixTransform" .$ toJSArg inv) env
    # (dragX, env) = (target .# "getAttribute" .$ toJSArg "dragx") env
    # (dragY, env) = (target .# "getAttribute" .$ toJSArg "dragy") env
    # (px, env)    = .? (p .# "x") env
    # (py, env)    = .? (p .# "y") env
    # (dragX, env) = if (jsIsNull dragX)
                       (toJSVal 0, env)
                       (("parseInt" .$ toJSArg dragX) env)
    # (dragY, env) = if (jsIsNull dragY)
                       (toJSVal 0, env)
                       (("parseInt" .$ toJSArg dragY) env)
    # (px, py, dragX, dragY) = (jsValToInt px, jsValToInt py, jsValToInt dragX, jsValToInt dragY)
    = (((cst,{mst & mPos = MouseDown, dragOffsetX = px - dragX, dragOffsetY = py - dragY},i`),i`),env)

mousedragup_elt :: m ComponentId {JSObj JSEvent} ((ClientSt,MouseSt,m),m) *JSWorld -> *(!(!(!ClientSt, MouseSt, !m),!m), !*JSWorld)
mousedragup_elt i` cid es ((cst,_,_),i) env
= (((cst, defMouseSt, i`),i`),env)

select_elt :: m ComponentId {JSObj JSEvent} ((ClientSt,m),m) *JSWorld -> *(!(!(!ClientSt,!m),!m), !*JSWorld)
select_elt i` cid _ ((cst,_),i) env
= (((cst,i`),i`),env)

main_id :: ComponentId -> ComponentId
main_id cid = cid

main_svg_id :: ComponentId -> ComponentId
main_svg_id cid = cid +++ "-svg"

elt_id :: ComponentId Int -> ComponentId
elt_id cid nr = main_svg_id cid +++ "-" +++ toString nr

boundingbox :: [ModelImage] -> (Int,Int,Int,Int)
boundingbox mrs
	= foldr boundbox (zero,zero,zero,zero) mrs
where
	boundbox (Line   _ {ModelLine | pos=(x,y),end=(x2,y2)}) (minx,miny,maxx,maxy) = (minList [minx,x,x2], minList [miny,y,y2], maxList [maxx,x,x2], maxList [maxy,y,y2])
	boundbox (Rect   _ {ModelRect | pos=(x,y),size=(w,h)})  (minx,miny,maxx,maxy) = (min minx x, min miny y, max maxx (x+w), max maxy (y+h))
	boundbox (Circle _ {ModelCircle | pos=(x,y),r})         (minx,miny,maxx,maxy) = (min minx (x-r), min miny (y-r), max maxx (x+r), max maxy (y+r))

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
