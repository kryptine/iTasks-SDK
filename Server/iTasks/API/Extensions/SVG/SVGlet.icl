implementation module iTasks.API.Extensions.SVG.SVGlet

import qualified Data.Map as DM
import Graphics.Scalable
import Graphics.Scalable.Internal
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor
import iTasks.UI.JS.Interface
from StdOrdList import minList, maxList
import StdOverloaded
import StdArray
import StdMisc
import Data.Array
import Data.List
import Data.Func
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Set as DS
from StdFunc import `bind`, flip
import Text
from Data.IntMap.Strict import :: IntMap, instance Functor IntMap
import qualified Data.IntMap.Strict as DIS
import Data.Matrix
import iTasks.API.Extensions.Platform

derive class iTask Image, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr


CLICK_DELAY :== 225
svgns =: "http://www.w3.org/2000/svg"

//Predefined object methods
(`addEventListener`)       obj args :== obj .# "addEventListener"       .$ args
(`setAttribute`)           obj args :== obj .# "setAttribute"           .$ args
(`setAttributeNS`)         obj args :== obj .# "setAttributeNS"         .$ args
(`createElementNS`)        obj args :== obj .# "createElementNS"        .$ args
(`appendChild`)            obj args :== obj .# "appendChild"            .$ args
(`removeChild`)            obj args :== obj .# "removeChild"            .$ args
(`getComputedTextLength`)  obj args :== obj .# "getComputedTextLength"  .$ args
(`createSVGPoint`)         obj args :== obj .# "createSVGPoint"         .$ args
(`getScreenCTM`)           obj args :== obj .# "getScreenCTM"           .$ args
(`inverse`)                obj args :== obj .# "inverse"                .$ args
(`matrixTransform`)        obj args :== obj .# "matrixTransform"        .$ args

:: *GenSVGStVal s =
  { uniqueIdCounter :: !Int
  , genStates       :: !*SpanEnvs
  }

:: DropTarget = DropTarget
:: MousePos = MouseUp | MouseDown

:: SVGDragState v = 
  { svgMousePos     :: !MousePos
  , svgDropCallback :: !Maybe ((Maybe (Set ImageTag)) Real Real v -> v)
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe (JSObj DropTarget)
  }

derive class iTask Set, DropTarget, MousePos, ImageTag

fromSVGLet :: (SVGLet s v) -> Editor s | iTask s 
fromSVGLet svglet = fromEditlet (svgRenderer svglet)

svgRenderer :: (SVGLet s v) -> Editlet s | iTask s
svgRenderer svglet=:{initView,renderImage,updView,updModel}
  = { genUI   = genUI
	, initUI  = initUI
    , onEdit  = onEdit
    , onRefresh = onRefresh
    }
  where
	genUI dp val world
		# attr = 'DM'.unions [sizeAttr FlexSize FlexSize, valueAttr (toJSON val)]
		= (Ok (uia UIComponent attr,newFieldMask), world)

	initUI me world
		//Set attributes
        # world = (me .# "clickCount" .= (toJSVal 0)) world
  		# world = jsPutCleanVal "dragState" initDragState me world
		//Set methods	
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
		# world = (me .# "onAttributeChange" .= jsOnAttributeChange) world
		# (jsInitDOMEl,world) = jsWrapFun (initDOMEl me) world
		# world = (me .# "initDOMEl" .= jsInitDOMEl) world
		= world

	initDragState = {SVGDragState|svgMousePos=MouseUp,svgDropCallback=Nothing,svgTrueCoordsX=0.0,svgTrueCoordsY=0.0
                                 ,svgGrabPointX=0.0,svgGrabPointY=0.0,svgDragTarget=Nothing}

	initDOMEl me args world
		# (value,world) = .? (me .# "value") world
		# (json,world) = jsValToJSONNode value world
		= case fromJSON json of
			Nothing = (jsNull,world)
			Just s 	= (jsNull,onNewState me svglet s world)

	onAttributeChange me args world
		| jsArgToString (args !! 0) == "stateChange"
			# (json,world)  = jsValToJSONNode (toJSVal (args !! 1)) world
			= case fromJSON json of
				Nothing = (jsNull,world)
				Just s  = (jsNull,onNewState me svglet s world)
		| otherwise
			= (jsNull,jsTrace "Unknown attribute change" world)

  	onRefresh _ new old mask vst 
		= (Ok (if (old === new) NoChange (ChangeUI [SetAttribute "stateChange" (toJSON new)] []),mask),new,vst)

  	onEdit _ _ st m ust = (Ok (NoChange,m),st,ust)

onNewState :: !(JSVal a) !(SVGLet s v) !s !*JSWorld -> *JSWorld | JSONEncode{|*|} s
onNewState me svglet=:{initView,renderImage} s world
	#! cid                  = "FIXME: SOME UNIQUE STRING"
	#! v                    = initView s
	#! world 				= jsPutCleanVal "view" v me world  //Store the view value on the component
	#! world 				= jsPutCleanVal "model" s me world  //Store the model value on the component
  	#! image                = renderImage s v [(ImageTagUser no cid, ImageTagUser no cid) \\ no <- [0..]]
	// Determine the fonts used in the image, measure text sizes and adjust the image using the known sizes
  	#! fontMap              = gatherFonts image
    #! (realFontMap, world) = if ('DM'.null fontMap) ('DM'.newMap, world) (calcTextLengths fontMap world)
    #! (image, spanEnvs)    = imageFromState image realFontMap
    #! fixVal               = fixEnvs {FixSpansSt | fixSpansDidChange = False, fixSpansSpanEnvs = spanEnvs}
	// Create the new SVG content from the image
    #! (syn, clval)         = genSVG image { uniqueIdCounter = 0, genStates = fixVal.fixSpansSpanEnvs }
    #! (imXSp, imYSp)       = syn.genSVGSyn_imageSpanReal
    #! (imXSp, imYSp)       = (toString (to2dec imXSp), toString (to2dec imYSp))
    #! svgStr               = browserFriendlySVGEltToString (SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns]
                                             [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp]
                                             syn.genSVGSyn_svgElts)
    #! svgStr               = replaceSubString editletId cid svgStr
	// Update the DOM element with the new SVG content
    #! (parser, world)      = new "DOMParser" () world
    #! (doc, world)         = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
    #! (newSVG, world)      = .? (doc .# "firstChild") world
    #! (domEl, world)       = .? (me .# "domEl") world
  	#! (currSVG, world)     = .? (domEl .# "firstChild") world
  	#! (_, world)           = if (jsIsNull currSVG)
                              ((domEl `appendChild` newSVG) world)
                              ((domEl .# "replaceChild" .$ (newSVG, currSVG)) world)
	// Register javascript event handlers for all event handlers in the image
    #! world                = registerSVGEvents me svglet cid newSVG syn.genSVGSyn_events world
    #! world                = registerDraggables me svglet cid newSVG syn.genSVGSyn_draggable syn.genSVGSyn_idMap world
 	= world

imageFromState :: !(Image v) !(Map FontDef (Map String Real)) -> *(!Image v, !*SpanEnvs)
imageFromState img env
  #! spanEnvs  = { spanEnvImageTagPostTrans  = 'DIS'.newMap
                 , spanEnvImageSpanPostTrans = 'DIS'.newMap
                 , spanEnvGridTag            = 'DIS'.newMap
                 , spanEnvGridSpan           = 'DIS'.newMap
                 , spanEnvFonts              = env
                 }
  #! (img, st) = desugarAndTag img { desugarAndTagCounter  = 0
                                   , desugarAndTagSpanEnvs = spanEnvs}
  = (img, st.desugarAndTagSpanEnvs)

registerSVGEvents :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !(Map String (ImageAttr v)) !*JSWorld -> *JSWorld | JSONEncode{|*|} s
registerSVGEvents me svglet cid svg onclicks world
  = 'DM'.foldrWithKey (registerEvent me svglet cid svg) world onclicks
  where
  registerEvent :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !String !(ImageAttr v) !*JSWorld -> *JSWorld | JSONEncode{|*|} s
  registerEvent me svglet cid svg elemId (ImageOnClickAttr {local,onclick}) world
    = registerNClick me svglet cid svg elemId onclick local world
  registerEvent me svglet cid svg elemId (ImageOnMouseDownAttr {local, onmousedown}) world
    = actuallyRegister me svglet cid svg elemId "mousedown" onmousedown local world
  registerEvent me svglet cid svg elemId (ImageOnMouseUpAttr   {local, onmouseup}) world
    = actuallyRegister me svglet cid svg elemId "mouseup" onmouseup local world
  registerEvent me svglet cid svg elemId (ImageOnMouseOverAttr {local, onmouseover}) world
    = actuallyRegister me svglet cid svg elemId "mouseover" onmouseover local world
  registerEvent me svglet cid svg elemId (ImageOnMouseMoveAttr {local, onmousemove}) world
    = actuallyRegister me svglet cid svg elemId "mousemove" onmousemove local world
  registerEvent me svglet cid svg elemId (ImageOnMouseOutAttr  {local, onmouseout}) world
    = actuallyRegister me svglet cid svg elemId "mouseout"  onmouseout  local world

registerNClick :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !String !(Int v -> v) !Bool *JSWorld -> *JSWorld | JSONEncode{|*|} s
registerNClick me svglet cid svg elemId sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! (cb, world)   = jsWrapFun (mkNClickCB me svglet svg elemId sttf local cid) world
  #! (_, world)    = (elem `addEventListener` ("click", cb, False)) world
  = world

actuallyRegister :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !String !String !(v -> v) !Bool! *JSWorld -> *JSWorld | JSONEncode{|*|} s
actuallyRegister me svglet cid svg elemId evt sttf local world
  #! elemId        = replaceSubString editletId cid elemId
  #! (elem, world) = (svg .# "getElementById" .$ elemId) world
  #! (cb,world)    = jsWrapFun (doImageEvent me svglet svg elemId sttf local) world
  #! (_, world)    = (elem `addEventListener` (evt, cb, True)) world
  = world

mkNClickCB :: !(JSVal a) !(SVGLet s v) !(JSObj svg) !String !(Int v -> v) !Bool !String ![JSArg] !*JSWorld-> *(JSVal (), !*JSWorld) | JSONEncode{|*|} s
mkNClickCB me svglet svg elemId sttf local cid args world
  #! world            = case args of [a:_] = snd (((toJSVal a) .# "stopPropagation" .$ ()) world) ; _ = world
  //If another click already registered a timeout, clear that timeout
  #! (to,world)      = .? (me .# "clickTimeOut") world
  #! world           = if (jsIsUndefined to || jsIsNull to) world (snd (("clearTimeout" .$ to) world))
  //Register a callback for the click after a small timeout
  #! (cb,world)      = jsWrapFun (doNClickEvent me svglet svg elemId sttf local) world
  #! (to,world)  	 =  ("setTimeout" .$ (cb, CLICK_DELAY)) world
  #! world           = (me .# "clickTimeOut" .= to) world
  //Increase click counter, so we can determine how many times the element was clicked when the timeout passes
  #! (nc,world)      = .? (me .# "clickCount") world
  #! world           = (me .# "clickCount" .= (toJSVal (jsValToInt nc + 1))) world
  = (jsNull,world)

doNClickEvent :: !(JSVal a) !(SVGLet s v) !(JSObj svg) !String !(Int v -> v) !Bool ![JSArg] !*JSWorld-> *(JSVal (), !*JSWorld) | JSONEncode{|*|} s
doNClickEvent me svglet svg elemId sttf local args world
  // Get click count
  #! (nc,world)      = .? (me .# "clickCount") world
  ///Reset click count
  #! world           = (me .# "clickCount" .= (toJSVal 0)) world
  #! nc              = jsValToInt nc
  = doImageEvent me svglet svg elemId (sttf nc) local args world

doImageEvent :: !(JSVal a) !(SVGLet s v) !(JSObj svg) !String !(v -> v) !Bool [JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
doImageEvent me svglet svg elemId sttf local _ world
  // Get model & view value 
  #! (view,world) 	 = jsGetCleanVal "view" me world
  #! (model,world) 	 = jsGetCleanVal "model" me world
  // Update the view & the model
  #! view  			 = sttf view
  #! model           = svglet.SVGLet.updModel model view
  #! world           = jsPutCleanVal "view" view me world
  #! world           = jsPutCleanVal "model" model me world
  // If not local, fire an itasks edit event 
  | local
  	//Don't trigger an event, just re-render
  	= (jsNull,onNewState me svglet model world)
  //Send edit event
  #! (json,world)     = (jsWindow .# "JSON.parse" .$ (toString (toJSON model))) world //TODO: Should not really print+parse here
  #! (taskId,world)   = .? (me .# "taskId") world
  #! (editorId,world) = .? (me .# "editorId") world
  #! (_,world)  	  = (me .# "doEditEvent" .$ (taskId,editorId,json)) world
  //Re-render
  = (jsNull,onNewState me svglet model world)

registerDraggables :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !(Map String (ImageAttr v)) !(Map String (Set ImageTag)) !*JSWorld -> *JSWorld 
registerDraggables me svglet cid svg draggables idMap world
  #! (domEl, world)       = .? (me .# "domEl") world
  #! (svgRoot, world)     = .? (domEl .# "firstChild") world
  #! idMap                = 'DM'.foldrWithKey (\k v m -> 'DM'.put (replaceSubString editletId cid k) v m) 'DM'.newMap idMap
  //All draggable elements share a common mousemove and mouseup event
  #! (cbMove,world)       = jsWrapFun (doMouseDragMove me svglet cid svgRoot) world
  #! (cbUp,world)         = jsWrapFun (doMouseDragUp me svglet cid svgRoot idMap) world
  #! (_, world)            = (svgRoot `addEventListener` ("mousemove", cbMove, True)) world
  #! (_, world)            = (svgRoot `addEventListener` ("mouseup",   cbUp,   True)) world
  //Register individual mousedown events
  = 'DM'.foldrWithKey (registerDraggable me svglet cid svg) world draggables

registerDraggable :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !String !(ImageAttr v) !*JSWorld -> *JSWorld 
registerDraggable me svglet cid svg elemId (ImageDraggableAttr {draggable = Nothing}) world = world
registerDraggable me svglet cid svg elemId (ImageDraggableAttr {draggable = Just sttf}) world
  #! elemId          = replaceSubString editletId cid elemId
  #! (elem, world)   = (svg .# "getElementById" .$ elemId) world
  #! (cbDown, world) = jsWrapFun (doMouseDragDown me svglet cid svg sttf elemId elem) world
  #! (_, world)      = (elem `addEventListener` ("mousedown", cbDown, True)) world
  = world

doMouseDragDown :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) ((Maybe (Set ImageTag)) Real Real v -> v) !String !(JSObj o) [JSArg] !*JSWorld
                   -> *(!JSVal (), !*JSWorld) 
doMouseDragDown me svglet cid svgRoot sttf elemId elem args world
  #! (ds,world)             = jsGetCleanVal "dragState" me world
  #! (targetElement, world) = (svgRoot .# "getElementById" .$ elemId) world
  #! (_, world)             = (targetElement .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (boundingRect, world)  = (targetElement .# "getBoundingClientRect" .$ ()) world
  #! (left, world)          = .? (boundingRect .# "left") world
  #! (top, world)           = .? (boundingRect .# "top") world
  #! (p, world)             = (svgRoot `createSVGPoint` ()) world
  #! world                  = (p .# "x" .= left) world
  #! world                  = (p .# "y" .= top) world
  #! (m, world)             = (svgRoot `getScreenCTM` ()) world
  #! (inv, world)           = (m `inverse` ()) world
  #! (p, world)             = (p `matrixTransform` inv) world
  #! (px, world)            = .? (p .# "x") world
  #! (py, world)            = .? (p .# "y") world
  #! (e, f)                 = (jsValToReal px, jsValToReal py)
  #! ds = {SVGDragState|ds & svgDropCallback = Just sttf, svgMousePos = MouseDown, svgDragTarget = Just targetElement
          ,svgGrabPointX = ds.SVGDragState.svgTrueCoordsX - e, svgGrabPointY = ds.SVGDragState.svgTrueCoordsY - f}
  #! world                  = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragMove :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) [JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragMove me svglet cid svgRoot args world
  #! (ds,world)  = jsGetCleanVal "dragState" me world
  #! evt         = toJSVal (args !! 0)
  #! (newTrueCoordsX, newTrueCoordsY, world) = getNewTrueCoords me evt world
  | ds.SVGDragState.svgMousePos =!= MouseDown || ds.SVGDragState.svgDragTarget =: Nothing
 	#! ds = {SVGDragState|ds & svgTrueCoordsX = newTrueCoordsX, svgTrueCoordsY = newTrueCoordsY}
    #! world  = jsPutCleanVal "dragState" ds me world
    = (jsNull,world)
  #! dragTarget           = fromJust ds.SVGDragState.svgDragTarget
  #! (domEl, world)       = .? (me .# "domEl") world
  #! (svgRoot, world)     = .? (domEl .# "firstChild") world
  // Append the dragTarget to the root of the SVG element for two reasons:
  //   1. To allow it to be dragged over all other elements
  //   2. To not be bothered by the offsets of one or more groups it might initially be in
  #! (_, world) = (svgRoot `appendChild` dragTarget) world
  #! newX       = newTrueCoordsX - ds.SVGDragState.svgGrabPointX
  #! newY       = newTrueCoordsY - ds.SVGDragState.svgGrabPointY
  #! (_, world) = (dragTarget `setAttribute` ("transform", "translate(" +++ toString newX +++ "," +++ toString newY +++ ")")) world
  #! ds = {SVGDragState|ds & svgTrueCoordsX = newTrueCoordsX, svgTrueCoordsY = newTrueCoordsY}
  #! world  = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragUp :: !(JSVal a) !(SVGLet s v) !String !(JSObj svg) !(Map String (Set ImageTag)) [JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragUp me svglet cid svgRoot idMap args world
  #! evt                   = toJSVal (args !! 0)
  #! (ds,world)  = jsGetCleanVal "dragState" me world
  | ds.SVGDragState.svgDragTarget =: Nothing
    #! ds     = {SVGDragState|ds & svgMousePos = MouseUp, svgDragTarget = Nothing}
    #! world  = jsPutCleanVal "dragState" ds me world
  	= (jsNull,world)
  #! (evtTarget, world)    = .? (evt .# "target") world
  #! dragTarget            = fromJust ds.SVGDragState.svgDragTarget
  #! (_, world)            = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (parentId, world)     = firstIdentifiableParentId evtTarget world
  // Get model & view value 
  #! (view,world) 	 = jsGetCleanVal "view" me world
  #! (model,world) 	 = jsGetCleanVal "model" me world
  #! (view,model)    = case ds.SVGDragState.svgDropCallback of
                         Just sttf
                              # xdiff  = ds.SVGDragState.svgTrueCoordsX - ds.SVGDragState.svgGrabPointX
                              # ydiff  = ds.SVGDragState.svgTrueCoordsY - ds.SVGDragState.svgGrabPointY
                              # view`  = sttf ('DM'.get parentId idMap) xdiff ydiff view 
                              # model` = svglet.SVGLet.updModel model view
                              = (view,model)
                         Nothing
                              = (view,model)
  #! ds    = {SVGDragState|ds & svgMousePos = MouseUp, svgDragTarget = Nothing}
  #! world = jsPutCleanVal "view" view me world
  #! world = jsPutCleanVal "model" model me world
  #! world = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

firstIdentifiableParentId :: !(JSObj a) *JSWorld -> *(String, *JSWorld)
firstIdentifiableParentId elem world
  #! (idval, world) = .? (elem .# "id") world
  | jsIsNull idval
      #! (parent, world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  #! idval = jsValToString idval
  | idval == ""
      #! (parent, world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  | otherwise = (idval, world)

getNewTrueCoords :: !(JSVal a) !(JSObj JSEvent) !*JSWorld -> *(!Real, !Real, !*JSWorld)
getNewTrueCoords me evt world
  #! (domEl, world)        = .? (me .# "domEl") world
  #! (svgRoot, world)      = .? (domEl .# "firstChild") world
  #! (newScale, world)     = .? (svgRoot .# "currentScale") world
  #! newScale              = jsValToReal newScale
  #! (translation, world)  = .? (svgRoot .# "currentTranslate") world
  #! (translationX, world) = .? (translation .# "x") world
  #! (translationY, world) = .? (translation .# "y") world
  #! (translationX, translationY) = (jsValToReal translationX, jsValToReal translationY)
  #! (clientX, world)      = .? (evt .# "clientX") world
  #! (clientY, world)      = .? (evt .# "clientY") world
  #! (clientX, clientY)    = (jsValToReal clientX, jsValToReal clientY)
  #! newTrueCoordsX        = (clientX - translationX) / newScale
  #! newTrueCoordsY        = (clientY - translationY) / newScale
  = (newTrueCoordsX, newTrueCoordsY, world)

calcTextLengths :: !(Map FontDef (Set String)) !*JSWorld -> *(!Map FontDef (Map String Real), !*JSWorld)
calcTextLengths fontdefs world
  #! (svg, world)  = (jsDocument `createElementNS` (svgns, "svg")) world
  #! (body, world) = .? (jsDocument .# "body") world
  #! (_, world)    = (body `appendChild` svg) world
  #! (elem, world) = (jsDocument `createElementNS` (svgns, "text")) world
  #! (_, world)    = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
  #! (_, world)    = (svg `appendChild` elem) world
  #! (res, world)  = 'DM'.foldrWithKey (f elem) ('DM'.newMap, world) fontdefs
  #! (_, world)    = (svg `removeChild` elem) world
  #! (_, world)    = (body `removeChild` svg) world
  = (res, world)
  where
  f :: !(JSVal (JSObject a)) !FontDef !(Set String) !*(!Map FontDef (Map String Real), !*JSWorld) -> *(!Map FontDef (Map String Real), !*JSWorld)
  f elem fontdef strs (acc, world)
    #! fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                     , ("font-size",    toString fontdef.fontysize)
                     , ("font-stretch", fontdef.fontstretch)
                     , ("font-style",   fontdef.fontstyle)
                     , ("font-variant", fontdef.fontvariant)
                     , ("font-weight",  fontdef.fontweight)
                     , ("alignment-baseline", "auto")
                     , ("dominant-baseline", "auto")
                     , ("x", "-10000")
                     , ("y", "-10000")
                     ]
    #! world       = strictFoldl (\world args -> snd ((elem `setAttribute` args) world)) world fontAttrs
    #! (ws, world) = 'DS'.fold (g elem) ('DM'.newMap, world) strs
    = ('DM'.put fontdef ws acc, world)
  g :: !(JSVal (JSObject a)) !String !*(!Map String Real, !*JSWorld) -> *(!Map String Real, !*JSWorld)
  g elem str (acc, world)
    #! world        = (elem .# "textContent" .= str) world
    #! (ctl, world) = (elem `getComputedTextLength` ()) world
    = ('DM'.put str (jsValToReal ctl) acc, world)

:: *FixSpansSt =
  { fixSpansDidChange :: !Bool
  , fixSpansSpanEnvs  :: !*SpanEnvs
  }

:: DesugarAndTagSt a :== State *DesugarAndTagStVal a

:: *DesugarAndTagStVal =
  { desugarAndTagCounter  :: !Int
  , desugarAndTagSpanEnvs :: !*SpanEnvs
  }

class nextNo a :: !*a -> *(!Int, !*a)

instance nextNo (GenSVGStVal s) where
  nextNo st = (st.uniqueIdCounter, {st & uniqueIdCounter = st.uniqueIdCounter + 1})

instance nextNo DesugarAndTagStVal where
  nextNo st = (st.desugarAndTagCounter, {st & desugarAndTagCounter = st.desugarAndTagCounter + 1})

:: State s a :== s -> *(!a, !s)

:: ErrorMessage :== String

:: DesugarAndTagSyn s =
  { desugarAndTagSyn_ImageContent        :: !ImageContent s
  , desugarAndTagSyn_TotalSpan_PreTrans  :: !ImageSpan
  , desugarAndTagSyn_TotalSpan_PostTrans :: !ImageSpan
  , desugarAndTagSyn_OffsetCorrection    :: !ImageOffset
  }

cacheImageSpanPostTrans :: !Int !(Set ImageTag) !ImageSpan !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheImageSpanPostTrans n imTas sp st
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvImageSpanPostTrans = 'DIS'.put n sp spanEnvs.spanEnvImageSpanPostTrans}
  #! env      = 'DS'.fold (putImgTag n) spanEnvs.spanEnvImageTagPostTrans imTas
  #! spanEnvs = {spanEnvs & spanEnvImageTagPostTrans = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}

cacheGridSpans :: !Int !(Set ImageTag) ![Span] ![Span] !*DesugarAndTagStVal -> *DesugarAndTagStVal
cacheGridSpans n imTas xsps ysps st
  #! xsps`    = 'DIS'.fromList (strictTRZip2 [0..] xsps)
  #! ysps`    = 'DIS'.fromList (strictTRZip2 [0..] ysps)
  #! spanEnvs = st.desugarAndTagSpanEnvs
  #! spanEnvs = {spanEnvs & spanEnvGridSpan = 'DIS'.put n (xsps`, ysps`) spanEnvs.spanEnvGridSpan}
  #! env      = 'DS'.fold (putImgTag n) spanEnvs.spanEnvGridTag imTas
  #! spanEnvs = {spanEnvs & spanEnvGridTag = env}
  = {st & desugarAndTagSpanEnvs = spanEnvs}

putImgTag :: !Int !ImageTag !(IntMap Int) -> IntMap Int
putImgTag n t env = 'DIS'.put (numTag t) n env

point2Vec :: !(!Span, !Span) -> Vector Span
point2Vec (x, y) = {x, y, px 1.0}

appTF :: !(Matrix Span) !(!Span, !Span) -> (!Span, !Span)
appTF m p
  #! m = mulMatrixVec m (point2Vec p)
  = (m.[0].[0], m.[1].[0])

translateTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
translateTF sx sy p
  = appTF { {px 1.0, px 0.0, sx}
          , {px 0.0, px 1.0, sy}
          , {px 0.0, px 0.0, px 1.0}
          } p

scaleTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
scaleTF sx sy p
  = appTF { {sx,     px 0.0, px 0.0}
          , {px 0.0, sy,     px 0.0}
          , {px 0.0, px 0.0, px 1.0}
          } p

rotateTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
rotateTF a p
  #! a = toRad a
  = appTF { {px (cos a), px (0.0 - sin a), px 0.0}
          , {px (sin a), px (cos a),       px 0.0}
          , {px 0.0,     px 0.0,           px 1.0}
          } p

skewXTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewXTF a p
  = appTF { {px 1.0, px (tan (toRad a)), px 0.0}
          , {px 0.0, px 1.0,             px 0.0}
          , {px 0.0, px 0.0,             px 1.0}
          } p

skewYTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewYTF a p
  = appTF { {px 1.0,             px 0.0, px 0.0}
          , {px (tan (toRad a)), px 1.0, px 0.0}
          , {px 0.0,             px 0.0, px 1.0}
          } p

revFstsSnds :: ![(!a, !b)] -> (![a], ![b])
revFstsSnds xs = strictFoldl (\(xs, ys) (x, y) -> ([x:xs], [y:ys])) ([], []) xs

applyTransforms :: ![ImageTransform] !ImageSpan -> (!ImageSpan, !ImageOffset)
applyTransforms ts (xsp, ysp)
  #! origPoints     = [(zero, zero), (xsp, zero), (zero, ysp), (xsp, ysp)]
  #! newPoints      = strictFoldr f origPoints ts
  #! (allXs, allYs) = revFstsSnds newPoints
  #! minX           = minSpan allXs
  #! maxX           = maxSpan allXs
  #! minY           = minSpan allYs
  #! maxY           = maxSpan allYs
  = ((maxX - minX, maxY - minY), (zero - minX, zero - minY))
  where
  f :: !ImageTransform ![(!Span, !Span)] -> [(!Span, !Span)]
  f (RotateImage th) coords
    #! (allXs, allYs) = revFstsSnds coords
    #! minX           = minSpan allXs
    #! maxX           = maxSpan allXs
    #! minY           = minSpan allYs
    #! maxY           = maxSpan allYs
    #! cx             = (maxX - minX) /. 2.0
    #! cy             = (maxY - minY) /. 2.0
    #! translated     = strictTRMap (translateTF (zero - cx) (zero - cy)) coords
    #! rotated        = strictTRMap (rotateTF th) translated
    = strictTRMap (translateTF cx cy) rotated
  f (SkewXImage th)      coords = strictTRMap (skewXTF th) coords
  f (SkewYImage th)      coords = strictTRMap (skewYTF th) coords
  f (FitImage xsp` ysp`) coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          = strictTRMap (scaleTF (xsp` / (brX - tlX)) (ysp` / (brY - tlX))) coords
  f (FitXImage xsp`)     coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          #! factor  = xsp` / (brX - tlX)
          = strictTRMap (scaleTF factor factor) coords
  f (FitYImage ysp`)     coords
    = case coords of
        [(tlX, tlY), _, _, (brX, brY)]
          #! factor  = ysp` / (brY - tlY)
          = strictTRMap (scaleTF factor factor) coords
  f (ScaleImage xsp` ysp`) coords
    = strictTRMap (scaleTF (px xsp`) (px ysp`)) coords
  f (ScaleXImage xsp`)     coords
    = strictTRMap (scaleTF (px xsp`) (px 1.0)) coords
  f (ScaleYImage ysp`)     coords
    = strictTRMap (scaleTF (px 1.0) (px ysp`)) coords
  f _ coords = coords

gatherFonts :: !(Image s) -> Map FontDef (Set String)
gatherFonts {content, mask, attribs, transform}
  = gatherFontsUnions [ gatherFontsImageContent content
                      , gatherFontsMask mask
                      , gatherFontsAttribs attribs
                      , gatherFontsTransforms transform
                      ]
  where
  gatherFontsImageContent :: !(ImageContent m) -> Map FontDef (Set String)
  gatherFontsImageContent (Basic _ (l, r)) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
  gatherFontsImageContent (Line {lineSpan = (l, r), lineContent, markers}) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r, gatherFontsMarkers markers, gatherFontsLineContent lineContent]
    where
    gatherFontsMarkers :: !(Maybe (Markers m)) -> Map FontDef (Set String)
    gatherFontsMarkers (Just {markerStart, markerMid, markerEnd}) = gatherFontsUnions (strictTRMapRev gatherFonts (maybeToList markerStart ++ maybeToList markerMid ++ maybeToList markerEnd))
    gatherFontsMarkers _ = 'DM'.newMap
    gatherFontsLineContent :: !LineContent -> Map FontDef (Set String)
    gatherFontsLineContent (PolygonImage ios)  = gatherFontsUnions (gatherFontsPairs ios)
    gatherFontsLineContent (PolylineImage ios) = gatherFontsUnions (gatherFontsPairs ios)
    gatherFontsLineContent _                   = 'DM'.newMap
  gatherFontsImageContent (Composite {host, compose}) = gatherFontsUnions [gatherFontsCompose compose : strictTRMapRev gatherFonts (maybeToList host)]
    where
    gatherFontsCompose :: !(Compose m) -> Map FontDef (Set String)
    gatherFontsCompose (AsGrid    _ offss _ imgss) = gatherFontsUnions (strictTRMapRev gatherFonts (flattenTR imgss) ++ (gatherFontsPairs (flattenTR offss)))
    gatherFontsCompose (AsCollage   offs    imgs)  = gatherFontsUnions (strictTRMapRev gatherFonts imgs ++ gatherFontsPairs offs)
    gatherFontsCompose (AsOverlay   offs  _ imgs)  = gatherFontsUnions (strictTRMapRev gatherFonts imgs ++ gatherFontsPairs offs)
  gatherFontsMask :: !(Maybe (Image m)) -> Map FontDef (Set String)
  gatherFontsMask (Just img) = gatherFonts img
  gatherFontsMask _          = 'DM'.newMap
  gatherFontsAttribs :: !(Set (ImageAttr m )) -> Map FontDef (Set String)
  gatherFontsAttribs attribs = gatherFontsUnions (strictTRMapRev gatherFontsAttrib ('DS'.toList attribs))
    where
    gatherFontsAttrib :: !(ImageAttr m) -> Map FontDef (Set String)
    gatherFontsAttrib (ImageStrokeWidthAttr {strokewidth}) = gatherFontsSpan strokewidth
    gatherFontsAttrib (ImageXRadiusAttr     {xradius})     = gatherFontsSpan xradius
    gatherFontsAttrib (ImageYRadiusAttr     {yradius})     = gatherFontsSpan yradius
    gatherFontsAttrib _                                    = 'DM'.newMap
  gatherFontsTransforms :: ![ImageTransform] -> Map FontDef (Set String)
  gatherFontsTransforms transforms = gatherFontsUnions (strictTRMapRev gatherFontsTransform transforms)
    where
    gatherFontsTransform :: !ImageTransform -> Map FontDef (Set String)
    gatherFontsTransform (FitImage l r) = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
    gatherFontsTransform (FitXImage l)  = gatherFontsSpan l
    gatherFontsTransform (FitYImage l)  = gatherFontsSpan l
    gatherFontsTransform _              = 'DM'.newMap

gatherFontsPairs :: ![(!Span, !Span)] -> [Map FontDef (Set String)]
gatherFontsPairs pairs = strictFoldl f [] pairs
  where
  f :: ![Map FontDef (Set String)] !(!Span, !Span) -> [Map FontDef (Set String)]
  f acc (x, y) = [gatherFontsSpan x : gatherFontsSpan y : acc]

gatherFontsSpan :: !Span -> Map FontDef (Set String)
gatherFontsSpan (AddSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (SubSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (MulSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (DivSpan l r)                   = gatherFontsUnions [gatherFontsSpan l, gatherFontsSpan r]
gatherFontsSpan (AbsSpan x)                     = gatherFontsSpan x
gatherFontsSpan (MinSpan xs)                    = gatherFontsUnions (strictTRMapRev gatherFontsSpan xs)
gatherFontsSpan (MaxSpan xs)                    = gatherFontsUnions (strictTRMapRev gatherFontsSpan xs)
gatherFontsSpan (LookupSpan (TextXSpan fd str)) = 'DM'.singleton fd ('DS'.singleton str)
gatherFontsSpan _                               = 'DM'.newMap

gatherFontsUnions :: ![Map FontDef (Set String)] -> Map FontDef (Set String)
gatherFontsUnions m = 'DM'.unionsWith 'DS'.union m

desugarAndTagMaybeImage :: !(Maybe (Image s)) !*DesugarAndTagStVal -> (!Maybe (Image s), !*DesugarAndTagStVal)
desugarAndTagMaybeImage (Just img) st
  #! (img, st) = desugarAndTag img st
  = (Just img, st)
desugarAndTagMaybeImage n st = (n, st)

mkTotalSpanPostTrans :: !(Image s) -> (!Span, !Span)
mkTotalSpanPostTrans {uniqId}
  #! newTag = ImageTagSystem uniqId
  = (LookupSpan (ImageXSpan newTag), LookupSpan (ImageYSpan newTag)) 

desugarAndTag :: !(Image s) !*DesugarAndTagStVal -> *(!Image s, !*DesugarAndTagStVal)
desugarAndTag {content, mask, attribs, transform, tags} st
  #! (mask, st)      = desugarAndTagMaybeImage mask st
  #! (no, st)        = nextNo st
  #! newTag          = ImageTagSystem no
  #! (syn, st)       = desugarAndTagImageContent content transform tags st
  #! tags            = 'DS'.insert newTag tags
  #! st              = cacheImageSpanPostTrans no tags syn.desugarAndTagSyn_TotalSpan_PostTrans st
  #! img             = { Image
                       | content             = syn.desugarAndTagSyn_ImageContent
                       , mask                = mask
                       , attribs             = attribs
                       , transform           = transform
                       , tags                = tags
                       , uniqId              = no
                       , totalSpanPreTrans   = syn.desugarAndTagSyn_TotalSpan_PreTrans  // TODO Get rid of these fields in favor of cached spans
                       , transformCorrection = syn.desugarAndTagSyn_OffsetCorrection    // TODO Get rid of these fields in favor of cached spans
                       }
  = (img, st)
  where
  desugarAndTagImageContent :: !(ImageContent s) ![ImageTransform] !(Set ImageTag) !*DesugarAndTagStVal
                            -> *(!DesugarAndTagSyn s, !*DesugarAndTagStVal)
  desugarAndTagImageContent (Basic bi imSp) transform tags st
    #! (imSp`, imOff) = applyTransforms transform imSp
    = ({ desugarAndTagSyn_ImageContent        = Basic bi imSp
       , desugarAndTagSyn_TotalSpan_PreTrans  = imSp
       , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
       , desugarAndTagSyn_OffsetCorrection    = imOff
       }, st)
  desugarAndTagImageContent (Line {lineSpan, lineContent, markers}) transform tags st
    #! (markers, st)  = desugarAndTagMarkers markers st
    #! (imSp`, imOff) = applyTransforms transform lineSpan
    = ({ desugarAndTagSyn_ImageContent        = Line { LineImage
                                                     | lineSpan    = lineSpan
                                                     , markers     = markers
                                                     , lineContent = lineContent }
       , desugarAndTagSyn_TotalSpan_PreTrans  = lineSpan
       , desugarAndTagSyn_TotalSpan_PostTrans = imSp`
       , desugarAndTagSyn_OffsetCorrection    = imOff
       }, st)
    where
    desugarAndTagMarkers :: !(Maybe (Markers s)) !*DesugarAndTagStVal
                         -> *(!(Maybe (Markers s)), !*DesugarAndTagStVal)
    desugarAndTagMarkers (Just {markerStart, markerMid, markerEnd}) st
      #! (markerStart, st) = desugarAndTagMaybeImage markerStart st
      #! (markerMid, st)   = desugarAndTagMaybeImage markerMid st
      #! (markerEnd, st)   = desugarAndTagMaybeImage markerEnd st
      = (Just {markerStart = markerStart, markerMid = markerMid, markerEnd = markerEnd}, st)
    desugarAndTagMarkers n st = (n, st)
  desugarAndTagImageContent (Composite {host, compose}) transform tags st
    #! (host, st)                   = desugarAndTagMaybeImage host st
    #! ((compose, composeSpan), st) = desugarAndTagCompose compose host tags st
    #! (host, span)                 = case host of
                                       Just hostImg
                                          -> (host, mkTotalSpanPostTrans hostImg)
                                       _  -> (Nothing, composeSpan)
    #! (span`, corr)                = applyTransforms transform span
    = ({ desugarAndTagSyn_ImageContent        = Composite { CompositeImage
                                                          | host    = host
                                                          , compose = compose
                                                          }
       , desugarAndTagSyn_TotalSpan_PreTrans  = span
       , desugarAndTagSyn_TotalSpan_PostTrans = span`
       , desugarAndTagSyn_OffsetCorrection    = corr
       }, st)
    where
    desugarAndTagCompose :: !(Compose s) !(Maybe (Image s)) !(Set ImageTag) !*DesugarAndTagStVal
                         -> *(!(!Compose s, !ImageSpan), !*DesugarAndTagStVal)
    desugarAndTagCompose (AsGrid (numcols, numrows) offsetss iass imgss) host tags st
      #! (imgss, st) = strictTRMapSt (strictTRMapSt desugarAndTag) imgss st
      #! (tag, st)   = nextNo st
      #! sysTags     = ImageTagSystem tag
      #! colIndices  = [0 .. numcols - 1]
      #! rowIndices  = [0 .. numrows - 1]
      #! gridSpan    = maybe ( strictFoldl (\acc n -> LookupSpan (ColumnXSpan sysTags n) + acc) (px 0.0) colIndices
                             , strictFoldl (\acc n -> LookupSpan (RowYSpan sysTags n)    + acc) (px 0.0) rowIndices
                             )
                             mkTotalSpanPostTrans host
      #! spanss      = strictTRMap (strictTRMap mkTotalSpanPostTrans) imgss
      #! st          = cacheGridSpans tag ('DS'.insert sysTags tags)
                                      (strictTRMap (maxSpan o strictTRMap fst) (transpose spanss))
                                      (strictTRMap (maxSpan o strictTRMap snd) spanss) st
      #! offsets`    = calculateGridOffsets (strictTRMap (\n -> LookupSpan (ColumnXSpan sysTags n)) colIndices)
                                            (strictTRMap (\n -> LookupSpan (RowYSpan sysTags n))    rowIndices) iass imgss offsetss
      #! offsets`    = reverseTR (flattenTR offsets`)
      = (( AsCollage offsets` (flattenTR imgss)
         , gridSpan), st)
      where
      calculateGridOffsets :: ![Span] ![Span] ![[ImageAlign]] ![[Image s]] ![[(!Span, !Span)]] -> [[(!Span, !Span)]]
      calculateGridOffsets cellXSpans cellYSpans alignss imagess offsetss
        = fst (strictFoldl (mkRows cellXSpans) ([], zero) (strictTRZip4 alignss imagess cellYSpans offsetss))
        where
        mkRows :: ![Span] !(![[(!Span, !Span)]], !Span) !(![(!XAlign, !YAlign)], ![Image s], !Span, ![(!Span, !Span)])
               -> (![[(!Span, !Span)]], !Span)
        mkRows cellXSpans (acc, yoff) (aligns, imgs, cellYSpan, offsets)
          = ( [fst (strictFoldl (mkCols cellYSpan yoff) ([], zero) (strictTRZip4 aligns imgs cellXSpans offsets)) : acc]
            , yoff + cellYSpan)
        mkCols :: !Span !Span !(![(!Span, !Span)], !Span) !(!(!XAlign, !YAlign), !Image s, !Span, !(!Span, !Span))
               -> (![(!Span, !Span)], !Span)
        mkCols cellYSpan yoff (acc, xoff) (align, img=:{transformCorrection = (tfXCorr, tfYCorr)}, cellXSpan, (manXOff, manYOff))
          #! (alignXOff, alignYOff) = calcAlignOffset cellXSpan cellYSpan (mkTotalSpanPostTrans img) align
          = ([( xoff + alignXOff + manXOff + tfXCorr
              , yoff + alignYOff + manYOff + tfYCorr) : acc], xoff + cellXSpan)

    desugarAndTagCompose (AsCollage offsets imgs) host tags st
      #! (imgs, st) = strictTRMapSt desugarAndTag imgs st
      = (( AsCollage offsets imgs
         , maybe (calculateComposedSpan (strictTRMap mkTotalSpanPostTrans imgs) offsets) mkTotalSpanPostTrans host), st)
    desugarAndTagCompose (AsOverlay offsets ias imgs) host tags st
      #! (imgs, st)     = strictTRMapSt desugarAndTag imgs st
      #! spans          = strictTRMap mkTotalSpanPostTrans imgs
      #! (  maxXSpan
          , maxYSpan)   = maybe (maxSpan (strictTRMap fst spans), maxSpan (strictTRMap snd spans))
                                (\x -> x.totalSpanPreTrans) host
      #! alignOffsets   = strictTRZipWith (calcAlignOffset maxXSpan maxYSpan) spans ias
      #! placingOffsets = strictTRZipWith3 addOffset alignOffsets offsets imgs
      = ( ( AsCollage placingOffsets imgs
          , maybe (calculateComposedSpan spans placingOffsets) mkTotalSpanPostTrans host)
        , st)
      where
      addOffset :: !(!Span, !Span) !(!Span, !Span) !(Image s) -> (!Span, !Span)
      addOffset (x1, y1) (x2, y2) {transformCorrection = (xoff, yoff)} = (x1 + x2 + xoff, y1 + y2 + yoff)

:: *SpanEnvs =
  { spanEnvImageTagPostTrans  :: !IntMap Int
  , spanEnvImageSpanPostTrans :: !IntMap ImageSpan
  , spanEnvGridTag            :: !IntMap Int
  , spanEnvGridSpan           :: !IntMap (!IntMap Span, !IntMap Span)
  , spanEnvFonts              :: !Map FontDef (Map String Real)
  }

fixEnvs :: !*FixSpansSt -> *FixSpansSt
fixEnvs st
  = fixEnvs` True {FixSpansSt | st & fixSpansDidChange = False}
  where
  fixEnvs` :: !Bool !*FixSpansSt -> *FixSpansSt
  fixEnvs` False st=:{fixSpansDidChange = False} = st
  fixEnvs` _ st
    #! st = fixImageSpansPostTrans st
    #! st = fixGridSpans           st
    = fixEnvs` False st
  fixImageSpansPostTrans :: !*FixSpansSt -> *FixSpansSt
  fixImageSpansPostTrans st
    #! fixSpansSpanEnvs          = st.fixSpansSpanEnvs
    #! spanEnvImageSpanPostTrans = fixSpansSpanEnvs.spanEnvImageSpanPostTrans
    #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
    #! st                        = {st & fixSpansSpanEnvs = fixSpansSpanEnvs}
    = 'DIS'.foldrWithKey f st spanEnvImageSpanPostTrans
    where
    f :: !Int !(!Span, !Span) !*FixSpansSt -> *FixSpansSt
    f k (PxSpan _, PxSpan _) st = {FixSpansSt | st & fixSpansDidChange = False}
    f k (w=:(PxSpan _), h) st
      #! (h`, st`) = fixSpans h {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st` & fixSpansDidChange = st.fixSpansDidChange}
    f k (w, h=:(PxSpan _)) st
      #! (w`, st`) = fixSpans w {st & fixSpansDidChange = False}
      | st`.fixSpansDidChange
        #! fixSpansSpanEnvs          = st`.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st` & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st` & fixSpansDidChange = st.fixSpansDidChange}
    f k (w, h) st
      #! (w`, st1) = fixSpans w {st & fixSpansDidChange = False}
      #! (h`, st2) = fixSpans h {st1 & fixSpansDidChange = False}
      | st1.fixSpansDidChange || st2.fixSpansDidChange
        #! fixSpansSpanEnvs          = st2.fixSpansSpanEnvs
        #! spanEnvImageSpanPostTrans = 'DIS'.put k (w`, h`) fixSpansSpanEnvs.spanEnvImageSpanPostTrans
        #! fixSpansSpanEnvs          = {fixSpansSpanEnvs & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = {st2 & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st2 & fixSpansDidChange = st.fixSpansDidChange}
  fixGridSpans :: !*FixSpansSt -> *FixSpansSt
  fixGridSpans st=:{fixSpansSpanEnvs, fixSpansDidChange = origDidChange}
    #! fixSpansSpanEnvs = st.fixSpansSpanEnvs
    #! spanEnvGridSpan  = fixSpansSpanEnvs.spanEnvGridSpan
    #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
    #! st               = {st & fixSpansSpanEnvs = fixSpansSpanEnvs}
    #! st               = 'DIS'.foldrWithKey f {st & fixSpansDidChange = False} spanEnvGridSpan
    = {st & fixSpansDidChange = st.fixSpansDidChange || origDidChange}
    where
    f :: !Int !(!IntMap Span, !IntMap Span) !*FixSpansSt -> *FixSpansSt
    f k (xsps, ysps) st=:{fixSpansDidChange = origDidChange}
      #! (xsps`, st1) = 'DIS'.mapSt fixWithState xsps {st & fixSpansDidChange = False}
      #! (ysps`, st2) = 'DIS'.mapSt fixWithState ysps {st1 & fixSpansDidChange = False}
      | st1.fixSpansDidChange || st2.fixSpansDidChange
        #! fixSpansSpanEnvs = st2.fixSpansSpanEnvs
        #! spanEnvGridSpan  = 'DIS'.put k (xsps`, ysps`) fixSpansSpanEnvs.spanEnvGridSpan
        #! fixSpansSpanEnvs = {fixSpansSpanEnvs & spanEnvGridSpan = spanEnvGridSpan}
        = {st2 & fixSpansSpanEnvs = fixSpansSpanEnvs, fixSpansDidChange = True}
      | otherwise = {st2 & fixSpansDidChange = origDidChange}

fixWithState :: !Span !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixWithState v st
  #! (v, st`) = fixSpans v {st & fixSpansDidChange = False}
  = (v, {st` & fixSpansDidChange = st`.fixSpansDidChange || st.fixSpansDidChange})

isPxSpan :: !Span -> Bool
isPxSpan (PxSpan _) = True
isPxSpan _          = False

fixSpans :: !Span !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixSpans sp=:(PxSpan _) st = (sp, {st & fixSpansDidChange = False})
fixSpans osp=:(AddSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` + y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(SubSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` - y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(MulSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` * y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(DivSpan x y) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not (isPxSpan x` || st1.fixSpansDidChange) = (osp, {st1 & fixSpansDidChange = False})
  #! (y`, st2) = fixSpans y {st1 & fixSpansDidChange = False}
  = case x` / y` of
      sp=:(PxSpan _) -> (sp, {st2 & fixSpansDidChange = True})
      sp             -> (sp, st2)
fixSpans osp=:(AbsSpan x) st
  #! (x`, st1) = fixSpans x {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  | otherwise
  = case x` of
      PxSpan x` | x` < 0.0 = (PxSpan (abs x`), {st & fixSpansDidChange = True})
      sp                   = (sp, st1)
fixSpans osp=:(MinSpan xs) st
  #! (xs, st1) = strictTRMapSt fixWithState xs {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  = case minSpan xs of
      sp=:(PxSpan _) -> (sp, {st1 & fixSpansDidChange = True})
      sp             -> (sp, st1)
fixSpans osp=:(MaxSpan xs) st
  #! (xs, st1) = strictTRMapSt fixWithState xs {st & fixSpansDidChange = False}
  | not st1.fixSpansDidChange = (osp, st1)
  = case maxSpan xs of
      sp=:(PxSpan _) -> (sp, {st1 & fixSpansDidChange = True})
      sp             -> (sp, st1)
fixSpans (LookupSpan lu) st = fixLookupSpans lu st

fixLookupSpans :: !LookupSpan !*FixSpansSt -> *(!Span, !*FixSpansSt)
fixLookupSpans (TextXSpan fd str) st
  #! sw = case 'DM'.get fd st.fixSpansSpanEnvs.spanEnvFonts of
            Just fs -> case 'DM'.get str fs of
                         Just sw -> sw
                         _       -> 0.0
            _       -> 0.0
  = (PxSpan sw, {st & fixSpansDidChange = True})
fixLookupSpans osp=:(ImageXSpan t) st
  #! ses                      = st.fixSpansSpanEnvs
  #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
  #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
        #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (xsp=:(PxSpan _), _)
              = (xsp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
            Just _
              = (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
fixLookupSpans osp=:(ImageYSpan t) st
  #! ses                      = st.fixSpansSpanEnvs
  #! spanEnvImageTagPostTrans = ses.spanEnvImageTagPostTrans
  #! ses                      = {ses & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = ses.spanEnvImageSpanPostTrans
        #! ses                       = {ses & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (_, ysp=:(PxSpan _))
              = (ysp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
            Just _
              = (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
fixLookupSpans osp=:(ColumnXSpan t n) st
  #! ses            = st.fixSpansSpanEnvs
  #! spanEnvGridTag = ses.spanEnvGridTag
  #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = ses.spanEnvGridSpan
        #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (xs, _)
              = case 'DIS'.get n xs of
                  Just xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                  _                    -> (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
fixLookupSpans osp=:(RowYSpan t n) st
  #! ses            = st.fixSpansSpanEnvs
  #! spanEnvGridTag = ses.spanEnvGridTag
  #! ses            = {ses & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = ses.spanEnvGridSpan
        #! ses             = {ses & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (_, xs)
              = case 'DIS'.get n xs of
                  Just xsn=:(PxSpan _) -> (xsn, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
                  _                    -> (LookupSpan osp, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = False})
            _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})
      _ = (PxSpan 0.0, {st & fixSpansSpanEnvs = ses, fixSpansDidChange = True})

numTag (ImageTagUser n _) = n
numTag (ImageTagSystem n) = n + 8096

:: ImageSpanReal :== (!Real, !Real)

:: ImageOffsetReal :== (!Real, !Real)

:: GenSVGSyn s =
  { genSVGSyn_svgElts       :: ![SVGElt]
  , genSVGSyn_imageSpanReal :: !ImageSpanReal
  , genSVGSyn_events        :: !Map String (ImageAttr s)
  , genSVGSyn_draggable     :: !Map String (ImageAttr s)
  , genSVGSyn_idMap         :: !Map String (Set ImageTag)
  }

mkGenSVGSyn =: { genSVGSyn_svgElts       = []
               , genSVGSyn_imageSpanReal = (0.0, 0.0)
               , genSVGSyn_events        = 'DM'.newMap
               , genSVGSyn_draggable     = 'DM'.newMap
               , genSVGSyn_idMap         = 'DM'.newMap
               }

editletId =: "__INTERNAL_editletId_PLACEHOLDER__"

mkMaskId :: !String !Int -> String
mkMaskId editletId uniqId = "maskId-" +++ editletId +++ toString uniqId

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkOnClickId :: !String !Int -> String
mkOnClickId editletId uniqId = "onClickId-" +++ editletId +++ toString uniqId

mkOnMouseDownId :: !String !Int -> String
mkOnMouseDownId editletId uniqId = "onMouseDownId-" +++ editletId +++ toString uniqId

mkOnMouseUpId :: !String !Int -> String
mkOnMouseUpId editletId uniqId = "onMouseUpId-" +++ editletId +++ toString uniqId

mkOnMouseOverId :: !String !Int -> String
mkOnMouseOverId editletId uniqId = "onMouseOverId-" +++ editletId +++ toString uniqId

mkOnMouseMoveId :: !String !Int -> String
mkOnMouseMoveId editletId uniqId = "onMouseMoveId-" +++ editletId +++ toString uniqId

mkOnMouseOutId :: !String !Int -> String
mkOnMouseOutId editletId uniqId = "onMouseOutId-" +++ editletId +++ toString uniqId

getSvgAttrs :: ![Maybe SVGAttr] -> [SVGAttr]
getSvgAttrs as = [a \\ Just a <- as]

mkUrl :: !String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (toString (to2dec imXSp)), HeightAttr (toString (to2dec imYSp))]

to2dec :: !Real -> Real
to2dec n = toReal (toInt (n * 100.0)) / 100.0

mkUniqId :: !String !Int -> String
mkUniqId editletId uniqId = "uniqId-" +++ editletId +++ toString uniqId

imageMaskId :: !*a -> *(!String, !*a) | nextNo a
imageMaskId clval
  #! (uid, clval) = nextNo clval
  = (mkMaskId editletId uid, clval)

mkElt :: !String !(Maybe (GenSVGSyn .a)) !(GenSVGSyn .b) -> [SVGElt]
mkElt _      Nothing     syn = syn.genSVGSyn_svgElts
mkElt maskId (Just mask) syn
  = [ DefsElt [] [] [MaskElt [IdAttr maskId] [] mask.genSVGSyn_svgElts]
    : syn.genSVGSyn_svgElts]

genSVGMaybeImage :: !(Maybe (Image s)) !*(GenSVGStVal s) -> (!Maybe (GenSVGSyn s), !*GenSVGStVal s)
genSVGMaybeImage (Just img) st
  #! (syn, st) = genSVG img st
  = (Just syn, st)
genSVGMaybeImage _ st = (Nothing, st)

splitAttribs :: ![(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s))]
             -> (![Maybe SVGAttr], ![Map String (ImageAttr s)], ![Map String (ImageAttr s)])
splitAttribs [] = ([], [], [])
splitAttribs [(x, y, z) : rest]
  #! (xs, ys, zs) = splitAttribs rest
  = ([x:xs], [y:ys], [z:zs])

genSVG :: !(Image s) !*(GenSVGStVal s) -> *(!GenSVGSyn s, !*GenSVGStVal s)
genSVG {content, mask, attribs, transform, tags, uniqId, totalSpanPreTrans = (txsp, tysp)} st
  #! (attribs, st)         = strictTRMapSt genSVGImageAttr ('DS'.toList attribs) st
  #! (txsp, st)            = evalSpan txsp st
  #! (tysp, st)            = evalSpan tysp st
  #! currTag               = ImageTagSystem uniqId
  #! (txsp`, st)           = evalSpan (LookupSpan (ImageXSpan currTag)) st
  #! (tysp`, st)           = evalSpan (LookupSpan (ImageYSpan currTag)) st
  #! (maskId, st)          = imageMaskId st
  #! (imAts`, evts, drags) = splitAttribs attribs
  #! interactive           = strictFoldl (\acc (_, m1, m2) -> acc || not ('DM'.null m1) || not ('DM'.null m2)) False attribs
  #! (syn, st)             = genSVGImageContent content interactive (txsp, tysp) (maybe imAts` (const [Just (MaskAttr (mkUrl maskId)) : imAts`]) mask) transform tags uniqId st
  #! (mask, st)            = genSVGMaybeImage mask st
  = ({ genSVGSyn_svgElts       = mkElt maskId mask syn
     , genSVGSyn_imageSpanReal = (txsp`, tysp`)
     , genSVGSyn_events        = 'DM'.unions [syn.genSVGSyn_events : evts]
     , genSVGSyn_draggable     = 'DM'.unions [syn.genSVGSyn_draggable : drags]
     , genSVGSyn_idMap         = 'DM'.put (mkUniqId editletId uniqId) tags syn.genSVGSyn_idMap
     }, st)
  where
  genSVGImageAttr :: !(ImageAttr s) !*(GenSVGStVal s)
                  -> *(!(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*(GenSVGStVal s))
  genSVGImageAttr (ImageStrokeAttr { stroke }) st
    = ((Just (StrokeAttr (PaintColor stroke Nothing)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageStrokeWidthAttr { strokewidth }) st
    #! (w, st) = evalSpan strokewidth st
    = ((Just (StrokeWidthAttr (StrokeWidthLength (toString w, PX))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageXRadiusAttr { xradius }) st
    #! (r, st) = evalSpan xradius st
    = ((Just (RxAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageYRadiusAttr { yradius }) st
    #! (r, st) = evalSpan yradius st
    = ((Just (RyAttr (toString r, PX)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageStrokeOpacityAttr { opacity }) st
    = ((Just (StrokeOpacityAttr (toString opacity)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageFillOpacityAttr { opacity }) st
    = ((Just (FillOpacityAttr (FillOpacity (toString opacity))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageFillAttr { fill }) st
    = ((Just (FillAttr (PaintColor fill Nothing)), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr (ImageDashAttr { dash }) st
    = ((Just (StrokeDashArrayAttr (DashArray (strictTRMap toString dash))), 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr attr=:(ImageOnClickAttr _) st
    = mkEvent mkOnClickId attr st
  genSVGImageAttr attr=:(ImageOnMouseDownAttr _) st
    = mkEvent mkOnMouseDownId attr st
  genSVGImageAttr attr=:(ImageOnMouseUpAttr _) st
    = mkEvent mkOnMouseUpId attr st
  genSVGImageAttr attr=:(ImageOnMouseOverAttr _) st
    = mkEvent mkOnMouseOverId attr st
  genSVGImageAttr attr=:(ImageOnMouseMoveAttr _) st
    = mkEvent mkOnMouseMoveId attr st
  genSVGImageAttr attr=:(ImageOnMouseOutAttr _) st
    = mkEvent mkOnMouseOutId attr st
  genSVGImageAttr (ImageDraggableAttr { draggable = Nothing }) st
    = ((Nothing, 'DM'.newMap, 'DM'.newMap), st)
  genSVGImageAttr attr=:(ImageDraggableAttr _) st
    #! ocId = mkUniqId editletId uniqId
    = ((Nothing, 'DM'.newMap, 'DM'.singleton ocId attr), st)
  genSVGImageAttr _ st
    = ((Nothing, 'DM'.newMap, 'DM'.newMap), st)

  mkEvent :: !(String Int -> String) !(ImageAttr s) !*(GenSVGStVal s)
          -> *(!(!Maybe SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s)), !*(GenSVGStVal s))
  mkEvent mkIdFun attr clval
    #! ocId = mkUniqId editletId uniqId // TODO FIXME: Shouldn't this be mkIdFun instead of mkUniqId?
    = ((Nothing, 'DM'.singleton ocId attr, 'DM'.newMap), clval)

  genSVGImageContent :: !(ImageContent s) !Bool !ImageSpanReal ![Maybe SVGAttr] ![ImageTransform] !(Set ImageTag) Int !*(GenSVGStVal s)
                     -> *(!GenSVGSyn s, !*GenSVGStVal s) 
  genSVGImageContent (Basic baIm (xsp, ysp)) interactive _ attribs transform _ uniqId st
    #! isText          = case baIm of
                           TextImage _ _ -> True
                           _             -> False
    #! (xsp, st)       = evalSpan xsp st
    #! (ysp, st)       = evalSpan ysp st
    #! imSp            = (xsp, ysp)
    #! (transform, st) = strictTRMapSt (genSVGTransform isText imSp) transform st
    = genSVGBasicImage interactive attribs (flattenTR transform) baIm imSp st
    where
    genSVGBasicImage :: !Bool ![Maybe SVGAttr] ![SVGTransform] !BasicImage !ImageSpanReal !*(GenSVGStVal s)
                     -> *(!GenSVGSyn s, !*GenSVGStVal s)
    genSVGBasicImage interactive attribs transform EmptyImage imSp st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = mkGroup hattrs (mkAttrs attribs transform) [] }, st)
    genSVGBasicImage interactive attribs transform (TextImage fd str) imSp st
      // TODO Currently we manually translate text by fontysize pixels to
      // compensate for the "auto" baseline. The result look OK, but a bit off
      // compare to the old approach where we forced the origin to be the
      // top-left corner (which didn't work with zooming)
      // We need to offset by the font's descent height, but that's not easy to
      // calculate currently (there are no JS APIs for that yet). Current
      // heuristic: we assume that the ex-height is half of the font height. We
      // assume that the descent height is half of the ex-height. Therefore, we
      // multiply by 0.75
      #! hattrs = [XmlspaceAttr "preserve"]
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [TextElt hattrs (addAttr (TransformAttr [TranslateTransform (toString 0.0) (toString (fd.fontysize * 0.75))]) (mkAttrs attribs transform ++ fontAttrs fd.fontysize)) str] }
         , st)
      where
      fontAttrs :: !Real -> [SVGAttr]
      fontAttrs fsz = [ AlignmentBaselineAttr "auto"
                      , DominantBaselineAttr "auto"
                      , FontFamilyAttr fd.fontfamily
                      , FontSizeAttr (toString fsz)
                      , FontStyleAttr fd.fontstyle
                      , FontStretchAttr fd.fontstretch
                      , FontVariantAttr fd.fontvariant
                      , FontWeightAttr fd.fontweight
                      , TextRenderingAttr "geometricPrecision"
                      ]

    genSVGBasicImage interactive attribs transform RectImage imSp st
      #! hattrs = mkWH imSp
      #! hattrs = if interactive [IdAttr (mkUniqId editletId uniqId) : hattrs] hattrs
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [RectElt hattrs (mkAttrs attribs transform)] }, st)
    genSVGBasicImage interactive attribs transform (RawImage svgStr) imSp st
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [RawElt svgStr] }, st)

    genSVGBasicImage interactive attribs transform CircleImage (imXSp`, _) st
      #! r = imXSp` / 2.0
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [CircleElt (if interactive [IdAttr (mkUniqId editletId uniqId)] [])
                                               [ RAttr (toString (to2dec r), PX), CxAttr (toString (to2dec r), PX)
                                               , CyAttr (toString (to2dec r), PX) : (mkAttrs attribs transform) ]] }, st)
    genSVGBasicImage interactive attribs transform EllipseImage (imXSp, imYSp) st
      = ({ mkGenSVGSyn & genSVGSyn_svgElts = [EllipseElt (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs attribs transform ++
                                               [ RxAttr (toString (to2dec (imXSp / 2.0)), PX), RyAttr (toString (to2dec (imYSp / 2.0)), PX)
                                               , CxAttr (toString (to2dec (imXSp / 2.0)), PX), CyAttr (toString (to2dec (imYSp / 2.0)), PX)])] }, st)
  genSVGImageContent (Line {lineSpan = (xsp, ysp), markers, lineContent}) interactive totalSpanPreTrans attribs transform tags uniqId st
    #! (xsp, st)       = evalSpan xsp st
    #! (ysp, st)       = evalSpan ysp st
    #! (transform, st) = strictTRMapSt (genSVGTransform False (xsp, ysp)) transform st
    #! (markers, st)   = genSVGMarkers markers st
    = genSVGLineContent (xsp, ysp) markers (flattenTR transform) lineContent st
    where
    genSVGMarkers :: !(Maybe (Markers s)) !*(GenSVGStVal s)
                  -> *(!Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s)), !*GenSVGStVal s)
    genSVGMarkers (Just { markerStart, markerMid, markerEnd }) st
      #! (markerStart, st) = genSVGMaybeImage markerStart st
      #! (markerMid, st)   = genSVGMaybeImage markerMid st
      #! (markerEnd, st)   = genSVGMaybeImage markerEnd st
      = (Just (markerStart, markerMid, markerEnd), st)
    genSVGMarkers _ st = (Nothing, st)

    genSVGLineContent :: !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), Maybe (GenSVGSyn s), Maybe (GenSVGSyn s))) ![SVGTransform] !LineContent !*(GenSVGStVal s)
                      -> *(!GenSVGSyn s, !*GenSVGStVal s)
    genSVGLineContent sp=:(xspan, yspan) markers transform (SimpleLineImage sl) st
      #! (y1, y2) = case sl of
                      Slash     -> (toString (to2dec yspan), "0.0")
                      Backslash -> ("0.0", toString (to2dec yspan))
      = mkLine LineElt [X1Attr ("0.0", PX), X2Attr (toString (to2dec xspan), PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : mkAttrs attribs transform] sp markers st
    genSVGLineContent sp=:(xspan, yspan) markers transform (PolygonImage points) st
      #! (offsets, st) = evalListOfSpanPair points st
      = mkLine PolygonElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs attribs transform] sp markers st
    genSVGLineContent sp=:(xspan, yspan) markers transform (PolylineImage points) st
      #! (offsets, st) = evalListOfSpanPair points st
      = mkLine PolylineElt [PointsAttr (strictTRMap (\(x, y) -> (toString (to2dec x), toString (to2dec y))) offsets) : mkAttrs attribs transform] sp markers st

    mkLine :: !([HtmlAttr] [SVGAttr] -> SVGElt) ![SVGAttr] !ImageSpanReal !(Maybe (Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s), !Maybe (GenSVGSyn s))) !*(GenSVGStVal s)
           -> *(!GenSVGSyn s, !*GenSVGStVal s)
    mkLine constr atts spans (Just (mmStart, mmMid, mmEnd)) clval
      #! (uid1, clval) = nextNo clval
      #! (uid2, clval) = nextNo clval
      #! (uid3, clval) = nextNo clval
      #! markersAndIds = [  (m, i, s, d, p)
                         \\ Just (m, i, s, d, p) <- [ mkMarkerAndId mmStart (mkMarkerId editletId uid1) MarkerStartAttr
                                                    , mkMarkerAndId mmMid   (mkMarkerId editletId uid2) MarkerMidAttr
                                                    , mkMarkerAndId mmEnd   (mkMarkerId editletId uid3) MarkerEndAttr ]]
      = ({ mkGenSVGSyn
         & genSVGSyn_svgElts   = [ constr [] (atts ++ strictTRMap (\(_, x, _, _, _) -> x) markersAndIds)
                                 , DefsElt [] [] (strictTRMap (\(x, _, _, _, _) -> x) markersAndIds)]
         , genSVGSyn_events    = 'DM'.unions (strictTRMap (\(_, _, x, _, _) -> x) markersAndIds)
         , genSVGSyn_draggable = 'DM'.unions (strictTRMap (\(_, _, _, x, _) -> x) markersAndIds)
         , genSVGSyn_idMap     = 'DM'.unions (strictTRMap (\(_, _, _, _, x) -> x) markersAndIds)
         }, clval) // TODO Correct offsets? What about the transformations?
      where
      // TODO Marker size etc?
      mkMarkerAndId :: !(Maybe (GenSVGSyn s)) !String !(String -> SVGAttr) -> Maybe (!SVGElt, !SVGAttr, !Map String (ImageAttr s), !Map String (ImageAttr s), !Map String (Set ImageTag))
      mkMarkerAndId (Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal = (w, h), genSVGSyn_events, genSVGSyn_draggable, genSVGSyn_idMap}) mid posAttr
        #! wStr = toString (to2dec w)
        #! hStr = toString (to2dec h)
        = Just ( MarkerElt [IdAttr mid] [ OrientAttr "auto"
                                        , ViewBoxAttr "0" "0" wStr hStr
                                        , RefXAttr (wStr, PX)
                                        , RefYAttr (toString (to2dec (h / 2.0)), PX)
                                        , MarkerHeightAttr (hStr, PX)
                                        , MarkerWidthAttr (wStr, PX)
                                        ] genSVGSyn_svgElts
               , posAttr (mkUrl mid)
               , genSVGSyn_events
               , genSVGSyn_draggable
               , genSVGSyn_idMap)
      mkMarkerAndId _ _ _ = Nothing
    mkLine constr atts spans _ st = ({ mkGenSVGSyn & genSVGSyn_svgElts = [constr [] atts]}, st)

  genSVGImageContent (Composite {host, compose}) interactive totalSpanPreTrans attribs transform tags uniqId st
    #! (host, st)    = genSVGMaybeImage host st
    #! (compose, st) = genSVGCompose compose host totalSpanPreTrans attribs transform tags st
    #! (cpId, st)    = getCpId st
    #! (elts, spans, onclicks, draggables, idMap)
         = case host of
             Just {genSVGSyn_svgElts, genSVGSyn_imageSpanReal, genSVGSyn_events, genSVGSyn_draggable, genSVGSyn_idMap}
               = (genSVGSyn_svgElts ++ compose.genSVGSyn_svgElts, genSVGSyn_imageSpanReal, 'DM'.union genSVGSyn_events compose.genSVGSyn_events, 'DM'.union genSVGSyn_draggable compose.genSVGSyn_draggable, 'DM'.union genSVGSyn_idMap compose.genSVGSyn_idMap)
             _ = (compose.genSVGSyn_svgElts, compose.genSVGSyn_imageSpanReal, compose.genSVGSyn_events, compose.genSVGSyn_draggable, compose.genSVGSyn_idMap)
    #! (transform, st) = strictTRMapSt (genSVGTransform False spans) transform st
    = ({ genSVGSyn_imageSpanReal = (0.0, 0.0)
       , genSVGSyn_svgElts       = mkGroup (if interactive [IdAttr (mkUniqId editletId uniqId)] []) (mkAttrs attribs (flattenTR transform)) elts
       , genSVGSyn_events        = onclicks
       , genSVGSyn_draggable     = draggables
       , genSVGSyn_idMap         = idMap
       }, st)
    where
    getCpId :: !*(GenSVGStVal s) -> (!String, !*GenSVGStVal s)
    getCpId clval
      #! (n, clval) = nextNo clval
      = (mkClipPathId editletId n, clval)
    genSVGCompose :: !(Compose s) !(Maybe (GenSVGSyn s)) !ImageSpanReal ![Maybe SVGAttr] ![ImageTransform] !(Set ImageTag) !*(GenSVGStVal s)
                  -> (!GenSVGSyn s, !*GenSVGStVal s)
    genSVGCompose (AsCollage offsets imgs) host totalSpanPreTrans attribs transform tags st
      #! (offsets, st)    = evalListOfSpanPair offsets st
      #! (imgsSps, st)    = strictTRMapSt genSVG imgs st
      #! (ss, es, ds, is) = splitSyn imgsSps
      = ({ genSVGSyn_svgElts       = flattenTR (strictTRZipWith mkTranslateGroup offsets ss)
         , genSVGSyn_events        = es
         , genSVGSyn_draggable     = ds
         , genSVGSyn_idMap         = is
         , genSVGSyn_imageSpanReal = totalSpanPreTrans }, st) // Setting genSVGSyn_imageSpanReal is required here. It needs to be totalSpanPreTrans, because transforms will be calculated just after this.
      where
      splitSyn :: ![GenSVGSyn s] -> (![[SVGElt]], !Map String (ImageAttr s), !Map String (ImageAttr s), !Map String (Set ImageTag))
      splitSyn [] = ([], 'DM'.newMap, 'DM'.newMap, 'DM'.newMap)
      splitSyn [syn : rest]
        #! (ss, es, ds, is) = splitSyn rest
        = ([syn.genSVGSyn_svgElts : ss], 'DM'.union syn.genSVGSyn_events es, 'DM'.union syn.genSVGSyn_draggable ds, 'DM'.union syn.genSVGSyn_idMap is)
    // These aren't used. They're translated to collages in fixSpans. We
    // provide them here only because we must if we don't want the evaluation
    // to crash.
    genSVGCompose compose host totalSpanPreTrans attribs transform tags st
      = (mkGenSVGSyn, st)

  genSVGTransform :: !Bool !ImageSpanReal !ImageTransform !*(GenSVGStVal s)
                  -> (![SVGTransform], !*GenSVGStVal s)
  genSVGTransform isText (xsp, ysp) (RotateImage imAn) st
    // FIXME: We currently devide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
    #! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
    = ([RotateTransform (toString (to2dec (toDeg imAn))) (Just (toString (to2dec (xsp / 2.0)), toString (to2dec yoff)))], st)
  genSVGTransform _ _ (SkewXImage imAn) st
    = ([SkewXTransform (toString (toDeg imAn))], st)
  genSVGTransform _ _ (SkewYImage imAn) st
    = ([SkewYTransform (toString (toDeg imAn))], st)
  genSVGTransform isText (xsp, ysp) (FitImage sp1 sp2) st
    #! (sp1, st) = evalSpan sp1 st
    #! (sp2, st) = evalSpan sp2 st
    #! factorx   = to2dec (sp1 / xsp)
    #! factory   = to2dec (sp2 / ysp)
    #! attrs     = [ScaleTransform (toString factorx) (toString factory)]
    #! attrs     = case isText of
                     True
                       = [TranslateTransform "0" (toString ysp) : attrs]
                     _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (FitXImage sp) st
    #! (sp, st) = evalSpan sp st
    #! factor   = to2dec (sp / xsp)
    #! scale    = if (xsp > 0.0) (toString factor) "1.0"
    #! attrs    = [ScaleTransform scale scale]
    #! attrs    = case isText of
                    True
                      #! yoff = to2dec (ysp * 0.7 * factor)
                      = [TranslateTransform "0" (toString yoff) : attrs]
                    _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (FitYImage sp) st
    #! (sp, st) = evalSpan sp st
    #! factor   = to2dec (sp / ysp)
    #! scale    = if (ysp > 0.0) (toString factor) "1.0"
    #! attrs    = [ScaleTransform scale scale]
    #! attrs    = case isText of
                    True
                      = [TranslateTransform "0" (toString ysp) : attrs]
                    _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleImage sp1 sp2) st
    #! attrs = [ScaleTransform (toString sp1) (toString sp2)]
    #! attrs = case isText of
                 True
                   = [TranslateTransform "0" (toString ysp) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleXImage sp) st
    #! attrs = [ScaleTransform (toString sp) "1.0"]
    #! attrs = case isText of
                 True
                   #! yoff = to2dec (ysp * 0.7 * sp)
                   = [TranslateTransform "0" (toString yoff) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) (ScaleYImage sp) st
    #! attrs = [ScaleTransform "1.0" (toString sp)]
    #! attrs = case isText of
                 True
                   = [TranslateTransform "0" (toString ysp) : attrs]
                 _ = attrs
    = (attrs, st)
  genSVGTransform isText (xsp, ysp) FlipXImage st
    = ([TranslateTransform (toString xsp) "0", ScaleTransform "-1" "1"], st)
  genSVGTransform isText (xsp, ysp) FlipYImage st
    #! ysp = if isText ((~ ysp) * 0.7) ysp
    = ([TranslateTransform "0" (toString ysp), ScaleTransform "1" "-1"], st)

instance + (Real, Real) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _      _      []                  = []
mkGroup []     []     xs                  = xs
mkGroup hattrs []     [GElt [] sattrs xs] = [GElt hattrs sattrs xs]
mkGroup []     sattrs [GElt hattrs [] xs] = [GElt hattrs sattrs xs]
mkGroup []     [tfattr=:(TransformAttr [TranslateTransform x y])] xs = map f xs
  where
  f :: !SVGElt -> SVGElt
  f (GElt        hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = GElt       hattrs (addAttr (dualTransformTranslate x y x` y`) attrs) elts
  f (GElt        hattrs attrs elts)                                              = GElt       hattrs (addAttr tfattr attrs) elts
  f (TextElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = TextElt    hattrs (addAttr (dualTransformTranslate x y x` y`) attrs) elts
  f (TextElt     hattrs attrs elts)                                              = TextElt    hattrs (addAttr tfattr attrs) elts
  f (EllipseElt  hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = EllipseElt hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (EllipseElt  hattrs attrs)                                                   = EllipseElt hattrs (addAttr tfattr attrs)
  f (RectElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = RectElt    hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (RectElt     hattrs attrs)                                                   = RectElt    hattrs (addAttr tfattr attrs)
  f (CircleElt   hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = CircleElt  hattrs (addAttr (dualTransformTranslate x y x` y`) attrs)
  f (CircleElt   hattrs attrs)                                                   = CircleElt  hattrs (addAttr tfattr attrs)
  f (LineElt _ [X1Attr (x1, PX), X2Attr (x2, PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : attrs]) = LineElt [] [X1Attr (lineAdd x1 x, PX), X2Attr (lineAdd x2 x, PX), Y1Attr (lineAdd y1 y, PX), Y2Attr (lineAdd y2 y, PX) : attrs]
  f elt                                                                                      = GElt    [] [tfattr] [elt]
  lineAdd :: !String !SVGNumber -> String
  lineAdd strVal n = toString (to2dec (toReal strVal + toReal n))
mkGroup has    sas elts = [GElt has sas elts]

dualTransformTranslate :: !a !a !a !a -> SVGAttr | toReal a
dualTransformTranslate x y x` y` = TransformAttr [TranslateTransform (toString (to2dec (toReal x + toReal x`))) (toString (to2dec (toReal y + toReal y`)))]

addAttr :: !SVGAttr ![SVGAttr] -> [SVGAttr]
addAttr (TransformAttr tfs) attrs = addTransforms tfs attrs []
  where
  addTransforms :: ![SVGTransform] ![SVGAttr] ![SVGAttr] -> [SVGAttr]
  addTransforms tfs []                            acc = reverseTR acc ++ [TransformAttr tfs]
  addTransforms tfs [TransformAttr tfs` : attrs`] acc = reverseTR acc ++ [TransformAttr (tfs ++ tfs`) : attrs`]
  addTransforms tfs [attr:attrs]                  acc = addTransforms tfs attrs [attr:acc]
addAttr attr attrs = [attr:attrs]

mkAttrs :: ![Maybe SVGAttr] ![SVGTransform] -> [SVGAttr]
mkAttrs imAts [] = getSvgAttrs imAts
mkAttrs imAts xs = addAttr (TransformAttr xs) (getSvgAttrs imAts)

calcAlignOffset :: !Span !Span !(!Span, !Span) !ImageAlign -> (!Span, !Span)
calcAlignOffset maxxsp maxysp (imXSp, imYSp) (xal, yal) = (mkXAl maxxsp imXSp xal, mkYAl maxysp imYSp yal)
  where
  mkXAl :: !Span !Span !XAlign -> Span
  mkXAl maxxsp imXSp AtLeft    = zero
  mkXAl maxxsp imXSp AtMiddleX = (maxxsp /. 2.0) - (imXSp /. 2.0)
  mkXAl maxxsp imXSp AtRight   = maxxsp - imXSp

  mkYAl :: !Span !Span !YAlign -> Span
  mkYAl maxysp imYSp AtTop     = zero
  mkYAl maxysp imYSp AtMiddleY = (maxysp /. 2.0) - (imYSp /. 2.0)
  mkYAl maxysp imYSp AtBottom  = maxysp - imYSp

calculateComposedSpan :: ![(!Span, !Span)] ![(!Span, !Span)] -> (!Span, !Span)
calculateComposedSpan spans offs
  = strictFoldl f (zero, zero) (strictTRZip2Rev offs spans)
  where
  f :: !(!Span, !Span) !(!(!Span, !Span), !(!Span, !Span)) -> (!Span, !Span)
  f (maxX, maxY) ((xoff, yoff), (imXSp, imYSp)) = (maxSpan [maxX, xoff + imXSp], maxSpan [maxY, yoff + imYSp])

mkTranslateGroup :: !ImageOffsetReal ![SVGElt] -> [SVGElt]
mkTranslateGroup (xoff, yoff) contents
  = mkGroup [] (mkTransformTranslateAttr (to2dec xoff, to2dec yoff)) contents

mkTransformTranslateAttr :: !(!Real, !Real) -> [SVGAttr]
mkTransformTranslateAttr (0.0,   0.0)   = []
mkTransformTranslateAttr (xGOff, yGOff) = [TransformAttr [TranslateTransform (toString xGOff) (toString yGOff)]]

evalListOfSpanPair :: ![(!Span, !Span)] !*(GenSVGStVal s) -> *(![(!Real, !Real)], !*(GenSVGStVal s))
evalListOfSpanPair xs st = mapSt evalSpanPair xs st

evalSpanPair :: !(!Span, !Span) !*(GenSVGStVal s) -> *(!(!Real, !Real), !*(GenSVGStVal s))
evalSpanPair (xsp, ysp) st
  #! (xsp, st) = evalSpan xsp st
  #! (ysp, st) = evalSpan ysp st
  = ((xsp, ysp), st)

evalSpan :: !Span !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)
evalSpan (PxSpan r)    st = (r, st)
evalSpan (AddSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l + r, st)
evalSpan (SubSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l - r, st)
evalSpan (MulSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l * r, st)
evalSpan (DivSpan l r) st
  #! (l, st) = evalSpan l st
  #! (r, st) = evalSpan r st
  = (l / r, st)
evalSpan (AbsSpan x)   st
  #! (x, st) = evalSpan x st
  = (abs x, st)
evalSpan (MinSpan xs)  st
  #! (xs, st) = mapSt evalSpan xs st
  = (minList xs, st)
evalSpan (MaxSpan xs)  st
  #! (xs, st) = mapSt evalSpan xs st
  = (maxList xs, st)
evalSpan (LookupSpan lu) st = evalLookupSpans lu st

evalLookupSpans :: !LookupSpan !*(GenSVGStVal s) -> *(!Real, !*GenSVGStVal s)
evalLookupSpans (TextXSpan fd str) st
  #! sw = case 'DM'.get fd st.genStates.spanEnvFonts of
            Just fs -> case 'DM'.get str fs of
                         Just sw -> sw
                         _       -> 0.0
            _       -> 0.0
  = (sw, st)
evalLookupSpans (ImageXSpan t)     st
  #! genStates                = st.genStates
  #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
  #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = genStates.spanEnvImageSpanPostTrans
        #! genStates                 = {genStates & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (PxSpan x, _)
              = (x, {st & genStates = genStates})
            Just (xsp, _)
              = evalSpan xsp {st & genStates = genStates}
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (ImageYSpan t)     st
  #! genStates                = st.genStates
  #! spanEnvImageTagPostTrans = genStates.spanEnvImageTagPostTrans
  #! genStates                = {genStates & spanEnvImageTagPostTrans = spanEnvImageTagPostTrans}
  = case 'DIS'.get (numTag t) spanEnvImageTagPostTrans of
      Just n
        #! spanEnvImageSpanPostTrans = genStates.spanEnvImageSpanPostTrans
        #! genStates                 = {genStates & spanEnvImageSpanPostTrans = spanEnvImageSpanPostTrans}
        = case 'DIS'.get n spanEnvImageSpanPostTrans of
            Just (_, PxSpan x)
              = (x, {st & genStates = genStates})
            Just (_, ysp)
              = evalSpan ysp {st & genStates = genStates}
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (ColumnXSpan t colIdx)  st
  #! genStates      = st.genStates
  #! spanEnvGridTag = genStates.spanEnvGridTag
  #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = genStates.spanEnvGridSpan
        #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (xs, _)
              = case 'DIS'.get colIdx xs of
                  Just (PxSpan x) -> (x, {st & genStates = genStates})
                  Just sp         -> evalSpan sp {st & genStates = genStates}
                  _               -> (0.0, {st & genStates = genStates})
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})
evalLookupSpans (RowYSpan t rowIdx)     st
  #! genStates      = st.genStates
  #! spanEnvGridTag = genStates.spanEnvGridTag
  #! genStates      = {genStates & spanEnvGridTag = spanEnvGridTag}
  = case 'DIS'.get (numTag t) spanEnvGridTag of
      Just cacheIdx
        #! spanEnvGridSpan = genStates.spanEnvGridSpan
        #! genStates       = {genStates & spanEnvGridSpan = spanEnvGridSpan}
        = case 'DIS'.get cacheIdx spanEnvGridSpan of
            Just (_, xs)
              = case 'DIS'.get rowIdx xs of
                  Just (PxSpan x) -> (x, {st & genStates = genStates})
                  Just sp         -> evalSpan sp {st & genStates = genStates}
                  _               -> (0.0, {st & genStates = genStates})
            _ = (0.0, {st & genStates = genStates})
      _ = (0.0, {st & genStates = genStates})

//JAVASCRIPT UTIL: Should be in UI.JS.Interface or something

jsValToJSONNode :: !(JSVal a) !*JSWorld -> *(!JSONNode, !*JSWorld)
jsValToJSONNode val world
	//Null
	| jsIsNull val
		= (JSONNull,world)	
	//Arrays
	# (check,world) = jsIsArray val world
	| check
		# (len,world) = .? (val .# "length") world
		# (els,world) = readArrayEls val 0 (jsValToInt len) world
		= (JSONArray els,world)
	//Objects	
	| jsTypeof val == "object"
		# (keys,world) = readObjectKeys val world
		# (fields,world) = readObjectFields keys val world
		= (JSONObject (zip (keys,fields)),world)
	//Primitives
	= case fromJSValUnsafe val of
		(v :: Bool)    = (JSONBool v, world)
		(v :: Int)     = (JSONInt v, world)
 		(v :: Real)    = (JSONReal v, world)
		(v :: String)  = (JSONString v, world)
		_ 			   = (JSONError,world)
where
	readArrayEls arr i n world
		| i >= n = ([],world)
		# (jel,world) = .? (val .# i) world
		# (el,world)  = jsValToJSONNode jel world
		# (els,world) = readArrayEls arr (i + 1) n world
		= ([el:els],world)
		
	readObjectKeys obj world
		# (jskeys,world) = (jsWindow .# "Object.keys" .$ obj) world
		= fromJSArray jskeys jsValToString world

	readObjectFields [] obj world = ([],world)
	readObjectFields [k:ks] obj world
		# (jel,world)  = .? (obj .# k) world
		# (el,world)   = jsValToJSONNode jel world
		# (els,world)  = readObjectFields ks obj world
		= ([el:els],world)

