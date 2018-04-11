implementation module iTasks.Extensions.SVG.SVGEditor

import Graphics.Scalable.Internal.Image`
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.JS.Encoding
import StdArray, StdBool, StdEnum, StdInt, StdMisc, StdReal, StdTuple
from StdFunc import o
from Data.Generics.GenEq import generic gEq
from Data.Either import :: Either (..)
import Data.List
import Data.Error
import Data.MapCollection
from Data.Map import :: Map, instance Functor (Map k)
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Text
import Data.Matrix
import Text.HTML
import Text.JSON
import Math.Geometry

// JavaScript object attribute labels:
JS_ATTR_VIEW       :== "view"
JS_ATTR_MODEL      :== "model"
JS_ATTR_FONT_SPANS :== "font_spans"
JS_ATTR_TEXT_SPANS :== "text_spans"

CLICK_DELAY        :== 225
svgns =: "http://www.w3.org/2000/svg"

//Predefined object methods
(`addEventListener`)      obj args :== obj .# "addEventListener"      .$ args
(`setAttribute`)          obj args :== obj .# "setAttribute"          .$ args
(`setAttributeNS`)        obj args :== obj .# "setAttributeNS"        .$ args
(`createElementNS`)       obj args :== obj .# "createElementNS"       .$ args
(`appendChild`)           obj args :== obj .# "appendChild"           .$ args
(`removeChild`)           obj args :== obj .# "removeChild"           .$ args
(`getComputedTextLength`) obj args :== obj .# "getComputedTextLength" .$ args
(`createSVGPoint`)        obj args :== obj .# "createSVGPoint"        .$ args
(`getScreenCTM`)          obj args :== obj .# "getScreenCTM"          .$ args
(`inverse`)               obj args :== obj .# "inverse"               .$ args
(`matrixTransform`)       obj args :== obj .# "matrixTransform"       .$ args

:: ImageSpanReal :== (!Real, !Real)

:: DropTarget      = DropTarget
:: MousePos        = MouseUp | MouseDown
:: SVGDragState v  = 
  { svgMousePos     :: !MousePos
  , svgDropCallback :: !SVGDragFun v
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe (JSObj DropTarget)
  }
derive gEq MousePos

derive JSONEncode FontDef
derive JSONDecode FontDef

//	The server side state:
derive JSEncode FontDef, Map, Set, ViaImg, ImgEventhandler`, DefuncImgEventhandler`
derive JSDecode FontDef, Map, Set, ViaImg, ImgEventhandler`, DefuncImgEventhandler`
//derive JSEncode Maybe, FontDef, Map, Set, Angle, BasicImg, BasicImgAttr, DraggableAttr, GridSpan, HostImg, ImageTag, Img, ImgEventhandler, ImgPath, ImgTables, ImgTransform, LineMarkers, LookupSpan, 
//                OnClickAttr, OnMouseDownAttr, OnMouseMoveAttr, OnMouseOutAttr, OnMouseOverAttr, OnMouseUpAttr, Span, SVGColor
//derive JSDecode Maybe, FontDef, Map, Set, Angle, BasicImg, BasicImgAttr, DraggableAttr, GridSpan, HostImg, ImageTag, Img, ImgEventhandler, ImgPath, ImgTables, ImgTransform, LineMarkers, LookupSpan, 
//                OnClickAttr, OnMouseDownAttr, OnMouseMoveAttr, OnMouseOutAttr, OnMouseOverAttr, OnMouseUpAttr, Span, SVGColor

/*	the server side state is a list of state elements:
	at FM_FONTS:  the FontSpans are stored
	at FM_TEXTS:  the TextSpans are stored
	at FM_IMG:    the Img is stored
	at FM_TABLES: the (ImgTables v) are stored
*/
initServerSideState :: EditMask
initServerSideState = CompoundMask (repeatn 2 newFieldMask)

mkFieldMask :: !s -> FieldMask | JSEncode{|*|} s
mkFieldMask s = {FieldMask | touched = False, valid = True, state = encodeOnServer s}

FM_FONTS  :== 0	// index in CompoundMask to access fonts cache
FM_TEXTS  :== 1	// index in CompoundMask to access texts widths
//FM_IMG    :== 2	// index in CompoundMask to access img
//FM_TABLES :== 3	// index in CompoundMask to access tables

//	access functions to get and set server side state elements:
getFontsCache :: !EditMask -> FontSpans
getFontsCache (CompoundMask entries)
	= case entries !! FM_FONTS of
		FieldMask {FieldMask | state}
			= case decodeOnServer state of
				Just fonts = fonts
				nothing    = 'DM'.newMap
		_ = 'DM'.newMap

setFontsCache :: !FontSpans !EditMask -> EditMask
setFontsCache fonts (CompoundMask entries)
	= CompoundMask (updateAt FM_FONTS (FieldMask (mkFieldMask fonts)) entries)

getTextsCache :: !EditMask -> TextSpans
getTextsCache (CompoundMask entries)
	= case entries !! FM_TEXTS of
		FieldMask {FieldMask | state}
			= case decodeOnServer state of
				Just texts = texts
				nothing    = 'DM'.newMap
		_ = 'DM'.newMap

setTextsCache :: !TextSpans !EditMask -> EditMask
setTextsCache texts (CompoundMask entries)
	= CompoundMask (updateAt FM_TEXTS (FieldMask (mkFieldMask texts)) entries)

/*getImgState :: !EditMask -> Maybe Img
getImgState (CompoundMask entries)
	= case entries !! FM_IMG of
		FieldMask {FieldMask | state}
			= decodeOnServer state
		_	= Nothing

setImgState :: !Img !EditMask -> EditMask
setImgState img (CompoundMask entries)
	= CompoundMask (updateAt FM_IMG (FieldMask (mkFieldMask (Just img))) entries)

clearImgState :: !EditMask -> EditMask
clearImgState (CompoundMask entries)
	= CompoundMask (updateAt FM_IMG (FieldMask (mkFieldMask Nothing)) entries)

getImgTablesState :: !EditMask -> Maybe (ImgTables v) | JSDecode{|*|} v
getImgTablesState (CompoundMask entries)
	= case entries !! FM_TABLES of
		FieldMask {FieldMask | state}
			= decodeOnServer state
		_	= Nothing

setImgTablesState :: !(ImgTables v) !EditMask -> EditMask | JSEncode{|*|} v
setImgTablesState tables (CompoundMask entries)
	= CompoundMask (updateAt FM_TABLES (FieldMask (mkFieldMask (Just tables))) entries)

clearImgTablesState :: !EditMask -> EditMask
clearImgTablesState (CompoundMask entries)
	= CompoundMask (updateAt FM_TABLES (FieldMask (mkFieldMask Nothing)) entries)*/

imgTagSource :: !String -> *TagSource
imgTagSource taskId
  = [(ImageTagUser no taskId, ImageTagUser no taskId) \\ no <- [0..]]

newImgTables :: ImgTables m
newImgTables
  = {ImgTables | imgEventhandlers = 'DM'.newMap
               , imgNewFonts      = 'DS'.newSet
               , imgNewTexts      = 'DM'.newMap
               , imgMasks         = 'DM'.newMap
               , imgLineMarkers   = 'DM'.newMap
               , imgPaths         = 'DM'.newMap
               , imgSpans         = 'DM'.newMap
               , imgGrids         = 'DM'.newMap
               , imgTags          = 'DM'.newMap
               , imgUniqIds       = 0
    }

//	collection of SVG attributes required for the server-client communication

//  svgBodyAttr: use this to provide the client with a complete SVG-rendering of an image
svgBodyAttr :: !SVGElt -> UIAttributes
svgBodyAttr svg = 'DM'.fromList [("svgBody",JSONString (browserFriendlySVGEltToString svg))]

svgEventhandlersAttr :: !ImgEventhandlers` -> UIAttributes
svgEventhandlersAttr es = 'DM'.fromList [("svgEventhandlers", encodeOnServer es)]

svgNewFontsAttr :: !ImgFonts -> UIAttributes
svgNewFontsAttr newFonts = 'DM'.fromList [("svgNewFonts", encodeOnServer ('DS'.toList newFonts))]

svgNewTextsAttr :: !ImgTexts -> UIAttributes
svgNewTextsAttr newTexts = 'DM'.fromList [("svgNewTexts", encodeOnServer [(fd,'DS'.toList texts) \\ (fd,texts) <- 'DM'.toList newTexts])]

//	transform the functions of an SVGEditor into an Editor:
fromSVGEditor :: (SVGEditor s v) -> Editor s | iTask s & JSEncode{|*|}, JSDecode{|*|} s
fromSVGEditor svglet
  = { Editor
    | genUI     = withClientSideInit initClientSideUI initServerSideUI
    , onEdit    = onEditFromClient
    , onRefresh = onEditFromContext
    }
where
//	initServerSideUI is called first.
//	It provides initial information to the client via the attributes of the client component.
//	initServerSideUI :: !DataPath !s !*VSt -> *(!MaybeErrorString (!UI,!EditMask), !*VSt) | iTask s & JSEncode{|*|} s
	initServerSideUI dp val world=:{VSt | taskId}
	  = case serverSVG svglet 'DM'.newMap 'DM'.newMap taskId val (svglet.initView val) of			// start to generate the image server-side
	      Left (img,tables=:{ImgTables | imgNewFonts=new_fonts,imgNewTexts=new_txts})				// image incomplete because of missing font/text-width information
		    #! attrs = 'DM'.unions (size_and_model
		                            ++
		                            if ('DS'.null new_fonts) [] [svgNewFontsAttr new_fonts]			// the fonts for which metrics still need to be determined
		                            if ('DM'.null new_txts)  [] [svgNewTextsAttr new_txts] 			// the texts for which metrics still need to be determined
		                           )
		    = (Ok (uia UIComponent attrs,state), world)
	      Right (svg,es)																			// image complete, send it to client
		    #! attrs = 'DM'.unions (size_and_model
		                            ++ [svgBodyAttr svg]                                            // the complete SVG body
		                            ++ [svgEventhandlersAttr (defuncImgEventhandlers es)]           // the defunctionalized image event handlers
		                           )
		    = (Ok (uia UIComponent attrs,state), world)
	where
		state          = setTextsCache 'DM'.newMap (setFontsCache 'DM'.newMap initServerSideState)
		size_and_model = [sizeAttr FlexSize FlexSize, valueAttr (encodeOnServer val)]

//	initClientSideUI is called after initServerSideUI.
//	Information exchange from server -> client occurs via the attributes of the client object.
//  First initDOMEl initialises the client. Subsequent changes are handled with onAttributeChange.
//	Information exchange from client -> server occurs via `doEditEvent` that emits a triplet (taskId,editorId,json) in which json 
//	is the serialized data that the client sends to the server. The server receives this serialized data via onEditFromClient.
	initClientSideUI :: !(JSObj ()) !*JSWorld -> *JSWorld
	initClientSideUI me world
	// Set attributes
      #! world                       = (me .# "clickCount" .= (toJSVal 0)) world
	  #! world                       = jsPutCleanVal "dragState" initDragState me world
	// Set methods	
	  #! (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
	  #! world                       = (me .# "onAttributeChange" .= jsOnAttributeChange) world
	  #! (jsInitDOMEl,world)         = jsWrapFun (initDOMEl me) world
	  #! world                       = (me .# "initDOMEl" .= jsInitDOMEl) world
	// Initialize caches
	  #! world                       = jsPutCleanVal JS_ATTR_FONT_SPANS 'DM'.newMap me world   // initialize font spans cache
	  #! world                       = jsPutCleanVal JS_ATTR_TEXT_SPANS 'DM'.newMap me world   // initialize text-widths cache
	  = world
	where
		initDragState                = {SVGDragState | svgMousePos     = MouseUp
		                                             , svgDropCallback = \_ _ v -> v
		                                             , svgTrueCoordsX  = 0.0
		                                             , svgTrueCoordsY  = 0.0
		                                             , svgGrabPointX   = 0.0
		                                             , svgGrabPointY   = 0.0
		                                             , svgDragTarget   = Nothing
		                               }
		
		initDOMEl me args world
		  #! (value,world) = .? (me .# "attributes.value") world
		  #! (value,world) = decodeOnClient value world
		  = (jsNull,onNewState svglet me value world)
	
		onAttributeChange me args world
		| jsArgToString (args !! 0) == "stateChange"
		  #! (value,world) = decodeOnClient (toJSVal (args !! 1)) world
		  = (jsNull,onNewState svglet me value world)
		| otherwise
		  = (jsNull,jsTrace "Unknown attribute change" world)

//	onEditFromClient is called at the server side whenever the associated client component has evaluated `doEditEvent`.
//	The server component deserializes the received json data to determine the proper action.
  	onEditFromClient :: !DataPath !(!DataPath,!JSONNode) !s !EditMask !*VSt -> (!MaybeErrorString (!UIChange,!EditMask), !s, !*VSt) | iTask s & JSEncode{|*|} s
  	onEditFromClient _ (_,json) st m vst 
	  = case fromJSON json of 	
	      Just nst = (Ok (NoChange,m),nst,vst)
	      Nothing  = (Ok (NoChange,m),st, vst)

//	onEditFromContext is called at the server side whenever the context has acquired a new data model that needs to be rendered at the associated client component.	
//	This information is passed to the associated client via its attributes, and will be handled via the `onAttributeChange` function.
  	onEditFromContext :: !DataPath !s !s !EditMask !*VSt -> (!MaybeErrorString (!UIChange,!EditMask), !s, !*VSt) | iTask s & JSEncode{|*|} s
  	onEditFromContext _ new old mask vst 
	  = (Ok (if (gEq{|*|} old new) NoChange (ChangeUI [SetAttribute "stateChange" (encodeOnServer new)] []),mask),new,vst)

//	server side rendering of model value:
serverSVG :: !(SVGEditor s v) !FontSpans !TextSpans !String !s !v -> Either (!Img,!ImgTables v) (!SVGElt,!ImgEventhandlers v)
serverSVG {SVGEditor | renderImage} font_spans text_spans taskId s v
  #! image`               = renderImage s v (imgTagSource taskId)
  #! (img,tables=:{ImgTables | imgNewFonts=new_fonts,imgNewTexts=new_txts})
                          = toImg image` font_spans text_spans newImgTables
  | not ('DS'.null new_fonts) || not ('DM'.null new_txts)                    // some font / text-width information is missing: need to ask the client
      = Left (img,tables)
  #! {ImgTables | imgEventhandlers=es,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags}
                          = tables
  = case resolve_all_spans tags font_spans text_spans img masks markers paths spans grids of
      Error error         = abort error
      Ok (img,masks,markers,paths,spans,grids)
        #! svg            = genSVGElt img taskId ('DM'.keys es) masks markers paths spans grids
        = Right (svg,es)

//	client side rendering of model value:
onNewState :: !(SVGEditor s v) !(JSVal a) !s !*JSWorld -> *JSWorld | JSONEncode{|*|} s
onNewState svglet=:{SVGEditor | initView,renderImage} me s world
  #! (cidJS,world)        = .? (me .# "attributes.taskId") world
  #! taskId               = jsValToString cidJS
  #! v                    = initView s
  #! world                = jsPutCleanVal JS_ATTR_VIEW  v me world           // Store the view value on the component
  #! world                = jsPutCleanVal JS_ATTR_MODEL s me world           // Store the model value on the component
  #! (font_spans,world)   = jsGetCleanVal JS_ATTR_FONT_SPANS me world        // Load the cached font spans
  #! (text_spans,world)   = jsGetCleanVal JS_ATTR_TEXT_SPANS me world        // Load the cached text width spans
  #! image`               = renderImage s v (imgTagSource taskId)
  #! (img,{ImgTables | imgEventhandlers=es,imgNewFonts=new_fonts,imgNewTexts=new_txts,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags})
                          = toImg image` font_spans text_spans newImgTables
  #! (font_spans,world)   = addNewFontSpans  new_fonts font_spans me world   // Add missing font spans to cached font spans
  #! (text_spans,world)   = addNewTextsSpans new_txts  text_spans me world   // Add missing text width spans to cached text width spans
  = case resolve_all_spans tags font_spans text_spans img masks markers paths spans grids of
      Error error         = abort error
      Ok (img,masks,markers,paths,spans,grids)
        #! svg            = genSVGElt img taskId ('DM'.keys es) masks markers paths spans grids
        #! svgStr         = browserFriendlySVGEltToString svg
        #! (newSVG,world) = updSVGString svgStr me world
        #! world          = registerEventhandlers svglet me taskId newSVG es tags world
        = world

//	generate the entire SVG element from an Img with all spans resolved:
genSVGElt :: !Img !String ![ImgTagNo] !ImgMasks !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> SVGElt
genSVGElt img taskId interactive_imgs masks markers paths spans grids
  #! mask_defs      = genSVGMasks masks taskId interactive_imgs markers paths spans grids
  #! svg_elems      = genSVGElts  img   taskId interactive_imgs markers paths spans grids
  #! (imXSp,imYSp)  = getImgRootSize img spans
  #! imXSp          = to2decString imXSp
  #! imYSp          = to2decString imYSp
  = SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns] [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp] (mask_defs ++ svg_elems)

//	update the DOM element with the new SVG content, represented as a string:
updSVGString :: !String !(JSVal a) !*JSWorld -> (!JSObj svg,!*JSWorld)
updSVGString svgStr me world
  #! (parser, world) = new "DOMParser" () world
  #! (doc,    world) = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
  #! (newSVG, world) = .? (doc .# "firstChild") world
  #! (domEl,  world) = .? (me .# "domEl") world
  #! (currSVG,world) = .? (domEl .# "firstChild") world
  #! (_,      world) = if (jsIsNull currSVG)
                          ((domEl `appendChild` newSVG) world)
                          ((domEl .# "replaceChild" .$ (newSVG, currSVG)) world)
  = (newSVG,world)

//	return the dimensions of the root image:
getImgRootSize :: !Img !ImgSpans -> (!Real,!Real)
getImgRootSize img=:{Img | uniqId} spans
	= case 'DM'.find uniqId spans of
	    (PxSpan w,PxSpan h) = (w,h)
	    _                   = abort "Unexpected error in module SVGEditor (getImgRootSize): size of root image is undetermined."

//	generate the svg-defs for the masks used in this image:
genSVGMasks :: !ImgMasks !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
genSVGMasks masks taskId es markers paths spans grids
	= [  DefsElt [] [] [MaskElt [IdAttr (mkMaskId taskId no)] [] (genSVGElts m taskId es markers paths spans grids)]
	  \\ (no,m) <- 'DM'.toList masks
	  ]

//	measure new font dimensions and add them to the known set of font dimensions:	
addNewFontSpans :: !ImgFonts !FontSpans !(JSVal a) !*JSWorld -> (!FontSpans,!*JSWorld)
addNewFontSpans newFonts font_spans me world
  | 'DS'.null newFonts  = (font_spans,world)
  #! (font_spans,world) = calcImgFontsSpans newFonts font_spans world
  #! world              = jsPutCleanVal JS_ATTR_FONT_SPANS font_spans me world
  = (font_spans, world)
where
// compute the font dimensions of new fonts that are used in an image, and add them to the known font dimensions
	calcImgFontsSpans :: !ImgFonts !FontSpans !*JSWorld -> (!FontSpans,!*JSWorld)
	calcImgFontsSpans new_fonts font_spans world
	  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
	  #! (body,world) = .? (jsDocument .# "body") world
	  #! (_,   world) = (body `appendChild` svg) world
	  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
	  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
	  #! (_,   world) = (svg `appendChild` elem) world
	  #! (res, world) = foldl (calcFontSpan elem) (font_spans,world) ('DS'.toList new_fonts)
	  #! (_,   world) = (svg `removeChild` elem) world
	  #! (_,   world) = (body `removeChild` svg) world
	  = (res,  world)
	where
		calcFontSpan :: !(JSVal (JSObject a)) !*(!FontSpans,!*JSWorld) !FontDef -> *(!FontSpans,!*JSWorld)
		calcFontSpan elem (font_spans, world) fontdef
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
		  #! (fd, world) = calcFontDescent elem fontdef.fontysize world
		  = ('DM'.put fontdef fd font_spans, world)
		
		calcFontDescent :: !(JSVal (JSObject a)) !Real !*JSWorld -> (!Real, !*JSWorld)
		// same heuristic as used below (at function 'genSVGBasicImage'), must be replaced by proper determination of descent of current font
		calcFontDescent elem fontysize world
		  = (fontysize * 0.25,world)

//	measure new text dimensions and add them to the known set of text dimensions:	
addNewTextsSpans :: !ImgTexts !TextSpans !(JSVal a) !*JSWorld -> (!TextSpans,!*JSWorld)
addNewTextsSpans newTexts text_spans me world
  | 'DM'.null newTexts  = (text_spans,world)
  #! (text_spans,world) = calcImgTextsLengths newTexts text_spans world
  #! world              = jsPutCleanVal JS_ATTR_TEXT_SPANS text_spans me world
  = (text_spans, world)
where
// compute the string widths of new texts that are used in an image, and add them to the known collection of string widths
	calcImgTextsLengths :: !ImgTexts !TextSpans !*JSWorld -> (!TextSpans, !*JSWorld)
	calcImgTextsLengths texts text_spans world
	  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
	  #! (body,world) = .? (jsDocument .# "body") world
	  #! (_,   world) = (body `appendChild` svg) world
	  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
	  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
	  #! (_,   world) = (svg `appendChild` elem) world
	  #! (res, world) = 'DM'.foldrWithKey (calcTextLengths elem) (text_spans, world) texts
	  #! (_,   world) = (svg `removeChild` elem) world
	  #! (_,   world) = (body `removeChild` svg) world
	  = (res,  world)
	where
		calcTextLengths :: !(JSVal (JSObject a)) !FontDef !(Set String) !*(!TextSpans, !*JSWorld) -> *(!TextSpans, !*JSWorld)
		calcTextLengths elem fontdef strs (text_spans, world)
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
		  #! (ws, world) = 'DS'.fold (calcTextLength elem) ('DM'.newMap, world) strs
		  = ('DM'.alter (merge ws) fontdef text_spans, world)
		where
			merge :: !(Map String TextSpan) !(Maybe (Map String TextSpan)) -> Maybe (Map String TextSpan)
			merge ws` (Just ws) = Just ('DM'.union ws` ws)
			merge ws` nothing   = Just ws`
		
		calcTextLength :: !(JSVal (JSObject a)) !String !*(!Map String TextSpan, !*JSWorld) -> *(!Map String TextSpan, !*JSWorld)
		calcTextLength elem str (text_spans, world)
		  #! world        = (elem .# "textContent" .= str) world
		  #! (ctl, world) = (elem `getComputedTextLength` ()) world
		  = ('DM'.put str (jsValToReal ctl) text_spans, world)

//	register the event handlers of the img:
registerEventhandlers :: !(SVGEditor s v) !(JSVal a) !String !(JSObj svg) !(ImgEventhandlers v) !ImgTags !*JSWorld -> *JSWorld | JSONEncode{|*|} s
registerEventhandlers svglet me taskId svg es tags world
  #! (domEl,  world)     = .? (me .# "domEl") world
  #! (svgRoot,world)     = .? (domEl .# "firstChild") world
  #! idMap               = invertToMapSet (fmap (mkUniqId taskId) tags)
// all draggable elements share a common mousemove and mouseup event:
  #! (cbMove, world)     = jsWrapFun (doMouseDragMove svglet me svgRoot) world
  #! (cbUp,   world)     = jsWrapFun (doMouseDragUp   svglet me svgRoot idMap) world
  #! (_,      world)     = (svgRoot `addEventListener` ("mousemove", cbMove, True)) world
  #! (_,      world)     = (svgRoot `addEventListener` ("mouseup",   cbUp,   True)) world
// register all individual event handlers:
  = 'DM'.foldrWithKey (registerEventhandler svglet me svg) world es
where
	registerEventhandler :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !ImgTagNo ![ImgEventhandler v] !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	registerEventhandler svglet me svg uniqId es world = foldr (register svglet me svg (mkUniqId taskId uniqId)) world es
	where
		register :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(ImgEventhandler v) !*JSWorld -> *JSWorld | JSONEncode{|*|} s
		register svglet me svg elemId (ImgEventhandlerOnClickAttr {OnClickAttr | local,onclick}) world
			= registerNClick svglet me svg elemId onclick local world
		register svglet me svg elemId (ImgEventhandlerOnMouseDownAttr {OnMouseDownAttr | local,onmousedown}) world
			= actuallyRegister svglet me svg elemId "mousedown" onmousedown local world
		register svglet me svg elemId (ImgEventhandlerOnMouseUpAttr {OnMouseUpAttr | local,onmouseup}) world
			= actuallyRegister svglet me svg elemId "mouseup" onmouseup local world
		register svglet me svg elemId (ImgEventhandlerOnMouseOverAttr {OnMouseOverAttr | local,onmouseover}) world
			= actuallyRegister svglet me svg elemId "mouseover" onmouseover local world
		register svglet me svg elemId (ImgEventhandlerOnMouseMoveAttr {OnMouseMoveAttr |local,onmousemove}) world
			= actuallyRegister svglet me svg elemId "mousemove" onmousemove local world
		register svglet me svg elemId (ImgEventhandlerOnMouseOutAttr  {OnMouseOutAttr |local,onmouseout}) world
			= actuallyRegister svglet me svg elemId "mouseout"  onmouseout  local world
		register svglet me svg elemId (ImgEventhandlerDraggableAttr {DraggableAttr | draggable}) world
			= registerDraggable svglet me svg elemId draggable world

actuallyRegister :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !String !(v -> v) !Bool! *JSWorld -> *JSWorld | JSONEncode{|*|} s
actuallyRegister svglet me svg elemId evt sttf local world
  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
  #! (cb,  world) = jsWrapFun (doImageEvent svglet me svg elemId sttf local) world
  #! (_,   world) = (elem `addEventListener` (evt, cb, True)) world
  = world

doImageEvent :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(v -> v) !Bool ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
doImageEvent svglet me svg elemId sttf local _ world
// Get model & view value 
  #! (view, world)    = jsGetCleanVal JS_ATTR_VIEW  me world
  #! (model,world)    = jsGetCleanVal JS_ATTR_MODEL me world
// Update the view & the model
  #! view             = sttf view
  #! model            = svglet.SVGEditor.updModel model view
  #! world            = jsPutCleanVal JS_ATTR_VIEW  view  me world
  #! world            = jsPutCleanVal JS_ATTR_MODEL model me world
// If not local, fire an itasks edit event 
  | local
// Don't trigger an event, just re-render
  	= (jsNull,onNewState svglet me model world)
// Send edit event
  #! (json,    world) = (jsWindow .# "JSON.parse" .$ (toString (toJSON model))) world //TODO: Should not really print+parse here
  #! (taskId,  world) = .? (me .# "attributes.taskId") world
  #! (editorId,world) = .? (me .# "attributes.editorId") world
  #! (_,       world) = (me .# "doEditEvent" .$ (taskId,editorId,json)) world
// Re-render
  = (jsNull,onNewState svglet me model world)

registerNClick :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(Int v -> v) !Bool !*JSWorld -> *JSWorld | JSONEncode{|*|} s
registerNClick svglet me svg elemId sttf local world
  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
  #! (cb,  world) = jsWrapFun (mkNClickCB svglet me svg elemId sttf local) world
  #! (_,   world) = (elem `addEventListener` ("click", cb, False)) world
  = world

mkNClickCB :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(Int v -> v) !Bool ![JSArg] !*JSWorld-> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
mkNClickCB svglet me svg elemId sttf local args world
  #! world           = case args of [a:_] = snd (((toJSVal a) .# "stopPropagation" .$ ()) world) ; _ = world
// If another click already registered a timeout, clear that timeout
  #! (to,world)      = .? (me .# "clickTimeOut") world
  #! world           = if (jsIsUndefined to || jsIsNull to) world (snd (("clearTimeout" .$ to) world))
// Register a callback for the click after a small timeout
  #! (cb,world)      = jsWrapFun (doNClickEvent svglet me svg elemId sttf local) world
  #! (to,world)  	 =  ("setTimeout" .$ (cb, CLICK_DELAY)) world
  #! world           = (me .# "clickTimeOut" .= to) world
// Increase click counter, so we can determine how many times the element was clicked when the timeout passes
  #! (nc,world)      = .? (me .# "clickCount") world
  #! world           = (me .# "clickCount" .= (toJSVal (jsValToInt nc + 1))) world
  = (jsNull,world)

doNClickEvent :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(Int v -> v) !Bool ![JSArg] !*JSWorld-> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
doNClickEvent svglet me svg elemId sttf local args world
// Get click count
  #! (nc,world)      = .? (me .# "clickCount") world
// Reset click count
  #! world           = (me .# "clickCount" .= (toJSVal 0)) world
  #! nc              = jsValToInt nc
  = doImageEvent svglet me svg elemId (sttf nc) local args world

registerDraggable :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !String !(SVGDragFun v) !*JSWorld -> *JSWorld
registerDraggable svglet me svg elemId f world
  #! (elem,  world) = (svg .# "getElementById" .$ elemId) world
  #! (cbDown,world) = jsWrapFun (doMouseDragDown svglet me svg f elemId elem) world
  #! (_,     world) = (elem `addEventListener` ("mousedown", cbDown, True)) world
  = world

doMouseDragDown :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !(SVGDragFun v) !String !(JSObj o) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld)
doMouseDragDown svglet me svgRoot sttf elemId elem args world
  #! (ds,           world) = jsGetCleanVal "dragState" me world
  #! (targetElement,world) = (svgRoot .# "getElementById" .$ elemId) world
  #! (_,            world) = (targetElement .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (boundingRect, world) = (targetElement .# "getBoundingClientRect" .$ ()) world
  #! (left,         world) = .? (boundingRect .# "left") world
  #! (top,          world) = .? (boundingRect .# "top") world
  #! (p,            world) = (svgRoot `createSVGPoint` ()) world
  #!                world  = (p .# "x" .= left) world
  #!                world  = (p .# "y" .= top) world
  #! (m,            world) = (svgRoot `getScreenCTM` ()) world
  #! (inv,          world) = (m `inverse` ()) world
  #! (p,            world) = (p `matrixTransform` inv) world
  #! (px,           world) = .? (p .# "x") world
  #! (py,           world) = .? (p .# "y") world
  #! (e,f)                 = (jsValToReal px, jsValToReal py)
  #! ds                    = { SVGDragState 
                             | ds & svgDropCallback = sttf
                                  , svgMousePos     = MouseDown
                                  , svgDragTarget   = Just targetElement
                                  , svgGrabPointX   = ds.SVGDragState.svgTrueCoordsX - e
                                  , svgGrabPointY   = ds.SVGDragState.svgTrueCoordsY - f
                             }
  #!                world  = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragMove :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragMove svglet me svgRoot args world
  #! (ds,world)      = jsGetCleanVal "dragState" me world
  #! evt             = toJSVal (args !! 0)
  #! (newTrueCoordsX, newTrueCoordsY, world)
                     = getNewTrueCoords me evt world
  | not (gEq{|*|} ds.SVGDragState.svgMousePos MouseDown) || ds.SVGDragState.svgDragTarget =: Nothing
 	#! ds            = { SVGDragState 
 	                   | ds & svgTrueCoordsX = newTrueCoordsX
 	                        , svgTrueCoordsY = newTrueCoordsY
 	                   }
    #! world         = jsPutCleanVal "dragState" ds me world
    = (jsNull,world)
  #! dragTarget      = fromJust ds.SVGDragState.svgDragTarget
  #! (domEl,  world) = .? (me .# "domEl") world
  #! (svgRoot,world) = .? (domEl .# "firstChild") world
// Append the dragTarget to the root of the SVG element for two reasons:
//   1. To allow it to be dragged over all other elements
//   2. To not be bothered by the offsets of one or more groups it might initially be in
  #! (_, world)     = (svgRoot `appendChild` dragTarget) world
  #! newX           = newTrueCoordsX - ds.SVGDragState.svgGrabPointX
  #! newY           = newTrueCoordsY - ds.SVGDragState.svgGrabPointY
  #! (_, world)     = (dragTarget `setAttribute` ("transform", "translate(" +++ toString newX +++ "," +++ toString newY +++ ")")) world
  #! ds             = { SVGDragState
                      | ds & svgTrueCoordsX = newTrueCoordsX
                           , svgTrueCoordsY = newTrueCoordsY
                      }
  #! world          = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragUp :: !(SVGEditor s v) !(JSVal a) !(JSObj svg) !(Map String (Set ImageTag)) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragUp svglet me svgRoot idMap args world
  #! evt               = toJSVal (args !! 0)
  #! (ds,world)        = jsGetCleanVal "dragState" me world
  | ds.SVGDragState.svgDragTarget =: Nothing
    #! ds              = { SVGDragState
                         | ds & svgMousePos   = MouseUp
                              , svgDragTarget = Nothing
                         }
    #! world           = jsPutCleanVal "dragState" ds me world
  	= (jsNull,world)
  #! (evtTarget,world) = .? (evt .# "target") world
  #! dragTarget        = fromJust ds.SVGDragState.svgDragTarget
  #! (_, world)        = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (parentId, world) = firstIdentifiableParentId evtTarget world
// Get model & view value 
  #! (view, world)     = jsGetCleanVal JS_ATTR_VIEW me world
  #! (model,world)     = jsGetCleanVal JS_ATTR_MODEL me world
  #! xdiff             = ds.SVGDragState.svgTrueCoordsX - ds.SVGDragState.svgGrabPointX
  #! ydiff             = ds.SVGDragState.svgTrueCoordsY - ds.SVGDragState.svgGrabPointY
  #! view              = ds.SVGDragState.svgDropCallback ('DM'.findWithDefault 'DS'.newSet parentId idMap) (xdiff,ydiff) view 
  #! model             = svglet.SVGEditor.updModel model view
  #! ds                = { SVGDragState
                         | ds & svgMousePos   = MouseUp
                              , svgDragTarget = Nothing
                         }
  #! world             = jsPutCleanVal JS_ATTR_VIEW  view  me world
  #! world             = jsPutCleanVal JS_ATTR_MODEL model me world
  #! world             = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

firstIdentifiableParentId :: !(JSObj a) !*JSWorld -> *(!String, !*JSWorld)
firstIdentifiableParentId elem world
  #! (idval,world)      = .? (elem .# "id") world
  | jsIsNull idval
      #! (parent,world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  #! idval = jsValToString idval
  | idval == ""
      #! (parent,world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  | otherwise
      = (idval, world)

getNewTrueCoords :: !(JSVal a) !(JSObj JSEvent) !*JSWorld -> *(!Real, !Real, !*JSWorld)
getNewTrueCoords me evt world
  #! (domEl,       world) = .? (me .# "domEl") world
  #! (svgRoot,     world) = .? (domEl .# "firstChild") world
  #! (newScale,    world) = .? (svgRoot .# "currentScale") world
  #! newScale             = jsValToReal newScale
  #! (translation, world) = .? (svgRoot .# "currentTranslate") world
  #! (translationX,world) = .? (translation .# "x") world
  #! (translationY,world) = .? (translation .# "y") world
  #! (clientX,     world) = .? (evt .# "clientX") world
  #! (clientY,     world) = .? (evt .# "clientY") world
  #! newTrueCoordsX       = ((jsValToReal clientX) - (jsValToReal translationX)) / newScale
  #! newTrueCoordsY       = ((jsValToReal clientY) - (jsValToReal translationY)) / newScale
  = (newTrueCoordsX, newTrueCoordsY, world)

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

mkMaskId :: !String !Int -> String
mkMaskId editletId uniqId = "maskId-" +++ editletId +++ toString uniqId

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkUniqId :: !String !Int -> String
mkUniqId editletId uniqId = "uniqId-" +++ editletId +++ toString uniqId

mkUrl :: !String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (to2decString imXSp), HeightAttr (to2decString imYSp)]

to2decString :: !Real -> String
to2decString r = toString (toReal (toInt (r * 100.0)) / 100.0)

genSVGElts :: !Img !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
genSVGElts {Img | uniqId, transform, host, overlays, offsets} taskId es markers paths spans grids
	= mkGroup (if interactive [IdAttr (mkUniqId taskId uniqId)] [])
	          (genSVGTransform host transform spans taskId)
	          (  genSVGHost     uniqId   host    taskId es markers paths spans grids 
	          ++ genSVGOverlays overlays offsets taskId es markers paths spans grids
	          )
where
	interactive = isMember uniqId es
	
	genSVGTransform :: !HostImg !(Maybe ImgTransform) !ImgSpans !String -> [SVGAttr]
	genSVGTransform (CompositeImg img) (Just tf) spans taskId
		= [genTransform isTextHost imgSpan tf taskId]
	where
		isTextHost  = case img.Img.host of
		                BasicHostImg (TextImg _ _) _ = True
		                _                            = False
		imgSpan     = case 'DM'.get img.Img.uniqId spans of
			            Just (PxSpan w, PxSpan h) = (w,h)
			            Just _                    = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unresolvedErrorMsg  "image")
			            nothing                   = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unavailableErrorMsg "image")
		
		genTransform :: !Bool !ImageSpanReal !ImgTransform !String -> SVGAttr
		genTransform isText (xsp, ysp) (RotateImg imAn) _
		// FIXME: We currently divide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
			#! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
			= TransformAttr [RotateTransform (to2decString (toDeg imAn)) (Just (to2decString (xsp / 2.0), to2decString yoff))]
		genTransform _ _ (SkewXImg imAn) _
			= TransformAttr [SkewXTransform (toString (toDeg imAn))]
		genTransform _ _ (SkewYImg imAn) _
			= TransformAttr [SkewYTransform (toString (toDeg imAn))]
		genTransform isText (xsp, ysp) (FitImg spx spy) _
			| isText     = TransformAttr [translate, scale]
			| otherwise  = TransformAttr            [scale]
		where
			(fx,fy)      = case (spx,spy) of
			                 (PxSpan rx, PxSpan ry) = (to2decString (rx / xsp), to2decString (ry / ysp))
			                 _                      = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg  "fit"))
			translate    = TranslateTransform "0" (toString ysp)
			scale        = ScaleTransform fx fy
		genTransform isText (xsp, ysp) (FitXImg sp) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			fx          = case sp of
			                PxSpan rx = rx / xsp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fitx"))
			fxy         = if (xsp > 0.0) (to2decString fx) "1.0"
			translate   = TranslateTransform "0" (to2decString (ysp * 0.7 * fx))
			scale       = ScaleTransform fxy fxy
		genTransform isText (xsp, ysp) (FitYImg sp) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			fy          = case sp of
			                PxSpan ry = ry / ysp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fity"))
			fxy         = if (ysp > 0.0) (to2decString fy) "1.0"
			translate   = TranslateTransform "0" (toString ysp)
			scale       = ScaleTransform fxy fxy
		genTransform isText (_, ysp) (ScaleImg fx fy) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			translate   = TranslateTransform "0" (toString ysp)
			scale       = ScaleTransform (to2decString fx) (to2decString fy)
		genTransform isText (xsp, ysp) FlipXImg _
			= TransformAttr [TranslateTransform (to2decString xsp) "0", ScaleTransform "-1" "1"]
		genTransform isText (xsp, ysp) FlipYImg _
			= TransformAttr [TranslateTransform "0" (to2decString ysp`), ScaleTransform "1" "-1"]
		where
			ysp`        = if isText ((~ ysp) * 0.7) ysp
		genTransform _ _ (MaskImg uniqId) taskId
			= MaskAttr (mkUrl (mkMaskId taskId uniqId))
	genSVGTransform _ _ _ _
		= []
	
	genSVGHost :: !ImgTagNo !HostImg !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGHost no host=:(BasicHostImg basic attrs) taskId es markers paths spans grids
		= genSVGBasicHostImg no basic (genSVGImageAttrs attrs) taskId es markers paths spans grids
	where
		genSVGImageAttrs :: !(Set BasicImgAttr) -> [SVGAttr]
		genSVGImageAttrs atts = strictTRMap genSVGImageAttr ('DS'.toList atts)
		where
			genSVGImageAttr :: !BasicImgAttr -> SVGAttr
			genSVGImageAttr (BasicImgStrokeAttr      color)      = StrokeAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgStrokeWidthAttr (PxSpan w)) = StrokeWidthAttr (StrokeWidthLength (toString w, PX))
			genSVGImageAttr (BasicImgXRadiusAttr     (PxSpan r)) = RxAttr (toString r, PX)
			genSVGImageAttr (BasicImgYRadiusAttr     (PxSpan r)) = RyAttr (toString r, PX)
			genSVGImageAttr (BasicImgStrokeOpacityAttr op)       = StrokeOpacityAttr (toString op)
			genSVGImageAttr (BasicImgFillOpacityAttr   op)       = FillOpacityAttr (FillOpacity (toString op))
			genSVGImageAttr (BasicImgFillAttr        color)      = FillAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgDashAttr        dash)       = StrokeDashArrayAttr (DashArray (strictTRMap toString dash))
			genSVGImageAttr _ = abort "Unexpected error in module SVGEditor (local function genSVGImageAttr of genSVGElts): unresolved span value encountered."
	genSVGHost no host=:(RawHostImg content) taskId es markers paths spans grids
		= [RawElt content]
	genSVGHost no host=:(CompositeImg img) taskId es markers paths spans grids
		= genSVGElts img taskId es markers paths spans grids
	
	genSVGBasicHostImg :: !ImgTagNo !BasicImg ![SVGAttr] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGBasicHostImg no EmptyImg attrs taskId es markers paths spans grids
		= []
	genSVGBasicHostImg no (TextImg {fontfamily,fontysize,fontstyle,fontstretch,fontvariant,fontweight} txt) attrs taskId es markers paths spans grids
		= [TextElt [XmlspaceAttr "preserve"] (keepTransformAttrsTogether (TransformAttr [TranslateTransform (toString 0.0) (toString (fontysize * 0.75))]) (attrs ++ fontAttrs)) txt]
    where
		fontAttrs = [ AlignmentBaselineAttr "auto"
		            , DominantBaselineAttr  "auto"
		            , FontFamilyAttr        fontfamily
		            , FontSizeAttr          (toString fontysize)
		            , FontStyleAttr         fontstyle
		            , FontStretchAttr       fontstretch
		            , FontVariantAttr       fontvariant
		            , FontWeightAttr        fontweight
		            , TextRenderingAttr     "geometricPrecision"
		            ]
	genSVGBasicHostImg no RectImg attrs taskId es markers paths spans grids
		= [RectElt sizeAtts attrs]
	where
		sizeAtts          = case 'DM'.get no spans of
		                      Just (PxSpan w, PxSpan h) = mkWH (w,h)
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "rect"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "rect"))
	genSVGBasicHostImg no CircleImg attrs taskId es markers paths spans grids
		= [CircleElt [] [RAttr (radius,PX), CxAttr (radius,PX), CyAttr (radius,PX) : attrs]]
	where
		radius            = case 'DM'.get no spans of
		                      Just (PxSpan w,h)         = to2decString (w / 2.0)
		                      Just (_,_)                = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "circle"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "circle"))
	genSVGBasicHostImg no EllipseImg attrs taskId es markers paths spans grids
		= [EllipseElt [] [RxAttr (xradius,PX), CxAttr (xradius,PX), RyAttr (yradius,PX), CyAttr (yradius,PX) : attrs]]
	where
		(xradius,yradius) = case 'DM'.get no spans of
		                      Just (PxSpan w, PxSpan h) = (to2decString (w / 2.0), to2decString (h / 2.0))
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "ellipse"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "ellipse"))
	genSVGBasicHostImg no PolylineImg attrs taskId es markers` paths spans grids
		= [ PolylineElt [] [PointsAttr (strictTRMap (polypointToPointsAttr "polyline") points) : attrs ++ markerAttrs]
		  : map (\elt -> DefsElt [] [] [elt]) markerElts		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
		  ]
	where
		markers                   = case 'DM'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'DM'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polyline"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polyline" markers taskId es markers` paths spans grids)
	genSVGBasicHostImg no PolygonImg attrs taskId es markers` paths spans grids
		= [ PolygonElt [] [PointsAttr (strictTRMap (polypointToPointsAttr "polygon") points) : attrs ++ markerAttrs]
		  : map (\elt -> DefsElt [] [] [elt]) markerElts		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
		  ]
	where
		markers                   = case 'DM'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'DM'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polygon"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polygon" markers taskId es markers` paths spans grids)
	
	genSVGLineMarkers :: !String !LineMarkers !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [(SVGElt,SVGAttr)]
	genSVGLineMarkers elt {LineMarkers | lineStart,lineMid,lineEnd} taskId es markers paths spans grids
		= [  genSVGLineMarker elt img posAttr taskId es markers paths spans grids 
		  \\ (Just img,posAttr) <- [ (lineStart,MarkerStartAttr)
		                           , (lineMid,  MarkerMidAttr)
		                           , (lineEnd,  MarkerEndAttr)
		                           ]
		  ]
	where
		genSVGLineMarker :: !String !Img !(String -> SVGAttr) !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> (!SVGElt,!SVGAttr)
		genSVGLineMarker elt img=:{Img | uniqId} posAttr taskId es markers paths spans grids
			= ( MarkerElt [ IdAttr mid ]
			              [ OrientAttr       "auto"
                          , ViewBoxAttr      "0" "0" wStr hStr
                          , RefXAttr         (wStr, PX)
                          , RefYAttr         (to2decString (h / 2.0), PX)
                          , MarkerHeightAttr (hStr, PX)
                          , MarkerWidthAttr  (wStr, PX)
                          ]
                          (genSVGElts img taskId es markers paths spans grids)
			  , posAttr (mkUrl mid)
			  )
		where
			mid   = mkMarkerId taskId uniqId
			(w,h) = case 'DM'.get uniqId spans of
			          Just (PxSpan w, PxSpan h) = (w,h)
			          Just _                    = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unresolvedErrorMsg  elt))
			          nothing                   = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unavailableErrorMsg elt))
			wStr = to2decString w
        	hStr = to2decString h

	genSVGOverlays :: ![Img] ![ImageOffset] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGOverlays overlays offsets taskId es markers paths spans grids
		= flatten [mkGroup [] (mkTransformTranslateAttr off) (genSVGElts img taskId es markers paths spans grids) \\ img <- overlays & off <- offsets]
	where
		mkTransformTranslateAttr :: !ImageOffset -> [SVGAttr]
		mkTransformTranslateAttr (PxSpan dx,PxSpan dy)
		| dx == 0.0 && dy == 0.0   = []
		| otherwise                = [TransformAttr [TranslateTransform (to2decString dx) (to2decString dy)]]
		mkTransformTranslateAttr _ = abort (lookupSpanErrorMsg "genSVGOverlays" (unresolvedErrorMsg "Img"))
	
	polypointToPointsAttr :: !String !ImageOffset -> (!String,!String)
	polypointToPointsAttr elt (PxSpan dx,PxSpan dy) = (to2decString dx, to2decString dy)
	polypointToPointsAttr elt _                     = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg elt))
		
	unresolvedErrorMsg :: !String -> String
	unresolvedErrorMsg elt = "unresolved span value of " +++ elt +++ " encountered."
	
	unavailableErrorMsg :: !String -> String
	unavailableErrorMsg elt = "no span value exists for " +++ elt +++ "."
	
	lookupSpanErrorMsg :: !String !String -> String
	lookupSpanErrorMsg local_fun error = "Unexpected error in module SVGEditor (local function " +++ local_fun +++ " of genSVGElts): " +++ error

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _      _      []                  = []
mkGroup []     []     xs                  = xs
mkGroup hattrs []     [GElt [] sattrs xs] = [GElt hattrs sattrs xs]
mkGroup []     sattrs [GElt hattrs [] xs] = [GElt hattrs sattrs xs]
mkGroup []     [tfattr=:(TransformAttr [TranslateTransform x y])] xs = map f xs
where
  f :: !SVGElt -> SVGElt
  f (GElt        hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = GElt       hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs) elts
  f (GElt        hattrs attrs elts)                                              = GElt       hattrs (keepTransformAttrsTogether tfattr attrs) elts
  f (TextElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = TextElt    hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs) elts
  f (TextElt     hattrs attrs elts)                                              = TextElt    hattrs (keepTransformAttrsTogether tfattr attrs) elts
  f (EllipseElt  hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = EllipseElt hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (EllipseElt  hattrs attrs)                                                   = EllipseElt hattrs (keepTransformAttrsTogether tfattr attrs)
  f (RectElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = RectElt    hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (RectElt     hattrs attrs)                                                   = RectElt    hattrs (keepTransformAttrsTogether tfattr attrs)
  f (CircleElt   hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = CircleElt  hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (CircleElt   hattrs attrs)                                                   = CircleElt  hattrs (keepTransformAttrsTogether tfattr attrs)
  f (LineElt _ [X1Attr (x1, PX), X2Attr (x2, PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : attrs]) = LineElt [] [X1Attr (lineAdd x1 x, PX), X2Attr (lineAdd x2 x, PX), Y1Attr (lineAdd y1 y, PX), Y2Attr (lineAdd y2 y, PX) : attrs]
  f elt                                                                                      = GElt    [] [tfattr] [elt]

  lineAdd :: !String !SVGNumber -> String
  lineAdd strVal n = to2decString (toReal strVal + toReal n)
mkGroup has    sas elts = [GElt has sas elts]

dualTransformTranslate :: !a !a !a !a -> SVGAttr | toReal a
dualTransformTranslate x y x` y` = TransformAttr [TranslateTransform (to2decString (toReal x + toReal x`)) (to2decString (toReal y + toReal y`))]

// PA: this is rather cumbersome;
// better plan is to keep the SVG transforms separate when creating the Img and in the end do put them at the end of the [SVGAttr]-list where they seem to end up
keepTransformAttrsTogether :: !SVGAttr ![SVGAttr] -> [SVGAttr]
keepTransformAttrsTogether (TransformAttr tfs) attrs
	= filter (not o isTransformAttr) attrs ++ [TransformAttr (tfs ++ flatten [tfs` \\ TransformAttr tfs` <- attrs])]
keepTransformAttrsTogether attr attrs
	= [attr : attrs]

isTransformAttr :: !SVGAttr -> Bool
isTransformAttr (TransformAttr _) = True
isTransformAttr _ = False
