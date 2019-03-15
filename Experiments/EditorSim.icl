module EditorSim
/**
* Simulation of messages passing between editors to figure out the design details
*/
import iTasks
import Data.Maybe, Data.Either, Data.Tuple, Data.Functor, Data.List, Data.Func, Data.Error
import qualified Data.Map as DM
import Text, Text.HTML
from StdFunc import seqList, :: St(..)
from Data.Foldable import maximumBy
import StdArray
import Data.Map.GenJSON

import qualified Graphics.Scalable.Image as GI
from Graphics.Scalable.Image import :: FillAttr(..), <@<,  :: Image, :: Host(..)
from Graphics.Scalable.Image import class tuneImage, instance tuneImage FillAttr
from Graphics.Scalable.Image import class margin, instance margin (!Span, !Span, !Span, !Span), instance margin Span
import iTasks.Extensions.SVG.SVGEditor
import StdMisc, StdDebug

//TODOS:
// - Model an example with input field validation for mandatory fields etc. such as 'empty','validated', etc

//PROBLEMS:
// - Link methods not complete enough yet... (impossible to react to refreshes on server...)

//Definition of editors

// p: The parameter set (same as in sds)
// r: Data read from an external source (sds) based on p. Data will arrive with some delay and can be updated later
// w: Data to be written back to the external source (sds)

// s: Server-side state
// c: Client-side state
// m: Messages exchanged to synchronize between client and server (and potentially between sub editors)

:: NxtEditor p r w s c m =
  { client :: NxtEditorClient c m
  , server :: NxtEditorServer p r w s c m
  }

:: NxtEditorClient c m =
  { init      :: (Maybe c) -> NxtDOM
  , state     :: NxtDOM -> c
  , onEvent   :: NxtDOMRef String NxtDOM -> ([NxtClientOutMessage m],NxtDOM)
  , onMessage :: (NxtClientInMessage m) NxtDOM -> ([NxtClientOutMessage m],NxtDOM)
  }

:: NxtEditorServer p r w s c m =
  { init      :: p NxtMajorVersion -> (s, NxtVersionTree, c)
  , parameter :: s -> p
  , value     :: s -> Maybe w
  , onRefresh :: r NxtMajorVersion s NxtVersionTree -> ([NxtServerOutMessage m], s, NxtVersionTree, NxtWrite) 
  , onMessage :: (NxtServerInMessage m) s NxtVersionTree -> ([NxtServerOutMessage m], s, NxtVersionTree, Bool)
  }

:: NxtWrite :== Bool //The Bool is a 'write' signal that indicates if something significant has changed

:: NxtVersion :== (!NxtMajorVersion,!NxtMinorVersion) //First: which read from sds, Second: which revision by edits
:: NxtMajorVersion :== Int
:: NxtMinorVersion :== Int

//Together with the served side state we need to track all versions explicitly
//(the client versions are encoded in the DOM)
:: NxtVersionTree
	= NVTBasic     !NxtVersion
	| NVTGlue      !NxtVersionTree !NxtVersionTree
	| NVTMultiple  !NxtVersion [NxtVersionTree]

//Messages commonly only affect part of a datastructure, so no complete version tree is communicated
:: NxtPartialVersionTree 
	= NVPVersion !(Maybe NxtVersion) ![(Int,NxtPartialVersionTree)]

:: NxtWithPartialVersion m = { message :: m, version :: NxtPartialVersionTree }
:: NxtWithPartialVersions m = { message :: m, oldVersion :: NxtPartialVersionTree, newVersion :: NxtPartialVersionTree}

:: NxtClientInMessage m
	= NxtClientInRemote m NxtPartialVersionTree NxtPartialVersionTree //(message, oldVersion, newVersion)
	| NxtClientInLocal m //local loopback on client

:: NxtClientOutMessage m = NxtClientOut m NxtPartialVersionTree //(message, version)

:: NxtServerInMessage m
	= NxtServerInRemote m NxtPartialVersionTree
	| NxtServerInLocal m NxtMajorVersion //When we feedback messages, we need to pass along the major version of message that created the feedback

:: NxtServerOutMessage m = NxtServerOut m NxtPartialVersionTree NxtPartialVersionTree

instance Functor NxtServerInMessage
where
	fmap f (NxtServerInRemote m v) = NxtServerInRemote (f m) v
	fmap f (NxtServerInLocal m v) = NxtServerInLocal (f m) v

instance Functor NxtClientInMessage
where
	fmap f (NxtClientInRemote m ov nv) = NxtClientInRemote (f m) ov nv
	fmap f (NxtClientInLocal m) = NxtClientInLocal (f m)

//TODO: this is not minimal enough: We create a partial version info structure, but it actually contains all versions!
toPartialVersion (NVTBasic v) = NVPVersion (Just v) []
toPartialVersion (NVTGlue v1 v2) = NVPVersion Nothing [(0,toPartialVersion v1),(1,toPartialVersion v2)]
toPartialVersion (NVTMultiple v vs) = NVPVersion (Just v) [(n,toPartialVersion cv) \\ cv <- vs & n <- [0..]]

emptyPartialVersion = NVPVersion Nothing []
selectPartialVersion pos (NVPVersion _ items) = case [v \\ (i,v) <- items | i == pos] of
	[v:_] = v
	_     = emptyPartialVersion

maxMajorVersion (NVPVersion mbv vs) = foldr max (maybe 0 fst mbv) (map (maxMajorVersion o snd) vs)

getVersion:: NxtVersionTree -> NxtVersion
getVersion (NVTBasic v) = v
getVersion (NVTMultiple v _) = v

//Simulated DOM/JSWorld
:: NxtDOMRef :== [Int]
:: NxtDOM :== NxtDOMNode

:: NxtDOMNode = 
	{ attributes :: Map String String
	, children   :: [NxtDOMNode]
	, history    :: [(NxtVersion,NxtDOMNode)] //Would normally be tracked in JS outside the DOM
	}

:: VersionedServerState s =
	{ state            :: s
	, readVersion      :: NxtMajorVersion //Increments each time the linked sds refreshes
	, stateVersion     :: NxtVersionTree //Holds the versions of all parts of an editor
	}

//Untyped clientside configuration
:: NxtUI = { attributes  :: Map String String, children :: [NxtUI]}

//Untyped message for transfer and configuration
:: NxtChange
  = NxtNoChange
  | NxtReplace NxtUI
  | NxtChange [NxtAttrChange] [NxtStructureChange]

:: NxtAttrChange = NxtSetAttr String String | NxtDelAttr String
:: NxtStructureChange
  = NxtAddChild Int NxtUI
  | NxtRemChild Int
  | NxtUpdChild Int NxtChange

derive class iTask NxtDOMNode, VersionedServerState, NxtUI, NxtChange, NxtAttrChange, NxtStructureChange
derive class iTask NxtWithPartialVersion, NxtWithPartialVersions, NxtVersionTree, NxtPartialVersionTree
derive class iTask NxtServerInMessage, NxtServerOutMessage, NxtClientInMessage, NxtClientOutMessage

derive JSEncode NxtDOMNode, Map
derive JSDecode NxtDOMNode, Map

//Typed messages for dynamic editors that contain children
:: ContainerMsg c m
	= NxtInsertChild Position (Maybe c)
	| NxtRemoveChild Position
	| NxtUpdateChild Position m

:: Position :== Int

derive class iTask ContainerMsg

class EditMessage m
where
  encodeEditMessage :: m -> NxtChange
  decodeEditMessage :: NxtChange -> m

class EditUI c
where
  encodeEditUI :: c -> NxtUI
  decodeEditUI :: NxtUI -> c


nextRevision (version,revision) = (version, revision + 1)

checkRevision (NVPVersion (Just (v1,r1)) _) (v2,r2) = (v1 == v2) && (r1 == r2 + 1) //Only accept the next revision
checkRevision _ _ = False

getVersionFromDOM {NxtDOMNode|attributes}
	# major = maybe 0 toInt ('DM'.get "major-version" attributes)
	# minor = maybe 0 toInt ('DM'.get "minor-version" attributes)
	= (major,minor) 

setVersionInDOM (major,minor) dom=:{NxtDOMNode|attributes}
	# attributes = 'DM'.put "major-version" (toString major) attributes
	# attributes = 'DM'.put "minor-version" (toString minor) attributes
	= {NxtDOMNode|dom & attributes = attributes}

pushHistoryInDOM dom=:{attributes,children,history}
	# version = getVersionFromDOM dom
	= {NxtDOMNode|attributes = attributes, children = children, history = [(version,dom):history]}

resetHistoryInDOM dom = {NxtDOMNode|dom & history = []}

//Get the version of a child element in a compound structure
childversion pos mv = case [v \\ (n,v) <- mv | n == pos] of
	[] = NVPVersion Nothing []
	vs = last vs

//Definitions of a test editor
nxtNumberField :: NxtEditor () Int Int String String String
nxtNumberField = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
 where
  init c
    = {NxtDOMNode|attributes = 'DM'.fromList [("type","numberfield"),("value",maybe "" fromString c)], children = [], history = []}

  state dom=:{NxtDOMNode|attributes} = fromMaybe "" ('DM'.get "value" attributes)

  onEvent [] e dom=:{NxtDOMNode|attributes}
	# version = nextRevision $ getVersionFromDOM dom
    # msg = [NxtClientOut e (NVPVersion (Just version) [])]
	# dom = pushHistoryInDOM dom
    # dom = setVersionInDOM version {NxtDOMNode|dom & attributes = 'DM'.put "value" (fromString e) attributes}
    = (msg,dom)
  onEvent _ _ dom = ([],dom)

  onMessage (NxtClientInLocal message) dom=:{NxtDOMNode|attributes,history}
	# version = nextRevision $ getVersionFromDOM dom
    # msg = [NxtClientOut message (NVPVersion (Just version) [])]
	# dom = pushHistoryInDOM dom
    # dom = setVersionInDOM version {NxtDOMNode|dom & attributes = 'DM'.put "value" message attributes}
    = (msg,dom)

  onMessage (NxtClientInRemote message (NVPVersion (Just oldVersion) _) (NVPVersion (Just newVersion) _)) dom=:{NxtDOMNode|attributes,history}
	# version = getVersionFromDOM dom
	| version <> oldVersion //The server was not up-to date, mitigate potential conflict
		# curValue = fromJust ('DM'.get "value" attributes)
		# oldValue = firstJust ['DM'.get "value" attributes \\ (v,{NxtDOMNode|attributes}) <- history | v == oldVersion]
		//If we only extended the message, we can add the extension 
		| startsWith oldValue curValue
			# extension = subString (textSize oldValue) (textSize curValue) curValue
			# newValue = message +++ extension
			//Set message value as first step in history
			# dom = setVersionInDOM newVersion dom
			# dom = {NxtDOMNode|dom & attributes = 'DM'.put "value" message attributes}
			# dom = pushHistoryInDOM $ resetHistoryInDOM dom
			//Retore the extension and set as message
			# newVersion = nextRevision newVersion
			# dom = setVersionInDOM newVersion dom
			# dom = {NxtDOMNode|dom & attributes = 'DM'.put "value" newValue attributes}
    		# msg = [NxtClientOut newValue (NVPVersion (Just newVersion) [])]
			= (msg,dom)
		| otherwise //Too bad, we have lost our edits, best notify the user somehow by a visual or audible cue
			# dom = {NxtDOMNode|dom & attributes = 'DM'.put "value" message attributes}
			# dom = setVersionInDOM newVersion dom
			# dom = resetHistoryInDOM dom
			= ([],dom)
	| otherwise //Everything was as expected
		# dom = {NxtDOMNode|dom & attributes = 'DM'.put "value" message attributes}
		# dom = setVersionInDOM newVersion dom
		# dom = resetHistoryInDOM dom
		= ([],dom)
  where
    firstJust [Just x:_] = x

 server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
 where
  init () v = ("",NVTBasic (v,0), "")
  parameter _ = ()
  value s = Just (toInt s)

  onRefresh r rv  _ (NVTBasic v)
	= ([NxtServerOut (toString r) (NVPVersion (Just v) []) (NVPVersion (Just (rv,0)) [])], toString r,NVTBasic (rv,0), False)

  onMessage (NxtServerInRemote message version) c (NVTBasic v )
	| checkRevision version v = ([], message, NVTBasic (nextRevision v), True)
	| otherwise               = ([], c, NVTBasic v, False)

nxtButton :: NxtEditor () Bool Bool Bool (String,Bool) Bool
nxtButton = {client=client,server=server}
where
 client = {init=init,state=state,onEvent=onEvent,onMessage=onMessage}
 where
  init c 
	# (label,clicked) = fromMaybe ("button",False) c
	= {NxtDOMNode|attributes = 'DM'.fromList [("type","button"),("label",label),("clicked",if clicked "true" "false")],children = [], history = []}

  state dom=:{NxtDOMNode|attributes}
	# clicked = case 'DM'.get "clicked" attributes of
		(Just "true") = True
		_             = False
	# label = fromJust ('DM'.get "label" attributes)
	= (label,clicked)

  onEvent [] "click" dom=:{NxtDOMNode|attributes}
	# major = maybe 0 toInt ('DM'.get "major-version" attributes)
	# minor = maybe 1 (inc o toInt) ('DM'.get "minor-version" attributes)
    # msg = [NxtClientOut True (NVPVersion (Just (major,minor)) [])]
    # dom = {NxtDOMNode|dom & attributes = 'DM'.put "minor-version" (toString minor) $'DM'.put "clicked" "true" attributes}
    = (msg,dom)
  onEvent _ _ dom
	= ([],dom)

  onMessage (NxtClientInLocal value) dom=:{NxtDOMNode|attributes} //Similar to onEvent, but can also set value to False
	# major = maybe 0 toInt ('DM'.get "major-version" attributes)
	# minor = maybe 1 (inc o toInt) ('DM'.get "minor-version" attributes)
    # msg = [NxtClientOut value (NVPVersion (Just (major,minor)) [])]
    # dom = {NxtDOMNode|dom & attributes = 'DM'.put "minor-version" (toString minor) $'DM'.put "clicked" "false" attributes}
    = (msg,dom)

  onMessage (NxtClientInRemote message oldVersion newVersion) dom=:{NxtDOMNode|attributes}
	# attributes = 'DM'.put "clicked" (if message "true" "false") attributes
	//Update version
	# attributes = case newVersion of
		(NVPVersion (Just (major,minor)) _)
			= 'DM'.put "minor-version" (toString minor) $ 'DM'.put "major-version" (toString major) attributes
		_ 
			= attributes
    = ([],{NxtDOMNode|dom & attributes = attributes})

 server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
 where
  init () v = (False,NVTBasic (v,0),("Click me",False))
  parameter _ = ()
  value s = Just s

  onRefresh r rv _ (NVTBasic v) = ([NxtServerOut r (NVPVersion (Just v) []) (NVPVersion (Just (rv,0)) [])], r, NVTBasic (rv,0), False)

  onMessage (NxtServerInRemote message version) c (NVTBasic v)
	| checkRevision version v = ([], message, NVTBasic (nextRevision v), True)
	| otherwise               = ([], c, NVTBasic v, False)


toVersionAttr (x,y) = toString x +++ "-" +++ toString y

fromVersionAttr s = case split "-" s of
	[x,y:_] = (toInt x,toInt y)
	_       = (0,0)

addVersionAttr key (Just version) (NxtNoChange) = NxtChange [NxtSetAttr key (toVersionAttr version)] []
addVersionAttr key (Just version) (NxtChange attrChanges childChanges) = NxtChange (attrChanges ++ [NxtSetAttr key (toVersionAttr version)]) childChanges
addVersionAttr key (Just version) (NxtReplace ui=:{NxtUI|attributes}) = NxtReplace {NxtUI|ui & attributes = 'DM'.put key (toVersionAttr version) attributes}
addVersionAttr key Nothing message = message

getVersionAttr key (NxtChange attrChanges childChanges)
	= case [fromVersionAttr v \\ NxtSetAttr k v <- attrChanges | key == k] of
		[] =  Nothing
		versions = Just (last versions)
getVersionAttr key (NxtReplace {NxtUI|attributes}) 
	= fmap fromVersionAttr ('DM'.get key attributes)
getVersionAttr key enc = Nothing

class overlayVersions a :: String NxtPartialVersionTree a -> a

instance overlayVersions NxtChange
where
	overlayVersions key (NVPVersion Nothing []) (NxtNoChange) = NxtNoChange
	overlayVersions key (NVPVersion mbv cvs) (NxtNoChange) = NxtChange attrChanges childChanges
	where
		attrChanges = maybe [] (\version -> [NxtSetAttr key (toVersionAttr version)]) mbv
		childChanges = [NxtUpdChild n (overlayVersions key cv NxtNoChange) \\ (n,cv) <- cvs]

	overlayVersions key (NVPVersion mbv cvs) (NxtChange attrChanges childChanges) = NxtChange attrChanges` childChanges`
	where
		attrChanges` = attrChanges ++ maybe [] (\version -> [NxtSetAttr key (toVersionAttr version)]) mbv
		childChanges` = childChanges ++ [NxtUpdChild n (overlayVersions key cv NxtNoChange) \\ (n,cv) <- cvs]

	overlayVersions key version=:(NVPVersion mbv cvs) (NxtReplace ui) = NxtReplace (overlayVersions key version ui)

instance overlayVersions NxtUI
where
	overlayVersions key (NVPVersion mbv cvs) {NxtUI|attributes,children} = {NxtUI|attributes=attributes`,children=children`}
	where 
		attributes` = maybe attributes (\version -> 'DM'.put key (toVersionAttr version) attributes) mbv
		children` = [maybe c (\v -> overlayVersions key v c) ('DM'.get i cvsMap) \\ c <- children & i <- [0..]]
		cvsMap = 'DM'.fromList cvs

class getOverlayedVersions a :: String a -> NxtPartialVersionTree

instance getOverlayedVersions NxtChange 
where
	getOverlayedVersions key NxtNoChange = NVPVersion Nothing []
	getOverlayedVersions key (NxtReplace ui) = getOverlayedVersions key ui
	getOverlayedVersions key (NxtChange attrChanges childChanges)
		# version = foldl setVersion Nothing attrChanges
		# childVersions = [(n, getOverlayedVersions key change) \\ (NxtUpdChild n change) <- childChanges]
		= NVPVersion version (filter (not o emptyChange o snd) childVersions)
	where
		setVersion cur (NxtSetAttr k v) = if (k == key) (Just (fromVersionAttr v)) cur
		setVersion cur _ = cur

		emptyChange (NVPVersion Nothing []) = True
		emptyChange _ = False

instance getOverlayedVersions NxtUI
where
	getOverlayedVersions key {NxtUI|attributes,children}
	 	# mbv = fmap fromVersionAttr ('DM'.get key attributes)
		# cvs = filter (not o emptyVersion o snd) [(i,getOverlayedVersions key c) \\ c <- children & i <- [0..]]
		= NVPVersion mbv cvs
	where
		emptyVersion (NVPVersion Nothing []) = True	
		emptyVersion _ = False

instance EditMessage (NxtServerOutMessage m) | EditMessage m
where
	encodeEditMessage (NxtServerOut message oldVersion newVersion)
		= overlayVersions "old-version" oldVersion
		$ overlayVersions "new-version" newVersion
		$ encodeEditMessage message

	decodeEditMessage enc
		= let  message = decodeEditMessage enc
		       oldVersion = getOverlayedVersions "old-version" enc
		       newVersion = getOverlayedVersions "new-version" enc
		  in (NxtServerOut message oldVersion newVersion)

instance EditMessage (NxtClientOutMessage m) | EditMessage m
where
	encodeEditMessage (NxtClientOut message version)
		= overlayVersions "version" version
		$ encodeEditMessage message

	decodeEditMessage enc
		= let message = decodeEditMessage enc
		      version = getOverlayedVersions "version" enc
		  in (NxtClientOut message version)

instance EditMessage (NxtWithPartialVersion m) | EditMessage m
where
	encodeEditMessage {message,version}
		= overlayVersions "version" version
		$ encodeEditMessage message

	decodeEditMessage enc
		= {message=decodeEditMessage enc
		  ,version = getOverlayedVersions "version" enc
		  }

instance EditMessage (NxtWithPartialVersions m) | EditMessage m
where
	encodeEditMessage {message,oldVersion,newVersion}
		= overlayVersions "old-version" oldVersion
		$ overlayVersions "new-version" newVersion
		$ encodeEditMessage message

	decodeEditMessage enc
		= {message=decodeEditMessage enc
		  ,oldVersion = getOverlayedVersions "old-version" enc
		  ,newVersion = getOverlayedVersions "new-version" enc
		  }

instance EditMessage (Either a b) | EditMessage a & EditMessage b
where
  encodeEditMessage (Left value) = NxtChange [] [NxtUpdChild 0 (encodeEditMessage value)]
  encodeEditMessage (Right value) = NxtChange [] [NxtUpdChild 1 (encodeEditMessage value)]

  decodeEditMessage (NxtChange _ [NxtUpdChild 0 dec:_]) = Left (decodeEditMessage dec)
  decodeEditMessage (NxtChange _ [NxtUpdChild 1 dec:_]) = Right (decodeEditMessage dec)

instance EditMessage String //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage value = NxtChange [NxtSetAttr "value" value] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" value:_] []) = value

instance EditUI String
where
  encodeEditUI value = {NxtUI|attributes='DM'.fromList [("value",value)],children=[]}
  decodeEditUI {NxtUI|attributes} = fromJust ('DM'.get "value" attributes)

instance EditMessage Bool //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage value = NxtChange [NxtSetAttr "value" (if value "true" "false")] []

  decodeEditMessage (NxtChange [NxtSetAttr "value" "true":_] []) = True
  decodeEditMessage (NxtChange [NxtSetAttr "value" "false":_] []) = False

instance EditUI Bool
where
  encodeEditUI value = {NxtUI|attributes='DM'.fromList [("value",if value "true" "false")],children=[]}
  decodeEditUI {NxtUI|attributes} = case ('DM'.get "value" attributes) of (Just "true") = True; _ = False

instance EditMessage (Maybe a,Maybe b) | EditMessage a & EditMessage b
where
  encodeEditMessage (mba, mbb)
	= NxtChange [] (maybe [] (\a -> [NxtUpdChild 0 (encodeEditMessage a)]) mba
		 ++ maybe [] (\b -> [NxtUpdChild 1 (encodeEditMessage b)]) mbb)

  decodeEditMessage (NxtChange _ [NxtUpdChild 0 enca,NxtUpdChild 1 encb:_])
		= (Just (decodeEditMessage enca), Just (decodeEditMessage encb))
  decodeEditMessage (NxtChange _ [NxtUpdChild 0 enca:_]) = (Just (decodeEditMessage enca),Nothing)
  decodeEditMessage (NxtChange _ [NxtUpdChild 1 encb:_]) = (Nothing,Just (decodeEditMessage encb))
  decodeEditMessage _ = (Nothing,Nothing)

instance EditUI (Maybe a) | EditUI a
where
	encodeEditUI Nothing = {NxtUI|attributes='DM'.newMap,children=[]}
	encodeEditUI (Just x) = {NxtUI|attributes='DM'.newMap,children=[encodeEditUI x]}

	decodeEditUI {NxtUI|children=[]} = Nothing
	decodeEditUI {NxtUI|children=[m]} = Just (decodeEditUI m)

instance EditUI (a, b) | EditUI a & EditUI b
where
  encodeEditUI (a,b) = {NxtUI|attributes='DM'.newMap,children = [encodeEditUI a,encodeEditUI b]}
  decodeEditUI m = (decodeEditUI m,decodeEditUI m)

instance EditMessage (ContainerMsg c m) | EditUI c & EditMessage m
where
	encodeEditMessage (NxtInsertChild pos c) = NxtChange [] [NxtAddChild pos (encodeEditUI c)]
	encodeEditMessage (NxtRemoveChild pos) = NxtChange [] [NxtRemChild pos]
	encodeEditMessage (NxtUpdateChild pos m) = NxtChange [] [NxtUpdChild pos (encodeEditMessage m)]

	decodeEditMessage (NxtChange _ [NxtAddChild pos ui:_]) = NxtInsertChild pos (decodeEditUI ui)
	decodeEditMessage (NxtChange _ [NxtRemChild pos:_]) = NxtRemoveChild pos
	decodeEditMessage (NxtChange _ [NxtUpdChild pos m:_]) = NxtUpdateChild pos (decodeEditMessage m)

// ### Composition

//Combine two editors into one that can do both
//Based on the initial parameter, the appropriate one is selected
//The editor is biased left 
alternative :: 
	(NxtEditor p1 r1 w1 s1 c1 m1)
	(NxtEditor p2 r2 w2 s2 c2 m2)
    ->
	(NxtEditor (Either p1 p2) (Either r1 r2) (Either w1 w2) (Either s1 s2) (Either c1 c2) (Either m1 m2))
alternative e1 e2 = {NxtEditor|server=server,client=client} 
where
	server = {init = init, parameter = parameter, value=value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init (Left p) mv = let (s,vs,c) = (e1.server.NxtEditorServer.init p mv) in  (Left s,vs,Left c)
		init (Right p) mv = let (s,vs,c) = (e2.server.NxtEditorServer.init p mv) in (Right s,vs,Right c)

		parameter (Left s) = Left (e1.server.NxtEditorServer.parameter s)
		parameter (Right s) = Right (e2.server.NxtEditorServer.parameter s)

		value (Left s) = fmap Left (e1.server.NxtEditorServer.value s)
		value (Right s) = fmap Right (e2.server.NxtEditorServer.value s)

		onRefresh (Left r) rv (Left s) sv
			# (ms,s,sv,w) = e1.server.NxtEditorServer.onRefresh r rv s sv
			= ([NxtServerOut (Left message) oldVersion newVersion \\ (NxtServerOut message oldVersion newVersion) <- ms], Left s, sv, w)
		onRefresh (Right r) rv (Right s) sv
			# (ms,s,sv,w) = e2.server.NxtEditorServer.onRefresh r rv s sv
			= ([NxtServerOut (Right message) oldVersion newVersion \\ (NxtServerOut message oldVersion newVersion) <- ms], Right s, sv, w)

		onMessage (NxtServerInRemote (Left m) version) (Left s) sv 
			# (ms,s,sv,w) = e1.server.NxtEditorServer.onMessage (NxtServerInRemote m version) s sv
			= ([NxtServerOut (Left message) oldVersion newVersion \\ (NxtServerOut message oldVersion newVersion) <- ms], Left s, sv, w)
		onMessage (NxtServerInRemote (Right m) version) (Right s) sv
			# (ms,s,sv,w) = e2.server.NxtEditorServer.onMessage (NxtServerInRemote m version) s sv
			= ([NxtServerOut (Right message) oldVersion newVersion \\ (NxtServerOut message oldVersion newVersion) <- ms], Right s, sv, w)

    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init Nothing
			# dom=:{NxtDOMNode|attributes} = e1.client.NxtEditorClient.init Nothing
			= {NxtDOMNode|dom & attributes = 'DM'.put "alternative" "left" attributes}
		init (Just (Left c))
			# dom=:{NxtDOMNode|attributes} = e1.client.NxtEditorClient.init (Just c)
			= {NxtDOMNode|dom & attributes = 'DM'.put "alternative" "left" attributes}
		init (Just (Right c))
			# dom=:{NxtDOMNode|attributes} = e2.client.NxtEditorClient.init (Just c)
			= {NxtDOMNode|dom & attributes = 'DM'.put "alternative" "right" attributes}

		state dom=:{NxtDOMNode|attributes}
    		# alt = fromJust ('DM'.get "alternative" attributes)
			| alt == "left"
				= Left (e1.client.NxtEditorClient.state dom)
			| otherwise
				= Right (e2.client.NxtEditorClient.state dom)
	
		onEvent ref event dom=:{NxtDOMNode|attributes}
    		# alt = fromJust ('DM'.get "alternative" attributes)
			| alt == "left"
				# (ms,dom) = e1.client.NxtEditorClient.onEvent ref event dom 
				= ([NxtClientOut (Left message) version \\ (NxtClientOut message version) <- ms], dom)
			| otherwise
				# (ms,dom) = e2.client.NxtEditorClient.onEvent ref event dom 
				= ([NxtClientOut (Right message) version \\ (NxtClientOut message version) <- ms], dom)

		onMessage (NxtClientInRemote (Left m) oldVersion newVersion) dom 
			# (ms,dom) = e1.client.NxtEditorClient.onMessage (NxtClientInRemote m oldVersion newVersion) dom 
			= ([NxtClientOut (Left message) version \\ (NxtClientOut message version) <- ms], dom)
		onMessage (NxtClientInRemote (Right m) oldVersion newVersion) dom 
			# (ms,dom) = e2.client.NxtEditorClient.onMessage (NxtClientInRemote m oldVersion newVersion) dom 
			= ([NxtClientOut (Right message) version \\ (NxtClientOut message version) <- ms], dom)

multiple :: (NxtEditor p r w s c m) -> (NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
multiple editor = {NxtEditor|server=server,client=client}
where
	server = {init = init, parameter = parameter, value=value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init p v = ((p,[]),NVTMultiple (v,0) [], [])
		parameter (p,_) = p
		value (_,ss) = Just (map editor.server.NxtEditorServer.value ss)

		onRefresh mbrs rv (p,ss) (NVTMultiple stv vs)//A naive linear side by side diff to see what needs updating
			# (msgs,ss,vs,write) = compare 0 ss vs mbrs
			= (msgs, (p,ss), NVTMultiple stv vs, False)//By default we need to do a diff
		where
			//Compare first items side by side
			compare i [s:ss] [v:vs] [Nothing:mbrs]
				# (msgs, ss, vs, write) = compare (i + 1) ss vs mbrs
				= (msgs, [s:ss], [v:vs], write)

			compare i [s:ss] [vo:vs] [Just r:mbrs]
				# (ms, s, vn, writes) = editor.server.NxtEditorServer.onRefresh r rv s vo
				# (msgs, ss, vs, writess) = compare (i + 1) ss vs mbrs
				= ([NxtServerOut (NxtUpdateChild i message) (toPartialVersion vo) (toPartialVersion vn) \\ (NxtServerOut message oldVersion newVersion) <- ms] ++ msgs
				  , [s:ss], [vn:vs], writes || writess) //TODO: determine proper versions...

			//New read list has more items
			compare i [] _ mbrs
				# (msgs, ss, vs, ws) = unzip4 [create i` mbr \\ mbr <- mbrs & i` <- [i..]]
				= (flatten msgs, ss, vs, or ws)
			where
				create i mbr
					# (s,vi,c) = editor.server.NxtEditorServer.init p (fst stv)
					# (ms, s, v, write) = maybe ([],s,vi,False) (\r -> editor.server.NxtEditorServer.onRefresh r rv s vi) mbr 
					= ([NxtServerOut (NxtInsertChild i (Just c)) emptyPartialVersion (toPartialVersion vi)
					   :[NxtServerOut (NxtUpdateChild i message) (NVPVersion Nothing [(i,oldVersion)]) (NVPVersion Nothing [(i,newVersion)]) \\ (NxtServerOut message oldVersion newVersion) <- ms]], s, v, write)

			//New read list has less (remove existing)
			compare i ss _ [] = (repeatn (length ss) (NxtServerOut (NxtRemoveChild i) emptyPartialVersion emptyPartialVersion),[],[],False) //TODO: versions...

		onMessage (NxtServerInRemote (NxtUpdateChild pos m) (NVPVersion _ mv)) (p,ss) (NVTMultiple stv vs)//Route to the corresponding child
			| pos >= length ss || pos < 0 = ([],(p,ss),NVTMultiple stv vs,False) //Out of bounds, (maybe abort instead for the simulation)
			# (ms,s,v,write) = editor.server.NxtEditorServer.onMessage (NxtServerInRemote m (childversion pos mv)) (ss !! pos) (vs !! pos)
			= ([NxtServerOut (NxtUpdateChild pos message) (multiversion pos oldVersion) (multiversion pos newVersion)
			   \\ (NxtServerOut message oldVersion newVersion) <-ms ], (p, updateAt pos s ss), NVTMultiple stv (updateAt pos v vs), write)
			//TODO: create the right version structure
            where
				multiversion pos v = NVPVersion Nothing [(pos,v)]

		onMessage (NxtServerInRemote (NxtRemoveChild pos) (NVPVersion _ mv)) (p,ss) (NVTMultiple stv vs)
			| pos >= length ss || pos < 0 = ([],(p,ss),NVTMultiple stv vs, False) //Out of bounds, (maybe abort instead for the simulation)
			= ([], (p, removeAt pos ss), NVTMultiple stv (removeAt pos vs), True)

		onMessage (NxtServerInRemote (NxtInsertChild pos Nothing) (NVPVersion _ mv)) (p,ss) (NVTMultiple stv vs)
			| pos > length ss || pos < 0 = ([],(p,ss),NVTMultiple stv vs, False) //Out of bounds, (maybe abort instead for the simulation)
			# (s,v,_) = editor.server.NxtEditorServer.init p (fst stv) 
			= ([], (p, insertAt pos s ss), NVTMultiple stv (insertAt pos v vs), True)

		onMessage msg (p,ss) state
			= abort "OEPS.."

    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init Nothing = {NxtDOMNode|attributes = attributes, children = [], history = []}
		init (Just cs)
			= {NxtDOMNode|attributes = attributes, children = [editor.client.NxtEditorClient.init (Just c) \\ c <- cs], history = []}
		attributes = 'DM'.fromList [("type","multiple")]

		onEvent [n:ref] event dom=:{NxtDOMNode|children}
			| n < 0 || n >= length children = ([],dom)
			# (ms,child) = editor.client.NxtEditorClient.onEvent ref event (children !! n)
			= ([NxtClientOut (NxtUpdateChild n message) (NVPVersion Nothing [(n,version)]) \\ (NxtClientOut message version) <- ms], {NxtDOMNode|dom & children = updateAt n child children})
		onEvent _ _ dom
			= ([],dom)

		onMessage (NxtClientInRemote (NxtInsertChild pos c) oldVersion newVersion) dom=:{NxtDOMNode|children} //TODO: Check structure versions...
			# child = editor.client.NxtEditorClient.init c
			= ([],{NxtDOMNode|dom & children = insertAt pos child children})

		onMessage (NxtClientInLocal (NxtInsertChild pos c)) dom=:{NxtDOMNode|children} //TODO: Revert and versioning
			# child = editor.client.NxtEditorClient.init c
			= ([NxtClientOut (NxtInsertChild pos c) emptyPartialVersion],{NxtDOMNode|dom & children = insertAt pos child children})

		onMessage (NxtClientInRemote (NxtRemoveChild pos) oldVersion newVersion) dom=:{NxtDOMNode|children} //TODO: Check structure versions...
			= ([],{NxtDOMNode|dom & children = removeAt pos children})

		onMessage (NxtClientInLocal (NxtRemoveChild pos)) dom=:{NxtDOMNode|children} //TODO: should be able to revert to older version and send versions...
			= ([NxtClientOut (NxtRemoveChild pos) emptyPartialVersion],{NxtDOMNode|dom & children = removeAt pos children})

		onMessage (NxtClientInRemote (NxtUpdateChild pos m) oldVersion newVersion) dom=:{NxtDOMNode|children}
			# (ms,child) = editor.client.NxtEditorClient.onMessage
				(NxtClientInRemote m (selectPartialVersion pos oldVersion) (selectPartialVersion pos newVersion)) (children !! pos)
			= ([(NxtClientOut (NxtUpdateChild pos message) emptyPartialVersion) \\ (NxtClientOut message version) <- ms] //TODO: Determine version
			  ,{NxtDOMNode|dom & children = updateAt pos child children})

		onMessage (NxtClientInLocal m) dom
			= trace_n "UNIMPLEMENTED" ([],dom)

		state dom=:{NxtDOMNode|children}
			= map editor.client.NxtEditorClient.state children

//Compose by juxtaposition, no need to specify interdependency
glue ::
		(NxtEditor p1 r1 w1 s1 c1 m1)
        (NxtEditor p2 r2 w2 s2 c2 m2)
        ->
        (NxtEditor (p1,p2) (Maybe r1,Maybe r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
glue e1 e2 = {NxtEditor|server=server,client=client}
where
  server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
  where
	init (p1,p2) mv
		# (s1,vs1,c1) = e1.server.NxtEditorServer.init p1 mv
		# (s2,vs2,c2) = e2.server.NxtEditorServer.init p2 mv
		= ((s1,s2),NVTGlue vs1 vs2, (c1,c2))

	parameter (s1,s2) = (e1.server.NxtEditorServer.parameter s1, e2.server.NxtEditorServer.parameter s2)

	value (s1,s2) = case (e1.server.NxtEditorServer.value s1, e2.server.NxtEditorServer.value s2) of
		(Nothing,Nothing) = Nothing
		(mb1,mb2) = Just (mb1,mb2)

    onRefresh (mbr1,mbr2) rv (s1,s2) (NVTGlue sv1 sv2)
       # (m1, s1, sv1, w1) = maybe ([],s1,sv1,False) (\r1 -> e1.server.NxtEditorServer.onRefresh r1 rv s1 sv1) mbr1
       # (m2, s2, sv2, w2) = maybe ([],s2,sv2,False) (\r2 -> e2.server.NxtEditorServer.onRefresh r2 rv s2 sv2) mbr2
       = (zipMessagesWithVersions m1 m2, (s1,s2), NVTGlue sv1 sv2, w1 || w2)

    onMessage (NxtServerInLocal (mb1,mb2) vm) (s1,s2) (NVTGlue sv1 sv2)
       # (ms1, s1, sv1, w1) = maybe ([],s1,sv1,False) (\m1 -> e1.server.NxtEditorServer.onMessage (NxtServerInLocal m1 vm) s1 sv1) mb1
       # (ms2, s2, sv2, w2) = maybe ([],s2,sv2,False) (\m2 -> e2.server.NxtEditorServer.onMessage (NxtServerInLocal m2 vm) s2 sv2) mb2
       = (zipMessagesWithVersions ms1 ms2, (s1,s2), NVTGlue sv1 sv2, w1 || w2)

    onMessage (NxtServerInRemote (mb1,mb2) (NVPVersion _ cvs)) (s1,s2) (NVTGlue sv1 sv2)
       # (ms1, s1, sv1, w1) = maybe ([],s1,sv1,False) (\m1 -> e1.server.NxtEditorServer.onMessage (NxtServerInRemote m1 (childversion 0 cvs)) s1 sv1) mb1
       # (ms2, s2, sv2, w2) = maybe ([],s2,sv2,False) (\m2 -> e2.server.NxtEditorServer.onMessage (NxtServerInRemote m2 (childversion 1 cvs)) s2 sv2) mb2
       = (zipMessagesWithVersions ms1 ms2, (s1,s2), NVTGlue sv1 sv2, w1 || w2)
	
  zipMessagesWithVersions [NxtServerOut x ovx nvx:xs] [NxtServerOut y ovy nvy:ys]
	= [NxtServerOut (Just x, Just y) (NVPVersion Nothing [(0,ovx),(1,ovy)]) (NVPVersion Nothing [(0,nvx),(1,nvy)]):zipMessagesWithVersions xs ys]
  zipMessagesWithVersions [] ys
	= [NxtServerOut (Nothing,Just y) (NVPVersion Nothing [(1,ovy)]) (NVPVersion Nothing [(1,nvy)]) \\ (NxtServerOut y ovy nvy) <- ys]
  zipMessagesWithVersions xs []
	= [NxtServerOut (Just x,Nothing) (NVPVersion Nothing [(0,ovx)]) (NVPVersion Nothing [(0,nvx)]) \\ (NxtServerOut x ovx nvx) <- xs]
 
  client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
  where
    init c
		# (c1,c2) = maybe (Nothing,Nothing) (\(cx,cy) -> (Just cx,Just cy)) c
		= {NxtDOMNode|attributes=attributes,children = [e1.client.NxtEditorClient.init c1, e2.client.NxtEditorClient.init c2], history = []}
	attributes = 'DM'.fromList [("type","glue")]

    onEvent [0:ref] event dom=:{NxtDOMNode|children=[c1,c2]}
      # (m1,c1) = e1.client.NxtEditorClient.onEvent ref event c1
	  = ([NxtClientOut (Just message,Nothing) (NVPVersion Nothing [(0,version)]) \\ (NxtClientOut message version) <-m1],{NxtDOMNode|dom & children = [c1,c2]})
    onEvent [1:ref] event dom=:{NxtDOMNode|children=[c1,c2]}
      # (m2,c2) = e2.client.NxtEditorClient.onEvent ref event c2
	  = ([NxtClientOut (Nothing,Just message) (NVPVersion Nothing [(1,version)]) \\ (NxtClientOut message version) <-m2],{NxtDOMNode|dom & children = [c1,c2]})
	onEvent _ _ dom = ([],dom)

    onMessage (NxtClientInLocal (mb1,mb2)) dom=:{NxtDOMNode|children=[c1,c2]}
      # (m1,c1) = maybe ([],c1) (\m1 -> e1.client.NxtEditorClient.onMessage (NxtClientInLocal m1) c1) mb1
      # (m2,c2) = maybe ([],c2) (\m2 -> e2.client.NxtEditorClient.onMessage (NxtClientInLocal m2) c2) mb2
      = (zipClientOutMessages m1 m2, {NxtDOMNode|dom & children = [c1,c2]})

    onMessage (NxtClientInRemote (mb1,mb2) (NVPVersion _ omvs) (NVPVersion _ nmvs)) dom=:{NxtDOMNode|children=[c1,c2]}
      # (m1,c1) = maybe ([],c1) (\m1 -> e1.client.NxtEditorClient.onMessage (NxtClientInRemote m1 (childversion 0 omvs) (childversion 0 nmvs)) c1) mb1
      # (m2,c2) = maybe ([],c2) (\m2 -> e2.client.NxtEditorClient.onMessage (NxtClientInRemote m2 (childversion 1 omvs) (childversion 1 nmvs)) c2) mb2
      = (zipClientOutMessages m1 m2, {NxtDOMNode|dom & children = [c1,c2]})

    state dom=:{NxtDOMNode|children=[c1,c2]}
	  = (e1.client.NxtEditorClient.state c1, e2.client.NxtEditorClient.state c2)

  zipClientOutMessages [NxtClientOut x vx:xs] [NxtClientOut y vy:ys]
	= [NxtClientOut (Just x, Just y) (NVPVersion Nothing [(0,vx),(1,vy)]):zipClientOutMessages xs ys]
  zipClientOutMessages [] ys
	= [NxtClientOut (Nothing,Just y) (NVPVersion Nothing [(1,vy)]) \\ (NxtClientOut y vy) <- ys]
  zipClientOutMessages xs []
	= [NxtClientOut (Just x,Nothing) (NVPVersion Nothing [(0,vx)]) \\ (NxtClientOut x vx) <- xs]
 
linkm ::
	([s] (ContainerMsg c m) -> (Bool, [ContainerMsg c m]))
	([c] (ContainerMsg c m) -> (Bool, [ContainerMsg c m]))
	(NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
	->
	(NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
linkm mserver mclient editor = {NxtEditor|server=server,client=client}
where
	server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
	where
		init = editor.server.NxtEditorServer.init
		parameter = editor.server.NxtEditorServer.parameter
		value = editor.server.NxtEditorServer.value

    	onRefresh rs rv (p,ss) (NVTMultiple v vs)
			# (msgs, (p,ss), NVTMultiple v vs, mbw)  = editor.server.NxtEditorServer.onRefresh rs rv (p,ss) (NVTMultiple v vs)
			# (msgs, p, ss, v, vs, mbwm) = foldl modifyMsg ([],p,ss,v,vs,False) msgs
			= (msgs, (p,ss), NVTMultiple v vs, mbw || mbwm)

		onMessage m (p,ss) (NVTMultiple v vs)
			# (msgs,(p,ss), NVTMultiple v vs, mbw) = editor.server.NxtEditorServer.onMessage m (p,ss) (NVTMultiple v vs)
			# (msgs,p,ss,v,vs,mbwm) = foldl modifyMsg ([],p,ss,v,vs,False) msgs
			= (msgs,(p,ss), NVTMultiple v vs, mbw || mbwm)

		modifyMsg (msgs,p,ss,v,vs,mbw) msg=:(NxtServerOut message oldVersion newVersion)
			//Modify the outgoing messages
			# (passOn,feedBack) = mserver ss message
			//Feedback messages
			# (feedbackOutput,p,ss,v,vs,mbwm) = foldl (feedBackMsg (maxMajorVersion newVersion)) ([],p,ss,v,vs,False) feedBack
			= (msgs ++ (if passOn [NxtServerOut message oldVersion newVersion] []) ++ feedbackOutput, p, ss, v, vs, mbw || mbwm)

		feedBackMsg vm (msgs,p,ss,v,vs,mbw) msg
			# (emsgs,(p,ss),NVTMultiple v vs,mbwm) = onMessage (NxtServerInLocal msg vm) (p,ss) (NVTMultiple v vs)
			= (msgs ++ emsgs,p,ss,v,vs,mbw || mbwm)

	client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
	where
		init = editor.client.NxtEditorClient.init
		state = editor.client.NxtEditorClient.state

    	onEvent ref event dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent ref event dom
			# (msgs,dom) = foldl modifyMsg ([],dom) msgs
			= (msgs,dom)

		onMessage msg dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage msg dom
			# (msgs,dom) = foldl modifyMsg ([],dom) msgs
			= (msgs,dom)

		modifyMsg (msgs,dom) (NxtClientOut message version)
			# cs = state dom
			# (passOn,feedBack) = mclient cs message
			# (feedbackOutput,dom) = foldl feedBackMsg ([],dom) feedBack
			= (msgs ++ (if passOn [NxtClientOut message version] []) ++ feedbackOutput, dom)

		feedBackMsg (msgs,dom) msg
			# (emsgs,dom) = onMessage (NxtClientInLocal msg) dom
			= (msgs ++ emsgs,dom)

//Define the dependencies by defining feedback on messages
//NOTE: Only one the last 'writes' to the data source are be returned, is this ok?
linkg ::
        //Rewrite from server to client with feedback to server
        (s1 s2 (Maybe m1, Maybe m2) -> (Bool, [(Maybe m1, Maybe m2)]))
        //Rewrite from client to server with feedback to client
        (c1 c2 (Maybe m1, Maybe m2) -> (Bool, [(Maybe m1, Maybe m2)])) //Pass on, feedback message
        (NxtEditor p (Maybe r1,Maybe r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        ->
        (NxtEditor p (Maybe r1,Maybe r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))

linkg modServerToClient modClientToServer editor = {NxtEditor|server=server,client=client}
where
	server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
	where
		init = editor.server.NxtEditorServer.init
		parameter = editor.server.NxtEditorServer.parameter
		value = editor.server.NxtEditorServer.value

    	onRefresh (r1,r2) rv (s1,s2) (NVTGlue v1 v2)
			# (msgs,(s1,s2),NVTGlue v1 v2,mbw) = editor.server.NxtEditorServer.onRefresh (r1,r2) rv (s1,s2) (NVTGlue v1 v2)
			# (msgs,(s1,s2),(v1,v2),mbwm) = foldl modifyMsg ([],(s1,s2),(v1,v2),False) msgs
			= (msgs, (s1,s2),NVTGlue v1 v2, mbw || mbwm)

		onMessage m (s1,s2) (NVTGlue v1 v2)
			# (msgs,(s1,s2), NVTGlue v1 v2, mbw) = editor.server.NxtEditorServer.onMessage m (s1,s2) (NVTGlue v1 v2)
			# (msgs,(s1,s2), (v1,v2), mbwm) = foldl modifyMsg ([],(s1,s2),(v1,v2),False) msgs 
			= (msgs,(s1,s2), NVTGlue v1 v2,  mbw || mbwm)

		modifyMsg (msgs,(s1,s2),(v1,v2),mbw) msg=:(NxtServerOut message oldVersion newVersion)
			//Modify the outgoing messages
			# (passOn,feedBack) = modServerToClient s1 s2 message
			# (feedbackOutput,(s1,s2),(v1,v2),mbwm) = foldl (feedBackMsg (maxMajorVersion newVersion)) ([], (s1,s2),(v1,v2), False) feedBack
			= (msgs ++ (if passOn [NxtServerOut message oldVersion newVersion] []) ++ feedbackOutput, (s1,s2), (v1,v2), mbw || mbwm)

		feedBackMsg vm (msgs,(s1,s2),(v1,v2),mbw) msg
			# (emsgs,(s1,s2),NVTGlue v1 v2,mbwm) = onMessage (NxtServerInLocal msg vm) (s1,s2) (NVTGlue v1 v2)
			= (msgs ++ emsgs,(s1,s2),(v1,v2), mbw || mbwm)

	client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
	where
		init = editor.client.NxtEditorClient.init
		state = editor.client.NxtEditorClient.state

    	onEvent ref event dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent ref event dom
			# (msgs,dom) = foldl modifyMsg ([],dom) msgs
			= (msgs,dom)

		onMessage msg dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage msg dom
			# (msgs,dom) = foldl modifyMsg ([],dom) msgs
			= (msgs,dom)

		modifyMsg (msgs,dom) (NxtClientOut message version)
			# (c1,c2) = state dom
			# (passOn,feedBack) = modClientToServer c1 c2 message
			# (feedbackOutput,dom) = foldl feedBackMsg ([],dom) feedBack
			= (msgs ++ (if passOn [NxtClientOut message version] []) ++ feedbackOutput, dom)

		feedBackMsg (msgs,dom) msg
			# (emsgs,dom) = onMessage (NxtClientInLocal msg) dom
			= (msgs ++ emsgs,dom)

mapp :: (pa -> pb, pb -> pa) (NxtEditor pa r w s c m) -> (NxtEditor pb r w s c m) 
mapp (t,f) e = {NxtEditor|server = server, client = e.client}
where
	server = {init = init, parameter = parameter, value = e.server.NxtEditorServer.value
		, onRefresh = e.server.NxtEditorServer.onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
    where
		init p = e.server.NxtEditorServer.init (f p)
		parameter s = t (e.server.NxtEditorServer.parameter s)

mapr :: (rb -> ra) (NxtEditor p ra w s c m) -> (NxtEditor p rb w s c m) 
mapr t e = {NxtEditor|server = server, client = e.client}
where
	server = {init = e.server.NxtEditorServer.init, parameter = e.server.NxtEditorServer.parameter, value = e.server.NxtEditorServer.value
		, onRefresh = onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
	where
		onRefresh r s = e.server.NxtEditorServer.onRefresh (t r) s

mapw :: (wa -> Maybe wb) (NxtEditor p r wa s c m) -> (NxtEditor p r wb s c m)
mapw f e = {NxtEditor|server = server, client = e.client}
where
	server = {init = e.server.NxtEditorServer.init, parameter = e.server.NxtEditorServer.parameter, value = value
		, onRefresh = e.server.NxtEditorServer.onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
	where
		value s = maybe Nothing f (e.server.NxtEditorServer.value s)

maps :: (sa -> sb, sb -> sa) (NxtEditor p r w sa c m) -> (NxtEditor p r w sb c m)
maps (t,f) e = {NxtEditor|server = server, client = e.client}
where
	server = {init = init, parameter = parameter, value = value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init p rv
			# (s,v,c) = e.server.NxtEditorServer.init p rv
			= (t s,v,c)

		parameter s = e.server.NxtEditorServer.parameter (f s)
		value s = e.server.NxtEditorServer.value (f s)
		onRefresh r rv s v
			# (ms,s,v, w) = e.server.NxtEditorServer.onRefresh r rv (f s) v
			= (ms,t s,v, w)
		onMessage m s v
			# (ms,s,v,w) = e.server.NxtEditorServer.onMessage m (f s) v
			= (ms,t s,v,w)

mapc :: (ca -> cb, cb -> ca) (NxtEditor p r w s ca m) -> (NxtEditor p r w s cb m)
mapc (t,f) e = {NxtEditor|server = server, client = client}
where
	server = {init = init, parameter = e.server.NxtEditorServer.parameter, value = e.server.NxtEditorServer.value
		, onRefresh = e.server.NxtEditorServer.onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
	where
		init p rv
			# (s,v,c) = e.server.NxtEditorServer.init p rv
			= (s,v,t c)
	client = {init = init, onEvent = e.client.NxtEditorClient.onEvent
		, onMessage = e.client.NxtEditorClient.onMessage, state = state }
	where
		init mbc = e.client.NxtEditorClient.init (fmap f mbc)
		state dom = t (e.client.NxtEditorClient.state dom)

//FIXME: This can't be right, version information should be mapped together with the messages
mapm :: (ma -> mb, mb -> ma) (NxtEditor p r w s c ma) -> (NxtEditor p r w s c mb)
mapm (t,f) e = {NxtEditor|server = server, client = client}
where
	server = {init = e.server.NxtEditorServer.init, parameter = e.server.NxtEditorServer.parameter, value = e.server.NxtEditorServer.value
		, onRefresh = onRefresh, onMessage = onMessage}
	where
		onRefresh r rv s v
			# (ms,s,v,w) = e.server.NxtEditorServer.onRefresh r rv s v
			= ([NxtServerOut (t m) oldVersion newVersion \\ NxtServerOut m oldVersion newVersion <- ms], s,v,w)

		onMessage m s v
			# m = case m of 
				(NxtServerInRemote mm mv) = NxtServerInRemote (f mm) mv
				(NxtServerInLocal mm rv) = NxtServerInLocal (f mm) rv
			# (ms,s,v,w) = e.server.NxtEditorServer.onMessage m s v
			= ([NxtServerOut (t m) oldVersion newVersion \\ NxtServerOut m oldVersion newVersion <- ms],s,v,w)

	client = {init = e.client.NxtEditorClient.init, onEvent = onEvent
		, onMessage = onMessage, state = e.client.NxtEditorClient.state }
	where
		onEvent ref ev dom
			# (ms,dom) = e.client.NxtEditorClient.onEvent ref ev dom
			= ([NxtClientOut (t m) version \\ NxtClientOut m version <- ms],dom)
		onMessage m dom
			# m = case m of 
				(NxtClientInRemote mm ov nv) = NxtClientInRemote (f mm) ov nv
				(NxtClientInLocal mm) = NxtClientInLocal (f mm)
			# (ms,dom) = e.client.NxtEditorClient.onMessage m dom
			= ([NxtClientOut (t m) version \\ NxtClientOut m version <- ms],dom)
mapg ::
		((p1,p2) -> p, p -> (p1,p2))                                       //Fuse parameter
		((Maybe w1, Maybe w2) -> Maybe w, r -> (r1,r2))                          //Fuse read/write
        ((c1,c2) -> c, c -> (c1,c2))                                       //Fuse client configuration
        ((s1,s2) -> s, s -> (s1,s2))                                       //Fuse server state
        ((Maybe m1, Maybe m2) -> (Maybe m), m -> (Maybe m1, Maybe m2))     //Fuse messages

        (NxtEditor (p1,p2) (r1,r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        ->
        (NxtEditor p r w s c m)
mapg mp (mw,mr) mc ms (mmt,mmf) editor
	= mapm (fromJust o mmt, mmf) $ maps ms $ mapc mc $ mapw mw $ mapr mr $ mapp mp $ editor

validate :: (s -> Bool) (NxtEditor p r w s c m) -> (NxtEditor p r w (s,Bool) (c,Bool) (Either m Bool))
validate checkfun editor = {client=client,server=server}
where
	server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
	where
		init p rv
			# (s,v,c) = editor.server.NxtEditorServer.init p rv
			# valid = checkfun s
			= ((s,valid),v,(c,valid))

		parameter s
			= editor.server.NxtEditorServer.parameter (fst s) 
		value (s,valid)
			= if (not valid) Nothing (editor.server.NxtEditorServer.value s)

    	onRefresh rs rv (s,valid) v
			# (msgs, s, v`, mbw)  = editor.server.NxtEditorServer.onRefresh rs rv s v
			# valid` = checkfun s
			# msgs` = [NxtServerOut (Left m) ov nv \\ (NxtServerOut m ov nv) <- msgs]
					++ if (valid <> valid`) [NxtServerOut (Right valid`) (toPartialVersion v) (toPartialVersion v`)] []
			= (msgs`, (s,valid`), v`, mbw)

		onMessage (NxtServerInRemote (Left m) mv) (s,valid) v
			# (msgs, s, v`, mbw)  = editor.server.NxtEditorServer.onMessage (NxtServerInRemote m mv) s v 
			# valid` = checkfun s
			# msgs` = [NxtServerOut (Left m) ov nv \\ (NxtServerOut m ov nv) <- msgs]
					++ if (valid <> valid`) [NxtServerOut (Right valid`) (toPartialVersion v) (toPartialVersion v`)] []
			= (msgs`, (s,valid`), v`, mbw)

	client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
	where
		init (Just (c,valid))
			# dom = editor.client.NxtEditorClient.init (Just c)
			= {NxtDOMNode|dom & attributes = 'DM'.put "valid" (if valid "true" "false") dom.NxtDOMNode.attributes}
		init Nothing = editor.client.NxtEditorClient.init Nothing 

		state dom = (editor.client.NxtEditorClient.state dom, False)

    	onEvent ref event dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent ref event dom
			= ([NxtClientOut (Left m) v \\ (NxtClientOut m v) <- msgs],dom)

		onMessage (NxtClientInRemote (Right valid) ov nv) dom
			= ([],{NxtDOMNode|dom & attributes = 'DM'.put "valid" (if valid "true" "false") dom.NxtDOMNode.attributes})
		onMessage (NxtClientInRemote (Left m) ov nv) dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage (NxtClientInRemote m ov nv) dom
			= ([NxtClientOut (Left m) v \\ (NxtClientOut m v) <- msgs],dom)

//Simulation
simulate :: (NxtEditor p r w s c m) p (Maybe r) -> Task () | iTask r & iTask w & iTask s & iTask c & iTask m & EditMessage m
simulate editor p mbr
  = withShared initNetworkState
  \networkState ->
    withShared initServerState
  \serverState ->
    withShared initClientState
  \clientState ->
       allTasks
		[simulateServer editor serverState networkState <<@ Title "Server" @! ()
		,viewNetwork networkState <<@ Title "Network" @! ()
		,simulateClient editor clientState networkState <<@ Title "Client" @! ()			
		] <<@ ArrangeHorizontal @! ()
where
	(initClientState,initServerState,initNetworkState) = initStates
	where
		initReadVersion = 0
		initStates
			# (s,v,c) = editor.server.NxtEditorServer.init p initReadVersion
			# cs = editor.client.NxtEditorClient.init (Just c)
			# (s2c,s,sv,rv) = case mbr of
				Nothing = ([],s,v,initReadVersion)
				Just r
					# rv = initReadVersion + 1
					# (ms,s,sv,_) = editor.server.NxtEditorServer.onRefresh r rv s v
					= (ms,s,sv,rv)
			= (cs,initVersions s rv sv, (map encodeEditMessage s2c,[]))

		initVersions s rv sv = {state = s, readVersion = rv, stateVersion = sv}

simulateServer editor serverState networkState
   = viewSharedInformation () [ViewAs serverView] serverState
   >^* [OnAction (Action "Refresh") (always (doServerRefresh <<@ InWindow))
       ,OnAction (Action "Message") (always doServerMessage)
       ]
where
	serverView s = (s,editor.server.NxtEditorServer.value s.VersionedServerState.state)

	doServerRefresh
		=  enterInformation "Enter the refresh value" []
		>>= \v -> upd (setStates v) (serverState >*< networkState)
	where
		setStates r ({state,readVersion,stateVersion},(s2c,c2s))
			# readVersion = readVersion + 1
			# (msgs,state,stateVersion,mbw) = editor.server.NxtEditorServer.onRefresh r readVersion state stateVersion 
			= ({state=state,readVersion = readVersion, stateVersion = stateVersion}, (s2c ++ map encodeEditMessage msgs, c2s))

	doServerMessage
		= upd setStates (serverState >*< networkState)
	where
		setStates (s=:{state,readVersion,stateVersion},(s2c,c2s)) = case c2s of
			[m:ms]
				# (msgs,state,stateVersion,mbw) = editor.server.NxtEditorServer.onMessage (fromClientOut (decodeEditMessage m)) state stateVersion
				= ({s & state=state,stateVersion = stateVersion}, (s2c ++ (map encodeEditMessage msgs),ms))
			_
				= (s,(s2c,c2s))
	  
		fromClientOut (NxtClientOut m v) = NxtServerInRemote m v

viewNetwork networkState = viewSharedInformation () [] networkState

simulateClient editor clientState networkState
   =   viewSharedInformation () [ViewUsing id nxtDOMView] clientState
   >^* [OnAction (Action "Event") (always (doClientEvent <<@ InWindow))
       ,OnAction (Action "Message") (always doClientMessage)
       ,OnAction (Action "All Messages") (always doClientMessages)
       ]
where
  doClientEvent
    =  enterInformation "Enter the event expression" []
    >>= \e -> upd (setStates e) (clientState >*< networkState)
  where
     setStates (ref,e) (dom,(s2c,c2s))
        # (msgs,dom) = editor.client.onEvent ref e dom
        = (dom,(s2c,c2s ++ map encodeEditMessage msgs))

  doClientMessage = doClientMessages` (Just 1)
  doClientMessages = doClientMessages` Nothing

  doClientMessages` mbLimit = upd (updStates mbLimit) (clientState >*< networkState)
  where
	updStates (Just 0) (dom,(s2c,c2s)) = (dom,(s2c,c2s)) //Limit reached
	updStates _ (dom,([],c2s)) = (dom,([],c2s)) //No more messages 
  	updStates mbLimit (dom,(s2c=:[m:ms],c2s))
		# (msgs,dom) = editor.client.NxtEditorClient.onMessage (fromServerOut (decodeEditMessage m)) dom
        = updStates (maybe Nothing (\i -> Just (i - 1)) mbLimit) (dom,(ms, c2s ++ map encodeEditMessage msgs))

	fromServerOut (NxtServerOut m ov nv) = NxtClientInRemote m ov nv

nxtDOMView = fromSVGEditor {initView = const (), renderImage = render , updModel = const}
where
	render x () ts = 'GI'.margin ('GI'.px 10.0) (fst (tree x ts))

	tree dom=:{NxtDOMNode|children=[]} = \ts -> node dom ts
	tree dom=:{NxtDOMNode|attributes,children}
   		= \[(t1,ut1), (t2,ut2) : ts] ->
			let (image,  ts1) = node dom ts
			    (images, ts2) = seqList (map tree children) ts1
			in ( 'GI'.above (repeat AtLeft) [] Nothing []
				[ image
				, 'GI'.beside (repeat AtTop) [] Nothing []
					[ 'GI'.yline ('GI'.imageyspan t1 - 'GI'.imageyspan t2)
					, 'GI'.tag ut1
						('GI'.grid (Columns 2) (ColumnMajor,LeftToRight,TopToBottom) [] [] [] []
							(repeatn (length children) ('GI'.xline (px 10.0)) ++ init images ++ ['GI'.tag ut2 (last images)])
							NoHost
							)
					] NoHost
				] NoHost
			, ts2
			)

  	node {NxtDOMNode|attributes} ts = (withMargins image,ts)
    where
		withMargins i = 'GI'.margin ('GI'.px 0.0,'GI'.px 0.0,'GI'.px 5.0,'GI'.px 0.0) i

		row (k,v) = 'GI'.beside (repeat AtTop) [] Nothing [] ['GI'.text font (k +++ ": " +++ v)] NoHost
		rows = map row ('DM'.toList attributes)
		
		text = 'GI'.above (repeat AtLeft) [] Nothing [] rows NoHost
		image = 'GI'.overlay [(AtMiddleX,AtMiddleY)] [] [text] (Host box)
		box = 'GI'.rect ('GI'.textxspan font maxtext + 'GI'.textxspan font "MM")
			('GI'.px (toReal numattr * (height + text_y_margin))) <@< {fill = white}
		maxtext = maximumBy (\a b -> size a < size b) ["":[(k +++ ": " +++ v) \\ (k,v) <- 'DM'.toList attributes]]
		numattr = 'DM'.mapSize attributes

		font          = arial height
		height        = 10.0
		text_y_margin = 5.0
		bottom        = 5.0
		white = 'GI'.toSVGColor "white"
		arial = 'GI'.normalFontDef "Arial"

//Test editor: Numberfield with a local increment button
testCounterEditor = linkg s2c c2s (glue nxtNumberField nxtButton)
//testCounterEditor = mapg mapp (mapw,mapr) mapc maps mapm (linkg id s2c c2s (glue nxtNumberField nxtButton))
where
	//No changes on the server side
	s2c _ _ msg = (True,[])

	//Update the value, when the button is clicked (drop message to server and sent local messages instead
    c2s c1 c2 (_,Just clicked) =  (False, if clicked [ (Nothing,Just False), (Just (fromInt (toInt c1 + 1)), Nothing)] [])
    c2s c1 c2 msg = (True,[])

	//Only expose the number field
	mapc = (fst, \x -> (x,("Increment",False)))
	maps = (fst, \x -> (x,False))

	mapm = (fst,\x -> (Just x,Nothing))
	mapp = (const (), const ((),()))
	mapw = fst
	mapr x = (x,False)

//More complex test editor: A list of numbers where each element has a delete button and there is a global add button

testListWithAddAndDelete :: NxtEditor () [Maybe Int] [Maybe Int] [String] [String] (ContainerMsg String String)
testListWithAddAndDelete
	= mapp (const (), const ((),()))
	$ mapr (\x -> (Just x,Nothing))
	$ mapw (\w -> fst w)
	$ maps (fst,\x -> (x,False))
	$ mapc (fst,\x -> (x,("Add",False)))
	$ mapm (\(Just m,_) -> m, \m -> (Just m,Nothing))
	$ linkg rserver rclient (glue testListWithDelete nxtButton)
where
	//Ignore messages from the server
	rserver s1 s2 m = (True,[])

	rclient c1 c2 (_,Just True) = (False,[(Just (NxtInsertChild (length c1) (Just "42")), Just False)]) 
	rclient c1 c2 m = (True,[])

testListWithDelete :: NxtEditor () [Maybe Int] [Maybe Int] [String] [String] (ContainerMsg String String)
testListWithDelete
	= maps (snd,\x -> ((),x))
	$ mapm (tom,fromm)
	$ linkm rserver rclient (multiple testListItemEditor)
where
	//Ignore messages from the server
	rserver ss m = (True,[])

	//Turn a click of the button into the appropriate remove messages: remove locally and remote
	rclient c (NxtUpdateChild n (Right True)) = (False,[NxtRemoveChild n])
	rclient c m = (True,[]) //Ignore other messages

	//Simplify message type (delete clicks are client-side so never occur outside the editor)
	tom (NxtInsertChild pos c) = NxtInsertChild pos c
	tom (NxtRemoveChild pos) = NxtRemoveChild pos
	tom (NxtUpdateChild pos (Left m)) = NxtUpdateChild pos m
	fromm (NxtInsertChild pos c) = NxtInsertChild pos c
	fromm (NxtRemoveChild pos) = NxtRemoveChild pos
	fromm (NxtUpdateChild pos m) = NxtUpdateChild pos (Left m)

testListItemEditor :: NxtEditor () Int Int String String (Either String Bool)
testListItemEditor 
	= mapp (const (), const ((),()))
	$ mapr (\x -> (Just x,Nothing))
	$ mapw (\w -> fst w)
	$ maps (fst,\x -> (x,False))
	$ mapc (fst,\x -> (x,("Delete",False)))
	$ mapm (toEither,fromEither)
	$ glue nxtNumberField nxtButton
where
	toEither (Just l,_) = Left l
	toEither (_,Just r) = Right r
	fromEither (Left l) = (Just l,Nothing)
	fromEither (Right r) = (Nothing,Just r)

//Start world = doTasks (simulate nxtNumberField () (Just 42)) world
//Start world = doTasks (simulate (glue nxtNumberField nxtButton) ((),()) (Just (Just 12, Just False)) ) world
//Start world = doTasks (simulate testCounterEditor ((),()) (Just (Just 12, Just False))) world
//Start world = doTasks (simulate (validate (\x -> toInt x > 42) nxtNumberField) () (Just 12)) world
//Start world = doTasks (simulate listWithDelete () (Just [Just n \\ n <- [1..4]])) world
//Start world = doTasks (simulate testListItemEditor () (Just 42)) world
Start world = doTasks (simulate testListWithAddAndDelete () (Just [Just n \\ n <- [1..3]])) world
