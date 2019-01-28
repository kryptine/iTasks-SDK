module EditorSim
/**
* Simulation of messages passing between editors to figure out the design details
*/
import iTasks
import Data.Maybe, Data.Either, Data.Tuple, Data.Functor, Data.List, Data.Func
import qualified Data.Map as DM
import Text, Text.HTML

import qualified Graphics.Scalable.Image as GI
import iTasks.Extensions.SVG.SVGEditor

//TODOS:
// - Recreate an integer list editor using the composition functions
// - Add a consistency checking / synchronization mechanism for data protection
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
  { init      :: (Maybe c) NxtDOMRef NxtDOM -> NxtDOM
  , onEvent   :: (NxtDOMRef,String) NxtDOMRef NxtDOM -> ([m],NxtDOM)
  , onMessage :: m      NxtDOMRef NxtDOM -> ([m],NxtDOM)
  , state     :: NxtDOMRef NxtDOM -> (c,NxtDOM)
  }

:: NxtEditorServer p r w s c m =
  { init      :: p -> (s,c)
  , parameter :: s -> p
  , value     :: s -> Maybe w
  , onRefresh :: r s -> ([m], s, Bool) //The Bool is a 'write' signal that indicates if something significant has changed
  , onMessage :: m s -> ([m], s, Bool)
  }

//Simulated DOM/JSWorld
:: NxtDOMRef :== Int
:: NxtDOM :== Map String String

newDOMRef :: NxtDOM -> (NxtDOMRef,NxtDOM)
newDOMRef dom
	# n = maybe 0 toInt ('DM'.get "nextREF" dom) + 1
	= (n,'DM'.put "nextREF" (toString n) dom)

//Util
getDOMChildren :: NxtDOMRef NxtDOM -> ([NxtDOMRef],NxtDOM)
getDOMChildren ref dom
	= (fromMaybe [] (maybe Nothing (fromJSON o fromString) ('DM'.get (domRef "children" ref) dom)), dom)

setDOMChildren :: [NxtDOMRef] NxtDOMRef NxtDOM -> NxtDOM
setDOMChildren children ref dom
	= 'DM'.put (domRef "children" ref) (toString (toJSON children)) dom

domRef attr ref = toString ref +++ "-" +++ attr

//Untyped clientside configuration
:: NxtUI :== Map String String

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

derive class iTask NxtChange, NxtAttrChange, NxtStructureChange

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

//Definitions of a test editor
nxtNumberField :: NxtEditor () Int Int String String String
nxtNumberField = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
 where
  init c ref dom
    = 'DM'.put (domRef "value" ref) (maybe "" fromString c) dom

  onEvent (eref,e) ref dom
    | eref <> ref = ([],dom)
    # msg = [e]
    # dom = 'DM'.put (domRef "value" ref) (fromString e) dom
    = (msg,dom)

  onMessage m ref dom
    # dom = 'DM'.put (domRef "value" ref) m dom
    = ([],dom)

  state ref dom
    = (fromMaybe "A" ('DM'.get (domRef "value" ref) dom),dom)

 server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
 where
  init () = ("","")
  parameter _ = ()
  value s = Just (toInt s)

  onRefresh s _ = ([toString s], toString s, False)
  onMessage m _ = ([], m, True)

nxtButton :: NxtEditor () Bool Bool Bool (String,Bool) Bool
nxtButton = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
 where
  init c ref dom
	# (label,clicked) = fromMaybe ("button",False) c
    # dom = 'DM'.put (domRef "label" ref) label dom
    # dom = 'DM'.put (domRef "clicked" ref) (if clicked "true" "false") dom
    = dom

  onEvent (eref,"click") ref dom
    | eref =!= ref = ([],dom)
    # msg = [True]
    # dom = 'DM'.put (domRef "clicked" ref) "true" dom
    = (msg,dom)

  onMessage m ref dom
    # dom = 'DM'.put (domRef "clicked" ref) (if m "true" "false") dom
    = ([],dom)

  state ref dom
	# clicked = case 'DM'.get (domRef "clicked" ref) dom of
		(Just "true") = True
		_             = False
	# label = fromJust ('DM'.get (domRef "label" ref) dom)
	= ((label,clicked),dom)

 server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
 where
  init _ = (False,("Click me",False))
  parameter _ = ()
  value s = Just s

  onRefresh s _ = ([s], s, False)
  onMessage m _ = ([], m, True)

instance EditMessage String //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage v = NxtChange [NxtSetAttr "value" v] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" v] []) = v

instance EditUI String
where
  encodeEditUI v = 'DM'.fromList [("value",v)]
  decodeEditUI m = fromJust ('DM'.get "value" m)

instance EditMessage Bool //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage v = NxtChange [NxtSetAttr "value" (if v "true" "false")] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" "true"] []) = True
  decodeEditMessage (NxtChange [NxtSetAttr "value" "false"] []) = False

instance EditUI Bool
where
  encodeEditUI v = 'DM'.fromList [("value",if v "true" "false")]
  decodeEditUI m = case ('DM'.get "value" m) of (Just "true") = True; _ = False

instance EditMessage (Maybe a,Maybe b) | EditMessage a & EditMessage b
where
  encodeEditMessage (mba, mbb)
	= NxtChange [] (maybe [] (\a -> [NxtUpdChild 0 (encodeEditMessage a)]) mba
		 ++ maybe [] (\b -> [NxtUpdChild 1 (encodeEditMessage b)]) mbb)

  decodeEditMessage (NxtChange [] [NxtUpdChild 0 enca,NxtUpdChild 1 encb]) = (Just (decodeEditMessage enca),Just (decodeEditMessage encb))
  decodeEditMessage (NxtChange [] [NxtUpdChild 0 enca]) = (Just (decodeEditMessage enca),Nothing)
  decodeEditMessage (NxtChange [] [NxtUpdChild 1 encb]) = (Nothing,Just (decodeEditMessage encb))
  decodeEditMessage _ = (Nothing,Nothing)

instance EditUI (Maybe a) | EditUI a
where
	encodeEditUI Nothing = 'DM'.newMap
	encodeEditUI (Just x) = encodeEditUI x

	decodeEditUI m = if ('DM'.null m) Nothing (Just (decodeEditUI m))

instance EditUI (a, b) | EditUI a & EditUI b
where
  encodeEditUI (a,b) = 'DM'.union (encodeEditUI a) (encodeEditUI b) //FIXME: This can't work with overlapping keys...
  decodeEditUI m = (decodeEditUI m,decodeEditUI m)

instance EditMessage (ContainerMsg c m) | EditUI c & EditMessage m
where
	encodeEditMessage (NxtInsertChild pos c) = NxtChange [] [NxtAddChild pos (encodeEditUI c)]
	encodeEditMessage (NxtRemoveChild pos) = NxtChange [] [NxtRemChild pos]
	encodeEditMessage (NxtUpdateChild pos m) = NxtChange [] [NxtUpdChild pos (encodeEditMessage m)]

	decodeEditMessage (NxtChange [] [NxtAddChild pos ui]) = NxtInsertChild pos (decodeEditUI ui)
	decodeEditMessage (NxtChange [] [NxtRemChild pos]) = NxtRemoveChild pos
	decodeEditMessage (NxtChange [] [NxtUpdChild pos m]) = NxtUpdateChild pos (decodeEditMessage m)

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
    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init Nothing ref dom
    		# dom = 'DM'.put (domRef "alternative" ref) "left" dom
			= e1.client.NxtEditorClient.init Nothing ref dom
		init (Just (Left c)) ref dom 
    		# dom = 'DM'.put (domRef "alternative" ref) "left" dom
			= e1.client.NxtEditorClient.init (Just c) ref dom
		init (Just (Right c)) ref dom 
    		# dom = 'DM'.put (domRef "alternative" ref) "right" dom
			= e2.client.NxtEditorClient.init (Just c) ref dom
		
		onEvent event ref dom
    		# alt = fromJust ('DM'.get (domRef "alternative" ref) dom)
			| alt == "left"
				# (ms,dom) = e1.client.NxtEditorClient.onEvent event ref dom 
				= (map Left ms,dom)
			| otherwise
				# (ms,dom) = e2.client.NxtEditorClient.onEvent event ref dom 
				= (map Right ms,dom)

		onMessage (Left msg) ref dom 
			# (ms,dom) = e1.client.NxtEditorClient.onMessage msg ref dom 
			= (map Left ms,dom)
		onMessage (Right msg) ref dom 
			# (ms,dom) = e2.client.NxtEditorClient.onMessage msg ref dom 
			= (map Right ms,dom)

		state ref dom
    		# alt = fromJust ('DM'.get (domRef "alternative" ref) dom)
			| alt == "left"
				# (c,dom) = e1.client.NxtEditorClient.state ref dom 
				= (Left c,dom)
			| otherwise
				# (c,dom) = e2.client.NxtEditorClient.state ref dom 
				= (Right c,dom)

	server = {init = init, parameter = parameter, value=value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init (Left p) = (\(s,c) -> (Left s,Left c)) (e1.server.NxtEditorServer.init p)
		init (Right p) = (\(s,c) -> (Right s,Right c)) (e2.server.NxtEditorServer.init p)

		parameter (Left s) = Left (e1.server.NxtEditorServer.parameter s)
		parameter (Right s) = Right (e2.server.NxtEditorServer.parameter s)

		value (Left s) = fmap Left (e1.server.NxtEditorServer.value s)
		value (Right s) = fmap Right (e2.server.NxtEditorServer.value s)

		onRefresh (Left r) (Left s) 
			# (ms,s,w) = e1.server.NxtEditorServer.onRefresh r s
			= (map Left ms, Left s, w)
		onRefresh (Right r) (Right s) 
			# (ms,s,w) = e2.server.NxtEditorServer.onRefresh r s
			= (map Right ms, Right s, w)

		onMessage (Left m) (Left s) 
			# (ms,s,w) = e1.server.NxtEditorServer.onMessage m s
			= (map Left ms, Left s, w)
		onMessage (Right m) (Right s) 
			# (ms,s,w) = e2.server.NxtEditorServer.onMessage m s
			= (map Right ms, Right s, w)

multiple :: (NxtEditor p r w s c m) -> (NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
multiple editor = {NxtEditor|server=server,client=client}
where
    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init Nothing ref dom
			= setDOMChildren [] ref dom
		init (Just cs) ref dom
			# (rs,dom) = foldl init` ([],dom) cs
			= setDOMChildren (reverse rs) ref dom
		where
			init` (rs,dom) c
				# (r,dom) = newDOMRef dom
				= ([r:rs],editor.client.NxtEditorClient.init (Just c) r dom)

		onEvent event ref dom
			# (rs,dom) = getDOMChildren ref dom
			# (ms,dom) = foldl onEvent` ([],dom) rs
			= (flatten [map (NxtUpdateChild i) m \\ m <- reverse ms & i <- [0..]], dom)
		where
			onEvent` (ms,dom) cref
				# (m,dom) = editor.client.NxtEditorClient.onEvent event cref dom
				= ([m:ms],dom)

		onMessage (NxtInsertChild pos c) ref dom
			# (rs,dom) = getDOMChildren ref dom
			# (r,dom) = newDOMRef dom
			# dom = editor.client.NxtEditorClient.init c r dom
			# dom = setDOMChildren (insertAt pos r rs) ref dom
			= ([],dom)

		onMessage (NxtRemoveChild pos) ref dom
			# (rs,dom) = getDOMChildren ref dom
			# dom = setDOMChildren (removeAt pos rs) ref dom //No garbage collection, we just remove the reference.
			= ([],dom)

		onMessage (NxtUpdateChild pos m) ref dom
			# (rs,dom) = getDOMChildren ref dom
			# (ms,dom) = editor.client.NxtEditorClient.onMessage m (rs !! pos) dom
			= ([NxtUpdateChild pos m \\ m <- ms], dom)

		state ref dom
			# (rs,dom) = getDOMChildren ref dom
			# (cs,dom) = foldl state` ([],dom) rs
			= (reverse cs, dom)
		where
			state` (cs,dom) ref
				# (c,dom) = editor.client.NxtEditorClient.state ref dom
				= ([c:cs],dom)

	server = {init = init, parameter = parameter, value=value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init p = ((p,[]),[])
		parameter (p,_) = p
		value (_,ss) = Just (map editor.server.NxtEditorServer.value ss)

		onRefresh mbrs (p,ss) //A naive linear side by side diff to see what needs updating
			# (msgs,ss,write) = compare 0 ss mbrs
			= (msgs, (p,ss), False)//By default we need to do a diff
		where
			//Compare first items side by side
			compare i [s:ss] [Nothing:mbrs]
				# (msgs, ss, write) = compare (i + 1) ss mbrs
				= (msgs, [s:ss], write)
			compare i [s:ss] [Just r:mbrs]
				# (ms, s, writes) = editor.server.NxtEditorServer.onRefresh r s
				# (msgs, ss, writess) = compare (i + 1) ss mbrs
				= (map (NxtUpdateChild i) ms ++ msgs, [s:ss], writes || writess)
			//New read list has more items
			compare i [] mbrs
				# (msgs, ss, ws) = unzip3 [create i` mbr \\ mbr <- mbrs & i` <- [i..]]
				= (flatten msgs, ss, or ws)
			where
				create i mbr
					# (s,c) = editor.server.NxtEditorServer.init p
					# (ms, s, write) = maybe ([],s,False) (\r -> editor.server.NxtEditorServer.onRefresh r s) mbr
					= ([NxtInsertChild i (Just c): map (NxtUpdateChild i) ms], s, write)
			//New read list has less (remove existing)
			compare i ss [] = (repeatn (length ss) (NxtRemoveChild i),[],False)

		onMessage (NxtUpdateChild pos m) (p,ss)  //Route to the corresponding child
			| pos >= length ss || pos < 0 = ([],(p,ss),False) //Out of bounds, (maybe abort instead for the simulation)
			# (ms,s,write) = editor.server.NxtEditorServer.onMessage m (ss !! pos)
			= (map (NxtUpdateChild pos) ms, (p, updateAt pos s ss), write)

		onMessage (NxtRemoveChild pos) (p,ss) 
			| pos >= length ss || pos < 0 = ([],(p,ss),False) //Out of bounds, (maybe abort instead for the simulation)
			= ([], (p, removeAt pos ss), True)

		onMessage (NxtInsertChild pos Nothing) (p,ss) 
			| pos > length ss || pos < 0 = ([],(p,ss),False) //Out of bounds, (maybe abort instead for the simulation)
			# (s,_) = editor.server.NxtEditorServer.init p
			= ([], (p, insertAt pos s ss), True)
			
//Compose by juxtaposition, no need to specify interdependency
glue ::
		(NxtEditor p1 r1 w1 s1 c1 m1)
        (NxtEditor p2 r2 w2 s2 c2 m2)
        ->
        (NxtEditor (p1,p2) (r1,r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
glue e1 e2 = {NxtEditor|server=server,client=client}
where
  server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
  where
	init (p1,p2)
		# (s1,c1) = e1.server.NxtEditorServer.init p1
		# (s2,c2) = e2.server.NxtEditorServer.init p2
		= ((s1,s2),(c1,c2))

	parameter (s1,s2) = (e1.server.NxtEditorServer.parameter s1, e2.server.NxtEditorServer.parameter s2)

	value (s1,s2) = case (e1.server.NxtEditorServer.value s1, e2.server.NxtEditorServer.value s2) of
		(Nothing,Nothing) = Nothing
		(mb1,mb2) = Just (mb1,mb2)

    onRefresh (s1n,s2n) (s1o,s2o)
       # (m1, s1, w1) = e1.server.NxtEditorServer.onRefresh s1n s1o
       # (m2, s2, w2) = e2.server.NxtEditorServer.onRefresh s2n s2o
       = (zipMessages m1 m2, (s1,s2), w1 || w2)

    onMessage (mb1,mb2) (s1,s2)
       # (m1, s1, w1) = maybe ([],s1,False) (\m1 -> e1.server.NxtEditorServer.onMessage m1 s1) mb1
       # (m2, s2, w2) = maybe ([],s2,False) (\m2 -> e2.server.NxtEditorServer.onMessage m2 s2) mb2
       = (zipMessages m1 m2, (s1,s2), w1 || w2)

  client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
  where
    init c ref dom
		# (c1,c2) = maybe (Nothing,Nothing) (\(cx,cy) -> (Just cx,Just cy)) c
		# (r1,dom) = newDOMRef dom
		# (r2,dom) = newDOMRef dom
		# dom = setDOMChildren [r1,r2] ref dom
		= e2.client.NxtEditorClient.init c2 r2 (e1.client.NxtEditorClient.init c1 r1 dom)

    onEvent event ref dom
	  # ([r1,r2:_],dom) = getDOMChildren ref dom
      # (m1,dom) = e1.client.NxtEditorClient.onEvent event r1 dom
      # (m2,dom) = e2.client.NxtEditorClient.onEvent event r2 dom
      = (zipMessages m1 m2, dom)

    onMessage (mb1,mb2) ref dom
	  # ([r1,r2:_],dom) = getDOMChildren ref dom
      # (m1,dom) = maybe ([],dom) (\m1 -> e1.client.NxtEditorClient.onMessage m1 r1 dom) mb1
      # (m2,dom) = maybe ([],dom) (\m2 -> e2.client.NxtEditorClient.onMessage m2 r2 dom) mb2
      = (zipMessages m1 m2, dom)

    state ref dom
	  # ([r1,r2:_],dom) = getDOMChildren ref dom
	  # (c1,dom) = e1.client.NxtEditorClient.state r1 dom
	  # (c2,dom) = e2.client.NxtEditorClient.state r2 dom
	  = ((c1,c2),dom)

  zipMessages [x:xs] [y:ys] = [(Just x, Just y):zipMessages xs ys]
  zipMessages [] ys = [(Nothing,Just y) \\ y <- ys]
  zipMessages xs [] = [(Just x,Nothing) \\ x <- xs]

//multiple :: (NxtEditor p r w s c m) -> (NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
linkm ::
	([c] -> [c])
	([s] (ContainerMsg c m) -> ([ContainerMsg c m],[ContainerMsg c m]))
	([c] (ContainerMsg c m) -> ([ContainerMsg c m],[ContainerMsg c m]))
	(NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))
	->
	(NxtEditor p [Maybe r] [Maybe w] (p,[s]) [c] (ContainerMsg c m))

linkm minit mserver mclient editor = {NxtEditor|server=server,client=client}
where
	server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
	where
		init p
			# (s,c) = editor.server.NxtEditorServer.init p
			= (s,minit c)

		parameter = editor.server.NxtEditorServer.parameter
		value = editor.server.NxtEditorServer.value

    	onRefresh r (p,ss)
			# (msgs, (p,ss), mbw)  = editor.server.NxtEditorServer.onRefresh r (p,ss)
			# (msgs, p, ss, mbwm) = foldl modifyMsg ([],p,ss,False) msgs
			= (msgs, (p,ss), mbw || mbwm)

		onMessage m (p,ss)
			# (msgs,(p,ss), mbw) = editor.server.NxtEditorServer.onMessage m (p,ss)
			# (msgs,p,ss, mbwm) = foldl modifyMsg ([],p,ss,False) msgs
			= (msgs,(p,ss), mbw || mbwm)

		modifyMsg (msgs,p,ss,mbw) msg
			//Modify the outgoing messages
			# (passOn,feedBack) = mserver ss msg
			//Feedback messages
			# (feedbackOutput,p,ss,mbwm) = foldl feedBackMsg ([],p,ss,False) feedBack
			= (msgs ++ passOn ++ feedbackOutput, p, ss, mbw || mbwm)

		feedBackMsg (msgs,p,ss,mbw) msg
			# (emsgs,(p,ss),mbwm) = onMessage msg (p,ss)
			= (msgs ++ emsgs, p, ss, mbw || mbwm)

	client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
	where
		init = editor.client.NxtEditorClient.init
		state = editor.client.NxtEditorClient.state

    	onEvent event ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent event ref dom
			# (msgs,dom) = foldl (modifyMsg ref) ([],dom) msgs
			= (msgs,dom)

		onMessage msg ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage msg ref dom
			# (msgs,dom) = foldl (modifyMsg ref) ([],dom) msgs
			= (msgs,dom)

		modifyMsg ref (msgs,dom) msg
			# (cs,dom) = state ref dom
			# (passOn,feedBack) = mclient cs msg
			# (feedbackOutput,dom) = foldl (feedBackMsg ref) ([],dom) feedBack
			= (msgs ++ passOn ++ feedbackOutput, dom)

		feedBackMsg ref (msgs,dom) msg
			# (emsgs,dom) = onMessage msg ref dom
			= (msgs ++ emsgs,dom)

//Define the dependencies by defining feedback on messages
//NOTE: Only one the last 'writes' to the data source are be returned, is this ok?
linkg ::
        //Rewrite the initial client configuration
        ((c1,c2) -> (c1,c2))
        //Rewrite from server to client with feedback to server
        (s1 s2 (Maybe m1, Maybe m2) -> ([(Maybe m1, Maybe m2)],[(Maybe m1, Maybe m2)]))
        //Rewrite from client to server with feedback to client
        (c1 c2 (Either m1 m2) -> ([Either m1 m2],[Either m1 m2]))
        (NxtEditor p r (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        ->
        (NxtEditor p r (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))

linkg modClientInit modServerToClient modClientToServer editor = {NxtEditor|server=server,client=client}
where
	server = {init=init,parameter=parameter,value=value,onRefresh=onRefresh,onMessage=onMessage}
	where
		init p
			# (s,c) = editor.server.NxtEditorServer.init p
			= (s,modClientInit c)

		parameter = editor.server.NxtEditorServer.parameter
		value = editor.server.NxtEditorServer.value

    	onRefresh sn so
			# (msgs,(s1,s2),mbw) = editor.server.NxtEditorServer.onRefresh sn so
			# (msgs,(s1,s2),mbwm) = foldl modifyMsg ([],(s1,s2),False) msgs
			= (msgs, (s1,s2),mbw || mbwm)

		onMessage m s
			# (msgs,(s1,s2), mbw) = editor.server.NxtEditorServer.onMessage m s
			# (msgs,(s1,s2), mbwm) = foldl modifyMsg ([],(s1,s2),False) msgs
			= (msgs,(s1,s2), mbw || mbwm)

		modifyMsg (msgs,(s1,s2),mbw) msg
			//Modify the outgoing messages
			# (passOn,feedBack) = modServerToClient s1 s2 msg
			//Feedback messages
			# (feedbackOutput,(s1,s2),mbwm) = foldl feedBackMsg ([],(s1,s2),False) feedBack
			= (msgs ++ passOn ++ feedbackOutput, (s1,s2), mbw || mbwm)

		feedBackMsg (msgs,(s1,s2),mbw) msg
			# (emsgs,(s1,s2),mbwm) = onMessage msg (s1,s2)
			= (msgs ++ emsgs,(s1,s2),mbw || mbwm)

	client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
	where
		init = editor.client.NxtEditorClient.init
		state = editor.client.NxtEditorClient.state

    	onEvent event ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent event ref dom
			# (msgs,dom) = foldl (modifyMsg ref) ([],dom) (map toEither msgs)
			= (msgs,dom)

		onMessage msg ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage msg ref dom
			# (msgs,dom) = foldl (modifyMsg ref) ([],dom) (map toEither msgs)
			= (msgs,dom)

		modifyMsg ref (msgs,dom) msg
			# ((c1,c2),dom) = state ref dom
			# (passOn,feedBack) = modClientToServer c1 c2 msg
			# (feedbackOutput,dom) = foldl (feedBackMsg ref) ([],dom) feedBack
			= (msgs ++ map fromEither passOn ++ feedbackOutput, dom)

		feedBackMsg ref (msgs,dom) msg
			# (emsgs,dom) = onMessage (fromEither msg) ref dom
			= (msgs ++ emsgs,dom)

		toEither (Just m1,_) = Left m1
		toEither (_,Just m2) = Right m2
		fromEither (Left m1) = (Just m1,Nothing)
		fromEither (Right m2) = (Nothing,Just m2)

//Get rid of the tupling and combine the parts into a unified state, configuration and values
mapg ::
		((p1,p2) -> p, p -> (p1,p2))                                       //Fuse parameter
		((Maybe w1, Maybe w2) -> w, r -> (r1,r2))                          //Fuse read/write //FIXME
        ((c1,c2) -> c, c -> (c1,c2))                                       //Fuse client configuration
        ((s1,s2) -> s, s -> (s1,s2))                                       //Fuse server state
        ((Maybe m1, Maybe m2) -> (Maybe m), m -> (Maybe m1, Maybe m2))     //Fuse messages

        (NxtEditor (p1,p2) (r1,r2) (Maybe w1, Maybe w2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        ->
        (NxtEditor p r w s c m)

mapg (pfrom,pto) (wfrom,rto) (cfrom,cto) (sfrom,sto) (mfrom,mto) editor = {NxtEditor|server=server,client=client}
where
    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init c ref dom = editor.client.NxtEditorClient.init (fmap cto c) ref dom

		onEvent event ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent event ref dom
			= ([x \\ Just x <- map mfrom msgs],dom)

		onMessage msg ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage (mto msg) ref dom
			= ([x \\ Just x <- map mfrom msgs],dom)

		state ref dom
			# (c,dom) = editor.client.NxtEditorClient.state ref dom
			= (cfrom c,dom)

	server = {init = init, parameter = parameter, value=value, onRefresh = onRefresh, onMessage = onMessage}
    where
		init p
			# (s,c) = editor.server.NxtEditorServer.init (pto p)
			= (sfrom s, cfrom c)

		parameter s = pfrom (editor.server.NxtEditorServer.parameter (sto s))

		value s = fmap wfrom (editor.server.NxtEditorServer.value (sto s))

		onRefresh r s
			# (msgs,s,w) = editor.server.NxtEditorServer.onRefresh (rto r) (sto s)
			= ([x \\ Just x <- map mfrom msgs],sfrom s, w)

		onMessage msg s
			# (msgs,s,w) = editor.server.NxtEditorServer.onMessage (mto msg) (sto s)
			= ([x \\ Just x <- map mfrom msgs],sfrom s, w)

gluema ::
	(NxtEditor p1 r1 w1 s1 c1 m1)
	(NxtEditor p2 r2 w2 s2 c2 m2)
	->
	(NxtEditor (Either p1 p2) [Maybe (Either r1 r2)] [Maybe (Either w1 w2)]
		(Either p1 p2, [(Either s1 s2)]) [Either c1 c2] (ContainerMsg (Either c1 c2) (Either m1 m2)))
gluema e1 e2 = multiple (alternative e1 e2)

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

mapw :: (wa -> wb) (NxtEditor p r wa s c m) -> (NxtEditor p r wb s c m)
mapw f e = {NxtEditor|server = server, client = e.client}
where
	server = {init = e.server.NxtEditorServer.init, parameter = e.server.NxtEditorServer.parameter, value = value
		, onRefresh = e.server.NxtEditorServer.onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
	where
		value s = fmap f (e.server.NxtEditorServer.value s)

maps :: (sa -> sb, sb -> sa) (NxtEditor p r w sa c m) -> (NxtEditor p r w sb c m)
maps (t,f) e = {NxtEditor|server = server, client = e.client}
where
	server = {init = init, parameter = parameter, value = value, onRefresh = onRefresh, onMessage = onMessage}
	where
		init p = appFst t (e.server.NxtEditorServer.init p)
		parameter s = e.server.NxtEditorServer.parameter (f s)
		value s = e.server.NxtEditorServer.value (f s)
		onRefresh r s 
			# (ms,s,w) = e.server.NxtEditorServer.onRefresh r (f s)
			= (ms,t s,w)
		onMessage m s 
			# (ms,s,w) = e.server.NxtEditorServer.onMessage m (f s)
			= (ms,t s,w)

mapc :: (ca -> cb, cb -> ca) (NxtEditor p r w s ca m) -> (NxtEditor p r w s cb m)
mapc (t,f) e = {NxtEditor|server = server, client = client}
where
	server = {init = init, parameter = e.server.NxtEditorServer.parameter, value = e.server.NxtEditorServer.value
		, onRefresh = e.server.NxtEditorServer.onRefresh, onMessage = e.server.NxtEditorServer.onMessage}
	where
		init p = appSnd t (e.server.NxtEditorServer.init p)
	client = {init = init, onEvent = e.client.NxtEditorClient.onEvent
		, onMessage = e.client.NxtEditorClient.onMessage, state = state }
	where
		init mbc ref dom = e.client.NxtEditorClient.init (fmap f mbc) ref dom
		state ref dom = appFst t (e.client.NxtEditorClient.state ref dom)

mapm :: (ma -> mb, mb -> ma) (NxtEditor p r w s c ma) -> (NxtEditor p r w s c mb)
mapm (t,f) e = {NxtEditor|server = server, client = client}
where
	server = {init = e.server.NxtEditorServer.init, parameter = e.server.NxtEditorServer.parameter, value = e.server.NxtEditorServer.value
		, onRefresh = onRefresh, onMessage = onMessage}
	where
		onRefresh r s
			# (ms,s,w) = e.server.NxtEditorServer.onRefresh r s
			= (map t ms,s,w)
		onMessage m s 
			# (ms,s,w) = e.server.NxtEditorServer.onMessage (f m) s
			= (map t ms,s,w)

	client = {init = e.client.NxtEditorClient.init, onEvent = onEvent
		, onMessage = onMessage, state = e.client.NxtEditorClient.state }
	where
		onEvent ev ref dom
			# (ms,dom) = e.client.NxtEditorClient.onEvent ev ref dom
			= (map t ms,dom)
		onMessage m ref dom
			# (ms,dom) = e.client.NxtEditorClient.onMessage (f m) ref dom
			= (map t ms,dom)

//Simulation
simulate :: (NxtEditor p r w s c m) p (Maybe r) -> Task () | iTask r & iTask w & iTask s & iTask c & iTask m & EditMessage m
simulate editor p mbr
  = withShared initNetworkState
  \networkState ->
    withShared initServerState
  \serverState ->
    withShared initClientState
  \clientState ->
       (simulateServer editor serverState networkState
  -&&- viewNetwork networkState
  -&&- simulateClient editor clientState networkState)
  @! ()
where
	(initClientState,initServerState,initNetworkState) = initStates
	where
		initStates
			# (s,c) = editor.server.NxtEditorServer.init p
			# cs = editor.client.NxtEditorClient.init (Just c) 0 'DM'.newMap
			# (s2c,s,_) = maybe ([],s,False) (\r -> editor.server.NxtEditorServer.onRefresh r s) mbr
			= (cs,s,(map encodeEditMessage s2c,[]))

simulateServer editor serverState networkState
   = viewSharedInformation (Title "Server") [ViewAs serverView] serverState
   >^* [OnAction (Action "Refresh") (always (doServerRefresh <<@ InWindow))
       ,OnAction (Action "Message") (always doServerMessage)
       ]
where
  serverView s = (s,editor.server.NxtEditorServer.value s)

  doServerRefresh
   =  enterInformation "Enter the refresh value" []
   >>= \v -> upd (setStates v) (serverState >*< networkState)
   where
     setStates v (s,(s2c,c2s))
        # (msgs,s,mbw) = editor.server.NxtEditorServer.onRefresh v s
        = (s, (s2c ++ map encodeEditMessage msgs, c2s))

  doServerMessage
   = upd setStates (serverState >*< networkState)
   where
     setStates (s,(s2c,c2s)) = case c2s of
        [m:ms]
          # (msgs,s,mbw) = editor.server.NxtEditorServer.onMessage (decodeEditMessage m) s
          = (s, (s2c ++ map encodeEditMessage msgs,ms))
        _
          = (s,(s2c,c2s))

viewNetwork networkState
   = viewSharedInformation (Title "Network") [] networkState

simulateClient editor clientState networkState
   =   viewSharedInformation (Title "Client") [ViewAs clientView] clientState
   >^* [OnAction (Action "Event") (always (doClientEvent <<@ InWindow))
       ,OnAction (Action "Message") (always doClientMessage)
       ]
where
  clientView dom
    = DivTag [] [DivTag [] [StrongTag [] [Text k,Text ":"],SpanTag [] [Text v]] \\ (k,v) <- 'DM'.toList dom]

  doClientEvent
    =  enterInformation "Enter the event expression" []
    >>= \e -> upd (setStates e) (clientState >*< networkState)
  where
     setStates e (dom,(s2c,c2s))
        # (msgs,dom) = editor.client.onEvent e 0 dom
        = (dom,(s2c,c2s ++ map encodeEditMessage msgs))

  doClientMessage
   = upd setStates (clientState >*< networkState)
   where
     setStates (dom,(s2c,c2s)) = case s2c of
        [m:ms]
          # (msgs,dom) = editor.client.NxtEditorClient.onMessage (decodeEditMessage m) 0 dom
          = (dom,(ms, c2s ++ map encodeEditMessage msgs))
        _
          = (dom,(s2c,c2s))

//Test editor: Numberfield with a local increment button
testEditor = mapg mapp (mapw,mapr) mapc maps mapm (linkg id s2c c2s (glue nxtNumberField nxtButton))
where
	//No changes on the server side
	s2c _ _ msg = ([msg],[])

	//Update the value, when the button is clicked
    c2s c1 c2 (Right True) = let n = fromInt (toInt c1 + 1) in ([Left n],[Right False,Left n])
    c2s c1 c2 msg = ([msg],[])

	//Only expose the number field
	mapc = (fst, \x -> (x,("Increment",False)))
	maps = (fst, \x -> (x,False))

	mapm = (fst,\x -> (Just x,Nothing))
	mapp = (const (), const ((),()))
	mapw = fst
	mapr x = (x,False)

testRead = Just 42

testParam = ()

//More complex test editor: A list of numbers where each element has a delete button and there is a global add button
testListEditor :: 
	NxtEditor 
		() //p
		[Maybe Int] //r 
		[Maybe Int] //w
		[String] //s
		([(String,Bool)],Bool) //c
		(Maybe (ContainerMsg (String,(String,Bool)) (Maybe String, Maybe Bool)), Maybe Bool) //m

testListEditor
	= mapp (const (), const (((),()),()))
	$ mapr (\rs -> ( map (fmap (\i -> (i,False))) rs, False))
	$ mapw ((map (fmap (\(Just i,_) -> i))) o fromMaybe [] o fst)
	$ maps (\((_,ss),_) -> map fst ss, \ss -> ((((),()),[(s,False)\\ s <- ss]),False))
	$ mapc (\(cs,(_,a)) -> (map (appSnd snd) cs,a), \(cs,a) ->([(s,("Removed",d))\\ (s,d) <-cs],("Add item",a)))
	$ listWithAddAndDelete

listWithAddAndDelete = linkg id rserver rclient (glue listWithDelete nxtButton)
where
	rserver s1 s2 m = ([m],[])
	rclient c1 c2 (Right True) = ([Left (NxtInsertChild (length c1) Nothing)],[Right False]) 

listWithDelete = linkm id rserver rclient (multiple testListItemEditor)
where
	//Ignore messages from the server
	rserver ss m = ([m],[])

	//Turn a click of the button into the appropriate remove messages: remove locally and remote
	rclient c (NxtUpdateChild n (mbx,Just True)) = ([NxtRemoveChild n],[NxtRemoveChild n])
	rclient c m = ([m],[]) //Ignore other messages
	
testListItemEditor :: NxtEditor ((),()) (Int,Bool) (Maybe Int,Maybe Bool) (String,Bool) (String,(String,Bool))  (Maybe String, Maybe Bool)
testListItemEditor = glue nxtNumberField nxtButton

Start world = doTasks (simulate testListItemEditor ((),()) Nothing) world

//treeEditor = fromSVGEditor {initView = id, renderImage = \_ _ _ -> ('GI'.circle (PxSpan 25.0)), updModel = flip const}
//Start world = doTasks (viewInformation "Test" [ViewUsing id treeEditor] ()) world
