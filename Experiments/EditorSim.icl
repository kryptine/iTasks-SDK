module EditorSim
/**
* Simulation of messages passing between editors to figure out the design details 
*/
import iTasks
import Data.Maybe, Data.Either, Data.Functor
import qualified Data.Map as DM
import Text, Text.HTML

//Definition of editors
:: NxtEditor v s c m =
  { client :: NxtEditorClient c m
  , server :: NxtEditorServer v s c m
  }

:: NxtEditorClient c m =
  { init      :: c NxtDOMRef NxtDOM -> NxtDOM
  , onEvent   :: (NxtDOMRef,String) NxtDOMRef NxtDOM -> ([m],NxtDOM)
  , onMessage :: m      NxtDOMRef NxtDOM -> ([m],NxtDOM)
  , state     :: NxtDOMRef NxtDOM -> (c,NxtDOM)
  }

:: NxtEditorServer v s c m =
  { value     :: ((Maybe v) -> s, s -> Maybe v)
  , configure :: s -> c 
  , onRefresh :: s s -> ([m], s)
  , onMessage :: m s -> ([m], s)
  }

//Simulated DOM/JSWorld
:: NxtDOMRef :== [Int]
:: NxtDOM :== Map String String 

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

class EditMessage m
where
  encodeEditMessage :: m -> NxtChange
  decodeEditMessage :: NxtChange -> m

//Definitions of a test editor
nxtNumberField :: NxtEditor Int String String String
nxtNumberField = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
 where
  init c ref dom
    = 'DM'.put ("value-" +++ join "-" (map toString ref)) (fromString c) dom
  
  onEvent (eref,e) ref dom
    | eref =!= ref = ([],dom)
    # msg = [e]
    # dom = 'DM'.put ("value-" +++ join "-" (map toString ref)) (fromString e) dom
    = (msg,dom)
 
  onMessage m ref dom
    # dom = 'DM'.put ("value-" +++ join "-" (map toString ref)) m dom
    = ([],dom)

  state ref dom
    = (fromMaybe "A" ('DM'.get ("value-" +++ join "-" (map toString ref)) dom),dom)

 server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
 where
  configure s = s
  value = (maybe "" toString, \s -> Just (toInt s))
  onRefresh s _ = ([s], s)
  onMessage m _ = ([], m)

nxtButton :: NxtEditor Bool Bool (String,Bool) Bool
nxtButton = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
 where
  init (label,clicked) ref dom
    # dom = 'DM'.put ("label-" +++ join "-" (map toString ref)) label dom
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) (if clicked "true" "false") dom
    = dom
  
  onEvent (eref,"click") ref dom
    | eref =!= ref = ([],dom)
    # msg = [True]
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) "true" dom
    = (msg,dom)
 
  onMessage m ref dom
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) (if m "true" "false") dom
    = ([],dom)

  state ref dom
	# clicked = case 'DM'.get ("clicked-" +++ join "-" (map toString ref)) dom of
		(Just "true") = True
		_             = False
	# label = fromJust ('DM'.get ("label-" +++ join "-" (map toString ref)) dom)
	= ((label,clicked),dom)

 server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
 where
  configure s = ("Click me",s)
  value = (fromMaybe False, Just)
  onRefresh s _ = ([s], s)
  onMessage m _ = ([], m)

instance EditMessage String //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage v = NxtChange [NxtSetAttr "value" v] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" v] []) = v

instance EditMessage Bool //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage v = NxtChange [NxtSetAttr "value" (if v "true" "false")] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" "true"] []) = True
  decodeEditMessage (NxtChange [NxtSetAttr "value" "false"] []) = False

instance EditMessage (Maybe a,Maybe b) | EditMessage a & EditMessage b
where
  encodeEditMessage (mba, mbb)
	= NxtChange [] (maybe [] (\a -> [NxtUpdChild 0 (encodeEditMessage a)]) mba
		 ++ maybe [] (\b -> [NxtUpdChild 1 (encodeEditMessage b)]) mbb)

  decodeEditMessage (NxtChange [] [NxtUpdChild 0 enca,NxtUpdChild 1 encb]) = (Just (decodeEditMessage enca),Just (decodeEditMessage encb))
  decodeEditMessage (NxtChange [] [NxtUpdChild 0 enca]) = (Just (decodeEditMessage enca),Nothing)
  decodeEditMessage (NxtChange [] [NxtUpdChild 1 encb]) = (Nothing,Just (decodeEditMessage encb))
  decodeEditMessage _ = (Nothing,Nothing)

// Composition

//Compose by juxtaposition, no need to specify interdependency
glue :: (NxtEditor v1 s1 c1 m1)
        (NxtEditor v2 s2 c2 m2)
        ->
        (NxtEditor (Maybe v1, Maybe v2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
glue e1 e2 = {NxtEditor|server=server,client=client}
where
  server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
  where
    value = (fromv,tov)
    where 
      fromv Nothing = (fst e1.server.NxtEditorServer.value Nothing, fst e2.server.NxtEditorServer.value Nothing) 
      fromv (Just (mv1,mv2)) = (fst e1.server.NxtEditorServer.value mv1, fst e2.server.NxtEditorServer.value mv2)

      tov (s1,s2)
        # mv1 = snd e1.server.NxtEditorServer.value s1
        # mv2 = snd e2.server.NxtEditorServer.value s2
        = (if (mv1 =: Nothing && mv2 =: Nothing) Nothing (Just (mv1,mv2)))

    configure (s1,s2) = (e1.server.NxtEditorServer.configure s1, e2.server.NxtEditorServer.configure s2)

    onRefresh (s1n,s2n) (s1o,s2o)
       # (m1, s1) = e1.server.NxtEditorServer.onRefresh s1n s1o
       # (m2, s2) = e2.server.NxtEditorServer.onRefresh s2n s2o
       = (zipMessages m1 m2, (s1,s2))

    onMessage (mb1,mb2) (s1,s2)
       # (m1,s1) = maybe ([],s1) (\m1 -> e1.server.NxtEditorServer.onMessage m1 s1) mb1
       # (m2,s2) = maybe ([],s2) (\m2 -> e2.server.NxtEditorServer.onMessage m2 s2) mb2
       = (zipMessages m1 m2, (s1,s2))

  client = {init=init,onEvent=onEvent,onMessage=onMessage,state=state}
  where
    init (c1,c2) ref dom = e2.client.init c2 (ref ++ [1]) (e1.client.init c1 (ref ++ [0]) dom)

    onEvent event ref dom
      # (m1,dom) = e1.client.NxtEditorClient.onEvent event (ref ++ [0]) dom
      # (m2,dom) = e2.client.NxtEditorClient.onEvent event (ref ++ [1]) dom
      = (zipMessages m1 m2, dom)

    onMessage (mb1,mb2) ref dom
      # (m1,dom) = maybe ([],dom) (\m1 -> e1.client.NxtEditorClient.onMessage m1 (ref ++ [0]) dom) mb1
      # (m2,dom) = maybe ([],dom) (\m2 -> e2.client.NxtEditorClient.onMessage m2 (ref ++ [1]) dom) mb2
      = (zipMessages m1 m2, dom)

    state ref dom
	  # (c1,dom) = e1.client.NxtEditorClient.state (ref ++ [0]) dom
	  # (c2,dom) = e2.client.NxtEditorClient.state (ref ++ [1]) dom
	  = ((c1,c2),dom)

  zipMessages [x:xs] [y:ys] = [(Just x, Just y):zipMessages xs ys] 
  zipMessages [] ys = [(Nothing,Just y) \\ y <- ys]
  zipMessages xs [] = [(Just x,Nothing) \\ x <- xs]

//Define the dependencies by defining feedback on messages
link ::
        //Rewrite the initial client configuration
        ((c1,c2) -> (c1,c2))
        //Rewrite from server to client with feedback to server
        (s1 s2 (Maybe m1, Maybe m2) -> ([(Maybe m1, Maybe m2)],[(Maybe m1, Maybe m2)]))
        //Rewrite from client to server with feedback to client
        (c1 c2 (Either m1 m2) -> ([Either m1 m2],[Either m1 m2])) 
        (NxtEditor v (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        ->      
        (NxtEditor v (s1,s2) (c1,c2) (Maybe m1, Maybe m2))

link modClientInit modServerToClient modClientToServer editor = {NxtEditor|server=server,client=client}
where
	server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
	where
		value = editor.server.NxtEditorServer.value
		configure = modClientInit o editor.server.NxtEditorServer.configure

    	onRefresh sn so
			# (msgs,(s1,s2)) = editor.server.NxtEditorServer.onRefresh sn so
			# (msgs,(s1,s2)) = foldl modifyMsg ([],(s1,s2)) msgs
			= (msgs, (s1,s2))

		onMessage m s
			# (msgs,(s1,s2)) = editor.server.NxtEditorServer.onMessage m s 
			# (msgs,(s1,s2)) = foldl modifyMsg ([],(s1,s2)) msgs
			= (msgs,(s1,s2))

		modifyMsg (msgs,(s1,s2)) msg
			//Modify the outgoing messages
			# (passOn,feedBack) = modServerToClient s1 s2 msg
			//Feedback messages
			# (feedbackOutput,(s1,s2)) = foldl feedBackMsg ([],(s1,s2)) feedBack
			= (msgs ++ passOn ++ feedbackOutput, (s1,s2))

		feedBackMsg (msgs,(s1,s2)) msg
			# (emsgs,(s1,s2)) = onMessage msg (s1,s2)
			= (msgs ++ emsgs,(s1,s2))

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
fuse :: 
        ((c1,c2) -> c, c -> (c1,c2))                                       //Fuse client configuration
        ((s1,s2) -> s, s -> (s1,s2))                                       //Fuse server state
        ((Maybe m1, Maybe m2) -> (Maybe m), m -> (Maybe m1, Maybe m2))     //Fuse messages
        ((Maybe v1, Maybe v2) -> (Maybe v), (Maybe v) -> (Maybe v1, Maybe v2)) //Fuse checked interface
        (NxtEditor (Maybe v1, Maybe v2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        -> 
        (NxtEditor v s c m)

fuse (cfrom,cto) (sfrom,sto) (mfrom,mto) (vfrom,vto) editor = {NxtEditor|server=server,client=client}
where
    client = {init = init, onEvent = onEvent, onMessage = onMessage, state = state}
	where
		init c ref dom = editor.client.NxtEditorClient.init (cto c) ref dom

		onEvent event ref dom
			# (msgs,dom) = editor.client.NxtEditorClient.onEvent event ref dom
			= ([x \\ Just x <- map mfrom msgs],dom)

		onMessage msg ref dom 
			# (msgs,dom) = editor.client.NxtEditorClient.onMessage (mto msg) ref dom
			= ([x \\ Just x <- map mfrom msgs],dom)

		state ref dom
			# (c,dom) = editor.client.NxtEditorClient.state ref dom
			= (cfrom c,dom)

	server = {value = (valueto,valuefrom), configure = configure, onRefresh = onRefresh, onMessage = onMessage}
    where
		valueto mbv
			# mbv  = case vto mbv of (Nothing,Nothing) = Nothing; v = Just v
			= sfrom ((fst editor.server.NxtEditorServer.value) mbv)
		valuefrom s
			# mbv =  (snd editor.server.NxtEditorServer.value) (sto s) 
			= maybe Nothing vfrom mbv

		configure s = cfrom (editor.server.NxtEditorServer.configure (sto s))

		onRefresh sn so
			# (msgs,s) = editor.server.NxtEditorServer.onRefresh (sto sn) (sto so)
			= ([x \\ Just x <- map mfrom msgs],sfrom s)

		onMessage msg s
			# (msgs,s) = editor.server.NxtEditorServer.onMessage (mto msg) (sto s)
			= ([x \\ Just x <- map mfrom msgs],sfrom s)


//Simulation
simulate :: (NxtEditor v s c m) (Maybe v) -> Task () | iTask v & iTask s & iTask c & iTask m & EditMessage m
simulate editor value
  = withShared ([],[])
  \networkState ->
    withShared (initServerState editor value)
  \serverState ->
    withShared (initClientState editor value)
  \clientState ->
       (simulateServer editor serverState networkState
  -&&- viewNetwork networkState
  -&&- simulateClient editor clientState networkState)
  @! ()

initServerState editor v = (fst editor.server.NxtEditorServer.value) v
initClientState editor v = editor.client.NxtEditorClient.init c [1] 'DM'.newMap
where
 s = (fst editor.server.NxtEditorServer.value) v
 c = editor.server.NxtEditorServer.configure s

simulateServer editor serverState networkState
   = viewSharedInformation (Title "Server") [ViewAs serverView] serverState
   >^* [OnAction (Action "Refresh") (always (doServerRefresh <<@ InWindow))
       ,OnAction (Action "Message") (always doServerMessage)
       ]
where
  serverView s 
    = (s,(snd editor.server.NxtEditorServer.value) s)

  doServerRefresh
   =  enterInformation "Enter the refresh value" []
   >>= \v -> upd (setStates v) (serverState >*< networkState)
   where
     setStates v (s,(s2c,c2s)) 
        # sv = (fst editor.server.NxtEditorServer.value) v
        # (msgs,s) = editor.server.NxtEditorServer.onRefresh sv s
        = (s, (s2c ++ map encodeEditMessage msgs, c2s))

  doServerMessage
   = upd setStates (serverState >*< networkState)
   where
     setStates (s,(s2c,c2s)) = case c2s of
        [m:ms] 
          # (msgs,s) = editor.server.NxtEditorServer.onMessage (decodeEditMessage m) s
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
        # (msgs,dom) = editor.client.onEvent e [1] dom
        = (dom,(s2c,c2s ++ map encodeEditMessage msgs))

  doClientMessage
   = upd setStates (clientState >*< networkState)
   where
     setStates (dom,(s2c,c2s)) = case s2c of
        [m:ms] 
          # (msgs,dom) = editor.client.NxtEditorClient.onMessage (decodeEditMessage m) [1] dom  
          = (dom,(ms, c2s ++ map encodeEditMessage msgs)) 
        _ 
          = (dom,(s2c,c2s))


//Test editor: Numberfield with a local increment button

testEditor = fuse fusec fuses fusem fusev (link id s2c c2s (glue nxtNumberField nxtButton))
where
	//No changes on the server side
	s2c _ _ msg = ([msg],[])

	//Update the value, when the button is clicked
    c2s c1 c2 (Right True) = let n = fromInt (toInt c1 + 1) in ([Left n],[Right False,Left n]) 
    c2s c1 c2 msg = ([msg],[])

	//Only expose the number field
	fusec = (fst, \x -> (x,("Increment",False)))
	fuses = (fst, \x -> (x,False))

	fusem = (fst,\x -> (Just x,Nothing)) 
	fusev = (fst,\x -> (x,Nothing))

testValue = Just 42

Start world = doTasks (simulate testEditor testValue) world
