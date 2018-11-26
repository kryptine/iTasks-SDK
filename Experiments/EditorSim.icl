module EditorSim
/**
* Simulation of messages passing between editors to figure out the design details 
*/
import iTasks
import Data.Maybe
import qualified Data.Map as DM
import Text, Text.HTML

//Definition of editors
:: NxtEditor v s c m =
  { client :: NxtEditorClient c m
  , server :: NxtEditorServer v s c m
  }

:: NxtEditorClient c m =
  { init      :: c NxtDOMRef NxtDOM -> NxtDOM
  , onEvent   :: (NxtDOMRef,String) NxtDOMRef NxtDOM -> (Maybe m,NxtDOM)
  , onMessage :: m      NxtDOMRef NxtDOM -> (Maybe m,NxtDOM)
  }

:: NxtEditorServer v s c m =
  { value     :: ((Maybe v) -> s, s -> Maybe v)
  , configure :: s -> c 
  , onRefresh :: s s -> (Maybe m, s)
  , onMessage :: m s -> (Maybe m, s)
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
 client = {init=init,onEvent=onEvent,onMessage=onMessage}
 where
  init c ref dom
    = 'DM'.put ("value-" +++ join "-" (map toString ref)) (fromString c) dom
  
  onEvent (eref,e) ref dom
    | eref =!= ref = (Nothing,dom)
    # msg = Just e
    # dom = 'DM'.put ("value-" +++ join "-" (map toString ref)) (fromString e) dom
    = (msg,dom)
 
  onMessage m ref dom
    # dom = 'DM'.put ("value-" +++ join "-" (map toString ref)) m dom
    = (Nothing,dom)

 server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
 where
  configure s = s
  value = (maybe "" toString, \s -> Just (toInt s))
  onRefresh s _ = (Just s, s)
  onMessage m _ = (Nothing, m)

nxtButton :: NxtEditor Bool Bool (String,Bool) Bool
nxtButton = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage}
 where
  init (label,clicked) ref dom
    # dom = 'DM'.put ("label-" +++ join "-" (map toString ref)) label dom
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) (if clicked "true" "false") dom
    = dom
  
  onEvent (eref,"click") ref dom
    | eref =!= ref = (Nothing,dom)
    # msg = Just True
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) "true" dom
    = (msg,dom)
 
  onMessage m ref dom
    # dom = 'DM'.put ("clicked-" +++ join "-" (map toString ref)) (if m "true" "false") dom
    = (Nothing,dom)

 server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
 where
  configure s = ("Click me",s)
  value = (fromMaybe False, Just)
  onRefresh s _ = (Just s, s)
  onMessage m _ = (Nothing, m)

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
       # (mb1, s1) = e1.server.NxtEditorServer.onRefresh s1n s1o
       # (mb2, s2) = e2.server.NxtEditorServer.onRefresh s2n s2o
       = (if (mb1 =: Nothing && mb2 =: Nothing) Nothing (Just (mb1,mb2)), (s1,s2))

    onMessage (mb1,mb2) (s1,s2)
       # (mb1,s1) = maybe (Nothing,s1) (\m1 -> e1.server.NxtEditorServer.onMessage m1 s1) mb1
       # (mb2,s2) = maybe (Nothing,s2) (\m2 -> e2.server.NxtEditorServer.onMessage m2 s2) mb2
       = (if (mb1 =: Nothing && mb2 =: Nothing) Nothing (Just (mb1,mb2)), (s1,s2))

  client = {init=init,onEvent=onEvent,onMessage=onMessage}
  where
    init (c1,c2) ref dom = e2.client.init c2 ref (e1.client.init c1 ref dom)

    onEvent event ref dom
      # (mb1,dom) = e1.client.NxtEditorClient.onEvent event (ref ++ [0]) dom
      # (mb2,dom) = e2.client.NxtEditorClient.onEvent event (ref ++ [1]) dom
      = (if (mb1 =: Nothing && mb2 =: Nothing) Nothing (Just (mb1,mb2)), dom)

    onMessage (mb1,mb2) ref dom
      # (mb1,dom) = maybe (Nothing,dom) (\m1 -> e1.client.NxtEditorClient.onMessage m1 (ref ++ [0]) dom) mb1
      # (mb2,dom) = maybe (Nothing,dom) (\m2 -> e2.client.NxtEditorClient.onMessage m2 (ref ++ [1]) dom) mb2
      = (if (mb1 =: Nothing && mb2 =: Nothing) Nothing (Just (mb1,mb2)), dom)

/*
//Define the dependencies by defining feedback on messages
link ::
        //Rewrite the initial client configuration
        ((c1,c2) -> (c1,c2))
        //Rewrite from server to client with feedback to server
        (s1 s2 (Maybe m1, Maybe m2) -> ([(Maybe m1,Maybe m2)],[(Maybe m1, Maybe m2)]) 
        //Rewrite from client to server with feedback to client
        (c1 c2 (Either m1 m2) -> ([Either m1 m2],[Either m1 m2])) 
        (Editor v s (c1,c2) (Maybe m1, Maybe m2))
        ->      
        (Editor v s (c1,c2) (Maybe m1, Maybe m2))

//Get rid of the tuples and combine the parts into a unified state, configuration and 
fuse :: 
        ((c1,c2) -> c, c -> (c1,c2))                                 //Unify client configuration
        ((s1,s2) -> s, s -> (s1,s2))                                 //Unify server state
        (m -> (Maybe m1, Maybe m2), (Maybe m1, Maybe m2) -> Maybe m) //Unify messages
        (EditorValue v -> (EditorValue v1, EditorValue v2), (EditorValue v1,EditorValue v2) -> EditorValue v) //Unity checked interface
        (Editor (EditorValue v1, EditorValue v2) (s1,s2) (c1,c2) (Maybe m1, Maybe m2))
        -> 
        (Editor v s c m)
*/

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
        # (mbMsg,s) = editor.server.NxtEditorServer.onRefresh sv s
        = (s,(maybe s2c (\m -> s2c ++ [encodeEditMessage m]) mbMsg,c2s))

  doServerMessage
   = upd setStates (serverState >*< networkState)
   where
     setStates (s,(s2c,c2s)) = case c2s of
        [m:ms] 
          # (mbMsg,s) = editor.server.NxtEditorServer.onMessage (decodeEditMessage m) s
          = (s,(maybe s2c (\m -> s2c ++ [encodeEditMessage m]) mbMsg,ms)) 
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
        # (mbMsg,dom) = editor.client.onEvent e [1] dom
        = (dom,maybe (s2c,c2s) (\m -> (s2c,c2s++[encodeEditMessage m])) mbMsg)

  doClientMessage
   = upd setStates (clientState >*< networkState)
   where
     setStates (dom,(s2c,c2s)) = case s2c of
        [m:ms] 
          # (mbMsg,dom) = editor.client.NxtEditorClient.onMessage (decodeEditMessage m) [1] dom  
          = (dom,(ms,maybe c2s (\m -> c2s ++ [encodeEditMessage m]) mbMsg)) 
        _ 
          = (dom,(s2c,c2s))


//Test editor
testEditor = glue nxtNumberField nxtButton
testValue = Nothing

Start world = doTasks (simulate testEditor testValue) world
