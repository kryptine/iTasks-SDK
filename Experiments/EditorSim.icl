module EditorSim
/**
* Simulation of messages passing between editors to figure out the design details 
*/
import iTasks
import Data.Maybe
import qualified Data.Map as DM
import Text.HTML

//Definition of editors
:: NxtEditor v s c m =
  { client :: NxtEditorClient c m
  , server :: NxtEditorServer v s c m
  }

:: NxtEditorClient c m =
  { init      :: c NxtDOMRef NxtDOM -> NxtDOM
  , onEvent   :: String NxtDOMRef NxtDOM -> (Maybe m,NxtDOM)
  , onMessage :: m      NxtDOMRef NxtDOM -> (Maybe m,NxtDOM)
  }

:: NxtEditorServer v s c m =
  { value     :: ((Maybe v) -> s, s -> Maybe v)
  , configure :: s -> c 
  , onRefresh :: s s -> (Maybe m, s)
  , onMessage :: m s -> (Maybe m, s)
  }

//Simulated DOM/JSWorld
:: NxtDOMRef :== Int
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
numberField :: NxtEditor Int String String String
numberField = {client=client,server=server}
where
 client = {init=init,onEvent=onEvent,onMessage=onMessage}
 where
  init c ref dom
    = 'DM'.put ("value-" +++ toString ref) (fromString c) dom
  
  onEvent e ref dom
    # msg = Just e
    # dom = 'DM'.put ("value-" +++ toString ref) (fromString e) dom
    = (msg,dom)
 
  onMessage m ref dom
    # dom = 'DM'.put ("value-" +++ toString ref) m dom
    = (Nothing,dom)

 server = {value=value,configure=configure,onRefresh=onRefresh,onMessage=onMessage}
 where
  configure s = s
  value = (maybe "" toString, \s -> Just (toInt s))
  onRefresh s _ = (Just s, s)
  onMessage m _ = (Nothing, m)

instance EditMessage String //If strings are used as edit type, it's just the value attribute
where
  encodeEditMessage v = NxtChange [NxtSetAttr "value" v] []
  decodeEditMessage (NxtChange [NxtSetAttr "value" v] []) = v

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
initClientState editor v = editor.client.NxtEditorClient.init c 1 'DM'.newMap
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
        # (mbMsg,dom) = editor.client.onEvent e 1 dom
        = (dom,maybe (s2c,c2s) (\m -> (s2c,c2s++[encodeEditMessage m])) mbMsg)

  doClientMessage
   = upd setStates (clientState >*< networkState)
   where
     setStates (dom,(s2c,c2s)) = case s2c of
        [m:ms] 
          # (mbMsg,dom) = editor.client.NxtEditorClient.onMessage (decodeEditMessage m) 1 dom  
          = (dom,(ms,maybe c2s (\m -> c2s ++ [encodeEditMessage m]) mbMsg)) 
        _ 
          = (dom,(s2c,c2s))

Start world = doTasks (simulate numberField (Just 42)) world
