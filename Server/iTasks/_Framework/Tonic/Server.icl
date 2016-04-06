implementation module iTasks._Framework.Tonic.Server

import iTasks
from Text import class Text, instance Text String
import qualified Text as T
import qualified Data.Map as DM
from Data.Map import :: Map
import qualified Data.IntMap.Strict as DIS
import qualified Data.List as DL
from Data.IntMap.Strict import :: IntMap
import iTasks._Framework.Tonic.Blueprints
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Tonic.Types
import iTasks._Framework.Tonic.Images

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

:: ViewerSettings =
  { autoPlay :: Bool
  }

derive class iTask ServerState, ViewerSettings

shViewerSettings :: Shared ViewerSettings
shViewerSettings = sharedStore "shViewerSettings" { autoPlay = True }

foldT_ :: (a -> Task ()) [a] -> Task ()
foldT_ f []       = return ()
foldT_ f [x : xs] = f x >>| foldT_ f xs

:: TonicGenRTMap :== Map ComputationId [((ModuleName, FuncName), GenBlueprintInstance)]

:: GenBlueprintInstance =
  { gbpi_computationId    :: !ComputationId
  , gbpi_activeNode       :: !(ComputationId, ExprId)
  , gbpi_previouslyActive :: !Map ExprId ComputationId
  , gbpi_parentId         :: !ComputationId
  , gbpi_blueprint        :: !TonicFunc
  , gbpi_case_branches    :: !Map ExprId Int
  , gbpi_bpref            :: !BlueprintIdent
  }

derive class iTask GenBlueprintInstance

newRTMapFromMessages :: [(TonicMessage, Bool)] -> Task TonicGenRTMap
newRTMapFromMessages xs = updRTMapFromMessages xs 'DM'.newMap

updRTMapFromMessages :: [(TonicMessage, Bool)] TonicGenRTMap -> Task TonicGenRTMap
updRTMapFromMessages []              rtMap = return rtMap
updRTMapFromMessages [(msg, _) : xs] rtMap = processMessage msg rtMap >>= updRTMapFromMessages xs

// Partial function :(
mkParentId :: ComputationId -> ComputationId
mkParentId [x : xs] = xs

processMessage :: TonicMessage TonicGenRTMap -> Task TonicGenRTMap
processMessage (TMNewTopLevel tmn) rtMap
  =           getModule tmn.tmn_bpModuleName
  >>= \mod -> case getTonicFunc mod tmn.tmn_bpFunctionName of
                Just func
                  # bpinst = { GenBlueprintInstance
                             | gbpi_computationId    = tmn.tmn_computationId
                             , gbpi_activeNode       = ([], []) // TODO Better representation? Or better default?
                             , gbpi_previouslyActive = 'DM'.newMap
                             , gbpi_parentId         = mkParentId tmn.tmn_computationId
                             , gbpi_blueprint        = func
                             , gbpi_case_branches    = 'DM'.newMap
                             , gbpi_bpref            = { BlueprintIdent
                                                       | bpr_moduleName = tmn.tmn_bpModuleName
                                                       , bpr_taskName   = tmn.tmn_bpFunctionName
                                                       }
                             }
                  = return (insertIntoRTMap bpinst rtMap)
                _ = return rtMap
  where
  insertIntoRTMap bpinst rtMap
    # comps = case 'DM'.get tmn.tmn_computationId rtMap of
                Just xs -> xs
                _       -> []
    # comps = comps ++ [((tmn.tmn_bpModuleName, tmn.tmn_bpFunctionName), bpinst)]
    = 'DM'.put tmn.tmn_computationId comps rtMap
processMessage (TMApply tma) rtMap
    # mParentBP = readRTMap (mkParentId tma.tma_computationId) tma.tma_bpModuleName tma.tma_bpFunctionName rtMap
    = case mParentBP of
        Just parentBPInst
          = return (updateRTMap tma parentBPInst rtMap)
        _ = return rtMap
  where
  readRTMap :: ComputationId ModuleName FuncName TonicGenRTMap -> Maybe GenBlueprintInstance
  readRTMap bpId mn tn rtMap
    = case 'DM'.get bpId rtMap of
        Just xs -> case [bp \\ ((mn`, fn), bp) <- xs | mn == mn` && tn == fn] of
                     [x : _] -> Just x
                     _       -> Nothing
        _ -> Nothing
  updateRTMap :: TMApply GenBlueprintInstance TonicGenRTMap -> TonicGenRTMap
  updateRTMap tma parentBPInst rtMap
    # oldActiveNodes = 'DM'.put (snd parentBPInst.gbpi_activeNode) (fst parentBPInst.gbpi_activeNode) parentBPInst.gbpi_previouslyActive
    # newParent      = { parentBPInst
                       & gbpi_activeNode       = (tma.tma_computationId, tma.tma_nodeId)
                       , gbpi_previouslyActive = oldActiveNodes}
    = case 'DM'.get tma.tma_computationId rtMap of
        Just [(x, _) : xs] // TODO Really? How do we determine which one to write to?
          = 'DM'.put tma.tma_computationId [(x, newParent) : xs] rtMap
        _ = rtMap

undef = undef
standAloneTonicViewer :: Task ()
standAloneTonicViewer
  = allTasks [ updateSharedInformation "Viewer settings" [] shViewerSettings @! ()
             , startViewer -1 @! ()
             ] @! ()
  where
  startViewer idx = get (tonicServerShare |+| shViewerSettings) >>= runViewer idx
  runViewer :: Int ((Bool, [(TonicMessage, Bool)]), ViewerSettings) -> Task ()
  runViewer curIdx ((_, messages), {autoPlay = True})
    # newMessages = takeWhile (not o snd) messages
    =   foldT_ processMessages newMessages
    >>| toggleMessages (length newMessages)
    >>| waitForTimer {Time | hour = 0, min = 0, sec = 1}
    >>| startViewer curIdx
    where
    // TODO FIXME: Don't do the reverses; it's expensive. Do it the smart way
    toggleMessages n = upd (\(b, msgs) -> (b, reverse (f n (reverse msgs)))) tonicServerShare
      where
      f _ [] = []
      f n [mp=:(msg, True) : msgs] = [mp : f n msgs]
      f n [mp=:(msg, _) : msgs]
        | n < 1     = [mp : msgs]
        | otherwise = [(msg, True) : f (n - 1) msgs]
    processMessages :: (TonicMessage, Bool) -> Task ()
    processMessages (TMNewTopLevel msg, _)
      = viewInformation () [] "Not implemented!" @! ()
    processMessages (TMApply msg, _)
      =           getModule msg.tma_bpModuleName
      >>= \mod -> case getTonicFunc mod msg.tma_bpFunctionName of
                    Just func
                      # prevMsgs = [] // TODO FIXME
                      # numPrev                     = length prevMsgs
                      # inst                        = mkInstance msg.tma_nodeId func
                      # inst & bpi_previouslyActive = 'DM'.fromList [(msg.tma_nodeId, TaskId 1 i) \\ (TMApply msg, _) <- prevMsgs & i <- reverse [0..numPrev]]
                      # currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
                      # inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
                      # inst & bpi_activeNodes      = case currActive of
                                                        [(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino numPrev, msg.tma_nodeId)) inst.bpi_activeNodes
                      = return ()
                    _ = return ()

  runViewer curIdx st=:((recording, messages), _)
    # curIdx  = if (curIdx < 0) (length messages - 1) curIdx
    # numMsgs = length messages
    # lastIdx = numMsgs - 1
    | curIdx >= 0 && curIdx < numMsgs
      # notFirst = curIdx < numMsgs - 1
      # notLast  = curIdx > 0
      =   viewMessage (messages !! curIdx) (drop curIdx messages)
      >>* [ OnAction (Action "Start recording" []) (ifCond (not recording) (toggleRecording >>| runViewer curIdx st))
          , OnAction (Action "Stop recording" [])  (ifCond recording       (toggleRecording >>| runViewer curIdx st))
          , OnAction (Action "First" [])           (ifCond notFirst        (runViewer lastIdx st))
          , OnAction (Action "Previous" [])        (ifCond notFirst        (runViewer (curIdx + 1) st))
          , OnAction (Action "Next" [])            (ifCond notLast         (runViewer (curIdx - 1) st))
          , OnAction (Action "Last" [])            (ifCond notLast         (runViewer 0 st))
          , OnAction (Action "Refresh" [])         (always                 (startViewer curIdx))
          ]
    | otherwise
      =   viewInformation () [] "No recordings yet"
      >>* [ OnAction (Action "Start recording" []) (ifCond (not recording) (toggleRecording >>| runViewer curIdx st))
          , OnAction (Action "Stop recording" [])  (ifCond recording       (toggleRecording >>| runViewer curIdx st))
          , OnAction (Action "Refresh" [])         (always                 (startViewer curIdx))
          ]

    where
    toggleRecording = upd (\(b, msgs) -> (not b, msgs)) tonicServerShare
  viewMessage :: (TonicMessage, Bool) [(TonicMessage, Bool)] -> Task ()
  viewMessage (TMNewTopLevel msg, _) prevMsgs
    = viewInformation () [] "Not implemented!" @! ()
  viewMessage (TMApply msg, _) prevMsgs
    =           getModule msg.tma_bpModuleName
    >>= \mod -> case getTonicFunc mod msg.tma_bpFunctionName of
                  Just func
                    # numPrev                     = length prevMsgs
                    # inst                        = mkInstance msg.tma_nodeId func
                    # inst & bpi_previouslyActive = 'DM'.fromList [(msg.tma_nodeId, TaskId 1 i) \\ (TMApply msg, _) <- prevMsgs & i <- reverse [0..numPrev]]
                    # currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
                    # inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
                    # inst & bpi_activeNodes      = case currActive of
                                                      [(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino numPrev, msg.tma_nodeId)) inst.bpi_activeNodes
                    = viewInstance inst
                  _ = viewInformation () [] "No blueprint found!" @! ()

//viewTonic :: Task ()
//viewTonic = whileUnchanged tonicServerShare (updateBP Nothing o reverse o snd)
  //where
  //updateBP :: (Maybe BlueprintInstance) [TonicMessage] -> Task ()
  //updateBP Nothing    [] = viewInformation () [] "Waiting for blueprint" @! ()
  //updateBP (Just bpi) [] = viewInstance bpi
  //updateBP Nothing [TMApply msg : msgs]
    //=           getModule msg.tma_bpModuleName
    //>>= \mod -> case getTonicFunc mod msg.tma_bpFunctionName of
                  //Just func
                    //# inst = mkInstance msg.tma_nodeId func
                    //= updateBP (Just inst) msgs
                  //_ = viewInformation () [] "Waiting for blueprint" @! ()
  //updateBP (Just inst) [TMApply msg : msgs]
    //# currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
    //# inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
    //# inst & bpi_activeNodes      = case currActive of
                                      //[(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino (tid + 1), msg.tma_nodeId)) inst.bpi_activeNodes
    //= updateBP (Just inst) msgs

viewInstance :: !BlueprintInstance -> Task ()
viewInstance bpi=:{bpi_blueprint, bpi_bpref = {bpr_moduleName, bpr_taskName}}
  = updateInformation ()
      [imageUpdate id (\_ -> mkInstanceImage [] bpi 'DM'.newMap 'DM'.newMap Nothing False) (const id) (const id) (\_ _ -> Nothing) (const id)]
      { ActionState
      | state  = { tis_task    = bpi.bpi_blueprint
                 , tis_depth   = { Scale | min = 0, cur = 0, max = 0}
                 , tis_compact = False }
      , action = Nothing}
      @! ()

nulDT = DateTime { Date | day = 0, mon = 0, year = 0 } { Time | hour = 0, min = 0, sec = 0 }

mkInstance :: NodeId TonicFunc -> BlueprintInstance
mkInstance nid tf =
  { BlueprintInstance
  | bpi_taskId           = TaskId 1 0
  , bpi_startTime        = nulDT
  , bpi_lastUpdated      = nulDT
  , bpi_endTime          = Nothing
  , bpi_activeNodes      = 'DM'.singleton (TaskId 1 0) ('DIS'.singleton 0 (TaskId 1 1, nid))
  , bpi_previouslyActive = 'DM'.newMap
  , bpi_parentTaskId     = TaskId 0 0
  , bpi_currentUser      = Nothing
  , bpi_blueprint        = tf
  , bpi_case_branches    = 'DM'.newMap
  , bpi_index            = 0
  , bpi_bpref            = { BlueprintIdent
                           | bpr_moduleName = tf.tf_module
                           , bpr_taskName   = tf.tf_name }
  }

messageArchive :: Shared [TonicMessage]
messageArchive = sharedStore "messageArchive" []

tonicServerShare :: Shared (Bool, [(TonicMessage, Bool)])
tonicServerShare = sharedStore "tonicServerShare" (True, [])

acceptAndViewTonicTraces :: Task (Bool, [(TonicMessage, Bool)])
acceptAndViewTonicTraces
  = acceptTonicTraces tonicServerShare
      ||-
    viewSharedInformation "Logged traces" [] tonicServerShare

acceptTonicTraces :: !(Shared (Bool, [(TonicMessage, Bool)])) -> Task [ServerState]
acceptTonicTraces tonicShare
  = tcplisten 9000 True tonicShare { ConnectionHandlers
                                   | onConnect      = onConnect
                                   , whileConnected = whileConnected
                                   , onDisconnect   = onDisconnect
                                   }
  where
  onConnect :: String (Bool, [(TonicMessage, Bool)])
            -> (MaybeErrorString ServerState, Maybe (Bool, [(TonicMessage, Bool)]), [String], Bool)
  onConnect host olderMessages
    = ( Ok { oldData = ""
           , clientIp = host}
      , Just olderMessages
      , ["Welcome!"]
      , False)

  whileConnected :: (Maybe String) ServerState (Bool, [(TonicMessage, Bool)])
                 -> (MaybeErrorString ServerState, Maybe (Bool, [(TonicMessage, Bool)]), [String], Bool)
  whileConnected (Just newData) st=:{oldData} (recording, olderMessages)
    # collectedData        = oldData +++ 'T'.trim newData
    # (messages, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
    # newMsgs              = if recording [(msg, False) \\ Just msg <- map strToMessage messages] []
    # tmsgs                = newMsgs ++ olderMessages
    = (Ok {st & oldData = leftover}, Just (recording, tmsgs), [], False)
    where
    strToMessage :: !String -> Maybe TonicMessage
    strToMessage str = fromJSON (fromString str)

    partitionMessages :: [String] -> ([String], String)
    partitionMessages []  = ([], "")
    partitionMessages [x] = ([], x)
    partitionMessages [x:y:xs]
      # (msgs, leftover) = partitionMessages [y:xs]
      = ([x:msgs], leftover)

  whileConnected Nothing st olderMessages
    = (Ok st, Nothing, [], False)

  onDisconnect :: ServerState (Bool, [(TonicMessage, Bool)])
               -> (MaybeErrorString ServerState, Maybe (Bool, [(TonicMessage, Bool)]))
  onDisconnect st lines
    = (Ok st, Just lines)


