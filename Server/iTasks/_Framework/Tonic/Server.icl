implementation module iTasks._Framework.Tonic.Server

import iTasks
from Text import class Text, instance Text String
import qualified Text as T
import qualified Data.Map as DM
from Data.Map import :: Map
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
import iTasks._Framework.Tonic.Blueprints
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet

derive class iTask TonicMessage, ServerState

playbackTonic :: Task ()
playbackTonic = get tonicServerShare >>= \(_, messages) -> playbackTonic` (length messages - 1)
  where
  playbackTonic` curIdx
    =                             get tonicServerShare
    >>= \(recording, messages) -> let numMsgs = length messages
                                      lastIdx = numMsgs - 1 in
                                  if (curIdx >= 0 && curIdx < numMsgs)
                                    (let notFirst = curIdx < numMsgs - 1
                                         notLast  = curIdx > 0 in
                                     (   viewMessage (messages !! curIdx) (drop curIdx messages)
                                     >>* [ OnAction (Action "Start recording" []) (ifCond (not recording) (toggleRecording >>| playbackTonic` curIdx))
                                         , OnAction (Action "Stop recording" [])  (ifCond recording       (toggleRecording >>| playbackTonic` curIdx))
                                         , OnAction (Action "First" [])    (ifCond notFirst (playbackTonic` lastIdx))
                                         , OnAction (Action "Previous" []) (ifCond notFirst (playbackTonic` (curIdx + 1)))
                                         , OnAction (Action "Next" [])     (ifCond notLast  (playbackTonic` (curIdx - 1)))
                                         , OnAction (Action "Last" [])     (ifCond notLast  (playbackTonic` 0))
                                         ]))
                                    (   viewInformation () [] "No recordings yet"
                                    >>* [OnAction (Action "Try again" []) (always playbackTonic)])
  toggleRecording = upd (\(b, msgs) -> (not b, msgs)) tonicServerShare
  viewMessage msg prevMsgs
    =           getModule msg.bpModuleName
    >>= \mod -> case getTonicFunc mod msg.bpFunctionName of
                  Just func
                    # inst                        = mkInstance msg.nodeId func
                    # inst & bpi_previouslyActive = 'DM'.fromList [(msg.nodeId, TaskId 1 i) \\ msg <- prevMsgs & i <- reverse [0..length prevMsgs]]
                    # currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
                    # inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
                    # inst & bpi_activeNodes      = case currActive of
                                                      [(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino (length prevMsgs), msg.nodeId)) inst.bpi_activeNodes
                    = viewInstance inst
                  _ = viewInformation () [] "No blueprint found!" @! ()

viewTonic :: Task ()
viewTonic = whileUnchanged tonicServerShare (updateBP Nothing o reverse o snd)
  where
  updateBP :: (Maybe BlueprintInstance) [TonicMessage] -> Task ()
  updateBP Nothing    [] = viewInformation () [] "Waiting for blueprint" @! ()
  updateBP (Just bpi) [] = viewInstance bpi
  updateBP Nothing [msg:msgs]
    =           getModule msg.bpModuleName
    >>= \mod -> case getTonicFunc mod msg.bpFunctionName of
                  Just func
                    # inst = mkInstance msg.nodeId func
                    = updateBP (Just inst) msgs
                  _ = viewInformation () [] "Waiting for blueprint" @! ()
  updateBP (Just inst) [msg:msgs]
    # currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
    # inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
    # inst & bpi_activeNodes      = case currActive of
                                      [(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino (tid + 1), msg.nodeId)) inst.bpi_activeNodes
    = updateBP (Just inst) msgs

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

tonicServerShare :: Shared (Bool, [TonicMessage])
tonicServerShare = sharedStore "tonicServerShare" (False, [])

acceptAndViewTonicTraces :: Task (Bool, [TonicMessage])
acceptAndViewTonicTraces
  = acceptTonicTraces tonicServerShare
      ||-
    viewSharedInformation "Logged traces" [] tonicServerShare

acceptTonicTraces :: !(Shared (Bool, [TonicMessage])) -> Task [ServerState]
acceptTonicTraces tonicShare
  = tcplisten 9000 True tonicShare { ConnectionHandlers
                                   | onConnect      = onConnect
                                   , whileConnected = whileConnected
                                   , onDisconnect   = onDisconnect
                                   }
  where
  onConnect :: String (Bool, [TonicMessage])
            -> (MaybeErrorString ServerState, Maybe (Bool, [TonicMessage]), [String], Bool)
  onConnect host olderMessages
    = ( Ok { oldData = ""
           , clientIp = host}
      , Just olderMessages
      , ["Welcome!"]
      , False)

  whileConnected :: (Maybe String) ServerState (Bool, [TonicMessage])
                 -> (MaybeErrorString ServerState, Maybe (Bool, [TonicMessage]), [String], Bool)
  whileConnected (Just newData) st=:{oldData} (recording, olderMessages)
    # collectedData        = oldData +++ 'T'.trim newData
    # (messages, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
    # newMsgs              = if recording [msg \\ Just msg <- map strToMessage messages] []
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

  onDisconnect :: ServerState (Bool, [TonicMessage])
               -> (MaybeErrorString ServerState, Maybe (Bool, [TonicMessage]))
  onDisconnect st lines
    = (Ok st, Just lines)


