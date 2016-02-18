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

viewTonic :: Task ()
viewTonic = whileUnchanged tonicServerShare (updateBP Nothing o reverse)
  where
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
      [imageUpdate id (mkInstanceImage [] bpi 'DM'.newMap 'DM'.newMap Nothing False) (const id) (const id) (\_ _ -> Nothing) (const id)]
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

tonicServerShare :: Shared [TonicMessage]
tonicServerShare = sharedStore "tonicServerShare" []

acceptAndViewTonicTraces :: Task [TonicMessage]
acceptAndViewTonicTraces
  = acceptTonicTraces tonicServerShare
      ||-
    viewSharedInformation "Logged traces" [] tonicServerShare

acceptTonicTraces :: !(RWShared () [TonicMessage] [TonicMessage]) -> Task [ServerState]
acceptTonicTraces tonicShare
  = tcplisten 9000 True tonicShare { ConnectionHandlers
                                   | onConnect      = onConnect
                                   , whileConnected = whileConnected
                                   , onDisconnect   = onDisconnect
                                   }
  where
  onConnect :: String [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage], [String], Bool)
  onConnect host olderMessages
    = ( Ok { oldData = ""
           , clientIp = host}
      , Just olderMessages
      , ["Welcome!"]
      , False)

  whileConnected :: (Maybe String) ServerState [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage], [String], Bool)
  whileConnected (Just newData) st=:{oldData} olderMessages
    # collectedData        = oldData +++ 'T'.trim newData
    # (messages, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
    # mbTMsgs              = Just ([msg \\ Just msg <- map (fromJSON o fromString) messages] ++ olderMessages)
    = (Ok {st & oldData = leftover}, mbTMsgs, [], False)
    where
    partitionMessages :: [String] -> ([String], String)
    partitionMessages []  = ([], "")
    partitionMessages [x] = ([], x)
    partitionMessages [x:y:xs]
      # (msgs, leftover) = partitionMessages [y:xs]
      = ([x:msgs], leftover)

  whileConnected Nothing st olderMessages
    = (Ok st, Nothing, [], False)

  onDisconnect :: ServerState [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage])
  onDisconnect st lines
    = (Ok st, Just lines)


