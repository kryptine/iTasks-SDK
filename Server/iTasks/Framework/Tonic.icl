implementation module iTasks.Framework.Tonic

import iTasks.Framework.Engine
import iTasks.Framework.SDS
import qualified iTasks.Framework.SDS as DSDS
import iTasks.Framework.IWorld
import iTasks.Framework.Tonic.AbsSyn
import iTasks.Framework.TaskState
import iTasks.API.Core.TaskCombinators
import iTasks.API.Core.Tasks
import iTasks.API.Core.Types
import iTasks.API.Core.SDSs
import iTasks.API.Common.TaskCombinators
import iTasks.API.Common.ImportTasks
import iTasks.API.Common.InteractionTasks
import iTasks.API.Extensions.Admin.UserAdmin
import iTasks.API.Extensions.Tonic.Toniclet
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import StdArray
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor, Data.Graph, Data.List
import qualified Data.Map as DM

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive gText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, GParType, NodeContents, StepElem,
  StepFilter, TTaskApp, StepCond

derive class iTask TonicRT

tonicSharedRT :: Shared TonicRTMap
tonicSharedRT = sharedStore "tonicSharedRT" 'DM'.newMap

getModule :: String -> Task TonicModule
getModule moduleName
  =           getTonicDir >>-
    \dir ->   accWorld (readFile (dir </> (moduleName +++ ".tonic"))) >>-
    \mjson -> case mjson of
                Ok json   -> case fromJSON (fromString json) of
                               Just gg  -> return gg
                               _        -> err "Failed to deserialize JSON"
                Error msg -> err (toString msg)
  where
  err msg = throw ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg)

tonicWrapTaskBody :: ModuleName TaskName [(VarName, Task ())] (TaskDict a) (Task a) -> Task a
tonicWrapTaskBody mn tn args TaskDict t = tonicWrapTaskBody` mn tn args t

tonicWrapTaskBody` :: ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args (Task eval) = getModule mn >>- \m -> Task (eval` m)
  where
    eval` mod event evalOpts taskTree=:(TCDestroy tt) iworld
      # iworld = case taskIdFromTaskTree tt of
                   Just currTaskId=:(TaskId x y)
                     # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
                     = case mrtMap of
                         Ok rtMap -> snd ('DSDS'.write ('DM'.del currTaskId rtMap) tonicSharedRT iworld)
                         _        -> iworld
                   _ = iworld
      = eval event evalOpts taskTree iworld
    eval` mod event evalOpts=:{callTrace=[parentTaskNo:_]} taskTree iworld
      = case taskIdFromTaskTree taskTree of
          Just (currTaskId=:(TaskId instanceNo _))
            # bpinst = getTask mod tn
            # tonicRT          = { trt_taskId       = currTaskId
                                 , trt_params       = args
                                 , trt_bpref        = (mn, tn)
                                 , trt_bpinstance   = bpinst
                                 , trt_activeNodeId = Nothing
                                 , trt_parentTaskId = TaskId instanceNo parentTaskNo
                                 , trt_output       = Nothing
                                 }
            # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
            = case mrtMap of
                Ok rtMap
                  # (_, iworld)  = 'DSDS'.write ('DM'.put currTaskId tonicRT rtMap) tonicSharedRT iworld
                  # (tr, iworld) = eval event evalOpts taskTree iworld
                  # iworld = case tr of
                               ValueResult tv _ _ _
                                 # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
                                 = case mrtMap of
                                     Ok rtMap
                                       # rtMap = case 'DM'.get currTaskId rtMap of
                                                   Just rt -> 'DM'.put currTaskId {rt & trt_output = tvViewInformation tv} rtMap
                                                   _       -> rtMap
                                       # (_, iworld) = 'DSDS'.write rtMap tonicSharedRT iworld
                                       = iworld
                                     _ = iworld
                               _ = iworld
                  = (tr, iworld)
                _ = eval event evalOpts taskTree iworld
          _ = eval event evalOpts taskTree iworld
    eval` _ event evalOpts taskTree iworld = eval event evalOpts taskTree iworld
    tvViewInformation NoValue     = Nothing
    tvViewInformation (Value v _) = Just (viewInformation "Task result" [] v >>| return ())

tonicWrapApp :: ModuleName TaskName Int (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{callTrace=[parentTaskNo:_]} taskTree iworld
    = case taskIdFromTaskTree taskTree of
        Just currTaskId=:(TaskId instanceNo _)
          # parentTaskId = TaskId instanceNo parentTaskNo
          # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
          = case mrtMap of
              Ok rtMap
                # rtMap = case 'DM'.get parentTaskId rtMap of
                            Just rt -> 'DM'.put parentTaskId {rt & trt_activeNodeId = Just nid} rtMap
                            _       -> rtMap
                # (_, iworld) = 'DSDS'.write rtMap tonicSharedRT iworld
                = eval event evalOpts taskTree iworld
              _ = eval event evalOpts taskTree iworld
        _ = eval event evalOpts taskTree iworld
  eval` event evalOpts taskTree iworld = eval event evalOpts taskTree iworld

getTonicModules :: Task [String]
getTonicModules
  =         getTonicDir >>-
    \dir -> accWorld (readDirectory dir) >>-
    \mfs -> case mfs of
              Ok fs   -> return (map dropExtension (filter noDots fs))
              Error _ -> throw "Failed to read Tonic dir"
  where
  noDots str = not (str.[0] == '.')

getTonicDir :: Task String
getTonicDir = mkInstantTask f
  where
  f _ iworld
    # (server, iworld) = iworld!server
    = (Ok (server.paths.appDirectory </> "tonic"), iworld)

tonicLogin :: String -> Task ()
tonicLogin appName = tonicUI appName
//tonicLogin :: String -> Task Void
//tonicLogin appName = forever (
      //(viewTitle "Tonic"
  //||- enterInformation ("Login", "Enter your credentials and login") [])
  //>>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      //])
  //where
  //authenticatedTonic {Credentials|username, password}
    //=            authenticateUser username password >>=
      //\mbUser -> case mbUser of
                   //Just user -> workAs user (tonicUI appName)
                   //Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

derive class iTask FileError

getTasks :: TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: TonicModule String -> Maybe TonicTask
getTask tm tn = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task ()
tonicUI appName
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Active Dynamic cockpit it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just viewStatic)
    , OnAction (Action "Dynamic Task Instance Browser" []) (\_ -> Just viewDynamic)
    ]

viewStatic :: Task ()
viewStatic
  =      (selectModule >&> withSelection noModuleSelection (
  \mn -> getModule mn >>-
  \tm -> (selectTask tm >&> withSelection noTaskSelection (
  \tn -> maybe (return ())
           (\tt -> viewStaticTask tm tt @! ())
           (getTask tm tn)
         )) <<@ ArrangeSplit Horizontal True
         )) <<@ ArrangeSplit Horizontal True
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: TonicModule TonicTask -> Task (Editlet (Maybe TonicTask) [TonicletDiff])
viewStaticTask {tm_name} tt =
      viewInformation ("Arguments for task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") [] tt.tt_args
  ||- viewInformation
        ("Static visual task representation of task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") []
        (toniclet tt Nothing)

viewDynamic :: Task ()
viewDynamic =
          enterChoiceWithShared "Active blueprint instances" [] (mapRead 'DM'.elems tonicSharedRT) >>=
  \trt -> maybe (return ())
            (\bp -> viewInformation (blueprintTitle trt bp) [] ()
                ||- viewTaskArguments trt bp
                ||- viewSharedInformation "Blueprint:"
                      [ViewWith (\_ -> toniclet bp trt.trt_activeNodeId)]
                      tonicSharedRT
                 @! ())
            trt.trt_bpinstance
  where
  blueprintTitle    trt bp = snd trt.trt_bpref +++ " yields " +++ aOrAn bp.tt_resty
  viewTaskArguments trt bp = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs trt bp) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection              = viewInformation () [] "Select argument..."
  collectArgs       trt bp = zipWith (\(argnm, argty) (_, vi) -> (argnm +++ " is " +++ aOrAn argty, vi)) bp.tt_args trt.trt_params
  aOrAn str
    | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
    | otherwise                                       = "a " +++ str

tonicViewer :: String -> PublishedTask
tonicViewer appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)
