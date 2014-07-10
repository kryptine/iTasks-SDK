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
import iTasks.API.Extensions.Tonic.TonicRenderer
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import qualified StdArray as SA
from StdArray import class Array, instance Array {#} Char
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
  noDots str = not ('SA'.select str 0 == '.')

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

derive class iTask MaybeError, FileError

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
  =            selectModule >>=
  \(mn, tm) -> selectTask tm >>=
  \(tn, tt) -> viewStaticTask tn mn tt >>|
  return ()

selectModule :: Task (String, TonicModule)
selectModule
  =      getTonicModules >>-
         enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)] >>=
  \mn -> getModule mn >>=
  \m  -> return (mn, m)

selectTask :: TonicModule -> Task (String, TonicTask)
selectTask tm
  =      enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm) >>=
  \tn -> case getTask tm tn of
           Just tt -> return (tn, tt)
           _       -> throw "Should not happen"

viewStaticTask :: String String TonicTask -> Task (Editlet (Maybe TonicTask) [TonicletDiff])
viewStaticTask tn mn tt =
      viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
  ||- viewInformation
        ("Static visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'") []
        (toniclet tonicRenderer (Just tt) Nothing)
  <<@ FullScreen

viewDynamic :: Task ()
viewDynamic =
             enterChoiceWithShared "Active blueprint instances" [] (mapRead 'DM'.elems tonicSharedRT) >>=
  \trt    -> case trt.trt_bpinstance of
               Just bpinst
                 ->            get tonicSharedRT >>-
                    \mp     -> viewInformation ((snd trt.trt_bpref) +++  " yields " +++ (aOrAn bpinst.tt_resty)) [] ()
                           ||- ((enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (zipWith (\(argnm, argty) (_, vi) -> (argnm +++ " is " +++ aOrAn argty, vi)) bpinst.tt_args trt.trt_params) >&> withSelection snd) <<@ ArrangeSplit Horizontal True)
                           ||- viewSharedInformation "Blueprint:"
                                 [ViewWith (\_ -> toniclet tonicRenderer trt.trt_bpinstance trt.trt_activeNodeId)]
                                 tonicSharedRT
                           @! ()
               _ -> return ()
  where
  aOrAn str
    | 'SA'.size str > 0 && isMember ('SA'.select str 0) ['e', 'E', 'u', 'U', 'i', 'I', 'o', 'O', 'a', 'A'] = "an " +++ str
    | otherwise = "a " +++ str

withSelection tfun s = whileUnchanged s (maybe (viewInformation () [] "Select argument..." @? const NoValue) tfun)

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)
