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
//import iTasks.API.Extensions.Tonic.Toniclet
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import StdArray
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor, Data.Graph, Data.List
import qualified Data.Map as DM

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

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
tonicWrapTaskBody` mn tn args (Task eval) = getModule mn >>- Task o eval`
  where
    eval` mod event evalOpts taskTree=:(TCDestroy tt) iworld
      = eval event evalOpts taskTree (maybeSt iworld attemptDel (taskIdFromTaskTree tt))
      where
      attemptDel currTaskId iworld
        # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
        = okSt iworld (\rtMap -> snd o 'DSDS'.write ('DM'.del currTaskId rtMap) tonicSharedRT) mrtMap
    eval` mod event evalOpts=:{callTrace} taskTree=:(TCInit currTaskId=:(TaskId instanceNo _) _) iworld
      # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
      = eval event evalOpts taskTree (okSt iworld (updateInstance instanceNo) mrtMap)
      where
      updateInstance instanceNo rtMap iworld
        # tonicRT = { trt_taskId       = currTaskId
                    , trt_params       = args
                    , trt_bpref        = (mn, tn)
                    , trt_bpinstance   = getTask mod tn
                    , trt_activeNodeId = Nothing
                    , trt_parentTaskId = maybe (TaskId instanceNo 0) (\rt -> rt.trt_taskId) (firstParent rtMap instanceNo callTrace)
                    , trt_output       = Nothing
                    }
        = snd ('DSDS'.write ('DM'.put currTaskId tonicRT rtMap) tonicSharedRT iworld)
    eval` mod event evalOpts taskTree iworld
      # (tr, iworld) = eval event evalOpts taskTree iworld
      = (tr, maybeSt iworld (readAndUpdateRTMap tr) (taskIdFromTaskTree taskTree))
      where
      readAndUpdateRTMap tr currTaskId iworld
        # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
        = okSt iworld (updateRTMap tr currTaskId) mrtMap
      updateRTMap tr currTaskId rtMap iworld
        = maybeSt iworld
            (\rt -> snd o 'DSDS'.write ('DM'.put currTaskId {rt & trt_output = resultToOutput tr} rtMap) tonicSharedRT)
            ('DM'.get currTaskId rtMap)
    resultToOutput (ValueResult tv _ _ _) = tvViewInformation tv
    resultToOutput _                      = Nothing
    tvViewInformation NoValue     = Nothing
    tvViewInformation (Value v _) = Just (viewInformation "Task result" [] v @! ())

firstParent :: TonicRTMap Int [Int] -> Maybe TonicRT
firstParent _     instanceNo [] = Nothing
firstParent rtMap instanceNo [parentTaskNo:parentTaskNos]
  = maybe (firstParent rtMap instanceNo parentTaskNos) Just ('DM'.get (TaskId instanceNo parentTaskNo) rtMap)

tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{callTrace} taskTree iworld
    = eval event evalOpts taskTree (maybeSt iworld
                                      (\(TaskId instanceNo _) -> addTrace instanceNo callTrace)
                                      (taskIdFromTaskTree taskTree))
    where
    addTrace instanceNo callTrace iworld
      # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
      = okSt iworld updRTMap mrtMap
      where
      updRTMap rtMap iworld
        # rtMap = maybe rtMap
                    (\rt -> 'DM'.put rt.trt_taskId {rt & trt_activeNodeId = Just nid} rtMap)
                    (firstParent rtMap instanceNo callTrace)
        = snd ('DSDS'.write rtMap tonicSharedRT iworld)

tonicWrapAppLam1 :: ModuleName TaskName [Int] (a -> Task b) -> a -> Task b
tonicWrapAppLam1 mn tn nid f = \x -> tonicWrapApp mn tn nid (f x)

tonicWrapAppLam2 :: ModuleName TaskName [Int] (a b -> Task c) -> a b -> Task c
tonicWrapAppLam2 mn tn nid f = \x y -> tonicWrapApp mn tn nid (f x y)

tonicWrapAppLam3 :: ModuleName TaskName [Int] (a b c -> Task d) -> a b c -> Task d
tonicWrapAppLam3 mn tn nid f = \x y z -> tonicWrapApp mn tn nid (f x y z)

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
                   //Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" @! ()

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

//viewStaticTask :: TonicModule TonicTask -> Task (Editlet (Maybe TonicTask) [TonicletDiff])
viewStaticTask {tm_name} tt =
      viewInformation ("Arguments for task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") [] tt.tt_args
  ||- viewInformation
        ("Static visual task representation of task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") []
        (return ()) // TODO       (toniclet tt Nothing)

viewDynamic :: Task ()
viewDynamic =
          enterChoiceWithShared "Active blueprint instances" [] (mapRead 'DM'.elems tonicSharedRT) >>=
  \trt -> maybe (return ())
            (\bp -> viewInformation (blueprintTitle trt bp) [] ()
                ||- viewTaskArguments trt bp
                ||- viewSharedInformation "Blueprint:"
                      // [ViewWith (\_ -> toniclet bp trt.trt_activeNodeId)]
                      [] // TODO
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
