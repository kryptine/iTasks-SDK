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
import iTasks.API.Extensions.SVG.SVGlet
//import iTasks.API.Extensions.Tonic.Toniclet
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import StdArray
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor, Data.List
import qualified Data.Map as DM
from Data.Set import :: Set
import qualified Data.Set as DS
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Graphics.Scalable
from Control.Monad.State import :: State, :: StateT, :: Identity, instance Monad StateT, instance Applicative StateT, instance Functor StateT
from Control.Monad.Identity import instance Monad Identity, instance Applicative Identity, instance Functor Identity
import qualified Control.Applicative as CA
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap

derive class iTask TonicRT

tonicSharedRT :: Shared TonicRTMap
tonicSharedRT = sharedStore "tonicSharedRT" 'DM'.newMap

getModule :: !String -> Task TonicModule
getModule moduleName
  =           getTonicDir >>-
    \dir ->   accWorld (readFile (dir </> (moduleName +++ ".tonic"))) >>-
    \mjson -> case mjson of
                Ok json   -> case fromJSON (fromString json) of
                               Just gg  -> return gg
                               _        -> err ("Failed to deserialize JSON: " +++ json)
                Error msg -> err (toString msg)
  where
  err msg = throw ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg)

tonicViewInformation :: !String !a -> Task () | iTask a
tonicViewInformation d v = viewInformation d [] v @! ()

tonicWrapTaskBody :: !ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody mn tn args (Task eval) = getModule mn >>- Task o eval`
  where
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
                    , trt_parentTaskId = maybe (TaskId instanceNo 0)
                                           (\rt -> rt.trt_taskId)
                                           (firstParent rtMap instanceNo callTrace)
                    , trt_output       = Nothing
                    }
        = snd ('DSDS'.write ('DM'.put currTaskId tonicRT rtMap) tonicSharedRT iworld)
    eval` mod event evalOpts taskTree=:(TCDestroy tt) iworld
      = eval event evalOpts taskTree (maybeSt iworld attemptDel (taskIdFromTaskTree tt))
      where
      attemptDel currTaskId iworld
        # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
        = okSt iworld (\rtMap -> snd o 'DSDS'.write ('DM'.del currTaskId rtMap) tonicSharedRT) mrtMap
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
firstParent _     _          [] = Nothing
firstParent rtMap instanceNo [parentTaskNo:parentTaskNos]
  = maybe (firstParent rtMap instanceNo parentTaskNos) Just
      ('DM'.get (TaskId instanceNo parentTaskNo) rtMap)
import StdDebug
tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{callTrace} taskTree iworld
    = eval event evalOpts taskTree (maybeSt iworld
                                      (addTrace callTrace)
                                      (taskIdFromTaskTree taskTree))
    where
    addTrace callTrace (TaskId instanceNo taskNo) iworld
      # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
      = okSt iworld updRTMap mrtMap
      where
      updRTMap rtMap iworld
        //# traceStr = foldr (\x xs -> toString x +++ " " +++ xs) "" callTrace
        # nids     = foldr (\x xs -> toString x +++ " " +++ xs) "" nid
        //# iworld = trace_n (mn +++ "." +++ tn +++ " traceStr: " +++ traceStr +++ " nids: " +++ nids) iworld
        # rtMap = maybe rtMap // TODO 0 0 2 isn't traced here. parent not found somehow?
                    (\rt -> trace_n (toString rt.trt_taskId +++ " " +++ nids) 'DM'.put rt.trt_taskId {rt & trt_activeNodeId = Just nid} rtMap)
                    (firstParent rtMap instanceNo [taskNo:callTrace])
        = snd ('DSDS'.write rtMap tonicSharedRT iworld)
  eval` event evalOpts taskTree iworld = eval event evalOpts taskTree iworld

tonicWrapAppLam1 :: ModuleName TaskName [Int] (a -> Task b) -> a -> Task b
tonicWrapAppLam1 mn tn nid f = \x -> tonicWrapApp mn tn nid (f x)

tonicWrapAppLam2 :: ModuleName TaskName [Int] (a b -> Task c) -> a b -> Task c
tonicWrapAppLam2 mn tn nid f = \x y -> tonicWrapApp mn tn nid (f x y)

tonicWrapAppLam3 :: ModuleName TaskName [Int] (a b c -> Task d) -> a b c -> Task d
tonicWrapAppLam3 mn tn nid f = \x y z -> tonicWrapApp mn tn nid (f x y z)

tonicSharedListOfTask :: Shared (Map (TaskId, [Int]) (IntMap TaskId))
tonicSharedListOfTask = sharedStore "tonicSharedListOfTask" 'DM'.newMap

tonicWrapParallel :: ModuleName TaskName [Int] ([Task a] -> Task b) [Task a] -> Task b
tonicWrapParallel mn tn nid f ts = Task eval
  where
  eval event evalOpts taskTree iworld
    # (ts, iworld) = case taskIdFromTaskTree taskTree of
                       Just tid
                         # k              = (tid, nid)
                         # (mlot, iworld) = 'DSDS'.read tonicSharedListOfTask iworld
                         = case mlot of
                             Ok mlot
                               # (_, iworld)    = 'DSDS'.write ('DM'.put k 'DIS'.newMap mlot) tonicSharedListOfTask iworld
                               = (tonicWrapListOfTask mn tn nid tid ts, iworld)
                             _
                               = (ts, iworld)
                       _
                         = (ts, iworld)
    = case f ts of
        Task eval` -> eval` event evalOpts taskTree iworld

tonicWrapListOfTask :: ModuleName TaskName [Int] TaskId [Task a] -> [Task a]
tonicWrapListOfTask mn tn nid parentId ts = zipWith registerTask [0..] ts
  where
  registerTask :: Int (Task a) -> Task a
  registerTask n (Task eval) = Task eval`
    where
    eval` event evalOpts taskTree iworld
      # (mlot, iworld) = 'DSDS'.read tonicSharedListOfTask iworld
      # iworld         = case taskIdFromTaskTree taskTree of
                           Just tid -> okSt iworld (updLoT tid) mlot
                           _        -> iworld
      = eval event evalOpts taskTree iworld
    updLoT tid mlot iworld
      # k      = (parentId, nid)
      # tidMap = case 'DM'.get k mlot of
                    Just tidMap -> tidMap
                    _           -> 'DIS'.newMap
      # tidMap = 'DIS'.put n tid tidMap
      # mlot   = 'DM'.put k tidMap mlot
      = snd ('DSDS'.write mlot tonicSharedListOfTask iworld)

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

getTasks :: !TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: !TonicModule !String -> Maybe TonicTask
getTask tm tn = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task ()
tonicUI appName
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Dynamic Task Instance Browser it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just viewStatic)
    , OnAction (Action "Dynamic Task Instance Browser" []) (\_ -> Just viewDynamic)
    ]

viewStatic :: Task ()
viewStatic
  =      (selectModule >&> withSelection noModuleSelection (
  \mn -> getModule mn >>-
  \tm -> (selectTask tm >&> withSelection noTaskSelection (
  \tn -> maybe (return ())
           (\tt -> viewStaticTask [] tm tt @! ())
           (getTask tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
            <<@ FullScreen
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: [(ModuleName, TaskName)] TonicModule TonicTask -> Task ()
viewStaticTask navstack {tm_name} tt =
      viewInformation ("Arguments for task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") [] tt.tt_args
  ||- updateInformation
        ("Static visual task representation of task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'")
        [imageUpdate id (mkTaskImage (defaultTRT tt) 'DM'.newMap 'DM'.newMap) (const id)]
        {ActionState | state = tt, action = Nothing} >>*
        [ OnValue (doAction (navigate (Just (tm_name, tt.tt_name))))
        , OnAction (Action "Back" []) (back navstack)] @! ()
  where
  back []           _ = Nothing
  back [prev:stack] _ = Just (navigate` Nothing prev)
  navigate mparent (mn, tn) _ = navigate` mparent (mn, tn)
  navigate` mparent (mn, tn)
    = getModule mn >>*
      [ OnValue onNavVal
      , OnAllExceptions (const (return ()))
      ]
    where
    onNavVal (Value tm _) = fmap (\tt -> viewStaticTask (maybe navstack (\parent -> [parent:navstack]) mparent) tm tt @! ()) (getTask tm tn)
    onNavVal _            = Nothing
  defaultTRT tt
    = { trt_taskId       = TaskId -1 -1
      , trt_params       = []
      , trt_bpref        = (tm_name, tt.tt_name)
      , trt_bpinstance   = Just tt
      , trt_activeNodeId = Nothing
      , trt_parentTaskId = TaskId -2 -2
      , trt_output       = Nothing
      }


dynamicParent :: TaskId -> Task (Maybe TonicRT)
dynamicParent childId
  =       get tonicSharedRT >>-
  \rtm -> return (maybe Nothing
                    (\rt -> 'DM'.get rt.trt_parentTaskId rtm)
                    ('DM'.get childId rtm))

:: DynamicView =
  { moduleName :: String
  , taskName   :: String
  }

derive class iTask DynamicView

viewDynamic :: Task ()
viewDynamic
  = (enterChoiceWithShared "Active blueprint instances" [ChooseWith (ChooseFromGrid customView)] (mapRead 'DM'.elems tonicSharedRT) >&>
    withSelection noBlueprintSelection viewInstance) <<@ ArrangeWithSideBar 0 LeftSide 340 True
                                                     <<@ FullScreen

  where
  customView rt = { DynamicView | moduleName = fst rt.trt_bpref, taskName = snd rt.trt_bpref }
  //customView rt = rt
  noBlueprintSelection = viewInformation () [] "Select blueprint instance"

viewInstance :: TonicRT -> Task ()
viewInstance trt=:{trt_bpinstance = Just bp} =
                      dynamicParent trt.trt_taskId >>-
  \mbprnt ->          (viewInformation (blueprintTitle trt bp) [] () ||-
                      viewTaskArguments trt bp ||-
                      (watch (tonicSharedListOfTask |+| tonicSharedRT) >>-
  \(maplot, rtmap) -> updateInformation "Blueprint:" [imageUpdate id (mkTaskImage trt maplot rtmap) (const id)] {ActionState | state = bp, action = Nothing} @! ())) >>*
                      [OnAction (Action "Parent task" [ActionIcon "open"]) (\_ -> fmap viewInstance mbprnt)]
  where
  blueprintTitle    trt bp = snd trt.trt_bpref +++ " yields " +++ prefixAOrAn bp.tt_resty
  viewTaskArguments trt bp = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs trt bp) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection              = viewInformation () [] "Select argument..."
  collectArgs       trt bp = zipWith (\(argnm, argty) (_, vi) -> (argnm +++ " is " +++ prefixAOrAn argty, vi)) bp.tt_args trt.trt_params
viewInstance _ = return ()

tonicViewer :: String -> PublishedTask
tonicViewer appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

outgoingTaskEdges :: TonicRT -> Set (ModuleName, TaskName)
outgoingTaskEdges trt = outgoingTaskEdges` trt 'DM'.newMap
  where
  outgoingTaskEdges` trt acc
    //# entry = case 'DM'.get trt.trt_bpref acc of
                //Just m -> m
                //_      -> []
    # succs = fmap (\tt -> successors tt.tt_body) trt.trt_bpinstance
    = case succs of
        Just ss -> ss
        _       -> 'DS'.newSet

  successors :: !TExpr -> Set (ModuleName, TaskName)
  successors (TBind lhs _ rhs)    = 'DS'.union (successors lhs) (successors rhs)
  successors (TReturn texpr)      = successors texpr
  successors (TTaskApp _ mn tn _) = 'DS'.singleton (mn, tn)
  successors (TLet _ bdy)         = successors bdy
  successors (TCaseOrIf _ pats)   = 'DS'.unions (map (successors o snd) pats)
  successors (TStep lexpr conts)  = 'DS'.union (successors lexpr) ('DS'.unions [succStepCont x \\ T x <- conts])
  successors (TParallel _ par)    = succPar par
  successors (TAssign _ t)        = successors t
  successors (TShare ts sn args)  = 'DS'.newSet
  successors (TTransform lhs _ _) = successors lhs
  successors (TVar _ pp)          = 'DS'.newSet
  successors (TCleanExpr _ pp)    = 'DS'.newSet

  succStepCont (StepOnValue    sf)   = succStepCont` sf
  succStepCont (StepOnAction _ sf)   = succStepCont` sf
  succStepCont (StepOnException _ e) = successors e

  succStepCont` (Always     e)         = successors e
  succStepCont` (HasValue   _ e)       = successors e
  succStepCont` (IfStable   _ e)       = successors e
  succStepCont` (IfUnstable _ e)       = successors e
  succStepCont` (IfCond     _ _ e)     = successors e
  succStepCont` (IfValue    _ _ _ _ e) = successors e
  succStepCont` (CustomFilter _)       = 'DS'.newSet

  succPar (ParSumL e1 e2)  = 'DS'.union (successors e1) (successors e2)
  succPar (ParSumR e1 e2)  = 'DS'.union (successors e1) (successors e2)
  succPar (ParSumN (T es)) = 'DS'.unions (map successors es)
  succPar (ParProd (T es)) = 'DS'.unions (map successors es)
  succPar _                = 'DS'.newSet


ArialRegular10px :== { fontfamily  = "Arial"
                     , fontysize   = 10.0
                     , fontstretch = "normal"
                     , fontstyle   = "normal"
                     , fontvariant = "normal"
                     , fontweight  = "normal"
                     }

ArialBold10px :== { fontfamily  = "Arial"
                  , fontysize   = 10.0
                  , fontstretch = "normal"
                  , fontstyle   = "normal"
                  , fontvariant = "normal"
                  , fontweight  = "bold"
                  }

ArialItalic10px :== { fontfamily = "Arial"
                  , fontysize    = 10.0
                  , fontstretch  = "normal"
                  , fontstyle    = "italic"
                  , fontvariant  = "normal"
                  , fontweight   = "normal"
                  }

:: MkImageInh =
  { inh_trt    :: !TonicRT
  , inh_maplot :: !(Map (!TaskId, ![Int]) (IntMap TaskId))
  , inh_rtmap  :: !TonicRTMap
  }

mkTaskImage :: !TonicRT !(Map (TaskId, [Int]) (IntMap TaskId)) !TonicRTMap !ModelTy -> Image ModelTy
mkTaskImage trt maplot rtmap {ActionState | state = tt}
             #! inh = { inh_trt = trt, inh_maplot = maplot, inh_rtmap = rtmap }
             =  'CMS'.evalState (tExpr2Image inh tt.tt_body `b`
  \tt_body` -> tTaskDef tt.tt_name tt.tt_resty tt.tt_args tt_body`) 0

:: ModelTy :== ActionState (ModuleName, TaskName) TonicTask

:: TImg :== State Int (Image ModelTy)

(`b`) ma a2mb :== bind ma a2mb

tExpr2Image :: !MkImageInh !TExpr -> TImg
tExpr2Image inh (TBind lhs mpat rhs)       = tBind         inh lhs mpat rhs
tExpr2Image inh (TReturn texpr)            = tReturn       inh texpr
tExpr2Image inh (TTaskApp eid mn tn targs) = tTaskApp      inh eid mn tn targs
tExpr2Image inh (TLet pats bdy)            = tLet          inh pats bdy
tExpr2Image inh (TCaseOrIf e pats)         = tCaseOrIf     inh e pats
tExpr2Image inh (TStep lexpr conts)        = tStep         inh lexpr conts
tExpr2Image inh (TParallel eid par)        = tParallel     inh eid par
tExpr2Image inh (TAssign usr t)            = tAssign       inh usr t
tExpr2Image inh (TShare ts sn args)        = tShare        inh ts sn args
tExpr2Image inh (TTransform lhs vn args)   = tTransformApp inh lhs vn args
tExpr2Image inh (TVar _ pp)                = 'CA'.pure (text ArialRegular10px pp)
tExpr2Image inh (TCleanExpr _ pp)          = 'CA'.pure (text ArialRegular10px pp)

tArrowTip :: Image ModelTy
tArrowTip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]

tLineMarker :: Maybe (Markers ModelTy)
tLineMarker = Just {defaultMarkers & markerEnd = Just tArrowTip}

tHorizConn :: Image ModelTy
tHorizConn = xline Nothing (px 8.0)

tHorizConnArr :: Image ModelTy
tHorizConnArr = xline tLineMarker (px 16.0)

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip)}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just tArrowTip}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip), markerEnd = Just tArrowTip}) (px 16.0)

// TODO margin around cases
tCaseOrIf :: !MkImageInh !PPExpr ![(!Pattern, !TExpr)] -> TImg
tCaseOrIf inh ppexpr pats
  #! patStrs  = map fst pats
  #! patExprs = map snd pats
  =         'CM'.mapM (\_ -> dispenseUniq) patExprs `b`
  \uniqs -> 'CM'.mapM (tExpr2Image inh) patExprs `b`
            ('CA'.pure o mkCaseOrIf patStrs uniqs)
  where
  mkCaseOrIf :: ![String] ![Int] ![Image ModelTy] -> Image ModelTy
  mkCaseOrIf patStrs uniqs nextTasks
    #! nextTasks    = prepCases patStrs uniqs nextTasks
    #! vertConn     = mkVertConn uniqs
    #! nextTasks`   = above (repeat AtMiddleX) [] nextTasks Nothing
    #! textHeight   = ArialRegular10px.fontysize
    #! textWidth    = textxspan ArialRegular10px ppexpr
    #! edgeMargin   = textHeight * 2.0
    #! centerX      = (textWidth /. 2.0) + px edgeMargin
    #! leftCorner   = (px 0.0, y textHeight edgeMargin (px 0.0))
    #! topCorner    = (centerX, ~ (y textHeight edgeMargin centerX))
    #! rightCorner  = (centerX *. 2.0, y textHeight edgeMargin (px 0.0))
    #! bottomCorner = (centerX, y textHeight edgeMargin centerX)
    #! diamond      = polygon Nothing [ leftCorner, topCorner, rightCorner, bottomCorner ]
                        <@< { fill   = toSVGColor "white" }
                        <@< { stroke = toSVGColor "black" }
    #! diamond`   = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ diamond
                                                               , text ArialRegular10px ppexpr] Nothing
    = beside (repeat AtMiddleY) [] [diamond`, tHorizConn, vertConn, nextTasks`, vertConn] Nothing
  y :: !Real !Real !Span -> Span
  y textHeight edgeMargin x = x *. (textHeight / edgeMargin)

mkVertConn :: ![Int] -> Image ModelTy
mkVertConn uniqs
  | length uniqs < 2 = empty (px 0.0) (px 0.0)
  | otherwise
      #! firstUniq = hd uniqs
      #! lastUniq  = last uniqs
      #! allYSpans = foldr (\x acc -> imageyspan (imageTag x) + acc) (px 0.0) uniqs
      = above (repeat AtMiddleX) []
          [ yline Nothing (imageyspan (imageTag firstUniq) /. 2.0) <@< { stroke = toSVGColor "white" }
          , yline Nothing (allYSpans - (imageyspan (imageTag firstUniq) /. 2.0) - (imageyspan (imageTag lastUniq) /. 2.0)) <@< { stroke = toSVGColor "black" }
          , yline Nothing (imageyspan (imageTag lastUniq) /. 2.0) <@< { stroke = toSVGColor "white" } ]
          Nothing

tShare :: !MkImageInh !TShare !VarName ![VarName] -> TImg
tShare inh sh sn args = dispenseUniq `b` (mkShareImg sh sn args)
  where
  mkShareImg :: !TShare !VarName ![VarName] !Int -> TImg
  mkShareImg sh sn args uniq
    #! boxTxt  = case sh of
                   Get          -> "    "
                   (Set ppexpr) -> ppexpr
                   (Upd ppexpr) -> ppexpr
    #! boxRect = rect (textxspan ArialRegular10px boxTxt + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                   <@< { fill   = toSVGColor "white" }
                   <@< { stroke = toSVGColor "black" }
    #! boxImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [boxRect, text ArialRegular10px boxTxt] Nothing
    #! arr     = case sh of
                   Get        -> tVertDownConnArr
                   Set ppexpr -> tVertUpConnArr
                   Upd ppexpr -> tVertUpDownConnArr
    #! shareArr = tag (imageTag uniq) (above (repeat AtMiddleX) [] [mkShare, arr] Nothing)
    #! emptyImg = empty zero (imageyspan (imageTag uniq))
    // TODO Add arrows to/from box if the box is smaller than the share
    = 'CA'.pure (above (repeat AtMiddleX) [] [shareArr, boxImg, emptyImg] Nothing)
  mkShare :: Image ModelTy
  mkShare
    #! box1Rect = rect (textxspan ArialRegular10px sn + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                    <@< { fill   = toSVGColor "white" }
                    <@< { stroke = toSVGColor "black" }
    #! box1Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box1Rect, text ArialRegular10px sn] Nothing
    #! box2Text = above (repeat AtMiddleX) [] (map (text ArialRegular10px) args) Nothing
    #! numArgs  = length args
    #! box2Rect = rect (maxSpan (map (textxspan ArialRegular10px) args) + px 5.0) (px (ArialRegular10px.fontysize * toReal numArgs + 10.0))
                    <@< { fill   = toSVGColor "white" }
                    <@< { stroke = toSVGColor "black" }
    #! box2Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] (if (numArgs > 0) [box2Rect, box2Text] []) Nothing
    = above (repeat AtMiddleX) [] [box1Img, box2Img] Nothing

tLet :: !MkImageInh ![(!Pattern, !PPExpr)] !TExpr -> TImg
tLet inh pats expr
  =          dispenseUniq `b`
  \textNo -> tExpr2Image inh expr `b`
             ('CA'.pure o mkLet pats textNo)
  where
  mkLet :: ![(!String, !String)] !Int !(Image ModelTy) -> Image ModelTy
  mkLet pats textNo t
    #! letText = tag (imageTag textNo) (above (repeat (AtMiddleX)) [] (map (\(var, expr) -> text ArialRegular10px (var +++ " = " +++ expr)) pats) Nothing)
    #! letBox  = rect (imagexspan (imageTag textNo)) (px ArialRegular10px.fontysize *. (length pats + 1))
                   <@< { fill   = toSVGColor "white" }
                   <@< { stroke = toSVGColor "black" }
    #! letImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
    = beside (repeat AtMiddleY) [] [letImg, tHorizConnArr, t] Nothing

tBind :: !MkImageInh !TExpr !(Maybe Pattern) !TExpr -> TImg
tBind inh l mpat r
  =      tExpr2Image inh l `b`
  \l` -> tExpr2Image inh r `b`
  \r` -> 'CA'.pure (beside (repeat AtMiddleY) [] [l`, tHorizConnArr, r`] Nothing) // TODO Add label

tParallel :: !MkImageInh ![Int] !TParallel -> TImg
tParallel inh eid (ParSumL l r)
  =      tExpr2Image inh l `b`
  \l` -> tExpr2Image inh r `b`
  \r` -> 'CA'.pure (mkParSumL l` r`)
  where
  mkParSumL :: !(Image ModelTy) !(Image ModelTy) -> Image ModelTy
  mkParSumL l` r`
    #! l` = margin (px 5.0, px 5.0) l`
    #! r` = margin (px 5.0, px 5.0) r`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel inh eid (ParSumR l r)
  =      tExpr2Image inh l `b`
  \l` -> tExpr2Image inh r `b`
  \r` -> 'CA'.pure (mkParSumR l` r`)
  where
  mkParSumR :: !(Image ModelTy) !(Image ModelTy) -> Image ModelTy
  mkParSumR l` r`
    #! l` = margin (px 5.0, px 5.0) l`
    #! r` = margin (px 5.0, px 5.0) r`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel inh eid (ParSumN ts) = mkParSum inh eid ts `b` ('CA'.pure o mkParSumN)
  where
  mkParSumN :: ![Image ModelTy] -> Image ModelTy
  mkParSumN ts`
    #! ts` = map (margin (px 5.0, px 5.0)) ts`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ above (repeat AtMiddleX) [] ts` Nothing, /* TODO lines to last delim,*/ tParSum] Nothing
  mkParSum :: !MkImageInh ![Int] !(PPOr [TExpr]) -> State Int [Image ModelTy]
  mkParSum inh eid (PP pp)
    = case 'DM'.get (inh.inh_trt.trt_taskId, eid) inh.inh_maplot of
        Just mptids
          -> 'CM'.mapM (\(mn, tn) -> tTaskApp inh eid "" tn []) [trt.trt_bpref \\ Just trt <- map (\tid -> 'DM'.get tid inh.inh_rtmap) ('DIS'.elems mptids)]
        _ -> 'CA'.pure [text ArialRegular10px pp]
  mkParSum _ _ (T xs) = 'CM'.mapM (tExpr2Image inh) xs
tParallel inh eid (ParProd ts)
  =         mkParProd inh eid ts `b`
  \imgs  -> 'CM'.mapM (\_ -> dispenseUniq) imgs `b`
  \uniqs -> 'CA'.pure (mkParProdCases uniqs imgs)
  where
  mkParProdCases :: ![Int] ![Image ModelTy] -> Image ModelTy
  mkParProdCases uniqs ts`
    #! ts`      = prepCases [] uniqs ts`
    #! vertConn = mkVertConn uniqs
    = beside (repeat AtMiddleY) [] [tParProd, tHorizConn, vertConn, above (repeat AtMiddleX) [] ts` Nothing, tHorizConn, vertConn, tHorizConnArr, tParProd] Nothing
  mkParProd :: !MkImageInh ![Int] !(PPOr [TExpr]) -> State Int [Image ModelTy]
  mkParProd inh eid (PP pp)
    = case 'DM'.get (inh.inh_trt.trt_taskId, eid) inh.inh_maplot of
        Just mptids
          -> 'CM'.mapM (\(mn, tn) -> tTaskApp inh eid "" tn []) [trt.trt_bpref \\ Just trt <- map (\tid -> 'DM'.get tid inh.inh_rtmap) ('DIS'.elems mptids)]
        _ -> 'CA'.pure [text ArialRegular10px pp]
  mkParProd _ _ (T xs)  = 'CM'.mapM (tExpr2Image inh) xs

moduleTaskNameForTaskId :: TonicRTMap TaskId -> Maybe (ModuleName, TaskName)
moduleTaskNameForTaskId trtmap tid
  = case 'DM'.get tid trtmap of
      Just trt -> Just trt.trt_bpref
      _        -> Nothing

tDiamond :: Image ModelTy
tDiamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0))
             <@< { fill   = toSVGColor "black" }
             <@< { stroke = toSVGColor "none" }

tStepStar :: Image ModelTy
tStepStar = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, star] Nothing
  where
  star = polygon Nothing
           [ (px 5.0, px 0.0)
           , (px 2.0, px 10.0)
           , (px 9.5, px 4.0)
           , (px 0.0, px 4.0)
           , (px 8.0, px 10.0) ] <@< { fill   = toSVGColor "white" }
                                 <@< { stroke = toSVGColor "none" }

tParSum :: Image ModelTy
tParSum = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, tPlus] Nothing

tParProd :: Image ModelTy
tParProd = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, rotate (deg 45.0) tPlus] Nothing

tPlus :: Image ModelTy
tPlus
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing
  where
  line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tStartSymb :: Image ModelTy
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image ModelTy
tStopSymb  = rect (px 16.0) (px 16.0)

prefixAOrAn :: !String -> String
prefixAOrAn str
  | str == "Unit"                                   = "nothing"
  | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
  | otherwise                                       = "a " +++ str

// TODO Start / stop symbols here
tTaskDef :: !String !String ![(!String, !String)] !(Image ModelTy) -> TImg
tTaskDef taskName resultTy taskArgsAndTys tdbody
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> dispenseUniq `b`
  \bodyNo -> 'CA'.pure (tTaskDef` taskName resultTy taskArgsAndTys tdbody nameNo argsNo bodyNo)
  where
  tTaskDef` :: !String !String ![(!String, !String)] !(Image ModelTy) !Int !Int !Int -> Image ModelTy
  tTaskDef` taskName resultTy taskArgsAndTys tdbody nameNo argsNo bodyNo
    #! maxXSpan     = maxSpan [imagexspan (imageTag nameNo), imagexspan (imageTag argsNo), imagexspan (imageTag bodyNo)]
    #! bgRect       = rect maxXSpan (imageyspan (imageTag nameNo) + imageyspan (imageTag argsNo) + imageyspan (imageTag bodyNo))
                        <@< { fill        = toSVGColor "white" }
                        <@< { stroke      = toSVGColor "black" }
                        <@< { strokewidth = px 1.0 }
                        <@< { xradius     = px 5.0 }
                        <@< { yradius     = px 5.0 }
    #! taskNameImg  = tag (imageTag nameNo) (margin (px 5.0) (text ArialBold10px (taskName +++ " yields " +++ prefixAOrAn resultTy)))
    #! taskArgsImgs = tag (imageTag argsNo) (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialRegular10px o mkArgAndTy) taskArgsAndTys) Nothing))
    #! taskBodyImgs = tag (imageTag bodyNo) (margin (px 5.0) (beside (repeat AtMiddleY) [] [tStartSymb, tHorizConnArr, tdbody, tHorizConnArr, tStopSymb] Nothing))
    #! taskContents = above (repeat AtLeft) [] (case taskArgsAndTys of
                                                 [] -> [taskNameImg, xline Nothing maxXSpan, taskBodyImgs]
                                                 _  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs, xline Nothing maxXSpan, taskBodyImgs]) Nothing
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskContents] Nothing
    where
    mkArgAndTy (arg, ty) = arg +++ " is " +++ prefixAOrAn ty

tTransformApp :: !MkImageInh !TExpr !VarName ![VarName] -> TImg
tTransformApp inh texpr tffun args
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> tExpr2Image inh texpr `b`
  \expr   -> 'CA'.pure (tTransformApp` inh.inh_trt tffun args nameNo argsNo expr)
  where
  tTransformApp` :: !TonicRT !VarName ![VarName] !Int !Int !(Image ModelTy) -> Image ModelTy
  tTransformApp` trt tffun args nameNo argsNo expr
    #! maxXSpan   = maxSpan [imagexspan (imageTag nameNo), imagexspan (imageTag argsNo)]
    #! bgRect     = rect maxXSpan (imageyspan (imageTag nameNo) + imageyspan (imageTag argsNo))
                      <@< { fill        = toSVGColor "white" }
                      <@< { stroke      = toSVGColor "black" }
                      <@< { strokewidth = px 1.0 }
    #! tfNameImg  = tag (imageTag nameNo) (margin (px 5.0) (text ArialItalic10px tffun))
    #! tfArgsImgs = tag (imageTag argsNo) (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
    #! tfContents = above (repeat AtLeft) [] (case args of
                                                [] -> [tfNameImg]
                                                _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
    #! tfApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
    = beside (repeat AtMiddleY) [] [tfApp, tHorizConnArr, expr] Nothing

tTaskApp :: !MkImageInh !ExprId !ModuleName !VarName ![TExpr] -> TImg
tTaskApp inh eid modName taskName taskArgs
  =             'CM'.mapM (tExpr2Image inh) taskArgs `b`
  \taskArgs` -> dispenseUniq `b`
  \tnNo      -> dispenseUniq `b`
  \taNo      -> 'CA'.pure (tTaskApp` inh.inh_trt eid modName taskName taskArgs` tnNo taNo)
  where
  tTaskApp` :: !TonicRT !ExprId !ModuleName !VarName ![Image ModelTy] !Int !Int -> Image ModelTy
  tTaskApp` trt eid modName taskName taskArgs` tnNo taNo
    #! maxXSpan     = maxSpan [imagexspan (imageTag tnNo), imagexspan (imageTag taNo)]
    #! bgRect       = rect maxXSpan (imageyspan (imageTag tnNo) + imageyspan (imageTag taNo))
                        <@< { fill        = if (Just eid == trt.trt_activeNodeId) (toSVGColor "lightgreen") (toSVGColor "white") }
                        <@< { stroke      = toSVGColor "black" }
                        <@< { strokewidth = px 1.0 }
                        <@< { xradius     = px 5.0 }
                        <@< { yradius     = px 5.0 }
    #! taskNameImg  = tag (imageTag tnNo) (margin (px 5.0) (text ArialBold10px taskName))
    #! taskArgsImgs = tag (imageTag taNo) (margin (px 5.0) (above (repeat AtLeft) [] taskArgs` Nothing))
    #! taskText     = above (repeat AtMiddleX) [] (case taskArgs` of
                                                    [] -> [taskNameImg]
                                                    _  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs]) Nothing
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
        <@< { ondblclick = \st -> { ActionState | st & action = Just (modName, taskName) } }

dispenseUniq :: State Int Int
dispenseUniq
  =     'CMS'.gets id `b`
  \s -> 'CMS'.put (s + 1) `b`
  \_ -> 'CA'.pure s

tReturn :: !MkImageInh !TExpr -> TImg
tReturn inh retval
  =           tExpr2Image inh retval `b`
  \retval` -> dispenseUniq `b`
  \tagNo   -> 'CA'.pure (tReturn` retval` tagNo)
  where
  tReturn` :: !(Image ModelTy) !Int -> Image ModelTy
  tReturn` retval` tagNo
    #! retval` = tag (imageTag tagNo) retval`
    #! oval    = ellipse (imagexspan (imageTag tagNo) + px 20.0) (imageyspan (imageTag tagNo) + px 10.0)
                   <@< { fill        = toSVGColor "white" }
                   <@< { stroke      = toSVGColor "black" }
                   <@< { strokewidth = px 1.0 }
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [oval, retval`] Nothing

tAssign :: !MkImageInh !TUser !TExpr -> TImg
tAssign inh user assignedTask
  =          tExpr2Image inh assignedTask `b`
  \at     -> dispenseUniq `b`
  \userNo -> dispenseUniq `b`
  \atNo   -> 'CA'.pure (tAssign` at userNo atNo)
  where
  tAssign` :: !(Image ModelTy) !Int !Int -> Image ModelTy
  tAssign` at userNo atNo
    #! maxXSpan = maxSpan [imagexspan (imageTag userNo), imagexspan (imageTag atNo)]
    #! taskNameImg = tag (imageTag userNo) (margin (px 5.0) (text ArialBold10px (ppUser user)))
    #! bgRect  = rect maxXSpan (imageyspan (imageTag userNo) + imageyspan (imageTag atNo))
                  <@< { fill        = toSVGColor "white" }
                  <@< { stroke      = toSVGColor "black" }
                  <@< { strokewidth = px 1.0 }
                  <@< { xradius     = px 5.0 }
                  <@< { yradius     = px 5.0 }
                  <@< { dash        = [5, 5] }
    #! at      = tag (imageTag atNo) (margin (px 5.0) (beside (repeat AtMiddleY) [] [tStartSymb, tHorizConnArr, at, tHorizConnArr, tStopSymb] Nothing))
    #! content = above (repeat AtMiddleX) [] [beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing, xline Nothing maxXSpan, at] Nothing
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing

ppUser :: !TUser -> String
ppUser TUAnyUser                       = "Any user"
ppUser (TUUserWithIdent ident)         = "User " +++ ident
ppUser (TUUserWithRole role)           = "Any user with role " +++ role
ppUser TUSystemUser                    = "Any system user"
ppUser TUAnonymousUser                 = "Any anonymous user"
ppUser (TUAuthenticatedUser usr roles) = "User " +++ usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles

tStep :: !MkImageInh !TExpr ![PPOr TStepCont] -> TImg
tStep inh lhsExpr conts
  =          'CM'.mapM (\_ -> dispenseUniq) conts `b`
  \uniqs  -> tExpr2Image inh lhsExpr `b`
  \lhs    -> 'CM'.mapM (tStepCont inh) conts `b`
  \conts` -> 'CA'.pure (tStep` lhs conts` uniqs)
  where
  tStep` :: !(Image ModelTy) ![Image ModelTy] ![Int] -> Image ModelTy
  tStep` lhs conts` uniqs
    #! conts`   = prepCases [] uniqs conts`
    #! vertConn = mkVertConn uniqs
    #! contsImg = above (repeat AtMiddleX) [] conts` Nothing
    = beside (repeat AtMiddleY) [] [lhs, tHorizConnArr, tStepStar, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tStepStar] Nothing

prepCases :: ![String] ![Int] ![Image ModelTy] -> [Image ModelTy]
prepCases patStrs uniqs pats
  #! pats     = zipWith (\uniq pat -> pat) uniqs pats
  #! maxXSpan = maxSpan (map (imagexspan o imageTag) uniqs)
  = zipWith3 (prepCase maxXSpan) uniqs pats (patStrs ++ repeat "")
  where
  prepCase :: !Span !Int !(Image ModelTy) !String -> Image ModelTy
  prepCase maxXSpan uniq pat patStr
    #! pat       = tag (imageTag uniq) pat
    = case patStr of
        ""
          #! linePart  = (maxXSpan - imagexspan (imageTag uniq)) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), leftLine, pat, rightLine] Nothing
        patStr
          #! textWidth = textxspan ArialRegular10px patStr + px 10.0
          #! linePart  = (maxXSpan - imagexspan (imageTag uniq) - textWidth) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          #! textBox   = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (ArialRegular10px.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text ArialRegular10px patStr] Nothing
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), textBox, leftLine, pat, rightLine] Nothing

tStepCont :: !MkImageInh !(PPOr TStepCont) -> TImg
tStepCont _   (PP pp) = 'CA'.pure (text ArialRegular10px pp)
tStepCont inh (T t)   = tStepCont` inh.inh_trt t
  where
  tStepCont` :: !TonicRT !TStepCont -> TImg
  tStepCont` trt (StepOnValue      sfilter) = tStepFilter trt Nothing sfilter
  tStepCont` trt (StepOnAction act sfilter) = tStepFilter trt (Just act) sfilter
  tStepCont` trt (StepOnException mpat te)  = tExpr2Image inh te `b` ('CA'.pure o mkOnException)
    where
    // TODO mpat
    mkOnException :: !(Image ModelTy) -> Image ModelTy
    mkOnException t = beside (repeat AtMiddleY) [] [tException, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter :: !TonicRT !(Maybe String) !TStepFilter -> TImg
  tStepFilter trt mact sfilter
    =        dispenseUniq `b`
    \uniq -> tStepFilter` trt uniq mact sfilter
  tStepFilter` :: !TonicRT !Int !(Maybe String) !TStepFilter -> TImg
  tStepFilter` trt uniq mact (Always te) = tExpr2Image inh te `b` ('CA'.pure o mkAlways uniq mact)
    where
    mkAlways :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
    mkAlways uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact alwaysFilter, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` trt uniq mact (HasValue mpat te) = tExpr2Image inh te `b` ('CA'.pure o mkHasValue uniq mact)
    where
    // TODO mpat
    mkHasValue :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
    mkHasValue uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact hasValueFilter, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` trt uniq mact (IfStable mpat te) = tExpr2Image inh te `b` ('CA'.pure o mkIfStable uniq mact)
    where
    // TODO mpat
    mkIfStable :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
    mkIfStable uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact tStable, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` trt uniq mact (IfUnstable mpat te) = tExpr2Image inh te `b` ('CA'.pure o mkIfUnstable uniq mact)
    where
    // TODO mpat
    mkIfUnstable :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
    mkIfUnstable uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact tUnstable, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` trt uniq mact (IfCond pp mpat te) = tExpr2Image inh te `b` ('CA'.pure o mkIfCond uniq mact)
    where
    // TODO mpat pp
    mkIfCond :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
    mkIfCond uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact alwaysFilter, tHorizConnArr, /* TODO edge and conditional */ t] Nothing
  tStepFilter` trt uniq mact (IfValue pat fn vars mpat te) = tExpr2Image inh te `b` \t -> tIfValue fn vars `b` ('CA'.pure o mkIfValue pat uniq mact t)
    where
    mkIfValue :: !String !Int !(Maybe String) !(Image ModelTy) !(Image ModelTy) -> Image ModelTy
    mkIfValue pat uniq mact t c = beside (repeat AtMiddleY) [] [addAction uniq mact hasValueFilter, tHorizConn, text ArialRegular10px pat, tHorizConnArr, c, tHorizConnArr, /* TODO mpat */ t] Nothing
  tStepFilter` trt uniq mact (CustomFilter pp) = 'CA'.pure (text ArialRegular10px pp)

alwaysFilter :: Image ModelTy
alwaysFilter = above (repeat AtMiddleX) [] [tStable, tUnstable, tNoVal] Nothing

hasValueFilter :: Image ModelTy
hasValueFilter = above (repeat AtMiddleX) [] [tStable, tUnstable] Nothing

addAction :: !Int !(Maybe String) !(Image ModelTy) -> Image ModelTy
addAction uniq (Just action) img
  #! imgtag    = imageTag uniq
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan imgtag + px 5.0) (imageyspan imgtag + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
                                               , tag imgtag (above (repeat AtMiddleX) [] [ beside (repeat AtMiddleY) [] [littleman, text ArialBold10px action] Nothing
                                                                                         , img] Nothing)
                                               ] Nothing
addAction _ _ img = img

littleman :: Image a
littleman = (overlay [] [(px -2.0, px 8.0), (px 3.0, px 1.0)] [ circle (px 20.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}
                                                              , circle (px 10.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}] Nothing) <@< {mask = rect (px 16.0) (px 16.0) <@< {fill = toSVGColor "white"}}

tIfValue :: !VarName ![VarName] -> TImg
tIfValue tffun args
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> 'CA'.pure (tIfValue` nameNo argsNo)
  where
  tIfValue` :: !Int !Int -> Image ModelTy
  tIfValue` nameNo argsNo
    #! maxXSpan   = maxSpan [imagexspan (imageTag nameNo), imagexspan (imageTag argsNo)]
    #! bgRect     = rect maxXSpan (imageyspan (imageTag nameNo) + imageyspan (imageTag argsNo))
                      <@< { fill        = toSVGColor "white" }
                      <@< { stroke      = toSVGColor "black" }
                      <@< { strokewidth = px 1.0 }
    #! tfNameImg  = tag (imageTag nameNo) (margin (px 5.0) (text ArialItalic10px tffun))
    #! tfArgsImgs = tag (imageTag argsNo) (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
    #! tfContents = above (repeat AtLeft) [] (case args of
                                                [] -> [tfNameImg]
                                                _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing

tException :: Image ModelTy
tException
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "!!"] Nothing

tStable :: Image ModelTy
tStable
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, rect (px 8.0) (px 8.0) <@< { fill = toSVGColor "black" }] Nothing

tUnstable :: Image ModelTy
tUnstable
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "W"] Nothing

tNoVal :: Image ModelTy
tNoVal
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "X"] Nothing

tLineArrow :: Image ModelTy
tLineArrow = polygon Nothing [ (px 0.0, px 0.0)
                             , (px 8.0, px 4.0)
                             , (px 0.0, px 8.0) ]

uniDirLineMarkers :: Maybe (Markers ModelTy)
uniDirLineMarkers = Just { markerStart = Nothing
                         , markerMid   = Nothing
                         , markerEnd   = Just tLineArrow }

biDirLineMarkers :: Maybe (Markers ModelTy)
biDirLineMarkers = Just { markerStart = Just tLineArrow
                        , markerMid   = Nothing
                        , markerEnd   = Just tLineArrow }
