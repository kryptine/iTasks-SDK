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
import iTasks.API.Extensions.Admin.WorkflowAdmin
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
import Text

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

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
tonicWrapTaskBody mn tn args (Task eval) = getModule mn >>*
  [ OnValue onModule
  , OnAllExceptions onException
  ]
  where
    onModule (Value mod _) = Just (Task (eval` (Just mod)))
    onModule _             = Nothing
    onException _ = Task (eval` Nothing)
    eval` mod event evalOpts=:{callTrace} taskTree=:(TCInit currTaskId=:(TaskId instanceNo _) _) iworld
      # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
      = eval event evalOpts taskTree (okSt iworld (updateInstance instanceNo) mrtMap)
      where
      updateInstance instanceNo rtMap iworld
        # (curr, iworld) = iworld!current
        # tonicRT = { trt_taskId        = currTaskId
                    , trt_params        = args
                    , trt_bpref         = (mn, tn)
                    , trt_bpinstance    = case mod of
                                            Just mod -> getTask mod tn
                                            _        -> Nothing
                    , trt_activeNodeId  = Nothing
                    , trt_parentTaskId  = maybe (TaskId instanceNo 0)
                                            (\rt -> rt.trt_taskId)
                                            (firstParent rtMap instanceNo callTrace)
                    , trt_involvedUsers = [curr.user]
                    , trt_output        = Nothing
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
        # (curr, iworld) = iworld!current
        = maybeSt iworld
            (\rt -> snd o 'DSDS'.write ('DM'.put currTaskId {rt & trt_output        = resultToOutput tr
                                                                , trt_involvedUsers = [curr.user : resultUsers tr]} rtMap) tonicSharedRT)
            ('DM'.get currTaskId rtMap)
    resultToOutput (ValueResult tv _ _ _) = tvViewInformation tv
    resultToOutput _                      = Nothing
    resultUsers (ValueResult _ te _ _) = te.TaskEvalInfo.involvedUsers
    resultUsers _                      = []
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

tonicLogin :: Task ()
tonicLogin = tonicUI
//tonicLogin :: Task Void
//tonicLogin = forever (
      //(viewTitle "Tonic"
  //||- enterInformation ("Login", "Enter your credentials and login") [])
  //>>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      //])
  //where
  //authenticatedTonic {Credentials|username, password}
    //=            authenticateUser username password >>=
      //\mbUser -> case mbUser of
                   //Just user -> workAs user tonicUI
                   //Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" @! ()

derive class iTask FileError

getTasks :: !TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: !TonicModule !String -> Maybe TonicTask
getTask tm tn = 'DM'.get tn tm.tm_tasks

tonicUI :: Task ()
tonicUI
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Dynamic Task Instance Browser it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just tonicStaticBrowser)
    , OnAction (Action "Dynamic Task Instance Browser" []) (\_ -> Just tonicDynamicBrowser)
    ]

tonicStaticWorkflow :: Workflow
tonicStaticWorkflow = workflow "Tonic Static Browser" "Tonic Static Browser" tonicStaticBrowser

tonicDynamicWorkflow :: Workflow
tonicDynamicWorkflow = workflow "Tonic Dynamic Browser" "Tonic Dynamic Browser" tonicDynamicBrowser

tonicStaticBrowser :: Task ()
tonicStaticBrowser
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

viewTitle` :: !a -> Task a | iTask a
viewTitle` a = viewInformation (Title title) [ViewWith view] a <<@ InContainer
  where
  title = toSingleLineText a
  view a = DivTag [] [SpanTag [StyleAttr "font-size: 16px"] [Text title]]

viewStaticTask :: [(ModuleName, TaskName)] TonicModule TonicTask -> Task ()
viewStaticTask navstack tm=:{tm_name} tt =
      viewTitle` ("Task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "', which yields " +++ prefixAOrAn (ppTCleanExpr tt.tt_resty))
  ||- (if (length tt.tt_args > 0) (viewInformation "Arguments" [ViewWith (map (\(varnm, ty) -> ppTCleanExpr varnm +++ " is " +++ prefixAOrAn (ppTCleanExpr ty)))] tt.tt_args @! ()) (return ()))
  ||- updateInformation ()
        [imageUpdate id (mkTaskImage (defaultTRT tt) 'DM'.newMap 'DM'.newMap) (const id)]
        {ActionState | state = tt, action = Nothing} >>*
        [ OnValue (doAction (navigate tm tt))
        , OnAction (Action "Back" []) (back tm tt navstack)] @! ()
  where
  back _  _  []           _ = Nothing
  back tm tt [prev:stack] _ = Just (nav` id tm tt prev stack)
  navigate tm tt next _     = nav` (\ns -> [(tm.tm_name, tt.tt_name):navstack]) tm tt next navstack
  nav` mkNavStack tm tt (mn, tn) navstack
    = getModule mn >>*
      [ OnValue onNavVal
      , OnAllExceptions (const (viewStaticTask navstack tm tt))
      ]
    where
    onNavVal (Value tm` _) = fmap (\tt` -> viewStaticTask (mkNavStack navstack) tm` tt` @! ()) (getTask tm` tn)
    onNavVal _             = Nothing
  defaultTRT tt
    = { trt_taskId        = TaskId -1 -1
      , trt_params        = []
      , trt_bpref         = (tm_name, tt.tt_name)
      , trt_bpinstance    = Just tt
      , trt_activeNodeId  = Nothing
      , trt_parentTaskId  = TaskId -2 -2
      , trt_involvedUsers = []
      , trt_output        = Nothing
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

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

:: BlueprintQuery
  = ModuleName String
  | TaskName String
  | UserInvolved String
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

derive class iTask BlueprintQuery

tonicDynamicBrowser :: Task ()
tonicDynamicBrowser = enterQuery >&> withSelection (tonicDynamicBrowser` Nothing) tonicDynamicBrowser`

tonicDynamicBrowser` q =
    (enterChoiceWithShared "Active blueprint instances" [ChooseWith (ChooseFromGrid customView)] (mapRead (filterActiveTasks q o 'DM'.elems) tonicSharedRT) >&>
    withSelection noBlueprintSelection viewInstance) <<@ ArrangeWithSideBar 0 LeftSide 700 True
                                                     <<@ FullScreen

  where
  filterActiveTasks Nothing tasks = tasks
  filterActiveTasks (Just q) tasks
    = [t \\ t <- tasks | doFilter t q ]
    where
    doFilter trt (ModuleName mn)   = mn == "" || indexOf mn (fst trt.trt_bpref) >= 0
    doFilter trt (TaskName tn)     = tn == "" || indexOf tn (snd trt.trt_bpref) >= 0
    doFilter trt (UserInvolved un) = un == "" || indexOf un (toString (toJSON trt.trt_involvedUsers)) >= 0
    doFilter trt (AndQuery l r)    = doFilter trt l && doFilter trt r
    doFilter trt (OrQuery l r)     = doFilter trt l || doFilter trt r
  //customView rt = { DynamicView | moduleName = fst rt.trt_bpref, taskName = snd rt.trt_bpref }
  customView rt = rt
  noBlueprintSelection = viewInformation () [] "Select blueprint instance"

viewInstance :: TonicRT -> Task ()
viewInstance trt=:{trt_bpinstance = Just bp} =
                      dynamicParent trt.trt_taskId >>-
  \mbprnt ->          (viewInformation (blueprintTitle trt bp) [] () ||-
                      viewTaskArguments trt bp ||-
                      (watch (tonicSharedListOfTask |+| tonicSharedRT) >>-
  \(maplot, rtmap) -> updateInformation "Blueprint:" [imageUpdate id (mkTaskImage trt maplot rtmap) (const id)] {ActionState | state = bp, action = Nothing} )) >>*
                      [OnAction (Action "Parent task" [ActionIcon "open"]) (\_ -> fmap viewInstance mbprnt)]
  where
  blueprintTitle    trt bp = snd trt.trt_bpref +++ " yields " +++ prefixAOrAn (ppTCleanExpr bp.tt_resty)
  viewTaskArguments trt bp = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs trt bp) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection              = viewInformation () [] "Select argument..."
  collectArgs       trt bp = zipWith (\(argnm, argty) (_, vi) -> (ppTCleanExpr argnm +++ " is " +++ prefixAOrAn (ppTCleanExpr argty), vi)) bp.tt_args trt.trt_params
viewInstance _ = return ()

tonicViewer :: PublishedTask
tonicViewer = publish "/tonic" (WebApp []) (\_ -> tonicLogin)

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

mkTaskImage :: !TonicRT !(Map (TaskId, [Int]) (IntMap TaskId)) !TonicRTMap !ModelTy *TagSource -> Image ModelTy
mkTaskImage trt maplot rtmap {ActionState | state = tt} tsrc
  #! inh              = { inh_trt = trt, inh_maplot = maplot, inh_rtmap = rtmap }
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (img, _)         = tTaskDef tt.tt_name tt.tt_resty tt.tt_args tt_body` tsrc
  = img

:: ModelTy :== ActionState (ModuleName, TaskName) TonicTask

tExpr2Image :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpr2Image inh (TBind lhs mpat rhs)       tsrc = tBind         inh lhs mpat rhs tsrc
tExpr2Image inh (TReturn texpr)            tsrc = tReturn       inh texpr tsrc
tExpr2Image inh (TTaskApp eid mn tn targs) tsrc = tTaskApp      inh eid mn tn targs tsrc
tExpr2Image inh (TLet pats bdy)            tsrc = tLet          inh pats bdy tsrc
tExpr2Image inh (TCaseOrIf e pats)         tsrc = tCaseOrIf     inh e pats tsrc
tExpr2Image inh (TStep lexpr conts)        tsrc = tStep         inh lexpr conts tsrc
tExpr2Image inh (TParallel eid par)        tsrc = tParallel     inh eid par tsrc
tExpr2Image inh (TAssign usr t)            tsrc = tAssign       inh usr t tsrc
tExpr2Image inh (TShare ts sn args)        tsrc = tShare        inh ts sn args tsrc
tExpr2Image inh (TTransform lhs vn args)   tsrc = tTransformApp inh lhs vn args tsrc
tExpr2Image inh (TVar _ pp)                tsrc = (text ArialRegular10px pp, tsrc)
tExpr2Image inh (TCleanExpr _ pp)          tsrc = (text ArialRegular10px (ppTCleanExpr pp), tsrc)

ppTCleanExpr :: !TCleanExpr -> String
ppTCleanExpr (PPCleanExpr pp)       = sugarPP pp
ppTCleanExpr (AppCleanExpr _ pp []) = sugarPP pp
ppTCleanExpr (AppCleanExpr _ "_Cons" xs)   = "[" +++ ppTCleanExprList xs +++ "]"
ppTCleanExpr (AppCleanExpr _ "_Tuple2" xs) = "(" +++ ppTCleanExprTuple xs +++ ")"
ppTCleanExpr (AppCleanExpr _ "_Tuple3" xs) = "(" +++ ppTCleanExprTuple xs +++ ")"
ppTCleanExpr (AppCleanExpr _ "_Tuple4" xs) = "(" +++ ppTCleanExprTuple xs +++ ")"
ppTCleanExpr (AppCleanExpr (TLeftAssoc  n) pp [l, r]) = ppTCleanExpr` l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` r
ppTCleanExpr (AppCleanExpr (TRightAssoc n) pp [l, r]) = ppTCleanExpr` l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` r
ppTCleanExpr (AppCleanExpr _               pp xs)     = sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map ppTCleanExpr` xs)

ppTCleanExpr` :: !TCleanExpr -> String
ppTCleanExpr` (PPCleanExpr pp)       = sugarPP pp
ppTCleanExpr` (AppCleanExpr _ pp []) = sugarPP pp
ppTCleanExpr` (AppCleanExpr (TLeftAssoc  n) pp [l, r]) = "(" +++ ppTCleanExpr` l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` r +++ ")"
ppTCleanExpr` (AppCleanExpr (TRightAssoc n) pp [l, r]) = "(" +++ ppTCleanExpr` l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` r +++ ")"
ppTCleanExpr` (AppCleanExpr _               pp xs)     = "(" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map ppTCleanExpr` xs) +++ ")"

ppTCleanExprList :: ![TCleanExpr] -> String
ppTCleanExprList []  = ""
ppTCleanExprList [x : PPCleanExpr "_Nil" : _] = ppTCleanExpr x
ppTCleanExprList [x:xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprList xs

ppTCleanExprTuple :: ![TCleanExpr] -> String
ppTCleanExprTuple []  = ""
ppTCleanExprTuple [x] = ppTCleanExpr x
ppTCleanExprTuple [x:xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprTuple xs

sugarPP "_Unit"   = "nothing"
sugarPP "_Nil"    = "[]"
sugarPP "_String" = "String"
sugarPP pp = pp

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

refsForList :: [a] !*TagSource -> *(!*[*TagRef], !*TagSource)
refsForList [] tsrc = ([], tsrc)
refsForList [_:xs] tsrc
  #! (rs, tsrc) = refsForList xs tsrc
  #! (r, tsrc)  = mkTagRef tsrc
  = ([r:rs], tsrc)

// TODO margin around cases
tCaseOrIf :: !MkImageInh !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseOrIf inh texpr pats tsrc
  #! ppexpr = case texpr of
                TCleanExpr _ (PPCleanExpr x) -> x
                _                            -> "TODO RENDER GRAPH"
  #! patStrs  = map (ppTCleanExpr o fst) pats
  #! patExprs = map snd pats
  #! (nextTasks, tsrc) = mapSt (tExpr2Image inh) patExprs tsrc
  #! (refs, tsrc) = refsForList patExprs tsrc
  #! (nextTasks, refs) = prepCases patStrs nextTasks refs
  #! (vertConn, refs)  = mkVertConn refs
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
  = (beside (repeat AtMiddleY) [] [diamond`, tHorizConn, vertConn, nextTasks`, vertConn] Nothing, tsrc)
  where
  y :: !Real !Real !Span -> Span
  y textHeight edgeMargin x = x *. (textHeight / edgeMargin)

tShare :: !MkImageInh !TShare !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tShare inh sh sn args tsrc
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
  #! (shareArr, shareref, tsrc) = tagWithSrc tsrc (above (repeat AtMiddleX) [] [mkShare, arr] Nothing)
  #! (sharetag, shareref) = tagFromRef shareref
  #! emptyImg = empty zero (imageyspan sharetag)
  // TODO Add arrows to/from box if the box is smaller than the share
  = (above (repeat AtMiddleX) [] [shareArr, boxImg, emptyImg] Nothing, tsrc)
  where
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

tLet :: !MkImageInh ![(!Pattern, !TExpr)] !TExpr !*TagSource -> *(!Image ModelTy, *TagSource)
tLet inh pats expr tsrc
  #! (t, tsrc)               = tExpr2Image inh expr tsrc
  #! (letText, txtref, tsrc) = tagWithSrc tsrc (above (repeat (AtLeft)) [] (map (\(var, expr) -> text ArialRegular10px (ppTCleanExpr var +++ " = " +++ case expr of
                                                                                                                                                         TCleanExpr _ (PPCleanExpr x) -> x
                                                                                                                                                         _                            -> "TODO tLet")) pats) Nothing)
  #! (txttag, txtref)        = tagFromRef txtref
  #! letBox  = rect (imagexspan txttag) (px ArialRegular10px.fontysize *. (length pats + 1))
                 <@< { fill   = toSVGColor "white" }
                 <@< { stroke = toSVGColor "black" }
  #! letImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
  = (beside (repeat AtMiddleY) [] [letImg, tHorizConnArr, t] Nothing, tsrc)

tBind :: !MkImageInh !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! linePart   = case mpat of
                    Just pat -> [l`, tHorizConn, tTextWithGreyBackground ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, r`]
                    _        -> [l`, tHorizConnArr, r`]
  = (beside (repeat AtMiddleY) [] linePart Nothing, tsrc)

tParallel :: !MkImageInh ![Int] !TParallel !*TagSource -> *(!Image ModelTy, !*TagSource)
tParallel inh eid (ParSumL l r) tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! l` = margin (px 5.0, px 5.0) l`
  #! r` = margin (px 5.0, px 5.0) r`
  = (beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing, tsrc)
tParallel inh eid (ParSumR l r) tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! l` = margin (px 5.0, px 5.0) l`
  #! r` = margin (px 5.0, px 5.0) r`
  = (beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing, tsrc)
tParallel inh eid (ParSumN ts) tsrc
  #! (ts`, tsrc) = mkParSum inh eid ts tsrc
  #! ts` = map (margin (px 5.0, px 5.0)) ts`
  = ( beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ above (repeat AtMiddleX) [] ts` Nothing, /* TODO lines to last delim,*/ tParSum] Nothing
    , tsrc)
  where
  mkParSum :: !MkImageInh ![Int] !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParSum inh eid (PP pp) tsrc
    = case 'DM'.get (inh.inh_trt.trt_taskId, eid) inh.inh_maplot of
        Just mptids
          -> mapSt (\(mn, tn) -> tTaskApp inh eid "" tn []) [trt.trt_bpref \\ Just trt <- map (\tid -> 'DM'.get tid inh.inh_rtmap) ('DIS'.elems mptids)] tsrc
        _ -> ([text ArialRegular10px pp], tsrc)
  mkParSum _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc
tParallel inh eid (ParProd ts) tsrc
  #! (imgs, tsrc)     = mkParProd inh eid ts tsrc
  #! (refs, tsrc)     = refsForList imgs tsrc
  #! (ts, refs)       = prepCases [] imgs refs
  #! (vertConn, refs) = mkVertConn refs
  = ( beside (repeat AtMiddleY) [] [tParProd, tHorizConn, vertConn, above (repeat AtMiddleX) [] ts Nothing, tHorizConn, vertConn, tHorizConnArr, tParProd] Nothing
    , tsrc)
  where
  mkParProd :: !MkImageInh ![Int] !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParProd inh eid (PP pp) tsrc
    = case 'DM'.get (inh.inh_trt.trt_taskId, eid) inh.inh_maplot of
        Just mptids
          -> mapSt (\(mn, tn) -> tTaskApp inh eid "" tn []) [trt.trt_bpref \\ Just trt <- map (\tid -> 'DM'.get tid inh.inh_rtmap) ('DIS'.elems mptids)] tsrc
        _ -> ([text ArialRegular10px pp], tsrc)
  mkParProd _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc

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
  | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
  | otherwise                                       = "a " +++ str

// TODO Start / stop symbols here
tTaskDef :: !String !TCleanExpr ![(!TCleanExpr, !TCleanExpr)] !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskDef taskName resultTy _ tdbody tsrc
  #! (taskBodyImgs, bdyref, tsrc) = tagWithSrc tsrc (margin (px 5.0) (beside (repeat AtMiddleY) [] [tStartSymb, tHorizConnArr, tdbody, tHorizConnArr, tStopSymb] Nothing))
  #! (bdytag, bdyref) = tagFromRef bdyref
  #! bgRect           = rect (imagexspan bdytag) (imageyspan bdytag)
                          <@< { fill        = toSVGColor "white" }
                          <@< { stroke      = toSVGColor "black" }
                          <@< { strokewidth = px 1.0 }
                          <@< { xradius     = px 5.0 }
                          <@< { yradius     = px 5.0 }
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskBodyImgs] Nothing, tsrc)
  where
  mkArgAndTy :: !(!String, !TCleanExpr) -> String
  mkArgAndTy (arg, ty) = arg +++ " is " +++ prefixAOrAn (ppTCleanExpr ty)

tTransformApp :: !MkImageInh !TExpr !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tTransformApp inh texpr tffun args tsrc
  #! (tfNameImg,  nmref, tsrc)   = tagWithSrc tsrc (margin (px 5.0) (text ArialItalic10px tffun))
  #! (tfArgsImgs, argsref, tsrc) = tagWithSrc tsrc (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! (nmtag, nmref)   = tagFromRef nmref
  #! (argstag, argsref) = tagFromRef argsref
  #! (expr, tsrc) = tExpr2Image inh texpr tsrc
  #! maxXSpan   = maxSpan [imagexspan nmtag, imagexspan argstag]
  #! bgRect     = rect maxXSpan (imageyspan nmtag + imageyspan argstag)
                    <@< { fill        = toSVGColor "white" }
                    <@< { stroke      = toSVGColor "black" }
                    <@< { strokewidth = px 1.0 }
  #! tfContents = above (repeat AtLeft) [] (case args of
                                              [] -> [tfNameImg]
                                              _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  #! tfApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  = (beside (repeat AtMiddleY) [] [tfApp, tHorizConnArr, expr] Nothing, tsrc)

tTaskApp :: !MkImageInh !ExprId !ModuleName !VarName ![TExpr] !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskApp inh eid modName taskName taskArgs tsrc
  #! (taskArgs`, tsrc)           = mapSt (tExpr2Image inh) taskArgs tsrc
  #! (taskNameImg,  tnref, tsrc) = tagWithSrc tsrc (margin (px 5.0) (text ArialBold10px taskName))
  #! (taskArgsImgs, taref, tsrc) = tagWithSrc tsrc (margin (px 5.0) (above (repeat AtLeft) [] taskArgs` Nothing))
  #! (tntag, tnref) = tagFromRef tnref
  #! (tatag, taref) = tagFromRef taref
  #! maxXSpan       = maxSpan [imagexspan tntag, imagexspan tatag]
  #! bgRect         = rect maxXSpan (imageyspan tntag + imageyspan tatag)
                        <@< { fill        = if (Just eid == inh.inh_trt.trt_activeNodeId) (toSVGColor "lightgreen") (toSVGColor "white") }
                        <@< { stroke      = toSVGColor "black" }
                        <@< { strokewidth = px 1.0 }
                        <@< { xradius     = px 5.0 }
                        <@< { yradius     = px 5.0 }
  #! taskText       = above (repeat AtMiddleX) [] (case taskArgs` of
                                                     [] -> [taskNameImg]
                                                     _  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs]) Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
      <@< { ondblclick = \st -> { ActionState | st & action = Just (modName, taskName) } }
    , tsrc)

tReturn :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tReturn inh retval tsrc
  #! (retval`, tsrc)      = tExpr2Image inh retval tsrc
  #! (retval`, ref, tsrc) = tagWithSrc tsrc retval`
  #! (tag, ref)           = tagFromRef ref
  #! oval                 = ellipse (imagexspan tag + px 20.0) (imageyspan tag + px 10.0)
                              <@< { fill        = toSVGColor "white" }
                              <@< { stroke      = toSVGColor "black" }
                              <@< { strokewidth = px 1.0 }
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [oval, retval`] Nothing, tsrc)

tAssign :: !MkImageInh !TUser !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tAssign inh user assignedTask tsrc
  #! (at, tsrc)  = tExpr2Image inh assignedTask tsrc
  #! (atr, tsrc) = mkTagRef tsrc
  #! (atag, atr) = tagFromRef atr
  #! (utr, tsrc) = mkTagRef tsrc
  #! (utag, utr) = tagFromRef utr
  #! maxXSpan    = maxSpan [imagexspan utag, imagexspan atag]
  #! (taskNameImg, utr) = tagWithRef utr (margin (px 5.0) (text ArialBold10px (ppUser user)))
  #! bgRect      = rect maxXSpan (imageyspan utag + imageyspan atag)
                     <@< { fill        = toSVGColor "white" }
                     <@< { stroke      = toSVGColor "black" }
                     <@< { strokewidth = px 1.0 }
                     <@< { xradius     = px 5.0 }
                     <@< { yradius     = px 5.0 }
                     <@< { dash        = [5, 5] }
  #! (at, atr)   = tagWithRef atr (margin (px 5.0) (beside (repeat AtMiddleY) [] [tStartSymb, tHorizConnArr, at, tHorizConnArr, tStopSymb] Nothing))
  #! content     = above (repeat AtMiddleX) [] [beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing, xline Nothing maxXSpan, at] Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

ppUser :: !TUser -> String
ppUser TUAnyUser                       = "Any user"
ppUser (TUUserWithIdent ident)         = "User " +++ ident
ppUser (TUUserWithRole role)           = "Any user with role " +++ role
ppUser TUSystemUser                    = "Any system user"
ppUser TUAnonymousUser                 = "Any anonymous user"
ppUser (TUAuthenticatedUser usr roles) = "User " +++ usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles
ppUser (TUVariableUser usr)            = "User referred to by variable " +++ usr

tStep :: !MkImageInh !TExpr ![PPOr TStepCont] !*TagSource -> *(!Image ModelTy, !*TagSource)
tStep inh lhsExpr conts tsrc
  #! (lhs, tsrc)      = tExpr2Image inh lhsExpr tsrc
  #! (refs, tsrc)     = refsForList conts tsrc
  #! (conts`, tsrc)   = mapSt (tStepCont inh) conts tsrc
  #! (conts`, refs)   = prepCases [] conts` refs
  #! (vertConn, refs) = mkVertConn refs
  #! contsImg = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [lhs, tHorizConnArr, tStepStar, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tStepStar] Nothing
    , tsrc)

extractTags :: !*[*TagRef] -> *(![ImageTag], !*[*TagRef])
extractTags [] = ([], [])
extractTags [x:xs]
  #! (ts, rs) = extractTags xs
  #! (t, r) = tagFromRef x
  = ([t:ts], [r:rs])

tagImgs :: ![Image ModelTy] !*[*TagRef] -> *(![Image ModelTy], !*[*TagRef])
tagImgs [] [] = ([], [])
tagImgs [i:is] [r:rs]
  #! (is, rs) = tagImgs is rs
  #! (i, r) = tagWithRef r i
  = ([i:is], [r:rs])

prepCases :: ![String] ![Image ModelTy] !*[*TagRef] -> *(![Image ModelTy], !*[*TagRef])
prepCases patStrs pats refs
  #! (pats, refs) = tagImgs pats refs
  #! (tags, refs) = extractTags refs
  #! pats     = zipWith (\_ pat -> pat) tags pats // To sync lengths
  #! maxXSpan = maxSpan (map imagexspan tags)
  = (zipWith3 (prepCase maxXSpan) pats (patStrs ++ repeat "") tags, refs)
  where
  prepCase :: !Span !(Image ModelTy) !String !ImageTag -> Image ModelTy
  prepCase maxXSpan pat patStr tag
    = case patStr of
        ""
          #! linePart  = (maxXSpan - imagexspan tag) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), leftLine, pat, rightLine] Nothing
        patStr
          #! textWidth = textxspan ArialRegular10px patStr + px 10.0
          #! linePart  = (maxXSpan - imagexspan tag - textWidth) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          #! textBox   = tTextWithGreyBackground ArialRegular10px patStr
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), textBox, leftLine, pat, rightLine] Nothing

tTextWithGreyBackground font txt
  #! textWidth = textxspan font txt + px 10.0
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (font.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text font txt] Nothing

mkVertConn :: !*[*TagRef] -> *(!Image ModelTy, !*[*TagRef])
mkVertConn refs
  | length refs < 2 = (empty (px 0.0) (px 0.0), refs)
  | otherwise
      #! (ts, refs) = extractTags refs
      #! firstTag  = hd ts
      #! lastTag   = last ts
      #! allYSpans = foldr (\x acc -> imageyspan x + acc) (px 0.0) ts
      = (above (repeat AtMiddleX) []
           [ yline Nothing (imageyspan firstTag /. 2.0) <@< { stroke = toSVGColor "white" }
           , yline Nothing (allYSpans - (imageyspan firstTag /. 2.0) - (imageyspan lastTag /. 2.0)) <@< { stroke = toSVGColor "black" }
           , yline Nothing (imageyspan lastTag /. 2.0) <@< { stroke = toSVGColor "white" } ]
           Nothing
        , refs)

tStepCont :: !MkImageInh !(PPOr TStepCont) !*TagSource -> *(!Image ModelTy, !*TagSource)
tStepCont _   (PP pp) tsrc = (text ArialRegular10px pp, tsrc)
tStepCont inh (T t)   tsrc = tStepCont` inh.inh_trt t tsrc
  where
  tStepCont` :: !TonicRT !TStepCont !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepCont` trt (StepOnValue      sfilter) tsrc = tStepFilter trt Nothing sfilter tsrc
  tStepCont` trt (StepOnAction act sfilter) tsrc = tStepFilter trt (Just act) sfilter tsrc
  tStepCont` trt (StepOnException mpat te)  tsrc
    #! (img, tsrc) = tExpr2Image inh te tsrc
    // TODO mpat
    = (beside (repeat AtMiddleY) [] [tException, tHorizConnArr, /* TODO edge */ img] Nothing, tsrc)
  tStepFilter :: !TonicRT !(Maybe String) !TStepFilter !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter trt mact sfilter tsrc
    #! (ref, tsrc) = mkTagRef tsrc
    = tStepFilter` trt mact sfilter ref tsrc
  tStepFilter` :: !TonicRT !(Maybe String) !TStepFilter !*TagRef !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter` trt mact (Always te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (HasValue mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /* TODO edge */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfStable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConnArr, /* TODO edge */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfUnstable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConnArr, /* TODO edge */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfCond pp mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge and conditional */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfValue pat fn vars mpat te) ref tsrc
    #! (t, tsrc)   = tExpr2Image inh te tsrc
    #! (ifv, tsrc) = tIfValue fn vars tsrc
    #! img         = beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConn, text ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, ifv, tHorizConnArr, /* TODO mpat */ t] Nothing
    = (img, tsrc)
  tStepFilter` trt mact (CustomFilter pp) ref tsrc = (text ArialRegular10px pp, tsrc)
  addAction :: !(Maybe String) !(Image ModelTy) !*TagRef -> Image ModelTy
  addAction (Just action) img ref
    #! l = above (repeat AtMiddleX) [] [ beside (repeat AtMiddleY) [] [littleman, text ArialBold10px action] Nothing
                                       , img] Nothing
    #! (imgtag, ref) = tagFromRef ref
    #! (l, ref) = tagWithRef ref l
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan imgtag + px 5.0) (imageyspan imgtag + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
                                                 , l] Nothing
  addAction _ img _ = img

alwaysFilter :: Image ModelTy
alwaysFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable, tNoVal] Nothing

hasValueFilter :: Image ModelTy
hasValueFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable] Nothing


littleman :: Image a
littleman = (overlay [] [(px -2.0, px 8.0), (px 3.0, px 1.0)] [ circle (px 20.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}
                                                              , circle (px 10.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}] Nothing) <@< {mask = rect (px 16.0) (px 16.0) <@< {fill = toSVGColor "white"}}

tIfValue :: !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tIfValue tffun args tsrc
  #! (nameref, tsrc)    = mkTagRef tsrc
  #! (nametag, nameref) = tagFromRef nameref
  #! (argsref, tsrc)    = mkTagRef tsrc
  #! (argstag, argsref) = tagFromRef argsref
  #! maxXSpan   = maxSpan [imagexspan nametag, imagexspan argstag]
  #! bgRect     = rect maxXSpan (imageyspan nametag + imageyspan argstag)
                    <@< { fill        = toSVGColor "white" }
                    <@< { stroke      = toSVGColor "black" }
                    <@< { strokewidth = px 1.0 }
  #! (tfNameImg, nameref) = tagWithRef nameref (margin (px 5.0) (text ArialItalic10px tffun))
  #! (tfArgsImgs, argsref) = tagWithRef argsref (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! tfContents = above (repeat AtLeft) [] (case args of
                                              [] -> [tfNameImg]
                                              _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing, tsrc)

tException :: Image ModelTy
tException
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "!!"] Nothing

tStable :: Image ModelTy
tStable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 44, g = 160, b = 44} }

tUnstable :: Image ModelTy
tUnstable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 255, g = 127, b = 14} }

tNoVal :: Image ModelTy
tNoVal = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 214, g = 39, b = 40} }

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
