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
import Graphics.Scalable
from Control.Monad.State import :: State, :: StateT, :: Identity, instance Monad StateT, instance Applicative StateT, instance Functor StateT
from Control.Monad.Identity import instance Monad Identity, instance Applicative Identity, instance Functor Identity
import qualified Control.Applicative as CA
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS

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

tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{callTrace} taskTree iworld
    # traceStr = foldr (\x xs -> toString x +++ " " +++ xs) "" callTrace
    # nids     = foldr (\x xs -> toString x +++ " " +++ xs) "" nid
    = eval event evalOpts taskTree (maybeSt iworld
                                      (addTrace callTrace)
                                      (taskIdFromTaskTree taskTree))
    where
    addTrace callTrace (TaskId instanceNo taskNo) iworld
      # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
      = okSt iworld updRTMap mrtMap
      where
      updRTMap rtMap iworld
        # rtMap = maybe rtMap
                    (\rt -> 'DM'.put rt.trt_taskId {rt & trt_activeNodeId = Just nid} rtMap)
                    (firstParent rtMap instanceNo [taskNo:callTrace])
        = snd ('DSDS'.write rtMap tonicSharedRT iworld)
  eval` event evalOpts taskTree iworld = eval event evalOpts taskTree iworld

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
           (\tt -> viewStaticTask tm tt @! ())
           (getTask tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
            <<@ FullScreen
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: TonicModule TonicTask -> Task ()
viewStaticTask {tm_name} tt =
      viewInformation ("Arguments for task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'") [] tt.tt_args
  ||- viewInformation
        ("Static visual task representation of task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "'")
        [imageView (mkTaskImage Nothing)]
        tt @! ()

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
viewDynamic = enterChoiceWithShared "Active blueprint instances" [ChooseWith (ChooseFromGrid customView)] (mapRead 'DM'.elems tonicSharedRT) >>= viewInstance
  where
  customView rt = rt // { DynamicView | moduleName = fst rt.trt_bpref, taskName = snd rt.trt_bpref }

viewInstance :: TonicRT -> Task ()
viewInstance trt=:{trt_bpinstance = Just bp} =
             dynamicParent trt.trt_taskId >>-
  \mbprnt -> (viewInformation (blueprintTitle trt bp) [] () ||-
             viewTaskArguments trt bp ||- viewInformation "Blueprint:"
               [imageView (mkTaskImage trt.trt_activeNodeId)]
               bp
               @! ()) >>*
            [OnAction (Action "Parent task" [ActionIcon "open"]) (\_ -> fmap viewInstance mbprnt)]
  where
  blueprintTitle    trt bp = snd trt.trt_bpref +++ " yields " +++ prefixAOrAn bp.tt_resty
  viewTaskArguments trt bp = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs trt bp) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection              = viewInformation () [] "Select argument..."
  collectArgs       trt bp = zipWith (\(argnm, argty) (_, vi) -> (argnm +++ " is " +++ prefixAOrAn argty, vi)) bp.tt_args trt.trt_params
viewInstance _ = return ()

tonicViewer :: String -> PublishedTask
tonicViewer appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

// TODO Start / stop symbols
mkTaskImage :: !(Maybe [Int]) !TonicTask -> Image TonicTask
mkTaskImage activeNodeId tt
             = 'CMS'.evalState (tExpr2Image activeNodeId tt.tt_body `b`
  \tt_body` -> tTaskDef tt.tt_name tt.tt_resty tt.tt_args tt_body`) 0

:: TImg :== State Int (Image TonicTask)

(`b`) ma a2mb :== bind ma a2mb

tExpr2Image :: !(Maybe [Int]) !TExpr -> TImg
tExpr2Image activeNodeId (TBind lhs mpat rhs)     = tBind         activeNodeId lhs mpat rhs
tExpr2Image activeNodeId (TReturn texpr)          = tReturn       activeNodeId texpr
tExpr2Image activeNodeId (TTaskApp eid tn targs)  = tTaskApp      activeNodeId eid tn targs
tExpr2Image activeNodeId (TLet pats bdy)          = tLet          activeNodeId pats bdy
tExpr2Image activeNodeId (TCaseOrIf e pats)       = tCaseOrIf     activeNodeId e pats
tExpr2Image activeNodeId (TStep lexpr conts)      = tStep         activeNodeId lexpr conts
tExpr2Image activeNodeId (TParallel par)          = tParallel     activeNodeId par
tExpr2Image activeNodeId (TAssign usr t)          = tAssign       activeNodeId usr t
tExpr2Image activeNodeId (TShare ts sn args)      = tShare        activeNodeId ts sn args
tExpr2Image activeNodeId (TTransform lhs vn args) = tTransformApp activeNodeId lhs vn args
tExpr2Image activeNodeId (TVar pp)                = 'CA'.pure (text ArialRegular10px pp)
tExpr2Image activeNodeId (TCleanExpr pp)          = 'CA'.pure (text ArialRegular10px pp)

tArrowTip :: Image TonicTask
tArrowTip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]

tLineMarker :: Maybe (Markers TonicTask)
tLineMarker = Just {defaultMarkers & markerEnd = Just tArrowTip}

tHorizConn :: Image TonicTask
tHorizConn = xline Nothing (px 8.0)

tHorizConnArr :: Image TonicTask
tHorizConnArr = xline tLineMarker (px 16.0)

tVertDownConnArr :: Image TonicTask
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip)}) (px 16.0)

tVertUpConnArr :: Image TonicTask
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just tArrowTip}) (px 16.0)

tVertUpDownConnArr :: Image TonicTask
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip), markerEnd = Just tArrowTip}) (px 16.0)

// TODO margin around cases
tCaseOrIf :: !(Maybe [Int]) !PPExpr ![(!Pattern, !TExpr)] -> TImg
tCaseOrIf activeNodeId ppexpr pats
  #! patStrs  = map fst pats
  #! patExprs = map snd pats
  =         'CM'.mapM (\_ -> dispenseUniq) patExprs `b`
  \uniqs -> 'CM'.mapM (tExpr2Image activeNodeId) patExprs `b`
            ('CA'.pure o mkCaseOrIf patStrs uniqs)
  where
  mkCaseOrIf :: ![String] ![Int] ![Image TonicTask] -> Image TonicTask
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

mkVertConn :: ![Int] -> Image TonicTask
mkVertConn uniqs
  | length uniqs < 2 = empty (px 0.0) (px 0.0)
  | otherwise
      #! firstUniq  = hd uniqs
      #! lastUniq   = last uniqs
      #! restUniqs  = init (tl uniqs)
      #! nextYSpans = foldr (\x acc -> imageyspan (imageTag x) + acc) (px 0.0) restUniqs
      = above (repeat AtMiddleX) []
          [ yline Nothing (imageyspan (imageTag firstUniq) /. 2.0) <@< { stroke = toSVGColor "white" }
          , yline Nothing (nextYSpans - (imageyspan (imageTag firstUniq) /. 2.0) - (imageyspan (imageTag lastUniq) /. 2.0))
          , yline Nothing (imageyspan (imageTag lastUniq) /. 2.0) <@< { stroke = toSVGColor "white" } ]
          Nothing

tShare :: !(Maybe [Int]) !TShare !VarName ![VarName] -> TImg
tShare activeNodeId sh sn args = dispenseUniq `b` (mkShareImg sh sn args)
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
  mkShare :: Image TonicTask
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

tLet :: !(Maybe [Int]) ![(!Pattern, !PPExpr)] !TExpr -> TImg
tLet activeNodeId pats expr
  =          dispenseUniq `b`
  \textNo -> tExpr2Image activeNodeId expr `b`
             ('CA'.pure o mkLet pats textNo)
  where
  mkLet :: ![(!String, !String)] !Int !(Image TonicTask) -> Image TonicTask
  mkLet pats textNo t
    #! letText = tag (imageTag textNo) (above (repeat (AtMiddleX)) [] (map (\(var, expr) -> text ArialRegular10px (var +++ " = " +++ expr)) pats) Nothing)
    #! letBox  = rect (imagexspan (imageTag textNo)) (px ArialRegular10px.fontysize *. (length pats + 1))
                   <@< { fill   = toSVGColor "white" }
                   <@< { stroke = toSVGColor "black" }
    #! letImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
    = beside (repeat AtMiddleY) [] [letImg, tHorizConnArr, t] Nothing

tBind :: !(Maybe [Int]) !TExpr !(Maybe Pattern) !TExpr -> TImg
tBind activeNodeId l mpat r
  =      tExpr2Image activeNodeId l `b`
  \l` -> tExpr2Image activeNodeId r `b`
  \r` -> 'CA'.pure (beside (repeat AtMiddleY) [] [l`, tHorizConnArr, r`] Nothing) // TODO Add label

tParallel :: !(Maybe [Int]) !TParallel -> TImg
tParallel activeNodeId (ParSumL l r)
  =      tExpr2Image activeNodeId l `b`
  \l` -> tExpr2Image activeNodeId r `b`
  \r` -> 'CA'.pure (mkParSumL l` r`)
  where
  mkParSumL :: !(Image TonicTask) !(Image TonicTask) -> Image TonicTask
  mkParSumL l` r`
    #! l` = margin (px 5.0, px 5.0) l`
    #! r` = margin (px 5.0, px 5.0) r`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel activeNodeId (ParSumR l r)
  =      tExpr2Image activeNodeId l `b`
  \l` -> tExpr2Image activeNodeId r `b`
  \r` -> 'CA'.pure (mkParSumR l` r`)
  where
  mkParSumR :: !(Image TonicTask) !(Image TonicTask) -> Image TonicTask
  mkParSumR l` r`
    #! l` = margin (px 5.0, px 5.0) l`
    #! r` = margin (px 5.0, px 5.0) r`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ l`, r`, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel activeNodeId (ParSumN ts) = mkParSum ts `b` ('CA'.pure o mkParSumN)
  where
  mkParSumN :: ![Image TonicTask] -> Image TonicTask
  mkParSumN ts`
    #! ts` = map (margin (px 5.0, px 5.0)) ts`
    = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ above (repeat AtMiddleX) [] ts` Nothing, /* TODO lines to last delim,*/ tParSum] Nothing
  mkParSum :: !(PPOr [TExpr]) -> State Int [Image TonicTask]
  mkParSum (PP pp) = 'CA'.pure [text ArialRegular10px pp]
  mkParSum (T xs)  = 'CM'.mapM (tExpr2Image activeNodeId) xs
tParallel activeNodeId (ParProd ts)
  =         mkParProd ts `b`
  \imgs  -> 'CM'.mapM (\_ -> dispenseUniq) imgs `b`
  \uniqs -> 'CA'.pure (mkParProdCases uniqs imgs)
  where
  mkParProdCases :: ![Int] ![Image TonicTask] -> Image TonicTask
  mkParProdCases uniqs ts`
    #! ts`      = prepCases [] uniqs ts`
    #! vertConn = mkVertConn uniqs
    = beside (repeat AtMiddleY) [] [tParProd, tHorizConn, vertConn, above (repeat AtMiddleX) [] ts` Nothing, tHorizConn, vertConn, tHorizConnArr, tParProd] Nothing
  mkParProd :: !(PPOr [TExpr]) -> State Int [Image TonicTask]
  mkParProd (PP pp) = 'CA'.pure [text ArialRegular10px pp]
  mkParProd (T xs)  = 'CM'.mapM (tExpr2Image activeNodeId) xs

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

tDiamond :: Image TonicTask
tDiamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0))
             <@< { fill   = toSVGColor "black" }
             <@< { stroke = toSVGColor "none" }

tStepStar :: Image TonicTask
tStepStar = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, star] Nothing
  where
  star = polygon Nothing
           [ (px 5.0, px 0.0)
           , (px 2.0, px 10.0)
           , (px 9.5, px 4.0)
           , (px 0.0, px 4.0)
           , (px 8.0, px 10.0) ] <@< { fill   = toSVGColor "white" }
                                 <@< { stroke = toSVGColor "none" }

tParSum :: Image TonicTask
tParSum = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, tPlus] Nothing

tParProd :: Image TonicTask
tParProd = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, rotate (deg 45.0) tPlus] Nothing

tPlus :: Image TonicTask
tPlus
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing
  where
  line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tStartSymb :: Image TonicTask
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image TonicTask
tStopSymb  = rect (px 16.0) (px 16.0)

prefixAOrAn :: !String -> String
prefixAOrAn str
  | str == "Unit"                                   = "nothing"
  | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
  | otherwise                                       = "a " +++ str

// TODO Start / stop symbols here
tTaskDef :: !String !String ![(!String, !String)] !(Image TonicTask) -> TImg
tTaskDef taskName resultTy taskArgsAndTys tdbody
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> dispenseUniq `b`
  \bodyNo -> 'CA'.pure (tTaskDef` taskName resultTy taskArgsAndTys tdbody nameNo argsNo bodyNo)
  where
  tTaskDef` :: !String !String ![(!String, !String)] !(Image TonicTask) !Int !Int !Int -> Image TonicTask
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

tTransformApp :: !(Maybe [Int]) !TExpr !VarName ![VarName] -> TImg
tTransformApp activeNodeId texpr tffun args
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> tExpr2Image activeNodeId texpr `b`
  \expr   -> 'CA'.pure (tTransformApp` activeNodeId tffun args nameNo argsNo expr)
  where
  tTransformApp` :: !(Maybe [Int]) !VarName ![VarName] !Int !Int !(Image TonicTask) -> Image TonicTask
  tTransformApp` activeNodeId tffun args nameNo argsNo expr
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

tTaskApp :: !(Maybe [Int]) !ExprId !VarName ![TExpr] -> TImg
tTaskApp activeNodeId eid taskName taskArgs
  =             'CM'.mapM (tExpr2Image activeNodeId) taskArgs `b`
  \taskArgs` -> dispenseUniq `b`
  \tnNo      -> dispenseUniq `b`
  \taNo      -> 'CA'.pure (tTaskApp` activeNodeId eid taskName taskArgs` tnNo taNo)
  where
  tTaskApp` :: !(Maybe [Int]) !ExprId !VarName ![Image TonicTask] !Int !Int -> Image TonicTask
  tTaskApp` activeNodeId eid taskName taskArgs` tnNo taNo
    #! maxXSpan     = maxSpan [imagexspan (imageTag tnNo), imagexspan (imageTag taNo)]
    #! bgRect       = rect maxXSpan (imageyspan (imageTag tnNo) + imageyspan (imageTag taNo))
                        <@< { fill        = if (Just eid == activeNodeId) (toSVGColor "lightgreen") (toSVGColor "white") }
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

dispenseUniq :: State Int Int
dispenseUniq
  =     'CMS'.gets id `b`
  \s -> 'CMS'.put (s + 1) `b`
  \_ -> 'CA'.pure s

tReturn :: !(Maybe [Int]) !TExpr -> TImg
tReturn activeNodeId retval
  =           tExpr2Image activeNodeId retval `b`
  \retval` -> dispenseUniq `b`
  \tagNo   -> 'CA'.pure (tReturn` retval` tagNo)
  where
  tReturn` :: !(Image TonicTask) !Int -> Image TonicTask
  tReturn` retval` tagNo
    #! retval` = tag (imageTag tagNo) retval`
    #! oval    = ellipse (imagexspan (imageTag tagNo) + px 20.0) (imageyspan (imageTag tagNo) + px 10.0)
                   <@< { fill        = toSVGColor "white" }
                   <@< { stroke      = toSVGColor "black" }
                   <@< { strokewidth = px 1.0 }
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [oval, retval`] Nothing

tAssign :: !(Maybe [Int]) !TUser !TExpr -> TImg
tAssign activeNodeId user assignedTask
  =          tExpr2Image activeNodeId assignedTask `b`
  \at     -> dispenseUniq `b`
  \userNo -> dispenseUniq `b`
  \atNo   -> 'CA'.pure (tAssign` at userNo atNo)
  where
  tAssign` :: !(Image TonicTask) !Int !Int -> Image TonicTask
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
    #! content = above (repeat AtMiddleX) [] [taskNameImg, xline Nothing maxXSpan, at] Nothing
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing

ppUser :: !TUser -> String
ppUser TUAnyUser                       = "Any user"
ppUser (TUUserWithIdent ident)         = "User " +++ ident
ppUser (TUUserWithRole role)           = "Any user with role " +++ role
ppUser TUSystemUser                    = "Any system user"
ppUser TUAnonymousUser                 = "Any anonymous user"
ppUser (TUAuthenticatedUser usr roles) = "User " +++ usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles

tStep :: !(Maybe [Int]) !TExpr ![PPOr TStepCont] -> TImg
tStep activeNodeId lhsExpr conts
  =          'CM'.mapM (\_ -> dispenseUniq) conts `b`
  \uniqs  -> tExpr2Image activeNodeId lhsExpr `b`
  \lhs    -> 'CM'.mapM (tStepCont activeNodeId) conts `b`
  \conts` -> 'CA'.pure (tStep` lhs conts` uniqs)
  where
  tStep` :: !(Image TonicTask) ![Image TonicTask] ![Int] -> Image TonicTask
  tStep` lhs conts` uniqs
    #! conts`   = prepCases [] uniqs conts`
    #! vertConn = mkVertConn uniqs
    #! contsImg = above (repeat AtMiddleX) [] conts` Nothing
    = beside (repeat AtMiddleY) [] [lhs, tHorizConnArr, tStepStar, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tStepStar] Nothing

prepCases :: ![String] ![Int] ![Image TonicTask] -> [Image TonicTask]
prepCases patStrs uniqs pats
  #! pats     = zipWith (\uniq pat -> pat) uniqs pats
  #! maxXSpan = maxSpan (map (imagexspan o imageTag) uniqs)
  = zipWith3 (prepCase maxXSpan) uniqs pats (patStrs ++ repeat "")
  where
  prepCase :: !Span !Int !(Image TonicTask) !String -> Image TonicTask
  prepCase maxXSpan uniq pat patStr
    #! linePart  = (maxXSpan - imagexspan (imageTag uniq)) /. 2.0
    #! leftLine  = xline tLineMarker (px 16.0 + linePart)
    #! rightLine = xline Nothing (px 8.0 + linePart)
    = beside (repeat AtMiddleY) [] [text ArialRegular10px patStr, leftLine, pat, rightLine] Nothing

tStepCont :: !(Maybe [Int]) !(PPOr TStepCont) -> TImg
tStepCont _            (PP pp) = 'CA'.pure (text ArialRegular10px pp)
tStepCont activeNodeId (T t)   = tStepCont` activeNodeId t
  where
  tStepCont` :: !(Maybe [Int]) !TStepCont -> TImg
  tStepCont` activeNodeId (StepOnValue      sfilter) = tStepFilter activeNodeId Nothing sfilter
  tStepCont` activeNodeId (StepOnAction act sfilter) = tStepFilter activeNodeId (Just act) sfilter
  tStepCont` activeNodeId (StepOnException mpat te)  = tExpr2Image activeNodeId te `b` ('CA'.pure o mkOnException)
    where
    // TODO mpat
    mkOnException :: !(Image TonicTask) -> Image TonicTask
    mkOnException t = beside (repeat AtMiddleY) [] [tException, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter :: !(Maybe [Int]) !(Maybe String) !TStepFilter -> TImg
  tStepFilter activeNodeId mact sfilter
    =        dispenseUniq `b`
    \uniq -> tStepFilter` activeNodeId uniq mact sfilter
  tStepFilter` :: !(Maybe [Int]) !Int !(Maybe String) !TStepFilter -> TImg
  tStepFilter` activeNodeId uniq mact (Always te) = tExpr2Image activeNodeId te `b` ('CA'.pure o mkAlways uniq mact)
    where
    mkAlways :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
    mkAlways uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact alwaysFilter, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` activeNodeId uniq mact (HasValue mpat te) = tExpr2Image activeNodeId te `b` ('CA'.pure o mkHasValue uniq mact)
    where
    // TODO mpat
    mkHasValue :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
    mkHasValue uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact hasValueFilter, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` activeNodeId uniq mact (IfStable mpat te) = tExpr2Image activeNodeId te `b` ('CA'.pure o mkIfStable uniq mact)
    where
    // TODO mpat
    mkIfStable :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
    mkIfStable uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact tStable, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` activeNodeId uniq mact (IfUnstable mpat te) = tExpr2Image activeNodeId te `b` ('CA'.pure o mkIfUnstable uniq mact)
    where
    // TODO mpat
    mkIfUnstable :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
    mkIfUnstable uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact tUnstable, tHorizConnArr, /* TODO edge */ t] Nothing
  tStepFilter` activeNodeId uniq mact (IfCond pp mpat te) = tExpr2Image activeNodeId te `b` ('CA'.pure o mkIfCond uniq mact)
    where
    // TODO mpat pp
    mkIfCond :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
    mkIfCond uniq mact t = beside (repeat AtMiddleY) [] [addAction uniq mact alwaysFilter, tHorizConnArr, /* TODO edge and conditional */ t] Nothing
  tStepFilter` activeNodeId uniq mact (IfValue pat fn vars mpat te) = tExpr2Image activeNodeId te `b` \t -> tIfValue fn vars `b` ('CA'.pure o mkIfValue pat uniq mact t)
    where
    mkIfValue :: !String !Int !(Maybe String) !(Image TonicTask) !(Image TonicTask) -> Image TonicTask
    mkIfValue pat uniq mact t c = beside (repeat AtMiddleY) [] [addAction uniq mact hasValueFilter, tHorizConn, text ArialRegular10px pat, tHorizConnArr, c, tHorizConnArr, /* TODO mpat */ t] Nothing

alwaysFilter :: Image TonicTask
alwaysFilter = above (repeat AtMiddleX) [] [tStable, tUnstable, tNoVal] Nothing

hasValueFilter :: Image TonicTask
hasValueFilter = above (repeat AtMiddleX) [] [tStable, tUnstable] Nothing

addAction :: !Int !(Maybe String) !(Image TonicTask) -> Image TonicTask
addAction uniq mact img
  = case mact of
      Just action
        #! imgtag = imageTag uniq
        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan imgtag + px 5.0) (imageyspan imgtag + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
                                                     , tag imgtag (above (repeat AtMiddleX) [] [text ArialBold10px action, img] Nothing) // TODO Draw little figure
                                                     ] Nothing
      _ = img

tIfValue :: !VarName ![VarName] -> TImg
tIfValue tffun args
  =          dispenseUniq `b`
  \nameNo -> dispenseUniq `b`
  \argsNo -> 'CA'.pure (tIfValue` nameNo argsNo)
  where
  tIfValue` :: !Int !Int -> Image TonicTask
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

tException :: Image TonicTask
tException
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "!!"] Nothing

tStable :: Image TonicTask
tStable
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, rect (px 8.0) (px 8.0) <@< { fill = toSVGColor "black" }] Nothing

tUnstable :: Image TonicTask
tUnstable
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "W"] Nothing

tNoVal :: Image TonicTask
tNoVal
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "X"] Nothing

tLineArrow :: Image TonicTask
tLineArrow = polygon Nothing [ (px 0.0, px 0.0)
                             , (px 8.0, px 4.0)
                             , (px 0.0, px 8.0) ]

uniDirLineMarkers :: Maybe (Markers TonicTask)
uniDirLineMarkers = Just { markerStart = Nothing
                         , markerMid   = Nothing
                         , markerEnd   = Just tLineArrow }

biDirLineMarkers :: Maybe (Markers TonicTask)
biDirLineMarkers = Just { markerStart = Just tLineArrow
                        , markerMid   = Nothing
                        , markerEnd   = Just tLineArrow }
