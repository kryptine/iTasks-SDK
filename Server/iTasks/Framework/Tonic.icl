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
                               _        -> err ("Failed to deserialize JSON: " +++ json)
                Error msg -> err (toString msg)
  where
  err msg = throw ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg)

tonicViewInformation :: String a -> Task () | iTask a
tonicViewInformation d v = viewInformation d [] v @! ()

tonicWrapTaskBody :: ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
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
firstParent _     instanceNo [] = Nothing
firstParent rtMap instanceNo [parentTaskNo:parentTaskNos]
  = maybe (firstParent rtMap instanceNo parentTaskNos) Just
      ('DM'.get (TaskId instanceNo parentTaskNo) rtMap)

tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{callTrace} taskTree iworld
    # traceStr = foldr (\x xs -> toString x +++ " " +++ xs) "" callTrace
    # nids = foldr (\x xs -> toString x +++ " " +++ xs) "" nid
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

getTasks :: TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: TonicModule String -> Maybe TonicTask
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
         )) <<@ ArrangeSplit Horizontal True
         )) <<@ ArrangeSplit Horizontal True
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
        [imageView mkTaskImage]
        tt @! ()

dynamicParent :: TaskId -> Task (Maybe TonicRT)
dynamicParent childId
  =       get tonicSharedRT >>-
  \rtm -> return (maybe Nothing
                    (\rt -> 'DM'.get rt.trt_parentTaskId rtm)
                    ('DM'.get childId rtm))

viewDynamic :: Task ()
viewDynamic = enterChoiceWithShared "Active blueprint instances" [] (mapRead 'DM'.elems tonicSharedRT) >>= viewInstance

viewInstance :: TonicRT -> Task ()
viewInstance {trt_bpinstance = Nothing}      = return ()
viewInstance trt=:{trt_bpinstance = Just bp} =
             dynamicParent trt.trt_taskId >>-
  \mbprnt -> (viewInformation (blueprintTitle trt bp) [] () ||-
             viewTaskArguments trt bp ||- viewSharedInformation "Blueprint:"
               [] // TODO
               tonicSharedRT
               @! ()) >>*
            [OnAction (Action "Parent task" [ActionIcon "open"]) (\_ -> fmap viewInstance mbprnt)]
  where
  blueprintTitle    trt bp = snd trt.trt_bpref +++ " yields " +++ prefixAOrAn bp.tt_resty
  viewTaskArguments trt bp = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs trt bp) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection              = viewInformation () [] "Select argument..."
  collectArgs       trt bp = zipWith (\(argnm, argty) (_, vi) -> (argnm +++ " is " +++ prefixAOrAn argty, vi)) bp.tt_args trt.trt_params

tonicViewer :: String -> PublishedTask
tonicViewer appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

mkTaskImage :: TonicTask -> Image TonicTask
mkTaskImage tt = tTaskDef tt.tt_name tt.tt_resty tt.tt_args (tExpr2Image tt.tt_body)

tExpr2Image :: TExpr -> Image TonicTask
tExpr2Image (TBind lhs mpat rhs)     = tBind lhs mpat rhs
tExpr2Image (TReturn texpr)          = tReturn texpr
tExpr2Image (TTaskApp eid tn targs)  = tTaskApp tn (map tExpr2Image targs) // TODO Use eid when we add interaction
tExpr2Image (TLet pats bdy)          = abort "tExpr2Image TLet: not implemented"
tExpr2Image (TCaseOrIf e pats)       = abort "tExpr2Image TCaseOrIf: not implemented"
tExpr2Image (TStep lexpr conts)      = tStep lexpr conts
tExpr2Image (TParallel par)          = tParallel par
tExpr2Image (TAssign usr t)          = tAssign usr t
tExpr2Image (TShare ts sn args)      = abort "tExpr2Image TShare: not implemented"
tExpr2Image (TTransform lhs vn args) = abort "tExpr2Image TTransform: not implemented"
tExpr2Image (TVar pp)                = text ArialRegular10px pp
tExpr2Image (TCleanExpr pp)          = text ArialRegular10px pp


tBind :: TExpr (Maybe Pattern) TExpr -> Image TonicTask
tBind l mpat r
  = beside (repeat AtMiddleY) [] [tExpr2Image l, tExpr2Image r] Nothing // TODO Add edge + label

tParallel :: TParallel -> Image TonicTask
tParallel (ParSumL l r)
  = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ tExpr2Image l, tExpr2Image r, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel (ParSumR l r)
  = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ tExpr2Image l, tExpr2Image r, /* TODO lines to last delim,*/ tParSum] Nothing
tParallel (ParSumN ts)
  # ts` = above (repeat AtMiddleX) [] (mkParSum ts) Nothing
  = beside (repeat AtMiddleY) [] [tParSum, /* TODO lines to tasks,*/ ts`, /* TODO lines to last delim,*/ tParSum] Nothing
  where
  mkParSum (PP pp) = [text ArialRegular10px pp]
  mkParSum (T xs)  = map tExpr2Image xs
tParallel (ParProd ts)
  # ts` = above (repeat AtMiddleX) [] (mkParProd ts) Nothing
  = beside (repeat AtMiddleY) [] [tParProd, /* TODO lines to tasks,*/ ts`, /* TODO lines to last delim,*/ tParProd] Nothing
  where
  mkParProd (PP pp) = [text ArialRegular10px pp]
  mkParProd (T xs)  = map tExpr2Image xs

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
tDiamond = rotate (degree 45.0) (rect 16 16)
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
tParSum = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, plus] Nothing
  where
  plus = overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing
  line f = f Nothing 10 <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tParProd :: Image TonicTask
tParProd = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, plus] Nothing
  where
  plus = rotate (degree 45.0) (overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing)
  line f = f Nothing 10 <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tStartSymb :: Image TonicTask
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image TonicTask
tStopSymb  = rect 16 16

prefixAOrAn :: String -> String
prefixAOrAn str
  | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
  | otherwise                                       = "a " +++ str

tTaskDef :: String String [(String, String)] (Image TonicTask) -> Image TonicTask
tTaskDef taskName resultTy taskArgsAndTys tdbody
  # bgRect       = rect maxXSpan (imageyspan [imageTag "tTaskDef_taskNameImg"] + imageyspan [imageTag "tTaskDef_taskArgsImgs"] + imageyspan [imageTag "tTaskDef_taskBodyImgs"])
                     <@< { fill        = toSVGColor "white" }
                     <@< { stroke      = toSVGColor "black" }
                     <@< { strokewidth = px 1.0 }
                     <@< { xradius     = px 5.0 }
                     <@< { yradius     = px 5.0 }
  # taskNameImg  = tag [imageTag "tTaskDef_taskNameImg"]  (margin 5 (text ArialBold10px (taskName +++ " yields " +++ prefixAOrAn resultTy))) // TODO a/an
  # taskArgsImgs = tag [imageTag "tTaskDef_taskArgsImgs"] (margin 5 (above (repeat AtLeft) [] (map (text ArialRegular10px o mkArgAndTy) taskArgsAndTys) Nothing))
  # taskBodyImgs = tag [imageTag "tTaskDef_taskBodyImgs"] (margin 5 tdbody)
  # taskContents = above (repeat AtLeft) [] (case taskArgsAndTys of
                                               [] -> [taskNameImg, xline Nothing maxXSpan, taskBodyImgs]
                                               _  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs, xline Nothing maxXSpan, taskBodyImgs]) Nothing
  # tTaskDef     = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskContents] Nothing
  = tTaskDef
  where
  maxXSpan = maxSpan [imagexspan [imageTag "tTaskDef_taskNameImg"], imagexspan [imageTag "tTaskDef_taskArgsImgs"], imagexspan [imageTag "tTaskDef_taskBodyImgs"]]
  mkArgAndTy (arg, ty) = arg +++ " is " +++ prefixAOrAn ty

tTransformApp :: String [String] -> Image TonicTask
tTransformApp tffun args
  # bgRect     = rect maxXSpan (imageyspan [imageTag "tTransformApp_tfNameImg"] + imageyspan [imageTag "tTransformApp_tfArgsImgs"])
                   <@< { fill        = toSVGColor "white" }
                   <@< { stroke      = toSVGColor "black" }
                   <@< { strokewidth = px 1.0 }
  # tfNameImg  = tag [imageTag "tTransformApp_tfNameImg"]  (margin 5 (text ArialItalic10px tffun))
  # tfArgsImgs = tag [imageTag "tTransformApp_tfArgsImgs"] (margin 5 (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  # tfContents = above (repeat AtLeft) [] (case args of
                                             [] -> [tfNameImg]
                                             _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  # tfApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  = tfApp
  where
  maxXSpan = maxSpan [imagexspan [imageTag "tTransformApp_tfNameImg"], imagexspan [imageTag "tTransformApp_tfArgsImgs"]]

tTaskApp :: String [Image TonicTask] -> Image TonicTask
tTaskApp taskName taskArgs
  # bgRect       = rect maxXSpan (imageyspan [imageTag "tTaskApp_taTaskNameImg"] + imageyspan [imageTag "tTaskApp_taTaskArgsImgs"])
                     <@< { fill        = toSVGColor "white" }
                     <@< { stroke      = toSVGColor "black" }
                     <@< { strokewidth = px 1.0 }
                     <@< { xradius     = px 5.0 }
                     <@< { yradius     = px 5.0 }
  # taskNameImg  = tag [imageTag "tTaskApp_taTaskNameImg"]  (margin 5 (text ArialBold10px taskName))
  # taskArgsImgs = tag [imageTag "tTaskApp_taTaskArgsImgs"] (margin 5 (above (repeat AtLeft) [] taskArgs Nothing))
  # taskText     = above (repeat AtMiddleX) [] (case taskArgs of
                                                  [] -> [taskNameImg]
                                                  _  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs]) Nothing
  # taskApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
  = taskApp
  where
  maxXSpan = maxSpan [imagexspan [imageTag "tTaskApp_taTaskNameImg"], imagexspan [imageTag "tTaskApp_taTaskArgsImgs"]]

tReturn :: TExpr -> Image TonicTask
tReturn retval
  # retval = tag [imageTag "tReturn_retval"] (tExpr2Image retval) // TODO Do we need to maintain a counter here instead? Might be needed to truly uniquely identify the image
  # oval   = ellipse (imagexspan [imageTag "tReturn_retval"]) (px 20.0)
               <@< { fill        = toSVGColor "white" }
               <@< { stroke      = toSVGColor "black" }
               <@< { strokewidth = px 1.0 }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [oval, retval] Nothing

tAssign :: TUser TExpr -> Image TonicTask
tAssign user assignedTask
  # bgRect       = rect maxXSpan (px ArialBold10px.fontysize + imageyspan [imageTag "tAssign_assignedTask"])
                     <@< { fill        = toSVGColor "white" }
                     <@< { stroke      = toSVGColor "black" }
                     <@< { strokewidth = px 1.0 }
                     <@< { xradius     = px 5.0 }
                     <@< { yradius     = px 5.0 }
                     <@< { dash        = [5, 5] }
  # taskNameImg  = tag [imageTag "tAssign_user"] (margin 5 (text ArialBold10px (toString user)))
  # taskText     = above (repeat AtMiddleX) [] [ taskNameImg, xline Nothing maxXSpan
                                               , tag [imageTag "tAssign_assignedTask"] (tExpr2Image assignedTask)] Nothing
  # taskApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
  = taskApp
  where
  maxXSpan = maxSpan [imagexspan [imageTag "tAssign_user"], imagexspan [imageTag "tAssign_assignedTask"]]

tStep :: TExpr [PPOr TStepCont] -> Image TonicTask
tStep lhsExpr conts
  # lhs  = tExpr2Image lhsExpr
  # rhss = above (repeat AtMiddleX) [] (map tStepCont conts) Nothing
  = beside (repeat AtMiddleY) [] [lhs, /* TODO line to first star, */tStepStar, /* TODO lines to steps,*/ rhss, /* TODO lines to last star,*/ tStepStar] Nothing

tStepCont :: (PPOr TStepCont) -> Image TonicTask
tStepCont (PP pp) = text ArialRegular10px pp
tStepCont (T t)   = tStepCont` t
  where
  tStepCont` (StepOnValue          sfilter) = text ArialRegular10px "tStepCont` TODO"
  tStepCont` (StepOnAction    act  sfilter) = text ArialRegular10px "tStepCont` TODO"
  tStepCont` (StepOnException mpat te)      = text ArialRegular10px "tStepCont` TODO"
  tStepFilter (Always                      te) = text ArialRegular10px "tStepFilter TODO"
  tStepFilter (HasValue               mpat te) = text ArialRegular10px "tStepFilter TODO"
  tStepFilter (IfStable               mpat te) = text ArialRegular10px "tStepFilter TODO"
  tStepFilter (IfUnstable             mpat te) = text ArialRegular10px "tStepFilter TODO"
  tStepFilter (IfCond     pp          mpat te) = text ArialRegular10px "tStepFilter TODO"
  tStepFilter (IfValue    pat fn vars mpat te) = text ArialRegular10px "tStepFilter TODO"

instance toString TUser where
  toString _ = "toString TUser"
  toString TUAnyUser                       = "Any user"
  toString (TUUserWithIdent ident)         = "User " +++ ident
  toString (TUUserWithRole role)           = "Any user with role " +++ role
  toString TUSystemUser                    = "Any system user"
  toString TUAnonymousUser                 = "Any anonymous user"
  toString (TUAuthenticatedUser usr roles) = "User " +++ usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles

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
