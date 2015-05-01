implementation module iTasks._Framework.Tonic.Images

import StdArray
import StdBool
import StdTuple
from StdFunc import o
import Data.Func
import Data.List
import Data.Maybe
import qualified Data.Map as DM
from Data.Map import instance Functor (Map a)
from Data.Set import :: Set
import qualified Data.Set as DS
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Graphics.Scalable
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Tonic.Types
import iTasks._Framework.Tonic.Util
import iTasks.API.Core.Types
import iTasks.API.Extensions.SVG.SVGlet

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintIdent

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

ArialItalic10px :== { fontfamily  = "Arial"
                    , fontysize   = 10.0
                    , fontstretch = "normal"
                    , fontstyle   = "italic"
                    , fontvariant = "normal"
                    , fontweight  = "normal"
                    }

:: MkImageInh =
  { inh_trt          :: !BlueprintRef
  , inh_maplot       :: !ListsOfTasks
  , inh_task_apps    :: ![TaskAppRenderer]
  , inh_compact      :: !Bool
  , inh_prev         :: !Map ExprId TaskId
  , inh_inaccessible :: !Bool
  , inh_in_maybe     :: !Bool
  , inh_selected     :: !Set (ModuleName, TaskName, ExprId)
  , inh_outputs      :: !Map TaskId TStability
  , inh_selDetail    :: !Maybe ClickMeta
  }

mkTaskImage :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef !ListsOfTasks !(Map TaskId TStability) !(Set (ModuleName, TaskName, ExprId)) !(Maybe ClickMeta) !Bool !ModelTy *TagSource -> Image ModelTy
mkTaskImage rs prev trt maplot outputs selected selDetail compact {ActionState | state = tis} tsrc
  #! tt               = tis.tis_task
  #! inh              = { MkImageInh
                        | inh_trt          = trt
                        , inh_maplot       = maplot
                        , inh_task_apps    = rs
                        , inh_compact      = compact
                        , inh_prev         = prev
                        , inh_inaccessible = False
                        , inh_in_maybe     = False
                        , inh_selected     = selected
                        , inh_outputs      = outputs
                        , inh_selDetail    = selDetail
                        }
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (img, _)         = tTaskDef tt.tt_name tt.tt_resty tt.tt_args tt_body` tsrc
  = img

tExpr2Image :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpr2Image inh (TBind lhs mpat rhs)       tsrc = tBind         inh lhs mpat rhs tsrc
tExpr2Image inh (TReturn eid e)            tsrc = tReturn       inh eid e tsrc
tExpr2Image inh (TTaskApp eid mn tn targs) tsrc = tTaskApp      inh eid mn tn targs tsrc
tExpr2Image inh (TLet pats bdy)            tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TCaseOrIf e pats)         tsrc = tCaseOrIf     inh e pats tsrc
tExpr2Image inh (TStep lexpr conts)        tsrc = tStep         inh lexpr conts tsrc
tExpr2Image inh (TParallel eid par)        tsrc = tParallel     inh eid par tsrc
tExpr2Image inh (TAssign usr d t)          tsrc = tAssign       inh usr d t tsrc
tExpr2Image inh (TFunctor lhs vn args)     tsrc = tFunctorApp   inh lhs vn args tsrc
tExpr2Image inh (TVar eid pp)              tsrc = tVar          inh eid pp tsrc
tExpr2Image inh (TCleanExpr eid pp)        tsrc = tCleanExpr    inh eid pp tsrc
tExpr2Image inh (TExpand tn e)             tsrc = tExpand       inh tn e tsrc

tArrowTip :: Image ModelTy
tArrowTip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]

tLineMarker :: Maybe (Markers ModelTy)
tLineMarker = Just {defaultMarkers & markerEnd = Just tArrowTip}

tSmallHorizConn :: Image ModelTy
tSmallHorizConn = xline Nothing (px 4.0)

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

tReturn :: !MkImageInh !ExprId !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tReturn inh=:{inh_in_maybe = True} eid expr tsrc = tExpr2Image inh expr tsrc
tReturn inh                        eid expr tsrc = tTaskApp inh eid "" "return" [expr] tsrc

tExpand :: !MkImageInh !TaskName !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpand inh tn e [(exprTag, uExprTag) : (taskNameTag, uTaskNameTag) : tsrc]
  #! (childExpr, tsrc) = tExpr2Image inh e tsrc
  #! childExpr         = tag uExprTag (margin (px 5.0) childExpr)
  #! maxXSpan          = maxSpan [imagexspan taskNameTag, imagexspan exprTag]
  #! taskNameImg       = tag uTaskNameTag (margin (px 5.0) (text ArialBold10px tn))
  #! bgRect            = rect maxXSpan (imageyspan taskNameTag + imageyspan exprTag)
                           <@< { fill        = toSVGColor "white" }
                           <@< { stroke      = toSVGColor "black" }
                           <@< { strokewidth = px 1.0 }
                           <@< { xradius     = px 5.0 }
                           <@< { yradius     = px 5.0 }
                           <@< { dash        = [5, 5] }
  #! lineImg           = xline Nothing maxXSpan <@< { dash = [5, 5] }
  #! content           = above (repeat AtMiddleX) [] [taskNameImg, lineImg, childExpr] Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tVar :: !MkImageInh !ExprId !String !*TagSource -> *(!Image ModelTy, !*TagSource)
tVar inh eid pp tsrc
  = case inh.inh_trt.bpr_instance of
      Just bpinst
        = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
            Just mptids
              = case 'DIS'.elems mptids of
                  [(moduleName, taskName) : _]
                    = tTaskApp inh eid moduleName taskName [] tsrc
                  _ = mkDef tsrc
            _ = mkDef tsrc
      _ = mkDef tsrc
  where
  mkDef :: !*TagSource -> *(!Image ModelTy, !*TagSource)
  mkDef tsrc
    #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
    = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing, tsrc)

tCleanExpr :: !MkImageInh !ExprId !TCleanExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tCleanExpr inh eid pp tsrc = (text ArialRegular10px (ppTCleanExpr pp), tsrc)

containsActiveNodes :: !MkImageInh !TExpr -> Bool
containsActiveNodes inh (TBind lhs mpat rhs) = containsActiveNodes inh lhs || containsActiveNodes inh rhs
containsActiveNodes inh (TReturn _ e)        = containsActiveNodes inh e
containsActiveNodes inh (TTaskApp eid _ _ _) = 'DM'.member eid inh.inh_prev || maybe False (\bpi -> isJust (activeNodeTaskId eid bpi.bpi_activeNodes)) inh.inh_trt.bpr_instance
containsActiveNodes inh (TLet pats bdy)      = containsActiveNodes inh bdy
containsActiveNodes inh (TCaseOrIf e pats)   = foldr (\(_, e) acc -> acc || containsActiveNodes inh e) False pats
containsActiveNodes inh (TStep lexpr conts)  = containsActiveNodes inh lexpr || foldr f False conts
  where
  f :: !(PPOr TStepCont) Bool -> Bool
  f (PP _) acc = acc
  f (T e)  acc = acc || containsActiveNodesStepCont inh e
containsActiveNodes inh (TParallel _ par)  = containsActiveNodesTParallel inh par
containsActiveNodes inh (TAssign _ _ t)    = containsActiveNodes inh t
containsActiveNodes inh (TFunctor lhs _ _) = containsActiveNodes inh lhs
containsActiveNodes inh (TExpand _ e)      = containsActiveNodes inh e
containsActiveNodes inh (TVar _ _)         = False
containsActiveNodes inh (TCleanExpr _ _)   = False

containsActiveNodesTParallel :: !MkImageInh !TParallel -> Bool
containsActiveNodesTParallel inh (ParSumL l r)   = containsActiveNodes inh l || containsActiveNodes inh r
containsActiveNodesTParallel inh (ParSumR l r)   = containsActiveNodes inh l || containsActiveNodes inh r
containsActiveNodesTParallel inh (ParSumN conts) = containsActiveNodesTParallel` inh conts
containsActiveNodesTParallel inh (ParProd conts) = containsActiveNodesTParallel` inh conts
containsActiveNodesTParallel` :: !MkImageInh !(PPOr [TExpr]) -> Bool
containsActiveNodesTParallel` _   (PP _) = False
containsActiveNodesTParallel` inh (T es) = foldr (\x acc -> acc || containsActiveNodes inh x) False es

containsActiveNodesStepCont :: !MkImageInh !TStepCont -> Bool
containsActiveNodesStepCont inh (StepOnValue    sf)   = containsActiveNodesStepFilter inh sf
containsActiveNodesStepCont inh (StepOnAction _ sf)   = containsActiveNodesStepFilter inh sf
containsActiveNodesStepCont inh (StepOnException _ e) = containsActiveNodes inh e

containsActiveNodesStepFilter :: !MkImageInh !TStepFilter -> Bool
containsActiveNodesStepFilter inh (Always          e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh (HasValue      _ e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh (IfStable      _ e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh (IfUnstable    _ e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh (IfCond      _ _ e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh (IfValue _ _ _ _ e) = containsActiveNodes inh e
containsActiveNodesStepFilter inh _                   = False

//// TODO margin around cases
tCaseOrIf :: !MkImageInh !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseOrIf inh texpr pats tsrc
  #! ppexpr         = case texpr of
                        TCleanExpr _ (PPCleanExpr x) -> x
                        _                            -> "TODO RENDER GRAPH"
  #! patStrs        = map (ppTCleanExpr o fst) pats
  #! patExprs       = map snd pats
  #! branchActivity = map (containsActiveNodes inh) patExprs
  #! someActivity   = foldr (\x acc -> x || acc) False branchActivity
  #! patExprs`      = zip2 patExprs branchActivity
  #! (nextTasks, tsrc)       = mapSt (\(patExpr, possiblyActive) tsrc -> tExpr2Image {inh & inh_inaccessible = someActivity && not possiblyActive} patExpr tsrc) patExprs` tsrc
  #! (nextTasks, refs, tsrc) = prepCases patStrs nextTasks tsrc
  #! vertConn     = mkVertConn refs
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
  #! diamond`     = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ diamond
                                                               , text ArialRegular10px ppexpr] Nothing
  = (beside (repeat AtMiddleY) [] [diamond`, tHorizConn, vertConn, nextTasks`, vertConn] Nothing, tsrc)
  where
  y :: !Real !Real !Span -> Span
  y textHeight edgeMargin x = x *. (textHeight / edgeMargin)

//tShare :: !MkImageInh !TShare !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
//tShare inh sh sn args [(sharetag, uShareTag) : tsrc]
  //#! boxTxt  = case sh of
                 //Get          -> "    "
                 //(Set ppexpr) -> ppexpr
                 //(Upd ppexpr) -> ppexpr
  //#! boxRect = rect (textxspan ArialRegular10px boxTxt + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                 //<@< { fill   = toSVGColor "white" }
                 //<@< { stroke = toSVGColor "black" }
  //#! boxImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [boxRect, text ArialRegular10px boxTxt] Nothing
  //#! arr     = case sh of
                 //Get        -> tVertDownConnArr
                 //Set ppexpr -> tVertUpConnArr
                 //Upd ppexpr -> tVertUpDownConnArr
  //#! shareArr = tag uShareTag (above (repeat AtMiddleX) [] [mkShare, arr] Nothing)
  //#! emptyImg = empty zero (imageyspan sharetag)
  //// TODO Add arrows to/from box if the box is smaller than the share
  //= (above (repeat AtMiddleX) [] [shareArr, boxImg, emptyImg] Nothing, tsrc)
  //where
  //mkShare :: Image ModelTy
  //mkShare
    //#! box1Rect = rect (textxspan ArialRegular10px sn + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                    //<@< { fill   = toSVGColor "white" }
                    //<@< { stroke = toSVGColor "black" }
    //#! box1Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box1Rect, text ArialRegular10px sn] Nothing
    //#! box2Text = above (repeat AtMiddleX) [] (map (text ArialRegular10px) args) Nothing
    //#! numArgs  = length args
    //#! box2Rect = rect (maxSpan (map (textxspan ArialRegular10px) args) + px 5.0) (px (ArialRegular10px.fontysize * toReal numArgs + 10.0))
                    //<@< { fill   = toSVGColor "white" }
                    //<@< { stroke = toSVGColor "black" }
    //#! box2Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] (if (numArgs > 0) [box2Rect, box2Text] []) Nothing
    //= above (repeat AtMiddleX) [] [box1Img, box2Img] Nothing

tLet :: !MkImageInh ![(!Pattern, !TExpr)] !TExpr !*TagSource -> *(!Image ModelTy, *TagSource)
tLet inh pats expr [(txttag, uTxtTag) : tsrc]
  = case expr of
      TLet pats` bdy
        = tLet inh (pats ++ pats`) bdy tsrc
      _
        #! (t, tsrc) = tExpr2Image inh expr tsrc
        #! binds     = foldr (\(var, expr) acc -> [text ArialRegular10px (ppTCleanExpr var) : text ArialRegular10px " = " : text ArialRegular10px (ppExpr expr) : acc]) [] pats
        #! letText   = tag uTxtTag (grid (Columns 3) (RowMajor, LeftToRight, TopToBottom) [] [] binds Nothing)
        #! letWidth  = imagexspan txttag + px 10.0
        #! letHeight = px ArialRegular10px.fontysize *. (length pats + 1)
        #! letBox    = rect letWidth letHeight
                         <@< { fill   = toSVGColor "white" }
                         <@< { stroke = toSVGColor "black" }
        #! letImg    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
        #! linePart  = xline Nothing ((letWidth - px 8.0) /. 2.0)
        #! connBox   = beside (repeat AtMiddleY) [] [linePart, rect (px 8.0) (px 8.0), linePart] Nothing
        #! letImg    = above (repeat AtMiddleX) [] [letImg, yline Nothing (px 8.0), connBox, empty zero (letHeight + px 8.0)] Nothing
        = (beside (repeat AtMiddleY) [] [letImg, tHorizConnArr, t] Nothing, tsrc)
  where
  ppExpr :: !TExpr -> String
  ppExpr (TCleanExpr _ clexp) = ppTCleanExpr clexp
  ppExpr _                    = "TODO tLet"

tBind :: !MkImageInh !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! linePart   = case mpat of
                    // TODO Make outgoing edge label clickable to display output value
                    Just pat -> [l`, tHorizConn, tTextWithGreyBackground ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, r`]
                    _        -> [l`, tHorizConnArr, r`]
  = (beside (repeat AtMiddleY) [] linePart Nothing, tsrc)

//// TODO Highlight nodes here?
tParallel :: !MkImageInh !ExprId !TParallel !*TagSource -> *(!Image ModelTy, !*TagSource)
tParallel inh eid (ParSumL l r) tsrc // TODO This is actually not correct yet... first image shouldn't have lines
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! l` = margin (px 5.0, px 0.0) l`
  #! r` = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [vertConn,  parImg, vertConn, tHorizConn ] Nothing, tsrc)
tParallel inh eid (ParSumR l r) tsrc // TODO This is actually not correct yet... second image shouldn't have lines
  #! (l`, tsrc)           = tExpr2Image inh l tsrc
  #! (r`, tsrc)           = tExpr2Image inh r tsrc
  #! l`                   = margin (px 5.0, px 0.0) l`
  #! r`                   = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [vertConn,  parImg, vertConn, tHorizConnArr] Nothing, tsrc)
tParallel inh eid (ParSumN ts) tsrc
  #! (ts`, tsrc) = mkParSum inh eid ts tsrc
  #! ts` = map (margin (px 5.0, px 0.0)) ts`
  #! (ts`, refs, tsrc) = prepCases [] ts` tsrc
  #! vertConn          = mkVertConn refs
  #! contsImg          = above (repeat AtMiddleX) [] ts` Nothing
  = ( beside (repeat AtMiddleY) [] [vertConn, contsImg, vertConn, tHorizConnArr] Nothing
    , tsrc)
  where
  mkParSum :: !MkImageInh !ExprId !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParSum inh eid (PP pp) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\(moduleName, taskName) -> tTaskApp inh eid moduleName taskName []) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0)  (px (ArialRegular10px.fontysize + 10.0))
      = ([overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing], tsrc)
  mkParSum _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc
tParallel inh eid (ParProd ts) tsrc
  #! (imgs, tsrc)     = mkParProd inh eid ts tsrc
  #! (ts, refs, tsrc) = prepCases [] imgs tsrc
  #! vertConn         = mkVertConn refs
  = ( beside (repeat AtMiddleY) [] [tHorizConn, vertConn, above (repeat AtMiddleX) [] ts Nothing, vertConn, tHorizConnArr] Nothing
    , tsrc)
  where
  mkParProd :: !MkImageInh !ExprId !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParProd inh eid (PP pp) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\(moduleName, taskName) -> tTaskApp inh eid moduleName taskName []) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0)  (px (ArialRegular10px.fontysize + 10.0))
      = ([overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing], tsrc)
  mkParProd _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc

tDiamond :: Image ModelTy
tDiamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0))
             <@< { fill   = toSVGColor "black" }
             <@< { stroke = toSVGColor "none" }

tStepStar :: Image ModelTy
tStepStar = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, star] Nothing
  where
  star :: Image ModelTy
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
  line :: !((Maybe a) Span -> Image ModelTy) -> Image ModelTy
  line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tStartSymb :: Image ModelTy
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image ModelTy
tStopSymb = rect (px 16.0) (px 16.0)

tTaskDef :: !String !TCleanExpr [(TCleanExpr, TCleanExpr)] !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskDef taskName resultTy _ tdbody [(bdytag, uBodyTag) : tsrc]
  #! taskBodyImgs = tag uBodyTag (margin (px 5.0) tdbody)
  #! bgRect       = tRoundedRect (imagexspan bdytag) (imageyspan bdytag)
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskBodyImgs] Nothing, tsrc)
  where
  mkArgAndTy :: !(!String, !TCleanExpr) -> String
  mkArgAndTy (arg, ty) = arg +++ " :: " +++ ppTCleanExpr ty

tFunctorApp :: !MkImageInh !TExpr !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tFunctorApp inh texpr tffun args [(nmtag, uNmTag) : (argstag, uArgsTag) : tsrc]
  #! tfNameImg    = tag uNmTag (margin (px 5.0) (text ArialItalic10px tffun))
  #! tfArgsImgs   = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! (expr, tsrc) = tExpr2Image inh texpr tsrc
  #! maxXSpan     = maxSpan [imagexspan nmtag, imagexspan argstag]
  #! bgRect       = rect maxXSpan (imageyspan nmtag + imageyspan argstag)
                      <@< { fill        = toSVGColor "white" }
                      <@< { stroke      = toSVGColor "black" }
                      <@< { strokewidth = px 1.0 }
  #! tfContents   = above (repeat AtLeft) [] (case args of
                                                [] -> [tfNameImg]
                                                _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  #! tfApp        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  = (beside (repeat AtMiddleY) [] [tfApp, tHorizConnArr, expr] Nothing, tsrc)

activeNodeTaskId :: !ExprId !(Map ListId (IntMap (TaskId, ExprId))) -> Maybe TaskId
activeNodeTaskId eid activeNodes
  = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems activeNodes) | eid == nid] of
      [tid : _] -> Just tid
      _         -> Nothing

tTaskApp :: !MkImageInh !ExprId !ModuleName !VarName ![TExpr] !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskApp inh eid modName taskName taskArgs tsrc
  #! (taskArgs`, tsrc)  = mapSt (tExpr2Image inh) taskArgs tsrc
  #! mActiveTid         = case inh.inh_trt.bpr_instance of
                            Just bpinst -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _           -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = 'DM'.get eid inh.inh_prev
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! wasActive          = isJust mPrevActiveTid
  #! stability          = maybe (maybe TNoVal (const TStable) mPrevActiveTid) (\tid -> fromMaybe TNoVal ('DM'.get tid inh.inh_outputs)) mActiveTid
  #! taskIdStr          = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> " (" +++ toString x +++ ")"
                            (_, Just x) -> " (" +++ toString x +++ ")"
                            _           -> ""
  #! taskName           = taskName +++ taskIdStr
  #! (renderOpts, tsrc) = mapSt (\ta -> ta inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName modName taskName taskArgs`) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultTaskApp inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName modName taskName taskArgs taskArgs` tsrc
  #! clickMeta          = mkClickMeta (fmap (\x -> x.bpi_taskId) inh.inh_trt.bpr_instance) mbNavTo
  #! taskApp            = taskApp <@< { onclick = navigateOrSelect clickMeta, local = False }
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}
                                 , click_origin_mbnodeId} -> bpident_moduleName == inh.inh_trt.bpr_moduleName && bpident_taskName == inh.inh_trt.bpr_taskName && click_origin_mbnodeId == Just eid
                            _                             -> False
  #! valAnchor          = circle (px 12.0) <@< { onclick = openDetails clickMeta, local = False }
                                           <@< { fill = case stability of
                                                          TNoVal    -> WhiteColor
                                                          TStable   -> PrevActiveColor
                                                          TUnstable -> CurrActiveColor
                                               }
                                           <@< { stroke = if valNodeIsSelected (toSVGColor "navy") (toSVGColor "black") }
                                           <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! isSelected         = 'DS'.member (inh.inh_trt.bpr_moduleName, inh.inh_trt.bpr_taskName, eid) inh.inh_selected
  #! inclArr            = beside (repeat AtMiddleY) [] (if isSelected [taskApp, valAnchor] [taskApp]) Nothing
  #! resImg             = inclArr
  = (resImg, tsrc)
  where
  mkClickMeta :: !(Maybe TaskId) !(Maybe TaskId) -> ClickMeta
  mkClickMeta mborig mbtarget =
    { click_origin_mbbpident = Just { bpident_moduleName = inh.inh_trt.bpr_moduleName
                                    , bpident_taskName   = inh.inh_trt.bpr_taskName
                                    , bpident_taskId     = mborig
                                    }
    , click_origin_mbnodeId  = Just eid
    , click_target_bpident   = { bpident_moduleName = modName
                               , bpident_taskName   = taskName
                               , bpident_taskId     = mbtarget
                               }
    }

  navigateOrSelect :: !ClickMeta !Int !ModelTy -> ModelTy
  navigateOrSelect meta 1 st = { ActionState | st & action = Just (TSelectNode, meta) }
  navigateOrSelect meta 2 st = { ActionState | st & action = Just (TNavAction, meta) }
  navigateOrSelect _    _ st = st

  openDetails :: !ClickMeta !Int !ModelTy -> ModelTy
  openDetails meta 1 st = { ActionState | st & action = Just (TDetailAction, meta) }
  openDetails _    _ st = st

tRoundedRect :: !Span !Span -> Image a
tRoundedRect width height
  = rect width height
      <@< { fill        = toSVGColor "white" }
      <@< { stroke      = toSVGColor "black" }
      <@< { strokewidth = px 1.0 }
      <@< { xradius     = px 5.0 }
      <@< { yradius     = px 5.0 }

tDefaultTaskApp :: !Bool !Bool !Bool !Bool !(Set (ModuleName, TaskName, ExprId)) !ExprId !ModuleName !TaskName !ModuleName !TaskName ![TExpr] ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultTaskApp isCompact isActive wasActive isInAccessible selectedNodes nodeId parentModName parentTaskName modName taskName argsExprs taskArgs tsrc
  #! isEditor = elem taskName [ "viewInformation"
                              , "updateInformation"
                              , "enterInformation"
                              , "updateSharedInformation"
                              , "viewSharedInformation"
                              , "updateInformationWithShared"
                              , "editChoice"
                              , "editChoiceAs"
                              , "enterChoice"
                              , "enterChoiceAs"
                              , "updateChoice"
                              , "updateChoiceAs"
                              , "editChoiceWithShared"
                              , "editChoiceWithSharedAs"
                              , "enterChoiceWithShared"
                              , "enterChoiceWithSharedAs"
                              , "updateChoiceWithShared"
                              , "updateChoiceWithSharedAs"
                              , "editSharedChoice"
                              , "editSharedChoiceAs"
                              , "editSharedChoiceWithShared"
                              , "editSharedChoiceWithSharedAs"
                              , "enterMultipleChoice"
                              , "updateMultipleChoice"
                              , "enterSharedMultipleChoice"
                              , "updateSharedMultipleChoice"
                              , "wait"
                              , "waitForTime"
                              , "waitForDate"
                              , "waitForDateTime"
                              , "waitForTimer"
                              , "chooseAction"
                              , "viewTitle"
                              , "viewSharedTitle"
                              ]
  #! taskArgs = case (isCompact, isEditor, argsExprs) of
                  (True, True, [TCleanExpr _ (PPCleanExpr tn) : _]) -> if (size tn > 0 && tn.[0] == '"') [text ArialRegular10px tn] []
                  (True, _, _) -> []
                  _            -> taskArgs
  = tDefaultTaskApp` isCompact isActive wasActive isInAccessible nodeId selectedNodes parentModName parentTaskName modName taskName taskArgs tsrc

CurrActiveColor   :== toSVGColor "LimeGreen"
PrevActiveColor   :== toSVGColor "DeepSkyBlue"
InaccessibleColor :== toSVGColor "Gainsboro"
WhiteColor        :== toSVGColor "White"

appColor :: !Bool !Bool !Bool -> SVGColor
appColor isActive wasActive isInAccessible
  = if isActive
      CurrActiveColor
      (if wasActive
          PrevActiveColor
          (if isInAccessible
              InaccessibleColor
              WhiteColor
          )
      )

tDefaultTaskApp` :: !Bool !Bool !Bool !Bool !ExprId !(Set (ModuleName, TaskName, ExprId)) !ModuleName !TaskName !ModuleName !TaskName ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultTaskApp` isCompact isActive wasActive isInAccessible nodeId selectedNodes parentModName parentTaskName modName taskName taskArgs [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
  #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
  #! bgColor     = appColor isActive wasActive isInAccessible
  #! isSelected  = 'DS'.member (parentModName, parentTaskName, nodeId) selectedNodes
  = case taskArgs of
      []
        #! bgRect = tRoundedRect (imagexspan tntag) (imageyspan tntag) <@< { fill = bgColor }
                                                                       <@< { stroke = if isSelected (toSVGColor "navy") (toSVGColor "black") }
                                                                       <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskNameImg] Nothing, tsrc)
      taskArgs
        #! argsImg  = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] taskArgs Nothing))
        #! maxXSpan = maxSpan [imagexspan tntag, imagexspan argstag]
        #! content  = margin (px 5.0) (above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, argsImg] Nothing)
        #! bgRect   = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = bgColor }
                                                                                    <@< { stroke = if isSelected (toSVGColor "navy") (toSVGColor "black") }
                                                                                    <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tAssign :: !MkImageInh !TUser !String !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tAssign inh user desc assignedTask [(assignTaskTag, uAssignTaskTag) : (taskNameTag, uTaskNameTag) : (manTag, uManTag) : tsrc]
  #! (assignedTask, tsrc) = tExpr2Image inh assignedTask tsrc
  #! assignedTask         = tag uAssignTaskTag (margin (px 5.0) assignedTask)
  #! maxXSpan             = maxSpan [imagexspan taskNameTag + imagexspan manTag, imagexspan assignTaskTag]
  #! taskNameImg          = tag uTaskNameTag (margin (px 5.0) (text ArialBold10px (ppUser user +++ if (desc == "") "" (": " +++ desc))))
  #! bgRect               = rect maxXSpan (imageyspan taskNameTag + imageyspan assignTaskTag)
                              <@< { fill        = toSVGColor "white" }
                              <@< { stroke      = toSVGColor "black" }
                              <@< { strokewidth = px 1.0 }
                              <@< { xradius     = px 5.0 }
                              <@< { yradius     = px 5.0 }
                              <@< { dash        = [5, 5] }
  #! littleman            = tag uManTag littleman
  #! content              = above (repeat AtMiddleX) [] [beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing, xline Nothing maxXSpan, assignedTask] Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

ppUser :: !TUser -> String
ppUser TUAnyUser                       = "Any user"
ppUser (TUUserWithIdent ident)         = ident
ppUser (TUUserWithRole role)           = "Anyone with role " +++ role
ppUser TUSystemUser                    = "System user"
ppUser TUAnonymousUser                 = "Anonymous user"
ppUser (TUAuthenticatedUser usr roles) = usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles
ppUser (TUVariableUser usr)            = usr

tStep :: !MkImageInh !TExpr ![PPOr TStepCont] !*TagSource -> *(!Image ModelTy, !*TagSource)
tStep inh lhsExpr conts tsrc
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! branchActivity       = map (\x -> case x of PP _ -> False; T e -> containsActiveNodesStepCont inh e) conts
  #! someActivity         = foldr (\x acc -> x || acc) False branchActivity
  #! (conts`, tsrc)       = mapSt (\(cont, possiblyActive) -> tStepCont {inh & inh_inaccessible = someActivity && not possiblyActive} cont) (zip2 conts branchActivity) tsrc
  #! (conts`, refs, tsrc) = prepCases [] conts` tsrc
  #! vertConn             = mkVertConn refs
  #! contsImg             = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [lhs, tHorizConnArr, tStepStar, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tStepStar] Nothing
    , tsrc)

tagImgs :: ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], !*TagSource)
tagImgs [] tsrc = ([], [], tsrc)
tagImgs [i : is] tsrc
  #! (is, ts, tsrc) = tagImgs is tsrc
  #! ((i, t), tsrc) = tagWithSrc tsrc i
  = ([i : is], [t : ts], tsrc)

prepCases :: ![String] ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], *TagSource)
prepCases patStrs pats tsrc
  #! (pats, tags, tsrc) = tagImgs pats tsrc
  #! maxXSpan = maxSpan (map imagexspan tags)
  = (zipWith3 (prepCase maxXSpan) pats (patStrs ++ repeat "") tags, tags, tsrc)
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

mkVertConn :: ![ImageTag] -> Image ModelTy
mkVertConn ts
  | length ts < 2 = empty (px 0.0) (px 0.0)
  | otherwise
      #! firstTag  = hd ts
      #! lastTag   = last ts
      #! allYSpans = foldr (\x acc -> imageyspan x + acc) (px 0.0) ts
      = above (repeat AtMiddleX) []
          [ yline Nothing (imageyspan firstTag /. 2.0) <@< { stroke = toSVGColor "white" }
          , yline Nothing (allYSpans - (imageyspan firstTag /. 2.0) - (imageyspan lastTag /. 2.0)) <@< { stroke = toSVGColor "black" }
          , yline Nothing (imageyspan lastTag /. 2.0) <@< { stroke = toSVGColor "white" } ]
          Nothing

tStepCont :: !MkImageInh !(PPOr TStepCont) !*TagSource -> *(!Image ModelTy, !*TagSource)
tStepCont _   (PP pp) tsrc = (text ArialRegular10px pp, tsrc)
tStepCont inh (T t)   tsrc = tStepCont` inh.inh_trt t tsrc
  where
  tStepCont` :: !BlueprintRef !TStepCont !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepCont` trt (StepOnValue      sfilter) tsrc = tStepFilter trt Nothing sfilter tsrc
  tStepCont` trt (StepOnAction act sfilter) tsrc = tStepFilter trt (Just act) sfilter tsrc
  tStepCont` trt (StepOnException mpat te)  tsrc
    #! (img, tsrc) = tExpr2Image inh te tsrc
    // TODO mpat
    = (beside (repeat AtMiddleY) [] [tException, tHorizConnArr, /*[> TODO edge <]*/ img] Nothing, tsrc)
  tStepFilter :: !BlueprintRef !(Maybe String) !TStepFilter !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter trt mact sfilter [ref : tsrc]
    = tStepFilter` trt mact sfilter ref tsrc
  tStepFilter` :: !BlueprintRef !(Maybe String) !TStepFilter !*TagRef !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter` trt mact (Always te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* [> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (HasValue mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /*[> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfStable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConnArr, /*[> TODO edge <] */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfUnstable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConnArr,/* [> TODO edge <] */t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfCond pp mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /*[> TODO edge and conditional <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfValue pat fn vars mpat te) ref tsrc
    #! (t, tsrc)   = tExpr2Image inh te tsrc
    #! (ifv, tsrc) = tIfValue fn vars tsrc
    #! img         = beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConn, text ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, ifv, tHorizConnArr, /*[> TODO mpat <] */ t] Nothing
    = (img, tsrc)
  tStepFilter` trt mact (WithoutValue te) ref tsrc
    #! (t, tsrc) = tExpr2Image {inh & inh_in_maybe = True} te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* [> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (WithValue mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image {inh & inh_in_maybe = True} te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /*[> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (WithStable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image {inh & inh_in_maybe = True} te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /*[> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (WithUnstable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image {inh & inh_in_maybe = True} te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /*[> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (CustomFilter pp) ref tsrc = (text ArialRegular10px pp, tsrc)
  addAction :: !(Maybe String) !(Image ModelTy) !*TagRef -> Image ModelTy
  addAction (Just action) img (t, uT)
    #! l = tag uT (above (repeat AtMiddleX) [] [ beside (repeat AtMiddleY) [] [littleman, text ArialBold10px action] Nothing
                                               , img] Nothing)
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan t + px 5.0) (imageyspan t + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
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
tIfValue tffun args [(nameTag, uNameTag) : (argsTag, uArgsTag) : tsrc]
  #! maxXSpan   = maxSpan [imagexspan nameTag, imagexspan argsTag]
  #! bgRect     = rect maxXSpan (imageyspan nameTag + imageyspan argsTag)
                    <@< { fill        = toSVGColor "white" }
                    <@< { stroke      = toSVGColor "black" }
                    <@< { strokewidth = px 1.0 }
  #! tfNameImg  = tag uNameTag (margin (px 5.0) (text ArialItalic10px tffun))
  #! tfArgsImgs = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! tfContents = above (repeat AtLeft) [] (case args of
                                              [] -> [tfNameImg]
                                              _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing, tsrc)

tException :: Image ModelTy
tException
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "OrangeRed " }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "!!"] Nothing

tStable :: Image ModelTy
tStable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor "DeepSkyBlue" }

tUnstable :: Image ModelTy
tUnstable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor "LimeGreen" }

tNoVal :: Image ModelTy
tNoVal = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor "White" }

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