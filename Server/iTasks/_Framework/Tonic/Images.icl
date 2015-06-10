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
import Graphics.Scalable.Internal
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Tonic.Types
import iTasks._Framework.Tonic.Pretty
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
  , inh_in_step      :: !Bool
  , inh_in_mapp      :: !Bool
  , inh_in_fapp      :: !Bool
  , inh_in_case      :: !Bool
  , inh_selected     :: !Set (!ModuleName, !TaskName, !ExprId)
  , inh_outputs      :: !Map TaskId TStability
  , inh_selDetail    :: !Maybe ClickMeta
  }

mkTaskImage :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef
               !ListsOfTasks !(Map TaskId TStability)
               !(Set (ModuleName, TaskName, ExprId)) !(Maybe ClickMeta) !Bool
               !ModelTy *TagSource -> Image ModelTy
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
                        , inh_in_step      = False
                        , inh_in_mapp      = False
                        , inh_in_fapp      = False
                        , inh_in_case      = False
                        , inh_selected     = selected
                        , inh_outputs      = outputs
                        , inh_selDetail    = selDetail
                        }
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (img, _)         = tTaskDef trt tt.tt_module tt.tt_name tt.tt_resty tt.tt_args [] tt_body` tsrc
  = img

tExpr2Image :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpr2Image inh (TMApp eid mty mn tn targs prio) tsrc = tMApp         inh eid mty mn tn targs prio tsrc
tExpr2Image inh (TFApp fn targs assoc)      tsrc = tFApp         inh fn targs assoc tsrc
tExpr2Image inh (TLet pats bdy)             tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TCaseOrIf e pats)          tsrc = tCaseOrIf     inh e pats tsrc
tExpr2Image inh (TVar eid pp)               tsrc = tVar          inh eid pp tsrc
tExpr2Image inh (TLit pp)                   tsrc = tLit          inh pp tsrc
tExpr2Image inh (TExpand args tt)           tsrc = tExpand       inh args tt tsrc
tExpr2Image inh (TSel e es)                 tsrc = tSel          inh e es tsrc
tExpr2Image inh (TLam args e)               tsrc = tLam          inh args e tsrc

tLam :: !MkImageInh ![TExpr] !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tLam inh vars e tsrc
  #! (r, tsrc) = tExpr2Image inh e tsrc
  #! lineParts  = case vars of
                    []   -> [tHorizConnArr, r]
                    vars -> [tHorizConn, tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" vars), tHorizConnArr, r]
  = (beside (repeat AtMiddleY) [] lineParts Nothing, tsrc)

tSel :: !MkImageInh !TExpr ![TExpr] !*TagSource -> *(!Image ModelTy, !*TagSource)
tSel inh e es tsrc
  #! (eImg, tsrc) = tExpr2Image inh e tsrc
  = (eImg, tsrc) // TODO es

tFApp :: !MkImageInh !FunName ![TExpr] !TAssoc !*TagSource -> *(!Image ModelTy, !*TagSource)
tFApp inh fn args assoc tsrc = (text ArialRegular10px (ppTExpr (TFApp fn args assoc)), tsrc)

tArrowTip :: Image ModelTy
tArrowTip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]

tLineMarker :: Maybe (Markers ModelTy)
tLineMarker = Just {defaultMarkers & markerEnd = Just tArrowTip}

tSmallHorizConn :: Image ModelTy
tSmallHorizConn = xline Nothing (px 4.0)

tHorizConn :: Image ModelTy
tHorizConn = xline Nothing (px 8.0)

tShortHorizConn :: Image ModelTy
tShortHorizConn = xline Nothing (px 4.0)

tHorizConnArr :: Image ModelTy
tHorizConnArr = xline tLineMarker (px 16.0)

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip)}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just tArrowTip}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip), markerEnd = Just tArrowTip}) (px 16.0)

//tReturn :: !MkImageInh !ExprId !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
//tReturn inh=:{inh_in_maybe = True} eid expr tsrc = tExpr2Image inh expr tsrc
//tReturn inh                        eid expr tsrc = tMApp inh eid "" "return" [expr] tsrc

tExpand :: !MkImageInh ![TExpr] !TonicTask !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpand inh argnames tt tsrc
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  = tTaskDef inh.inh_trt tt.tt_module tt.tt_name tt.tt_resty tt.tt_args argnames tt_body` tsrc

tLit :: !MkImageInh !String !*TagSource -> *(!Image ModelTy, !*TagSource)
tLit inh pp tsrc
  | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case = (text ArialRegular10px pp, tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing, tsrc)

instance toString (Maybe a) | toString a where
  toString (Just x) = "Just " +++ toString x
  toString _        = "Nothing"

import StdDebug
tVar :: !MkImageInh !(Maybe ExprId) !String !*TagSource -> *(!Image ModelTy, !*TagSource)
tVar inh eid pp tsrc
  = case (inh.inh_trt.bpr_instance, eid) of
      (Just bpinst, Just eid)
        #! tsrc = trace_n ("tVar Just bpinst " +++ toString bpinst.bpi_taskId +++ " " +++ toString eid) tsrc
        = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
            Just mptids
              #! tsrc = trace_n ("tVar Just mptids" +++ toString bpinst.bpi_taskId +++ " " +++ toString eid) tsrc
              = case 'DIS'.elems mptids of
                  [(moduleName, taskName) : _]
                    = tMApp inh eid Nothing moduleName taskName [] TNonAssoc tsrc
                  _
                    #! tsrc = trace_n ("tVar Nothing elems" +++ toString bpinst.bpi_taskId +++ " " +++ toString eid) tsrc
                    = mkDef tsrc
            _
             #! tsrc = trace_n ("tVar Nothing mptids " +++ toString bpinst.bpi_taskId +++ " " +++ toString eid) tsrc
             = mkDef tsrc
      _ = mkDef tsrc
  where
  mkDef :: !*TagSource -> *(!Image ModelTy, !*TagSource)
  mkDef tsrc
    | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case = (text ArialRegular10px pp, tsrc)
    | otherwise
        #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing, tsrc)

containsActiveNodes :: !MkImageInh !TExpr -> Bool
containsActiveNodes inh (TFApp _ args _)      = foldr (\e acc -> acc || containsActiveNodes inh e) False args
containsActiveNodes inh (TMApp eid _ _ _ _ _) = 'DM'.member eid inh.inh_prev || maybe False (\bpi -> isJust (activeNodeTaskId eid bpi.bpi_activeNodes)) inh.inh_trt.bpr_instance
containsActiveNodes inh (TLet pats bdy)       = containsActiveNodes inh bdy
containsActiveNodes inh (TCaseOrIf e pats)    = foldr (\(_, e) acc -> acc || containsActiveNodes inh e) False pats
containsActiveNodes inh (TExpand args tt)     = containsActiveNodes inh tt.tt_body
containsActiveNodes inh (TSel e es)           = containsActiveNodes inh e || foldr (\e acc -> acc || containsActiveNodes inh e) False es
containsActiveNodes inh (TLam _ e)            = containsActiveNodes inh e
containsActiveNodes inh _                     = False

tCaseOrIf :: !MkImageInh !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseOrIf inh texpr pats tsrc
  #! patStrs         = map (ppTExpr o fst) pats
  #! patExprs        = map snd pats
  #! branchActivity  = map (containsActiveNodes inh) patExprs
  #! someActivity    = foldr (\x acc -> x || acc) False branchActivity
  #! patExprs`       = zip2 patExprs branchActivity
  #! (nextTasks, tsrc)       = mapSt (\(patExpr, possiblyActive) tsrc -> tExpr2Image {inh & inh_inaccessible = someActivity && not possiblyActive} patExpr tsrc) patExprs` tsrc
  #! (nextTasks, refs, tsrc) = prepCases patStrs nextTasks tsrc
  #! vertConn        = mkVertConn refs
  #! nextTasks`      = above (repeat AtLeft) [] nextTasks Nothing
  #! (diamond, tsrc) = tCaseDiamond inh texpr tsrc
  = (beside (repeat AtMiddleY) [] [diamond, tHorizConn, vertConn, nextTasks`, vertConn] Nothing, tsrc)

tCaseDiamond :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseDiamond inh texpr [(diamondTag, uDiamondTag) : tsrc]
  #! (exprImg, tsrc) = tExpr2Image {inh & inh_in_case = True} texpr tsrc
  #! exprImg         = tag uDiamondTag exprImg
  #! textHeight      = imageyspan diamondTag
  #! textWidth       = imagexspan diamondTag
  #! edgeMargin      = textHeight *. 2.0
  #! centerX         = (textWidth /. 2.0) + edgeMargin
  #! leftCorner      = (px 0.0, y textHeight edgeMargin (px 0.0))
  #! topCorner       = (centerX, ~ (y textHeight edgeMargin centerX))
  #! rightCorner     = (centerX *. 2.0, y textHeight edgeMargin (px 0.0))
  #! bottomCorner    = (centerX, y textHeight edgeMargin centerX)
  #! diamond         = polygon Nothing [ leftCorner, topCorner, rightCorner, bottomCorner ]
                         <@< { fill   = toSVGColor "white" }
                         <@< { stroke = toSVGColor "black" }
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [diamond, exprImg] Nothing, tsrc)
  where
  y :: !Span !Span !Span -> Span
  y textHeight edgeMargin x = x * (textHeight / edgeMargin)

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
        #! binds     = foldr (\(var, expr) acc -> [text ArialRegular10px (ppTExpr var) : text ArialRegular10px " = " : text ArialRegular10px (ppTExpr expr) : acc]) [] pats
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

tBind :: !MkImageInh !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! linePart   = case mpat of
                    Just pat -> [l`, tHorizConn, tTextWithGreyBackground ArialRegular10px (ppTExpr pat), tHorizConnArr, r`]
                    _        -> [l`, tHorizConnArr, r`]
  = (beside (repeat AtMiddleY) [] linePart Nothing, tsrc)

//// TODO Highlight nodes here?
tParSumL :: !MkImageInh !ExprId !TExpr !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tParSumL inh eid l r tsrc // TODO This is actually not correct yet... first image shouldn't have lines
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! l` = margin (px 5.0, px 0.0) l`
  #! r` = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtLeft) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [vertConn,  parImg, vertConn, tHorizConn ] Nothing, tsrc)
tParSumR :: !MkImageInh !ExprId !TExpr !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tParSumR inh eid l r tsrc // TODO This is actually not correct yet... second image shouldn't have lines
  #! (l`, tsrc)           = tExpr2Image inh l tsrc
  #! (r`, tsrc)           = tExpr2Image inh r tsrc
  #! l`                   = margin (px 5.0, px 0.0) l`
  #! r`                   = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtLeft) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [vertConn, parImg, vertConn] Nothing, tsrc)
tParSumN :: !MkImageInh !ExprId !(Either TExpr [TExpr]) !*TagSource -> *(!Image ModelTy, !*TagSource)
tParSumN inh eid ts tsrc
  #! (ts`, tsrc)       = mkParSum inh eid ts tsrc
  #! ts`               = map (margin (px 5.0, px 0.0)) ts`
  #! (ts`, refs, tsrc) = prepCases [] ts` tsrc
  #! vertConn          = mkVertConn refs
  #! contsImg          = above (repeat AtLeft) [] ts` Nothing
  = ( beside (repeat AtMiddleY) [] [vertConn, contsImg, vertConn] Nothing
    , tsrc)
  where
  mkParSum :: !MkImageInh !ExprId !(Either TExpr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource) // TODO This is wrong
  mkParSum inh eid (Left e) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\(moduleName, taskName) -> tMApp inh eid Nothing moduleName taskName [] TNonAssoc) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      # (img, tsrc) = tExpr2Image inh e tsrc
      = ([img], tsrc)
  mkParSum _ _ (Right es) tsrc = mapSt (tExpr2Image inh) es tsrc
tParProdN :: !MkImageInh !ExprId !(Either TExpr [TExpr]) !*TagSource -> *(!Image ModelTy, !*TagSource)
tParProdN inh eid ts tsrc
  #! (imgs, tsrc)     = mkParProd inh eid ts tsrc
  #! (ts, refs, tsrc) = prepCases [] imgs tsrc
  #! vertConn         = mkVertConn refs
  = ( beside (repeat AtMiddleY) [] [vertConn, above (repeat AtLeft) [] ts Nothing, vertConn] Nothing
    , tsrc)
  where
  mkParProd :: !MkImageInh !ExprId !(Either TExpr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParProd inh eid (Left pp) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\(moduleName, taskName) -> tMApp inh eid Nothing moduleName taskName [] TNonAssoc) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      # (img, tsrc) = tExpr2Image inh pp tsrc
      = ([img], tsrc)
  mkParProd _ _ (Right xs) tsrc = mapSt (tExpr2Image inh) xs tsrc

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

tTaskDef :: !BlueprintRef !String !String !TExpr ![(!TExpr, !TExpr)] ![TExpr] !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskDef bpr moduleName taskName resultTy args argvars tdbody [(nameTag, uNameTag) : (argsTag, uArgsTag) : (bdytag, uBodyTag) : tsrc]
  #! taskIdStr    = case bpr of
                      {bpr_instance = Just {bpi_taskId}} -> " (" +++ toString bpi_taskId +++ ")"
                      _                                  -> ""
  #! taskNameImg  = tag uNameTag (margin (px 5.0) (beside (repeat AtMiddleY) [] [text ArialRegular10px (moduleName +++ "."), text ArialBold10px (taskName +++ " :: " +++ ppTExpr resultTy), text ArialRegular10px taskIdStr] Nothing))
  #! argsImg      = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map mkArgAndTy (zip2 args (map Just argvars ++ repeat Nothing))) Nothing))
  #! taskBodyImgs = tag uBodyTag (margin (px 5.0) tdbody)
  #! maxX         = maxSpan [imagexspan nameTag, imagexspan argsTag, imagexspan bdytag]
  #! maxXLine     = xline Nothing maxX
  #! bgRect       = tRoundedRect maxX
                                 (imageyspan nameTag + imageyspan argsTag + imageyspan bdytag)
  #! imgs         = if (length args < 1) [taskNameImg, maxXLine, taskBodyImgs] [taskNameImg, maxXLine, argsImg, maxXLine, taskBodyImgs]
  #! contentsImg  = above (repeat AtLeft) [] imgs Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, contentsImg] Nothing, tsrc)
  where
  mkArgAndTy :: !(!(!TExpr, !TExpr), !Maybe TExpr) -> Image ModelTy
  mkArgAndTy ((arg, ty), mvar) = text ArialRegular10px (ppTExpr arg +++ " :: " +++ ppTExpr ty +++ (maybe "" (\x -> " = " +++ ppTExpr x) mvar))

//tFunctorApp :: !MkImageInh !TExpr !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
//tFunctorApp inh texpr tffun args [(nmtag, uNmTag) : (argstag, uArgsTag) : tsrc]
  //#! tfNameImg    = tag uNmTag (margin (px 5.0) (text ArialItalic10px tffun))
  //#! tfArgsImgs   = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  //#! (expr, tsrc) = tExpr2Image inh texpr tsrc
  //#! maxXSpan     = maxSpan [imagexspan nmtag, imagexspan argstag]
  //#! bgRect       = rect maxXSpan (imageyspan nmtag + imageyspan argstag)
                      //<@< { fill        = toSVGColor "white" }
                      //<@< { stroke      = toSVGColor "black" }
                      //<@< { strokewidth = px 1.0 }
  //#! tfContents   = above (repeat AtLeft) [] (case args of
                                                //[] -> [tfNameImg]
                                                //_  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  //#! tfApp        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  //= (beside (repeat AtMiddleY) [] [tfApp, tHorizConnArr, expr] Nothing, tsrc)

activeNodeTaskId :: !ExprId !(Map ListId (IntMap (TaskId, ExprId))) -> Maybe TaskId
activeNodeTaskId eid activeNodes
  = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems activeNodes) | eid == nid] of
      [tid : _] -> Just tid
      _         -> Nothing

tMApp :: !MkImageInh !ExprId !(Maybe TypeName) !ModuleName !VarName ![TExpr] !TAssoc !*TagSource -> *(!Image ModelTy, !*TagSource)
tMApp inh eid _ "iTasks.API.Extensions.User" "@:" [lhsExpr : rhsExpr : _] _ tsrc
  = tAssign inh lhsExpr rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Common.TaskCombinators" ">>|" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Core.Types" ">>=" [lhsExpr : TLam [var : _] rhsExpr : _] _ tsrc
  = tBind inh lhsExpr (Just var) rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Core.Types" ">>=" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Common.TaskCombinators" ">>*" [lhsExpr : rhsExpr : _] _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-&&-" [lhsExpr : rhsExpr : _] _ tsrc
  = renderTaskApp inh eid mn tn [lhsExpr, rhsExpr] "Parallel (-&&-): both tasks" tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"allTasks" [x] assoc tsrc
  #! ts = if (tExprIsList x) (tUnsafeExpr2List x) [x]
  = renderTaskApp inh eid mn tn ts "Parallel allTasks" tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"anyTask" [x] assoc tsrc
  #! ts = if (tExprIsList x) (tUnsafeExpr2List x) [x]
  = renderTaskApp inh eid mn tn ts "Parallel anyTask" tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||-" [lhsExpr : rhsExpr : _] _ tsrc
  = renderTaskApp inh eid mn tn [lhsExpr, rhsExpr] "Parallel (-||-): any task" tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"||-" [lhsExpr : rhsExpr : _] _ tsrc
  = renderTaskApp inh eid mn tn [lhsExpr, rhsExpr] "Parallel (||-): right bias" tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||" [lhsExpr : rhsExpr : _] _ tsrc
  = renderTaskApp inh eid mn tn [lhsExpr, rhsExpr] "Parallel (-||): left bias" tsrc
tMApp inh eid _ modName taskName taskArgs _ tsrc = renderTaskApp inh eid modName taskName taskArgs taskName tsrc

renderTaskApp inh eid modName taskName taskArgs displayName tsrc
  #! (taskArgs`, tsrc)  = mapSt (tExpr2Image {inh & inh_in_mapp = True}) taskArgs tsrc
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
  #! displayName        = displayName +++ taskIdStr
  #! (renderOpts, tsrc) = mapSt (\ta -> ta inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName modName displayName taskArgs`) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultMApp inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName modName displayName taskArgs taskArgs` tsrc
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

tDefaultMApp :: !Bool !Bool !Bool !Bool !(Set (ModuleName, TaskName, ExprId)) !ExprId !ModuleName !TaskName !ModuleName !TaskName ![TExpr] ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultMApp isCompact isActive wasActive isInAccessible selectedNodes nodeId parentModName parentTaskName modName taskName argsExprs taskArgs tsrc
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
                  (True, True, [TVar _ tn : _]) -> if (size tn > 0 && tn.[0] == '"') [text ArialRegular10px tn] []
                  (True, _, _) -> []
                  _            -> taskArgs
  = tDefaultMApp` isCompact isActive wasActive isInAccessible nodeId selectedNodes parentModName parentTaskName modName taskName taskArgs tsrc

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

tDefaultMApp` :: !Bool !Bool !Bool !Bool !ExprId !(Set (ModuleName, TaskName, ExprId)) !ModuleName !TaskName !ModuleName !TaskName ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultMApp` isCompact isActive wasActive isInAccessible nodeId selectedNodes parentModName parentTaskName modName taskName taskArgs [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
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
        #! taskArgs = map (margin (px 1.5, px 0.0)) taskArgs
        #! argsImg  = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] taskArgs Nothing))
        #! maxXSpan = maxSpan [imagexspan tntag, imagexspan argstag]
        #! content  = above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, argsImg] Nothing
        #! bgRect   = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = bgColor }
                                                                                    <@< { stroke = if isSelected (toSVGColor "navy") (toSVGColor "black") }
                                                                                    <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tAssign :: !MkImageInh !TExpr !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tAssign inh lhsExpr assignedTask [(assignTaskTag, uAssignTaskTag) : (headerTag, uHeaderTag) : tsrc]
  #! (desc, user)         = case lhsExpr of
                              (TFApp "_Tuple2" [usr, str : _] _) -> (ppTExpr str, mkUser usr)
                              usr                                -> ("", mkUser usr)
  #! (assignedTask, tsrc) = tExpr2Image inh assignedTask tsrc
  #! assignedTask         = tag uAssignTaskTag (margin (px 5.0) assignedTask)
  #! maxXSpan             = maxSpan [imagexspan headerTag, imagexspan assignTaskTag]
  #! taskNameImg          = margin (px 5.0) (text ArialBold10px (user +++ if (desc == "") "" (": " +++ desc)))
  #! assignHeader         = tag uHeaderTag (beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing)
  #! content              = above (repeat AtMiddleX) [] [assignHeader, xline Nothing maxXSpan, assignedTask] Nothing
  #! bgRect               = rect maxXSpan (imageyspan headerTag + imageyspan assignTaskTag)
                              <@< { fill        = toSVGColor "white" }
                              <@< { stroke      = toSVGColor "black" }
                              <@< { strokewidth = px 1.0 }
                              <@< { xradius     = px 5.0 }
                              <@< { yradius     = px 5.0 }
                              <@< { dash        = [5, 5] }
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)
  where
  mkUser (TFApp "AnyUser" _ _)          = "Any user"
  mkUser (TFApp "UserWithId" [uid:_] _) = ppTExpr uid
  mkUser (TFApp "UserWithRole" [r:_] _) = "Anyone with role " +++ ppTExpr r
  mkUser (TFApp "SystemUser" _ _)       = "System user"
  mkUser (TFApp "AnonymousUser" _ _)    = "Anonymous user"
  mkUser (TFApp "AuthenticatedUser" [uid:rs:_] _) = ppTExpr uid +++ " with roles " +++ foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" (tSafeExpr2List rs)
  mkUser (TFApp usr _ _)                = usr
  mkUser (TVar _ ppe)                   = ppe
  mkUser (TLit ppe)                     = ppe
  mkUser _                              = ""

tStep :: !MkImageInh !ExprId !TExpr !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tStep inh eid lhsExpr conts tsrc
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! conts                = tSafeExpr2List conts
  #! branchActivity       = map (containsActiveNodes inh) conts
  #! someActivity         = foldr (\x acc -> x || acc) False branchActivity
  #! (conts`, tsrc)       = mapSt (\(cont, possiblyActive) -> tStepCont {inh & inh_inaccessible = someActivity && not possiblyActive} cont) (zip2 conts branchActivity) tsrc
  #! (conts`, refs, tsrc) = prepCases [] conts` tsrc
  #! vertConn             = mkVertConn refs
  #! contsImg             = above (repeat AtLeft) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [lhs, tHorizConn, vertConn, contsImg, vertConn] Nothing
    , tsrc)

tExprIsList :: TExpr -> Bool
tExprIsList (TFApp "_Cons" _ _) = True
tExprIsList (TFApp "_Nil"  _ _) = True
tExprIsList _                   = False

tUnsafeExpr2List :: TExpr -> [TExpr]
tUnsafeExpr2List (TFApp "_Cons" [hd : tl : _] _) = [hd : tUnsafeExpr2List tl]
tUnsafeExpr2List (TFApp "_Nil"  _             _) = []

tSafeExpr2List :: TExpr -> [TExpr]
tSafeExpr2List (TFApp "_Cons" [hd : tl : _] _) = [hd : tUnsafeExpr2List tl]
tSafeExpr2List (TFApp "_Nil"  _             _) = []
tSafeExpr2List e                               = [e]

tStepCont :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tStepCont inh (TFApp "OnAction" [TFApp "Action" [TLit action : _] _ : cont : _ ] _) tsrc
  = mkStepCont inh (Just action) cont tsrc
tStepCont inh (TFApp "OnValue"  [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont inh (TFApp "OnException" [cont : _ ] _)     tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont inh (TFApp "OnAllExceptions" [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc

mkStepCont inh mact (TFApp "always" [x : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh x tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifStable" [TLam pats e : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh e tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConn, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifStable" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifUnstable" [TLam pats e : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh e tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConn, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifUnstable" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifValue" [conditionApp : continuationApp : _] _) [ref : tsrc]
  #! (conditionImg, tsrc)    = tCaseDiamond inh conditionApp tsrc
  #! (continuationImg, tsrc) = tExpr2Image inh continuationApp tsrc
  = ( beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr, addAction mact alwaysFilter ref, tShortHorizConn, /* TODO edge */ continuationImg] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "hasValue" [TLam pats e : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh e tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConn, tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" pats), tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "hasValue" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "ifCond" [conditionApp : continuationApp : _] _) [ref : tsrc]
  #! (conditionImg, tsrc)    = tCaseDiamond inh conditionApp tsrc
  #! (continuationImg, tsrc) = tExpr2Image inh continuationApp tsrc
  = ( beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr, addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge */ continuationImg] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "always" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "withoutValue" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "withValue" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "withStable" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact (TFApp "withUnstable" [mapp : _] _) [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  = ( beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConnArr, /* TODO edge */ x] Nothing
    , tsrc)
mkStepCont inh mact e [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh e tsrc
  = ( beside (repeat AtMiddleY) [] [tException, x] Nothing
    , tsrc)

addAction :: !(Maybe String) !(Image ModelTy) !*TagRef -> Image ModelTy
addAction (Just action) img (t, uT)
  #! l = tag uT (above (repeat AtMiddleX) [] [ margin (px 3.0) (beside (repeat AtMiddleY) [] [littleman, text ArialBold10px (" " +++ action)] Nothing)
                                             , img] Nothing)
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan t + px 5.0) (imageyspan t + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
                                               , l] Nothing
addAction _ img _ = img

alwaysFilter :: Image ModelTy
alwaysFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable, tNoVal] Nothing

hasValueFilter :: Image ModelTy
hasValueFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable] Nothing

tagImgs :: ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], !*TagSource)
tagImgs [] tsrc = ([], [], tsrc)
tagImgs [i : is] tsrc
  #! (is, ts, tsrc) = tagImgs is tsrc
  #! ((i, t), tsrc) = tagWithSrc tsrc i
  = ([i : is], [t : ts], tsrc)

prepCases :: ![String] ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], *TagSource)
prepCases patStrs pats tsrc
  #! allPatStrs         = patStrs ++ repeat ""
  #! pats               = zipWith addLhs pats allPatStrs
  #! (pats, tags, tsrc) = tagImgs pats tsrc
  #! maxXSpan           = maxSpan (map imagexspan tags)
  = (zipWith (prepCase maxXSpan) pats tags, tags, tsrc)
  where
  addLhs :: !(Image ModelTy) !String -> Image ModelTy
  addLhs pat "" = beside (repeat AtMiddleY) [] [tHorizConnArr, pat] Nothing
  addLhs pat patStr
    #! textBox = tTextWithGreyBackground ArialRegular10px patStr
    = beside (repeat AtMiddleY) [] [tHorizConn, textBox, tHorizConnArr, pat] Nothing

  prepCase :: !Span !(Image ModelTy) !ImageTag -> Image ModelTy
  prepCase maxXSpan pat tag
    #! rightLine = xline Nothing ((maxXSpan - imagexspan tag) + px 8.0)
    = margin (px 2.5, px 0.0) (beside (repeat AtMiddleY) [] [pat, rightLine] Nothing)

tTextWithGreyBackground font txt
  #! textWidth = textxspan font txt + px 10.0
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (font.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text font txt] Nothing

mkVertConn :: ![ImageTag] -> Image ModelTy
mkVertConn ts
  | length ts < 2 = empty (px 0.0) (px 0.0)
  | otherwise
      #! firstTag  = hd ts
      #! lastTag   = last ts
      #! allYSpans = (foldr (\x acc -> imageyspan x + acc) (px 0.0) ts) + px (toReal (length ts) * 5.0)
      #! firstY    = imageyspan firstTag + px 5.0
      #! lastY     = imageyspan lastTag + px 5.0
      = above (repeat AtMiddleX) []
          [ yline Nothing (firstY /. 2.0) <@< { stroke = toSVGColor "white" }
          , yline Nothing (allYSpans - (firstY /. 2.0) - (lastY /. 2.0)) <@< { stroke = toSVGColor "black" }
          , yline Nothing (lastY /. 2.0) <@< { stroke = toSVGColor "white" } ]
          Nothing

littleman :: Image a
littleman
  #! maskRect = rect (px 16.0) (px 16.0) <@< {FillAttr | fill = toSVGColor "white"}
                                         <@< {StrokeWidthAttr | strokewidth = px 0.0}
  = (overlay [] [(px -2.0, px 8.0), (px 3.0, px 1.0)] [ circle (px 20.0) <@< {StrokeWidthAttr | strokewidth = px 1.0} <@< {StrokeAttr | stroke = toSVGColor "white"}
                                                      , circle (px 10.0) <@< {StrokeWidthAttr | strokewidth = px 1.0} <@< {StrokeAttr | stroke = toSVGColor "white"}] (Just maskRect)) <@< {MaskAttr | mask = maskRect}

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
