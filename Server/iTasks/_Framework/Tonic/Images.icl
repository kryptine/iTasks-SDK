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
import Text

TonicBlue     =: toSVGColor "DeepSkyBlue"
TonicDarkBlue =: toSVGColor "navy"
TonicGreen    =: toSVGColor "LimeGreen"
TonicWhite    =: toSVGColor "white"
TonicBlack    =: toSVGColor "black"
TonicRed      =: toSVGColor "OrangeRed"
TonicGrey     =: toSVGColor "Gainsboro"

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

:: InhMkImg =
  { inh_trt          :: !BlueprintRef
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
  , inh_selDetail    :: !Maybe (Either ClickMeta (!ModuleName, !TaskName, !TaskId, !Int))
  , inh_stepActions  :: !Map TaskId [UIAction]
  }

:: SynMkImg =
  { syn_img    :: !Image ModelTy
  , syn_status :: !TStatus
  }

:: TStatus = AllDone | IsActive | NotActive

derive JSONEncode TStatus

instance == TStatus where
  (==) AllDone  AllDone    = True
  (==) IsActive IsActive   = True
  (==) NotActive NotActive = True
  (==) _        _          = False

mkTaskImage :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef
               !(Map TaskId TStability) !(Map TaskId [UIAction])
               !(Set (!ModuleName, !TaskName, !ExprId)) !(Maybe (Either ClickMeta (!ModuleName, !TaskName, !TaskId, !Int))) !Bool
               !ModelTy *TagSource -> Image ModelTy
mkTaskImage rs prev trt outputs stepActions selected selDetail compact {ActionState | state = tis} tsrc
  #! tt               = tis.tis_task
  #! inh              = { InhMkImg
                        | inh_trt          = trt
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
                        , inh_stepActions  = stepActions
                        }
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (img, _)         = tTaskDef inh trt tt.tt_module tt.tt_name tt.tt_resty tt.tt_args [] tt_body`.syn_img tsrc
  = img

tExpr2Image :: !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tExpr2Image inh (TMApp meid mtid mty mn tn targs prio) tsrc = tMApp     inh meid mtid mty mn tn targs prio tsrc
tExpr2Image inh (TFApp fn targs assoc)           tsrc       = tFApp     inh fn targs assoc tsrc
tExpr2Image inh (TLet pats bdy)                  tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TCaseOrIf e pats)               tsrc       = tCaseOrIf inh e pats tsrc
tExpr2Image inh (TVar eid pp)                    tsrc       = tVar      inh eid pp tsrc
tExpr2Image inh (TLit pp)                        tsrc       = tLit      inh pp tsrc
tExpr2Image inh (TExpand args tt)                tsrc       = tExpand   inh args tt tsrc
tExpr2Image inh (TSel e es)                      tsrc       = tSel      inh e es tsrc
tExpr2Image inh (TRecUpd vn e es)                tsrc       = tRecUpd   inh vn e es tsrc
tExpr2Image inh (TLam args e)                    tsrc       = tLam      inh args e tsrc

tLam :: !InhMkImg ![TExpr] !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tLam inh vars e tsrc
  #! (r, tsrc) = tExpr2Image inh e tsrc
  #! lineParts = case vars of
                   []   -> [tHorizConnArr r.syn_status, r.syn_img]
                   vars -> [tHorizConn r.syn_status, tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" vars), tHorizConnArr r.syn_status, r.syn_img]
  #! img       = beside (repeat AtMiddleY) [] lineParts Nothing
  = ({syn_img = img, syn_status = r.syn_status}, tsrc)

tSel :: !InhMkImg !TExpr ![TExpr] !*TagSource -> *(!SynMkImg, !*TagSource)
tSel inh e es tsrc
  = ( { syn_img    = text ArialRegular10px (ppTExpr e +++ "." +++ ppIntersperse ppTExpr "." es)
      , syn_status = NotActive}
    , tsrc)

tRecUpd :: !InhMkImg !VarName !TExpr ![TExpr] !*TagSource -> *(!SynMkImg, !*TagSource)
tRecUpd inh vn e es tsrc
  = ( { syn_img    = text ArialRegular10px ("{ " +++ vn % (1, size vn) +++ " | " +++ ppTExpr e +++ " & " +++ ppES es +++ "}")
      , syn_status = NotActive }
    , tsrc)
  where
  ppES []     = ""
  ppES [x]    = ppTExpr x
  ppES [TNoBind : xs] = ppES xs
  ppES [x : xs] = ppTExpr x +++ " " +++ ppES xs

tFApp :: !InhMkImg !FunName ![TExpr] !TPriority !*TagSource -> *(!SynMkImg, !*TagSource)
tFApp inh fn args assoc tsrc
  = ( { syn_img    = text ArialRegular10px (ppTExpr (TFApp fn args assoc))
      , syn_status = NotActive}
    , tsrc)

tArrowTip :: !TStatus -> Image ModelTy
tArrowTip status
  #! tip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]
  = case status of
      AllDone  = tip <@< { fill   = TonicBlue }
                     <@< { stroke = TonicBlack }
      IsActive = tip <@< { fill   = TonicGreen }
                     <@< { stroke = TonicBlack }
      _        = tip

tLineMarker :: !TStatus -> Maybe (Markers ModelTy)
tLineMarker status = Just {defaultMarkers & markerEnd = Just (tArrowTip status)}

tSmallHorizConn :: Image ModelTy
tSmallHorizConn = xline Nothing (px 4.0)

tHorizConn :: !TStatus -> Image ModelTy
tHorizConn AllDone  = rect (px 8.0) (px 3.0) <@< { fill = TonicBlue }
tHorizConn IsActive = rect (px 8.0) (px 3.0) <@< { fill = TonicGreen }
tHorizConn _        = xline Nothing (px 8.0)

tShortHorizConn :: !TStatus -> Image ModelTy
tShortHorizConn AllDone  = rect (px 4.0) (px 3.0) <@< { fill = TonicBlue }
tShortHorizConn IsActive = rect (px 4.0) (px 3.0) <@< { fill = TonicGreen }
tShortHorizConn _        = xline Nothing (px 4.0)

tHorizConnArr :: !TStatus -> Image ModelTy
tHorizConnArr status = beside (repeat AtMiddleY) [] [tHorizConn status, tArrowTip status] Nothing

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip NotActive))}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just (tArrowTip NotActive)}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip NotActive)), markerEnd = Just (tArrowTip NotActive)}) (px 16.0)

tExpand :: !InhMkImg ![TExpr] !TonicTask !*TagSource -> *(!SynMkImg, !*TagSource)
tExpand inh argnames tt tsrc
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (td_img, tsrc)   = tTaskDef inh inh.inh_trt tt.tt_module tt.tt_name tt.tt_resty tt.tt_args argnames tt_body`.syn_img tsrc
  = ({ syn_img = td_img
     , syn_status = tt_body`.syn_status}
    , tsrc)

tLit :: !InhMkImg !String !*TagSource -> *(!SynMkImg, !*TagSource)
tLit inh pp tsrc
  | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case = ({syn_img = text ArialRegular10px pp, syn_status = NotActive}, tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing
      = ({syn_img = img, syn_status = NotActive}, tsrc)

instance toString (Maybe a) | toString a where
  toString (Just x) = "Just " +++ toString x
  toString _        = "Nothing"

tVar :: !InhMkImg !(Maybe ExprId) !String !*TagSource -> *(!SynMkImg, !*TagSource)
tVar inh eid pp tsrc
  | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case
      = ({syn_img = text ArialRegular10px pp, syn_status = NotActive}, tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing
      = ({syn_img = img, syn_status = NotActive}, tsrc)

activityStatus :: !Bool !InhMkImg !TExpr -> TStatus
activityStatus needAllActive inh (TFApp _ args _)      = determineStatus needAllActive (map (activityStatus needAllActive inh) args)
activityStatus needAllActive inh (TMApp (Just eid) _ _ _ _ exprs _)
  | 'DM'.member eid inh.inh_prev = AllDone
  | otherwise
      = case inh.inh_trt.bpr_instance of
          Just {bpi_activeNodes} | isJust (activeNodeTaskId eid bpi_activeNodes) = IsActive
          _ = determineStatus needAllActive (map (activityStatus needAllActive inh) exprs)
activityStatus needAllActive inh (TLet pats bdy)       = activityStatus needAllActive inh bdy
activityStatus needAllActive inh (TCaseOrIf e pats)    = determineStatus needAllActive (map (activityStatus needAllActive inh o snd) pats)
activityStatus needAllActive inh (TExpand args tt)     = activityStatus needAllActive inh tt.tt_body
activityStatus needAllActive inh (TLam _ e)            = activityStatus needAllActive inh e
activityStatus needAllActive inh _                     = NotActive

determineStatus :: !Bool ![TStatus] -> TStatus
determineStatus _ [IsActive : _] = IsActive
determineStatus needAllActive [AllDone : xs]
  | needAllActive
      = case determineStatus needAllActive xs of
          AllDone -> AllDone
          x       -> x
  | otherwise = AllDone
determineStatus needAllActive [_ : xs] = determineStatus needAllActive xs
determineStatus _ _        = NotActive

determineSynStatus :: !Bool ![SynMkImg] -> TStatus
determineSynStatus needAllActive syns = determineStatus needAllActive (map (\x -> x.syn_status) syns)

tCaseOrIf :: !InhMkImg !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!SynMkImg, !*TagSource)
tCaseOrIf inh texpr pats [(contextTag, _) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image False True (map (\(p, t) -> (Just p, t, True)) pats) contextTag tsrc
  #! (exprImg, tsrc)      = tExpr2Image {inh & inh_in_case = True} texpr tsrc
  #! (diamond, tsrc)      = tCaseDiamond inh exprImg.syn_img tsrc
  #! lineAct              = case syn_branches.syn_status of
                              NotActive -> NotActive
                              _         -> AllDone
  #! img                  = beside (repeat AtMiddleY) [] [diamond, tHorizConn lineAct, syn_branches.syn_img] Nothing
  = ({syn_img = img, syn_status = syn_branches.syn_status}, tsrc)

tCaseDiamond :: !InhMkImg !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseDiamond inh exprImg [(diamondTag, uDiamondTag) : tsrc]
  #! exprImg         = tag uDiamondTag exprImg
  #! textHeight      = imageyspan diamondTag
  #! textWidth       = maxSpan [px 50.0, imagexspan diamondTag]
  #! edgeMargin      = textHeight *. 2.0
  #! centerX         = (textWidth /. 2.0) + edgeMargin
  #! leftCorner      = (px 0.0, y textHeight edgeMargin (px 0.0))
  #! topCorner       = (centerX, ~ (y textHeight edgeMargin centerX))
  #! rightCorner     = (centerX *. 2.0, y textHeight edgeMargin (px 0.0))
  #! bottomCorner    = (centerX, y textHeight edgeMargin centerX)
  #! diamond         = polygon Nothing [ leftCorner, topCorner, rightCorner, bottomCorner ]
                         <@< { fill   = TonicWhite }
                         <@< { stroke = TonicBlack }
  #! img             = overlay (repeat (AtMiddleX, AtMiddleY)) [] [diamond, exprImg] Nothing
  = (img, tsrc)
  where
  y :: !Span !Span !Span -> Span
  y textHeight edgeMargin x = x * (textHeight / edgeMargin)

tLet :: !InhMkImg ![(!Pattern, !TExpr)] !TExpr !*TagSource -> *(!SynMkImg, *TagSource)
tLet inh pats expr [(txttag, uTxtTag) : tsrc]
  = case expr of
      TLet pats` bdy
        = tLet inh (pats ++ pats`) bdy tsrc
      _
        #! (t, tsrc)       = tExpr2Image inh expr tsrc
        #! (patRhss, tsrc) = mapSt (tExpr2Image inh) (map snd pats) tsrc
        #! binds     = foldr (\(var, expr) acc -> [text ArialRegular10px (ppTExpr var) : text ArialRegular10px " = " : expr.syn_img : acc]) [] (zip2 (map fst pats) patRhss)
        #! letText   = tag uTxtTag (grid (Columns 3) (RowMajor, LeftToRight, TopToBottom) [] [] binds Nothing)
        #! letWidth  = imagexspan txttag + px 10.0
        #! letHeight = imageyspan txttag + px 10.0
        #! letBox    = rect letWidth letHeight
                         <@< { fill   = TonicWhite }
                         <@< { stroke = TonicBlack }
        #! letImg    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
        #! linePart  = xline Nothing ((letWidth - px 8.0) /. 2.0)
        #! connBox   = beside (repeat AtMiddleY) [] [linePart, rect (px 8.0) (px 8.0), linePart] Nothing
        #! letImg    = above (repeat AtMiddleX) [] [letImg, yline Nothing (px 8.0), connBox, empty zero (letHeight + px 8.0)] Nothing
        #! img       = beside (repeat AtMiddleY) [] [letImg, tHorizConnArr t.syn_status, t.syn_img] Nothing
        = ({syn_img = img, syn_status = t.syn_status}, tsrc)

tBind :: !InhMkImg !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! lineAct    = case r`.syn_status of
                    NotActive -> NotActive
                    _         -> AllDone
  #! linePart   = case mpat of
                    Just pat -> [l`.syn_img, tHorizConn lineAct, tTextWithGreyBackground ArialRegular10px (ppTExpr pat), tHorizConnArr lineAct, r`.syn_img]
                    _        -> [l`.syn_img, tHorizConnArr lineAct, r`.syn_img]
  #! img        = beside (repeat AtMiddleY) [] linePart Nothing
  = ({syn_img = img, syn_status = r`.syn_status}, tsrc)

tParSumL :: !InhMkImg !(Maybe ExprId) !String !String !TExpr !TExpr !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumL inh eid mn tn l r [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False [(Nothing, l, True), (Nothing, r, False)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (-||): left bias" syn_branches uContextTag tsrc
tParSumR :: !InhMkImg !(Maybe ExprId) !String !String !TExpr !TExpr !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumR inh eid mn tn l r [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False [(Nothing, l, False), (Nothing, r, True)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (||-): right bias" syn_branches uContextTag tsrc
tParSumN :: !InhMkImg !(Maybe ExprId) !String !String !String ![TExpr]
            !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False (map (\x -> (Nothing, x, True)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc
tParProdN :: !InhMkImg !(Maybe ExprId) !String !String !String ![TExpr]
             !*TagSource
          -> *(!SynMkImg, !*TagSource)
tParProdN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False (map (\x -> (Nothing, x, True)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc

renderParallelContainer :: !InhMkImg !(Maybe ExprId) !ModuleName !TaskName !String
                           !SynMkImg !*ImageTag !*TagSource
                        -> *(!SynMkImg, !*TagSource)
renderParallelContainer inh meid moduleName taskName descr syn_branches uContextTag tsrc
  #! mActiveTid         = case (inh.inh_trt.bpr_instance, meid) of
                            (Just bpinst, Just eid) -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _                       -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = case meid of
                            Just eid -> 'DM'.get eid inh.inh_prev
                            _        -> Nothing
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! stability          = maybe (maybe TNoVal (const TStable) mPrevActiveTid) (\tid -> fromMaybe TNoVal ('DM'.get tid inh.inh_outputs)) mActiveTid
  #! taskIdStr          = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> " (" +++ toString x +++ ")"
                            (_, Just x) -> " (" +++ toString x +++ ")"
                            _           -> ""
  #! displayName        = descr +++ taskIdStr
  #! (taskApp, tsrc)    = tParApp inh.inh_compact meid inh.inh_selected inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName displayName syn_branches tsrc
  #! clickMeta          = mkClickMeta inh meid moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_trt.bpr_instance) mbNavTo
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just (Left
                                   { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}
                                   , click_origin_mbnodeId}) -> bpident_moduleName == inh.inh_trt.bpr_moduleName && bpident_taskName == inh.inh_trt.bpr_taskName && click_origin_mbnodeId == meid
                            _                                -> False
  #! taskApp            = taskApp <@< { onclick = navigateOrSelect clickMeta, local = False }
  #! valAnchor          = circle (px 12.0) <@< { onclick = openDetails clickMeta, local = False }
                                           <@< { fill = case stability of
                                                          TNoVal    -> TonicWhite
                                                          TStable   -> TonicBlue
                                                          TUnstable -> TonicGreen
                                               }
                                           <@< { stroke = if valNodeIsSelected TonicDarkBlue TonicBlack }
                                           <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! isSelected         = case meid of
                            Just eid -> 'DS'.member (inh.inh_trt.bpr_moduleName, inh.inh_trt.bpr_taskName, eid) inh.inh_selected
                            _        -> False
  #! inclArr            = beside (repeat AtMiddleY) [] (if isSelected [taskApp, valAnchor] [taskApp]) Nothing
  = ({syn_img = inclArr, syn_status = if isActive IsActive (if (isJust mPrevActiveTid) AllDone NotActive)}, tsrc)
  where
  tParApp :: !Bool !(Maybe ExprId) !(Set (!ModuleName, !TaskName, !ExprId))
             !ModuleName !TaskName !TaskName !SynMkImg !*TagSource
          -> *(!Image ModelTy, !*TagSource)
  tParApp isCompact mNodeId selectedNodes parentModName parentTaskName taskName syn_branches [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
    #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
    #! taskNameImg = tag uContextTag taskNameImg
    #! isSelected  = case mNodeId of
                       Just nodeId -> 'DS'.member (parentModName, parentTaskName, nodeId) selectedNodes
                       _           -> False
    #! maxXSpan    = maxSpan [imagexspan tntag, imagexspan argstag]
    #! content     = above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, tag uArgsTag syn_branches.syn_img] Nothing
    #! bgRect      = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = TonicWhite }
                                                                                   <@< { stroke = if isSelected TonicDarkBlue TonicBlack }
                                                                                   <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
    #! img         = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing
    = (img, tsrc)

  navigateOrSelect :: !ClickMeta !Int !ModelTy -> ModelTy
  navigateOrSelect meta 1 st = { ActionState | st & action = Just (TSelectNode, meta) }
  navigateOrSelect _    _ st = st

mkClickMeta :: !InhMkImg !(Maybe ExprId) !ModuleName !TaskName !(Maybe TaskId) !(Maybe TaskId) -> ClickMeta
mkClickMeta inh mbnid modName taskName mborig mbtarget =
  { click_origin_mbbpident = Just { bpident_moduleName = inh.inh_trt.bpr_moduleName
                                  , bpident_taskName   = inh.inh_trt.bpr_taskName
                                  , bpident_taskId     = mborig
                                  }
  , click_origin_mbnodeId  = mbnid
  , click_target_bpident   = { bpident_moduleName = modName
                             , bpident_taskName   = taskName
                             , bpident_taskId     = mbtarget
                             }
  }

tStartSymb :: Image ModelTy
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image ModelTy
tStopSymb = rect (px 16.0) (px 16.0)

tTaskDef :: !InhMkImg !BlueprintRef !String !String !TExpr ![(!TExpr, !TExpr)] ![TExpr] !(Image ModelTy) !*TagSource
         -> *(!Image ModelTy, !*TagSource)
tTaskDef inh bpr moduleName taskName resultTy args argvars tdbody [(nameTag, uNameTag) : (argsTag, uArgsTag) : (bdytag, uBodyTag) : tsrc]
  #! userStr      = case bpr of
                      {bpr_instance = Just {bpi_currentUser = Just cu}} -> " Performed by " +++ toString cu
                      _                                                 -> ""
  #! taskIdStr    = case bpr of
                      {bpr_instance = Just {bpi_taskId}} -> " (" +++ toString bpi_taskId +++ ")"
                      _                                  -> ""
  #! taskNameImg  = beside (repeat AtMiddleY) [] [ text ArialRegular10px (moduleName +++ ".")
                                                 , text ArialBold10px (taskName +++ " :: " +++ ppTExpr resultTy)
                                                 , text ArialRegular10px taskIdStr
                                                 , text ArialRegular10px userStr] Nothing
  #! taskNameImg  = tag uNameTag (margin (px 5.0) taskNameImg)
  #! binds        = flatten (zipWith3 mkArgAndTy args [0..] (map Just argvars ++ repeat Nothing))
  #! argsText     = grid (Columns 4) (RowMajor, LeftToRight, TopToBottom) [] [] (map (margin (px 1.0, px 0.0)) binds) Nothing
  #! argsImg      = tag uArgsTag (margin (px 5.0) argsText)
  #! taskBodyImgs = tag uBodyTag (margin (px 5.0) tdbody)
  #! maxX         = maxSpan [imagexspan nameTag, imagexspan argsTag, imagexspan bdytag]
  #! maxXLine     = xline Nothing maxX
  #! bgRect       = tRoundedRect maxX
                                 (imageyspan nameTag + imageyspan argsTag + imageyspan bdytag)
  #! imgs         = if (length args < 1) [taskNameImg, maxXLine, taskBodyImgs] [taskNameImg, maxXLine, argsImg, maxXLine, taskBodyImgs]
  #! contentsImg  = above (repeat AtLeft) [] imgs Nothing
  #! img          = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, contentsImg] Nothing
  = (img, tsrc)
  where
  mkArgAndTy :: !(!TExpr, !TExpr) !Int !(Maybe TExpr) -> [Image ModelTy]
  mkArgAndTy (arg, ty) i mvar
    #! meta = mkClickMeta inh Nothing moduleName taskName Nothing Nothing
    = [ text ArialRegular10px (ppTExpr arg) <@< { onclick = selectArg bpr meta i, local = False}
      , text ArialRegular10px " :: "        <@< { onclick = selectArg bpr meta i, local = False}
      , text ArialRegular10px (ppTExpr ty)  <@< { onclick = selectArg bpr meta i, local = False}
      , text ArialRegular10px (maybe "" (\x -> " = " +++ ppTExpr x) mvar) <@< { onclick = selectArg bpr meta i, local = False}
      ]

  selectArg :: !BlueprintRef !ClickMeta !Int !Int !ModelTy -> ModelTy
  selectArg {bpr_instance = Just {bpi_taskId}} meta i 1 st = { ActionState | st & action = Just (TSelectArg i, meta) }
  selectArg _                                  _    _ _ st = st

activeNodeTaskId :: !ExprId !(Map ListId (IntMap (!TaskId, !ExprId))) -> Maybe TaskId
activeNodeTaskId eid activeNodes
  = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems activeNodes) | eid == nid] of
      [tid : _] -> Just tid
      _         -> Nothing

tMApp :: !InhMkImg !(Maybe ExprId) !(Maybe (!Int, !Int)) !(Maybe TypeName) !ModuleName !VarName ![TExpr]
         !TPriority !*TagSource
      -> *(!SynMkImg, !*TagSource)
tMApp inh _ _ _ "iTasks.API.Extensions.User" "@:" [lhsExpr : rhsExpr : _] _ tsrc
  = tAssign inh lhsExpr rhsExpr tsrc
tMApp inh _ _ _ "iTasks.API.Common.TaskCombinators" ">>|" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh _ _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : TLam [var : _] rhsExpr : _] _ tsrc
  = tBind inh lhsExpr (Just var) rhsExpr tsrc
tMApp inh _ _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh eid _ _ "iTasks.API.Common.TaskCombinators" ">>*" [lhsExpr : rhsExpr : _] _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ _ "iTasks.API.Core.TaskCombinators" "step" [lhsExpr : _ : rhsExpr : _] _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-&&-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParProdN inh eid mn tn "Parallel (-&&-): both tasks" [lhsExpr, rhsExpr] tsrc
tMApp inh eid _ mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"allTasks" [x] assoc tsrc
  = tParProdN inh eid mn tn "Parallel allTasks" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid _ mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"anyTask" [x] assoc tsrc
  = tParSumN inh eid mn tn "Parallel anyTask" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumN inh eid mn tn "Parallel (-||-): any task" [lhsExpr, rhsExpr] tsrc
tMApp inh eid _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"||-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumR inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh eid _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumL inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh eid mtid _ modName taskName taskArgs _ tsrc
  #! inh = {inh & inh_in_mapp = True}
  = renderTaskApp inh eid mtid modName taskName taskArgs taskName tsrc

renderTaskApp :: !InhMkImg !(Maybe ExprId) !(Maybe (!Int, !Int)) !String !String ![TExpr] !String !*TagSource
              -> *(!SynMkImg, !*TagSource)
renderTaskApp inh meid mtid moduleName taskName taskArgs displayName tsrc
  #! (taskArgs`, tsrc)  = mapSt (tExpr2Image inh) taskArgs tsrc
  #! taskArgs`          = map (\x -> x.syn_img) taskArgs`
  #! mActiveTid         = case (inh.inh_trt.bpr_instance, meid) of
                            (Just bpinst, Just eid) -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _                       -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = case meid of
                            Just eid -> 'DM'.get eid inh.inh_prev
                            _        -> Nothing
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! wasActive          = isJust mPrevActiveTid
  #! stability          = maybe (maybe TNoVal (const TStable) mPrevActiveTid) (\tid -> fromMaybe TNoVal ('DM'.get tid inh.inh_outputs)) mActiveTid
  #! taskIdStr          = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> " (" +++ toString x +++ ")"
                            (_, Just x) -> " (" +++ toString x +++ ")"
                            _           -> ""
  #! displayName        = displayName +++ taskIdStr
  #! (renderOpts, tsrc) = mapSt (\ta -> ta inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected meid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName moduleName displayName taskArgs`) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultMApp inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_selected meid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName moduleName displayName taskArgs taskArgs` tsrc
  #! clickMeta          = mkClickMeta inh meid moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_trt.bpr_instance) mbNavTo
  #! taskApp            = taskApp <@< { onclick = navigateOrSelect clickMeta, local = False }
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just (Left
                                   { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}
                                   , click_origin_mbnodeId}) -> bpident_moduleName == inh.inh_trt.bpr_moduleName && bpident_taskName == inh.inh_trt.bpr_taskName && click_origin_mbnodeId == meid
                            _                                -> False
  #! valAnchor          = circle (px 12.0) <@< { onclick = openDetails clickMeta, local = False }
                                           <@< { fill = case stability of
                                                          TNoVal    -> TonicWhite
                                                          TStable   -> TonicBlue
                                                          TUnstable -> TonicGreen
                                               }
                                           <@< { stroke = if valNodeIsSelected TonicDarkBlue TonicBlack }
                                           <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! isSelected         = case meid of
                            Just eid -> 'DS'.member (inh.inh_trt.bpr_moduleName, inh.inh_trt.bpr_taskName, eid) inh.inh_selected
                            _        -> False
  #! inclArr            = beside (repeat AtMiddleY) [] (if isSelected [taskApp, valAnchor] [taskApp]) Nothing
  = ({syn_img = inclArr, syn_status = if isActive IsActive (if (isJust mPrevActiveTid) AllDone NotActive)}, tsrc)
  where
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
      <@< { fill        = TonicWhite }
      <@< { stroke      = TonicBlack }
      <@< { strokewidth = px 1.0 }
      <@< { xradius     = px 5.0 }
      <@< { yradius     = px 5.0 }

tDefaultMApp :: !Bool !Bool !Bool !Bool !(Set (!ModuleName, !TaskName, !ExprId))
                !(Maybe ExprId) !ModuleName !TaskName !ModuleName !TaskName ![TExpr]
                ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)
tDefaultMApp isCompact isActive wasActive isInAccessible selectedNodes mNodeId parentModName parentTaskName modName taskName argsExprs taskArgs tsrc
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
  = tDefaultMApp` isCompact isActive wasActive isInAccessible mNodeId selectedNodes parentModName parentTaskName modName taskName taskArgs tsrc

appColor :: !Bool !Bool !Bool -> SVGColor
appColor isActive wasActive isInAccessible
  = if isActive
      TonicGreen
      (if wasActive
          TonicBlue
          (if isInAccessible
              TonicGrey
              TonicWhite
          )
      )

tDefaultMApp` :: !Bool !Bool !Bool !Bool !(Maybe ExprId) !(Set (!ModuleName, !TaskName, !ExprId)) !ModuleName !TaskName !ModuleName !TaskName ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultMApp` isCompact isActive wasActive isInAccessible mNodeId selectedNodes parentModName parentTaskName modName taskName taskArgs [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
  #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
  #! bgColor     = appColor isActive wasActive isInAccessible
  #! isSelected  = case mNodeId of
                     Just nodeId -> 'DS'.member (parentModName, parentTaskName, nodeId) selectedNodes
                     _           -> False
  = case taskArgs of
      []
        #! bgRect = tRoundedRect (imagexspan tntag) (imageyspan tntag) <@< { fill = bgColor }
                                                                       <@< { stroke = if isSelected TonicDarkBlue TonicBlack }
                                                                       <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskNameImg] Nothing, tsrc)
      taskArgs
        #! argsImg  = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (margin (px 1.0, px 0.0)) taskArgs) Nothing))
        #! maxXSpan = maxSpan [imagexspan tntag, imagexspan argstag]
        #! content  = above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, argsImg] Nothing
        #! bgRect   = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = bgColor }
                                                                                    <@< { stroke = if isSelected TonicDarkBlue TonicBlack }
                                                                                    <@< { strokewidth = if isSelected (px 3.0) (px 1.0) }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tAssign :: !InhMkImg !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tAssign inh lhsExpr assignedTask [(assignTaskTag, uAssignTaskTag) : (headerTag, uHeaderTag) : tsrc]
  #! (desc, user)         = case lhsExpr of
                              (TFApp "_Tuple2" [usr, str : _] _) -> (ppTExpr str, mkUser usr)
                              usr                                -> ("", mkUser usr)
  #! (assignedTask, tsrc) = tExpr2Image inh assignedTask tsrc
  #! assignedTaskImg      = tag uAssignTaskTag (margin (px 5.0) assignedTask.syn_img)
  #! maxXSpan             = maxSpan [imagexspan headerTag, imagexspan assignTaskTag]
  #! taskNameImg          = margin (px 5.0) (text ArialBold10px (user +++ if (desc == "") "" (": " +++ desc)))
  #! assignHeader         = tag uHeaderTag (beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing)
  #! content              = above (repeat AtMiddleX) [] [assignHeader, xline Nothing maxXSpan, assignedTaskImg] Nothing
  #! bgRect               = rect maxXSpan (imageyspan headerTag + imageyspan assignTaskTag)
                              <@< { fill        = TonicWhite }
                              <@< { stroke      = TonicBlack }
                              <@< { strokewidth = px 1.0 }
                              <@< { xradius     = px 5.0 }
                              <@< { yradius     = px 5.0 }
                              <@< { dash        = [5, 5] }
  #! img                  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing
  = ({syn_img = img, syn_status = assignedTask.syn_status}, tsrc)
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

tStep :: !InhMkImg !(Maybe ExprId) !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStep inh eid lhsExpr conts [(contextTag, _) : tsrc]
  #! actions              = case inh.inh_trt.bpr_instance of
                              Just bpinst -> case 'DM'.get bpinst.bpi_taskId inh.inh_stepActions of
                                               Just xs -> xs
                                               _       -> []
                              _           -> []
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! conts                = tSafeExpr2List conts
  #! (syn_branches, tsrc) = tBranches inh (tStepCont actions) False True (map (\t -> (Nothing, t, True)) conts) contextTag tsrc
  #! img                  = beside (repeat AtMiddleY) [] [lhs.syn_img, tHorizConn syn_branches.syn_status, syn_branches.syn_img] Nothing
  = ( {syn_img = img, syn_status = lhs.syn_status}
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

tStepCont :: ![UIAction] !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStepCont actions inh (TFApp "OnAction" [TFApp "Action" [TLit actionLit : _] _ : cont : _ ] _) tsrc
  = mkStepCont inh (Just (actionLit, foldr f False actions)) cont tsrc
  where
  f {UIAction | action = Action an _, enabled} acc = (replaceSubString "\"" "" an == replaceSubString "\"" "" actionLit && enabled) || acc
  f _ acc = acc
tStepCont _ inh (TFApp "OnValue"  [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp "OnException" [cont : _ ] _)     tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp "OnAllExceptions" [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc

mkStepCont :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
mkStepCont inh mact (TFApp "always" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TFApp "never" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TFApp "withoutValue" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TFApp "ifStable" e _) tsrc
  = stepIfStableUnstableHasValue inh mact tStable e tsrc
mkStepCont inh mact (TFApp "ifUnstable" e _) tsrc
  = stepIfStableUnstableHasValue inh mact tUnstable e tsrc
mkStepCont inh mact (TFApp "hasValue" e _) [ref : tsrc]
  = stepIfStableUnstableHasValue inh mact hasValueFilter e tsrc
mkStepCont inh mact (TFApp "ifValue" [conditionApp : continuationApp : _] _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TFApp "ifCond" [conditionApp : continuationApp : _] _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TFApp "withValue" [mapp : _] _) tsrc
  = stepWithValue inh mact hasValueFilter mapp tsrc
mkStepCont inh mact (TFApp "withStable" [mapp : _] _) tsrc
  = stepWithValue inh mact tStable mapp tsrc
mkStepCont inh mact (TFApp "withUnstable" [mapp : _] _) tsrc
  = stepWithValue inh mact tUnstable mapp tsrc
mkStepCont inh mact e [ref : tsrc]
  #! (x, tsrc)            = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh tException tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, x.syn_img] Nothing 
  = ( {syn_img = img, syn_status = x.syn_status}
    , tsrc)

stepAlwaysNeverWithoutVal :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !*TagSource
                          -> *(!SynMkImg, !*TagSource)
stepAlwaysNeverWithoutVal inh mact mapp [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  #! img       = beside (repeat AtMiddleY) [] [addAction mact (tHorizConnArr x.syn_status) ref, x.syn_img] Nothing
  = ( {syn_img = img, syn_status = x.syn_status}
    , tsrc)

stepIfValueCond :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !TExpr !*TagSource
                -> *(!SynMkImg, !*TagSource)
stepIfValueCond inh mact conditionApp continuationApp [ref : tsrc]
  #! (exprImg, tsrc)         = tExpr2Image {inh & inh_in_case = True} conditionApp tsrc
  #! (conditionImg, tsrc)    = tCaseDiamond inh exprImg.syn_img tsrc
  #! (continuationImg, tsrc) = tExpr2Image inh continuationApp tsrc
  #! img                     = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr exprImg.syn_status, addAction mact (tShortHorizConn exprImg.syn_status) ref, continuationImg.syn_img] Nothing
  = ( {syn_img = img, syn_status = continuationImg.syn_status}
    , tsrc)

stepWithValue :: !InhMkImg !(Maybe (!String, !Bool)) !(Image ModelTy) !TExpr !*TagSource
              -> *(!SynMkImg, !*TagSource)
stepWithValue inh mact filter mapp [ref : tsrc]
  #! (x, tsrc)            = tExpr2Image inh mapp tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr x.syn_status, addAction mact (tHorizConnArr x.syn_status) ref, x.syn_img] Nothing
  = ( {syn_img = img, syn_status = x.syn_status}
    , tsrc)

stepIfStableUnstableHasValue :: !InhMkImg !(Maybe (!String, !Bool))
                                !(Image ModelTy) ![TExpr] !*TagSource
                             -> *(!SynMkImg, !*TagSource)
stepIfStableUnstableHasValue inh mact filter [TLam pats e : _] [ref : tsrc]
  #! (syn_e, tsrc)        = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr syn_e.syn_status, addAction mact (tHorizConn syn_e.syn_status) ref, tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" pats), tHorizConnArr syn_e.syn_status, syn_e.syn_img] Nothing
  = ( {syn_img = img, syn_status = syn_e.syn_status}
    , tsrc)
stepIfStableUnstableHasValue inh mact filter [e : _] [ref : tsrc]
  #! (syn_e, tsrc)        = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr syn_e.syn_status, addAction mact (tHorizConnArr syn_e.syn_status) ref, syn_e.syn_img] Nothing
  = ( {syn_img = img, syn_status = syn_e.syn_status}
    , tsrc)

addAction :: !(Maybe (!String, !Bool)) !(Image ModelTy) !*TagRef -> Image ModelTy
addAction (Just (action, enabled)) arr (t, uT)
  #! l = tag uT (margin (px 3.0) (beside (repeat AtMiddleY) [] [littleman, tuneIf (not enabled) (text ArialBold10px (" " +++ action)) {fill = toSVGColor "#666"}] Nothing))
  #! l` = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan t + px 5.0) (imageyspan t + px 5.0) <@< {fill        = toSVGColor (if enabled "#ebebeb" "#fff")}
                                                                                                            <@< {strokewidth = px 1.0}
                                                                                                            <@< {stroke      = toSVGColor (if enabled "#000" "#ccc")}
                                                                                                            <@< {dash        = if enabled [] [5, 5] }
                                                     , l] Nothing
  = beside (repeat AtMiddleY) [] [l`, arr] Nothing
addAction _ _ _ = empty (px 0.0) (px 0.0)

hasValueFilter :: Image ModelTy
hasValueFilter = beside (repeat AtMiddleY) [] [ rect (px 16.0) (px 8.0) <@< { fill = TonicBlue }
                                              , rect (px 16.0) (px 8.0) <@< { fill = TonicGreen }
                                              , text ArialBold10px " Has value"] Nothing

tBranches :: !InhMkImg !(InhMkImg TExpr *TagSource -> *(!SynMkImg, !*TagSource))
             !Bool !Bool ![(!Maybe Pattern, !TExpr, !Bool)] !ImageTag !*TagSource
          -> *(!SynMkImg, !*TagSource)
tBranches inh mkBranch needAllActive inclVertConns exprs contextTag tsrc
  #! (allTags, nonUTags, tsrc) = takeNTags (length exprs) tsrc
  #! maxXSpan                  = maxSpan (map imagexspan [contextTag : nonUTags])
  #! allBranchActivity         = map (activityStatus needAllActive inh o (\(_, x, _) -> x)) exprs
  #! existsSomeActivity        = let f AllDone  _   = True
                                     f IsActive _   = True
                                     f _        acc = acc
                                 in foldr f False allBranchActivity
  #! (syns, tsrc)              = foldr (iter existsSomeActivity maxXSpan) ([], tsrc) (zip3 exprs allBranchActivity allTags)
  #! branchImg                 = above (repeat AtLeft) [] (map (\x -> x.syn_img) syns) Nothing
  #! status                    = determineSynStatus needAllActive syns
  = case inclVertConns of
      True
        #! vertConn = mkVertConn nonUTags
        = ( { syn_img    = beside (repeat AtMiddleY) [] [vertConn, branchImg, vertConn] Nothing
            , syn_status = status }
          , tsrc)
      _ = ({ syn_img    = branchImg
           , syn_status = status}, tsrc)
  where
  iter :: !Bool !Span !(!(!Maybe Pattern, !TExpr, !Bool), !TStatus, !*TagRef)
          !*(![SynMkImg], !*TagSource)
       -> *(![SynMkImg], !*TagSource)
  iter existsSomeActivity maxXSpan ((pat, texpr, showRhs), currBranchActivity, (imgTag, uImgTag)) (syns, tsrc)
    #! (syn, tsrc) = mkBranch {inh & inh_inaccessible = existsSomeActivity && currBranchActivity == NotActive} texpr tsrc
    #! lhsLineAct  = case (currBranchActivity, syn.syn_status) of
                       (NotActive, NotActive) -> NotActive
                       _                      -> AllDone
    #! lhs         = case pat of
                       Nothing
                         = beside (repeat AtMiddleY) [] [tHorizConnArr lhsLineAct, syn.syn_img] Nothing
                       Just pat
                         #! textBox = tTextWithGreyBackground ArialRegular10px (ppTExpr pat)
                         = beside (repeat AtMiddleY) [] [tHorizConn lhsLineAct, textBox, tHorizConnArr lhsLineAct, syn.syn_img] Nothing
    #! lhs         = tag uImgTag (margin (px 2.5, px 0.0) lhs)
    #! lineWidth   = (maxXSpan - imagexspan imgTag) + px 8.0
    #! img = case showRhs of
               True
                 #! rhs = case syn.syn_status of
                            AllDone  -> rect lineWidth (px 3.0) <@< { fill = TonicBlue }
                            _        -> xline Nothing lineWidth
                 = beside (repeat AtMiddleY) [] [lhs, rhs] Nothing
               _ = lhs
    = ([{syn_img = img, syn_status = currBranchActivity} : syns], tsrc)

  takeNTags :: !Int !*TagSource -> *(!*[*TagRef], ![ImageTag], !*TagSource)
  takeNTags n [tr=:(_, nonUTag):tsrc]
    | n < 1 = ([], [], tsrc)
    | otherwise
        #! (allTags, nonUTags, tsrc) = takeNTags (n - 1) tsrc
        = ([tr : allTags], [nonUTag : nonUTags], tsrc)

  mkVertConn :: ![ImageTag] -> Image ModelTy
  mkVertConn ts
    | length ts < 2 = empty (px 0.0) (px 0.0)
    | otherwise
        #! firstTag   = hd ts
        #! lastTag    = last ts
        #! allYSpans  = foldr (\x acc -> imageyspan x + acc) (px 0.0) ts
        #! halfFirstY = imageyspan firstTag /. 2.0
        #! halfLastY  = imageyspan lastTag /. 2.0
        = above (repeat AtMiddleX) []
            [ yline Nothing halfFirstY <@< { stroke = TonicWhite }
            , yline Nothing (allYSpans - halfFirstY - halfLastY) <@< { stroke = TonicBlack }
            , yline Nothing halfLastY <@< { stroke = TonicWhite } ]
            Nothing

tTextWithGreyBackground :: !FontDef !String -> Image ModelTy
tTextWithGreyBackground font txt
  #! textWidth = textxspan font txt + px 10.0
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (font.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text font txt] Nothing

littleman :: Image a
littleman
  #! maskRect = rect (px 16.0) (px 16.0) <@< {FillAttr | fill = TonicWhite}
                                         <@< {StrokeWidthAttr | strokewidth = px 0.0}
  = (overlay [] [(px -2.0, px 8.0), (px 3.0, px 1.0)] [ circle (px 20.0) <@< {StrokeWidthAttr | strokewidth = px 1.0} <@< {StrokeAttr | stroke = TonicWhite}
                                                      , circle (px 10.0) <@< {StrokeWidthAttr | strokewidth = px 1.0} <@< {StrokeAttr | stroke = TonicWhite}] (Just maskRect)) <@< {MaskAttr | mask = maskRect}

tException :: Image ModelTy
tException = beside (repeat AtMiddleY) [] [ rect (px 16.0) (px 8.0) <@< { fill = TonicRed }
                                          , text ArialBold10px " Exception"] Nothing

tStable :: Image ModelTy
tStable = beside (repeat AtMiddleY) [] [ rect (px 16.0) (px 8.0) <@< { fill = TonicBlue }
                                       , text ArialBold10px " Stable"] Nothing

tUnstable :: Image ModelTy
tUnstable = beside (repeat AtMiddleY) [] [ rect (px 16.0) (px 8.0) <@< { fill = TonicGreen }
                                         , text ArialBold10px " Unstable"] Nothing
