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
  { inh_trt           :: !BlueprintRef
  , inh_task_apps     :: ![TaskAppRenderer]
  , inh_compact       :: !Bool
  , inh_prev          :: !Map ExprId TaskId
  , inh_inaccessible  :: !Bool
  , inh_in_maybe      :: !Bool
  , inh_in_step       :: !Bool
  , inh_in_mapp       :: !Bool
  , inh_in_fapp       :: !Bool
  , inh_in_case       :: !Bool
  , inh_outputs       :: !Map ExprId TStability
  , inh_selDetail     :: !Maybe (Either ClickMeta (!ModuleName, !FuncName, !TaskId, !Int))
  , inh_stepActions   :: !Map TaskId [UIAction]
  }

:: SynMkImg =
  { syn_img       :: !Image ModelTy
  , syn_status    :: !TStatus
  , syn_stability :: !TStability
  }

:: TStatus = TAllDone | TIsActive | TNotActive

instance == TStatus where
  (==) TAllDone   TAllDone   = True
  (==) TIsActive  TIsActive  = True
  (==) TNotActive TNotActive = True
  (==) _          _          = False

mkTaskImage :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef
               !(Map ExprId TStability) !(Map TaskId [UIAction])
               !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !TaskId, !Int))) !Bool
               !ModelTy *TagSource -> Image ModelTy
mkTaskImage rs prev trt outputs stepActions selDetail compact {ActionState | state = tis} tsrc
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
                        , inh_outputs      = outputs
                        , inh_selDetail    = selDetail
                        , inh_stepActions  = stepActions
                        }
  #! (tf_body`, tsrc) = tExpr2Image inh tt.tf_body tsrc
  #! (img, _)         = tTaskDef inh trt tt.tf_module tt.tf_name tt.tf_resty tt.tf_args [] tf_body`.syn_img tsrc
  = img

tExpr2Image :: !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tExpr2Image inh (TMApp eid mty mn tn targs prio) tsrc = tMApp     inh eid mty mn tn targs prio tsrc
tExpr2Image inh (TFApp fn targs assoc)           tsrc = tFApp     inh fn targs assoc tsrc
tExpr2Image inh (TLet pats bdy)                  tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TIf eid c t e)                  tsrc = tIf       inh eid c t e tsrc
tExpr2Image inh (TCase eid e pats)               tsrc = tCase     inh eid e pats tsrc
tExpr2Image inh (TVar eid pp _)                  tsrc = tVar      inh eid pp tsrc
tExpr2Image inh (TLit pp)                        tsrc = tLit      inh pp tsrc
tExpr2Image inh (TPPExpr pp)                     tsrc = tPPExpr   inh pp tsrc
tExpr2Image inh (TExpand args tt)                tsrc = tExpand   inh args tt tsrc
tExpr2Image inh (TSel e es)                      tsrc = tSel      inh e es tsrc
tExpr2Image inh (TRecUpd vn e es)                tsrc = tRecUpd   inh vn e es tsrc
tExpr2Image inh (TLam args e)                    tsrc = tLam      inh args e tsrc
tExpr2Image inh (TAugment orig extra)            tsrc = tAugment  inh orig extra tsrc

tAugment :: !InhMkImg !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tAugment inh orig extra tsrc
  #! (orig`, tsrc)  = tExpr2Image inh orig tsrc
  #! (extra`, tsrc) = tExpr2Image inh extra tsrc
  = ( {orig` & syn_img = beside (repeat AtMiddleY) [] [orig`.syn_img, text ArialRegular10px " (", extra`.syn_img, text ArialRegular10px ")"] Nothing}
    , tsrc)

tLam :: !InhMkImg ![TExpr] !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tLam inh vars e tsrc
  #! (r, tsrc) = tExpr2Image inh e tsrc
  #! lineParts = case vars of
                   []   -> [tHorizConnArr (r.syn_status, r.syn_stability), r.syn_img]
                   vars -> [tHorizConn (r.syn_status, r.syn_stability), tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" vars), tHorizConnArr (r.syn_status, r.syn_stability), r.syn_img]
  #! img       = beside (repeat AtMiddleY) [] lineParts Nothing
  = ( { syn_img       = img
      , syn_status    = r.syn_status
      , syn_stability = r.syn_stability
      }
    , tsrc)

tSel :: !InhMkImg !TExpr ![TExpr] !*TagSource -> *(!SynMkImg, !*TagSource)
tSel inh e es tsrc
  = ( { syn_img       = text ArialRegular10px (ppTExpr e +++ "." +++ ppIntersperse ppTExpr "." es)
      , syn_status    = TNotActive
      , syn_stability = TStable
      }
    , tsrc)

tRecUpd :: !InhMkImg !VarName !TExpr ![TExpr] !*TagSource -> *(!SynMkImg, !*TagSource)
tRecUpd inh vn e es tsrc
  = ( { syn_img       = text ArialRegular10px ("{ " +++ vn % (1, size vn) +++ " | " +++ ppTExpr e +++ " & " +++ ppES es +++ "}")
      , syn_status    = TNotActive
      , syn_stability = TStable
      }
    , tsrc)
  where
  ppES []     = ""
  ppES [x]    = ppTExpr x
  ppES [TNoBind : xs] = ppES xs
  ppES [x : xs] = ppTExpr x +++ " " +++ ppES xs

tFApp :: !InhMkImg !FuncName ![TExpr] !TPriority !*TagSource -> *(!SynMkImg, !*TagSource)
tFApp inh fn args assoc tsrc
  = ( { syn_img       = text ArialRegular10px (ppTExpr (TFApp fn args assoc))
      , syn_status    = TNotActive
      , syn_stability = TStable
      }
    , tsrc)

tArrowTip :: !(!TStatus, !TStability) -> Image ModelTy
tArrowTip status
  #! tip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]
  = case status of
      (TNotActive, _) = tip <@< { fill   = TonicBlack }
                            <@< { stroke = TonicBlack }
      (_, TNoVal)     = tip <@< { fill   = TonicWhite }
                            <@< { stroke = TonicBlack }
      (_, TStable)    = tip <@< { fill   = TonicBlue }
                            <@< { stroke = TonicBlack }
      (_, TUnstable)  = tip <@< { fill   = TonicGreen }
                            <@< { stroke = TonicBlack }

tLineMarker :: !(!TStatus, !TStability) -> Maybe (Markers ModelTy)
tLineMarker status = Just {defaultMarkers & markerEnd = Just (tArrowTip status)}

tSmallHorizConn :: Image ModelTy
tSmallHorizConn = xline Nothing (px 4.0)

tHorizConn :: !(!TStatus, !TStability) -> Image ModelTy
tHorizConn (TNotActive, _) = xline Nothing (px 8.0)
tHorizConn (_, TNoVal)     = rect (px 8.0) (px 3.0) <@< { fill = TonicWhite }
tHorizConn (_, TStable)    = rect (px 8.0) (px 3.0) <@< { fill = TonicBlue }
tHorizConn (_, TUnstable)  = rect (px 8.0) (px 3.0) <@< { fill = TonicGreen }

tShortHorizConn :: !(!TStatus, !TStability) -> Image ModelTy
tShortHorizConn (TNotActive, _) = xline Nothing (px 4.0)
tShortHorizConn (_, TNoVal)     = rect (px 4.0) (px 3.0) <@< { fill = TonicWhite }
tShortHorizConn (_, TStable)    = rect (px 4.0) (px 3.0) <@< { fill = TonicBlue }
tShortHorizConn (_, TUnstable)  = rect (px 4.0) (px 3.0) <@< { fill = TonicGreen }

tHorizConnArr :: !(!TStatus, !TStability) -> Image ModelTy
tHorizConnArr status = beside (repeat AtMiddleY) [] [tHorizConn status, tArrowTip status] Nothing

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip (TNotActive, TNoVal)))}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just (tArrowTip (TNotActive, TNoVal))}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip (TNotActive, TNoVal))), markerEnd = Just (tArrowTip (TNotActive, TNoVal))}) (px 16.0)

tExpand :: !InhMkImg ![TExpr] !TonicFunc !*TagSource -> *(!SynMkImg, !*TagSource)
tExpand inh argnames tt tsrc
  #! (tf_body`, tsrc) = tExpr2Image inh tt.tf_body tsrc
  #! (td_img, tsrc)   = tTaskDef inh inh.inh_trt tt.tf_module tt.tf_name tt.tf_resty tt.tf_args argnames tf_body`.syn_img tsrc
  = ({ syn_img       = td_img
     , syn_status    = tf_body`.syn_status
     , syn_stability = tf_body`.syn_stability
     }
    , tsrc)

tPPExpr :: !InhMkImg !String !*TagSource -> *(!SynMkImg, !*TagSource)
tPPExpr inh pp tsrc
  | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case
      = ( { syn_img       = text ArialRegular10px pp
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing
      = ( { syn_img       = img
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)

tLit :: !InhMkImg !TLit !*TagSource -> *(!SynMkImg, !*TagSource)
tLit inh (TBool   x) tsrc = tPPExpr inh (toString x) tsrc
tLit inh (TInt    x) tsrc = tPPExpr inh (toString x) tsrc
tLit inh (TReal   x) tsrc = tPPExpr inh (toString x) tsrc
tLit inh (TString x) tsrc = tPPExpr inh x tsrc

instance toString (Maybe a) | toString a where
  toString (Just x) = "Just " +++ toString x
  toString _        = "Nothing"

tVar :: !InhMkImg !ExprId !String !*TagSource -> *(!SynMkImg, !*TagSource)
tVar inh eid pp tsrc
  | inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case
      = ( { syn_img       = text ArialRegular10px pp
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing
      = ( { syn_img       = img
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)

activityStatus :: !Bool !InhMkImg !TExpr -> TStatus
activityStatus needAllActive inh (TFApp _ args _)      = determineStatus needAllActive (map (activityStatus needAllActive inh) args)
activityStatus needAllActive inh (TMApp eid _ _ _ exprs _)
  | 'DM'.member eid inh.inh_prev = TAllDone
  | otherwise
      = case inh.inh_trt.bpr_instance of
          Just {bpi_activeNodes} | isJust (activeNodeTaskId eid bpi_activeNodes) = TIsActive
          _ = determineStatus needAllActive (map (activityStatus needAllActive inh) exprs)
activityStatus needAllActive inh (TLet pats bdy)       = activityStatus needAllActive inh bdy
activityStatus needAllActive inh (TIf _ c t e)         = determineStatus needAllActive (map (activityStatus needAllActive inh) [t, e])
activityStatus needAllActive inh (TCase _ e pats)      = determineStatus needAllActive (map (activityStatus needAllActive inh o snd) pats)
activityStatus needAllActive inh (TExpand args tt)     = activityStatus needAllActive inh tt.tf_body
activityStatus needAllActive inh (TLam _ e)            = activityStatus needAllActive inh e
activityStatus needAllActive inh _                     = TNotActive

determineStability :: ![TStability] -> TStability
determineStability []              = TNoVal
determineStability [TUnstable : _] = TUnstable
determineStability [TStable : _]   = TStable
determineStability [_ : xs]        = determineStability xs

determineSynStability :: ![SynMkImg] -> TStability
determineSynStability syns = determineStability (map (\x -> x.syn_stability) syns)

determineStatus :: !Bool ![TStatus] -> TStatus
determineStatus _ [TIsActive : _] = TIsActive
determineStatus needAllActive [TAllDone : xs]
  | needAllActive
      = case determineStatus needAllActive xs of
          TAllDone -> TAllDone
          x       -> x
  | otherwise = TAllDone
determineStatus needAllActive [_ : xs] = determineStatus needAllActive xs
determineStatus _ _        = TNotActive

determineSynStatus :: !Bool ![SynMkImg] -> TStatus
determineSynStatus needAllActive syns = determineStatus needAllActive (map (\x -> x.syn_status) syns)

tIf :: !InhMkImg !ExprId !TExpr !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tIf inh eid cexpr texpr eexpr [(contextTag, _) : tsrc]
  #! (cexpr, ut, ue)      = case inh.inh_trt of
                              {bpr_instance = Just bpi} -> case 'DM'.get eid bpi.bpi_case_branches of
                                                             Just 0  -> (TAugment cexpr (TLit (TBool True)), False, True)
                                                             Just -1 -> (TAugment cexpr (TLit (TBool False)), True, False)
                                                             _       -> (cexpr, False, False)
                              _      -> (cexpr, False, False)
  #! (exprImg, tsrc)      = tExpr2Image {inh & inh_in_case = True} cexpr tsrc
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image False True [ (Just (TLit (TBool True)), texpr, True, ut)
                                                                 , (Just (TLit (TBool False)), eexpr, True, ue)] contextTag tsrc
  #! (diamond, tsrc)      = tCaseDiamond inh exprImg.syn_img tsrc
  #! lineAct              = case syn_branches.syn_status of
                              TNotActive -> (TNotActive, TNoVal)
                              _          -> (TAllDone, syn_branches.syn_stability)
  #! img                  = beside (repeat AtMiddleY) [] [diamond, tHorizConn lineAct, syn_branches.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_branches.syn_status
      , syn_stability = syn_branches.syn_stability
      }
    , tsrc)

tCase :: !InhMkImg !ExprId !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!SynMkImg, !*TagSource)
tCase inh eid texpr pats [(contextTag, _) : tsrc]
  #! mbranch              = case inh.inh_trt of
                              {bpr_instance = Just bpi} -> 'DM'.get eid bpi.bpi_case_branches
                              _                         -> Nothing
  #! pats`                = map (\(n, (p, t)) -> (Just p, t, True, Just n == mbranch)) (zip2 [0..] pats)
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image False True pats` contextTag tsrc
  #! (exprImg, tsrc)      = tExpr2Image {inh & inh_in_case = True} texpr tsrc
  #! (diamond, tsrc)      = tCaseDiamond inh exprImg.syn_img tsrc
  #! lineAct              = case syn_branches.syn_status of
                              TNotActive -> (TNotActive, TNoVal)
                              _          -> (TAllDone, syn_branches.syn_stability)
  #! img                  = beside (repeat AtMiddleY) [] [diamond, tHorizConn lineAct, syn_branches.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_branches.syn_status
      , syn_stability = syn_branches.syn_stability
      }
    , tsrc)

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
        #! lineAct         = case t.syn_status of
                               TNotActive -> (TNotActive, TNoVal)
                               _          -> (TAllDone, t.syn_stability)
        #! (patRhss, tsrc) = mapSt (tExpr2Image inh) (map snd pats) tsrc
        #! binds     = foldr (\(var, expr) acc -> [text ArialRegular10px (ppTExpr var) : text ArialRegular10px " = " : expr.syn_img : acc]) [] (zip2 (map fst pats) patRhss)
        #! letText   = tag uTxtTag (grid (Columns 3) (RowMajor, LeftToRight, TopToBottom) [] [] binds Nothing)
        #! letWidth  = imagexspan txttag + px 10.0
        #! letHeight = imageyspan txttag + px 10.0
        #! letBox    = rect letWidth letHeight
                         <@< { fill   = TonicWhite }
                         <@< { stroke = TonicBlack }
        #! letImg    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
        #! linePart         = case t.syn_status of
                               TNotActive -> xline Nothing ((letWidth - px 8.0) /. 2.0)
                               _          -> rect ((letWidth - px 8.0) /. 2.0) (px 3.0) <@< { fill = TonicBlue }
        #! connBox   = beside (repeat AtMiddleY) [] [linePart, rect (px 8.0) (px 8.0), linePart] Nothing
        #! letImg    = above (repeat AtMiddleX) [] [letImg, yline Nothing (px 8.0), connBox, empty zero (letHeight + px 8.0)] Nothing
        #! img       = beside (repeat AtMiddleY) [] [letImg, tHorizConnArr lineAct, t.syn_img] Nothing
        = ( { syn_img       = img
            , syn_status    = t.syn_status
            , syn_stability = t.syn_stability
            }
          , tsrc)

tBind :: !InhMkImg !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! lineAct    = case r`.syn_status of
                    TNotActive -> (TNotActive, TNoVal)
                    _          -> (TAllDone, l`.syn_stability)
  #! linePart   = case mpat of
                    Just pat -> [l`.syn_img, tHorizConn lineAct, tTextWithGreyBackground ArialRegular10px (ppTExpr pat), tHorizConnArr lineAct, r`.syn_img]
                    _        -> [l`.syn_img, tHorizConnArr lineAct, r`.syn_img]
  #! img        = beside (repeat AtMiddleY) [] linePart Nothing
  #! newStat    = case (l`.syn_status, r`.syn_status) of
                    (TNotActive, TNotActive) -> TNotActive
                    (_,          TNotActive) -> TIsActive
                    (_,          x)          -> x
  = ( { syn_img       = img
      , syn_status    = newStat
      , syn_stability = l`.syn_stability
      }
    , tsrc)

tParSumL :: !InhMkImg !ExprId !String !String !TExpr !TExpr !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumL inh eid mn tn l r [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False [(Nothing, l, True, False), (Nothing, r, False, False)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (-||): left bias" syn_branches uContextTag tsrc
tParSumR :: !InhMkImg !ExprId !String !String !TExpr !TExpr !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumR inh eid mn tn l r [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False [(Nothing, l, False, False), (Nothing, r, True, False)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (||-): right bias" syn_branches uContextTag tsrc
tParSumN :: !InhMkImg !ExprId !String !String !String ![TExpr]
            !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False (map (\x -> (Nothing, x, True, False)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc
tParProdN :: !InhMkImg !ExprId !String !String !String ![TExpr]
             !*TagSource
          -> *(!SynMkImg, !*TagSource)
tParProdN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches inh tExpr2Image True False (map (\x -> (Nothing, x, True, False)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc

renderParallelContainer :: !InhMkImg !ExprId !ModuleName !FuncName !String
                           !SynMkImg !*ImageTag !*TagSource
                        -> *(!SynMkImg, !*TagSource)
renderParallelContainer inh eid moduleName taskName descr syn_branches uContextTag tsrc
  #! isDynamic          = isJust inh.inh_trt.bpr_instance
  #! mActiveTid         = case inh.inh_trt.bpr_instance of
                            Just bpinst -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _           -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = 'DM'.get eid inh.inh_prev
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! stability          = let f tid = fromMaybe TNoVal (maybe Nothing (\bpinst -> 'DM'.get eid inh.inh_outputs) inh.inh_trt.bpr_instance)
                          in maybe (maybe TNoVal f mPrevActiveTid) f mActiveTid
  #! mTaskId            = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> Just x
                            (_, Just x) -> Just x
                            _           -> Nothing
  #! taskIdStr          = maybe "" (\x -> " (" +++ toString x +++ ")") mTaskId
  #! displayName        = descr +++ taskIdStr
  #! (taskApp, tsrc)    = tParApp inh.inh_compact eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName displayName syn_branches tsrc
  #! clickMeta          = mkClickMeta inh (Just eid) moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_trt.bpr_instance) mbNavTo
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just (Left
                                   { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}
                                   , click_origin_mbnodeId}) -> bpident_moduleName == inh.inh_trt.bpr_moduleName && bpident_taskName == inh.inh_trt.bpr_taskName && click_origin_mbnodeId == Just eid
                            _                                -> False
  #! valAnchor          = circle (px 12.0) <@< { onclick = openDetails clickMeta, local = False }
                                           <@< { fill = case stability of
                                                          TNoVal    -> TonicWhite
                                                          TStable   -> TonicBlue
                                                          TUnstable -> TonicGreen
                                               }
                                           <@< { stroke = if valNodeIsSelected TonicDarkBlue TonicBlack }
                                           <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! inclArr            = beside (repeat AtMiddleY) [] (if isDynamic [taskApp, valAnchor] [taskApp]) Nothing
  = ( { syn_img       = inclArr
      , syn_status    = if isActive TIsActive (if (isJust mPrevActiveTid) TAllDone TNotActive)
      , syn_stability = stability
      }
    , tsrc)
  where
  tParApp :: !Bool !ExprId !ModuleName !FuncName !FuncName !SynMkImg !*TagSource
          -> *(!Image ModelTy, !*TagSource)
  tParApp isCompact eid parentModName parentFuncName taskName syn_branches [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
    #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
    #! taskNameImg = tag uContextTag taskNameImg
    #! maxXSpan    = maxSpan [imagexspan tntag, imagexspan argstag]
    #! content     = above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, tag uArgsTag syn_branches.syn_img] Nothing
    #! bgRect      = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = TonicWhite }
                                                                                   <@< { stroke = TonicBlack }
                                                                                   <@< { strokewidth = px 1.0 }
    #! img         = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing
    = (img, tsrc)

mkClickMeta :: !InhMkImg !(Maybe ExprId) !ModuleName !FuncName !(Maybe TaskId) !(Maybe TaskId) -> ClickMeta
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
  #! userImg      = case bpr of
                      {bpr_instance = Just {bpi_currentUser = Just cu}} -> beside (repeat AtMiddleY) [] [margin (px 0.0, px 0.0, px 0.0, px 8.0) littleman, text ArialRegular10px (" " +++ toString cu)] Nothing
                      _                                                 -> empty zero zero
  #! taskIdStr    = case bpr of
                      {bpr_instance = Just {bpi_taskId}} -> " (" +++ toString bpi_taskId +++ ")"
                      _                                  -> ""
  #! taskNameImg  = beside (repeat AtMiddleY) [] [ text ArialRegular10px (moduleName +++ ".")
                                                 , text ArialBold10px (taskName +++ " :: " +++ ppTExpr resultTy)
                                                 , text ArialRegular10px taskIdStr
                                                 , userImg] Nothing
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

tMApp :: !InhMkImg !ExprId !(Maybe TypeName) !ModuleName !VarName ![TExpr]
         !TPriority !*TagSource
      -> *(!SynMkImg, !*TagSource)
tMApp inh _ _ "iTasks.API.Extensions.User" "@:" [lhsExpr : rhsExpr : _] _ tsrc
  = tAssign inh lhsExpr rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Common.TaskCombinators" ">>|" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : TLam [var : _] rhsExpr : _] _ tsrc
  = tBind inh lhsExpr (Just var) rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : rhsExpr : _] _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Common.TaskCombinators" ">>*" [lhsExpr : rhsExpr : _] _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Core.TaskCombinators" "step" [lhsExpr : _ : rhsExpr : _] _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-&&-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParProdN inh eid mn tn "Parallel (-&&-): both tasks" [lhsExpr, rhsExpr] tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"allTasks" [x] assoc tsrc
  = tParProdN inh eid mn tn "Parallel allTasks" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"anyTask" [x] assoc tsrc
  = tParSumN inh eid mn tn "Parallel anyTask" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumN inh eid mn tn "Parallel (-||-): any task" [lhsExpr, rhsExpr] tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"||-" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumR inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||" [lhsExpr : rhsExpr : _] _ tsrc
  = tParSumL inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"@!" [lhsExpr : _] _ tsrc
  = tExpr2Image inh lhsExpr tsrc
tMApp inh eid _ modName taskName taskArgs _ tsrc
  #! inh = {inh & inh_in_mapp = True}
  = renderTaskApp inh eid modName taskName taskArgs taskName tsrc

renderTaskApp :: !InhMkImg !ExprId !String !String ![TExpr] !String !*TagSource
              -> *(!SynMkImg, !*TagSource)
renderTaskApp inh eid moduleName taskName taskArgs displayName tsrc
  #! (taskArgs`, tsrc)  = mapSt (tExpr2Image inh) taskArgs tsrc
  #! taskArgs`          = map (\x -> x.syn_img) taskArgs`
  #! isDynamic          = isJust inh.inh_trt.bpr_instance
  #! mActiveTid         = case inh.inh_trt.bpr_instance of
                            Just bpinst -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _           -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = 'DM'.get eid inh.inh_prev
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! wasActive          = isJust mPrevActiveTid
  #! stability          = let f tid = fromMaybe TNoVal (maybe Nothing (\bpinst -> 'DM'.get eid inh.inh_outputs) inh.inh_trt.bpr_instance)
                          in maybe (maybe TNoVal f mPrevActiveTid) f mActiveTid
  #! mTaskId            = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> Just x
                            (_, Just x) -> Just x
                            _           -> Nothing
  #! taskIdStr          = maybe "" (\x -> " (" +++ toString x +++ ")") mTaskId
  #! displayName        = displayName +++ taskIdStr
  #! (renderOpts, tsrc) = mapSt (\ta -> ta inh.inh_compact isActive wasActive inh.inh_inaccessible eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName moduleName displayName taskArgs`) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultMApp inh.inh_compact isActive wasActive inh.inh_inaccessible eid inh.inh_trt.bpr_moduleName inh.inh_trt.bpr_taskName moduleName displayName taskArgs taskArgs` tsrc
  #! clickMeta          = mkClickMeta inh (Just eid) moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_trt.bpr_instance) mbNavTo
  #! taskApp            = taskApp <@< { onclick = navigateOrSelect clickMeta, local = False }
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just (Left
                                   { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}
                                   , click_origin_mbnodeId}) -> bpident_moduleName == inh.inh_trt.bpr_moduleName && bpident_taskName == inh.inh_trt.bpr_taskName && click_origin_mbnodeId == Just eid
                            _                                -> False
  #! valAnchor          = circle (px 12.0) <@< { onclick = openDetails clickMeta, local = False }
                                           <@< { fill = case stability of
                                                          TNoVal    -> TonicWhite
                                                          TStable   -> TonicBlue
                                                          TUnstable -> TonicGreen
                                               }
                                           <@< { stroke = if valNodeIsSelected TonicDarkBlue TonicBlack }
                                           <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! inclArr            = beside (repeat AtMiddleY) [] (if isDynamic [taskApp, valAnchor] [taskApp]) Nothing
  = ( { syn_img       = inclArr
      , syn_status    = if isActive TIsActive (if (isJust mPrevActiveTid) TAllDone TNotActive)
      , syn_stability = stability
      }
    , tsrc)
  where
  navigateOrSelect :: !ClickMeta !Int !ModelTy -> ModelTy
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

tDefaultMApp :: !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                !ModuleName !FuncName ![TExpr] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)
tDefaultMApp isCompact isActive wasActive isInAccessible eid parentModName parentFuncName modName taskName argsExprs taskArgs tsrc
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
                  (True, True, [TVar _ tn _ : _]) -> if (size tn > 0 && tn.[0] == '"') [text ArialRegular10px tn] []
                  (True, _, _) -> []
                  _            -> taskArgs
  = tDefaultMApp` isCompact isActive wasActive isInAccessible eid parentModName parentFuncName modName taskName taskArgs tsrc

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

tDefaultMApp` :: !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                 !ModuleName !FuncName ![Image ModelTy] !*TagSource
              -> *(!Image ModelTy, !*TagSource)
tDefaultMApp` isCompact isActive wasActive isInAccessible eid parentModName parentFuncName modName taskName taskArgs [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
  #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
  #! bgColor     = appColor isActive wasActive isInAccessible
  = case taskArgs of
      []
        #! bgRect = tRoundedRect (imagexspan tntag) (imageyspan tntag) <@< { fill = bgColor }
                                                                       <@< { stroke = TonicBlack }
                                                                       <@< { strokewidth = px 1.0 }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskNameImg] Nothing, tsrc)
      taskArgs
        #! argsImg  = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (margin (px 1.0, px 0.0)) taskArgs) Nothing))
        #! maxXSpan = maxSpan [imagexspan tntag, imagexspan argstag]
        #! content  = above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, argsImg] Nothing
        #! bgRect   = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = bgColor }
                                                                                    <@< { stroke = TonicBlack }
                                                                                    <@< { strokewidth = px 1.0 }
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
  = ( { syn_img       = img
      , syn_status    = assignedTask.syn_status
      , syn_stability = assignedTask.syn_stability
      }
    , tsrc)
  where
  mkUser (TFApp "AnyUser" _ _)          = "Any user"
  mkUser (TFApp "UserWithId" [uid:_] _) = ppTExpr uid
  mkUser (TFApp "UserWithRole" [r:_] _) = "Anyone with role " +++ ppTExpr r
  mkUser (TFApp "SystemUser" _ _)       = "System user"
  mkUser (TFApp "AnonymousUser" _ _)    = "Anonymous user"
  mkUser (TFApp "AuthenticatedUser" [uid:rs:_] _) = ppTExpr uid +++ " with roles " +++ foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" (tSafeExpr2List rs)
  mkUser (TFApp usr _ _)                = usr
  mkUser (TVar _ ppe _)                 = ppe
  mkUser (TLit (TString ppe))           = ppe
  mkUser (TPPExpr ppe)                  = ppe
  mkUser _                              = ""

tStep :: !InhMkImg !ExprId !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStep inh eid lhsExpr conts [(contextTag, _) : tsrc]
  #! actions              = case inh.inh_trt.bpr_instance of
                              Just bpinst -> case 'DM'.get bpinst.bpi_taskId inh.inh_stepActions of
                                               Just xs -> xs
                                               _       -> []
                              _           -> []
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! conts                = tSafeExpr2List conts
  #! (syn_branches, tsrc) = tBranches inh (tStepCont actions) False True (map (\t -> (Nothing, t, True, False)) conts) contextTag tsrc
  #! img                  = beside (repeat AtMiddleY) [] [lhs.syn_img, tHorizConn (lineStatus syn_branches), syn_branches.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_branches.syn_status
      , syn_stability = syn_branches.syn_stability
      }
    , tsrc)


lineStatus :: !SynMkImg -> (!TStatus, !TStability)
lineStatus {syn_status = TNotActive} = (TNotActive, TNoVal)
lineStatus {syn_stability}           = (TAllDone, syn_stability)

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

derive class iTask UIAction

tStepCont :: ![UIAction] !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStepCont actions inh (TFApp "OnAction" [TFApp "Action" [actionLit : _] _ : cont : _ ] _) tsrc
  = mkStepCont inh (Just (ppTExpr actionLit, foldr f False actions)) cont tsrc
  where
  f {UIAction | action = Action an _, enabled} acc = (replaceSubString "\"" "" an == replaceSubString "\"" "" (ppTExpr actionLit) && enabled) || acc
  f _ acc = acc
tStepCont _ inh (TFApp "OnValue"  [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp "OnException" [cont : _ ] _)     tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp "OnAllExceptions" [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc

mkStepCont :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "always" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "never" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withoutValue" [mapp : _] _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifStable" e _) tsrc
  = stepIfStableUnstableHasValue inh mact tStable e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifUnstable" e _) tsrc
  = stepIfStableUnstableHasValue inh mact tUnstable e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "hasValue" e _) [ref : tsrc]
  = stepIfStableUnstableHasValue inh mact hasValueFilter e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifValue" [conditionApp : continuationApp : _] _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifCond" [conditionApp : continuationApp : _] _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withValue" [mapp : _] _) tsrc
  = stepWithValue inh mact hasValueFilter mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withStable" [mapp : _] _) tsrc
  = stepWithValue inh mact tStable mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withUnstable" [mapp : _] _) tsrc
  = stepWithValue inh mact tUnstable mapp tsrc
mkStepCont inh mact e [ref : tsrc]
  #! (x, tsrc)            = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh tException tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, x.syn_img] Nothing 
  = ( { syn_img       = img
      , syn_status    = x.syn_status
      , syn_stability = x.syn_stability
      }
    , tsrc)

stepAlwaysNeverWithoutVal :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !*TagSource
                          -> *(!SynMkImg, !*TagSource)
stepAlwaysNeverWithoutVal inh mact mapp [ref : tsrc]
  #! (x, tsrc) = tExpr2Image inh mapp tsrc
  #! img       = beside (repeat AtMiddleY) [] [addAction mact (tHorizConnArr (lineStatus x)) ref, x.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = x.syn_status
      , syn_stability = x.syn_stability
      }
    , tsrc)

stepIfValueCond :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !TExpr !*TagSource
                -> *(!SynMkImg, !*TagSource)
stepIfValueCond inh mact conditionApp continuationApp [ref : tsrc]
  #! (exprImg, tsrc)         = tExpr2Image {inh & inh_in_case = True} conditionApp tsrc
  #! (conditionImg, tsrc)    = tCaseDiamond inh exprImg.syn_img tsrc
  #! (continuationImg, tsrc) = tExpr2Image inh continuationApp tsrc
  #! img                     = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (lineStatus exprImg), addAction mact (tShortHorizConn (lineStatus exprImg)) ref, continuationImg.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = continuationImg.syn_status
      , syn_stability = continuationImg.syn_stability
      }
    , tsrc)

stepWithValue :: !InhMkImg !(Maybe (!String, !Bool)) !(Image ModelTy) !TExpr !*TagSource
              -> *(!SynMkImg, !*TagSource)
stepWithValue inh mact filter mapp [ref : tsrc]
  #! (x, tsrc)            = tExpr2Image inh mapp tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (lineStatus x), addAction mact (tHorizConnArr (lineStatus x)) ref, x.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = x.syn_status
      , syn_stability = x.syn_stability
      }
    , tsrc)

stepIfStableUnstableHasValue :: !InhMkImg !(Maybe (!String, !Bool))
                                !(Image ModelTy) ![TExpr] !*TagSource
                             -> *(!SynMkImg, !*TagSource)
stepIfStableUnstableHasValue inh mact filter [TLam pats e : _] [ref : tsrc]
  #! (syn_e, tsrc)        = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (lineStatus syn_e), addAction mact (tHorizConn (lineStatus syn_e)) ref, tTextWithGreyBackground ArialRegular10px (foldr (\x xs -> ppTExpr x +++ " " +++ xs) "" pats), tHorizConnArr (lineStatus syn_e), syn_e.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_e.syn_status
      , syn_stability = syn_e.syn_stability
      }
    , tsrc)
stepIfStableUnstableHasValue inh mact filter [e : _] [ref : tsrc]
  #! (syn_e, tsrc)        = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (lineStatus syn_e), addAction mact (tHorizConnArr (lineStatus syn_e)) ref, syn_e.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_e.syn_status
      , syn_stability = syn_e.syn_stability
      }
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
             !Bool !Bool ![(!Maybe Pattern, !TExpr, !Bool, !Bool)] !ImageTag !*TagSource
          -> *(!SynMkImg, !*TagSource)
tBranches inh mkBranch needAllActive inclVertConns exprs contextTag tsrc
  #! (allTags, nonUTags, tsrc) = takeNTags (length exprs) tsrc
  #! maxXSpan                  = maxSpan (map imagexspan [contextTag : nonUTags])
  #! allBranchActivity         = map (activityStatus needAllActive inh o (\(_, x, _, _) -> x)) exprs
  #! existsSomeActivity        = let f TAllDone  _   = True
                                     f TIsActive _   = True
                                     f _        acc = acc
                                 in foldr f False allBranchActivity
  #! (syns, tsrc)              = foldr (iter existsSomeActivity maxXSpan) ([], tsrc) (zip3 exprs allBranchActivity allTags)
  #! branchImg                 = above (repeat AtLeft) [] (map (\x -> x.syn_img) syns) Nothing
  #! status                    = determineSynStatus needAllActive syns
  = case inclVertConns of
      True
        #! vertConn = mkVertConn nonUTags
        = ( { syn_img       = beside (repeat AtMiddleY) [] [vertConn, branchImg, vertConn] Nothing
            , syn_status    = status
            , syn_stability = determineSynStability syns
            }
          , tsrc)
      _ = ( { syn_img       = branchImg
            , syn_status    = status
            , syn_stability = determineSynStability syns
            }
          , tsrc)
  where
  iter :: !Bool !Span !(!(!Maybe Pattern, !TExpr, !Bool, !Bool), !TStatus, !*TagRef)
          !*(![SynMkImg], !*TagSource)
       -> *(![SynMkImg], !*TagSource)
  iter existsSomeActivity maxXSpan ((pat, texpr, showRhs, unreachable), currBranchActivity, (imgTag, uImgTag)) (syns, tsrc)
    #! (syn, tsrc) = mkBranch {inh & inh_inaccessible = unreachable || (existsSomeActivity && currBranchActivity == TNotActive)} texpr tsrc
    #! lhsLineAct  = case (currBranchActivity, syn.syn_status) of
                       (TNotActive, TNotActive) -> (TNotActive, TNoVal)
                       _                        -> (TAllDone, syn.syn_stability)
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
                            TAllDone  -> rect lineWidth (px 3.0) <@< { fill = TonicBlue }
                            _        -> xline Nothing lineWidth
                 = beside (repeat AtMiddleY) [] [lhs, rhs] Nothing
               _ = lhs
    = ([{ syn_img = img
        , syn_status = currBranchActivity
        , syn_stability = syn.syn_stability
        } : syns], tsrc)

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
