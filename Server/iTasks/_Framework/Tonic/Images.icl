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
import iTasks.UI.Definition
import iTasks.API.Core.Types
import iTasks.API.Extensions.SVG.SVGlet
import Text
import StdMisc

TonicBlue     =: toSVGColor "#00bfff" // "DeepSkyBlue"
TonicDarkBlue =: toSVGColor "#000080" // "navy"
TonicGreen    =: toSVGColor "#32cd32" // "LimeGreen"
TonicWhite    =: toSVGColor "#ffffff" // "white"
TonicBlack    =: toSVGColor "#000000" // "black"
TonicRed      =: toSVGColor "#ff4500" // "OrangeRed"
TonicGrey     =: toSVGColor "#dcdcdc" // "Gainsboro"

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintRef

ArialRegular10px :== { fontfamily  = "Arial"
                     , fontysize   = 10.0
                     , fontstretch = "normal"
                     , fontstyle   = "normal"
                     , fontvariant = "normal"
                     , fontweight  = "normal"
                     }

ArialBold6px :== { fontfamily  = "Arial"
                 , fontysize   = 6.0
                 , fontstretch = "normal"
                 , fontstyle   = "normal"
                 , fontvariant = "normal"
                 , fontweight  = "bold"
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
  { inh_bpinst             :: !Maybe BlueprintInstance
  , inh_bpref              :: !BlueprintIdent
  , inh_task_apps          :: ![TaskAppRenderer]
  , inh_compact            :: !Bool
  , inh_prev               :: !Map ExprId TaskId
  , inh_inaccessible       :: !Bool
  , inh_future_unreachable :: !Bool
  , inh_in_maybe           :: !Bool
  , inh_in_step            :: !Bool
  , inh_in_mapp            :: !Bool
  , inh_in_fapp            :: !Bool
  , inh_in_case            :: !Bool
  , inh_in_branch          :: !Bool
  , inh_in_lam             :: !Bool
  , inh_in_parallel        :: !Bool
  , inh_in_let             :: !Bool
  , inh_outputs            :: !Map ExprId TStability
  , inh_selDetail          :: !Maybe (Either ClickMeta (!ModuleName, !FuncName, !TaskId, !Int))
  , inh_stepActions        :: !Map ExprId [UIAction]
  , inh_prev_statstab      :: !(!TStatus, !TStability)
  , inh_augments           :: ![Image ModelTy]
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

mkStaticImage :: ![TaskAppRenderer] !BlueprintIdent !Bool !ModelTy *TagSource
              -> Image ModelTy
mkStaticImage rs bpident compact {ActionState | state = tis} tsrc
  #! tt               = tis.tis_task
  #! inh              = { InhMkImg
                        | inh_bpinst             = Nothing
                        , inh_bpref              = bpident
                        , inh_task_apps          = rs
                        , inh_compact            = compact
                        , inh_prev               = 'DM'.newMap
                        , inh_inaccessible       = False
                        , inh_future_unreachable = False
                        , inh_in_maybe           = False
                        , inh_in_step            = False
                        , inh_in_mapp            = False
                        , inh_in_fapp            = False
                        , inh_in_case            = False
                        , inh_in_branch          = False
                        , inh_in_lam             = False
                        , inh_in_parallel        = False
                        , inh_in_let             = False
                        , inh_outputs            = 'DM'.newMap
                        , inh_selDetail          = Nothing
                        , inh_stepActions        = 'DM'.newMap
                        , inh_prev_statstab      = (TNotActive, TNoVal)
                        , inh_augments           = []
                        }
  #! (tf_body`, tsrc) = tExpr2Image inh tt.tf_body tsrc
  #! (img, _)         = tTaskDef inh tt.tf_module tt.tf_name tt.tf_resty tt.tf_args [] tf_body`.syn_img tsrc
  = img


mkInstanceImage :: ![TaskAppRenderer] !BlueprintInstance
                   !(Map ExprId TStability) !(Map ExprId [UIAction])
                   !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !TaskId, !Int)))
                   !Bool !ModelTy *TagSource
                -> Image ModelTy
mkInstanceImage rs bpi outputs stepActions selDetail compact {ActionState | state = tis} tsrc
  #! tt               = tis.tis_task
  #! inh              = { InhMkImg
                        | inh_bpinst             = Just bpi
                        , inh_bpref              = bpi.bpi_bpref
                        , inh_task_apps          = rs
                        , inh_compact            = compact
                        , inh_prev               = bpi.bpi_previouslyActive
                        , inh_inaccessible       = False
                        , inh_future_unreachable = False
                        , inh_in_maybe           = False
                        , inh_in_step            = False
                        , inh_in_mapp            = False
                        , inh_in_fapp            = False
                        , inh_in_case            = False
                        , inh_in_branch          = False
                        , inh_in_lam             = False
                        , inh_in_parallel        = False
                        , inh_in_let             = False
                        , inh_outputs            = outputs
                        , inh_selDetail          = selDetail
                        , inh_stepActions        = stepActions
                        , inh_prev_statstab      = (TNotActive, TNoVal)
                        , inh_augments           = []
                        }
  #! (tf_body`, tsrc) = tExpr2Image inh tt.tf_body tsrc
  #! (img, _)         = tTaskDef inh tt.tf_module tt.tf_name tt.tf_resty tt.tf_args [] tf_body`.syn_img tsrc
  = img

tExpr2Image :: !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tExpr2Image inh (TMApp eid mty mn tn targs prio ptr) tsrc = tMApp     {inh & inh_in_parallel = False} eid mty mn tn targs prio ptr tsrc
tExpr2Image inh (TFApp eid fn targs assoc)           tsrc = tFApp     inh eid fn targs assoc tsrc
tExpr2Image inh (TLet pats bdy)                      tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TIf eid c t e)                      tsrc = tIf       inh eid c t e tsrc
tExpr2Image inh (TCase eid e pats)                   tsrc = tCase     inh eid e pats tsrc
tExpr2Image inh (TVar eid pp ptr)                    tsrc = tVar      inh eid pp ptr tsrc
tExpr2Image inh (TLit pp)                            tsrc = tLit      inh pp tsrc
tExpr2Image inh (TPPExpr pp)                         tsrc = tPPExpr   inh pp tsrc
tExpr2Image inh (TExpand args tt)                    tsrc = tExpand   inh args tt tsrc
tExpr2Image inh (TSel e es)                          tsrc = tSel      inh e es tsrc
tExpr2Image inh (TRecUpd vn e es)                    tsrc = tRecUpd   inh vn e es tsrc
tExpr2Image inh (TLam args e)                        tsrc = tLam      inh args e tsrc
tExpr2Image inh (TAugment orig extra)                tsrc = tAugment  inh orig extra tsrc

tAugment :: !InhMkImg !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tAugment inh orig extra tsrc
  #! (extra`, tsrc) = tExpr2Image {inh & inh_in_case = True} extra tsrc
  = tExpr2Image {inh & inh_augments = [extra`.syn_img : inh.inh_augments]} orig tsrc

filterLamVars vars
  = filter (\x -> case x of
                    TVar _ "_x" _ -> False
                    _             -> True) vars

tLam :: !InhMkImg ![TExpr] !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tLam inh vars e tsrc
  #! (r, tsrc) = tExpr2Image inh e tsrc
  #! vars      = filterLamVars vars
  #! vars      = map ppTExpr vars
  #! lineParts = case vars of
                   []   -> [ tHorizConnArr (fillColorFromStatStab (r.syn_status, r.syn_stability))
                           , r.syn_img]
                   vars -> [ tHorizConn (fillColorFromStatStab (r.syn_status, r.syn_stability))
                           , tTextWithGreyBackground ArialRegular10px (strictFoldr (\x xs -> x +++ " " +++ xs) "" vars)
                           , tHorizConnArr (fillColorFromStatStab (r.syn_status, r.syn_stability))
                           , r.syn_img]
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

tFApp :: !InhMkImg !ExprId !FuncName ![TExpr] !TPriority !*TagSource -> *(!SynMkImg, !*TagSource)
tFApp inh eid fn args assoc tsrc
  | not (inh.inh_in_step || inh.inh_in_parallel) && (inh.inh_in_let || inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case)
    = ( { syn_img       = text ArialRegular10px (ppTExpr (TFApp eid fn args assoc))
        , syn_status    = TNotActive
        , syn_stability = TStable
        }
      , tsrc)
  | otherwise
      #! pp  = ppTExpr (TFApp eid fn args assoc)
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing
      = ( { syn_img       = img
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)

tArrowTip :: !(Maybe SVGColor) -> Image ModelTy
tArrowTip color
  = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ] <@< { stroke = TonicBlack }
                                                                             <@< { fill   = fromMaybe TonicBlack color }

fillColorFromStatStab (TNotActive, _) = Nothing
fillColorFromStatStab (_, TNoVal)     = Just TonicWhite
fillColorFromStatStab (_, TStable)    = Just TonicBlue
fillColorFromStatStab (_, TUnstable)  = Just TonicGreen

tLineMarker :: !(Maybe SVGColor) -> Maybe (Markers ModelTy)
tLineMarker status = Just {defaultMarkers & markerEnd = Just (tArrowTip status)}

tSmallHorizConn :: Image ModelTy
tSmallHorizConn = xline Nothing (px 4.0)

tHorizConn :: !(Maybe SVGColor) -> Image ModelTy
tHorizConn Nothing  = xline Nothing (px 8.0)
tHorizConn (Just c) = rect (px 8.0) (px 3.0) <@< { fill = c }

tShortHorizConn :: !(Maybe SVGColor) -> Image ModelTy
tShortHorizConn Nothing  = xline Nothing (px 4.0)
tShortHorizConn (Just c) = rect (px 4.0) (px 3.0) <@< { fill = c }

tHorizConnArr :: !(Maybe SVGColor) -> Image ModelTy
tHorizConnArr status = beside (repeat AtMiddleY) [] [tHorizConn status, tArrowTip status] Nothing

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip Nothing))}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just (tArrowTip Nothing)}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) (tArrowTip Nothing)), markerEnd = Just (tArrowTip Nothing)}) (px 16.0)

tExpand :: !InhMkImg ![TExpr] !TonicFunc !*TagSource -> *(!SynMkImg, !*TagSource)
tExpand inh argnames tt tsrc
  #! (tf_body`, tsrc) = tExpr2Image inh tt.tf_body tsrc
  #! (td_img, tsrc)   = tTaskDef inh tt.tf_module tt.tf_name tt.tf_resty tt.tf_args argnames tf_body`.syn_img tsrc
  = ({ syn_img       = td_img
     , syn_status    = tf_body`.syn_status
     , syn_stability = tf_body`.syn_stability
     }
    , tsrc)

tPPExpr :: !InhMkImg !String !*TagSource -> *(!SynMkImg, !*TagSource)
tPPExpr inh pp tsrc
  | not (inh.inh_in_step || inh.inh_in_parallel) && (inh.inh_in_let || inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case)
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

tVar :: !InhMkImg !ExprId !String !Int !*TagSource -> *(!SynMkImg, !*TagSource)
tVar inh eid pp ptr tsrc
  #! pp = if (pp == "_x") ("x" +++ toString ptr) pp
  #! txtImg = text ArialRegular10px pp
  | not (inh.inh_in_step || inh.inh_in_parallel) && (inh.inh_in_let || inh.inh_in_mapp || inh.inh_in_fapp || inh.inh_in_case)
      = ( { syn_img       = txtImg
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)
  | otherwise
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
      #! img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, txtImg] Nothing
      = ( { syn_img       = img
          , syn_status    = TNotActive
          , syn_stability = TStable
          }
        , tsrc)

determineStability :: ![TStability] -> TStability
determineStability []              = TNoVal
determineStability [TUnstable : _] = TUnstable
determineStability [TStable : _]   = TStable
determineStability [_ : xs]        = determineStability xs

determineSynStability :: ![SynMkImg] -> TStability
determineSynStability syns = determineStability (map (\x -> x.syn_stability) syns)

determineStatus :: !Bool ![TStatus] -> TStatus
determineStatus _           [TIsActive : _] = TIsActive
determineStatus needAllDone [TAllDone : xs]
  | needAllDone = determineStatus needAllDone xs
  | otherwise   = TAllDone
determineStatus needAllDone [_ : xs] = determineStatus needAllDone xs
determineStatus needAllDone _
  | needAllDone = TAllDone
  | otherwise   = TNotActive

determineSynStatus :: !Bool ![SynMkImg] -> TStatus
determineSynStatus needAllDone syns = determineStatus needAllDone (map (\x -> x.syn_status) syns)

tIf :: !InhMkImg !ExprId !TExpr !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tIf inh eid cexpr texpr eexpr tsrc
  #! (cexpr, ut, ue) = case inh.inh_bpinst of
                         Just bpi -> case 'DM'.get eid bpi.bpi_case_branches of
                                       Just 0 -> (TAugment cexpr (TLit (TBool True)), False, True)
                                       Just 1 -> (TAugment cexpr (TLit (TBool False)), True, False)
                                       _      -> (cexpr, False, False)
                         _        -> (cexpr, False, False)
  = tCaseOrIf inh cexpr [ (Just (TLit (TBool True)), texpr, True, ut)
                        , (Just (TLit (TBool False)), eexpr, True, ue)] tsrc

tCase :: !InhMkImg !ExprId !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!SynMkImg, !*TagSource)
tCase inh eid texpr pats tsrc
  #! mbranch = case inh.inh_bpinst of
                 Just bpi -> 'DM'.get eid bpi.bpi_case_branches
                 _        -> Nothing
  #! pats`   = case mbranch of
                 Just bridx -> map (\(n, (p, t)) -> (Just p, t, True, n <> bridx)) (strictTRZip2 [0..] pats)
                 _          -> map (\(p, t) -> (Just p, t, True, False)) pats
  = tCaseOrIf inh texpr pats` tsrc

tCaseOrIf :: !InhMkImg !TExpr ![(Maybe TExpr, TExpr, Bool, Bool)] !*TagSource -> *(!SynMkImg, !*TagSource)
tCaseOrIf inh texpr pats [(contextTag, _) : tsrc]
  #! (exprImg, tsrc)      = tExpr2Image {inh & inh_in_case = True} texpr tsrc
  #! (diamond, tsrc)      = tCaseDiamond inh exprImg.syn_img tsrc
  #! (syn_branches, tsrc) = tBranches {inh & inh_in_branch = True} tExpr2Image False True pats contextTag tsrc
  #! lineAct              = case syn_branches.syn_status of
                              TNotActive -> Nothing
                              _          -> case inh.inh_prev_statstab of
                                              (_, TStable)   -> Just TonicBlue
                                              (_, TUnstable) -> Just TonicGreen
                                              _              -> Just TonicWhite
  #! img                  = beside (repeat AtMiddleY) [] [diamond, tHorizConn lineAct, syn_branches.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_branches.syn_status
      , syn_stability = syn_branches.syn_stability
      }
    , tsrc)

tCaseDiamond :: !InhMkImg !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseDiamond inh exprImg [(diamondTag, uDiamondTag) : tsrc]
  #! exprImg      = tag uDiamondTag exprImg
  #! imgHeight    = imageyspan diamondTag
  #! imgWidth     = maxSpan [px 50.0, imagexspan diamondTag]
  #! edgeMargin   = imgHeight *. 2.0
  #! centerX      = (imgWidth /. 2.0) + edgeMargin
  #! leftCorner   = (px 0.0, px 0.0)
  #! topCorner    = (centerX, ~ edgeMargin)
  #! rightCorner  = (centerX *. 2.0, px 0.0)
  #! bottomCorner = (centerX, edgeMargin)
  #! diamond      = polygon Nothing [leftCorner, topCorner, rightCorner, bottomCorner]
                      <@< { fill   = TonicWhite }
                      <@< { stroke = TonicBlack }
  #! img          = overlay (repeat (AtMiddleX, AtMiddleY)) [] [diamond, exprImg] Nothing
  = (img, tsrc)

tLet :: !InhMkImg ![(!Pattern, !TExpr)] !TExpr !*TagSource -> *(!SynMkImg, *TagSource)
tLet inh pats expr [(txttag, uTxtTag) : tsrc]
  #! inh = {inh & inh_in_let = True}
  = case expr of
      TLet pats` bdy
        = tLet inh (pats ++ pats`) bdy tsrc
      _
        #! (t, tsrc)       = tExpr2Image inh expr tsrc
        #! (patRhss, tsrc) = strictTRMapSt (tExpr2Image inh) (map snd pats) tsrc
        #! binds           = strictFoldr (\(var, expr) acc -> [text ArialRegular10px (ppTExpr var) : text ArialRegular10px " = " : expr.syn_img : acc]) [] (strictTRZip2 (strictTRMap fst pats) patRhss)
        #! letText         = tag uTxtTag (grid (Columns 3) (RowMajor, LeftToRight, TopToBottom) [] [] binds Nothing)
        #! letWidth        = imagexspan txttag + px 8.0
        #! letHeight       = imageyspan txttag + px 8.0
        #! letBox          = rect letWidth letHeight
                               <@< { fill   = TonicWhite }
                               <@< { stroke = TonicBlack }
        #! letImg          = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
        #! linePart        = case t.syn_status of
                               TNotActive -> xline Nothing ((letWidth - px 8.0) /. 2.0)
                               _          -> rect ((letWidth - px 8.0) /. 2.0) (px 3.0) <@< { fill = case inh.inh_prev_statstab of
                                                                                                       (_, TUnstable) -> TonicGreen
                                                                                                       (_, TStable)   -> TonicBlue
                                                                                                       _              -> TonicWhite }
        #! lineAct              = case t.syn_status of
                                    TNotActive -> Nothing
                                    _          -> case inh.inh_prev_statstab of
                                                    (_, TStable)   -> Just TonicBlue
                                                    (_, TUnstable) -> Just TonicGreen
                                                    _              -> Just TonicWhite
        #! connBox         = beside (repeat AtMiddleY) [] [linePart, rect (px 8.0) (px 8.0), linePart] Nothing
        #! letImg          = above (repeat AtMiddleX) [] [letImg, yline Nothing (px 8.0), connBox, empty zero (letHeight + px 8.0)] Nothing
        #! img             = beside (repeat AtMiddleY) [] [letImg, tHorizConnArr lineAct, t.syn_img] Nothing
        = ( { syn_img       = img
            , syn_status    = t.syn_status
            , syn_stability = t.syn_stability
            }
          , tsrc)

tBind :: !InhMkImg !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image {inh & inh_prev_statstab = (l`.syn_status, l`.syn_stability), inh_in_branch = inh.inh_in_branch && l`.syn_status == TNotActive} r tsrc
  #! lineAct    = case r`.syn_status of
                    TNotActive -> (TNotActive, TNoVal)
                    _          -> (TAllDone, l`.syn_stability)
  #! lineAct    = fillColorFromStatStab lineAct
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
  #! (syn_branches, tsrc) = tBranches {inh & inh_in_parallel = True} tExpr2Image True False [(Nothing, l, True, False), (Nothing, r, False, False)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (-||): left bias" syn_branches uContextTag tsrc
tParSumR :: !InhMkImg !ExprId !String !String !TExpr !TExpr !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumR inh eid mn tn l r [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches {inh & inh_in_parallel = True} tExpr2Image True False [(Nothing, l, False, False), (Nothing, r, True, False)] contextTag tsrc
  = renderParallelContainer inh eid mn tn "Parallel (||-): right bias" syn_branches uContextTag tsrc
tParSumN :: !InhMkImg !ExprId !String !String !String ![TExpr]
            !*TagSource
         -> *(!SynMkImg, !*TagSource)
tParSumN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches {inh & inh_in_parallel = True} tExpr2Image True False (strictTRMap (\x -> (Nothing, x, True, False)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc
tParProdN :: !InhMkImg !ExprId !String !String !String ![TExpr]
             !*TagSource
          -> *(!SynMkImg, !*TagSource)
tParProdN inh eid mn tn descr ts [(contextTag, uContextTag) : tsrc]
  #! (syn_branches, tsrc) = tBranches {inh & inh_in_parallel = True} tExpr2Image True False (strictTRMap (\x -> (Nothing, x, True, False)) ts) contextTag tsrc
  = renderParallelContainer inh eid mn tn descr syn_branches uContextTag tsrc

renderParallelContainer :: !InhMkImg !ExprId !ModuleName !FuncName !String
                           !SynMkImg !*ImageTag !*TagSource
                        -> *(!SynMkImg, !*TagSource)
renderParallelContainer inh eid moduleName taskName descr syn_branches uContextTag tsrc
  #! isDynamic         = isJust inh.inh_bpinst
  #! mActiveTid        = case inh.inh_bpinst of
                           Just bpinst -> activeNodeTaskId eid bpinst.bpi_activeNodes
                           _           -> Nothing
  #! isActive          = isJust mActiveTid
  #! mPrevActiveTid    = 'DM'.get eid inh.inh_prev
  #! mbNavTo           = if isActive mActiveTid mPrevActiveTid
  #! stability         = let f tid = fromMaybe TNoVal (maybe Nothing (\bpinst -> 'DM'.get eid inh.inh_outputs) inh.inh_bpinst)
                         in maybe (maybe TNoVal f mPrevActiveTid) f mActiveTid
  #! mTaskId           = case (mActiveTid, mPrevActiveTid) of
                           (Just x, _) -> Just x
                           (_, Just x) -> Just x
                           _           -> Nothing
  #! taskIdStr         = maybe "" (\x -> " (" +++ toString x +++ ")") mTaskId
  #! displayName       = descr +++ taskIdStr
  #! (taskApp, tsrc)   = tParApp inh.inh_compact eid inh.inh_bpref.bpr_moduleName inh.inh_bpref.bpr_taskName displayName syn_branches tsrc
  #! clickMeta         = mkClickMeta inh (Just eid) moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_bpinst) mbNavTo
  #! valNodeIsSelected = case inh.inh_selDetail of
                           Just (Left
                                  { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName, bpident_taskId}
                                  , click_origin_mbnodeId})
                             ->    bpident_moduleName == inh.inh_bpref.bpr_moduleName
                                && bpident_taskName == inh.inh_bpref.bpr_taskName
                                && bpident_taskId == fmap (\x -> x.bpi_taskId) inh.inh_bpinst
                                && click_origin_mbnodeId == Just eid
                           _ -> False
  #! valAnchor         = rect (px 8.0) (px 8.0) <@< { onclick = openDetails clickMeta, local = False }
                                                <@< { fill = case stability of
                                                               TNoVal    -> TonicWhite
                                                               TStable   -> TonicBlue
                                                               TUnstable -> TonicGreen
                                                    }
                                                <@< { stroke = if valNodeIsSelected TonicDarkBlue TonicBlack }
                                                <@< { strokewidth = if valNodeIsSelected (px 3.0) (px 1.0) }
  #! inclArr           = beside (repeat AtMiddleY) [] (if isDynamic [taskApp, valAnchor] [taskApp]) Nothing
  = ( { syn_img       = inclArr
      , syn_status    = if isActive TIsActive (if (isJust mPrevActiveTid) TAllDone TNotActive)
      , syn_stability = stability
      }
    , tsrc)
  where
  tParApp :: !Bool !ExprId !ModuleName !FuncName !FuncName !SynMkImg !*TagSource
          -> *(!Image ModelTy, !*TagSource)
  tParApp isCompact eid parentModName parentFuncName taskName syn_branches [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
    #! taskNameImg = tag uTnTag (margin (px 2.5, px 5.0) (text ArialBold10px taskName))
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
  { click_origin_mbbpident = Just { bpident_moduleName = inh.inh_bpref.bpr_moduleName
                                  , bpident_taskName   = inh.inh_bpref.bpr_taskName
                                  , bpident_taskId     = mborig
                                  }
  , click_origin_mbnodeId  = mbnid
  , click_target_bpident   = { bpident_moduleName = modName
                             , bpident_taskName   = taskName
                             , bpident_taskId     = mbtarget
                             }
  }

tTaskDef :: !InhMkImg !String !String !TExpr ![(!TExpr, !TExpr)] ![TExpr] !(Image ModelTy) !*TagSource
         -> *(!Image ModelTy, !*TagSource)
tTaskDef inh moduleName taskName resultTy args argvars tdbody [(nameTag, uNameTag) : (argsTag, uArgsTag) : (bdytag, uBodyTag) : tsrc]
  #! userImg      = case inh.inh_bpinst of
                      Just {bpi_currentUser = Just cu} -> beside (repeat AtMiddleY) [] [margin (px 0.0, px 0.0, px 0.0, px 8.0) littleman, text ArialRegular10px (" " +++ toString cu)] Nothing
                      _                                -> empty zero zero
  #! taskIdStr    = case inh.inh_bpinst of
                      Just {bpi_taskId} -> " (" +++ toString bpi_taskId +++ ")"
                      _                 -> ""
  #! taskNameImg  = beside (repeat AtMiddleY) [] [ text ArialRegular10px (moduleName +++ ".")
                                                 , text ArialBold10px (taskName +++ " :: " +++ ppTExpr resultTy)
                                                 , text ArialRegular10px taskIdStr
                                                 , userImg] Nothing
  #! taskNameImg  = case inh.inh_bpinst of
                      Just {bpi_currentUser = Just _} -> tag uNameTag (margin (px 2.0, px 5.0) taskNameImg)
                      _                               -> tag uNameTag (margin (px 5.0) taskNameImg)
  #! binds        = flatten (strictTRZipWith3 mkArgAndTy args [0..] (strictTRMap Just argvars ++ repeat Nothing))
  #! argsText     = grid (Columns 4) (RowMajor, LeftToRight, TopToBottom) [] [] (strictTRMap (margin (px 1.0, px 0.0)) binds) Nothing
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
    #! clickHandler = { onclick = selectArg inh i, local = False}
    = [ text ArialRegular10px (ppTExpr arg) <@< clickHandler
      , text ArialRegular10px " :: "        <@< clickHandler
      , text ArialRegular10px (ppTExpr ty)  <@< clickHandler
      , text ArialRegular10px (maybe "" (\x -> " = " +++ ppTExpr x) mvar) <@< clickHandler
      ]

  selectArg :: !InhMkImg !Int !Int !ModelTy -> ModelTy
  selectArg inh=:{inh_bpinst = Just {bpi_taskId}} i 1 st
    #! meta = mkClickMeta inh Nothing moduleName taskName (Just bpi_taskId) (Just bpi_taskId)
    = { ActionState | st & action = Just (TSelectArg i, meta) }
  selectArg _ _ _ st = st

activeNodeTaskId :: !ExprId !(Map ListId (IntMap (!TaskId, !ExprId))) -> Maybe TaskId
activeNodeTaskId eid activeNodes
  = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems activeNodes) | eid == nid] of
      [tid : _] -> Just tid
      _         -> Nothing

tMApp :: !InhMkImg !ExprId !(Maybe TypeName) !ModuleName !VarName ![TExpr]
         !TPriority !(Maybe VarPtr) !*TagSource
      -> *(!SynMkImg, !*TagSource)
tMApp inh _ _ "iTasks.API.Extensions.User" "@:" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tAssign inh lhsExpr rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Core.Types" ">>|" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : TLam vars rhsExpr : _] _ _ tsrc
  # var = case filterLamVars vars of
            [var : _] = Just var
            _         = Nothing
  = tBind inh lhsExpr var rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Core.Types" ">>=" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Common.TaskCombinators" ">>-" [lhsExpr : TLam vars rhsExpr : _] _ _ tsrc
  # var = case filterLamVars vars of
            [var : _] = Just var
            _         = Nothing
  = tBind inh lhsExpr var rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Common.TaskCombinators" ">>-" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Common.TaskCombinators" ">>~" [lhsExpr : TLam vars rhsExpr : _] _ _ tsrc
  # var = case filterLamVars vars of
            [var : _] = Just var
            _         = Nothing
  = tBind inh lhsExpr var rhsExpr tsrc
tMApp inh _ _ "iTasks.API.Common.TaskCombinators" ">>~" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tBind inh lhsExpr Nothing rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Common.TaskCombinators" ">>*" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid _ "iTasks.API.Core.TaskCombinators" "step" [lhsExpr : _ : rhsExpr : _] _ _ tsrc
  = tStep inh eid lhsExpr rhsExpr tsrc
tMApp inh eid mtn mn=:"iTasks.API.Core.TaskCombinators" tn=:"parallel" [x : _] assoc _ tsrc
  # xs = if (tExprIsList x) (tUnsafeExpr2List x) [x]
  # xs = let f (TFApp _ "_Tuple2" [_ : x : _] _) = x
             f x                                 = x
          in strictTRMap f xs
  = tParProdN inh eid mn tn "Parallel tasks" xs tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-&&-" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tParProdN inh eid mn tn "Parallel (-&&-): both tasks" [lhsExpr, rhsExpr] tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"allTasks" [x] assoc _ tsrc
  = tParProdN inh eid mn tn "Parallel allTasks" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid mtn mn=:"iTasks.API.Common.TaskCombinators" tn=:"anyTask" [x] assoc _ tsrc
  = tParSumN inh eid mn tn "Parallel anyTask" (if (tExprIsList x) (tUnsafeExpr2List x) [x]) tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||-" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tParSumN inh eid mn tn "Parallel (-||-): any task" [lhsExpr, rhsExpr] tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"||-" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tParSumR inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh eid _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"-||" [lhsExpr : rhsExpr : _] _ _ tsrc
  = tParSumL inh eid mn tn lhsExpr rhsExpr tsrc
tMApp inh _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"@!" [lhsExpr : _] _ _ tsrc
  = tExpr2Image inh lhsExpr tsrc
tMApp inh _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"<<@" [lhsExpr : _] _ _ tsrc
  = tExpr2Image inh lhsExpr tsrc
tMApp inh _ _ mn=:"iTasks.API.Common.TaskCombinators" tn=:"@>>" [_ : rhsExpr : _] _ _ tsrc
  = tExpr2Image inh rhsExpr tsrc
tMApp inh eid _ modName taskName taskArgs _ _ tsrc
  #! inh = {inh & inh_in_mapp = True}
  = renderTaskApp inh eid modName taskName taskArgs taskName tsrc

renderTaskApp :: !InhMkImg !ExprId !String !String ![TExpr] !String !*TagSource
              -> *(!SynMkImg, !*TagSource)
renderTaskApp inh eid moduleName taskName taskArgs displayName tsrc
  #! (taskArgs`, tsrc)  = strictTRMapSt (tExpr2Image inh) taskArgs tsrc
  #! taskArgs`          = strictTRMap (\x -> x.syn_img) taskArgs`
  #! isDynamic          = isJust inh.inh_bpinst
  #! mActiveTid         = case inh.inh_bpinst of
                            Just bpinst -> activeNodeTaskId eid bpinst.bpi_activeNodes
                            _           -> Nothing
  #! isActive           = isJust mActiveTid
  #! mPrevActiveTid     = 'DM'.get eid inh.inh_prev
  #! mbNavTo            = if isActive mActiveTid mPrevActiveTid
  #! wasActive          = isJust mPrevActiveTid
  #! stability          = let f tid = fromMaybe TNoVal (maybe Nothing (\bpinst -> 'DM'.get eid inh.inh_outputs) inh.inh_bpinst)
                          in maybe (maybe TNoVal f mPrevActiveTid) f mActiveTid
  #! mTaskId            = case (mActiveTid, mPrevActiveTid) of
                            (Just x, _) -> Just x
                            (_, Just x) -> Just x
                            _           -> Nothing
  #! augments           = maybe inh.inh_augments (\x -> [text ArialBold10px ("(" +++ toString x +++ ")") : inh.inh_augments]) mTaskId
  #! (renderOpts, tsrc) = strictTRMapSt (\ta -> ta isDynamic inh.inh_in_branch inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_future_unreachable eid inh.inh_bpref.bpr_moduleName inh.inh_bpref.bpr_taskName moduleName displayName taskArgs` augments) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultMApp isDynamic inh.inh_in_branch inh.inh_compact isActive wasActive inh.inh_inaccessible inh.inh_future_unreachable eid inh.inh_bpref.bpr_moduleName inh.inh_bpref.bpr_taskName moduleName displayName taskArgs taskArgs` augments tsrc
  #! clickMeta          = mkClickMeta inh (Just eid) moduleName taskName (fmap (\x -> x.bpi_taskId) inh.inh_bpinst) mbNavTo
  #! taskApp            = taskApp <@< { onclick = navigateOrSelect clickMeta, local = False }
  #! valNodeIsSelected  = case inh.inh_selDetail of
                            Just (Left
                                   { click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName, bpident_taskId}
                                   , click_origin_mbnodeId})
                              ->    bpident_moduleName == inh.inh_bpref.bpr_moduleName
                                 && bpident_taskName == inh.inh_bpref.bpr_taskName
                                 && bpident_taskId == fmap (\x -> x.bpi_taskId) inh.inh_bpinst
                                 && click_origin_mbnodeId == Just eid
                            _ -> False
  #! valAnchor          = rect (px 8.0) (px 8.0) <@< { onclick = openDetails clickMeta, local = False }
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

tDefaultMApp :: !Bool !Bool !Bool !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                !ModuleName !FuncName ![TExpr] ![Image ModelTy] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)
tDefaultMApp isDynamic inBranch isCompact isActive wasActive isInAccessible isUnreachable eid parentModName parentFuncName modName taskName argsExprs taskArgs augments tsrc
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
  = tDefaultMApp` isDynamic inBranch isCompact isActive wasActive isInAccessible isUnreachable eid parentModName parentFuncName modName taskName taskArgs augments tsrc

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

tDefaultMApp` :: !Bool !Bool !Bool !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                 !ModuleName !FuncName ![Image ModelTy] ![Image ModelTy] !*TagSource
              -> *(!Image ModelTy, !*TagSource)
tDefaultMApp` isDynamic inBranch isCompact isActive wasActive isInAccessible isUnreachable eid parentModName parentFuncName modName taskName taskArgs augments [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
  #! taskNameImg       = tag uTnTag (margin (px 2.5, px 5.0) (beside (repeat AtMiddleY) [] [text ArialBold10px taskName : text ArialRegular10px " " : intersperse (text ArialRegular10px " ") augments] Nothing))
  #! bgColor           = appColor isActive wasActive isInAccessible
  #! futureUnreachable = isUnreachable && not isInAccessible
  #! futureReachable   = not isUnreachable && not isInAccessible && not (isActive || wasActive)
  #! strokeColor       = if isDynamic
                           (if futureUnreachable TonicRed (if (futureReachable && inBranch) TonicGreen TonicBlack))
                           TonicBlack
  #! strokeWidth       = if (isDynamic && (futureUnreachable || futureReachable) && inBranch) (px 3.0) (px 1.0)
  = case taskArgs of
      []
        #! bgRect = tRoundedRect (imagexspan tntag) (imageyspan tntag) <@< { fill = bgColor }
                                                                       <@< { stroke = strokeColor }
                                                                       <@< { strokewidth = strokeWidth }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskNameImg] Nothing, tsrc)
      taskArgs
        #! argsImg   = tag uArgsTag (margin (px 2.5, px 5.0) (above (repeat AtLeft) [] (strictTRMap (margin (px 1.0, px 0.0)) taskArgs) Nothing))
        #! maxXSpan  = maxSpan [imagexspan tntag, imagexspan argstag]
        #! txtBgRect = tRoundedRect maxXSpan (px 15.0) <@< { fill = bgColor }
                                                       <@< { stroke = strokeColor }
                                                       <@< { strokewidth = strokeWidth }
        #! content   = above (repeat AtLeft) [] [overlay (repeat (AtMiddleX, AtMiddleY)) [] [txtBgRect, taskNameImg] Nothing, argsImg] Nothing
        #! bgRect    = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = TonicWhite }
                                                                                     <@< { stroke = strokeColor }
                                                                                     <@< { strokewidth = strokeWidth }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tAssign :: !InhMkImg !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tAssign inh lhsExpr assignedTask [(assignTaskTag, uAssignTaskTag) : (headerTag, uHeaderTag) : tsrc]
  #! (desc, user)         = case lhsExpr of
                              (TFApp _ "_Tuple2" [usr, str : _] _) -> (ppTExpr str, mkUser usr)
                              usr                                  -> ("", mkUser usr)
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
  mkUser (TFApp _ "AnyUser" _ _)          = "Any user"
  mkUser (TFApp _ "UserWithId" [uid:_] _) = ppTExpr uid
  mkUser (TFApp _ "UserWithRole" [r:_] _) = "Anyone with role " +++ ppTExpr r
  mkUser (TFApp _ "SystemUser" _ _)       = "System user"
  mkUser (TFApp _ "AnonymousUser" _ _)    = "Anonymous user"
  mkUser (TFApp _ "AuthenticatedUser" [uid:rs:_] _) = ppTExpr uid +++ " with roles " +++ strictFoldr (\x xs -> ppTExpr x +++ " " +++ xs) "" (tSafeExpr2List rs)
  mkUser (TFApp _ usr _ _)                = usr
  mkUser (TVar _ ppe _)                   = ppe
  mkUser (TLit (TString ppe))             = ppe
  mkUser (TPPExpr ppe)                    = ppe
  mkUser _                                = ""

tStep :: !InhMkImg !ExprId !TExpr !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStep inh eid lhsExpr conts [(contextTag, _) : tsrc]
  #! actions              = case 'DM'.get eid inh.inh_stepActions of
                              Just xs -> xs
                              _       -> []
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! conts                = case tSafeExpr2List conts of
                              [t]   -> [(Nothing, t, False, False)]
                              conts -> strictTRMap (\t -> (Nothing, t, True, False)) conts
  #! (syn_branches, tsrc) = tBranches {inh & inh_prev_statstab = (lhs.syn_status, lhs.syn_stability)} (tStepCont actions) False True conts contextTag tsrc
  #! img                  = beside (repeat AtMiddleY) [] [lhs.syn_img, tHorizConn (fillColorFromStatStab (lineStatus lhs)), syn_branches.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_branches.syn_status
      , syn_stability = syn_branches.syn_stability
      }
    , tsrc)


lineStatus :: !SynMkImg -> (!TStatus, !TStability)
lineStatus {syn_status = TNotActive} = (TNotActive, TNoVal)
lineStatus {syn_stability}           = (TAllDone, syn_stability)

tExprIsList :: TExpr -> Bool
tExprIsList (TFApp _ "_Cons" _ _) = True
tExprIsList (TFApp _ "_Nil"  _ _) = True
tExprIsList _                     = False

tUnsafeExpr2List :: TExpr -> [TExpr]
tUnsafeExpr2List (TFApp _ "_Cons" [hd : tl : _] _) = [hd : tUnsafeExpr2List tl]
tUnsafeExpr2List (TFApp _ "_Nil"  _             _) = []
tUnsafeExpr2List _                                 = abort "tUnsafeExpr2List"

tSafeExpr2List :: TExpr -> [TExpr]
tSafeExpr2List (TFApp _ "_Cons" [hd : tl : _] _) = [hd : tUnsafeExpr2List tl]
tSafeExpr2List (TFApp _ "_Nil"  _             _) = []
tSafeExpr2List e                                 = [e]

tStepCont :: ![UIAction] !InhMkImg !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
tStepCont actions inh (TFApp _ "OnAction" [TFApp _ "Action" [actionLit : _] _ : cont : _ ] _) tsrc
  = mkStepCont inh (Just (ppTExpr actionLit, strictFoldr f False actions)) cont tsrc
  where
  f {UIAction | action = Action an _, enabled} acc = (replaceSubString "\"" "" an == replaceSubString "\"" "" (ppTExpr actionLit) && enabled) || acc
  f _ acc = acc
tStepCont _ inh (TFApp _ "OnValue"  [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp _ "OnException" [cont : _ ] _)     tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh (TFApp _ "OnAllExceptions" [cont : _ ] _) tsrc
  = mkStepCont inh Nothing cont tsrc
tStepCont _ inh expr tsrc = tExpr2Image inh expr tsrc

mkStepCont :: !InhMkImg !(Maybe (!String, !Bool)) !TExpr !*TagSource -> *(!SynMkImg, !*TagSource)
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "always" [mapp : _] _ _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "never" [mapp : _] _ _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withoutValue" [mapp : _] _ _) tsrc
  = stepAlwaysNeverWithoutVal inh mact mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifStable" e _ _) tsrc
  = stepIfStableUnstableHasValue inh mact tStable e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifUnstable" e _ _) tsrc
  = stepIfStableUnstableHasValue inh mact tUnstable e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "hasValue" e _ _) [ref : tsrc]
  = stepIfStableUnstableHasValue inh mact hasValueFilter e tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifValue" [conditionApp : continuationApp : _] _ _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "ifCond" [conditionApp : continuationApp : _] _ _) tsrc
  = stepIfValueCond inh mact conditionApp continuationApp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withValue" [mapp : _] _ _) tsrc
  = stepWithValue inh mact hasValueFilter mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withStable" [mapp : _] _ _) tsrc
  = stepWithValue inh mact tStable mapp tsrc
mkStepCont inh mact (TMApp _ _ "iTasks.API.Common.TaskCombinators" "withUnstable" [mapp : _] _ _) tsrc
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
  #! img       = beside (repeat AtMiddleY) [] [addAction mact (tHorizConnArr (stepArrActivity inh x)) ref, x.syn_img] Nothing
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
  #! img                     = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (stepArrActivity inh continuationImg), addAction mact (tShortHorizConn (stepArrActivity inh continuationImg)) ref, continuationImg.syn_img] Nothing
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
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (stepArrActivity inh x), addAction mact (tHorizConnArr (stepArrActivity inh x)) ref, x.syn_img] Nothing
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
  #! imgs1                = [ conditionImg
                            , tHorizConnArr (stepArrActivity inh syn_e)
                            ]
  #! pats                 = filterLamVars pats
  #! imgs2                = if (length pats > 0)
                              [ addAction mact (tHorizConn (stepArrActivity inh syn_e)) ref
                              , tTextWithGreyBackground ArialRegular10px (strictFoldr (\x xs -> ppTExpr x +++ " " +++ xs) "" pats)]
                              [addAction mact (tShortHorizConn (stepArrActivity inh syn_e)) ref]
  #! imgs3                = [ tHorizConnArr (stepArrActivity inh syn_e)
                            , syn_e.syn_img
                            ]
  #! img                  = beside (repeat AtMiddleY) [] (imgs1 ++ imgs2 ++ imgs3) Nothing
  = ( { syn_img       = img
      , syn_status    = syn_e.syn_status
      , syn_stability = syn_e.syn_stability
      }
    , tsrc)
stepIfStableUnstableHasValue inh mact filter [e : _] [ref : tsrc]
  #! (syn_e, tsrc)        = tExpr2Image inh e tsrc
  #! (conditionImg, tsrc) = tCaseDiamond inh filter tsrc
  #! img                  = beside (repeat AtMiddleY) [] [conditionImg, tHorizConnArr (stepArrActivity inh syn_e), addAction mact (tHorizConnArr (stepArrActivity inh syn_e)) ref, syn_e.syn_img] Nothing
  = ( { syn_img       = img
      , syn_status    = syn_e.syn_status
      , syn_stability = syn_e.syn_stability
      }
    , tsrc)

stepArrActivity inh syn
  = fillColorFromStatStab (case (syn.syn_status, syn.syn_stability) of
                             (TNotActive, _) -> (TNotActive, TNoVal)
                             _               -> inh.inh_prev_statstab)

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
hasValueFilter = beside (repeat AtMiddleY) [] [ tStableBox, tUnstableBox, text ArialBold10px " Has value"] Nothing

tBranches :: !InhMkImg !(InhMkImg TExpr *TagSource -> *(!SynMkImg, !*TagSource))
             !Bool !Bool ![(!Maybe Pattern, !TExpr, !Bool, !Bool)] !ImageTag !*TagSource
          -> *(!SynMkImg, !*TagSource)
tBranches inh mkBranch needAllDone inclVertConns exprs contextTag tsrc
  #! (allTags, nonUTags, tsrc) = takeNTags (length exprs) tsrc
  #! maxXSpan                  = maxSpan (strictTRMap imagexspan [contextTag : nonUTags])
  #! (allBranchActivity, tsrc) = strictTRMapSt branchStatus exprs tsrc
  #! existsSomeActivity        = someActivity allBranchActivity
  #! (syns, tsrc)              = strictTRMapSt (iter existsSomeActivity maxXSpan) (strictTRZip3 exprs allBranchActivity allTags) tsrc
  #! branchImg                 = above (repeat AtLeft) [] (strictTRMap (\x -> x.syn_img) syns) Nothing
  #! status                    = determineSynStatus needAllDone syns
  | inclVertConns
    #! vertConn = mkVertConn nonUTags
    = ( { syn_img       = beside (repeat AtMiddleY) [] [vertConn, branchImg, vertConn] Nothing
        , syn_status    = status
        , syn_stability = determineSynStability syns
        }
      , tsrc)
  | otherwise
    = ( { syn_img       = branchImg
        , syn_status    = status
        , syn_stability = determineSynStability syns
        }
      , tsrc)
  where
  branchStatus :: !(Maybe Pattern, !TExpr, Bool, Bool) !*TagSource -> *(!TStatus, !*TagSource)
  branchStatus (_, x, _, _) tsrc
    #! (syn, tsrc) = mkBranch inh x tsrc
    = (syn.syn_status, tsrc)

  iter :: !Bool !Span !(!(!Maybe Pattern, !TExpr, !Bool, !Bool), !TStatus, !*TagRef)
          !*TagSource
       -> *(!SynMkImg, !*TagSource)
  iter existsSomeActivity maxXSpan ((pat, texpr, showRhs, unreachable), currBranchActivity, (imgTag, uImgTag)) tsrc
    #! inaccessible = inh.inh_inaccessible || (existsSomeActivity && currBranchActivity == TNotActive)
    #! unreachable  = inh.inh_future_unreachable || unreachable
    #! (syn, tsrc)  = mkBranch {inh & inh_inaccessible = inaccessible, inh_future_unreachable = unreachable} texpr tsrc
    #! lhsLineAct   = fillColorFromStatStab (if inaccessible (TNotActive, TNoVal)
                                               (case syn.syn_status of
                                                  TNotActive -> (TNotActive, TNoVal)
                                                  _          -> inh.inh_prev_statstab))
    #! lhs          = case pat of
                        Nothing
                          = beside (repeat AtMiddleY) [] [tHorizConnArr lhsLineAct, syn.syn_img] Nothing
                        Just pat
                          #! textBox = tTextWithGreyBackground ArialRegular10px (ppTExpr pat)
                          = beside (repeat AtMiddleY) [] [tHorizConn lhsLineAct, textBox, tHorizConnArr lhsLineAct, syn.syn_img] Nothing
    #! img          = case showRhs of
                        True
                          #! lhs       = tag uImgTag (margin (px 2.5, px 0.0) lhs)
                          #! lineWidth = (maxXSpan - imagexspan imgTag) + px 8.0
                          #! rhs       = case syn.syn_status of
                                           TAllDone ->
                                             case syn.syn_stability of
                                               TNoVal    -> rect lineWidth (px 3.0) <@< { fill = TonicWhite }
                                               TStable   -> rect lineWidth (px 3.0) <@< { fill = TonicBlue }
                                               TUnstable -> rect lineWidth (px 3.0) <@< { fill = TonicGreen }
                                           _        -> xline Nothing lineWidth
                          = beside (repeat AtMiddleY) [] [lhs, rhs] Nothing
                        _ = lhs
    = ({ syn_img       = img
       , syn_status    = syn.syn_status
       , syn_stability = syn.syn_stability
       }, tsrc)

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
        #! allYSpans  = strictFoldl (\acc x -> imageyspan x + acc) (px 0.0) ts
        #! halfFirstY = imageyspan firstTag /. 2.0
        #! halfLastY  = imageyspan lastTag /. 2.0
        = above (repeat AtMiddleX) []
            [ yline Nothing halfFirstY <@< { stroke = TonicWhite }
            , yline Nothing (allYSpans - halfFirstY - halfLastY) <@< { stroke = TonicBlack }
            , yline Nothing halfLastY <@< { stroke = TonicWhite } ]
            Nothing

someActivity :: ![TStatus] -> Bool
someActivity [TAllDone : _]  = True
someActivity [TIsActive : _] = True
someActivity [_ : xs]        = someActivity xs
someActivity _               = False

tTextWithGreyBackground :: !FontDef !String -> Image ModelTy
tTextWithGreyBackground font txt
  #! textWidth = textxspan font txt + px 10.0
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (font.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text font txt] Nothing

littleman :: Image a
littleman
  #! head      = circle (px 8.0) <@< {StrokeWidthAttr | strokewidth = px 0.0}
  #! shoulders = rect (px 16.0) (px 4.0) <@< {StrokeWidthAttr | strokewidth = px 0.0}
                                         <@< { xradius     = px 5.0 }
                                         <@< { yradius     = px 5.0 }
  #! arm       = rect (px 3.0) (px 6.0) <@< {StrokeWidthAttr | strokewidth = px 0.0}
  #! vline     = yline Nothing (px 4.0) <@< {StrokeAttr | stroke = TonicWhite}
                                        <@< {StrokeWidthAttr | strokewidth = px 2.0}
  #! chest     = rect (px 10.0) (px 5.0) <@< {StrokeWidthAttr | strokewidth = px 0.0}
  #! arms      = beside [] [(px 0.0, px (-2.0)), (px 0.0, px 0.0), (px 0.0, px (-1.0)), (px 0.0, px 0.0), (px 0.0, px (-2.0))] [arm, vline, chest, vline, arm] Nothing
  = above (repeat AtMiddleX) [] [head, shoulders, arms] Nothing

tException :: Image ModelTy
tException = beside (repeat AtMiddleY) [] [ tExceptionBox, text ArialBold10px " Exception"] Nothing

tExceptionBox = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (px 8.0) (px 8.0) <@< { fill = TonicRed }
                                                           , text ArialBold6px "E" ] Nothing

tStable :: Image ModelTy
tStable = beside (repeat AtMiddleY) [] [tStableBox, text ArialBold10px " Stable"] Nothing

tStableBox = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (px 8.0) (px 8.0) <@< { fill = TonicBlue }
                                                        , text ArialBold6px "S" ] Nothing


tUnstable :: Image ModelTy
tUnstable = beside (repeat AtMiddleY) [] [tUnstableBox, text ArialBold10px " Unstable"] Nothing

tUnstableBox = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (px 8.0) (px 8.0) <@< { fill = TonicGreen }
                                                          , text ArialBold6px "U" ] Nothing


strictTRMapSt :: !(.a -> .(.st -> .(!b, !.st))) ![.a] !.st -> .(![b], !.st)
strictTRMapSt f xs st
  #! (rs, st) = strictTRMapStAcc f xs [] st
  = (reverseTR rs, st)

strictTRMapStAcc :: !(.a -> .(.st -> .(!b, !.st))) ![.a] ![b] !.st -> .(![b], !.st)
strictTRMapStAcc f []     acc st = (acc, st)
strictTRMapStAcc f [x:xs] acc st
  #! (r, st) = f x st
  = strictTRMapStAcc f xs [r : acc] st

strictTRZip2 :: ![a] ![b] -> [(!a, !b)]
strictTRZip2 as bs = reverseTR (strictTRZip2Rev as bs)

strictTRZip2Rev :: ![a] ![b] -> [(!a, !b)]
strictTRZip2Rev as bs = strictTRZip2Acc as bs []

strictTRZip2Acc :: ![a] ![b] ![(!a, !b)] -> [(!a, !b)]
strictTRZip2Acc [a:as] [b:bs] acc
  = strictTRZip2Acc as bs [(a, b):acc]
strictTRZip2Acc _ _ acc = acc

strictTRZip3 :: ![.a] ![.b] ![.c] -> [(!.a, !.b, !.c)]
strictTRZip3 as bs cs = reverseTR (strictTRZip3Rev as bs cs)

strictTRZip3Rev :: ![.a] ![.b] ![.c] -> [(!.a, !.b, !.c)]
strictTRZip3Rev as bs cs = strictTRZip3Acc as bs cs []

strictTRZip3Acc :: !u:[v:a] !w:[x:b] !y:[z:c] !u0:[v0:(!v:a, !x:b, !z:c)] -> w0:[x0:(!v:a, !x:b, !z:c)], [x0 u <= v,x0 w <= x,x0 y <= z,u0 <= v0,u0 <= w0,w0 v0 <= x0]
strictTRZip3Acc [a:as] [b:bs] [c:cs] acc
  = strictTRZip3Acc as bs cs [(a, b, c):acc]
strictTRZip3Acc _ _ _ acc = acc

strictTRZipWith3 :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3 f as bs cs = reverseTR (strictTRZipWith3Rev f as bs cs)

strictTRZipWith3Rev :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Rev f as bs cs = strictTRZipWith3Acc f as bs cs []

strictTRZipWith3Acc :: !(a b c -> d) ![a] ![b] ![c] ![d] -> [d]
strictTRZipWith3Acc f [a:as] [b:bs] [c:cs] acc
  = strictTRZipWith3Acc f as bs cs [f a b c : acc]
strictTRZipWith3Acc _ _ _ _ acc = acc

strictFoldl :: !(.a -> .(.b -> .a)) !.a ![.b] -> .a
strictFoldl f b [] = b
strictFoldl f b [x:xs]
  #! r = f b x
  = strictFoldl f r xs
