implementation module iTasks._Framework.Tonic.AbsSyn

import Text.JSON
from GenEq import generic gEq
from Data.Map import :: Map
import qualified Data.Map as DM
import StdBool, StdList, StdTuple
from StdOverloaded import class == (..)

derive JSONEncode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive JSONDecode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive gEq TonicModule, TonicTask, TExpr, TPriority, TAssoc

instance == TonicModule where
  (==) tm1 tm2 =  tm1.tm_name              == tm2.tm_name
               && 'DM'.toList tm1.tm_tasks == 'DM'.toList tm2.tm_tasks

instance == TonicTask where
  (==) tt1 tt2 =  tt1.tt_module    == tt2.tt_module
               && tt1.tt_name      == tt2.tt_name
               && tt1.tt_iclLineNo == tt2.tt_iclLineNo
               && tt1.tt_resty     == tt2.tt_resty
               && tt1.tt_args      == tt2.tt_args
               && tt1.tt_body      == tt2.tt_body

instance == TExpr where
  (==) (TVar  eid1 ppe1) (TVar eid2 ppe2) = eid1 == eid2 && ppe1 == ppe2
  (==) (TLit  ppe1) (TLit ppe2) = ppe1 == ppe2
  (==) (TMApp eid1 tn1 mn1 vn1 as1 p1) (TMApp eid2 tn2 mn2 vn2 as2 p2) = eid1 == eid2 && tn1 == tn2 && mn1 == mn2 && vn1 == vn2 && as1 == as2 && p1 == p2
  (==) (TFApp vn1 es1 p1) (TFApp vn2 es2 p2) = vn1 == vn2 && es1 == es2 && p1 == p2
  (==) (TLam  es1 e1) (TLam es2 e2) = es1 == es2 && e1 == e2
  (==) (TSel  e1 es1) (TSel e2 es2) = e1 == e2 && es1 == es2
  (==) (TRecUpd vn1 te1 es1) (TRecUpd vn2 te2 es2) = vn1 == vn2 && te1 == te2 && es1 == es2
  (==) TNoBind TNoBind = True
  (==) (TLet bs1 e1) (TLet bs2 e2) = bs1 == bs2 && e1 == e2
  (==) (TCaseOrIf e1 ps1) (TCaseOrIf e2 ps2) = e1 == e2 && ps1 == ps2
  (==) (TExpand es1 tt1) (TExpand es2 tt2) = es1 == es2 && tt1 == tt2
  (==) _ _ = False

instance == TAssoc where
  (==) TLeftAssoc  TLeftAssoc  = True
  (==) TRightAssoc TRightAssoc = True
  (==) TNoAssoc    TNoAssoc    = True
  (==) _           _           = False

instance == TPriority where
  (==) (TPrio a1 n1) (TPrio a2 n2) = a1 == a2 && n1 == n2
  (==) TNoPrio       TNoPrio       = True
  (==) _             _             = False
