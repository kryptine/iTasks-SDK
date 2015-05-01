implementation module iTasks._Framework.Tonic.Util

import StdArray
import StdBool
import StdClass
import StdOverloaded
import StdString
import Data.List
import iTasks._Framework.Tonic.AbsSyn

ppTCleanExpr :: !TCleanExpr -> String
ppTCleanExpr tcexpr = ppTCleanExpr` 0 tcexpr
  where
  ppTCleanExpr` :: !Int !TCleanExpr -> String
  ppTCleanExpr` _ (PPCleanExpr "_Nil")   = "[]"
  ppTCleanExpr` _ (PPCleanExpr pp)       = sugarPP pp
  ppTCleanExpr` _ (AppCleanExpr _ pp []) = sugarPP pp
  ppTCleanExpr` _ (AppCleanExpr _ "_List" [x:_]) = "[" +++ ppTCleanExpr x +++ "]"
  ppTCleanExpr` _ (AppCleanExpr _ "_Cons" xs)    = "[" +++ ppTCleanExprList xs +++ "]"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple2" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple3" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple4" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ pp [x:xs])
    | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTCleanExprTuple xs +++ " }"
  ppTCleanExpr` d (AppCleanExpr (TLeftAssoc  n) pp [l, r]) = if (d > 0) "(" "" +++ ppTCleanExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTCleanExpr` d (AppCleanExpr (TRightAssoc n) pp [l, r]) = if (d > 0) "(" "" +++ ppTCleanExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTCleanExpr` d (AppCleanExpr _               pp xs)     = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTCleanExpr` (d + 1)) xs) +++ if (d > 0) ")" ""

ppTCleanExprList :: ![TCleanExpr] -> String
ppTCleanExprList []                             = ""
ppTCleanExprList [PPCleanExpr "_Nil"]           = ""
ppTCleanExprList [x, PPCleanExpr "_Nil"]        = ppTCleanExpr x
ppTCleanExprList [x, AppCleanExpr _ "_Cons" xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprList xs
ppTCleanExprList [x:xs]                         = ppTCleanExpr x +++ ", " +++ ppTCleanExprList xs

ppTCleanExprTuple :: ![TCleanExpr] -> String
ppTCleanExprTuple []  = ""
ppTCleanExprTuple [x] = ppTCleanExpr x
ppTCleanExprTuple [x:xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprTuple xs

sugarPP "_Unit"   = "nothing"
sugarPP "_String" = "String"
sugarPP pp = pp

