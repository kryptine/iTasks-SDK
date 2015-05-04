implementation module iTasks._Framework.Tonic.Pretty

import StdArray
import StdBool
import StdClass
import StdOverloaded
import StdString
import Data.List
import iTasks._Framework.Tonic.AbsSyn

ppTExpr :: !TExpr -> String
ppTExpr tcexpr = ppTExpr` 0 tcexpr
  where
  ppTExpr` :: !Int !TExpr -> String
  ppTExpr` _ (TVar _ pp)             = sugarPP pp
  ppTExpr` _ (TLit pp)               = sugarPP pp
  ppTExpr` _ (TFApp _ pp [])         = sugarPP pp
  ppTExpr` _ (TFApp _ "_List" [x:_]) = "[" +++ ppTExpr x +++ "]"
  ppTExpr` _ (TFApp _ "_Cons" xs)    = "[" +++ ppTExprList xs +++ "]"
  ppTExpr` _ (TFApp _ "_Tuple2" xs)  = "(" +++ ppTExprTuple xs +++ ")"
  ppTExpr` _ (TFApp _ "_Tuple3" xs)  = "(" +++ ppTExprTuple xs +++ ")"
  ppTExpr` _ (TFApp _ "_Tuple4" xs)  = "(" +++ ppTExprTuple xs +++ ")"
  ppTExpr` _ (TFApp _ pp [x:xs])
    | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTExprTuple xs +++ " }"
  ppTExpr` d (TFApp (TLeftAssoc  n) pp [l, r]) = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTExpr` d (TFApp (TRightAssoc n) pp [l, r]) = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTExpr` d (TFApp _               pp xs)     = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) xs) +++ if (d > 0) ")" ""
  ppTExpr` _ (TMApp _ _ _ pp [])         = sugarPP pp
  ppTExpr` _ (TMApp _ _ _ pp [x:xs])
    | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTExprTuple xs +++ " }"
  ppTExpr` d (TMApp _ _ _ pp xs)     = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) xs) +++ if (d > 0) ")" ""
  ppTExpr` d (TSel e es) = ppTExpr e +++ "." +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) es)
  ppTExpr` _ (TLam vars e) = "λ" +++ foldr (\x xs -> x +++ " " +++ xs) "" vars +++ "→ " +++ ppTExpr e
  ppTExpr` d (TExpand _ e) = ppTExpr` d e
  ppTExpr` _ _ = "ppTExpr: encountered more complex expression than we would like to pretty-print here..."

ppTExprList :: ![TExpr] -> String
ppTExprList []                      = ""
ppTExprList [TVar _ "_Nil"]         = ""
ppTExprList [x, TVar _ "_Nil"]      = ppTExpr x
ppTExprList [x, TFApp _ "_Cons" xs] = ppTExpr x +++ ", " +++ ppTExprList xs
ppTExprList [x:xs]                  = ppTExpr x +++ ", " +++ ppTExprList xs

ppTExprTuple :: ![TExpr] -> String
ppTExprTuple []     = ""
ppTExprTuple [x]    = ppTExpr x
ppTExprTuple [x:xs] = ppTExpr x +++ ", " +++ ppTExprTuple xs

sugarPP "_Nil"    = "[]"
sugarPP "_Unit"   = "()"
sugarPP "_String" = "String"
sugarPP pp = pp

