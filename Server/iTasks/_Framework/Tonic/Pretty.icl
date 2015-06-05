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

ppTExpr` :: !Int !TExpr -> String
ppTExpr` _ (TVar _ pp)             = sugarPP pp
ppTExpr` _ (TLit pp)               = sugarPP pp
ppTExpr` _ (TFApp pp [] _)         = sugarPP pp
ppTExpr` _ (TFApp "_List" [x:_] _) = "[" +++ ppTExpr x +++ "]"
ppTExpr` _ (TFApp "_Cons" xs _)    = "[" +++ ppTExprList xs +++ "]"
ppTExpr` _ (TFApp "_Tuple2" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp "_Tuple3" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp "_Tuple4" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp pp [x:xs] _)
  | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTExprTuple xs +++ " }"
ppTExpr` d (TFApp pp [l, r] (TLeftAssoc  n)) = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
ppTExpr` d (TFApp pp [l, r] (TRightAssoc n)) = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
ppTExpr` d (TFApp pp xs _)                   = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) xs) +++ if (d > 0) ")" ""
ppTExpr` _ (TMApp _ _ _ pp [] _)     = sugarPP pp
ppTExpr` _ (TMApp _ _ _ pp [x:xs] _)
  | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTExprTuple xs +++ " }"
ppTExpr` d (TMApp _ _ _ pp xs _) = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) xs) +++ if (d > 0) ")" ""
ppTExpr` d (TSel e es) = ppTExpr e +++ "." +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTExpr` (d + 1)) es)
ppTExpr` _ (TLam vars e) = "λ" +++ foldr (\x xs -> x +++ " " +++ xs) "" vars +++ "→ " +++ ppTExpr e
ppTExpr` d (TCaseOrIf e cs) = "case " +++ ppTExpr` d e +++ " of { " +++ ppCases d cs +++ "}"
ppTExpr` d (TExpand _ e) = ppTExpr` d e
ppTExpr` _ _ = "ppTExpr: encountered more complex expression than we would like to pretty-print here..."

ppCases _ []               = ""
ppCases d [(pat, expr)]    = ppTExpr` d pat +++ " -> " +++ ppTExpr` d expr
ppCases d [(pat, expr):xs] = ppTExpr` d pat +++ " -> " +++ ppTExpr` d expr +++ "; " +++ ppCases d xs

ppTExprList :: ![TExpr] -> String
ppTExprList []                      = ""
ppTExprList [TVar _ "_Nil"]         = ""
ppTExprList [x, TVar _ "_Nil"]      = ppTExpr x
ppTExprList [x, TFApp "_Cons" xs _] = ppTExpr x +++ ", " +++ ppTExprList xs
ppTExprList [x:xs]                  = ppTExpr x +++ ", " +++ ppTExprList xs

ppTExprTuple :: ![TExpr] -> String
ppTExprTuple []     = ""
ppTExprTuple [x]    = ppTExpr x
ppTExprTuple [x:xs] = ppTExpr x +++ ", " +++ ppTExprTuple xs

sugarPP "_Nil"    = "[]"
sugarPP "_Unit"   = "()"
sugarPP "_String" = "String"
sugarPP pp = pp

