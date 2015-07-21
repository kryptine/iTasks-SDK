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
ppTExpr` _ (TVar _ pp _)           = sugarPP pp
ppTExpr` _ (TPPExpr pp)            = sugarPP pp
ppTExpr` _ (TLit lit)              = ppLit lit
ppTExpr` _ (TFApp pp [] _)         = sugarPP pp
ppTExpr` _ (TFApp "_List" [x:_] _) = "[" +++ ppTExpr x +++ "]"
ppTExpr` _ xs=:(TFApp "_Cons" _ _) = "[" +++ ppTExprList xs +++ "]"
ppTExpr` _ (TFApp "_Tuple2" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp "_Tuple3" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp "_Tuple4" xs _)  = "(" +++ ppTExprTuple xs +++ ")"
ppTExpr` _ (TFApp pp xs _)
  | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++
                                   " | " +++ ppIntersperse ppTExpr ", " xs +++ " }"
ppTExpr` d (TFApp pp [l, r] (TPrio TLeftAssoc n))  = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
ppTExpr` d (TFApp pp [l, r] (TPrio TRightAssoc n)) = if (d > 0) "(" "" +++ ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r +++ if (d > 0) ")" ""
ppTExpr` d (TFApp pp [l, r] (TPrio TNoAssoc n))    = ppTExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTExpr` (d + 1) r
ppTExpr` d (TFApp pp xs _)       = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ ppIntersperse (ppTExpr` (d + 1)) " " xs +++ if (d > 0) ")" ""
ppTExpr` _ (TMApp _ _ _ pp [] _) = sugarPP pp
ppTExpr` _ (TMApp _ _ _ pp [x:xs] _)
  | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " +++ ppTExprTuple xs +++ " }"
ppTExpr` d (TMApp _ _ _ pp xs _) = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ ppIntersperse (ppTExpr` (d + 1)) " " xs +++ if (d > 0) ")" ""
ppTExpr` d (TSel e es)      = ppTExpr e +++ "." +++ ppIntersperse (ppTExpr` (d + 1)) " " es
ppTExpr` d (TLam vars e)    = if (d > 0) "(" "" +++ "\\" +++ ppIntersperse (ppTExpr` (d + 1)) " " vars +++ "-> " +++ ppTExpr e +++ if (d > 0) ")" ""
ppTExpr` d (TIf c t e)      = "if (" +++ ppTExpr` d c +++ ") (" +++ ppTExpr` d t +++ ") (" +++ ppTExpr` d e +++ ")"
ppTExpr` d (TCase e cs)     = "case " +++ ppTExpr` d e +++ " of { " +++ ppCases d cs +++ "}"
ppTExpr` d (TExpand _ tt)   = ppTExpr` d tt.tf_body
ppTExpr` d TNoBind          = ""
ppTExpr` _ _ = "ppTExpr: encountered more complex expression than we would like to pretty-print here..."

ppCases :: !Int ![(!Pattern, !TExpr)] -> String
ppCases d xs = ppIntersperse (\(pat, expr) -> ppTExpr` d pat +++ " -> " +++ ppTExpr` d expr) "; " xs

ppTExprList :: !TExpr -> String
ppTExprList e
  | endsWithNil e = ppTExprNilList e
  | otherwise     = ppTExprList` e
  where
  ppTExprList` :: !TExpr -> String
  ppTExprList` (TFApp "_Cons" [x, xs] _) = ppTExpr x +++ " : " +++ ppTExprList xs
  ppTExprList` x                         = ppTExpr x

  ppTExprNilList :: !TExpr -> String
  ppTExprNilList (TFApp "_Cons" [x, TFApp "_Nil" _ _] _) = ppTExpr x
  ppTExprNilList (TFApp "_Cons" [x, xs] _)               = ppTExpr x +++ ", " +++ ppTExprList xs
  ppTExprNilList x                                       = ppTExpr x

endsWithNil :: !TExpr -> Bool
endsWithNil (TFApp "_Cons" [x, xs] _) = endsWithNil xs
endsWithNil (TFApp "_Nil" _ _)        = True
endsWithNil _                         = False

ppTExprTuple :: ![TExpr] -> String
ppTExprTuple xs = ppIntersperse ppTExpr ", " xs

sugarPP "_Nil"    = "[]"
sugarPP "_Unit"   = "()"
sugarPP "_String" = "String"
sugarPP pp = pp

ppLit :: !TLit -> String
ppLit (TBool   x) = toString x
ppLit (TInt    x) = toString x
ppLit (TReal   x) = toString x
ppLit (TString x) = x

ppIntersperse :: !(a -> String) !String ![a] -> String
ppIntersperse _ _   []     = ""
ppIntersperse f _   [x]    = f x
ppIntersperse f sep [x:xs] = f x +++ sep +++ ppIntersperse f sep xs
