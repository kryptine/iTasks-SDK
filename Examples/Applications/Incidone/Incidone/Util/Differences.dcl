definition module Incidone.Util.Differences
import iTasks
import Data.Maybe

showDifferences :: a a -> String | gDifferences{|*|} a

generic gDifferences a | gText a, gEq a :: a a -> [(Maybe String, String, String)]
derive gDifferences Int,Real,Bool,String,UNIT,EITHER,CONS,RECORD,PAIR,FIELD of {gfd_name},OBJECT
derive gDifferences (,)
