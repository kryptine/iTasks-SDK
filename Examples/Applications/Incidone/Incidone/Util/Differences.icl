implementation module Incidone.Util.Differences
import iTasks
import Data.Maybe, Text

//Generic function for concise value representations in logs
showDifferences :: a a -> String | gDifferences{|*|} a
showDifferences x y = join "\n\n" [maybe "" (\l -> l +++ ":\n") label +++ "Old value: " +++ old +++ "\nNew value: "+++new \\ (label,old,new) <- gDifferences{|*|} x y]

generic gDifferences a | gText a, gEq a :: a a -> [(Maybe String, String, String)]

gDifferences{|Int|} x y = if (x === y) [] [(Nothing,toSingleLineText x,toSingleLineText y)]
gDifferences{|Real|} x y = if (x === y) [] [(Nothing,toSingleLineText x,toSingleLineText y)]
gDifferences{|Bool|} x y = if (x === y) [] [(Nothing,toSingleLineText x,toSingleLineText y)]
gDifferences{|String|} x y = if (x === y) [] [(Nothing,toSingleLineText x,toSingleLineText y)]

gDifferences{|UNIT|} x y = []
gDifferences{|EITHER|} _ _ _ _ _ _ _ _ = [] //CONS & EITHER should not really happen, when OBJECT already does the comparison
gDifferences{|CONS|} _ _ _ _ _ = []
gDifferences{|RECORD|} difA vizA eqA (RECORD x) (RECORD y) = difA x y
gDifferences{|PAIR|} difA vizA eqA difB vizB eqB (PAIR x1 x2) (PAIR y1 y2) = difA x1 y1 ++ difB x2 y2
gDifferences{|FIELD of {gfd_name}|} difA vizA eqA (FIELD x) (FIELD y) = [(Just gfd_name,old,new) \\  (_,old,new) <- difA x y]
gDifferences{|OBJECT|} difA vizA eqA (OBJECT x) (OBJECT y)
    | eqA x y   = []
                = [(Nothing,concat (vizA AsSingleLine (Just x)),concat (vizA AsSingleLine (Just y)))]

derive gDifferences (,)
