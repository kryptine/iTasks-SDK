definition module GenMerge

import StdGeneric, StdMaybe, Void, Either, Types

mergeValues :: a a a -> a | gMerge{|*|} a

:: MergeMode
:: MergeResult a
generic gMerge a :: MergeMode a a a -> MergeResult a

derive gMerge OBJECT, CONS, PAIR, FIELD, EITHER, UNIT
derive gMerge Int, Real, Char, Bool, String
derive gMerge Document, [], Maybe, Either, (,), (,,), (,,,), Void, Display, Editable, Hidden
derive bimap MergeResult