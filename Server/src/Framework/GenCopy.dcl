definition module GenCopy

from TSt import :: TSt
import Types

generic gMakeSharedCopy a :: !a !(DBid b) -> a
generic gMakeLocalCopy a :: !a !*TSt -> (a,!*TSt)

derive gMakeSharedCopy OBJECT, CONS, PAIR, FIELD, EITHER, UNIT
derive gMakeSharedCopy Int, Real, Char, Bool, String, Dynamic
derive gMakeSharedCopy [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden, Document
derive gMakeLocalCopy OBJECT, CONS, PAIR, FIELD, EITHER, UNIT
derive gMakeLocalCopy Int, Real, Char, Bool, String, Dynamic
derive gMakeLocalCopy [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden, Document