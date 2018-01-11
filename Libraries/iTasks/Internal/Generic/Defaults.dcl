definition module iTasks.Internal.Generic.Defaults

from StdGeneric import :: UNIT,::EITHER,::PAIR,::OBJECT,::CONS,::RECORD,::FIELD
/**
* Creates default values
*
* @param Conspos path, this may be passed to create a specific constructor of an ADT.
*        If you simply want to create the first constructor you can pass an empty list.
*/
generic gDefault a :: a

derive	gDefault UNIT, PAIR, EITHER, CONS, OBJECT, RECORD, FIELD
derive	gDefault Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (->), Dynamic
derive	gDefault Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp, Timespec

from Text.JSON import :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Map import :: Map
from System.Time import :: Timestamp, :: Timespec

// Wrapper functions for updating
defaultValue :: a | gDefault{|*|} a
