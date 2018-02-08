definition module iTasks.Internal.Generic.Defaults

import Data.Generics.GenDefault

derive gDefault (->), Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp

from Text.JSON import :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Map import :: Map
from System.Time import :: Timestamp

// Wrapper functions for updating
defaultValue :: a | gDefault{|*|} a
