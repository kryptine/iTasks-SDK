definition module iTasks.API.Core.SDSCombinators

from iTasks.Framework.SDS import :: RWShared, :: ROShared, :: WriteShare
from Data.Void import :: Void
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString

// read combinator
(>?>) infixl 6 :: !(RWShared rx wx) !(rx -> MaybeErrorString (RWShared ry wy)) -> RWShared ry wx
// write combinator
(>!>) infixl 6 :: !(RWShared r w`) !(!w -> MaybeErrorString (RWShared r` w``), !w r` -> MaybeErrorString [WriteShare]) -> RWShared r w
