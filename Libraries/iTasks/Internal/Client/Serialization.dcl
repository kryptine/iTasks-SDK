definition module iTasks.Internal.Client.Serialization

from Data.Error import :: MaybeError, :: MaybeErrorString
from iTasks.Internal.IWorld import :: IWorld

serialize_for_client :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)
