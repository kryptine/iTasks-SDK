definition module iTasks.Internal.Client.Serialization

from Data.Error import :: MaybeError, :: MaybeErrorString
from iTasks.Internal.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt

serialize_for_client :: f !*IWorld -> *(!MaybeErrorString String, !*IWorld)

serialize_in_vst :: f !*VSt -> *(!String, !*VSt)
