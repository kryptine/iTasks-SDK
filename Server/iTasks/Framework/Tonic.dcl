definition module iTasks.Framework.Tonic

from iTasks.Framework.Shared import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.API.Core.CoreCombinators import class tune
from iTasks.API.Core.SystemTypes import :: User

:: Tonic = Tonic String

tonicShare :: !User -> Shared String

instance tune Tonic

