definition module iTasks.Framework.Tonic

from iTasks.Framework.Shared import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.CoreCombinators import class tune
from iTasks.API.Core.SystemTypes import :: User
from iTasks.API.Core.CoreTasks import :: Task

:: TonicTune =
	{ moduleName  :: String
	, taskName    :: String
	, entryUniqId :: Int
	, exitUniqId  :: Int
	}

:: TraceType = EnterTrace | ExitTrace

:: TonicTrace =
	{ traceType  :: !TraceType
	, tuneInfo   :: !TonicTune
	}

derive class iTask TonicTrace, TraceType, TonicTune

userActiveTask :: !User -> Shared [TonicTrace]

tonicTune :: String String Int Int (Task a) -> Task a

instance tune TonicTune

tonicLogin :: String -> Task Void

tonicPubTask :: String -> PublishedTask
