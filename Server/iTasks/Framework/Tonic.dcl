definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task
from System.Time import :: Timestamp

:: TonicTune =
	{ moduleName  :: String
	, taskName    :: String
	, entryUniqId :: Int
	, exitUniqId  :: Int
	, valAsStr    :: Maybe String
	}

:: TraceType = EnterTrace | ExitTrace

:: TonicTrace =
	{ traceType  :: !TraceType
	, tuneInfo   :: !TonicTune
	, traceUser  :: !User
	, traceTime  :: !Timestamp
	}

derive class iTask TonicTrace, TraceType, TonicTune

tonicTraces :: Shared [TonicTrace]

tonicTune :: String String Int Int (Task a) -> Task a

tonicBind :: String String Int Int (Task a) (a -> Task b) -> Task b | iTask a & iTask b

instance tune TonicTune

tonicLogin :: String -> Task Void

tonicPubTask :: String -> PublishedTask
