implementation module iTasks.Framework.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from System.Time					import :: Timestamp, time
from Text.JSON						import :: JSONNode
from iTasks.API.Core.Types	        import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from iTasks.Framework.TaskState		import :: TaskListEntry
from iTasks.Framework.UIDiff		import :: UIUpdate

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World
from StdFunc import const

from Data.List import splitWith
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

import System.Time, StdList, Text.Encodings.Base64, _SystemArray, StdBool, StdTuple, Text.JSON, Data.Error, Data.Map
import iTasks.Framework.TaskStore, iTasks.Framework.Util
import iTasks.Framework.Serialization
import iTasks.Framework.SDS

iworldLocalDate :: Shared Date
iworldLocalDate = createReadWriteSDS "IWorld" "localDate" read write
where
    read Void iworld=:{IWorld|clocks={localDate}} = (Ok localDate,iworld)
    write Void localDate iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & localDate=localDate}})

iworldLocalTime :: Shared Time
iworldLocalTime = createReadWriteSDS "IWorld" "localTime" read write
where
    read Void iworld=:{IWorld|clocks={localTime}} = (Ok localTime,iworld)
    write Void localTime iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & localTime=localTime}})

iworldUTCDate :: Shared Date
iworldUTCDate = createReadWriteSDS "IWorld" "utcDate" read write
where
    read Void iworld=:{IWorld|clocks={utcDate}} = (Ok utcDate,iworld)
    write Void utcDate iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & utcDate=utcDate}})

iworldUTCTime :: Shared Time
iworldUTCTime = createReadWriteSDS "IWorld" "utcTime" read write
where
    read Void iworld=:{IWorld|clocks={utcTime}} = (Ok utcTime,iworld)
    write Void utcTime iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & utcTime=utcTime}})

updateClocks :: !*IWorld -> *IWorld
updateClocks iworld=:{IWorld|clocks,current,world}
	# (timestamp,world)		= time world
    //Determine current date and time
	# (DateTime localDate localTime,world)		= currentLocalDateTimeWorld world
	# (DateTime utcDate utcTime,world)			= currentUTCDateTimeWorld world
	# iworld = {IWorld|iworld  & current = {current & timestamp = timestamp}, world = world}
    //Write SDS's if necessary
    # iworld = if (localDate == clocks.localDate) iworld (snd (write localDate iworldLocalDate iworld))
    # iworld = if (localTime == clocks.localTime) iworld (snd (write localTime iworldLocalTime iworld))
    # iworld = if (utcDate == clocks.utcDate) iworld (snd (write utcDate iworldUTCDate iworld))
    # iworld = if (utcTime == clocks.utcTime) iworld (snd (write utcTime iworldUTCTime iworld))
    = iworld

//Determine the expiration of request, thereby determining the poll interval of
//polling clients
REGULAR_EXPIRY		:== 10000
FAST_EXPIRY			:== 100
IMMEDIATE_EXPIRY	:== 0

getResponseExpiry :: !InstanceNo !*IWorld -> (!Maybe Int, !*IWorld) 
getResponseExpiry instanceNo iworld=:{refreshQueue=[]} = (Just REGULAR_EXPIRY,iworld)
getResponseExpiry instanceNo iworld=:{refreshQueue} = (Just FAST_EXPIRY,iworld)

addUIUpdates :: !InstanceNo ![UIUpdate] !*IWorld -> *IWorld
addUIUpdates instanceNo [] iworld = iworld
addUIUpdates instanceNo updates iworld=:{uiUpdates}
	= {iworld & uiUpdates = put instanceNo (maybe updates (\u -> u ++ updates) (get instanceNo uiUpdates)) uiUpdates}

popUIUpdates :: ![InstanceNo] !*IWorld -> (![(!InstanceNo,![UIUpdate])],!*IWorld)
popUIUpdates instances iworld=:{uiUpdates}
    # uiUpdates     = toList uiUpdates
    # outUpdates    = [m \\ m=:(instanceNo,updates) <- uiUpdates | isMember instanceNo instances]
    # uiUpdates     = [m \\ m=:(instanceNo,_) <- uiUpdates | not (isMember instanceNo instances)]
	= (outUpdates, {iworld & uiUpdates = fromList uiUpdates})

clearUIUpdates :: !InstanceNo !*IWorld -> *IWorld
clearUIUpdates instanceNo iworld=:{uiUpdates}
    = {iworld & uiUpdates = del instanceNo uiUpdates}

//Wrapper instance for file access
instance FileSystem IWorld
where
	fopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = fopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
	fclose file iworld=:{IWorld|world}
		# (ok,world) = fclose file world
		= (ok,{IWorld|iworld & world = world})
	stdio iworld=:{IWorld|world}
		# (io,world) = stdio world
		= (io,{IWorld|iworld & world = world})
	sfopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = sfopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
