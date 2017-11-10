implementation module iTasks.API.Extensions.Distributed.RemoteTask

import iTasks
from Text import class Text, instance Text String
import qualified Text as T
import qualified Data.Map as DM
import Text.Encodings.Base64
import iTasks.API.Extensions.Distributed._Formatter
import iTasks.API.Extensions.Distributed._Evaluation
import iTasks.API.Extensions.Distributed._Util
import iTasks.API.Extensions.Distributed._Types
import iTasks.API.Extensions.Distributed.Instance
import iTasks.API.Extensions.Distributed._SDS
import iTasks.API.Extensions.Distributed._Attributes
import iTasks.API.Extensions.Distributed.Engine
from iTasks.API.Extensions.Distributed.Task import :: Domain(..)

remoteAssignTask :: !TaskAttributes (Task a) Domain -> Task a | iTask a
remoteAssignTask attributes task domain
	= get currentTaskInstanceNo
	>>= \taskid -> sendDistributedInstance taskid task attributes domain
