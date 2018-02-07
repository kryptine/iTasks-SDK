implementation module iTasks.Internal.Distributed.RemoteTask

import iTasks
//from Text import class Text, instance Text String
//import qualified Text as T
//import qualified Data.Map as DM
//import Text.Encodings.Base64
//import iTasks.Extensions.Distributed._Formatter
//import iTasks.Extensions.Distributed._Evaluation
//import iTasks.Extensions.Distributed._Util
//import iTasks.Extensions.Distributed._Types
import iTasks.Internal.Distributed.Instance
//import iTasks.Extensions.Distributed._SDS
//import iTasks.Extensions.Distributed._Attributes
from iTasks.Internal.Distributed.Domain import :: Domain(..)

remoteAssignTask :: !TaskAttributes (Task a) Domain -> Task a | iTask a
remoteAssignTask attributes task domain
	= get currentTaskInstanceNo
	>>= \taskid -> sendDistributedInstance taskid task attributes domain

