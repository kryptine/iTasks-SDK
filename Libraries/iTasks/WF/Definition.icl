implementation module iTasks.WF.Definition

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.TaskState import :: TaskTree
from iTasks.Internal.TaskEval import :: TaskEvalOpts, :: TaskEvalInfo
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Editor.Generic

from Text.JSON import :: JSONNode
from Data.Map import :: Map(..)
from Data.Maybe import :: Maybe
from System.Time import :: Timestamp
import Data.Functor
import Text, Text.JSON

import StdString, StdClass, StdBool, StdInt


exception :: !e -> TaskException | TC, toString e
exception e = (dynamic e, toString e)

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s

//Task id

instance toString TaskId
where
	toString (TaskId topNo taskNo)		= join "-" [toString topNo,toString taskNo]

instance fromString TaskId
where
	fromString s = case split "-" s of
		[topNo,taskNo]	= TaskId (toInt topNo) (toInt taskNo)
		_				= TaskId 0 0

instance == TaskId
where
	(==) (TaskId a0 b0) (TaskId a1 b1) = a0 == a1 && b0 == b1

instance < TaskId
where
	(<) (TaskId a0 b0) (TaskId a1 b1) = if (a0 == a1) (b0 < b1) (a0 < a1)

class toInstanceNo t :: t -> InstanceNo
instance toInstanceNo InstanceNo where toInstanceNo no = no
instance toInstanceNo TaskId where toInstanceNo (TaskId no _) = no

