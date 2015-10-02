definition module iTasks._Framework.UILayout

import iTasks.API.Core.TaskLayout
import iTasks._Framework.Task

taskUITree :: !TaskTree -> TaskUITree

taskUI :: !TaskUIs !TaskTree -> TaskRep

instance toString TaskUITree

// this function should not be necessary: TaskRep should be transformed to (Maybe UIDef)
iso :: !TaskRep -> Maybe UIDef
