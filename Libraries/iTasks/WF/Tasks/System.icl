implementation module iTasks.WF.Tasks.System

import iTasks.Internal.EngineTasks
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.Util
import iTasks.WF.Definition

traceValue :: a -> Task a | iTask a
traceValue v = mkInstantTask (\_ iworld->(Ok v, iShowErr [toSingleLineText v] iworld))

shutDown :: Int -> Task ()
shutDown exitCode = mkInstantTask (\taskId iworld -> (Ok (), {IWorld|iworld & shutdown = Just exitCode}))
