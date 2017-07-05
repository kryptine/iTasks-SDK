implementation module iTasks.WF.Tasks.System

import iTasks.WF.Definition
import iTasks.Internal.Task
import iTasks.Internal.IWorld
import iTasks.Internal.Generic.Visualization
import StdDebug
import Data.Error, Data.Maybe

traceValue :: a -> Task a | iTask a
traceValue v = mkInstantTask eval
where
    eval _ iworld
       # iworld = trace_n (toSingleLineText v) iworld
       = (Ok v,iworld)

shutDown :: Int -> Task ()
shutDown exitCode = mkInstantTask (\taskId iworld -> (Ok (), {IWorld|iworld & shutdown = Just exitCode}))

