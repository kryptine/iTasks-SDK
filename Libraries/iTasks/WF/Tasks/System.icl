implementation module iTasks.WF.Tasks.System

import iTasks.WF.Definition
import iTasks._Framework.Task
import iTasks._Framework.IWorld
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

