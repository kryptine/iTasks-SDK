definition module Tests.Common.MinimalTasks
/**
* This module defines a series of very simple tasks
* to quickly verify the core task constructs individually.
*/
import iTasks

minimalEditor :: Task String
minimalEditlet :: Task String
minimalStep :: Task String
minimalParallel :: Task (String,String)
minimalParallelOperations :: Task [Int] 
minimalForever :: Task String
