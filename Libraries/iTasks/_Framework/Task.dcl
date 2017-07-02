definition module iTasks._Framework.Task
/**
* This module provides types for the definition of tasks.
*/

import iTasks.API.Core.Types
import iTasks._Framework.Generic
import iTasks.WF.Definition
from iTasks._Framework.Tonic.AbsSyn import :: ExprId (..)
from iTasks.WF.Tasks.IO import :: ExternalProcessHandlers, :: ConnectionHandlers

from iTasks._Framework.TaskState			import :: TaskTree
from iTasks.SDS.Definition import :: RWShared
from iTasks.UI.Definition import :: UIChange
from Data.Map			import :: Map
from Data.CircularStack import :: CircularStack
from System.OSError		import :: MaybeOSError

derive JSONEncode		Task
derive JSONDecode		Task
derive gDefault			Task
derive gText	        Task
derive gEditor			Task
derive gEq				Task

//Low-level tasks that handle network connections
:: ConnectionTask = ConnectionTask !(ConnectionHandlersIWorld Dynamic Dynamic Dynamic) !(RWShared () Dynamic Dynamic)

//Definition of low-level network interaction
/*
:: ConnectionHandlers l r w = 
    { onConnect         :: !(String r   -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onData            :: !(String l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onShareChange     :: !(       l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onDisconnect      :: !(       l r -> (!MaybeErrorString l, Maybe w                  ))
	}
*/
//Version of connection handlers with IWorld side-effects that is still necessary for built-in framework handlers
:: ConnectionHandlersIWorld l r w =
    { onConnect     :: !(String r   *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onData        :: !(String l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onShareChange :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onTick        :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onDisconnect  :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w,                   !*IWorld))
    }

//Low-level task that handles external processes
:: ExternalProcessTask = ExternalProcessTask !(ExternalProcessHandlers Dynamic Dynamic Dynamic) !(RWShared () Dynamic Dynamic)

/*
:: ExitCode = ExitCode !Int
:: ExternalProcessHandlers l r w =
    { onStartup     :: !(           r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onOutData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onErrData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onShareChange :: !(         l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onExit        :: !(ExitCode l r -> (!MaybeErrorString l, !Maybe w                  ))
    }
*/

//Background computation tasks
:: BackgroundTask = BackgroundTask !(*IWorld -> *(!MaybeError TaskException (), !*IWorld))

/**
* Wraps a set of connection handlers and a shared source as a connection task
*/
wrapConnectionTask :: (ConnectionHandlers l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w

/**
* Wraps a set of handlers and a shared source as an external process task
*/
wrapExternalProcTask :: !(ExternalProcessHandlers l r w) !(RWShared () r w) -> ExternalProcessTask | TC l & TC r & TC w & iTask l

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a

