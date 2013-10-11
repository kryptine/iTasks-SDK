implementation module iTasks.Framework.Client.Tasklet

import StdEnv, Data.Void, iTasks.API.Core.Client.Tasklet

handleJSEvent :: (TaskletEventHandlerFunc a) !TaskId *JSWorld -> Void
handleJSEvent origHandler taskId event = undef

handleInterfaceCall :: !f !String !arg -> Void 
handleInterfaceCall origHandler taskId arg = undef
